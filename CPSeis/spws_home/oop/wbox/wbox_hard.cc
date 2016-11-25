/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/

//---------------------- wbox_hard.cc -------------------------//
//---------------------- wbox_hard.cc -------------------------//
//---------------------- wbox_hard.cc -------------------------//

//          implementation file for the WboxHard class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_hard.hh"
#include "wbox/wbox_screen.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_field.hh"
#include "wbox/wbox_vector.hh"
#include "wbox/wbox_link.hh"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxHard::WboxHard(WboxBox *box)
         :
            _box             (box),
            _hardcopy        (NULL)
{
  assert(_box);
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxHard::~WboxHard()
{
}



//----------------------- maybe save table ----------------------------//
//----------------------- maybe save table ----------------------------//
//----------------------- maybe save table ----------------------------//

     // public.

void WboxHard::maybeSaveTable(const char *endkey)  const
{
  if(strcmp(endkey, "HARDCOPY") != 0) return;
  saveTable(NULL);
}



//----------------------- save table ----------------------------------//
//----------------------- save table ----------------------------------//
//----------------------- save table ----------------------------------//

     // public.

void WboxHard::saveTable(const char *filename)  const
{
  if(filename == NULL || filename[0] == '\0' || filename[0] == ' ')
      {
      const char *filename2 = privateGetFilename();
      if(_hardcopy)
          {
          int numlines = privateGetNumlines();
          void *data   = _box->getUserData();
          const char *filename3 = _hardcopy(data, numlines, filename2);
          if(filename3) privateSaveTable(filename3);
          }
      else
          {
          privateSaveTable(filename2);
          }
      }
  else
      {
      privateSaveTable(filename);
      }
}



//--------------------- private get filename ----------------------//
//--------------------- private get filename ----------------------//
//--------------------- private get filename ----------------------//

   // private.

const char *WboxHard::privateGetFilename()  const
{
  static char filename[200];
  static int count = 0;
  count++;
  sprintf(filename, "%s.hardcopy%d", _box->getBoxName(), count);
  return filename;
}



//------------------------ private get numlines ----------------------//
//------------------------ private get numlines ----------------------//
//------------------------ private get numlines ----------------------//

     // private.
     // returns an approximate number.

int WboxHard::privateGetNumlines()  const
{
  int nrow = _box->getNrow();
  int lmin = _box->getLmin();
  int lmax = _box->getLmax();
  for(int itab = lmin; itab <= lmax; itab++)
      {
      WboxLink *link = _box->getLinkPointer(itab);
      int n = link->getN();
      nrow += n;
 //// nrow += _box->getLinkPointer(itab)->getN();  // equivalent.
      }
  return nrow;
}



//----------------------- divider ---------------------------------//
//----------------------- divider ---------------------------------//
//----------------------- divider ---------------------------------//

static void divider(FILE *stream)
{
  fprintf(stream,
 "------------------------------------------------------------------------\n");
}



//--------------------- private save table ----------------------------//
//--------------------- private save table ----------------------------//
//--------------------- private save table ----------------------------//

     // private.

void WboxHard::privateSaveTable(const char *filename)  const
{
  assert(filename);
  char *message = str_newstrcat("saving table to file ", filename, NULL);
  _box->showMessage(message);
  _box->getScreenPointer()->ringBell();
  free(message);

  FILE *stream = fopen(filename, "w");
  if(stream == NULL) 
    {
    printf("Could not create file %s\n", filename);
    return;
    }
  fprintf(stream, "file %s\n", filename);
  fprintf(stream, "saved %s\n", str_time_date());
  divider(stream);
  privateSaveImage(stream);
  privateSaveArrays(stream);
  divider(stream);
  fprintf(stream, "end file %s\n", filename);
  fprintf(stream, "saved %s\n", str_time_date());
  fclose(stream);

  message = str_newstrcat("table saved to file ", filename, NULL);
  _box->showMessage(message);
  _box->getScreenPointer()->ringBell();
  free(message);
}



//------------------------ adjust textbuf ---------------------------//
//------------------------ adjust textbuf ---------------------------//
//------------------------ adjust textbuf ---------------------------//

   // if this is a toggle or radio button: adjusts textbuf and returns 2.
   // otherwise: returns argument nchar.

static int adjust_textbuf(char *textbuf, int svar, int nchar)
{
  if((svar == 3 || svar == 4 || svar == -3 || svar == -4) &&
     (textbuf[0] != '\0' && textbuf[0] != ' '))
      {
      if(textbuf[0] == '0') strcpy(textbuf, "<F");
      else                  strcpy(textbuf, "T>");
      return 2;
      }
  return nchar;
}



//------------------------- private save image --------------------------//
//------------------------- private save image --------------------------//
//------------------------- private save image --------------------------//

     // private.

void WboxHard::privateSaveImage(FILE *stream)  const
{
  int irow;
/*
  int lmin = _box->getLmin();
  int lmax = _box->getLmax();
*/
  int kmin = _box->getKmin();
  int kmax = _box->getKmax();
/*
  int nrow = _box->getNrow();
*/
  int nrow = _box->getNrow() - _box->numBottomRows();
  int ncol = _box->getNcol();
/*
  nrow--;
  if(lmax >= lmin) nrow--;
*/
  if(nrow == 0 || ncol == 0) return;
  if(kmax < kmin) return;
  char **buffer  = new char* [nrow];
  char  *textbuf = new char  [ncol+1];
  for(irow = 0; irow < nrow; irow++)
      {
      buffer[irow] = new char [ncol+1];
      memset(buffer[irow], ' ', ncol);
      buffer[irow][ncol] = '\0';
      }
  for(int ifield = kmin; ifield <= kmax; ifield++)
      {
      WboxField  *field  = _box->getFieldPointer(ifield);
      int irow2 = field->getIrow();
      int icol2 = field->getIcol();
      int nchar = field->getNchar();
      int index = field->getIndex();
      int svar  = field->getUserSwitch(index);
      int ident = field->getIdent();
      if(ident == 9998 || ident == 9999) continue;  // will happen.
      if(svar == -77 || svar == -99)     continue;  // might happen.
      if(irow2 > nrow)                   continue;  // might happen.
      if(icol2+nchar-1 > ncol)           continue;  // might happen.
      field->getAndEncodeUserValue(index, textbuf);
      nchar = adjust_textbuf(textbuf, svar, nchar);
      memcpy(&buffer[irow2-1][icol2-1], textbuf, nchar);
      }
  for(irow = 0; irow < nrow; irow++)
      {
      str_remove_trailing_blanks(buffer[irow], buffer[irow]);
      fprintf(stream, "%s\n", buffer[irow]);
      delete [] buffer[irow];
      }
  delete [] buffer;
  delete [] textbuf;
}



//------------------------- private save arrays --------------------------//
//------------------------- private save arrays --------------------------//
//------------------------- private save arrays --------------------------//

     // private.

void WboxHard::privateSaveArrays(FILE *stream)  const
{
  int ncol = _box->getStartingNcol();
  int lmin = _box->getLmin();
  int lmax = _box->getLmax();
  char *buffer  = new char [ncol+1];
  char *textbuf = new char [ncol+1];
  for(int itab = lmin; itab <= lmax; itab++)
      {
      divider(stream);
      WboxLink *link = _box->getLinkPointer(itab);
      int vmin = link->getVmin();
      int vmax = link->getVmax();
      int n    = link->getN();

/////////// print prompts:

      memset(buffer, ' ', ncol);
      buffer[ncol] = '\0';
      for(int ivector = vmin; ivector <= vmax; ivector++)
          {
          WboxVector *vector = _box->getVectorPointer(ivector);
          WboxLink   *link2  = vector->getLinkPointer();
          if(link2 == NULL) continue;
          int ifield = vector->getIfield0();
          WboxField *field = _box->getFieldPointer(ifield);
          int icol  = field->getIcol();
          int nchar = field->getNchar();
          int svar  = field->getUserSwitch(1);
          if(svar == -77 || svar == -99) continue;    // might happen.
          if(icol + nchar - 1 > ncol)    continue;    // should not happen.
          field->getAndEncodeUserValue(1, textbuf);
          nchar = adjust_textbuf(textbuf, svar, nchar);
          memcpy(&buffer[icol-1], textbuf, nchar);
          }
      str_remove_trailing_blanks(buffer, buffer);
      fprintf(stream, "%s\n", buffer);

/////////// print arrays:

      for(int i = 0; i < n; i++)
          {
          memset(buffer, ' ', ncol);
          buffer[ncol] = '\0';
          for(int ivector = vmin; ivector <= vmax; ivector++)
              {
              WboxVector *vector = _box->getVectorPointer(ivector);
              WboxLink   *link2  = vector->getLinkPointer();
              if(link2 == NULL) continue;
              int icol  = vector->getIcol();
              int nchar = vector->getNchar();
              int svar  = vector->getUserSwitch(i+1);
              if(svar == -77 || svar == -99) continue;   // might happen.
              if(icol + nchar - 1 > ncol)    continue;   // should not happen.
              vector->getAndEncodeUserValue(i+1, textbuf);
              nchar = adjust_textbuf(textbuf, svar, nchar);
              memcpy(&buffer[icol-1], textbuf, nchar);
              }
          str_remove_trailing_blanks(buffer, buffer);
          fprintf(stream, "%s\n", buffer);
          }
      }
  delete [] buffer;
  delete [] textbuf;
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

