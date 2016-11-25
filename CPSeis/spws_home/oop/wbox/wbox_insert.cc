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

//---------------------- wbox_insert.cc -------------------------//
//---------------------- wbox_insert.cc -------------------------//
//---------------------- wbox_insert.cc -------------------------//

//         implementation file for the WboxInsert class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_insert.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_field.hh"
#include "wbox/wbox_vector.hh"
#include "wbox/wbox_link.hh"
#include "str.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>




//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxInsert::WboxInsert(WboxBox *box)
         :
            _box     (box)
{
  assert(_box);
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxInsert::~WboxInsert()
{
}



//-------------------------- show ignore message --------------------//
//-------------------------- show ignore message --------------------//
//-------------------------- show ignore message --------------------//

      // private.
      // if one of these messages is seen, it means that the
      // programmer should take the indicated action in the
      // trap (if desired), and then set endkey to " ".
      // this is because this class is unable to take the
      // action because one or more of the arrays (or n) is not
      // updated by a pointer.

void WboxInsert::showIgnoreMessage(const char *endkey)
{
  if     (!strcmp(endkey, "INSERT"))
                           _box->maybeShowMessage("INSERT ignored");
  else if(!strcmp(endkey, "REMOVE"))
                           _box->maybeShowMessage("REMOVE ignored");
  else if(!strcmp(endkey, "^P"))
                           _box->maybeShowMessage("PASTE ignored");
  else if(!strcmp(endkey, "^C"))
                           _box->maybeShowMessage("CLEAR ignored");
}



//--------------------- static variables -----------------------------//
//--------------------- static variables -----------------------------//
//--------------------- static variables -----------------------------//


#define NUMBER 50

static WboxLink *linkkeep = NULL;
static int      indexkeep = 0;
static int      ivg[NUMBER];
static float    fvg[NUMBER];
static double   dvg[NUMBER];
static char     cvg[NUMBER][101];



//-------------------------- insert or remove -------------------//
//-------------------------- insert or remove -------------------//
//-------------------------- insert or remove -------------------//

  // public.
  // insert or remove a row at the location of the datafield specified
  //   by _box->_ksave.
  // if nread is not zero, this routine does nothing.
  // if this datafield is not part of an array, this routine does nothing.
  // if the update method for any vector in this set of linked arrays
  //   (or for n) is to call an update function, this routine does nothing.
  // the variable number _box->_ksave will not change.
  // n may be increased or decreased, and array variables may be moved.

  // REMOVE deletes the row and puts it into the row-buffer.
  // INSERT inserts a new row from the row-buffer.  If the row-buffer is
  //    empty, or contains info from another set of linked arrays, or the
  //    index does not match the info in the row-buffer, the inserted row
  //    is set to nils.
  // ^P (PASTE) inserts a new row from the row-buffer.  If the row-buffer is
  //    empty, or contains info from another set of linked arrays, the
  //    inserted row is set to nils.
  // ^C (CLEAR) clears the row buffer to nils (unless it contains info
  //    from another set of linked arrays).

void WboxInsert::insertOrRemove(char *endkey, int nread)
{
  if(!strcmp(endkey, "INSERTED") || !strcmp(endkey, "REMOVED"))
      {
      strcpy(endkey, " ");  
      return;
      }

  if(strcmp(endkey, "INSERT"  ) && strcmp(endkey, "REMOVE" ) &&
     strcmp(endkey, "^P"      ) && strcmp(endkey, "^C"     )) return;

  WboxField *field = _box->getActiveFieldPointer();
  WboxLink  *link  = field->getLinkPointer();
  if(nread != 0 || link == NULL) return;
  if(link->getNpoint() == NULL && link->getNpointInt() == NULL)
      {
/*
      showIgnoreMessage(endkey);
*/
      return;
      }

  int kmin = _box->getKmin();
  int kmax = _box->getKmax();

  for(int ifield = kmin; ifield <= kmax; ifield++)
      {
      WboxField *field2 = _box->getFieldPointer(ifield);
      if(field2->getLinkPointer() == link &&
         field2->getUptype() == WboxVector::_FUN)
          {
/*
          showIgnoreMessage(endkey);
*/
          return;
          }
      }

  if(!strcmp(endkey, "^C") && link == linkkeep)
      {
      for(int i = 0; i < NUMBER; i++)
          {
          strcpy(cvg[i], " ");
          ivg[i] = INIL;
          fvg[i] = FNIL;
          dvg[i] = DNIL;
          }
      _box->maybeShowMessage("ROW-BUFFER CLEARED");
      }
  else if(!strcmp(endkey, "INSERT") || !strcmp(endkey, "^P"))
      {
      insert(endkey);
      }
  else if(!strcmp(endkey, "REMOVE"))
      {                           
      remove(endkey);
      }
}



//---------------- use array and use radio -------------------//
//---------------- use array and use radio -------------------//
//---------------- use array and use radio -------------------//

  // private.
  // useArray returns TRUE if array is to be used for inserting/deleting.
  // useRadio returns TRUE if array is to be used for stepping radio value.
  // array is used only when the datafield is the first one of the array
  //   (in the correct set of linked arrays).
  // array is used only if no previous array has the same pointer.

int WboxInsert::useArray(WboxLink *link, WboxField *field, int ifield)
{
  if(field->getLinkPointer() != link) return FALSE;
  if(field->getIrow() != link->getIarow()) return FALSE;
  if(field->getUptype() != WboxVector::_POINT) return FALSE;
  int kmin = _box->getKmin();
  for(int ifield3 = kmin; ifield3 < ifield; ifield3++)
      {
      WboxField *field3 = _box->getFieldPointer(ifield3);
      if(field ->getUpdatePointer() ==
         field3->getUpdatePointer()) return FALSE;
      }
  return TRUE;
}



int WboxInsert::useRadio(WboxLink *link, WboxField *field)
{
  if(field->getLinkPointer() != link) return FALSE;
  if(field->getIrow() != link->getIarow()) return FALSE;
  if(field->getUptype() != WboxVector::_RPOINT) return FALSE;
  return TRUE;
}



//-------------------------- insert -----------------------------//
//-------------------------- insert -----------------------------//
//-------------------------- insert -----------------------------//

   // private.
   // inserts a row if possible.
   // increments n (in user area) if successful.
   // sets endkey to "INSERTED" if successful.

void WboxInsert::insert(char *endkey)
{
  WboxField *field = _box->getActiveFieldPointer();
  WboxLink  *link  = field->getLinkPointer();
  int        index = field->getIndex();
  int        n     = field->getN    ();
  int        nmax  = field->getNmax ();

  int use_buffer = (link == linkkeep &&
         (index == indexkeep || !strcmp(endkey, "^P")));

  if(n >= nmax)
      {
      _box->maybeShowMessage("NO ROOM TO INSERT");
      return;
      }
  else if(index > n && use_buffer == FALSE)
      {
      _box->maybeShowMessage("NO NEED TO INSERT");
      return;
      }

  int kount = 0;
  int kmin = _box->getKmin();
  int kmax = _box->getKmax();

  for(int ifield2 = kmin; ifield2 <= kmax; ifield2++)
      {
      WboxField *field2 = _box->getFieldPointer(ifield2);
       if(useArray(link, field2, ifield2))
           {
           if(index <= n)
               {
               for(int i = n; i >= index; i--)
                   {
                   field2->copyUserValue(i, i + 1);
                   }
               }
           if(use_buffer && kount < NUMBER)
               {
               field2->putUserValue(index, ivg[kount],
                        fvg[kount], dvg[kount], cvg[kount]);
               kount++;
               }
           else
               {
               field2->decodeAndPutUserValue(index, " ");
               }
           }
       else if(useRadio(link, field2))
           {
           if(link == linkkeep && index == indexkeep)
               {
               field2->stepUserRadioValue(index, "MAYBE");
               }
           else
               {
               field2->stepUserRadioValue(index, "INSERT");
               }
           }
      }
  link->putN(n + 1);
  strcpy(endkey, "INSERTED");
  _box->maybeShowMessage("ROW INSERTED");
}



//-------------------------- remove -----------------------------//
//-------------------------- remove -----------------------------//
//-------------------------- remove -----------------------------//

   // private.
   // removes a row if possible.
   // decrements n (in user area) if successful.
   // sets endkey to "REMOVED" if successful.
   // saves link pointer and index into static variables if successful.
   // if all values to be put into buffer are nil, the buffer is not used.

void WboxInsert::remove(char *endkey)
{
  WboxField *field = _box->getActiveFieldPointer();
  WboxLink  *link  = field->getLinkPointer();
  int        index = field->getIndex();
  int        n     = field->getN    ();

  if(index > n)
      {
      _box->maybeShowMessage("NOTHING TO REMOVE");
      return;
      }

  int kmin  = _box->getKmin();
  int kmax  = _box->getKmax();

  int use_buffer;
  if(link == linkkeep)
      {
      use_buffer = FALSE;
      for(int ifield2 = kmin; ifield2 <= kmax; ifield2++)
          {
          WboxField *field2 = _box->getFieldPointer(ifield2);
          if(useArray(link, field2, ifield2))
              {
              char textbuf[101];
              field2->getAndEncodeUserValue(index, textbuf);
              str_remove_all_blanks(textbuf, textbuf);
              if(textbuf[0] != '\0' && textbuf[0] != ' ') use_buffer = TRUE;
              }
          }
      }
  else
      {
      use_buffer = TRUE;
      }

  int kount = 0;

  for(int ifield2 = kmin; ifield2 <= kmax; ifield2++)
      {
      WboxField *field2 = _box->getFieldPointer(ifield2);
      if(useArray(link, field2, ifield2))
           {
           if(use_buffer && kount < NUMBER)
               {
               field2->getUserValue(index, &ivg[kount],
                        &fvg[kount], &dvg[kount], cvg[kount]);
               kount++;
               }
           if(index < n)
               {
               for(int i = index; i <= n - 1; i++)
                   {
                   field2->copyUserValue(i + 1, i);
                   }
               }
           }
       else if(useRadio(link, field2))
           {
           field2->stepUserRadioValue(index, "REMOVE");
           }
      }

  link->putN(n - 1);
  strcpy(endkey, "REMOVED");
  _box->maybeShowMessage("ROW REMOVED");
  linkkeep = link;
  indexkeep = index;
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

