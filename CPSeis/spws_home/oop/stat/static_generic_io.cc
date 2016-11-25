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

//--------------------- static_generic_io.cc ---------------------//
//--------------------- static_generic_io.cc ---------------------//
//--------------------- static_generic_io.cc ---------------------//

//         implementation file for the StaticGenericIO class
//                  not derived from any class
//                      subdirectory stat



#include "stat/static_generic_io.hh"
#include "stat/static_decode.hh"
#include "cprim.h"
#include "readcard.h"
#include "named_constants.h"
#include "inquire.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StaticGenericIO::StaticGenericIO(StatStruct *ss)
           :
          _ss           (ss),
          _decode       (NULL),
          _keepname     (NULL),
          _xmin         (FNIL),
          _xmax         (FNIL),
          _ymin         (FNIL),
          _ymax         (FNIL),
          _vmin         (FNIL),
          _vmax         (FNIL)
{
  assert(_ss);
  _keepname = newstr("");
  _decode = new StaticDecode();
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StaticGenericIO::~StaticGenericIO()
{
  free(_keepname);
  delete _decode;
}



//------------------------ how read generic file -------------------//
//------------------------ how read generic file -------------------//
//------------------------ how read generic file -------------------//


void StaticGenericIO::howReadGenericFile(const char *how)
{
  _decode->howDecode(how);
  _xmin = FNIL;
  _xmax = FNIL;
  _ymin = FNIL;
  _ymax = FNIL;
  _vmin = FNIL;
  _vmax = FNIL;
}



const char *StaticGenericIO::howReadGenericFile()  const
{
  return _decode->howDecode();
}



int StaticGenericIO::badGenericReadCode()  const
{
  return _decode->badDecode();
}



//--------------------- scan generic file ------------------------//
//--------------------- scan generic file ------------------------//
//--------------------- scan generic file ------------------------//


int StaticGenericIO::scanGenericFile(char *msg)
{
  _xmin = FNIL;
  _xmax = FNIL;
  _ymin = FNIL;
  _ymax = FNIL;
  _vmin = FNIL;
  _vmax = FNIL;
  if(_keepname[0] == '\0')
      {
      strcpy(msg, "blank filename when trying to scan generic file");
      return 1;
      }
  FILE *stream = fopen(_keepname, "r");
  if(stream == NULL)
      {
      strcpy(msg, "open error when trying to scan generic file");
      return 1;
      }

  int starting = TRUE;
  int kount = 0;
  while(TRUE)
      {
      kount++;
      const char *card = readcard(stream);
      if(card == NULL)
          {
          if(feof(stream))
              {
              break;
              }
          else if(ferror(stream))
              {
              sprintf(msg,
        "read error (card %d) while scanning generic static file", kount);
              fclose(stream);
              return 1;
              }
          else
              {
              sprintf(msg,
        "line too long (card %d) or contains null characters", kount);
              fclose(stream);
              return 1;
              }
          }
      else if(card[0] == '#')
          {
          continue;
          }
      else
          {
          float xbin, ybin, value;
          int error = _decode->decodeValue (card, &xbin, &ybin, &value);
          if(error)
              {
              sprintf(msg,
     "error scanning static values (card %d) on generic static file", kount);
              fclose(stream);
              return 1;
              }
          if(starting)
              {
              _xmin = xbin;
              _xmax = xbin;
              _ymin = ybin;
              _ymax = ybin;
              _vmin = value;
              _vmax = value;
              starting = FALSE;
              }
          else
              {
              _xmin = MinimumValue(xbin, _xmin);
              _xmax = MaximumValue(xbin, _xmax);
              _ymin = MinimumValue(ybin, _ymin);
              _ymax = MaximumValue(ybin, _ymax);
              _vmin = MinimumValue(value, _vmin);
              _vmax = MaximumValue(value, _vmax);
              }
          }
      }
  fclose(stream);
  strcpy(msg, "generic static file successfully scanned for bin limits");
  return 0;
}



//-------------------- supply required header info ------------------//
//-------------------- supply required header info ------------------//
//-------------------- supply required header info ------------------//

         // private.

int StaticGenericIO::supplyRequiredHeaderInfo(StatStruct *other)
{
  assert(other);
  char type[20];
  stat_get_type(other, type);
  long  nhx  = stat_get_nhx  (other);
  long  nhy  = stat_get_nhy  (other);
  long  nhx2 = stat_get_nhx2 (other);
  long  nhy2 = stat_get_nhy2 (other);
  float x1   = stat_get_x1   (other);
  float y1   = stat_get_y1   (other);
  float xinc = stat_get_xinc (other);
  float yinc = stat_get_yinc (other);
  long  nx   = stat_get_nx   (other);
  long  ny   = stat_get_ny   (other);

  if(type[0] == ' ' || type[0] == '\0') return 1;
  if(nhx  == INIL) return 1;
  if(nhy  == INIL) return 1;
  if(nhx2 == INIL) return 1;
  if(nhy2 == INIL) return 1;
  if(x1   == FNIL) return 1;
  if(y1   == FNIL) return 1;
  if(xinc == FNIL) return 1;
  if(yinc == FNIL) return 1;
  if(nx   == INIL) return 1;
  if(ny   == INIL) return 1;

  stat_free_pointer(_ss);
  stat_set_type    (_ss, type); 
  stat_set_nhx     (_ss, nhx );
  stat_set_nhy     (_ss, nhy );
  stat_set_nhx2    (_ss, nhx2);
  stat_set_nhy2    (_ss, nhy2);
  stat_set_x1      (_ss, x1  );
  stat_set_y1      (_ss, y1  );
  stat_set_xinc    (_ss, xinc);
  stat_set_yinc    (_ss, yinc);
  stat_set_nx      (_ss, nx  );
  stat_set_ny      (_ss, ny  );
  return 0;
}



//------------------------- read file --------------------------------//
//------------------------- read file --------------------------------//
//------------------------- read file --------------------------------//


int  StaticGenericIO::readFile   (const char *filename, char *msg,
                                  StatStruct *other, const char *how)
{
  assert(filename && msg && other && how);
  if(filename[0] == '\0')
      {
      strcpy(msg, "blank filename when trying to read generic file");
      return 1;
      }
  FILE *stream = fopen(filename, "r");
  if(stream == NULL)
      {
      strcpy(msg, "open error when trying to read generic file");
      return 1;
      }
  int error = supplyRequiredHeaderInfo(other);
  if(error)
      {
      strcpy(msg, "some header info not set before reading generic file");
      return 1;
      }
  howReadGenericFile(how);
  float  x1      = stat_get_x1     (_ss);
  float  y1      = stat_get_y1     (_ss);
  float  xinc    = stat_get_xinc   (_ss);
  float  yinc    = stat_get_yinc   (_ss);
  long   nx      = stat_get_nx     (_ss);
  long   ny      = stat_get_ny     (_ss);
  int    values  = FALSE;    // whether started to read values.
  int    carding = FALSE;    // whether in process of reading history cards.
  long   skipped = 0;
  long   used    = 0;
  stat_free_pointer(_ss);
  stat_remove_cards(_ss);
  float *pointer = stat_get_pointer(_ss);
  assert(!pointer);
  if(pointer == NULL)
      {
      stat_clear_values(_ss);
      pointer = stat_get_pointer(_ss);
      }
  assert(pointer);
  int kount = 0;
  while(TRUE)
      {
      kount++;
      const char *card = readcard(stream);
      if(card == NULL)
          {
          if(feof(stream))
              {
              break;
              }
          else if(ferror(stream))
              {
              sprintf(msg,
                 "read error (card %d) on generic static file", kount);
              fclose(stream);
              return 1;
              }
          else
              {
              sprintf(msg,
         "line too long (card %d) or contains null characters", kount);
              fclose(stream);
              return 1;
              }
          }
      else if(card[0] == '#')
          {
          char token[200];
          int e = sscanf((char*)card, "# FSF %s", token);
          if(e == 1)
              {
              if     (!strcmp(token, "begin")) carding = TRUE;
              else if(!strcmp(token, "end"  )) carding = FALSE;
              }
          else
              {
              if(carding) stat_add_card(_ss, (char*)(&card[1]));
              }
          continue;
          }
      else
          {
          float xbin, ybin, value;
          int error = _decode->decodeValue (card, &xbin, &ybin, &value);
          if(error)
              {
              sprintf(msg, "error reading static values (card %d)", kount);
              fclose(stream);
              return 1;
              }
          if(ybin == FNIL && ny > 1)
              {
              sprintf(msg, "no ybin values when ny > 1 (card %d)", kount);
              fclose(stream);
              return 1;
              }
          assert(pointer);
          float   fx = (xbin - x1) / xinc;
          float   fy = (ybin - y1) / yinc;
          long    ix = NearestInteger(fx);
          long    iy = NearestInteger(fy);
          if(ybin == FNIL && ny == 1) iy = 0;
          if(ix < 0 || ix >= nx ||
             iy < 0 || iy >= ny)
              {
              skipped++;
              }
          else
              {
              long index = ix + nx * iy;
              pointer[index] = value;
              used++;
              }
          values = TRUE;
          }
      }
  char card[81];
  sprintf(card, "Generic file read  by %s:  %s",
                  stat_get_program_pointer(_ss), stat_get_time_string());
  stat_add_card(_ss, card);
  fclose(stream);
  strcpy(msg, "successfully read generic static file");
  return 0;
}



//----------------------- validate file ------------------------------//
//----------------------- validate file ------------------------------//
//----------------------- validate file ------------------------------//

      // this is the only place where the following information
      // is obtained from the file (and even then only if the
      // previous information was nil):
      //       type  nhx  nhy  nhx2  nhy2
      //       x1  y1  xinc  yinx  nx  ny
      //       howReadGenericFile
      // scanGenericFile and readFile use the above information
      // as it was obtained by validateFile or set by the user.
      // scanGenericFile and readFile do not attempt to get this
      // information from the file (as validateFile does).


long StaticGenericIO::validateFile   (const char *filename, char *info)
{
  if(filename[0] == '\0')
      {
      free(_keepname);
      _keepname = newstr("");
      stat_bad_validation(_ss);
      howReadGenericFile("");
      return INQUIRE_VALID_NO;
      }
  FILE *stream = fopen(filename, "r");
  if(stream == NULL)
      {
      free(_keepname);
      _keepname = newstr("");
      stat_bad_validation(_ss);
      howReadGenericFile("");
      return INQUIRE_VALID_NO;
      }
  int first_time = FALSE;
  if(strcmp(_keepname, filename))
      {
      free(_keepname);
      _keepname = newstr(filename);
      stat_bad_validation(_ss);
      howReadGenericFile("");
      first_time = TRUE;
      }
  char type[20];
                   stat_get_type   (_ss, type);
  long   nhx     = stat_get_nhx    (_ss);
  long   nhy     = stat_get_nhy    (_ss);
  long   nhx2    = stat_get_nhx2   (_ss);
  long   nhy2    = stat_get_nhy2   (_ss);
  float  x1      = stat_get_x1     (_ss);
  float  y1      = stat_get_y1     (_ss);
  float  xinc    = stat_get_xinc   (_ss);
  float  yinc    = stat_get_yinc   (_ss);
  long   nx      = stat_get_nx     (_ss);
  long   ny      = stat_get_ny     (_ss);
  stat_free_pointer(_ss);
  stat_remove_cards(_ss);
  int carding = FALSE;
  int howing  = FALSE;
  int kount   = 0;
  while(TRUE)
      {
      kount++;
      const char *card = readcard(stream);
      if(card == NULL)
          {
          if(feof(stream))
              {
              break;
              }
          }
      else if(card[0] == '\0')
          {
          continue;
          }
      else if(card[0] == '#')
          {
          char token[200];
          int e = sscanf((char*)card, "# FSF %s", token);
          if(e != 1)
              {
              if(carding)
                  {
                  stat_add_card(_ss, "junk");
                  }
              else if(howing)
                  {
                  if(first_time)
                      {
                      howReadGenericFile(card);
                      }
                  howing = FALSE;
                  }
              continue;
              }
          e = 2;
          if(!strcmp(token, "type"))
              {
              if(first_time)
              if(type[0] == '\0' || type[0] == ' ')
                         e = sscanf((char*)card, "# FSF %s %s" , token, type);
              }
          else if(!strcmp(token, "nhx"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %ld", token, &nhx);
              }
          else if(!strcmp(token, "nhy"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %ld", token, &nhy);
              }
          else if(!strcmp(token, "nhx2"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %ld", token, &nhx2);
              }
          else if(!strcmp(token, "nhy2"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %ld", token, &nhy2);
              }
          else if(!strcmp(token, "x1"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %g" , token, &x1);
              }
          else if(!strcmp(token, "y1"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %g" , token, &y1);
              }
          else if(!strcmp(token, "xinc"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %g" , token, &xinc);
              }
          else if(!strcmp(token, "yinc"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %g" , token, &yinc);
              }
          else if(!strcmp(token, "nx"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %ld", token, &nx);
              }
          else if(!strcmp(token, "ny"))
              {
              if(first_time)
                         e = sscanf((char*)card, "# FSF %s %ld", token, &ny);
              }
          else if(!strcmp(token, "ncards"))
              {
              }
          else if(!strcmp(token, "nvalues"))
              {
              }
          else if(!strcmp(token, "begin"))
              {
              carding = TRUE;
              }
          else if(!strcmp(token, "end"))
              {
              carding = FALSE;
              }
          else if(!strcmp(token, "code"))
              {
              howing = TRUE;
              }
          else
              {
              sprintf(info, "bad FSF code on card number %d", kount);
              stat_bad_validation(_ss);
              fclose(stream);
              return INQUIRE_VALID_NO;
              }
          if(e != 2)
              {
              sprintf(info, "read error on card number %d", kount);
              stat_bad_validation(_ss);
              fclose(stream);
              return INQUIRE_VALID_NO;
              }
          }
      else
          {
          break;
          }
      }
  int bad = 11;
  if(strcmp(type, " ")) { stat_set_type   (_ss, type); bad--; }
  if(nhx  != INIL)      { stat_set_nhx    (_ss, nhx ); bad--; }
  if(nhy  != INIL)      { stat_set_nhy    (_ss, nhy ); bad--; }
  if(nhx2 != INIL)      { stat_set_nhx2   (_ss, nhx2); bad--; }
  if(nhy2 != INIL)      { stat_set_nhy2   (_ss, nhy2); bad--; }
  if(x1   != FNIL)      { stat_set_x1     (_ss, x1  ); bad--; }
  if(y1   != FNIL)      { stat_set_y1     (_ss, y1  ); bad--; }
  if(xinc != FNIL)      { stat_set_xinc   (_ss, xinc); bad--; }
  if(yinc != FNIL)      { stat_set_yinc   (_ss, yinc); bad--; }
  if(nx   != INIL)      { stat_set_nx     (_ss, nx  ); bad--; }
  if(ny   != INIL)      { stat_set_ny     (_ss, ny  ); bad--; }
  if(bad == 0)
      {
      sprintf(info, "x and y headers %d %d  --  x and y bins %d %d",
                                 nhx, nhy, nx, ny);
      }
  else
      {
      sprintf(info, "(file has %d missing items)", bad);
      }
  fclose(stream);
  return INQUIRE_VALID_YES;
}



//------------------------- save file --------------------------------//
//------------------------- save file --------------------------------//
//------------------------- save file --------------------------------//


int  StaticGenericIO::saveFile   (const char *filename, char *msg)
{
  if(filename[0] == '\0')
      {
      strcpy(msg, "blank filename when trying to save generic file");
      return 1;
      }
  FILE *stream = fopen(filename, "w");
  if(stream == NULL)
      {
      strcpy(msg, "open error when trying to save generic file");
      return 1;
      }
  int error = privateSaveHeader(stream);
  if(error)
      {
      strcpy(msg, "error trying to save generic file header");
      fclose(stream);
      return 1;
      }
  error = privateSaveCards(stream);
  if(error)
      {
      strcpy(msg, "error trying to save cards to generic file");
      fclose(stream);
      return 1;
      }
  error = privateSaveValues(stream);
  if(error)
      {
      strcpy(msg, "error trying to save values to generic file");
      fclose(stream);
      return 1;
      }
  fclose(stream);
  strcpy(msg, "successfully saved generic static file");
  return 0;
}



//------------------- private save header ----------------------------//
//------------------- private save header ----------------------------//
//------------------- private save header ----------------------------//


int StaticGenericIO::privateSaveHeader (FILE *stream)
{
  float        x1      = stat_get_x1     (_ss);
  float        y1      = stat_get_y1     (_ss);
  float        xinc    = stat_get_xinc   (_ss);
  float        yinc    = stat_get_yinc   (_ss);
  long         nx      = stat_get_nx     (_ss);
  long         ny      = stat_get_ny     (_ss);
  long         ncards  = stat_get_ncards (_ss);
  const float *pointer = stat_get_pointer(_ss);
  long   nvalues;
  if(pointer) nvalues = nx * ny;
  else        nvalues = 0;
  int e = 0;
  if(e >= 0) e = fprintf(stream, "# FSF code follows\n");
  if(e >= 0) e = fprintf(stream, "#       X Y M\n");
  if(e >= 0) e = fprintf(stream, "# FSF type       %s\n",
                                                 stat_get_type_pointer (_ss));
  if(e >= 0) e = fprintf(stream, "# FSF nhx        %d\n", stat_get_nhx (_ss));
  if(e >= 0) e = fprintf(stream, "# FSF nhy        %d\n", stat_get_nhy (_ss));
  if(e >= 0) e = fprintf(stream, "# FSF nhx2       %d\n", stat_get_nhx2(_ss));
  if(e >= 0) e = fprintf(stream, "# FSF nhy2       %d\n", stat_get_nhy2(_ss));
  if(e >= 0) e = fprintf(stream, "# FSF x1         %g\n", x1                );
  if(e >= 0) e = fprintf(stream, "# FSF y1         %g\n", y1                );
  if(e >= 0) e = fprintf(stream, "# FSF xinc       %g\n", xinc              );
  if(e >= 0) e = fprintf(stream, "# FSF yinc       %g\n", yinc              );
  if(e >= 0) e = fprintf(stream, "# FSF nx         %d\n", nx                );
  if(e >= 0) e = fprintf(stream, "# FSF ny         %d\n", ny                );
  if(e >= 0) e = fprintf(stream, "# FSF ncards     %d\n", ncards + 1        );
  if(e >= 0) e = fprintf(stream, "# FSF nvalues    %d\n", nvalues           );
  if(e < 0) return 1;
  return 0;
}



//------------------- private save cards ----------------------------//
//------------------- private save cards ----------------------------//
//------------------- private save cards ----------------------------//


int StaticGenericIO::privateSaveCards (FILE *stream)
{
  int e = fprintf(stream, "# FSF begin history cards +++++++++++++\n");
  if(e < 0) return 1;
  long ncards = stat_get_ncards(_ss);
  for(int i = 0; i < ncards; i++)
      {
      char card[81];
      strncpy(card, stat_get_card_pointer(_ss, i+1), 80);
      card[79] = '\0';
      int e = fprintf(stream, "#%s\n", card);
      if(e < 0) return 1;
      }
  char card[81];
  sprintf(card, "Generic file saved by %s:  %s",
                  stat_get_program_pointer(_ss), stat_get_time_string());
  e = fprintf(stream, "#%s\n", card);
  if(e < 0) return 1;
  e = fprintf(stream, "# FSF end history cards +++++++++++++\n");
  if(e < 0) return 1;
  return 0;
}



//------------------- private save values ----------------------------//
//------------------- private save values ----------------------------//
//------------------- private save values ----------------------------//


int StaticGenericIO::privateSaveValues (FILE *stream)
{
  float        x1      = stat_get_x1     (_ss);
  float        y1      = stat_get_y1     (_ss);
  float        xinc    = stat_get_xinc   (_ss);
  float        yinc    = stat_get_yinc   (_ss);
  long         nx      = stat_get_nx     (_ss);
  long         ny      = stat_get_ny     (_ss);
  const float *pointer = stat_get_pointer(_ss);
  if(pointer)
      {
      int e = fprintf(stream, "#         XBIN          YBIN         VALUE\n");
      if(e < 0) return 1;
      for(long iy = 0; iy < ny; iy++)
          {
          for(long ix = 0; ix < nx; ix++)
              {
              float  xbin = x1 + ix * xinc;
              float  ybin = y1 + iy * yinc;
              long  index = ix + nx * iy;
              float value = pointer[index];
              int e = fprintf(stream, " %13g %13g %13g\n", xbin, ybin, value);
              if(e < 0) return 1;
              }
          }
      }
  int e = fprintf(stream, "#end of generic static file\n");
  if(e < 0) return 1;
  return 0;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

