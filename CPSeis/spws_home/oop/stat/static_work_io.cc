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

//--------------------- static_work_io.cc ---------------------//
//--------------------- static_work_io.cc ---------------------//
//--------------------- static_work_io.cc ---------------------//

//         implementation file for the StaticWorkIO class
//                  not derived from any class
//                      subdirectory stat


#include "stat/static_work_io.hh"
#include "cprim.h"
#include "inquire.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


#define LENV    20
#define LENT    10
#define PHRASE  "static workfile"


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


StaticWorkIO::StaticWorkIO(StatStruct *ss)
           :
          _ss           (ss)
{
  assert(_ss);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

StaticWorkIO::~StaticWorkIO()
{
}



//------------------------- save file --------------------------------//
//------------------------- save file --------------------------------//
//------------------------- save file --------------------------------//

        // adds one history card to saved file if add is TRUE.

int  StaticWorkIO::saveFile (const char *filename, char *msg, int add)
{
  if(filename[0] == '\0')
      {
      strcpy(msg, "blank filename when trying to save workfile");
      return 1;
      }
  FILE *stream = fopen(filename, "wb");
  if(stream == NULL)
      {
      strcpy(msg, "open error when trying to save workfile");
      return 1;
      }
  int error = privateSaveHeader(stream, add);
  if(error)
      {
      strcpy(msg, "error trying to save workfile header");
      fclose(stream);
      return 1;
      }
  error = privateSaveCards(stream, add);
  if(error)
      {
      strcpy(msg, "error trying to save card to workfile");
      fclose(stream);
      return 1;
      }
  error = privateSaveValues(stream);
  if(error)
      {
      strcpy(msg, "error trying to save values to workfile");
      fclose(stream);
      return 1;
      }
  fclose(stream);
  strcpy(msg, "successfully saved static workfile");
  return 0;
}



//------------------------- read file --------------------------------//
//------------------------- read file --------------------------------//
//------------------------- read file --------------------------------//

        // adds one history card to memory if add is TRUE.

int  StaticWorkIO::readFile   (const char *filename, char *msg, int add)
{
  if(filename[0] == '\0')
      {
      strcpy(msg, "blank filename when trying to read workfile");
      return 1;
      }
  FILE *stream = fopen(filename, "rb");
  if(stream == NULL)
      {
      strcpy(msg, "open error when trying to read workfile");
      return 1;
      }
  long ncards;
  int  values;
  int error = privateReadHeader(stream, &ncards, &values);
  if(error)
      {
      strcpy(msg, "error trying to read workfile header");
      fclose(stream);
      return 1;
      }
  error = privateReadCards(stream, ncards, add);
  if(error)
      {
      strcpy(msg, "error trying to read card from workfile");
      fclose(stream);
      return 1;
      }
  error = privateReadValues(stream, values);
  if(error)
      {
      strcpy(msg, "error trying to read values from workfile");
      fclose(stream);
      return 1;
      }
  fclose(stream);
  strcpy(msg, "successfully read static workfile");
  return 0;
}



//----------------------- validate file ------------------------------//
//----------------------- validate file ------------------------------//
//----------------------- validate file ------------------------------//


long StaticWorkIO::validateFile  (const char *filename, char *info)
{
  if(filename[0] == '\0')
      {
      stat_bad_validation(_ss);
      return INQUIRE_VALID_NO;
      }
  FILE *stream = fopen(filename, "rb");
  if(stream == NULL)
      {
      stat_bad_validation(_ss);
      return INQUIRE_VALID_NO;
      }
  long ncards;
  int  values;
  int error = privateReadHeader(stream, &ncards, &values);
  if(error)
      {
      stat_bad_validation(_ss);
      fclose(stream);
      return INQUIRE_VALID_NO;
      }
  for(int i = 0; i < ncards; i++)
      {
      stat_add_card(_ss, "junk");
      }
  fclose(stream);
  long nhx = stat_get_nhx(_ss);
  long nhy = stat_get_nhy(_ss);
  long nx  = stat_get_nx (_ss);
  long ny  = stat_get_ny (_ss);
  sprintf(info, "x and y headers %d %d  --  x and y bins %d %d",
                                 nhx, nhy, nx, ny);
  return INQUIRE_VALID_YES;
}



//----------------------- private save header --------------------------//
//----------------------- private save header --------------------------//
//----------------------- private save header --------------------------//


int  StaticWorkIO::privateSaveHeader(FILE *stream, int add)
{
  char verify[LENV];
  char type  [LENT];
  memset (verify, '\0', LENV);
  memset (type  , '\0', LENT);
  strncpy(type  , stat_get_type_pointer(_ss), LENT - 1);
  strncpy(verify, PHRASE                    , LENV - 1);
  long         nhx     = stat_get_nhx    (_ss);
  long         nhy     = stat_get_nhy    (_ss);
  long         nhx2    = stat_get_nhx2   (_ss);
  long         nhy2    = stat_get_nhy2   (_ss);
  float        x1      = stat_get_x1     (_ss);
  float        y1      = stat_get_y1     (_ss);
  float        xinc    = stat_get_xinc   (_ss);
  float        yinc    = stat_get_yinc   (_ss);
  long         nx      = stat_get_nx     (_ss);
  long         ny      = stat_get_ny     (_ss);
  long         ncards  = stat_get_ncards (_ss);
  const float *pointer = stat_get_pointer(_ss);
  int    values  = (pointer != NULL);
  if(add) ncards++;
  size_t nsaved = 1;
  if(nsaved == 1) nsaved = fwrite( verify, sizeof verify, 1, stream);
  if(nsaved == 1) nsaved = fwrite( type  , sizeof type  , 1, stream);
  if(nsaved == 1) nsaved = fwrite(&nhx   , sizeof(long ), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&nhy   , sizeof(long ), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&nhx2  , sizeof(long ), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&nhy2  , sizeof(long ), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&x1    , sizeof(float), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&y1    , sizeof(float), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&xinc  , sizeof(float), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&yinc  , sizeof(float), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&nx    , sizeof(long ), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&ny    , sizeof(long ), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&ncards, sizeof(long ), 1, stream);
  if(nsaved == 1) nsaved = fwrite(&values, sizeof(int  ), 1, stream);
  int error = (nsaved != 1);
  return error;
}



//----------------------- private read header --------------------------//
//----------------------- private read header --------------------------//
//----------------------- private read header --------------------------//


int StaticWorkIO::privateReadHeader
                          (FILE *stream, long *ncards, int *values)
{
  char verify[LENV];
  char type  [LENT];
  long   nhx    ;
  long   nhy    ;
  long   nhx2   ;
  long   nhy2   ;
  float  x1     ;
  float  y1     ;
  float  xinc   ;
  float  yinc   ;
  long   nx     ;
  long   ny     ;
  size_t nread = 1;
  if(nread == 1) nread = fread( verify, sizeof verify, 1, stream);
  if(nread == 1) nread = fread( type  , sizeof type  , 1, stream);
  if(nread == 1) nread = fread(&nhx   , sizeof(long ), 1, stream);
  if(nread == 1) nread = fread(&nhy   , sizeof(long ), 1, stream);
  if(nread == 1) nread = fread(&nhx2  , sizeof(long ), 1, stream);
  if(nread == 1) nread = fread(&nhy2  , sizeof(long ), 1, stream);
  if(nread == 1) nread = fread(&x1    , sizeof(float), 1, stream);
  if(nread == 1) nread = fread(&y1    , sizeof(float), 1, stream);
  if(nread == 1) nread = fread(&xinc  , sizeof(float), 1, stream);
  if(nread == 1) nread = fread(&yinc  , sizeof(float), 1, stream);
  if(nread == 1) nread = fread(&nx    , sizeof(long ), 1, stream);
  if(nread == 1) nread = fread(&ny    , sizeof(long ), 1, stream);
  if(nread == 1) nread = fread( ncards, sizeof(long ), 1, stream);
  if(nread == 1) nread = fread( values, sizeof(int  ), 1, stream);
  int error = (nread != 1);
  if(!error)
      {
      if(verify[LENV - 1] != '\0')                error = TRUE;
      if(type  [LENT - 1] != '\0')                error = TRUE;
      if(strncmp(verify, PHRASE, LENV - 1))       error = TRUE;
      if(nhx  < 1)                                error = TRUE;
      if(nhy  < 0)                                error = TRUE;
      if(nhx2 < 0)                                error = TRUE;
      if(nhy2 < 0)                                error = TRUE;
      if(xinc  == 0.0 || yinc == 0.0)             error = TRUE;
      if(nx  < 1 || nx  > 999999)                 error = TRUE;
      if(ny  < 1 || ny  > 999999)                 error = TRUE;
      if(nx * ny  > 9999999)                      error = TRUE;
      if(*ncards  < 0 || *ncards  > 9999)         error = TRUE;
      }
  stat_free_pointer(_ss);
  stat_remove_cards(_ss);
  if(error)
      {
      *ncards = 0;
      *values = FALSE;
      return error;
      }
  stat_set_type(_ss, type);
  stat_set_nhx (_ss, nhx);
  stat_set_nhy (_ss, nhy);
  stat_set_nhx2(_ss, nhx2);
  stat_set_nhy2(_ss, nhy2);
  stat_set_x1  (_ss, x1);
  stat_set_y1  (_ss, y1);
  stat_set_xinc(_ss, xinc);
  stat_set_yinc(_ss, yinc);
  stat_set_nx  (_ss, nx);
  stat_set_ny  (_ss, ny);
  return error;
}



//----------------------- private save cards --------------------------//
//----------------------- private save cards --------------------------//
//----------------------- private save cards --------------------------//


int  StaticWorkIO::privateSaveCards (FILE *stream, int add)
{
  long ncards = stat_get_ncards(_ss);
  for(int i = 0; i < ncards; i++)
      {
      char card[81];
      strncpy(card, stat_get_card_pointer(_ss, i+1), 80);
      card[80] = '\0';
      size_t nsaved = fwrite(card, sizeof card, 1, stream);
      if(nsaved != 1) return 1;
      }
  if(add)
      {
      char card[81];
      sprintf(card, "Workfile saved by %s:  %s",
                   stat_get_program_pointer(_ss), stat_get_time_string());
      size_t nsaved = fwrite(card, sizeof card, 1, stream);
      if(nsaved != 1) return 1;
      }
  return 0;
}



//----------------------- private read cards -------------------------//
//----------------------- private read cards -------------------------//
//----------------------- private read cards -------------------------//


int StaticWorkIO::privateReadCards(FILE *stream, long ncards, int add)
{
  for(int i = 0; i < ncards; i++)
      {
      char card[81];
      size_t nread = fread(card, sizeof card, 1, stream);
      if(nread != 1) return 1;
      stat_add_card(_ss, card);
      }
  if(add)
      {
      char card[81];
      sprintf(card, "Workfile read  by %s:  %s",
                   stat_get_program_pointer(_ss), stat_get_time_string());
      stat_add_card(_ss, card);
      }
  return 0;
}



//----------------------- private save values --------------------------//
//----------------------- private save values --------------------------//
//----------------------- private save values --------------------------//


int StaticWorkIO::privateSaveValues(FILE *stream)
{
  const float *pointer = stat_get_pointer(_ss);
  if(pointer)
      {
      long nx = stat_get_nx(_ss);
      long ny = stat_get_ny(_ss);
      unsigned int number = (unsigned int)(nx * ny);
      size_t nsaved = fwrite(pointer, sizeof(float), number, stream);
      if(nsaved != number) return 1;
      }
  return 0;
}



//----------------------- private read values -------------------------//
//----------------------- private read values -------------------------//
//----------------------- private read values -------------------------//


int StaticWorkIO::privateReadValues(FILE *stream, int values)
{
  if(values)
      {
      float *pointer = stat_get_pointer(_ss);
      if(pointer == NULL)
          {
          stat_clear_values(_ss);
          pointer = stat_get_pointer(_ss);
          }
      assert(pointer);
      long nx = stat_get_nx(_ss);
      long ny = stat_get_ny(_ss);
      unsigned int number = (unsigned int)(nx * ny);
      size_t nread = fread(pointer, sizeof(float), number, stream);
      if(nread != number) return 1;
      }
  return 0;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

