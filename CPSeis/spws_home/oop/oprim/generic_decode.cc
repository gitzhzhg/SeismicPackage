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

//--------------------- generic_decode.cc ---------------------//
//--------------------- generic_decode.cc ---------------------//
//--------------------- generic_decode.cc ---------------------//

//         implementation file for the GenericDecode class
//                  not derived from any class
//                      subdirectory oprim



#include "oprim/generic_decode.hh"
#include "cprim.h"
#include "str.h"
#include "named_constants.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>
#include <ctype.h>


#define NMAX  60
#define NBUF  222


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


GenericDecode::GenericDecode(const char *how,
                             const char *required,
                             const char *optional)
           :
          _how          (newstr("")),
          _required     (newstr(required)),
          _optional     (newstr(optional)),
          _allowed      (NULL),
          _bad          (newstr("unset code")),
          _orientation  (BAD_CODE),
          _num_fields   (0),
          _num_cards    (1),
          _num_received (0),
          _index1       (new int  [NMAX]),
          _nchar        (new int  [NMAX]),
          _letter       (new char [NMAX]),
          _card         (newstr("")),
          _length       (0),
          _decode_error (FALSE),
          _reason       (REASON_UNSET)
{
  assert(how && required && optional);
  constructorHelper (how);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

GenericDecode::~GenericDecode()
{
  free(_how);
  free(_required);
  free(_optional);
  free(_allowed);
  free(_bad);
  delete [] _index1;
  delete [] _nchar;
  delete [] _letter;
  free(_card);
}

void GenericDecode::constructorHelper (const char *how)
{
/*
  remove_all_blanks     (_required, _required);
  convert_case_to_upper (_required, _required);
  remove_all_blanks     (_optional, _optional);
  convert_case_to_upper (_optional, _optional);
*/
  str_remove_all_blanks (_required, _required);
  str_to_upper          (_required, _required);
  str_remove_all_blanks (_optional, _optional);
  str_to_upper          (_optional, _optional);
  _allowed = newstrcat(_required, " -/", _optional, NULL);
  howDecode(how);
}

void GenericDecode::reset (const char *how,
                           const char *required,
                           const char *optional)
{
  assert(how && required && optional);
  free(_how);
  free(_required);
  free(_optional);
  free(_allowed);
  _how      = newstr("");
  _required = newstr(required);
  _optional = newstr(optional);
  _allowed  = NULL;
  constructorHelper (how);
}


//------------------------ how decode ------------------------------//
//------------------------ how decode ------------------------------//
//------------------------ how decode ------------------------------//

   // public.
   // if first character is #, replaces it with the second character.

void GenericDecode::howDecode(const char *how)
{
/////////////////////////// grab new code:

  assert(how);
  free(_how);
  _how = newstr(how);
/*
  remove_trailing_blanks(_how, _how);
  convert_case_to_upper (_how, _how);
*/
  str_remove_trailing_blanks(_how, _how);
  str_to_upper              (_how, _how);

/////////////////////////// replace first character if it is #:

  if(_how[0] == '#')
      {
      if(strlen(_how) >= 2) _how[0] = _how[1];
      else                  _how[0] = '\0';
      }

/////////////////////////// setup general information needed below:

  int i, j;
  int len_how = strlen(_how);
  int len_required = strlen(_required);
  int len_optional = strlen(_optional);
  int len_allowed  = strlen(_allowed);
  char buffer[NBUF];
/*
  char *letterstring = " ";
*/
  char letterstring[2] = " ";

  free(_bad);
  _bad = newstr("");

  _decode_error = FALSE;
  _reason = REASON_UNSET;

/////////////////////////// learn whether dash and slash exist:

  int dash_exists = FALSE;
  _num_cards = 1;
  for(j = 0; j < len_how; j++)
      {
      if(how[j] == '-') dash_exists = TRUE;
      if(how[j] == '/') _num_cards++;
      }
  _num_received = 0;

/////////////////////////// determine orientation:

  _orientation = ORDER_ORIENTED;
  for(j = 0; j < len_how; j++)
      {
      int letter = _how[j];
      if(letter == ' ' || letter == '-' || letter == '/') continue;
      if(j > 0 && letter == _how[j-1]) _orientation = COLUMN_ORIENTED;
      }
  if(dash_exists && _orientation == COLUMN_ORIENTED)
      {
      _orientation = BAD_CODE;
      free(_bad);
      _bad = newstr("dash not allowed when column-oriented");
      return;
      }
  if(_num_cards > 1 && _orientation == COLUMN_ORIENTED)
      {
      _orientation = BAD_CODE;
      free(_bad);
      _bad = newstr("slash not allowed when column-oriented");
      return;
      }

/////////////////////////// check for required letters:

  strcpy(buffer, "required letters not used: ");
  for(i = 0; i < len_required; i++)
      {
      int letter = _required[i];
      int ok = FALSE;
      for(j = 0; j < len_how; j++)
          {
          if(letter == _how[j]) ok = TRUE;
          }
      if(!ok)
          {
          _orientation = BAD_CODE;
          letterstring[0] = letter;
          strcat(buffer, letterstring);
          }
      }
  if(_orientation == BAD_CODE)
      {
      free(_bad);
      _bad = newstr(buffer);
      return;
      }

/////////////////////////// check for invalid letters:

  letterstring[0] = ' ';
  strcpy(buffer, "invalid letters used: ");
  for(j = 0; j < len_how; j++)
      {
      int letter = _how[j];
      int ok = FALSE;
      for(i = 0; i < len_allowed; i++)
          {
          if(letter == _allowed[i]) ok = TRUE;
          }
      if(!ok)
          {
          _orientation = BAD_CODE;
          if(letter != letterstring[0])
              {
              letterstring[0] = letter;
              strcat(buffer, letterstring);
              }
          }
      }
  if(_orientation == BAD_CODE)
      {
      free(_bad);
      _bad = newstr(buffer);
      return;
      }

/////////////////////////// get arrays _index1, _nchar, and _letter:

  _num_fields = 0;

  if(_orientation == ORDER_ORIENTED)
      {
      for(int i = 0; i < len_how; i++)
          {
          if(_how[i] == ' ' || _how[i] == '/') continue;
          _index1[_num_fields] = i;
          _nchar [_num_fields] = 0;
          _letter[_num_fields] = _how[i];
          _num_fields++;
          assert(_num_fields < NMAX);
          }
      }

  else   // if(_orientation == COLUMN_ORIENTED)
      {
      int between = FALSE;
      for(int i = 0; i < len_how; i++)
          {
          assert(_how[i] != '-' && _how[i] != '/');
          if(between)
              {
              if(_how[i] != _how[i-1]) between = FALSE;
              }
          if(_how[i] == ' ') continue;
          if(!between)
              {
              between = TRUE;
              _index1[_num_fields] = i;
              _nchar [_num_fields] = 0;
              _letter[_num_fields] = _how[i];
              _num_fields++;
              assert(_num_fields < NMAX);
              }
          _nchar[_num_fields-1]++;
          }
      }

  if(_num_fields == 0)
      {
      _orientation = BAD_CODE;
      free(_bad);
      _bad = newstr("empty code");
      return;
      }
/****
         printf("num fields = %d\n", _num_fields);
         for(i = 0; i < _num_fields; i++)
             {
             printf("i=%d  index1=%d  nchar=%d  letter=%c\n",
                              i, _index1[i], _nchar[i], _letter[i]);
             }
****/

/////////////////////////// check for unique letters:
/////////////////////////// (it is OK for '-' to be used more than once)

  for(int ifield = 0; ifield < _num_fields; ifield++)
      {
      int letter = _letter[ifield];
      if(letter == '-') continue;
      assert(letter != ' ' && letter != '/');
      for(int ifield2 = 0; ifield2 < _num_fields; ifield2++)
          {
          if(ifield2 == ifield) continue;
          if(letter == _letter[ifield2]) _orientation = BAD_CODE;
          }
      }
  if(_orientation == BAD_CODE)
      {
      free(_bad);
      _bad = newstr("same letter used for more than one field");
      return;
      }

/////////////////////////// the code is good:

      free(_bad);
      _bad = newstr("");
}



//---------------------------- has code --------------------------//
//---------------------------- has code --------------------------//
//---------------------------- has code --------------------------//

       // public.

int GenericDecode::hasCode(char letter)  const
{
  for(int ifield = 0; ifield < _num_fields; ifield++)
      {
      if(letter == _letter[ifield]) return TRUE;
      }
  return FALSE;
}



//----------------------------- decode card ------------------------//
//----------------------------- decode card ------------------------//
//----------------------------- decode card ------------------------//

       // public.

int GenericDecode::decodeCard(const char *card)
{
  assert(_orientation != BAD_CODE);
  assert(_num_received <= _num_cards);
  assert(card);
  if(_decode_error && _num_cards > 1)  // drop one card and add new card.
      {
      assert(_num_received == _num_cards);
      char *oldcard = _card;
      char *first_vtab = strchr(oldcard, '\v');
      assert(first_vtab);
      _card = newstrcat(&first_vtab[1], "\v", card, NULL);
      free(oldcard);
      }
  else if(_num_received == _num_cards || _num_received == 0)  // start new card.
      {
      free(_card);
      _card = newstr(card);
      _num_received = 1;
      }
  else                             // append new card.
      {
      char *oldcard = _card;
      _card = newstrcat(oldcard, "\v", card, NULL);
      free(oldcard);
      _num_received++;
      }
  _decode_error = FALSE;
  _reason = REASON_UNSET;
  _length = strlen(_card);
  if(_num_received < _num_cards) return FALSE;
  return privateDecode();
}



//------------------------- private decode ----------------------------//
//------------------------- private decode ----------------------------//
//------------------------- private decode ----------------------------//

       // private.

int GenericDecode::privateDecode()
{
/****
printf("%s\n", _card);
****/
  if(_orientation == ORDER_ORIENTED)
      {
      int kount = 0;
      int between = FALSE;
      for(int i = 0; i < _length; i++)
          {
          if(between && isspace(_card[i])) between = FALSE;
          if(isspace(_card[i])) continue;
          if(!between)
              {
              between = TRUE;
              if(kount < _num_fields)
                  {
                  _index1[kount] = i;
                  _nchar [kount] = 0;
                  }
              kount++;
              }
          if(kount <= _num_fields) _nchar[kount-1]++;
          }
      if(kount < _num_fields)
          {
          _decode_error = TRUE;
          return TRUE;
          }
      }

  for(int ifield = 0; ifield < _num_fields; ifield++)
      {
      int index1 = _index1[ifield];
      int nchar  = _nchar [ifield];
      nchar = MinimumValue(nchar, _length - index1);
      if(nchar <= 0)
          {
          _decode_error = TRUE;
          return TRUE;
          }
      }
  _decode_error = FALSE;
  return FALSE;
}



//------------------------- get field -------------------------------//
//------------------------- get field -------------------------------//
//------------------------- get field -------------------------------//

     // private.
     // sets _reason to REASON_VALID, REASON_INVALID, or REASON_NONMATCH.
     // resets _decode_error to TRUE if _reason is set to REASON_INVALID.
     // returns NULL for REASON_INVALID and REASON_NONMATCH.

const char *GenericDecode::getField(char letter)
{
  assert(_orientation  !=   BAD_CODE);
  assert(_num_received == _num_cards);
  letter = toupper(letter);
  static char field[NBUF];
  for(int ifield = 0; ifield < _num_fields; ifield++)
      {
      if(letter == _letter[ifield])
          {
          int index1 = _index1[ifield];
          int nchar  = _nchar [ifield];
          nchar = MinimumValue(nchar, _length - index1);
          if(nchar <= 0)
              {
              _decode_error = TRUE;
              _reason = REASON_INVALID;
              return NULL;
              }
          memcpy(field, &_card[index1], nchar);
          field[nchar] = '\0';
          _reason = REASON_VALID;
          return field;
          }
      }
  _reason = REASON_NONMATCH;   // this is not considered a decode error.
  return NULL;
}



//----------------------- get decoded value ----------------------//
//----------------------- get decoded value ----------------------//
//----------------------- get decoded value ----------------------//

     // public.

long GenericDecode::getLongValue(char letter)
{
  const char *field = getField(letter);
  if(field == NULL) return INIL;
/****
  return atol(field);
****/
  int value;
  int istat;
/*
  convert_ss2ii_simple((char*)field, &value, &istat);
*/
  str_ss2ii_simple((char*)field, &value, &istat);
  if(istat <= 0)
      {
      _decode_error = TRUE;
      _reason = REASON_INVALID;
      return INIL;
      }
  return value;
/********
  long value;
  int num = sscanf((char*)field, "%ld", &value);
  if(num <= 0) return INIL;
  return value;
********/
}



float GenericDecode::getFloatValue(char letter)
{
  const char *field = getField(letter);
  if(field == NULL) return FNIL;
/****
  return (float)atof(field);
****/
  float value;
  int istat;
/*
  convert_ss2ff_simple((char*)field, &value, &istat);
*/
  str_ss2ff_simple((char*)field, &value, &istat);
  if(istat <= 0)
      {
      _decode_error = TRUE;
      _reason = REASON_INVALID;
      return FNIL;
      }
  return value;
/********
  float value;
  int num = sscanf((char*)field, "%f", &value);
  if(num <= 0) return FNIL;
  return value;
********/
}



double GenericDecode::getDoubleValue(char letter)
{
  const char *field = getField(letter);
  if(field == NULL) return DNIL;
/****
  return atof(field);
****/
  double value;
  int istat;
/*
  convert_ss2dd_simple((char*)field, &value, &istat);
*/
  str_ss2dd_simple((char*)field, &value, &istat);
  if(istat <= 0)
      {
      _decode_error = TRUE;
      _reason = REASON_INVALID;
      return DNIL;
      }
  return value;
/********
  double value;
  int num = sscanf((char*)field, "%lf", &value);
  if(num <= 0) return DNIL;
  return value;
********/
}



const char *GenericDecode::getStringValue(char letter)
{
  return getField(letter);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

