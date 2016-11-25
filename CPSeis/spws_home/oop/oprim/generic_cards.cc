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

//-------------------------- generic_cards.cc -------------------------//
//-------------------------- generic_cards.cc -------------------------//
//-------------------------- generic_cards.cc -------------------------//

//           implementation file for the GenericCards class
//                    not derived from any class
//                        subdirectory oprim


#include "oprim/generic_cards.hh"
#include "oprim/void_array.hh"
#include "cprim.h"
#include "readcard.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>
#include <ctype.h>


//----------------------------- constructor ----------------------------//
//----------------------------- constructor ----------------------------//
//----------------------------- constructor ----------------------------//


GenericCards::GenericCards()
               :
                 _array           (NULL),
                 _nread           (0)
{
  _array = new VoidArray();
}



//------------------------------ destructor -----------------------------//
//------------------------------ destructor -----------------------------//
//------------------------------ destructor -----------------------------//


GenericCards::~GenericCards()
{
  deleteAllCards();
  delete _array;
}



//--------------------------- get values ---------------------------------//
//--------------------------- get values ---------------------------------//
//--------------------------- get values ---------------------------------//

   // public.

long GenericCards::numCards()  const
{
  return _array->numElements();
}


const char *GenericCards::getCard(long index)  const
{
  static const char *blank = " ";
  const char *card = (const char*)_array->element(index);
  if(card) return card;
  return blank;
}



//-------------------------- private free card ---------------------------//
//-------------------------- private free card ---------------------------//
//-------------------------- private free card ---------------------------//

       // private.

void GenericCards::privateFreeCard(long index)
{
  char *card = (char*)_array->element(index);
  if(card) free(card);
  _array->replaceElement(index, NULL);
}



//---------------------------- set values ----------------------------------//
//---------------------------- set values ----------------------------------//
//---------------------------- set values ----------------------------------//

       // public.

void GenericCards::createBlankCards(long num)
{
  deleteAllCards();
  _array->createNullElements(num);
}



void GenericCards::replaceCard(long index, const char *card)
{
  assert(card);
  privateFreeCard(index);
  char *card2 = newstr(card);
  _array->replaceElement(index, card2);
}



long GenericCards::appendCard(const char *card)
{
  assert(card);
  char *card2 = newstr(card);
  return _array->appendElement(card2);
}



void GenericCards::insertCard(long index, const char *card)
{
  assert(card);
  char *card2 = newstr(card);
  _array->insertElement(index, card2);
}



void GenericCards::deleteCard(long index)
{
  privateFreeCard(index);
  _array->removeElement(index);
}



void GenericCards::deleteAllCards()
{
  long num = numCards();
  for(long index = 0; index < num; index++)
      {
      privateFreeCard(index);
      }
  _array->clearElements();
  _nread = 0;
}



//-------------------------- private read cards ----------------------------//
//-------------------------- private read cards ----------------------------//
//-------------------------- private read cards ----------------------------//

     // private.
     // correct number of cards (num) have already been created.
     // _nread is set to number of cards actually read.
     // _nread will not exceed num.

int GenericCards::privateReadCards (const char *filename, long num, char *msg)
{
  assert(filename && msg);
  _nread = 0;
  if(filename[0] == '\0')
      {
      strcpy(msg, "no file specified");
      return TRUE;
      }
  FILE *stream = fopen(filename, "r");
  if(stream == NULL)
      {
      strcpy(msg, "file not found");
      return TRUE;
      }
  for(int index = 0; index < num; index++)
      {
      const char *card = readcard(stream);
      if(card)
          {
          int len = strlen(card);
          for(int i = 0; i < len; i++)
              {
              if(isspace(card[i])) ((char*)card)[i] = ' ';
              }
          replaceCard(index, card);
          _nread++;
          }
      else if(feof(stream))
          {
          sprintf(msg, "file contains %d card images", index);
          fclose(stream);
          return FALSE;
          }
      else if(ferror(stream))
          {
          sprintf(msg, "read error on card image %d on the file", index+1);
          fclose(stream);
          return TRUE;
          }
      else
          {
          if(index == 0) strcpy (msg, "binary file");
          else           sprintf(msg,
        "card image %d too long or contains nulls - probably binary data",
                                                           index+1);
          fclose(stream);
          return TRUE;
          }
      }
  sprintf(msg, "%d card images successfully read from beginning of file",
                                                   (int)num);
  fclose(stream);
  return FALSE;
}



//-------------------------- read cards from file --------------------------//
//-------------------------- read cards from file --------------------------//
//-------------------------- read cards from file --------------------------//

     // public virtual function.

int GenericCards::readCardsFromFile(const char *filename, long num, char *msg)
{
  assert(filename);
  assert(num > 0);
  if(msg)
      {
      createBlankCards(num);
      return privateReadCards(filename, num, msg);
      }
  createBlankCards(num+1);
  char msg2[222];
  int error = privateReadCards(filename, num, msg2);
  char buffer[222];
  sprintf(buffer, "(%s)", msg2);
  replaceCard(_nread, buffer);
  _nread++;
  return error;
}



//---------------------------------- end -----------------------------------//
//---------------------------------- end -----------------------------------//
//---------------------------------- end -----------------------------------//

