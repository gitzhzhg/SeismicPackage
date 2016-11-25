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

//--------------------- history_cards.cc ----------------------------//
//--------------------- history_cards.cc ----------------------------//
//--------------------- history_cards.cc ----------------------------//

//         implementation file for the HistoryCards class
//                    not derived from any class
//                         subdirectory oprim


#include "oprim/history_cards.hh"
#include "oprim/pjar_wrapper.hh"
#include "str.h"
#include "readcard.h"
#include "named_constants.h"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


HistoryCards::HistoryCards(long maxcards, const char *progname)
           :
        _maxcards     (maxcards),
        _ncards       (0),
        _array        (NULL),
        _last         (-1),
        _needs_saving (TRUE),
        _progname     (NULL),
        _disabled     (FALSE)
{
  assert(_maxcards > 10);
  if(progname) _progname = str_newstr(progname);
  else         _progname = str_newstr(" ");
  privateAdd("(no history cards)");
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

HistoryCards::~HistoryCards()
{
  assert(_array);
  for(long index = 0; index < _ncards; index++)
      {
      free(_array[index]);
      }
  if(_progname) free(_progname);
  delete [] _array;
}



//---------------------- private real -------------------------------//
//---------------------- private real -------------------------------//
//---------------------- private real -------------------------------//

         // private.
         // given logical index, returns real index.

long HistoryCards::privateReal(long index)  const
{
  assert(index >= 0 && index < _ncards);
  long real = _last + 1 + index;
  if(real >= _ncards) real -= _ncards;
  assert(real >= 0 && real < _ncards);
  return real;
}



//------------------- private replace ---------------------------//
//------------------- private replace ---------------------------//
//------------------- private replace ---------------------------//

         // private.
         // replaces last card.

void HistoryCards::privateReplace(const char *card)
{
  long real = privateReal(_ncards-1);
  free(_array[real]);
  _array[real] = str_newstr(card);
}



//------------------------- private add ------------------------------//
//------------------------- private add ------------------------------//
//------------------------- private add ------------------------------//

         // private.
         // appends one card.

void HistoryCards::privateAdd(const char *card)
{
  assert(card);
  if(_ncards < _maxcards) privateReallocate();
  else                    privateRoll      ();
  _array[_last] = str_newstr(card);
}



//---------------------- private reallocate ---------------------------//
//---------------------- private reallocate ---------------------------//
//---------------------- private reallocate ---------------------------//

        // private.
        // reallocates the array to hold one more card pointer.
        // copies old array to new array.
        // does not set the new card pointer or reallocate the new card.
        // increments _ncards and _last.

void HistoryCards::privateReallocate()
{
  assert(_ncards < _maxcards);
  assert(_last == _ncards-1);
  char **newarray = new char * [_ncards+1];
  if(_array)
      {
      assert(_ncards > 0);
      memcpy(newarray, _array, (int)_ncards * sizeof(char*));
      delete [] _array;
      }
  _array = newarray;
  _ncards++;
  _last++;
}



//---------------------- private roll ---------------------------------//
//---------------------- private roll ---------------------------------//
//---------------------- private roll ---------------------------------//

        // private.
        // increments _last (in a cirular fashion).
        // frees the card now pointed to by the _last pointer.
        // does not reset the new card pointer or reallocate the new card.

void HistoryCards::privateRoll()
{
  assert(_ncards == _maxcards);
  _last++;
  if(_last == _ncards) _last = 0;
  free(_array[_last]);
}



//---------------------- get history card ------------------------------//
//---------------------- get history card ------------------------------//
//---------------------- get history card ------------------------------//

       // public.

const char *HistoryCards::getHistoryCard(long index)  const
{
  long real = privateReal(index);
  return _array[real];
}



//---------------------- add history card ----------------------------//
//---------------------- add history card ----------------------------//
//---------------------- add history card ----------------------------//

       // public.

void HistoryCards::addHistoryCard(const char *card)
{
  if(!card) return;
  if(str_trimmed_length(card) == 0) return;
  if(_disabled) return;
  if(_ncards == 1 &&
    strncmp(getHistoryCard(_ncards-1), "(no history cards)", 10) == 0)
      {
      privateReplace(card);
      }
  else if(_ncards >= 2 && strcmp(card, getHistoryCard(_ncards-1)) == 0 &&
              strcmp(card, getHistoryCard(_ncards-2)) == 0)
      {
      privateReplace("(previous card repeated 2 times)");
      }
  else if(_ncards >= 2 &&
    strncmp(getHistoryCard(_ncards-1), "(previous card repeated", 15) == 0 &&
    strcmp(card, getHistoryCard(_ncards-2)) == 0)
      {
      int kount;
      sscanf((char*)getHistoryCard(_ncards-1),
                               "(previous card repeated %d", &kount);
      char buffer[80];
      sprintf(buffer, "(previous card repeated %d times)", kount + 1);
      privateReplace(buffer);
      }
  else
      {
      privateAdd(card);
      }
  _needs_saving = TRUE;
}



//----------------------- delete history cards --------------------//
//----------------------- delete history cards --------------------//
//----------------------- delete history cards --------------------//

       // public.

void HistoryCards::deleteHistoryCards()
{
  assert(_array);
  for(long index = 0; index < _ncards; index++)
      {
      free(_array[index]);
      }
  delete [] _array;
  _ncards = 0;
  _array  = NULL;
  _last   = -1;
  privateAdd("(no history cards)");
  _needs_saving = TRUE;
}


//------------------------ copy history cards -----------------------------//
//------------------------ copy history cards -----------------------------//
//------------------------ copy history cards -----------------------------//

     // public.

void HistoryCards::copyHistoryCards (const HistoryCards *other)
{
  deleteHistoryCards();
  addHistoryCards   (other);
}



//------------------------ add history cards -----------------------------//
//------------------------ add history cards -----------------------------//
//------------------------ add history cards -----------------------------//

     // public.

void HistoryCards::addHistoryCards (const HistoryCards *other)
{
  int ncards = other->numHistoryCards();
  for(int indx = 0; indx < ncards; indx++)
      {
      const char *card = other->getHistoryCard(indx);
      addHistoryCard(card);
      }
}



//-------------------- get history cards from pjar --------------------------//
//-------------------- get history cards from pjar --------------------------//
//-------------------- get history cards from pjar --------------------------//

     // public.

void HistoryCards::getHistoryCardsFromPjar (PjarWrapper *pjar)
{
  deleteHistoryCards();
  pjar->chooseSection("history");
  int ncards = pjar->numCards();
  for(int indx = 0; indx < ncards; indx++)
      {
      char card[111];
      pjar->getCard(indx, card);
      addHistoryCard(card);
      }
}



//------------------------ put history cards into pjar ---------------------//
//------------------------ put history cards into pjar ---------------------//
//------------------------ put history cards into pjar ---------------------//

     // public.

void HistoryCards::putHistoryCardsIntoPjar
                                  (PjarWrapper *pjar, int skiphist)  const
{
  long  ncards = numHistoryCards();
  if(ncards == 1 &&
    strncmp(getHistoryCard(ncards-1), "(no history cards)", 10) == 0) ncards--;
  pjar->chooseSection("history");
  pjar->clearCards();
  for(int indx = 0; indx < ncards; indx++)
      {
      const char *card = getHistoryCard(indx);
      pjar->addCard(card);
      }
  if(!skiphist) pjar->newCard(_progname);
}



//-------------------------- read history cards ------------------------//
//-------------------------- read history cards ------------------------//
//-------------------------- read history cards ------------------------//

     // public.

int HistoryCards::readHistoryCards(const char *filename, char *msg)
{
  assert(filename && msg);
  deleteHistoryCards();
  _needs_saving = FALSE;
  long index;

  if(filename[0] == '\0')
      {
      strcpy(msg, "blank history input filename");
      return TRUE;
      }
  FILE *stream = fopen(filename, "r");
  if(stream == NULL)
      {
      strcpy(msg, "unable to open the history file for read");
      return TRUE;
      }
  for(index = 0; TRUE; index++)
      {
      const char *card = readcard(stream);
      if(card)
          {
          if(index == 0) privateReplace(card);
          else           privateAdd(card);
          }
      else if(feof(stream))
          {
          break;
          }
      else if(ferror(stream))
          {
          sprintf(msg, "history file read error on card %d", index+1);
          fclose(stream);
          return TRUE;
          }
      else
          {
          sprintf(msg, "bad card %d on history file", index+1);
          fclose(stream);
          return TRUE;
          }
      }
  sprintf(msg, "%d history cards read from history file", index);
  fclose(stream);
  return FALSE;
}



//-------------------------- save history cards ----------------------//
//-------------------------- save history cards ----------------------//
//-------------------------- save history cards ----------------------//

     // public.

int HistoryCards::saveHistoryCardsIfNeeded
                         (const char *filename, char *msg)
{
  assert(filename && msg);
  if(!_needs_saving)
      {
      strcpy(msg, "history cards do not need saving");
      return FALSE;
      }
  return saveHistoryCards(filename, msg);
}



int HistoryCards::saveHistoryCards(const char *filename, char *msg)
{
  assert(filename && msg);
  _needs_saving = FALSE;
  if(filename[0] == '\0')
      {
      strcpy(msg, "blank history output filename");
      return TRUE;
      }
  FILE *stream = fopen(filename, "w");
  if(stream == NULL)
      {
      strcpy(msg, "unable to open the history file for write");
      return TRUE;
      }
  for(long index = 0; index < _ncards; index++)
      {
      int err = fprintf(stream, "%s\n", getHistoryCard(index));
      if(err <= 0)
          {
          sprintf(msg, "history file write error on card %d", index+1);
          fclose(stream);
          return TRUE;
          }
      }
  sprintf(msg, "%d card images successfully written to history file", _ncards);
  fclose(stream);
  return FALSE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

