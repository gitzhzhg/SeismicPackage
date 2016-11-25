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

//-------------------------- history cards.hh ----------------------//
//-------------------------- history cards.hh ----------------------//
//-------------------------- history cards.hh ----------------------//

//               header file for the HistoryCards class
//                    not derived from any class
//                        subdirectory oprim

   // This class maintains an array of history cards which it owns.
   // History cards are always added to the end (appended).
   // The user cannot remove, modify, or rearrange the history cards,
   //   except to delete all history cards.
   // There is always at least one card, and never more than _maxcards
   //   cards.  Before the user adds a card, there will be one original
   //   card stating that there are no cards.  The first time the user
   //   adds a card, this original card will be replaced.
   // Instead of adding multiple history cards which happen to be
   //   identical, a single card containing a repeat count is added
   //   or updated instead.

   // if _ncards < _maxcards:
   //   an attempt to add a card will cause the array to be reallocated
   //     to hold all previous cards and the new card.

   // if _ncards == _maxcards:
   //   an attempt to add a card will cause the oldest card to be lost,
   //     and the number of cards will be unchanged.

   // if _ncards <= _maxcards:
   //   _last will be the same as the user-specified (logical) index.
   //   _last will be equal to _ncards-1.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _HISTORY_CARDS_HH_
#define _HISTORY_CARDS_HH_


class HistoryCards
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  const long _maxcards;     // maximum number of history cards.
  long       _ncards;       // number of history cards.
  char     **_array;        // array of history cards (NULL if none).
  long       _last;         // real index of last card in array.
  int        _needs_saving; // TRUE if history cards need saving.
  char      *_progname;     // name of program using these history cards.
  int        _disabled;     // TRUE if history cards additions are disabled.
 

//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:

              HistoryCards (long maxcards = 500, const char *progname = 0);
  virtual    ~HistoryCards ();

  long        maxNumHistoryCards  ()            const  { return _maxcards; }
  long        numHistoryCards     ()            const  { return _ncards; }
  const char *getHistoryCard      (long index)  const;

  void        addHistoryCard      (const char *card);
  void        deleteHistoryCards  ();

public:   // copy history cards from another instance of this class.

  void        copyHistoryCards    (const HistoryCards *other);
  void        addHistoryCards     (const HistoryCards *other);

public:   // disable or enable history card additions (initially enabled).

  void  disableHistoryCardAdditions() { _disabled = 1; }
  void   enableHistoryCardAdditions() { _disabled = 0; }

public:   // copy cards to or from a pickle jar.
          // these function access the "history" pickle jar section.
          // the put routine adds a card made from the _progname variable.
          // set SKIPHIST to TRUE to skip adding a last history card.

  void  getHistoryCardsFromPjar (class PjarWrapper *pjar);
  void  putHistoryCardsIntoPjar (class PjarWrapper *pjar,
                                 int skiphist = 0)  const;

public:   // copy cards to or from a text file.
          // sets msg and returns read/write error TRUE or FALSE.

  int  readHistoryCards          (const char *filename, char *msg);
  int  saveHistoryCards          (const char *filename, char *msg);
  int  saveHistoryCardsIfNeeded  (const char *filename, char *msg);

private:

  long        privateReal       (long index)  const;
  void        privateReplace    (const char *card);
  void        privateAdd        (const char *card);
  void        privateReallocate ();
  void        privateRoll       ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
