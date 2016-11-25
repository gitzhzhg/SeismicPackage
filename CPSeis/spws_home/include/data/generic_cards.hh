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

//------------------------ generic_cards.hh -----------------------//
//------------------------ generic_cards.hh -----------------------//
//------------------------ generic_cards.hh -----------------------//

//                header file for the GenericCards class
//                    not derived from any class
//                         subdirectory oprim

     // This class maintains an array of card images.
     // This class owns the array and the cards in the array.
     // This class can read cards from a text file if desired.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _GENERIC_CARDS_HH_
#define _GENERIC_CARDS_HH_

#include <stdio.h>


class GenericCards
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  class VoidArray   *_array;     // owned by this class.
  int                _nread;     // number of cards read from file.


//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:

           GenericCards ();
  virtual ~GenericCards ();

public:  // get values.

  long        numCardsRead ()            const  { return _nread; }
  long        numCards     ()            const;
  const char *getCard      (long index)  const;

public:  // set values.
         // appendCard returns index of new card.

  void  createBlankCards (long num);
  void  replaceCard      (long index, const char *card);
  long  appendCard                   (const char *card);
  void  insertCard       (long index, const char *card);
  void  deleteCard       (long index);
  void  deleteAllCards   ();

public:  // read cards from a text file.
         // num is the maximum number of cards to read.
         // if the file contains fewer cards, the excess will be blank.
         // sets msg and returns read error TRUE or FALSE.
         // if msg is NULL, sets a card to msg after the last card read.
         // can be optionally replaced in a derived class.

  virtual int readCardsFromFile (const char *filename, long num = 80,
                                 char *msg = 0);

private:

  void  privateFreeCard  (long index);
  int   privateReadCards (const char *filename, long num, char *msg);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
