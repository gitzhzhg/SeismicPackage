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

//------------------------ sl_starting_cards.hh ---------------------//
//------------------------ sl_starting_cards.hh ---------------------//
//------------------------ sl_starting_cards.hh ---------------------//

//            header file for the SLStartingCards class
//                derived from the SLDatabox class
//                         subdirectory sl

        // This class displays card images from the beginning of a
        // disk file.  This class also displays and updates the
        // "template" parameter for a foreign input file.  In the
        // debug mode, this class displays parameter datacards
        // instead of card images from the file.

 
#ifndef _SL_STARTING_CARDS_HH_
#define _SL_STARTING_CARDS_HH_

#include "sl/sl_databox.hh"


class SLStartingCards  :  public SLDatabox
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class FileSupportInterface *_interface;   // pointer to external object.
  class FileSupportInterface *_cards;       // pointer to external object.
  int                         _debug;       // true or false.
/********
  int                         _filter;      // true or false.
********/

    // _interface contains parameters for reading or writing the file.
    // _cards contains starting cards of validated input or output file.
    // _interface and _cards should be the same for input files.
    // _interface and _cards should be different for output files.


//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:

           SLStartingCards(SLDelay *slparent,
                           FileSupportInterface *interface,
                           FileSupportInterface *cards,
                           int nrows_init = -1);
  virtual ~SLStartingCards();

  FileSupportInterface *getInterface()  const     { return _interface; }
  FileSupportInterface *getCards    ()  const     { return _cards; }
  int                   isDebug     ()  const     { return _debug; }
  void                  setDebug    (int debug)   { _debug  = debug; }
/********
  int                   isFilter    ()  const     { return _filter; }
  void                  setFilter   (int filter)  { _filter = filter; }
********/

private:

  void makeHelper();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
