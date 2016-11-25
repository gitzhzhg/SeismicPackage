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

//------------------------ sl_history_cards.hh ---------------------//
//------------------------ sl_history_cards.hh ---------------------//
//------------------------ sl_history_cards.hh ---------------------//

//            header file for the SLHistoryCards class
//                derived from the SLDatabox class
//                         subdirectory sl

       // This class displays history cards stored in an instance
       // of the HistoryCards class.  The displayed HistoryCards
       // instance can be changed at any time and can be NULL.

 
#ifndef _SL_HISTORY_CARDS_HH_
#define _SL_HISTORY_CARDS_HH_

#include "sl/sl_databox.hh"


class SLHistoryCards  :  public SLDatabox
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class HistoryCards *_history; // current HistoryCards object being displayed.
  int                 _allow;   // whether to allow user to add cards manually.

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

public:

  SLHistoryCards (SLDelay *slparent, HistoryCards *history = 0, int allow = 0);
  virtual ~SLHistoryCards();

  void showLastHistoryCard();    // scrolls to last history card.

  void setHistory (HistoryCards *history)  { _history = history; }

  HistoryCards *getHistory()  const  { return _history; }
  int           getAllow  ()  const  { return _allow; }

private:

  virtual void makeHelper();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
