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

//------------------------ sl_card_table.hh --------------------------//
//------------------------ sl_card_table.hh --------------------------//
//------------------------ sl_card_table.hh --------------------------//

//              header file for the SLCardTable class
//                derived from the SLDatabox class
//                           subdirectory sl

 
        // This class is used for displaying a table of the card
        // images residing in the specified GenericCards class
        // or HistoryCards class.  A code string residing in the specified
        // GenericDecode class can be optionally displayed along with
        // the GenericCards class.

        // Example of title:   "first part of file" or "project history"
        // Example of message:
        //    "Type code for reading file (press Help for instructions):"


#ifndef _SL_CARD_TABLE_HH_
#define _SL_CARD_TABLE_HH_

#include "sl/sl_databox.hh"


class SLCardTable  :  public SLDatabox
{

//----------------------------- data -------------------------------//
//----------------------------- data -------------------------------//
//----------------------------- data -------------------------------//

private:

  char                *_title;    // title for the table of card images.
  char                *_message;  // instruction for the optional code string.
  class HistoryCards  *_hist;     // either _cards or _hist will be NULL.
  class GenericCards  *_cards;    // either _cards or _hist will be NULL.
  class GenericDecode *_decode;   // can be NULL.
  int                  _ncol;     // number of characters to display.

//------------------------ functions ------------------------------//
//------------------------ functions ------------------------------//
//------------------------ functions ------------------------------//

public:          // constructors and destructor

           SLCardTable(SLDelay *slparent, char *name,
                       char *title, char *message,
                       class GenericCards  *cards,
                       class GenericDecode *decode = 0,
                       int ncol = 80, int omit = 1);
           SLCardTable(SLDelay *slparent, char *name,
                       char *title,                    
                       class HistoryCards  *cards,
                       int ncol = 80, int omit = 1);
  virtual ~SLCardTable();

  class HistoryCards  *hist  ()  const  { return _hist; }
  class GenericCards  *cards ()  const  { return _cards; }
  class GenericDecode *decode()  const  { return _decode; }

  void                 scrollToEnd();

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
