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

//------------------------ read_cards.hh ----------------------------//
//------------------------ read_cards.hh ----------------------------//
//------------------------ read_cards.hh ----------------------------//

//             header file for the ReadCards class
//                 not derived from any class
//                     subdirectory oprim


//    This class reads card images from a text file.
//    The text file is opened in the constructor.
//    The text file is closed in the destructor.
//    See the implementation file for documentation.


#ifndef _READ_CARDS_HH_
#define _READ_CARDS_HH_

#include <stdio.h>


class ReadCards
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  const char *_card;    // pointer to card last read.
  FILE       *_stream;  // pointer to stream.
  int         _eof;     // whether last read was EOF.
  int         _error;   // whether last read had an error.
  int         _errno;   // error number of last read.
  int         _reread;  // whether to re-read the same card.

//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:    // constructor and destructor.

  ReadCards(const char *filename);
  virtual ~ReadCards();

  const char *readCard         ();
  void        unreadCard       ();

  int         openError        ()  const  { return (_stream == 0); }
  int         eofEncountered   ()  const  { return _eof; }
  int         errorEncountered ()  const  { return _error; }
  int         errorNumber      ()  const  { return _errno; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
