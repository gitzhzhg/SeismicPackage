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


//---------------------- read_cards.cc ------------------------//
//---------------------- read_cards.cc ------------------------//
//---------------------- read_cards.cc ------------------------//

//            implementation file for the ReadCards class
//                    not derived from any class     
//                        subdirectory oprim


#include "oprim/read_cards.hh"
#include "cprim.h"
#include "readcard.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>


//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//


ReadCards::ReadCards(const char *filename)
          :
            _card            (NULL),
            _stream          (NULL),
            _eof             (TRUE),
            _error           (TRUE),
            _errno           (0),
            _reread          (FALSE)
{
  _stream = fopen(filename, "r");
}



ReadCards::~ReadCards()
{
  if(_stream) fclose(_stream);
}



//------------------------- read card -----------------------------//
//------------------------- read card -----------------------------//
//------------------------- read card -----------------------------//

    // public.
    // This function reads one line ("card image") from a disk file.
    // This line is a string of characters terminated by \n.
    // The returned pointer points to a string in static memory.
    // This string is terminated with \0, and does not contain
    // the \n character.  A NULL is returned if an EOF is encountered
    // or an error occurs.

const char *ReadCards::readCard()
{
  assert(_stream);
  if(_reread)
      {
      _reread = FALSE;
      }
  else
      {
      _card  = readcard(_stream);
      _eof   = feof     (_stream);
      _error = ferror   (_stream);
      _errno = errno;
      }
  return _card;
}



//------------------------- unread card ----------------------------//
//------------------------- unread card ----------------------------//
//------------------------- unread card ----------------------------//

    // public.
    // Call this function to "push back" the card onto the file.
    // Then the next call to readCard will return the same
    //   string as the previous call.

void ReadCards::unreadCard()
{
  assert(_reread == FALSE);
  _reread = TRUE;
}



//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//


