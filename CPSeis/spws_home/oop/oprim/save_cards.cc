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


//---------------------- save_cards.cc ------------------------//
//---------------------- save_cards.cc ------------------------//
//---------------------- save_cards.cc ------------------------//

//            implementation file for the SaveCards class
//                    not derived from any class     
//                        subdirectory oprim


#include "oprim/save_cards.hh"
#include "cprim.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>


//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//


SaveCards::SaveCards(const char *filename)
          :
            _stream          (NULL),
            _errno           (0)
{
  _stream = fopen(filename, "w");
}



SaveCards::~SaveCards()
{
  if(_stream) fclose(_stream);
}



//------------------------- save card -----------------------------//
//------------------------- save card -----------------------------//
//------------------------- save card -----------------------------//

    // public.
    // This function saves one line ("card image") to a disk file.
    // This string is terminated with \0, and does not contain
    // the \n character.
    // This line on the file will be a string terminated by \n.
    // Returns error = TRUE if a write error occurs.
    // Returns error = FALSE otherwise.

int SaveCards::saveCard(const char *card)
{
  assert(_stream);
  assert(card);
  int number = fprintf(_stream, "%s\n", card);
  int error = ferror(_stream);
  if(number < 0) error = TRUE;  // should be redundant.
  _errno = errno;
  return error;
}



//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//


