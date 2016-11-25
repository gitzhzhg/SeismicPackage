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

//----------------------- wbox_messages.hh -------------------------//
//----------------------- wbox_messages.hh -------------------------//
//----------------------- wbox_messages.hh -------------------------//

//            header file for the WboxMessages class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_MESSAGES_HH_
#define _WBOX_MESSAGES_HH_


class WboxMessages
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class WboxBox  *_box;
  class WboxText *_text;

  int   _showing;     // for general message area (0 or 1 or 2).
  int   _iflag;       // for array message area (TRUE or FALSE).


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:   // constructor and destructor.

  WboxMessages (WboxBox *box, WboxText *text);

  virtual ~WboxMessages();

public:   // display information in general message area.

  void  showMessage               (const char *msg);
  void  maybeShowMessage          (const char *msg);
  void  showWindowboxInfo         ();

public:   // display information in array message area.

  void  maybeDisplayArrayInfo    ();
  void  displayArrayInfo         (int index, int n, int itab);

private:

  void  show     (const char *msg, int svar);
  void  display  (int index, int n, int itab);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


