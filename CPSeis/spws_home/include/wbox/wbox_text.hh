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

//----------------------- wbox_text.hh -------------------------//
//----------------------- wbox_text.hh -------------------------//
//----------------------- wbox_text.hh -------------------------//

//              header file for the WboxText class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_TEXT_HH_
#define _WBOX_TEXT_HH_


class WboxText
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  enum { WBOX_TSIZE = 102 };

  class WboxBox *_box;

  char  _oldtext[WBOX_TSIZE];  // old text of entered field.
  char  _textbuf[WBOX_TSIZE];  // new text of entered field.
  int   _itoggle;              // text entry mode.
  int   _nloc;                 // curser location in datafield.
  int   _changed;              // whether text has changed (TRUE/FALSE).
  int   _remember;             // button press flag (0, 1, -1).


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  WboxText (WboxBox *box);

  virtual ~WboxText();

  const char *getTextbuf                ()  const  { return _textbuf; }
  const char *getActiveFieldDescription ()  const;

  void  initializeText  ();
  int   updateText      (const char *charr, const char *endkey,
                         int irow, int icol);

  void  reactToButtonMotionOrRelease (char       *endkey, int irow, int icol);
  void  updateTextEntryField         (const char *endkey, int irow, int icol);
  void  displayCurser                (const char *text = 0);  // NULL

private:


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


