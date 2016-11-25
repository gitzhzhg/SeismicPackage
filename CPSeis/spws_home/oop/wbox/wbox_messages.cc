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

//---------------------- wbox_messages.cc -------------------------//
//---------------------- wbox_messages.cc -------------------------//
//---------------------- wbox_messages.cc -------------------------//

//        implementation file for the WboxMessages class
//                  not derived from any class
//                       subdirectory wbox


  // This class manages the two message lines at the bottom of the
  //     windowbox.


#include "wbox/wbox_messages.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_text.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_screen.hh"
#include "wbox/wbox_field.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxMessages::WboxMessages(WboxBox *box, WboxText *text)
         :
            _box             (box),
            _text            (text),
            _showing         (0),
            _iflag           (FALSE)
{
  assert(_box && _text);
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxMessages::~WboxMessages()
{
}



//----------------------- show message ------------------------------//
//----------------------- show message ------------------------------//
//----------------------- show message ------------------------------//

     // public.
     // this message will have priority over any message displayed
     //   with maybeShowMessage.

void WboxMessages::showMessage(const char *msg)
{
  show(msg, -34);
  _showing = 2;
}



//----------------------- maybe show message -----------------------//
//----------------------- maybe show message -----------------------//
//----------------------- maybe show message -----------------------//

     // public.
     // any message displayed with showMessage will have priority
     //   over this message.

void WboxMessages::maybeShowMessage(const char *msg)
{
  if(_showing == 2) return;
  show(msg, -34);
  _showing = 1;
}



//---------------------- show windowbox info ------------------------//
//---------------------- show windowbox info ------------------------//
//---------------------- show windowbox info ------------------------//

   // public.
   // display information about the windowbox or highlighted datafield.
   // WILL NOT display any information if showMessage
   //   has been called since the last call to this routine.
   // MIGHT NOT display any information if maybeShowMessage
   //   has been called since the last call to this routine.

void WboxMessages::showWindowboxInfo()
{
  int has_focus   = _box->hasFocus();
  int svar        = _box->getActiveFieldPointer()->getSvar();
  const char *msg = _text->getActiveFieldDescription();

  if     (_showing  == 2) {                                   } // do not draw.
  else if(!has_focus    ) { show("inactive window"    , -33); }
  else if(svar      <= 0) { show("all fields inactive", -33); }
  else if(_showing  == 1) {                                   } // do not draw.
  else                    { show(msg                  , -33); }

  _showing = 0;
}



//----------------- maybe display array info -----------------------//
//----------------- maybe display array info -----------------------//
//----------------- maybe display array info -----------------------//

     // public.

void WboxMessages::maybeDisplayArrayInfo()
{
  if(_iflag)
      {
      _iflag = FALSE;
      return;
      }
  WboxField *field = _box->getActiveFieldPointer();
  int        index = field->getIndex();
  int        n     = field->getN    ();
  int        itab  = field->getItab ();
  display(index, n, itab);
}



//----------------------- display array info -----------------------//
//----------------------- display array info -----------------------//
//----------------------- display array info -----------------------//

     // public.
     // called by WboxFind.

void WboxMessages::displayArrayInfo(int index, int n, int itab)
{
  _iflag = TRUE;
  display(index, n, itab);
}



//-------------------------- show -----------------------------------//
//-------------------------- show -----------------------------------//
//-------------------------- show -----------------------------------//

     // private.

void WboxMessages::show(const char *msg, int svar)
{
  if(_box->getOmitFlag()) return;
  int   nrow         = _box->getNrow();
  int   ncol         = _box->getNcol();
  WboxScreen *screen = _box->getScreenPointer();
  WboxAllbox *allbox = _box->getAllboxPointer();
  screen->draw(_box, nrow, 1, msg, ncol-1, svar);
  allbox->flushBuffer();
}



//-------------------------- display ---------------------------------//
//-------------------------- display ---------------------------------//
//-------------------------- display ---------------------------------//

     // private.

void WboxMessages::display(int index, int n, int itab)
{
  if(_box->getOmitFlag()) return;
  if(!_box->hasLinkedArrays()) return;
  int         nrow   = _box->getNrow();
  int         ncol   = _box->getNcol();
  WboxScreen *screen = _box->getScreenPointer();
  char buffer[101];
  if(itab >= 1 && _box->hasFocus())
      {
      sprintf(buffer, "index %-5d  of %-5d", index, n);
      if(_iflag) strcat(buffer, "  value");
      }
  else
      {
      strcpy(buffer, " ");
      }
  screen->draw(_box, nrow-1, 1, buffer, ncol-1, -33);
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

