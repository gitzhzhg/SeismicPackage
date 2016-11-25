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

//---------------------- wbox_text.cc -------------------------//
//---------------------- wbox_text.cc -------------------------//
//---------------------- wbox_text.cc -------------------------//

//          implementation file for the WboxText class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_text.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_screen.hh"
#include "wbox/wbox_field.hh"
#include "str.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>



//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxText::WboxText(WboxBox *box)
         :
            _box        (box),
            _itoggle    (0),
            _nloc       (0),
            _changed    (FALSE),
            _remember   (0)
{
  assert(_box);
  strcpy(_oldtext, " ");
  strcpy(_textbuf, " ");
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxText::~WboxText()
{
}



//---------------------- static messages ------------------------//
//---------------------- static messages ------------------------//
//---------------------- static messages ------------------------//


static char msg[][20] = {
         "replace mode",           // _itoggle == 1  svar = 1 or 7 or 22
         "insert mode",            // _itoggle == 2  svar = 1 or 7 or 22
         "overstrike mode",        // _itoggle == 3  svar = 1 or 7 or 22
         "pushbutton",             // _itoggle == 4  svar = 2 or 6
         "toggle button",          // _itoggle == 5  svar = 3
         "radio button",           // _itoggle == 6  svar = 4
         "non-editable field",     // _itoggle == 7  svar = 5 11 12 13 14
};



//---------------- get active field description --------------------//
//---------------- get active field description --------------------//
//---------------- get active field description --------------------//

    // public.
    // returns description and mode of active (highlighted) datafield.

const char *WboxText::getActiveFieldDescription()  const
{
  assert(_itoggle >= 1 && _itoggle <= 7);
  return msg[_itoggle - 1];
}



//-------------------------- initialize text ---------------------//
//-------------------------- initialize text ---------------------//
//-------------------------- initialize text ---------------------//

       // private.
       // text is set from the active (highlighted) datafield.

void WboxText::initializeText()
{
  WboxAllbox *allbox = _box->getAllboxPointer();
  WboxField  *active = _box->getActiveFieldPointer();
  int           svar = active->getSvar();
  int       abs_svar = AbsoluteValue(svar);

  _nloc    = 1;
  _itoggle = 1;
  if(abs_svar >=  2 && abs_svar <=  5) _itoggle = abs_svar + 2;
  if(abs_svar ==  6)                   _itoggle = 4;
  if(abs_svar >= 11 && abs_svar <= 14) _itoggle = 7;
  _changed  = FALSE;
  allbox->textEntryCompleted();

  active->getAndEncodeUserValue(active->getIndex(), _textbuf);
  strcpy(_oldtext, _textbuf);
}



//---------------------- all blank --------------------------//
//---------------------- all blank --------------------------//
//---------------------- all blank --------------------------//

   // returns TRUE if the string is zero length or filled with blanks.
   // returns FALSE if there are any non-blank characters in text.

static int all_blank(const char *text)
{
  int n = strlen(text);
  int i;
  for(i = 0; i < n; i++)
      {
      if(text[i] != ' ') return FALSE;
      }
  return TRUE;
}



//-------------------- make sure ----------------------------//
//-------------------- make sure ----------------------------//
//-------------------- make sure ----------------------------//

          // make sure text has length >= nchar.

static void make_sure(char *text, int nchar)
{
  int length = strlen(text);
  if(length >= nchar) return;
  char temporary[300];
  assert(nchar >= 1 && nchar < 300);
  memset(temporary,' ',nchar);
  temporary[nchar] = '\0';
  memcpy(temporary,text,length);
  assert(temporary[nchar] == '\0');
  strcpy(text, temporary);
  length = strlen(text);
  assert(length >= nchar && length < 300);
}



//-------------------------- update text -------------------------//
//-------------------------- update text -------------------------//
//-------------------------- update text -------------------------//

    // private.
    // the active (highlighted) datafield is used.
    // _oldtext and _textbuf must both be valid upon entry.
    // _oldtext and _textbuf must be null-terminated.
    // _oldtext and _textbuf can be any length.
    // _oldtext and _textbuf are both reset in this routine.
    // returns nread == -1 if we have not completed the field.
    // returns nread ==  0 if we have not modified  the field.
    // returns nread >=  1 if we have     modified  the field.

#define RING_AND_RETURN { allbox->ringBell(); return nread; }


int WboxText::updateText(const char *charr, const char *endkey,
                         int irow, int icol)
{
  WboxAllbox *allbox = _box->getAllboxPointer();
  WboxField  *active = _box->getActiveFieldPointer();
  int svar           = active->getEffectiveSwitch();
  int irow2          = active->getIrow ();
  int icol2          = active->getIcol ();
  int nchar2         = active->getNchar();
  if(active->isCvar() && active->getLength() < nchar2)
                                         nchar2 = active->getLength();
  char temporary[300];
  make_sure(_oldtext, nchar2); 
  make_sure(_textbuf, nchar2); 

  int nread = -1;   // we have (temporarily) not yet completed a field.
  allbox->textBeingEntered();

  if(!strcmp(endkey, " "))                        // add a character.
      {
      if(_itoggle >= 4 || svar <= 0) RING_AND_RETURN
      strcpy(temporary, _textbuf);
      if(_nloc < nchar2)
          {
          if(_itoggle == 1)                          // replace mode.
              {
              memset(&temporary[_nloc], ' ', nchar2 - _nloc);
              }
          else if(_itoggle == 2)                           // insert mode.
              {
              memcpy(&temporary[_nloc], &_textbuf[_nloc - 1], nchar2 - _nloc);
              }
          }
      temporary[_nloc - 1] = charr[0];
      int istat = active->validateText(temporary);
      if(istat <= -1) RING_AND_RETURN      // error occurred.

      _changed = TRUE;
      memcpy(_textbuf, temporary, nchar2);
      _nloc = MinimumValue(_nloc + 1, nchar2);
      }

  else if(!strcmp(endkey, "DELETE"))      // delete a character.
      {
      if(_itoggle >= 4 || svar <= 0) RING_AND_RETURN
      if(_nloc == nchar2)
          {
          if(_textbuf[_nloc - 1] == ' ')
              {
              _nloc = MaximumValue(_nloc - 1, 1);
              }
          _textbuf[_nloc - 1] = ' ';
          }
      else if(_nloc > 1)
          {
          _nloc--;
          memcpy(temporary, &_textbuf[_nloc], nchar2 - _nloc);
          memcpy(&_textbuf[_nloc - 1], temporary, nchar2 - _nloc);
          _textbuf[nchar2 - 1] = ' ';
          }
      if(_nloc == 1 && all_blank(_textbuf)) strcpy(_textbuf, _oldtext);
      }

  else if(!strcmp(endkey, "$UP"))
      {
      if(_itoggle >= 4 || svar <= 0) RING_AND_RETURN
      _itoggle--;
      if(_itoggle == 0) _itoggle = 3;
      }

  else if(!strcmp(endkey, "$DOWN"))
      {
      if(_itoggle >= 4 || svar <= 0) RING_AND_RETURN
      _itoggle++;
      if(_itoggle == 4) _itoggle = 1;
      }

  else if(!strcmp(endkey, "$LEFT"))
      {
      if(_itoggle >= 4 || svar <= 0) RING_AND_RETURN
      _nloc = MaximumValue(_nloc - 1, 1);
      _itoggle = 2;
      }

  else if(!strcmp(endkey, "$RIGHT"))
      {
      if(_itoggle >= 4 || svar <= 0) RING_AND_RETURN
      _nloc = MinimumValue(_nloc + 1, nchar2);
      _itoggle = 2;
      }

  else if(!strcmp(endkey, "RELEASE"))
      {
      return nread;
      }

  else if(!strcmp(endkey, "BUTTON") && _itoggle <= 3 && irow2 == irow
               && icol >= icol2 && icol < icol2 + nchar2)
      {
      if(icol != icol2 + _nloc - 1)
          {
          _nloc = icol - icol2 + 1;
          _itoggle = 2;
          }
      else if(_itoggle == 1 && _nloc > 1)
          {
          _nloc = 1;
          }
      else
          {
          _itoggle++;
          if(_itoggle == 4) _itoggle = 1;
          }
      }

  else if(!memcmp(_textbuf, _oldtext, nchar2) && !_changed)
                             // we did not change this field.
      {
      nread = 0;
      allbox->textEntryCompleted();
      }

  else                       // we changed this field.
      {
      int istat = active->validateText(_textbuf);
      if(istat <= -1)
          {
          _box->maybeShowMessage("DATA ENTRY ERROR");
          RING_AND_RETURN
          }
      nread = 1;
      allbox->textEntryCompleted();
      for(int i = 0; i < nchar2; i++)
          {
          if(_textbuf[i] != ' ') nread = i+1;
          }
      strcpy(_oldtext, _textbuf);
      }
  return nread;
}



//----------------------- text is zero ---------------------------//
//----------------------- text is zero ---------------------------//
//----------------------- text is zero ---------------------------//

static int text_is_zero(const char *text)
{
  char text2[200];
  str_remove_trailing_blanks(text2, (char*)text);
  if(text2[0] == '\0') return TRUE;
  if(strcmp(text2, "0") == 0) return TRUE;
  if(strcmp(text2, " ") == 0) return TRUE;
  return FALSE;
}



//---------------- react to button motion or release -------------------//
//---------------- react to button motion or release -------------------//
//---------------- react to button motion or release -------------------//

      // public.
      // uses member variable _remember.
      // does nothing unless endkey is MOTION or RELEASE or RETURN.
      // might reset _textbuf to "1" or "0".
      // might reset endkey to "RETURN" or "finished".
      // draws to screen.

      // _remember ==  0 if mouse button was not pressed on a
      //                   pushbutton, toggle button, or radio button.
      // _remember ==  1 if the pointer is in the active datafield.
      // _remember == -1 if the pointer is not in the active datafield.

void WboxText::reactToButtonMotionOrRelease(char *endkey, int irow, int icol)
{
  WboxScreen *screen = _box->getScreenPointer();
  WboxAllbox *allbox = _box->getAllboxPointer();
  WboxField  *active = _box->getActiveFieldPointer();

  if(!strcmp(endkey, "MOTION"))
      {
      int   min_switch = 1;
      WboxField *field = _box->findFieldPointer(irow, icol, min_switch);
      if(_remember == -1 && field == active)
          {
          if(_itoggle == 4)              // pushbutton.
              {
              screen->draw2(_box, active, _textbuf, 1);
              }
          else if(_itoggle == 5)         // toggle button.
              {
              if(text_is_zero(_textbuf)) screen->draw3(_box, active, "1");
              else                       screen->draw3(_box, active, "0");
              }
          else if(_itoggle == 6)         // radio button.
              {
              screen->draw3(_box, active, "1");
              }
          _remember = 1;
          }
      else if(_remember == 1 && field != active)
          {
          screen->draw3(_box, active, _textbuf);
          _remember = -1;
          }
      strcpy(endkey, "finished");
      return;
      }
  else if(!strcmp(endkey, "RELEASE"))
      {
      if(_remember <= 0)
          {
          _remember = 0;
          strcpy(endkey, "finished");
          return;
          }
      if(_itoggle == 4)              // pushbutton.
          {
          screen->draw3(_box, active, _textbuf);
          strcpy(endkey, "RETURN");
          }
      else if(_itoggle == 5)         // toggle button.
          {
          if(text_is_zero(_textbuf)) strcpy(_textbuf, "1");
          else                       strcpy(_textbuf, "0");
          strcpy(endkey, "RETURN");
          }
      else if(_itoggle == 6)         // radio button.
          {
          strcpy(_textbuf, "1");
          strcpy(endkey, "RETURN");
          }
      _remember = 0;
      }
  else if(!strcmp(endkey, "RETURN"))
      {
      if(_itoggle == 4)              // pushbutton.
          {
          screen->draw2(_box, active, _textbuf, 1);
          allbox->flushBuffer();
          allbox->wasteTime(10000);
/////     screen->draw2(_box, active, _textbuf, 2);  // not necessary.
          }
      else if(_itoggle == 5)         // toggle button.

          {
          if(text_is_zero(_textbuf)) strcpy(_textbuf, "1");
          else                       strcpy(_textbuf, "0");
          }
      else if(_itoggle == 6)         // radio button.
          {
          strcpy(_textbuf, "1");
          }
      _remember = 0;
      }
}



//--------------- update text entry field ---------------------------//
//--------------- update text entry field ---------------------------//
//--------------- update text entry field ---------------------------//

         // public.
         // uses member variable _remember.
         // draws to screen.

void WboxText::updateTextEntryField(const char *endkey, int irow, int icol)
{
  WboxScreen *screen  = _box->getScreenPointer();
  WboxAllbox *allbox  = _box->getAllboxPointer();
  WboxField  *active  = _box->getActiveFieldPointer();

  if(!strcmp(endkey, "BUTTON") && _itoggle >= 4)
      {
      int   min_switch = 1;
      WboxField *field = _box->findFieldPointer(irow, icol, min_switch);
      if(field == active) _remember = 1;
      }

  if(_remember == 1 && _itoggle == 4)            // pushbutton.
      {
      screen->draw2(_box, active, _textbuf, 1);
      }
  else if(_remember == 1 && _itoggle == 5)        // toggle button.
      {
      if(text_is_zero(_textbuf)) screen->draw3(_box, active, "1");
      else                       screen->draw3(_box, active, "0");
      }
  else if(_remember == 1 && _itoggle == 6)         // radio button.
      {
      screen->draw3(_box, active, "1");
      }
  else
      {
      screen->draw3(_box, active, _textbuf);
      }

  if(allbox->getDebug() >= 2)
      printf("%6d %-10s %-8s update text entry field  remember = %d\n",
               _box->getIbox(), _box->getBoxName(), endkey, _remember);
}



//---------------------- display curser -----------------------------//
//---------------------- display curser -----------------------------//
//---------------------- display curser -----------------------------//

         // displays curser and associated highlighted text
         //   for the active datafield.
         // if text is NULL, _textbuf is used.
         // draws to screen.

void WboxText::displayCurser(const char *text)
{
  assert(_nloc >= 1);
  if(text == NULL) text = _textbuf;

  WboxScreen *screen = _box->getScreenPointer();
  WboxField  *active = _box->getActiveFieldPointer();
  int         irow   = active->getIrow ();
  int         icol   = active->getIcol ();
  int         nchar  = active->getNchar();
  int         svar   = active->getSvar ();
  const char *text2  = &text[_nloc - 1];
  int         icol2  = icol + _nloc - 1;

  if(!_box->hasFocus() || svar <= 0) return;

  if(_itoggle == 1)                    // replace.
      {
      int j = _nloc;
      for(int i = _nloc; i <= nchar; i++)
          {
          if(text[i - 1] != ' ') j = i;
          }
      screen->draw(_box, irow, icol2, text2, j - _nloc + 1, 40);
      }
  else if(_itoggle == 2)               // insert.
      {
      screen->draw(_box, irow, icol2, text2, 1, 50);
      }
  else if(_itoggle == 3)               // overstrike.
      {
      screen->draw(_box, irow, icol2, text2, 1, 40);
      }
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

