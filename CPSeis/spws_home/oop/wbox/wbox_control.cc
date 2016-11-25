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

//---------------------- wbox_control.cc -------------------------//
//---------------------- wbox_control.cc -------------------------//
//---------------------- wbox_control.cc -------------------------//

//          implementation file for the WboxControl class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_control.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_screen.hh"
#include "wbox/wbox_help.hh"
#include "wbox/wbox_hard.hh"
#include "wbox/wbox_find.hh"
#include "wbox/wbox_events.hh"
#include "wbox/wbox_text.hh"
#include "wbox/wbox_roll.hh"
#include "wbox/wbox_insert.hh"
#include "wbox/wbox_messages.hh"
#include "wbox/wbox_field.hh"
#include "wbox/wbox_vector.hh"
#include "wbox/wbox_link.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>




//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxControl::WboxControl(WboxBox *box)
         :
            _box              (box),
            _help             (NULL),
            _hard             (NULL),
            _find             (NULL),
            _events           (NULL),
            _text             (NULL),
            _roll             (NULL),
            _insert           (NULL),
            _messages         (NULL)
{
  assert(box);

  _text        = new WboxText    (_box);
  _roll        = new WboxRoll    (_box);
  _insert      = new WboxInsert  (_box);
  _messages    = new WboxMessages(_box, _text);
  _hard        = new WboxHard    (_box);
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxControl::~WboxControl()
{
  delete _text;
  delete _roll;
  delete _insert;
  delete _messages;
  delete _hard;
  if(_help)   delete _help;
  if(_find)   delete _find;
  if(_events) delete _events;
}





//--------------------- get values -------------------------------//
//--------------------- get values -------------------------------//
//--------------------- get values -------------------------------//


WboxField *WboxControl::getActiveFieldPointer()  const
{
  return _roll->getActiveFieldPointer();
}



//------------------------- set focus ----------------------------//
//------------------------- set focus ----------------------------//
//------------------------- set focus ----------------------------//

       // public.
       // a new active datafield is chosen.
       // if called from a trap (for this same windowbox),
       //   this routine simply remembers the information,
       //   which will be retrieved and acted upon after
       //   returning from the trap.

static int IDENT_SET_FROM_TRAP        = 0;
static int INDEX_SET_FROM_TRAP        = 0;
static int SET_FOCUS_CALLED_FROM_TRAP = FALSE;

void WboxControl::setFocus(int ident, int index)
{
  if(_box->getIbox() == _box->getAllboxPointer()->getTrapBoxNumber())
      {
      IDENT_SET_FROM_TRAP        = ident;
      INDEX_SET_FROM_TRAP        = index;
      SET_FOCUS_CALLED_FROM_TRAP = TRUE;
      }
  else
      {
      _roll->gotoSpecifiedDatafield(ident, index);
      }
}



//------------------------ send event ------------------------------//
//------------------------ send event ------------------------------//
//------------------------ send event ------------------------------//

       // public.

void WboxControl::sendClientMessageEvent(const char *endkey)  const
{
  if(_events) _events->sendClientMessageEvent(endkey);
}



void WboxControl::sendImmediateEvent(const char *endkey)
{
  if(_events) _events->sendImmediateEvent(endkey);
}



//--------------------- complete canvas --------------------------------//
//--------------------- complete canvas --------------------------------//
//--------------------- complete canvas --------------------------------//

     // complete the canvas after registrations are finished.

void WboxControl::completeCanvas (HelpCtx hctx, Widget toplevel,
                              const char *helpfile, const char *helptitle)
{
  _help   = new WboxHelp   (_box, hctx, toplevel, helpfile, helptitle);
  _find   = new WboxFind   (_box, _messages);
  _events = new WboxEvents (_box);
}



//------------ show message or maybe show message ------------------//
//------------ show message or maybe show message ------------------//
//------------ show message or maybe show message ------------------//

         // public.

void WboxControl::showMessage(const char *msg)
{
  _messages->showMessage(msg);
}


void WboxControl::maybeShowMessage(const char *msg)
{
  _messages->maybeShowMessage(msg);
}


void WboxControl::showWindowboxInfo()
{
  _messages->showWindowboxInfo();
}




//--------------------- save table ---------------------------------//
//--------------------- save table ---------------------------------//
//--------------------- save table ---------------------------------//

         // public.

void WboxControl::registerHardcopy(WboxHardcopy *hardcopy)
{
  _hard->registerHardcopy(hardcopy);
}


void WboxControl::saveTable(const char *filename)  const
{
  _hard->saveTable(filename);
}



//------------------ call user trap --------------------------------//
//------------------ call user trap --------------------------------//
//------------------ call user trap --------------------------------//

   // private.
   // the active datafield is used to choose the trap to call.
   // this trap uses _text->getTextbuf() (or a blank) for textbuf.
   // this trap uses the argument value (or 1) for nread.
   // this trap uses variable values obtained from the argument list.
   // ident and index do not have to be preset.
   // ident and index are first set to the values for the active datafield.
   // the user trap might change the values of ident,index,endkey.
   // useFindFields might change the values of ident,index,endkey.
   // useFindFields might change the value  of nread.

void WboxControl::callUserTrap
               (int ivar, float fvar, double dvar, const char *cvar,
                int *nread, char *endkey,
                int *ident, int *index)
{
  int skip_trap = FALSE;
  if(*nread == 0)
      {
      if(!strcmp(endkey, "FOCUSIN" )) skip_trap = TRUE;
      if(!strcmp(endkey, "FOCUSOUT")) skip_trap = TRUE;
      if(!strcmp(endkey, "SCROLL"  )) skip_trap = TRUE;
      if(!strcmp(endkey, "BUTTON"  ) && !_box->hasFocus()) skip_trap = TRUE;
      }
  WboxField *active = _roll->getActiveFieldPointer();

  if(skip_trap)
      {
      *ident = active->getIdent();
      *index = active->getIndex();
      }
  else
      {
      WboxAllbox *allbox = _box->getAllboxPointer();
      int         ibox   = _box->getIbox();
      allbox->setTrapBoxNumber(ibox);
      SET_FOCUS_CALLED_FROM_TRAP = FALSE;
      if(!strcmp(endkey, "RESTORED") ||
         !strcmp(endkey, "INSERTED") ||
         !strcmp(endkey, "REMOVED"))
            {
            active->callTrap(ivar, fvar, dvar, cvar,
                      " ", 1, endkey,   ident, index);
            }
      else
            {
            int nkeep = active->getN();
            int ikeep = *index;
            active->callTrap(ivar, fvar, dvar, cvar,
                      _text->getTextbuf(), *nread, endkey,   ident, index);
            if(ikeep <= nkeep && active->getN() == nkeep + 1)
                      _messages->maybeShowMessage("row inserted");
            if(ikeep <= nkeep && active->getN() == nkeep - 1)
                      _messages->maybeShowMessage("row removed");
            }
      if(SET_FOCUS_CALLED_FROM_TRAP)
          {
          *ident = IDENT_SET_FROM_TRAP;
          *index = INDEX_SET_FROM_TRAP;
          }
      allbox->setTrapBoxNumber(0);
      }
     
  if(_find) _find->useFindFields(ident, index, nread, endkey);
}



//------------------ call user trap --------------------------------//
//------------------ call user trap --------------------------------//
//------------------ call user trap --------------------------------//

   // private.
   // the active datafield is used to choose the trap to call.
   // this trap uses a blank for textbuf.
   // this trap uses a zero for nread.
   // this trap uses local variable values obtained from WboxField.
   // the user might change the value of endkey.
   // this trap ignores changes to ident and index.

void WboxControl::callUserTrap(char *endkey)
{
  WboxField *active = _roll->getActiveFieldPointer();
  int ident;
  int index;
  WboxAllbox *allbox = _box->getAllboxPointer();
  int         ibox   = _box->getIbox();
  allbox->setTrapBoxNumber(ibox);
  active->callTrap(" ", 0, endkey,   &ident, &index);
  allbox->setTrapBoxNumber(0);
}



//------------------------ update fields ---------------------------//
//------------------------ update fields ---------------------------//
//------------------------ update fields ---------------------------//

    // public.
    // two values of endkey get special treatment: EXPOSE and UPDATE.
    // irow,icol,irow2,icol2 are used only when endkey is EXPOSE.
    // if this windowbox has the focus:
    //  (1) if endkey is UPDATE, the array message line is updated.
    //  (2) the curser and associated highlighted text are re-drawn.

void WboxControl::updateFields(const char *endkey,
                           int irow, int icol, int irow2, int icol2)
{
  char endkey2[50];
  strcpy(endkey2, "REDRAW");
  callUserTrap(endkey2);
  int kmin = _box->getKmin();
  int kmax = _box->getKmax();

  for(int ifield = kmin; ifield <= kmax; ifield++)
      {
      WboxField *field = _box->getFieldPointer(ifield);
      field->updateField(endkey, irow, icol, irow2, icol2);
      }

  if(_box->hasFocus())
      {
      WboxField *active = _roll->getActiveFieldPointer();
      if(!strcmp(endkey, "UPDATE"))_messages->maybeDisplayArrayInfo();
      char textbuf[200];
      active->encodeValue(textbuf);
      _text->displayCurser(textbuf);
      printDebug(endkey, "fixing active datafield in active windowbox");
      }
}



//----------------------- update active field ----------------------//
//----------------------- update active field ----------------------//
//----------------------- update active field ----------------------//

        // private.

void WboxControl::updateActiveField(const char *endkey,
                           int irow, int icol, int irow2, int icol2)
{
  WboxField *active = _roll->getActiveFieldPointer();
  active->updateField(endkey, irow, icol, irow2, icol2);
}



//------------------------- boxtrap -------------------------------//
//------------------------- boxtrap -------------------------------//
//------------------------- boxtrap -------------------------------//

         // public.

void WboxControl::boxtrap(const char *charr, const char *endkey9,
                             int irow, int icol, int irow2, int icol2)
{
  WboxAllbox *allbox  = _box->getAllboxPointer();
  WboxField  *active  = _roll->getActiveFieldPointer();
  int         ident   = active->getIdent();
  int         index   = active->getIndex();

  static int nread = 0;
  char endkey[10];
  strcpy(endkey, (char*)endkey9);

//---------check before scrolling whether text entry field is finished.

  if(!strcmp(endkey, "BSCROLL") && nread >= 0) return;

//---------we have changed the size of the screen.

  if(!strcmp(endkey, "CONFIGUR"))
      {
      goto label40;
      }

//---------we have exposed a part of the screen.

  else if(!strcmp(endkey, "EXPOSE"))
      {
      goto label40;
      }

//---------get button position and display help.

  else if(!strcmp(endkey, "MOTION"))
      {
      _help->showHelp(irow, icol);
      }

//---------react to a button motion or release event.

  if(!strcmp(endkey, "MOTION") || !strcmp(endkey, "RELEASE") ||
     !strcmp(endkey, "RETURN"))
      {
      _text->reactToButtonMotionOrRelease(endkey, irow, icol);
      if(!strcmp(endkey, "finished")) return;
      }

//----------------------update the text entry field.

  nread = _text->updateText(charr, endkey, irow, icol);
  if(nread < 0) goto label80;   // user has not completed text entry field.

//---------------------save value for restoring if necessary.
//---------------------also put new value into pointee if possible.

  ////////// if(nread > 0) savePreviousValue();          // removed 5/9/97
  if(nread > 0)                                          // added 5/9/97
      {                                                  // added 5/9/97
      if(!strcmp(endkey, "DO")) strcpy(endkey, "DONT");  // added 5/9/97
      savePreviousValue();                               // added 5/9/97
      }                                                  // added 5/9/97

//--------------------------call user trap.

              active  = _roll->getActiveFieldPointer();
  int         ivar;
  float       fvar;
  double      dvar;
  char        cvar[200];
  active->decodeValue(_text->getTextbuf(), &ivar, &fvar, &dvar, cvar);

label20:

  callUserTrap(ivar, fvar, dvar, cvar, &nread, endkey, &ident, &index);

//------------------------rewrite variable in text entry field.

  updateActiveField(endkey, irow, icol, irow2, icol2);
  _box->getScreenPointer()->eraseHighlightBox(_box);

  if(nread == 0)
      {
      if(!strcmp(endkey, "FOCUSIN" )) goto label40;
      if(!strcmp(endkey, "FOCUSOUT")) goto label40;
      if(!strcmp(endkey, "SCROLL"  )) goto label40;
/////// new (testing):
      if(!strcmp(endkey, "BUTTON"  ) && !_box->hasFocus()) goto label40;
      }


//---------------unmanage or destroy the windowbox if requested.

  if(!strcmp(endkey, "UNMANAGE"))
      {
      _box->unmanage();
      return;
      }
  else if(!strcmp(endkey, "DESTROY"))
      {
      _box->destroy();
      return;
      }

//-------------------------restore bad value, with message, if necessary.

////  if(nread == 0)                                     // removed 5/9/97
////      {                                              // removed 5/9/97
      if(!strcmp(endkey, "RESTORE") || !strcmp(endkey, "DO"))
          {
          int restored = restorePreviousValue(&ivar, &fvar, &dvar, cvar);
          if(restored)
              {
              strcpy(endkey, "RESTORED");
              goto label20;
              }
          }
////      }                                              // removed 5/9/97

//--------------------------increment n if necessary.

  if(nread > 0)
      {
      WboxField *active = _roll->getActiveFieldPointer();
      WboxLink  *link   = active->getLinkPointer();
      if(link)
          {
          int index = active->getIndex();  // fortran code uses index
                                           // returned from trap.
                                           // i think that is wrong.
          int n     = link->getN   ();
          int nmax  = link->getNmax();
          if(index == n + 1 && index <= nmax) link->putN(n + 1);
          }
      }

//-------------------get new active datafield from ident and index.

  _roll->gotoSpecifiedDatafield(ident, index);
           // ident and index are values returned from trap.
           // a new active datafield is chosen.
           // linked array set is scrolled if necessary.

//-----------------------------insert or delete a line.

      /// something happens below only if endkey is one of these values:
      ///   INSERTED  REMOVED  INSERT REMOVE ^P ^C.

  _insert->insertOrRemove(endkey, nread);

  if(!strcmp(endkey, "INSERTED") || !strcmp(endkey, "REMOVED"))
      {
      WboxField *active = _roll->getActiveFieldPointer();
      active->getUserValue(index, &ivar, &fvar, &dvar, cvar);
      goto label20;
      }
      
//-------------------------------find new active datafield.

label40:

  _roll->gotoNewDatafield(endkey, irow, icol);

  updateActiveField(endkey, irow, icol, irow2, icol2);
           ////////// this seems to be crucial!!!

//-------call user trap to say we have arrived.
//-------this trap is not allowed to change any arguments or values.

  if(strcmp(endkey, "FOCUSOUT") && _box->hasFocus())
      {
      char endkey2[10];
      strcpy(endkey2, "ARRIVED");
      callUserTrap(endkey2);
      }

//-----------------------------initialize the new text to display.

  _text->initializeText();

//----------------------------draw boxes and messages.

  _box->getScreenPointer()->drawHighlightBox(_box);
  _messages->maybeDisplayArrayInfo();
  printDebug(endkey, "draw highlight box and index line");

//----------------update all windowbox fields.

  allbox->updateFields(_box, endkey, irow, icol, irow2, icol2);

//-----------------------------update the text entry field.

label80:

  _text->updateTextEntryField(endkey, irow, icol);

//--------------display curser, message line, and help.

  _text->displayCurser();
  _messages->showWindowboxInfo();
  _help->showHelp(endkey);
  _hard->maybeSaveTable(endkey);
  printDebug(endkey, "display curser, draw message, show help");

//--------------create and/or modify the scrollbars.

  if(!strcmp(endkey, "SCROLL")) allbox->updateScrollbars();
}



//--------------- save and restore previous value ------------------//
//--------------- save and restore previous value ------------------//
//--------------- save and restore previous value ------------------//

   // private.

   // save:    save old value from active datafield local value.
   //          save new value from textbuf.
   //          to be called only when nread > 0.
   //          to be called before a trap is called.
   //          also put new value into user area if possible.

   // restore: return value previously saved.
   //          to be called after a trap is called.
   //          recommend calling only when nread == 0.
   //          if active datafield and index match,
   //            return saved value, and return restored = TRUE.
   //          otherwise return restored = FALSE.
   //          if TRUE is returned, call the trap again.

static const WboxField *field_keep = NULL;
static int              index_keep;
static int              nmax_keep;
static int              n_keep;
static int              ivar_keep[2];
static float            fvar_keep[2];
static double           dvar_keep[2];
static char             cvar_keep[2][200];
static int              flip;

    /// later maybe allow this stuff to be saved separately
    /// for each windowbox, or even for each datafield?

void WboxControl::savePreviousValue()
{
  const char *textbuf = _text->getTextbuf();
  WboxField  *active  = _roll->getActiveFieldPointer();
  int uptype = active->getUptype();
  if(uptype == WboxVector::_INDEX || uptype == WboxVector::_RPOINT)
      {
      field_keep = NULL;
      return;
      }
  field_keep = active;
  index_keep = active->getIndex();
  nmax_keep  = active->getNmax();
  n_keep     = active->getN();
  flip = 0;
  active->getValue(&ivar_keep[flip], &fvar_keep[flip], &dvar_keep[flip],
                                                       &cvar_keep[flip][0]);
  flip = 1;
  active->decodeValue(textbuf, &ivar_keep[flip], &fvar_keep[flip],
                               &dvar_keep[flip], &cvar_keep[flip][0]);
  active->putUserValue(index_keep, ivar_keep[flip], fvar_keep[flip],
                                   dvar_keep[flip], &cvar_keep[flip][0]);
}



int WboxControl::restorePreviousValue
               (int *ivar, float *fvar, double *dvar, char *cvar)
{
  WboxField *active = _roll->getActiveFieldPointer();
  int        uptype = active->getUptype();
  if(uptype             == WboxVector::_INDEX  ||
     uptype             == WboxVector::_RPOINT ||
     active             != field_keep          ||
     active->getIndex() != index_keep          ||
     active->getNmax () != nmax_keep           ||
     active->getN    () != n_keep)
      {
      _messages->maybeShowMessage("previous value unchanged");
      return FALSE;
      }
  if(flip == 1) flip = 0;
  else          flip = 1;
  switch(active->getItype())
     {
     case WboxVector::_IVAR:        *ivar = ivar_keep[flip]    ; break;
     case WboxVector::_FVAR:        *fvar = fvar_keep[flip]    ; break;
     case WboxVector::_DVAR:        *dvar = dvar_keep[flip]    ; break;
     case WboxVector::_CVAR:  strcpy(cvar, &cvar_keep[flip][0]); break;
     default:                 assert(FALSE);
     }
  _messages->maybeShowMessage("previous value restored");
  return TRUE;
}



//------------------ restore previous user value -------------------//
//------------------ restore previous user value -------------------//
//------------------ restore previous user value -------------------//

  // public.
  // first restores (in the user area) the previous value.
  // then sets ident and index to the location of the value restored.
  // sets istat to  1 if the restoration is successful.
  // sets istat to -1 if the restoration is not successful.

  // normally to be called from a trap to get the previous value
  //   which existed before the value was changed to the new value.
  // this mechanism is needed only when a pointer to the value
  //   was registered with the windowbox routines.
  // the restoration will fail for the following reasons:
  //   (1) the windowbox does not match that in field_keep.
  //   (2) the variable is not being updated by pointer (_POINT).
  //   (3) the index, nmax, or n do not match those in field_keep.
  //   (4) we are not in a trap.
  // can be called more than once - each time, the variable is flipped.

void WboxControl::restorePreviousUserValue(int *ident, int *index, int *istat)
{
  *ident = 0;
  *index = 0;
  *istat = -1;
  if(field_keep == NULL) return;
  if(field_keep->getBoxPointer() != _box) return;
  if(field_keep->getUptype() != WboxVector::_POINT) return;
  if(field_keep->getIndex () != index_keep) return;
  if(field_keep->getN     () !=     n_keep) return;
  if(field_keep->getNmax  () !=  nmax_keep) return;
  if(_box->getAllboxPointer()->getTrapBoxNumber() != _box->getIbox()) return;
  if(flip == 1) flip = 0;
  else          flip = 1;
  field_keep->putUserValue(index_keep, ivar_keep[flip], fvar_keep[flip],
                                       dvar_keep[flip], &cvar_keep[flip][0]);
  *ident = field_keep->getIdent();
  *index = field_keep->getIndex();
  *istat = 1;
}



//----------------------- print debug ------------------------------//
//----------------------- print debug ------------------------------//
//----------------------- print debug ------------------------------//

        // private.

void WboxControl::printDebug(const char *endkey, const char *msg)
{
  if(_box->getAllboxPointer()->getDebug() >= 2)
      {
      printf("%6d %-10s %-8s %s\n",
          _box->getIbox(), _box->getBoxName(), endkey, msg);
      }
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

