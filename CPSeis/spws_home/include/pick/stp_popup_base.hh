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

//------------------------ stp_popup_base.hh ---------------------//
//------------------------ stp_popup_base.hh ---------------------//
//------------------------ stp_popup_base.hh ---------------------//

//               header file for the StpPopupBase class
//                  derived from the SLDialog class
//                         subdirectory pick


#ifndef _STP_POPUP_BASE_HH_
#define _STP_POPUP_BASE_HH_

#include "sl/sl_dialog.hh"
#include "plot/pick_base.hh"
#include <X11/Intrinsic.h>

/*
typedef void PickingInformTrap (void *data, long ident, int picking);
*/

class StpPopupBase : public SLDialog
{
  friend class StpInform;
  friend class StpPick;

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

protected:

  enum Why { _STARTING, _UPDATE_REQUESTED, _STOPPING,
             _NEW_DISPLAY, _LEFT, _RIGHT, _END_DISPLAY,
             _NEW_DISPLAY_COMING };

private:

  class SeisWinMan         *_winman;       // new 9/4/97
/*
  class SeisPlot           *_sp;           // removed 9/4/97
*/
  class StpInform          *_inform;
  class StpPick            *_pick;
  class SeisVectLinkedList *_vect;        // new 9/2/97

  char         *_picking_mode;
  char         *_help_token;
  char         *_help_fallback;
  char         *_rub_color_name;
  Cursor        _cursor;
  int           _rub_line_width;

  Boolean   _picking;    // whether picking is in progress.
  Boolean   _changed;    // whether data has changed since last saved.


//--------- functions normally called from derived class --------//
//--------- functions normally called from derived class --------//
//--------- functions normally called from derived class --------//

protected:      // constructor

  StpPopupBase(SLDelay *slparent, char *name, HelpCtx hctx,
                        class SeisPlot    *sp,
                        const Boolean      want_update_button,
                        const Boolean      want_keyhelp_button,
                        const char * const picking_mode,
                        const char * const help_token,
                        const char * const help_fallback);

public:         // destructor

  virtual ~StpPopupBase();

public:   // must be called from destructor of derived class.
          // also called when the stop picking button is pressed.
          // also can be called by application.

  void stopPicking ();
  virtual void stopActivity() { stopPicking(); }  // overrides SLShellContainer

protected:   // set picking variables (optionally used)

  void setPickingColorName (const char * const rub_color_name);
  void setPickingCursor    (Cursor             cursor);
  void setPickingLineWidth (int                rub_line_width);
  void setHelpFallback     (const char * const help_fallback);
  void changeHelpToken     (const char * const help_token);


//------------------ virtual functions to override ---------------//
//------------------ virtual functions to override ---------------//
//------------------ virtual functions to override ---------------//

protected:     // called only by this base class

  virtual void    updateFile    (SeisPlot *, Why /*why*/) {}
  virtual void    updateVectors (SeisPlot *, Why /*why*/) = 0;

  virtual class SeisVectLinkedList *createVectors (SeisPlot *sp) = 0;
  virtual void                      deleteVectors () = 0;

  virtual class SLFilePairPlus *getFilePairPlus() = 0;

  virtual void becomingActive  () {}
  virtual void becomingInactive() {}

  class TpReferenceBase *_ref;

protected:        // called only by StpPick
            
  virtual Boolean verifyPickingAction (int /*button*/,
                                PickBase::Modifier /*modifier*/,
                                PickBase::Action  /*action*/)
                         { return TRUE; }

  virtual void pickingActionCompleted (SeisPlot *sp,
                                int button,
                                PickBase::Modifier modifier,
                                long direction,
                                long first_trace, long last_trace,
                                float first_time, float last_time) = 0;


//--------------------- other functions -----------------------//
//--------------------- other functions -----------------------//
//--------------------- other functions -----------------------//

public:       // convenience functions

  static void  holdAllVectors();
  static void flushAllVectors();

public:     // access to variables

  Boolean     haveDisplay       ()    const;
  Boolean     pickingInProgress ()    const  { return _picking; }
  Boolean     dataIsChanged     ()    const  { return _changed; }
/*
  SeisPlot   *getSeisPlot       ()    const  { return _sp; }
*/
  SeisPlot   *getSeisPlot       ()    const;
  SeisWinMan *getSeisWinMan     ()    const  { return _winman; }

private:

  void setChangedToTrue ();                 // called only by StpPick
  void newDisplay (SeisPlot *sp, Why why);  // called only by StpInform
  void endDisplay (SeisPlot *sp, Why why);  // called only by StpInform

   ////// the following three functions added 9/2/97:
  void addSeisPlot        (SeisPlot *sp);  // called only by StpInform
/*
  void newCurrentSeisPlot (SeisPlot *sp);  // called only by StpInform
*/
  void removeSeisPlot     (SeisPlot *sp);  // called only by StpInform

  Boolean startPicking ();  // called only when ok/apply button is pressed

  void    saveToFile   (SeisPlot *sp, Why why);

  virtual Boolean preManageNotify    ();    // overrides SLDialog
  virtual void    postUnmanageNotify ();    // overrides SLDialog

  static void okTrap            (void *data, long ident);
  static void applyTrap         (void *data, long ident);
  static void updateTrap        (void *data, long ident);
  static void endTrap           (void *data, long ident);

  static long okSenseUpfun      (void *data);
  static long updateSenseUpfun  (void *data);
  static long endSenseUpfun     (void *data);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
