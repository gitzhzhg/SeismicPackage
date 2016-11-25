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


//------------------------ sl_dialog.hh ---------------------//
//------------------------ sl_dialog.hh ---------------------//
//------------------------ sl_dialog.hh ---------------------//

//              header file for the SLDialog class
//            derived from the SLShellContainer class
//                        subdirectory sl


     // Call workArea() to let this class create and return an
     //   SLSmartForm work area.
     // Call setWorkArea(xxx) to give to this class any SLDelay object
     //   which is a child of this class to use as a work area.
     // Call workArea() to get the work area which was created
     //   earlier by either one of the above two calls.  This will
     //   not necessarily be an SLSmartForm if it had been supplied
     //   by a call to setWorkArea(xxx).

     // Attachments of work area, separator, and bottom area
     // are not finished until work area is either set or gotten,
     // by calling setWorkArea(xxx) or workArea().  Then the attachments
     // are done immediately if dialog shell is already made, or
     // otherwise later, when the dialog shell is made.  If the dialog
     // shell is already made, the above two functions will make the
     // work area right away if necessary.

     // The dialog shell is never made in the constructor.

#ifndef _SL_DIALOG_HH_
#define _SL_DIALOG_HH_

#include "sl/sl_shell_container.hh"
#include "sl/prim_support.hh"
#include "sl/attachment_list.hh"


class SLSmartForm;
class SLSep;
class SLpPush;

class SLDialog : public SLShellContainer
{

//------------------------ data --------------------------------//
//------------------------ data --------------------------------//
//------------------------ data --------------------------------//

private:

  SLDelay       *_work;        // object for work area.
  SLSep         *_sep;         // optional separator.
  SLSmartForm   *_bottom;      // form object for bottom buttons.
  char          *_title;
  int            _button;      // last button pressed.
  AttachmentList _attlist;     // linked list of attachment info.

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:

  SLDialog (SLDelay *slparent, char *name,
                         const HelpCtx hctx           = NULL,
                         const Boolean show_separator = FALSE,
                         const int     num_colors     = 0,
                         const int     may_icon       = UseResource,
                         const int     screen_number  = UseResource);
  SLDialog (Widget    wparent, char *name,
                         const HelpCtx hctx           = NULL,
                         const Boolean show_separator = FALSE,
                         const int     num_colors     = 0,
                         const int     may_icon       = UseResource,
                         const int     screen_number  = UseResource);
  virtual ~SLDialog ();

  void   constructorHelper (const Boolean show_separator);
  virtual void closing     () { unmanage(); }
  void         setTitle    (char *title);
  void         setWorkArea (SLDelay *work);
  SLSmartForm *workArea    ();
  SLSmartForm *bottomArea  ()  const  { return _bottom; }
  Boolean      isManaged   ();

private:

  SLpPush *privateAddBottomPush (char *name, char *label, long ident,
                                 AtrapFun *trap, void *data);

public:   // Add pushbuttons to bottom area.
          // These are convenience routines only.
          // You can add any SLDelay object to the bottom area directly.
          // If the AtrapFun function is not specified, SLDialog::notify
          //   will be called.  If not overridden, SLDialog::notify will
          //   then call one of the other ..notify methods below.

  SLpPush *addBottomPush   (char *name, long ident = 0,
                            AtrapFun *trap = NULL, void *data = NULL);
  SLpPush *addBottomOK     (long ident = 0,
                            AtrapFun *trap = NULL, void *data = NULL);
  SLpPush *addBottomApply  (long ident = 0,
                            AtrapFun *trap = NULL, void *data = NULL);
  SLpPush *addBottomCancel (long ident = 0,
                            AtrapFun *trap = NULL, void *data = NULL);
  SLpPush *addBottomRemove (long ident = 0,
                            AtrapFun *trap = NULL, void *data = NULL);
  SLpPush *addBottomUndo   (long ident = 0,
                            AtrapFun *trap = NULL, void *data = NULL);
  SLpPush *addBottomReset  (long ident = 0,
                            AtrapFun *trap = NULL, void *data = NULL);
  SLpPush *addBottomKeyhelp(char *helpname = NULL);
  SLpPush *addBottomHelp   (char *helpname = NULL);

public:  // instead of overriding these, override the protected methods below.

// if not managed, manage   calls preManageNotify.
// if     managed, unmanage calls preUnmanageNotify.
//                 update   calls updateNotify, then updateChildren.
//                 notify   calls various methods below.
//                 make     calls makeNotify.

  virtual void    manage  ();                 // overrides SLBase
  virtual void    unmanage();                 // overrides SLBase
  virtual void    update  ();                 // overrides SLDelay
  virtual Boolean notify  (SLPrim *gui);      // overrides SLDelay
  virtual Widget  make    (Widget p = NULL);  // overrides SLDelay

protected:  // override these in derived classes.

  // if preManageNotify   returns TRUE,
  //             this dialog pops up   and postManageNotify   is called.
  // if preUnmanageNotify returns TRUE,
  //             this dialog pops down and postUnmanageNotify is called.
  // if okNotify        returns TRUE, unmanage is called.
  // if cancelNotify    returns TRUE, unmanage is called.
  // if removeNotify    returns TRUE, unmanage is called.

  virtual Boolean preManageNotify    () { return TRUE; }
  virtual Boolean preUnmanageNotify  () { return TRUE; }
  virtual void    postManageNotify   () {}
  virtual void    postUnmanageNotify () {}

  // The following method need not be overridden unless you wish to create
  // your children in make().  Alternatively, you may create your children
  // in the derived constructor.  The make() method is never called from
  // this constructor, but it may be called, if required, from the
  // derived constructor.

  virtual void    makeNotify     ()              {}

  // Any SLPrim object in this dialog box may be updated within this
  // overridden method:

  virtual void    updateNotify   ()              {}

  // One of the following methods will be called from the notify()
  // method for any SLPrim object in this dialog box, if you register
  // this dialog box by calling its setNotify() method.  The first
  // six are called for bottom buttons of the same name.  The method
  // activateNotify() is called for all other pushbuttons and arrow
  // buttons (excluding the above-mentioned bottom buttons).  The
  // last four are called for the appropriate value-changed GUI.

  virtual Boolean okNotify       ()              { return TRUE; }
  virtual void    applyNotify    ()              {}
  virtual Boolean cancelNotify   ()              { return TRUE; }
  virtual Boolean removeNotify   ()              { return TRUE; }
  virtual void    undoNotify     ()              {}
  virtual void    resetNotify    ()              {}
  virtual void    activateNotify (long/*ident*/)    {}
  virtual Boolean longNotify  (long/*ident*/,long/*oldvar*/,long/*newvar*/)
                                                 { return TRUE; }
  virtual Boolean floatNotify (long/*ident*/,float/*oldvar*/,float/*newvar*/)
                                                 { return TRUE; }
  virtual Boolean doubleNotify(long/*ident*/,double/*oldvar*/, double/*newvar*/)
                                                 { return TRUE; }
  virtual Boolean charNotify  (long/*ident*/,char *,char *)
                                                 { return TRUE; }

  // The first three Boolean returns above indicate whether to call
  // unmanage().  The last four Booleans say whether to keep the
  // new value.

//------------------------ end of functions -------------------------//
//------------------------ end of functions -------------------------//
//------------------------ end of functions -------------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
