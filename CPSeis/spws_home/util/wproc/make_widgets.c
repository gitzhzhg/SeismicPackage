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
/*
C      make_widgets.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y
C              written in c -- designed to be called from c
C
C     Utility Name:  make_widgets    (create widgets)
C          Written:  93/02/01  by:  Tom Stoeckley
C     Last revised:  93/06/04  by:  Tom Stoeckley
C
C  Purpose:  Shorthand routines to create widgets which respond to
C            user action or accept user input.  These routines also
C            provide additional functionality.
C
C  Note:     These routines add a special structure to the widget's
C            userData resource; therefore this resource is not available
C            to the user of widgets created by these routines.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/wproc   (shared)
C  library:                wproc.a            (shared)
C  header file:            wproc.h            (shared)
C  source file:            make_widgets.c
C
C  The user should include the above header file in his code.

C  documented functions:   (listed below)
C
C  static functions:  
C      cancel_callback       destroy_callback      modify_callback
C      text_callback         overview_callback
C      common_efforts        do_updates            help_text
C      make_scale2           make_text2            make_arrow2
C      make_toggle2          make_text3
C      update_push           update_radio          update_scale
C      update_arrow          update_ivar           update_fvar
C      update_dvar           update_cvar           update_label
C      call_update_function  startup_widget        modify_widget
C      focus_event_handler
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     wproc.a    cprim.a
C  header files:  wproc.h    cprim.h
C  functions:
C      overview_help           add_HELP
C      convert_ss2ii           convert_ss2ff           convert_ss2dd          
C      make_chain              insert_link             remove_link
C      first_link              next_link
C      safe_strcpy             safe_strcmp
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 93/06/04  Stoeckley  Change to call convert_ii2ss in cprim.a, and
C                            to use wproc.h.
C  1. 93/02/01  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C             DEFINITIONS OF ARGUMENTS USED REPEATEDLY
C       (those used in only a few functions are listed with the
C                documentation for those functions)
C
C  Widget             w = created widget (returned).
C  Widget        parent = parent of widget to create.
C  String          name = name of widget to create.
C  String         label = label or title to display in widget.
C  long             *sn = pointer to sensitivity variable in user area.
C  struct HELPCTX *hctx = pointer to help context (added to widget).
C  void           *data = pointer to user data (passed to user function).
C  void        (*fun)() = user-written function to call (see below).
C
C  Any of the arguments label, sn, hctx, data, and/or fun can be NULL.
C  The user-written function can be either a C routine or a Fortran routine.
C  The user-written function is called whenever the user activates the widget
C    or changes its value (depending on the type of widget).
C  If sn != NULL, the current value of *sn is used to set the
C    sensitivity of w at creation time, and whenever update_widgets()
C    is called.
C
C  sn >= -1 manages the widget if it is unmanaged.
C  sn >=  1 sets the widget sensitive.
C  sn ==  0 sets the widget insensitive (sensitive for label widget).
C  sn ==  0 sets the widget insensitive ( not dim  for  text widget).
C  sn == -1 sets the widget insensitive (   dim    for  text widget).
C  sn <= -2 unmanages the widget.
C-----------------------------------------------------------------------
C      ARGUMENT LISTS FOR USER-WRITTEN WIDGET-SPECIFIC FUNCTIONS
C
C       void fun(data, error,          sn)              push
C       void fun(data, error)                 pushquest,questbox
C       void fun(data, old_ivar, ivar, sn)              radio
C       void fun(data, old_ivar, ivar, sn, minv, maxv)  scale,arrow
C       void fun(data, old_ivar, ivar, sn)              itext
C       void fun(data, old_fvar, fvar, sn)              ftext
C       void fun(data, old_dvar, dvar, sn)              dtext
C       void fun(data, old_cvar, cvar, sn)              ctext,filetext
C       void fun(data, old_cvar, cvar)         pushfile,filebox
C       void fun(data,                 sn)              label
C
C  void   *data     = pointer to user data.
C  long   *error    = pointer to error preset to zero.
C  long   *old_ivar = pointer to previous value of modified variable.
C  float  *old_fvar = pointer to previous value of modified variable.
C  double *old_dvar = pointer to previous value of modified variable.
C  char   *old_cvar = pointer to previous value of modified variable.
C  long       *ivar = pointer to current value of variable.
C  float      *fvar = pointer to current value of variable.
C  double     *dvar = pointer to current value of variable.
C  char       *cvar = pointer to current value of variable.
C  long         *sn = pointer to sensitivity variable.
C  long       *minv = pointer to minimum value of *ivar.
C  long       *maxv = pointer to maximum value of *ivar.
C
C  These user-written functions are registered in calls to "make_..."
C    or "add_..._function".
C  The user-written function can be either a C or Fortran routine.
C    If it is a Fortran routine, remember that character strings are
C    null-terminated hollerith (integer) data which must be converted
C    to blank-filled character strings by calling convert_hh2cc (or
C    converted back by calling convert_cc2hh).
C  All pointers are to locations in temporary memory (except for data).
C  The previous value = the current value except for functions registered
C    in calls to "make_...".
C
C  Changing *error cancels a subsequent action to pop down a dialog box.
C    (This is relevant only for user-written functions registered by 
C    calls to "make_..." which are called when the user activates the 
C    widget, and only when the widget "wpop" has also been registered.)
C  Changing the previous value has no effect.
C  Changing the current value changes the value displayed in the widget.
C  Changing *sn changes the sensitivity of the widget.
C  Changing *minv or *maxv changes the permitted range for *ivar.
C
C  If ivar, fvar, dvar, cvar, sn, minv, or maxv is NOT a NULL pointer
C    in the "make_..." call, the following differences from the above
C    documentation occur for the affected variable:
C    - If the user-written function has been registered by a call to
C        "make_..." (and therefore is called when the widget is
C        activated or has its value changed), the value in the user
C        data will have already been changed to be the new value in 
C        the widget.
C    - Changing the current value will have no effect.
C    - Upon return from the user-written function, the value in the user
C        data will be used to set the value in the widget.
C    - The whole idea is that the widget values will always automatically
C        match the values in the user data, even as these values change,
C        without the programmer having to do anything; this will work 
C        even if you set the user-written function to NULL in the
C        "make_..." call!
C
C  The arguments for the user-written function can be omitted from the
C    right.  The order of the arguments has been chosen to make this a
C    convenient option.  If you register all the widget variables
C    (i.e. do not use NULL pointers) in the "make_..." call, you will 
C    need only the first two arguments.
C-----------------------------------------------------------------------
C  To update widgets:
C
C                 void update_widgets (void)
C
C  Updates the sensitivity and/or variable value of all widgets created
C    by calls to "make_...".
C  Automatically called after the user has activated, or changed the
C    value displayed in, any widget created by "make_...".
C  Also causes a call to all user-written functions registered by calls
C    to "add_verify_function".
C  This routine can be called from C or Fortran.
C-----------------------------------------------------------------------
C  To register user-written functions for updating widgets:
C
C          void add_update_function (updatefun, updatedata)
C
C  void (*updatefun)() = user function to call for additional updates.
C  void  *updatedata   = pointer to user data.
C
C      User function to write:  void updatefun(updatedata)
C
C  This registers a user-written function to be called just before every
C    call to update_widgets().
C  The user-written function can be written in C or Fortran.
C  More than one user-written function can be registered with repeated
C    calls to this routine.
C
C  The purpose of these user-written functions is to calculate any
C    variables (registered in calls to "make_...") which need to be
C    updated for display by the widget, when such values may depend on
C    other variables calculated in other parts of the program, and
C    when it is inconvenient to keep these variables up-to-date in other
C    parts of the program.  This is an alternative to the use of 
C    widget-specific user-written functions registered by calls to 
C    "add_verify_function".
C
C  Another purpose of these user-written functions is to update any
C    widgets which have not been created by calls to "make_...".
C-----------------------------------------------------------------------
C  To register additional widget-specific user-written functions:
C
C          void add_startup_function  (w, startupfun )
C          void add_verify_function   (w, verifyfun  )
C          void add_focusin_function  (w, focusinfun )
C          void add_focusout_function (w, focusoutfun)
C
C  Widget              w = widget created by a call to "make_...".
C  void (*startupfun )() = user function to be called at creation.
C  void (*verifyfun  )() = user function to be called for verification.
C  void (*focusinfun )() = user function to be called at focusin.
C  void (*focusoutfun)() = user function to be called at focusout.
C
C  The arguments for these user-written functions are the same as those
C    for the functions registered in the "make_..." call.
C
C  startupfun details:
C    The user-written function will be called right after the widget is 
C    created.  This is appropriate if you wish to determine the widget's 
C    values after creation (based upon its default values and/or values 
C    set in a resource file).
C
C  verifyfun details:
C    The user-written function will be called whenever the widget's 
C    value(s) or sensitivity need verification.  This happens whenever
C    update_widgets() is called.  This is appropriate when the value to 
C    be displayed in the widget changes based on other parts of the 
C    program, and the easiest way to calculate and display its value is 
C    to do it in the user-written function.
C
C  focusinfun details:
C    The user-written function will be called whenever the widget gets 
C    the keyboard focus.  This is appropriate if you need to take some 
C    action (such as highlighting a portion of a graph, or displaying 
C    a message) at this time.
C
C  focusoutfun details:
C    The user-written function will be called whenever the widget loses 
C    the keyboard focus.  This is appropriate if you need to take some 
C    action (such as removing a message) at this time.
C-----------------------------------------------------------------------
C  To put (or replace) user data into the widget for later retrieval:
C  To get (retrieve) user data from the widget:
C
C               void   put_user_data(w, data)
C               data = get_user_data(w)
C
C  Widget   w = widget in which the data pointer is stored.
C  void *data = pointer to user data (the same as in the "make_..." call).
C
C  The widget must have been created by one of the "make_..." routines.
C  This data (when putting) will replace any data previously stored by
C    the "make_..." routine which created the widget.
C  These convenience routines are required because the widget's
C    UserData resource is already being used by this utility, and 
C    should not be used directly by the programmer.  These convenience 
C    routines store the user data pointer in the structure which this 
C    utility in turn stores in the UserData resource.
C-----------------------------------------------------------------------
C                  ROUTINES WHICH CREATE WIDGETS
C-----------------------------------------------------------------------
C  To create a managed label widget:
C
C         w = make_label(parent, name, label, sn)
C
C     User function to write:   void fun(data,sn)
C-----------------------------------------------------------------------
C  To create a managed arrow widget:
C
C  w = make_uarrow (parent, name, sn, hctx, data, fun, ivar,minv,maxv)  up
C  w = make_darrow (parent, name, sn, hctx, data, fun, ivar,minv,maxv)  down
C  w = make_larrow (parent, name, sn, hctx, data, fun, ivar,minv,maxv)  left
C  w = make_rarrow (parent, name, sn, hctx, data, fun, ivar,minv,maxv)  right
C  w = make_harrow2(parent, name, sn, hctx, data, fun, ivar,minv,maxv)  pair 
C
C     User function to write:   void fun(data,old,ivar,sn,minv,maxv)
C
C  long     *ivar = pointer to current value in user area.
C  long     *minv = pointer to minimum value in user area.
C  long     *maxv = pointer to maximum value in user area.
C  long *new_ivar = pointer to new value selected by user.
C
C  Calls user-written function when activated.
C  *ivar (and *new_ivar) is any integer over the range *minv thru *maxv.
C  *ivar (and *new_ivar) are incremented if up or right arrow is pressed.
C  *ivar (and *new_ivar) are decremented if down or left arrow is pressed.
C  Arrow goes insensitive when *ivar is at the edge of its range.
C  If minv or maxv is NULL, *minv and *maxv in the user-written function will
C    always be preset so that *ivar is not at the edge of its range.
C  make_harrow2 makes a pair of left-right arrows in a rowcolumn;
C    the rowcolumn ID is returned.
C-----------------------------------------------------------------------
C  To create an unmanaged question dialog box:
C
C      w  = make_questbox(parent, name, label, hctx, data, fun, wpop) 
C
C     User function to write:   void fun(data,error)
C
C  Widget  wpop = widget to unmanage after successful user-written function.
C  long  *error = pointer to error preset to 0.
C
C  Sets dialogStyle to full application modal.
C  Pressing ok on the question dialog calls the user-written function.
C  Pressing ok or cancel on the question dialog pops it down.
C  Pressing help on the question dialog calls overview help.
C  error is an optional argument in the user-written routine, and is
C     useful only when wpop is not null.
C  The widget wpop is unmanaged (popped down) after returning
C     from user-written function, but only if *error is still 0.
C-----------------------------------------------------------------------
C  To create an unmanaged file selection dialog box:
C
C      w  = make_filebox(parent, name, label, hctx, data, fun, wtext) 
C
C     User function to write:   void fun(data,old,ivar)
C
C  Widget wtext    = text widget to receive file name (can be NULL).
C  char  *new_cvar = file name chosen by user.
C
C  Does file filter when managed.
C  Pressing ok on the file selection box puts file name into text widget
C    and calls the user-written function.
C  Pressing ok or cancel on the file selection box pops it down.
C  Pressing help on the file selection box calls overview help.
C-----------------------------------------------------------------------
C  To create a managed pushbutton:
C
C   w = make_pushhelp (parent, name, label, sn, hctx)
C   w = make_push     (parent, name, label, sn, hctx, data, fun, wpop)
C   w = make_pushquest(parent, name, label, sn, hctx, data, fun, wpop)
C   w = make_pushfile (parent, name, label, sn, hctx, data, fun, wtext)
C
C          User function to write:   void fun(data,error,sn)
C
C  Widget  wpop = widget to manage or unmanage.
C  Widget wtext = text widget to receive file name (can be NULL).
C  long  *error = pointer to error preset to 0.
C
C  Calls user-written function when activated.
C
C  error is an optional argument in the user-written routine, and is
C     useful only when wpop is not null.
C
C  make_pushhelp details:
C     Commonly used to create Help buttons in a pulldown menu or at the
C        bottom of a dialog box.
C     Displays overview help (using name of widget) when activated.
C     
C  make_push details:
C     Commonly used to create buttons in pulldown menus, dialog boxes,
C        and anyplace else where buttons are needed.
C     Calls user-written function when activated.
C     The widget wpop is unmanaged (popped down) after returning
C        from user-written function, but only if *error is still 0.
C     The widget wpop is commonly used while creating OK, Cancel, or
C        Remove buttons at the bottom of a dialog box, where wpop is 
C        the child of the shell.
C
C  make_pushquest details:
C     Commonly used instead of make_push when the user should verify 
C        the impending action before the user-written function is called.
C     This is a combination of make_push and make_questbox.
C     A question dialog box is also created.
C     The pushbutton ID is returned.
C     Pushbutton pops up question dialog if it is not managed.
C     Pushbutton pops down question dialog if it is already managed.
C     Pressing ok on the question dialog calls the user-written function.
C     Pressing ok or cancel on the question dialog pops it down.
C     Pressing help on the question dialog calls overview help.
C     The name of the question dialog is "questbox".
C     The widget wpop is unmanaged (popped down) after returning
C        from user-written function, but only if *error is still 0.
C-----------------------------------------------------------------------
C  To create a managed horizontal or vertical scale widget:
C
C w = make_hscale (parent, name, label, sn, hctx, data, fun, ivar, minv, maxv)
C w = make_vscale (parent, name, label, sn, hctx, data, fun, ivar, minv, maxv)
C w = make_hscale2(parent, name, label, sn, hctx, data, fun, ivar, minv, maxv)
C
C    User function to write:   void fun(data,old,ivar,sn,minv,maxv)
C
C  long     *ivar = pointer to current value in user area.
C  long     *minv = pointer to minimum value in user area.
C  long     *maxv = pointer to maximum value in user area.
C  long *new_ivar = pointer to new value selected by user.
C
C  Calls user-written function when value is changed.
C  *ivar (and *new_ivar) is any integer over the range *minv thru *maxv.
C  The new value *new_ivar passed to the user-written function can be reset in 
C     the user-written function.  The new value will be displayed in the widget
C     if ivar is NULL.
C  If ivar is not NULL, the current value of *ivar is:
C    - displayed in w at creation time;
C    - set to the new value *new_ivar before calling the user-written function;
C    - displayed in w after calling the user-written function, and whenever
C        update_widgets() is called.
C  make_hscale2 is the same as make_hscale except there are also arrows.
C  To omit the label (and save space), set label to zero length ("").
C-----------------------------------------------------------------------
C  To create a toggle widget or option button:
C
C   w = make_toggle(parent, name, label, sn, hctx, data, fun, ivar)
C   w = make_radio (parent, name, label, sn, hctx, data, fun, ivar, id)
C   w = make_opt   (parent, name, label, sn, hctx, data, fun, ivar, id)
C
C      User function to write:   void fun(data,old,ivar,sn)
C
C  long     *ivar = pointer to current value in user area.
C  long        id = ident of radio or option button (should not be zero).
C  long *new_ivar = pointer to new value selected by user.
C
C  Calls user-written function when value is changed.
C  For toggle buttons:
C     - the parent should not be a radiobox.
C     - *ivar (and new_ivar) is 1 (depressed) or 0 (otherwise).
C  For radio buttons:
C     - the parent should be a radiobox.
C     - *ivar (and new_ivar) is equal to id (depressed) or 0 (otherwise).
C     - all radio buttons in the same radiobox should have the same
C         addresses for fun and ivar, but separate values for id.
C  For option buttons:
C     - the parent must be an option menu created with make_option.
C     - *ivar (and new_ivar) is equal to id (if this was the last option
C         button activated) or 0 (otherwise).
C     - all option buttons in the same option menu should have the same
C         addresses for fun and ivar, but separate values for id.
C  The new value *new_ivar passed to the user-written function can be reset in 
C     the user-written function.  The new value will be displayed in the widget
C     if ivar is NULL.
C  If ivar is not NULL, the current value of *ivar is:
C    - displayed in w at creation time;
C    - set to the new value *new_ivar before calling the user-written function;
C    - displayed in w after calling the user-written function, and whenever
C        update_widgets() is called.
C-----------------------------------------------------------------------
C  To create a managed text widget:
C
C w = make_ctext   (parent,name,sn,hctx,data,fun, cvar, nchar,nvar) string
C w = make_filetext(parent,name,sn,hctx,data,fun, cvar, nchar,nvar) string
C w = make_itext   (parent,name,sn,hctx,data,fun, ivar, nchar)      long
C w = make_ftext   (parent,name,sn,hctx,data,fun, fvar, nchar,ndec) float
C w = make_dtext   (parent,name,sn,hctx,data,fun, dvar, nchar,ndec) double
C
C      User function to write:   void fun(data,old,cvar,sn)   string
C      User function to write:   void fun(data,old,ivar,sn)   long
C      User function to write:   void fun(data,old,fvar,sn)   float
C      User function to write:   void fun(data,old,dvar,sn)   double
C
C  char   *cvar = pointer to current      text        value in user area.
C  long   *ivar = pointer to current     integer      value in user area.
C  float  *fvar = pointer to current  floating point  value in user area.
C  double *dvar = pointer to current double precision value in user area.
C  long   nchar = number of characters to display in text widget.
C  long    nvar = number of characters in cvar.
C  long    ndec = maximum number of decimal digits to display.
C
C  char   *new_cvar = pointer to new      text        value typed in by user.
C  long   *new_ivar = pointer to new     integer      value typed in by user.
C  float  *new_fvar = pointer to new  floating point  value typed in by user.
C  double *new_dvar = pointer to new double precision value typed in by user.
C
C  make_filetext is a special version of make_ctext which expands the file
C      name typed in by the user.
C  If nchar > 0, XmNcolumns and XmNmaxLength are set to nchar.
C  If nvar = 0, cvar is null-terminated and of unspecified length.
C  If nvar > 0, cvar is null-terminated and of length nvar + room for '\0'.
C  If nvar < 0, cvar is blank-filled    and of length abs(nvar).
C  Calls user-written function when activated (return key pressed), and
C      also when losing focus (but only if value was modified).
C  For numeric fields (long, float, double), illegal keystrokes cause
C      the bell to ring.
C  For numeric fields, a blank field is interpreted as a nil value,
C      whose value can be set or retrieved by calling these routines: 
C           get_inil(long   *inil)     set_inil(long   *inil)
C           get_fnil(float  *fnil)     set_fnil(float  *fnil)
C           get_dnil(double *dnil)     set_dnil(double *dnil)
C  The new value passed to the user function can be reset in the user
C     function.  The new value will be displayed in the widget if the
C     pointer to the current value is NULL.
C  If the pointer to the current value is not NULL, the current value:
C    - is displayed in w at creation time;
C    - is set to the new value before calling the user-written function;
C    - is displayed in w after calling the user function, and whenever 
C        update_widgets() is called.
C-----------------------------------------------------------------------
C  To create a managed label/text widget combination:
C
C w = make_ctext2   (parent,name,label,sn,hctx,data,fun, cvar, nchar,nvar)
C w = make_ctext3   (parent,name,label,sn,hctx,data,fun, cvar, nchar,nvar)
C w = make_filetext2(parent,name,label,sn,hctx,data,fun, cvar, nchar,nvar)
C w = make_filetext4(parent,name,label,sn,hctx,data,fun, cvar, nchar,nvar)
C w = make_itext2   (parent,name,label,sn,hctx,data,fun, ivar, nchar)    
C w = make_itext3   (parent,name,label,sn,hctx,data,fun, ivar, nchar)    
C w = make_ftext2   (parent,name,label,sn,hctx,data,fun, fvar, nchar,ndec)
C w = make_ftext3   (parent,name,label,sn,hctx,data,fun, fvar, nchar,ndec)
C w = make_dtext2   (parent,name,label,sn,hctx,data,fun, dvar, nchar,ndec)
C w = make_dtext3   (parent,name,label,sn,hctx,data,fun, dvar, nchar,ndec)
C
C  These are identical to the same functions without the "2" suffix,
C    except that a label widget, followed by the text widget, is placed
C    into a horizontal rowcolumn widget.
C  The "3" suffix puts the text widget before the label widget.
C  The rowcolumn widget ID is returned.
C  For filetext4, the label widget is replaced by a pushbutton with a
C    file selection box popup.  The pushbutton precedes the text widget.
C  The supplied name is the name of the text widget.
C  The name of the rowcolumn widget   has "_rc" appended.
C  The name of the label widget       has "_lb" appended.
C  The name of the file selection box has "_fb" appended.
C  The name of the pushbutton widget  has "_pb" appended.
C-----------------------------------------------------------------------
C                                NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
*/



/*----------------------- header files ---------------------------------*/

#include <stdio.h>
#include <Xm/FileSB.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/ArrowB.h>
#include <Xm/Scale.h>
#include <Xm/Separator.h>
#include <X11/StringDefs.h>
#include "wproc.h"
#include "cprim.h"
#include "str.h"
#include "exptilde_crou.h"




/*--------------------- data declarations needed internally ------------*/
/*---------------------- not needed by users ---------------------------*/

static ChainStruct *chain   = NULL;
static ChainStruct *chainup = NULL;

enum { VAR_CHAR, VAR_FILE, VAR_LONG, VAR_FLOAT, VAR_DOUBLE,
       VAR_QUESTBOX, VAR_FILEBOX, VAR_LABEL, VAR_ARROWP, VAR_ARROWM,
       VAR_PUSH, VAR_OPT, VAR_SCALE, VAR_RADIO };

enum { WHY_MODIFY, WHY_STARTUP, WHY_VERIFY, WHY_FOCUSIN, WHY_FOCUSOUT };

typedef struct _UpdateStruct
{
  void (*updatefun)();
  void  *updatedata;
  LinkStruct link;
} UpdateStruct;

typedef struct _ShortStruct    /* allocated for each widget */
{
  Widget w, wtext, wpop;
  struct HELPCTX *hctx;
  void *data;
  void (*fun)();
  void (*startupfun)();
  void (*verifyfun)();
  void (*focusinfun)();
  void (*focusoutfun)();
  void (*crfun)();
  int vartype;
  long nvar, ndec;
  long *spoint;      /* pointer to  sensitivity  in user area */
  void *vpoint;      /* pointer to   variable    in user area */
  long *minpoint;    /* pointer to minimum value in user area */
  long *maxpoint;    /* pointer to maximum value in user area */
  long   sn;         /* local copy of sensitivity     */
  long   sn2;        /* local copy of another sensitivity for scale,arrow */
  long   ivar;       /* local copy of long   variable */
  float  fvar;       /* local copy of float  variable */
  double dvar;       /* local copy of double variable */
  char   cvar[200];  /* local copy of char   variable */
  long   minv;       /* local copy of minimum value for scale,arrow,scroll */
  long   maxv;       /* local copy of maximum value for scale,arrow,scroll */
  long   id;         /* ident for radio button */
  char new_text[200];/* copy of current text in text widget */
  char old_text[200];/* copy of previous text in text widget */
  LinkStruct link;
} ShortStruct;
   
#define WWNN       Widget parent, String name
#define LL         char   *label
#define SS         long   *sn
#define HH         struct HELPCTX *hctx
#define DDFF       void   *data, void (*fun)()
#define CVAR       char   *cvar
#define IVAR       long   *ivar
#define FVAR       float  *fvar
#define DVAR       double *dvar
 
  
#define ENTERLIST   WWNN, LL, SS, HH, DDFF
#define ENTERLIST2  WWNN,     SS, HH, DDFF
#define ARGLIST     parent, name, label, sn, hctx, data, fun
#define ARGLIST2    parent, name,        sn, hctx, data, fun

#define debug 0



/* ------------------------ put or get user data ---------------------------- */

void put_user_data(Widget w, void *data)
{
  ShortStruct *ss;
  XtVaGetValues(w, XmNuserData, &ss, NULL);
  ss->data = data;
}


void *get_user_data(Widget w)
{
  ShortStruct *ss;
  XtVaGetValues(w, XmNuserData, &ss, NULL);
  return ss->data;
}



/* --------- add a user function to be called prior to updating ------------- */

void add_update_function(void (*updatefun)(), void *updatedata)
{
  UpdateStruct *uu;

/*
  if(!chainup) chainup = MakeChain(UpdateStruct, link);
*/
  if(!chainup) chainup = make_chain(XtOffsetOf(UpdateStruct, link));
  
  uu = (UpdateStruct*)calloc(1,sizeof(UpdateStruct));
  uu->updatefun  = updatefun ;                    
  uu->updatedata = updatedata;                    
  insert_link(chainup, (void*)uu);
}



/* -------- call user update functions and then update widgets -------------- */

static void do_updates()
{
  UpdateStruct *uu;

  for( uu = (UpdateStruct*)first_link(chainup);  uu;
       uu = (UpdateStruct*) next_link(chainup, (void*)uu) )
    {
    if(uu->updatefun) uu->updatefun(uu->updatedata);
    }
  update_widgets();
}




/*------------- useful macros for below --------------------------------*/

#define STARTING ( why == WHY_STARTUP   )
#define MODIFIED ( why == WHY_MODIFY    )




/*------------- update pushbutton widget -------------------------------*/

static void update_push(int why, void (*fun)(), ShortStruct *ss)
{
  long snkeep, error = 0L;

  if(STARTING)
       {
       get_widget_sense (ss->w, &ss->sn);
       }
  snkeep = ss->sn;
  if(fun)
       {
       fun(ss->data, &error, &ss->sn);
       }
  if(ss->spoint) ss->sn = *ss->spoint;
  if(ss->sn != snkeep) set_widget_sense(ss->w, ss->sn);
  if (STARTING || MODIFIED)
       {
       if(ss->wpop && XtIsWidget(ss->wpop) && !error)
                                        XtUnmanageChild(ss->wpop);
       }
}



/*------------- update pushbutton widget in option menu ----------------*/

static void update_opt(int why, void (*fun)(), ShortStruct *ss)
{
  long snkeep, old, ivarkeep;

  if(STARTING)
       {
       get_widget_sense (ss->w, &ss->sn);
       if(ss->ivar == 0) ss->ivar = ss->id;
       if(ss->vpoint  ) ss->ivar = *(long*)ss->vpoint;
       }
  old = ss->ivar;
  if(MODIFIED)          
       {
       ss->ivar = ss->id;
       if(ss->vpoint && ss->ivar) *(long*)ss->vpoint = ss->ivar;
       }
  snkeep   = ss->sn;
  ivarkeep = ss->ivar;
  if(fun && (!MODIFIED || (ss->ivar != old) ) )
       {
       fun(ss->data, &old, &ss->ivar, &ss->sn);
       }
  if(ss->spoint  ) ss->sn   =        *ss->spoint;
  if(ss->vpoint  ) ss->ivar = *(long*)ss->vpoint;
  if(ss->sn   != snkeep  ) set_widget_sense(ss->w, ss->sn);
  if(ss->ivar != ivarkeep || STARTING) set_widget_opt(ss->w, ss->ivar, ss->id);
}




/*------------- update radio button widget -----------------------------*/

static void update_radio(int why, void (*fun)(), ShortStruct *ss)
{
  long snkeep, old, ivarkeep;

  if(STARTING)
       {
       get_widget_sense (ss->w, &ss->sn  );
       get_widget_radio (ss->w, &ss->ivar, ss->id);
       }
  old = ss->ivar;
  if(MODIFIED)          
       {
       get_widget_radio (ss->w, &ss->ivar, ss->id);
       if(ss->vpoint && ss->ivar) *(long*)ss->vpoint = ss->ivar;
       }
  snkeep   = ss->sn;
  ivarkeep = ss->ivar;
  if(fun && ss->ivar && (!MODIFIED || (ss->ivar != old) ) )
       {
       fun(ss->data, &old, &ss->ivar, &ss->sn);
       }
  if(ss->spoint  ) ss->sn   =        *ss->spoint;
  if(ss->vpoint  ) ss->ivar = *(long*)ss->vpoint;
  if(ss->sn   != snkeep  ) set_widget_sense (ss->w, ss->sn  );
  if(ss->ivar != ivarkeep) set_widget_radio (ss->w, ss->ivar, ss->id);
}





/*------------- update scale widget ------------------------------------*/

static void update_scale(int why, void (*fun)(), ShortStruct *ss)
{
  long sn2keep, old, ivarkeep, minvkeep, maxvkeep;

  if(STARTING)
       {
       get_widget_sense (ss->w, &ss->sn2 );
       get_widget_ivar  (ss->w, &ss->ivar);
       get_widget_minvar(ss->w, &ss->minv);
       get_widget_maxvar(ss->w, &ss->maxv);
       ss->sn = ss->sn2;
       }
  old = ss->ivar;
  if(MODIFIED)          
       {
       get_widget_ivar  (ss->w, &ss->ivar);
       if(ss->vpoint) *(long*)ss->vpoint = ss->ivar;
       }
  sn2keep  = ss->sn2;
  ivarkeep = ss->ivar;
  minvkeep = ss->minv;
  maxvkeep = ss->maxv;
  if(fun)
       {
       fun(ss->data, &old, &ss->ivar, &ss->sn, &ss->minv, &ss->maxv);
       }
  if(ss->spoint  ) ss->sn   =        *ss->spoint;
  if(ss->vpoint  ) ss->ivar = *(long*)ss->vpoint;
  if(ss->minpoint) ss->minv = *(long*)ss->minpoint;
  if(ss->maxpoint) ss->maxv = *(long*)ss->maxpoint;
  if(ss->minv > ss->ivar) ss->minv = ss->ivar;
  if(ss->maxv < ss->ivar) ss->maxv = ss->ivar;
  ss->sn2 = ss->sn;
  if(ss->sn2 >= 1 && ss->maxv <= ss->minv) ss->sn2 = -1;
  if(ss->sn2  != sn2keep ) set_widget_sense (ss->w, ss->sn2 );
/*
  if(ss->ivar != ivarkeep) set_widget_ivar  (ss->w, ss->ivar);
  if(ss->minv != minvkeep ||
     ss->maxv != maxvkeep)
       {
                                set_widget_minvar(ss->w, ss->minv);
       if(ss->maxv <= ss->minv) set_widget_maxvar(ss->w, ss->minv+1);
       else                     set_widget_maxvar(ss->w, ss->maxv  );
       }
*/
  if(ss->ivar != ivarkeep ||
     ss->minv != minvkeep ||
     ss->maxv != maxvkeep)
       set_widget_minmaxvar(ss->w, ss->minv, ss->maxv, ss->ivar);
}
    




/*------------- update arrow widget ------------------------------------*/

static void update_arrow(int why, void (*fun)(), ShortStruct *ss)
{
  long sn2keep, old;

  if(STARTING)
       {
       get_widget_sense (ss->w, &ss->sn2);
       ss->ivar = 1;
       ss->sn = ss->sn2;
       }
  old = ss->ivar;
  if(MODIFIED)          
       {
       if(ss->vartype == VAR_ARROWP) ss->ivar++;
       if(ss->vartype == VAR_ARROWM) ss->ivar--;
       if(ss->vpoint) *(long*)ss->vpoint = ss->ivar;
       }
  ss->minv = ss->ivar - 2;
  ss->maxv = ss->ivar + 2;
  sn2keep  = ss->sn2;
  if(fun)
       {
       fun(ss->data, &old, &ss->ivar, &ss->sn, &ss->minv, &ss->maxv);
       }
  if(ss->spoint  ) ss->sn   =        *ss->spoint;
  if(ss->vpoint  ) ss->ivar = *(long*)ss->vpoint;
  if(ss->minpoint) ss->minv = *(long*)ss->minpoint;
  if(ss->maxpoint) ss->maxv = *(long*)ss->maxpoint;
  ss->sn2 = ss->sn;
  if(ss->sn2 >= 1)
       {
       if(ss->vartype == VAR_ARROWP && ss->ivar >= ss->maxv) ss->sn2 = -1;
       if(ss->vartype == VAR_ARROWM && ss->ivar <= ss->minv) ss->sn2 = -1;
       }
  if(ss->sn2 != sn2keep)
       {
       set_widget_sense (ss->w, ss->sn2);
       }
}





/*------------- update widget with integer value -----------------------*/

static void update_ivar(int why, void (*fun)(), ShortStruct *ss)
{
  long snkeep, old, ivarkeep;

  if(STARTING)
       {
       get_widget_sense (ss->w, &ss->sn  );
       get_widget_ivar  (ss->w, &ss->ivar);
       }
  old = ss->ivar;
  if(MODIFIED)          
       {
       get_widget_ivar  (ss->w, &ss->ivar);
       if(ss->vpoint) *(long*)ss->vpoint = ss->ivar;
       }
  snkeep   = ss->sn;
  ivarkeep = ss->ivar;
  if(fun)
       {
       fun(ss->data, &old, &ss->ivar, &ss->sn);
       }
  if(ss->spoint  ) ss->sn   =        *ss->spoint;
  if(ss->vpoint  ) ss->ivar = *(long*)ss->vpoint;
  if(ss->sn   != snkeep  ) set_widget_sense (ss->w, ss->sn  );
  if(ss->ivar != ivarkeep) set_widget_ivar  (ss->w, ss->ivar);
}

 



/*------------- update widget with floating point value ----------------*/

static void update_fvar(int why, void (*fun)(), ShortStruct *ss)
{
  long snkeep;
  float old, fvarkeep;

  if(STARTING)
       {
       get_widget_sense(ss->w, &ss->sn  );
       get_widget_fvar (ss->w, &ss->fvar);
       }
  old = ss->fvar;
  if(MODIFIED)          
       {
       get_widget_fvar (ss->w, &ss->fvar);
       if(ss->vpoint) *(float*)ss->vpoint = ss->fvar;
       }
  snkeep   = ss->sn;
  fvarkeep = ss->fvar;
  if(fun)
       {
       fun(ss->data, &old, &ss->fvar, &ss->sn);
       }
  if(ss->spoint)           ss->sn   =         *ss->spoint;
  if(ss->vpoint)           ss->fvar = *(float*)ss->vpoint;
  if(ss->sn   != snkeep  ) set_widget_sense(ss->w, ss->sn  );
  if(ss->fvar != fvarkeep) set_widget_fvar (ss->w, ss->fvar, ss->ndec);
}

 



/*------------- update widget with double precision value --------------*/

static void update_dvar(int why, void (*fun)(), ShortStruct *ss)
{
  long snkeep;
  double old, dvarkeep;

  if(STARTING)
       {
       get_widget_sense (ss->w, &ss->sn  );
       get_widget_dvar  (ss->w, &ss->dvar);
       }
  old = ss->dvar;
  if(MODIFIED)          
       {
       get_widget_dvar  (ss->w, &ss->dvar);
       if(ss->vpoint) *(double*)ss->vpoint = ss->dvar;
       }
  snkeep   = ss->sn;
  dvarkeep = ss->dvar;
  if(fun)
       {
       fun(ss->data, &old, &ss->dvar, &ss->sn);
       }
  if(ss->spoint  ) ss->sn   =          *ss->spoint;
  if(ss->vpoint  ) ss->dvar = *(double*)ss->vpoint;
  if(ss->sn   != snkeep  ) set_widget_sense (ss->w, ss->sn  );
  if(ss->dvar != dvarkeep) set_widget_dvar  (ss->w, ss->dvar, ss->ndec);
}

 



/*------------- update widget with character string value --------------*/

static void update_cvar(int why, void (*fun)(), ShortStruct *ss)
{
  long snkeep;
  char old[200], cvarkeep[200];

  if(STARTING)
       {
       get_widget_sense (ss->w, &ss->sn  );
       get_widget_cvar  (ss->w,  ss->cvar);
       }
  safe_strcpy(old, ss->cvar);
  if(MODIFIED)          
       {
       get_widget_cvar  (ss->w, ss->cvar);
       if(ss->vpoint) string_copy(ss->vpoint, ss->nvar, ss->cvar, 0);
       }
  snkeep   = ss->sn;
  safe_strcpy(cvarkeep, ss->cvar);
  if(fun)
       {
       fun(ss->data, old, ss->cvar, &ss->sn);
       }
  if(ss->spoint)             ss->sn  =   *ss->spoint;
  if(ss->vpoint) string_copy(ss->cvar, 0, ss->vpoint, ss->nvar);
  if(ss->sn != snkeep) set_widget_sense(ss->w, ss->sn);
  if(safe_strcmp(ss->cvar, cvarkeep))
       {
       set_widget_cvar(ss->w, ss->cvar);
       }
  if (STARTING || MODIFIED)
       {
       if(ss->wtext && XtIsWidget(ss->wtext))
                                set_widget_cvar(ss->wtext, ss->cvar);
       }
}

 



/*------------- update label widget ------------------------------------*/

static void update_label(int why, void (*fun)(), ShortStruct *ss)
{
  long snkeep;

  if(STARTING)
       {
       get_widget_sense (ss->w, &ss->sn);
       }
  snkeep = ss->sn;
  if(fun)
       {
       fun(ss->data, &ss->sn);
       }
  if(ss->spoint) ss->sn = *ss->spoint;
  if(ss->sn != snkeep) set_widget_sense (ss->w, ss->sn);
}





/*-------------- call user function ------------------------------------*/

static void call_update_function(int why, void (*fun)(), ShortStruct *ss)
{
  switch(ss->vartype)
      {
      case VAR_PUSH:      update_push (why, fun, ss);  break;
      case VAR_OPT:       update_opt  (why, fun, ss);  break;
      case VAR_LONG:      update_ivar (why, fun, ss);  break;
      case VAR_FLOAT:     update_fvar (why, fun, ss);  break;
      case VAR_DOUBLE:    update_dvar (why, fun, ss);  break;
      case VAR_CHAR:      update_cvar (why, fun, ss);  break;
      case VAR_FILE:      update_cvar (why, fun, ss);  break;
      case VAR_FILEBOX:   update_cvar (why, fun, ss);  break;
      case VAR_QUESTBOX:  update_push (why, fun, ss);  break;
      case VAR_RADIO:     update_radio(why, fun, ss);  break;
      case VAR_ARROWP:    update_arrow(why, fun, ss);  break;
      case VAR_ARROWM:    update_arrow(why, fun, ss);  break;
      case VAR_SCALE:     update_scale(why, fun, ss);  break;
      case VAR_LABEL:     update_label(why, fun, ss);  break;
      }
}


static void startup_widget(ShortStruct *ss)
{
  call_update_function(WHY_STARTUP, ss->startupfun, ss);
  do_updates();
}


static void modify_widget(ShortStruct *ss)
{
  call_update_function(WHY_MODIFY, ss->fun, ss);
  do_updates();
}


static void modify_callback(Widget w, ShortStruct *ss, void *call)
{
  modify_widget(ss);
}


static void focus_event_handler(Widget w, ShortStruct *ss, XEvent *event)
{
  switch(event->type)
      {
      case FocusIn: if(ss->focusinfun) 
          call_update_function(WHY_FOCUSIN , ss->focusinfun  ,ss);
          do_updates();
          break;
      case FocusOut: if(ss->focusoutfun)
          call_update_function(WHY_FOCUSOUT, ss->focusoutfun, ss);
          do_updates();
          break;
      }
}



/*------------- update all widgets -------------------------------------*/
       /* uses global linked chain of ShortStruct */

void update_widgets(void)
{
  ShortStruct *ss;
  Boolean function_called = False;

  for( ss = (ShortStruct*)first_link(chain);  ss;
       ss = (ShortStruct*) next_link(chain, (void*)ss) )
    {
    call_update_function(WHY_VERIFY, ss->verifyfun, ss);
    if(ss->verifyfun) function_called = True;
    }
    if(!function_called) return;
  for( ss = (ShortStruct*)first_link(chain);  ss;
       ss = (ShortStruct*) next_link(chain, (void*)ss) )
    {
    call_update_function(WHY_VERIFY, NULL, ss);
    }
}



/*---------------- various small callbacks -----------------------------*/

static void cancel_callback (Widget w, ShortStruct *ss, void *call)
{
  XtUnmanageChild(w);
}




static void destroy_callback (Widget w, ShortStruct *ss, void *call)
{
  remove_link(chain, (void*)ss);
  free(ss);
}




/*------------- allocate (and partially initialize) shortstruct --------*/
     /* creates linked chain first time */
     /* also manage widget, add structure to user data, add help
        to widget, and add destroy callback to widget */

static ShortStruct *common_efforts(Widget w, char *label, SS, HH, DDFF,
                                                           int vartype)
{
  ShortStruct *ss;

  if(!chain) chain = make_chain(XtOffsetOf(ShortStruct, link));
  
  ss = (ShortStruct*)calloc(1,sizeof(ShortStruct));
  ss->w           = w   ; 
  ss->spoint      = sn  ;
  ss->hctx        = hctx;
  ss->data        = data;
  ss->fun         = fun ;
  ss->vartype     = vartype;
  ss->new_text[0] = '\0';
  ss->old_text[0] = '\0';
  insert_link(chain, (void*)ss);
  XtVaSetValues(w, XmNuserData, ss, NULL);
  XtAddCallback(w, XmNdestroyCallback,
                         (XtCallbackProc)destroy_callback, (XtPointer)ss);
  if(label) set_widget_cvar(w, label);

  if(vartype != VAR_QUESTBOX && vartype != VAR_FILEBOX)
       {
       XtManageChild(w);
       if(hctx) add_HELP(w, helper, hctx);
       }
  return ss;
}



/*--------- add user-written function to widget ------------------------*/

void add_startup_function(Widget w, void (*startupfun)())
{
  ShortStruct *ss;
  XtVaGetValues(w, XmNuserData, &ss, NULL);
  ss->startupfun = startupfun;
}

void add_verify_function(Widget w, void (*verifyfun)())
{
  ShortStruct *ss;
  XtVaGetValues(w, XmNuserData, &ss, NULL);
  ss->verifyfun = verifyfun;
}

void add_focusin_function(Widget w, void (*focusinfun)())
{
  ShortStruct *ss;
  XtVaGetValues(w, XmNuserData, &ss, NULL);
  ss->focusinfun = focusinfun;
  if(!ss->focusoutfun)
     XtAddEventHandler (w, FocusChangeMask, FALSE,
                    (XtEventHandler)focus_event_handler, (XtPointer)ss);
}

void add_focusout_function(Widget w, void (*focusoutfun)())
{
  ShortStruct *ss;
  XtVaGetValues(w, XmNuserData, &ss, NULL);
  ss->focusoutfun = focusoutfun;
  if(!ss->focusinfun)
     XtAddEventHandler (w, FocusChangeMask, FALSE, 
                    (XtEventHandler)focus_event_handler, (XtPointer)ss);
}





/*--------- callback to display overview help --------------------------*/
/*----- overview help is identified by name of widget and parents ------*/

static void overview_callback (Widget w, ShortStruct *ss, void *call)
{
  char name[200];

  if (ss->hctx)
       {
       get_full_name (w, name);
       overview_help(name, ss->hctx);
       }
}



/*---------------- undo action for text widgets ------------------------*/

static void undo_action(Widget w, XEvent *event, String *params,
                                                   Cardinal *num_params)
{
  char *text;
  ShortStruct *ss;

  XtVaGetValues(w, XmNuserData, &ss, NULL);

  set_widget_cvar(ss->w, ss->old_text);
  modify_widget(ss);
  text = XmTextGetString(w);
  if(safe_strcmp(text, ss->new_text))
     {
     safe_strcpy(ss->old_text, ss->new_text);
     safe_strcpy(ss->new_text, text);
     }
  XtFree(text);
}


static void add_undo_action(Widget w)
{
  static XtActionsRec actions_table[] = {
            { "UndoAction", (XtActionProc)undo_action },
           };
  static char default_translations[] =
             "Any<Key>Menu  : UndoAction(D) \n\
              Ctrl<Key>D    : UndoAction(D)";
  XtAppContext app_context;
  static XtTranslations trans_table;
  static Boolean start = TRUE;

  if(start)
       {
       app_context = XtWidgetToApplicationContext(w);
       XtAppAddActions(app_context, actions_table, XtNumber(actions_table));
       trans_table = XtParseTranslationTable(default_translations);
       start = FALSE;
       }
  XtOverrideTranslations(w, trans_table);
}




/*-------- callback for text widget created by make_text... ------------*/
/*-- calls user-written function upon activate -------------------------*/
/*-- calls user-written function upon losing focus (if value changed) --*/

static void text_callback (Widget w, ShortStruct *ss,
                                       XmTextVerifyCallbackStruct *call)
{
  size_t lentext;
  char *text, *selection;
  static char original_cvar[200];
  static Boolean changed, restore;
  char new_cvar[200];
  int new_ivar, istat;    float new_fvar;    double new_dvar;

  if(!call->event) return;
  switch(call->reason)
    {
    case XmCR_MODIFYING_TEXT_VALUE:
        text = XmTextGetString(w);
        lentext = strlen(text);
        memcpy(new_cvar, text, call->startPos);
        memcpy(new_cvar + call->startPos, call->text->ptr, call->text->length);
        memcpy(new_cvar + call->startPos + call->text->length,
                   text + call->endPos, lentext - call->endPos);
        new_cvar[call->startPos + call->text->length + lentext - call->endPos]
                                                         = '\0';
        selection = XmTextGetSelection(w);
        if(debug) printf(
               "[ModVal]  original: %s  current: %s  new: %s  selection: %s\n",
               original_cvar, text, new_cvar, selection);
        XtFree(text);
/*
        if(original_cvar[0] && !new_cvar[0] && !selection)
           {
           restore = TRUE; return;
           }
*/
        if(selection) XtFree(selection);
        switch(ss->vartype)
           {
/*
           case VAR_LONG  : convert_ss2ii(new_cvar, &new_ivar, &istat); break;
           case VAR_FLOAT : convert_ss2ff(new_cvar, &new_fvar, &istat); break;
           case VAR_DOUBLE: convert_ss2dd(new_cvar, &new_dvar, &istat); break;
*/
           case VAR_LONG  : str_ss2ii(new_cvar, &new_ivar, &istat); break;
           case VAR_FLOAT : str_ss2ff(new_cvar, &new_fvar, &istat); break;
           case VAR_DOUBLE: str_ss2dd(new_cvar, &new_dvar, &istat); break;
           default: istat = 1; break;
           }
        if (istat == -1) call->doit = FALSE;
        break;

    case XmCR_VALUE_CHANGED:
        text = XmTextGetString(w);
/*
        if(restore && !text[0]) XmTextSetString(w, original_cvar);
*/
        if(debug) printf(
               "[VChngd]  original: %s  current: %s  restore: %d\n",
               original_cvar, text, restore);
        XtFree(text);
        changed = TRUE;
        restore = FALSE;
        break;

    case XmCR_FOCUS:
        text = XmTextGetString(w);
        safe_strcpy(original_cvar, text);
        if(debug) printf(
               "[Focus]  original and current: %s\n",
               original_cvar);
        XmTextSetSelection(w, 0, strlen(text), (Time)0);
        XmTextSetInsertionPosition(w, strlen(text));
        XtFree(text);
        changed = FALSE;
        restore = FALSE;
        break;

    case XmCR_LOSING_FOCUS:
        XmTextClearSelection(w, (Time)0);
        if(!changed) break;

    case XmCR_ACTIVATE:
        text = XmTextGetString(w);
        switch(ss->vartype)
           {
           case VAR_CHAR:
               break;
           case VAR_FILE:
               exptilde_crou2(new_cvar, text);
               set_widget_cvar(ss->w, new_cvar);
               break;
           case VAR_LONG:
/*
               convert_ss2ii(text, &new_ivar, &istat);
*/
               str_ss2ii(text, &new_ivar, &istat);
               set_widget_ivar(ss->w,  new_ivar);
               break;
           case VAR_FLOAT:
/*
               convert_ss2ff(text, &new_fvar, &istat);
*/
               str_ss2ff(text, &new_fvar, &istat);
               set_widget_fvar(ss->w,  new_fvar, ss->ndec);
               break;
           case VAR_DOUBLE:
/*
               convert_ss2dd(text, &new_dvar, &istat);
*/
               str_ss2dd(text, &new_dvar, &istat);
               set_widget_dvar(ss->w,  new_dvar, ss->ndec);
               break;
           }
        XtFree(text);
        modify_widget(ss);
        text = XmTextGetString(w);
        if(safe_strcmp(text, ss->new_text))
           {
           safe_strcpy(ss->old_text, ss->new_text);
           safe_strcpy(ss->new_text, text);
           }
        XtFree(text);
        changed = FALSE;
    }
}





/*------------- routine for creating a managed label widget ------------*/

Widget make_label(WWNN, LL, SS)
{
  Widget w;
  ShortStruct *ss;

  w = XmCreateLabel(parent, name, NULL, 0);

  ss = common_efforts(w, label, sn, NULL, NULL, NULL, VAR_LABEL);
  startup_widget(ss);
  return w;
}




/*------------- routines for creating an arrow -------------------------*/

static Widget make_arrow2(ENTERLIST2, IVAR, long *minv, long*maxv,
                                long vartype, unsigned char direction)
{
  Widget w;
  int i;
  Arg args[5];
  ShortStruct *ss;

  i=0;
  XtSetArg(args[i], XmNshadowThickness, 0); i++;
  XtSetArg(args[i], XmNwidth,          30); i++;
  XtSetArg(args[i], XmNheight,         30); i++;
  XtSetArg(args[i], XmNarrowDirection, direction); i++;
  w = XmCreateArrowButton(parent, name, args, i);

  ss = common_efforts(w, NULL, sn, hctx, data, fun, vartype);
  ss->vpoint   = (void*)ivar ;                    
  ss->minpoint = minv;                    
  ss->maxpoint = maxv;                    
  XtAddCallback(w, XmNactivateCallback,
                (XtCallbackProc)modify_callback, (XtPointer)ss);
  startup_widget(ss);
  return w;
}

Widget make_uarrow(ENTERLIST2, IVAR, long *minv, long *maxv)
{ return make_arrow2(ARGLIST2, ivar, minv, maxv, VAR_ARROWP, XmARROW_UP); }

Widget make_darrow(ENTERLIST2, IVAR, long *minv, long *maxv)
{ return make_arrow2(ARGLIST2, ivar, minv, maxv, VAR_ARROWM, XmARROW_DOWN); }

Widget make_larrow(ENTERLIST2, IVAR, long *minv, long *maxv)
{ return make_arrow2(ARGLIST2, ivar, minv, maxv, VAR_ARROWM, XmARROW_LEFT); }

Widget make_rarrow(ENTERLIST2, IVAR, long *minv, long *maxv)
{ return make_arrow2(ARGLIST2, ivar, minv, maxv, VAR_ARROWP, XmARROW_RIGHT); }

Widget make_harrow2(ENTERLIST2, long *ivar, long *minv, long *maxv)
{
  Widget w;
  char rname[80],dname[80],uname[80];

  strcpy(rname, name);  strcat(rname, "_r");
  strcpy(dname, name);  strcat(dname, "_d");
  strcpy(uname, name);  strcat(uname, "_u");
  w = make_row(parent, rname);
  XmRemoveTabGroup(w);
  make_larrow (w     , dname,      sn,hctx,data,fun,ivar,minv,maxv);
  make_rarrow (w     , uname,      sn,hctx,data,fun,ivar,minv,maxv);
  return w;
}





 
/*---------------- create an unmanaged question dialog box -------------*/

Widget make_questbox(WWNN, LL, HH, DDFF, Widget wpop)
{
  Widget w;
  ShortStruct *ss;

  w = XmCreateQuestionDialog(parent, name, NULL, 0);
  XtVaSetValues(w, XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL, NULL);
  if(label) set_widget_cvar(XtParent(w), label);

  ss = common_efforts(w, NULL, NULL, hctx, data, fun, VAR_QUESTBOX);
  ss->wpop   = wpop;
  XtAddCallback(w, XmNokCallback      , 
                    (XtCallbackProc)modify_callback, (XtPointer)ss);
  XtAddCallback(w, XmNokCallback      , 
                    (XtCallbackProc)cancel_callback, (XtPointer)ss);
  XtAddCallback(w, XmNcancelCallback  , 
                    (XtCallbackProc)cancel_callback, (XtPointer)ss);
  XtAddCallback(w, XmNhelpCallback    , 
                    (XtCallbackProc)overview_callback, (XtPointer)ss);
/*  startup_widget(ss); */
  return w;
}



 
/*------------- create an unmanaged file selection dialog box ----------*/

Widget make_filebox(WWNN, LL, HH, DDFF, Widget wtext)
{
  Widget w;
  ShortStruct *ss;

  w = XmCreateFileSelectionDialog(parent, name, NULL, 0);
  if(label) set_widget_cvar(XtParent(w), label);

  ss = common_efforts(w, NULL, NULL, hctx, data, fun, VAR_FILEBOX);
  ss->wtext = wtext;
  XtAddCallback(w, XmNokCallback      , 
                    (XtCallbackProc)modify_callback, (XtPointer)ss);
  XtAddCallback(w, XmNokCallback      , 
                    (XtCallbackProc)cancel_callback, (XtPointer)ss);
  XtAddCallback(w, XmNcancelCallback  , 
                    (XtCallbackProc)cancel_callback, (XtPointer)ss);
  XtAddCallback(w, XmNhelpCallback    , 
                    (XtCallbackProc)overview_callback, (XtPointer)ss);
/*  startup_widget(ss); */
  return w;
}
 
 
 
 
/*------------ routines for creating a managed pushbutton --------------*/

Widget make_pushhelp(WWNN, LL, SS, HH)
{
  Widget w;
  int i;
  Arg args[5];
  ShortStruct *ss;

  i=0;
  XtSetArg(args[i], XmNmarginHeight  , 0  ); i++;
  w = XmCreatePushButton(parent, name, args, i);

  ss = common_efforts(w, label, sn, hctx, NULL, NULL, VAR_PUSH);
  XtAddCallback(w, XmNactivateCallback, 
                    (XtCallbackProc)overview_callback, (XtPointer)ss);
  startup_widget(ss);
  return w;
}

Widget make_push(ENTERLIST, Widget wpop)
{
  Widget w;
  int i;
  Arg args[5];
  ShortStruct *ss;

  i=0;
  XtSetArg(args[i], XmNmarginHeight  , 0  ); i++;
  w = XmCreatePushButton(parent, name, args, i);

  ss = common_efforts(w, label, sn, hctx, data, fun, VAR_PUSH);
  ss->wpop = wpop;
  XtAddCallback(w, XmNactivateCallback, 
                    (XtCallbackProc)modify_callback, (XtPointer)ss);
  startup_widget(ss);
  return w;
}


 
 
/*------------ create a managed pushbutton in an option menu -----------*/

Widget make_opt(ENTERLIST, long *ivar, long id)
{
  Widget w;
  int i;
  Arg args[5];
  ShortStruct *ss;

  i=0;
  XtSetArg(args[i], XmNmarginHeight  , 0  ); i++;
  w = XmCreatePushButton(parent, name, args, i);

  ss = common_efforts(w, label, sn, hctx, data, fun, VAR_OPT);
  ss->vpoint = (void*)ivar;                    
  ss->id     = id;                    
  XtAddCallback(w, XmNactivateCallback, 
                    (XtCallbackProc)modify_callback, (XtPointer)ss);
  startup_widget(ss);
  return w;
}




/*------ routine for creating a pushbutton which pops up a questbox ----*/
        /* also creates the question box                           */
        /* pressing ok on questbox calls user-written function             */
        /* pressing ok or cancel on questbox pops down questbox    */
        /* pressing help on questbox calls overview help           */
        /* name of questbox is "questbox"                          */
        /* pushbutton widget id is returned */

Widget make_pushquest(ENTERLIST, Widget wpop)
{
  Widget w, wbox;

  wbox = make_questbox(parent, "questbox", label,     hctx, data, 
                fun, wpop);
  w    = make_push    (parent,    name   , label, sn, hctx, wbox, 
/*              manage_widget, NULL); */
  (void (*)())  manage_widget, NULL);	/* 18May94 --- ehs */
  return w;
}





/*------ routine for creating a pushbutton which pops up a filebox -----*/
        /* also creates the file selection box                    */
        /* pressing ok on filebox calls user-written function             */
        /* pressing ok or cancel on filebox pops down filebox     */
        /* pressing help on filebox calls overview help           */
        /* name of filebox is "filebox"                           */
        /* pushbutton widget id is returned */

Widget make_pushfile(ENTERLIST, Widget wtext)
{
  Widget w, wbox;

  wbox = make_filebox(parent, "filebox", label,     hctx, data, 
               fun, wtext);
  w    = make_push   (parent,   name   , label, sn, hctx, wbox, 
/*             manage_widget, NULL); */
  (void (*)()) manage_widget, NULL);	/* 18May94 --- ehs */
  return w;
}




/*------- routines for creating a managed scale widget -----------------*/

static Widget make_scale2(ENTERLIST, long *ivar, long *minv, long *maxv,
                                            unsigned char orientation)
{
  Widget w;
  int i;
  Arg args[5];
  ShortStruct *ss;

  i=0;
  XtSetArg(args[i], XmNshowValue  , True       ); i++;
  XtSetArg(args[i], XmNorientation, orientation); i++;
  w = XmCreateScale(parent, name, args, i);

  ss = common_efforts(w, label, sn, hctx, data, fun, VAR_SCALE);
  ss->vpoint   = (void*)ivar ;                    
  ss->minpoint = minv;                    
  ss->maxpoint = maxv;                    
  XtAddCallback(w, XmNvalueChangedCallback, 
                    (XtCallbackProc)modify_callback, (XtPointer)ss);
  startup_widget(ss);
  return w;
}

Widget make_hscale(ENTERLIST, long *ivar, long *minv, long *maxv)
{ return make_scale2(ARGLIST, ivar, minv, maxv, XmHORIZONTAL); }

Widget make_vscale(ENTERLIST, long *ivar, long *minv, long *maxv)
{ return make_scale2(ARGLIST, ivar, minv, maxv, XmVERTICAL); }

Widget make_hscale2(ENTERLIST, long *ivar, long *minv, long *maxv)
{
  Widget w;
  char rname[80],dname[80],uname[80];

  strcpy(rname, name);  strcat(rname, "_r");
  strcpy(dname, name);  strcat(dname, "_d");
  strcpy(uname, name);  strcat(uname, "_u");
  w = make_row(parent, rname);
  XmRemoveTabGroup(w);
  make_larrow (w     , dname,      sn,hctx,data,fun,ivar,minv,maxv);
  make_hscale (w     ,  name,label,sn,hctx,data,fun,ivar,minv,maxv);
  make_rarrow (w     , uname,      sn,hctx,data,fun,ivar,minv,maxv);
  return w;
}




/*------------- routine for creating a managed toggle button -----------*/

static Widget make_toggle2(ENTERLIST, long *ivar, long id, long vartype)
{
  Widget w;
  int i;
  Arg args[5];
  ShortStruct *ss;

  i=0;
  XtSetArg(args[i], XmNmarginHeight  , 0  ); i++;
  XtSetArg(args[i], XmNmarginTop     , 0  ); i++;
  XtSetArg(args[i], XmNmarginBottom  , 0  ); i++;
  w = XmCreateToggleButton(parent, name, args, i);

  ss = common_efforts(w, label, sn, hctx, data, fun, vartype);
  ss->vpoint = (void*)ivar;                    
  ss->id     = id;                    
  XtAddCallback(w, XmNvalueChangedCallback, 
                    (XtCallbackProc)modify_callback, (XtPointer)ss);
  startup_widget(ss);
  return w;
}

Widget make_toggle(ENTERLIST, long *ivar)
{ return make_toggle2(ARGLIST, ivar, 0L, VAR_LONG); }

Widget make_radio(ENTERLIST, long *ivar, long id)
{ return make_toggle2(ARGLIST, ivar, id, VAR_RADIO); }





/*------------ routines for creating a single-line text widget ---------*/

static Widget make_text2(ENTERLIST2, void *var, long nchar, long nvar,
                                             long ndec, int vartype)
{
  Widget w;
  int i;
  Arg args[5];
  ShortStruct *ss;

  i=0;
  if(nchar > 0)
       {
       XtSetArg(args[i], XmNcolumns  , (short)nchar); i++;
       XtSetArg(args[i], XmNmaxLength, (int)nchar  ); i++;
       }
  XtSetArg(args[i], XmNmarginHeight  , 0  ); i++;
  w = XmCreateText(parent, name, args, i);

  ss = common_efforts(w, NULL, sn, hctx, data, fun, vartype);
  ss->vpoint   = var ; 
  ss->nvar     = nvar;
  ss->ndec     = ndec;
  XtAddCallback(w, XmNactivateCallback    , 
                    (XtCallbackProc)text_callback   , (XtPointer)ss);
  XtAddCallback(w, XmNfocusCallback       , 
                    (XtCallbackProc)text_callback   , (XtPointer)ss);
  XtAddCallback(w, XmNlosingFocusCallback , 
                    (XtCallbackProc)text_callback   , (XtPointer)ss);
  XtAddCallback(w, XmNmodifyVerifyCallback, 
                    (XtCallbackProc)text_callback   , (XtPointer)ss);
  XtAddCallback(w, XmNvalueChangedCallback, 
                    (XtCallbackProc)text_callback   , (XtPointer)ss);
  add_change_case_actions(w);
  add_undo_action(w);
  XmRemoveTabGroup(w);
  startup_widget(ss);
  return w;
}

Widget make_ctext(ENTERLIST2, char *cvar, long nchar, long nvar)
{ return make_text2(ARGLIST2, (void*)cvar, nchar,nvar,0L  ,VAR_CHAR); }

Widget make_filetext(ENTERLIST2, char *cvar, long nchar, long nvar)
{ return make_text2(ARGLIST2, (void*)cvar, nchar,nvar,0L  ,VAR_FILE); }

Widget make_itext(ENTERLIST2, long *ivar, long nchar)
{ return make_text2(ARGLIST2, (void*)ivar, nchar,0L  ,0L  ,VAR_LONG); }

Widget make_ftext(ENTERLIST2, float *fvar, long nchar, long ndec)
{ return make_text2(ARGLIST2, (void*)fvar, nchar,0L  ,ndec,VAR_FLOAT); }

Widget make_dtext(ENTERLIST2, double *dvar, long nchar, long ndec)
{ return make_text2(ARGLIST2, (void*)dvar, nchar,0L  ,ndec,VAR_DOUBLE); }





/*------------ routines for creating a label/text combo ----------------*/

static Widget text_row(Widget parent ,char *name)
{
  Widget w;
  char rname[80];

  strcpy(rname, name);  strcat(rname, "_rc");
  w = make_row(parent, rname);
  XmRemoveTabGroup(w);
  return w;
}



static void text_label(Widget parent ,char *name, char *label, long *sn)
{
  char lname[80];

  strcpy(lname, name);  strcat(lname, "_lb");
  make_label(parent, lname, label, sn);
}

/*
static Widget help_text(Widget parent ,char *name, char *label, long *sn)
{
  Widget w;
  char rname[80],lname[80];

  strcpy(rname, name);  strcat(rname, "_rc");
  strcpy(lname, name);  strcat(lname, "_lb");
  w = make_row(parent, rname);
  make_label(w, lname, label, sn);
  XmRemoveTabGroup(w);
  return w;
}
*/



Widget make_ctext2(ENTERLIST, char *cvar, long nchar, long nvar)
{
  Widget w = text_row(parent, name);
  text_label(w, name, label, sn);
  make_ctext(w, name, sn, hctx, data, fun, cvar, nchar, nvar);
  return w;
}

Widget make_ctext3(ENTERLIST, char *cvar, long nchar, long nvar)
{
  Widget w = text_row(parent, name);
  make_ctext(w, name, sn, hctx, data, fun, cvar, nchar, nvar);
  text_label(w, name, label, sn);
  return w;
}

Widget make_filetext2(ENTERLIST, char *cvar, long nchar, long nvar)
{
  Widget w = text_row(parent, name);
  text_label(w, name, label, sn);
  make_filetext(w, name, sn, hctx, data, fun, cvar, nchar, nvar);
  return w;
}

Widget make_filetext4(ENTERLIST, char *cvar, long nchar, long nvar)
{
  Widget w, top, wtext, wpush, wbox;
  char rname[80],fname[80],pname[80];

  top = get_toplevel_shell(parent);
  strcpy(rname, name);  strcat(rname, "_rc");
  strcpy(fname, name);  strcat(fname, "_fb");
  strcpy(pname, name);  strcat(pname, "_pb");
  w = make_small  (parent,rname);
  wtext = make_filetext(w, name,      sn,hctx,data,fun, cvar, nchar, nvar);
  wbox = make_filebox(top,fname,label,   hctx,data,fun,             wtext);
  wpush = make_push    (w,pname,label,sn,hctx,wbox,
/*              manage_widget, NULL); */
  (void (*)())  manage_widget, NULL);	/* 18May94 --- ehs */
  attach_widget(wpush,   w    , NULL,   w, w, 0,0,0,0);
  attach_widget(wtext,   wpush, w   ,   w, w, 0,0,0,0);
  XmRemoveTabGroup(w);
  return w;
}


Widget make_itext2(ENTERLIST, long *ivar, long nchar)
{
  Widget w = text_row(parent, name);
  text_label(w, name, label, sn);
  make_itext(w, name, sn, hctx, data, fun, ivar, nchar);
  return w;
}

Widget make_itext3(ENTERLIST, long *ivar, long nchar)
{
  Widget w = text_row(parent, name);
  make_itext(w, name, sn, hctx, data, fun, ivar, nchar);
  text_label(w, name, label, sn);
  return w;
}

Widget make_ftext2(ENTERLIST, float *fvar, long nchar, long ndec)
{
  Widget w = text_row(parent, name);
  text_label(w, name, label, sn);
  make_ftext(w, name, sn, hctx, data, fun, fvar, nchar, ndec);
  return w;
}

Widget make_ftext3(ENTERLIST, float *fvar, long nchar, long ndec)
{
  Widget w = text_row(parent, name);
  make_ftext(w, name, sn, hctx, data, fun, fvar, nchar, ndec);
  text_label(w, name, label, sn);
  return w;
}

Widget make_dtext2(ENTERLIST, double *dvar, long nchar, long ndec)
{
  Widget w = text_row(parent, name);
  text_label(w, name, label, sn);
  make_dtext(w, name, sn, hctx, data, fun, dvar, nchar, ndec);
  return w;
}

Widget make_dtext3(ENTERLIST, double *dvar, long nchar, long ndec)
{
  Widget w = text_row(parent, name);
  make_dtext(w, name, sn, hctx, data, fun, dvar, nchar, ndec);
  text_label(w, name, label, sn);
  return w;
}


Widget make_attached_sep(Widget p, char *name)
{
 Widget w;
 w= XtVaCreateManagedWidget( name, xmSeparatorWidgetClass, p,
                             XmNrightOffset,      5,
                             XmNleftOffset,       5,
                             XmNrightAttachment,  XmATTACH_FORM,
                             XmNleftAttachment,   XmATTACH_FORM, NULL );
 return w; 
}
/*--------------------------- end --------------------------------------*/
