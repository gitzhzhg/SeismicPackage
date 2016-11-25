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
C      widget_util.c
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
C     Utility Name:  widget_util      (widget utility routines)
C          Written:  93/02/25  by:  Tom Stoeckley
C     Last revised:  97/03/10  by:  Tom Stoeckley
C
C  Purpose:       General stand-alone utility routines for dealing 
C                 with widgets.  None of these routines create widgets.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/wproc   (shared)
C  library:                wproc.a            (shared)
C  header file:            wproc.h            (shared)
C  source file:            widget_util.c
C
C  static functions:       change_case_actions    spacing_event_handler
C                          get_dim                
C
C  documented functions:   add_spacing_event_handler   raise_widget
C                          add_change_case_actions     attach_widget
C   sensitize_arrow        sensitize_scale             sensitize_text
C   get_widget_sense       set_widget_sense            set_widget_opt
C   set_widget_ivar        set_widget_fvar             set_widget_dvar
C   set_widget_cvar        set_widget_minmax           set_widget_minvar
C   set_widget_maxvar      set_widget_minmaxvar        set_widget_radio
C   get_widget_ivar        get_widget_fvar             get_widget_dvar
C   get_widget_cvar        get_widget_minmax           get_widget_minvar
C   get_widget_maxvar      get_widget_minmaxvar        get_widget_radio
C   set_compound_resource  get_compound_resource       
C   get_shell_widget       get_toplevel_shell          get_shell_child
C   create_watch_cursor    start_watch_cursor          stop_watch_cursor
C   define_cursor          undefine_cursor
C   set_cursor_on_shells   unset_cursor_on_shells      get_full_name
C   get_help               manage_widget
C   reduce_segments        draw_segments
C   get_color              get_line_gc                 get_rubber_gc
C   rubberband_start       rubberband_move        rubberband_stop
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     wproc.a    cprim.a
C  header files:  wproc.h    cprim.h
C  functions:
C       setup_help               ctxh_set_csstr        ctxh_set_overstr
C       convert_ss2ii            convert_ss2ff         convert_ss2dd           
C       convert_ii2ss            convert_ff2ss         convert_dd2ss           
C       get_inil                 get_fnil              get_dnil 
C       remove_trailing_blanks   safe_strcpy
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  7. 97/03/10  Stoeckley  Deactivate reduce_segments by adding a line
C                            which always returns without change.  This
C                            routine seems to be very inefficient when
C                            the line is not monotonic.  This routine
C                            seems no longer to be needed to improve
C                            re-draw times.
C  6. 93/11/08  Stoeckley  Add flexibility to spacing_event_handler.
C  5. 93/10/21  Stoeckley  Change set_widget_cvar back to hardwired space
C                            [1000] (plus checks) because of apparent
C                            bug in malloc.
C  4. 93/08/09  Stoeckley  Change set_widget_cvar to allocate space for
C                            text rather than use hardwired [200].
C  3. 93/07/20  Stoeckley  Add rubberband drawing routines.
C  2. 93/06/04  Stoeckley  Change to call get_inil, convert_ii2ss,
C                            and remove_trailing_blanks in cprim.a,
C                            and to use wproc.h.
C  1. 93/02/25  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C
C  For pointers, the flag (i,o,b) refers to the contents pointed to
C  by the pointer, not to the value of the pointer itself.  The pointer
C  value is required upon INPUT in all cases.
C-----------------------------------------------------------------------
C       WORKING ON THIS ....
C  To add a callback to a text widget:
C
C                                   i   i     i    i       i
C          void add_itext_callback (w, ivar, fun, data, use_nils)
C          void add_ftext_callback (w, fvar, fun, data, use_nils)
C          void add_dtext_callback (w, dvar, fun, data, use_nils)
C          void add_ctext_callback (w, cvar, fun, data          )
C
C  Widget         w = text widget to add this callback to.
C  long       *ivar = pointer to target     integer      value in user area.
C  float      *fvar = pointer to target  floating point  value in user area.
C  double     *dvar = pointer to target double precision value in user area.
C  char       *cvar = pointer to target      text        value in user area.
C  void    (*fun)() = user-written function to call (see below).
C  void       *data = pointer to user data (passed to user function).
C  Boolean use_nils = whether to allow nil values and error flags.
C
C  If ivar, fvar, dvar, or cvar is not NULL, that target location in
C    user memory will receive the new value entered by the user of the
C    program.
C
C  If fun() is not NULL, that function will be called when a new
C    value is entered by the user of the program, and after the
C    target (if not NULL) is reset to this new value.
C
C  The pointer to user data can be NULL.
C
C  A new value will be considered entered by the user whenever the
C    return key is pressed (whether or not the value has changed) or
C    whenever the widget loses keyboard focus (but only if the value
C    has been changed).
C
C  Also, the program can call the function call_text_update_function(w),
C    probably right after text creation, to force this callback to 
C    be executed; this has the purpose of setting the variable in 
C    user memory from the app-defaults file.
C
C  If use_nils is True:
C         -- blank text will convert to a nil numeric value.
C         -- a conversion error will convert to a numeric error flag
C                which is displayed as an asterisk.
C  If use_nils is False: 
C         -- blank text will convert to a zero numeric value which
C                is then displayed as a zero.
C         -- a conversion error will convert to a zero numeric value
C                which is then displayed as a zero.
C
C  A conversion error is unlikely since illegal characters are
C    detected and rejected as they are typed.
C
C  A nil value or an error flag value can be retrieved or reset by
C    calling routines in get_inil.c.
C
C                 --------------------------------
C
C  The user function must have this argument list:
C                 i     b       i        i
C       void fun(data, ivar, old_ivar, status)      integer      
C       void fun(data, fvar, old_fvar, status)      floating point
C       void fun(data, dvar, old_dvar, status)      double precision
C       void fun(data, cvar, old_cvar, status)      text
C
C  void       *data = pointer to user data.
C  long       *ivar = pointer to current value of variable.
C  float      *fvar = pointer to current value of variable.
C  double     *dvar = pointer to current value of variable.
C  char       *cvar = pointer to current value of variable.
C  long    old_ivar = previous value of variable.
C  float   old_fvar = previous value of variable.
C  double  old_dvar = previous value of variable.
C  char   *old_cvar = previous value of variable.
C  long      status = 1 (successful number or text)
C                     0 (blank text, number is a nil value or zero)
C                    -1 (error, number is an error flag or zero)
C
C  If status = 1, non-blank text was entered.  And for numbers, a
C                   successful number was converted from the text.
C  If status = 0, blank text was entered.  And for numbers, the value
C                   is nil if use_nils = True; and the value is zero,
C                   and the display is changed to zero, otherwise.
C  If status = -1, a conversion error occured (numbers only).  The
C                   value is an error flag if use_nils = True.
C
C  If the current value is reset in the user function, the displayed 
C    value will be reset accordingly after returning from the user 
C    function.
C
C  If the target is not NULL, the pointer to the current value will
C    be the pointer to the target, so that changing the current value
C    or changing the target will have the same effect.
C-----------------------------------------------------------------------
C       WORKING ON THIS ....
C  To layout children widgets in efficient rows and columns:
C
C                                       i      i
C         void layout_children_in_cols (w, num_in_col)
C         void layout_children_in_rows (w, num_in_row)
C
C  Widget       w = widget whose children are to be arranged.
C  int num_in_col = number of widgets in a column.
C  int num_in_row = number of widgets in a row.
C
C  If the widget is a form, attachment resources will be set.
C    Otherwise, x and y resources will be set.
C  If the first child is a label widget, and the second child is a 
C    horizontal separator widget, the first and second children are
C    placed at the top, and the remaining children are arranged in
C    rows or columns.
C-----------------------------------------------------------------------
C  To add an event handler to control the spacing of children:
C
C                                           i
C            void add_spacing_event_handler(w);
C
C  Widget w = form or bulletin board widget whose children are to be
C               kept evenly spaced.
C
C  The children are maintained at an even horizontal spacing both
C    initially, and whenever the width of the widget changes.
C  Commonly used for a widget which contains the pushbuttons at the
C    bottom of a dialog box.
C-----------------------------------------------------------------------
C  To add actions to change the case of text:
C
C                                         i
C            void add_change_case_actions(w)
C
C  Widget w = text widget to add these actions to.
C
C  The action Ctrl-U changes selected text to upper case.
C  The action Ctrl-L changes selected text to lower case.
C-----------------------------------------------------------------------
C  To raise a widget to its frame:
C                                     i
C               wraise = raise_widget(w)
C
C  Widget w      = widget to raise.
C  Widget wraise = the widget or its frame.
C
C  Returns NULL if w is NULL or is not a widget.
C  Returns the parent of w if its parent is a frame.
C  Otherwise returns w.
C  Used by the routine attach_widget (see below).
C-----------------------------------------------------------------------
C  To attach a widget:
C                            i    i      i      i       i
C         void attach_widget(w, wleft, wright, wtop, wbottom,
C                               oleft, oright, otop, obottom)
C
C  Widget w       = widget to attach to form or to other widget.
C  Widget wleft   = widget on the left.
C  Widget wright  = widget on the right.
C  Widget wtop    = widget on the top. 
C  Widget wbottom = widget on the bottom.
C  int    oleft   = left offset.
C  int    oright  = right offset.
C  int    otop    = top offset. 
C  int    obottom = bottom offset.
C
C  The following info illustrates the actions taken for left attachments.
C  The other three attachments are dealt with similarly.
C
C  wleft       XmNleftAttachment  XmNleftWidget  XmNleftOffset
C  ----        -----------------  -------------  -------------
C  NULL        (not set)          (not set)      oleft (not set if ==0)
C  "parent"    XmATTACH_FORM      (not set)      oleft (not set if ==0)
C  otherwise   XmATTACH_WIDGET    wleft          oleft (not set if ==0)
C
C  If parent of w     is a frame, the frame is used instead of w.
C  If parent of wleft is a frame, the frame is used instead of wleft.
C  The word "parent" above means that if wleft is a form widget, and is
C    the parent of w or its frame, the indicated action occurs.
C-----------------------------------------------------------------------
C  To set the sensitivity of a widget:
C
C                              i        i
C       void sensitize_arrow(widget, sensitive)    arrow button
C       void sensitize_scale(widget, sensitive)    scale widget
C       void sensitize_text (widget, sn       )    text widget
C
C  Widget     widget = widget to use.
C  Boolean sensitive = True to sensitize and False to insensitize.
C  long           sn = sensitivity variable.
C
C  In addition to setting the sensitivity, these routines set 
C    additional resources to provide a visual clue to the sensitivity.
C  For text widgets, sn >=  1 sets the widget sensitive.
C  For text widgets, sn ==  0 sets the widget insensitive.
C  For text widgets, sn == -1 sets the widget insensitive with dim text.
C  The color of the text in insensitive scale widgets (and in
C    insensitive text widgets with sn = -1) can be specified by 
C    the custom resource dimsense (default gray55).
C-----------------------------------------------------------------------
C  To set (or get) the sensitivity and/or managed status of a widget:
C
C                                       i      i
C               void set_widget_sense(widget,  sn)
C               void get_widget_sense(widget, &sn)
C                                       i      o
C  Widget widget = widget to use.
C  long       sn = sensitivity variable.
C
C  sn >= -1 manages the widget if it is unmanaged.
C  sn >=  1 sets the widget sensitive.
C  sn ==  0 sets the widget insensitive (sensitive for label widget).
C  sn ==  0 sets the widget insensitive ( not dim  for  text widget).
C  sn == -1 sets the widget insensitive (   dim    for  text widget).
C  sn <= -2 unmanages the widget.
C
C  The set_widget_sense routine may call sensitize_arrow or
C    sensitize_text or sensitize_scale, depending on the type of 
C    widget, in addition to the usual Xt routines.
C  The get_widget_sense routine returns only the values 1 or -1 or -2.
C-----------------------------------------------------------------------
C  To set (or get) the value displayed by a widget:
C
C                                 i       i        i        i    i
C     void set_widget_cvar     (widget,  cvar)
C     void set_widget_ivar     (widget,  ivar)
C     void set_widget_fvar     (widget,  fvar,                   ndec)
C     void set_widget_dvar     (widget,  dvar,                   ndec)
C     void set_widget_radio    (widget,  ivar,                   id)
C     void set_widget_opt      (widget,  ivar,                   id)
C     void set_widget_minmax   (widget,  minvar,  maxvar)
C     void set_widget_minmaxvar(widget,  minvar,  maxvar,  ivar)
C     void set_widget_minvar   (widget,  minvar)
C     void set_widget_maxvar   (widget,  maxvar)
C
C                                 i       o        o        o    i
C     void get_widget_cvar     (widget,  cvar)
C     void get_widget_ivar     (widget, &ivar)
C     void get_widget_fvar     (widget, &fvar)
C     void get_widget_dvar     (widget, &dvar)
C     void get_widget_radio    (widget, &ivar,                   id)
C     void get_widget_minmax   (widget, &minvar, &maxvar)
C     void get_widget_minmaxvar(widget, &minvar, &maxvar, &ivar)
C     void get_widget_minvar   (widget, &minvar)
C     void get_widget_maxvar   (widget, &maxvar)
C
C  Widget widget = widget to use.
C  char    *cvar = character string       to display.
C  long     ivar = integer value          to display.
C  float    fvar = floating point value   to display.
C  double   dvar = double precision value to display.
C  long     ndec = maximum number of decimals to display.
C  long       id = identification for radio button.
C  long   minvar = minimum value of range (for scale and scrollbar).
C  long   maxvar = maximum value of range (for scale and scrollbar).
C
C  If the widget is a scale, ivar is its value.
C  If the widget is a toggle button, ivar is its state (1=true, 0=false).
C  Otherwise, any numeric value is converted to/from text and displayed as
C    the text, label, title, or whatever the specified widget understands.
C  The "..._radio" routines are specific for radio buttons:
C    When setting: the state is set to true if ivar == id, otherwise to false.
C    When getting: ivar is set to id if the state is true, otherwise to 0.
C  The "..._opt" routine is specific for option buttons (pushbuttons
C      created by make_opt):
C    The option menu will display this option button if ivar == id.
C    Otherwise nothing is done.
C  If a value cannot be gotten, a nil numeric value (or empty string)
C    is returned.
C-----------------------------------------------------------------------
C  To set a compound string resource from a character string:
C  To get a character string from a compound string resource:
C
C                                     i     i      i
C          void set_compound_resource(w, resname, cvar)
C          void get_compound_resource(w, resname, cvar)
C                                     i     i      o
C
C  Widget      w = widget which owns the resource.
C  char *resname = name of resource (e.g. XmNlabelString).
C  char    *cvar = character string to set or get.
C-----------------------------------------------------------------------
C  To get the shell widget ID for any widget:
C
C                                          i
C                 shell = get_shell_widget(w) 
C
C  Widget w     = widget for which you want the shell.
C  Widget shell = shell widget found.
C
C  If w is a shell widget, returns w.
C  Otherwise traces back to find shell, and returns it.
C  Returns NULL if w is not a widget, or traceback is not successful.
C-----------------------------------------------------------------------
C  To get the toplevel shell widget ID for any widget:
C
C                                            i
C              toplevel = get_toplevel_shell(w)
C
C  Widget w        = widget for which you want the toplevel shell.
C  Widget toplevel = toplevel shell widget found.
C
C  If w is the toplevel shell widget, returns w.
C  Otherwise traces back to find toplevel, and returns it.
C  Returns NULL if w is not a widget, or traceback is not successful.
C-----------------------------------------------------------------------
C  To get the shell child widget ID for any widget:
C
C                                         i
C                 child = get_shell_child(w)
C
C  Widget w     = widget for which you want the shell child.
C  Widget child = child of the shell widget found.
C
C  If w is the immediate child of its shell, returns w.
C  If w is a shell widget, returns its child.
C  Otherwise traces back to find shell, and returns its child.
C  Returns NULL if w is not a widget, or traceback is not successful.
C-----------------------------------------------------------------------
C  To create, start, or stop display of watch cursor:
C
C                                        i
C             void create_watch_cursor(widget)
C             void  start_watch_cursor(void)
C             void   stop_watch_cursor(void)
C
C  Widget widget = any widget in the display.
C
C  The first function must be called before the others.
C  The watch cursor will be displayed in the toplevel shell of 
C    the display, and in all dialog boxes in the display (for the
C    application which has the widget).
C  NOTE: These functions can be used only for one display, which would
C    be the display containing the specified widget.  Calling the first
C    function more than once has no effect.  The better way to do this
C    for more than one display would be to create a C++ class with
C    this same functionality.
C-----------------------------------------------------------------------
C  To set the cursor to a desired appearance:
C
C                                             i       i
C               void define_cursor         (widget, cursor)
C               void set_cursor_on_shells  (widget, cursor)
C               void undefine_cursor       (widget)
C               void unset_cursor_on_shells(widget)
C
C  Widget widget = any widget in the desired shell, or in the application.
C  Cursor cursor = cursor to display.
C
C  Set cursor to XC_watch to display a watch.
C  Set cursor to None to revert it to its usual appearance.
C  The first routine displays the cursor in the shell which contains
C     the given widget.
C  The second routine displays the cursor in the toplevel shell of the
C    display which contains the widget, and in all dialog boxes in the
C    display (for the application which has the widget).
C  The third and fourth routines revert the cursor to the default
C    appearance.
C  Symbols for valid cursors are in the X11/cursorfont.h header file.
C-----------------------------------------------------------------------
C  To get the full name of a widget:
C
C                                  i   o
C               void get_full_name(w, name)
C
C  Widget   w = widget whose full name you want.
C  char *name = full name, including ancestry (returned).
C
C  The user must supply sufficient room in the name for the full string.
C-----------------------------------------------------------------------
C  To get context_sensitive help:
C                                          i
C                          hctx = get_help(w)
C
C  Widget             w = any widget.
C  struct HELPCTX *hctx = pointer to help context.
C
C  Creates context-sensitive help when called the first time.
C  Returns the same help context each subsequent time called.
C  Gets help file  from application resources (resource name helpfile).
C  Gets help title from application resources (resource name helptitle).
C-----------------------------------------------------------------------
C  To manage a widget:
C                                       i
C                    void manage_widget(w)
C
C  Widget w = the widget to manage.
C
C  With the exceptions below, this routine simply manages the widget.
C  If the widget is a file selection box or message box, this routine
C    unmanages the widget if it is already managed.
C  If the widget is a file selection box, this routine does a file 
C    filter when the widget is managed.
C  This function will be removed when no longer needed (when the
C    fileChoice widget is used).
C-----------------------------------------------------------------------
C  To reduce and draw line segments:
C
C                                   b  b    i
C    number2 = reduce_segments     (x, y, number)
C    void      draw_segments(w, gc, x, y, number2, x1, y1, x2, y2)
C                                   i  i    i      i   i   i   i
C
C  long x[number] = abscissae of points defining a line.
C  long y[number] = ordinates of points defining a line.
C  long   number  = number of points defining a line.
C  long   number2 = reduced number of points defining a line.
C  Widget       w = widget to which line segments are to be drawn.
C  GC          gc = graphics context to use for drawing segments.
C  int     x1, y1 = upper left  corner of bounding rectangle.
C  int     x2, y2 = lower right corner of bounding rectangle.
C
C  The variables x[],y[], x1,y1, x2,y2 are in pixel units.
C  The first routine reduces the number of points defining the line
C    by omitting points when this causes an error of no more than one 
C    pixel.  The first and last points are unchanged.  This speeds up 
C    xlib by reducing the number of segments to be drawn.  The reduced 
C    number is returned, and the abscissae and ordinates are changed 
C    accordingly.
C  The second routine converts the points to segments and draws them.
C    It is assumed that x1 < x2 and y1 < y2.  All line segments are 
C    drawn in full if x1 = x2 = y1 = y2 = 0.  Otherwise, segments are 
C    truncated if a portion lies outside range [y1,y2].
C-----------------------------------------------------------------------
C  To get a named color:
C  To get a graphics context for drawing a colored line:
C  To get a graphics context for drawing a rubber band using GXinvert:
C
C                          i   i        i         i      i        i
C   pix = get_named_color (w, cmap,             cname, gsname, defblack)
C   gc  = get_line_gc     (w, cmap, line_width, cname, gsname, defblack)
C   gc  = get_rubber_gc   (w,       line_width                         )
C
C  Widget         w = any widget.
C  Colormap    cmap = colormap (or 0 to use the default colormap).
C  int   line_width = desired width of line.
C  char      *cname = desired color name.
C  char     *gsname = gray scale name (in case server is gray scale only).
C  Boolean defblack = True for black or False for white (for b/w servers).
C  Pixel        pix = returned color pixel.
C
C  get_line_gc calls get_named_color.
C-----------------------------------------------------------------------
C  To draw a rubber band:
C                                   i     i  i
C       rs = rubberband_start(    event1, w, gc)
C     void   rubberband_move (rs, event2)
C     void   rubberband_stop (rs, event3, &x1, &y1, &x2, &y2, &direction)
C                             i     i      o    o    o    o       o
C
C  RubberbandStruct *rs = pointer to opaque rubberband structure.
C  XButtonEvent *event1 = X event when button is pressed.
C  XMotionEvent *event2 = X event when button is moved.
C  XButtonEvent *event3 = X event when button is released.
C  Widget             w = widget receiving the events.
C  GC                gc = graphics context for drawing rubberband.
C  long              x1 = X pixel at left end of rubberband.
C  long              y1 = Y pixel at left end of rubberband.
C  long              x2 = X pixel at right end of rubberband.
C  long              y2 = Y pixel at right end of rubberband.
C  long       direction = direction the rubberband was drawn.
C
C  These routines should be called from action routines or event
C    handlers.
C  The first routine should be called once when the button is pressed.
C  The second routine should be called each time the button is moved.
C  The third routine should be called once when the button is released.
C
C  The rubberband structure is allocated by rubberband_start and
C    de-allocated by rubberband_stop.
C
C  The graphics context should be a type which un-draws a line the
C    second time the line is drawn (for example, one which is returned
C    by the function get_rubber_gc).
C  The direction will be set to the constant FORWARD (left-to-right)
C    or BACKWARD (right-to-left).  These constants are defined in
C    the cprim.h header file.
C-----------------------------------------------------------------------
C                                NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
*/



/*----------------------- header files ---------------------------------*/

#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/ToggleB.h>
#include <Xm/ArrowB.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/DrawingA.h>
#include <Xm/BulletinB.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/Scale.h>
#include <Xm/ScrollBar.h>
#include <Xm/DialogS.h>
#include <Xm/XmP.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include "wproc.h"
#include "cprim.h"
#include "str.h"
#include "named_constants.h"



/*---------------- change case action ----------------------------------*/

static void change_case_actions(Widget w, XEvent *event, String *params,
                                                   Cardinal *num_params)
{
  char *space, *space2;
  int length, i;
  XmTextPosition left, right;

  space2 = XmTextGetSelection(w);
  if(space2)
       {
       if(XmTextGetSelectionPosition(w, &left, &right))
            {
            if(right > left)
                 {
                 length = right - left;
                 space = XtMalloc(length+1);
                 strncpy(space, space2, length);
                 *(space+length) = '\0';
                 for (i=0; i < length; i++)
                     {
                     if(params[0][0] == 'U')
                          *(space+i) = toupper(*(space+i));
                     else if(params[0][0] == 'L')
                          *(space+i) = tolower(*(space+i));
                     }
                 XmTextReplace(w, left, right, space);
                 XtFree(space);
                 XmTextClearSelection(w,event->xkey.time);
                 }
            }
       }
  XtFree(space2);
}


void add_change_case_actions(Widget w)
{
  static XtActionsRec actions_table[] = {
            { "ChangeCase", (XtActionProc)change_case_actions },
           };
  static char default_translations[] =
             "Ctrl<Key>U : ChangeCase(U)  \n\
              Ctrl<Key>L : ChangeCase(L)" ;
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
  XtAugmentTranslations(w, trans_table);
}



/*--------- set or get a compound string resource ----------------------*/

void set_compound_resource(Widget w, String resname, char *cvar)
{
  XmString string;

  string = XmStringCreateLtoR(cvar, XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues(w, resname, string, NULL);
  XmStringFree(string);

}



void get_compound_resource(Widget w, String resname, char *cvar)
{
  XmString string;
  char *text;

  XtVaGetValues(w, resname, &string, NULL);
  if(XmStringGetLtoR(string,XmSTRING_DEFAULT_CHARSET,&text) )
      {
      safe_strcpy(cvar, text);
      XtFree(text);
      }
  else
      {
      cvar[0] = '\0';
      }
  XmStringFree(string);
}




/*------- get custom resource for sensitize_text -----------------------*/

static Pixel get_dim(Widget w)
{
 typedef struct _Custom
     {
     Pixel dim;
     } Custom;
  static Custom custom;
  static XtResource resources[] = {
       { "dimsense",  "Dimsense",  XtRPixel, sizeof(Pixel),
                      XtOffsetOf(Custom,dim),  XtRString, "gray55" },
     };
  static Boolean start = TRUE;
  Widget toplevel;

  if(start)
       {
       toplevel = get_toplevel_shell(w);
       XtGetApplicationResources(toplevel, &custom,
                                resources, XtNumber(resources), NULL, 0);
       start = FALSE;
       }
  return custom.dim;
}




/*------- adjust sensitivity of various widgets ------------------------*/
/* sets sensitivity true or false. */
/* also makes adjustments so there is a visual indication of sensitivity. */
/* does nothing if widget id is NULL or not a widget.  */

void sensitize_arrow(Widget w, Boolean sensitive)
{
    Pixel color;

    if(!w || !XtIsWidget(w)) return;
    if(sensitive) color = BlackPixelOfScreen(XtScreen(w));
    else          XtVaGetValues(w, XmNbackground, &color, NULL);
    XtVaSetValues(w, XmNforeground, color, NULL);
    XtSetSensitive(w, sensitive);
}


void sensitize_scale(Widget w, Boolean sensitive)
{
  Pixel color;

  if(!w || !XtIsWidget(w)) return;
  if(sensitive) color = BlackPixelOfScreen(XtScreen(w));
  else          color = get_dim(w);
  XtVaSetValues(w, XmNforeground, color, NULL);
  if(sensitive) XtSetSensitive(w, True );
  else          XtSetSensitive(w, False);
}


void sensitize_text(Widget w, long sn)
{
  Pixel color;

  if(!w || !XtIsWidget(w)) return;
  if(sn >= 0) color = BlackPixelOfScreen(XtScreen(w));
  else        color = get_dim(w);
  XtVaSetValues(w, XmNforeground, color, NULL);
  if(sn >= 1) XtSetSensitive(w, True );
  else        XtSetSensitive(w, False);
}




/*------------------- set or get widget sense --------------------------*/
           /*  sn >=  1   sensitive                                */
           /*  sn ==  0   insensitive (sensitive for label widget) */
           /*  sn ==  0   insensitive ( not dim  for  text widget) */
           /*  sn == -1   insensitive (   dim    for  text widget) */
           /*  sn <= -2   unmanaged                                */
     /* when getting, returns only 1 or -1 or -2 */

void set_widget_sense(Widget w, long sn)
{
  Boolean sensitive, labelsense;
  WidgetClass class;

  if (!w || !XtIsWidget(w)) return;
  else if(XtIsManaged(w)) { if(sn <= -2) { XtUnmanageChild(w); return; } }
  else if(sn <= -2)     return;
  else                  XtManageChild(w);

  sensitive  = (sn >= 1);
  labelsense = (sn >= 0);
  class = XtClass(w);
  if     (class == xmArrowButtonWidgetClass) sensitize_arrow(w, sensitive );
  else if(class == xmScaleWidgetClass)       sensitize_scale(w, sensitive );
  else if(class == xmTextWidgetClass)        sensitize_text (w, sn        );
  else if(class == xmLabelWidgetClass)       XtSetSensitive (w, labelsense);
  else if(class == xmPushButtonWidgetClass)  XtSetSensitive (w, sensitive );
  else if(class == xmToggleButtonWidgetClass)XtSetSensitive (w, sensitive );
  else                                       XtSetSensitive (w, sensitive );
}


void get_widget_sense(Widget w, long *sn)
{
/*
  Boolean sensitive;
  WidgetClass class;
*/

  if (!w || !XtIsWidget(w)) *sn = -2;
  else if(!XtIsManaged(w))  *sn = -2;
  else if(XtIsSensitive(w)) *sn =  1;
  else *sn = -1;
}



/*------------------- set or get widget cvar ---------------------------*/
                  /* works for many widgets */

#define TEXTLENGTH 1000

void set_widget_cvar(Widget w, char *cvar)
{
  char text[TEXTLENGTH];
/*
  char *text;         / * was char text[200]; * /
*/
  WidgetClass class;
  if(!w || !XtIsWidget(w)) return;
/*
  text = XtMalloc(MaximumValue(strlen(cvar), 1));
*/
  strncpy(text, cvar, TEXTLENGTH - 1);
  text[TEXTLENGTH - 1] = '\0';
  str_remove_trailing_blanks(text, text);
/*
  str_remove_trailing_blanks(text, cvar);
*/
  class = XtClass(w);
  if(class == xmTextWidgetClass)
      {
      XtVaSetValues(w, XmNvalue, text, NULL); /* will not call callback? */
/*
      XmTextSetString(w, text); / * calls modify & value changed callback? * /
*/
      XmTextSetInsertionPosition(w, strlen(text));
      }

  else if(class == xmLabelWidgetClass ||
          class == xmPushButtonWidgetClass ||
          class == xmCascadeButtonWidgetClass ||
          class == xmToggleButtonWidgetClass)
      set_compound_resource(w, XmNlabelString, text);

  else if(class == xmFileSelectionBoxWidgetClass)
      set_compound_resource(w, XmNdirSpec, text);

  else if(class == xmMessageBoxWidgetClass)
      set_compound_resource(w, XmNmessageString, text);

  else if(class == xmScaleWidgetClass)
      set_compound_resource(w, XmNtitleString, text);

  else if(class == xmDialogShellWidgetClass)
      XtVaSetValues(w, XmNtitle, text, NULL);
/*
  XtFree(text);      / * added when text[200] was replaced above * /
*/
}


void get_widget_cvar(Widget w, char *cvar)
{
  char *text;
  WidgetClass class;
  if(!w || !XtIsWidget(w)) { cvar[0] = '\0'; return; }
  class = XtClass(w);
  if(class == xmTextWidgetClass)
      {
      text = XmTextGetString(w);
      safe_strcpy(cvar, text);
      XtFree(text);
      }
  else if(class == xmDialogShellWidgetClass)
      {
      XtVaGetValues(w, XmNtitle, &text, NULL);
      safe_strcpy(cvar, text);
      XtFree(text);
      }
  else if(class == xmLabelWidgetClass ||
          class == xmPushButtonWidgetClass ||
          class == xmCascadeButtonWidgetClass ||
          class == xmToggleButtonWidgetClass)
      get_compound_resource(w, XmNlabelString, cvar);

  else if(class == xmFileSelectionBoxWidgetClass)
      get_compound_resource(w, XmNdirSpec, cvar);

  else if(class == xmMessageBoxWidgetClass)
      get_compound_resource(w, XmNmessageString, cvar);

  else if(class == xmScaleWidgetClass)
      get_compound_resource(w, XmNtitleString, cvar);

  else
      cvar[0] = '\0';
}




/*------------------- set or get widget ivar ---------------------------*/
                  /* scale sets value  */
                  /* toggle sets state */
                  /* otherwise converted to text */

void set_widget_ivar(Widget w, long ivar)
{                                       
/*
  Boolean state;                       
*/
  char cvar[200];
  short nchar3;
  int nchar;
  int ivar2 = (int)ivar;
  WidgetClass class;

  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if (class == xmToggleButtonWidgetClass)
      {
      XmToggleButtonSetState(w, (Boolean)ivar, False);
      return;
      }
  else if(class == xmScaleWidgetClass)
      {
      XmScaleSetValue(w, ivar);
      return;
      }
  else if(class == xmTextWidgetClass)
      {
      XtVaGetValues(w, XmNcolumns, &nchar3, NULL);
      nchar = (int)nchar3;
      }
  else
      {
      nchar = 7;
      }
  str_ii2ss(ivar2, cvar, nchar);
  set_widget_cvar(w, cvar);
}



void get_widget_ivar(Widget w, long *ivar)
{
  char cvar[200];
  int istat;
  int ivar2;
  WidgetClass class;

/*
  get_inil(&ivar2);
*/
  ivar2 = INIL;
  *ivar = (long)ivar2;
  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if (class == xmToggleButtonWidgetClass)
      {
      *ivar = (long)XmToggleButtonGetState(w);
      return;
      }
  else if(class == xmScaleWidgetClass)
      {
      XmScaleGetValue(w, &ivar2);
      *ivar = (long)ivar2;
      return;
      }
  get_widget_cvar(w, cvar);
  str_ss2ii(cvar, &ivar2, &istat);
  *ivar = (long)ivar2;
}




/*------------------- set widget pushbutton in option menu -------------*/

void set_widget_opt(Widget w, long ivar, long id)
{                                       
  Widget parent, woption;
  WidgetClass class;
  if(!w || !XtIsWidget(w)) return;
  parent = XtParent(w);
  class = XtClass(parent);
  if (class != xmRowColumnWidgetClass) return;
  class = XtClass(w);
  if (class == xmPushButtonWidgetClass && ivar == id)
      {
      XtVaGetValues(parent, XmNuserData, &woption, NULL);
      if(woption) XtVaSetValues(woption, XmNmenuHistory, w, NULL);
      }
}




/*------------------- set or get widget radio button -------------------*/

void set_widget_radio(Widget w, long ivar, long id)
{                                       
  WidgetClass class;
  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if (class == xmToggleButtonWidgetClass)
      {
      if(ivar == id) XmToggleButtonSetState(w, True , False);
      else           XmToggleButtonSetState(w, False, False);
      }
}



void get_widget_radio(Widget w, long *ivar, long id)
{
  Boolean state;
  int ivar2;
  WidgetClass class;

/*
  get_inil(&ivar2);
*/
  ivar2 = INIL;
  *ivar = (long)ivar2;
  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if (class == xmToggleButtonWidgetClass)
      {
      state = XmToggleButtonGetState(w);
      if(state) *ivar = id; else *ivar = 0;
      }
}




/*------------------- set or get widget fvar ---------------------------*/
                  /* converted to text */

void set_widget_fvar(Widget w, float fvar, long ndec)
{
  char cvar[200];
  short nchar3;
  int nchar;
  int ndec2 = (int)ndec;
  WidgetClass class;

  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmTextWidgetClass)
      {
      XtVaGetValues(w, XmNcolumns, &nchar3, NULL);
      nchar = (int)nchar3;
      }
  else
      {
      nchar = 13;
      }
  str_ff2ss(fvar, cvar, nchar, ndec2);
  set_widget_cvar(w, cvar);
}


void get_widget_fvar(Widget w, float *fvar)
{
  char cvar[200];
  int istat;

/*
  get_fnil(fvar);
*/
  *fvar = FNIL;
  if(!w || !XtIsWidget(w)) return;
  get_widget_cvar(w, cvar);
  str_ss2ff(cvar, fvar, &istat);
}




/*------------------- set or get widget dvar ---------------------------*/
                  /* converted to text */

void set_widget_dvar(Widget w, double dvar, long ndec)
{
  char cvar[200];
  short nchar3;
  int nchar;
  int ndec2 = (int)ndec;
  WidgetClass class;
/*
                      char *text;
*/

  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmTextWidgetClass)
      {
      XtVaGetValues(w, XmNcolumns, &nchar3, NULL);
      nchar = (int)nchar3;
      }
  else
      {
      nchar = 13;
      }
  str_dd2ss(dvar, cvar, nchar, ndec2);
  set_widget_cvar(w, cvar);
}


void get_widget_dvar(Widget w, double *dvar)
{
  char cvar[200];
  int istat;

/*
  get_dnil(dvar);
*/
  *dvar = DNIL;
  if(!w || !XtIsWidget(w)) return;
  get_widget_cvar(w, cvar);
  str_ss2dd(cvar, dvar, &istat);
}




/*----------- set or get widget minimum and maximum --------------------*/
                /* for scale and scrollbar */

void set_widget_minmaxvar(Widget w, long minvar, long maxvar,
                                                 long ivar)
{
  WidgetClass class;
  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmScaleWidgetClass || class == xmScrollBarWidgetClass)
     {
     minvar = MinimumValue(minvar, ivar);
     maxvar = MaximumValue(maxvar, ivar);
     if(minvar == maxvar) maxvar++;
     XtVaSetValues(w, XmNminimum, (int)minvar,
                      XmNmaximum, (int)maxvar,
                      XmNvalue,   (int)ivar, NULL);
     }
}

void get_widget_minmaxvar(Widget w, long *minvar, long *maxvar,
                                                  long *ivar)
{
  int minimum, maximum, value;
  WidgetClass class;

  *minvar = (long)INIL;
  *maxvar = (long)INIL;
  *ivar   = (long)INIL;
/*
  get_inil(minvar);
  get_inil(maxvar);
  get_inil(  ivar);
*/
  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmScaleWidgetClass || class == xmScrollBarWidgetClass)
      {
      XtVaGetValues(w, XmNminimum, &minimum,
                       XmNmaximum, &maximum,
                       XmNvalue,   &value, NULL);
      *minvar = (long)minimum;
      *maxvar = (long)maximum;
      *ivar   = (long)value;
      }
}



void set_widget_minmax(Widget w, long minvar, long maxvar)
{
  WidgetClass class;
  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmScaleWidgetClass || class == xmScrollBarWidgetClass)
     {
     if(maxvar <= minvar)
     XtVaSetValues(w, XmNminimum, (int)minvar,
                      XmNmaximum, (int)(minvar+1), NULL);
     else
     XtVaSetValues(w, XmNminimum, (int)minvar,
                      XmNmaximum, (int)maxvar, NULL);
     }
}

void get_widget_minmax(Widget w, long *minvar, long *maxvar)
{
  int minimum, maximum;
  WidgetClass class;

  *minvar = (long)INIL;
  *maxvar = (long)INIL;
/*
  get_inil(minvar);
  get_inil(maxvar);
*/
  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmScaleWidgetClass || class == xmScrollBarWidgetClass)
      {
      XtVaGetValues(w, XmNminimum, &minimum,
                       XmNmaximum, &maximum, NULL);
      *minvar = (long)minimum;
      *maxvar = (long)maximum;
      }
}



void set_widget_minvar(Widget w, long minvar)
{
  WidgetClass class;

  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmScaleWidgetClass || class == xmScrollBarWidgetClass)
     {
     XtVaSetValues(w, XmNminimum, (int)minvar, NULL);
     }
}


void set_widget_maxvar(Widget w, long maxvar)
{
  WidgetClass class;

  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmScaleWidgetClass || class == xmScrollBarWidgetClass)
     {
     XtVaSetValues(w, XmNmaximum, (int)maxvar, NULL);
     }
}


void get_widget_minvar(Widget w, long *minvar)
{
  int minimum;
  WidgetClass class;

  *minvar = (long)INIL;
/*
  get_inil(minvar);
*/
  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmScaleWidgetClass || class == xmScrollBarWidgetClass)
      {
      XtVaGetValues(w, XmNminimum, &minimum, NULL);
      *minvar = (long)minimum;
      }
}


void get_widget_maxvar(Widget w, long *maxvar)
{
  int maximum;
  WidgetClass class;

  *maxvar = (long)INIL;
/*
  get_inil(maxvar);
*/
  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmScaleWidgetClass || class == xmScrollBarWidgetClass)
      {
      XtVaGetValues(w, XmNmaximum, &maximum, NULL);
      *maxvar = (long)maximum;
      }
}




/*-------------------- raise widget ------------------------------------*/

Widget raise_widget(Widget w)
{
  if(!w || !XtIsWidget(w)) return NULL;
  if(XtClass(XtParent(w)) == xmFrameWidgetClass) return XtParent(w);
  return w;
}



/*-------------------- attach widget -----------------------------------*/

void attach_widget(Widget w,
          Widget wleft, Widget wright, Widget wtop, Widget wbottom,
          int    oleft, int    oright, int    otop, int    obottom)
{
  int i=0;
  Arg args[22];
  Widget wform;

  if(!w) return;
  w = raise_widget(w);
  wform = XtParent(w);
  if(XtClass(wform) != xmFormWidgetClass) return;
  if(wleft   != wform) wleft   = raise_widget(wleft  );
  if(wright  != wform) wright  = raise_widget(wright );
  if(wtop    != wform) wtop    = raise_widget(wtop   );
  if(wbottom != wform) wbottom = raise_widget(wbottom);

#define SETARG(NAME,VALUE)   XtSetArg(args[i], NAME, VALUE); i++;

  if(wform == wleft  ) { SETARG(XmNleftAttachment  , XmATTACH_FORM  ) }
  else if    (wleft  ) { SETARG(XmNleftAttachment  , XmATTACH_WIDGET)
                         SETARG(XmNleftWidget      , wleft          ) }
  if         (oleft  ) { SETARG(XmNleftOffset      , oleft          ) }

  if(wform == wright ) { SETARG(XmNrightAttachment , XmATTACH_FORM  ) }
  else if    (wright ) { SETARG(XmNrightAttachment , XmATTACH_WIDGET)
                         SETARG(XmNrightWidget     , wright         ) }
  if         (oright ) { SETARG(XmNrightOffset     , oright         ) }

  if(wform == wtop   ) { SETARG(XmNtopAttachment   , XmATTACH_FORM  ) }
  else if    (wtop   ) { SETARG(XmNtopAttachment   , XmATTACH_WIDGET)
                         SETARG(XmNtopWidget       , wtop           ) }
  if         (otop   ) { SETARG(XmNtopOffset       , otop           ) }

  if(wform == wbottom) { SETARG(XmNbottomAttachment, XmATTACH_FORM  ) }
  else if    (wbottom) { SETARG(XmNbottomAttachment, XmATTACH_WIDGET)
                         SETARG(XmNbottomWidget    , wbottom        ) }
  if         (obottom) { SETARG(XmNbottomOffset    , obottom        ) }

  XtSetValues(w, args, i);
}




/*------------- miscellaneous helpful functions ------------------------*/

Widget get_shell_widget(Widget w)
{
  Widget shell = w;

  while(shell && XtIsWidget(shell))
       {
       if(XtIsShell(shell)) return shell;
       shell = XtParent(shell);
       }
  return NULL;
}



Widget get_toplevel_shell(Widget w)
{
  Widget shell = w, toplevel = NULL;

  while(shell && XtIsWidget(shell))
       {
       toplevel = shell;
       shell = XtParent(shell);
       }
  return toplevel;
}



Widget get_shell_child(Widget w)
{
  WidgetList children;
  Cardinal num_children=0, num_widget_children=0;
  Widget shell;
  Widget ret_child= NULL;
  int i;

  shell = get_shell_widget(w);
  if(!shell) return NULL;
  XtVaGetValues(shell, XmNnumChildren, &num_children,
                       XmNchildren   , &children    , NULL);

  for(i= 0; (i<num_children); i++) {
        if (XtIsWidget(children[i])) {
                 ret_child= children[i];
                 num_widget_children++;
        }
  }

  if (num_widget_children == 2 &&
    !strcmp(XtName(children[1]),"TipShell")) {
    ret_child = children[0];
  }
  else if (num_widget_children > 1) {
     printf("get_shell_child: shell has %d widget children - aborting.\n", 
             num_widget_children);
     assert(False); 
  }
  return ret_child;
}



/*---------------- set and unset the cursor ----------------------*/


void define_cursor(Widget w, Cursor cursor)
{
  Widget shell = get_shell_widget(w);
  if(shell) XDefineCursor(XtDisplay(shell), XtWindow(shell), cursor);
}



void undefine_cursor(Widget w)
{
  Widget shell = get_shell_widget(w);
  if(shell) XDefineCursor(XtDisplay(shell), XtWindow(shell), None);
}




typedef struct
{
  Cursor cursor;
} CarryCursor;


static void set_cursor_helper(Widget w, void *data)
{
  CarryCursor *data2 = (CarryCursor*)data;
  if(XtIsShell(w)) XDefineCursor(XtDisplay(w), XtWindow(w), data2->cursor);
}


void set_cursor_on_shells(Widget w, Cursor cursor)
{
  CarryCursor data;
  data.cursor = cursor;
  wprocTravWTree(w, set_cursor_helper, (void*)(&data));
  XFlush(XtDisplay(w));
}



void unset_cursor_on_shells(Widget w)
{
  set_cursor_on_shells(w, None);
}


/*----------- create, start, and stop watch cursor ------------------*/

  
static Widget toplevel_widget = NULL;
static Cursor watch_cursor;


void create_watch_cursor(Widget w)
{
  if(toplevel_widget) return;
  toplevel_widget = get_toplevel_shell(w);
  watch_cursor = XCreateFontCursor(XtDisplay(toplevel_widget), XC_watch);
}


void start_watch_cursor(void)
{
  set_cursor_on_shells(toplevel_widget, watch_cursor);
}


void stop_watch_cursor(void)
{
  unset_cursor_on_shells(toplevel_widget);
}

  

/*------------------- get full name of widget ------------------*/
  
void get_full_name(Widget w, char *name)   /* name must be large enough */
{
  char buffer[20][80];
  Widget p;
  int i, j=0;

  for (i=0, p=w;   i<20 && p;   i++, p=XtParent(p))
       {
       strncpy(buffer[i], XtName(p), 80);   j=i;
       buffer[i][79] = '\0';
       }
  strcpy(name, buffer[j]);
  for (i=j-1; i>=0; i--)
       {
       strcat(name, ".");
       strcat(name, buffer[i]);
       }
}




/*------------- function to create context-sensitive help --------------*/
       /* creates context-sensitive help when called the first time */
       /* returns the same help context each subsequent time called */
       /* gets helpfile and helptitle from application resources */
       /* the argument is any widget     */

struct HELPCTX *get_help(Widget w)
{
 typedef struct _Custom
     {
/*
     void *helpdata;
*/
     XrmDatabase helpdata;   /* changed from void *helpdata 12/19/94 for sun */
     char *helpfile, *helptitle;
     struct HELPCTX *helpctx;
     } Custom;
  static Custom custom;
  static XtResource resources[] = {
    { "helpfile",  "Helpfile",  XtRString, sizeof(String),
         XtOffsetOf(Custom,helpfile),  XtRString, "Application_help" },
    { "helptitle", "Helptitle", XtRString, sizeof(String),
         XtOffsetOf(Custom,helptitle), XtRString, "Help for Application" },
  };
  static Boolean start = TRUE;
  Widget toplevel;

  if(start)
       {
       toplevel = get_toplevel_shell(w);
       XtGetApplicationResources(toplevel, &custom,
                                resources, XtNumber(resources), NULL, 0);
       custom.helpdata = NULL;
       custom.helpctx = setup_help(toplevel, &custom.helpdata,
                             custom.helpfile, custom.helptitle);
       ctxh_set_csstr  (NULL,custom.helpctx);
       ctxh_set_overstr(NULL,custom.helpctx);
       start = FALSE;
       }
  return custom.helpctx;
}





/*------- event handler for managing spacing of children ---------------*/
/*------- called upon MapNotify and ConfigureNotify --------------------*/

static void spacing_event_handler(Widget w, void *data, XEvent *event)
{
    int i, j, offset;
    float spacing;
    Arg args[22];
    WidgetList children;
    Cardinal num_children;
    Dimension form_width, width, sum;

    i=0;
    XtSetArg(args[i], XmNchildren    , &children     ); i++;
    XtSetArg(args[i], XmNnumChildren , &num_children ); i++;
    XtSetArg(args[i], XmNwidth       , &form_width   ); i++;
    XtGetValues(w, args, i);
    if(num_children == 0 || children == NULL) return;
    for(j = 0, sum = 0; j < num_children; j++)
         {
         i=0;
         XtSetArg(args[i], XmNwidth, &width); i++;
         XtGetValues(children[j], args, i);
         sum += width;
         }
    if(form_width < sum)                                  /* added 2/17/98 */
         {                                                /* added 2/17/98 */
         i=0;                                             /* added 2/17/98 */
         XtSetArg(args[i], XmNwidth, &sum); i++;          /* added 2/17/98 */
         XtSetValues(w, args, i);                         /* added 2/17/98 */
         }                                                /* added 2/17/98 */
    spacing = ( (float)form_width - (float)sum ) / num_children;
    if(form_width < sum)                                  /* added 2/17/98 */
         {                                                /* added 2/17/98 */
         spacing = 0;                                     /* added 2/17/98 */
         }                                                /* added 2/17/98 */
    for(j = 0, sum = 0; j < num_children; j++)
         {
         offset = (j + 0.5) * spacing + sum + 0.5;
         i=0;
         XtSetArg(args[i], XmNrightAttachment, XmATTACH_NONE); i++;
         XtSetArg(args[i], XmNleftAttachment , XmATTACH_FORM); i++;
         XtSetArg(args[i], XmNleftOffset     , offset       ); i++;
         XtSetArg(args[i], XmNx          , (Position)offset ); i++;
         XtSetValues(children[j], args, i);
         i=0;
         XtSetArg(args[i], XmNwidth, &width); i++;
         XtGetValues(children[j], args, i);
         sum += width;
         }
    if(form_width < sum)                                  /* added 2/17/98 */
         {                                                /* added 2/17/98 */
         i=0;                                             /* added 2/17/98 */
         XtSetArg(args[i], XmNrightAttachment, XmATTACH_FORM); i++; /*added*/
         XtSetValues(children[num_children-1], args, i);            /*added*/
         }                                                /* added 2/17/98 */
}
  
 

void add_spacing_event_handler(Widget w)
{
 XtAddEventHandler(w, StructureNotifyMask, FALSE,
                (XtEventHandler)spacing_event_handler, NULL);
}




/*--------- routine for managing a widget ------------------------------*/
 /* if file selection box or message box, unmanages if already managed */
 /* if file selection box, does a file filter when managed */

void manage_widget(Widget w)
{
  WidgetClass class;

  if(!w || !XtIsWidget(w)) return;
  class = XtClass(w);
  if(class == xmFileSelectionBoxWidgetClass)
        {
        if (XtIsManaged(w)) XtUnmanageChild(w);
        else   
             {
             XmFileSelectionDoSearch(w, NULL);
             XtManageChild(w);
             }
        }
  else if(class == xmMessageBoxWidgetClass)
        {
        if (XtIsManaged(w)) XtUnmanageChild(w);
        else                XtManageChild(w);
        }
  else
        {
        XtManageChild(w);
        }
}

  

 
/*-------------- reduce the number of line segments --------------------*/

long reduce_segments(long x[], long y[], long number)
{
  int i, j, k, l = 0;
  float num, denom, slope, try, tolerance = 1.;

  if(number >= 0) return number;  /* this turns off this subroutine 3/10/97 */
  if(number <= 2) return number;
  for(i = 0; i < number-2; i++)
       {
       for(k = i+2; k < number; k++)
            {
            num = y[k] - y[i];
            denom = x[k] - x[i];
            if (denom) slope = num / denom;
            else       slope = 0.;
            for(j = i+1; j < k; j++)
                 {
                 try = y[i] + slope * (x[j] - x[i]);
                 if(fabs(try - y[j]) > tolerance)
                      {
                      l++;
                      x[l] = x[k-1];
                      y[l] = y[k-1];
                      i = k-2;      /* start now at point just established */
                      j = number;   /* jump out of j loop */
                      k = number;   /* jump out of k loop */
                      }
                 }
            }
       }
  l++;
  x[l] = x[number-1];
  y[l] = y[number-1];
  return l+1;
}


   

/*-------------- draw line segments ------------------------------------*/

void draw_segments(Widget w, GC gc, long x[], long y[], long number,
                                     int x1, int y1, int x2, int y2)
{
  int i, j = 0;
  XSegment *segments;
  float slope;
  
  if(number < 1 || w == NULL || gc == NULL ||!XtIsManaged(w)) return;
  segments = (XSegment *)malloc(number * sizeof(XSegment));
  if(number == 1)
       {
       segments[j].x1 = x[0];
       segments[j].y1 = y[0]; 
       segments[j].x2 = x[0];
       segments[j].y2 = y[0];
       j++;
       if(x1 || x2 || y1 || y2)
            {
            if(x[0] < x1 || x[0] > x2 || y[0] < y1 || y[0] > y2) j--;
            }
       }
  else
       {
       for(i = 0; i < number-1; i++)
            {
            if(x1 || x2 || y1 || y2)
                 {
                 if(x[i] <= x1 && x[i+1] <= x1) continue;
                 if(x[i] >= x2 && x[i+1] >= x2) continue;
                 if(y[i] <= y1 && y[i+1] <= y1) continue;
                 if(y[i] >= y2 && y[i+1] >= y2) continue;
                 }
            segments[j].x1 = x[i];
            segments[j].y1 = y[i]; 
            segments[j].x2 = x[i+1];
            segments[j].y2 = y[i+1];
            if((x1 || x2 || y1 || y2) && y[i+1] != y[i])
                 {
                 slope = (float)(x[i+1]-x[i]) / (float)(y[i+1]-y[i]);
                 if(y[i] < y1)
                      {
                      segments[j].x1 = x[i] + (y1-y[i]) * slope + 0.5;
                      segments[j].y1 = y1;
                      }
                 if(y[i+1] < y1)
                      {
                      segments[j].x2 = x[i+1] + (y1-y[i+1]) * slope + 0.5;
                      segments[j].y2 = y1;
                      }
                 if(y[i] > y2)
                      {
                      segments[j].x1 = x[i] + (y2-y[i]) * slope + 0.5;
                      segments[j].y1 = y2;
                      }
                 if(y[i+1] > y2)
                      {
                      segments[j].x2 = x[i+1] + (y2-y[i+1]) * slope + 0.5;
                      segments[j].y2 = y2;
                      }
                 }
            j++;
            }
       }
  if(j > 0) XDrawSegments(XtDisplay(w), XtWindow(w), gc, segments, j);
  free(segments);
}



/*------------ get color or graphics contexts ------------------------*/

Pixel get_named_color(Widget w, Colormap cmap,
                          char *cname, char *gsname, Boolean defblack)
{
  ColorScell color_scell;
  Display *disp = XtDisplay(w);

  if(!cmap) XtVaGetValues(w, XmNcolormap, &cmap, NULL);
/*
        this is not right if there is a private colormap:
  if(!cmap) cmap = DefaultColormap(disp, DefaultScreen(disp));
*/
  strcpy(color_scell.cname , cname );
  strcpy(color_scell.gsname, gsname);
  color_scell.defblack = defblack;
  alloc_scell(disp, cmap, &color_scell, False, NULL, NULL);
  return color_scell.pixel;
}


GC get_line_gc(Widget w, Colormap cmap, int line_width,
                          char *cname, char *gsname, Boolean defblack)
{
  Display *disp = XtDisplay(w);
  Window   wind = XtWindow (w);
  unsigned long mask = GCLineWidth | GCForeground;
  XGCValues values;

  values.line_width = line_width;
  values.foreground = get_named_color(w, cmap, cname, gsname, defblack);
  return XCreateGC(disp, wind, mask, &values);
}


GC get_rubber_gc(Widget w, int line_width)
{
  Display *disp = XtDisplay(w);
  Window   wind = XtWindow (w);
  unsigned long mask = GCLineWidth | GCFunction;
  XGCValues values;

  values.line_width = line_width;
  values.function   = GXinvert;
  return XCreateGC(disp, wind, mask, &values);
}



/*--------- rubberband drawing routines --------------------------------*/

struct _RubberbandStruct
{
  Display *disp;
  Window wind;
  GC gc;
  long x1, y1, x2, y2, direction;
} ;


#define RUBBERBAND_DRAW   \
  XDrawLine(rs->disp, rs->wind, rs->gc, rs->x1, rs->y1, rs->x2, rs->y2);


RubberbandStruct *rubberband_start(XButtonEvent *event, Widget w, GC gc)
{
  RubberbandStruct *rs;

  rs = (RubberbandStruct*)malloc(sizeof(RubberbandStruct));
  rs->disp = XtDisplay(w);
  rs->wind = XtWindow(w);
  rs->gc   = gc;
  rs->x1 = rs->x2 = event->x;
  rs->y1 = rs->y2 = event->y;
  RUBBERBAND_DRAW;       /* draw */
  return rs;
}


void rubberband_move(RubberbandStruct *rs, XMotionEvent *event)
{
  RUBBERBAND_DRAW;       /* undraw */
  rs->x2 = event->x;
  rs->y2 = event->y;
  RUBBERBAND_DRAW;       /* draw */
}


void rubberband_stop(RubberbandStruct *rs, XButtonEvent *event,
              long *x1, long *y1, long *x2, long *y2, long *direction)
{
  RUBBERBAND_DRAW;       /* undraw */
  rs->x2 = event->x;
  rs->y2 = event->y;
  if(rs->x2 < rs->x1)
       {
       rs->x1 = rs->x1 ^ rs->x2 ; rs->y1 = rs->y1 ^ rs->y2 ;
       rs->x2 = rs->x1 ^ rs->x2 ; rs->y2 = rs->y1 ^ rs->y2 ;
       rs->x1 = rs->x1 ^ rs->x2 ; rs->y1 = rs->y1 ^ rs->y2 ;
       rs->direction = BACKWARD;
       }
  else
       {
       rs->direction = FORWARD;
       }
  *x1 = rs->x1;
  *y1 = rs->y1;
  *x2 = rs->x2;
  *y2 = rs->y2;
  *direction = rs->direction;
  free(rs);
}

Bool wpDoesSaveUnders(Screen *screen)
{
	Bool retval;

	retval = DoesSaveUnders(screen);

	if (retval)
	{
		char *vendor = ServerVendor(DisplayOfScreen(screen));
		if (!strcmp(vendor, "Hewlett-Packard Company"))
			retval = False;
	}

	return retval;
}

/*
 * test code for wpDoesSaveUnders
 *
 *main(int argc, char **argv)
 *{
 *	XtAppContext	app;
 *	Widget		toplevel;
 *	
 *	toplevel = XtVaAppInitialize(&app, "Junk", NULL, 0, (Cardinal *) &argc,
 *		argv, NULL, NULL);
 *
 *	printf("wpDoesSaveUnders:  %u\n", wpDoesSaveUnders(XtScreen(toplevel)));
 *
 *	XtAppMainLoop(app);
 *}
 */


/*------------------- unattach all the children of a form ------------------*/
void unattach(Widget w)
{
  WidgetList wlist;
  Cardinal   numw;
  int i;
 
  if (w) {
      XtVaGetValues(w, XmNchildren,    &wlist,
                    XmNnumChildren, &numw, NULL );

      for(i= 0; (i<numw); i++)
         XtVaSetValues(wlist[i],
                   XmNtopAttachment, XmATTACH_NONE,
                   XmNleftAttachment, XmATTACH_NONE,
                   XmNrightAttachment, XmATTACH_NONE,
                   XmNbottomAttachment, XmATTACH_NONE,
                   NULL );
  }
}
/*--------------------------- end --------------------------------------*/
