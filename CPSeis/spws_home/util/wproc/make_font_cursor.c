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
C      make_font_cursor.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C             written in c -- designed to be called from c
C
C     Utility Name:  make_font_cursor          (make a font cursor)
C          Written:  93/09/17  by:  Tom Stoeckley
C     Last revised:  93/09/17  by:  Tom Stoeckley
C
C  Purpose:       Make a font cursor to replace the usual pointer for
C                 temporary display.  For example, this font cursor
C                 could be displayed while waiting for a time-consuming
C                 task to be completed.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY     
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/wproc   (shared)
C  library:                wproc.a            (shared)
C  header file:            wproc.h            (shared)
C  source file:            make_font_cursor.c
C
C  static functions:       start_or_stop
C
C  documented functions:   make_font_cursor    add_font_cursor
C                          start_font_cursor   remove_font_cursor
C                          stop_font_cursor    destroy_font_cursor
C                          make_font_cursor_watch
C                          make_font_cursor_crosshair
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES 
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     wproc.a
C  header files:  wproc.h
C  functions:     none
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 93/09/17  Stoeckley  Initial version.
C-----------------------------------------------------------------------
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
C  To create a font cursor of any shape:
C
C                                         i    i
C                 fcs = make_font_cursor (w, shape)
C
C  Widget              w = any widget in the desired display.
C  unsigned int    shape = the desired shape of the mouse pointer.
C  FontCursorStruct *fcs = pointer to opaque structure.
C
C  The header file <X11/cursorfont.h> must be included in the code
C    which calls this routine; the shape parameter must be one of 
C    the constants found in this header file.
C-----------------------------------------------------------------------
C  Convenience routines to create special font cursors:
C
C                                         i 
C       fcs = make_font_cursor_watch     (w)     creates XC_watch
C       fcs = make_font_cursor_crosshair (w)     creates XC_crosshair
C       fcs = make_font_cursor_circle    (w)     creates XC_circle   
C
C  Widget              w = any widget in the desired display.
C  FontCursorStruct *fcs = pointer to opaque structure.
C
C  These routines call make_font_cursor.
C-----------------------------------------------------------------------
C  To add a shell to, or remove a shell from, the list of shells which
C    will display the font cursor:
C
C                                           i   i
C                void     add_font_cursor (fcs, w)
C                void  remove_font_cursor (fcs, w)
C
C  FontCursorStruct *fcs = pointer to opaque structure.
C  Widget              w = any widget in the desired shell.
C
C  The first routine must be called for each shell in which the font
C    cursor is to be displayed.  If this is to be a cursor being
C    displayed during a time-consuming operation, the toplevel shell,
C    plus any dialog shells which might be displayed, should be added.
C  If fcs or w is NULL, these routines will do nothing.
C  If the same shell is added more than once, the second attempt will
C    do nothing.
C  If a previously added shell is destroyed, the second routine must
C    be called prior to the destruction.
C  If an attempt is made to remove a shell which was not added, the
C    routine will do nothing.
C-----------------------------------------------------------------------
C  To start or stop display of the font cursor:
C
C                                              i
C                    void  start_font_cursor (fcs)
C                    void   stop_font_cursor (fcs)
C
C  FontCursorStruct *fcs = pointer to opaque structure.
C
C  Call the first  routine to begin displaying the font cursor.
C  Call the second routine to stop  displaying the font cursor.
C  These routines flush the buffer before returning.  Although this
C    action may not be necessary to force the font cursor to change
C    immediately, it is still done here to facilitate the flushing
C    of any other actions (such as displaying text) associated with
C    the operation occurring between the calls to these two routines.
C    For example, the user can reset the message in a text widget
C    prior to calling either of these routines, and be assured that
C    the message will show up immediately.
C-----------------------------------------------------------------------
C  To destroy the structure and associated font cursor:
C
C                                            i
C                fcs = destroy_font_cursor (fcs)
C
C  CursorFontStruct *fcs = pointer to opaque structure.
C
C  This routine always returns NULL.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC
*/


#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include "wproc.h"

#define NSHELLMAX 50


/*-------------------- opaque structure ------------------------------*/

struct _FontCursorStruct
  {
  Cursor cursor;
  Widget toplevel, shell[NSHELLMAX];
  Display *display;
  long nshell;
  Boolean showing;
  } ;




/*-------------------- make font cursor ------------------------------*/

FontCursorStruct *make_font_cursor(Widget w, unsigned int shape)
{
  FontCursorStruct *fcs;

  if(!w) return NULL;
  fcs = (FontCursorStruct*)calloc(1, sizeof(FontCursorStruct));
  if(!fcs) return NULL;
  fcs->toplevel = get_toplevel_shell(w);
  fcs->display = XtDisplay(fcs->toplevel);
  fcs->cursor = XCreateFontCursor(fcs->display, shape);
  fcs->nshell = 0;
  fcs->showing = False;
  return fcs;
}



/*------------ make font cursor (convenience routines) ---------------*/

FontCursorStruct *make_font_cursor_watch(Widget w)
{
  return make_font_cursor(w, XC_watch);
}


FontCursorStruct *make_font_cursor_crosshair(Widget w)
{
  return make_font_cursor(w, XC_crosshair);
}


FontCursorStruct *make_font_cursor_circle(Widget w)
{
  return make_font_cursor(w, XC_circle);
}



/*------------------------ add font cursor ---------------------------*/

void add_font_cursor(FontCursorStruct *fcs, Widget w)
{
  Widget shell;
  int i;

  if(!fcs || !w || fcs->nshell >= NSHELLMAX || fcs->showing) return;
  shell = get_shell_widget(w);
  if(!shell) return;
  for(i = 0; i < fcs->nshell; i++)
       {
       if(shell == fcs->shell[i]) return;
       }
  fcs->shell[fcs->nshell] = shell;
  fcs->nshell++;
}




/*------------------ remove font cursor ------------------------------*/

void remove_font_cursor(FontCursorStruct *fcs, Widget w)
{
  Widget shell;
  int i;

  if(!fcs || !w || fcs->showing) return;
  shell = get_shell_widget(w);
  if(!shell) return;
  for(i = 0; i < fcs->nshell; i++)
       {
       if(shell == fcs->shell[i]) fcs->shell[i] = NULL;
       }
  if(fcs->shell[fcs->nshell] == NULL) fcs->nshell--;
}




/*-------------------- destroy font cursor ---------------------------*/

FontCursorStruct *destroy_font_cursor(FontCursorStruct *fcs)
{
  if(!fcs) return NULL;
  XFreeCursor(fcs->display, fcs->cursor);
  free(fcs);
  return NULL;
}





/*-------------------- start or stop font cursor ---------------------*/

void start_or_stop(FontCursorStruct *fcs, Cursor cursor)
{
  int i;

  for(i = 0; i < fcs->nshell; i++)
       {
       if(fcs->shell[i])
           XDefineCursor(fcs->display, XtWindow(fcs->shell[i]), cursor);
       }
  XFlush(fcs->display);
}


void start_font_cursor(FontCursorStruct *fcs)
{
  if(!fcs) return;
  start_or_stop(fcs, fcs->cursor);
}


void stop_font_cursor(FontCursorStruct *fcs)
{
  if(!fcs) return;
  start_or_stop(fcs, None);
}


/*-------------------------- end -------------------------------------*/

