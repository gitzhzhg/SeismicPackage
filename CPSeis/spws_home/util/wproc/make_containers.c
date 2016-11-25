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
C      make_containers.c
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C              written in c -- designed to be called from c
C
C     Utility Name:  make_containers    (create container widgets)
C          Written:  93/02/01  by:  Tom Stoeckley
C     Last revised:  93/02/01  by:  Tom Stoeckley
C
C  Purpose:   Shorthand routines to create container widgets and
C             separator widgets, and provide additional functionality. 
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine:                ultrix
C  source code directory:  ~spws/util/trslib
C  library:                trslib.a
C  source files:           make_containers.c   make_containers.h
C----------------------------------------------------------------------
C             DIFFERENCES BETWEEN CODE ON DIFFERENT MACHINES
C                                n/a
C-----------------------------------------------------------------------
C                ROUTINES ON SOURCE FILE make_containers.c
C
C                 Documented routines are listed later.
C             Undocumented static routines are listed here.
C
C                    make_line2   make_rowcolumn2
C-----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C     (C, X, Xt, and Motif libraries and header files not included)
C
C  Libraries:     trslib.a   wproc.a
C  Header files:  wproc.h    widget_util.h
C-----------------------------------------------------------------------
C                 EXTERNALS REFERENCED BY THIS UTILITY
C          (C, X, Xt, and Motif library routines not included)
C
C                add_HELP     add_spacing_event_handler
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2.
C  1. 93/02/01  Stoeckley  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                     DEFINITIONS OF ARGUMENTS
C
C  Widget             w = created widget (returned).
C  Widget        parent = parent of widget to create.
C  String          name = name of widget to create.
C  String         label = label or title to display in widget.
C  struct HELPCTX *hctx = pointer to help context (added to widget).
C
C  Either of the arguments label or hctx can be NULL.
C-----------------------------------------------------------------------
C  To create a pulldown menu:
C
C               w = make_pulldown (parent, name, label, hctx)
C
C  Parent should be the menubar widget.
C  Creates both the pulldown menu and the associated cascade button.
C      The pulldown menu has "pd" appended to the given name.
C      The cascade button has "cas" appended to the given name.
C  Returns the pulldown menu widget, which is to be the parent of
C      the menu items (usually pushbuttons).
C  Context-sensitive help and label are added to the cascade button.
C  If the name is "help", this menu is put at the right of the menu bar.
C-----------------------------------------------------------------------
C  To create an unmanaged dialog shell:
C
C                    w = make_shell(parent, name, label)
C
C  Returns a form widget which is the child of the shell.
C  The shell widget has "_shell" appended to its name.
C  Parent should normally be the toplevel or main window widget.
C  Sets autoUnmanage to False and resizePolicy to RESIZE_NONE.
C  Sets horizontalSpacing and verticalSpacing to 4.
C-----------------------------------------------------------------------
C  To create a managed container for buttons at bottom of dialog box:
C
C                    w = make_bottom(parent, name)
C
C  Attaches to parent (assumed to be a form) on bottom and sides.
C  Manages the spacing of its children when reconfigured.
C-----------------------------------------------------------------------
C  To create various managed container widgets:
C
C     w = make_small           (parent, name)   small form
C     w = make_form            (parent, name)   form
C     w = make_bull            (parent, name)   bulletin board
C     w = make_draw            (parent, name)   drawing area
C     w = make_row             (parent, name)   horizontal rowcolumn
C     w = make_column          (parent, name)   vertical rowcolumn
C     w = make_radiobox        (parent, name)   radiobox
C     w = make_option          (parent, name)   option menu
C
C     w = make_framed_form     (parent, name)   form
C     w = make_framed_bull     (parent, name)   bulletin board
C     w = make_framed_draw     (parent, name)   drawing area
C     w = make_framed_row      (parent, name)   horizontal rowcolumn
C     w = make_framed_column   (parent, name)   vertical rowcolumn
C     w = make_framed_radiobox (parent, name)   radiobox
C
C  For the form widget, sets horizontalSpacing and verticalSpacing to 4.
C  Where indicated, puts a frame around the widget.
C  The frame widget has "_frame" appended to its name.
C  The radiobox is a vertical rowcolumn which allows inhomogeneous
C     children, with XmNpacking = XmPACK_TIGHT.
C-----------------------------------------------------------------------
C  To create a managed separator widget:
C
C           w = make_hline(parent, name)    horizontal separator
C           w = make_vline(parent, name)    vertical separator
C-----------------------------------------------------------------------
C                                NOTES
C
C 1.
C-----------------------------------------------------------------------
C\END DOC
*/



/*----------------------- header files ---------------------------------*/

#include <stdio.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <Xm/DrawingA.h>
#include <Xm/BulletinB.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/DialogS.h>
#include <X11/StringDefs.h>
#include "wproc.h"
#include "cprim.h"


#define WWNN       Widget parent, String name
 


/*------------- routine for creating a pulldown menu -------------------*/
/*----sets appropriate resource on menubar widget for help pulldown ----*/
                     /* parent is menubar */

Widget make_pulldown(WWNN, char *label, struct HELPCTX *hctx)
{
  Widget w_pane, w;
  int i;
  Arg args[5];
  char pdname[80], casname[80];

  strcpy(pdname , name);   strcat(pdname , "pd");
  strcpy(casname, name);   strcat(casname, "cas");

  i=0;
  w_pane = XmCreatePulldownMenu(parent, pdname, args, i);
  i=0;
  XtSetArg(args[i], XmNsubMenuId, w_pane); i++;
  w = XmCreateCascadeButton(parent, casname, args, i);
  if(label) set_widget_cvar(w, label);
  XtManageChild(w);
  if(hctx) add_HELP(w, helper, hctx);
  if(!strcmp(name, "help"))
       {
       i=0;
       XtSetArg(args[i], XmNmenuHelpWidget, w); i++;
       XtSetValues(parent, args, i);
       }
  return w_pane;
}



/*------------- routine for creating an option menu --------------------*/

Widget make_option(WWNN, char *label, struct HELPCTX *hctx)
{
  Widget w_pane, w, wbutton;
  int i;
  Arg args[5];
  char pdname[80], optname[80];

  strcpy(pdname , name);   strcat(pdname , "pd");
  strcpy(optname, name);   strcat(optname, "opt");

  i=0;
  w_pane = XmCreatePulldownMenu(parent, pdname, args, i);
  i=0;
  XtSetArg(args[i], XmNsubMenuId, w_pane); i++;
  w = XmCreateOptionMenu(parent, optname, args, i);
  if(label) set_compound_resource(w, XmNlabelString, label);
  wbutton = XmOptionButtonGadget(w);
  XtVaSetValues(wbutton, XmNalignment, XmALIGNMENT_BEGINNING, NULL);
  XtVaSetValues(w_pane, XmNuserData, w, NULL);
  XtManageChild(w);
  if(hctx) add_HELP(w, helper, hctx);
  return w_pane;
}



/*------- routine for creating an unmanaged form dialog shell ----------*/
                    /* parent may be toplevel */

Widget make_shell(WWNN, char *label)
{
  Widget w;
  int i;
  Arg args[5];

  i=0;
  XtSetArg(args[i], XmNautoUnmanage, False); i++;
  XtSetArg(args[i], XmNresizePolicy, XmRESIZE_NONE); i++;
  XtSetArg(args[i], XmNhorizontalSpacing, 4); i++;
  XtSetArg(args[i], XmNverticalSpacing, 4); i++;
  w = XmCreateFormDialog(parent, name, args, i);
  if(label) set_widget_cvar(XtParent(w), label);
  return w;
}



/*------ routine for creating a managed frame widget -------------------*/
           /* appends "_frame" to the given name */

Widget make_frame(WWNN)
{
  Widget w;
  char fname[80];

  strcpy(fname, name);   strcat(fname, "_frame");
  w = XmCreateFrame(parent, fname, NULL, 0);
  XtManageChild(w);
  return w;
}



/*------ routine for creating a managed widget for bottom buttons ------*/
    /* attaches to parent (assumed to be a form) on bottom and sides */
         /* manages the spacing of its children when reconfigured */

Widget make_bottom(WWNN)
{
  Widget w;

/*
  w = make_form(parent, name);
*/
  w = make_bull(parent, name);
  XtVaSetValues(w, XmNtopAttachment   , XmATTACH_NONE,
                   XmNleftAttachment  , XmATTACH_FORM,
                   XmNrightAttachment , XmATTACH_FORM,
                   XmNbottomAttachment, XmATTACH_FORM, NULL);
  add_spacing_event_handler(w);
  return w;

}
                 /* puts a frame around the widget */
/*
Widget make_bottom(WWNN)
{
  Widget temp, w;

  temp = make_frame(parent, name);
  XtVaSetValues(temp, XmNtopAttachment   , XmATTACH_NONE,
                      XmNleftAttachment  , XmATTACH_FORM,
                      XmNrightAttachment , XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_FORM, NULL);
  w = make_form(temp, name);
  add_spacing_event_handler(w);
  return w;
}
*/


/*--------- create a managed form, bulletin board, or drawing area -----*/

Widget make_small(WWNN)
{
  Widget w;

  w = XmCreateForm(parent, name, NULL, 0);
  XtManageChild(w);
  return w;
}


Widget make_form(WWNN)
{
  Widget w;
  int i;
  Arg args[5];

  i=0;
  XtSetArg(args[i], XmNhorizontalSpacing, 4); i++;
  XtSetArg(args[i], XmNverticalSpacing, 4); i++;
  w = XmCreateForm(parent, name, args, i);
  XtManageChild(w);
  return w;
}


Widget make_bull(WWNN)
{
  Widget w;

  w = XmCreateBulletinBoard(parent, name, NULL, 0);
  XtManageChild(w);
  return w;
}


Widget make_draw(WWNN)
{
  Widget w;

  w = XmCreateDrawingArea(parent, name, NULL, 0);
  XtManageChild(w);
  return w;
}




/*------------- create a managed rowcolumn widget ----------------------*/

static Widget make_rowcolumn2(WWNN, unsigned char orientation)
{
  Widget w;
  int i;
  Arg args[5];

  i=0;
  XtSetArg(args[i], XmNorientation, orientation); i++;
  XtSetArg(args[i], XmNmarginHeight, 0); i++;
  XtSetArg(args[i], XmNmarginWidth , 0); i++;
  w = XmCreateRowColumn(parent, name, args, i);
  XtManageChild(w);
  return w;
}

Widget make_row(WWNN)
{ return make_rowcolumn2(parent, name, XmHORIZONTAL); }

Widget make_column(WWNN)
{ return make_rowcolumn2(parent, name, XmVERTICAL); }




/*---------------- create a managed radio box --------------------------*/

Widget make_radiobox(WWNN)
{
  Widget w;

  w = make_column(parent, name);
  XtVaSetValues(w, XmNradioBehavior, True        ,        
                   XmNpacking      , XmPACK_TIGHT, NULL); 
  XtVaSetValues(w, XmNisHomogeneous, False       , NULL);
  return w;
}




/*------------ create various managed framed widgets -------------------*/

#define FRAMEIT(MAKE_FRAMED_WIDGET,MAKE_WIDGET)  \
Widget MAKE_FRAMED_WIDGET(WWNN)                  \
{                                                \
  Widget frame = make_frame (parent, name);      \
  return         MAKE_WIDGET(frame , name);      \
}

FRAMEIT (make_framed_form    , make_form    )
FRAMEIT (make_framed_bull    , make_bull    )
FRAMEIT (make_framed_draw    , make_draw    )
FRAMEIT (make_framed_row     , make_row     )
FRAMEIT (make_framed_column  , make_column  )
FRAMEIT (make_framed_radiobox, make_radiobox)




/*------------ create a managed separator widget -----------------------*/

static Widget make_line2(WWNN, unsigned char orientation)
{
  Widget w;
  int i;
  Arg args[5];

  i=0;
  XtSetArg(args[i], XmNorientation, orientation); i++;
  w = XmCreateSeparator(parent, name, args, i);
  XtManageChild(w);
  return w;
}

Widget make_hline(WWNN)
{ return make_line2(parent, name, XmHORIZONTAL); }

Widget make_vline(WWNN)
{ return make_line2(parent, name, XmVERTICAL); }


/*--------------------------- end --------------------------------------*/

