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
 * Name        : pickfile
 * File        : pickfile.c
 * Executable  : -
 * Author      : Trey Roby
 * Date        : 11/1/91
 *
 * This routine sets the fileselection box up and sets up callbacks to 
 * pop it up and remove it as well.
 *
 * It is passed the text widget for the result; the push button widget
 * to pop the file sel box up; and a pointer to a character string where
 * the file name goes.
 *
 * The routine can optionally be passed a routine and data to be called when 
 * the OK, FILTER, or CANCEL button is pushed.  This routine is called after
 * the standard actions are taken.  This user specified routine is called 
 * just like a callback with the same parameters.
 * It passes an XmAnyCallbackStruct.  The reason is XmCR_OK when the 
 * OK button is pushed, XmCR_APPLY when the FILTER button is pushed, and
 * XmCR_CANCEL when the Cancel button is pushed.
 *
 * The file selection box Widget ID is returned.
 *
 * PARAMETERS:
 *               Widget text -       text widget to contain the  filename
 *               Widget push -       push button widget to popup the 
 *                                   file sel box 
 *               char *filename -    character pointer for filename that will
 *                                   be updated in the callback
 *               void (*morecb)() -  optional routine call on OK or Cancel
 *               void *data       -  data passed as user data to the option
 *                                   routine
 *
 *             RETURNS  Widget      - the widget ID of the file selection box
 *
 *
 * CHANGES:
 */

/* -------------------------------------------------------------------------*/
/* -------------------------------------------------------------------------*/

#include "fast_file_proc.h"
#include "cenv.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/FileSB.h>
#include <Xm/FileSBP.h>

#include <stdio.h>

#define LOCALDEBUG 0 



struct fileCB { Widget  filew;
                char    *filename;
                Widget  Fileshell;
                void    (*more_cb)();
                void     *data;  };


/* PROTOTYPE */
static void setfile(Widget w, struct fileCB*, XmAnyCallbackStruct *);
static void applyfile(Widget w, struct fileCB*, XmAnyCallbackStruct *);
static void diefile(Widget w, struct fileCB*, XmAnyCallbackStruct *);
/* PROTOTYPE */




/*
 * ------------------------------ FUNCTION ----------------------------------
 */
Widget pickfile( Widget text,          /* text widget for filename */
                 Widget push,          /* push button widget to popup box */
                 char   *filename,     /* character pointer for filename */
                 void    (*more_cb)(), /* function that could be called */
                 void    *data )       /* function data */

{

/*
 *  Create the file selection box, add the call backup to manage it to the 
 *  push button, add help.  Callback will update the text widget and the 
 *  filename.
 */


   Widget Fileshell, tempw;
   struct fileCB *pushinfo;
   Arg args[2];

     /*
      * Make three callback structures on for each button callback
      */
     pushinfo= (struct fileCB *)malloc( sizeof (struct fileCB) );

     /*
      * Initialize the structures
      */
     pushinfo->filew=  text;
     pushinfo->filename=  filename;

     /*
      * Add callback to push button widget which will 
      * popup the fileselection box
      */
     XtAddCallback( push, XmNactivateCallback, 
                    (XtCallbackProc)setfile, (XtPointer)pushinfo);

      /*
       * create transient widget for popup file box
       */
     XtSetArg (args[0], XmNdirSearchProc , fast_dir_proc );
     XtSetArg (args[1], XmNfileSearchProc, fast_file_proc);
     Fileshell= XmCreateFileSelectionDialog (
                   XtParent(text),     "Fileshell",
		   args, 2);

      pushinfo->Fileshell=  Fileshell;

      XtAddCallback( Fileshell, XmNokCallback, 
                     (XtCallbackProc)setfile, (XtPointer)pushinfo );

      XtAddCallback( Fileshell, XmNapplyCallback, 
                     (XtCallbackProc)applyfile, (XtPointer)pushinfo );

      XtAddCallback( Fileshell, XmNcancelCallback, 
                     (XtCallbackProc)setfile, (XtPointer)pushinfo );

      XtAddCallback( Fileshell, XmNdestroyCallback, 
                     (XtCallbackProc)diefile, (XtPointer)pushinfo );

      /*
       *  if define user wants a routine to when success and the ok 
       *  button is pushed
       */
      pushinfo->more_cb= more_cb; 
      pushinfo->data= data;
      /*
       *  unmanage the help button
       */
      tempw= XmFileSelectionBoxGetChild( Fileshell, XmDIALOG_HELP_BUTTON);
      XtUnmanageChild(tempw);
      

     return (Fileshell);   /* return the file selection box widget */
}

/***************************************************************************/
/*         End of Main code  - Now the callbacks                           */
/***************************************************************************/




/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static void setfile(Widget w, 
                     struct fileCB   *infoptr,
                     XmAnyCallbackStruct *calldata )

/*
 * this routine is called from the ok button and the cancel
 * button of the file selection box and the push button used to pop the
 * fileselection up
 */


{
  Boolean fman;
  Arg arglist[4] ;
  char *fname;
  int i, n;
  Widget tempw;
  XmString dirmask;


  fman=XtIsManaged( infoptr->Fileshell); /* see if file sel box is managed */

  XmProcessTraversal(infoptr->filew, XmTRAVERSE_CURRENT); 
  
  if (NOT fman) {        /* if file sel box is not managed the manage it */
      XtManageChild(infoptr->Fileshell);
      n=0;
      XtSetArg( arglist[n], XmNdirMask, &dirmask ); n++;
      XtGetValues( infoptr->Fileshell, arglist, n);
      XmFileSelectionDoSearch( infoptr->Fileshell, dirmask); 
      XmStringFree(dirmask);
  ENDif
  else {                /* othewise process and unmanage it */
      if (calldata->reason == XmCR_OK) {/* if the user selected a file 
                                         * the get the filename that he selected
                                         * and put it in text widget
                                         */
           tempw= XmFileSelectionBoxGetChild( 
                              infoptr->Fileshell, XmDIALOG_TEXT);
           fname= XmTextGetString( tempw); /* get the string */
           strcpy(infoptr->filename, fname); /* put it in the char array */
#          if LOCALDEBUG
               printf("file name = %s\n", fname);
#          endif
           i=0;
           XmTextSetString(infoptr->filew, fname); /*put it in a text widget */
           XtFree(fname); 
      ENDif
      XtUnmanageChild(infoptr->Fileshell);   /* remove the file sel box */

      /*
       * Call if an additional proc was specified.  This user specified
       * routine is called if the ok or the cancel button is pushed.
       * It is called with the same parameters the callback with the 
       * user specified data sent.
       */
      if (infoptr->more_cb)
             infoptr->more_cb( w, infoptr->data, calldata );
  ENDelse
}

/*
 * ------------------------------ FUNCTION ----------------------------------
 */
static void diefile(Widget w, 
                    struct fileCB   *infoptr,
                    XmAnyCallbackStruct *dummy)

/*
 * this routine is called when the file popup is destroyed
 */
{
   free (infoptr);
}

static void applyfile (Widget w, struct fileCB *infoptr,
  XmAnyCallbackStruct *calldata)
/*
 * this routine is called filter button of the file selection box
 */
{
  Arg arglist[4] ;
  int n;
  XmString dirmask;
  Widget filter_button_widget;
  Window filter_button_window;

  XmProcessTraversal (infoptr->filew, XmTRAVERSE_CURRENT); 
  
  if (calldata->reason == XmCR_APPLY) {
    /* this is an apply call back - find out if it was due to filter button */
    filter_button_widget = XmFileSelectionBoxGetChild (infoptr->Fileshell,
      XmDIALOG_APPLY_BUTTON);
    filter_button_window = XtWindow (filter_button_widget);
    if (calldata->event->xany.window == filter_button_window) {
      /* the event window is identical to the Filter button window */
      reset_file_list ();
      n=0;
      XtSetArg (arglist[n], XmNdirMask, &dirmask );
      n++;
      XtGetValues (infoptr->Fileshell, arglist, n);
      /* do the file search again */
      XmFileSelectionDoSearch (infoptr->Fileshell, dirmask); 
      XmStringFree (dirmask);
    }
  }
}
