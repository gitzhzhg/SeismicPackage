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
 * Name        : make_okbox
 * File        : make_okbox.c
 * Executable  : -
 * Author      : Trey Roby
 * Date        : 11/1/91
 *
 * This functions makes an unmanaged message box with only an OK button. 
 * the dialog type (warning, error, etc) is passed as the third parameter.
 *
 * PARAMETERS:
 *             Widget        parent - parent widget
 *             char          name[] - widget name
 *             unsigned char type   - the dialog type (warning, error, etc),
 *                                    this is a motif constant that is
 *                                    documented under XmMessageBox widget,
 *                                    the XmNdialogType resource in the
 *                                    programmers reference.
 *
 *             RETURNS  Widget      - the widget ID of the dialog box
 *
 * 
 * CHANGES:
 */

/* -------------------------------------------------------------------------*/

#include "cenv.h"
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>




/*
 * ------------------------------ FUNCTION ----------------------------------
 */
Widget make_okbox( Widget         parent,
                   char           name[],
#ifdef CRAY
                   long  type )
#else
                   unsigned char  type )
#endif

{
  Widget box, tempw;
  Arg arglist[3];
  int n;

   /*
    * create the dialog box 
    */ 
   box= XmCreateMessageDialog( parent, name, NULL, 0);
   /*
    * set the type that the user passed
    */
   n=0;
   XtSetArg(arglist[n], XmNdialogType, type); n++;
   XtSetValues( box, arglist, n);
   /*
    * get rid of the buttons
    */
   tempw=XmMessageBoxGetChild( box, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild(tempw);
   tempw=XmMessageBoxGetChild( box, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild(tempw);

   return (box);       /* return the new widget ID */
}
