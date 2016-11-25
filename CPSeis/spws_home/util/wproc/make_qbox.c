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
 * Name        : make_qbox
 * File        : make_qbox.c
 * Executable  : -
 * Author      : Trey Roby
 * Date        : 11/1/91
 *
 * This functions makes an unmanaged question message box with an OK and Cancel
 * button.  It also sets callbacks for when these buttons are pushed.
 *
 * PARAMETERS:
 *             Widget        parent    - parent widget
 *             char          name[]    - widget name
 *             void         (*okcb)()  - the OK routine to be called.  If it is
 *                                       NULL no callback is added.
 *             void         (*cancb)() - the Cancel routine to be called.  If 
 *                                       it is NULL no callback is added.
 *             void          *cbdata   - a pointer to the data that is to be
 *                                       passed to the callbacks.
 *
 *
 *             RETURNS  Widget      - the widget ID of the error dialog box
 *
 * 
 * CHANGES:
 */

/* -------------------------------------------------------------------------*/

#include "cenv.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>




/*
 * ------------------------------ FUNCTION ----------------------------------
 */
Widget make_qbox( Widget parent,
                  char   name[],
                  void   (*okcb)(),
                  void   (*cancb)(),
                  void   *cbdata  )

{
  Widget qbox, tempw;

  /*
   * create the unmanaged question message box and ax the help button
   */
  qbox= XmCreateQuestionDialog( parent, name, NULL, 0);
  tempw=XmMessageBoxGetChild( qbox, XmDIALOG_HELP_BUTTON );
  XtUnmanageChild(tempw);
  /*
   * if an ok callback passed the add the callback
   */
  if (okcb)
      XtAddCallback( qbox, XmNokCallback, okcb, cbdata);
  /*
   * if a cancel callback passed the add the callback
   */
  if (cancb)
      XtAddCallback( qbox, XmNcancelCallback, cancb, cbdata);

  return (qbox);      /* return the new widget ID */
}
