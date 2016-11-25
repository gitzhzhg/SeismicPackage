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

/*-------------------------- refpick.h ----------------------------------*/

#ifndef _REFPICK_H
#define _REFPICK_H


#include "wproc.h"
#include "image.h"

#define REFPICK_STARTING      1  /* when starting up the picking */
#define REFPICK_SCANL         2  /* scan left */
#define REFPICK_SCANR         3  /* scan right */
#define REFPICK_MOVIEL        4  /* movie left using arrow button  */
#define REFPICK_MOVIER        5  /* movie right using arrow button  */
#define REFPICK_MOVIELOOPING  6  /* movie using scale widget */
#define REFPICK_ZOOMING       7  /* zooming and unzooming */
#define REFPICK_UPDATING      8  /* update button on control panel */


/*-------------------- function prototypes -----------------------------*/

#ifdef __cplusplus
extern "C" {                          /* for C++ */
#endif


/*-----Create structure for refpick dialog box and return structure pointer. */
             /* xxx(ddd, FP_OK)  and  xxx(ddd, FP_CANCEL) */

void *refpick_pop (Widget parent, HelpCtx hctx,
        struct PlotImage *image, int line_width, void (*xxx)(), void *ddd);


/*-----Pop up the refpick dialog box for beginning picking. */

void refpick_begin_pop (void *refpop);


/*-----Pop up the refpick dialog box for subsequent info/control. */

void refpick_control_pop (void *refpop);


/*-----Draw picks in the specified region. */

void refpick_draw (void *refpop, int x, int y, int width, int height);


/*-----Whether to act on picking actions. */

void refpick_picking_set (void *refpop, Boolean act);


/*-----Call this when data is initially read in, after scanning, after
       zooming or un-zooming, or after movielooping. */

void refpick_new_display (void *refpop, long why);


/*-----Call this before scanning, before zooming or un-zooming, or 
       before movielooping. */

void refpick_end_display (void *refpop);


/*-----Call this at any time when picking is being terminated for any reason.
       This routine calls refpick_end_display as a safety measure. */

void refpick_stop (void *refpop);



#ifdef __cplusplus
}
#endif


#endif

/*--------------------------- end --------------------------------------*/

