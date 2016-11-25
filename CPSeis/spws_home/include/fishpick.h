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

/*------------------------- fishpick.h ----------------------------------*/

           /* fish correlation picking in cbyt */

#ifndef _FISHPICK_H
#define _FISHPICK_H


#include "wproc.h"
#include "image.h"


#define FISHPICK_STARTING      1  /* when starting up the picking */
#define FISHPICK_SCANL         2  /* scan left */
#define FISHPICK_SCANR         3  /* scan right */
#define FISHPICK_MOVIEL        4  /* movie left using arrow button  */
#define FISHPICK_MOVIER        5  /* movie right using arrow button  */
#define FISHPICK_MOVIELOOPING  6  /* movie using scale widget */
#define FISHPICK_ZOOMING       7  /* zooming and unzooming */
#define FISHPICK_UPDATING      8  /* update button on control panel */


/*-------------------- function prototypes -----------------------------*/

#ifdef __cplusplus
extern "C" {                          /* for C++ */
#endif


/*-----Create structure for fishpick dialog box and return structure pointer. */
             /* xxx(ddd, FP_OK)  and  xxx(ddd, FP_CANCEL) */

void *fishpick_pop (Widget parent, struct HELPCTX *hctx,
        struct PlotImage *image, int line_width, void (*xxx)(), void *ddd);


/*-----Pop up the fishpick dialog box for beginning picking. */

void fishpick_begin_pop (void *fishpop);


/*-----Pop up the fishpick dialog box for subsequent info/control. */

void fishpick_control_pop (void *fishpop);


/*-----Draw picks in the specified region. */

void fishpick_draw (void *fishpop, int x, int y, int width, int height);


/*-----Whether to act on picking actions. */

void fishpick_picking_set (void *fishpop, Boolean act);


/*-----Call this when data is initially read in, after scanning, after
       zooming or un-zooming, or after movielooping. */

void fishpick_new_display (void *fishpop, long why);


/*-----Call this before scanning, before zooming or un-zooming, or 
       before movielooping. */

void fishpick_end_display (void *fishpop);


/*-----Call this at any time when picking is being terminated for any reason.
       This routine calls fishpick_end_display as a safety measure. */

void fishpick_stop (void *fishpop);



#ifdef __cplusplus
}
#endif


#endif

/*--------------------------- end --------------------------------------*/

