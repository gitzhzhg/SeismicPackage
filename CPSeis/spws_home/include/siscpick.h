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

/*------------------------- siscpick.h ----------------------------------*/

           /* sisc correlation picking in cbyt */

#ifndef _SISCPICK_H
#define _SISCPICK_H


#include "wproc.h"
#include "image.h"


#define SISCPICK_STARTING      1  /* when starting up the picking */
#define SISCPICK_SCANL         2  /* scan left */
#define SISCPICK_SCANR         3  /* scan right */
#define SISCPICK_MOVIEL        4  /* movie left using arrow button  */
#define SISCPICK_MOVIER        5  /* movie right using arrow button  */
#define SISCPICK_MOVIELOOPING  6  /* movie using scale widget */
#define SISCPICK_ZOOMING       7  /* zooming and unzooming */
#define SISCPICK_UPDATING      8  /* update button on control panel */


/*-------------------- function prototypes -----------------------------*/

#ifdef __cplusplus
extern "C" {                          /* for C++ */
#endif


/*-----Create structure for siscpick dialog box and return structure pointer. */
             /* xxx(ddd, FP_OK)  and  xxx(ddd, FP_CANCEL) */

void *siscpick_pop (Widget parent, struct HELPCTX *hctx,
        struct PlotImage *image, int line_width, void (*xxx)(), void *ddd);


/*-----Pop up the siscpick dialog box for beginning picking. */

void siscpick_begin_pop (void *siscpop);


/*-----Pop up the siscpick dialog box for subsequent info/control. */

void siscpick_control_pop (void *siscpop);


/*-----Draw picks in the specified region. */

void siscpick_draw (void *siscpop, int x, int y, int width, int height);


/*-----Whether to act on picking actions. */

void siscpick_picking_set (void *siscpop, Boolean act);


/*-----Call this when data is initially read in, after scanning, after
       zooming or un-zooming, or after movielooping. */

void siscpick_new_display (void *siscpop, long why);


/*-----Call this before scanning, before zooming or un-zooming, or 
       before movielooping. */

void siscpick_end_display (void *siscpop);


/*-----Call this at any time when picking is being terminated for any reason.
       This routine calls siscpick_end_display as a safety measure. */

void siscpick_stop (void *siscpop);



#ifdef __cplusplus
}
#endif


#endif

/*--------------------------- end --------------------------------------*/

