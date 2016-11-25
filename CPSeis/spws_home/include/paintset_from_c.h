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

/*----------------------- paintset_from_c.h ---------------------------*/
/*----------------------- paintset_from_c.h ---------------------------*/
/*----------------------- paintset_from_c.h ---------------------------*/

/*********************
  This class is a C-callable interface to the C++ Paintset and
    PaintsetCollection classes.
  This class is callable from both C and C++, but is intended to be
    called from C.
*********************/

#ifndef _PAINTSET_FROM_C_H_
#define _PAINTSET_FROM_C_H_

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif

/*------------------------------ functions ---------------------------------*/
/*------------------------------ functions ---------------------------------*/
/*------------------------------ functions ---------------------------------*/

void      paintset_add_resources  (Screen *screen, Arg *arglist, int *n);
int       paintset_depth          (Screen *screen);
Visual   *paintset_visual         (Screen *screen);
Colormap  paintset_colormap       (Screen *screen);
Pixel     paintset_white          (Screen *screen);
Pixel     paintset_black          (Screen *screen);

/*--------------------------- end functions --------------------------------*/
/*--------------------------- end functions --------------------------------*/
/*--------------------------- end functions --------------------------------*/

#ifdef __cplusplus
}
#endif

#endif

/*------------------------------- end -------------------------------------*/
/*------------------------------- end -------------------------------------*/
/*------------------------------- end -------------------------------------*/

