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

#ifndef DUMBMANAGP_H
#define DUMBMANAGP_H


#include <Xm/XmP.h>
#include <X11/Core.h>



/*#include <Xm/ManagerP.h> */
#include "dumb_manage.h"
#include "wproc.h"



/* 
 * New fields for the DumbManager widget CLASS record 
 */

typedef struct {
        int make_compiler_happy;        /* keep compiler happy */
} DumbManagerClassPart;


/* 
 * Full class record declaration 
 */
typedef struct _DumbManagerClassRec {
   CoreClassPart        core_class;
   CompositeClassPart   composite_class;
   ConstraintClassPart  constraint_class;
   XmManagerClassPart   manager_class;
   DumbManagerClassPart dumbmanager_class;
} DumbManagerClassRec;

extern  DumbManagerClassRec dumbManagerClassRec;



/* 
 * New fields for the DumbManager INSTANCE record 
 */
typedef struct {
        int make_compiler_happy;        /* keep compiler happy */
} DumbManagerPart;

/*
 * Full instance record declaration
 */
typedef struct _DumbManagerRec {
   CorePart         core;
   CompositePart    composite;
   ConstraintPart   constraint;
   XmManagerPart    manager;
   DumbManagerPart  dumbManager;
} DumbManagerRec;

#endif
/* DON'T ADD STUFF AFTER THIS #endif */
