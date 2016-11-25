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
 * Copyright(c) 1992 Bell Communications Research, Inc. (Bellcore)
 *                        All rights reserved
 * Permission to use, copy, modify and distribute this material for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies, and that the name of Bellcore not be used in advertising
 * or publicity pertaining to this material without the specific,
 * prior written permission of an authorized representative of
 * Bellcore.
 *
 * BELLCORE MAKES NO REPRESENTATIONS AND EXTENDS NO WARRANTIES, EX-
 * PRESS OR IMPLIED, WITH RESPECT TO THE SOFTWARE, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR ANY PARTICULAR PURPOSE, AND THE WARRANTY AGAINST IN-
 * FRINGEMENT OF PATENTS OR OTHER INTELLECTUAL PROPERTY RIGHTS.  THE
 * SOFTWARE IS PROVIDED "AS IS", AND IN NO EVENT SHALL BELLCORE OR
 * ANY OF ITS AFFILIATES BE LIABLE FOR ANY DAMAGES, INCLUDING ANY
 * LOST PROFITS OR OTHER INCIDENTAL OR CONSEQUENTIAL DAMAGES RELAT-
 * ING TO THE SOFTWARE.
 *
 * ClipWidget Author: Andrew Wason, Bellcore, aw@bae.bellcore.com
 */

/*
 * ClipP.h - Private definitions for Clip widget
 */

#ifndef _Xbae_ClipP_h
#define _Xbae_ClipP_h

/*
 *  "@(#)ClipP.h	3.4 7/8/92"
 */

#include <Xm/XmP.h>
#include "clip.h"

/*
 * New type for class method
 */

#if defined (__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

typedef void (*XbaeClipRedrawProc)(
#if NeedFunctionPrototypes
				   Widget	/* w */
#endif
				   );

#if defined (__cplusplus) || defined(c_plusplus)
}
#endif

/*
 * New fields for the Clip widget class record
 */
typedef struct {
    XbaeClipRedrawProc	redraw;
    XtPointer		extension;
} XbaeClipClassPart;

/*
 * Full class record declaration
 */
typedef struct _XbaeClipClassRec {
    CoreClassPart		core_class;
    XmPrimitiveClassPart	primitive_class;
    XbaeClipClassPart		clip_class;
} XbaeClipClassRec;

extern XbaeClipClassRec xbaeClipClassRec;

/*
 * New inheritance constant
 */
#define XbaeInheritRedraw ((XbaeClipRedrawProc) _XtInherit)

/*
 * New fields for the Clip widget record
 */
typedef struct {
    /* resources */
    XtExposeProc	expose_proc;	/* function to call on expose */
    XtCallbackList	focus_callback;	/* callbacks for when we get focus */
} XbaeClipPart;

/*
 * Full instance record declaration
 */
typedef struct _XbaeClipRec {
    CorePart		core;
    XmPrimitivePart	primitive;
    XbaeClipPart	clip;
} XbaeClipRec;

#endif /* _Xbae_ClipP_h */
