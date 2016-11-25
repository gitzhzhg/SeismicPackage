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
#ifndef _Window_h
#define _Window_h

#include <X11/Core.h>
#include <Xm/Xm.h>	/* ehs   07Jan94 */

/****************************************************************
 *
 * Window widget
 *
 ****************************************************************/

/* Resources:

 Name			 Class			RepType		Default Value
 ----			 -----			-------		-------------
 XiNwindowID	 XiCWindowID	Window		NULL

*/

/* declare specific WindowWidget class and instance datatypes */

typedef struct _WindowClassRec*	WindowWidgetClass;
typedef struct _WindowRec*		WindowWidget;

#define XiNwindowID "windowID"
#define XiCWindowID "WindowID"

/* declare the class constant */

#ifndef VMS
extern WidgetClass xiWindowWidgetClass;
#else
#ifdef __GNUC__
externalref (WidgetClass, xiWindowWidgetClass);
#else
externalref  WidgetClass  xiWindowWidgetClass ;
#endif /* __GNUC__ */
#endif /* VMS */

#endif /* _Window_h */
