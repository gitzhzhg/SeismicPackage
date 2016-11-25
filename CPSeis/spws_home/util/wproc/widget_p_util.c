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
 * sep 14 - ed's hack stoped working with R5  - TR
 */
/*
#ifdef ultrix
#define _XTAPPSTRUCT
#endif
*/

#ifdef _XTAPPSTRUCT
#include <X11/IntrinsicI.h>
#endif

#include <Xm/XmP.h>

#ifndef _XTAPPSTRUCT
	typedef struct {
		char pad1[12];
		Display **list;
#ifndef vms
		char pad2[156 - sizeof(Display **)];
#else
		char pad2[156 - sizeof(Display **) + sizeof(unsigned long)];
#endif
		short count;
	} myXtAppStruct;
#endif


void get_popup_list(Widget w, WidgetList *plist, Cardinal *nump)
{
  *plist= w->core.popup_list;
  *nump=  w->core.num_popups;
}


char *class_name( Widget w)
{
 return (w->core.widget_class->core_class.class_name);
}

char *classptr_name( WidgetClass wc)
{
 return (wc->core_class.class_name);
}

void appContextToDpyList(XtAppContext app, Display ***list, int *count)
{
#ifdef _XTAPPSTRUCT
	*list  =       app->list ;
	*count = (int) app->count;
#else
	*list  =       ((myXtAppStruct *) app)->list ;
	*count = (int) ((myXtAppStruct *) app)->count;
#endif
}
