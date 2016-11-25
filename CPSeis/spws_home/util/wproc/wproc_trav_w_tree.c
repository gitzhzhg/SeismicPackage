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
#include <stdio.h>
#include <assert.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include "dumb_manage.h"


Boolean is_a_manager(Widget w);


void wprocTravWTree(Widget w,
                    void   (*func)(Widget, void*),
                    void   *data )
{
  WidgetList wlist;
  WidgetList plist;
  Cardinal   numw;
  Cardinal   nump;
  int i;

  func(w, data); 
  if ( XtIsComposite(w) ) {
         XtVaGetValues(w, XmNchildren,    &wlist,
                          XmNnumChildren, &numw, NULL ); 
         get_popup_list(w, &plist, &nump);
         for(i= 0; (i<numw); i++) {
             if (is_a_manager(wlist[i])) wprocTravWTree(wlist[i], func, data);
             else                        func(wlist[i], data); 
         } /* end loop */
         if (plist) {
             for(i= 0; (i<nump); i++) {
                 wprocTravWTree(plist[i], func, data);
             }
         }
  } /* end if */
}



Boolean is_a_manager(Widget w)
{
  Boolean result;

  if (XtIsComposite(w)) {
       if ( ( XtIsShell(w)                               ) ||
            ( XtClass(w) == xmBulletinBoardWidgetClass   ) ||
            ( XtClass(w) == xmDrawingAreaWidgetClass     ) ||
            ( XtClass(w) == xmFrameWidgetClass           ) ||
            ( XtClass(w) == xmPanedWindowWidgetClass     ) ||
            ( XtClass(w) == xmRowColumnWidgetClass       ) ||
            ( XtClass(w) == xmScrolledWindowWidgetClass  ) ||
            ( XtClass(w) == xmMainWindowWidgetClass      ) ||
            ( XtClass(w) == dumbManagerWidgetClass      ) ||
            ( XtClass(w) == xmFormWidgetClass            ) )
                  result= True;
       else
                  result= False;
  } /* end if */
  else
       result= False;

  return (result);
}
