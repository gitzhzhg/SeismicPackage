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

//---------------------- sl_paned_window.cc -----------------------//
//---------------------- sl_paned_window.cc -----------------------//
//---------------------- sl_paned_window.cc -----------------------//

//          implementation file for the SLPanedWindow class
//                  derived from the SLDelay class
//                        subdirectory sl


#include "sl/sl_paned_window.hh"


//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        ".spacing:      18",
        ".sashHeight:   14",
        ".sashWidth:    30",
            NULL };



//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//


SLPanedWindow::SLPanedWindow(  SLDelay *slparent,
                               char    *name,
                               HelpCtx  hctx,
                               Boolean  doframe,
                               Boolean  make_if_can,
                               Boolean  manage_now)
            : SLDelay(slparent,name, hctx, doframe),
                 _manage_now (manage_now)
{
  supportUnmadeDefaults(slparent);
  setFallbackResources(defres);
  if (slparent->made() && make_if_can) make();
}


SLPanedWindow::SLPanedWindow(  Widget   wparent,
                               char    *name,
                               HelpCtx  hctx,
                               Boolean  doframe,
                               Boolean  make_now,
                               Boolean  manage_now)
            : SLDelay(wparent, name, hctx, doframe),
                 _manage_now (manage_now)
{
  supportUnmadeDefaults(wparent);
  setFallbackResources(defres);
  if(make_now) make();
}


SLPanedWindow::SLPanedWindow(  Widget   w,
                               HelpCtx  hctx,
                               Boolean  make_now,
                               Boolean  manage_now)
            : SLDelay(XtName(w), hctx),
                 _manage_now (manage_now)
{
  setTopWidget(w);
  setFallbackResources(defres);
  if(make_now) make();
}


SLPanedWindow::~SLPanedWindow()
{
}



//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//

Widget SLPanedWindow::make(Widget p)
{
  if(!made())
       {
       Widget w = SLDelay::make(p);
       if(!w)
           {
           w = XmCreatePanedWindow
                             (makeFrameIfNeeded(wParent()), _name, NULL, 0);
           setTopWidget(w);
           }
       if(_manage_now) XtManageChild(w);
       }
  makeChildren();
  return topWidget();
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
