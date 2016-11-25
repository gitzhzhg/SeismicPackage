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

//---------------------- sl_row_column.cc -----------------------//
//---------------------- sl_row_column.cc -----------------------//
//---------------------- sl_row_column.cc -----------------------//

//          implementation file for the SLRowColumn class
//                  derived from the SLDelay class
//                        subdirectory sl


#include "sl/sl_row_column.hh"


//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//
//-------------- constructors and destructor ----------------//


SLRowColumn::SLRowColumn(  SLDelay *slparent,
                           char    *name,
                           HelpCtx  hctx,
                           Boolean  doframe,
                           Boolean  make_if_can,
                           Boolean  manage_now,
                           int      direction  )
            : SLDelay(slparent,name, hctx, doframe),
                 _manage_now (manage_now),
                 _direction  (direction)
{
  supportUnmadeDefaults(slparent);
  if (slparent->made() && make_if_can) make();
}


SLRowColumn::SLRowColumn(  Widget   wparent,
                           char    *name,
                           HelpCtx  hctx,
                           Boolean  doframe,
                           Boolean  make_now,
                           Boolean  manage_now,
                           int      direction  )
            : SLDelay(wparent, name, hctx, doframe),
                 _manage_now (manage_now),
                 _direction  (direction)
{
  supportUnmadeDefaults(wparent);
  if(make_now) make();
}


SLRowColumn::SLRowColumn(  Widget   w,
                           HelpCtx  hctx,
                           Boolean  make_now,
                           Boolean  manage_now,
                           int      direction  )
            : SLDelay(XtName(w), hctx),
                 _manage_now (manage_now),
                 _direction  (direction)
{
  setTopWidget(w);
  if(make_now) make();
}


SLRowColumn::~SLRowColumn()
{
}



//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//
//-------------------------- make ----------------------------//

Widget SLRowColumn::make(Widget p)
{
  if(!made())
       {
       Widget w = SLDelay::make(p);
       if(!w)
           {
           w = XmCreateRowColumn(makeFrameIfNeeded(wParent()), _name, NULL, 0);
           setTopWidget(w);
           }
       if(_manage_now) XtManageChild(w);
       if(_direction == _HORIZONTAL)
            XtVaSetValues(w, XmNorientation, XmHORIZONTAL, NULL);
       else if(_direction == _VERTICAL)
            XtVaSetValues(w, XmNorientation, XmVERTICAL  , NULL);
       }
  makeChildren();
  return topWidget();
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
