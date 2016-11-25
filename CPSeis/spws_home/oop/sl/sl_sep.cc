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

//------------------------- sl_sep.cc -------------------------------//
//------------------------- sl_sep.cc -------------------------------//
//------------------------- sl_sep.cc -------------------------------//

//            implementation file for the SLSep class
//                derived from the SLDelay class
//                       subdirectory sl


#include "sl/sl_sep.hh"


//-------------- constructors and destructor --------------------//
//-------------- constructors and destructor --------------------//
//-------------- constructors and destructor --------------------//


SLSep::SLSep(SLDelay *slparent, char *name, long direction)
           : SLDelay(slparent, name),
                  _direction (direction)
{
  if(slparent->topWidget()) make();
}


SLSep::SLSep(Widget    wparent, char *name, long direction)
           : SLDelay( wparent, name),
                  _direction (direction)
{
  make();
}


SLSep::SLSep(Widget w)
           : SLDelay(XtParent(w), XtName(w)),
                  _direction (_UNKNOWN)
{
  setTopWidget(w);
  make();
}


SLSep::~SLSep(void)
{
}


//------------------------ make --------------------------//
//------------------------ make --------------------------//
//------------------------ make --------------------------//

static String defres[] = {
     ".orientation: HORIZONTAL",
     NULL };


Widget SLSep::make(Widget p)
{
  if(!made())
     {
     Widget w = SLDelay::make(p);
     if(!w)
        {
        setDefaultResources(XtDisplay(wParent()), instanceName(), defres);
        Arg args[5];
        int i = 0;
        if(_direction == _HORIZONTAL)
             { XtSetArg(args[i], XmNorientation, XmHORIZONTAL); i++; }
        else if(_direction == _VERTICAL)
             { XtSetArg(args[i], XmNorientation, XmVERTICAL  ); i++; }
        w = XmCreateSeparator(wParent(), (char*)instanceName(), args, i);
        setTopWidget(w);
        XtManageChild(w);
        }
     }
  return topWidget();
}


//---------------------------- end ----------------------------------//
//---------------------------- end ----------------------------------//
//---------------------------- end ----------------------------------//
