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
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "sl/view_win.hh"
#include <Xm/Separator.h>




void ViewObj::setXY( Position x, Position y)
{
  if (x != DontChange) XtVaSetValues( topWidget(), XmNx, x,  NULL);
  if (y != DontChange) XtVaSetValues( topWidget(), XmNy, y,  NULL);
}

void ViewObj::setDim( const Dimension width, const Dimension height)
{
  if (width  != DontChange) XtVaSetValues( topWidget(), XmNwidth, width,  NULL);
  if (height != DontChange) XtVaSetValues( topWidget(), XmNheight, height,NULL);
}


int ViewObj::opposite( int dir)
{
 int retval;

 switch (dir) {
    case OnTop    : retval= OnBottom; break;
    case OnBottom : retval= OnTop;    break;
    case OnRight  : retval= OnLeft;   break;
    case OnLeft   : retval= OnRight;  break;
 } // end Switch

 return (retval);
}


Dimension ViewObj::parentHeight() 
{ 
   Dimension h;
   XtVaGetValues( XtParent(topWidget()), XmNheight, &h,  NULL);
   return h;
};

Dimension ViewObj::parentWidth()
{ 
   Dimension w;
   XtVaGetValues( XtParent(topWidget()), XmNwidth, &w,  NULL);
   return w;
}


Dimension ViewObj::myHeight() 
{ 
   Dimension h;
   XtVaGetValues( topWidget(), XmNheight, &h,  NULL);
   return h;
};

Dimension ViewObj::myWidth()
{ 
   Dimension w;
   XtVaGetValues( topWidget(), XmNwidth, &w,  NULL);
   return w;
}


Position ViewObj::myX()
{ 
   Position x;

   if(_use_requested_x_location) return _requested_x_location;

   XtVaGetValues( topWidget(), XmNx, &x,  NULL); 
   return x;
};

Position ViewObj::myY()
{ 
   Position y;
   XtVaGetValues( topWidget(), XmNy, &y,  NULL);
   return y;
};

Position ViewObj::wX(Widget w)
{ 
   Position x;
   XtVaGetValues( w, XmNx, &x,  NULL); 
   return x;
};

Position ViewObj::wY(Widget w)
{ 
   Position y;
   XtVaGetValues( w, XmNy, &y,  NULL);
   return y;
};

Dimension ViewObj::wWidth(Widget w)
{ 
   Dimension width;
   XtVaGetValues( w, XmNwidth, &width,  NULL);
   return width;
};

Dimension ViewObj::wHeight(Widget w)
{ 
   Dimension height;
   XtVaGetValues( w, XmNheight, &height,  NULL);
   return height;
};
