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
#ifndef SLFRAME_H
#define SLFRAME_H

#include <Xm/Frame.h>
#include "sl/sl_delay.hh"
#include "wproc.h"



class SLFrame : public SLBase {


  protected:

  public:
     SLFrame(  Widget        p,
               char          *name,
               Boolean       make_now = True ) :
                SLDelay(name,NULL)  { if (make_now) make(p);
                                      else supportUnmadeDefaults(p);}

     SLFrame(  Display       *dpy,
               char          *name ) :
                SLDelay(name,NULL)  {}

     SLFrame(  PsuedoWidget  *pw,
               char          *name ) :
                SLDelay(name,NULL)  { supportUnmadeDefaults(pw);}

     virtual void make(Widget p =NULL) 
          { SLDelay::make(p);
            _topw= XtVaCreateManagedWidget( _name, topClass(), p, NULL); }

     virtual WidgetClass topClass() { return(xmFrameWidgetClass); };
};

#endif
