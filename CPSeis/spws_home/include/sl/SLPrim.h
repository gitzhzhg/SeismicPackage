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
#ifndef SLSCALE_H
#define SLSCALE_H

#include <Xm/Scale.h>
#include "wproc.h"
#include "sl/sl_delay.hh"



class SLScale : public SLDelay {


  private:

  protected:
     static void GetValueCallback(Widget, XtPointer, XmScaleCallbackStruct* );
     int             _value;
     int              *_valptr;
     virtual void    GetValue(Widget, XtPointer, XmScaleCallbackStruct* );
     virtual void    ScaleAction(int);
     virtual void    ScaleActionDrag(int);
     virtual void    ScaleActionVC(int);
     void            init(const Display*);
     Boolean         _value_set;
     Boolean         _doing_drag;

  public:
     SLScale(  Widget        p,
               char          *name,
               HelpCtx       hctx,
               int           *valptr = NULL,
               Boolean       make_now = True );
     SLScale(  SLDelay       *contain,
               char          *name,
               HelpCtx       hctx,
               int           *valptr =NULL,
               Boolean       make_if_can = True );
     SLScale(  PsuedoWidget  *pw,
               char          *name,
               HelpCtx       hctx,
               int           *valptr =NULL);
     virtual         ~SLScale() {};

     int GetScaleValue(){ return getScaleValue(); };
     int getScaleValue(){ if (_valptr) *_valptr= _value; return _value; };
     void setScaleValue(int val);
     void setRange(int, int);
     void getRange(int*, int*);
     virtual Widget make(Widget =NULL);
     virtual WidgetClass topClass() { return(xmScaleWidgetClass); };
     virtual void reloadDefaults(Boolean do_method= True);

};


class SLScaleDrag : public SLScale {
  protected:
  public:
     SLScaleDrag(  Widget        p,
                   char          *name,
                   HelpCtx       hctx,
                   int           *valptr = NULL,
                   Boolean       make_now = True ) :
              SLScale(p,name,hctx,valptr,False) {if (make_now) make(p); }

     SLScaleDrag(  SLDelay       *contain,
                   char          *name,
                   HelpCtx       hctx,
                   int           *valptr = NULL,
                   Boolean       make_if_can = True ) :
              SLScale(contain,name,hctx,valptr,False) 
            {if ((contain->made())&&(make_if_can)) make(contain->topWidget());}

     SLScaleDrag(  PsuedoWidget  *pw,
                   char          *name,
                   HelpCtx       hctx,
                   int           *valptr ) :
              SLScale(pw,name,hctx,valptr) {}
     virtual         ~SLScaleDrag() {};
     Boolean  doingDrag();
     Widget make(Widget p =NULL);
};
#endif
