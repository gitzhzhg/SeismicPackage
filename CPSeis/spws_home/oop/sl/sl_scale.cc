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
#include <Xm/Scale.h>
#include <stdio.h>
#include <stdlib.h>
#include "sl/sl_scale.hh"
#include "sl/psuedo_widget.hh"


/*
 * ==================================================================
 *                        SLScale Class
 * ==================================================================
 */



static String  scaleres[]= {
               ".orientation:       HORIZONTAL",
               ".showValue:         True",
               NULL };




SLScale::SLScale(  Widget        p,
                   char          *name,
                   HelpCtx       hctx,
                   int           *valptr,
                   Boolean       make_now)
           : SLDelay(name, hctx), _value(0), _valptr(valptr), 
             _value_set(False), _doing_drag(False)
{
 init( XtDisplay(p) );
 if (make_now) make(p);
 else {
     supportUnmadeDefaults(p);
     _value= _pw_topw->scaleDef();
 }
 if (_valptr) *_valptr= getScaleValue();
}

SLScale::SLScale(  SLDelay       *contain,
                   char          *name,
                   HelpCtx       hctx,
                   int           *valptr,
                   Boolean       make_if_can)
           : SLDelay(contain, name, hctx), _value(0), _valptr(valptr), 
             _value_set(False), _doing_drag(False)
{
 if (contain->made())  init( XtDisplay(contain->topWidget() ) );
 else                  init( (contain->pW())->display() );

 if ((contain->made())&&(make_if_can)) make(contain->topWidget());
 else {  
     supportUnmadeDefaults(contain->pW());
      _value= _pw_topw->scaleDef();
 }
 if (_valptr) *_valptr= getScaleValue();
}


SLScale::SLScale(  PsuedoWidget  *pw,
                   char          *name,
                   HelpCtx       hctx,
                   int           *valptr )
           : SLDelay(name, hctx), _value(0), _valptr(valptr),
             _value_set(False), _doing_drag(False)
{
  init(pw->display() );
  supportUnmadeDefaults(pw);
  _value= _pw_topw->scaleDef();
 if (_valptr) *_valptr= _value;
}


void SLScale::init( const Display *dpy)
{
  setDefaultResources( dpy, _name, scaleres);
}


Widget SLScale::make( Widget p)
{
  Widget w;

  if ( made() ) return topWidget();
  SLDelay::make(p);
  p= wParent();

  w = XtVaCreateManagedWidget( _name, topClass(), p, NULL );
  XtAddCallback( w, XmNvalueChangedCallback, 
                   (XtCallbackProc)GetValueCallback, (XtPointer)this);
  if (_value_set)
       XmScaleSetValue( w, _value);
  else
       XmScaleGetValue( w, &_value);

  setTopWidget(w);
  install_help();
  if (_valptr) *_valptr= _value;


  return topWidget();
}

void SLScale::GetValueCallback( Widget                w, 
                           XtPointer             udata,
                           XmScaleCallbackStruct *CBdata )
{
  SLScale *obj = (SLScale *)udata;

//   the following test on the callback structure event was to accomodate a
//   change in INTELSOL that resulted in callbacks occuring when our code
//   calls XtVaSetValue to set the scalebar value.  when these callbacks
//   are made, the callback structure event is nil, however, when these
//   callbacks occur as a result of gui actions the structure event is
//   non-nil.
  if (CBdata->event) 
    obj->GetValue(w, udata, CBdata);
}


void SLScale::GetValue( Widget,
                        XtPointer,
                        XmScaleCallbackStruct *CBdata )
{
  XmScaleGetValue( topWidget(), &_value);
  if (_valptr) *_valptr= _value;
  ScaleAction(_value);
  if (CBdata->reason == XmCR_DRAG) {
              _doing_drag= True;
              ScaleActionDrag(_value);
  }
  if (CBdata->reason == XmCR_VALUE_CHANGED) {
              _doing_drag= False;
              ScaleActionVC(_value);
  }
}

void SLScale::getRange(int *low, int *high)
{

  if ( made() ) {
      XtVaGetValues( topWidget(), XmNminimum, low, 
                            XmNmaximum, high, NULL);
      XmScaleGetValue( topWidget(), &_value);
      if (_valptr) *_valptr= _value;
  }
  else puts("SLScale::getRange class must be first made");
}

void SLScale::setRange(int low, int high)
{

  if ( made() ) {
      if ( (_value < low) || (_value > high) ) {
           XmScaleSetValue( topWidget(), low);
           _value= low;
      } // End if
      XtVaSetValues( topWidget(), XmNminimum, low, 
                            XmNmaximum, high, NULL);
      if (_valptr) *_valptr= _value;
  }
  else puts("SLScale::setRange class must be first made");
}

void SLScale::setScaleValue(int val)
{
  if (val != _value) {
      if ( made() ) {
         _value= val;
         if (_valptr) *_valptr= val;
         XmScaleSetValue(topWidget(), val);
      }
      else {
         _value_set= True;
         _value= val;
         if (_valptr) *_valptr= val;
    
      }
      ScaleAction(_value);
      ScaleActionVC(_value);
  } // end if
}

void SLScale::ScaleAction( int ) { }

void SLScale::ScaleActionDrag(int value)
{
  callNotifyComplex(value);
}
void SLScale::ScaleActionVC(int value)
{
  callNotifyComplex(value);
}


/*
 * ================== reload defaults ==========================
 */
void SLScale::reloadDefaults(Boolean do_method)
{
  if ( made() ) {
     DefLoadWValue(topWidget());
     XmScaleGetValue(topWidget(), &_value);
     if (do_method) {
          ScaleAction(_value);
          ScaleActionVC(_value);
     }
  } // End if
  else {
     _value= _pw_topw->scaleDef();
    if (_valptr) *_valptr= _value;
  }
}

Widget SLScaleDrag::make(Widget p)
{ 
  if (made()) return topWidget();
  SLScale::make(p);
  XtAddCallback( topWidget(), XmNdragCallback,
                 (XtCallbackProc)GetValueCallback, 
                 (XtPointer)this );
  return topWidget();
}

Boolean  SLScaleDrag::doingDrag() { return _doing_drag; }
