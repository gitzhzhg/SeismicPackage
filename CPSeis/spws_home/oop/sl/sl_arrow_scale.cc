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
#include "sl/sl_arrow_scale.hh"
#include "sl/sl_movie_order.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_push_box.hh"
#include <Xm/Scale.h>
#include <Xm/Form.h>
#include <Xm/ArrowB.h>
#include <Xm/PushB.h>
#include <stdio.h>
#include <stdlib.h>


/*
 * ==================================================================
 *                        SLArrowScale Class
 * ==================================================================
 */



static String  res[]= {
               ".scale.orientation:       HORIZONTAL",
               ".scale.showValue:         True",
               ".Larrow.arrowDirection:   ARROW_LEFT",
               ".Rarrow.arrowDirection:   ARROW_RIGHT",
               ".XmArrowButton.width:           40",
               ".XmArrowButton.height:          40",
               "*XmArrowButton.shadowThickness:  0",
               NULL };




SLArrowScale::SLArrowScale(  SLDelay  *contain,
                             char     *name,
                             HelpCtx  hctx,
                             int      *valptr,
                             Boolean  dowrap, 
                             Boolean  make_if_can,
                             Boolean  use_filenames ) 

            : SLDelay(contain, name, hctx, False), _value(1), 
              _valptr(valptr), _dowrap(dowrap), _was_arrow(False),
              _use_filenames(use_filenames), _use_movie_order(False),
              _movie_order_set(False), _sequence_value(1)
{
  setDefaultResources( contain->pW()->display(), name, res);
  supportUnmadeDefaults(contain);
  if ((contain->made())&&(make_if_can)) make(contain->topWidget());

}

SLArrowScale::SLArrowScale(  Widget   p,
                             char     *name,
                             HelpCtx  hctx,
                             int      *valptr,
                             Boolean  dowrap,
                             Boolean  make_now ,
                             Boolean  use_filenames) 

            : SLDelay(p, name, hctx, False), _value(1), 
               _valptr(valptr), _dowrap(dowrap), _was_arrow(False),
               _use_filenames(use_filenames), _use_movie_order(False),
               _movie_order_set(False), _sequence_value(1)
{
  setDefaultResources( p, name, res);
  if (make_now) make(p);
  supportUnmadeDefaults(p);
}

SLArrowScale::~SLArrowScale()
{
 delete _scale;
 delete _movie_order;
}


Widget SLArrowScale::make(Widget p)
{


  if ( made() ) return topWidget();
  SLDelay::make(p);

  Widget w;
  w = XtVaCreateManagedWidget( instanceName(), topClass(), 
                               makeFrameIfNeeded(p), NULL);
  setTopWidget(w);


  _Larrow = XtVaCreateManagedWidget( "Larrow", xmArrowButtonWidgetClass, w, 
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNbottomAttachment,  XmATTACH_FORM, NULL);

  _Rarrow = XtVaCreateManagedWidget( "Rarrow", xmArrowButtonWidgetClass, w,
                                 XmNrightAttachment, XmATTACH_FORM,
                                 XmNbottomAttachment,  XmATTACH_FORM, NULL);

  _scale= new SLWkScale(w, getHelpCtx(), this, _valptr);
  _value= _scale->getScaleValue();
  _scale->getRange( &_low, &_high);
  if (_dowrap) _low++, _high--;

  if ((_value<_low)||(_value>_high)) setValue( _low);


  _order_push = XtVaCreateManagedWidget( "Change Order",
                              xmPushButtonWidgetClass, w,
                              XmNtopAttachment,    XmATTACH_WIDGET,
                              XmNtopWidget,        _scale->W(),
                              XmNtopOffset,        1,
                              XmNleftAttachment,   XmATTACH_WIDGET,
                              XmNleftWidget,       _Larrow,
                              XmNleftOffset,       1, 
                              XmNrightAttachment,  XmATTACH_WIDGET,
                              XmNrightWidget,      _Rarrow,
                              XmNrightOffset,      1, NULL);


  XtVaSetValues( _scale->W(), XmNtopAttachment,   XmATTACH_FORM,
                              XmNleftAttachment,  XmATTACH_WIDGET,
                              XmNleftWidget,      _Larrow,
                              XmNrightAttachment, XmATTACH_WIDGET,
                              XmNrightWidget,     _Rarrow, NULL );


  XtAddCallback( _Larrow, XmNactivateCallback,
                        (XtCallbackProc)ArrowPressCallback, (XtPointer)this );
  XtAddCallback( _Rarrow, XmNactivateCallback,
                        (XtCallbackProc)ArrowPressCallback, (XtPointer)this );
  XtAddCallback( _order_push, XmNactivateCallback,
                        (XtCallbackProc)PushButtonCallback, (XtPointer)this );


  _movie_order = new SLMovieOrder(topWidget(), "Movie_Order", getHelpCtx(),
                                  this, _use_filenames);

  return topWidget();

}



void SLArrowScale::ArrowPressCallback( Widget w, 
                                       XtPointer udata,
                                       XmArrowButtonCallbackStruct *CBdata)
{
  SLArrowScale *obj = (SLArrowScale *)udata;
  obj->ArrowPress(w, udata, CBdata);
}



void SLArrowScale::ArrowPress( Widget w, XtPointer,
                               XmArrowButtonCallbackStruct *)

{

  if(_use_movie_order) {
    _sequence_value = _value = getMovieOrderValue( w == _Larrow );
    _movie_order_set = True;
  }
  else {
    if (w == _Larrow) {
        if (_dowrap) {
              if (_value == _low) _value= _high;
              else                _value--;
        } // End if
        else {
              if (_value > _low)  _value--;
        } // End else
    } // End if 
 
    else if  (w == _Rarrow) {
        if (_dowrap) {
              if (_value == _high) _value= _low;
              else                 _value++;
        } // End if
        else {
              if (_value < _high) _value++;
        } // End else
    } // End if 
  }

  _was_arrow= True;
  _scale->setScaleValue( _value);
  //ValueChangeAction( _value);
  //callNotifyComplex(_value);
  _was_arrow= False;
  _movie_order_set = False;
}


void SLArrowScale::PushButtonCallback( Widget w, 
                                       XtPointer udata,
                                       XmPushButtonCallbackStruct *CBdata)
{
SLArrowScale *obj = (SLArrowScale *)udata;

  obj->_movie_order->makeAndManage();

}


void SLArrowScale::clearSelections()
{
  _use_movie_order = False;
  _movie_order->unmanage();
  _movie_order->clearSelections();
}


void SLArrowScale::setRange(int low, int high)
{

  //This method should be called as a plot or frame is added or deleted so
  //we will use it to set the sensitivity and unmanagement of the 
  //movie order options if we have less than 3 plots or frames.
  if(high <= 2 || high < _high)
    {
    clearSelections();
    if(high <= 2) XtSetSensitive(_order_push, False);
    }
  else
    {
    XtSetSensitive(_order_push, True);
    }


  _low= low;
  _high= high;
  if ( (_value < _low) || (_value > _high) )
         setValue( _low);
  if (_dowrap) {
         _scale->setRange(low-1, high+1);
  } // End if
  else
         _scale->setRange(low, high);
}

void SLArrowScale::setValue(int val)
{

 if ( (val >= _low) && (val <= _high) ) {
       _value= val;
       _scale->setScaleValue( val);
 } // End if
 else {
  printf("SLArrowScale::setValue: value passed - %d, is out of range\n",val); 
  printf("SLArrowScale::setValue: current range is %d - %d\n", _low, _high); 
 } // End if

}

void SLArrowScale::ValueChangeAction( long ) { }


void SLWkScale::ScaleActionVC( int val)
{

  if(_sa->_use_movie_order)
    {
    if(_sa->_movie_order_set == False)
      {
      Boolean decrementing;
      if (_sa->_value > 0) {
	decrementing = val < _sa->_value;
      }
      else {
	decrementing = val < _sa->_low;
      }
      _sa->_sequence_value = val = _sa->getMovieOrderValue(decrementing);
      XmScaleSetValue( topWidget(), val );
      }
    }

  if (_sa->_dowrap) {
       if (val > _sa->_high) {
              XmScaleSetValue( topWidget(), _sa->_low );
              val= _sa->_low;
       } // end if
       if (val < _sa->_low)  {
              XmScaleSetValue( topWidget(), _sa->_high );
              val= _sa->_high;
       } // end if
  } // End if

  _sa->_value= val; 
  if (_sa->_valptr) *_sa->_valptr= val; 
  _value= val; 
  //_sa->_was_arrow= False;
  _sa->_movie_order_set = False;
  if (_valptr) *_valptr= val; 
  _sa->ValueChangeAction( val);
  _sa->callNotifyComplex(_value);

}

void SLWkScale::ScaleActionDrag( int val)
{
  int oldvalue= _sa->_value;


  //If we are doing a movie order we must disable the
  //the movie order option when the user drags the scale.
  //If we allowed the movie order to change the scale bar to the next
  //legitimate value it would cause the scale bar to move uncontrollably
  //since the user has it pressed with his mouse
  if(_sa->_use_movie_order)
    {
      //Do nothing
    }

  if (_sa->_dowrap) {
       if (val > _sa->_high)  {
                XmScaleSetValue( topWidget(), _sa->_high );
                val= _sa->_high;
       }
       else if (val < _sa->_low) {
                XmScaleSetValue( topWidget(), _sa->_low );
                val= _sa->_low;
       }
  } // End if
  _sa->_value= val; 
  if (_sa->_valptr) *_sa->_valptr= val; 
  _value= val; 
  if (_valptr) *_valptr= val; 

  //_sa->_was_arrow= False;
  if (oldvalue != val) {
           _sa->ValueChangeAction( val);
           _sa->callNotifyComplex(val);
  }

  _sa->_movie_order_set = False;

}


Boolean SLArrowScale::doingDrag()
{
 return _scale->doingDrag();
}


long SLArrowScale::getNumFrames()
{
  return _high;
}

void SLArrowScale::setFilename(int i, char *filename)
{
  assert(i < MAX_FRAMES);
  strcpy(&_filenames[i][0], filename);
}

char *SLArrowScale::getFilename(int i)
{
  return &_filenames[i][0];
}

int SLArrowScale::getMovieOrderValue(Boolean decrementing)
{
int new_val;

  if(decrementing)
    new_val = _movie_order->getPreviousFrame(_value);
  else
    new_val = _movie_order->getNextFrame(_value);

  return new_val;
}


int SLArrowScale::isMovieOrderValue(int val)
{
  return _movie_order->isMovieOrderValue(val);
}


/*
 * ================== reload defaults ==========================
 */
void SLArrowScale::reloadDefaults(Boolean do_method)
{
   _scale->reloadDefaults(False);
   _value=  _scale->getScaleValue();

   if      (_value > _high)  _scale->setScaleValue( _high);
   else if (_value < _low)   _scale->setScaleValue( _low);
   
   //_was_arrow= False;
   if (do_method) { 
        ValueChangeAction(_value);
        callNotifyComplex(_value);
   }
}
