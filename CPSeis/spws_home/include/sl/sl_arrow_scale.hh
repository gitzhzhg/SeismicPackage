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
#ifndef SLARROWSCALE_H
#define SLARROWSCALE_H

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "wproc.h"
#include "sl/sl_delay.hh"
#include "sl/sl_scale.hh"


typedef void (*SLRadioButtonfunc)(void*,long);




class SLWkScale;


class SLArrowScale : public SLDelay {

  private:
    static void ArrowPressCallback(Widget, XtPointer, 
                                   XmArrowButtonCallbackStruct*);
    static void PushButtonCallback(Widget, XtPointer, 
                                   XmPushButtonCallbackStruct*);
  protected:
    void ArrowPress(Widget, XtPointer, XmArrowButtonCallbackStruct*);
    virtual void ValueChangeAction( long value);
    virtual int getMovieOrderValue(Boolean decrementing);
    virtual int isMovieOrderValue(int val);
    int         _value;
    int         _sequence_value;
    int         *_valptr;
    Widget      _Larrow;
    Widget      _Rarrow;
    SLScaleDrag *_scale;
    Widget      _order_push;
    int         _high;
    int         _low;
    Boolean     _dowrap;
    Boolean     _was_arrow;
    Boolean     _use_filenames;
    Boolean     _use_movie_order;
    Boolean     _movie_order_set;
    class SLMovieOrder *_movie_order;
    

  public:
    SLArrowScale(Widget   p,
                 char    *name,
                 HelpCtx  hctx           =NULL,
                 int     *valptr         =NULL,
                 Boolean  dowrap         =True,
                 Boolean  make_now       =True,
                 Boolean  user_filenames =False );

    SLArrowScale(SLDelay *contain,
                 char    *name,
                 HelpCtx  hctx          =NULL,
                 int     *valptr        =NULL,
                 Boolean  dowrap        =True,
                 Boolean  make_if_can   =True,
                 Boolean  use_filenames =False );

    enum{MAX_FRAMES = 100000};

    virtual ~SLArrowScale();
    virtual Widget make(Widget p =NULL);
    void    setValue(int);
    int     getValue() { return (_scale->getScaleValue() ) ;};
    void    clearSelections();
    void    setRange(int, int);
    Boolean arrowAction() { return (_was_arrow); }
    Widget  leftW()  { return _Larrow;};
    Widget  rightW() { return _Rarrow;};
    Widget  scaleW() { return _scale->W();};
    void    setWrap(Boolean dowrap) 
                   { _dowrap= dowrap; setRange(_high, _low); };
    friend  class SLWkScale;
    virtual void reloadDefaults(Boolean do_method= True);
    virtual WidgetClass topClass() { return(xmFormWidgetClass); };
    Boolean doingDrag();
    virtual long getNumFrames();
    void    setFilename(int i, char *filename);
    virtual char* getFilename(int i);
    char    _filenames[MAX_FRAMES] [512];   
    void    setUseMovieOrder(Boolean use){_use_movie_order = use;}

};


class SLWkScale : public SLScaleDrag {
   protected:
       virtual void    ScaleActionDrag(int);
       virtual void    ScaleActionVC(int);
       SLArrowScale    *_sa;
   public:
       SLWkScale(  Widget        p,
                   HelpCtx       hctx,
                   SLArrowScale  *sa,
                   int           *valptr = NULL )
            : SLScaleDrag(p, "scale", hctx, valptr), _sa(sa) {};
       ~SLWkScale(){};
};

#endif
