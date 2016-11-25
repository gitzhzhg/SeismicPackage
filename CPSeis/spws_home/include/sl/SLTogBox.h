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
#ifndef SLTOGBOX_HH
#define SLTOGBOX_HH

#include <Xm/RowColumn.h>
#include <Xm/Xm.h>
#include "wproc.h"
#include "sl/sl_delay.hh"


typedef enum {SL_NOTSET, SL_TRUE, SL_FALSE} SL_ThreeState;

typedef struct _SLTog {
                            char      *name;
                            long      *target;
                            long       ident;
                           } SLTog, *SLTogAry;


typedef struct _SLTogW {
                            Widget w;
                            char   *name;
                            long   *target;
                            long   ident;
                            SL_ThreeState value_ifset;
                           } SLTogW, *SLTogAryW;

typedef void (*SLToggleButtonfunc)(void*,long,Boolean);


class SLTogBox : public SLDelay {

  private:

    static void DoToggleCallback(Widget, XtPointer,
                                 XmToggleButtonCallbackStruct*);
    SLToggleButtonfunc _altTogAction;
    void              *_altTogData;
    static int errorHandler(Display *, XErrorEvent *);

  protected:
    virtual void  DoToggle(Widget, XtPointer,
                           XmToggleButtonCallbackStruct*);
    virtual void TogAction( long ident, Boolean set );
    long  retidx( long ident);
    void  preCreate(int &);
    SLTogAryW          _togary;
    unsigned int       _arycnt;
    Boolean            _dotitle;
    

    void init( const Display *dpy, SLTogAry togary);


  public:
    SLTogBox( Widget           p,
              char             *name,
              HelpCtx          hctx,
              SLTogAry         togary,
              unsigned int     arycnt,
              Boolean          doframe  =False,
              Boolean          dotitle  =False,
              Boolean          make_now =True );

    SLTogBox( PsuedoWidget     *pw,
              char             *name,
              HelpCtx          hctx,
              SLTogAry         togary,
              unsigned int     arycnt,
              Boolean          doframe  =False,
              Boolean          dotitle  =False );

    SLTogBox( SLDelay          *contain,
              char             *name,
              HelpCtx          hctx,
              SLTogAry         togary,
              unsigned int     arycnt,
              Boolean          doframe  =False,
              Boolean          dotitle  =False,
              Boolean          make_if_can =True );
    ~SLTogBox();

     void SetTog( long ident, Boolean state );
     Widget TogW( long ident );      
     Boolean IsSelected(long ident);
     virtual Widget make(Widget p =NULL);
     void setAltChoiceAction( SLToggleButtonfunc action, void *data)
                                   { _altTogAction= action;
                                     _altTogData  = data; };
     virtual WidgetClass topClass() { return(xmRowColumnWidgetClass); };
     virtual void reloadDefaults(Boolean do_method= True);
};



#endif
