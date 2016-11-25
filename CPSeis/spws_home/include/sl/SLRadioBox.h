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
#ifndef WPROCRADIOBOX_H
#define WPROCRADIOBOX_H

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include "wproc.h"
#include "sl/sl_delay.hh"


typedef struct _SLRadio {
                          char *name;
                          int select_value;
                        } SLRadio, *SLRadioAry;


typedef struct _SLRadioW {
                          Widget w;
                          char *name;
                          int   select_value;
                         } SLRadioW, *SLRadioAryW;

typedef void (*SLRadioButtonfunc)(void*,long);

class SLRadioBox : public SLDelay {

  private:

    static void WhichButtonCallback(Widget, XtPointer,
                                    XmToggleButtonCallbackStruct*);
    SLRadioButtonfunc _altChoiceAction;
    void             *_altChoiceData;

  protected:
    virtual void WhichButton(Widget, XtPointer,
                             XmToggleButtonCallbackStruct*);
    virtual void ChoiceAction( long button);
    Boolean     _value_set;
    int  _curr_choice;
    SLRadioAryW  _togary;
    void  preCreate(int &);
    Boolean      _dotitle;
    unsigned int _arycnt;
    long         *_target;
    void init( const Display *dpy, SLRadioAry togary);

  public:
     SLRadioBox( Widget           p,
                 char             *name,
                 HelpCtx          hctx,
                 SLRadioAry       togary,
                 unsigned int     arycnt,
                 long             *target =NULL, 
                 Boolean          doframe =False,
                 Boolean          dotitle =False,
                 Boolean          make_now=True );

     SLRadioBox( SLDelay          *contain,
                 char             *name,
                 HelpCtx          hctx,
                 SLRadioAry       togary,
                 unsigned int     arycnt,
                 long             *target =NULL, 
                 Boolean          doframe =False,
                 Boolean          dotitle =False,
                 Boolean          make_if_can =True );

     SLRadioBox( PsuedoWidget     *pw,
                 char             *name,
                 HelpCtx          hctx,
                 SLRadioAry       togary,
                 unsigned int     arycnt,
                 long             *target =NULL, 
                 Boolean          doframe =False,
                 Boolean          dotitle =False);
     ~SLRadioBox();
     void SetRadio( long );               // the select_value is passed
     Widget GetRadioWidget( long );       // the select_value is passed
     int WhichSelected();
     void setAltChoiceAction( SLRadioButtonfunc action, void *data) 
                                   { _altChoiceAction= action;
                                     _altChoiceData  = data; };

     virtual Widget make(Widget p =NULL);
     virtual WidgetClass topClass() { return(xmRowColumnWidgetClass); };
     virtual void reloadDefaults(Boolean do_method= True);

};
#endif
