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
#include <stdlib.h>
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include "wproc.h"
#include "sl/psuedo_widget.hh"
#include "sl/sl_radio_box.hh"

static String  defres[]= {
    "*XmToggleButton.alignment:   ALIGNMENT_BEGINNING",
    "*XmToggleButton.marginWidth: 0",
    "*XmToggleButton.marginHeight: 0",
    "*XmToggleButton.shadowThickness: 0",
    ".spacing:             0",

    NULL };


SLRadioBox::SLRadioBox( Widget           p,
                        char             *name,
                        HelpCtx          hctx,
                        SLRadioAry       togary,
                        unsigned int     arycnt,
                        long             *target, 
                        Boolean          doframe,
                        Boolean          dotitle,
                        Boolean          make_now )
          : SLDelay(p,name, hctx, doframe), _arycnt(arycnt), _target(target),
            _altChoiceAction(NULL), _altChoiceData(NULL), 
            _dotitle(dotitle), _value_set(False)
{
 init( XtDisplay(p), togary);
 if (make_now) make(p);
 else          supportUnmadeDefaults(p);
 if (_target) *_target= WhichSelected();
}

SLRadioBox::SLRadioBox( PsuedoWidget     *pw,
                        char             *name,
                        HelpCtx          hctx,
                        SLRadioAry       togary,
                        unsigned int     arycnt,
                        long             *target, 
                        Boolean          doframe,
                        Boolean          dotitle )
         : SLDelay(pw, name, hctx, doframe), _arycnt(arycnt), _target(target),
           _altChoiceAction(NULL), _altChoiceData(NULL), 
           _dotitle(dotitle), _value_set(False)
{
 supportUnmadeDefaults(pw);
 init(pw->display(), togary);
}

SLRadioBox::SLRadioBox( SLDelay          *contain,
                        char             *name,
                        HelpCtx          hctx,
                        SLRadioAry       togary,
                        unsigned int     arycnt,
                        long             *target, 
                        Boolean          doframe,
                        Boolean          dotitle,
                        Boolean          make_if_can)
     : SLDelay(contain, name, hctx, doframe), _arycnt(arycnt), _target(target),
       _altChoiceAction(NULL), _altChoiceData(NULL), 
       _dotitle(dotitle), _value_set(False)
{
 supportUnmadeDefaults(contain);
 init((contain->pW())->display(), togary);
 if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}

void SLRadioBox::init( const Display *dpy, SLRadioAry togary)
{
  setDefaultResources( dpy, _name, defres);
  _togary= (SLRadioAryW)calloc( _arycnt, sizeof ( SLRadioW) );
  for (int i= 0; (i<_arycnt); i++) {
        _togary[i].name= newstr( togary[i].name );
        _togary[i].select_value= togary[i].select_value;
  }
}


Widget SLRadioBox::make( Widget  p)
{
  int tnum= 0;
  Widget w;

  if ( made() ) return topWidget();
  SLDelay::make(p);

  p= wParent();

  w= XtVaCreateManagedWidget( _name, topClass(), makeFrameIfNeeded(p), 
                                  XmNadjustMargin,  False,
                                  XmNisAligned,     False, 
                                  XmNradioBehavior, True, 
                                  NULL);

  XtVaSetValues( w, XmNisHomogeneous,  False, NULL);
   
  setTopWidget(w);
 

  for (int i= 0; (i<_arycnt); i++) {
      if (_dotitle) preCreate(tnum);
      if ( strcmp( _togary[i].name, SLBlankSpace ) != 0 ) {
           _togary[i].w=            XtVaCreateManagedWidget( 
                                    _togary[i].name, xmToggleButtonWidgetClass,
                                    topWidget(), XmNuserData, 
                                    _togary[i].select_value, NULL );
           if (getHelpCtx()) add_HELP(_togary[i].w,helper,getHelpCtx());
           XtAddCallback( _togary[i].w, XmNvalueChangedCallback,
                        (XtCallbackProc)&WhichButtonCallback, (XtPointer)this );
           if (!_value_set)
                  if (XmToggleButtonGetState( _togary[i].w) )
                             _curr_choice=  _togary[i].select_value;
      } // End if
      else {
            Pixel color;            
            _togary[i].w=   XtVaCreateManagedWidget( "", 
                                  xmLabelWidgetClass, topWidget(), NULL);
            XtVaGetValues( _togary[i].w, XmNbackground, &color, NULL);
            XtVaSetValues( _togary[i].w, XmNforeground, color, NULL);
      } // End else
   
  } // End Loop
  if (_value_set) SetRadio(_curr_choice);

  return topWidget();
}


SLRadioBox::~SLRadioBox()

{
  if (_togary) {
        for(int i=0; (i<_arycnt); i++ )
                    if (_togary[i].name) free(_togary[i].name );
        free( _togary);
  }
}

void SLRadioBox::WhichButtonCallback(Widget w,
                                     XtPointer udata,
                                     XmToggleButtonCallbackStruct *CBdata) 

{
  SLRadioBox *obj = (SLRadioBox *)udata;

  obj->WhichButton(w, udata, CBdata); 
}

void SLRadioBox::WhichButton(Widget w, XtPointer,
                             XmToggleButtonCallbackStruct *CBdata) 

{
    long wconst;
    if (CBdata->set) {
          XtVaGetValues( w, XmNuserData, &wconst, NULL );
          _curr_choice= wconst;
          if (_target) *_target= wconst;
          ChoiceAction( wconst);
          if (_altChoiceAction) _altChoiceAction(_altChoiceData, wconst);
          callNotifyComplex((int)wconst);
    } // End if
}


void SLRadioBox::ChoiceAction(long) {}



void  SLRadioBox::SetRadio( long button) 
{

  Boolean found;
  int i;

  for(i= 0, found = False; ( (i<_arycnt) && (!found) ); i++) {
      if ( _togary[i].select_value == button) found= True;
  } // End loop
 
  if ( made() ) {
     if ( XtClass(  _togary[i-1].w) == xmToggleButtonWidgetClass ) {
          if (found) 
             XmToggleButtonSetState( _togary[i-1].w, True, True);
          else
             printf( 
              "SLRadioBox::SetRadio: radio button not in class -value: %d\n",
               button );
     } // End If
     else
        printf( "SLRadioBox::SetRadio: cannot set blank space -value: %d\n",
                  button );
  } // End if
  else {
     _curr_choice= (int)button;
     _value_set= True;
  }
}


Widget SLRadioBox::GetRadioWidget( long button) 
{

  Boolean found;
  Widget ret_w= NULL;
  int i;

  for(i= 0, found = False; ( (i<_arycnt) && (!found) ); i++) {
      if ( _togary[i].select_value == button) {
                ret_w= _togary[i].w;
                found= True;
      } // End if
  } // End loop
 
  return (ret_w);
}



int SLRadioBox::WhichSelected() 
{ 
 int answer= -1;
 if ((made())||(_value_set)) answer= _curr_choice;
 else {
   if ( canDefault() ) {
      for(int i=0; ((i<_arycnt) && (answer == -1)); i++) {
         if ( _pw_topw->childTogDef(_togary[i].name) ) 
                        answer= _togary[i].select_value;
      }
   }  
   else printf( "SLRadioBox::WhichSelected: unmade defaults not enabled.\n");
 }
 return (answer);
}





void SLRadioBox::preCreate(int &tnum)
{
    short cols;
    char lstr[30]; 
    Boolean dotitle;
    int rows;
    int idx;


    XtVaGetValues( topWidget(), XmNnumColumns, &cols, NULL);
    rows= _arycnt / cols; 
    
    if (tnum == 0) {
          dotitle= True;
          idx= 0;
    }
    else {
          dotitle= ((tnum % rows ) == 0);
          idx= tnum / rows;
    }

    if ( dotitle ) {
        sprintf( lstr, "T%d", idx );
        XtVaCreateManagedWidget( lstr, xmLabelWidgetClass, topWidget(), 
                                       XmNmarginLeft,  0,
                                       XmNmarginRight, 0, NULL);
    }
    tnum++;
}

void SLRadioBox::reloadDefaults(Boolean do_method)
{
  if ( made() ) {
     for(int i= 0; (i< _arycnt); i++) {
         DefLoadWValue(_togary[i].w);
         if ( XmToggleButtonGetState(_togary[i].w) == True ) {
                 _curr_choice= _togary[i].select_value;
                 if (_target) *_target= _curr_choice;
         } // End if
     } // End loop
     if (do_method) {
          ChoiceAction( _curr_choice);
          if (_altChoiceAction) _altChoiceAction(_altChoiceData, _curr_choice);
     }
  } // End if
  else {
     _curr_choice= WhichSelected();
     if (_target) *_target= _curr_choice;
     _value_set= False;
  }

}
