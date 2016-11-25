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
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include "wproc.h"
#include "sl/sl_tog_box.hh"
#include "sl/psuedo_widget.hh"


static String  defres[]= {
    "*XmToggleButton.alignment:   ALIGNMENT_BEGINNING",
    "*XmToggleButton.marginWidth: 0",
    "*XmToggleButton.marginHeight: 0",
    "*XmToggleButton.shadowThickness: 0",
    ".spacing:                    0",
    NULL };

SLTogBox::SLTogBox( Widget           p,
                    char             *name,
                    HelpCtx          hctx,
                    SLTogAry         togary,
                    unsigned int     arycnt,
                    Boolean          doframe,
                    Boolean          dotitle, 
                    Boolean          make_now )

        : SLDelay(p, name, hctx, doframe), _arycnt(arycnt),
          _altTogAction(NULL), _altTogData(NULL),
          _dotitle(dotitle)

{
 init( XtDisplay(p), togary);
 if (make_now) make(p);
 supportUnmadeDefaults(p);

 for (int i= 0; (i<_arycnt); i++) {
    if (_togary[i].target) *_togary[i].target= IsSelected(togary[i].ident);
 }

}



SLTogBox::SLTogBox( PsuedoWidget     *pw,
                    char             *name,
                    HelpCtx          hctx,
                    SLTogAry         togary,
                    unsigned int     arycnt,
                    Boolean          doframe,
                    Boolean          dotitle)

        : SLDelay(name, hctx, doframe), _arycnt(arycnt),
          _altTogAction(NULL), _altTogData(NULL),
          _dotitle(dotitle)

{
 supportUnmadeDefaults(pw);
 init( pw->display(), togary);

 for (int i= 0; (i<_arycnt); i++) {
    if (_togary[i].target) *_togary[i].target= IsSelected(togary[i].ident);
 }

}

SLTogBox::SLTogBox( SLDelay          *contain,
                    char             *name,
                    HelpCtx          hctx,
                    SLTogAry         togary,
                    unsigned int     arycnt,
                    Boolean          doframe,
                    Boolean          dotitle,
                    Boolean          make_if_can)

        : SLDelay(contain, name, hctx, doframe), _arycnt(arycnt),
          _altTogAction(NULL), _altTogData(NULL),
          _dotitle(dotitle)

{
 init((contain->pW())->display(), togary);
 supportUnmadeDefaults(contain);
 if ((contain->made())&&(make_if_can)) make(contain->topWidget());
 for (int i= 0; (i<_arycnt); i++) {
    if (_togary[i].target) *_togary[i].target= IsSelected(togary[i].ident);
 }

}



void SLTogBox::init(const Display *dpy, SLTogAry togary)
{
  setDefaultResources( dpy, _name, defres);

  // get target defaults

  _togary= (SLTogAryW)calloc( _arycnt, sizeof ( SLTogW) );
  for (int i= 0; (i<_arycnt); i++) {
     _togary[i].target= togary[i].target;
     _togary[i].ident= togary[i].ident;
     _togary[i].name= newstr( togary[i].name );
     _togary[i].value_ifset= SL_NOTSET;
  }

}



Widget SLTogBox::make(Widget p)
{
  int tnum= 0;
  Widget w;

  if ( made() ) return topWidget();
  SLDelay::make(p);

  p= wParent();

  w= XtVaCreateManagedWidget( _name, topClass(), makeFrameIfNeeded(p),
                              XmNadjustMargin,  False,
                              XmNisAligned,     False, NULL );

  XtVaSetValues( w, XmNisHomogeneous,  False, NULL);

  setTopWidget(w);
 

  for (int i= 0; (i<_arycnt); i++) {
      if (_dotitle) preCreate(tnum);
      _togary[i].w=      XtVaCreateManagedWidget( _togary[i].name,
                                              xmToggleButtonWidgetClass,
                                              w, XmNuserData, i, NULL );
      install_help(_togary[i].w );
      XtAddCallback( _togary[i].w, XmNvalueChangedCallback,
                     (XtCallbackProc)&DoToggleCallback, (XtPointer)this );
      if (_togary[i].value_ifset == SL_TRUE)
              XmToggleButtonSetState(_togary[i].w, True, False);
      else if (_togary[i].value_ifset == SL_FALSE)
              XmToggleButtonSetState(_togary[i].w, False, False);
      if ( _togary[i].target )
          *_togary[i].target= XmToggleButtonGetState( _togary[i].w);
                
  } // End Loop

  return topWidget();
}



SLTogBox::~SLTogBox()

{
  if (_togary) {
        for(int i=0; (i<_arycnt); i++ )
                    if (_togary[i].name) free(_togary[i].name );
        free( _togary);
  }

}

void SLTogBox::DoToggleCallback(Widget w,
                                XtPointer udata,
                                XmToggleButtonCallbackStruct *CBdata) 

{
  SLTogBox *obj = (SLTogBox *)udata;

  obj->DoToggle(w, udata, CBdata); 

}

void SLTogBox::DoToggle(Widget w, XtPointer,
                        XmToggleButtonCallbackStruct *CBdata) 

{
    long wconst;
    XtVaGetValues( w, XmNuserData, &wconst, NULL );
    if (_togary[wconst].target) *_togary[wconst].target= CBdata->set;
    TogAction( _togary[wconst].ident, CBdata->set );
    if (_altTogAction) 
        _altTogAction(_altTogData,  _togary[wconst].ident, CBdata->set);
    callNotifyComplex((int)_togary[wconst].ident);
}


void SLTogBox::TogAction(long , Boolean ) { }



void  SLTogBox::SetTog( long ident, Boolean state) 
{

  long i= retidx( ident);
  if (i>-1) {
      if (made()) {
          XmToggleButtonSetState( _togary[i].w, state, True );
      } // End if
      else {
          if (_togary[i].target) *_togary[i].target= (long)state;
          _togary[i].value_ifset= state ? SL_TRUE : SL_FALSE;
      }
  } // end if
  else
    fprintf(stderr,"SLTogBox::SetTog toggle not found: value : %d", ident);
}


Widget SLTogBox::TogW( long ident) 
{

  Widget ret_w= NULL;

  if (made()) {
     long i= retidx( ident);
     if (i>-1) ret_w=  _togary[i].w;
  } // End if
  else
     printf( "SLTogBox::TogW: Class must be made.\n");
 
  return (ret_w);

}


long  SLTogBox::retidx( long ident)
{
  Boolean found;
  long i=0;

  for(i= 0, found = False; ( (i<_arycnt) && (!found) ); i++) {
      if ( _togary[i].ident == ident) {
                found= True;
      } // End if
  } // End loop

  if (!found) i= 0;

  return (i-1);
}

Boolean  SLTogBox::IsSelected(long ident)
{
  Boolean retval= False;
  long i= retidx( ident);

  if ( made() ) {
     if (i>-1) retval= XmToggleButtonGetState( _togary[i].w);
  }
  else {
     PsuedoWidget *pw;
     pw= new PsuedoWidget( _pw_topw, _togary[i].name,
                                          xmToggleButtonWidgetClass);
     retval= pw->togDef();
     delete pw;
  }

  return (retval);
}

void SLTogBox::preCreate(int &tnum)
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


void SLTogBox::reloadDefaults(Boolean _do_method)
{
  Boolean val;
  if ( made() ) {
     for(int i= 0; (i< _arycnt); i++) {
         DefLoadWValue(_togary[i].w);
         if ( (val= XmToggleButtonGetState(_togary[i].w)) == True ) {
                 if (_togary[i].target) *_togary[i].target= True;
         }
         else {
                 if (_togary[i].target) *_togary[i].target= False;
         } // End else
         if (_do_method) {
              TogAction( _togary[i].ident, val);
              if (_altTogAction) 
                     _altTogAction(_altTogData, _togary[i].ident, val);
         }
     } // End loop
  } // End if
  else {
     for(int i= 0; (i< _arycnt); i++) {
          if (_togary[i].target) 
                 *_togary[i].target= _pw_topw->childTogDef(_togary[i].name);
          _togary[i].value_ifset= SL_NOTSET;
     } // End loop
  }
}
