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
#include <assert.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include "wproc.h"
#include "sl/psuedo_widget.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_shell_container.hh"
#include "sl/sl_pull_pop.hh"


static String  defres[]= {
    NULL };

SLPushBox::SLPushBox( const Widget           p,
                      char             *name,
                      const HelpCtx          hctx,
                      const SLPushAry        pushary,
                      const unsigned int     arycnt,
                      const Boolean          doframe,
                      const Boolean          dotitle,
                      const Boolean          make_now ) : 
       SLDelay(name,hctx,doframe),_arycnt(arycnt), _dotitle(dotitle),
       _altPushAction(NULL), _altPushData(NULL), _box_type(NORMAL)
{
 init( XtDisplay(p), pushary);
 if (make_now) make(p);
 else          supportUnmadeDefaults(p);
}

SLPushBox::SLPushBox( const PsuedoWidget     *pw,
                            char             *name,
                      const HelpCtx          hctx,
                      const SLPushAry        pushary,
                      const unsigned int     arycnt,
                      const Boolean          doframe,
                      const Boolean          dotitle ) :
       SLDelay(name,hctx,doframe),_arycnt(arycnt), _dotitle(dotitle),
       _altPushAction(NULL), _altPushData(NULL), _box_type(NORMAL)
{
 supportUnmadeDefaults(pw);
 init( pw->display(), pushary);
}

SLPushBox::SLPushBox(       SLDelay          *contain,
                            char             *name,
                      const HelpCtx          hctx,
                      const SLPushAry        pushary,
                      const unsigned int     arycnt,
                      const Boolean          doframe,
                      const Boolean          dotitle,
                      const Boolean          make_if_can ) :
       SLDelay(contain,name,hctx,doframe),_arycnt(arycnt), _dotitle(dotitle),
       _altPushAction(NULL), _altPushData(NULL), _box_type(NORMAL)
{
 init((contain->pW())->display(), pushary);
 supportUnmadeDefaults(contain);
 if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}



void SLPushBox::init(const Display *dpy, SLPushAry pushary)
{
  setDefaultResources( dpy, _name, defres);
  if (_arycnt>0) { 
        _pushary= (SLPushAryW)calloc( _arycnt, sizeof ( SLPushW) );
        for (int i= 0; (i<_arycnt); i++) {
            _pushary[i].ident= pushary[i].ident;
            _pushary[i].name= newstr( pushary[i].name );
      
        } // End Loop
  }
  else {
        _pushary= NULL;
  }
  setAltPushAction();
}


Widget SLPushBox::make(Widget p)
{
  int tnum= 0;
  Widget w;

  if ( made() ) return topWidget();
  SLDelay::make(p);
  p= wParent();

  switch (_box_type) {
      case NORMAL: 
                   w= XtVaCreateManagedWidget(_name, topClass(), 
                                                  makeFrameIfNeeded(p),
                                                  XmNadjustMargin,False,
                                                  XmNisAligned,   False, NULL);
                   XtVaSetValues( w, XmNisHomogeneous,  False, NULL);
                   break;
      case POPUP: 
                   w= XmCreatePopupMenu(p, _name, NULL, 0 );
                   XtAddEventHandler(p, ButtonPressMask, False, 
                                    (XtEventHandler)DoMBAction, (XtPointer)this); 
                   break;
      case PULLDOWN: 
                   w= XmCreatePulldownMenu(p, _name, NULL, 0 );
                   break;

  }
  setTopWidget(w);

  for (int i= 0; (i<_arycnt); i++) {
      if (_dotitle) {
        preCreate(tnum);
        if (_box_type == POPUP) 
           XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, 
                                   topWidget(), NULL);
      }

      _pushary[i].w=  XtVaCreateManagedWidget( _pushary[i].name,
                                               xmPushButtonWidgetClass,
                                               topWidget(), XmNuserData, i,
                                               NULL );
      install_help(_pushary[i].w );
      XtAddCallback( _pushary[i].w, XmNactivateCallback,
                     (XtCallbackProc)&DoPushCallback, (XtPointer)this );
  } // End Loop

 
  return topWidget();
}


void SLPushBox::addButton(char *name, int ident, SLShellContainer *contain)
{
 if (!checkidx(ident)) {
    assert(name);
    _pushary= (SLPushAryW)realloc( _pushary, sizeof ( SLPushW) * ++_arycnt );
    int i= _arycnt-1;
    _pushary[i].ident= ident;
    _pushary[i].name= newstr( name );
    _pushary[i].contain= contain;
    if (made())  {
         _pushary[i].w=  XtVaCreateManagedWidget( _pushary[i].name,
                                                  xmPushButtonWidgetClass,
                                                  topWidget(), 
                                                  XmNuserData, i, NULL);
         install_help(_pushary[i].w );
         XtAddCallback( _pushary[i].w, XmNactivateCallback,
                        (XtCallbackProc)&DoPushCallback, (XtPointer)this );
    } // end if
    else  {
         _pushary[i].w=  NULL;
    }
 }
 else {
    printf( "SLPushBox::addButton: Button with ident of %d already exist\n",
           ident);
 }

}

void SLPushBox::addButton(char *name, SLShellContainer *contain)
{
   addButton(name, SLPullPop::NOIDENT, contain);
}

void SLPushBox::delButton(int ident)
{
  SLPushAryW   tmpary;
  int i, j;
  
  if (checkidx(ident)) {
      long target= retidx(ident);
      tmpary= (SLPushAryW)calloc( _arycnt-1, sizeof ( SLPushW) );
      for(i=0, j=0; (i<_arycnt); i++) {
         if (i!=target) {
            memcpy( &tmpary[j],&_pushary[i], sizeof(SLPushW) );
            XtVaSetValues(tmpary[j].w, XmNuserData, j, NULL);
            j++;
         } // end if
      } // end loop
      _arycnt--;
      if (made()) XtDestroyWidget(_pushary[target].w);
      free(_pushary[target].name);
      free(_pushary);
      _pushary= tmpary;
  }
  else {
    printf( "SLPushBox::delButton: Button with ident of %d does not exist\n",
           ident);

  }

}

SLPushBox::~SLPushBox()

{
  if (_pushary) {
        for(int i=0; (i<_arycnt); i++ )
                    if (_pushary[i].name) free(_pushary[i].name );
        free( _pushary);
  }

}


void SLPushBox::DoPushCallback(Widget w,
                              XtPointer udata,
                              XmPushButtonCallbackStruct *CBdata) 
{
  SLPushBox *obj = (SLPushBox *)udata;

  obj->DoPush(w, udata, CBdata); 
}

void SLPushBox::DoPush(Widget w, XtPointer, XmPushButtonCallbackStruct*) 
{
    long i;
    SLPushAryW   button_ele;
    XtVaGetValues( w, XmNuserData, &i, NULL );
    
    button_ele= &_pushary[i];
    pushAction(_pushary[i].ident);
    /*
     * As we call the other functions we need to make sure that the 
     * _pushary was not changed in any way (by adds or deletes).  If it
     * was changed we will just bail out.
     */
    if (i<_arycnt) {
        if (_pushary[i].contain) _pushary[i].contain->makeAndManage();
        if (_altPushAction && button_ele == &_pushary[i]) {
              _altPushAction(_altPushData, _pushary[i].ident );
        } // end if
        if (i<_arycnt)
            if (button_ele == &_pushary[i])
                  callNotifyComplex((int)_pushary[i].ident);
    } // end if

}


void SLPushBox::pushAction(long) {}




Widget SLPushBox::pushW( long ident) 
{
  return ( _pushary[retidx(ident)].w );
}

Boolean SLPushBox::checkidx( long ident)
{
  Boolean found;
  long i;

  for(i= 0, found = False; ( (i<_arycnt) && (!found) ); i++) {
      if ( _pushary[i].ident == ident) {
                found= True;
      } // End if
  } // End loop

  if (found && (ident==SLPullPop::NOIDENT) )  
         found= False;

  return (found);
}

long  SLPushBox::retidx(long ident)
{
  Boolean found;
  long i;

  for(i= 0, found = False; ( (i<_arycnt) && (!found) ); i++) {
      if ( _pushary[i].ident == ident) {
                found= True;
      } // End if
  } // End loop

  if (!found) {
       i= 1;
       printf( "Invalid ident passed to SLPushBox value - %d\n", ident);
  } // End If

  return (i-1);
}

void SLPushBox::preCreate(int &tnum)
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



void SLPushBox::DoMBAction( Widget w,
                            XtPointer udata,
                            XEvent *event)
{
  SLPushBox *obj = (SLPushBox *)udata;
  obj->MBAction(w, udata, event); 
}

void SLPushBox::MBAction( Widget, XtPointer, XEvent *event)
{
   int button;
   XtVaGetValues(topWidget(),  XmNwhichButton, &button, NULL);
   if (event->xbutton.button == button) {
       XmMenuPosition(topWidget(), (XButtonEvent*)event);
       manage();
   }
}

Boolean SLPushBox::pushExist(int ident)
{
  Boolean found;
  long i;

  for(i= 0, found = False; ( (i<_arycnt) && (!found) ); i++) {
      if ( _pushary[i].ident == ident) {
                found= True;
      } // End if
  } // End loop
  return (found);
}
