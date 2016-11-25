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
#include <stdarg.h>
#include <sys/utsname.h>

#include <Xm/MainW.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/Protocols.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/Label.h>
#include <X11/IntrinsicP.h>

#include "cprim.h"
#include <assert.h>
#include "sl/sl_pull_pop.hh"
#include "sl/sl_app.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_radio.hh"
#include "sl/slp_push.hh"
#include "sl/sl_client_message.hh"

// $Id: sl_pull_pop.cc,v 1.3 2005/12/13 15:38:24 spws Exp $
// $Name: 12-12-2005 $


////////////////// new ///////////////////////
#include "sl/paintset_collection.hh"
////////////////// new ///////////////////////


enum { MENU_NONE, MENU_PULLDOWN, MENU_CASCADE, 
       MENU_CASCADE_ALONE, MENU_POPUP};

SLPullPop *SLPullPop::_last_obj= NULL;


/*
 * Constructor for putting pulldown menus or popup menus on a SLApp
 */
SLPullPop::SLPullPop(char *name, HelpCtx hctx, SLApp *app, Boolean ispull)
        : SLDelay (name, hctx), _default_func(NULL), 
          _default_data(NULL), _ppary(NULL), _arycnt(0), _radio_set(False),
          _toggle_added(False), _menu_type(MENU_NONE), _popup_title(NULL),
          _ident(NOIDENT), _slapp(app), _mapCallbackProc(0),
          _mapCallbackInstance(0)
{
    if (!hctx) setHelpCtx(app->getHelpCtx());
    if (ispull) 
          doPulldown(app->menuBar());
    else  
          doPopup(app->getWorkArea() );
 
    setComplexNotify(app);
    // SLPullPop make is never called when we go thru this constructor
}

/*
 * Constructor for putting cascade menus on a SLPullPop
 */
SLPullPop::SLPullPop(char *name, SLPullPop *pullpop, const int ident)
        : SLDelay (pullpop, name), _default_func(NULL), 
          _default_data(NULL), _ppary(NULL), _arycnt(0), _radio_set(False),
          _toggle_added(False), _menu_type(MENU_NONE), _popup_title(NULL),
          _ident(ident), _slapp(NULL), _mapCallbackProc(0),
          _mapCallbackInstance(0)
{
    _menu_type= MENU_CASCADE;
    if (pullpop->made()) make(pullpop->_rc);
}

/*
 * Constructor for popup menus on any Widget
 */
SLPullPop::SLPullPop(char *name, Widget w, HelpCtx hctx)
        : SLDelay (name, hctx), _default_func(NULL), 
          _default_data(NULL), _ppary(NULL), _arycnt(0), _radio_set(False),
          _toggle_added(False), _menu_type(MENU_POPUP), _popup_title(NULL),
          _ident(NOIDENT), _slapp(NULL), _mapCallbackProc(0),
          _mapCallbackInstance(0)
{
    assert(w);
    make(w);
}


/*
 * Constructor for cascade buttons or popups on any SL Class
 */
SLPullPop::SLPullPop(SLDelay *contain,
                     char    *name,
                     Boolean  is_cascade,
                     Boolean  make_if_can)
        : SLDelay (contain,name), _default_func(NULL), 
          _default_data(NULL), _ppary(NULL), _arycnt(0), _radio_set(False),
          _toggle_added(False), _popup_title(NULL), _ident(NOIDENT),
          _slapp(NULL), _mapCallbackProc(0), _mapCallbackInstance(0)
{
    assert(contain);
    supportUnmadeDefaults(contain);
    if (is_cascade)
          _menu_type= MENU_CASCADE_ALONE;
    else
          _menu_type= MENU_POPUP;
    if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}


void SLPullPop::doPulldown(Widget p)
{
  char *myname, *name= (char*)instanceName();
  Arg arglist[22];
  int n=0;

  n=0;

# if ( (XmVERSION*10 + XmREVISION) >= 12 )
        Boolean doit= True;


#   ifdef __ultrix 
          struct utsname osname;
          uname(&osname);
          if (osname.release[2] < '4') doit= False;
#   endif


          //We have found that Motif does not support tear off
          //pull downs on the secondary monitor of dual headed
          //machines (it crashes when the tear off is selected
          //by the user). Therefore I am disabling that option
          //until the Solaris version of Motif is fixed. MLS 02/2000
          if( DefaultScreenOfDisplay(XtDisplay(p)) != XtScreen(p) )
            doit = False;
          else
            doit= True;   


    if (doit) {
          XtSetArg( arglist[n], XmNtearOffModel, XmTEAR_OFF_ENABLED ); n++;
    }

# endif







/*******************************
////////////////// old ///////////////////////
   Colormap cmap;
   XtVaGetValues(p, XmNcolormap, &cmap, NULL);
   XtSetArg( arglist[n], XmNcolormap, cmap ); n++;
////////////////// old ///////////////////////
*******************************/

////////////////// new ///////////////////////
   PaintsetCollection::addResources (arglist, &n, p);
////////////////// new ///////////////////////

   _rc= XmCreatePulldownMenu( p, name, arglist, n);
   XtAddCallback( _rc, XmNmapCallback, (XtCallbackProc)doMapCallback,
                                      (XtPointer)this);
//  XtSetArg( arglist[n], XmNrowColumnType, XmMENU_PULLDOWN ); n++;
//  _rc= XtCreateManagedWidget( name, xmRowColumnWidgetClass, p, arglist, n);
  _cascade= XtVaCreateManagedWidget( name, xmCascadeButtonWidgetClass, p, 
                                     XmNsubMenuId, _rc, NULL);
  install_help(_cascade);

  myname= newstr(name);
  if (strcmp( strToLower(myname), "help")==0) {
         XtVaSetValues( p, XmNmenuHelpWidget, _cascade, NULL);
  }
  if (_slapp && !_slapp->menuHadBeenAdded()) {

/*************************
////////////////// old ///////////////////////
       XmCreatePulldownMenu( _rc, "dummy", NULL, 0);
////////////////// old ///////////////////////
*************************/

////////////////// new ///////////////////////
       XmCreatePulldownMenu( _rc, "dummy", arglist, n);
////////////////// new ///////////////////////

       _slapp->addedAPulldown();
  }
  free(myname);
  _menu_type= MENU_PULLDOWN;



  SLDelay::make(XtParent(_rc));
  setTopWidget(_cascade);
  setParentOfChildren(_rc);
}




void SLPullPop::doPopup(Widget work)
{
  char *name= (char*)instanceName();

  _cascade= NULL;
  if (work) {

/*************************
////////////////// old ///////////////////////
     _rc= XmCreatePopupMenu( work, name, NULL, 0);
////////////////// old ///////////////////////
*************************/

////////////////// new ///////////////////////
     Arg arglist[22];
     int n=0;
     PaintsetCollection::addResources (arglist, &n, work);
     _rc= XmCreatePopupMenu( work, name, arglist, n);
////////////////// new ///////////////////////

     _menu_type= MENU_POPUP;
  }
  else {
     puts( "SLPullPop: cannot create popup menu until SLApp has a work area");
     assert(work);
     _menu_type= MENU_NONE;
  } 
  SLDelay::make(XtParent(_rc));
  setTopWidget(_rc);
  XtAddCallback( _rc, XmNmapCallback, (XtCallbackProc)doMapCallback,
                                      (XtPointer)this);

  wprocTravWTree(work, oneWidget, (void*)this); 
}



SLPullPop::~SLPullPop()
{
  for (int i= 0; (i<(_arycnt)); i++) {
     if (_ppary[i].obj) delete _ppary[i].obj;
  }
  if (_ppary) free(_ppary);

}

Widget SLPullPop::make(Widget p) 
{
  if ( made() ) return topWidget();
  SLDelay::make(p);

  p= wParent();
  if (_menu_type == MENU_PULLDOWN)
     doPulldown(p);
  else if (_menu_type == MENU_CASCADE) {
     SLPullPop *pullpop= (SLPullPop*)slParent();
     doPulldown(p);
     pullpop->addNewElement(CASCADE);
     pullpop->_ppary[pullpop->_arycnt-1].obj= new SLpPush(_cascade, _ident);
     pullpop->checkUnique(pullpop->_arycnt-1);
     _menu_type= MENU_CASCADE;
  } // end if
  else if (_menu_type == MENU_CASCADE_ALONE) {
     Widget tmp= XtVaCreateManagedWidget( instanceName(), 
                                          xmRowColumnWidgetClass, p, 
                                          XmNrowColumnType,       XmMENU_BAR, 
                                          XmNmarginWidth,         0,
                                          XmNmarginHeight,        0,
                                          NULL);
     doPulldown(tmp);
     setTopWidget(tmp);
  }
  else
     doPopup(p);
  makeChildren();
  return topWidget();
}


/*
 * set the number of columns for the row column.
 */
void SLPullPop::setNumColumns(const int nc)
{
   XtVaSetValues(_rc, XmNpacking,     XmPACK_COLUMN,
                      XmNnumColumns,  nc, NULL);
}




void SLPullPop::new_and_copy()
                       
{
  SLPushPopAry newpp;

  newpp= (SLPushPopAry)calloc( ++_arycnt, sizeof (SLPushPop) );
  if (_ppary) {
       for (int i= 0; (i<(_arycnt-1)); i++) newpp[i]= _ppary[i];
       free(_ppary);
  }
  _ppary= newpp;
}

void SLPullPop::checkUnique(const int idx)
{
 int j, foundcnt;

 if (_ppary[idx].obj->id() == NOIDENT) {
    for(j= 0, foundcnt= 0; (j<_arycnt); j++) {
        if ( strcmp(_ppary[idx].obj->instanceName(),
                    _ppary[j].obj->instanceName()) == 0 ) foundcnt++;
    } // End loop
    if (foundcnt>1) 
     printf(
       "SLPullPop: Duplicate names for %s exist- this name must be unique.\n",
       (char*)_ppary[j].obj->instanceName() );
 }
 else {
    for(j= 0, foundcnt= 0; (j<_arycnt); j++) {
        if ( _ppary[idx].obj->id() == _ppary[j].obj->id()) foundcnt++;
    } // End loop
    if (foundcnt>1) 
     printf(
       "SLPullPop: Duplicate idents for %s exist- the idents must be unique.\n",
       (char*)_ppary[idx].obj->instanceName() );
 }
}


void SLPullPop::addNewElement(int etype)
{
 new_and_copy();
 int i= _arycnt-1;
 _ppary[i].element_type= etype;
 _ppary[i].func=   NULL; 
 _ppary[i].data=   NULL;
}


void SLPullPop::addPush(char *name, const int wconst)
{
 addNewElement(PUSH);
 _ppary[_arycnt-1].obj= new SLpPush(this, name, wconst);
 checkUnique(_arycnt-1);
}

void SLPullPop::addPush( char       *name, 
                         SLSelectionFunc func, 
                         void      *pdata,
                         const int wconst)
{
 addPush(name,wconst);
 _ppary[_arycnt-1].func= func;
 _ppary[_arycnt-1].data= pdata;
}

void SLPullPop::addPushUp( char *name, SLShellContainer *contain,
                           const int wconst)
{
 SLpPush *push;
 addNewElement(PUSHUP);
 _ppary[_arycnt-1].obj= push= new SLpPush(this, name, wconst);
 checkUnique(_arycnt-1);
 push->manageShellWhenPressed(contain);
 if (!contain)  {
      printf("SLPullPop::addPushUp: popup to manage must not be NULL\n");
      assert(contain);
 }
}

void SLPullPop::addPushUp( char *name, SLShellContainer *contain,
                     SLSelectionFunc func,
                     void *pdata, const int wconst)
{
 addPushUp(name,contain,wconst);
 _ppary[_arycnt-1].func= func;
 _ppary[_arycnt-1].data= pdata;
}


void SLPullPop::addRadio(char *name, const int wconst)
{
 if (_toggle_added) {
    puts( 
       "Radios may not be used in the same pull down menu as Toggle buttons.");
    assert(0);
 }
 if (!_radio_set) {
       XtVaSetValues( _rc, XmNradioBehavior, True, NULL);
       _radio_set= True;
 }
 addNewElement(RADIO);
 _ppary[_arycnt-1].obj= new SLpToggle(this, name, wconst);
 checkUnique(_arycnt-1);
}

void SLPullPop::addRadio( char *name, 
                          SLSelectionFunc func,
                          void *pdata, 
                          const int wconst)
{
 addRadio(name,wconst);
 _ppary[_arycnt-1].func= func;
 _ppary[_arycnt-1].data= pdata;
}



void SLPullPop::addTog(char *name, const int wconst)
{
 if (_radio_set) {
    puts( 
       "Toggles may not be used in the same pull down menu as radio buttons.");
    assert(0);
 }
 _toggle_added= True;
 addNewElement(TOG);
 _ppary[_arycnt-1].obj= new SLpToggle(this, name, wconst);
 checkUnique(_arycnt-1);
}

void SLPullPop::addTog( char *name, 
                        SLSelectionFunc func,
                        void *pdata, 
                        const int wconst)
{
 addTog(name,wconst);
 _ppary[_arycnt-1].func= func;
 _ppary[_arycnt-1].data= pdata;
}




Widget SLPullPop::addSep(char *name)
{
 return 
     XtVaCreateManagedWidget( name, xmSeparatorWidgetClass, _rc, NULL );
}

Boolean SLPullPop::notify(SLPrim *obj)
{
  int i=0;

  _last_obj= this;
  _last_name=  obj->instanceName();
  _last_ident= (int)obj->id();

  if (_last_ident==NOIDENT) i= retidx(obj->instanceName()); 
  else                      i= retidx(_last_ident); 

  
  /*
   * if this is a cascade button then don't do anything except return.
   */
  if (_ppary[i].element_type == CASCADE)  return True;


  /*
   *  We are always called twice in the radio situation.  We need to 
   *  filter out the button that is "unset".
   *  If the object is a radio button we only want to call the application
   *  when there is a True.  The button being set to false we do not
   *  want to pass on.  
   */
  if (_radio_set) {
        if ( (_ppary[i].element_type == RADIO) && (!obj->ivar()) )
           return True;
  }
  
  /*
   *  Call the proper function to notify of the action 
   */
  if (_ppary[i].func)
              _ppary[i].func( _ppary[i].data, obj->id() );
  else if (_default_func)
             _default_func( _default_data, obj->id() );
  else
             callNotifyComplex((int)obj->id());

  return True;
}


int  SLPullPop::retidx( const char* const name) const
{
  Boolean found;
  int i=0;

  for(i= 0, found = False; ( (i<_arycnt) && (!found) ); i++) {
      if ( strcmp(_ppary[i].obj->instanceName(), (char*)name) == 0) found= True;
  } // End loop

  if (!found) {
       i= 0;
       printf( "Invalid name passed to SLPullPop value - %s\n", (char*)name);
  } // End If


  return (i-1);
}



int  SLPullPop::retidx( const int ident) const
{
  Boolean found;
  int i=0;

  for(i= 0, found = False; ( (i<_arycnt) && (!found) ); i++) {
      if ( _ppary[i].obj->id() == ident) found= True;
  } // End loop

  if (!found) {
       i= 0;
       printf( "Invalid ident passed to SLPullPop value - %d\n", ident);
  } // End If


  return (i-1);
}



void SLPullPop::sensitive(Boolean set, int first_ident, ...)
{
  //if (!made()) return;
  va_list   args;
  va_start(args,  first_ident);

  for(int j=first_ident; (j>-1); j= va_arg(args, int) ) {
      _ppary[retidx(j)].obj->setSense(set);
  }
  va_end(args);
}



void SLPullPop::sensitive(Boolean set, char *first_name, ...)
{
  if (!made()) return;
  va_list   args;
  va_start(args,  first_name);

  for(char *str=first_name; (str); str= va_arg(args, char*) ) {
      _ppary[retidx(str)].obj->setSense(set);
  }
  va_end(args);
}


const Widget SLPullPop::getWidget(const int ident) const
{
  return _ppary[retidx(ident)].obj->W();
}

const Widget SLPullPop::getWidget(const char *name) const
{
  return _ppary[retidx(name)].obj->W();
}


Boolean SLPullPop::toggleValue(const int ident) const
{
  Boolean retval= False;
  int i= retidx(ident);
  if (_ppary[i].element_type ==  TOG) retval=  (Boolean)_ppary[i].obj->ivar();
  else {
    printf( "SLPullPop::toggleValue: ident of %d is not a toggle button.\n",
            ident);
  }
  return retval;
}

Boolean SLPullPop::toggleValue(const char* const name) const
{
  Boolean retval= False;
  int i= retidx(name);
  if (_ppary[i].element_type ==  TOG) retval=  (Boolean)_ppary[i].obj->ivar();
  else {
    printf( "SLPullPop::toggleValue: name- %s is not a toggle button.\n",
            name);
  }
  return retval;
}

void SLPullPop::setToggleValue(const int ident, const Boolean set)
{
  int i= retidx(ident);
  if (_ppary[i].element_type ==  TOG) _ppary[i].obj->setIvar(set);
  else {
    printf( "SLPullPop::setToggleValue: ident of %d is not a toggle button.\n",
            ident);
  }
}

void SLPullPop::setToggleValue(const char* const name, const Boolean set)
{
  int i= retidx(name);
  if (_ppary[i].element_type ==  TOG) _ppary[i].obj->setIvar(set);
  else {
    printf( "SLPullPop::setToggleValue: name- %s is not a toggle button.\n",
            name);
  }
}

void SLPullPop::setRadioValue(const int ident)
{
  int i= retidx(ident);
  /*
   * set the one toggle button to true and the others to false
   */
  if (_ppary[i].element_type ==  RADIO) {
           _ppary[i].obj->setIvar(True);
           for(int j=0; (j<_arycnt); j++)
                 if ((_ppary[j].element_type ==RADIO)&&(j!=i))
                          _ppary[j].obj->setIvar(False);
  } // end if RADIO
  else {
    printf( "SLPullPop::setRadioValue: ident of %d is not a radio button.\n",
            ident);
  }
}

void SLPullPop::setRadioValue(const char* const name)
{
  int i= retidx(name);
  /*
   * set the one toggle button to true and the others to false
   */
  if (_ppary[i].element_type ==  RADIO) {
           _ppary[i].obj->setIvar(True);
           for(int j=0; (j<_arycnt); j++)
                 if ((_ppary[j].element_type ==RADIO)&&(j!=i))
                          _ppary[j].obj->setIvar(False);
  } // end if RADIO
  else {
    printf( "SLPullPop::setRadioValue: name- %s is not a radio button.\n",
            name);
  }
}

int SLPullPop::radioValue() const
{
  int retval= -1;
  int i;
  Boolean found;
  if (_radio_set) {
       for(i= 0, found= False; ((i<_arycnt)&&(!found)); i++) {
            if (_ppary[i].element_type == RADIO) {
                   if ((Boolean)_ppary[i].obj->ivar()) {
                        retval= (int)_ppary[i].obj->id();
                        found= True;
                   }
             }
       } //end loop
       if (!found)
            printf( "SLPullPop::radioValue: No radio buttons set.\n");
  } //end if
  else {
    printf("SLPullPop::radioValue: No Radio buttons in this SLPullPop.\n");
  }
  return retval;
}


void SLPullPop::setTitle(char *title)
{
  if ( !_popup_title) {
        if (_arycnt == 0) {
             _popup_title= XtVaCreateManagedWidget( "title", 
                                                    xmLabelWidgetClass, 
                                                    _rc, NULL );
             wprocShowMsg(_popup_title, title);
             addSep();
        }
        else {
             printf("SLPullPop::setTitle: title must be set the first\n");
             printf("                     time before any button is added.\n");
             printf("                     Then is my be changed.\n");

        }
  }
  else {
        wprocShowMsg(_popup_title, title);
  }


}




SLPrim *SLPullPop::primObj(const int ident) const
{
  return _ppary[retidx(ident)].obj;
}


SLPrim *SLPullPop::primObj(const char *name) const
{
  return _ppary[retidx(name)].obj;
}



void SLPullPop::oneWidget(Widget w, void *data)
{
  SLPullPop *obj= (SLPullPop*)data;
  /*
   *  if the shell of the widget passed is the same as the 
   *  shell that the popup is intended to go on.
   */
  if (get_shell_widget(XtParent(get_shell_widget(obj->W()))) == 
                           get_shell_widget(w))
    XtAddEventHandler(w, ButtonPressMask, False, (XtEventHandler)DoMBAction,
                    (XtPointer)obj);
}

void SLPullPop::DoMBAction( Widget w,
                            XtPointer udata,
                            XEvent *event)
{
  SLPullPop *obj = (SLPullPop *)udata;
  obj->MBAction(w, udata, event);
}

void  SLPullPop::doMapCallback(Widget w, XtPointer udata, XEvent *event)
{
  /*
   * Send a client message to install the right color map
   * for this pulldown menu.  The MWM does not install private colormaps
   * correctly on pull down menus so we must do it ourselves.
   * However, it must be done after MWM install the wrong colormap.
   * That is why we do it in a client message.
   */
  SLPullPop *obj = (SLPullPop *)udata;
  SLClientMessage *cm= new SLClientMessage(w, "aname" );
  cm->setComplexNotify(obj);

  obj->handleMapCallback();
}

void  SLPullPop::handleMapCallback() {

    // called by static function doMapCallback

    // invoke mapCallback procedure if defined
    if (_mapCallbackProc && _mapCallbackInstance) {
        (*_mapCallbackProc)(_mapCallbackInstance);
    }
}

Boolean SLPullPop::notifyComplex(SLDelay*, int )
{
  /*
   * Install the correct private colormap.  MWM failed to do this
   * correctly so we must do it ourselves.
   */
  Colormap cmap;
  XtVaGetValues(topWidget(), XmNcolormap, &cmap, NULL);
  XInstallColormap(XtDisplay(topWidget()), cmap);
  return True;
}



void SLPullPop::MBAction( Widget, XtPointer, XEvent *event)
{
   unsigned int button;
   XtVaGetValues(_rc,  XmNwhichButton, &button, NULL);
   if (event->xbutton.button == button) {
       XmMenuPosition(_rc, (XButtonEvent*)event);
       manage();
   }
}


void SLPullPop::manageCascade()
{
    if (_cascade != NULL ) {
         XtManageChild ( _cascade );
    }
}

void SLPullPop::unmanageCascade()
{
    if (_cascade != NULL ) {
         XtUnmanageChild ( _cascade );
    }
}

void SLPullPop::mapCascade()
{
    if (_cascade != NULL ) {
         XtMapWidget ( _cascade );
    }
}

void SLPullPop::unmapCascade()
{
    if (_cascade != NULL ) {
         XtUnmapWidget ( _cascade );
    }
}

void SLPullPop::setMapCallbackProc(void (*f)(void*), void *instance) {

    // set the procedure to be called when the mapCallback occurs
    _mapCallbackProc = f;
    _mapCallbackInstance = instance;
}
