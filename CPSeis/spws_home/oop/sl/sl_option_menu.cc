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
#include <Xm/RowColumn.h>
#include "wproc.h"
#include "sl/psuedo_widget.hh"
#include "sl/sl_option_menu.hh"
#include "sl/sl_client_message.hh"


static String  defres[]= {
    ".spacing:                    0",
    ".marginWidth:                0",
    ".marginHeight:               0",
    NULL };


SLOptionMenu::SLOptionMenu( Widget             p,
                            char               *name,
                            HelpCtx            hctx,
                            SLPushAry          pushary,
                            const unsigned int arycnt,
                            long               *target,
                            Boolean            doframe,
                            Boolean            make_now ) :
       SLDelay(name,hctx,doframe),_arycnt(arycnt), _target(target),
       _altPushAction(NULL), _altPushData(NULL), 
       _def_value(NULL), _label_value(NULL)
{
 init( XtDisplay(p));
 supportUnmadeDefaults(p);
 if (pushary) {
    _curr_button= pushary[0].ident;
    getDefValueAndIdent(pushary);
    _buttons= new OptionPushBox( p, name, hctx, pushary, arycnt, this, False);
 } // end if
 else {
    pushary= makeDefaultsMenu();
    _curr_button= pushary[0].ident;
    getDefValueAndIdent(pushary);
    _buttons= new OptionPushBox( p, name, hctx, pushary, _arycnt, this, False);
    for(int i=0; (i<_arycnt); i++) free(pushary[i].name);
    free(pushary);
 }
 if (make_now) make(p);
 if (_target) *_target= whichSelected();
}


SLOptionMenu::SLOptionMenu( SLDelay            *contain,
                            char               *name,
                            HelpCtx            hctx,
                            SLPushAry          pushary,
                            const unsigned int arycnt,
                            long               *target,
                            Boolean            doframe,
                            Boolean            make_if_can ) :
       SLDelay(contain,name,hctx,doframe),_arycnt(arycnt), _target(target),
       _altPushAction(NULL), _altPushData(NULL),
       _def_value(NULL), _label_value(NULL)
{
 supportUnmadeDefaults(contain);
 init((contain->pW())->display());
 if (pushary) {
      _curr_button= pushary[0].ident;
      _buttons= new OptionPushBox( pW(), name, hctx, pushary, arycnt, this);
      if ((contain->made())&&(make_if_can)) make(contain->topWidget());
      getDefValueAndIdent(pushary);
 } // end if
 else {
      pushary= makeDefaultsMenu();
      _curr_button= pushary[0].ident;
      _buttons= new OptionPushBox( pW(), name, hctx, pushary, _arycnt, this);
      if ((contain->made())&&(make_if_can)) make(contain->topWidget());
      getDefValueAndIdent(pushary);
      for(int i=0; (i<_arycnt); i++) free(pushary[i].name);
      free(pushary);
 }
}


void SLOptionMenu::getDefValueAndIdent(SLPushAry pushary)
{
 Boolean found= False;
 if (_def_value) free(_def_value);
 _def_value= newstr(pW()->optionDef());
 _curr_button= pushary[0].ident;

 if (_def_value) {
    for (int i=0; (i<_arycnt); i++) {
        if (strcmp(_def_value,pushary[i].name) == 0) {
            found= True;
            _curr_button= pushary[i].ident;
        } // end if
    } // end loop
 } // end if
 else {
    _def_value= newstr(pushary[0].name);
 }

}

void SLOptionMenu::getDefValueAndIdent(SLPushAryW pushary)
{
 Boolean found= False;
 if (_def_value) free(_def_value);
 _def_value= newstr(pW()->optionDef());
 _curr_button= pushary[0].ident;

 if (_def_value) {
    for (int i=0; (i<_arycnt); i++) {
        if (strcmp(_def_value,pushary[i].name) == 0) {
            found= True;
            _curr_button= pushary[i].ident;
        } // end if
    } // end loop
 } // end if
 else {
    _def_value= newstr(pushary[0].name);
 }

}


SLPushAry SLOptionMenu::makeDefaultsMenu()
{
 SLPushAry pushary= NULL;
 char instance_str[80];
 char class_str[80];

 _arycnt= pW()->anyIntDef("optionCount", "OptionCount");
 if (_arycnt > 0) {
     pushary= (SLPushAry)calloc( sizeof(SLPush), _arycnt);
     for(int i=0; (i<_arycnt); i++) {
          pushary[i].ident= i;
          sprintf(instance_str, "optionElement_%1d", i);
          sprintf(class_str,    "OptionElement_%1d", i);
          if (_def_value) free(_def_value);
          _def_value= newstr(pW()->anyDef(instance_str, class_str));
          if (_def_value) {
               pushary[i].name= newstr(_def_value);
          }
          else {
               pushary[i].name= newstr("NONE");
          }
     } // end loop
 } // end if
 else {  // make defaults
     _arycnt= 1;
     pushary= (SLPushAry)calloc( sizeof(SLPush), _arycnt);
     pushary[0].ident= 0;
     pushary[0].name= newstr("NO DEFAULTS FOUND");
 } // end else
 return pushary;
}

void SLOptionMenu::addButton(char *name, int ident) 
{ 
  if (strcmp(_buttons->_pushary[0].name, "NO DEFAULTS FOUND") == 0) {
          delButton(0);
  }
  _buttons->addButton(name,ident);
  _arycnt= _buttons->_arycnt;
  getDefValueAndIdent(_buttons->_pushary);
}



SLOptionMenu::~SLOptionMenu()
{
  if (_label_value)  free(_label_value);
  if (_def_value)    free(_def_value);
  delete _buttons;
}


void SLOptionMenu::init( const Display *dpy)
{
  setDefaultResources( dpy, _name, defres);
}



Widget SLOptionMenu::make(Widget p)
{
  Widget w;
  Arg    arglist[22];

  if ( made() ) return topWidget();
  SLDelay::make(p);
  p= wParent();

  _buttons->setPushType(SLPushBox::PULLDOWN);
  _buttons->make(p);

  char *ns = "";

  XmString str= XmStringCreateSimple( _label_value ? _label_value : ns );
  int n=0;
  XtSetArg (arglist[n], XmNsubMenuId, _buttons->W() ); n++;
  XtSetArg (arglist[n], XmNlabelString,str ); n++;
//  w=  XmCreateOptionMenu( p, (char*)instanceName(), arglist, n);
  w=  createOptionMenu( p, (char*)instanceName(), _buttons->W(), str);
  setTopWidget(w);
  XtAddCallback(  _buttons->W(), XmNmapCallback, 
                  (XtCallbackProc)doMapCallback, (XtPointer)this);
  XtManageChild(topWidget());

  setButton(_curr_button);
  install_help();

  XmStringFree(str);
  return topWidget();
}


Widget SLOptionMenu::createOptionMenu(Widget    p, 
                                      char     *name, 
                                      Widget    poprc, 
                                      XmString  label)
{
  Widget w;
  w= XtVaCreateManagedWidget(name, xmRowColumnWidgetClass, p, 
                                      XmNsubMenuId,     poprc,
                                      XmNlabelString,   label, 
                                      XmNrowColumnType, XmMENU_OPTION, NULL);
  return w;
}


void  SLOptionMenu::doMapCallback(Widget w, XtPointer udata, XEvent*)
{
  /*
   * Send a client message to install the right color map
   * for this pulldown menu.  The MWM does not install private colormaps
   * correctly on pull down or option menus so we must do it ourselves.
   * However, it must be done after MWM install the wrong colormap.
   * That is why we do it in a client message.
   */
  SLOptionMenu *obj = (SLOptionMenu *)udata;
  SLClientMessage *cm= new SLClientMessage(w, "aname" );
  cm->setComplexNotify(obj);
}

Boolean SLOptionMenu::notifyComplex(SLDelay*, int )
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



int SLOptionMenu::whichSelected()
{
 return (int)_curr_button;
}

char *SLOptionMenu::whichSelectedString() 
{
 char *str= NULL;
 if (made()) {
       Widget w= buttonW(whichSelected());
       str= get_simp_labelstrptr(w);
 }
 else {
       str= newstr(_def_value);
 }
 return str;
}

Widget SLOptionMenu::buttonW(long button)
{
 return _buttons->pushW(button);
}

void OptionPushBox::pushAction( long ident)
{
 _opm->_curr_button= ident;
 _opm->callNotifyComplex((int)ident);
 _opm->optionAction((int)ident);
 if (_opm->_altPushAction) 
         _opm->_altPushAction(_opm->_altPushData, ident);

}

void SLOptionMenu::optionAction(int) { }

void SLOptionMenu::setButton(long ident)
{
  if (made()) {
     Widget w= _buttons->pushW(ident);
     XtVaSetValues( topWidget(), XmNmenuHistory, w, NULL);
     _curr_button= ident;
     callNotifyComplex((int)ident);
     optionAction((int)ident);
  } //  end if
  else {
     _curr_button= ident;
  }
}

void SLOptionMenu::delButton(int ident)
{
 _buttons->delButton(ident); 
 if (ident==_curr_button) {
     if (_buttons->_arycnt > 0) {
         _curr_button= _buttons->getAnyIdent();
         if (made())
              XtVaSetValues( topWidget(), 
                         XmNmenuHistory, _buttons->pushW(_curr_button), NULL);
     } // end if
     else 
         _curr_button= -9999;
 } 
}


void SLOptionMenu::setLabel(char *label)
{
 if (made()) {
     wprocShowMsg( XmOptionLabelGadget(topWidget()), label);
     if (_label_value) {
          free(_label_value);
          _label_value= NULL;
     }
 }
 else {
     if (_label_value) free(_label_value);
     _label_value= newstr(label);
 }
}

