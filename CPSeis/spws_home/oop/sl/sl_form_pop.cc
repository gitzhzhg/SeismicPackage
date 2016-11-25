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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
//#include <X11/Shell.h>
#include <Xm/XmP.h>
#include <Xm/Separator.h>
#include "sl/sl_form_pop.hh"
#include "sl/psuedo_widget.hh"
#include "sl/slp_push.hh"
#include "sl/sl_bb.hh"


#define PixelsPerInch(dsp, scrn) (int) (((float) DisplayWidth(dsp, scrn)) / \
                                 (0.0394 * DisplayWidthMM(dsp, scrn)))


static const String  defres[]= {
    "*button_control.shadowThickness:       0",
    "*button_control.rightOffset:           10",
    "*button_control.leftOffset:            10",
    "*button_control.bottomOffset:          15",
    "*button_control.OK.marginLeft:         10",
    "*button_control.OK.marginRight:        10",
    "*button_control.Help.marginLeft:       7",
    "*button_control.Help.marginRight:      7",
    NULL };

//  ".width:                              450",
//  ".height:                             325",


const char *BEGSTR= "overview_";


SLFormPop::SLFormPop(  const Widget        p, 
                       char                *name, 
                       const unsigned long buttons, 
                       const HelpCtx       hctx,
                       const Boolean       small_on_dpi,
                       const Boolean       make_now,
                       const Boolean       is_really_popup,
                       const int           num_colors,
                       const int           may_icon,
                       const int           screen_number) 
         : SLShellContainer(p, name, hctx, num_colors, 
                            may_icon, screen_number), 
           _buttons(buttons), 
           _completionFunc(NULL), _completionData(NULL), 
           _small_on_dpi(small_on_dpi),
           _overview_str(NULL), _default_button_ok(True), 
           _extra_push_ary(NULL), _num_extra_buttons(0),
           _total_buttons(0), _is_really_popup(is_really_popup),
           _button_ctl(NULL), 
           _ok(NULL), _can(NULL), _app(NULL), _help(NULL), _remove(NULL)
{
 supportUnmadeDefaults(p);
 if (is_really_popup) _button_ctl= new SLBB( this, "button_control", hctx);
 if (make_now) make(p);

}


SLFormPop::SLFormPop(  SLDelay            *contain,
                       char               *name, 
                       const unsigned long buttons, 
                       const HelpCtx       hctx,
                       const Boolean       small_on_dpi,
                       const Boolean       make_if_can,
                       const Boolean       is_really_popup,
                       const int           num_colors,
                       const int           may_icon,
                       const int           screen_number) 
         : SLShellContainer(contain, name, hctx, num_colors, 
                            may_icon, screen_number), 
           _buttons(buttons), 
           _completionFunc(NULL), _completionData(NULL), 
           _small_on_dpi(small_on_dpi),
           _overview_str(NULL), _default_button_ok(True), 
           _extra_push_ary(NULL), _num_extra_buttons(0),
           _total_buttons(0), 
           _is_really_popup(is_really_popup),
           _button_ctl(NULL), 
           _ok(NULL), _can(NULL), _app(NULL), _help(NULL), _remove(NULL)
{
 supportUnmadeDefaults(contain);
 if (is_really_popup) _button_ctl= new SLBB( this, "button_control", hctx);
 if ((contain->made())&&(make_if_can)) make(contain->topWidget());

}

SLFormPop::SLFormPop(  const PsuedoWidget  *pw,
                       char          *name, 
                       const unsigned long buttons, 
                       const HelpCtx       hctx,
                       const Boolean       small_on_dpi)
         : SLShellContainer(name, hctx), _buttons(buttons), 
           _completionFunc(NULL), _completionData(NULL), 
           _small_on_dpi(small_on_dpi),
           _overview_str(NULL), _default_button_ok(True), 
           _extra_push_ary(NULL), _num_extra_buttons(0),
           _total_buttons(0), _is_really_popup(True),
           _button_ctl(NULL), 
           _ok(NULL), _can(NULL), _app(NULL), _help(NULL), _remove(NULL)
{
 supportUnmadeDefaults(pw);
 _button_ctl= new SLBB( this, "button_control", hctx);
}



void SLFormPop::init( const Display*) {}

void SLFormPop::defaultButtonOK(Boolean set)
{
  defaultButton(FP_OK, set);
}


void SLFormPop::defaultButton(int ident, Boolean set)
{
  SLpPush *button;
  _default_button_ok= set;

  if (_button_ctl) {
     switch (ident) { 
         case FP_OK:     button= _ok;     break;
         case FP_APPLY:  button= _app;    break;
         case FP_CANCEL: button= _can;    break;
         case FP_HELP:   button= _help;   break;
         case FP_REMOVE: button= _remove; break;
         default:        assert(0); break;
     } // End switch ident

     if (made()) {
       if (set) {
         XtVaSetValues( _button_ctl->W(), XmNdefaultButton, button->W(), NULL);
         XtVaSetValues( topWidget(), XmNdefaultButton, button->W(), NULL);
       }
       else {
         XtVaSetValues( _button_ctl->W(), XmNdefaultButton, NULL, NULL);
         XtVaSetValues( topWidget(), XmNdefaultButton, NULL, NULL);
       }
     } // end if made
  } // end if _button_ctl
}

Widget SLFormPop::make(  Widget        p)
{
   Widget w;


   if (!made()) {
       setDefaultResources( pW()->display(), instanceName(), defres);
       SLShellContainer::make(p);
       p= wParent();

       if (_is_really_popup) 
                w= crePopup(p);
       else {   
                w= XtVaCreateManagedWidget(_name, xmFormWidgetClass,
                                           wParent(),  NULL);
                setTopWidget(w);
       }

       makeChildren();

       _overview_str= (char *)calloc( 1, 
                              strlen(XtName( w)) + strlen( BEGSTR) +1);
       sprintf( _overview_str, "%s%s", BEGSTR, XtName( w));



       if (_buttons & FP_DOOK) 
                  defaultButton(FP_OK, _default_button_ok);
       else if (_buttons & FP_DOREMOVE) 
                 defaultButton(FP_REMOVE, _default_button_ok);
   } // End if

   return topWidget();
}


Boolean SLFormPop::isDialog()
{ 
 if (_is_really_popup) return True;
 else                  return False;
}


SLFormPop::~SLFormPop()
{
  unattach(topWidget());
  if (_overview_str)   free(_overview_str);
  if (_extra_push_ary) free(_extra_push_ary);
}

void SLFormPop::new_and_copy()
{
  ExtraAry tmp_extra_ary;

  tmp_extra_ary= (ExtraAry)calloc( ++_num_extra_buttons, 
                                   sizeof (ExtraElement) );
  if (_extra_push_ary) {
       for (int i= 0; (i<(_num_extra_buttons-1)); i++) 
                   tmp_extra_ary[i]= _extra_push_ary[i];
       free(_extra_push_ary);
  } // End if
  _extra_push_ary= tmp_extra_ary;
}

void SLFormPop::addExtraButton(char *name, const int ident)
{
  if ((ident >= 0 ) && (ident <= 10)) {
        printf("SLFormPop::addExtraButton: Idents 0 thru 10 are reserved.\n");
        printf("SLFormPop::addExtraButton: Use ident greater than 10.\n");
        printf("SLFormPop::addExtraButton: name= %s, ident= %2d\n",
                                           name, ident);
        assert(0);
        return;
  }  // End if
  for (int i= 0; (i<_num_extra_buttons); i++) {
     if (ident == _extra_push_ary[i].obj->id()) {
        printf("SLFormPop::addExtraButton: Ident already in use.\n");
        printf("SLFormPop::addExtraButton: name= %s, ident= %2d\n",
                                           name, ident);
        assert(0);
        return;
     } // End if
  }   // End if
  new_and_copy();
  _extra_push_ary[_num_extra_buttons-1].func=   NULL;
  _extra_push_ary[_num_extra_buttons-1].data=   NULL;
  _extra_push_ary[_num_extra_buttons-1].obj= 
                           new SLpPush(_button_ctl, name, ident);
  _extra_push_ary[_num_extra_buttons-1].obj->setNotify(this);
  _total_buttons++;

}

Widget SLFormPop::buttonContainer() 
{ 
  return _button_ctl ? _button_ctl->W() : NULL;
}

void SLFormPop::managing()
{
 SLShellContainer::managing();
}



void SLFormPop::applyButton() 

{
   if ( ValidInput() ) {
       DoAction();
       if (_completionFunc) _completionFunc( _completionData, FP_APPLY);
   } // End if
}

void SLFormPop::okButton() 
{
   if ( ValidInput() ) {
          unmanage();
          XmUpdateDisplay(W());
          DoAction();
          if (_completionFunc) _completionFunc( _completionData, FP_OK);
   } // End If
}

void SLFormPop::removeButton() 
{
  unmanage();
  XmUpdateDisplay(W());
  if (_completionFunc) _completionFunc( _completionData, FP_REMOVE);
}


void SLFormPop::cancelButton() 
{
  UndoInput();
  unmanage();
  XmUpdateDisplay(W());
  if (_completionFunc) _completionFunc( _completionData, FP_CANCEL);
}

void SLFormPop::helpButton() 
{
  if (getHelpCtx()) overview_help( _overview_str, getHelpCtx() );
  if (_completionFunc) _completionFunc( _completionData, FP_HELP);
}

void SLFormPop::extraButton(int ident) 
{
  if (_completionFunc) _completionFunc( _completionData, ident);
}


Boolean SLFormPop::notify(SLPrim *obj)
{
  Boolean found= False;
  for(int i=0; ((i<_num_extra_buttons) && !found); i++)
          if (_extra_push_ary[i].obj == obj) found= True;
  if ( found         ||
      (obj == _ok)   ||  
      (obj == _can)  ||
      (obj == _app)  ||
      (obj == _help) ||
      (obj == _remove) ) {
     _last_button= (int)obj->id();
     switch (_last_button) { 
            case FP_OK:     okButton();                break;
            case FP_APPLY:  applyButton();             break;
            case FP_CANCEL: cancelButton();            break;
            case FP_HELP:   helpButton();              break;
            case FP_REMOVE: removeButton();            break;
            default:        extraButton(_last_button); break;
     } // end if
  } // end if
  return True;
}


/* ---------------------------------------------------------------- */

void SLFormPop::DoAction() {}

Boolean SLFormPop::ValidInput() { return True; }

void SLFormPop::UndoInput() {}

void SLFormPop::loadPopSysDefaults(String defres[], Boolean do_method)
{
    if (made())
          setResources( XtDisplay(W()), instanceName(), defres);
    else
          setResources( pW()->display(), instanceName(), defres);
    reloadDefaults(do_method);
}
/* ---------------------------------------------------------------- */



void SLFormPop::startResize( Widget w, XtPointer udata, XtPointer)
{
    SLFormPop *obj= (SLFormPop *)udata;
    obj->disButtons();
    if (XtIsShell(w))
        XtRemoveCallback( w, XmNpopupCallback, 
                      (XtCallbackProc)startResize, udata);
    else
        XtRemoveCallback( w, XmNmapCallback, 
                      (XtCallbackProc)startResize, udata);
}


void SLFormPop::cntrlResize( Widget, XtPointer udata,  XEvent *ev)
{
 if (ev->type == ConfigureNotify) {
    SLFormPop *obj= (SLFormPop *)udata;
    obj->disButtons();
 }
}


struct Bstruct { Widget    w;
                 Dimension width; };

void SLFormPop::disButtons()
{
  int i=0, j=0;
  Dimension qsize, start;
  Position nx;
  //Position ny;
  Dimension wid=0;
  Dimension total_but_width=0;
  Dimension wid_that_fit=0;
  int num_that_fit=0;
  const Dimension dist_btwn= 4;
  Bstruct *but = new Bstruct[_total_buttons];

  XtVaGetValues(_button_ctl->W(), XmNwidth, &wid, NULL);
  assert(wid);

  /*
   * get all the buttons in right order
   */
  if (_remove) but[i++].w= _remove->W();
  if (_ok)     but[i++].w= _ok->W();
  if (_app)    but[i++].w= _app->W();
  for (j=0; (j<_num_extra_buttons); but[i++].w= _extra_push_ary[j++].obj->W());
  if (_can)    but[i++].w= _can->W();
  if (_help)   but[i++].w= _help->W();

  assert(i == _total_buttons);

  /*
   * get all button widths and how many buttons fit
   */
  for (i=0; (i<_total_buttons); i++) {
      XtVaGetValues( but[i].w, XmNwidth, &but[i].width, NULL);
      total_but_width += but[i].width;
      if ( (total_but_width+ (i*dist_btwn)) < wid) {
                   num_that_fit++;
                   wid_that_fit += but[i].width; 
      }
  } // end loop
  /*
   * adjust how many buttons will fit
   */
  if (num_that_fit == 0) num_that_fit=1; // always allow for 1 button
  else if (num_that_fit > _total_buttons) num_that_fit= _total_buttons;

  /*
   * layout the buttons
   */
  qsize= (wid- wid_that_fit)/num_that_fit;
  for (i=0, start= 0; (i<num_that_fit); i++) {
      nx= (qsize / 2) + start;
      start+= but[i].width + qsize;
      XtManageChild(but[i].w);
      //XtVaGetValues( but[i].w, XmNy, &ny, NULL);
      //XtMoveWidget(but[i].w,nx, ny);
      XtVaSetValues( but[i].w, XmNx, nx, NULL);
  } // end loop
  /*
   * unmange any that don't fit
   */
  for (;(i<_total_buttons); i++) {
      XtUnmanageChild(but[i].w);
  }
  delete [] but;
}


static void reduceSize(Widget w)
{
   Dimension wid, height;
   int       min_wid, min_height;

   XtVaGetValues(w, XmNwidth,  &wid, XmNheight, &height, NULL);
   height= (Dimension)(height * .85);
   wid=    (Dimension)(wid * .8);
   XtVaSetValues(w, XmNwidth,   wid, XmNheight,  height, NULL);

   Widget parent= XtParent(w);
   XtVaGetValues(parent, XmNminWidth,  &min_wid,
                         XmNminHeight, &min_height, NULL);
   if ((min_wid<3000)&&(min_wid>100)) {
         min_wid= (int)(min_wid * .8);
         XtVaSetValues(parent, XmNminWidth, min_wid, NULL);
   }
   if ((min_height<3000)&&(min_height>100)) {
         min_height= (int)(min_height * .85);
         XtVaSetValues(parent, XmNminHeight, min_height, NULL);
   }
}

SLpPush *SLFormPop::addStandardButton( int          ident, 
                                       unsigned int flag, 
                                       char         *name)
{
  SLpPush *obj= NULL;
  if (_buttons & flag) {
      obj= new SLpPush(_button_ctl, name, (long)ident);
      obj->setNotify(this);
      _total_buttons++;
  }
  return obj;
}



Widget SLFormPop::crePopup(Widget p)
{
  Display  *dpy= XtDisplay(p);

  Widget frm= creDialog(p,topClass());

  long     dpi= PixelsPerInch( dpy, _screen_number);
  setTopWidget(frm);
  _button_ctl->make();

  XtVaSetValues( frm, XmNautoUnmanage, False, NULL);

  XtVaSetValues( _button_ctl->W(),
                        XmNmarginHeight,     0,
                        XmNmarginWidth,      0,
                        XmNtopAttachment,    XmATTACH_NONE,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment,  XmATTACH_FORM,
                        XmNleftAttachment,   XmATTACH_FORM, NULL );

  if ((_small_on_dpi)&&(dpi<80))  reduceSize(frm);

  _ok=     addStandardButton(FP_OK,     FP_DOOK,     "OK");
  _app=    addStandardButton(FP_APPLY,  FP_DOAPPLY,  "Apply");
  _can=    addStandardButton(FP_CANCEL, FP_DOCANCEL, "Cancel");
  _help=   addStandardButton(FP_HELP,   FP_DOHELP,   "Help");
  _remove= addStandardButton(FP_REMOVE, FP_DOREMOVE, "Remove");
  
  XtAddEventHandler( _button_ctl->W(),  StructureNotifyMask, False,
                     (XtEventHandler)cntrlResize, (XtPointer)this );
  XtAddCallback( frm, XmNmapCallback, (XtCallbackProc)startResize, 
                                      (XtPointer)this);
  XtAddCallback( XtParent(frm), XmNpopupCallback, (XtCallbackProc)startResize, 
                                      (XtPointer)this);

  return (frm);
}

Widget SLFormPop::getWidget(int ident)
{
  SLpPush *button= NULL;
  Widget retval=NULL;

  switch (ident) { 
         case FP_OK:     button= _ok;     break;
         case FP_APPLY:  button= _app;    break;
         case FP_CANCEL: button= _can;    break;
         case FP_HELP:   button= _help;   break;
         case FP_REMOVE: button= _remove; break;
  } // End if

  for(int i=0; ( (i<_num_extra_buttons)&&(!button) ); i++) {
       if (_extra_push_ary[i].obj->id() == ident) 
                       button= _extra_push_ary[i].obj;
  }
  if (button) {
        retval= button->W();
  }
  else {
        retval= NULL;
        //printf("SLFormPop::getWidget: button not found- ident: %d\n", ident);
  }

  return retval;
}



/* ---------------------------------------------------------------- */
/* ---------------------------------------------------------------- */
/* ---------------------------------------------------------------- */
SLFPopSep::SLFPopSep(  Widget        p, 
                       char          *name, 
                       unsigned long buttons, 
                       HelpCtx       hctx,
                       Boolean       small_on_dpi,
                       Boolean       make_now,
                       const Boolean is_really_popup,
                       const int     num_colors,
                       const int     may_icon,
                       const int     screen_number)
     : SLFormPop(p,name,buttons,hctx,small_on_dpi, False,
                 is_really_popup, num_colors, may_icon, screen_number) 
{
 if (make_now) make(p);
}


SLFPopSep::SLFPopSep(  SLDelay       *contain,
                       char          *name, 
                       unsigned long buttons, 
                       HelpCtx       hctx,
                       Boolean       small_on_dpi,
                       Boolean       make_if_can, 
                       const Boolean is_really_popup,
                       const int     num_colors, 
                       const int     may_icon,
                       const int     screen_number)
     : SLFormPop(contain,name,buttons,hctx,small_on_dpi, False,
                 is_really_popup, num_colors, may_icon, screen_number) 
{
 if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}

SLFPopSep::SLFPopSep(  PsuedoWidget  *pw,
                       char          *name, 
                       unsigned long buttons, 
                       HelpCtx       hctx,
                       Boolean       small_on_dpi) 
     : SLFormPop(pw,name,buttons,hctx,small_on_dpi)
{ }


Widget SLFPopSep::make(Widget p)
{  
   if ( !made() ) {
     SLFormPop::make(p);
     if (_is_really_popup)
          _lowsep= XtVaCreateManagedWidget( "lowsep", xmSeparatorWidgetClass, 
                                             topWidget(),
                             XmNrightAttachment,  XmATTACH_FORM,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNtopAttachment,    XmATTACH_NONE,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _button_ctl->W(),
                             XmNrightOffset,      5,
                             XmNleftOffset,       5,
                             XmNbottomOffset,     8,
                             NULL );
     else
          _lowsep= NULL;
   }

  return topWidget();
}

Widget SLFPopSep::bottomSeparator()
{ 
   if (isDialog()) return _lowsep;
   else            return topWidget();
}
