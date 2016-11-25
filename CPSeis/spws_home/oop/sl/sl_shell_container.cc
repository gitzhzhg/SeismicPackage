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
#include <X11/IntrinsicP.h>
#include "sl/sl_shell_container.hh"
#include "sl/sl_client_message.hh"
#include "sl/psuedo_widget.hh"

///////////////new///////////////////
#include "sl/paintset.hh"
#include "sl/paintset_collection.hh"
//#include "sl/color_handler.hh"
///////////////new///////////////////

#include "oprim/ll_charptr.hh"
#include "cprim.h"
#include <X11/cursorfont.h>
#include <Xm/DialogS.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <Xm/MwmUtil.h>



ContainerList SLShellContainer::_pop_list;
Wlist         SLShellContainer::_msg_list;
Wlist         SLShellContainer::_mode_list;
DisplayList   SLShellContainer::_dpy_list;


SLShellContainer::SLShellContainer(const Widget  p, 
                                   char         *name, 
                                   const HelpCtx hctx,
                                   const int     num_colors,
                                   const int     may_icon,
                                   const int     screen_number)
                    : SLDelay(p, name, hctx),
                      _num_test_colors(num_colors), 
                      _screen_number(screen_number),
                      _shell(NULL), _cmap(0),
                      _cmap_installed(True),
                      _may_icon(may_icon),
                      _screen_different(DONT_KNOW),
                      _current_activity_status(NoActivityStatus)
{ 
  _pop_list.add(this);
  assert( (may_icon==UseResource) || 
          (may_icon==MayIcon)     || 
          (may_icon==MayNotIcon));
  assert(screen_number >= UseResource);
}

SLShellContainer::SLShellContainer(char          *name, 
                                   const HelpCtx  hctx, 
                                   const int      may_icon)
                    : SLDelay(name, hctx),
                      _num_test_colors(0), 
                      _screen_number(UseParentScreen),
                      _may_icon(may_icon),
                      _shell(NULL), _cmap(0),
                      _cmap_installed(True),
                      _screen_different(NO_BOOL),
                      _current_activity_status(NoActivityStatus)
{ 
  _pop_list.add(this);
}

SLShellContainer::SLShellContainer(SLDelay       *contain, 
                                   char          *name, 
                                   const HelpCtx  hctx,
                                   const int      num_colors,
                                   const int      may_icon,
                                   const int      screen_number)
                    : SLDelay(contain, name, hctx),
                      _num_test_colors(num_colors), 
                      _screen_number(screen_number),
                      _shell(NULL), _cmap(0),
                      _cmap_installed(True),
                      _may_icon(may_icon),
                      _screen_different(DONT_KNOW),
                      _current_activity_status(NoActivityStatus)
{ 
  _pop_list.add(this);
  assert( (may_icon==UseResource) || 
          (may_icon==MayIcon)     || 
          (may_icon==MayNotIcon));
  assert(screen_number >= UseResource);
}


SLShellContainer::~SLShellContainer()  
{
  if (_shell) PaintsetCollection::releaseColors (_cmap, _num_test_colors);
  _pop_list.remove(this);
}

Widget SLShellContainer::make(Widget p)
{ 
  if (made()) return topWidget();
  if (_screen_different == DONT_KNOW) _screen_different= NO_BOOL;
  if (_may_icon == UseResource) setMayIcon();
  return SLDelay::make(p);
}


void SLShellContainer::iconifyAll()
{
  SLShellContainer *q;
  for(q= _pop_list.top(); (q != NULL); q= _pop_list.next() ) {
         Widget w= q->W();
         if ((q->made()) && (w)) {
              if (q->dialogShellType() != xmDialogShellWidgetClass) {
                     XIconifyWindow( XtDisplay(w), XtWindow(XtParent(w)),
                                     XScreenNumberOfScreen(XtScreen(w)) );
              } // end if
         } // end if
  } // end loop
}


void SLShellContainer::setClocks(Display *dpy, Cursor watch)
{
  SLShellContainer *q;
  for(q= _pop_list.top(); (q != NULL); q= _pop_list.next() ) {
         if ( (q->made()) && (q->W()) )
             if ((XtDisplay(q->W()) == dpy) && (XtWindow(q->W())))
                       define_cursor(q->W(),watch);
  }
}

void SLShellContainer::resetClocks(Display *dpy)
{
  SLShellContainer *q;
  for(q= _pop_list.top(); (q != NULL); q= _pop_list.next() ) {
         if ( (q->made()) && (q->W()) )
             if ((XtDisplay(q->W()) == dpy) && (XtWindow(q->W())))
                      define_cursor(q->W(),None);       
  }
}

SLShellContainer *SLShellContainer::anyContainer() 
{ 
  SLShellContainer *q, *p;
  Boolean found= False;
  
  p= q= _pop_list.current();

  if (!q->made()) {
     for(q= _pop_list.top(); ((q != NULL)&&(!found)); q= _pop_list.next() ) {
            if (q->made()) {
                  p=q;
                  found= True;
            }
     }
  }
  return p;
}

void SLShellContainer::closeAlert(Widget w)
{
   Atom wm_delete_window;
   Widget shell= get_shell_widget(w);

   wm_delete_window = XInternAtom(XtDisplay(w), "WM_DELETE_WINDOW", False);
   XmAddWMProtocols (shell, &wm_delete_window, 1);
   XmAddWMProtocolCallback( shell, wm_delete_window,
                            (XtCallbackProc)&windowClosingCallback,
                            (caddr_t)this );
}



Widget SLShellContainer::getMsgArea(Widget shell)
{
  Widget w;
  Boolean found= False;
  Widget retwidget= NULL;

  for(w= _msg_list.top(); ((w)&&(!found)); w= _msg_list.next()) {
         if (shell == get_shell_widget(w)) {
                    found= True;
                    retwidget= w;
         }
         else if (shell == get_toplevel_shell(w)) {
                    found= True;
                    retwidget= w;
         }
  }
  return retwidget;
}

char *SLShellContainer::setMsgWidget(Widget shell, char *str)
{
  Widget w;
  Boolean found= False;
  char *retstr= NULL;

  for(w= _msg_list.top(); ((w)&&(!found)); w= _msg_list.next()) {
         if (shell == get_toplevel_shell(w)) {
              found= True;
              retstr= wprocPushMsg(w, str);
         }
  }
  return retstr;

}

#define NRESETSHELLMSG \
 "SLShellContainer::resetMsgWidget: no message widget defined for this shell.\n"

void SLShellContainer::resetMsgWidget(Widget shell, char *str)
{
  Widget w;
  Boolean found= False;
  for(w= _msg_list.top(); ((w)&&(!found)); w= _msg_list.next()) {
         if (shell == get_toplevel_shell(w)) {
              found= True;
              if (str) wprocPopMsg(w, str);
         }
  }
}


void SLShellContainer::setAllMsgWidgets(CharPtrLinkedList *list, char *str)
{
  Widget w;
  char *retstr= NULL;

  for(w= _msg_list.top(); (w); w= _msg_list.next()) {
              retstr= wprocPushMsg(w, str);
              list->add(retstr);
  }

}

void SLShellContainer::resetAllMsgWidgets(CharPtrLinkedList *list)
{
  Widget w;
  char *str, *nstr= "";
  void *x;
  for(w= _msg_list.top(), str= list->top(&x); ((w)&&(str)); 
      w= _msg_list.next(), str= nstr ) {
              nstr= list->next(&x); 
              wprocPopMsg(w, str);
              list->remove(str);
  }
}


char *SLShellContainer::setModeWidget(Widget shell, char *str)
{
  Widget w;
  Boolean found= False;
  char *retstr= NULL;

  for(w= _mode_list.top(); ((w)&&(!found)); w= _mode_list.next()) {
         if (shell == get_toplevel_shell(w)) {
              found= True;
              retstr= wprocPushMsg(w, str);
         }
  }
  return retstr;
}

void SLShellContainer::resetModeWidget(Widget shell, char *str)
{
  Widget w;
  Boolean found= False;
  for(w= _mode_list.top(); ((w)&&(!found)); w= _mode_list.next()) {
         if (shell == get_toplevel_shell(w)) {
              found= True;
              if (str) wprocPopMsg(w, str);
              else     wprocShowMsg(w, "");
         }
  }
}

void SLShellContainer::refreshModeWidget(Widget shell, char *str)
{
  Widget w;
  Boolean found= False;
  for(w= _mode_list.top(); ((w)&&(!found)); w= _mode_list.next()) {
         if (shell == get_toplevel_shell(w)) {
              found= True;
              if (str) wprocShowMsg(w, str);
              else     wprocShowMsg(w, "");
         }
  }
}

void SLShellContainer::windowClosingCallback(Widget w,
                                             XtPointer udata,
                                             XtPointer CBdata)
{
  SLShellContainer *obj = (SLShellContainer *)udata;
  obj->windowClosing(w, udata, CBdata);
}



void SLShellContainer::windowClosing(Widget, XtPointer, XtPointer)
{
 closing();
}

void SLShellContainer::setTopWidget(Widget w) 
{
  if (w) {
     Display *dpy= XtDisplay(w);
     closeAlert(w); 
     if (!_dpy_list.find(dpy)) _dpy_list.add(dpy);
  }
  SLBase::setTopWidget(w);
} 



HelpCtx SLShellContainer::getShellHelpCtx(Widget shell)
{
  Boolean found= False;
  HelpCtx hctx = NULL;

  SLShellContainer *q;
  for(q= _pop_list.top(); ( (q != NULL) && (!found) && (!hctx) ); 
                                                  q= _pop_list.next() ) {
         if ( (q->made()) && (q->W()) ) {
             if (shell == get_toplevel_shell(q->W())) {
                  found= True;
                  hctx= q->getHelpCtx();
             } // end if
         } // end if
  } // end loop
  return hctx;
}


void SLShellContainer::setModal( ModalType mode)
{
  if (made()) {
    unsigned char modal_type;
    switch (mode) {
      case PrimAppModal: modal_type= XmDIALOG_PRIMARY_APPLICATION_MODAL; break;
      case FullAppModal: modal_type= XmDIALOG_FULL_APPLICATION_MODAL;    break;
      case Modeless:     modal_type= XmDIALOG_MODELESS;                  break;
      default:           assert(0); break;
    } // end switch
    XtVaSetValues(topWidget(), XmNdialogStyle, modal_type, NULL);
  }
  else
      printf("SLShellContainer::setModal: object %s is not made.\n",
              instanceName() );
}


int SLShellContainer::computeScreen(Widget p)
{
  int parent_scrno;
  assert(pW());
  const Display *dpy= pW()->display();
  if (p) parent_scrno= XScreenNumberOfScreen(XtScreen(p));
  else   parent_scrno= XScreenNumberOfScreen(XtScreen(pW()->anyW()));
  int total_screens= ScreenCount(dpy);

  if (_screen_number == UseParentScreen)  {
        _screen_number= parent_scrno;
  } // end if UseParentScreen
  else if (_screen_number == UseResource) {
        _screen_number= pW()->formScreenDef();
        if (_screen_number == PsuedoWidget::OtherScreen) {
            if (total_screens > 1) {
                  Boolean found= False;
                  for(int i=0; ((i<total_screens)&&(!found)); i++)  {
                          if (parent_scrno != i) {
                                _screen_number=i;
                                found= True;
                          } // end if
                  } // end loop
            } // end if
            else
                  _screen_number= parent_scrno;
        } // end if
        else if (  (_screen_number >= total_screens)
                 ||(_screen_number == PsuedoWidget::NoScreenResource) )
                    _screen_number= parent_scrno;
  } // end if UseResource
  else {
    if (_screen_number >= total_screens)  {
        _screen_number= parent_scrno;
    }
  } // end if- use the passed screen number
  return _screen_number;
}






Widget SLShellContainer::creDialog(const Widget p, const WidgetClass wclass)
{
  Arg         args[22];
  int         n=0;
  Display     *dpy= XtDisplay(p);
  Screen      *scr;

  _cmap_installed= False;


  setMayIcon();
  computeScreen(p);


  scr= ScreenOfDisplay(dpy, _screen_number);
  _screen_different=  (XtScreen(p) != scr) ? YES_BOOL : NO_BOOL;

  /*
   *  If this is on another screen then start with the default colormap,
   *  If we are on the same screen then use color map of the parent
   */

/**********************
///////////////old///////////////////
  if (_screen_different == YES_BOOL) {
          _cmap= DefaultColormapOfScreen(scr);
  } // end if
  else {
          XtVaGetValues(p, XmNcolormap, &_cmap, NULL);
  } // end else

  if (_num_test_colors >0) {
      if (!canIAlloc(_num_test_colors,0,p)) {
                 printf("am making private\n");
                 printf("am making private\n");
                 printf("am making private\n");
                 printf("am making private\n");
                  _cmap=  newcmap_andcpy_onscreen(dpy, scr, 10);
      } // end if
  } // end if _num_test_colors
///////////////old///////////////////
**********************/

///////////////new///////////////////
  Paintset *paintset;
  if (_screen_different == YES_BOOL) {
    paintset = PaintsetCollection::fetchByNumColors (scr,
      _num_test_colors);
  }
  else {
    paintset = PaintsetCollection::fetchByNumColors (p,
      _num_test_colors);
  }
  _cmap = paintset->colormap ();
  assert(_cmap);
///////////////new///////////////////

  /*
   * 1. the colormap map must be set if we are using a new color map
   *    or going to a new screen- because of this we just set the colormap
   *    anyway.
   * 2. the depth must be set if we are going to a new screen.
   * 3. screen must be set if we are going to a new screen.
   */

  XtSetArg (args[n], XmNallowShellResize, True); n++;

/**********************
///////////////old///////////////////
  XtSetArg(args[n],XmNcolormap, _cmap ); n++;
  if (_screen_different == YES_BOOL) {
          XtSetArg(args[n], XmNdepth, DefaultDepthOfScreen(scr) ); n++;
          XtSetArg(args[n], XmNscreen, scr); n++;
  } // end if _screen_different == YES_BOOL
///////////////old///////////////////
**********************/

////////////////new//////////////////
  paintset->addResources(args, &n);
////////////////new//////////////////

  char *shell_name= newstrcat(_name, "_popup", NULL);
  _shell= XtCreatePopupShell(shell_name, dialogShellType(), p, args, n);
  free(shell_name);

  Widget w= XtVaCreateWidget(_name, wclass, _shell,
                                    XmNresizePolicy, XmRESIZE_NONE, NULL);

  if (_cmap == DefaultColormapOfScreen(scr) ) _cmap_installed= True; 
  
  return w;
}

static Window getRootWindowChild(Display *dpy, Screen *scr, Window w) {

  Window root;
  Window parent= w;
  Window *children;
  unsigned int nchildren;

  for(; parent != RootWindowOfScreen(scr); ) { 
    w= parent;
    XQueryTree( dpy, w, &root, &parent, &children, &nchildren);
    XFree(children);
  }
  return w;
}


void SLShellContainer::addDialogRaiser() {


   if (made()) {
        Widget w= get_shell_widget(topWidget()); 
        if (XInternAtom(XtDisplay(w), "_DT_SM_PREFERENCES", True) != None) {
             XtAddEventHandler(w,
                          VisibilityChangeMask, False,
                          (XtEventHandler)raiseDialogEvent, (XtPointer)this );
             XtAddEventHandler(get_shell_widget(XtParent(w)),  
                          VisibilityChangeMask, False,
                          (XtEventHandler)raiseDialogEvent, (XtPointer)this );
        } // end if
   }
}

static void doClientMessage(void *udata) {
   SLShellContainer *obj= (SLShellContainer*)udata;
   obj->raiseDialog(get_shell_widget(obj->W()));
}


void SLShellContainer::raiseDialogEvent(Widget, XtPointer udata,XEvent*) {

   SLShellContainer *obj= (SLShellContainer*)udata;
   if (obj->made()) {
         new SLClientMessage(get_shell_widget(obj->W()), "dummy", 
                             doClientMessage, udata, NULL, 1);
   }
}

/*
 *void SLShellContainer::raiseDialog(Widget shell) {
 *      Window root;
 *      Window parent;
 *      Window *children;
 *      unsigned int nchildren;
 *      Window *wary;
 *      int i,j;

 *      int     parent_idx=-1, child_idx= -1;
 *      Widget pshell= get_shell_widget( XtParent(shell) );
 *      Display *dpy= XtDisplay(shell);
 *      Screen  *scr= XtScreen(shell);
 *      Window shellw=  getRootWindowChild( dpy, scr, XtWindow(shell) );
 *      Window pshellw= getRootWindowChild( dpy, scr, XtWindow(pshell) );
 *
 *      XQueryTree( dpy, RootWindowOfScreen(scr), &root, &parent, 
 *                  &children, &nchildren);
 *      wary= (Window*)malloc( nchildren * sizeof(Window) );
 *
 *      for(i=0,j=nchildren-1; (i<nchildren); wary[i++]= children[j--]);
//        for(i=0; (i<nchildren); i++) printf("children[%1d]= %x\n",
//                                             i, children[i]);
//        for(i=0; (i<nchildren); i++) printf("wary[%1d]= %x\n", i, wary[i]);
 *
 *
 *
//        for(i= 0; ((i<nchildren)&&((parent_idx<0)||(child_idx<0))); i++) {
//            if (wary[i] == shellw)  child_idx= i;
//            if (wary[i] == pshellw) parent_idx= i;
//        }
//        if (child_idx > parent_idx) {
//               wary[child_idx]=  pshellw;
//               wary[parent_idx]= shellw;
//        }
//
 *
 *       XRestackWindows(dpy, wary, nchildren);
 *      XFree(children);
 *      free(wary);
 *}
 */

/**/
  void SLShellContainer::raiseDialog(Widget shell) {
        if (XtIsManaged(W())) {
            XWindowChanges changes;
            Window root;
            Window parent;
            Window *children;
            int     parent_idx=-1, child_idx= -1;
            unsigned int nchildren;
            Widget pshell= get_shell_widget( XtParent(shell) );
            Display *dpy= XtDisplay(shell);
            Screen  *scr= XtScreen(shell);
            Window shellw=  getRootWindowChild( dpy, scr, XtWindow(shell) );
            Window pshellw= getRootWindowChild( dpy, scr, XtWindow(pshell) );
            changes.sibling= pshellw;
            changes.stack_mode= Above;
            XQueryTree( dpy, RootWindowOfScreen(scr), &root, &parent, 
                       &children, &nchildren);
            for(int i= 0; ((i<nchildren)&&((parent_idx<0)||(child_idx<0))); i++) {
                  if (children[i] == shellw)  child_idx= i;
                  if (children[i] == pshellw) parent_idx= i;
            }
            if (child_idx < parent_idx) 
                  XReconfigureWMWindow(dpy, shellw, XScreenNumberOfScreen(scr), 
                                       CWSibling|CWStackMode, &changes);
        }
  }
/**/ 

/*
 *void SLShellContainer::raiseDialog(Widget shell) {
 *
 * Window wary[2];
 * Display *dpy= XtDisplay(shell);
 * Screen  *scr= XtScreen(shell);
 * Widget pshell= get_shell_widget( XtParent(shell) );
 * wary[0]= getRootWindowChild( dpy, scr, XtWindow(shell) );
 * wary[1]= getRootWindowChild( dpy, scr, XtWindow(pshell) );
 * //XRaiseWindow(dpy, wary[0]);
 * XSync(dpy,False);
 * XRestackWindows(dpy, wary, 2);
 *}
 */

Boolean SLShellContainer::isPrivateCmap()
{
  if(!_cmap) _cmap = PaintsetCollection::colormap(topWidget());
  assert(_cmap);
  Screen *scr      = XtScreen(topWidget());
  Colormap def_cmap= DefaultColormapOfScreen(scr);
  return (def_cmap != _cmap);
}

Boolean SLShellContainer::canIAlloc(int number, int num_planes, Widget w)
{
/*
  if(!_cmap) _cmap = PaintsetCollection::colormap(topWidget());
 assert(_cmap);
 long stat;
 Display *dpy= XtDisplay(w);
 unsigned long pix[500];
 unsigned long pmsk[24];
 stat= XAllocColorCells( dpy, _cmap, True, pmsk, num_planes, pix,  number);
 if (stat) XFreeColors(dpy, _cmap, pix, number, num_planes);
 XFlush(dpy);
 return (Boolean)stat;
*/
// find out if the given widget can allocate the necessary colors
  Paintset *paintset = PaintsetCollection::fetch (topWidget());
  return PaintsetCollection::numColorsLeft(paintset,num_planes) >= number;
}

void SLShellContainer::managing()
{
  if(!_cmap) _cmap = PaintsetCollection::colormap(topWidget());
  assert(_cmap);
  if (!_cmap_installed) {
      _cmap_installed= True; 
      XSetWindowColormap (XtDisplay(topWidget()), 
                          XtWindow(topWidget()), _cmap);
      XSetWindowColormap (XtDisplay(topWidget()), 
                          XtWindow(_shell), _cmap);
      //XWindowChanges changes;
      //Widget shell = get_shell_widget( XtParent(topWidget() ) );
      //Widget pshell= get_shell_widget( XtParent(shell) );
      //Display *dpy= XtDisplay(shell);
      //Screen  *scr= XtScreen(shell);
      //Window shellw=  getRootWindowChild( dpy, scr, XtWindow(shell) );
      //Window pshellw= getRootWindowChild( dpy, scr, XtWindow(pshell) );
      //changes.sibling= pshellw;
      //changes.stack_mode= Above;
      //XConfigureWindow(dpy, shellw, CWSibling|CWStackMode, &changes);
  } // End if
}

WidgetClass SLShellContainer::dialogShellType()
{
  /*
   * topLevelShells can be iconed.
   */
  WidgetClass stype;
  if (_screen_different == DONT_KNOW) {
      printf("%s\n%s\n%s\n%s\n"
       "SLShellContainer::dialogShellType: it is too early to call this",
       "                  method, dialogShellType is not yet determined.",
       "                  You may call this method after make or creDialog",
       "                  is called." );
  } // end if

  if ( (_may_icon==MayIcon) || (_screen_different == YES_BOOL) )
        stype=  topLevelShellWidgetClass;
  else
        stype=  xmDialogShellWidgetClass;
  return stype;
}

void SLShellContainer::setScreenNumber(int scrno)
{
  if (!_shell) {
      _screen_number= scrno;
  }
  else {
     printf(
 "SLShellContainer::setScreenNumber: must be set before widget creation\n");
  }
}

void SLShellContainer::setMayIcon()
{
  if (!_shell) {
      if (_may_icon == UseResource) {
          if (pW()) {
              if (pW()->anyBooleanDef("mayIcon", "MayIcon"))
                      _may_icon= MayIcon;
              else
                      _may_icon= MayNotIcon;
          } // end if
          else  {
              _may_icon= MayNotIcon;
          } // end else
      } // end if
  } // end if !_shell
  else {
     printf(
      "SLShellContainer::setMayIcon: must be set before widget creation\n");
  }
}
void SLShellContainer::setNumberTestColors(int num)
{
  if (!_shell) {
      _num_test_colors= num;
  }
  else {
     printf(
"SLShellContainer::setNumberTestColors: must be set before widget creation\n");
  }
}

int SLShellContainer::numberTestColors ()
{
   return _num_test_colors;
}


void SLShellContainer::activityNotify(ActivityStatus status)
{
  _current_activity_status= status;
  callNotifyComplex();
  _current_activity_status= NoActivityStatus;
}

SLShellContainer::ActivityStatus SLShellContainer::getActivityStatus()
{
  return _current_activity_status;
}

void SLShellContainer::setTitle(char* title, Boolean set_to_icon)
{
  if (made()) {
     XtVaSetValues( get_shell_widget(W()), XtNtitle, title, NULL);
     if (set_to_icon && _may_icon==MayIcon) setIcon(title);
  }
  else {
     printf("SLShellContainer::setTitle: Object must be made first.\n");
  }
}

void SLShellContainer::setIcon(char* title, Pixmap icon_pix, Pixmap icon_mask)
{
  if (made()) {
     if (_may_icon==MayIcon) {
         Widget shell= get_shell_widget(W());
         XtVaSetValues( shell, XtNiconName, title, NULL);
         if ((icon_pix) && (icon_mask))
                  XtVaSetValues( shell, XtNiconPixmap, icon_pix,
                                        XtNiconMask,   icon_mask, NULL );
     } // end if
     else {
     printf("SLShellContainer::setIcon: Object is not iconable.\n");

     } // end if 
  } // end if
  else {
     printf("SLShellContainer::setIcon: Object must be made first.\n");
  }
}

