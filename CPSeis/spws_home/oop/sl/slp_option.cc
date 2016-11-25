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

//---------------------- slp_option.cc -----------------------------//
//---------------------- slp_option.cc -----------------------------//
//---------------------- slp_option.cc -----------------------------//

//         implementation file for the SLpOption class
//                      subdirectory sl


/*
//  _w       is the pulldown menu (parent of pushbuttons in the menu).
//  _woption is the option menu (gets hctx, manage, sense, label).
//  _woption is a child of _wparent.
//  _w       is a child of _wparent.
//  _w       is topWidget().
*/

//      NOW:
//  _w       is the pulldown menu (parent of pushbuttons in the menu).
//  _woption is the option menu (gets hctx, manage, sense, label).
//  _woption is a child of _wparent.
//  _w       is a child of _woption.
//  _w       is parentOfChildren().
//  _woption is topWidget().


#include "sl/slp_option.hh"
#include "sl/slp_push.hh"
#include "sl/sl_client_message.hh"
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
 


//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//
//-----------------constructor----------------------------------------//


SLpOption::SLpOption (SLDelay *slparent, char *name, long ident, char *label)
    : SLpBase(slparent, name, xmRowColumnWidgetClass, ident, _LONG),
        _woption     (NULL)
{
  createCvarResource();
  createIvarResource();
  createSenseResource();
  setupCvarValue(label);
  if(slparent->topWidget()) make();
}


SLpOption::SLpOption (Widget wparent, char *name, long ident, char *label)
    : SLpBase(wparent, name, xmRowColumnWidgetClass, ident, _LONG),
        _woption     (NULL)
{
  createCvarResource();
  createIvarResource();
  createSenseResource();
  setupCvarValue(label);
  make();
}



//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SLpOption::~SLpOption(void)
{
}


//-------------------- find option by ident ------------------//
//-------------------- find option by ident ------------------//
//-------------------- find option by ident ------------------//

SLpPush *SLpOption::findOptionByIdent(long ident)
{
  for( SLDelay *gui = topChild(); gui; gui = nextChild() )
       {
       SLpPush *opt = (SLpPush*)gui;
       if(opt->id() == ident) return opt;
       }
  return NULL;
}


//------------------- notify -------------------------------//
//------------------- notify -------------------------------//
//------------------- notify -------------------------------//

Boolean SLpOption::notify(SLPrim *gui)
{
  if(gui) callItrap(gui->id());
  return TRUE;
}



//-------------- add and manipulate children ----------------//
//-------------- add and manipulate children ----------------//
//-------------- add and manipulate children ----------------//

SLpPush *SLpOption::addOption(char *name, long ident, char *label)
{
  SLpPush *gui = new SLpPush(this, name, ident, label);
  return gui;
}


void SLpOption::removeOption(long ident)
{
  SLpPush *gui = findOptionByIdent(ident);
  if(gui) delete gui;
}


//--------------- set and setup cvar (by ident) -------------//

void SLpOption::setOptionLabel(long ident, char* value)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->setCvar(value);
}


void SLpOption::setupOptionLabelValue(long ident, char* value)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->setupCvarValue(value);
}


void SLpOption::setupOptionLabelPoint(long ident, char *point, long nvar)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->setupCvarPoint(point, nvar);
}


void SLpOption::setupOptionLabelFun(long ident, char* (*fun)(void*), void *data)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->setupCvarFun(fun, data);
}


//--------------- set and setup sense (by ident) ------------//

void SLpOption::setOptionSense(long ident, long value)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->setSense(value);
}


void SLpOption::setupOptionSenseValue(long ident, long value)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->setupSenseValue(value);
}


void SLpOption::setupOptionSensePoint(long ident, long *point)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->setupSensePoint(point);
}


void SLpOption::setupOptionSenseFun(long ident, long (*fun)(void*), void *data)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->setupSenseFun(fun, data);
}


//---------- manage and unmanage (by ident) -----------------//

void SLpOption::manageOption(long ident)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->manage();
}


void SLpOption::unmanageOption(long ident)
{
  SLpPush *opt = findOptionByIdent(ident);
  if(opt) opt->unmanage();
}



//---------------------- other functions --------------------//
//---------------------- other functions --------------------//
//---------------------- other functions --------------------//

//   manage() and unmanage() and setSenseResource()
//   are no longer needed
//   since _woption is now topWidget().

//   _woption can be replaced by topWidget() everywhere here,
//   and _woption is no longer needed as a member variable.


void SLpOption::manage(void)
{
  if(_woption) XtManageChild(_woption);
}


void SLpOption::unmanage(void)
{
  if(_woption) XtUnmanageChild(_woption);
}
 


void SLpOption::setCvarResource(void)
{
  if(!_woption) return;
  Widget wlabel = XmOptionLabelGadget(_woption);
  setCompoundStringResource(XmNlabelString, wlabel);
}


void SLpOption::setSenseResource(void)
{
  if(_woption) XtSetSensitive(_woption, (int)sense());
}
       // cannot use SLBase::setSensitivity(sense()) above since
       //   _woption is not the top widget.


void SLpOption::setIvarResource(void)
{
  if(!_woption) return;
  SLpPush *opt = findOptionByIdent(ivar());
  if(!opt || !opt->topWidget()) return;
  XtVaSetValues(_woption, XmNmenuHistory, opt->topWidget(), NULL);
}



static String  defres[]= {
    ".spacing:                    0",
    ".marginWidth:                0",
    ".marginHeight:               0",
    NULL };


//----------------------------- make -----------------------------//
//----------------------------- make -----------------------------//
//----------------------------- make -----------------------------//

Widget SLpOption::make(Widget p)
{
  if(!made())
     {
     setDefaultResources(XtDisplay(wParent()), instanceName(), defres);
     Widget w = SLDelay::make(p);
     if(!w)    // w is always NULL before make() in this class
        {
        Arg args[22];
        int i = 0;

        _woption = XmCreateOptionMenu(wParent(), (char*)instanceName(),
                                                      args, i);
        i = 0;
        w = XmCreatePulldownMenu(_woption, (char*)instanceName(),
                                                      args, i);
        setParentOfChildren(w);
        XtVaSetValues(_woption, XmNsubMenuId, w, NULL);
        setTopWidget(_woption);
        XtAddCallback(w, XmNmapCallback,
                      (XtCallbackProc)doMapCallback, (XtPointer)this);
/*
        w = XmCreatePulldownMenu(wParent(), (char*)instanceName(),
                                                      args, i);
        setTopWidget(w);
        i = 0;
        XtSetArg(args[i], XmNsubMenuId, w); i++;
        _woption = XmCreateOptionMenu(wParent(), (char*)instanceName(),
                                                      args, i);
*/
        XtManageChild(_woption);
        Widget wbutton = XmOptionButtonGadget(_woption);
        XtVaSetValues(wbutton, XmNalignment, XmALIGNMENT_BEGINNING, NULL);
        }
     setTraversal(_traversal);
   //  install_help(_woption);
     install_help();
     XtAddEventHandler (_woption, FocusChangeMask, FALSE,
                        (XtEventHandler)focusEventHandler, (XtPointer)this);
   //if(updateSenseNeverActivated()) setupSenseValue(XtIsSensitive(_woption));
     if(updateSenseNeverActivated()) setupSenseValue(SLBase::sensitivity());
     makeChildren();      // added February 6, 1995 to fix bug.
     updateSelf(TRUE);
     }
  makeChildren();
  return parentOfChildren();
}
       // cannot use SLBase::sensitivity() above since
       //   _woption is not the top widget.
                  // IT IS NOW!



//--------------------------- do map callback -------------------------//
//--------------------------- do map callback -------------------------//
//--------------------------- do map callback -------------------------//

       // private.

void  SLpOption::doMapCallback(Widget w, XtPointer udata, XEvent*)
{
  /*
   * Send a client message to install the right color map
   * for this pulldown menu.  The MWM does not install private colormaps
   * correctly on pull down or option menus so we must do it ourselves.
   * However, it must be done after MWM installs the wrong colormap.
   * That is why we do it in a client message.
   */
  SLpOption *obj = (SLpOption *)udata;
  SLClientMessage *cm= new SLClientMessage(w, "aname" );
  cm->setComplexNotify(obj);
}



//------------------------ notify complex ----------------------------//
//------------------------ notify complex ----------------------------//
//------------------------ notify complex ----------------------------//

      // public.

Boolean SLpOption::notifyComplex(SLDelay*, int )
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



//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
