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

//---------------------- sl_error_pop.cc -----------------------//
//---------------------- sl_error_pop.cc -----------------------//
//---------------------- sl_error_pop.cc -----------------------//

//          implementation file for the SLDialog class
//           derived from the SLShellContainer class
//                       subdirectory sl


     //  Care should be taken to make sure that this popup
     //  does not disappear from the display before it is
     //  popped down by pressing its OK button; otherwise
     //  it will not be deleted and the user will not be able
     //  to use the application.  This can happen if the parent
     //  of this object is unmanaged immediately after this
     //  object is created, before returning to the event loop.
     //  To avoid this, either make sure that the parent is not
     //  unmanaged at such a time, or make the parent be the main
     //  window.  I have been unsuccessful in finding an event
     //  which will signify that the popup has gone away.  (See
     //  the commented-out event handler information.)
 

#include "sl/sl_error_pop.hh"
#include "sl/paintset_collection.hh"
#include "wproc.h"
#include "cprim.h"
#include <Xm/MessageB.h>
#include <stdlib.h>
#include <iostream.h>
#include <assert.h>


//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//
//---------------------- constructors -----------------------//

SLErrorPop::SLErrorPop(Widget wparent, char *name, const char *msg)
     : SLShellContainer(wparent, name, NULL)
{
  constructorHelper(msg);
}


SLErrorPop::SLErrorPop(SLDelay *slparent, char *name, const char *msg)
     : SLShellContainer(slparent, name, NULL)
{
  constructorHelper(msg);
}



//--------------- callbacks and event handlers ------------------//
//--------------- callbacks and event handlers ------------------//
//--------------- callbacks and event handlers ------------------//

static void ok_callback(Widget w, XtPointer data, XtPointer /* call */)
{
  SLErrorPop *pop = (SLErrorPop*)data;
  //// cout << "am in SLErrorPop ok callback" << endl;
  if(w == pop->W()) delete pop;
}


    //  The following callback and event handler are not called
    //  in the crucial case when the error box disappears from
    //  the screen because the widget parent was unmanaged:

/*
static void unmap_callback(Widget w, XtPointer data, XtPointer call)
{
  SLErrorPop *pop = (SLErrorPop*)data;
  //// cout << "am in SLErrorPop unmap callback" << endl;
  if(w == pop->W()) delete pop;
}


static void event_handler(Widget w, XtPointer data, XEvent *event)
{
  SLErrorPop *pop = (SLErrorPop*)data;
  //// cout << "am in SLErrorPop event handler " << event->type << endl;
  if(event->type == UnmapNotify)
      {
      //// cout << "UnmapNotify" << endl;
      if(w == pop->W()) delete pop;
      }
  else if(event->type == VisibilityNotify)
      {
      //// cout << "VisibilityNotify" << endl;
      if(event->xvisibility.state == VisibilityFullyObscured)
         {
         //// cout << "VisibilityFullyObscured" << endl;
         if(w == pop->W()) delete pop;
         }
      }
}
*/



//--------------------- destructor ---------------------------//
//--------------------- destructor ---------------------------//
//--------------------- destructor ---------------------------//

SLErrorPop::~SLErrorPop()
{
/*
  if(made())
     {
     XtRemoveCallback(topWidget(), XmNunmapCallback,
               (XtCallbackProc)unmap_callback, (XtPointer)this);
     XtRemoveEventHandler(topWidget(),
               StructureNotifyMask || VisibilityChangeMask, FALSE,
               (XtEventHandler)event_handler, (XtPointer)this);
     }
*/
  //// cout << "am in SLErrorPop destructor" << endl;
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static String defres[]= {
    "*background:         red",
    "*messageAlignment:   ALIGNMENT_CENTER",
    "*dialogStyle:        DIALOG_FULL_APPLICATION_MODAL",
    NULL };



//----------------- constructor helper -----------------------//
//----------------- constructor helper -----------------------//
//----------------- constructor helper -----------------------//

void SLErrorPop::constructorHelper(const char *msg)
{
  assert(msg);
///////  setFallbackResources(defres);  // no pseudo widget.
  make();

  Widget shell = get_shell_widget(W());
  XtVaSetValues(shell, XtNtitle, _name, NULL);

  XmString string = XmStringCreateLtoR((char*)msg,
                                       XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues(topWidget(), XmNmessageString, string, NULL);
  XmStringFree(string);

  manage();
}



//---------------------------- make -----------------------------//
//---------------------------- make -----------------------------//
//---------------------------- make -----------------------------//


Widget SLErrorPop::make(Widget p)
{
  if(!made())
       {
       Widget w = SLShellContainer::make(p);
       if(!w)
           {
           char fullname[200];
           get_full_name(wParent(), fullname);
           strcat(fullname, "*");
           strcat(fullname, (char*)instanceName());
           setDefRes(XtDisplay(wParent()), fullname, defres);
   /// the "*" above allows for the fact that there is the name of
   /// the shell widget between the name of the parent and the
   /// instance name.
/*
           setDefaultResources(XtDisplay(wParent()), instanceName(), defres);
*/
	   Arg arglist[22];
	   int n = 0;
	   PaintsetCollection::addResources (arglist, &n, wParent());
           w= XmCreateErrorDialog(wParent(), _name, arglist, n);
           setTopWidget(w);
           XtAddCallback(w, XmNokCallback,
                         (XtCallbackProc)ok_callback, (XtPointer)this);
/*
           XtAddCallback(w, XmNunmapCallback,
                         (XtCallbackProc)unmap_callback, (XtPointer)this);
           XtAddEventHandler(w,
                   StructureNotifyMask || VisibilityChangeMask, FALSE,
                   (XtEventHandler)event_handler, (XtPointer)this);
*/
           Widget wc =XmMessageBoxGetChild(w, XmDIALOG_CANCEL_BUTTON);
           Widget wh =XmMessageBoxGetChild(w, XmDIALOG_HELP_BUTTON);
           XtUnmanageChild(wc);
           XtUnmanageChild(wh);
           }
       }
  return topWidget();
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
