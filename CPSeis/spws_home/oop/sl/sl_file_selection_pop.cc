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

//---------------------- sl_file_selection_pop.cc -----------------------//
//---------------------- sl_file_selection_pop.cc -----------------------//
//---------------------- sl_file_selection_pop.cc -----------------------//

//         implementation file for the SLFileSelectionPop class
//               derived from the SLShellContainer class
//                           subdirectory sl


     //  Care should be taken to make sure that this popup
     //  does not disappear from the display before it is
     //  popped down by pressing its YES or NO button; otherwise
     //  it will not be deleted and the user will not be able
     //  to use the application.  This can happen if the parent
     //  of this object is unmanaged immediately after this
     //  object is created, before returning to the event loop.
     //  To avoid this, either make sure that the parent is not
     //  unmanaged at such a time, or make the parent be the main
     //  window.  See the implementation file for SLErrorPop
     //  for more details.
 

#include "sl/sl_file_selection_pop.hh"
#include "sl/sl_prim.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"
#include "wproc.h"
#include "cprim.h"
#include "str.h"
#include "named_constants.h"
#include "fast_file_proc.h"
#include <Xm/FileSB.h>
#include <Xm/FileSBP.h>
#include <stdlib.h>
#include <iostream.h>
#include <assert.h>



//---------------------------- constructors ------------------//
//---------------------------- constructors ------------------//
//---------------------------- constructors ------------------//


SLFileSelectionPop::SLFileSelectionPop
                      (SLDelay *slparent, char *name,
                       const char            *pattern,
                       SLFileSelectionAnswer *yes_fun,
                       void                  *user_data)
     : SLShellContainer(slparent, name, NULL),
                 _pattern             (str_newstr(pattern)),
                 _yes_fun             (yes_fun),
                 _user_data           (user_data)
{
  assert(_yes_fun);
  assert(_user_data);
}



//--------------- callbacks -------------------------------------//
//--------------- callbacks -------------------------------------//
//--------------- callbacks -------------------------------------//

void SLFileSelectionPop::yesCallback (Widget w, XtPointer data,
  XtPointer call)
{
  SLFileSelectionPop *pop = (SLFileSelectionPop*)data;
  XmFileSelectionBoxCallbackStruct *call2
                         = (XmFileSelectionBoxCallbackStruct*)call;
  //// cout << "am in SLFileSelectionPop YES callback" << endl;

  char *filename;

  if (XmStringGetLtoR(call2->value,XmSTRING_DEFAULT_CHARSET,&filename)) {
    pop->_yes_fun (pop->_user_data, filename);
    XtFree (filename);
  }
  else {
    pop->_yes_fun (pop->_user_data, "NONE");
  }

//XSync(XtDisplay(w), FALSE);       // doesn't help
  XmUpdateDisplay (w);              // usually doesn't work (timing?)
//XSync(XtDisplay(w), FALSE);       // doesn't help
  SLPrim::updateEverything ();
  pop->unmanage ();
}



void SLFileSelectionPop::applyCallback (Widget w, XtPointer data,
  XtPointer call)
{
  SLFileSelectionPop *pop = (SLFileSelectionPop*)data;
  XmFileSelectionBoxCallbackStruct *call2
                         = (XmFileSelectionBoxCallbackStruct*)call;
  

  //// cout << "am in SLFileSelectionPop APPLY callback" << endl;

  Arg arglist[4] ;
  int n;
  XmString dirmask;
  Widget filter_button_widget;
  Window filter_button_window;

  //XmProcessTraversal (call2->filew, XmTRAVERSE_CURRENT); 
  
  if (call2->reason == XmCR_APPLY) {
    /* this is an apply call back - find out if it was due to filter button */
    filter_button_widget = XmFileSelectionBoxGetChild (
      pop->_filebox, XmDIALOG_APPLY_BUTTON);
    filter_button_window = XtWindow (filter_button_widget);
    if (call2->event->xany.window == filter_button_window) {
      /* the event window is identical to the Filter button window */
      reset_file_list ();
      n=0;
      XtSetArg (arglist[n], XmNdirMask, &dirmask );
      n++;
      XtGetValues (pop->_filebox, arglist, n);
      /* do the file search again */
      XmFileSelectionDoSearch (pop->_filebox, dirmask); 
      XmStringFree (dirmask);
    }

//  XSync(XtDisplay(w), FALSE);       // doesn't help
    XmUpdateDisplay (w);              // usually doesn't work (timing?)
//  XSync(XtDisplay(w), FALSE);       // doesn't help
    SLPrim::updateEverything ();
  }
}



void SLFileSelectionPop::noCallback
               (Widget w, XtPointer data, XtPointer /*call*/)
{
  SLFileSelectionPop *pop = (SLFileSelectionPop*)data;
  //// cout << "am in SLFileSelectionPop NO callback" << endl;
  pop->unmanage();
}



//--------------------- destructor ---------------------------//
//--------------------- destructor ---------------------------//
//--------------------- destructor ---------------------------//

SLFileSelectionPop::~SLFileSelectionPop()
{
  //// cout << "am in SLFileSelectionPop destructor" << endl;
  if(_pattern) free(_pattern);
}



//--------------------------- manage ----------------------------//
//--------------------------- manage ----------------------------//
//--------------------------- manage ----------------------------//

    // private.

void SLFileSelectionPop::manage()
{
  assert(made());
  if(XtIsManaged(topWidget()))
      {
 ///  XtUnmanageChild(topWidget());
      SLShellContainer::unmanage();
      }
  else
      {
      XmFileSelectionDoSearch(topWidget(), NULL);
 ///  XtManageChild(topWidget());
      SLShellContainer::manage();
      }
}



//---------------------------- make -----------------------------//
//---------------------------- make -----------------------------//
//---------------------------- make -----------------------------//


Widget SLFileSelectionPop::make(Widget p)
{
  if(!made()) {
    Widget w = SLShellContainer::make(p);
    if (!w) {
      Arg args[22];
      int nargs = 0;
      XtSetArg (args[nargs], XmNdirSearchProc , fast_dir_proc );
      nargs++;
      XtSetArg (args[nargs], XmNfileSearchProc, fast_file_proc);
      nargs++;
      if (!p) p = wParent ();
      Paintset *paintset = PaintsetCollection::fetch (p);
      paintset->addResources (args, &nargs);
      _filebox = XmCreateFileSelectionDialog (wParent(), _name, args, nargs);
      setTopWidget (_filebox);
      XtAddCallback (_filebox, XmNokCallback, (XtCallbackProc)yesCallback,
        (XtPointer)this);
      XtAddCallback (_filebox, XmNapplyCallback,
        (XtCallbackProc)applyCallback, (XtPointer)this);
      XtAddCallback (_filebox, XmNcancelCallback, (XtCallbackProc)noCallback,
        (XtPointer)this);
      Widget wh = XmFileSelectionBoxGetChild (_filebox, XmDIALOG_HELP_BUTTON);

      setPattern ();

      XtUnmanageChild (wh);
      Widget shell = get_shell_widget (_filebox);
      XtVaSetValues (shell, XtNtitle, _name, NULL);
    }
  }
  return topWidget();
}

char *SLFileSelectionPop::directory ()
{
  char *retval;

  if (made() && _filebox) {
    XmString          xmdir;
    XmStringContext   ctx;
    XmStringCharSet   cset;
    XmStringDirection dir;

    Boolean status, sep;

    XtVaGetValues (_filebox, XmNdirectory, &xmdir, NULL);
    if (!XmStringInitContext(&ctx,xmdir)) {
      retval = NULL;
    }
    else {
      status = XmStringGetNextSegment (ctx, &retval, &cset, &dir, &sep);
      XtFree (cset);
    }
    XmStringFreeContext (ctx);
    if (xmdir) XmStringFree (xmdir);
  }
  else {
    retval = NULL;
  }
  return retval;
}

void SLFileSelectionPop::setDirectory (char *directory)
{
  if (made() && _filebox && directory) {
    XmString xmdir = XmStringCreateLtoR (directory, XmSTRING_DEFAULT_CHARSET);
    XtVaSetValues (_filebox, XmNdirectory, xmdir, NULL);
    XmStringFree (xmdir);
  }
}

void SLFileSelectionPop::setPattern (char *pattern)
{
  if (pattern) {
    if (_pattern) {
      free (_pattern);
      _pattern = NULL;
    }
    if (strcmp(pattern,CNIL) != 0) {
      _pattern = newstr (pattern);
    }
  }

  if (made() && _filebox) {
    if (_pattern && strcmp(_pattern,CNIL) != 0) {
      XmString string = XmStringCreate (_pattern, XmSTRING_DEFAULT_CHARSET);
      Arg args[1];
      int nargs = 0;
      XtSetArg (args[nargs], XmNpattern, string); nargs++;
      XtSetValues (_filebox, args, nargs);
      XmStringFree (string);
    }
  }
}

//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
