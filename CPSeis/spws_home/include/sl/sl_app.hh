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
#ifndef SLAPP_H
#define SLAPP_H

#include <X11/X.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "sl/sl_shell_container.hh"
#include "wproc.h"


class PullPop;

typedef void (*SLClosingFunc)(void *);
typedef void (*WorkProcFunction)(void *);


class SLApp : public SLShellContainer {
  private:
    SLClosingFunc _altCloseAction;
    void         *_altCloseData;
    static void  oneWidget(Widget w, void *data);
    static void DoMBAction(Widget, XtPointer, XEvent*);


  protected:
      Boolean  _do_main;
      Widget   _toplevel;
      Widget   _mainw;
      Widget   _mbar;
      Widget   _statline;
      Widget   _mode;
      Widget   _mhelp;
      Display  *_dpy;
      Boolean  _menu_added;
      Boolean  _exiting;
      char     *_app_name;
      char     *_class_name;
      //char     *_bg_res;

       //----> CreateMain is never called with alt_cmap specified
       //----> by any C++ code in spws/programs or spws/oop:

      void createMain(Boolean private_colomap, Colormap alt_cmap =0,
        Boolean dynamic_colomap = False);
      XtAppContext _app_context;
      class IpcIO *_ipc_io;

  public:
     /*
      * Constructor for very first SLApp made does all Xt initialization
      */
     SLApp
       (char *app_name, 
        char *class_name, 
        int &argc,
        char **argv, 
        XrmOptionDescRec *options = NULL,
        int num_options = 0,
        Boolean private_cmap = False,
        char **fb = NULL,
        class IpcIO *ipc_io = NULL,
        Boolean dynamic_colomap = False);

     /*
      * Constructor for creating SLApp when you already know the display
      */

       //----> this SLApp is never called with cmap specified
       //----> by any C++ code in spws/programs:

     SLApp
       (Display *dpy, 
        char *app_name, 
        char *class_name, 
        int screen_number = UseResource,
        Boolean private_cmap = False,
        Colormap cmap = 0,
        char **fb = NULL,
        class IpcIO *ipc_io = NULL,
        Boolean dynamic_cmap = False);

     /*
      * Constructor for creating SLApp on another display
      */
     SLApp
       (char *app_name,
        char *class_name,
        String display_name,
        XtAppContext app_context,
        Boolean private_colomap = False,
        char **fb = NULL,
        class IpcIO *ipc_io = NULL,
        Boolean dynamic_cmap = False);

     ~SLApp();

     void setFallbacks(char **fb);
     //void setIconPixmaps(Pixmap icon, Pixmap mask);
     void getResources( XtPointer base, XtResourceList res, Cardinal num);
     Colormap cMap() {return _cmap;}   // return current color map
     void realize();                   // realize the widget tree
     virtual void realizing() {};      // called when realize is called
     int loop();                      // main loop- will realize if necessary
     void showStatline(Boolean s);
     void setHctx(HelpCtx hctx);
     void initHelp(char *help_file= "", char *helpstr= "Help Window");
     Widget topLevel()      {return(_toplevel);}
     Widget mainWindow()    {return(_mainw);}
     Widget menuBar()       {return(_mbar);}
     Widget modeWidget()    {return(_mode);}
     Widget statusWidget()  {return(_mhelp);}
     void   setMode(char *s)    {wprocShowMsg(_mode,s);  XFlush(_dpy);}
     void   setMessage(char *s) {wprocShowMsg(_mhelp,s); XFlush(_dpy);}
     void   setWorkArea(Widget w);
     Widget getWorkArea();
     void    addedAPulldown();
     Boolean menuHadBeenAdded();
                           
     void setWorkArea(SLDelay *sl) {setWorkArea(sl->W());}
     int addTo(const char *moption, PullPop *pull);

      //----> tryAgainWithPrivate is never called with cmap specified
      //----> by any C++ code in spws/programs or spws/oop:

     Colormap tryAgainWithPrivate(int number, int num_planes = 0,
       Colormap cmap= 0);
     Boolean  canIAlloc(int number, int num_planes=0);

     void addTransient();
     virtual void    closing();
     virtual void    closeMain();
     void setAltClosingAction( SLClosingFunc action, void *data)
                                   { _altCloseAction= action;
                                     _altCloseData  = data; };
     virtual Display *display() { return _dpy;}

     virtual Boolean isTopLevel()  { return True; };
     virtual WidgetClass topClass();
     virtual WidgetClass dialogShellType();

     class IpcIO *ipcIO ();
     XtWorkProcId addWorkProcToMainLoop (XtWorkProc work_proc,
       XtPointer data);

     void checkIpc ();

     void   registerWorkProcFunction (WorkProcFunction func, void *obj=NULL);
     void unregisterWorkProcFunction (WorkProcFunction func, void *obj=NULL);

  protected:
     void callWorkProcedures (float seconds = 0.1);
     static Boolean workProcedure (XtPointer obj);
     void mustGetSomeRest (float seconds = 0.1);

     int _xt_work_proc_id;
     int _work_proc_count;
     int _work_proc_alloc;
     WorkProcFunction *_work_proc_funcs;
     void **_work_proc_objs;
     

};

#endif
