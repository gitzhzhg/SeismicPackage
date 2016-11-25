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
//////////////new//////////////////
#include <time.h>
#include <math.h>
//////////////new//////////////////

#include <Xm/MainW.h>
#include <Xm/Protocols.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>

#include "cprim.h"
#include "initusage.h"
#include "sl/sl_app.hh"
#include "sl/psuedo_widget.hh"

//////////////new//////////////////
#include "sl/paintset.hh"
#include "sl/paintset_collection.hh"
//#include "paintset_from_c.h"
//////////////new//////////////////

#include "ipc/ipc_io.hh"

#define COPY_COLORS 20


static String  defres[]= {
    "*mbar.spacing:        7",
    "*statline.XmText.marginHeight: 1",
    "*statline.XmText.marginWidth:  1",
    "*statline.XmText.rows:         2",
    "*statline.XmText.editMode:     XmMULTI_LINE_EDIT",
    "*statline.XmText.editable:     False",
    NULL };

static char *statline_font     = "-*-lucida-bold-r-*-*-*-100-*-*-*-*-*-*";
static char *statline_font_fxd = "-*-fixed-bold-r-*-*-*-100-*-*-*-*-*-*";

static void putRequiredResources(char *classname, Display *dpy)
{
  int i;
  XrmDatabase system_db;
  char buf[1000];
  char *vendor= ServerVendor(dpy);
  Boolean extra= False;

  if ( strstr( vendor, "Silicon Graphics") )  {
      extra= True;
      printf("Loading SGI server specific resources\n");
  }

#if (XlibSpecificationRelease>=5)
  for(i=0; (i<ScreenCount(dpy)); i++) {
    system_db = XtScreenDatabase(ScreenOfDisplay(dpy,i));
    sprintf(buf, "%s*useColorObj: False", classname);
    XrmPutLineResource( &system_db, buf );

    if (extra) {
         sprintf(buf, 
             "%s*spfont: -*-*-medium-r-*-*-*-90-*-*-m-*-*-*", classname);
         XrmPutLineResource( &system_db, buf );
         sprintf(buf, 
             "%s*spboldfont: -*-*-bold-r-*-*-*-90-*-*-m-*-*-*", classname);
         XrmPutLineResource( &system_db, buf );
    } // end if sgi server

  } // end loop
#endif


}



SLApp::SLApp (char *app_name, char *class_name, int &argc, char **argv, 
  XrmOptionDescRec *options, int num_options, Boolean private_colomap,
  char **fb, IpcIO *ipc_io, Boolean dynamic_colomap) :
  SLShellContainer (app_name, NULL,MayIcon), 
  _menu_added      (False),
  _altCloseAction  (NULL),
  _altCloseData    (NULL),
  _toplevel        (NULL),
  _ipc_io          (ipc_io),
  _xt_work_proc_id (0),
  _work_proc_count (0),
  _work_proc_alloc (0),
  _exiting         (False)
{
  Screen *scr;
  Arg arglist[22];
  int n=0;

  i_exist(argc, argv, "-CheckAndExit");

//////////////new//////////////////
  // since the paintset is initially fetched in SLApp, then process the args
  int k2;
  for (k2 = 0; k2 < argc; k2++) {
    if (strcmp(argv[k2],"-private") == 0) {
      private_colomap = True;
    }
    else if (strcmp(argv[k2],"-vdynamic") == 0) {
      dynamic_colomap = True;
    }
  }
  //  private_colomap = True; // 04-29-05 in order to run 8-bit linux version
//////////////new//////////////////

  XtToolkitInitialize();
  _app_context = XtCreateApplicationContext();
  _dpy = XtOpenDisplay(_app_context, NULL, app_name, class_name,
          options, num_options, &argc, argv);
  //init_usage_reporting (app_name);
  if (!_dpy) {
     printf("Cannot open display.\n");
     //usage_exit (0);
  }
  //putRequiredResources(class_name, _dpy);
  putRequiredResources(app_name, _dpy);


  scr= DefaultScreenOfDisplay(_dpy);

  n=0;
  XtSetArg (arglist[n], XtNallowShellResize, TRUE); n++;


  ///////////// got all the way to XtAppMainLoop.
  ///////////// then got the following:
  // X Error of failed request:  BadMatch (invalid parameter attributes)
  // Major opcode of failed request:  1 (X_CreateWindow)
  // Serial number of failed request:  633
  // Current serial number in output stream:  828


//////////////new//////////////////
///////////// this does not help the above problem at XtAppMainLoop:
  Paintset *paintset = PaintsetCollection::fetch (scr, private_colomap,
    dynamic_colomap);

  setColormap (paintset->colormap());

  paintset->addResources (arglist, &n);
//////////////new//////////////////


  _toplevel= XtAppCreateShell( app_name, class_name,
                               applicationShellWidgetClass,
                               _dpy, arglist, n );

  //paintset_add_resources_w (_toplevel, arglist, &n); // for linking cfg

  if (fb) setFallbacks(fb);
  createMain(private_colomap, paintset->colormap());
  SLShellContainer::make(_toplevel);

  _app_name=   newstr(app_name);
  _class_name= newstr(class_name);

  //checkIpc ();
}


SLApp::SLApp (char *app_name, char *class_name, String display_name,
  XtAppContext app_context, Boolean private_colomap, char **fb,
  IpcIO *ipc_io, Boolean dynamic_colomap) :
  SLShellContainer (app_name, NULL, MayIcon), 
  _menu_added      (False),
  _altCloseAction  (NULL),
  _altCloseData    (NULL),
  _toplevel        (NULL),
  _app_context     (app_context),
  _ipc_io          (ipc_io),
  _xt_work_proc_id (0),
  _work_proc_count (0),
  _work_proc_alloc (0),
  _exiting         (False)
{
  Screen *scr;
  Arg arglist[22];
  int n=0;

	int none = 0;	// ehs 23jun94

  _dpy = XtOpenDisplay(_app_context, display_name, app_name, class_name,
			NULL, 0, &none, NULL);	// ehs 23jun94
  //init_usage_reporting (app_name);
  if (_dpy) {
      putRequiredResources(class_name, _dpy);
      scr= DefaultScreenOfDisplay(_dpy);


      n=0;
      XtSetArg (arglist[n], XtNallowShellResize, TRUE); n++;


//////////////new//////////////////
      Paintset *paintset = PaintsetCollection::fetch (scr, private_colomap,
        dynamic_colomap);

      setColormap (paintset->colormap());

      paintset->addResources (arglist, &n);
//////////////new//////////////////


      _toplevel= XtAppCreateShell( app_name, class_name,
                                   applicationShellWidgetClass,
                                   _dpy, arglist, n );
      if (fb) setFallbacks(fb);
      createMain(private_colomap, paintset->colormap());
      SLShellContainer::make(_toplevel);
    
      _app_name=   newstr(app_name);
      _class_name= newstr(class_name);
  }

  //checkIpc ();
}






SLApp::SLApp (Display *dpy, char *app_name, char *class_name,
  int screen_number, Boolean private_colomap, Colormap alt_cmap,
  char **fb, IpcIO *ipc_io, Boolean dynamic_colomap) :
  SLShellContainer (app_name, NULL, MayIcon), 
  _dpy             (dpy),
  _menu_added      (False),
  _altCloseAction  (NULL), 
  _altCloseData    (NULL),
  _toplevel        (NULL),
  _ipc_io          (ipc_io),
  _xt_work_proc_id (0),
  _work_proc_count (0),
  _work_proc_alloc (0),
  _exiting         (False)
{
      //----> this SLApp is never called with alt_cmap specified
      //----> by any C++ code in spws/programs.
      //----> Therefore, to keep things simple, I am enforcing the
      //----> continuation of this practice here:
                   assert(!alt_cmap);

  Arg arglist[22];
  int n=0;
  Screen *scr;

  putRequiredResources(class_name, _dpy);
  int  tot_screens= ScreenCount(_dpy);

  if (screen_number == UseResource) {
          scr= DefaultScreenOfDisplay(_dpy);
  }
  else if (screen_number == UseParentScreen) {
          scr= DefaultScreenOfDisplay(_dpy);
  }
  else if (screen_number < tot_screens) {
          scr= ScreenOfDisplay(dpy, screen_number);
  }
  else 
          scr= DefaultScreenOfDisplay(_dpy);

  n=0;
  XtSetArg (arglist[n], XtNallowShellResize, TRUE); n++;
  XtSetArg (arglist[n], XtNscreen, scr); n++;


//////////////new//////////////////
  Paintset *paintset = PaintsetCollection::fetch (scr, private_colomap,
    dynamic_colomap);

  setColormap (paintset->colormap());

  paintset->addResources (arglist, &n);
//////////////new//////////////////


  _toplevel= XtAppCreateShell( app_name, class_name, 
                               applicationShellWidgetClass,
                               _dpy, arglist, n );
  if (fb) setFallbacks(fb);
  createMain(private_colomap, alt_cmap);
  SLShellContainer::make(_toplevel);

  _app_name=   newstr(app_name);
  _class_name= newstr(class_name);

  //checkIpc ();
}





SLApp::~SLApp()
{
  if (_app_name)   free(_app_name);
  if (_class_name) free(_class_name);
  if (_toplevel)   XtDestroyWidget (_toplevel);
}

void SLApp::createMain(Boolean private_colomap, Colormap alt_cmap,
  Boolean dynamic_colomap)
{
      //----> createMain is never called with alt_cmap specified
      //----> by any C++ code in spws/programs or spws/oop.
      //----> Therefore, to keep things simple, I am enforcing the
      //----> continuation of this practice here:
      //             assert(!alt_cmap);
      // Had to go back and make things more complicated when the default
      //   colormap was not sufficient for main

  Screen *scr;
  Colormap cmap;

  setDefaultResources( _dpy, "mainw", defres);

  scr= XtScreen(_toplevel);


/************************
//////////////old//////////////////
  if (private_colomap)
      cmap=  alt_cmap ? alt_cmap : newcmap_andcpy(_toplevel, COPY_COLORS);
  else
      cmap=  DefaultColormapOfScreen(scr);

  _mainw= XtVaCreateManagedWidget( "mainw", xmMainWindowWidgetClass, _toplevel,
                  XmNcolormap,              cmap,
                  XmNcommandWindowLocation, XmCOMMAND_BELOW_WORKSPACE, NULL);
//////////////old//////////////////
************************/


//////////////new//////////////////
  Paintset *paintset;
  if (private_colomap) {
    paintset = PaintsetCollection::fetchExisting (alt_cmap);
  }
  else {
    paintset = PaintsetCollection::fetch (scr, private_colomap,
      dynamic_colomap);
  }

  cmap = paintset->colormap ();

  _mainw= XtVaCreateManagedWidget( "mainw", xmMainWindowWidgetClass, _toplevel,
                  XmNcolormap,              cmap,
                  XmNvisual  , paintset->visual(),
                  XmNdepth   , paintset->visualDepth(),
                  XmNcommandWindowLocation, XmCOMMAND_BELOW_WORKSPACE, NULL);
//////////////new//////////////////


  setColormap(cmap);

  /*
   * We cannot create the psuedo_widget with supportUnmadedefaults because
   * SLApp does not support the exact look of an sl object
   */
  _pw_topw= new PsuedoWidget( _toplevel, "mainw",  xmMainWindowWidgetClass,
                                         NULL);

  _mbar= XmCreateMenuBar( _mainw, "mbar", NULL, 0); 
 // PsuedoWidget *xx= new PsuedoWidget( _toplevel, NULL);
 // char *ss= xx->anyDef("background", "Background");
 //
 // Pixel col;
 // XtVaGetValues( _mainw, XmNbackground, &col, NULL);
 // XtVaSetValues( _mbar, XmNbackground, col, NULL);

    XtManageChild(_mbar);

  _statline = XtVaCreateManagedWidget ( "statline", xmFormWidgetClass,
    _mainw, NULL);
  Display *dpy = XtDisplay (_statline);
  Screen *sscr  = XtScreen (_statline);
  Window win   = RootWindowOfScreen (sscr);
  GC gc        = XCreateGC (dpy, win, 0, NULL);

  // attempt to use the defined statline_font
  XFontStruct *statline_font_obj = XLoadQueryFont (dpy, statline_font);
  XFreeFont (dpy, statline_font_obj); statline_font_obj = NULL; // test!
  if (statline_font_obj == NULL) {  // must used fixed!
    statline_font_obj = XLoadQueryFont (dpy, statline_font_fxd);
    assert (statline_font_obj); // insist that it exist
  }
  XSetFont (dpy, gc, statline_font_obj->fid);
  XFreeGC (dpy, gc);
  XFreeFont (dpy, statline_font_obj);

  showStatline(True);


  _mode =  XtVaCreateManagedWidget( "mode", xmTextWidgetClass, _statline,
                                    XmNtopAttachment,    XmATTACH_FORM,
                                    XmNbottomAttachment, XmATTACH_FORM,
                                    XmNrightAttachment,  XmATTACH_FORM, NULL);

  addModeArea(_mode);

  _mhelp = XtVaCreateManagedWidget( "mhelp", xmTextWidgetClass, _statline,
                                    XmNtopAttachment,    XmATTACH_FORM,
                                    XmNbottomAttachment, XmATTACH_FORM,
                                    XmNrightAttachment,  XmATTACH_WIDGET,
                                    XmNrightWidget,      _mode,
                                    XmNleftAttachment,   XmATTACH_FORM, NULL);
  addMsgArea(_mhelp);

  XmMainWindowSetAreas( _mainw,_mbar,_statline,NULL,NULL,NULL);
  setTopWidget(_mainw);
}

Colormap SLApp::tryAgainWithPrivate(int number, int num_planes, Colormap cmap)
{
      //----> tryAgainWithPrivate is never called with cmap specified
      //----> by any C++ code in spws/programs or spws/oop.
      //----> Therefore, to keep things simple, I am enforcing the
      //----> continuation of this practice here:
                   assert(!cmap);


//////////////new//////////////////
  Paintset *paintset = PaintsetCollection::fetchByNumColors (_toplevel,
    number, num_planes>0?1:0);
  if (paintset) {
    setColormap (paintset->colormap());
  }
  else {
    paintset = NULL;
    setColormap (NULL);
  }
//////////////new//////////////////


 XtDestroyWidget(_mainw);
 XtDestroyWidget(_toplevel);
 if (_pw_topw) delete _pw_topw;

 Arg arglist[22];
 int n=0;
 XtSetArg (arglist[n], XtNallowShellResize, TRUE); n++;


//////////////new//////////////////
  if (paintset) paintset->addResources(arglist, &n);
//////////////new//////////////////


 _toplevel= XtAppCreateShell( _app_name, _class_name, 
                               applicationShellWidgetClass,
                               _dpy, arglist, n );
 /*
  *  _wparent, _topw, _p_of_child are not normally accessed from 
  *  child classes of SLBase & SLDelay.
  *  This is a special case because the widgets of the object are
  *  changed after they are initially set.
  */
 _wparent=    _toplevel;
 setTopWidget(NULL);


 /*
  *  do create main again to create the main window stuff
  */

 createMain (True, paintset? paintset->colormap() : NULL);
 return(colormap());
}

Boolean SLApp::canIAlloc(int number, int num_planes)
{
/*
 long stat;
 unsigned long pix[500];
 unsigned long pmsk[24];
 stat= XAllocColorCells( _dpy, colormap(), True, pmsk, 
                         num_planes, pix,  number);
 if (stat) XFreeColors(_dpy, colormap(), pix, number, num_planes);
 XFlush(_dpy);

 return (Boolean)stat;
*/
// find out if the given widget can allocate the necessary colors
  Paintset *paintset = PaintsetCollection::fetch (_toplevel);
  return PaintsetCollection::numColorsLeft(paintset,num_planes) >= number;
}

void SLApp::setFallbacks(char **fb)
{
// XtAppSetFallbackResources( XtWidgetToApplicationContext(_toplevel), fb);
   XrmDatabase rdb = XrmGetStringDatabase ( "" );  // make empty res database

   /*
    *  Add the Component resources, prepending the name of the component
    */
   for(int i=0; ( fb[i] != NULL ); i++) {
       XrmPutLineResource( &rdb, fb[i] );
   }

   /*
    * Merge them into the Xt database, with lowest precendence
    */
   if ( rdb ) {
#        if (XlibSpecificationRelease>=5)
              XrmDatabase db = XtDatabase(_dpy);
              XrmCombineDatabase(rdb, &db, FALSE);
#        else
              XrmMergeDatabases ( _dpy->db, &rdb );
              _dpy->db = rdb;
#        endif
   } // End if

}


void SLApp::setHctx(HelpCtx hctx)
{
 setHelpCtx(hctx);
}

void SLApp::showStatline(Boolean s)
{
  if (s) XtManageChild(_statline);
  else   XtUnmanageChild(_statline);
}


void SLApp::setWorkArea(Widget w)
{
  XmMainWindowSetAreas( _mainw,_mbar,_statline,NULL,NULL,w);
}

static void test_map_xxxx( Widget w, XtPointer udata, XtPointer)
{
  printf("am mapping main window\n");
}

int SLApp::loop()    
{
/*
printf("1111111111111111 am entering main loop\n");
*/
  if (!XtIsRealized(W())) realize();
/*
printf("2222222222222222 am entering main loop\n");
*/
/*
                 XtAddCallback(_toplevel, XmNmapCallback,
                      (XtCallbackProc)test_map_xxxx, (XtPointer)this);
*/
/*
printf("3333333333333333 am entering main loop\n");
*/
  XtAppMainLoop(XtWidgetToApplicationContext(W()));
  printf ("I'm out\n");
  return 0;
}


void SLApp::getResources( XtPointer base, XtResourceList res, Cardinal num)
{
 XtGetApplicationResources(get_shell_widget(W()), base, res, num, NULL, 0);
}

void SLApp::initHelp(char *help_file, char *helpstr) 
{
   HelpCtx hctx= getHelpCtx();
   if (!hctx) {
      hctx= setup_help(mainWindow(), NULL, help_file, helpstr );
      setHelpCtx(hctx);
   }
}

void SLApp::realize() 
{ 
  Screen *scr;
  if (!getHelpCtx()) initHelp(); 
  scr= XtScreen(_toplevel);
  XtRealizeWidget(_toplevel);
/*************************
  if (colormap() != DefaultColormapOfScreen(scr) )
*************************/
         XSetWindowColormap (_dpy, XtWindow(_toplevel), colormap());
  realizing();
}

void SLApp::closing()
{ 
  closeMain();
}

Widget SLApp::getWorkArea() 
{ 
  Widget w;
  XtVaGetValues(_mainw, XmNworkWindow, &w, NULL);
  return w;
}

                           

void SLApp::closeMain()
{
  if (_altCloseAction) {
        _altCloseAction(_altCloseData);
  }
  else {
        printf("SLApp::closeMain: Application exiting from close\n");
        printf("SLApp::closeMain: no altCloseAction has been defined.\n");
        assert(_altCloseAction);
        //usage_exit (0);
  }
}
WidgetClass SLApp::topClass()
{ 
  return xmMainWindowWidgetClass;
};

WidgetClass SLApp::dialogShellType()
{
  return applicationShellWidgetClass;
}

IpcIO *SLApp::ipcIO ()
{
  return _ipc_io;
}

XtWorkProcId SLApp::addWorkProcToMainLoop (XtWorkProc work_proc,
  XtPointer data)
{
  return XtAppAddWorkProc (_app_context, work_proc, data);
}

void SLApp::checkIpc ()
{
  if (_ipc_io != NULL) _ipc_io->addWorkProcToMainLoop (this);
}

void SLApp::addedAPulldown()
{
  _menu_added= True;
}

Boolean SLApp::menuHadBeenAdded()
{
 return _menu_added;
}

#define INCR 5

// WorkProcFunction is a static function
//   obj is a specific instance containing data necessary for the function
//   if there is no need of instance data (i.e. no, or static data is used)
//   then obj may be NULL
void SLApp::registerWorkProcFunction (WorkProcFunction func, void *obj)
{
  if (_work_proc_count == _work_proc_alloc) {
    _work_proc_alloc += INCR;
    WorkProcFunction *tf = (WorkProcFunction *)malloc (
      _work_proc_alloc*sizeof(WorkProcFunction));
    void **to = (void **)malloc (_work_proc_alloc*sizeof(void *));
    int k2;
    for (k2 = 0; k2 < _work_proc_count; k2++) {
      tf[k2] = _work_proc_funcs[k2];
      to[k2] = _work_proc_objs [k2];
    }
    for (; k2 < _work_proc_alloc; k2++) {
      tf[k2] = NULL;
      to[k2] = NULL;
    }
    if (_work_proc_count > 0) {
      free (_work_proc_funcs);
      free (_work_proc_objs );
    }
    _work_proc_funcs = tf;
    _work_proc_objs  = to;
  }
  _work_proc_funcs[_work_proc_count] = func;
  _work_proc_objs [_work_proc_count] = obj ;
  _work_proc_count++;
  if (_work_proc_count == 1) {
    // When count gets above zero, start SLApp's workProcedure running as
    //   an XtWorkProc continually
    _xt_work_proc_id = XtAppAddWorkProc (_app_context, workProcedure,
      (XtPointer)this);
  }
}

void SLApp::unregisterWorkProcFunction (WorkProcFunction func, void *obj)
{
  int k2;
  for (k2 = 0; k2 < _work_proc_count; k2++) {
    if (func == _work_proc_funcs[k2] &&
        obj  == _work_proc_objs [k2]   ) {
      _work_proc_count--;
      for (; k2 < _work_proc_count; k2++) {
	_work_proc_funcs[k2] = _work_proc_funcs[k2+1];
	_work_proc_objs [k2] = _work_proc_objs [k2+1];
      }
      _work_proc_funcs[k2] = NULL;
      _work_proc_objs [k2] = NULL;
    }
  }
  if (_work_proc_count < 1) {
    // When count gets down to zero, remove SLApp' XtWorkProc completely
    XtRemoveWorkProc (_xt_work_proc_id);
    _xt_work_proc_id = 0;
  }
}

void SLApp::callWorkProcedures (float seconds)
{
  int k2;
  struct timeval tv_start, tv_end; 
  gettimeofday (&tv_start, NULL);
  // the given number of seconds is a minimum that this function must
  //   either be working or idling, otherwise, the application is in
  //   danger of using up all the CPU cycles without doing anything!
  for (k2 = 0; k2 < _work_proc_count; k2++) {
    (*_work_proc_funcs[k2])(_work_proc_objs[k2]);
  }
  gettimeofday (&tv_end, NULL);
  float diff_seconds = tv_end.tv_sec  - tv_start.tv_sec;
  float diff_usecs   = tv_end.tv_usec - tv_start.tv_usec;
  diff_seconds = diff_seconds + diff_usecs / 1000000.0;
  seconds -= diff_seconds;
  if (seconds > 0.0) {
    mustGetSomeRest (seconds);
  }
  else {
    int time_spent = 1;
  }
}

Boolean SLApp::workProcedure (XtPointer obj)
{
  SLApp *app = (SLApp *)obj;

  if (app->_exiting) {
    exit (0);
    // return True; // return was sufficient for cbyt, but not for va, csv,
                    //   cfg, & geopress; therefore exit(0) was used!!!!
  }

  app->callWorkProcedures (/*0.1*/);
  return app->_xt_work_proc_id == 0; // workProcedure continues to run so long
                                     //    as this is False
}

void SLApp::mustGetSomeRest (float seconds)
{
  struct timeval tv;
  int whole_secs  = (int)floor ((double)seconds);
  int whole_usecs = (int)floor ((double)(seconds - whole_secs) * 1000000);
  tv.tv_sec  = whole_secs ; // # of seconds to sleep
  tv.tv_usec = whole_usecs; // # of microseconds to sleep
  select (1, NULL, NULL, NULL, &tv);
}
