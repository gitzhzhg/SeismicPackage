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
#include <string.h>
#include <assert.h>
#include <Xm/Xm.h>
#include "wproc.h"
#include "cprim.h"
#include "cube_slice_app.hh"
#include "cube/cube_display.hh"
#include "cube/cube_select.hh"
#include "cube/cube_annotation_gui.hh"
#include "cube/cube_section_gui.hh"
#include "cube/cube_master.hh"
#include "cube/cube_movie_control.hh"
#include "cube/cube_movie_pop.hh"
#include "cube/cube_amplitude_gui.hh"
#include "cube/cube_table_guis.hh"
#include "cube/cube_random_line_pop.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_cbar_pop.hh"
#include "sp/seis_zoomop_pop.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/sl_form.hh"
#include "sl/sl_def_pop.hh"
#include "sl/error_handler.hh"
#include "sl/shell_mouse_help.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/version_info.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_ascii_viewer.hh"
#include "hardcopy/hardcopy_seis_pop.hh"
////////////////// new /////////////////////////
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"
#include "sl/colorset_collection.hh"
#include "sl/color_info_set.hh"
////////////////// new /////////////////////////

#include "initusage.h"


#define CLASS_NAME "Csv"
#define APP_NAME   "csv"



enum {ADDFILE, CHANGEFILE, DELFILE, QUIT};       // File   pulldown
enum {SCRLOCK, OVERVIS, SYNC_SLICES, ADD_GRAPH, REM_GRAPH}; // Option pulldown
enum {ZLINE, ZXLINE, ZTS, ZRL, ZOP};                     // Zoom   pulldown
enum {SHOWIL, SHOWXL, SHOWTS, SHOWWF};                   // Window  pulldown
enum {COLOR};                                            // Custom pulldown
enum {LHARD, XHARD, TSHARD, CONTROL };                   // tools pulldown
enum {CTX, OVERVIEW, VERSION, MOUSEHELP};                // Help   pulldown
enum {ZUPp, ZUPSEPp, ZDOWNp, ZORGINp, ADDFILEp };        // popup  menu
enum {REMTLINEb };                                       // bottom buttons
enum { _XH_OFF, _XH_SMALL, _XH_BIG };                    // crosshair radios

static char *fallback_resources[] = {
#     include "Csv.h"
      NULL
};

static char *fetch_proper_help_file()
{
   static char helpfile[120];
   strcpy(helpfile, getenv("CPSEIS_INSTALL_DIR"));
   strcat(helpfile, "/spws_home/app-defaults/Csv_help");
   printf("csv help file = %s\n", helpfile);
   return helpfile;
}
static char *helpfile = fetch_proper_help_file();


static XtResource res[]= {
      { "privateCmap", "PrivateCmap", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cs_res,private_cmap), XtRImmediate,
        (XtPointer) False } ,
      { "doBackingStore", "DoBackingStore", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cs_res,dobacking), XtRImmediate, (XtPointer) True } ,
      { "showHelp", "ShowHelp", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cs_res,showhelp), XtRImmediate, (XtPointer) False } ,
      { "cbarColors", "CbarColors", XtRInt, sizeof(long),
        XtOffsetOf(cs_res,num_col_colors), XtRImmediate, (XtPointer) 66 },
      { "helpFile", "HelpFile", XtRString, sizeof(char *),
        XtOffsetOf(cs_res,help_file), XtRString,
            (void*)helpfile },
      { "defaultFile", "DefaultFile", XtRString, sizeof(char *),
        XtOffsetOf(cs_res,def_file), XtRString, NULL },
};

static XrmOptionDescRec options[] = {
      { "-private",     "*privateCmap",     XrmoptionNoArg,  "True" },
      { "-help",        "*showHelp",        XrmoptionNoArg,  "True" },
      { "-nobs",        "*doBackingStore",  XrmoptionNoArg,  "False"},
      { "-bs",          "*doBackingStore",  XrmoptionNoArg,  "True" },
      { "-cbar",        "*cbarColors",      XrmoptionSepArg, NULL   },
      { "-helpfile",    "*helpFile",        XrmoptionSepArg, NULL   },
      { "-defaults",    "*defaultFile",     XrmoptionSepArg, NULL   },
     };



Boolean       CubeSliceApp::_static_initialized= False;
int           CubeSliceApp::_instance_cnt= 0;
CubeSliceApp *CubeSliceApp::_cube_graph[CubeSliceApp::MAX_WINDOWS];
Pixmap        CubeSliceApp::_icon= 0;
Pixmap        CubeSliceApp::_mask= 0;
cs_res        CubeSliceApp::_resources;

CubeSliceApp::CubeSliceApp(int& argc, char **argv) :
              SLApp( APP_NAME, CLASS_NAME, argc, argv,
                     options, XtNumber(options) ), 
              CubeInform(),
                     _cube_display(NULL)
{
  if (!_static_initialized) doStatics();  // color, help, icon, resources
  setFallbacks(fallback_resources);
  doColor();
  createMainArea();
  createPopups();
  createPullDowns();
  finalInit();
  _version= newstr(CUBE_VERSION);
}

CubeSliceApp::CubeSliceApp(CubeSliceApp *other, int screen) :
              SLApp( XtDisplay(other->W()), APP_NAME, CLASS_NAME, screen),
              CubeInform(),
              _cube_display(NULL)
{
  if (!_static_initialized) doStatics();  // color, help, icon, resources
  setFallbacks(fallback_resources);
  doColor();
  createMainArea();
  createPopups();
  createPullDowns();
  finalInit();
  realize();
}




CubeSliceApp::~CubeSliceApp()
{
  _instance_cnt--;

  CubeMaster::instance()->delInformer(this);

  delete _file_pull;    _file_pull    = NULL;
  delete _xh_cas;       _xh_cas       = NULL;
  delete _option_pull;  _option_pull  = NULL;
  delete _window_pull;  _window_pull  = NULL;
  delete _help_pull;    _help_pull    = NULL;
  delete _popup_menu;   _popup_menu   = NULL;
  //delete _cube_display; _cube_display = NULL;
  delete _cube_amp;     _cube_amp     = NULL;
  delete _cube_select;  _cube_select  = NULL;
  delete _cube_anno;    _cube_anno    = NULL;
  delete _cube_section; _cube_section = NULL;
  delete _version_info; _version_info = NULL;
  delete _main_form;    _main_form    = NULL;
  delete _cube_table;   _cube_table   = NULL;
  //delete _cube_display; _cube_display = NULL;

  if (_exiting || _instance_cnt == 0) {
    ColorsetCollection:: remove ();
    ColorInfoCollection::remove ();
    usage_exit (0);
  }
}


void CubeSliceApp::doStatics()
{
   Display *dpy= XtDisplay(topWidget());
   Screen  *scr= XtScreen(topWidget());;

   _static_initialized= True;

   // --------- resources ------------
   getResources( &_resources, res, XtNumber(res) );

 
   if (_resources.def_file) {
        DefLoadFileRes(topLevel(), _resources.def_file, False);
        printf("Loading resources from file: %s\n", _resources.def_file );
   }


   // --------- help ------------
   if (_resources.showhelp) showHelp();

   // --------- icon  ------------
 //_icon= XCreateBitmapFromData(dpy, RootWindowOfScreen(scr), cbyt_bits,50,50);
 //_mask= XCreateBitmapFromData(dpy, RootWindowOfScreen(scr), cbyt_mask, 50,50);
 //setIcon(CLASS_NAME, _mask, _icon);


}


void CubeSliceApp::showHelp()
{
  puts( "\nCubeSlice command line arguments are:");
  puts( "    -private    : always use private color map");
  puts( "    -cbar       : next argument specifies how many colors");
  puts( "    -helpfile   : next argument specifies the helpfile");
  puts( "    -defaults   : next argument specifies the defaults file");
  puts( "    -nobs       : don't do backing store");
  puts( "    -bs         : do backing store  (the default)");
  puts( "    -filename   : next argument specifies the input file");

  puts( "    -iconic     : startup Csv as icon");
  puts( "    -display    : next argument specifies display");
  puts( "    all other standard Xt arguments");
  usage_exit (0);
}


void CubeSliceApp::doColor()
{
   unsigned long cflags; 
   long visclass;
   Display *dpy= XtDisplay(topWidget());
   long    total_colors;

   // --------- color ------------
   cflags= test_vis(dpy, &visclass, &total_colors );

   if ( cflags & MAY_COL ) {
          Paintset *paintset = PaintsetCollection::fetchExistingByColormap (
            W());
	  if (!paintset->readOnly()) {
            _total_to_alloc=  _resources.num_col_colors +2;
            if (_total_to_alloc > total_colors - 30) {
               _total_to_alloc=  total_colors - 45;
               fprintf(stderr, 
                 "csv: This machine may only use %d colors.\n",
                 _total_to_alloc);
               fprintf(stderr, "csv: Setting cbar colors to %d colors.\n",
                 _total_to_alloc);
	    } // end if
	    /*
	     * Try to alloc number of colors needed for plot +
	     *              two extra that SeisPlot needs    +
	     *              another 20 for colors for the
	     *              appoximate number of colors other
	     *              classes will try to alloc.
	     */
////////////////// new ////////////////
// force to private colormap
/*
	    if (canIAlloc((int)_total_to_alloc+2+20,0)){
               _cando_colors= True;
	    } // end if
	    else {
	      fprintf(stderr, 
                "csv: Attemping to allocate private color table...\n");
*/
////////////////// new ////////////////
	      tryAgainWithPrivate (_total_to_alloc+2+20,0);
	      _cando_colors= isPrivateCmap();
	      if (_cando_colors)
		fprintf(stderr, "csv: Success.\n");
	      else {
		fprintf(stderr, 
		  "csv: Colors could not be allocated, try -private.\n");
                  fprintf(stderr, "csv: exitting.\n");
                  usage_exit (0);
               }
////////////////// new ////////////////
//          } // End else
////////////////// new ////////////////
	  }
	  else {
	    _total_to_alloc=  _resources.num_col_colors +2;
	    assert (canIAlloc((int)_total_to_alloc+2+20,0));
	    _cando_colors   = True; // don't know if it will work yet
	  }
   } // End if
   else {
     fprintf(stderr, "csv: This display does not support enough color.\n");
     fprintf(stderr, "csv: exitting.\n");
     usage_exit (0);
   } // end else
}



void CubeSliceApp::createMainArea()
{
  HelpCtx hctx;
  static char *info_labels[]= { CUBE_VERSION, CUBE_INFO };
  Pixel fgpix, bgpix;
  _instance_cnt++;

   setTitle("csv", False);
   //setIcon("csv", _icon, _mask);  // need to create icon
   setIcon("csv");
   initHelp( _resources.help_file, "Help for Cube Slice Viewer");
   hctx= getHelpCtx();

   _main_form=    new SLForm(mainWindow(),"main_form");
////////////////////////// new //////////////////////////
  bgpix = PaintsetCollection::white (DefaultScreenOfDisplay(XtDisplay(
    topWidget())));
  fgpix = PaintsetCollection::black (DefaultScreenOfDisplay(XtDisplay(
    topWidget())));
/////////////////////////// new //////////////////////////
/* 
   bgpix = WhitePixelOfScreen(XtScreen(topWidget()));
   fgpix = BlackPixelOfScreen(XtScreen(topWidget()));
*/
   _version_info = new VersionInfo(_main_form->W(),CUBE_VERSION_WIDGET,
                                  info_labels, XtNumber(info_labels),
                                  fgpix, bgpix);
   setWorkArea(_main_form);
   _cube_display= new CubeDisplay(_main_form->W(), "acube",
                                  (int) _total_to_alloc+2);
   _cube_display->setMessageWidget(statusWidget() );
   _cube_display->setModeWidget(modeWidget());
   XtVaSetValues(_cube_display->W(), XmNleftAttachment,   XmATTACH_FORM,
                                     XmNrightAttachment,  XmATTACH_FORM,
                                     XmNtopAttachment,    XmATTACH_FORM,
                                     XmNbottomAttachment, XmATTACH_FORM,
                                     NULL);
}



void CubeSliceApp::createPopups()
{

   HelpCtx hctx= getHelpCtx();
   _cube_amp =    new CubeAmplitudeGui(_main_form->W(), "cube_amp", hctx,
                                     _cube_display);
   _cube_section= new CubeSectionGui(_main_form->W(), "cube_section",
                                     hctx, _cube_display, _cube_amp);
   _cube_anno   = new CubeAnnotationGui(_main_form->W(), "cube_anno",
                                     hctx, _cube_display);

   
   _cube_select = new CubeSelect(_main_form->W(), "cube_select",  
                                     hctx, _cube_display, 
                                     _cube_section, _cube_anno,
                                     _cube_amp);
   _cube_table =  new CubeTableShowGui(_main_form->W(), "cube_table", hctx);

   _cube_display->getRandomLinePop()->setPlotParameters(_cube_section);
   _cube_display->getRandomLinePop()->setAnnotationParameters(_cube_anno);

}


void CubeSliceApp::createPullDowns()
{
  // Create Pull Downs - PullPop classes
   _file_pull  = new SLPullPop("file",   NULL, this);
   _option_pull= new SLPullPop("option", NULL, this);
   _zoom_pull  = new SLPullPop("zoom",   NULL, this);
   _tools_pull = new SLPullPop("tools",  NULL, this);
   _window_pull= new SLPullPop("window", NULL, this);
   _custom_pull= new SLPullPop("custom", NULL, this);
   _help_pull=   new SLPullPop("help",   NULL, this);
   _popup_menu=  new SLPullPop("popup",  NULL, this, False);
   _hidden_pull= new SLPullPop("hidden", NULL, this);

 // FILE Pulldown
   _file_pull->setComplexNotify(this);
   _file_pull->addPushUp("addfile",   _cube_select, ADDFILE);
   _file_pull->addPushUp("Change Cube...",_cube_select, CHANGEFILE);
   _file_pull->addPushUp("Delete Cube",_cube_select, DELFILE);
   _file_pull->sensitive(False, CHANGEFILE, -1);   
   _file_pull->sensitive(False, DELFILE, -1);   
   _file_pull->addSep();
   _file_pull->addPush("quit", QUIT);

 // OPTION Pulldown
   _option_pull->setComplexNotify(this);
   _option_pull->addTog("scrlock",     SCRLOCK);
   _option_pull->addTog("overvis",     OVERVIS);
   _option_pull->addTog("sync_slices", SYNC_SLICES);

   _xh_cas = new SLPullPop("cross_hair", _option_pull);
   _xh_cas->addRadio("crosshair_off"  , _XH_OFF  );
   _xh_cas->addRadio("crosshair_small", _XH_SMALL);
   _xh_cas->addRadio("crosshair_big"  , _XH_BIG  );

   switch (_xh_cas->radioValue())
   {
      case _XH_OFF  :  _cube_display->crosshairOff  ()  ;  break;
      default       :  _xh_cas->setRadioValue(_XH_SMALL);  /* no break */
      case _XH_SMALL:  _cube_display->crosshairSmall()  ;  break;
      case _XH_BIG  :  _cube_display->crosshairBig  ()  ;  break;
    }

   _xh_cas->setComplexNotify(this);

   _option_pull->addSep();

   int total_screens= ScreenCount(XtDisplay(topWidget()));
   if (total_screens > 1) {
          char number_str[20];
          _screen_cas= new SLPullPop("New window on screen...", _option_pull);
          _screen_cas->setComplexNotify(this);
          for(int i=0; (i<total_screens); i++) {
                  sprintf(number_str, "%d", i);        
                  _screen_cas->addPush(number_str, i);
          }
                     
   }
   else { // we only have one screen
          _screen_cas= NULL;
          _option_pull->addPush("add_graph",    ADD_GRAPH); 
   } // end else
   _option_pull->addPush("rem_graph",    REM_GRAPH);

 // ZOOM Pulldown
   _zoom_pull->setComplexNotify(this);
   _zoom_pull->addPush("zline",  ZLINE);
   _zoom_pull->addPush("zxline", ZXLINE);
   _zoom_pull->addPush("zts",    ZTS);
   _zoom_pull->addPush("zrl",    ZRL);
   _zoom_pull->addSep();
   _zoom_pull->addPushUp("zop", _cube_display->zoomOpPop());

 // TOOLS Pulldown
   _tools_pull->setComplexNotify(this);
   _tools_pull->addPushUp("control",
                           _cube_display->cubeMovieControl()->getMoviePop());
   _tools_pull->addPushUp("RandomLine", _cube_display->getRandomLinePop());
   _tools_pull->addPushUp("cube_table",  _cube_table);
   _tools_pull->addPushUp("cbar",     _cube_display->cbarPop());
   _tools_pull->addSep();
   _tools_pull->addPushUp("inline_hard", _cube_display->inlineHardcopyPop());
   _tools_pull->addPushUp("xline_hard",_cube_display->crosslineHardcopyPop());
   _tools_pull->addPushUp("ts_hard", _cube_display->timesliceHardcopyPop());
   _tools_pull->addSep();
   //_tools_pull->addPush("control",  CONTROL);


 // WINDOW Pulldown
   _window_pull->setComplexNotify(this);
   _window_pull->addTog("showil",   SHOWIL);
   _window_pull->addTog("showxl",   SHOWXL);
   _window_pull->addTog("showts",   SHOWTS);
   _window_pull->addTog("showwf",   SHOWWF);

 // MODIFY Pulldown
   _custom_pull->setComplexNotify(this);
   _custom_pull->addPush("color",   COLOR);
   _custom_pull->addPushUp("section",   _cube_section);
   _custom_pull->addPushUp("anno",   _cube_anno);
   _custom_pull->addPushUp("amp",   _cube_amp);

 // HELP Pulldown
   _help_pull->setComplexNotify(this);
   _help_pull->addPush("ctx",      CTX);
   _help_pull->addPush("version",  VERSION);
   _help_pull->addPush("overview", OVERVIEW);
   _help_pull->addTog("mousehelp", MOUSEHELP);
   add_cshelpcb(_help_pull->getWidget(CTX),getHelpCtx());

 // Hidden accelerator Pulldown
   _hidden_pull->setComplexNotify(this);
   _hidden_pull->addPush("accel_inline_movie_left",    MOVIE_INLINE_LEFT);
   _hidden_pull->addPush("accel_inline_movie_right",   MOVIE_INLINE_RIGHT);
   _hidden_pull->addPush("accel_crossline_movie_left", MOVIE_CROSSLINE_LEFT);
   _hidden_pull->addPush("accel_crossline_movie_right",MOVIE_CROSSLINE_RIGHT);
   _hidden_pull->addPush("accel_timeslice_movie_left", MOVIE_TIMESLICE_LEFT);
   _hidden_pull->addPush("accel_timeslice_movie_right",MOVIE_TIMESLICE_RIGHT);
   _hidden_pull->addPush("accel_cube_movie_left",      MOVIE_CUBE_LEFT);
   _hidden_pull->addPush("accel_cube_movie_right",     MOVIE_CUBE_RIGHT);
   _hidden_pull->addPush("accel_inline_scan_left",     PREVIOUS_INLINE_READ);
   _hidden_pull->addPush("accel_inline_scan_right",    NEXT_INLINE_READ);
   _hidden_pull->addPush("accel_crossline_scan_left",  PREVIOUS_CROSSLINE_READ);
   _hidden_pull->addPush("accel_crossline_scan_right", NEXT_CROSSLINE_READ);
   _hidden_pull->addPush("accel_timeslice_scan_left",  PREVIOUS_TIMESLICE_READ);
   _hidden_pull->addPush("accel_timeslice_scan_right", NEXT_TIMESLICE_READ);
   _hidden_pull->addPush("accel_inline_dataload",      LOAD_INLINE);
   _hidden_pull->addPush("accel_crossline_dataload",   LOAD_CROSSLINE);
   _hidden_pull->addPush("accel_timeslice_dataload",   LOAD_TIMESLICE);
      
}


void CubeSliceApp::finalInit()
{
   // do backing store stuff here
}
void CubeSliceApp::realizing()
{
  _cube_display->showit();
  _cube_display->lockScrolling(_option_pull->toggleValue(SCRLOCK));
  _cube_display->showOverlay(_option_pull->toggleValue(OVERVIS));
  _cube_display->showInline(_window_pull->toggleValue(SHOWIL));
  _cube_display->showCrossline(_window_pull->toggleValue(SHOWXL));
  _cube_display->showTimeSlice(_window_pull->toggleValue(SHOWTS));
  _cube_display->showWireFrame(_window_pull->toggleValue(SHOWWF));
  _cube_display->setSyncSlices(_option_pull->toggleValue(SYNC_SLICES));
  _hidden_pull->unmapCascade();
  showStatline( _help_pull->toggleValue(MOUSEHELP) );
  if (_resources.dobacking) {
     Cube *cube= _cube_display->currentDisplayedCube();
     cube->inlineSP()->backingStore(True);
     cube->crosslineSP()->backingStore(True);
     cube->timesliceSP()->backingStore(True);
  }
}


void CubeSliceApp::closeMain()
{
  delayDelete();
}




void CubeSliceApp::calledFromFile()
{
  if(_version_info != NULL) {
     delete _version_info;
     _version_info = NULL;
  }

  switch ( _file_pull->lastIdent() ) {
      case ADDFILE:
        _cube_select->changingFile(False);
        break;
      case CHANGEFILE:
        _cube_select->changingFile(True);
        break;
      case DELFILE:
        _cube_select->deleteFile();
        break;
      case QUIT:
                 _exiting= True;
                 delayDelete();
                 break;
  }
}



void CubeSliceApp::calledFromOption()
{
  switch ( _option_pull->lastIdent() ) {
    case SCRLOCK: 
          _cube_display->lockScrolling(_option_pull->toggleValue(SCRLOCK));
          break;
    case OVERVIS: 
          _cube_display->showOverlay(_option_pull->toggleValue(OVERVIS));
          break;
    case SYNC_SLICES: 
          _cube_display->setSyncSlices(_option_pull->toggleValue(SYNC_SLICES));
          break;

    case ADD_GRAPH:    {ShellWatch sw; 
                          new CubeSliceApp(this);
                       }
                       break;
    case REM_GRAPH:    if(_instance_cnt > 1) 
                         delayDelete(); 
                       break;
  } // end switch
}


void CubeSliceApp::calledFromZoom()
{
  switch ( _zoom_pull->lastIdent() ) {
       case ZLINE:
                   _cube_display->zoomUpSeparateWin(Cube::InLine);
                   break;
       case ZXLINE:
                   _cube_display->zoomUpSeparateWin(Cube::CrossLine);
                   break;
       case ZTS:
                   _cube_display->zoomUpSeparateWin(Cube::TimeSlice);
                   break;
       case ZRL:
                   _cube_display->getRandomLinePop()->zoomUpSeparateWin();
                   break;
  } // end switch
}

void CubeSliceApp::calledFromWindow()
{
  switch ( _window_pull->lastIdent() ) {
    case SHOWIL: 
             _cube_display->showInline(_window_pull->toggleValue(SHOWIL));
             break;
    case SHOWXL: 
             _cube_display->showCrossline(_window_pull->toggleValue(SHOWXL));
             break;
    case SHOWTS: 
             _cube_display->showTimeSlice(_window_pull->toggleValue(SHOWTS));
             break;
    case SHOWWF: 
             _cube_display->showWireFrame(_window_pull->toggleValue(SHOWWF));
             break;

  } // end switch
}

void CubeSliceApp::calledFromTools()
{
  //The only reason for this is to pop up a warning if more than one
  //display or cube is in color. Since there is only one color menu
  //the color bar is only accurate for the last display created.
  if(!strcmp("cbar", _tools_pull->lastName()))
     _cube_display->cbarPop();
}

void CubeSliceApp::calledFromCustom()
{
  switch ( _custom_pull->lastIdent() ) {
    case COLOR: 
         CubeMaster::instance()->colorPop(_cube_display)->makeAndManage();
            break;
  }
}


void CubeSliceApp::calledFromAccelerator()
{
 _cube_display->cubeMovieControl()->acceleratorRequest(
                                      (long)_hidden_pull->lastIdent() );
 _cube_display->getRandomLinePop()->acceleratorRequest(
                                      (long)_hidden_pull->lastIdent() );
}


void CubeSliceApp::calledFromHelp()
{
  ErrorHandler eh(W(),"version", True);

  switch ( _help_pull->lastIdent() ) {
    case VERSION:   eh.deliverInformation(_version);                    break;
    case OVERVIEW:  overview_help("help2", getHelpCtx());               break;
    case MOUSEHELP: showStatline( _help_pull->toggleValue(MOUSEHELP) ); break;
  }
}





Boolean CubeSliceApp::notifyComplex(SLDelay *obj, int ident)
{
   if      (obj==_file_pull)   calledFromFile();
   else if (obj==_option_pull) calledFromOption();
   else if (obj==_zoom_pull)   calledFromZoom();
   else if (obj==_window_pull) calledFromWindow();
   else if (obj==_help_pull)   calledFromHelp();
   else if (obj==_tools_pull)  calledFromTools();
   else if (obj==_custom_pull) calledFromCustom();
   else if (obj==_hidden_pull) calledFromAccelerator();
   else if (obj==_screen_cas)  {
                                  ShellWatch sw; 
                                  new CubeSliceApp(this, ident);
                               }
   else if (obj==_xh_cas)
   {
      switch (ident)
      {
         case _XH_OFF  :  _cube_display->crosshairOff  ()  ;  break;
         case _XH_SMALL:  _cube_display->crosshairSmall()  ;  break;
         case _XH_BIG  :  _cube_display->crosshairBig  ()  ;  break;
         default       :  assert(0);
       }
   }
   return True;
}

void CubeSliceApp::cubeIsCurrent(Cube *cube) 
{
  setTitleToFile(cube);
  setDeleteSensitive ();
}

void CubeSliceApp::newCubeCreated(Cube *cube) 
{
  if (_cube_display) {
       if (_cube_display->find(cube)) {
               addCube(cube);
               setTitleToFile(cube);
       }
  }
  else 
       addCube(cube);
}

void CubeSliceApp::postPlot(Cube *cube, int, int, int)
{
  setTitleToFile(cube);
  _file_pull->sensitive (True, CHANGEFILE, -1);
  setDeleteSensitive ();
}

void CubeSliceApp::destroyed (Cube *)
{
  if (_cube_display->count() < 1) {
    setTitleToFile (NULL);
    if (_file_pull != NULL) _file_pull->sensitive (False, CHANGEFILE, -1);
  }
  setDeleteSensitive ();
}

void CubeSliceApp::setDeleteSensitive ()
{
  if (_file_pull == NULL) return;

  if (_cube_display != NULL && _cube_display->firstCube()) {
    _file_pull->sensitive (False, DELFILE, -1);
  }
  else {
    _file_pull->sensitive (True, DELFILE, -1);
  }
}

void  CubeSliceApp::setTitleToFile(Cube *cube)
{
  char tstr[200]= "csv";
  char filename_only[256];
  char junk[256];
  if (cube && strlen(cube->primaryFilename()) > 0) {
         parse_file_(cube->primaryFilename(), filename_only, junk);
         sprintf(tstr, "csv- %s", filename_only );
  }
  setTitle(tstr, False);
  setIcon("csv");
}

