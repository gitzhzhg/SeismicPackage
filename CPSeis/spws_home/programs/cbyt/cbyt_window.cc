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
#include <Xm/Xm.h>
#include "wproc.h"
#include "cbyt_window.hh"
#include "cbyt_icon.h"
#include "sl/sl_pull_pop.hh"
#include "sl/sl_form.hh"
#include "sl/sl_def_app_pop.hh"
#include "sl/error_handler.hh"
#include "sl/shell_mouse_help.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/version_info.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_ascii_viewer.hh"
#include "sl/slp_file_data.hh"
#include "ipc/ll_sd.hh"

#include "sp/seis_plot.hh"
#include "sp/seis_plot_tie.hh"
#include "sp/seis_plot_under.hh"
#include "sp/seis_control.hh"
#include "sp/seis_multiplot_control.hh"
#include "sp/seis_select_ipc_pop.hh"
#include "sp/seis_tie_pop.hh"
#include "sp/seis_under_pop.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_cbar_pop.hh"
#include "sp/seis_info_pop.hh"
#include "sp/seis_zoomop_pop.hh"
#include "sp/seis_control.hh"
#include "sp/seis_inform.hh"
#include "sp/slave_pop.hh"
#include "sp/seis_header_pop.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_xyout_pop.hh"
#include "sp/seis_all_header_pop.hh"
#include "sp/spectra_menu.hh"
#include "sp/seis_lav_pop.hh"

#include "vu/seis_ovjd.hh"
#include "vu/seis_ovjd_pop.hh"
#include "vu/seis_avast.hh"
#include "vu/seis_avast_pop.hh"
#include "vu/header_dump_pop.hh"
#include "pick/seis_shift.hh"
#include "pick/seis_shift_pop.hh"
#include "pick/tp_ref_pop.hh"
#include "pick/tp_gen_pop.hh"
#include "pick/tp_sisc_pop.hh"
#include "pick/tp_fish_pop.hh"
#include "pick/tp_cc3d_pop.hh"
#include "pick/tp_mute_pop.hh"
#include "pick/tp_mute_shift_pop.hh"
#include "pick/tred_table_pop.hh"
//#include "pick/fk_pop.hh"
#include "vu/seis_label.hh"
#include "pick/seis_velocity.hh"
#include "pick/send_pick.hh"
#include "hardcopy/hardcopy_seis_pop.hh"
#include "ipc/ipc_io.hh"
#include "ipc/ipc_constants.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"
#include "sl/colorset_collection.hh"
#include "sl/color_info_set.hh"

#include "initusage.h"

//#include "tmp_work.hh"   /*TEMP*/ 

#define CLASS_NAME "Cbyt"
#define APP_NAME   "cbyt"


enum {ADDFILE,FILEUNDER, TIELINE, TIEUNDER, MULTI, QUIT}; // File   pulldown
enum {XYON, REMTLINE, TLSEPOP, TLSCRLOCK , TLSHOWAMP,    // Option pulldown
      TLRESET, SHOWANNO, SHOWOVER, 
      DRAGUNDER, HIDEUNDER, MULTI_MOVIE_LOCK,
      SLAVE_PLOT, ADD_GRAPH, ADD_BW_GRAPH, REM_GRAPH};   
enum {ZUP, ZUPSEP, ZDOWN, ZORGIN, ZOP};                  // Zoom   pulldown

enum {PLOTHEAD, REMPLOTHEAD, XYOUT, ALLHEAD, HDUMP};     // Header pulldown

enum {CBAR, DOTDMP, DUMPSCR, GINFO, GLOBALS, OVJD, AVAST,  // Tools  pulldown
      SHIFT, HARDCOPY, ADDLABEL, 
      DELALLLAB, SPECTRAMENU, LAV };               
enum {MUTE,MUTE_SHIFT,REF,GEN,SISC,FISH,CC3D,TRED,VEL,
                                             SEND_PICK}; // Pick   pulldown
enum {LINEAR,HYPER };                                    // Velocity cascade
enum {COLOR, SAVDEF, RESDEF, SYSDEF, UNITS };            // Custom pulldown
enum {CTX, OVERVIEW, VERSION, MOUSEHELP};                // Help   pulldown
enum {ZUPp, ZUPSEPp, ZDOWNp, ZORGINp, ADDFILEp};// popup  menu
enum {PICKMENUb, PICKENDb, PICKENDv, PICKENDs, REMTLINEb, 
      PICKENDa, PICK_FLATTEN_VEL, PICK_FLATTEN_PICKS,
      PICK_UNFLATTEN, PICK_SCAN_ACTION};                 // bottom buttons
enum {PICK_NONE, PICK_FIRST_BREAKS, PICK_SNAP};          // bottom button action

static char *plothelp= "mouse*plot.GENERAL:  BTN#1: none, BTN#2: none,\\n\
BTN#3: Popup Menu";

static char *tieplothelp= "mouse*tieplot.GENERAL:  BTN#1: none, BTN#2: none, \
BTN#3: Popup Menu";
 
  
static char *fallback_resources[] = {
#     include "Cbyt.h"
      NULL
};

static char *fetch_proper_help_file()
{
   static char helpfile[120];
   strcpy(helpfile, getenv("CPSEIS_INSTALL_DIR"));
   strcat(helpfile, "/spws_home/app-defaults/Cbyt_help");
   printf("cbyt help file = %s\n", helpfile);
   return helpfile;
}
static char *helpfile = fetch_proper_help_file();

static XtResource res[]= {
      { "graphFont", "GraphFont", XtRString, sizeof(char *),
        XtOffsetOf(cbyt_res,graph_font), XtRString,
        (void*)"-*-*-bold-r-*-*-*-100-*-*-m-*-*-*" },
      { "privateCmap", "PrivateCmap", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cbyt_res,private_cmap), XtRImmediate,
        (XtPointer) False } ,
      { "doBackingStore", "DoBackingStore", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cbyt_res,dobacking), XtRImmediate, (XtPointer) True } ,
      { "frameBuffer", "FrameBuffer", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cbyt_res,frame_buff), XtRImmediate, (XtPointer) True } ,
      { "showHelp", "ShowHelp", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cbyt_res,showhelp), XtRImmediate, (XtPointer) False } ,
      { "wiggleOnly", "WiggleOnly", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cbyt_res,wigonly), XtRImmediate, (XtPointer) False } ,
      { "noUnderlay", "NoUnderlay", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cbyt_res,no_underlay), XtRImmediate, (XtPointer) False } ,
      { "cbarColors", "CbarColors", XtRInt, sizeof(long),
        XtOffsetOf(cbyt_res,num_col_colors), XtRImmediate, (XtPointer) 66 },
      { "helpFile", "HelpFile", XtRString, sizeof(char *),
        XtOffsetOf(cbyt_res,help_file), XtRString,
            (void*)helpfile },
      { "defaultFile", "DefaultFile", XtRString, sizeof(char *),
        XtOffsetOf(cbyt_res,def_file), XtRString, NULL },
      { "vdynamicCmap", "vdynamicCmap", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cbyt_res,vdynamic_cmap), XtRImmediate,
        (XtPointer) False } ,
      { "dataFile", "DataFile", XtRString, sizeof(char *),
        XtOffsetOf(cbyt_res,input_file), XtRString, NULL },
      { "channel", "Channel", XtRString, sizeof(char *),
        XtOffsetOf(cbyt_res,channel), XtRString, NULL },
      { "servers", "Servers", XtRString, sizeof(char *),
        XtOffsetOf(cbyt_res,servers), XtRString, NULL },
};

static XrmOptionDescRec options[] = {
      { "-wo",          "*wiggleOnly",      XrmoptionNoArg,  "True" },
      { "-nounderlay",  "*noUnderlay",      XrmoptionNoArg,  "True" },
      { "-framebuffer", "*frameBuffer",     XrmoptionNoArg,  "True" },
      { "-private",     "*privateCmap",     XrmoptionNoArg,  "True" },
      { "-help",        "*showHelp",        XrmoptionNoArg,  "True" },
      { "-nobs",        "*doBackingStore",  XrmoptionNoArg,  "False"},
      { "-bs",          "*doBackingStore",  XrmoptionNoArg,  "True" },
      { "-cbar",        "*cbarColors",      XrmoptionSepArg, NULL   },
      { "-helpfile",    "*helpFile",        XrmoptionSepArg, NULL   },
      { "-defaults",    "*defaultFile",     XrmoptionSepArg, NULL   },
      { "-vdynamic",    "*vdynamicCmap",    XrmoptionNoArg,  "False" },
      { "-filename",    "*dataFile",        XrmoptionSepArg, NULL   },
      { "-channel",     "*channel",         XrmoptionSepArg, NULL   },
      { "-servers",     "*servers",         XrmoptionSepArg, NULL   },
     };

Boolean    CbytWindow::_static_initialized= False;
int        CbytWindow::_instance_cnt= 0;
CbytWindow *CbytWindow::_mother = NULL;
CbytWindow *CbytWindow::_cbyt_graph[CbytWindow::MAX_WINDOWS];
Pixmap     CbytWindow::_icon= 0;
Pixmap     CbytWindow::_mask= 0;
cbyt_res   CbytWindow::_resources;


class CbytInform : public SeisInform {

private:
     CbytWindow *_cbyt;
public:
     CbytInform( CbytWindow *cbyt, SeisPlot *sp) 
                            : SeisInform(sp), _cbyt(cbyt){};
     virtual void newPlot(SeisPlot *sp);
     virtual void noPlotDisplayed(SeisPlot *sp);
     virtual void addingTie(SeisPlotTie *sp, SeisPlot *tieplot );
     virtual void removingTie(SeisPlotTie *sp, SeisPlot *tieplot);
     virtual void postZoom(SeisPlot *, SeisPlot::ZoomDir );
     virtual void notCurrentInWindow(SeisPlot *);
     //virtual void newSeisPlotCreatedInWindow(SeisPlot *);
};



CbytWindow::CbytWindow (int& argc, char **argv, IpcIO *ipc_io) :
  SLApp (APP_NAME, CLASS_NAME, argc, argv, options, XtNumber(options), False,
    fallback_resources, ipc_io),
  _sp_under          (NULL),
  _tie_sp_under      (NULL),
  _color_pop         (NULL),
  _sp_under_pop      (NULL),
  _tie_under_pop     (NULL),
  _file_pull         (NULL), 
  _option_pull       (NULL),
  _zoom_pull         (NULL),
  _pick_pull         (NULL),
  _custom_pull       (NULL),
  _units_cas         (NULL),
  _help_pull         (NULL),
  _current_pick_pop  (NULL),
  _seis_velocity     (NULL),
  _send_pick         (NULL),
  _screen_cas        (NULL)
{
  if (!_static_initialized) doStatics();  // color, help, icon, resources
  setFallbacks(fallback_resources);
  doColor();
  createMainArea();
  createPopups(NULL);
  createPullDowns();
  finalInit();
  _version= newstr(CBYT_VERSION);
  _scan_action = PICK_NONE;
  _scan_sensitivity = TpPopupBase::SCAN_SENSITIVITY_ALL_OFF;
}


CbytWindow::CbytWindow (CbytWindow *other, Boolean color, int screen) :
  SLApp (XtDisplay(other->W()), CLASS_NAME, APP_NAME, screen,
    False, 0, fallback_resources, other->ipcIO()),
  _sp_under          (NULL),
  _tie_sp_under      (NULL),
  _color_pop         (NULL),
  _sp_under_pop      (NULL),
  _tie_under_pop     (NULL),
  _file_pull         (NULL),
  _option_pull       (NULL),
  _zoom_pull         (NULL),
  _pick_pull         (NULL),
  _custom_pull       (NULL),
  _units_cas         (NULL),
  _help_pull         (NULL),
  _current_pick_pop  (NULL),
  _seis_velocity     (NULL),
  _send_pick         (NULL),
  _screen_cas        (NULL)
{
  if (!_static_initialized) doStatics();  // color, help, icon, resources
  if (!color) _resources.wigonly= True;
  doColor();
  createMainArea();
  createPopups(other);
  createPullDowns();
  finalInit();
  realize();
  _scan_action = PICK_NONE;
}

CbytWindow::~CbytWindow()
{

//The following must be deleted in order to write out any potential files.
//Typically the deleteAndExit routine will be called and therefore
//the remaining deletes will not execute.
  _instance_cnt--;

 if ((_exiting) || (_instance_cnt==0)) {
   //usage_exit (0);
  }

  delete _seis_shift;
  delete _seis_shift_pop;
  if(_mute_pop) delete _mute_pop;
  if(_mute_shift_pop) delete _mute_shift_pop;
  if(_ref_pop) delete _ref_pop;
  if(_gen_pop) delete _gen_pop;
  if(_sisc_pop)delete _sisc_pop;
  if(_fish_pop)delete _fish_pop;
  if(_cc3d_pop)delete _cc3d_pop;
  if(_tred_pop)delete _tred_pop;
  if(_seis_velocity) delete _seis_velocity;
  if(_send_pick) delete _send_pick;
  delete _sm1;
  delete _sm2;
  delete _inform;
  if (_tie_sp_under)  delete _tie_sp_under;
  if (_sp_under)      delete _sp_under;
  delete _label;
  delete _tie_sp;
/*
  if (_color_pop)     delete _color_pop;
*/
  delete _sp;
  delete _header_graph;
  delete _control_area;
  delete _ovjd;
  delete _ovjd_pop;
  if (_avast) delete _avast;
  if (_avast_pop) delete _avast_pop;
  delete _header_dump_pop;
  delete _header_graph_pop;
  delete _global_data;
  delete _global_pop;
  delete _hard_data;
  delete _hard_pop;

  /* After removing Trey Roby's widget and using SLpFile instead,
     The following delete's cause exit errors. Indicating things are
     being deleted out of order or redundantly. So I simply do not delete
     them. Since usually, the application is exiting at this time anyway. KCC
  delete _sp_pop;
  if (_sp_under_pop)  delete _sp_under_pop;
  delete _tie_pop;
  if (_tie_under_pop) delete _tie_under_pop;
  */

  delete _zoom_op_pop;
  delete _slave_pop;
  delete _plot_info;
  delete _cbar;
  delete _save_def_data;
/*
  delete _savdefpop;
*/
  delete _get_def_data;
  delete _getdefpop;
  if (_version_info) delete _version_info;
  if (_screen_cas) delete _screen_cas;
  delete _units_cas;
  delete _vel_cas;
  delete _file_pull;
  delete _option_pull;
  delete _zoom_pull;
  delete _head_pull;
  delete _tool_pull;
  delete _pick_pull;
  delete _custom_pull;
  delete _help_pull;
  delete _main_form;
  delete _popup_menu;
  delete _spectra_menu;
  delete _lav_pop;

/* The following calls are problematic when _instance_cnt > 1
  ColorsetCollection:: remove ();
  ColorInfoCollection::remove ();
*/


 if ( (_exiting) || (_instance_cnt==0) ) {
          ColorsetCollection:: remove ();
          ColorInfoCollection::remove ();
          _slave_list->deleteAndExit();
  }
  else
          delete _slave_list;
}


void CbytWindow::doStatics()
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
   if (_resources.showhelp) {
         showHelp();
   } 

   // --------- icon  ------------
  _icon= XCreateBitmapFromData(dpy, RootWindowOfScreen(scr), cbyt_bits,50,50);
  _mask= XCreateBitmapFromData(dpy, RootWindowOfScreen(scr), cbyt_mask, 50,50);
  //setIcon(CLASS_NAME, _icon, _mask);
  setIcon(CLASS_NAME, _mask, _icon);


}



void CbytWindow::doColor ()
{
   unsigned long cflags; 
   long visclass;
   Display *dpy= XtDisplay(topWidget());
   long    total_colors;

   // --------- color ------------
   cflags= test_vis(dpy, &visclass, &total_colors );
   // _resources.wigonly = 1; //wmm added this 
   if (_resources.wigonly) {
        _cando_colors  = False;
        _cando_underlay= False;
        fprintf(stderr, "Cbyt: User requested only Black and White plots.\n");
        fprintf(stderr, "Cbyt: Underlay and Color Disabled.\n");
   } // end if
   else {
      if (cflags & MAY_COL) {
	Paintset *paintset = PaintsetCollection::fetchExistingByColormap (
          W());
	if (!paintset->readOnly()) {
          _total_to_alloc=  _resources.num_col_colors +2;
          if (((_total_to_alloc*2) < (total_colors -30))
            && (!_resources.no_underlay))
	    _cando_underlay= True;
          else {
	    _cando_underlay= False;
	    if (((_total_to_alloc*2) > (total_colors -30)) 
              && (!_resources.no_underlay) ) {
	      fprintf (stderr, 
                "Cbyt: Cannot do underlay with this many colors.\n");
	    } // end if
	    fprintf (stderr, "Cbyt: Underlay disabled.\n");
	    if (_total_to_alloc > total_colors - 30) {
              _total_to_alloc=  total_colors - 45;
	      fprintf (stderr, 
                "Cbyt: This machine may only use %d colors.\n",
                _total_to_alloc);
	      fprintf (stderr, "Cbyt: Setting cbar colors to %d colors.\n",
                _total_to_alloc);
	    } // end if
          } // end else
          /*
           * Try to alloc number of colors needed for plot +
           *              two extra that SeisPlot needs    +
           *              another 20 for colors for the
           *              appoximate number of colors other
           *              classes will try to alloc.
           */
//////////////////// new /////////////////////
	  // force to private colormap
/*
	  if (canIAlloc((int)_total_to_alloc+2+20,(_cando_underlay ? 1 :
            0)) && !_resources.private_cmap ) {
            _cando_colors= True;
          } // end if
          else {
            fprintf (stderr, 
              "Cbyt: Attempting to allocate private color table...\n");
*/
//////////////////// new /////////////////////
            tryAgainWithPrivate (_total_to_alloc+2+20,(_cando_underlay ? 1
              : 0));
            _cando_colors= isPrivateCmap ();
            if (_cando_colors)
              fprintf (stderr, "Cbyt: Success.\n");
            else {
              fprintf (stderr, 
                "Cbyt: Colors could not be allocated, try -private.\n");
              fprintf(stderr, 
                "Cbyt: You will only be able to do Black & White plots.\n");
            }
//////////////////// new /////////////////////
//        } // End else
//////////////////// new /////////////////////
	}
	else {
          _total_to_alloc=  _resources.num_col_colors +2;
	  PaintsetCollection::fetchByNumColors (W(),
	    _total_to_alloc+2+10, 1);
	  _cando_colors   = True; // don't know if it will work yet
	  _cando_underlay = True; // don't know what will happen yet
          // _cando_colors   = False; // wmm added
	  // _cando_underlay = False; // wmm added
	}
      } // End if
      else {
           fprintf(stderr, "Cbyt: This machine will only support\n");
           fprintf(stderr, "Cbyt: Black and White plots.\n");
           fprintf(stderr, "Cbyt: Underlay and Color Disabled.\n");
           _cando_colors  = False;
           _cando_underlay= False;
      } // end if
   } // end else
}

void CbytWindow::createPullDowns()
{
  // Create Pull Downs - PullPop classes
       _file_pull  = new SLPullPop("filecas",   NULL, this);
       _option_pull= new SLPullPop("optioncas", NULL, this);
       _zoom_pull  = new SLPullPop("zoomcas",   NULL, this);
       _head_pull  = new SLPullPop("headcas",   NULL, this);
       _tool_pull  = new SLPullPop("toolcas",   NULL, this);
       _pick_pull  = new SLPullPop("pickcas",   NULL, this);
       _custom_pull= new SLPullPop("customcas", NULL, this);
       _help_pull=   new SLPullPop("help",      NULL, this);
       _popup_menu=  new SLPullPop("popup",     NULL, this, False);

  // FILE Pulldown
       _file_pull->setComplexNotify(this);
       _file_pull->addPushUp("addfile",   _sp_pop, ADDFILE);
       if (_cando_underlay)  _file_pull->addPushUp("fileunder", 
                                                   _sp_under_pop, FILEUNDER);
       else                  _file_pull->addPush("fileunder",   FILEUNDER);
       _file_pull->addSep();
       _file_pull->addPushUp("tieline",   _tie_pop, TIELINE);
       if (_cando_underlay)  _file_pull->addPushUp("tieunder",  
                                           _tie_under_pop, TIEUNDER);
       else                  _file_pull->addPush("tieunder",   TIEUNDER);
       _file_pull->addSep();
       _file_pull->addPushUp("multifile",   _multi_file, MULTI),
       _file_pull->addSep();
       _file_pull->addPush("quit",      QUIT);

       if (!_cando_underlay) {
             XtUnmanageChild( _file_pull->getWidget(FILEUNDER) );
             XtUnmanageChild( _file_pull->getWidget(TIEUNDER) );
       }

  // OPTION Pulldown
       _option_pull->setComplexNotify(this);
       _option_pull->addTog("showanno",         SHOWANNO);
       _option_pull->addTog("showover",         SHOWOVER);
       _option_pull->addTog("multi_movie_lock", MULTI_MOVIE_LOCK);
       _option_pull->addSep();
       _option_pull->addPush("remtline",     REMTLINE);
       _option_pull->addPush("tlreset",      TLRESET);
       _option_pull->addTog("tlsepop",       TLSEPOP);
       _option_pull->addTog("tlscrlock",     TLSCRLOCK);
       _option_pull->addTog("tlshowamp",     TLSHOWAMP);
       _option_pull->addSep();
       _option_pull->addPushUp("slave_plot", _slave_pop, SLAVE_PLOT);

       int total_screens= ScreenCount(XtDisplay(topWidget()));
       if (total_screens > 1) {
              char number_str[20];
              _screen_cas= new SLPullPop("screens", _option_pull);
              _screen_cas->setComplexNotify(this);
              for(int i=0; (i<total_screens); i++) {
                      sprintf(number_str, "%d", i);        
                      _screen_cas->addPush(number_str, i);
              }
                         
       }
       else { // we only have one screen
           if (_cando_underlay) {  // put this in only if we support underlays
              _option_pull->addPush("add_graph",    ADD_GRAPH);
           } // end if
       } // end else
        _option_pull->addPush("add_bw_graph", ADD_BW_GRAPH);
        _option_pull->addPush("rem_graph",    REM_GRAPH);

       if (_slave_list->checkError() == HandlesErrors::_ERROR_NONE) {
          _sp->setSlaveList(_slave_list);
          if (_sp_under) _sp_under->setSlaveList(_slave_list);
       }
       else {
          printf(
             "Cbyt: slave_dpy not found: remote display will be disabled.\n");
          _option_pull->sensitive(False, SLAVE_PLOT, -1);
       }

  // ZOOM Pulldown
       _zoom_pull->setComplexNotify(this);
       _zoom_pull->addPush("zup",    ZUP);
       _zoom_pull->addPush("zupsep", ZUPSEP);
       _zoom_pull->addPush("zdown",  ZDOWN);
       _zoom_pull->addPush("zorgin", ZORGIN);
       _zoom_pull->addSep();
       _zoom_pull->addPushUp("zop", _zoom_op_pop, ZOP);

  // HEADER Pulldown
       _head_pull->setComplexNotify(this);
       _head_pull->addPushUp("xyout", _xyout_pop, XYOUT);
       _head_pull->addPushUp("allhead", _all_headers_pop, ALLHEAD);
       _head_pull->addSep();
       _head_pull->addPushUp("header_dump",   _header_dump_pop, HDUMP);
       _head_pull->addSep();
       _head_pull->addPushUp("plothead", _header_graph_pop, PLOTHEAD);
       _head_pull->addPush("remplothead", REMPLOTHEAD);

  // TOOLS Pulldown
       _tool_pull->setComplexNotify(this);
       _tool_pull->addPushUp("cbar",   _cbar,        CBAR);
       _tool_pull->addPushUp("ginfo",  _plot_info,   GINFO);
       _tool_pull->addPushUp("ovjd",   _ovjd_pop,    OVJD);
       _tool_pull->addPushUp("avast",   _avast_pop,    AVAST);
       _tool_pull->addPushUp("shift",   _seis_shift_pop, SHIFT);
       _tool_pull->addPushUp("globals", _global_pop,     GLOBALS);
       _tool_pull->addPushUp("hard_pop", _hard_pop,      HARDCOPY);
       _tool_pull->addPush("addlab",    ADDLABEL);
       _tool_pull->addPush("delalllab", DELALLLAB);
       _tool_pull->addPush("Spectra Analysis...",
                           SPECTRAMENU);
       _tool_pull->addPushUp ("Find LAV...", _lav_pop, LAV);

  // PICK Pulldown
       _pick_pull->setComplexNotify(this);
       _vel_cas = new SLPullPop("vel_estimator", _pick_pull, VEL);
       _vel_cas->setComplexNotify(this);

       _pick_pull->addPush("send_pick", SEND_PICK);
       _pick_pull->addPushUp("Mute Picking...", _mute_pop, MUTE);
       _pick_pull->addPushUp("Mute File Shifting...", _mute_shift_pop,
                             MUTE_SHIFT);
       _pick_pull->addPushUp("SCRS Refraction Statics Picking...",_ref_pop,REF);
       _pick_pull->addPushUp("General Trace Picking...", _gen_pop, GEN);
       _pick_pull->addPushUp("SISC Correlation Picking...", _sisc_pop, SISC);
       _pick_pull->addPushUp("FISH Correlation Picking...", _fish_pop, FISH);
       _pick_pull->addPushUp("CC3D Correlation Picking...", _cc3d_pop, CC3D);
   
       _pick_pull->addPushUp("Trace Editing...",            _tred_pop, TRED);
       //_pick_pull->addPushUp("FK Picking...",               _fk_pop  , FK  );
       _vel_cas->addRadio("Linear",  LINEAR);
       _vel_cas->addRadio("Hyperbolic", HYPER);
        
       _pick_pull->sensitive(False,MUTE,REF,GEN,SISC,FISH,CC3D,TRED,VEL,
         SEND_PICK,-1);
   

  // CUSTOMIZE Pulldown
       _custom_pull->setComplexNotify(this);
       if (_cando_colors) {
             _custom_pull->addPushUp("color",  _color_pop, COLOR);
             _custom_pull->addSep();
       }
       _units_cas= new SLPullPop("units", _custom_pull, UNITS);
       _units_cas->setComplexNotify(this);
       _units_cas->addRadio("metric",  PlotMetric);
       _units_cas->addRadio("english", PlotEnglish);
       _custom_pull->addPushUp("savdef", _savdefpop);
       _custom_pull->addPushUp("resdef", _getdefpop);
       _custom_pull->addPush("sysdef", SYSDEF);

       /*
        * TEMPORARY
        */
       //XtUnmanageChild( _custom_pull->getWidget(RESDEF) );
       XtUnmanageChild( _custom_pull->getWidget(SYSDEF) );

  // HELP Pulldown
       _help_pull->setComplexNotify(this);
       _help_pull->addPush("ctx",      CTX);
       _help_pull->addPush("version",  VERSION);
       _help_pull->addPush("overview", OVERVIEW);
       _help_pull->addTog("mousehelp", MOUSEHELP);
       add_cshelpcb(_help_pull->getWidget(CTX),getHelpCtx());


  // POPUP menu
       _popup_menu->setComplexNotify(this);
       _popup_menu->addPush("zup",    ZUPp);
       _popup_menu->addPush("zupsep", ZUPSEPp);
       _popup_menu->addPush("zdown",  ZDOWNp);
       _popup_menu->addPush("zorgin", ZORGINp);
       _popup_menu->addSep();
       _popup_menu->addPushUp("addfile",   _sp_pop, ADDFILEp);
       _popup_menu->addPushUp("multifile", _multi_file);

       _inform->noPlotDisplayed(_sp);
}


void CbytWindow::createMainArea()
{
  HelpCtx hctx;
  static char *info_labels[]= { CBYT_VERSION, CBYT_INFO };
  Pixel fgpix, bgpix;
  if (_instance_cnt == 0) _mother = this;
  _instance_cnt++;

   setTitle("Cbyt", False);
   setIcon("Cbyt", _icon, _mask);
   initHelp( _resources.help_file, "Help for Cbyt");
   hctx= getHelpCtx();

  _main_form=    new SLForm(mainWindow(),"main_form");
////////////////////////// new //////////////////////////
  bgpix = PaintsetCollection::white (DefaultScreenOfDisplay(XtDisplay(
    topWidget())));
  fgpix = PaintsetCollection::black (DefaultScreenOfDisplay(XtDisplay(
    topWidget())));
/////////////////////////// new //////////////////////////
/* 
  bgpix = WhitePixelOfScreen(DefaultScreenOfDisplay((XtDisplay(topWidget()))));
  fgpix = BlackPixelOfScreen(DefaultScreenOfDisplay((XtDisplay(topWidget()))));
*/
  _version_info = new VersionInfo(_main_form->W(),CBYT_VERSION_WIDGET,
                                  info_labels, XtNumber(info_labels),
                                  fgpix, bgpix);
  setWorkArea(_main_form);
  _sp=           new SeisPlotTie(_main_form->W(), "plot");
  if (_cando_underlay) _sp_under=     new SeisPlotUnder(_sp);
  _tie_sp=       new SeisPlot(_sp->W(), "tieplot");
  if (_cando_underlay) _tie_sp_under= new SeisPlotUnder(_tie_sp);
  if (_resources.input_file) {
        _sp->setFilename( _resources.input_file);
        _tie_sp->setFilename( _resources.input_file);
  }

/*TEMP*///  new TmpWork(_sp);


  _header_graph= new SeisPlot(_sp->W(), "header_graph");



  _sp->setHelpCtx(hctx);
  if (_sp_under)     _sp_under->setHelpCtx(hctx);
  _tie_sp->setHelpCtx(hctx);
  if (_tie_sp_under) _tie_sp_under->setHelpCtx(hctx);
  _header_graph->setHelpCtx(hctx);

  if (_cando_colors) _sp->setLoadableColors((int)_total_to_alloc);

  _control_area= new SeisControl(_main_form->W(), "control_area", _sp, hctx);
  _control_area->setAltPushAction(calledFromBottomStat, this);
  if (_sp_under)     _control_area->addControl(_sp_under);
  _control_area->addControl(_tie_sp);
  if (_tie_sp_under) _control_area->addControl(_tie_sp_under);
  _control_area->addXYControlOnly(_header_graph);

  _sp->setMessageWidget(statusWidget() );
  if (_sp_under) _sp_under->setMessageWidget(statusWidget() );
  _tie_sp->setMessageWidget(statusWidget() );
  if (_tie_sp_under) _tie_sp_under->setMessageWidget(statusWidget() );
  _header_graph->setMessageWidget(statusWidget() );

  _sp->setModeWidget(modeWidget(), "Mode: Plot Primary");
  if (_sp_under) _sp_under->setModeWidget(modeWidget(), "Mode: Plot Underlay");
  _tie_sp->setModeWidget(modeWidget(), "Mode: Plot Tieline");
  if (_tie_sp_under) _tie_sp_under->setModeWidget(modeWidget(), 
                                                  "Mode: Plot Tie Underlay");
  _header_graph->setModeWidget(modeWidget(), "Mode: Plot Header Word");

  XtVaSetValues(_sp->W(),           XmNleftAttachment,   XmATTACH_FORM,
                                    XmNrightAttachment,  XmATTACH_FORM,
                                    XmNtopAttachment,    XmATTACH_FORM,
                                    XmNbottomAttachment, XmATTACH_WIDGET,
                                    XmNbottomWidget,     _control_area->W(),
                                    NULL);

  XtVaSetValues(_control_area->W(), XmNleftAttachment,   XmATTACH_FORM,
                                    XmNrightAttachment,  XmATTACH_FORM,
                                    XmNbottomAttachment, XmATTACH_FORM, 
                                    XmNtopAttachment,    XmATTACH_NONE, NULL);

  if (XtWindow(_sp->W())) _sp->showit();

  _slave_list=new SlaveDisplayLinkedList(_sp->imageGraphic(),
                                        (int)_total_to_alloc, 
                                         _cando_underlay ? 1 : 0, "Cbyt");

  _label=  new SeisLabel(_sp, hctx);
  _label->addPlot(_tie_sp);

  _inform= new CbytInform(this,_sp);
  if (_sp_under) _inform->addSeisPlot(_sp_under);
  _inform->addSeisPlot(_tie_sp);
  if (_tie_sp_under) _inform->addSeisPlot(_tie_sp_under);

  _sm1= new ShellMouseHelp( _sp->imageGraphic(), "GENERAL", plothelp);
  _sm2= new ShellMouseHelp(_tie_sp->imageGraphic(), "GENERAL", tieplothelp);

}


void CbytWindow::createPopups(CbytWindow *other)
{
 HelpCtx hctx= getHelpCtx();




 //================= Use this code to allow the new trace selector option
 if (other)
    _sp_pop=  new SeisSelectIpcPop(_main_form->W(), "sp_pop", hctx, 
      _sp, other->ipcIO(), other->_sp, True);
 else
    _sp_pop=  new SeisSelectIpcPop(_main_form->W(),"sp_pop",hctx,
      _sp, ipcIO(), NULL, True);
 if (_cando_underlay) _sp_under_pop= new SeisUnderPop(_main_form->W(), 
                                                      "sp_under_pop", 
                                                      hctx, _sp_under,
                                                      _sp, True);
 _tie_pop  = new SeisTiePop(_main_form->W(), "tie_input", hctx, _tie_sp, 
                            _sp, True);
 if (_cando_underlay) _tie_under_pop= new SeisUnderPop(_main_form->W(), 
                                                       "tie_under_pop", 
                                                        hctx, _tie_sp_under, 
                                                        _tie_sp, True);
 //================== End code that uses the trace selector 


 /*====================== Use this code to disable the trace selector 
 if (other)
    _sp_pop=  new SeisSelectPop(_main_form->W(), "sp_pop", hctx, 
                                _sp, other->ipcIO(), other->_sp, False);
 else
    _sp_pop=  new SeisSelectPop(_main_form->W(),"sp_pop",hctx, _sp, ipcIO(),
      NULL,False);
 if (_cando_underlay) _sp_under_pop= new SeisUnderPop(_main_form->W(), 
                                                      "sp_under_pop", 
                                                      hctx, _sp_under,
                                                      _sp, False);
 _tie_pop  = new SeisTiePop(_main_form->W(), "tie_input", hctx, _tie_sp, 
                            _sp, False);
 if (_cando_underlay) _tie_under_pop= new SeisUnderPop(_main_form->W(), 
                                                       "tie_under_pop", 
                                                        hctx, _tie_sp_under, 
                                                        _tie_sp, False);


 ====================== End code that disables trace selector*/


 _zoom_op_pop= new SeisZoomOpPop(mainWindow(), "zoomop", hctx, _sp);
 if (_sp_under) _zoom_op_pop->addControl( _sp_under);
 _zoom_op_pop->addControl( _tie_sp);
 if (_tie_sp_under) _zoom_op_pop->addControl( _tie_sp_under);

 _lav_pop = new SeisLavPop (mainWindow(), "lav", hctx);
                    _lav_pop->add (_sp,           SeisLavPop::MAIN);
 if (_sp_under)     _lav_pop->add (_sp_under,     SeisLavPop::MAIN_UNDER);
                    _lav_pop->add (_tie_sp,       SeisLavPop::TIE);
 if (_tie_sp_under) _lav_pop->add (_tie_sp_under, SeisLavPop::TIE_UNDER);

 if (_cando_colors) {
       _color_pop= new SeisColorPop(mainWindow(), "colorpop", _sp, hctx);
       if (_sp_under) _color_pop->addSP( _sp_under);
       _color_pop->addSP( _tie_sp);
       if (_tie_sp_under) _color_pop->addSP( _tie_sp_under);
       if (_tie_under_pop) _tie_under_pop->setColorPop(_color_pop);
       if (_sp_under_pop) _sp_under_pop->setColorPop(_color_pop);
 }
 else {
       _sp_pop->colorOpEnabled(False);
       _tie_pop->colorOpEnabled(False);
 }
 _sp_pop->setColorPop(_color_pop);
 _tie_pop->setColorPop(_color_pop);

 _ovjd = new SeisOvjd(_sp);
 _ovjd_pop= new SeisOvjdPop(mainWindow(),"Offset Velocity", _ovjd, hctx);

 _avast = new SeisAvast(_sp);
 _avast_pop= new SeisAvastPop(mainWindow(),"AVA_Angle", _sp, _avast, hctx);
 _avast_pop->setComplexIdent(AVAST);
 _avast_pop->setComplexNotify(this);

 _header_graph_pop= new SeisHeaderPop(mainWindow(), "header_graph_pop", 
                                      _header_graph, _sp, hctx);
 _header_dump_pop=  new HeaderDumpPop(this, "header_dump", _sp, hctx);

 _seis_shift = new SeisShift(_sp);
 _seis_shift_pop = new SeisShiftPop(mainWindow(), "Seismic Shift", _sp,
                                    _seis_shift, hctx);

 _global_data = new SLpFileData ("global_file", (long)0, "Filename...",
			     "ASCII File", "trc*", SLpFile::_INPUT);
 _global_pop = new SLAsciiViewer(mainWindow(),"Global File Viewer", hctx,
                                 _global_data, True, False, _sp);

 _hard_data = new SLpFileData ("hardcopy_file", (long)0, "Plot File:",
			   "Plot File", "cgm", SLpFile::_OUTPUT);
 _hard_pop= new HardCopySeisPop(mainWindow(), "hardcopy_pop", hctx, _sp,
    _hard_data);

 _cbar= new SeisCbarPop(mainWindow(), "cbar", _sp, hctx);
 if (_sp_under) _cbar->addSP(_sp_under);
 _cbar->addSP(_tie_sp);
 if (_tie_sp_under) _cbar->addSP(_tie_sp_under);

 _multi_file= new SeisMultiPlotControl(mainWindow(), "multifile_pop", hctx,
                             _sp, _tie_sp,_sp_pop, _tie_pop,_color_pop,
                             _zoom_op_pop, _cbar, _ovjd, _lav_pop);

 _xyout_pop= new SeisXYOutPop(mainWindow(), "xyout_pop", hctx, _sp);
 _xyout_pop->addControl(_tie_sp);

 _all_headers_pop= new SeisAllHeaderPop(mainWindow(), "all_headers_pop", 
                                         hctx,_sp);
 _all_headers_pop->addControl(_tie_sp);

  _mute_pop = new TpMutePop(this, "Mute Picking",                  hctx, _sp);
  _mute_shift_pop= new TpMuteShiftPop(mainWindow(),"Mute File Shifting",hctx);
  _ref_pop =  new TpRefPop (this,"SCRS Refracion Statics Picking", hctx, _sp);
  _ref_pop->setPickingExternalFunction((PickingExternalFunction)pickingUpdate,
                                                                   this     );
  _gen_pop =  new TpGenPop (this,"General Trace Picking",          hctx, _sp);
  _gen_pop->setPickingExternalFunction((PickingExternalFunction)pickingUpdate,
                                                                   this     );
  _sisc_pop = new TpSiscPop(this,"SISC Correlation Picking",       hctx, _sp);
  _fish_pop = new TpFishPop(this,"FISH Correlation Picking",       hctx, _sp);
  _cc3d_pop = new TpCc3dPop(this,"CC3D Correlation Picking",       hctx, _sp);
   
  _tred_pop = new TredTablePop(this,"trace_edit",                  hctx, _sp);
  //_fk_pop   = new FkPop       (this, "FK Picking",                 hctx, _sp);

  _mute_pop->setSeisShift(_seis_shift);
  _ref_pop->setSeisShift (_seis_shift); 
  _gen_pop->setSeisShift (_seis_shift);
  _sisc_pop->setSeisShift(_seis_shift);
  _fish_pop->setSeisShift(_seis_shift);
  _cc3d_pop->setSeisShift(_seis_shift);
  _mute_pop->setComplexIdent(MUTE);
  _ref_pop->setComplexIdent (REF);
  _gen_pop->setComplexIdent (GEN);
  _sisc_pop->setComplexIdent(SISC);
  _fish_pop->setComplexIdent(FISH);
  _cc3d_pop->setComplexIdent(CC3D);
   
  _tred_pop->setComplexIdent(TRED);
  //_fk_pop  ->setComplexIdent(FK  );

 _plot_info= new SeisInfoPop(mainWindow(), "info", _sp, hctx);

 _slave_pop= new SlavePop(_main_form->W(), "slave", hctx, _slave_list, False);

  
 _get_def_data = new SLpFileData ("getdef_file", (long)0, "Defaults File: ",
			      "Restore Defaults file", "ad", SLpFile::_INPUT);
 _getdefpop= new SLDefAppPop(mainWindow(), "getdef", hctx, 
			     _get_def_data, this);

 _save_def_data = new SLpFileData ("savedef_file", (long)0, "Defaults File: ",
			       "Save Defaults File", "ad", SLpFile::_OUTPUT);
 _savdefpop= new SLDefAppPop(mainWindow(), "savdef", hctx, 
			     _save_def_data, this);

 _spectra_menu = new SpectraMenu(getWorkArea(), "Spectra Menu", hctx, _sp);


 setMode("Mode: View");
}

void CbytWindow::finalInit()
{
   _sp->showBar( _option_pull->toggleValue(TLSEPOP) );
   _sp->scrollBarsLocked( _option_pull->toggleValue(TLSCRLOCK) );
   showStatline( _help_pull->toggleValue(MOUSEHELP) );
   Boolean s= _option_pull->toggleValue(SHOWANNO);
   _sp->showAnnotation(s); 
   _sp->getSeisWinMan()->setMoviesLocked(
                            _option_pull->toggleValue(MULTI_MOVIE_LOCK));
   if (_sp_under) _sp_under->showAnnotation(s); 
   _tie_sp->showAnnotation(s); 
   if (_tie_sp_under) _tie_sp_under->showAnnotation(s); 
   _control_area->ifTieShowTime( !_option_pull->toggleValue(TLSHOWAMP) );
   calledFromUnits(_units_cas->radioValue());
   if (_option_pull->toggleValue(SHOWOVER)) {
        if (_sp_under)     _sp_under->show(); 
        if (_tie_sp_under) _tie_sp_under->show(); 
   }
   else {
        if (_sp_under)     _sp_under->hide(); 
        if (_tie_sp_under) _tie_sp_under->hide(); 
   }

}

void CbytWindow::realizing()
{
  _sp->showit();
  if (_resources.dobacking) {
       _sp->backingStore(True);
       _tie_sp->backingStore(True);
       if (_cando_underlay) _sp_under->backingStore(True);
       if (_cando_underlay) _tie_sp_under->backingStore(True);
       _header_graph->backingStore(True);
  }

  IpcIO *ipc_io = ipcIO ();
  if (ipc_io) {
    // InterProcess Communication Input/Output can be used
    //   set the designated ID for this application which is CBYT
    if (_resources.channel) {
      // set given serving channel for this application (used as a password)
      ipc_io->setApplAndChan (IPC::CBYT, _resources.channel);
    }
    if (_resources.servers) {
      // if this instance has been given serving hosts to serve it add them
      ipc_io->addServers (_resources.servers);
    }
  }
}

void CbytWindow::closeMain()
{
 delayDelete();
}


void CbytWindow::showHelp()
{
  puts( "\nCbyt command line arguments are:");
  puts( "    -wo         : to only do wiggle traces (no colors allocated)");
  puts( "    -nounderlay : disables underlay plots (less colors allocated)");
  puts( "    -private    : always use private color map");
  puts( "    -cbar       : next argument specifies how many colors");
  puts( "    -helpfile   : next argument specifies the helpfile");
  puts( "    -defaults   : next argument specifies the defaults file");
  puts( "    -vdynamic   : use 24-bit direct instead of static color");
  puts( "    -nobs       : don't do backing store");
  puts( "    -bs         : do backing store  (the default)");
  puts( "    -filename   : next argument specifies the input file");

  puts( "    -iconic     : startup cbyt as icon");
  puts( "    -display    : next argument specifies display");
  puts( "    all other standard Xt arguments");
  //usage_exit (0);
}

void CbytWindow::setWinTitle()
{
  char f1[100], uf1[100], f2[100], uf2[100], outstr[400];
  if (_sp->isPlotDisplayed()) {
        sprintf(outstr, "Cbyt - %s", strip_file(_sp->filename(), f1)); 
  }
  else strcpy(outstr, "Cbyt" );
  setIcon(outstr, _icon, _mask);
  
  if (_sp_under) {
    if (_sp_under->isPlotDisplayed()) {
        strcat(outstr, ", ");
        strcat(outstr, "UNDER - ");
        strcat(outstr, strip_file(_sp_under->filename(), uf1) );
    }
  }

  if (_tie_sp->isPlotDisplayed()) {
      strcat(outstr, ", ");
      strcat(outstr, "TIE - ");
      strcat(outstr, strip_file(_tie_sp->filename(), f2) );
  }

  if (_tie_sp_under) {
    if (_tie_sp_under->isPlotDisplayed()) {
        strcat(outstr, ", ");
        strcat(outstr, "UNDER - ");
        strcat(outstr, strip_file(_tie_sp_under->filename(), uf2) );
    }
  }

  setTitle(outstr, False);
}

void CbytWindow::deleteSelf ()
{
  if (this != _mother || _instance_cnt <= 1) {
    delayDelete ();
  }
  else {
    unmanage ();
  }
}


//==================================================================
//======================= Methods for Pulldowns ===================
//==================================================================
void CbytWindow::calledFromFile()
{
  if(_version_info != NULL) {
     delete _version_info; 
     _version_info = NULL;
  }

  switch ( _file_pull->lastIdent() ) {
      case QUIT:
                 _exiting= True;
                 delayDelete();
                 break;
  }
}



void CbytWindow::calledFromOption()
{
  Boolean s;

  ErrorHandler eh= W();
  switch ( _option_pull->lastIdent() ) {
      case SHOWANNO:     s= _option_pull->toggleValue(SHOWANNO);
                         _sp->showAnnotation(s); 
                         if (_sp_under) _sp_under->showAnnotation(s); 
                         _tie_sp->showAnnotation(s); 
                         if (_tie_sp_under) _tie_sp_under->showAnnotation(s); 
                         if (_header_graph) _header_graph->showAnnotation(s); 
                         break;

      case SHOWOVER:     if (_option_pull->toggleValue(SHOWOVER)) {
                              _sp_under->show(); 
                              _tie_sp_under->show(); 
                         }
                         else {
                              _sp_under->hide(); 
                              _tie_sp_under->hide(); 
                         }
                         break;

      case MULTI_MOVIE_LOCK: 
                         _sp->getSeisWinMan()->setMoviesLocked(
                                  _option_pull->toggleValue(MULTI_MOVIE_LOCK));
                        break;
      case TLSEPOP:   _sp->showBar( _option_pull->toggleValue(TLSEPOP) );break;
      case TLSCRLOCK: 
               _sp->scrollBarsLocked( _option_pull->toggleValue(TLSCRLOCK) );
                        break;
      case TLSHOWAMP: 
               _control_area->ifTieShowTime( 
                               !_option_pull->toggleValue(TLSHOWAMP) );
                        break;
      case REMTLINE:     _multi_file->delTie();        break;
      case TLRESET:      _sp->resetTimeOffset();       break;
      case ADD_GRAPH:    {ShellWatch sw; 
                          new CbytWindow(this);
                         }
                        break;
      case ADD_BW_GRAPH: {ShellWatch sw;
                          new CbytWindow(this,False);
                         }
                        break;
      case REM_GRAPH:    deleteSelf(); break;
  }
}


void CbytWindow::calledFromHeader()
{
  switch ( _head_pull->lastIdent() ) {
      case REMPLOTHEAD:  _header_graph_pop->hide(True);    break;


  }



}

void CbytWindow::calledFromZoom()
{
  switch ( _zoom_pull->lastIdent() ) {
      case ZUP:     _sp->zoomUp();
                    //_sp->getSeisWinMan()->lock("Locked for zoom.");
                    break;
      case ZUPSEP:  _sp->zoomUpSeparateWin();      break;
      case ZDOWN:   _sp->zoomDown();               break;
      case ZORGIN:  _sp->originalSize();           break;
  }
}

void CbytWindow::calledFromTools()
{
  switch ( _tool_pull->lastIdent() ) {
      case ADDLABEL:    _label->insertMoveOneLabel();                  break;
      case DELALLLAB:   _label->deleteAllLabels();                     break;
      case SPECTRAMENU: _spectra_menu->manage();                       break;
  }
}

void CbytWindow::calledFromPick()
{
  switch ( _pick_pull->lastIdent() ) {
  case SEND_PICK:
    calledFromSendPick ();
    break;
  }
}

void CbytWindow::calledFromCustom()
{
  switch ( _custom_pull->lastIdent() ) { }
}


void CbytWindow::calledFromVelocity()
{
int hyper = (int)_vel_cas->lastIdent();

  if(_seis_velocity != NULL)
    {
    _seis_velocity->setHyper(hyper);
    return;
    }
  _control_area->addPush( "vel_end", PICKENDv );
  _seis_velocity = new SeisVelocity(_sp,5,"blue",False,hyper,False);
}


void CbytWindow::calledFromSendPick()
{
  _control_area->addPush( "seis_pick_end", PICKENDs );
  _send_pick = new SendPick (this, _sp);
}


void CbytWindow::calledFromUnits(int units)
{
  _sp->setUnits(units); 
  if (_sp_under) _sp_under->setUnits(units); 
  _tie_sp->setUnits(units);
  if (_tie_sp_under) _tie_sp_under->setUnits(units);
}


void CbytWindow::calledFromHelp()
{
  ErrorHandler eh(W(),"version", True);

  switch ( _help_pull->lastIdent() ) {
    case VERSION:   eh.deliverInformation(_version);                    break;
    case OVERVIEW:  overview_help("help2", getHelpCtx());               break;
    case MOUSEHELP: showStatline( _help_pull->toggleValue(MOUSEHELP) ); break;
  }
}

void CbytWindow::calledFromPopup()
{
  switch ( _popup_menu->lastIdent() ) {
      case ZUP:       _sp->zoomUp();                 break;
      case ZUPSEP:    _sp->zoomUpSeparateWin();      break;
      case ZDOWN:     _sp->zoomDown();               break;
      case ZORGIN:    _sp->originalSize();           break;
  }
}

void CbytWindow::calledFromBottom(long ident)
{
  switch ( ident ) {
      case PICKMENUb: 
                       _current_pick_pop->manage();
                       break; 
      case PICKENDb: 
                       _current_pick_pop->stopActivity();
                       break; //stop picking 
      case PICKENDv:   
                       delete _seis_velocity; 
                       _seis_velocity = NULL;          
                       _control_area->delPush(PICKENDv);
                       break;
      case PICKENDs:   
                       delete _send_pick; 
                       _send_pick = NULL;          
                       _control_area->delPush(PICKENDs);
                       break;
      case PICKENDa:  
                       _avast->avastEnd(_sp) ;
                       _control_area->delPush(PICKENDa);
                       break;
      case REMTLINEb:  
                       _multi_file->delTie();
                       break;
      case PICK_FLATTEN_VEL:
                       if(_current_pick_pop == _ref_pop)
                          _ref_pop->flattenToVelocity();
                       else
                          _gen_pop->flattenToVelocity();
                       break;
      case PICK_FLATTEN_PICKS:
                       if(_current_pick_pop == _ref_pop)
                         _ref_pop->flattenPicks();
                       else
                         _gen_pop->flattenPicks();
                       break;
      case PICK_UNFLATTEN:
                       if(_current_pick_pop == _ref_pop)
                         _ref_pop->unflatten();
                       else
                         _gen_pop->unflatten();
                       break;
      case PICK_SCAN_ACTION:
                       setPickingScan();
                       break;
      default:         assert(0);
  }
}


//void CbytWindow::calledFromPicking(void *data, long ident, int picking)
                                  
void CbytWindow::pickAction(long ident, int picking)
{
  if (picking== SLShellContainer::StartingActivity) {  // start picking
       _control_area->addPush( "end_pick", PICKENDb );
       switch ( ident ) {
            case MUTE: 
                   _control_area->addPush( "mute_menu", PICKMENUb );
                   _current_pick_pop= _mute_pop;
                   break;
            case REF:
                 _control_area->addPush( "Flatten Vel",  PICK_FLATTEN_VEL);
                 _control_area->addPush( "Flatten Picks",PICK_FLATTEN_PICKS);
                 _control_area->addPush( "Unflatten",       PICK_UNFLATTEN);
                 _control_area->addPush( "No Scan Action...",
                                                           PICK_SCAN_ACTION);
                 _control_area->addPush( "ref_menu",     PICKMENUb );
                 _current_pick_pop= _ref_pop;
                 break;
            case GEN:
                 _control_area->addPush( "Flatten Vel",  PICK_FLATTEN_VEL);
                 _control_area->addPush( "Flatten Picks",PICK_FLATTEN_PICKS);
                 _control_area->addPush( "Unflatten",    PICK_UNFLATTEN);
                 _control_area->addPush( "No Scan Action...",
                                                           PICK_SCAN_ACTION);
                 _control_area->addPush( "gen_menu", PICKMENUb );
                 _current_pick_pop= _gen_pop;
                 break;
            case SISC: 
                        _control_area->addPush( "sisc_menu", PICKMENUb );
                        _current_pick_pop= _sisc_pop;
                        break;
            case FISH:
                        _control_area->addPush( "fish_menu", PICKMENUb );
                        _current_pick_pop= _fish_pop;
                        break;
            case CC3D:
                        _control_area->addPush( "cc3d_menu", PICKMENUb );
                        _current_pick_pop= _cc3d_pop;
                        break;
            case TRED:
                        _control_area->addPush( "tred_menu", PICKMENUb );
                        _current_pick_pop= _tred_pop;
                        break;
            /*
            case FK:
                        _control_area->addPush( "fk_menu"  , PICKMENUb );
                        _current_pick_pop= _fk_pop;
                        break;
            */
            default:    assert(0);
       } // end switch
  }
  else if (picking== SLShellContainer::PopUpNoActivity) {
       if (!_current_pick_pop) {

            _pick_pull->sensitive(False,MUTE,REF,GEN,SISC,FISH,CC3D,TRED,-1);
            _pick_pull->sensitive(True, (int)ident,-1);
   
       }
  }
  else if ( (picking== SLShellContainer::PopDownNoActivity)     ||
            (picking== SLShellContainer::PopDownEndingActivity) ||
            (picking== SLShellContainer::EndingActivity) ) {

       _pick_pull->sensitive(True,MUTE,REF,GEN,SISC,FISH,CC3D,TRED,-1);
   
       if (_current_pick_pop) {
            _control_area->delPush( PICKMENUb );
            _control_area->delPush( PICKENDb );
            if(_current_pick_pop == _ref_pop || 
               _current_pick_pop == _gen_pop )//get rid of scrs flatten, etc
              {
              _control_area->delPush(PICK_FLATTEN_VEL);
              _control_area->delPush(PICK_FLATTEN_PICKS);
              _control_area->delPush(PICK_UNFLATTEN);
              _control_area->delPush(PICK_SCAN_ACTION);
              }
            _current_pick_pop= NULL;
       }
  }
}
  

//Called by TpPopupBase when a picking option menu has changed
//Currently only SCRS and General trace picking is supported
//This sets the appropriate flatten and picking while scanning button
//options that are visible in the main windows bottom control area.
void CbytWindow::pickingUpdate(int which, void *data)
{
  CbytWindow *obj = (CbytWindow *)data;

  if(obj->_current_pick_pop != obj->_ref_pop &&
     obj->_current_pick_pop != obj->_gen_pop) return;

  switch(which)
    {
    case TpPopupBase::FLATTEN_VELOCITY:
      XtSetSensitive(obj->_control_area->getPush(PICK_UNFLATTEN), True);
      break;
    case TpPopupBase::FLATTEN_PICKS:
      XtSetSensitive(obj->_control_area->getPush(PICK_UNFLATTEN), True);
      break;
    case TpPopupBase::UNFLATTEN:
      XtSetSensitive(obj->_control_area->getPush(PICK_UNFLATTEN), False);
      break;
    case TpPopupBase::SCAN_ACTION_OFF:
      obj->_scan_action = 0;
      wprocShowMsg(obj->_control_area->getPushButton(PICK_SCAN_ACTION), 
                     "No Scan Action...");
      break;
    case TpPopupBase::SCAN_ACTION_PICKS:
      obj->_scan_action = 1;
      wprocShowMsg(obj->_control_area->getPushButton(PICK_SCAN_ACTION),
                     "First Breaks...");
      break;
    case TpPopupBase::SCAN_ACTION_SNAP:
      obj->_scan_action = 2;
      wprocShowMsg(obj->_control_area->getPushButton(PICK_SCAN_ACTION), 
                     "Snap...");
      break;
    case TpPopupBase::SCAN_SENSITIVITY_ALL_OFF:
    case TpPopupBase::SCAN_SENSITIVITY_FIRST_BREAK:
    case TpPopupBase::SCAN_SENSITIVITY_SNAP:
    case TpPopupBase::SCAN_SENSITIVITY_ALL_ON:
      obj->setPickingSensitivity(which);
      break;
    }
}



void CbytWindow::setPickingSensitivity(int which)
{
  switch(which)
    {
    case TpPopupBase::SCAN_SENSITIVITY_ALL_OFF:
      XtSetSensitive(_control_area->getPushButton(PICK_SCAN_ACTION), False);
      _scan_sensitivity = TpPopupBase::SCAN_SENSITIVITY_ALL_OFF;
      break;
    case TpPopupBase::SCAN_SENSITIVITY_FIRST_BREAK:
      XtSetSensitive(_control_area->getPushButton(PICK_SCAN_ACTION), True);
      _scan_sensitivity = TpPopupBase::SCAN_SENSITIVITY_FIRST_BREAK;
      break;
    case TpPopupBase::SCAN_SENSITIVITY_SNAP:
      XtSetSensitive(_control_area->getPushButton(PICK_SCAN_ACTION), True);
      _scan_sensitivity = TpPopupBase::SCAN_SENSITIVITY_SNAP;
      break;
    case TpPopupBase::SCAN_SENSITIVITY_ALL_ON:
      XtSetSensitive(_control_area->getPushButton(PICK_SCAN_ACTION), True);
      _scan_sensitivity = TpPopupBase::SCAN_SENSITIVITY_ALL_ON;
      break;
    }
}


//This is called when the user changes the scanning while picking
//options button in the main window's bottom control area while
//picking SCRS or general trace.
void CbytWindow::setPickingScan()
{

  ++_scan_action;
  if(_scan_action > PICK_SNAP)
     _scan_action = PICK_NONE;

  switch(_scan_sensitivity)
    {
    case TpPopupBase::SCAN_SENSITIVITY_FIRST_BREAK:
      _scan_action = PICK_FIRST_BREAKS;
      break;
    case TpPopupBase::SCAN_SENSITIVITY_SNAP:
      _scan_action = PICK_SNAP;
      break;
    }

  switch(_scan_action)
    {
      case PICK_NONE:
        wprocShowMsg(_control_area->getPushButton(PICK_SCAN_ACTION), 
                     "No Scan Action...");
        if(_current_pick_pop == _ref_pop)
          {
          _ref_pop->setBreakState(False);//Sets toggle on the ref pop
          _ref_pop->setAutoBreak(False);// Tells the pop to turn off 1st break
          _ref_pop->setSnapState(False);// Sets toggle on the ref pop
          _ref_pop->setAutoSnap(False); // Tells the ref pop to turn off snap
          }
        else
          {
          _gen_pop->setBreakState(False);//Sets toggle on the ref pop
          _gen_pop->setAutoBreak(False);// Tells the pop to turn off 1st break
          _gen_pop->setSnapState(False);// Sets toggle on the ref pop
          _gen_pop->setAutoSnap(False); // Tells the ref pop to turn off snap
          }
        break;

      case PICK_FIRST_BREAKS:
        wprocShowMsg(_control_area->getPushButton(PICK_SCAN_ACTION),
                     "First Breaks...");
        if(_current_pick_pop == _ref_pop)
          {
          _ref_pop->setBreakState(True);
          _ref_pop->setAutoBreak(True);
          _ref_pop->setSnapState(False);
          _ref_pop->setAutoSnap(False);
          }
        else
          {
          _gen_pop->setBreakState(True);
          _gen_pop->setAutoBreak(True);
          _gen_pop->setSnapState(False);
          _gen_pop->setAutoSnap(False);
          }
        break;

      case PICK_SNAP:
        wprocShowMsg(_control_area->getPushButton(PICK_SCAN_ACTION), 
                     "Snap...");
        if(_current_pick_pop == _ref_pop)
          {
          _ref_pop->setBreakState(False);
          _ref_pop->setAutoBreak(False);
          _ref_pop->setSnapState(True);
          _ref_pop->setAutoSnap(True);
          }
        else
          {
          _gen_pop->setBreakState(False);
          _gen_pop->setAutoBreak(False);
          _gen_pop->setSnapState(True);
          _gen_pop->setAutoSnap(True);
          }
        break;
    }
}


void CbytWindow::avastAction()
{
  int stat= _avast_pop->getActivityStatus();
  if (stat == SLShellContainer::StartingActivity) {
        if (!_avast->getAvastButtonStatus()) {
              _control_area->addPush( "avast_end", PICKENDa );
              _avast->setAvastButtonStatus(True);
        }
  }
  else if (stat == SLShellContainer::EndingActivity) {
        if (_avast->getAvastButtonStatus()) {
              _control_area->delPush(PICKENDa);
              _avast->setAvastButtonStatus(False);
        }
  }
  else assert(0);
}


Boolean CbytWindow::notifyComplex(SLDelay *obj, int ident)
{
   if      (obj==_file_pull)   calledFromFile();
   else if (obj==_option_pull) calledFromOption();
   else if (obj==_zoom_pull)   calledFromZoom();
   else if (obj==_head_pull)   calledFromHeader();
   else if (obj==_tool_pull)   calledFromTools();
   else if (obj==_pick_pull)   calledFromPick();
   else if (obj==_vel_cas)     calledFromVelocity();
   else if (obj==_custom_pull) calledFromCustom();
   else if (obj==_units_cas)   calledFromUnits( (int)_units_cas->lastIdent());
   else if (obj==_help_pull)   calledFromHelp();
   else if (obj==_popup_menu)  calledFromPopup();
   else if (obj==_screen_cas)  new CbytWindow(this, True, ident);

   else if (obj==_mute_pop)  pickAction(ident, _mute_pop->getActivityStatus());
   else if (obj==_mute_shift_pop) _mute_shift_pop->manage();
   else if (obj==_ref_pop)   pickAction(ident, _ref_pop->getActivityStatus());
   else if (obj==_gen_pop)   pickAction(ident, _gen_pop->getActivityStatus());
   else if (obj==_sisc_pop)  pickAction(ident, _sisc_pop->getActivityStatus());
   else if (obj==_fish_pop)  pickAction(ident, _fish_pop->getActivityStatus());
   else if (obj==_cc3d_pop)  pickAction(ident, _cc3d_pop->getActivityStatus());
   else if (obj==_tred_pop)  pickAction(ident, _tred_pop->getActivityStatus());
   //else if(obj==_fk_pop  )  pickAction(ident, _fk_pop  ->getActivityStatus());
   else if (obj==_avast_pop) avastAction();
   return True;
}

//==================================================================
//=================== Static function for Pulldowns ================
//==================================================================
void CbytWindow::calledFromBottomStat(void *obj,long ident)
     { ((CbytWindow*)obj)->calledFromBottom(ident); }


//==================================================================
//======================= CbytInform Methods =======================
//==================================================================
void CbytInform::newPlot(SeisPlot *sp)
{
 if (sp==_cbyt->_sp) {
    _cbyt->_file_pull->sensitive(True, TIELINE, -1);
    _cbyt->_tool_pull->sensitive(True,CBAR,GINFO,GLOBALS,OVJD,AVAST,SHIFT,
                                 SPECTRAMENU, -1);
    _cbyt->_head_pull->sensitive(True, PLOTHEAD, REMPLOTHEAD,HDUMP, -1);
    _cbyt->_zoom_pull->sensitive(True, ZUP, ZUPSEP, ZDOWN, -1);
    _cbyt->_popup_menu->sensitive(True, ZUPp, ZUPSEPp, ZDOWNp, -1);

    _cbyt->_global_pop->setFilename (_cbyt->_sp->filename());

    if (!_cbyt->_current_pick_pop)
           _cbyt->_pick_pull->sensitive(True,MUTE,REF,GEN,
                                        SISC,FISH,CC3D, TRED,VEL,
                                        SEND_PICK,-1);

    if (_cbyt->_sp->plottedPlotType() == PlotImage::PlotCOLOR)
       _cbyt->_file_pull->sensitive(False, FILEUNDER, -1);
    else
       _cbyt->_file_pull->sensitive(True, FILEUNDER, -1);
 } // end if

 if ( (sp==_cbyt->_sp_under) || (sp==_cbyt->_tie_sp_under) ) {
    _cbyt->_option_pull->sensitive(True, SHOWOVER, -1);
    _cbyt->_option_pull->setToggleValue(SHOWOVER, True);
 } // end if

 if(sp->isZoomed()) {
    _cbyt->_zoom_pull->sensitive(True, ZORGIN, -1);
    _cbyt->_popup_menu->sensitive(True, ZORGINp, -1);
 }
 else {
    _cbyt->_zoom_pull->sensitive(False, ZORGIN, -1);
    _cbyt->_popup_menu->sensitive(False, ZORGINp, -1);
 }

 if(sp->isAnUnderlay()) {
   //
   // does not work in this case--so don't allow it
   //   hypothesis is that PlotImage::memcpyThis doesn't work for
   //   underlays. tried to make a PlotImage copy constructor. first
   //   attempt failed. rather than debug the nightmare, tried a miriad
   //   of other less ambitious approaches to fix memcpyThis. gave up
   //   after a couple of weeks of blind alleys
   //
    _cbyt->_zoom_pull->sensitive(False, ZUPSEP, -1);
    _cbyt->_popup_menu->sensitive(False, ZUPSEPp, -1);
 }
 else {
    _cbyt->_zoom_pull->sensitive(True, ZUPSEP, -1);
    _cbyt->_popup_menu->sensitive(True, ZUPSEPp, -1);
 }

 _cbyt->setWinTitle();

}


void CbytInform::noPlotDisplayed(SeisPlot *sp)
{
 if (sp==_cbyt->_sp) {
    _cbyt->_file_pull->sensitive(False, FILEUNDER, TIELINE, TIEUNDER, -1);
    SeisPlotTie *spt = (SeisPlotTie *)sp;
    if (!spt->doingTie()) {
      _cbyt->_option_pull->sensitive(False, REMTLINE, TLRESET, -1);
    }
    _cbyt->_tool_pull->sensitive(False,CBAR,GINFO,GLOBALS,OVJD,AVAST,SHIFT,
                                 SPECTRAMENU, -1);
    _cbyt->_head_pull->sensitive(False, PLOTHEAD, REMPLOTHEAD, HDUMP,-1);
    if (_cbyt->_control_area->pushExist(REMTLINEb) && !spt->doingTie())
                      _cbyt->_control_area->delPush( REMTLINEb );

    if (!_cbyt->_current_pick_pop)
         _cbyt->_pick_pull->sensitive(False,MUTE,REF,GEN,
                                            SISC,FISH,CC3D, TRED,VEL,
				            SEND_PICK,-1);

    _cbyt->_zoom_pull->sensitive(False, ZUP, ZUPSEP, ZDOWN, ZORGIN, -1);
    _cbyt->_popup_menu->sensitive(False, ZUPp, ZUPSEPp, ZDOWNp, ZORGINp, -1);
 }

 if ( (_cbyt->_sp_under) && (_cbyt->_tie_sp_under) ) {
    if ( (!_cbyt->_sp_under->plotExist())
           && (!_cbyt->_tie_sp_under->plotExist()) )
                   _cbyt->_option_pull->sensitive(False, SHOWOVER, -1);
 }

 _cbyt->setWinTitle();

}


void CbytInform::addingTie(SeisPlotTie * /*sp*/, SeisPlot *tieplot )
{
 _cbyt->_file_pull->sensitive(True, TIEUNDER, -1);
 _cbyt->_option_pull->sensitive(True, REMTLINE, TLRESET, -1);
 if (!_cbyt->_control_area->pushExist(REMTLINEb))
           _cbyt->_control_area->addPush( "remtieline_b", REMTLINEb );

 if (tieplot->plottedPlotType() == PlotImage::PlotCOLOR)
       _cbyt->_file_pull->sensitive(False, TIEUNDER, -1);
 else
       _cbyt->_file_pull->sensitive(True, TIEUNDER, -1);
}


void CbytInform::removingTie(SeisPlotTie * /*sp*/, SeisPlot * /*tieplot*/)
{
 _cbyt->_file_pull->sensitive(False, TIEUNDER, -1);
 _cbyt->_option_pull->sensitive(False, REMTLINE, TLRESET, -1);
 if (_cbyt->_tie_under_pop) _cbyt->_tie_under_pop->unmanage();
 if (_cbyt->_control_area->pushExist(REMTLINEb))
             _cbyt->_control_area->delPush( REMTLINEb );
}


void CbytInform::postZoom(SeisPlot *sp, SeisPlot::ZoomDir)
{
 if(sp->isZoomed()) {
    _cbyt->_zoom_pull->sensitive(True, ZORGIN, -1);
    _cbyt->_popup_menu->sensitive(True, ZORGINp, -1);
 }
 else {
    _cbyt->_zoom_pull->sensitive(False, ZORGIN, -1);
    _cbyt->_popup_menu->sensitive(False, ZORGINp, -1);
 }
}

void CbytInform::notCurrentInWindow(SeisPlot *sp)
{
  int units= sp->units();
  if (sp == _cbyt->_sp) {               // primary sp
        _cbyt->_sp=  (SeisPlotTie *)sp->currentSPInWindow();
        _cbyt->_sp_under=  
                (SeisPlotUnder *)sp->currentSPInWindow()->getChainedSP();
        addSeisPlot(_cbyt->_sp);
        if (_cbyt->_sp_under) {
            addSeisPlot(_cbyt->_sp_under);
            if (_cbyt->_option_pull->toggleValue(SHOWOVER)) 
                     _cbyt->_sp_under->show(); 
            else
                     _cbyt->_sp_under->hide(); 
        } // end if
        if (_cbyt->_sp->isPlotDisplayed()) newPlot(_cbyt->_sp);
        else                               noPlotDisplayed(_cbyt->_sp);
  }
  else if (sp == _cbyt->_tie_sp) {      // tie line sp
        _cbyt->_tie_sp=  sp->currentSPInWindow();
        _cbyt->_tie_sp_under=  
                (SeisPlotUnder *)sp->currentSPInWindow()->getChainedSP();
        addSeisPlot(_cbyt->_tie_sp);
        if (_cbyt->_tie_sp_under) {
            addSeisPlot(_cbyt->_tie_sp_under);
            if (_cbyt->_option_pull->toggleValue(SHOWOVER)) 
                      _cbyt->_tie_sp_under->show(); 
            else          
                      _cbyt->_tie_sp_under->hide(); 
        } // end if
  }
  else
      assert(0);
  _cbyt->setWinTitle();
  _cbyt->_global_pop->setFilename( _cbyt->_sp->filename() );
  _cbyt->calledFromUnits(units);
}

//==================================================================
//======================= End CbytInform Methods ===================
//==================================================================
