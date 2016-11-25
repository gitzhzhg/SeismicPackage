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
#include "cfg_app.hh"
#include "cfg_icon.h"
#include "sl/sl_pull_pop.hh"
#include "sl/shell_mouse_help.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/version_info.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_quest_pop.hh"
#include "sl/sl_form.hh"
#include "sl/sl_def_pop.hh"
#include "sl/error_handler.hh"
#include "sl/container_list.hh"
#include "sl/slp_file_data.hh"

#include "sp/seis_zoomop_pop.hh"

#include "fgxp/fgxp_2d_pop.hh"
#include "fgxp/fgxp_3d_pop.hh"
#include "fg2d/fg2d_pop.hh"

#include "fgqc/fgqc_pop.hh"
#include "fgqc/fgqc_ovjd_plot.hh"

#include "fgmap/fg_map.hh"
#include "fgmap/fg_seis_plot.hh"
#include "fgmap/fg_seis_plot_list.hh"
#include "fgmap/fg_shell_msg.hh"
#include "fgmap/fg_plot_color_pop.hh"
#include "geom/field_geometry.hh"

#include "fggui/fg_watch_inform.hh"
#include "fggui/transform_pop.hh"
#include "fggui/fg_control_pop.hh"

#include "fggui/ld_edit_pop.hh"
#include "fggui/linelist_pop.hh"
#include "fggui/ld_pop.hh"
#include "fggui/rp_pop.hh"
#include "fggui/pp_pop.hh"
#include "fggui/zt_pop.hh"
#include "fggui/fgte_pop.hh"
#include "fggui/groups_pop.hh"
#include "fggui/midpoints_pop.hh"
#include "fggui/header_pop.hh"
#include "fggui/fg_miss_pop.hh"
#include "fggui/jd_read_pop.hh"
#include "fggui/jd_save_pop.hh"
#include "fggui/survey_read_pop.hh"
#include "fggui/survey_save_pop.hh"

#include "geom/fg_headers.hh"

////////////////// new /////////////////////////
#include "sl/paintset_collection.hh"
////////////////// new /////////////////////////

#include "initusage.h"

#define CLASS_NAME "Cfg"
#define APP_NAME   "cfg"

static char *fallback_resources[] = {
#     include "Cfg.h"
      NULL
};



enum {QUIT, ICONALL};       // File   pulldown
enum { SHOWSTAT};          // Option pulldown
enum { INMAIN, MAP, XP2DPLOTS, XP3DPLOTS, QCPLOTS, OVJDPLOTS};// Plots pulldown
//INMAIN Cascade
enum { MAPin, XP2DPLOTSin, XP3DPLOTSin, QCPLOTSin, OVJDPLOTSin, CHARTin};
enum {ZUP, ZUPSEP, ZDOWN, ZORGIN, ZOP};                  // Zoom   pulldown
enum {CTX, OVERVIEW, VERSION, MOUSEHELP};                // Help   pulldown
enum {ZUPp, ZUPSEPp, ZDOWNp, ZORGINp, ADDFILEp };        // popup  menu



/*
static char *fallback_resources[] = {
#     include "cfg_fb.h"
      NULL
};
*/

class CfgGetLoadDefs : public SLDefPop {
   public:
     CfgGetLoadDefs( Widget        p,
                     HelpCtx       hctx,
                     SLpFileData  *slp_file_data,
                     void         *data) :
             SLDefPop(p,"defpop",hctx,slp_file_data,data,False) {};
     virtual void saveFile( char *filename, void *data);
};


static char *fetch_proper_help_file()
{
   static char helpfile[120];
   strcpy(helpfile, getenv("CPSEIS_INSTALL_DIR"));
   strcat(helpfile, "/spws_home/app-defaults/Cfg_help");
   printf("cfg help file = %s\n", helpfile);
   return helpfile;
}
static char *helpfile = fetch_proper_help_file();


static XtResource res[]= {
      { "graphFont", "GraphFont", XtRString, sizeof(char *),
        XtOffsetOf(cfg_res,graph_font), XtRString,
        (void*)"-*-*-bold-r-*-*-*-100-*-*-m-*-*-*" },
      { "privateCmap", "PrivateCmap", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cfg_res,private_cmap), XtRImmediate,
        (XtPointer) False } ,
      { "doBackingStore", "DoBackingStore", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cfg_res,dobacking), XtRImmediate, (XtPointer) True } ,
      { "frameBuffer", "FrameBuffer", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cfg_res,frame_buff), XtRImmediate, (XtPointer) True } ,
      { "showHelp", "ShowHelp", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(cfg_res,showhelp), XtRImmediate, (XtPointer) False } ,
      { "helpFile", "HelpFile", XtRString, sizeof(char *),
        XtOffsetOf(cfg_res,help_file), XtRString,
            (void*)helpfile },
      { "defaultFile", "DefaultFile", XtRString, sizeof(char *),
        XtOffsetOf(cfg_res,def_file), XtRString, NULL },
};

static XrmOptionDescRec options[] = {
      { "-framebuffer", "*frameBuffer",     XrmoptionNoArg,  "True" },
      { "-private",     "*privateCmap",     XrmoptionNoArg,  "True" },
      { "-help",        "*showHelp",        XrmoptionNoArg,  "True" },
      { "-nobs",        "*doBackingStore",  XrmoptionNoArg,  "False"},
      { "-bs",          "*doBackingStore",  XrmoptionNoArg,  "True" },
      { "-cbar",        "*cbarColors",      XrmoptionSepArg, NULL   },
      { "-helpfile",    "*helpFile",        XrmoptionSepArg, NULL   },
      { "-defaults",    "*defaultFile",     XrmoptionSepArg, NULL   },
     };



#define SAVE_MSG "Save changes to geometry?"



CfgApp::CfgApp( int &argc, char **argv) :
              SLApp( APP_NAME, CLASS_NAME, argc, argv, 
                     options, XtNumber(options) ),
          _2dxp_main(NULL), _3dxp_main(NULL), _map_main(NULL),
          _question(NULL), _qcp_main(NULL), _ovjd_main(NULL), _chart_main(NULL),
          _plot_in_main(NULL), _plot_specific_pull(NULL), _fgsp_in_main(NULL),
          _expecting_save_pop(False)
{
  setFallbacks(fallback_resources);
  init();
  createPopups();
  createPullDowns();
  createMainArea();
  finalInit();
}



CfgApp::~CfgApp()
{
   delete _dcp;
   delete _tp;

   delete _messages;
   delete _savdefpop;
   delete _savdefdata;

   if (_version_info) delete _version_info;

   if (_2dxp_main) delete _2dxp_main;
   if (_3dxp_main) delete _3dxp_main;
   if (_qcp_main)  delete _qcp_main;
   if (_ovjd_main) delete _ovjd_main;
   if (_map_main)  delete _map_main;
   if (_chart_main)  delete _chart_main;

   delete _2dxp;
   delete _3dxp;
   delete _qcp;
   delete _ovjd;
   delete _map;
   delete _chart;

   delete _lines;
   delete _ld;
   delete _ld_qc;
   delete _rp;
   delete _pp;
   delete _zt1;
   delete _zt2;
   delete _zt3;
   delete _zt4;
   delete _tred;
   delete _groups;
   delete _midpoint;
   delete _head_pop;
   delete _miss_pop;

   delete _file_pull;
   delete _option_pull;
   delete _table_pull;
   delete _inmain_cas;
   delete _plots_pull;
   delete _help_pull;
   delete _plot_specific_pull;
   free (_version);

   delete _watch_inform;
   delete _main_form;
   delete _fgsp_list;
   delete _all_pops;
   delete _fg;
}

void CfgApp::showHelp()
{
  puts( "\nCfg command line arguments are:");
  puts( "        -helpfile : next argument specifies the helpfile");
  puts( "        -defaults : next argument specifies the defaults file");

  puts( "        -iconic   : startup cbyt as icon");
  puts( "        -display  : next argument specifies display");
  puts( "        all other standard Xt arguments");
  usage_exit (0);
}

void CfgApp::init()
{
   Display *dpy= XtDisplay(topWidget());
   Screen  *scr= XtScreen(topWidget());;

   getResources( &_resources, res, XtNumber(res) );

   // defaults file
   if (_resources.def_file) {
        DefLoadFileRes(topLevel(), _resources.def_file, False);
        printf("Loading resources from file: %s\n", _resources.def_file );
   }

   // --------- help ------------
   if (_resources.showhelp) {
         showHelp();
   }
   initHelp( _resources.help_file, "Help for Cfg");
   _fg= new FieldGeometry();
   _watch_inform= new FgWatchInform(_fg, W(), this);
   _messages= new FgShellMsg(W(), _fg);

  _icon= XCreateBitmapFromData(dpy, RootWindowOfScreen(scr), cfg_bits,50,50);
  _mask= XCreateBitmapFromData(dpy, RootWindowOfScreen(scr), cfg_mask, 50,50);
   setTitle("Cool Field Geometry", False);
   setIcon(CLASS_NAME, _mask, _icon);
  _version= newstr(CFG_VERSION);

}

void CfgApp::createMainArea()
{
  int numcolors = 66;
  static char *info_labels[]= { CFG_VERSION, CFG_INFO };
  Pixel fgpix, bgpix;
////////////////////////// new //////////////////////////
  bgpix = PaintsetCollection::white (DefaultScreenOfDisplay(XtDisplay(
    topWidget())));
  fgpix = PaintsetCollection::black (DefaultScreenOfDisplay(XtDisplay(
    topWidget())));
/////////////////////////// new //////////////////////////
/*
  bgpix= WhitePixelOfScreen(XtScreen(topWidget()));
  fgpix= BlackPixelOfScreen(XtScreen(topWidget()));
*/
  _main_form= new SLForm(W(),"main_form");
  _map_main=  new FgMap(_main_form->W(), _fgsp_list, this);
  setWorkArea(_main_form);
  _version_info = new VersionInfo(_main_form->W(),CFG_VERSION_WIDGET,
                                  info_labels, XtNumber(info_labels),
                                  fgpix, bgpix);
  /*
   * When they are ready create all plots so they can go in main window.
   */
  _2dxp_main= new FgXp2DPop(_main_form, "fgxp_2d_main", getHelpCtx(), _fg,
                            _fgsp_list, 6.0, this);
  _3dxp_main= new FgXp3DPop(_main_form, "fgxp_3d_main", getHelpCtx(), _fg,
                            _fgsp_list, 6.0, this);
  _qcp_main = new FgQcPop(_main_form->W(), "qc_main", getHelpCtx(), 
                          _fg, _fgsp_list, this);
  _ovjd_main= new FgQcOvjdPlot(_main_form->W(), "ovjd_main", getHelpCtx(), 
                               this, _fg, 
                               _fgsp_list, numcolors);
  _chart_main= new Fg2DPop(_main_form, "fg2d_main", getHelpCtx(), _fg,
                            _fgsp_list, 6.0, this);

  XtVaSetValues(_map_main->W(),  XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNtopAttachment,    XmATTACH_FORM,
                                 XmNbottomAttachment, XmATTACH_FORM, NULL);

  XtVaSetValues(_2dxp_main->W(), XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNtopAttachment,    XmATTACH_FORM,
                                 XmNbottomAttachment, XmATTACH_FORM, NULL);

  XtVaSetValues(_3dxp_main->W(), XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNtopAttachment,    XmATTACH_FORM,
                                 XmNbottomAttachment, XmATTACH_FORM, NULL);

  XtVaSetValues(_qcp_main->PW(), XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNtopAttachment,    XmATTACH_FORM,
                                 XmNbottomAttachment, XmATTACH_FORM, NULL);

  XtVaSetValues(_ovjd_main->W(), XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNtopAttachment,    XmATTACH_FORM,
                                 XmNbottomAttachment, XmATTACH_FORM, NULL);

  XtVaSetValues(_chart_main->W(), XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNtopAttachment,    XmATTACH_FORM,
                                 XmNbottomAttachment, XmATTACH_FORM, NULL);

  _map_main->pulldown()->unmanageCascade();
  _2dxp_main->pulldown()->unmanageCascade();
  _3dxp_main->pulldown()->unmanageCascade();
  _qcp_main->pulldown()->unmanageCascade();
  _ovjd_main->pulldown()->unmanageCascade();
  _chart_main->pulldown()->unmanageCascade();
  _map_main->unmanage();
  _2dxp_main->unmanage();
  _3dxp_main->unmanage();
  _qcp_main->unmanage();
  _ovjd_main->unmanage();
  _chart_main->unmanage();



}

void CfgApp::createPopups()
{
  assert(_fg);
  HelpCtx hctx= getHelpCtx();
  int numcolors =66;

  _all_pops= new ContainerList();

  // control popups
  _dcp= new FgControlPop(this, "dcp", hctx, _fg, _all_pops);
  _tp = new TransformPop(this, "tp" , hctx, _fg, _dcp, _all_pops);
  _dcp->addTransformPop(_tp);

  // I/O popups
  _jd_read_pop = new JdReadPop (this, "jd_read_pop", hctx, _fg, _dcp);
  _jd_save_pop = new JdSavePop (this, "jd_save_pop", hctx, _fg, _dcp);
  _survey_read_pop = new SurveyReadPop (this, "survey_read_pop", 
                                        hctx, _fg, _dcp);
  _survey_save_pop = new SurveySavePop (this, "survey_save_pop", 
                                        hctx, _fg, _dcp);

  // list of plots
   _fgsp_list= new FgSeisPlotList(W(),_fg, getHelpCtx(), _dcp, _tp, _all_pops);

  // table popups
  _ld_edit = new LdEditPop   (this, "ld_edit",  hctx, _fg);
  _lines   = new LinelistPop (this, "linelist", hctx, _fg, _dcp, 
                              _ld_edit, _all_pops);
  _ld      = new LdPop       (this, "ld",       hctx, _fg, _dcp, 
                              _ld_edit,1, _all_pops);
  _ld_qc   = new LdPop       (this, "ld_qc",    hctx, _fg, _dcp, 
                              _ld_edit,3, _all_pops);
  _rp      = new RpPop       (this, "rptable",  hctx, _fg, _dcp, _all_pops);
  _pp      = new PpPop       (this, "pp" ,      hctx, _fg, _dcp, _all_pops);
  _zt1     = new ZtPop       (this, "zt1",      hctx, _fg, _dcp, 1, _all_pops);
  _zt2     = new ZtPop       (this, "zt2",      hctx, _fg, _dcp, 2, _all_pops);
  _zt3     = new ZtPop       (this, "zt3",      hctx, _fg, _dcp, 3, _all_pops);
  _zt4     = new ZtPop       (this, "zt4",      hctx, _fg, _dcp, 4, _all_pops);
  _tred    = new FgTePop     (this, "tred",     hctx, _fg, _dcp, _all_pops);
  _groups  = new GroupsPop   (this, "groups",   hctx, _fg, _dcp, _all_pops);
  _midpoint= new MidpointsPop(this, "midpoint", hctx, _fg, _dcp, _all_pops);
  _head_pop= new HeaderPop   (this, "head_pop", hctx, _fg, _dcp, _all_pops);
  _miss_pop= new FgMissPop   (this, "miss_pop", hctx, _fg, _dcp, _all_pops);

  // general popups
  _savdefdata = new SLpFileData ("savedef_file", (long)0, "Defaults File:",
				 "Save Defaults File", "ad",
				 SLpFile::_OUTPUT);
  _savdefpop= new CfgGetLoadDefs(mainWindow(),hctx, _savdefdata, this);


  // plot popups
  _2dxp=   new FgXp2DPop(this, "fgxp_2d"  , hctx, _fg, _fgsp_list);
  _3dxp=   new FgXp3DPop(this, "fgxp_3d"  , hctx, _fg, _fgsp_list);
  _map=    new FgMap(W(), _fgsp_list);
  _qcp=    new FgQcPop(W(), "fgqc"  , hctx, _fg, _fgsp_list, NULL);
  _ovjd=   new FgQcOvjdPlot(W(), "ovjd", hctx, _fg, _fgsp_list, numcolors);
  _chart=  new Fg2DPop(this, "chart"  , hctx, _fg, _fgsp_list);


  _all_pops->add(_dcp);
  _all_pops->add(_tp);
  _all_pops->putSeparatorAtBottomElement();
  _all_pops->add(_lines);
  _all_pops->add(_ld);
  _all_pops->add(_ld_qc);
  _all_pops->add(_rp);
  _all_pops->add(_pp);
  _all_pops->add(_zt1);
  _all_pops->add(_zt2);
  _all_pops->add(_zt3);
  _all_pops->add(_zt4);
  _all_pops->add(_tred);
  _all_pops->add(_groups);
  _all_pops->add(_midpoint);
  _all_pops->add(_head_pop);
  _all_pops->add(_miss_pop);
  _all_pops->putSeparatorAtBottomElement();
  _all_pops->add(_2dxp);
  _all_pops->add(_3dxp);
  _all_pops->add(_map);
  _all_pops->add(_qcp);
  _all_pops->add(_ovjd->getOvjdPop());
  _all_pops->add(_chart);
  _all_pops->putSeparatorAtBottomElement();
  _all_pops->add(this);

  _ovjd->make(W());

}



void CfgApp::createPullDowns()
{
  // Create Pull Downs and Cascade PullPop classes
       _file_pull  = new SLPullPop("file",   NULL, this);
       _option_pull= new SLPullPop("option", NULL, this);
       _table_pull = new SLPullPop("table",  NULL, this);
       _zoom_pull  = new SLPullPop("zoom",   NULL, this);
       _plots_pull = new SLPullPop("plots",  NULL, this);
       _help_pull=   new SLPullPop("help",   NULL, this);


  // FILE   Pulldown
         _file_pull->addPushUp("readjd", _jd_read_pop);
         _file_pull->addPushUp("savejd", _jd_save_pop);
         _file_pull->addSep();
         _file_pull->addPushUp("readsurvey", _survey_read_pop);
         _file_pull->addPushUp("savesurvey", _survey_save_pop);
         _file_pull->addSep();
         _file_pull->addPush("iconall", ICONALL);
         _file_pull->addSep();
         _file_pull->addPush("quit"  , QUIT);

  // OPTION Pulldown
         _option_pull->addPushUp("dcp"  , _dcp);
         _option_pull->addPushUp("tp" , _tp);
         _option_pull->addSep();
         _option_pull->addPushUp("plotcolors", _fgsp_list->getPlotColorPop());
         _option_pull->addPushUp("savdef" , _savdefpop);
         _option_pull->addSep();
         _option_pull->addTog("showstat" , SHOWSTAT);
  // TABLE  Pulldown
         _table_pull->addPushUp("linelist",_lines);
         _table_pull->addPushUp("ld",      _ld);
         _table_pull->addPushUp("ld_qc",   _ld_qc);
         _table_pull->addPushUp("rptable", _rp);
         _table_pull->addPushUp("pp",      _pp);
         _table_pull->addSep();
         _table_pull->addPushUp("zt1",     _zt1);
         _table_pull->addPushUp("zt2",     _zt2);
         _table_pull->addPushUp("zt3",     _zt3);
         _table_pull->addPushUp("zt4",     _zt4);
         _table_pull->addPushUp("tred",    _tred);
         _table_pull->addSep();
         _table_pull->addPushUp("groups",  _groups);
         _table_pull->addPushUp("midpoint",_midpoint);
         _table_pull->addPushUp("head_pop",_head_pop);
         _table_pull->addSep();
         _table_pull->addPushUp("miss_pop",_miss_pop);

  // ZOOM Pulldown
         _zoom_pull->addPush("zup",    ZUP);
         _zoom_pull->addPush("zupsep", ZUPSEP);
         _zoom_pull->addPush("zdown",  ZDOWN);
         _zoom_pull->addPush("zorgin", ZORGIN);
         _zoom_pull->addSep();
         _zoom_pull->addPushUp("zoomop" , _fgsp_list->zoomOpPop());

  // PLOTS  Pulldown
         _inmain_cas= new SLPullPop("inmain", _plots_pull);
         _plots_pull->addSep();
         _plots_pull->addPushUp("fgxp_2d",_2dxp);
         _plots_pull->addPushUp("fgxp_3d",_3dxp);
         _plots_pull->addPushUp("fgqc",   _qcp);
         _plots_pull->addPushUp("ovjd",   _ovjd->getOvjdPop());
         //_plots_pull->addPush("ovjd",   OVJDPLOTS);
         _plots_pull->addPushUp("map",    _map);
         _plots_pull->addPushUp("chart",  _chart);
  // inMain Cascade
         _inmain_cas->addRadio("mapin"  ,     MAPin);
         _inmain_cas->addRadio("xp2dplotsin", XP2DPLOTSin);
         _inmain_cas->addRadio("xp3dplotsin", XP3DPLOTSin);
         _inmain_cas->addRadio("qcplotsin",   QCPLOTSin);
         _inmain_cas->addRadio("ovjdplotsin", OVJDPLOTSin);
         _inmain_cas->addRadio("chartin",     CHARTin);
  // HELP   Pulldown
         _help_pull->addPush("version",  VERSION);
         _help_pull->addPush("overview", OVERVIEW);


  // Set the notify methed
       _file_pull->setComplexNotify(this);
       _option_pull->setComplexNotify(this);
       _table_pull->setComplexNotify(this);
       _zoom_pull->setComplexNotify(this);
       _plots_pull->setComplexNotify(this);
       _inmain_cas->setComplexNotify(this);
       _help_pull->setComplexNotify(this);

}


void CfgApp::finalInit()
{
  calledFromInmain(_inmain_cas->radioValue());
  _plot_in_main->unmanage();
  if (_option_pull->toggleValue(SHOWSTAT)) _fgsp_list->showMessageArea();
  else                                     _fgsp_list->hideMessageArea();
}

void CfgApp::saveAndExit(Boolean exit_now)
{
   if (exit_now) usage_exit (0);
   _jd_save_pop->makeAndManage();
   _jd_save_pop->setModal(SLShellContainer::FullAppModal);
   _expecting_save_pop= True;
}

Boolean CfgApp::notifyComplex(SLDelay *obj, int ident)
{
   if(_version_info != NULL) {
      delete _version_info; 
      _version_info = NULL;
      _plot_in_main->makeAndManage();
   }

   if      (obj == _file_pull)   calledFromFile();
   else if (obj == _option_pull) calledFromOption();
   else if (obj == _table_pull)  calledFromTable();
   else if (obj == _zoom_pull)   calledFromZoom();
   else if (obj == _plots_pull)  calledFromPlots();
   else if (obj == _inmain_cas)  
               calledFromInmain(_inmain_cas->radioValue());
   else if (obj == _help_pull)   calledFromHelp();
   else if (obj == _question) {
               if (!ident) saveAndExit(True);
               else        saveAndExit(False);
   }
   else if (obj == _jd_save_pop) {
          if (_expecting_save_pop) {
               _jd_save_pop->setModal(SLShellContainer::Modeless);
               if (ident == FP_OK) saveAndExit(True);
               _expecting_save_pop= False;
          }
   }
   return True;
}

void CfgApp::calledFromFile()
{
  switch ( _file_pull->lastIdent() ) {
      case QUIT:
           if(_fg->dataNeedsSaving()) {
                 _question= new SLQuestPop(this, "Save Data?", SAVE_MSG, True);
           } // end if dataNeedsSaving
           else  {
             usage_exit (0);
	   }
	   _exiting = True;
           break;
      case ICONALL:
           SLShellContainer::iconifyAll();
           break;
  } // end switch
}

void CfgApp::calledFromOption()
{
  switch ( _option_pull->lastIdent() ) {
    case SHOWSTAT:  if (_option_pull->toggleValue(SHOWSTAT))
                              _fgsp_list->showMessageArea();
                    else      _fgsp_list->hideMessageArea();
                    break;
  }
}
void CfgApp::calledFromTable()
{
}

void CfgApp::calledFromZoom()
{
  if (_fgsp_in_main) {
    switch ( _zoom_pull->lastIdent() ) {
      case ZUP:     _fgsp_in_main->zoomUp();            break;
      case ZUPSEP:  _fgsp_in_main->zoomUpSeparateWin(); break;
      case ZDOWN:   _fgsp_in_main->zoomDown();          break;
      case ZORGIN:  _fgsp_in_main->originalSize();      break;
    } // end switch
  }
  else
      printf("_fgsp_in_main is NULL\n");
}

void CfgApp::calledFromInmain(int which_set)
{
  if (_plot_in_main) {
        _plot_in_main->unmanage();
        _fgsp_in_main= NULL;
  }
  if (_plot_specific_pull) {
      _plot_specific_pull->unmanageCascade();
      _plot_specific_pull= NULL;
  }
  switch (which_set) {
      case MAPin       : _plot_in_main= _map_main;  
                         if (_map_main) 
                               _plot_specific_pull= _map_main->pulldown();
                         break;
      case XP2DPLOTSin : _plot_in_main= _2dxp_main;
                          if (_2dxp_main) 
                                _plot_specific_pull= _2dxp_main->pulldown();
                         break;
      case XP3DPLOTSin : _plot_in_main= _3dxp_main;
                          if (_3dxp_main) 
                                _plot_specific_pull= _3dxp_main->pulldown();
                         break;

      case QCPLOTSin   : _plot_in_main= _qcp_main->P();
                          if (_qcp_main) 
                              _plot_specific_pull= _qcp_main->pulldown();
                         break;


      case OVJDPLOTSin   : _plot_in_main= _ovjd_main;
                          if (_ovjd_main) 
                              _plot_specific_pull= _ovjd_main->pulldown();
                         break;
 
      case CHARTin   : _plot_in_main= _chart_main;
                          if (_chart_main) 
                              _plot_specific_pull= _chart_main->pulldown();
                         break;
 
      default: assert(0); break;
  } // end switch

  if (_plot_in_main) {
          Widget find_widget, w;
          FgSeisPlot *q;
          void *x;
          Boolean found= False;
          _plot_in_main->makeAndManage();
          if (_plot_specific_pull) _plot_specific_pull->manageCascade();

          /*
           * Find which fgsp in the list is in the current managed plot.
           * This is done by finding the fgsp from the list that has
           * the _plot_in_main widget as a parent.
           */
          find_widget= _plot_in_main->W();
          for(q= _fgsp_list->top(&x); ((q)&&(!found)); 
                       q= _fgsp_list->next(&x))
                 for(w= q->W(); ((w)&&(!found)); w=XtParent(w)) 
                     if (w == find_widget) {
                          found= True;
                          _fgsp_in_main= q;
                     } // end if
 
  } // end if
}



void CfgApp::calledFromPlots()
{
  switch ( _plots_pull->lastIdent() ) {
    case OVJDPLOTS:  
                    _ovjd->getOvjdPop()->makeAndManage();
                    break;
  }
}
void CfgApp::calledFromHelp()
{
  ErrorHandler eh(W(),"version", True);

  switch ( _help_pull->lastIdent() ) {
    case VERSION:   eh.deliverInformation(_version);            break;
    case OVERVIEW:  overview_help("overview", getHelpCtx());    break;
  }
}

void CfgApp::realizing()
{
}

void CfgApp::closeMain()
{
  usage_exit(0);
}
//==================================================================
//======================= End Cfg Methods ==========================
//==================================================================



//==================================================================
//======================= CfgGetLoadDefs Methods ==================
//==================================================================
void CfgGetLoadDefs::saveFile( char *filename, void *data)
{
  DefInfo def;
  CfgApp *cfg= (CfgApp *)data;
  ShellStatMsg  bld_info(cfg->W(),"Saving Defaults...");
  if (filename) def= DefFileInit(filename, 12, True);
  else          def= DefStandardInit(get_shell_widget(cfg->mainWindow()), 12);

  SLDelay::makeAll();
  DefSave(def, cfg->mainWindow());
  DefEnd(def);
}
