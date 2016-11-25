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
// $Id: va_application.cc,v 1.9 2005/12/13 16:35:19 spws Exp $
// $Name: 12-13-2005 $

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Xm/Xm.h>
#include "wproc.h"
#include "cprim.h"
#include "va_application.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_iso_plotter.hh"
#include "vaplots/va_grid_plot.hh"
#include "vaplots/va_crossplot_plot.hh"
#include "vaplots/va_eta_plot.hh"
#include "vaplots/va_common_params.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/color_pop.hh"	/* ehs test */
#include "vf/vf_manager.hh"
#include "sl/sl_pull_pop.hh"
#include "sl/sl_form.hh"
#include "sl/error_handler.hh"
#include "sl/shell_mouse_help.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/version_info.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_ascii_viewer.hh"
#include "sl/view_win.hh"
#include "sl/sl_def_app_pop.hh"
#include "sp/seis_zoomop_pop.hh"
#include "sp/seis_cbar_pop.hh"
#include "sp/seis_contour_cbar_pop.hh"
#include "vaplots/va_bottom_control.hh"
#include "vaplots/va_input_pop.hh"
#include "vaplots/va_plot_control.hh"
#include "vaplots/va_header_shift.hh"
#include "vaplots/va_nmc_output.hh"
#include "vaplots/va_movie_pop.hh"
#include "vaplots/va_cp_pop.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_push.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_file_data.hh"
#include "sl/container_list.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_horizons.hh"
#include "sl/sl_ascii_viewer.hh"
#include "vu/seis_label.hh"
#include "vu/header_dump_pop.hh"
#include "hardcopy/hardcopy_seis_pop.hh"
#include "ipc/ipc_io.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"
#include "sl/colorset_collection.hh"
#include "sl/color_info_set.hh"


//from Tom
#include "vfgui/vfgui_watch.hh"
#include "vfgui/vfgui_quit.hh"
#include "vfgui/vfgui_status.hh"
#include "vfgui/vfgui_regulate.hh"
#include "vfgui/vfpop_read.hh"
#include "vfgui/vfpop_save.hh"
#include "vfgui/vfpop_list.hh"
#include "vfgui/vfpop_pick.hh"
#include "vfgui/vfpop_fun.hh"
#include "vfgui/vfpop_types.hh"
#include "vfgui/vfpop_names.hh"
#include "vfgui/vfpop_headers.hh"
#include "vfgui/vfpop_coords.hh"
#include "vfgui/vfpop_resample.hh"
#include "vfgui/vfpop_latsample.hh"
#include "vfgui/vfpop_nearsample.hh"
#include "vfgui/vfpop_raytrace.hh"
#include "vfgui/vfpop_delete.hh"
#include "vfgui/vfpop_multiply.hh"
#include "vfgui/vfpop_misc.hh"
#include "vfgui/vfpop_mute.hh"
#include "vfgui/vfpop_info.hh"
#include "vfgui/vfpop_history.hh"
#include "vfgui/vfpop_tol.hh"
#include "vfgui/vfpop_horizon_read.hh"
#include "vfgui/vfpop_horizon_list.hh"
#include "vfgui/vfpop_transform.hh"

#include "initusage.h"

#define CLASS_NAME "Va"
#define APP_NAME   "Va"



     

static char *fallback_resources[] = {
#     include "Va.h"
      "*helpfile:          Va_help",
      NULL
};

#define DEF_NUM_SEM_COLORS  34
#define DEF_NUM_ISO_COLORS  34
#define DEF_NUM_GVS_COLORS  34
#define DEF_NUM_CMP_COLORS  34

class VaGetLoadDefs : public SLDefPop {
   public:
     VaGetLoadDefs(  Widget        p,
                     char          *name,
                     HelpCtx       hctx,
                     SLpFileData   *slp_file_data,
                     void          *data,
                     Boolean       make_now) :
             SLDefPop(p,name,hctx,slp_file_data,data,make_now) {};
     virtual void saveFile( char *filename, void *data);
     virtual void getFile( char *filename, void *data);
};


static char *fetch_proper_help_file()
{
   static char helpfile[120];
   strcpy(helpfile, getenv("CPSEIS_INSTALL_DIR"));
   strcat(helpfile, "/spws_home/app-defaults/Va_help");
   printf("va help file = %s\n", helpfile);
   return helpfile;
}
static char *helpfile = fetch_proper_help_file();


static XtResource res[]= {
      { "privateCmap", "PrivateCmap", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(va_res,private_cmap), XtRImmediate,
        (XtPointer) False } ,
      { "doBackingStore", "DoBackingStore", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(va_res,dobacking), XtRImmediate, (XtPointer) True } ,

      { "semColors", "SemColors", XtRInt, sizeof(long),
        XtOffsetOf(va_res,num_sem_colors), XtRImmediate, (XtPointer) 34 },
      { "isoColors", "IsoColors", XtRInt, sizeof(long),
        XtOffsetOf(va_res,num_iso_colors), XtRImmediate, (XtPointer) 34 },
      { "gvsColors", "GvsColors", XtRInt, sizeof(long),
        XtOffsetOf(va_res,num_gvs_colors), XtRImmediate, (XtPointer) 34 },
      { "cmpColors", "CmpColors", XtRInt, sizeof(long),
        XtOffsetOf(va_res,num_cmp_colors), XtRImmediate, (XtPointer) 34 },
      { "noIsoUnderlay", "NoIsoUnderlay", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(va_res,no_iso_underlay), XtRImmediate, (XtPointer) False } ,
      { "autoSize", "AutoSize", XtRBoolean, sizeof(Boolean),
        XtOffsetOf(va_res,auto_size), XtRImmediate, (XtPointer) True } ,


      { "defaultFile", "DefaultFile", XtRString, sizeof(char *),
        XtOffsetOf(va_res,def_file), XtRString, NULL },

      { "semFile", "SemFile", XtRString, sizeof(char *),
        XtOffsetOf(va_res,sem_file), XtRString, NULL },
      { "cmpFile", "CmpFile", XtRString, sizeof(char *),
        XtOffsetOf(va_res,cmp_file), XtRString, NULL },
      { "gvsFile", "GvsFile", XtRString, sizeof(char *),
        XtOffsetOf(va_res,gvs_file), XtRString, NULL },
      { "velFile", "VelFile", XtRString, sizeof(char *),
        XtOffsetOf(va_res,vel_file), XtRString, NULL },


      { "helpFile", "VaResources", XtRString, sizeof(char *),
        XtOffsetOf(va_res,help_file), XtRString,
              (void*)helpfile },
};

static XrmOptionDescRec options[] = {
      { "-private",     "*privateCmap",     XrmoptionNoArg,  "True" },
      { "-help",        "*showHelp",        XrmoptionNoArg,  "True" },
      { "-nobs",        "*doBackingStore",  XrmoptionNoArg,  "False"},
      { "-bs",          "*doBackingStore",  XrmoptionNoArg,  "True" },

      { "-semcbar",     "*semColors",      XrmoptionSepArg, NULL   },
      { "-isocbar",     "*isoColors",      XrmoptionSepArg, NULL   },
      { "-gvscbar",     "*gvsColors",      XrmoptionSepArg, NULL   },
      { "-cmpcbar",     "*cmpColors",      XrmoptionSepArg, NULL   },
      { "-nounderlay",  "*noIsoUnderlay",  XrmoptionNoArg,  "True" },
      { "-defaults",    "*defaultFile",    XrmoptionSepArg, NULL   },
      { "-noas",        "*autoSize",       XrmoptionNoArg,  "False" },
      { "-noautosize",  "*autoSize",        XrmoptionNoArg, "False" },

      { "-semfile",    "*semFile",        XrmoptionSepArg, NULL   },
      { "-gvsfile",    "*gvsFile",        XrmoptionSepArg, NULL   },
      { "-cmpfile",    "*cmpFile",        XrmoptionSepArg, NULL   },
      { "-velfile",    "*velFile",        XrmoptionSepArg, NULL   },

      { "-helpfile",    "*helpFile",        XrmoptionSepArg, NULL   },
     };



Boolean       VaApplication::_static_initialized= False;
int           VaApplication::_instance_cnt= 0;
Pixmap        VaApplication::_icon= NULL;
Pixmap        VaApplication::_mask= NULL;
va_res        VaApplication::_resources;

VaApplication::VaApplication(int& argc, char **argv) :
              SLApp ((char*)APP_NAME, (char*)CLASS_NAME, argc, argv,
                     options, XtNumber(options), False, fallback_resources)
{
  _vf_manager = new VfManager((int)1, (int)1, "VA");
                                        //two data sets one is editable
  _horizons   = new VfHorizons(_vf_manager->informer());
  _regulate   = new VfguiRegulate  (_vf_manager);
  _clist      = new ContainerList  ();


  //setFallbacks(fallback_resources);
  if (!_static_initialized) doStatics();  // color, help, icon, resources
  doColor();
  createMainArea();
  createPopups();
  createPullDowns();
  finalInit();
  _version= newstr(VA_VERSION);
  //initHelp(_helpfile, "Help for the Velocity Analysis program");
}


VaApplication::~VaApplication()
{
  _instance_cnt--;

  delete _sem_cbar;
  delete _iso_cbar;
  delete _iso_contour_cbar;
  delete _file_pull;
  delete _option_pull;
  delete _display_pull;
  delete _edit_pull;
  delete _window_pull;
  delete _view_pull;
  delete _testing_pull;
  delete _header_pull;
  delete _tools_pull;
  delete _help_pull;
  delete _version_info;
  delete _main_form;
  delete _vf_manager;
  delete _horizons;
  delete _clist;
  delete _vect_colors;
  delete _semblance_header_dump_pop;
  delete _cmp_header_dump_pop;
  delete _gvs_header_dump_pop;
  delete _savdefdata;
  delete _globals_data;
  delete _hc_iso_data;
  delete _hc_grid_data;
  delete _hc_gvs_data;
  delete _hc_cmp_data;
  delete _hc_xp_data;
  delete _hc_sem_data;
  delete _hc_eta_data;

  if (_exiting || _instance_cnt == 0) {
    ColorsetCollection:: remove ();
    ColorInfoCollection::remove ();
    usage_exit (0);
  }
}


void VaApplication::doStatics()
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
   PlotBase::setWatchOK(False);

}


void VaApplication::showHelp()
{
}


void VaApplication::doColor()
{
   unsigned long cflags; 
   long visclass;
   Display *dpy= XtDisplay(topWidget());
   long    total_colors;

   // --------- color ------------
      cflags= test_vis(dpy, &visclass, &total_colors );

      if ( cflags & MAY_COL ) {
          _cando_underlay= !_resources.no_iso_underlay;
          _total_to_alloc= 
                         _resources.num_sem_colors +
                         _resources.num_iso_colors +
                         _resources.num_gvs_colors +
                         _resources.num_cmp_colors +
                         + VaPlot::NUM_CONTOUR_COLORS
                         + VaPlot::NUM_MISC_COLORS;
          if (_cando_underlay) _total_to_alloc+= _resources.num_iso_colors;
          if (_total_to_alloc > total_colors - 30) {
              _resources.num_sem_colors= DEF_NUM_SEM_COLORS;
              _resources.num_iso_colors= DEF_NUM_ISO_COLORS;
              _resources.num_gvs_colors= DEF_NUM_GVS_COLORS; 
              _resources.num_cmp_colors= DEF_NUM_CMP_COLORS;
              fprintf(stderr, "VA: This machine may only use %d colors.\n",
                                   total_colors - 30);
              fprintf(stderr, "VA: You have requested %d colors.\n",
                                   _total_to_alloc);
              _total_to_alloc= 
                         _resources.num_sem_colors +
                         _resources.num_iso_colors +
                         _resources.num_gvs_colors +
                         _resources.num_cmp_colors +
                         + VaPlot::NUM_CONTOUR_COLORS
                         + VaPlot::NUM_MISC_COLORS;
              if (_cando_underlay) _total_to_alloc+= _resources.num_iso_colors;
              assert(_total_to_alloc < total_colors - 30);
              fprintf(stderr, "Vao: Setting colors down to:\n");
              fprintf(stderr, "     Semblance %d:\n", DEF_NUM_SEM_COLORS);
              fprintf(stderr, "     ISO Vel %d:\n",   DEF_NUM_ISO_COLORS);
              fprintf(stderr, "     GVS: %d\n",       DEF_NUM_GVS_COLORS);
              fprintf(stderr, "     CMP: %d\n",       DEF_NUM_CMP_COLORS);
          } // end if
	  Paintset *paintset = PaintsetCollection::fetchExistingByColormap (
            W());
	  if (!paintset->readOnly()) {
/*
            if (canIAlloc((int)_total_to_alloc,0) &&
                !_resources.private_cmap            ) {
               _cando_colors = True;
	    } // end if
	    else {
	      fprintf(stderr, 
                "VA: Attemping to allocate private color table...\n");
*/
	      tryAgainWithPrivate (_total_to_alloc, 0);
	      _cando_colors = isPrivateCmap ();
	      if (_cando_colors) fprintf (stderr, "VA: Success.\n");
	      else {
		fprintf (stderr, 
                  "VA: Private color table could not be allocated.\n");
		fprintf (stderr, "VA: exiting.\n");
		//usage_exit (0);
	      }
//	    } // End else
	  }
	  else { // paintset->readOnly() != FALSE
	    if (!canIAlloc((int)_total_to_alloc,0)) {
	      //usage_exit (0);
	      assert (0);
	    }
	    _cando_colors = True;
	  }
      } // End if
      else {
           fprintf(stderr, "VA: This machine will not only support color\n");
           fprintf(stderr, "VA: with a loadable colormap.\n");
           fprintf(stderr, "VA: exiting.\n");
           //usage_exit (0);
      } // end if
}

void VaApplication::addMessageModeXYOut(SeisPlot *sp, char *modestr)
{
    sp->setMessageWidget(statusWidget() );
    sp->setModeWidget(modeWidget(), modestr);
    _zoom_op_pop->addControl(sp);
}

void VaApplication::createMainArea()
{
    HelpCtx hctx;
    static char *info_labels[]= { VA_VERSION, VA_INFO };
    Pixel fgpix, bgpix;
    _instance_cnt++;

    VaIsoPlot           *va_iso_plot;
    VaIsoPlotter        *va_iso_plotter;
    VaGridPlot          *va_grid_plot;
    VaSemblancePlot     *va_semblance_plot;
    VaGvsPlot           *va_gvs_plot;
    VaCmpPlot           *va_cmp_plot;
    VaCrossplotPlot     *va_crossplot_plot;
    VaEtaPlot           *va_eta_plot;

   setTitle("va", False);
   setIcon("va");
   initHelp( _resources.help_file, "Help for Velocity analysis");
   hctx= getHelpCtx();

   _main_form=    new SLForm(mainWindow(),"main_form");
//////////////// new /////////////////
  bgpix = PaintsetCollection::white (DefaultScreenOfDisplay(XtDisplay(
    topWidget())));
  fgpix = PaintsetCollection::black (DefaultScreenOfDisplay(XtDisplay(
    topWidget())));
//////////////// new /////////////////
/*/////////////// old /////////////////
   bgpix = WhitePixelOfScreen(XtScreen(topWidget()));
   fgpix = BlackPixelOfScreen(XtScreen(topWidget()));
*//////////////// old /////////////////
   _version_info = new VersionInfo(_main_form->W(),VA_VERSION_WIDGET,
                                  info_labels, XtNumber(info_labels),
                                  fgpix, bgpix);
   setWorkArea(_main_form);
   WinCtl *winctl= new WinCtl(_main_form->W(), "junk");

   _vect_colors = new VaVectColors();
   
   _common_params = new VaPlotCommonParams();

   va_semblance_plot = new VaSemblancePlot(_vf_manager, _horizons, winctl->W(),
                                            "semblance",  hctx, _common_params,
                                             _vect_colors, 
                                             _resources.num_sem_colors,
                                             VaPlot::NUM_CONTOUR_COLORS);
   va_gvs_plot = new VaGvsPlot(_vf_manager, _horizons, winctl->W(), 
                                            "gvs", hctx, _common_params,
                                             _vect_colors,
                                             _resources.num_gvs_colors);
   va_cmp_plot = new VaCmpPlot(_vf_manager, _horizons, winctl->W(),
                                            "cmp", hctx, _common_params,
                                             _vect_colors,
                                             _resources.num_cmp_colors);
   va_iso_plot = new VaIsoPlot(_vf_manager, _horizons, winctl->W(),"iso", hctx,
                                             _vect_colors,
                                             _resources.num_iso_colors);
   va_grid_plot= new VaGridPlot(_vf_manager, _horizons, winctl->W(),"grid",hctx,
                                             _vect_colors);
   va_crossplot_plot= new VaCrossplotPlot(_vf_manager, _horizons, winctl->W(),
                                             "crossplot",
                                             hctx, _common_params,
                                             _vect_colors);
   va_eta_plot = new VaEtaPlot(_vf_manager, _horizons,winctl->W(),
                                            "eta",  hctx, _common_params,
                                             _vect_colors, 
                                             _resources.num_sem_colors, 
                                             va_semblance_plot, va_cmp_plot);
   va_semblance_plot->registerEtaPlot(va_eta_plot);

   va_iso_plotter = new VaIsoPlotter(_vf_manager,va_iso_plot,va_gvs_plot);

   _va_header_shift = new VaHeaderShift(winctl->W(),"Header Shifts",hctx,
                                       _vf_manager, va_cmp_plot);
   
   _va_nmc_output = new VaNmcOutput(winctl->W(),"Nmc Output",hctx,
                                       _vf_manager, va_cmp_plot, va_iso_plot); 

   _color_pop = new ColorPop(this, "eds_test", hctx, _vect_colors);


   _plot_ctl= new VaPlotControl(_vf_manager, winctl, 
                                va_iso_plot, va_grid_plot,
                                va_semblance_plot,   va_gvs_plot,
                                va_cmp_plot, va_crossplot_plot,
                                va_eta_plot, _vect_colors);
   va_iso_plot->registerPlotControl(_plot_ctl);

   _bottom_ctl = new VaBottomControl(_main_form->W(),"control", 
                                     _plot_ctl, NULL);

   va_eta_plot->registerBottomControl(_bottom_ctl);
   
   XtVaSetValues( _bottom_ctl->W(), XmNrightAttachment, XmATTACH_FORM,
                                    XmNleftAttachment, XmATTACH_FORM,
                                    XmNbottomAttachment,XmATTACH_FORM, NULL);

   XtVaSetValues( winctl->W(), XmNleftAttachment,  XmATTACH_FORM,
                               XmNrightAttachment, XmATTACH_FORM,
                               XmNtopAttachment,   XmATTACH_FORM,
                               XmNbottomAttachment,XmATTACH_WIDGET,
                               XmNbottomWidget,    _bottom_ctl->W(), NULL);


}



void VaApplication::createPopups()
{

   HelpCtx hctx= getHelpCtx();
   _input_pop= new VaInputPop(_main_form->W(), "input", hctx, 
                              _clist, _plot_ctl);
   _read = new VfpopRead      (this, "cread", _vf_manager, _clist);
   _save = new VfpopSave       (this, "save", _vf_manager, _clist);
   _list = new VfpopList       (this, "list", _vf_manager, _clist);
   _pick = new VfpopPick       (this, "pick", _vf_manager, _clist);
   _fun  = new VfpopFun        (this, "fun" , _vf_manager, _clist);
   _type = new VfpopTypes      (this, "type", _vf_manager, _clist);
   _name = new VfpopNames      (this, "name", _vf_manager, _clist);
   _head = new VfpopHeaders    (this, "head", _vf_manager, _clist);
   _coor = new VfpopCoords     (this, "coor", _vf_manager, _clist);
   _res  = new VfpopResample   (this, "res" , _vf_manager, _clist);
   _lat  = new VfpopLatsample  (this, "lat" , _vf_manager, _clist);
   _near = new VfpopNearsample (this, "near", _vf_manager, _clist);
   _ray  = new VfpopRaytrace   (this, "ray" , _vf_manager, _clist);
   _del  = new VfpopDelete     (this, "del" , _vf_manager, _clist);
   _mult = new VfpopMultiply   (this, "mult", _vf_manager, _clist);
   _misc = new VfpopMisc       (this, "misc", _vf_manager, _clist);
   _mute = new VfpopMute       (this, "mute", _vf_manager, _clist, _pick);
   _info = new VfpopInfo       (this, "info", _vf_manager, _clist);
   _hist = new VfpopHistory    (this, "hist", _vf_manager, _clist);
   _tol  = new VfpopTol        (this, "tol" , _vf_manager);

   /*
    * horizon stuff
    */
   _trans = new VfpopTransform (this, "trans" , _horizons);

   _hor_read = new VfpopHorizonRead(this, "hread", _vf_manager,
	_horizons, _trans, _clist);
   _hor_list = new VfpopHorizonList(this, "hlist", _vf_manager,
	_horizons, _trans, _clist);

   _info->setVaSemblancePlot(_plot_ctl->semblance());
   _info->setVaCmpPlot      (_plot_ctl->cmp());

   _savdefdata = new SLpFileData ("savedef_file", (long)0, "Defaults File: ",
			       "Save Defaults File", "ad", SLpFile::_OUTPUT);
   _savdefpop= new SLDefAppPop(mainWindow(), "savdef", hctx,
                               _savdefdata, this);

   _movie_pop= new VaMoviePop(mainWindow(), "movie_pop", hctx, _plot_ctl);
   _zoom_op_pop= new SeisZoomOpPop(mainWindow(), "zoomop", hctx, NULL);


   _globals_data = new SLpFileData ("global_file", (long)0, "Filename...",
				  "ASCII File", "trc*", SLpFile::_INPUT);
   _globals_pop = new SLAsciiViewer(mainWindow(),"Global File Viewer", hctx,
                                    _globals_data, True, False,
				    _plot_ctl->semblance()->SP());

   _semblance_header_dump_pop=  new HeaderDumpPop(this, "Semblance Headers",
                                                  _plot_ctl->semblance()->SP(),
                                                  hctx);
   _cmp_header_dump_pop      =  new HeaderDumpPop(this, "Cmp Headers",
                                                  _plot_ctl->cmp()->SP(),
                                                  hctx);
   _gvs_header_dump_pop      =  new HeaderDumpPop(this, "Gvs Headers",
                                                  _plot_ctl->gvs()->SP(),
                                                  hctx);

   //temporary hardcopy pop ups. Will be combined later
   _hc_iso_data = new SLpFileData ("hardcopy_iso_file", (long)0,
				   "Plot File:", "Plot File", "cgm",
				   SLpFile::_OUTPUT);
   _hardcopy_iso = new HardCopyPop(mainWindow(), "hardcopy_iso", hctx, 
                                  _plot_ctl->iso()->SP(), _hc_iso_data);
   _hc_grid_data = new SLpFileData ("hardcopy_grid_file", (long)0,
				   "Plot File:", "Plot File", "cgm",
				   SLpFile::_OUTPUT);
   _hardcopy_grid= new HardCopyPop(mainWindow(), "hardcopy_grid", hctx, 
                                  _plot_ctl->grid()->SP(), _hc_grid_data);
   _hc_gvs_data = new SLpFileData ("hardcopy_gvs_file", (long)0,
				   "Plot File:", "Plot File", "cgm",
				   SLpFile::_OUTPUT);
   _hardcopy_gvs = new HardCopyPop(mainWindow(), "hardcopy_gvs",  hctx, 
				   _plot_ctl->gvs()->SP(), _hc_gvs_data,
				   HardCopyPop::PIP, False, 0.5F);
   _hc_cmp_data = new SLpFileData ("hardcopy_cmp_file", (long)0,
				   "Plot File:", "Plot File", "cgm",
				   SLpFile::_OUTPUT);
   _hardcopy_cmp = new HardCopySeisPop(mainWindow(), "hardcopy_cmp",  hctx, 
                                  _plot_ctl->cmp()->SP(), _hc_cmp_data);
   _hc_xp_data = new SLpFileData ("hardcopy_xp_file", (long)0,
				  "Plot File:", "Plot File", "cgm",
				  SLpFile::_OUTPUT);
   _hardcopy_crossplot = new HardCopyPop(mainWindow(),"hardcopy_crossplot", 
					 hctx, _plot_ctl->crossplot()->SP(),
					 _hc_xp_data);
   _hc_sem_data = new SLpFileData ("hardcopy_sem_file", (long)0,
				   "Plot File:", "Plot File", "cgm",
				   SLpFile::_OUTPUT);
   _hardcopy_semblance = new HardCopyPop(mainWindow(),"hardcopy_semblance", 
					 hctx, _plot_ctl->semblance()->SP(),
					 _hc_sem_data);
   _hc_eta_data = new SLpFileData ("hardcopy_eta_file", (long)0,
				   "Plot File:", "Plot File", "cgm",
				   SLpFile::_OUTPUT);
   _hardcopy_eta = new HardCopyPop(mainWindow(),"hardcopy_eta", 
				   hctx, _plot_ctl->eta()->SP(),
				   _hc_eta_data);
   _label=  new SeisLabel(_plot_ctl->iso()->SP(), hctx);
   _label->addPlot(_plot_ctl->grid()->SP());
   _label->addPlot(_plot_ctl->gvs()->SP());
   _label->addPlot(_plot_ctl->cmp()->SP());
   _label->addPlot(_plot_ctl->crossplot()->SP());
   _label->addPlot(_plot_ctl->semblance()->SP());
   _label->addPlot(_plot_ctl->eta()->SP()); // be sure and test adding labels


   _sem_cbar= new SeisCbarPop(mainWindow(), "cbar", 
                              _plot_ctl->semblance()->SP(), hctx); 
   _iso_cbar= new SeisCbarPop(mainWindow(), "cbar", 
                              _plot_ctl->iso()->SP(), hctx); 
   _iso_contour_cbar= new SeisContourCbarPop (mainWindow(),"cbar", 
                              _plot_ctl->iso()->SP(), hctx); 
   _cmp_cbar= new SeisCbarPop(mainWindow(), "cbar", 
                              _plot_ctl->cmp()->SP(), hctx); 
   _gvs_cbar= new SeisCbarPop(mainWindow(), "cbar", 
                              _plot_ctl->gvs()->SP(), hctx); 
}


void VaApplication::createPullDowns()
{
  HelpCtx hctx= getHelpCtx();

  // Create Pull Downs - PullPop classes

  _clist->add(_input_pop);
  _clist->add(_read);
  _clist->add(_save);
  _clist->add(_hor_read);
  _clist->putSeparatorAtBottomElement();
  _clist->add(_list);
  _clist->add(_pick);
  _clist->add(_fun);
  _clist->add(_hor_list);
  _clist->add(_trans);
  _clist->putSeparatorAtBottomElement();
  _clist->add(_type);
  _clist->add(_name);
  _clist->add(_head);
  _clist->add(_coor);
  _clist->add(_res);
  _clist->add(_lat);
  _clist->add(_near);
  _clist->add(_ray);
  _clist->add(_del);
  _clist->add(_mult);
  _clist->add(_misc);
  _clist->putSeparatorAtBottomElement();
  _clist->add(_mute);
  _clist->putSeparatorAtBottomElement();
  _clist->add(_info);
  _clist->add(_hist);
  _clist->putSeparatorAtBottomElement();
  _clist->add(_tol);
  _clist->putSeparatorAtBottomElement();



  _file_pull   = new SLPullPop("file",    hctx, this);
  _option_pull = new SLPullPop("option",  hctx, this);
  _edit_pull   = new SLPullPop("edit",    hctx, this);
  _window_pull = new SLPullPop("window",  hctx, this);
  _display_pull= new SLPullPop("display", hctx, this);
  _view_pull   = new SLPullPop("view",    hctx, this);
  _header_pull = new SLPullPop("Headers", hctx, this);
  _tools_pull  = new SLPullPop("tools",   hctx, this);
  _testing_pull= new SLPullPop("Testing", hctx, this);
  _help_pull   = new SLPullPop("help",    hctx, this);
  _popup_menu  = new SLPullPop("popup",   hctx, this, False);
  _popup_menu->setMapCallbackProc(&VaApplication::handlePopupMenuMap,
                                  (void *) this);
  

 // FILE Pulldown
  _file_pull->setComplexNotify(this);
  _file_pull->addPushUp("input",  _input_pop );
  _file_pull->addPushUp("cread",  _read);
  _file_pull->addPushUp("hread", _hor_read);
  _file_pull->addSep();
  _file_pull->addPushUp("save",  _save);
  _file_pull->addPush("backup3");
  SLpPush *backup3 = (SLpPush*)_file_pull->primObj("backup3");
  _regulate->regulateBackup(backup3);
  _file_pull->addSep();
  _file_pull->addPushUp("del" , _del);
  _file_pull->addPush("compare2");
  SLpPush *compare2 = (SLpPush*)_file_pull->primObj("compare2");
  _regulate->regulateCompare(NULL, compare2);
  _file_pull->addSep();
  _file_pull->addPush("quit", QUIT); 

 // Option Menu
  _option_pull->addPushUp("vacp",  _bottom_ctl->getVaControlPanel() );
  _option_pull->addPushUp("tol", _tol);
  _option_pull->addSep();
  _option_pull->addPushUp("zop", _zoom_op_pop);
  _option_pull->addSep();
  _option_pull->addTog ("lock4");
  SLpToggle *lock4 = (SLpToggle*)_option_pull->primObj("lock4");
  _regulate->regulateLock(lock4);

  _option_pull->addSep();
  _option_pull->addTog ("select1");
  _option_pull->addPush("select2");
  _option_pull->addPush("select3");
  SLpToggle *select1 = (SLpToggle*)_option_pull->primObj("select1");
  SLpPush   *select2 = (SLpPush  *)_option_pull->primObj("select2");
  SLpPush   *select3 = (SLpPush  *)_option_pull->primObj("select3");
  _regulate->regulateSelect(select1, select2, select3);

  _option_pull->addSep();
  _option_pull->addTog("inform1");
  SLpToggle *inform1 = (SLpToggle*)_option_pull->primObj("inform1");
  _regulate->regulateInform(inform1);
  _option_pull->addSep();
  _option_pull->addTog("Use reference file for Gvs picking",GVS_REFERENCE_FILE);


// Edit Menu
  _edit_pull->addPushUp("list", _list);
  _edit_pull->addPushUp("pick", _pick);
  _edit_pull->addPushUp("fun" , _fun);
  _edit_pull->addPushUp("hlist" , _hor_list);
  _edit_pull->addPushUp("trans" , _trans);
  _edit_pull->addSep();
  _edit_pull->addPushUp("type", _type);
  _edit_pull->addPushUp("name", _name);
  _edit_pull->addPushUp("head", _head);
  _edit_pull->addPushUp("coor", _coor);
  _edit_pull->addPushUp("res" , _res);
  _edit_pull->addPushUp("lat" , _lat);
  _edit_pull->addPushUp("near", _near);
  _edit_pull->addPushUp("ray" , _ray);
  _edit_pull->addPushUp("mult", _mult);
  _edit_pull->addPushUp("misc", _misc);
  _edit_pull->addSep();
  _edit_pull->addPushUp("mute", _mute);
  _edit_pull->addSep();

// Window Menu
  _plot_ctl->addOptions(_window_pull); 
  
 // Plot menu
  _display_pull->addPush("zoom", ZOOM);
  _display_pull->addSep();
  _display_pull->addPushUp("Semblance Menu", 
                                          _plot_ctl->semblance()->getDialog());
  _display_pull->addPushUp("Gvs Menu",          _plot_ctl->gvs()->getDialog());
  _display_pull->addPushUp("CMP Menu",          _plot_ctl->cmp()->getDialog());
  _display_pull->addPushUp("Iso Velocity Menu", _plot_ctl->iso()->getDialog());
  _display_pull->addPushUp("Grid Menu",         _plot_ctl->grid()->getDialog());
  _display_pull->addPushUp("Crossplot Menu", 
                                           _plot_ctl->crossplot()->getDialog());
  _display_pull->addPushUp("Beta ETA Menu",     _plot_ctl->eta()->getDialog());

  _display_pull->setComplexNotify(this);

  //Headers menu
  _header_pull->setComplexNotify(this);
  _header_pull->addPushUp("Semblance Headers", _semblance_header_dump_pop, 
                          SEMBLANCE_HEADERS);
  _header_pull->addPushUp("Cmp Headers",       _cmp_header_dump_pop, 
                          CMP_HEADERS);
  _header_pull->addPushUp("Gvs Headers",       _gvs_header_dump_pop, 
                          GVS_HEADERS);

 // Tools menu
  _tools_pull->addPushUp("Header Shifts...",           _va_header_shift);
  _tools_pull->addPushUp("Nmc Output...",              _va_nmc_output);
  _tools_pull->addSep();
  _tools_pull->addPushUp("Iso Hardcopy...",       _hardcopy_iso);
  _tools_pull->addPushUp("Grid Hardcopy...",      _hardcopy_grid);
  _tools_pull->addPushUp("Gvs Hardcopy...",       _hardcopy_gvs);
  _tools_pull->addPushUp("Semblance Hardcopy...", _hardcopy_semblance);
  _tools_pull->addPushUp("Cmp Hardcopy...",       _hardcopy_cmp);
  _tools_pull->addPushUp("Crossplot Hardcopy...", _hardcopy_crossplot);
  _tools_pull->addPushUp("ETA Hardcopy...",       _hardcopy_eta);

  _tools_pull->addSep();
  _tools_pull->addPushUp("Semblance Color Bar...", _sem_cbar);
  _tools_pull->addPushUp("ISO Vel. Color Bar...", _iso_cbar);
  _tools_pull->addPushUp("ISO Contour Bar...", _iso_contour_cbar);
  _tools_pull->addPushUp("CMP Color Bar...", _cmp_cbar);
  _tools_pull->addPushUp("GVS Color Bar...", _gvs_cbar);
  _tools_pull->addSep();
  _tools_pull->addPushUp("Movie Control",         _movie_pop);
  _tools_pull->addSep();
  _tools_pull->addPushUp("Save Defaults...", _savdefpop);
  _tools_pull->addSep();
  _tools_pull->addPush("addlab",    ADDLABEL);
  _tools_pull->addPush("delalllab", DELALLLAB);



  //testing pull
  _testing_pull->addPush("Apply NMC",               APPLY_NMC);
  _testing_pull->addPush("Remove NMC",              REMOVE_NMC);
  _testing_pull->addPush("Movie Iso",               MOVIE_ISO);
  _testing_pull->addPush("Movie Gvs",               MOVIE_GVS);
  _testing_pull->addPush("Movie Semblance",         MOVIE_SEMBLANCE);
  _testing_pull->addPush("Movie Cmp",               MOVIE_CMP);
  _testing_pull->addPushUp("Overlay colors",        _color_pop);


 // View menu
  _view_pull->addPushUp("info", _info);
  _view_pull->addPushUp("hist", _hist);
  _view_pull->addPushUp("View Globals", _globals_pop);
  _view_pull->addSep();

 // POPUP menu
  _popup_menu->setComplexNotify(this);
  _popup_menu->addPush("Normal Semblance Picking", NORMAL_SEMBLANCE);
  _popup_menu->addPush("Beta ETA Picking",    ETA_SEMBLANCE);  
  _popup_menu->addSep();
  _popup_menu->addPush("zoom", ZOOM);
  _popup_menu->addPushUp("input",  _input_pop );
  _popup_menu->addPushUp("vacp",  _bottom_ctl->getVaControlPanel() );
  _popup_menu->addSep();
  _popup_menu->addPushUp("list", _list);
  _popup_menu->addPushUp("pick", _pick);
  _popup_menu->addPushUp("fun" , _fun);
  _popup_menu->addSep();
  _popup_menu->addPush("Copy Previous Inline",    COPY_PREV_INLINE);
  _popup_menu->addPush("Copy Next Inline",        COPY_NEXT_INLINE);
  _popup_menu->addPush("Copy Previous Crossline", COPY_PREV_XLINE);
  _popup_menu->addPush("Copy Next Crossline",     COPY_NEXT_XLINE);
  _popup_menu->addPush("Copy Reference",          COPY_REFERENCE);
  _popup_menu->addPush("Copy Comparison",         COPY_COMPARISON);
  _popup_menu->sensitive(False, "Copy Previous Inline", "Copy Next Inline",
                         "Copy Previous Crossline", "Copy Reference",
                         "Copy Comparison",
                         NULL);

  // Help menu
  _help_pull->addPush("apphelp", APPHELP);
  SLpPush *phelp = (SLpPush *)_help_pull->primObj(APPHELP);
  phelp->showHelpWhenPressed("APPLICATION_OVERVIEW");


//******************** Problem area  *******************************
  //VfguiStatus *status= new VfguiStatus(_main_form, _vf_manager, _clist, FALSE);
  //_main_form->attach(status, _main_form, _main_form, _main_form, 
  //                 _main_form, 30, 30, 10, 10);
//******************** End problem area  *******************************

          new VfguiWatch (_vf_manager, this);
  _quit = new VfguiQuit  (this, _save, _vf_manager);



}


void VaApplication::finalInit()
{
   addMessageModeXYOut( _plot_ctl->semblance()->SP(), "Mode: Plot Semblance");
   addMessageModeXYOut( _plot_ctl->gvs()->SP(),       "Mode: Plot GVS");
   addMessageModeXYOut( _plot_ctl->cmp()->SP(),       "Mode: Plot CMP");
   addMessageModeXYOut( _plot_ctl->iso()->SP(),       "Mode: Plot ISO Vel");
   addMessageModeXYOut( _plot_ctl->grid()->SP(),      "Mode: Plot Grid");
   addMessageModeXYOut( _plot_ctl->crossplot()->SP(), "Mode: Plot Crossplot");
   addMessageModeXYOut( _plot_ctl->eta()->SP(),       "Mode: Plot ETA");

   if (_resources.auto_size) {
           Screen *scr;
           scr= DefaultScreenOfDisplay(XtDisplay( _main_form->W() ) );
           Dimension width=  (Dimension)(WidthOfScreen( scr) * .9);
           Dimension height= (Dimension)(HeightOfScreen( scr) * .85);
           XtVaSetValues( _main_form->W(), XmNheight, height,
                                           XmNwidth,  width, NULL );
   }

}
void VaApplication::realizing()
{
  _testing_pull->unmapCascade();
  
  if (_resources.sem_file) _input_pop->setSemFile(_resources.sem_file);
  if (_resources.cmp_file) _input_pop->setCMPFile(_resources.cmp_file);
  if (_resources.gvs_file) _input_pop->setGVSFile(_resources.gvs_file);
  if (_resources.vel_file) _input_pop->setVelFile(_resources.vel_file);

 
  if (_resources.sem_file  ||
      _resources.cmp_file  ||
      _resources.gvs_file  ||
      _resources.vel_file) {
          delete _version_info;
          _version_info= NULL;
          _input_pop->plotNow();
  }

}

void VaApplication::handlePopupMenuMap(void *instance) {

    if (instance) {
        (static_cast<VaApplication *> (instance))->handlePopupMenuMap();
    }
}

void VaApplication::handlePopupMenuMap() {
    if (_plot_ctl->isShowing(VaPlotControl::SEM)) {
        _popup_menu->sensitive(True, "Copy Previous Inline", "Copy Next Inline",
                               "Copy Previous Crossline", "Copy Next Crossline",
                               NULL);
        long semb_opt = VaSemblancePlot::SEMB_COPY_PREV_INLINE;
        if (_plot_ctl->semblance()->getSourceVelocityFuncIndex(semb_opt) < 0) {
            _popup_menu->sensitive(False, "Copy Previous Inline", NULL);
        }
        else {
            _popup_menu->sensitive(True, "Copy Previous Inline", NULL);
        }
        semb_opt = VaSemblancePlot::SEMB_COPY_NEXT_INLINE;
        if (_plot_ctl->semblance()->getSourceVelocityFuncIndex(semb_opt) < 0) {
            _popup_menu->sensitive(False, "Copy Next Inline", NULL);
        }
        else {
            _popup_menu->sensitive(True, "Copy Next Inline", NULL);
        }
        semb_opt = VaSemblancePlot::SEMB_COPY_PREV_XLINE;
        if (_plot_ctl->semblance()->getSourceVelocityFuncIndex(semb_opt) < 0) {
            _popup_menu->sensitive(False, "Copy Previous Crossline", NULL);
        }
        else {
            _popup_menu->sensitive(True, "Copy Previous Crossline", NULL);
        }
        semb_opt = VaSemblancePlot::SEMB_COPY_NEXT_XLINE;
        if (_plot_ctl->semblance()->getSourceVelocityFuncIndex(semb_opt) < 0) {
            _popup_menu->sensitive(False, "Copy Next Crossline", NULL);
        }
        else {
            _popup_menu->sensitive(True, "Copy Next Crossline", NULL);
        }
        semb_opt = VaSemblancePlot::SEMB_COPY_REFERENCE;
        if (_plot_ctl->semblance()->getSourceVelocityFuncIndex(semb_opt) < 0) {
            _popup_menu->sensitive(False, "Copy Reference", NULL);
        }
        else {
            _popup_menu->sensitive(True, "Copy Reference", NULL);
        }
        semb_opt = VaSemblancePlot::SEMB_COPY_COMPARISON;
        // editable velocity dataset are loaded into the beginning locations
        // of the velocity arrays. comparison velocity datasets, if any, are
        // after the editable datasets. see comments in VfManager.
        // if there are no comparison datasets, do not sensitize the popup
        // menu button for copying the comparison dataset.
        if (_vf_manager->numDatasets() - 
            _vf_manager->numEditableDatasets() < 1) {
            _popup_menu->sensitive(False, "Copy Comparison", NULL);
        }
        else {
            _popup_menu->sensitive(True, "Copy Comparison", NULL);
        }
    }
    else {
        _popup_menu->sensitive(False, "Copy Previous Inline", "Copy Next Inline",
                               "Copy Previous Crossline", "Copy Next Crossline",
                               NULL);
    }
}


void VaApplication::closeMain()
{
  _quit->askQuitQuestion();
}




void VaApplication::calledFromFile()
{
  if(_version_info != NULL) {
     delete _version_info;
     _version_info = NULL;
  }

  switch ( _file_pull->lastIdent() ) {
      case QUIT:
                 _exiting= True;
                 _quit->askQuitQuestion();
                 break;
  }
}

void VaApplication::calledFromPopup()
{
  long copy_opt;
  switch ( _popup_menu->lastIdent() ) {
      case ZOOM:  
                _bottom_ctl->multiZoomUpSeparateWindow();
                break;
      case NORMAL_SEMBLANCE:
                _plot_ctl->eta()->setActive(False);
                _bottom_ctl->manageEta(False);
                break;
      case ETA_SEMBLANCE:
               //Use the following if we decide we dont want to manage 
               //the eta menu everytime
               //if(_plot_ctl->eta()->managed() == False)
                _plot_ctl->eta()->manageGui();
                _plot_ctl->eta()->setActive(True);
                _bottom_ctl->manageEta(True);
                break;
      case COPY_PREV_INLINE:
                copy_opt = VaSemblancePlot::SEMB_COPY_PREV_INLINE;
                _plot_ctl->semblance()->copySourceVelocityFunc(copy_opt);
                break;
      case COPY_NEXT_INLINE:
                copy_opt = VaSemblancePlot::SEMB_COPY_NEXT_INLINE;
                _plot_ctl->semblance()->copySourceVelocityFunc(copy_opt);
                break;
      case COPY_PREV_XLINE:
                copy_opt = VaSemblancePlot::SEMB_COPY_PREV_XLINE;
                _plot_ctl->semblance()->copySourceVelocityFunc(copy_opt);
                break;
      case COPY_NEXT_XLINE:
                copy_opt = VaSemblancePlot::SEMB_COPY_NEXT_XLINE;
                _plot_ctl->semblance()->copySourceVelocityFunc(copy_opt);
                break;
      case COPY_REFERENCE:
                copy_opt = VaSemblancePlot::SEMB_COPY_REFERENCE;
                _plot_ctl->semblance()->copySourceVelocityFunc(copy_opt);
                break;
      case COPY_COMPARISON:
                copy_opt = VaSemblancePlot::SEMB_COPY_COMPARISON;
                _plot_ctl->semblance()->copySourceVelocityFunc(copy_opt);
                break;
  }
}



void VaApplication::calledFromOption() {}
void VaApplication::calledFromWindow() {}
void VaApplication::calledFromAccelerator() {}
void VaApplication::calledFromHelp()   {}

void VaApplication::calledFromTools()  
{
  switch(_tools_pull->lastIdent()) {
    case ADDLABEL:      _label->insertMoveOneLabel();                  break;
    case DELALLLAB:     _label->deleteAllLabels();                     break;
  }
}

void VaApplication::calledFromDisplay() 
{
  switch ( _display_pull->lastIdent() ) {
       case ZOOM:
             _bottom_ctl->multiZoomUpSeparateWindow(); 
             break;
  }

  if( !strcmp(_display_pull->lastName(), "Beta ETA Menu") )
    _bottom_ctl->manageEta(True);


}


// Temporary testing garbage
void VaApplication::calledFromTesting(int ident)
{
    VaPlot *va_iso_plot      = _plot_ctl->iso();
    VaPlot *va_grid_plot     = _plot_ctl->grid();
    VaPlot *va_semblance_plot= _plot_ctl->semblance();
    VaPlot *va_gvs_plot      = _plot_ctl->gvs();
    VaPlot *va_cmp_plot      = _plot_ctl->cmp();
    VaPlot *va_crossplot_plot= _plot_ctl->crossplot();
    VaPlot *va_eta_plot      = _plot_ctl->eta();

  switch(ident)
    {
    case APPLY_NMC:
      ((VaCmpPlot*)va_cmp_plot)->applyForwardMoveout();
    break;
    case REMOVE_NMC:
      ((VaCmpPlot*)va_cmp_plot)->applyReverseMoveout();
    break;
    }
}


Boolean VaApplication::notifyComplex(SLDelay *obj, int ident)
{
   if      (obj == _file_pull)    calledFromFile();
   else if (obj == _display_pull) calledFromDisplay();
   else if (obj == _quit)         _quit->askQuitQuestion();
   else if (obj == _testing_pull) calledFromTesting(ident);
   else if (obj == _window_pull)  _plot_ctl->calledFromWindow(ident);
   else if (obj == _tools_pull)   calledFromTools();
   else if (obj == _popup_menu)   calledFromPopup();
   else if (obj == _option_pull)  
            _plot_ctl->gvs()->setGvsReferenceFileOption(
                   _option_pull->toggleValue(GVS_REFERENCE_FILE));

   return True;
}
