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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================





//===========================================================================
//========  Main application class for the velocity analysis program ========
//========  Author Trey Roby 08/97                                   ========
//===========================================================================

#ifndef VA_APPLICATION_HH
#define VA_APPLICATION_HH

// $Id: va_application.hh,v 1.9 2006/06/05 14:45:41 spws Exp $
// $Name: 6-5-6b $

#include <Xm/Xm.h>
#include "sl/sl_app.hh"
#include "vf/vf_inform.hh"

class SLPullPop;
class SLForm;
class VersionInfo;
class VaIsoPlot;
class VaGridPlot;
class VaSemblancePlot;
class VaGvsPlot;
class VaCmpPlot;
class VaIsoPlot;
class VaGridPlot;
class VaEtaPlot;
class VfManager;
class VfguiQuit;
class WinCtl;
class TemporaryPop;
class SeisLocOut;
class ViewWin;
class ContainerList;
class VfguiRegulate;
class VfpopRead;
class VfpopSave;
class VfpopList;
class VfpopPick;
class VfpopFun;
class VfpopTypes;
class VfpopNames;
class VfpopHeaders;
class VfpopCoords;
class VfpopResample;
class VfpopLatsample;
class VfpopNearsample;
class VfpopRaytrace;
class VfpopDelete;
class VfpopMultiply;
class VfpopMisc;
class VfpopMute;
class VfpopInfo;
class VfpopHistory;
class VfpopTol;
class ColorPop;		/* ehs test */
class VaPlotCommonParams;
class VaVectColors;
class VaInputPop;
class VaPlotControl;
class VaHeaderShift;
class VaNmcOutput;
class SeisPlot;
class SLDefAppPop;
class SLpFileData;
class VaMoviePop;
class SeisZoomOpPop;
class SeisCbarPop;
class SeisContourCbarPop;
class VaBottomControl;
class SLAsciiViewer;
class HardCopyPop;
class VfHorizons;
class VfpopHorizonRead;
class VfpopHorizonList;
class VfpopTransform;
class SeisLabel;
class HeaderDumpPop;



#define VA_VERSION  "Va Version: 2.1, Revision: 20 June 5, 2006"
#define VA_VERSION_WIDGET "version_2"
#define VA_INFO "\n------------------------------------------------\n\
New:\n\
     1. Added support for LBO version 2 (FNIL).\n\
\n\
Bugs Fixed:\n\
     1. None.\n\
\n\
\n\
     "



typedef struct _appres {
                         char    *graph_font;    // font to use in graph
                         Boolean private_cmap;   // use a private color map
                         char    *help_file;     // name of help file
                         Boolean showhelp;       // show help for options
                         Boolean dobacking;      // do backing store
                         int     num_sem_colors;
                         int     num_iso_colors;
                         int     num_gvs_colors;
                         int     num_cmp_colors;
                         Boolean no_iso_underlay; // disable iso underlay plots
                         Boolean auto_size;       // size to 85% of screen

                         char    *sem_file;    // semblance file
                         char    *cmp_file;    // CMP file
                         char    *gvs_file;    // GVS file
                         char    *vel_file;    // velocity file

                         char    *def_file;      // defaults file to use
                      } va_res;

class VaApplication : public SLApp {

  public:
          VaApplication( int &argc, char **argv =NULL);
          ~VaApplication();
          virtual void closeMain();

          enum {MAX_WINDOWS=30};

          VfManager *vfManager(){return _vf_manager;}
          VaPlotCommonParams *getCommonParams(){return _common_params;}

          enum {ADDFILE, QUIT, ZOOM, APPHELP = 77,//File pulldown or popup menu
                SEMBLANCE_MENU, GVS_MENU, CMP_MENU, ISO_MENU, GRID_MENU,
                CROSSPLOT_MENU,                     //Plot pulldown
                APPLY_NMC, REMOVE_NMC, MOVIE_ISO, MOVIE_GVS, 
                MOVIE_SEMBLANCE, MOVIE_CMP, COLOR_POP, ADD_GRAPH, ADDLABEL,
                DELALLLAB, HEADER_SHIFTS, NMC_OUTPUT,  // tools  pulldown
                GVS_REFERENCE_FILE,                    // on option pulldown
                NORMAL_SEMBLANCE, ETA_SEMBLANCE,
                SEMBLANCE_HEADERS, CMP_HEADERS, GVS_HEADERS,
                COPY_PREV_INLINE, COPY_NEXT_INLINE,
                COPY_PREV_XLINE, COPY_NEXT_XLINE, COPY_REFERENCE,
                COPY_COMPARISON
          };  


  protected:
          VfManager           *_vf_manager;  
          VfHorizons          *_horizons;
          VfguiQuit           *_quit;
          ContainerList       *_clist;
          VfguiRegulate       *_regulate;
          char                *_helpfile;
          VaPlotCommonParams  *_common_params;

          void addMessageModeXYOut(SeisPlot *sp, char *modestr);

  private:

          VaPlotControl       *_plot_ctl;
          VaInputPop          *_input_pop;
          VaHeaderShift       *_va_header_shift;
          VaNmcOutput         *_va_nmc_output;
          SLDefAppPop         *_savdefpop;
          VaMoviePop          *_movie_pop;

    // Ed's picker colors
          VaVectColors        *_vect_colors;
    // Plot classes
          
    // statics shared by all instances
          static Boolean       _static_initialized;
          static int           _instance_cnt;
          static Pixmap        _icon;
          static Pixmap        _mask;
          static va_res        _resources;

     // general
          char               *_version;
//        Boolean             _exiting;
          Boolean             _cando_colors;
          Boolean             _cando_underlay;
          long                _total_to_alloc;
          VersionInfo        *_version_info;
          VaBottomControl    *_bottom_ctl;
          SeisZoomOpPop      *_zoom_op_pop;
          SeisCbarPop        *_sem_cbar;
          SeisCbarPop        *_iso_cbar;
          SeisContourCbarPop *_iso_contour_cbar;
          SeisCbarPop        *_gvs_cbar;
          SeisCbarPop        *_cmp_cbar;
          HardCopyPop        *_hardcopy_iso;
          HardCopyPop        *_hardcopy_grid;
          HardCopyPop        *_hardcopy_gvs;           
          HardCopyPop        *_hardcopy_semblance;
          HardCopyPop        *_hardcopy_cmp; 
          HardCopyPop        *_hardcopy_crossplot;
          HardCopyPop        *_hardcopy_eta;
          SeisLabel          *_label;
          HeaderDumpPop      *_semblance_header_dump_pop;
          HeaderDumpPop      *_cmp_header_dump_pop;
          HeaderDumpPop      *_gvs_header_dump_pop;

   // methods
          void doStatics();
          void doColor();
          void createPullDowns();
          void createMainArea();
          void createPopups();
          void finalInit();
          void showHelp();


     // SL classes
          SLPullPop       *_file_pull;
          SLPullPop       *_option_pull;
    //    SLPullPop       *_zoom_pull;
          SLPullPop       *_display_pull;
          SLPullPop       *_edit_pull;
          SLPullPop       *_window_pull;
          SLPullPop       *_view_pull;
          SLPullPop       *_testing_pull;
          SLPullPop       *_header_pull;
          SLPullPop       *_tools_pull;
          SLPullPop       *_help_pull;
          SLPullPop       *_popup_menu;
          SLForm          *_main_form;
          SLpFileData     *_savdefdata;
          SLAsciiViewer   *_globals_pop;
          SLpFileData     *_globals_data;
          SLpFileData     *_hc_iso_data;
          SLpFileData     *_hc_grid_data;
          SLpFileData     *_hc_gvs_data;
          SLpFileData     *_hc_cmp_data;
          SLpFileData     *_hc_xp_data;
          SLpFileData     *_hc_sem_data;
          SLpFileData     *_hc_eta_data;

    // Tom's Vfgui classes
          VfpopRead        *_read;
          VfpopSave        *_save;
          VfpopList        *_list;
          VfpopPick        *_pick;
          VfpopFun         *_fun;
          VfpopTypes       *_type;
          VfpopNames       *_name;
          VfpopHeaders     *_head;
          VfpopCoords      *_coor;
          VfpopResample    *_res;
          VfpopLatsample   *_lat;
          VfpopNearsample  *_near;
          VfpopRaytrace    *_ray;
          VfpopDelete      *_del;
          VfpopMultiply    *_mult;
          VfpopMisc        *_misc;
          VfpopMute        *_mute;
          VfpopInfo        *_info;
          VfpopHistory     *_hist;
          VfpopTol         *_tol;
          VfpopHorizonRead *_hor_read;
          VfpopHorizonList *_hor_list;
          VfpopTransform   *_trans;
          ColorPop         *_color_pop;		/* ehs test */

    // methods from pulldowns or buttons on bottom
          void calledFromFile();
          void calledFromTesting(int ident);
          void calledFromDisplay();
          void calledFromOption();
 //       void calledFromZoom();
          void calledFromWindow();
          void calledFromHelp();
          void calledFromTools();
          void calledFromAccelerator();
          void calledFromPopup();
    // many object call back to the function through notifyComplex
          Boolean notifyComplex(SLDelay *obj, int ident);

    // call when the program is realized
          virtual void realizing();

    /*! \name Popup Menu Mapping
        Interface for actions required as popup menu is mapped to screen.
     */
    //@{
    //! Receives doMapCallback action as just before map occurs.
          static void handlePopupMenuMap(void *instance);
    //! Action of static version passed to here. Sets button sensitivites
    //! on popup menu prior to mapping.
          void handlePopupMenuMap();
    //@}


};

#endif
