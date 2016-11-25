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
#ifndef  CBYTWINDOW_H
#define  CBYTWINDOW_H


#include <Xm/Xm.h>
#include "sl/sl_app.hh"


class SeisPlot;
class SeisPlotTie;
class SeisUnderPop;
class SeisPlotUnder;
class SeisControl;
class SeisSelectIpcPop;
class SeisColorPop;
class SeisZoomOpPop;
class SeisTiePop;
class SeisDrag;
class SlavePop;
class SlaveDisplayLinkedList;
class CbytInform;
class SeisCbarPop;
class SeisInfoPop;
class SeisOvjd;
class SeisOvjdPop;
class SeisAvast;
class SeisAvastPop;
class SeisShift;
class SeisShiftPop;
class SeisHeaderPop;
class SeisMultiPlotControl;
class SeisXYOutPop;
class SeisAllHeaderPop;


class SLPullPop;
class SLForm;
class SLDefPop;
class SLAsciiViewer;
class SLDefAppPop;
class SLpFileData;
class HeaderDumpPop;
class VersionInfo;
class TpMutePop;
class TpMuteShiftPop;
class TpRefPop;
class TpGenPop;
class TpSiscPop;
class TpFishPop;  
class TpCc3dPop;  
class TredTablePop;
class FkPop;
class SeisLabel;
class StpPopupBase;
class ShellMouseHelp;
class SeisVelocity;
class SendPick;
class HardCopyPop;
class SpectraMenu;
class SeisLavPop;

#define CBYT_VERSION  "Open Source Cbyt - Version: 3.1, Revision: 01 Dec 31, 2009"
#define CBYT_VERSION_WIDGET "version_3_1"
#define CBYT_INFO "\n------------------------------------------------\n\
New:\n\
     1. JavaSeis I/O was disabled.\n\
\n\
Bugs Fixed:\n\
\n\
\n\
"


typedef struct _appres {
                         char    *graph_font;    // font to use in graph
                         Boolean private_cmap;   // use a private color map
                         long    num_col_colors; // num color colors
                         char    *help_file;     // name of help file
                         Boolean frame_buff;     // server has frame buff limit
                         Boolean wigonly;        // only do wiggle traces
                         Boolean no_underlay;    // disable underlay plots
                         Boolean showhelp;       // show help for options
                         Boolean dobacking;      // do backing store
                         char    *def_file;      // defaults file to use
                         Boolean vdynamic_cmap;  // use a dynamic color map
                         char    *input_file;    // seismic file
                         char    *channel;       // IPC channel
                         char    *servers;       // servers needed
                      } cbyt_res;


class CbytWindow : public SLApp {

  friend class CbytInform;
  public:
          enum {MAX_WINDOWS=30};
  protected:
    // statics shared by all instances
          static Boolean _static_initialized;
          static int     _instance_cnt;
          static CbytWindow *_mother;
          static CbytWindow *_cbyt_graph[MAX_WINDOWS];
          static Pixmap    _icon;
          static Pixmap    _mask;
          static cbyt_res  _resources; 

    // general 
          char        *_version;
//        Boolean      _exiting;
          Boolean      _cando_colors;
          Boolean      _cando_underlay;
          long         _total_to_alloc;
          int          _scan_action;
          int          _scan_sensitivity;
          VersionInfo *_version_info;
          ShellMouseHelp *_sm1;
          ShellMouseHelp *_sm2;
          class IpcServer *_ipc_server;
          class IpcIO *_ipc_io;

    // SeisPlot classes & classes using SeisPlot
          SeisPlotTie            *_sp;
          SeisPlotUnder          *_sp_under;
          SeisPlot               *_tie_sp;
          SeisPlotUnder          *_tie_sp_under;
          SeisPlot               *_header_graph;

          SeisMultiPlotControl   *_multi_file;
          SeisControl            *_control_area;
          SeisSelectIpcPop       *_sp_pop;
          SeisUnderPop           *_sp_under_pop;
          SeisTiePop             *_tie_pop;
          SeisUnderPop           *_tie_under_pop;
          SeisColorPop           *_color_pop;
          SeisCbarPop            *_cbar;
          SeisInfoPop            *_plot_info;
          SeisZoomOpPop          *_zoom_op_pop;
          SeisHeaderPop          *_header_graph_pop;
          SeisDrag               *_drag_under;
          SlavePop               *_slave_pop;
          SlaveDisplayLinkedList *_slave_list;
          CbytInform             *_inform;
          SLDefAppPop            *_getdefpop;
          SLDefAppPop            *_savdefpop;
          HardCopyPop            *_hard_pop;
          SeisXYOutPop           *_xyout_pop;
          SeisAllHeaderPop       *_all_headers_pop;
          SpectraMenu            *_spectra_menu;
          SeisLavPop             *_lav_pop;


    // SL classes
          SLPullPop        *_file_pull;
          SLPullPop        *_option_pull;
          SLPullPop        *_zoom_pull;
          SLPullPop        *_head_pull;
          SLPullPop        *_tool_pull;
          SLPullPop        *_pick_pull;
          SLPullPop        *_custom_pull;
          SLPullPop        *_units_cas;
          SLPullPop        *_help_pull;
          SLPullPop        *_popup_menu;
          SLPullPop        *_vel_cas;
          SLPullPop        *_screen_cas;
          SLForm           *_main_form;
          SLAsciiViewer    *_global_pop;
          SLpFileData      *_global_data;
          SLpFileData      *_hard_data;
          SLpFileData      *_get_def_data;
          SLpFileData      *_save_def_data;


    // Picking classes
          SeisOvjd         *_ovjd;
          SeisOvjdPop      *_ovjd_pop;
          SeisAvast        *_avast;
          SeisAvastPop     *_avast_pop;
          SeisLabel        *_label;
          HeaderDumpPop    *_header_dump_pop;
          SeisShift        *_seis_shift;
          SeisShiftPop     *_seis_shift_pop;
          TpMutePop        *_mute_pop;
          TpMuteShiftPop   *_mute_shift_pop;
          TpRefPop         *_ref_pop;
          TpGenPop         *_gen_pop;
          TpSiscPop        *_sisc_pop;
          TpFishPop        *_fish_pop;
          TpCc3dPop        *_cc3d_pop;
          TredTablePop     *_tred_pop;
          FkPop            *_fk_pop  ;
          SeisVelocity     *_seis_velocity;
          SendPick         *_send_pick;
          SLShellContainer *_current_pick_pop; // active picking class

    // methods
          void doStatics();
          void doColor();
          void createPullDowns();
          void createMainArea();
          void createPopups(CbytWindow *other);
          void finalInit();
          void showHelp();
          void setWinTitle();
          void deleteSelf();

    // methods from pulldowns or buttons on bottom
          void calledFromFile();
          void calledFromOption();
          void calledFromHeader();
          void calledFromZoom();
          void calledFromTools();
          void calledFromPick();
          void calledFromCustom();
          void calledFromHelp();
          void calledFromPopup();
          void calledFromBottom(long);
          void calledFromUnits(int units);
          void calledFromVelocity();
          void calledFromSendPick();
          static void calledFromBottomStat(void *obj,long);

    // many object call back to the function through notifyComplex
          Boolean notifyComplex(SLDelay *obj, int ident);

    // call when picking starts, stops, or changes 
          void pickAction(long ident, int picking);

    // called by picking when flatten, unflatten or snap options change
          static void pickingUpdate(int which, void *data);

    // sets the sensitivity of the picking while scanning option button
          void setPickingSensitivity(int which);

    // sets the label for picking scanning actions
          void setPickingScan();

    // call when avast starts or stops 
          void avastAction();

    // call when the program is realized
          virtual void realizing();

  public:
          CbytWindow
            (int &argc,
             char **argv = NULL,
             class IpcIO *ipc_io = NULL);

          CbytWindow
            (CbytWindow *win, 
             Boolean color = True, 
             int screen = UseResource);

          ~CbytWindow ();

          virtual void closeMain ();
};

#endif
