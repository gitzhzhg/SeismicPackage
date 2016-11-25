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
#ifndef  CFGAPP_HH
#define  CFGAPP_HH


#include <Xm/Xm.h>
#include "sl/sl_app.hh"


class FieldGeometry;
class FgWatchInform;

class SLQuestPop;
class SLPullPop;
class SLFormPop;
class SLForm;
class SLpFileData;
class VersionInfo;

class FgSeisPlot;
class FgSeisPlotList;

class FgShellMsg;
class FgControlPop;
class TransformPop;

class FgXp2DPop;
class FgXp3DPop;
class Fg2DPop;
class FgQcPop;
class FgQcOvjdPlot;
class FgMap;

class FgHeaders;
class LinelistPop;
class LdPop;
class LdEditPop;
class RpPop;
class PpPop;
class ZtPop;
class FgTePop;
class GroupsPop;
class MidpointsPop;
class HeaderPop;
class FgMissPop;

class JdReadPop;
class JdSavePop;
class SurveyReadPop;
class SurveySavePop;

class CfgGetLoadDefs;

class ContainerList;

#define CFG_VERSION  "cfg - Version: 2.0, Revision: 45 August 14, 2006"
#define CFG_VERSION_WIDGET "version_2_0"
#define CFG_INFO "\n------------------------------------------------\n\
New :\n\
     1. Added support for fold maps with folds greater than 254.\n\
\n\
Bugs Fixed:\n\
     1. None.\n\
\n\
\n\
"


typedef struct _appres {
                         char    *graph_font;    // font to use in graph
                         Boolean private_cmap;   // use a private color map
//                       long    num_col_colors; // num color colors
                         char    *help_file;     // name of help file
                         Boolean frame_buff;     // server has frame buff limit
                         Boolean showhelp;       // show help for options
                         Boolean dobacking;      // do backing store
                         char    *def_file;      // defaults file to use
                      } cfg_res;


class CfgApp : public SLApp {

  private:
    // general 
          Pixmap          _icon;
          Pixmap          _mask;
          cfg_res         _resources; 
          char           *_version;
          VersionInfo    *_version_info;
          FieldGeometry  *_fg;
          FgWatchInform  *_watch_inform;
          FgShellMsg     *_messages;
          FgSeisPlotList *_fgsp_list;
          ContainerList  *_all_pops;
          SLFormPop      *_plot_in_main;
          FgSeisPlot     *_fgsp_in_main;
          SLForm         *_main_form;
          Boolean        _expecting_save_pop;

    // SL classes
          SLPullPop       *_file_pull;
          SLPullPop       *_option_pull;
          SLPullPop       *_table_pull;
          SLPullPop       *_zoom_pull;
          SLPullPop       *_plots_pull;
          SLPullPop       *_inmain_cas;
          SLPullPop       *_help_pull;
          SLPullPop       *_plot_specific_pull;
          SLQuestPop      *_question;
          SLpFileData     *_savdefdata;
          CfgGetLoadDefs  *_savdefpop;

    // Control classes
          FgControlPop   *_dcp; 
          TransformPop   *_tp;

    // Plot classes
          FgXp2DPop      *_2dxp;
          FgXp3DPop      *_3dxp;
          FgQcPop        *_qcp;
          FgQcOvjdPlot   *_ovjd;
          FgMap          *_map;
          Fg2DPop        *_chart;

    // Plot in main
          FgXp2DPop      *_2dxp_main;
          FgXp3DPop      *_3dxp_main;
          FgQcPop        *_qcp_main;
          FgQcOvjdPlot   *_ovjd_main;
          FgMap          *_map_main;
          Fg2DPop        *_chart_main;


    // Table classes
          LinelistPop  *_lines;
          LdEditPop    *_ld_edit;
          LdPop        *_ld;
          LdPop        *_ld_qc;
          RpPop        *_rp;
          PpPop        *_pp;
          ZtPop        *_zt1;
          ZtPop        *_zt2;
          ZtPop        *_zt3;
          ZtPop        *_zt4;
          FgTePop      *_tred;
          GroupsPop    *_groups;
          MidpointsPop *_midpoint;
          HeaderPop    *_head_pop;
          FgMissPop    *_miss_pop;

    // IO classes
          JdReadPop     *_jd_read_pop;
          JdSavePop     *_jd_save_pop;
          SurveyReadPop *_survey_read_pop;
          SurveySavePop *_survey_save_pop;

    // methods
          void init();
          void createPullDowns();
          void createPopups();
          void createMainArea();
          void finalInit();
          void showHelp();
          void setWinTitle();
          void saveAndExit(Boolean exit_now);
          virtual void realizing();

    // methods from pulldowns
          void calledFromFile();
          void calledFromOption();
          void calledFromTable();
          void calledFromZoom();
          void calledFromPlots();
          void calledFromInmain(int which_set);
          void calledFromHelp();
  protected:
          Boolean notifyComplex(SLDelay *obj, int ident);

  public:
          CfgApp( int &argc, char **argv =NULL);
          ~CfgApp();
          virtual void closeMain();
};

#endif
