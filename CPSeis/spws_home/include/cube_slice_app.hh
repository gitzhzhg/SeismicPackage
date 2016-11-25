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
#ifndef CUBE_SLICE_APP_HH
#define CUBE_SLICE_APP_HH

#include <Xm/Xm.h>
#include "sl/sl_app.hh"
#include "cube/cube_inform.hh"

class SLPullPop;
class SLForm;
class VersionInfo;
class CubeDisplay;
class CubeSelect;
class CubeAnnotationGui;
class CubeSectionGui;
class CubeAmplitudeGui;
class ShellMouseHelp;
class CubeMovieControl;
class CubeMovieControl;
class CubeTableShowGui;


#define CUBE_VERSION  "Cube Slice - Version: 2.0, Revision: 9 June 5, 2006"
#define CUBE_VERSION_WIDGET "version_2_0"
#define CUBE_INFO "\n------------------------------------------------\n\
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
                         int      num_col_colors;
                         char    *def_file;      // defaults file to use
                      } cs_res;

class CubeSliceApp : public SLApp, public CubeInform {

  public:
          enum {MAX_WINDOWS=30};
  private:

    // statics shared by all instances
          static Boolean       _static_initialized;
          static int           _instance_cnt;
          static CubeSliceApp *_cube_graph[MAX_WINDOWS];
          static Pixmap        _icon;
          static Pixmap        _mask;
          static cs_res        _resources;

     // general
          char           *_version;
//        Boolean         _exiting;
          Boolean         _cando_colors;
          long            _total_to_alloc;
          VersionInfo    *_version_info;
          ShellMouseHelp *_sm1;
          ShellMouseHelp *_sm2;


    // methods
          void doStatics();
          void doColor();
          void createPullDowns();
          void createMainArea();
          void createPopups();
          void finalInit();
          void showHelp();
          void setWinTitle();

     // SL classes
          SLPullPop       *_file_pull;
          SLPullPop       *_option_pull;
          SLPullPop       *_zoom_pull;
          SLPullPop       *_tools_pull;
          SLPullPop       *_window_pull;
          SLPullPop       *_custom_pull;
          SLPullPop       *_help_pull;
          SLPullPop       *_hidden_pull;
          SLPullPop       *_popup_menu;
          SLPullPop       *_screen_cas;
          SLPullPop       *_xh_cas;
          SLForm          *_main_form;


     // Cube classes
          CubeDisplay        *_cube_display;
          CubeSelect         *_cube_select;
          CubeAnnotationGui  *_cube_anno;
          CubeSectionGui     *_cube_section;
          CubeAmplitudeGui   *_cube_amp;
          CubeTableShowGui   *_cube_table;

    // methods from pulldowns or buttons on bottom
          void calledFromFile();
          void calledFromOption();
          void calledFromZoom();
          void calledFromWindow();
          void calledFromCustom();
          void calledFromHelp();
          void calledFromTools();
          void calledFromAccelerator();
    // many object call back to the function through notifyComplex
          Boolean notifyComplex(SLDelay *obj, int ident);

    // call when the program is realized
          virtual void realizing();

    // misc
          void setDeleteSensitive();
          void setTitleToFile(Cube *cube);


  public:
          CubeSliceApp( int &argc, char **argv =NULL);
          CubeSliceApp( CubeSliceApp *other, int screen =UseResource);
          ~CubeSliceApp();
          virtual void closeMain();
          virtual void cubeIsCurrent(Cube *);
          virtual void newCubeCreated(Cube *);
          virtual void postPlot(Cube *, int, int, int);
          virtual void destroyed (Cube *);
};

#endif
