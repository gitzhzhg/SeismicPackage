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
#ifndef CUBEDISPLAY_HH
#define CUBEDISPLAY_HH

#include "cube/cube_inform.hh"
#include "cube/cube.hh"
#include "sl/sl_form.hh"

class CubeList;
class CubeVisitor;
class Cube;
class ViewWin;
class WinCtl;
class SeisColorPop;
class SeisCbarPop;
class MasterCubeList;
class CubeMovieControl;
class CubeWireFrame;
class SeisPlot;
class HardCopySeisPop;
class SLpFileData;
class SeisLocOut;
class SLForm;
class CubeOverlays;
class SLPushBox;
class SLShellContainer;
class SeisZoomOpPop;
class CubeRandomLinePop;

class CubeDisplay : public CubeInform, public SLForm {

private:

protected:
  Cube             *_curr_displayed_cube;
  Cube             *_first_cube;
  SeisPlot         *_wire_sp;
  CubeWireFrame    *_cube_wf;
  CubeOverlays     *_cube_overlay;
  SeisZoomOpPop    *_zoom_op;
  SLForm           *_form;
  WinCtl           *_winctl;
  ViewWin          *_inline;
  ViewWin          *_xline;
  ViewWin          *_ts;
  ViewWin          *_wireframe;
  SeisLocOut       *_xyout;
  SeisColorPop     *_color_pop;
  MasterCubeList   *_master_list;
  Boolean           _lock_scrolling;
  Boolean           _restore_locking;
  Boolean           _sync_slices;
  int               _colors;
  int               _norm_type;
  CubeMovieControl *_cube_movie_control;
  CubeRandomLinePop *_cube_random_line_pop;
  Cube             *_validation_cube;
  SLpFileData      *_inline_hard_data;
  SLpFileData      *_xline_hard_data;
  SLpFileData      *_ts_hard_data;
  HardCopySeisPop  *_inline_hardpop;
  HardCopySeisPop  *_xline_hardpop;
  HardCopySeisPop  *_ts_hardpop;
  SLPushBox        *_bottom_push;
  SeisCbarPop      *_cbar;
  
  void syncSlices(Cube *oldcube);

  void transferColorParams(SeisPlot *insp, SeisPlot *xsp);

public:
  CubeDisplay(Widget p, char *name, int colors);
  ~CubeDisplay();
  Cube *newCube();
  Cube *currentDisplayedCube();

  // -- control which cube is displayed
  Cube *displayNextCube();
  Cube *displayPrevCube();
  Boolean displayCube(Cube *cube = NULL);
  void lockScrolling(Boolean lock);
  void showOverlay(Boolean show);

  // -- for validation
  Boolean cubeMatches(Cube *);
      

  // -- for status messages
  void setMessageWidget(Widget w);
  void setModeWidget(Widget w);

  // -- for setting parameters
  void allCubesAccept(CubeVisitor *);
  void currentCubeAccept(CubeVisitor *);
  void allCubesAcceptAndDelete(CubeVisitor *);
  void currentCubeAcceptAndDelete(CubeVisitor *);

  // -- misc
  void showit();
  void plotAllCubes();
  void plotCurrentCube();
  SeisColorPop *colorPop();
  void setSyncSlices(Boolean sync);
  CubeMovieControl *cubeMovieControl();
  CubeRandomLinePop *getRandomLinePop(){return _cube_random_line_pop;}
  Boolean firstCube ();

  // -- Color bar
  SeisCbarPop *cbarPop(); 

  // -- Zoom 
  SeisZoomOpPop *zoomOpPop(); 
  void zoomUpSeparateWin(Cube::WhichPlot which);

  // -- which windows are displayed
  void showTimeSlice(Boolean show);
  void showInline(Boolean show);
  void showCrossline(Boolean show);
  void showWireFrame(Boolean show);

  // -- hardcopy
  HardCopySeisPop *inlineHardcopyPop();
  HardCopySeisPop *crosslineHardcopyPop();
  HardCopySeisPop *timesliceHardcopyPop();

  void setNormType(int norm_type);
  int  normType();

  void addPushUp(char *name, SLShellContainer *pop);

  virtual void destroyed(Cube *);
  Widget getAmpWidget();

  // -- coupled cursor control
  void crosshairOff  ();
  void crosshairSmall();
  void crosshairBig  ();
      
};
#endif
