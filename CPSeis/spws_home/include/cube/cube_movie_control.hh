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
//********************************************
//Author Michael L. Sherrill 10/96
//Creates menu to control cube movie images
//********************************************

#ifndef CUBE_MOVIE_CONTROL_H
#define CUBE_MOVIE_CONTROL_H

#include "cube/cube_inform.hh"

enum{ PREVIOUS_INLINE_READ,  NEXT_INLINE_READ,         PREVIOUS_CROSSLINE_READ,
      NEXT_CROSSLINE_READ,   PREVIOUS_TIMESLICE_READ,  NEXT_TIMESLICE_READ,
      READ_INLINE,           READ_CROSSLINE,           READ_TIMESLICE,     
      LOAD_INLINE,           LOAD_CROSSLINE,           LOAD_TIMESLICE,
      LOAD_ALL,              SCAN_ARROWS,              READ_LINES,
      LOAD_BUTTONS,          MOVIE_INLINE_LEFT,        MOVIE_INLINE_RIGHT,
      MOVIE_CROSSLINE_LEFT,  MOVIE_CROSSLINE_RIGHT,    MOVIE_TIMESLICE_LEFT,
      MOVIE_TIMESLICE_RIGHT, MOVIE_CUBE_LEFT,          MOVIE_CUBE_RIGHT,
      DATA_CHANGES,          MOVIE_CHANGES};

class CubeDisplay;
class CubeMoviePop;



class CubeMovieControl : public CubeInform 
{

 friend class CubeMoviePop;

 private:
     CubeDisplay     *_cube_display;
     CubeMoviePop    *_cube_movie_pop;
     int              _inline_frame;
     int              _crossline_frame;
     int              _timeslice_frame;
     int              _cube_number;
     float            _read_panel;
     float            _skip_panels;
     float            _total_panels;
   
 protected:
     

 public:
     CubeMovieControl(Widget      w, 
                      HelpCtx     hctx, 
                      CubeDisplay *cube_disp);
     ~CubeMovieControl();
     enum{INLINE, CROSSLINE, TIMESLICE, CUBE};
     CubeMoviePop *getMoviePop() { return _cube_movie_pop; }
     void moviePopManage();

     //inform methods
     void newCubeCreated(Cube *);
     void newInLinePlot(Cube *,    int slice);
     void newCrossLinePlot(Cube *, int slice);
     void newTimeSlicePlot(Cube *, int slice);
     void destroyed(Cube *);

     //misc cube methods
     char *primaryFilename();
     void cubeIsCurrent(Cube *cube);
     Cube * getDisplayedCube();

     // visible frame info
     int visibleInLineFrame();
     int visibleCrossLineFrame();
     int visibleTimeSliceFrame();
     int visibleCubeFrame();

     // File line info
     int fileInLineTotal();
     int fileCrossLineTotal();
     int fileTimeSliceTotal();

     // data in memory info
     int memoryInLineTotal();
     int memoryCrossLineTotal();
     int memoryTimeSliceTotal();
     int memoryCubeTotal();
     int memoryFirstInLine();
     int memoryFirstCrossLine();
     int memoryFirstTimeSlice();
     int memoryLastInLine();
     int memoryLastCrossLine();
     int memoryLastTimeSlice();
     int memoryInLineSkip();
     int memoryCrossLineSkip();
     int memoryTimeSliceSkip();

     //Coordinate transformation line indices and world coordinate
     int getLineIndexFromWC(Cube::WhichPlot which, float coord);
     float getWCFromLineIndex(Cube::WhichPlot which, int index);

     //Read new data.
     int readData(int type, int first, int skip, int total);

     //Change movie frame
     void changeFrame(int which_sp, int frame);
   
     //Accelerator action method to read data or change movie frame
     void acceleratorRequest(long which);

     //Error popup
     void errorPopUp(Cube *cube);
};







#endif
