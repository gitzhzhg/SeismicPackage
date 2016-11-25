//********************************************
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
//Author Michael L. Sherrill 10/96
//Creates and controls cube movie images
//********************************************
#include "sp/seis_plot.hh"
#include "sl/sl_error_pop.hh"
#include "cube/cube.hh"
#include "cube/cube_display.hh"
#include "cube/cube_movie_control.hh"
#include "cube/cube_movie_pop.hh"
#include "cube/cube_master.hh"

#define read_error "You can't get there from here. It appears you have \n\
attempted to read past the end of your data.\nThe contents of the file are \
as follows:\n" 

//============================================================================
//====================== Constructor =========================================
//============================================================================
CubeMovieControl::CubeMovieControl( Widget              p,
                                    HelpCtx             hctx,
                                    CubeDisplay         *cube_display)
{
  _cube_display = cube_display;
  _cube_movie_pop = new CubeMoviePop(p, "Cube Movie Pop", hctx, this);
}


//============================================================================
//====================== Destructor                  =========================
//============================================================================
CubeMovieControl::~CubeMovieControl()
{
  if(_cube_movie_pop) delete _cube_movie_pop;
  CubeMaster::instance()->delInformer(this);
}


//============================================================================
//====================== Manage                  =========================
//============================================================================
void CubeMovieControl::moviePopManage()
{
  _cube_movie_pop->makeAndManage();
}


//============================================================================
//====================== Inform to add a new cube           ==================
//============================================================================
void CubeMovieControl::newCubeCreated(Cube *cube)
{
  addCube(cube);
  _cube_movie_pop->updateCubeInfo();
}


//============================================================================
//====================== Inform to activate a cube============================
//============================================================================
void CubeMovieControl::cubeIsCurrent(Cube * /*cube*/)
{
  _cube_movie_pop->updateCubeInfo();
}


//============================================================================
//====================== Inform that a cube is deleted =======================
//============================================================================
void CubeMovieControl::destroyed(Cube * /*cube*/)
{
  _cube_movie_pop->updateCubeInfo();
}

//============================================================================
//====================== Inform of new line plotted    =======================
//============================================================================
void CubeMovieControl::newInLinePlot(Cube * /*cube*/, int /*slice*/)
{
  _cube_movie_pop->updateCubeInfo();
}

//============================================================================
//====================== Inform of new line plotted    =======================
//============================================================================
void CubeMovieControl::newCrossLinePlot(Cube * /*cube*/ , int /*slice*/)
{
  _cube_movie_pop->updateCubeInfo();
}


//============================================================================
//====================== Inform of new line plotted    =======================
//============================================================================
void CubeMovieControl::newTimeSlicePlot(Cube * /*cube*/ , int /*slice*/)
{
  _cube_movie_pop->updateCubeInfo();
}


//============================================================================
//====================== Get visible inline frame     ========================
//============================================================================ 
int CubeMovieControl::visibleInLineFrame ()
{
  return getDisplayedCube() ? getDisplayedCube()->currentInLineFrame() : 0;
}


//============================================================================
//====================== Get visible crossline frame  ========================
//============================================================================ 
int CubeMovieControl::visibleCrossLineFrame ()
{
  return getDisplayedCube() ? getDisplayedCube()->currentCrossLineFrame() : 0;
}


//============================================================================
//====================== Get visible timeslice frame  ========================
//============================================================================ 
int CubeMovieControl::visibleTimeSliceFrame ()
{
  return getDisplayedCube() ? getDisplayedCube()->currentTimeSliceFrame() : 0;
}



//============================================================================
//====================== Get current displayed frame  ========================
//============================================================================ 
int CubeMovieControl::visibleCubeFrame ()
{
Cube *cube = getDisplayedCube();
Cube *temp;
int i = 0;

  for( temp = top(); temp; temp = next(), i++ )
     if(cube == temp) 
         return i;

  return i;
}


//============================================================================
//====================== Get the displayed cube   ============================
//============================================================================
Cube *CubeMovieControl::getDisplayedCube()
{
Cube *cube;

 for( cube = top(); cube; cube = next() )
   if( cube->isCurrentInWindow() && 
      (cube->currentLine()      != Cube::NoLinePlotted || 
       cube->currentCrossLine() != Cube::NoLinePlotted || 
       cube->currentTimeSlice() != Cube::NoLinePlotted) ) 
        return cube;

 return NULL;
}

//============================================================================
//====================== Get total inlines in file  ==========================
//============================================================================
int CubeMovieControl::fileInLineTotal() 
{
  return getDisplayedCube() ? getDisplayedCube()->totalLines() : 0;
} 

//============================================================================
//====================== Get total crosslines in file  =======================
//============================================================================
int CubeMovieControl::fileCrossLineTotal() 
{
  return getDisplayedCube() ? getDisplayedCube()->totalCrossLines() : 0;
}

//============================================================================
//====================== Get total timeslices in file ========================
//============================================================================ 
int CubeMovieControl::fileTimeSliceTotal ()
{
  return getDisplayedCube() ? getDisplayedCube()->totalTimeSlices() : 0;
}

//============================================================================
//====================== Get first inline in memory     ========================
//============================================================================ 
int CubeMovieControl::memoryFirstInLine ()
{
  return getDisplayedCube() ? getDisplayedCube()->firstMemoryInlineSlice() : 0;
}

//============================================================================
//====================== Get first crossline in memory ========================
//============================================================================ 
int CubeMovieControl::memoryFirstCrossLine ()
{
  return getDisplayedCube() ? getDisplayedCube()->firstMemoryCrosslineSlice():0;
}

//============================================================================
//====================== Get first timeslice in memory ========================
//============================================================================ 
int CubeMovieControl::memoryFirstTimeSlice ()
{
  return getDisplayedCube() ? getDisplayedCube()->firstMemoryTimeSlice() : 0;
}

//============================================================================
//====================== Compute last inline in memory      ==================
//============================================================================ 
int CubeMovieControl::memoryLastInLine ()
{
  return getDisplayedCube() ? getDisplayedCube()->lastMemoryInlineSlice() : 0;
}

//============================================================================
//====================== Compute last crossline in memory   ==================
//============================================================================ 
int CubeMovieControl::memoryLastCrossLine ()
{
  return getDisplayedCube() ? getDisplayedCube()->lastMemoryCrosslineSlice():0;
}

//============================================================================
//====================== Compute last timeslice in memory   ==================
//============================================================================ 
int CubeMovieControl::memoryLastTimeSlice ()
{
  return getDisplayedCube() ? getDisplayedCube()->lastMemoryTimeSlice() : 0;
}

//============================================================================
//====================== Get total inline frames in memory ===================
//============================================================================ 
int CubeMovieControl::memoryInLineTotal ()
{
  return getDisplayedCube() ? getDisplayedCube()->totalInLineFrames() : 0;
}

//============================================================================
//====================== Get total crossline frames in memory ================
//============================================================================ 
int CubeMovieControl::memoryCrossLineTotal ()
{
  return getDisplayedCube() ? getDisplayedCube()->totalCrossLineFrames() : 0;
}


//============================================================================
//====================== Get total timeslice frames in memory ================
//============================================================================ 
int CubeMovieControl::memoryTimeSliceTotal ()
{
  return getDisplayedCube() ? getDisplayedCube()->totalTimeSliceFrames() : 0;
}


//============================================================================
//====================== Get total number of cubes in memory =================
//============================================================================
int CubeMovieControl::memoryCubeTotal ()
{
Cube *cube;
int i = 0;

  for( cube = top(); cube; cube = next(), i++ ){}

  return i;

}



//============================================================================
//====================== Memory inline frame skips          ====================
//============================================================================ 
int CubeMovieControl::memoryInLineSkip()
{
  return getDisplayedCube() ? getDisplayedCube()->inLineFrameSkip() : 0;
}

//============================================================================
//====================== Memory crossline frame skips     ====================
//============================================================================ 
int CubeMovieControl::memoryCrossLineSkip()
{
  return getDisplayedCube() ? getDisplayedCube()->crossLineFrameSkip() : 0;
}

//============================================================================
//====================== Memory timeslice frame skips     ====================
//============================================================================ 
int CubeMovieControl::memoryTimeSliceSkip()
{
  return getDisplayedCube() ? getDisplayedCube()->timeSliceFrameSkip() : 0;
}


//============================================================================
//====================== Get current displayed cube filename =================
//============================================================================ 
char * CubeMovieControl::primaryFilename()
{
  return getDisplayedCube() ? getDisplayedCube()->primaryFilename() : (char*)"";
}



//============================================================================
//==================Get line index from a world coordinate  ==================
//============================================================================ 
int CubeMovieControl::getLineIndexFromWC(Cube::WhichPlot which, float coord)
{
Cube *cube;

  cube = getDisplayedCube();
  if(cube == NULL) 
     return -1; 
  else
    return cube->convertWCToIndex(which, coord);
}


//============================================================================
//==================Get a world coordinate from a line index =================
//============================================================================ 
float CubeMovieControl::getWCFromLineIndex(Cube::WhichPlot which, int index)
{
Cube *cube;

  cube = getDisplayedCube();
  if(cube == NULL) 
     return -1.0; 
  else
    return cube->convertIndexToWC(which, index);
}

//============================================================================
//====================== Method to change frame or cube displayed ============
//============================================================================
void CubeMovieControl::changeFrame(int which_sp, int frame)
{
Cube *cube;
int i = 1;

  cube = getDisplayedCube();
  if(cube == NULL) return; 

  switch (which_sp)
    {
    case Cube::InLine:
       if(cube->inlineSP())
          cube->movieToFrame(Cube::InLine, frame - 1);
       break;

    case Cube::CrossLine:
       if(cube->crosslineSP())
          cube->movieToFrame(Cube::CrossLine, frame - 1);
       break;

    case Cube::TimeSlice:
       if(cube->timesliceSP()) 
          cube->movieToFrame(Cube::TimeSlice, frame - 1);
       break;

    case Cube::AllPlots:
       for( cube = top(); cube; cube = next(), i++ )
         if(i == frame) 
           {
           _cube_display->displayCube(cube);
           _cube_movie_pop->updateCubeInfo();        
           }
       break;
    }

}


//============================================================================
//====================== Method to plot new data       =======================
//============================================================================
int CubeMovieControl::readData(int type, int first, int skip, int total)
{
int stat = 0;
Cube *cube;


  cube = getDisplayedCube();
  if(cube == NULL) return stat;
  if(first < 0) 
    {
    errorPopUp(cube);
    return stat;
    }
  

  switch(type)
    {
    case Cube::InLine:
      if( first + ((skip+1) * total) > cube->totalLines() )
        {
        errorPopUp(cube);
        return stat;
        }
      cube->setMovie(Cube::InLine, True);
      cube->setInlineSlice(first);
      cube->setSkipFrames(Cube::InLine, skip); 
      cube->setFrames(Cube::InLine, total); 
    break;

    case Cube::CrossLine:
      if( first + ((skip+1) * total) > cube->totalCrossLines() )
        {
        errorPopUp(cube);
        return stat;
        }
      cube->setMovie(Cube::CrossLine, True);
      cube->setCrosslineSlice(first);
      cube->setSkipFrames(Cube::CrossLine,skip); 
      cube->setFrames(Cube::CrossLine,total);
    break;

    case Cube::TimeSlice:
      if( first + ((skip+1) * total) > cube->totalTimeSlices() )
        {
        errorPopUp(cube);
        return stat;
        }
      cube->setMovie(Cube::TimeSlice, True);
      cube->setTimeSlice(first);
      cube->setSkipFrames(Cube::TimeSlice,skip); 
      cube->setFrames(Cube::TimeSlice,total);
    break;
    }


  stat = cube->plot( (Cube::WhichPlot)type );

  return stat;
}

//============================================================================
//====================== Method to plot new data       =======================
//============================================================================
void CubeMovieControl::acceleratorRequest(long which)
{
int not_used_now = 0;

  if(which < MOVIE_INLINE_LEFT)
    _cube_movie_pop->readPattern(which,not_used_now);
  else
    _cube_movie_pop->movieFrame(which);
}



//============================================================================
//====================== Create and popup an error     =======================
//============================================================================
void CubeMovieControl::errorPopUp(Cube *cube)
{
SLErrorPop *temp;
char tempstr[256];
char errstr[512];

  sprintf(tempstr,"Inlines    %d\nCrosslines %d\nTimeslices %d\n",
          cube->totalLines(),cube->totalCrossLines(),cube->totalTimeSlices());
  strcpy(errstr, read_error);
  strcat(errstr,tempstr); 
  temp = new SLErrorPop(_cube_display->W(), "Error", errstr);
}
