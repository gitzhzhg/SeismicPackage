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
//Author Michael L. Sherrill 05/97
//Creates class to generate random lines
//Used with menu CubeRandomLinePop
//********************************************

#ifndef CUBE_RANDOM_LINE_H
#define CUBE_RANDOM_LINE_H

#include "cube/cube_inform.hh"
#include "cube/cube_world_coords.hh"


class CubeDisplay;
class CubeRandomLinePicker;
class CubeRandomLinePlot;
class CubeRandomLinePop;

class CubeRandomLine 
{

 friend class CubeRandomLinePop;

 private:
     Cube                     *_cube;
     CubeRandomLinePicker     *_picker;
     CubeRandomLinePlot       *_cube_random_line_plot;
     int                      _cube_number;
     SeisPlot                 *_timeslice_sp;
     SeisPlot                 *_randomline_sp;  
     int                      *_read_request_array;
     long                     _number_traces;
     int                      *_crossline_array;
     int                      *_inline_array;
     int                      _picker_segments;
     long                     _enforce_sort;
     float                    *_xdata;
     float                    *_ydata;
     float                    *_vector_inlines;
     float                    *_vector_crosslines;
     float                    *_x_xor;
     float                    *_y_xor;
     class SeisVectLinkedList *_vect_ll;
     class VectData           *_vect_data_lines;
     class VectData           *_vect_data_markers;
     class VectData           *_vect_data_xor;
     class Vector             *_vectL;
     class Vector             *_vectM;
     class Vector             *_vectXOR;

 protected:
     long              _number_of_locations;     
     CubeWorldCoords   *_cube_world_coordinates;

 public:
     CubeRandomLine(Cube *cube);
     ~CubeRandomLine();
     enum{MINIMUM_PIXELS = 5, INITIAL_LOCATIONS = 50};
     CubeRandomLinePicker *getPicker();
     CubeRandomLinePlot *cubeRandomLinePlot(Widget p, char *name,
                                            HelpCtx  hctx,
                                            CubeRandomLinePop *pop, 
                                            int numcolors,
                                            CubeDisplay *cube_display);
     CubeRandomLinePlot *cubeRandomLinePlot ();
     void removePicker();
     void setNumberPickerSegments(int i);
     void sortSegments();
     void enforceSort(long i){_enforce_sort = i;}
     int  findNearestSegment(float xloc, float yloc);
     Boolean modifySegment(int xloc, int yloc);
     float *xarray(){return _xdata;}
     float *yarray(){return _ydata;}
     float *x_xor_array(){return _x_xor;}
     float *y_xor_array(){return _y_xor;}
     float *vectorCrosslines(){return _vector_crosslines;}
     float *vectorInlines(){return _vector_inlines;}
     class Vector *vectXOR(){return _vectXOR;}
     class VectData *vectDataXOR(){return _vect_data_xor;}
     class VectData *vectLines(){return _vect_data_lines;}
     class VectData *vectMarkers(){return _vect_data_markers;}
     class SeisVectLinkedList *vectLinkedList(){return _vect_ll;}
     int pickerSegments(){return _picker_segments;}
     void createVectors();
     void showVectors();
     void hideVectors();
     void createXORvector();
     void destroyXORvector();
     
     //Plot access methods
     SeisPlot *getTimeSliceSP();

     //Coordinate transformation line indices and world coordinate
     int getLineIndexFromWC(Cube::WhichPlot which, float coord);
     float getWCFromLineIndex(Cube::WhichPlot which, int index);
     void getCubeWorldCoordinates(float *xmin, float *xmax,
                                  float *ymin, float *ymax);

     //Read new data related.
     int readData();
     Boolean findTraces(Boolean create_read_array = False);
     Boolean createReadRequestArray();
     int *getReadRequestArray(){return _read_request_array;}


     //Access to cube line coordinate arrays
     int *getInlinePoints(){return _inline_array;} 
     int *getCrosslinePoints(){return _crossline_array;} 
     long getNumberTraces(){return _number_traces;}     

     //Error popup
     void errorPopUp(char *errstr);
};







#endif
