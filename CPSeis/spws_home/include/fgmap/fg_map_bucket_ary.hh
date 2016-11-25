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
#ifndef FG_MAP_BUCKET_ARY_HH
#define FG_MAP_BUCKET_ARY_HH

#include "fg_map_grid_bucket.hh"


class FgMapBucketAry {

private:
    FieldGeometry      *_fg;
    FgMapGridBucket  ***_bucket_ary;
    int                 _grid_size;
    double              _xinc;
    double              _yinc;
    double              _min_x;
    double              _max_x;
    double              _min_y;
    double              _max_y;
    int                 _by_grid;       // boolean value

protected:
   void buildBuckets();
   unsigned char computeGridLocation(float wx, float wy, int &i, int &j);
   void findClosestPointOnGrid(float wx, float wy, float &cx, float &cy);
   void computeMinMax();
   void testOther(int   i,
                  int   j,
                  float wx,
                  float wy,
                  FgMapGridElement **flg,
                  float *distance, 
                  int    only_selected);
   unsigned char squareCloser(int   gridx,
                              int   gridy,
                              float wx,
                              float wy,
                              float dist);
   void searchPerimeter( int lowx,
                         int highx,
                         int lowy,
                         int highy,
                         float wx, 
                         float wy, 
                         FgMapGridElement **flg,
                         float *dist,
                         int    only_selected);


public:
  // constructor - grid_size is how many grid squares there are on 
  //               one side, so grid_size of 10 would mean a 10 by 10
  //               square or 100 elements.  The by_grid parameter is
  //               a boolean value that indicates a map is layed out 
  //               by grid when true and by loc when false.
  FgMapBucketAry(FieldGeometry *fg, int grid_size= 10, int by_grid= 1);

   ~FgMapBucketAry();
   void putFlagInBucket(FgMapGridElement *flg);
   void getMinMax(float *min_x, float *max_x, float *min_y, float *max_y);
   void getXYinc(float *xinc, float *yinc);
   float translate(float wx, float wy, FgMapGridElement **flg, 
                   int only_selected);
   float findClosest(float wx, float wy, FgMapGridElement **flg,
                     int only_selected);
   void freeAllFlagsInBuckets();
   void removeFlag(long line_index, long flag_index);
   unsigned char isWithinAry(double x, double y);
};
#endif








