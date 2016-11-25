
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
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "fgmap/fg_map_bucket_ary.hh"
#include "fgmap/fg_map_grid_element.hh"
#include "geom/field_geometry.hh"


#ifndef False
#define False 0
#endif
#ifndef True
#define True 1
#endif




FgMapBucketAry::FgMapBucketAry(FieldGeometry *fg, int grid_size, int by_grid)
                             : _grid_size(grid_size), _bucket_ary(NULL),
                               _fg(fg), _by_grid(by_grid)
{
  computeMinMax();
  buildBuckets();
}

FgMapBucketAry::~FgMapBucketAry()
{
  for(int i=0; (i<_grid_size); i++) {
        for(int j=0; (j<_grid_size); j++) {
            delete _bucket_ary[i][j];
        }
        free(_bucket_ary[i]);
  }
  free(_bucket_ary);
}



/*
 * free every flag in every bucket
 */
void FgMapBucketAry::freeAllFlagsInBuckets()
{
  for(int i= 0; (i<_grid_size); i++) {
         for (int j=0; (j<_grid_size); j++) {
             _bucket_ary[i][j]->freeAllFlags();
         }
  }
}



/*
 * Alloc 2d array of bucket pointers and create the bucket objects.
 * Compute the min and max each buckets object.
 */
void FgMapBucketAry::buildBuckets()
{
  double x1=0, x2=0, y1=0, y2=0;
  int i;
  x2=_min_x;
  y2=_min_y;
  _xinc= (_max_x - _min_x) / _grid_size;
  _yinc= (_max_y - _min_y) / _grid_size;

  // make sure _xinc & _yinc are not zero
  if (_xinc < 0.001) _xinc= 0.1;
  if (_yinc < 0.001) _yinc= 0.1;

  _bucket_ary = (FgMapGridBucket***)malloc( sizeof (FgMapGridBucket**) *
                                                     _grid_size);
  for(i=0; (i<_grid_size); i++)
     _bucket_ary[i]= (FgMapGridBucket**)malloc( sizeof (FgMapGridBucket*) *
                                                      _grid_size);

  for(i= 0; (i<_grid_size); i++) {
         x1=  x2;
         x2+= _xinc;
         y2= _min_y;
         for (int j=0; (j<_grid_size); j++) {
             y1=  y2;
             y2+= _yinc;
             _bucket_ary[i][j]= new FgMapGridBucket(_fg, x1, y1, x2, y2,
                                                   _by_grid);
         }
  }
}

/*
 * Get a flag and compute the bucket the flag should go into.
 */
void FgMapBucketAry::putFlagInBucket(FgMapGridElement *flg)
{
  int found;
  int i,j;
  float x,y;
  flg->getXY(_fg,&x,&y,_by_grid);

  found= computeGridLocation(x,y,i,j);
  //assert(found);
  found= _bucket_ary[i][j]->putInIfFit(flg);
  assert(found);
} 



/*
 * Compute which bucket and given location is in.
 */
unsigned char FgMapBucketAry::computeGridLocation(float  wx, 
                                                  float  wy, 
                                                  int   &i, 
                                                  int   &j)
{
  unsigned char found= False;
  if ((wx >= _min_x) && (wx <= _max_x) && (wy >= _min_y) && (wy <= _max_y)){
           i= (int)((wx-_min_x) / _xinc);
           j= (int)((wy-_min_y) / _yinc);
           found= True;
  }
  if (!found) {
         if ((wx >= _min_x) && (wx <= _max_x))  
               i= (int)((wx-_min_x) / _xinc);
         else
               i= _grid_size-1;
         if ((wy >= _min_y) && (wy <= _max_y))
               j= (int)((wy-_min_y) / _yinc);
         else
               j= _grid_size-1;
  }
  return found;
}



void FgMapBucketAry::removeFlag(long line_index, long flag_index)
{
  int i,j;
  int found;
  float x,y;
  double rx, ry;
  if (_fg->flagHasReceiver(line_index, flag_index))  {
        if (_fg->receiverIsSkidded(line_index, flag_index)) {
             _fg->getSkiddedReceiverCoords( line_index, flag_index, &rx, &ry);
             x= (float)rx;
             y= (float)ry;
             computeGridLocation(x,y,i,j);
             _bucket_ary[i][j]->removeByIndex(line_index, flag_index);

        } // end IsSkidded
  } // end Has Receiver

  if (_fg->flagHasSource(line_index, flag_index)) {
        long tot_sources= _fg->numSourcesAtFlag(line_index, flag_index);
        for(int k= 0; (k<tot_sources); k++) {
           if (_fg->sourceIsSkidded(line_index, flag_index,k)) {
                _fg->getSkiddedSourceCoords(line_index, flag_index,k,&rx,&ry);
                x= (float)rx;
                y= (float)ry;
                computeGridLocation(x,y,i,j);
                _bucket_ary[i][j]->removeByIndex(line_index, flag_index);
           } // end if sourceIsSkidded
        } //end loop
  } // end if flagHasSource

  x= _fg->getXloc(line_index, flag_index);
  y= _fg->getYloc(line_index, flag_index);
  found= computeGridLocation(x,y,i,j);
  _bucket_ary[i][j]->removeByIndex(line_index, flag_index);
}

/*
 * compute: 1. is the grid square valid - if not return False
 *          2. is the grid square closer than the dist given
 *             if so return True else return False
 */
unsigned char FgMapBucketAry::squareCloser(int   gridx,
                                           int   gridy,
                                           float wx,
                                           float wy,
                                           float dist)
{
  unsigned char retval;
  float square_dist; 

  if ( (gridx >= 0) && (gridx < _grid_size) &&
       (gridy >= 0) && (gridy < _grid_size) ) {
         square_dist= _bucket_ary[gridx][gridy]->distanceFrom(wx,wy);
         retval= (square_dist < dist);
  }
  else 
     retval= False;

  return retval;
}


/*
 * Search the outermost squares of the specified area for a 
 * point closer that the specified distance.  if a closer point
 * is found update the passed distance with the new distance and
 * update the line and flag index of that closer point.
 */
void FgMapBucketAry::searchPerimeter( int lowx,
                                      int highx,
                                      int lowy,
                                      int highy,
                                      float wx, 
                                      float wy, 
                                      FgMapGridElement **flg,
                                      float *dist,
                                      int    only_selected)
{
  int i;
  for(i= lowx; (i<highx); i++) {         //top of square & 1 corner
       if (squareCloser(i,highy,wx,wy, *dist))
             testOther(i,highy,   wx, wy, flg, dist,only_selected);
  } // end loop
  for(i= lowx+1; (i<=highx); i++) {      //bottom of square & 1 corner
       if (squareCloser(i,lowy,wx,wy, *dist))
          testOther(i,lowy,   wx, wy, flg, dist,only_selected);
  } // end loop
  for(i= lowy+1; (i<=highy); i++) {      //left of square & 1 corner
       if (squareCloser(lowx,i,wx,wy, *dist))
          testOther(lowx,i,   wx, wy, flg, dist,only_selected);
  } // end loop
  for(i= lowy; (i<highy); i++) {         //right of square & 1 corner
       if (squareCloser(highx,i,wx,wy, *dist))
          testOther(highx,i,   wx, wy, flg, dist,only_selected);
  } // end loop

}

/*
 * what grid point on the edge of the grid is closest to the 
 * passed point.
 */
void FgMapBucketAry::findClosestPointOnGrid(float  x,
                                            float  y,
                                            float &closest_x,
                                            float &closest_y)
{

  float buff= ((_max_x-_min_x)/_grid_size)*.01;

  if     ((x > _min_x) && (x < _max_x))    closest_x= x;
  else if (x <= _min_x)                    closest_x= _min_x+buff;
  else if (x >= _max_x)                    closest_x= _max_x-buff;
  else assert(0);

  if     ((y > _min_y) && (y < _max_y)) closest_y= y;
  else if (y <= _min_y)                 closest_y= _min_y+buff;
  else if (y >= _max_y)                 closest_y= _max_y-buff;
  else assert(0);
}


/*
 * find a point from at given x,y - search entire grid until
 * a point is found.
 */
float FgMapBucketAry::findClosest(float wx, 
                                  float wy, 
                                  FgMapGridElement **flg,
                                  int                only_selected)
{
  float dist= NO_POINT;
  int found= False; 
  int startx, starty;
  int done;
  int level;
  float test_dist;
  int lowx, lowy, highx, highy;
  

  found= computeGridLocation(wx,wy,startx,starty);
  if (!found) {
        float close_x, close_y;
        findClosestPointOnGrid(wx,wy,close_x, close_y);
        found= computeGridLocation(close_x,close_y,startx,starty);
        //assert(found);
  }

  test_dist= dist= _bucket_ary[startx][starty]->closestPoint(wx, wy, flg,
                                                           only_selected);
  /*
   * Find a point that is closer than the point in the first bucket
   */
  for(done= False, level=1; (!done); level++) {
      lowx=  startx-level;
      lowy=  starty-level;
      highx= startx+level;
      highy= starty+level;
      searchPerimeter(lowx,highx,lowy,highy,wx,wy,flg,&test_dist,only_selected);
      /*
       * is the starting distance closer that the distance from 
       * searchPerimeter?
       */
      if (dist < test_dist) {  // time to stop looping 
           done= True;
      }   
      else {                   // search farther out for a point
           dist= test_dist;
      }
      if ( (lowx <= 0) && (lowy <= 0) && // we searched the entire grid
           (highy >= _grid_size-1)  &&
           (highx >= _grid_size-1) )
                 done= True;
  } // end loop
  return dist;
}




/*
 * find a point from at given x,y using a fast search method.
 * if a point is not found the passed square of the adjacent squares
 * the return NO_POINT.
 */
float FgMapBucketAry::translate(float wx, 
                                float wy, 
                                FgMapGridElement **flg,
                                int                only_selected)
{
 float dist= NO_POINT;
 unsigned int others= 0;
 int i,j;

  computeGridLocation(wx,wy,i,j);

  dist=  _bucket_ary[i][j]->closestPoint(wx, wy, flg, 
                                         only_selected,&others);
  if (others & BUCKET_N)  testOther(i,j-1,   wx, wy, flg, &dist,only_selected);
  if (others & BUCKET_S)  testOther(i,j+1,   wx, wy, flg, &dist,only_selected);
  if (others & BUCKET_E)  testOther(i+1,j,   wx, wy, flg, &dist,only_selected);
  if (others & BUCKET_W)  testOther(i-1,j,   wx, wy, flg, &dist,only_selected);
  if (others & BUCKET_NW) testOther(i-1,j-1, wx, wy, flg, &dist,only_selected);
  if (others & BUCKET_NE) testOther(i+1,j-1, wx, wy, flg, &dist,only_selected);
  if (others & BUCKET_SW) testOther(i-1,j+1, wx, wy, flg, &dist,only_selected);
  if (others & BUCKET_SE) testOther(i+1,j+1, wx, wy, flg, &dist,only_selected);
  return dist;
}



void FgMapBucketAry::testOther(int   i,
			       int   j,
                               float wx,
                               float wy,
                               FgMapGridElement **flg,
                               float *dist,
                               int    only_selected)
{
 float next_distance;
 FgMapGridElement *flg_test;

  if ( (i>=0) && (i< _grid_size) &&
       (j>=0) && (j< _grid_size) ) {
     next_distance= _bucket_ary[i][j]->closestPoint(wx, wy, &flg_test,
                                                    only_selected);
     if (next_distance < *dist) {
          *dist= next_distance;
          *flg= flg_test;
     }
  }
}


void FgMapBucketAry::computeMinMax()
{
  if (_by_grid) {
    _min_x= _fg->minimumXgridInSurvey();
    _max_x= _fg->maximumXgridInSurvey();
    _min_y= _fg->minimumYgridInSurvey();
    _max_y= _fg->maximumYgridInSurvey();
  }
  else {
    _min_x= _fg->minimumXlocInSurvey();
    _max_x= _fg->maximumXlocInSurvey();
    _min_y= _fg->minimumYlocInSurvey();
    _max_y= _fg->maximumYlocInSurvey();
  }

  _min_x= _min_x - (_max_x -_min_x+2) *.1;
  _max_x= _max_x + (_max_x -_min_x+2) *.1;
  _min_y= _min_y - (_max_y -_min_y+2) *.1;
  _max_y= _max_y + (_max_y -_min_y+2) *.1;
}

void FgMapBucketAry::getMinMax(float *min_x,
                               float *max_x,
                               float *min_y,
                               float *max_y)
{
  *min_x= _min_x;
  *max_x= _max_x;
  *min_y= _min_y;
  *max_y= _max_y;
}

unsigned char FgMapBucketAry::isWithinAry(double x, double y)
{
   unsigned char retval= False;
   if ( (x >= _min_x) && (x <= _max_x) && (y >= _min_y) && (y <= _max_y) ){
       retval= True;
   }
   return retval;
}



void FgMapBucketAry::getXYinc(float *xinc, float *yinc)
{
  *xinc= _xinc;
  *yinc= _yinc;

}
