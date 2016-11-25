#include <locale.h>
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
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <assert.h>
#include "fgmap/fg_map_grid_bucket.hh"
#include "fgmap/fg_map_grid_element.hh"
#include "geom/field_geometry.hh"


#ifndef False
#define False 0
#endif
#ifndef True
#define True 1
#endif



FgMapGridBucket::FgMapGridBucket( FieldGeometry *fg, 
                                  float          lowx, 
                                  float          lowy, 
                                  float          highx, 
                                  float          highy,
                                  int            is_grid) :
                     _fg(fg), _lowx(lowx),   _lowy(lowy), 
                              _highx(highx), _highy(highy),
                              _is_grid(is_grid), 
                              _search_stat(NOT_SEARCHED)
{
}


FgMapGridBucket::~FgMapGridBucket() 
{
  freeAllFlags();
}



void FgMapGridBucket::freeAllFlags()
{
  void *a;
  FgMapGridElement *q, *p;
  for(p= q= top(&a); (q); p=q ) {
       q= next(&a);
       delete p;
  }
}


/*
 * is the passed x,y in this bucket?
 */ 
int FgMapGridBucket::inBucket( float x, float y)
{
   int retval= False;
   if ( (x >= _lowx) && (x <= _highx) && (y >= _lowy) && (y <= _highy) )  {
           retval= True;
   }
   return retval;
}

#if 0
void FgMapGridBucket::getXY(FgMapGridElement *ele, float *x, float *y)
{
   
   if (ele->isSkidded()) {

   }  // end if isSkid
   else {
       if (!_is_grid) {
           *x= _fg->getXloc( ele->lineIdx(_fg), ele->flagIdx(_fg) );
           *y= _fg->getYloc( ele->lineIdx(_fg), ele->flagIdx(_fg) );
       } 
       else {
           *x= _fg->getXgrid( ele->lineIdx(_fg), ele->flagIdx(_fg) );
           *y= _fg->getYgrid( ele->lineIdx(_fg), ele->flagIdx(_fg) );
       }
   } // end else
}
#endif


int FgMapGridBucket::putInIfFit( FgMapGridElement *ele)
{
   float x, y;
   int retval= False;

   ele->getXY(_fg, &x, &y, _is_grid);

 //  if (inBucket(x,y))  {
 //  }
   retval= True;
   add(ele);
   if (_is_grid) ele->setGridBucket(this);
   else          ele->setLocBucket(this);

   return retval;
}


/*
 *  return true is the distance between x1,y1 & x2,y2 are less than
 *  dist.
 *  x1,y1 must be in the bucket for the test to occur otherwise 
 *  return false.  x2,y2 can be anywhere.
 */
char FgMapGridBucket::isCloser( float x1, 
                                float y1, 
                                float x2, 
                                float y2, 
                                float dist)
{
  float test_dist;
  if (inBucket(x1,x2))  test_dist= dist;
  else                  test_dist= (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1);
  return (test_dist < dist);
}


/*
 * compute the distance from this square to the given point
 */
float FgMapGridBucket::distanceFrom(float x, float y)
{
  float closest_x;
  float closest_y;
  float distance;

  if     ((x >= _lowx) && (x <= _highx)) closest_x= x;
  else if (x < _lowx)                    closest_x= _lowx;
  else if (x > _highx)                   closest_x= _highx;
  else assert(0);

  if     ((y >= _lowy) && (y <= _highy)) closest_y= y;
  else if (y < _lowy)                    closest_y= _lowy;
  else if (y > _highy)                   closest_y= _highy;
  else assert(0);

  distance= sqrt((closest_x-x)*(closest_x-x)+(closest_y-y)*(closest_y-y));
  return distance;
}


/*
 *  find the closest point in the bucket.  If the closer_sides in
 *  is not NULL then report which sides are closer to the passed
 *  point than the point close point found.  If the bucket contains
 *  not points then all 4 sides and 4 corners will be returned in 
 *  closer_sides.
 */
float FgMapGridBucket::closestPoint(float x,
                                    float y,
                                    FgMapGridElement **flg,
                                    int       only_selected_lines,
                                    unsigned int *closer_sides)
{
  float dist= NO_POINT;
  float test_dist;
  void *a;
  float x2, y2;
  FgMapGridElement *q;

  /*
   * find the closest point to the passed x,y.
   * used distance formula without square rooting for comparisons.
   */
  for(q= top(&a); (q); q= next(&a)) {
      if ((!only_selected_lines) || 
          (only_selected_lines && q->onASelectedLine(_fg))) {
           q->getXY(_fg, &x2, &y2, _is_grid);
           test_dist= (x2-x)*(x2-x) + (y2-y)*(y2-y);
           if (test_dist < dist) {
               dist= test_dist;
               *flg= q;
           }
      }
  }

  /*
   * if closer_sides parameter exist then find if a corner or a side
   * is closer than the closest point
   */
  if (closer_sides) {
      *closer_sides= 0;
      if (isCloser( x, _highy,     x,y,dist) ) *closer_sides|= BUCKET_N;
      if (isCloser( x, _lowy,      x,y,dist) ) *closer_sides|= BUCKET_S;
      if (isCloser( _highx,y,      x,y,dist) ) *closer_sides|= BUCKET_E;
      if (isCloser( _lowx, y,      x,y,dist) ) *closer_sides|= BUCKET_W;
      if (isCloser( _highx, _highy,x,y,dist) ) *closer_sides|= BUCKET_NE;
      if (isCloser( _lowx,  _highy,x,y,dist) ) *closer_sides|= BUCKET_NW;
      if (isCloser( _highx, _lowy, x,y,dist) ) *closer_sides|= BUCKET_SE;
      if (isCloser( _lowx,  _lowy, x,y,dist) ) *closer_sides|= BUCKET_SW;
  }

  if (dist!=NO_POINT) dist= (float)sqrt(dist);

  return dist;
}

/*
 * Delete the flag from the bucket specified by the line and flag
 * index.  Don't just remove from linked list but also delete the 
 * element.
 */
void FgMapGridBucket::removeByIndex(long line_index, long flag_index)
{
  FgMapGridElement *ele= findByIndex(line_index, flag_index);
  if (ele) {
     delete ele;
  }
}

void FgMapGridBucket::remove(FgMapGridElement *ele) 
{
  ele->setGridBucket(NULL);
  BaseLinkedList::remove((void*)ele);
}


FgMapGridElement *FgMapGridBucket::findByIndex(long line_index, 
                                               long flag_index)
{
  FgMapGridElement *q;
  FgMapGridElement *retval= NULL;
  void *a;
  for(q= top(&a); ((q)&&(!retval)); q= next(&a)) {
         if ((q->lineIdx(_fg) == line_index) && 
             (q->flagIdx(_fg) == flag_index)) {
                    retval= q;
         }
  }
  return retval;
}

void FgMapGridBucket::add(FgMapGridElement *ele)
{
   BucketElement *theElement = new BucketElement(ele);
   BaseLinkedList::add((Element *) theElement);
}

FgMapGridElement *FgMapGridBucket::top(void **ptr) 
{ 
   BucketElement* q= (BucketElement*)BaseLinkedList::top(ptr);
   return (q ? q->_ele : NULL);
}

FgMapGridElement *FgMapGridBucket::bottom(void **ptr)
{
   BucketElement* q= (BucketElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_ele : NULL);
}


FgMapGridElement *FgMapGridBucket::next(void **ptr)
{
   BucketElement* q= (BucketElement*)BaseLinkedList::next(ptr);
   return (q ? q->_ele : NULL);
}

FgMapGridElement *FgMapGridBucket::prev(void **ptr)
{
   BucketElement* q= (BucketElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_ele : NULL);
}

FgMapGridElement *FgMapGridBucket::current(void **ptr)
{
   BucketElement* q= (BucketElement*)BaseLinkedList::current(ptr);
   return (q ? q->_ele : NULL);
}
