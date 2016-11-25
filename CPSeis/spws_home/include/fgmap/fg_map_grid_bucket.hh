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
#ifndef FG_MAP_GRID_BUCKET_HH
#define FG_MAP_GRID_BUCKET_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"

class FieldGeometry;
class FgMapGridElement;

class BucketElement : public Element
{
   friend class FgMapGridBucket;

   protected:
      FgMapGridElement *_ele;
      BucketElement( FgMapGridElement *ele) : _ele(ele) {}
      ~BucketElement() {}
      int operator ==(void * const ele) const
                           { return ( (FgMapGridElement*)ele == _ele);}
      virtual void print() const {}
};


#define NO_POINT   500000000
#define BUCKET_N    (1<<0)
#define BUCKET_S    (1<<1)
#define BUCKET_E    (1<<2)
#define BUCKET_W    (1<<3)
#define BUCKET_NW   (1<<4)
#define BUCKET_NE   (1<<5)
#define BUCKET_SW   (1<<6)
#define BUCKET_SE   (1<<7)

class FgMapGridBucket : public BaseLinkedList
{


   private:
       float _lowx, _lowy;
       float _highx, _highy;
       FieldGeometry *_fg;
       int            _is_grid;  // Boolean value
       int            _search_stat;

    
   protected: 
       void add(FgMapGridElement *ele);
       FgMapGridElement *find(FgMapGridElement *ele)
             { return( (FgMapGridElement *)BaseLinkedList::find((void *)ele));}
       FgMapGridElement *top(void **ptr =    (void **) 0);
       FgMapGridElement *bottom(void **ptr = (void **) 0);
       FgMapGridElement *next(void **ptr =   (void **) 0);
       FgMapGridElement *prev(void **ptr =   (void **) 0);
       FgMapGridElement *current(void **ptr =(void **) 0);
       //void getXY(FgMapGridElement *ele, float *x, float *y);
       char isCloser( float x1, float y1, float x2, float y2, float dist);



   public:
       FgMapGridBucket(FieldGeometry *fg,
                       float          lowx, 
                       float          lowy, 
                       float          highx, 
                       float          highy,
                       int            is_grid);
       ~FgMapGridBucket();
       enum { NOT_SEARCHED, SEARCHED, TO_BE_SEARCHED};
       void removeByIndex(long line_index, long flag_index);
       FgMapGridElement *findByIndex(long line_index, long flag_index);

       void remove(FgMapGridElement *ele);

       int inBucket( float x, float y);
       int putInIfFit( FgMapGridElement *ele);
       float closestPoint(float x, 
                          float y, 
                          FgMapGridElement **flg,
                          int      only_selected_lines= 0,  // bool - false
                          unsigned int *closer_sides =(unsigned int *)0);
       void setBucket(int v) { _search_stat= v;}
       float distanceFrom(float x, float y);
       void freeAllFlags();
};
#endif
