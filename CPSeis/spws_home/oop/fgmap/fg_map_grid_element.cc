#include "fgmap/fg_map_grid_element.hh"
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
#include "fgmap/fg_map_grid_bucket.hh"
#include "geom/field_geometry.hh"
#include <stdlib.h>
#include <assert.h>
#include <locale.h>



FgMapGridElement::FgMapGridElement(FieldGeometry *fg, 
                                   long           line_idx,
                                   long           flag_idx,
                                   SkidType       skid_type,
                                   int            shot_number) :
                              _bucket_grid(NULL), _bucket_loc(NULL),
                              _skid_type(skid_type)
{
  _flagptr= fg->getFlagPointer(line_idx, flag_idx);
  assert(shot_number < 65535);
  _shot_skid_number= (short)shot_number;
}

FgMapGridElement::~FgMapGridElement()
{
  delFromBucket();
}


void FgMapGridElement::setGridBucket(FgMapGridBucket *bucket)
{
  _bucket_grid= bucket; 
}
void FgMapGridElement::setLocBucket(FgMapGridBucket *bucket)
{
 _bucket_loc= bucket; 
}

void FgMapGridElement::delFromBucket()
{
  if (_bucket_grid) _bucket_grid->remove(this);
  if (_bucket_loc)  _bucket_loc->remove(this);
  _bucket_grid= NULL;
  _bucket_loc= NULL;
}

long FgMapGridElement::lineIdx(FieldGeometry *fg)
{
 return (fg->getLineIndex(_flagptr));
}

long FgMapGridElement::flagIdx(FieldGeometry *fg) 
{ 
 return (fg->getFlagIndex(_flagptr));
}

int FgMapGridElement::isSkidded()
{
 return ( _skid_type != IsOnlyFlag);
}
int FgMapGridElement::isReceiverSkid()
{
  return ( _skid_type == IsReceiver);
}
int FgMapGridElement::isShotSkid()
{
  return ( _skid_type == IsShot);
}

int FgMapGridElement::onASelectedLine(FieldGeometry *fg)
{
  int line_idx= fg->getLineIndex(_flagptr);
  return fg->lineIsSelected(line_idx);
}


long FgMapGridElement::shotNumber()
{
  return _shot_skid_number;   // will be -1 if it is not valid
}

void FgMapGridElement::getXY(FieldGeometry *fg, 
                             float         *x,
                             float         *y, 
                             int           is_grid)
{
   double rx, ry;
   if (isSkidded()) {
         if ( _skid_type == IsReceiver) {
           fg->getSkiddedReceiverCoords( lineIdx(fg), flagIdx(fg), &rx, &ry);
         }
         else if ( _skid_type == IsShot) {
           fg->getSkiddedSourceCoords( lineIdx(fg), flagIdx(fg), 
                                      _shot_skid_number, &rx, &ry);
         }
         if (is_grid) {
              *x= fg->getXgridCoord(rx,ry);
              *y= fg->getYgridCoord(rx,ry);
         } // end if
         else {
              *x= (float)rx;
              *y= (float)ry;
         } // end else
   }  // end if isSkid
   else {
       if (!is_grid) {
           *x= fg->getXloc( lineIdx(fg), flagIdx(fg) );
           *y= fg->getYloc( lineIdx(fg), flagIdx(fg) );
       }
       else {
           *x= fg->getXgrid( lineIdx(fg), flagIdx(fg) );
           *y= fg->getYgrid( lineIdx(fg), flagIdx(fg) );
       }
   } // end else


}
