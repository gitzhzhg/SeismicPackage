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
#ifndef FG_MAP_GRID_ELEMENT_HH
#define FG_MAP_GRID_ELEMENT_HH

class FgMapGridBucket;
class FieldFlag;
class FieldGeometry;

class FgMapGridElement {


   private:
      FieldFlag        *_flagptr;
      short            _skid_type;
      short            _shot_skid_number;
      FgMapGridBucket  *_bucket_grid;
      FgMapGridBucket  *_bucket_loc;



   public:
      enum SkidType {IsReceiver, IsShot, IsOnlyFlag}; 
      FgMapGridElement(FieldGeometry *fg, 
                       long           line_idx, 
                       long           flag_idx,
                       SkidType       skid_type   =IsOnlyFlag,
                       int            shot_number =-1);
      ~FgMapGridElement();
      void setGridBucket(FgMapGridBucket *bucket);
      void setLocBucket(FgMapGridBucket *bucket);
      long lineIdx(FieldGeometry *fg);
      long flagIdx(FieldGeometry *fg);
      int  isSkidded();         // returns boolean value
      int  isReceiverSkid();    // returns boolean value
      int  isShotSkid();        // returns boolean value
      long shotNumber();        // returns shot num only if element is skided
      void delFromBucket();
      int onASelectedLine(FieldGeometry *fg);  // returns boolean value
      void getXY(FieldGeometry *fg, 
                 float         *x,
                 float         *y, 
                 int           is_grid);

};
#endif
