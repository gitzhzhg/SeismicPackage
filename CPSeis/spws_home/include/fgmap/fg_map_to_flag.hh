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
#ifndef FG_MAP_TO_FLAG_HH
#define FG_MAP_TO_FLAG_HH

#include "geom/fg_inform.hh"

class FgMapBucketAry;
class FieldGeometry;
class SeisPlot;
class PlotBase;
class SeisVectLinkedList;
class FgMapGridElement;



class FgMapToFlag : public FgInform {


  protected:
      FgMapBucketAry      *_bucket_ary_loc; 
      FgMapBucketAry      *_bucket_ary_grid;
      int                  _grid_size;
      Boolean              _built;
      Boolean              _i_need_to_rebuild;
      FieldGeometry       *_fg;
      int                  _threshold_dist_mm;
      SeisVectLinkedList  *_vlist;

      void loadBuckets();
      Boolean isWithInThreshold(SeisPlot *sp,
                                FgMapGridElement *flg,
                                int       x,
                                int       y,
                                Boolean   by_grid);

 
  public:
      enum SkidType { NoSkid, RecSkid, ShotSkid};
      FgMapToFlag(FieldGeometry *fg, int grid_size= 10);
      ~FgMapToFlag();
        
      void build();
      void release();
      void reBuild();
      void insertFlag(int line, int flag);
      Boolean translate(SeisPlot *sp,      //fast - does minimal searching
                        int       x, 
                        int       y, 
                        long     *line_index,
                        long     *flag_index,
                        SkidType *skid_type,
                        long     *shot_number,
                        Boolean   by_grid,
                        Boolean   only_sel_lines);
      Boolean findClosest(SeisPlot *sp,   //slower - does complete searching
                          int       x,
                          int       y, 
                          long     *line_index,
                          long     *flag_index,
                          SkidType *skid_type,
                          long     *shot_number,
                          Boolean   by_grid,
                          Boolean   only_sel_lines);
      void drawGrid(PlotBase *plot, Boolean by_grid);
      void undrawGrid();
      virtual void freezingDependentUpdates   (FieldGeometry *fg);
      virtual void dependentValuesOutOfDate   (FieldGeometry *fg);
      virtual void postResumeDependentUpdates (FieldGeometry *fg);
      virtual void postNewGridTransform (FieldGeometry *fg);  
      virtual void  preFlagValuesChanged (FieldGeometry *fg,
                       long ixl, int ident, long index, long nrem, long nins);
      virtual void postFlagValuesChanged (FieldGeometry *fg,
                       long ixl, int ident, long index, long nrem, long nins);
      virtual void preRemoveInsertLines  (FieldGeometry *fg,
                                          long index, long nrem, long nins);
      virtual void postRemoveInsertLines (FieldGeometry *fg,
                                          long index, long nrem, long nins);
      virtual void sourceGathersOutOfDate        (FieldGeometry *fg);
      virtual void postUpdateSourceGathers       (FieldGeometry *fg);
      virtual void receiverGathersOutOfDate      (FieldGeometry *fg);
      virtual void postUpdateReceiverGathers     (FieldGeometry *fg);
      virtual void startingChanges      (FieldGeometry *fg);
      virtual void finishedChanges      (FieldGeometry *fg);


};
#endif
