#include <Xm/Xm.h>
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
#include "fgmap/fg_map_to_flag.hh"
#include "fgmap/fg_map_grid_element.hh"
#include "fgmap/fg_map_bucket_ary.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sp/seis_plot.hh"
#include <stddef.h>
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>

#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"

#include "sl/shell_watch.hh"


#ifndef False
#define False 0
#endif


FgMapToFlag::FgMapToFlag(FieldGeometry *fg, int grid_size) :
                             FgInform(fg),
                             _grid_size(grid_size),
                             _built(False), _fg(fg),
                             _threshold_dist_mm(3),
                             _bucket_ary_loc(NULL), _bucket_ary_grid(NULL),
                             _vlist(NULL), _i_need_to_rebuild(False)
                             
{
  _built= False;
}


FgMapToFlag::~FgMapToFlag()
{
  release();
}


/*
 * free all memory in the grid
 */
void FgMapToFlag::release()
{
  ShellWatch sw;
  if (_built) {
      _bucket_ary_loc->freeAllFlagsInBuckets();
      delete _bucket_ary_loc;
      delete _bucket_ary_grid;
      _bucket_ary_loc= NULL;
      _bucket_ary_grid= NULL;
      _built= False;
  } // end if _built
}

/*
 * Build the grid 
 */
void FgMapToFlag::build()
{
 ShellWatch sw;
 if (!_built) {
      _bucket_ary_loc=  new FgMapBucketAry(_fg,_grid_size, False);
      _bucket_ary_grid= new FgMapBucketAry(_fg,_grid_size, True);
      loadBuckets();
      _built= True;
 }
}


/*
 * put all the flags into the buckets in the grid
 */
void FgMapToFlag::loadBuckets()
{
  long num_flags;
  long num_lines= _fg->numLines();

  for(int i= 0; (i<num_lines); i++) {    // loop thru each line
         num_flags= _fg->numFlagsOnLine(i);
         for (int j=0; (j<num_flags); j++) {  // each flag on the line
                insertFlag(i,j);
         } // end loop
  } // end loop
}


void FgMapToFlag::insertFlag(int line, int flag)
{
  FgMapGridElement *flg;
  int k;
  /*
   * first make a entry for the flag - set the source index if it has one
   */
  long tot_sources= _fg->numSourcesAtFlag(line,flag);
  if (_fg->flagHasSource(line,flag)) {
       int source_index= -1;
       for(k= 0;((k<tot_sources)&&(source_index==-1)); k++) {
          if (!_fg->sourceIsSkidded(line,flag, k)) {
                source_index= k;
          }
       }
       flg= new FgMapGridElement(_fg,line,flag, 
                                 FgMapGridElement::IsOnlyFlag, source_index);
  } // end if
  else
       flg= new FgMapGridElement(_fg,line,flag);
  _bucket_ary_loc->putFlagInBucket(flg);
  _bucket_ary_grid->putFlagInBucket(flg);
 
  /*
   * if there is a receiver and it is skidded make an entry
   */
  if (_fg->flagHasReceiver(line,flag)) {
      if (_fg->receiverIsSkidded(line,flag)) {
          flg= new FgMapGridElement(_fg,line,flag,
                                    FgMapGridElement::IsReceiver);
          _bucket_ary_loc->putFlagInBucket(flg);
          _bucket_ary_grid->putFlagInBucket(flg);
      } // end if receiverIsSkidded
  } // end if flagHasReceiver
 
  /*
   * if there are skidded shot make and entry for each
   */
  if (_fg->flagHasSource(line,flag)) {
       for(k= 0; (k<tot_sources); k++) {
          if (_fg->sourceIsSkidded(line,flag,k)) {
                flg= new FgMapGridElement(_fg,line,flag,
                                         FgMapGridElement::IsShot,k);
                _bucket_ary_loc->putFlagInBucket(flg);
                _bucket_ary_grid->putFlagInBucket(flg);
          } // end if sourceIsSkidded
       } //end loop 
  } // end if flagHasSource
}







Boolean FgMapToFlag::findClosest(SeisPlot *sp,
                                 int       x,
                                 int       y,
                                 long     *l_idx,
                                 long     *f_idx,
                                 SkidType *skid_type,
                                 long     *shot_number,
                                 Boolean   by_grid,
                                 Boolean   only_sel_lines)
{
  float dist= NO_POINT;
  FgMapGridElement *flg= NULL;
  Boolean found;

  if (_built) {
      *shot_number= -1;
      float wx= sp->xWC(x);        // change pixel to world coords
      float wy= sp->yWC(y);        // change pixel to world coords

      if (by_grid) dist= _bucket_ary_grid->findClosest(wx,wy,&flg,
                                                       only_sel_lines);
      else         dist= _bucket_ary_loc->findClosest(wx,wy,&flg,
                                                       only_sel_lines);
      if (flg) {
          *l_idx= flg->lineIdx(_fg);
          *f_idx= flg->flagIdx(_fg);

          if (flg->isReceiverSkid())   *skid_type= RecSkid;
          else if (flg->isShotSkid())  *skid_type= ShotSkid;
          else                         *skid_type= NoSkid;
          *shot_number= flg->shotNumber();
      } // end if
      else {
          dist= NO_POINT;
      }
  } // end if _built
  else {
       //  grid is not built
  } // end else
  found= (dist != NO_POINT);

  return ( found );
}


Boolean FgMapToFlag::translate(SeisPlot *sp,
                               int       x,
                               int       y,
                               long     *l_idx,
                               long     *f_idx,
                               SkidType *skid_type,
                               long     *shot_number,
                               Boolean   by_grid,
                               Boolean   only_sel_lines)
{
  float dist= NO_POINT;
  FgMapGridElement *flg= NULL;
  Boolean found= False;
  if (_built) {
      *shot_number= -1;
      float wx= sp->xWC(x);        // change pixel to world coords
      float wy= sp->yWC(y);        // change pixel to world coords

      if (by_grid) dist= _bucket_ary_grid->translate(wx,wy,&flg,only_sel_lines);
      else         dist= _bucket_ary_loc->translate(wx,wy,&flg,only_sel_lines);
      found= (dist != NO_POINT);
 
      if (found) {
             if (isWithInThreshold(sp, flg, x, y, by_grid))  {
                 *l_idx= flg->lineIdx(_fg);
                 *f_idx= flg->flagIdx(_fg);

                 if (flg->isReceiverSkid())  *skid_type= RecSkid;
                 else if (flg->isShotSkid()) *skid_type= ShotSkid;
                 else                        *skid_type= NoSkid;
                 *shot_number= flg->shotNumber();
             } // end if
             else
                 found= False;
      } // end if found
  } // end if _built

  return ( found );
}




/*
 * Determine if the distance between the point and the flag
 * location is less than the threshold distance
 */
Boolean FgMapToFlag::isWithInThreshold(SeisPlot *sp,
                                       FgMapGridElement *flg,
                                       int       x,
                                       int       y,
                                       Boolean   by_grid)
{
   float wx, wy;
   float pixel_dist;
   int   x2, y2;
   Display *dpy= XtDisplay(sp->W());
   int scrn= XScreenNumberOfScreen(XtScreen(sp->W()));
   int pix_per_mm;
   int total_dist;

   pix_per_mm =(int)(1 * ((float)DisplayWidth(dpy,scrn) /
                           (float)DisplayWidthMM(dpy, scrn)));
 

   flg->getXY(_fg,&wx, &wy,by_grid);
   x2= sp->xPixel(wx);
   y2= sp->yPixel(wy);
   pixel_dist= sqrt( (x2-x)*(x2-x) + (y2-y)*(y2-y) );

   total_dist= (int)(pixel_dist / pix_per_mm);

   return (total_dist <= _threshold_dist_mm);
}




void FgMapToFlag::undrawGrid()
{
   Vector *q;
   void *a;
   for(q= _vlist->top(&a); (q); q= _vlist->next(&a)) {
       delete q->getData();
   }
   delete _vlist;
   _vlist= NULL;
}


void FgMapToFlag::drawGrid(PlotBase *plot, Boolean by_grid)
{

  Vector             *vect;
  VectData           *data;
  float min_x, max_x, min_y, max_y;
  float xinc, yinc;
  float xl[2], yl[2];

  if (_vlist) undrawGrid();
  _vlist= new SeisVectLinkedList();
  _vlist->addPlot((SeisPlot*)plot);

  
  if (by_grid) {
     _bucket_ary_grid->getMinMax(&min_x, &max_x,  &min_y, &max_y);
     _bucket_ary_grid->getXYinc(&xinc, &yinc);
  }
  else {
     _bucket_ary_loc->getMinMax(&min_x, &max_x,  &min_y, &max_y);
     _bucket_ary_loc->getXYinc(&xinc, &yinc);
  }
  xl[0]= min_x;
  yl[0]= min_y;
  xl[1]= min_x;
  yl[1]= max_y;
  for(int i= 0; (i<=_grid_size); i++) {
         data= new VectData(2, xl, yl);
         vect= _vlist->add(data, "red", 2);
         vect->makeVisible();
         xl[0]= xl[1]= xl[1]+xinc;
  }

  xl[0]= min_x;
  yl[0]= min_y;
  xl[1]= max_x;
  yl[1]= min_y;
  for (int j=0; (j<_grid_size); j++) {
         data= new VectData(2, xl, yl);
         vect= _vlist->add(data, "red", 2);
         vect->makeVisible();
         yl[0]= yl[1]= yl[1]+yinc;
  }
}

void FgMapToFlag::reBuild()
{
  release();
  build();
/*TEMP*/
/**/  if (_vlist) {
/**/      PlotBase **plots= _vlist->getPlots(); 
/**/      drawGrid(plots[0], False);
/**/      delete plots;
/**/  }
}

void FgMapToFlag::startingChanges(FieldGeometry *)
{
  _i_need_to_rebuild= False;
}

void FgMapToFlag::finishedChanges(FieldGeometry *)
{
  if (_i_need_to_rebuild) reBuild();
  _i_need_to_rebuild= False;
}


void FgMapToFlag::freezingDependentUpdates(FieldGeometry *)
{
  //release();
}

void FgMapToFlag::dependentValuesOutOfDate(FieldGeometry *)
{
  release();
  _i_need_to_rebuild= False;
}


void FgMapToFlag::postResumeDependentUpdates(FieldGeometry *)
{
  if (_fg->totNumFlags() > 0) _i_need_to_rebuild= True;
  
}

void FgMapToFlag::postNewGridTransform (FieldGeometry *)
{
  _i_need_to_rebuild= True;
}

void FgMapToFlag::preFlagValuesChanged(FieldGeometry *,
                                       long           ixl, 
                                       int            ident,
                                       long           index, 
                                       long           nrem, 
                                       long           )
{
  /*
   * don't do anything unless the ident passed if FG_COORDS.
   */
  if (_built && ident == FG_COORDS && !_i_need_to_rebuild) { 
         long end= index+nrem;
         for(long i=index; (i<end); i++) {  // remove each flag from the grid
               _bucket_ary_loc->removeFlag(ixl, i);
         }
  }
}


void FgMapToFlag::sourceGathersOutOfDate(FieldGeometry *)
{
  _i_need_to_rebuild= True;
}

void FgMapToFlag::postUpdateSourceGathers(FieldGeometry *)
{
  _i_need_to_rebuild= True;
}

void FgMapToFlag::receiverGathersOutOfDate(FieldGeometry *)
{
  _i_need_to_rebuild= True;
}

void FgMapToFlag::postUpdateReceiverGathers(FieldGeometry *)
{
  _i_need_to_rebuild= True;
}

void FgMapToFlag::preRemoveInsertLines (FieldGeometry *,
                                        long           index, 
                                        long           nrem, 
                                        long           )
{
  if (!_i_need_to_rebuild) {
      long i, j;
      long end= index+nrem;
      long num_flags;
      for(i=index; (i<end); i++) {  // remove every line
          num_flags= _fg->numFlagsOnLine(i);
          for(j=0; (j<num_flags); j++) {  // remove every flag from each line
                   _bucket_ary_loc->removeFlag(i,j);
          } // end loop
      } // end loop
  } // end if !_i_need_to_rebuild
}

void FgMapToFlag::postRemoveInsertLines (FieldGeometry *,
                                         long           index, 
                                         long           , 
                                         long           nins)
{
  if (!_i_need_to_rebuild) {
      long i, j;
      long end= index+nins;
      long num_flags;
      Boolean bail_out= False;
      double x, y;
      for(i=index; ((i<end)&&(!bail_out)); i++) {
          num_flags= _fg->numFlagsOnLine(i);
          for(j=0; (j<num_flags); j++) {
                    x= _fg->getXloc( i,j);
                    y= _fg->getYloc( i,j);
                    if (!_bucket_ary_loc->isWithinAry(x,y)) {
                          bail_out= True;
                    }
          }  // end loop
      }  // end loop
      if (bail_out) {   // rebuild the whole grid
          reBuild();
      }  // end if bail_out
      else {
          for(i=index; (i<end); i++) {  // add each line
              num_flags= _fg->numFlagsOnLine(i);
              for(j=0; (j<num_flags); j++) {  // add every flag from each line
                       insertFlag((int)i,(int)j);
              } // end loop
          } // end loop
      } // end else bail_out
  } // end if !_i_need_to_rebuild



}


void FgMapToFlag::postFlagValuesChanged(FieldGeometry *,
                                        long           ixl, 
                                        int            ident, 
                                        long           index, 
                                        long           , 
                                        long           nins)
{
  /*
   * don't do anything unless the ident passed is FG_COORDS.
   */
  if ((ident == FG_COORDS)&&(!_i_need_to_rebuild)) {
         if (!_built) build();
         else {
            long end= index+nins;
            Boolean bail_out= False;
            double x, y;
 
            /*
             *  This loop determines whether we need to rebuild
             *  the whole grid.  We need to rebuild the whole grid
             *  if one of the flags are outside of the current grid.
             */
            for(long i=index; ((i<end) && (!bail_out)); i++) {
                x= _fg->getXloc( ixl,i);
                y= _fg->getYloc( ixl,i);
                if (!_bucket_ary_loc->isWithinAry(x,y)) {
                      bail_out= True;
                }
            }  // end loop
            if (bail_out) {   // rebuild the whole grid
                   reBuild();
            }  // end if
            else {
                   /*
                    *  Since we know all flags are within the grid we
                    *  can insert each flag in the grid.
                    */
                   for(long i=index; ((i<end) && (!bail_out)); i++) {
                        insertFlag((int)ixl,(int)i);
                   }  // end loop
            }  // end else
         }  // end else

  } // end if FG_COORDS
}
