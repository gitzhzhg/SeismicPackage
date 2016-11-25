#include "plot/pick_base.hh"
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
#include "sp/seis_plot.hh"
#include "geom/field_geometry.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "fgxp/fgxp_2d_plot.hh"
#include "fgxp/fgxp_data.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"
#include "fgmap/fg_pick_action.hh"
#include "cprim.h"




FgPickAction::FgPickAction( SeisPlot           *sp,
                            FieldGeometry      *fg,
                            FgXpPlotLinkedList *plot_list) :
                      _sp(sp), _fg(fg), _plot_list(plot_list), _mode(NO_ACTION)
{
  _vlist= new SeisVectLinkedList();
  _vlist->addPlot(sp);
}



FgPickAction::~FgPickAction()
{
  delete _vlist;
}



void FgPickAction::selectOneFlag(int x, int y)
{
   long flag_index;
   long line_index;
   char select_char;
   switch (_mode) {
        case SELECT:   select_char= SELECT_YES;   break;
        case UNSELECT: select_char= SELECT_MAYBE; break;
   }
   if (getCurrentIndex( x, y, &line_index, &flag_index))
         _fg->setFlagSelectValue(line_index, flag_index, select_char);
}


void FgPickAction::operateOnFlagsOnLine(int x1,
                                        int y1,
                                        int x2,
                                        int y2)
{
 FgXpData *xpdata;
 long line_index;
 long begin_index;
 long end_index;
 int  incrementor;
 int i;

 xpdata= (FgXpData*)_curr_line_vect->getData();   // yes, a down cast
 i= _curr_line_vect->closestIndex(x1,y1,_sp);
 begin_index=  xpdata->translateDataIndexToFlag(i);
 i=   _curr_line_vect->closestIndex(x2,y2,_sp);
 end_index=  xpdata->translateDataIndexToFlag(i);
 line_index= xpdata->getXIndex();
 _fg->preMultipleOperations();
 if (begin_index < end_index) incrementor= 1;
 else                         incrementor= -1;
 for(int j=(int)begin_index; (j!=end_index); j+= incrementor) {
       switch (_mode) {
          case SELECT :
                   _fg->setFlagSelectValue(line_index, j, SELECT_YES);
                   break;
          case UNSELECT :
                   _fg->setFlagSelectValue(line_index, j, SELECT_MAYBE);
                   break;
          case INTERPOLATE_VAL :
                   interpolateFlagValues(line_index, j);
                   break;
          default: assert(0);
                   break;
       }
 }
 _fg->postMultipleOperations();
}


void FgPickAction::interpolateFlagValues(long line_index, long flag_index)
{
  if (_xfield != FG_NONE)
         _fg->setDependentFlagValue(line_index, flag_index, _xfield, 0.0);
  if (_yfield != FG_NONE)
         _fg->setDependentFlagValue(line_index, flag_index, _yfield, 0.0);
}

void FgPickAction::interpolateOneFlag(int x, int y)
{
   long flag_index;
   long line_index;
   _fg->preMultipleOperations();
   if (getCurrentIndex( x, y, &line_index, &flag_index))
           interpolateFlagValues(line_index, flag_index);
   _fg->postMultipleOperations();
}


void FgPickAction::interpolateFlagsInArea(int x1, 
                                          int y1, 
                                          int x2, 
                                          int y2)
{
 
 VectorAreaPick  **vap;
 int d2, d3;
 int num_lines;
 void *x= NULL;
 FgXpPlot *vplot= _plot_list->top(&x);
 vplot->getFlagsInArea(x1,y1,x2,y2, _sp, &num_lines,&d2,&d3,&vap);
 _fg->preMultipleOperations();
 for(int i=0; (i<num_lines); i++) {
     assert( (vap[i]->vectorId[0] == vap[i]->vectorId[1]) &&
             (vap[i]->vectorId[0] == vap[i]->vectorId[2]) );
     for(int j=0; (j<vap[i]->numIndices); j++) {
           interpolateFlagValues(vap[i]->vectorId[0], (long)vap[i]->index[j]);
     }
 }
 _fg->postMultipleOperations();
}



void FgPickAction::startGroupInterpolation(int x, 
                                           int y, 
                                           int xfield, 
                                           int yfield)
{
 _xfield= xfield;
 _yfield= yfield;
 _mode= INTERPOLATE_VAL; 
 startGroup(x,y);
}

void FgPickAction::startLineSnapInterpolation(int x, 
                                              int y, 
                                              int xfield, 
                                              int yfield)
{
 _xfield= xfield;
 _yfield= yfield;
 _mode= INTERPOLATE_VAL; 
 startLineSnap(x,y);
}


void FgPickAction::startGroupSelection(int x, int y, Boolean set)
{
   _mode= set ? SELECT : UNSELECT;
   startGroup(x,y);
}

void FgPickAction::startGroup(int x, int y)
{
   _drag_in_progress= False;
   float xwc= _sp->xWC(x);
   float ywc= _sp->yWC(y);

   for(int i= 0; (i<5); i++) {
      _xsqr[i]= xwc;
      _ysqr[i]= ywc;
   }

   VectData *square_data= new VectData(5, _xsqr, _ysqr);
   _working_vect=  _vlist->add(square_data, "green", 2, True);
}

void FgPickAction::dragGroup(int x, int y)
{
   _drag_in_progress= True;
   float xwc= _sp->xWC(x);
   float ywc= _sp->yWC(y);

   _xsqr[1]=  xwc;
   _ysqr[1]=  _ysqr[0];

   _xsqr[2]=  xwc;
   _ysqr[2]=  ywc;

   _xsqr[3]=  _xsqr[0];
   _ysqr[3]=  ywc;


   // here goes the downcast again...

   ((VectData*)_working_vect->getData())->replace(1, 3, _xsqr+1, _ysqr+1);
}



void FgPickAction::endGroup(int x1, int y1, int x2, int y2)
{
   char select_char;


   delete _working_vect->getData();
   _vlist->remove(_working_vect);
   if ((_mode == SELECT)||(_mode == UNSELECT)) {
       switch (_mode) {
           case SELECT:   select_char= SELECT_YES;   break;
           case UNSELECT: select_char= SELECT_MAYBE; break;
       }
       if (_drag_in_progress)
           _plot_list->areaSelect(x1, x2, y1, y2, select_char, _sp);
       else
           selectOneFlag(x1,y1);
   } // end if
   else if (_mode == INTERPOLATE_VAL) {
       if (_drag_in_progress)
            interpolateFlagsInArea(x1,y1, x2, y2);
       else
            interpolateOneFlag(x1,y1);


   }
   _drag_in_progress= False;
}



void FgPickAction::startLineSnapSelection(int x, int y, Boolean set)
{
   _mode= set ? SELECT : UNSELECT;
   startLineSnap(x,y);
}


void FgPickAction::startLineSnap(int x, int y)
{
   long vect_index;
   FgXpData *xpdata;
   _drag_in_progress= False;
   _curr_line_vect=  _plot_list->getClosest(x,y,_sp);
   if (_curr_line_vect) vect_index= _curr_line_vect->closestIndex(x,y,_sp);

   if (_curr_line_vect) {
          xpdata= (FgXpData*)_curr_line_vect->getData();   // yes, a down cast
          _xsqr[1]= _xsqr[0]= xpdata->getX((int)vect_index);
          _ysqr[1]= _ysqr[0]= xpdata->getY((int)vect_index);
          VectData *dist_data= new VectData(2, _xsqr, _ysqr);
          _working_vect=  _vlist->add(dist_data, "green", 5, True);
   } // end if _curr_line_vect
}



void FgPickAction::dragLineSnap(int x, int y)
{
   FgXpData *xpdata;
   long data_index;

   if (_curr_line_vect) {
      _drag_in_progress= True;
      data_index= _curr_line_vect->closestIndex(x,y,_sp);
      xpdata= (FgXpData*)_curr_line_vect->getData();   // yes, a down cast

      _xsqr[1]= xpdata->getX((int)data_index);
      _ysqr[1]= xpdata->getY((int)data_index);

      // here goes the downcast again...
      ((VectData*)_working_vect->getData())->replace(1, 1, _xsqr+1, _ysqr+1);
   } // end if _curr_line_vect
}


void FgPickAction::endLineSnap(int x1, int y1, int x2, int y2)
{
   if (_curr_line_vect) {
      delete _working_vect->getData();
      _vlist->remove(_working_vect);
      if (_drag_in_progress)
           operateOnFlagsOnLine(x1,y1, x2, y2);
      else if (_mode == INTERPOLATE_VAL)
           interpolateOneFlag(x1,y1);
      else
           selectOneFlag(x1,y1);
      _drag_in_progress= False;
   } // end if _curr_line_vect
}


Vector* FgPickAction::getCurrentIndex( int   x,
                                       int   y,
                                       long *line_index,
                                       long *flag_index,
                                       long *vect_index,
                                       long *source_index)
{
  Vector *vect;
  FgXpData *xpdata;
  int i;
  int numsrcs;
  long *srcary= NULL;

  vect= _plot_list->getClosest(x,y,_sp);
  if (vect) {
      i= vect->closestIndex(x,y,_sp);
      xpdata= (FgXpData*)vect->getData();   // yes, a down cast
      *flag_index=  xpdata->translateDataIndexToFlag(i,&srcary,&numsrcs);
      *line_index= xpdata->getXIndex();
      if (vect_index) *vect_index= i;
      if (source_index) {
           if (numsrcs > 0) {
                 *source_index= srcary[0];
                 free(srcary);
           }
           else 
                 *source_index= 0;
      } // end if
  } // end if vect
  return vect;
}

