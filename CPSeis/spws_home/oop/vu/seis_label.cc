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
#include "sp/seis_winman.hh"
#include "sp/sp_list.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"
#include "vect/vector.hh"
#include "vu/seis_label.hh"
#include "vu/pick_label.hh"
#include "vu/label_info_list.hh"
#include "vu/label_input.hh"
#include "cprim.h"


const char *SeisLabel::_no_label_string= "XXXXXX";


SeisLabel::SeisLabel(SeisPlot *sp, HelpCtx hctx) :
         _lab_list(new LabelInfoList()), _hctx(hctx), SeisInform(sp)
{
 addPlot(sp);
 _insert_color= newstr("red");
 _label_pop= new LabelInput(sp->W(), "label_input", hctx, this);
}

SeisLabel::~SeisLabel() 
{
   void *x,*y;
   Vector *vect;
   PlotBase *q;
   SeisVectLinkedList *labels;

   
   for(q= _lab_list->top(&y); (q); q= _lab_list->next(&y) ) {
      labels= _lab_list->currentVectorList(&y);
      for(vect= labels->top(&x); (vect); vect= labels->next(&x) ) 
                   delete vect->getData();
      delete labels;
   }
   delete _lab_list;
   delete _label_pop;
   if (_insert_color) free(_insert_color);
}


void SeisLabel::insertMoveOneLabel()
{
  _lab_list->enablePicking(this);
  _just_one= True;
}

void SeisLabel::addPlot(SeisPlot *sp)
{
  SeisVectLinkedList *labels;
  if (!_lab_list->find(sp)) {
       _lab_list->add(sp,labels= new SeisVectLinkedList() );
       labels->addPlot(sp);
       addSeisPlot(sp);
  }
}

void SeisLabel::removePlot(SeisPlot *sp)
{
  void *x;
  _lab_list->find(sp, &x);
  delete _lab_list->currentVectorList(&x);
  if (sp->getSeisWinMan()->count() == 1) _lab_list->disablePicking();
  _lab_list->remove(sp);
}

void SeisLabel::notCurrentInWindow(SeisPlot *sp)
{
  void *x;
  if (!_lab_list->find(sp->currentSPInWindow(), &x)) {
       addPlot(sp->currentSPInWindow());
  }
}

void SeisLabel::destroyed(SeisPlot *sp)
{
  removePlot(sp);
}


void SeisLabel::doInsertLabel(PickLabel *picker, int x, int y, Boolean ismod)
{
  PlotBase *plot= picker->getPlot();
  void *dummy;
  float rect, pt;
  Boolean add_a_label= False;
  SeisVectLinkedList *labels;
  

  if ( _lab_list->find(plot, &dummy) ) 
              labels= _lab_list->currentVectorList(&dummy);
  else        assert(0);

  /*
   *   select a label to move or add a label.
   */
  _working_vect= labels->closestLabel(x,y,plot);

  if (!_working_vect) add_a_label= True;
  else {
     _working_vect->howCloseToLabel(x,y,plot,&rect,&pt);
     if (rect > 2)  add_a_label= True;
  }

  if (add_a_label) {
       float time=  plot->yWC(y);
       float trace= plot->xWC(x);
       VectData *label_pt= new VectData(1, &trace, &time);
       _working_vect= labels->addLabel(label_pt, _no_label_string, 
                          _insert_color, 
                          "-*-helvetica-bold-r-*-*-*-180-*-*-*-*-*-*" );
       _working_vect->makeVisible();
       _show_pop= True;
  }
  else {
      doMoveLabel(picker, x, y);
      if (ismod)
         _show_pop= True;
      else
         _show_pop= False;
  }

}

void SeisLabel::placeLabel(PlotBase *plot, Vector *vect, int x, int y)
{
  Vector::VectorOffset xisoff, yisoff;
  float converted_x;
  float converted_y;
  int vx, vy, width, height;

  vect->getXYOffsets(&xisoff, &yisoff);
  if (xisoff == Vector::IsOffset) {
       plot->getVisibleArea(&vx, &vy, &width, &height);      
       converted_x= x - vx;
       converted_y= y - vy;
  }
  else {
       converted_x= plot->xWC(x);    // get trace
       converted_y= plot->yWC(y);    // get time
  }

  /*
   * The following downcast is a defilement of all I know that is right
   * and good, but sometimes such things happen...
   */
  ((VectData*)vect->getData())->replace(0,1, &converted_x, &converted_y);
}




void SeisLabel::doMoveLabel(PickLabel *picker, int x, int y)
{
  PlotBase *plot= picker->getPlot();
  placeLabel(plot,_working_vect,x,y);
}

void SeisLabel::doFinishLabel(PickLabel *, int, int)
{
  _label_pop->setCurrentVector(_working_vect);
  if (_show_pop) {
        _label_pop->makeAndManage();
        _lab_list->disablePicking();
  }
  else if ( (!_show_pop) && (_just_one) ) {
        _lab_list->disablePicking();
  }
  _working_vect= NULL;
}


void SeisLabel::doNameLabel(char          *name, 
                            Vector        *vect, 
                            const char    *fontstr,
                            const char    *color,
                            const Boolean floating_lab)
{

  if (strlennull(name) == 0) {
       void *x;
       PlotBase *q;
       SeisVectLinkedList *labels;
       Boolean found= False;

       for(q= _lab_list->top(&x); ((q)&&(!found)); q= _lab_list->next(&x) ) {
                labels= _lab_list->currentVectorList(&x);
                if (labels->find(vect)) found= True;
       }
      delete vect->getData();
      labels->remove(vect);
  }
  else {
      vect->setLabel(name, fontstr);
      vect->setColor(color);
      setLabelType(vect, floating_lab);
      if (_just_one) {
          _lab_list->disablePicking();
      }
      else
          _lab_list->enablePicking(this);
  }
}


void SeisLabel::setLabelType( Vector *vect, const Boolean floating_lab)
{
  Vector::VectorOffset xisoff, yisoff;
  BaseData *data;
  PlotBase *plot= NULL, *q;
  void *d1;
  SeisVectLinkedList *vect_list;

  
  for(q= _lab_list->top(&d1), plot= NULL; ((q)&&(!plot)); 
                             q= _lab_list->next(&d1)) {
       vect_list= _lab_list->currentVectorList(&d1); 
       if (vect == vect_list->find(vect)) {
                 plot= q;
       }
  }
  assert(plot);
  

  vect->getXYOffsets(&xisoff, &yisoff);
  if ( (xisoff == Vector::IsOffset) && (!floating_lab) )  {
      /*
       * convert from floating to non-floating labels
       */
      int vx, vy, width, height;
      plot->getVisibleArea(&vx, &vy, &width, &height);      

      data= vect->getData();
      vect->makeInvisible();
      vect->setXYOffsets(Vector::IsNotOffset,Vector::IsNotOffset);
      placeLabel(plot,vect,(int)data->getX(0) + vx, (int)data->getY(0) + vy);
      vect->makeVisible();
  }
  else if ( (xisoff == Vector::IsNotOffset) && (floating_lab) )  {
      /*
       * convert from non-floating to floating labels
       */
      data= vect->getData();
      int x= plot->xPixel(data->getX(0));
      int y= plot->yPixel(data->getY(0));
      vect->makeInvisible();
      vect->setXYOffsets(Vector::IsOffset,Vector::IsOffset);
      placeLabel(plot,vect,x,y);
      vect->makeVisible();
  }
}




void SeisLabel::deleteAllLabels()
{
   void *x,*y;
   Vector *vect, *next_vect;
   PlotBase *q;
   SeisVectLinkedList *labels;

   _label_pop->unmanage();
   for(q= _lab_list->top(&y); (q); q= _lab_list->next(&y) ) {
      labels= _lab_list->currentVectorList(&y);
      for(vect= labels->top(&x); (vect); vect= next_vect )  {
                   next_vect= labels->next(&x);
                   delete vect->getData();
                   labels->remove(vect);
      }
   }
}


void SeisLabel::doDeleteLabel(PickLabel *picker, int x, int y)
{
  void *dummy;
  float rect, pt;
  SeisVectLinkedList *labels;
  PlotBase *plot= picker->getPlot();
  if ( _lab_list->find(plot, &dummy) ) 
             labels= _lab_list->currentVectorList(&dummy);
  else       assert(0);
  Vector *vect= labels->closestLabel(x,y,plot);
  if (vect) {
     vect->howCloseToLabel(x,y,plot,&rect,&pt);
     if (rect < 20) {
          delete vect->getData();
          labels->remove(vect);
     }
     else
          XBell( XtDisplay(_lab_list->top()->getWidget()), 0);
  }
  else
     XBell( XtDisplay(_lab_list->top()->getWidget()), 0);

  if (_just_one) {
      _lab_list->disablePicking();
  }

}

void SeisLabel::setInsertColor(char *color)
{
  if (_insert_color) free(_insert_color);
  _insert_color= newstr(color);
}
