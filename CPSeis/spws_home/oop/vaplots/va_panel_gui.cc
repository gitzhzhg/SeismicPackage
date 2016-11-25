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
// $Id: va_panel_gui.cc,v 1.4 2005/12/13 16:19:26 spws Exp $
// $Name: 12-13-2005 $

#include "sl/sl_arrow_scale.hh"
#include "sp/seis_inform.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_scrwin.hh"
#include "vaplots/va_plot.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_plot_control.hh"
#include "vaplots/va_panel_gui.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_manager.hh"
#include "sl/prim_support.hh"
#include "sl/paintset.hh"
#include "sl/paintset_collection.hh"
#include "vect/ll_vector.hh"


static String  defres[]= {
          "*XmScale.titleString: Panel Select",
          "*XmScale.scaleMultiple: 1",
    NULL};

struct CBdata {
        VaPanelGui *obj;
        int         panel_at_start_time;
      };



VaPanelGui::VaPanelGui(SLDelay       *contain, 
                       VaPlotControl *plot_ctl,
                       Widget         push) :
                    SLArrowScale(contain,"select",NULL, NULL, True,False ), 
                    SeisInform(), VfInform(plot_ctl->manager()),
                    _plot_ctl(plot_ctl), 
                    _sem_sp(NULL), _dragging_scale(False),
                    _total_panels(1), _first_panel(1), _skip(1),
                    _cmp_sp(NULL), _push(push), _doing_hardcopy(False)
{
/*
/////////////////////// old //////////////////////////
   Colormap cmap;
   XColor   used_color;
   XColor   dummy;
   XtVaGetValues(_push, XmNbackground, &_normal_pixel, 
                        XmNcolormap,   &cmap, 
                        NULL);
   if (XAllocNamedColor( XtDisplay(_push), cmap, "green", 
            &used_color, &dummy)) {
        _green_pixel= used_color.pixel; 
   }
   else {
        _green_pixel= _normal_pixel; 
        printf("could not allocate color green\n");
   }
/////////////////////// old //////////////////////////
*/
/////////////////////// new //////////////////////////
   Paintset *paintset = PaintsetCollection::fetchExistingByColormap (_push);
   if (!paintset->getPixelFromName("green",&_normal_pixel)) {
     _normal_pixel = paintset->white ();
   }
/////////////////////// new //////////////////////////
   setPush(False);
   setFallbackResources( (const char**)defres);
   addSeisPlot( plot_ctl->semblance()->SP() );
   addSeisPlot( plot_ctl->cmp()->SP() );
   if (contain->made()) make(contain->topWidget());
}


Widget VaPanelGui::make(Widget p)
{
   if ( made() ) return topWidget();
   SLArrowScale::make(p);
   p= wParent();


   setRange(1,2);
   useUnmapInsteadOfUnmanage(True);
   unmanage();
   return topWidget();
}


void VaPanelGui::ValueChangeAction(long value)
{
 VaSemblancePlot *sem= _plot_ctl->semblance();
 VaCmpPlot       *cmp= _plot_ctl->cmp();
 _sem_sp= sem->SP();
 _cmp_sp= cmp->SP();
 _current_displayed_panel= (int)value;

 if (!haveThisPanel((int)value)) {
       sem->setFirstPanelToPlot(value);
       cmp->setFirstPanelToPlot(value);
       SeisPlot        *compare_sp;
       compare_sp= sem->SP()->isPlotDisplayed() ? sem->SP() : cmp->SP();

       if (compare_sp->plottedFrames() == 1 && !doingDrag() ) {

            VfDataset *dataset= _plot_ctl->manager()->activeDataset();
            long prev_func= dataset->getActiveVelocityFunction();

            plotCurrentPanel();

            if ( !dataset->isLocked() && dataset->isEditable() ) {
                long new_func= dataset->getActiveVelocityFunction();
                if (new_func != prev_func) {
                    sem->copyPrevPanelVelocityFunc(prev_func, new_func);
                }
            }
       }
       else {
            _sem_sp->setRedrawAction(SeisPlot::Clear);
            _cmp_sp->setRedrawAction(SeisPlot::Clear);
            _sem_sp->redraw();
            _cmp_sp->redraw();
            setPush(True);
       }
 } // end if

 }

Boolean VaPanelGui::haveThisPanel(int panel)
{
 Boolean found= False;       
 long last=  (_total_panels-1)*(_skip+1) + _first_panel;

 if (_total_panels > 1) {
       long  i;
       int   j;
       for (i= _first_panel,j=0; (!found &&  i<=last); i+= (_skip+1),j++ )
               found= (i == panel);
       if (found) {
             _sem_sp->setRedrawAction(SeisPlot::Redraw);
             _cmp_sp->setRedrawAction(SeisPlot::Redraw);
             _sem_sp->movieToFrame(j-1);
             _cmp_sp->movieToFrame(j-1);
             setPush(False);
             setActiveFunctionTimer(panel);
       }
 }
 else if (_first_panel == panel) {
    found= True;
    _sem_sp->setRedrawAction(SeisPlot::Redraw);
    _cmp_sp->setRedrawAction(SeisPlot::Redraw);
    if (_sem_sp) _sem_sp->redraw();
    if (_cmp_sp) _cmp_sp->redraw();
    setActiveFunctionTimer(panel);
    setPush(False);
 }

 return found;
}


void VaPanelGui::newPlot(SeisPlot *sp)
{
 // set range
 VaSemblancePlot *sem= _plot_ctl->semblance();
 VaCmpPlot       *cmp= _plot_ctl->cmp();
 SeisPlot        *compare_sp;
 Boolean using_sem= (Boolean)sem->SP()->isPlotDisplayed();
 compare_sp= using_sem ? sem->SP() : cmp->SP();
 VaPlot *vaplot = using_sem ? (VaPlot*)sem : (VaPlot*)cmp;

 if ( sp == sem->SP() || sp == cmp->SP() ) {
     manage();
     if (using_sem) setRange(1,(int)sem->getNumberPanelsInFile() );
     else           setRange(1,(int)cmp->getNumberPanelsInFile() );
     _total_panels= compare_sp->plottedFrames();
     _skip        = compare_sp->skipFrames();
     if (compare_sp->plottedISkp() == 0) 
        _first_panel= 1;
     else    
        _first_panel = (compare_sp->plottedISkp() 
                                  / vaplot->tracesPerGroup() ) + 1;
     setPush(False);
     //if (using_sem) setValue((int)sem->getMovieFirstPanel());
     //else           setValue((int)cmp->getMovieFirstPanel());
     showPanel(sp);
     setActiveFunctionTimer(_current_displayed_panel); 
 }
 else { assert(0); }
}

void VaPanelGui::showPanel(SeisPlot *sp)
{

 VaSemblancePlot *sem= _plot_ctl->semblance();
 VaCmpPlot       *cmp= _plot_ctl->cmp();
 SeisPlot        *compare_sp;

 compare_sp= sem->SP()->isPlotDisplayed() ? sem->SP() : cmp->SP();

 if ( sp == compare_sp ) {
       long frame= sp->currentFrame();
       long panel= _first_panel + ( (frame) * (_skip+1) );
       setValue((int)panel);
 }

}

void VaPanelGui::postMovie(SeisPlot *sp, SeisPlot::MovieDir )
{
 showPanel(sp);
}

void VaPanelGui::noPlotDisplayed(SeisPlot *sp)
{
 VaSemblancePlot *sem= _plot_ctl->semblance();
 VaCmpPlot       *cmp= _plot_ctl->cmp();
 if ( sp == sem->SP() || sp == cmp->SP() ) {
      if (!sem->SP()->isPlotDisplayed() && !cmp->SP()->isPlotDisplayed() ) {
         unmanage();
         setPush(False);
      } // end if
 }
 else { assert(0); }
}

void VaPanelGui::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
}

void VaPanelGui::plotCurrentPanel()
{
 _sem_sp->setRedrawAction(SeisPlot::Redraw);
 _cmp_sp->setRedrawAction(SeisPlot::Redraw);
 if (_plot_ctl->semblance()->ableToPlot()) _plot_ctl->plot(VaPlotControl::SEM);
 if (_plot_ctl->cmp()->ableToPlot()) _plot_ctl->plot(VaPlotControl::CMP);
 setActiveFunction();
}

void VaPanelGui::setPush(Boolean set)
{
 XtSetSensitive(_push, set);
 XtVaSetValues(_push, XmNbackground, set ? _green_pixel : _normal_pixel, NULL);
}

void VaPanelGui::setActiveFunctionTimer(int panel)
{
 if (_doing_hardcopy) {
      setActiveFunction(); 
 }
 else {
      VectorLinkedList::holdNewPlots(1); //  TEST
      CBdata *data=  new CBdata();
      data->obj= this;
      data->panel_at_start_time= panel;
      assert(topWidget());
      XtAppAddTimeOut(XtWidgetToApplicationContext(topWidget()), 
                    40, timerCallback, data );
 } // end if
}

void VaPanelGui::setActiveFunction()
{
 VfDataset *dataset= _plot_ctl->manager()->activeDataset();
 VaSemblancePlot *sem= _plot_ctl->semblance();
 VaCmpPlot       *cmp= _plot_ctl->cmp();
 VaPlot *vaplot = sem->SP()->isPlotDisplayed() ? (VaPlot*)sem : (VaPlot*)cmp;

 if ( sem->SP()->isPlotDisplayed() || cmp->SP()->isPlotDisplayed() ) {
     float xloc= vaplot->getDisplayedXbin();
     float yloc= vaplot->getDisplayedYbin();
     long func=        dataset->findMatchingVelfun(xloc, yloc );
     long active_func= dataset->getActiveVelocityFunction();
     if ( dataset->isLocked() || !dataset->isEditable() ) {
           if (func == -1) {
                  _plot_ctl->vectorColors()->setShowActiveFunc(False);
           } // end if
           else {
                  if (active_func != func)
                           dataset->setActiveVelocityFunction(func);
                  _plot_ctl->vectorColors()->setShowActiveFunc(True);
           } // end else
     } // end if
     else {
           if (func == -1) {
                 dataset->findOrInsertVelfun( xloc, yloc );
           }
           else if (active_func != func) {
                 dataset->setActiveVelocityFunction(func);
           }
           _plot_ctl->vectorColors()->setShowActiveFunc(True);
     } // end else
 }

 VectorLinkedList::holdNewPlots(0); //  TEST
 PrimSupport::updateEverything();
}

void VaPanelGui::timerCallback(XtPointer udata, XtIntervalId *)
{
  CBdata *data= (CBdata*)udata;
  data->obj->timer(data->panel_at_start_time);
  delete data;
}


void VaPanelGui::timer(int panel)
{
  if (_current_displayed_panel == panel)
      setActiveFunction();
}

void VaPanelGui::doBigChange(VfDataset *)
{
   setActiveFunctionTimer(_current_displayed_panel);
}

void VaPanelGui::postTotalChanges(VfDataset *dataset)
{
   if (dataset == _plot_ctl->manager()->activeDataset()) {
       VaSemblancePlot *sem= _plot_ctl->semblance();
       VaCmpPlot       *cmp= _plot_ctl->cmp();
       VaPlot *vaplot = sem->SP()->isPlotDisplayed() ? 
                                      (VaPlot*)sem : (VaPlot*)cmp;
       long func= dataset->findMatchingVelfun( vaplot->getDisplayedXbin(), 
                                               vaplot->getDisplayedYbin());
       if (func == -1) setActiveFunctionTimer(_current_displayed_panel);
   }
}

void VaPanelGui::postChangeDataLock(VfDataset *dataset)
{
   if (dataset == _plot_ctl->manager()->activeDataset())
            doBigChange(dataset);
}

void VaPanelGui::postNewActiveDataset()
{
   doBigChange(_plot_ctl->manager()->activeDataset());
}

void VaPanelGui::postRemoveInsertVelocityFunctions
                       (VfDataset *dataset, long, long, long)
{
  if (dataset == _plot_ctl->manager()->activeDataset()) {
      if (dataset->numVelocityFunctions() == 0) {
           setActiveFunctionTimer(_current_displayed_panel);
      }
  } 
}

void VaPanelGui::preWriteHardCopy(SeisPlot *)  { _doing_hardcopy= True; }
void VaPanelGui::postWriteHardCopy(SeisPlot *) { _doing_hardcopy= False; }
