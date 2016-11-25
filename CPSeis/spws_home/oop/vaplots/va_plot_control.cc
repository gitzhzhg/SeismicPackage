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
// $Id: va_plot_control.cc,v 1.3 2004/06/07 14:06:22 wjdone Exp $
// $Name:  $

#include <assert.h>
#include <string.h>
#include <unistd.h>
#include "vaplots/va_plot_control.hh"
#include "vaplots/va_trace_file.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_grid_plot.hh"
#include "vaplots/va_crossplot_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_eta_plot.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_file_base.hh"
#include "vf/vf_dataset.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_scrwin.hh"
#include "sl/view_win.hh"
#include "sl/sl_pull_pop.hh"
#include "str.h"


#define True  1
#define False 0



VaPlotControl::VaPlotControl( VfManager       *manager,  
                              WinCtl          *winctl,
                              VaIsoPlot       *va_iso_plot,
                              VaGridPlot      *va_grid_plot,
                              VaSemblancePlot *va_semblance_plot,
                              VaGvsPlot       *va_gvs_plot,
                              VaCmpPlot       *va_cmp_plot,
                              VaCrossplotPlot *va_crossplot_plot,
                              VaEtaPlot       *va_eta_plot,
                              VaVectColors    *vect_colors ) :
                VfInform(manager),
                _manager(manager),
                _iso_plot(va_iso_plot),
                _grid_plot(va_grid_plot),
                _semblance_plot(va_semblance_plot),
                _gvs_plot(va_gvs_plot),
                _cmp_plot(va_cmp_plot),
                _crossplot_plot(va_crossplot_plot),
                _eta_plot(va_eta_plot),
                _winctl(winctl),
                _fail_string(NULL),
                _replot_all(False),
                _vect_colors(vect_colors),
                _i_am_plotting(False)
{
   _vel_file= new VfFileBase("Velocity file", "vel", manager);
   _sem_file= new VaTraceFile( _semblance_plot, "Semblance File");
   _cmp_file= new VaTraceFile( _cmp_plot, "CMP File");
   _gvs_file= new VaTraceFile( _gvs_plot, "GVS File");

   _crossplot_plot->setAbleToPlot(True);
   _grid_plot->setAbleToPlot(True);
   _iso_plot->setAbleToPlot(True);

   _iso_win=        new ViewWin(_winctl, va_iso_plot->SP()->W(),      NULL);
   _grid_win=       new ViewWin(_winctl, va_grid_plot->SP()->W(),     NULL);
   _gvs_win=        new ViewWin(_winctl, va_gvs_plot->SP()->W(),      NULL);
   _sem_win=        new ViewWin(_winctl, va_semblance_plot->SP()->W(),NULL);
   _cmp_win=        new ViewWin(_winctl, va_cmp_plot->SP()->W(),      NULL);
   _crossplot_win=  new ViewWin(_winctl, va_crossplot_plot->SP()->W(),NULL);
   _winctl->setMajor(ColumnMajor);

   addSeisPlot(_iso_plot->SP() );
   addSeisPlot(_grid_plot->SP() );
   addSeisPlot(_gvs_plot->SP() );
   addSeisPlot(_semblance_plot->SP() );
   addSeisPlot(_cmp_plot->SP() );
   addSeisPlot(_crossplot_plot->SP() );

   _iso_win->setOrder(0,0);
   _grid_win->setOrder(1,0);
   _gvs_win->setOrder(0,1);
   _crossplot_win->setOrder(1,1);
   _sem_win->setOrder(2,1);
   _cmp_win->setOrder(3,1);

   _iso_win->hide();
   _grid_win->hide();
   _gvs_win->hide();
   _sem_win->hide();
   _cmp_win->hide();
   _crossplot_win->hide();
   _crossplot_plot->getFileParameters(NULL);
}

VaPlotControl::~VaPlotControl()
{
   if (_fail_string) {
        free(_fail_string);
        _fail_string= NULL;
   }
}

void VaPlotControl::addOptions(SLPullPop *window_pull) 
{
  _window_pull= window_pull;
  _window_pull->addTog("GVS Window",       GVS);
  _window_pull->addTog("Semblance Window", SEM);
  _window_pull->addTog("CMP Window",       CMP);
  _window_pull->addTog("ISO Window",       VEL);
  _window_pull->addTog("Grid Window",      VGRID);
  _window_pull->addTog("Crossplot Window",   CROSSPLOT);
  updateOptions();
}

void VaPlotControl::calledFromWindow(int ident) 
{
    switch (ident) {
          case GVS:
                    if (_window_pull->toggleValue(ident)) _gvs_win->show();
                    else                                  _gvs_win->hide();
                    setMasterScrollBar();
                    break;
          case CMP:
                    if (_window_pull->toggleValue(ident)) _cmp_win->show();
                    else                                  _cmp_win->hide();
                    setMasterScrollBar();
                    break;
          case SEM:
                    if (_window_pull->toggleValue(ident)) _sem_win->show();
                    else                                  _sem_win->hide();
                    setMasterScrollBar();
                    break;
          case VEL:
                    if (_window_pull->toggleValue(ident)) {
                           _iso_win->show();
                           plot(VEL);
                    }
                    else {
                          _iso_win->hide();
                          //_iso_plot->SP()->cleanup();
                    }
                    break;
          case VGRID:
                    if (_window_pull->toggleValue(ident)) {
                        _grid_win->show();
                        plot(VGRID);
                    }
                    else {
                        _grid_win->hide();
                        _grid_plot->SP()->cleanup();
                    }
                    break;
          case CROSSPLOT:
                    if (_window_pull->toggleValue(ident)) {
                        _crossplot_win->show();
                        plot(CROSSPLOT);
                    }
                    else {
                        _crossplot_win->hide();
                        _crossplot_plot->SP()->cleanup();
                    }
                    setMasterScrollBar();
                    break;
           default:
                    assert(0);
                    break;
    }



}

Boolean VaPlotControl::isShowing(int ident) 
{
    Boolean retval;
    switch (ident) {
          case GVS:
                    retval = _gvs_win->isShowing();
                    break;
          case CMP:
                    retval = _cmp_win->isShowing();
                    break;
          case SEM:
                    retval = _sem_win->isShowing();
                    break;
          case VEL:
                    retval = _iso_win->isShowing();
                    break;
          case VGRID:
                    retval = _grid_win->isShowing();
                    break;
          case CROSSPLOT:
                    retval = _crossplot_win->isShowing();
                    break;
           default:
                    assert(0);
                    break;
    }
    return retval;
}

void VaPlotControl::updateOptions() 
{
   _window_pull->setToggleValue( VEL,      _iso_win->isShowing()  );
   _window_pull->setToggleValue( VGRID,    _grid_win->isShowing() );
   _window_pull->setToggleValue( SEM,      _sem_win->isShowing()  );
   _window_pull->setToggleValue( GVS,      _gvs_win->isShowing()  );
   _window_pull->setToggleValue( CMP,      _cmp_win->isShowing()  );
   _window_pull->setToggleValue( CROSSPLOT,  _crossplot_win->isShowing()  );

   _window_pull->sensitive( (Boolean)_gvs_plot->SP()->isPlotDisplayed(), 
                            GVS, -1 );
   _window_pull->sensitive( (Boolean)_cmp_plot->SP()->isPlotDisplayed(), 
                            CMP, -1 );
   _window_pull->sensitive( (Boolean)_semblance_plot->SP()->isPlotDisplayed(), 
                            SEM, -1 );
   if (_manager->activeDataset()->numVelocityFunctions() > 0)
         _window_pull->sensitive( True, VEL, -1 );
   else
         _window_pull->sensitive( False, VEL, -1 );
   _window_pull->sensitive( (Boolean)True, VGRID, -1 );
   _window_pull->sensitive( (Boolean)True, CROSSPLOT, -1 );
   
}

VaIsoPlot        *VaPlotControl::iso()        { return _iso_plot; }
VaGridPlot       *VaPlotControl::grid()       { return _grid_plot; }
VaSemblancePlot  *VaPlotControl::semblance()  { return _semblance_plot; }
VaGvsPlot        *VaPlotControl::gvs()        { return _gvs_plot; }
VaCmpPlot        *VaPlotControl::cmp()        { return _cmp_plot; }
VaCrossplotPlot  *VaPlotControl::crossplot()  { return _crossplot_plot; }
VaEtaPlot        *VaPlotControl::eta()        { return _eta_plot; }
VfManager *VaPlotControl::manager()   { return _manager; }

VaTraceFile  *VaPlotControl::semFile() { return _sem_file; }
VaTraceFile  *VaPlotControl::cmpFile() { return _cmp_file; }
VaTraceFile  *VaPlotControl::gvsFile() { return _gvs_file; }
VfFileBase *VaPlotControl::velFile() { return _vel_file; }

void VaPlotControl::updateOthers(FileBase *set_from)
{
        char *file_root= str_newstr(set_from->inputFilename());
        char *loc= NULL;

        if (set_from == _gvs_file) {
               loc= strstr(file_root, "cvst.trc");
               if (!loc) loc= strstr(file_root, "gvs.trc");
        } 
        else if (set_from == _cmp_file) {
               loc= strstr(file_root, "svat.trc");
        } 
        else if (set_from == _sem_file) {
               loc= strstr(file_root, "svas.trc");
        }
        else if (set_from == _vel_file) {
               loc= strstr(file_root, ".vel");
        }
        else 
               assert(0);

       if (loc) *loc= '\0';

       setIfEmpty(_gvs_file, file_root);
       setIfEmpty(_cmp_file, file_root);
       setIfEmpty(_sem_file, file_root);
       setIfEmpty(_vel_file, file_root);

       free (file_root);
}

void VaPlotControl::setIfEmpty(FileBase *set_to, 
                               char *file_root)
{
   if (strlen(set_to->inputFilename()) == 0) {
       char *end_stuff;
       char path[300];
       Boolean set_a_file= False;
       int i;
       char *new_file_root= (char*)malloc( strlen(file_root) + 2);
       strcpy(new_file_root, file_root);

       if      (set_to == _gvs_file) end_stuff= "cvst.trc";
       else if (set_to == _cmp_file) end_stuff= "svat.trc";
       else if (set_to == _sem_file) end_stuff= "svas.trc";
       else if (set_to == _vel_file) end_stuff= ".vel";
       else    assert(0);
 
       for(i= 0; ( (i<2)&& !set_a_file); i++) {
           if (i==1) {  // try another root
              int len= strlen(new_file_root);
              if (new_file_root[len-1] == '_')
                    new_file_root[len-1] = '\0';
              else if (new_file_root[len-1] != '_') {
                    new_file_root[len] = '_';
                    new_file_root[len+1] = '\0';
              }
           }
           sprintf(path, "%s%s", new_file_root, end_stuff);
 
           if (access(path, R_OK) == 0) {
               set_to->setInputFilename(path);
               set_a_file= True;
           } // end if
           else if (set_to == _gvs_file) {
               sprintf(path, "%s%s", new_file_root, "gvs.trc");
               if (access(path, R_OK) == 0) {
                  set_to->setInputFilename(path);
                  set_a_file= True;
               }
           } // end if
       }
       free(new_file_root);
   } // end if
}


VaPlotControl::PlotStat VaPlotControl::plot(PlotType which_plot) 
{

   PlotStat retval;
   switch (which_plot)
     {
       case SEM:
         if(_semblance_plot->plot()) {
            retval= VaSuccess;
         }
         else {
            retval= VaFail;
         }
       break;
 
       case GVS:
         if(_gvs_plot->plot()) {
            retval= VaSuccess;
         }
         else {
           retval= VaFail;
         }
       break;
 
       case CMP:
         if(_cmp_plot->plot()) {
           retval= VaSuccess;
         }
         else {
           retval= VaFail;
         }
       break;
 
       case VEL:
         _iso_plot->getFileParameters((char*)_vel_file->inputFilename());
         if(_iso_plot->plot()) {
           retval= VaSuccess;
         }
         else {
           retval= VaFail;
         }
       break;
 
       case VGRID:
         _grid_plot->getFileParameters(NULL);
         if(_grid_plot->plot()) {
           retval= VaSuccess;
         }
         else {
           retval= VaFail;
         }
       break;

       case CROSSPLOT:
         if(_crossplot_plot->plot()) {
           retval= VaSuccess;
         }
         else {
           retval= VaFail;
         }
       break;

       case ETA:
         if(_eta_plot->plot()) {
            retval= VaSuccess;
         }
         else {
            retval= VaFail;
         }
       break;

       case ALLPLOTS:
                retval= plotEverything();
       break;
 
       default:
       break;
     } // end switch
 
 return retval;

}

static void addToError(char *basestr, char *labelstr, VaPlot *plot)
{
   char *errstr= plot->SP()->lastError();
   strcat(basestr, "\n");
   strcat(basestr, labelstr);
   strcat(basestr, "\n");
   strcat(basestr, errstr);
   strcat(basestr, "\n");
   free(errstr);
}

static void checkShowOrHide(ViewWin *win, VaPlot *plot)
{
   if (plot->SP()->isPlotDisplayed()) {
            win->show();
   }
   else {
            win->hide();
   }
}

VaPlotControl::PlotStat VaPlotControl::plotIfNecessary(VaPlot   *plot_type, 
                                                       PlotType  which,
                                                       FileBase *file_base)
{
  PlotStat stat= VaSuccess;
  if (which == CROSSPLOT || which == VGRID) {
       if (_manager->activeDataset()->numVelocityFunctions() > 1)
            stat= plot(which);
  }
  else {
       if ( ((plot_type->SP()->fileHasNeverBeenPlotted()  &&
              file_base->inputValidity() == FileBase::VALID_YES)) ||
            (_replot_all &&
              file_base->inputValidity() == FileBase::VALID_YES) ) {
            plot_type->setAbleToPlot(True);
            stat= plot(which);
       }
       if (file_base->inputValidity() != FileBase::VALID_YES) {
                  plot_type->setAbleToPlot(False);
                  plot_type->SP()->cleanup();
       }
  }
  return stat;
}

VaPlotControl::PlotStat VaPlotControl::plotEverything() 
{
   PlotStat retval= VaSuccess;
   char tmp_fail_string[1000]= ""; 


   _i_am_plotting= True;
   int gvs_stat=       plotIfNecessary(_gvs_plot, GVS, _gvs_file);
   int cmp_stat=       plotIfNecessary(_cmp_plot, CMP, _cmp_file);
   int sem_stat=       plotIfNecessary(_semblance_plot, SEM, _sem_file);
   //int crossplot_stat= plotIfNecessary(_crossplot_plot,CROSSPLOT,_vel_file);
   //int grid_stat=      plotIfNecessary(_grid_plot, VGRID, NULL);
   
   _replot_all= False;

   if (_fail_string) {
        free(_fail_string);
        _fail_string= NULL;
   }

   if (gvs_stat != VaSuccess) {
       addToError(tmp_fail_string, "GVS:", _gvs_plot);
       retval= VaPartialSuccess;
   }
   if (cmp_stat != VaSuccess) {
       addToError(tmp_fail_string, "CMP:", _cmp_plot);
       retval= VaPartialSuccess;
   }
   if (sem_stat != VaSuccess) {
       addToError(tmp_fail_string, "Semblance:", _semblance_plot);
       retval= VaPartialSuccess;
   }
/*
   if (grid_stat != VaSuccess) {
       addToError(tmp_fail_string, "Grid:", _grid_plot);
       retval= VaPartialSuccess;
   }
   if (crossplot_stat != VaSuccess) {
       addToError(tmp_fail_string, "Crossplot:", _crossplot_plot);
       retval= VaPartialSuccess;
   }
*/

   if ( (gvs_stat     != VaSuccess) &&
        (cmp_stat     != VaSuccess) &&
        (sem_stat     != VaSuccess) ) {
//        (grid_stat    != VaSuccess) &&
//        (crossplot_stat != VaSuccess) ) {
          retval= VaFail;
   }

   checkShowOrHide(_gvs_win,     _gvs_plot);
   checkShowOrHide(_cmp_win,     _cmp_plot);
   checkShowOrHide(_sem_win,     _semblance_plot);

   VfDataset *dataset= _manager->activeDataset();
/*
   if (_semblance_plot->SP()->isPlotDisplayed()) {
          dataset->findOrInsertVelfun( _semblance_plot->getDisplayedXbin(),
                                       _semblance_plot->getDisplayedYbin());
   }
   else if (_cmp_plot->SP()->isPlotDisplayed()) {
          dataset->findOrInsertVelfun( _cmp_plot->getDisplayedXbin(),
                                       _cmp_plot->getDisplayedYbin());
   }
*/

   if (_grid_plot->SP()->isPlotDisplayed() && 
           ( _grid_win->isShowing() || is3d() ) )

            _grid_win->show();
   else
            _grid_win->hide();

   if (strlen(tmp_fail_string) > 0) _fail_string= str_newstr(tmp_fail_string);

   setMasterScrollBar();
   updateOptions();

   
   
   _i_am_plotting= False;
   return retval;
}


char *VaPlotControl::getFailString()
{
  return _fail_string;
}

void VaPlotControl::setReplotAll(Boolean s)
{
  _replot_all= s; 
}

static void ifShowingSlaveTo(ViewWin   *slave, 
                             SeisPlot  *slave_sp,
                             SeisPlot  *master_sp)
{
   SeisScrWin *slave_ssw= slave_sp->getSeisWinMan()->scrolledWindow();
   SeisScrWin *master_ssw= master_sp->getSeisWinMan()->scrolledWindow();

   master_ssw->slaveVertSBTo(NULL);
   if (slave->isShowing())   {
       slave_ssw->slaveVertSBTo( master_ssw );
       //slave_sp->showBorders(False, False, True, False);
       //slave_ssw->setLeftAnnotationOn(False);
       slave_ssw->setRightAnnotationOn(False);
   }
}

void VaPlotControl::setMasterScrollBar()
{
   if (_cmp_win->isShowing()) {
        SeisPlot *sp= _cmp_plot->SP();
        SeisScrWin *ssw= sp->getSeisWinMan()->scrolledWindow();
        //sp->showBorders(False, True, True, False);
        //ssw->setLeftAnnotationOn(False);
        ssw->setRightAnnotationOn(True);
        ifShowingSlaveTo(_sem_win, _semblance_plot->SP(), sp );
        ifShowingSlaveTo(_crossplot_win, _crossplot_plot->SP(), sp );
        ifShowingSlaveTo(_gvs_win, _gvs_plot->SP(), sp );
   }
   else if (_sem_win->isShowing()) {
        SeisPlot *sp= _semblance_plot->SP();
        SeisScrWin *ssw= sp->getSeisWinMan()->scrolledWindow();
        //sp->showBorders(False, True, True, False);
        //ssw->setLeftAnnotationOn(False);
        ssw->setRightAnnotationOn(True);
        ifShowingSlaveTo(_gvs_win, _gvs_plot->SP(), sp );
        ifShowingSlaveTo(_crossplot_win, _crossplot_plot->SP(), sp );
   }
   else if (_crossplot_win->isShowing()) {
        SeisPlot *sp= _crossplot_plot->SP();
        SeisScrWin *ssw= sp->getSeisWinMan()->scrolledWindow();
        //sp->showBorders(False, True, True, False);
        //ssw->setLeftAnnotationOn(False);
        ssw->setRightAnnotationOn(True);
        ifShowingSlaveTo(_gvs_win, _gvs_plot->SP(), sp );
   }
   else if (_gvs_win->isShowing()) {
        SeisPlot *sp= _gvs_plot->SP();
        SeisScrWin *ssw= sp->getSeisWinMan()->scrolledWindow();
        ssw->slaveVertSBTo(NULL);
        //sp->showBorders(False, True, True, False);
        //ssw->setLeftAnnotationOn(False);
        ssw->setRightAnnotationOn(True);
   }

}

VaVectColors *VaPlotControl::vectorColors()
{
   return _vect_colors;
}

Boolean VaPlotControl::is3d()
{
 VfDataset *ds= _manager->activeDataset();
 Boolean x= ( (ds->minimumXbinCenter() - ds->maximumXbinCenter()) != 0 );
 Boolean y= ( (ds->minimumYbinCenter() - ds->maximumYbinCenter()) != 0 );
 return (x && y);
}

Boolean VaPlotControl::nothingShowing()
{
   Boolean retval= True;
   if (_grid_win->isShowing()      ||
       _gvs_win->isShowing()       ||
       _crossplot_win->isShowing() ||
       _sem_win->isShowing()       ||
       _cmp_win->isShowing() )
            retval= False;

   return retval;
}

void VaPlotControl::postTotalChanges(VfDataset *dataset)
{
  if (dataset == _manager->activeDataset()) {
      _crossplot_plot->getFileParameters(NULL);
      _grid_plot->getFileParameters(NULL);
      _i_am_plotting= True;
      //PlotStat stat= plot(VEL);
      if (_iso_win->isShowing() || nothingShowing() ) {
                   plot(VEL);
                   checkShowOrHide(_iso_win, _iso_plot);
      }
      updateOptions();
      _i_am_plotting= False;
  } // end if
}

void VaPlotControl::newPlot(SeisPlot *sp)
{

  if (!_i_am_plotting) {
      if (sp == _iso_plot->SP() ) {
        // _window_pull->setToggleValue( VEL,    True );
         _window_pull->sensitive( (Boolean)True, VEL, -1 );
         //_iso_win->show();
      }
      else if (sp == _grid_plot->SP() ) {
         _window_pull->setToggleValue( VGRID,    True );
         _window_pull->sensitive( (Boolean)True, VGRID, -1 );
         _grid_win->show();
      }
      else if (sp == _gvs_plot->SP() ) {
      }
      else if (sp == _semblance_plot->SP() ) {
      }
      else if (sp == _cmp_plot->SP() ) {
      }
      else if (sp == _eta_plot->SP() ) {
      }
      else if (sp == _crossplot_plot->SP() ) {
         _window_pull->setToggleValue( CROSSPLOT,    True );
         _window_pull->sensitive( (Boolean)True, CROSSPLOT, -1 );
         _crossplot_win->show();
         setMasterScrollBar();
      }
  }
}

void VaPlotControl::showIsoWindow(Boolean show)
{
   if(show)
     {
     _iso_win->show();
     _window_pull->setToggleValue( VEL,    True );
     }
   else
     {
     _iso_win->hide();
     }
}
