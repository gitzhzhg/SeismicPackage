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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================



//=========================================================================
//========== Semblance plot class                                ==========
//========== Author Michael L. Sherrill 08/97                    ==========
//=========================================================================

// $Id: va_semblance_plot.cc,v 1.6 2004/11/03 19:14:55 wjdone Exp $
// $Name:  $

#include "vaplots/va_plot.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_semblance_gui.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_semblance_picks.hh"
#include "vaplots/va_common_params.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sp/seis_plot.hh"



VaSemblancePlot::VaSemblancePlot(VfManager         *vf_manager, 
                                 class VfHorizons  *horizons,
                                 Widget w,          char *name,
                                 HelpCtx            hctx,
                                 VaPlotCommonParams *common_params,
                                 VaVectColors       *vect_colors,
                                 long               numcolors,
                                 long               numcontourcolors)
                                 : VaPlot(vf_manager, w, name)
{
  _numcolors = numcolors;
  _numcontourcolors = numcontourcolors;
  _va_picks = new VaSemblancePicks(vf_manager, horizons, this, _sp,
                                   vect_colors);
  setCommonParams(common_params);
  _gui = new VaSemblanceGui(w, "Semblance Gui", hctx, this);
  if(_numcolors) _sp->setLoadableColors((int)_numcolors);
  if(_numcontourcolors) _sp->setContourColors((int)_numcontourcolors);
  _plot_type = SEMBLANCE;
}

VaSemblancePlot::~VaSemblancePlot()
{
  delete _va_picks;
}


//=========================================================================
//================== New seis plot in window  =============================
//=========================================================================
void VaSemblancePlot::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
  _sp = sp->currentSPInWindow();
  _gui->seisPlotChanged();
  //may need to notify picker via _va_picks here
}

//===========================================================================
//============= Return ptr to the dialog                       ==============
//===========================================================================
SLShellContainer *VaSemblancePlot::getDialog()
{
  return _gui;
}

void VaSemblancePlot::manageGui()
{
  _gui->makeAndManage();
}


void VaSemblancePlot::updateGui(Boolean update_file_limits)
{
  _gui->updateParams(update_file_limits);
}

int VaSemblancePlot::plot()
{
int stat;

  if(!needsReplotting()) return PlotImage::PlotSuccess;  
  if(!_able_to_plot) return PlotImage::ReadFail;

  setPlotParameters();
  stat = _sp->plot();
  if(stat == PlotImage::PlotSuccess)
    {
    _sp->backingStore(True);
    _common_params->setSemReplot(False);
    }
  return stat;

}




//===========================================================================
//============= Get trace file information                     ==============
//===========================================================================
int VaSemblancePlot::getFileParameters(char *filename)
{
long stat = PlotImage::DEFS_FAIL;
long max_traces = 0;
int user_aborted = 0;

  strcpy(_file_label,"Not successfull");

  allocateLocationArrays(MAX_LOCATIONS);

  _sp->allocateVelocityArray(MAX_LOCATIONS);

   _sp->setFilename(filename);

   //Not checking for variable trace groups  
   if(!_sp->usingSelector())
    {
    stat = _sp->getByteDefaults(_xlocations, _ylocations, getActiveXheader(), 
                       getActiveYheader(), &_number_locations_in_file);  
    if(stat != PlotImage::DEFS_OK) return stat;

    _traces_per_group = _sp->totalTraces() / _number_locations_in_file;
    }
  else
    {
    stat= _sp->findTracesByPattern(_sp->filename(), getActiveXheader(),
                                 getActiveYheader(),
                                 _sp->totalTraces(),&_number_locations_in_file, 
                                 &user_aborted, _xlocations, _ylocations);
    if(!stat) 
       return PlotImage::DEFS_FAIL;
    else
       stat = PlotImage::DEFS_OK;

    for(int i = 0; i < _number_locations_in_file; i++)
       if(max_traces < _sp->getSelectorNumTraces(i))
          max_traces = _sp->getSelectorNumTraces(i);
    
    _traces_per_group = max_traces;
    }

  // set traces per panel in PlotImage
  for (int i = 0; i < _number_locations_in_file; i++) {
      _sp->setTracesPerPanel(i, _traces_per_group);
  }


  _tmin = _sp->minTmin();
  _tmax = _sp->maxTmax();
  _velocity_min = _sp->minVel();
  _velocity_max = _sp->maxVel();
  _sp->setGridX1(_velocity_min);
  _sp->setGridX2(_velocity_max);

  updateGui();

  dataReadByXYheaders(getActiveXheader(),getActiveYheader());


sprintf(_file_label,"Panels: %d,  Traces/Panel: %d,  tmin: %4.3f,  tmax: %4.3f",
        numberLocationsInFile(), tracesPerGroup(), _sp->minTmin(),
        _sp->maxTmax());

  return stat;  
}



void VaSemblancePlot::setPlotParameters()
{

  _gui->setParameters();

}

//=========================================================================
//============= Change movie panel or read from file            ===========
//=========================================================================
void VaSemblancePlot::changePanel(float x, float y, 
                                  float /*time*/, float /*velocity*/)
{
int stat = PlotImage::PlotSuccess;
int frame;

      if(!_able_to_plot) return;

      if(!_sp->isPlotDisplayed()) return;
  
      //see if we can movie to new location
      if( (frame = getDisplayedPanelIndexFromXY(x, y))  >= 0 && 
          _blank_image == False)
        {
        if(_sp->currentFrame() != frame) _sp->movieToFrame(frame);
        return;
        }

      //Movie didnt match so see if we can read from file
      if( (frame = getFilePanelIndexFromXY(x, y)) >= 0)
        {
        //_first_trace = frame * _sp->originalTraces() + 1;
        _first_trace = frame * tracesPerGroup() + 1;
        updateGui(False);
        stat = plot();
        if(stat == PlotImage::PlotSuccess && _sp->movie())
          if( (frame = getDisplayedPanelIndexFromXY(x, y))  >= 0)
            if(_sp->currentFrame() != frame) _sp->movieToFrame(frame);
        _blank_image = False;
        return;
        }

      //Location is not in file so blank the image out
      _sp->blankImage(x,y);
      _blank_image = True;


}

//=========================================================================
//============= Get number of semblance gathers in the file     ===========
//=========================================================================
long VaSemblancePlot::getNumberPanelsInFile()
{
  return _number_locations_in_file;
}

//=========================================================================
//============= Get xlocation of a gather in the file     =================
//=========================================================================
const float VaSemblancePlot::getPanelXlocation(long i)
{
  return getXlocation(i);
}

//=========================================================================
//============= Get xlocation of a gather in the file     =================
//=========================================================================
const float VaSemblancePlot::getPanelYlocation(long i)
{
  return getYlocation(i);
}
//=========================================================================
//============= Get number movie panels displayed               ===========
//=========================================================================
long VaSemblancePlot::getMovieTotalPanels()
{
  return _sp->frames();
}

//=========================================================================
//============= Get first movie panel displayed                 ===========
//=========================================================================
long VaSemblancePlot::getMovieFirstPanel()
{
  return _gui->getFirstMoviePanel();
}

//=========================================================================
//============= Get skipped panels                              ===========
//=========================================================================
long VaSemblancePlot::getMovieSkipPanels()
{
  return _gui->getSkipMoviePanels();
}

//=========================================================================
//============= Set first panel to plot                         ===========
//=========================================================================
void VaSemblancePlot::setFirstPanelToPlot(long first_panel)
{

  _gui->setFirstPanelToPlot(first_panel);
}



//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//===========================================================================
//============  Informs only after this point                ================
//===========================================================================
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

void VaSemblancePlot::postNewActiveVelocityFunction (VfDataset *vfd)
{
float x,y;

  if(!_able_to_plot) return;

  if(!_sp->isPlotDisplayed()) return; 

  if((vfd != _vf_manager->activeDataset())
  || (vfd->getActiveVelocityFunction() == -1)) return;

  x = vfd->getXloc(vfd->getActiveVelocityFunction());
  y = vfd->getYloc(vfd->getActiveVelocityFunction()); 

  changePanel(x,y);
}

void VaSemblancePlot::postChangeCoords(VfDataset *vfd,long ifun,long /*nchng*/)
{
  if(!_able_to_plot) return;

  if(!_sp->isPlotDisplayed()) return;

  if(vfd != _vf_manager->activeDataset()) return;

  changePanel(vfd->getXloc(ifun),vfd->getYloc(ifun));
}

void VaSemblancePlot::postChangeBinTolerances()
{
VfDataset *vfd = _vf_manager->activeDataset();
long ifun = vfd->getActiveVelocityFunction();
  

  if(!_able_to_plot) return;

  if(!_sp->isPlotDisplayed()) return;

  if(ifun == -1) return;

  changePanel(vfd->getXloc(ifun),vfd->getYloc(ifun));
}

void VaSemblancePlot::copyPicksFromPrevPanelFunc(VfDataset &vfds,
                                                 long prev_func,
                                                 long new_func) {

    // copies the abscissa and ordinate arrays from function "prev_func" of
    // the specified velocity dataset to function "new_func" of the same
    // dataset

    // bail out if there are picks in the new function
    if (vfds.numPicks(new_func) > 0 || vfds.numPicks(prev_func) == 0) {
        /* debug
        fprintf(stdout,"Abort copy of previous function %ld with %d picks\n",
                prev_func,vfds.numPicks(prev_func));
        fprintf(stdout,"    to new function %ld with %d picks\n",new_func,
                vfds.numPicks(new_func));
         */
        return;
    }
    else {
        fprintf(stdout,"Copy previous function %ld with %d picks\n",
                prev_func,vfds.numPicks(prev_func));
        fprintf(stdout,"    to new function %ld with %d picks\n",new_func,
                vfds.numPicks(new_func));
    }

    // get temp arrays to hold abscissa and ordinate arrays
    float *absvalues = new float[vfds.numPicks(prev_func)];
    float *ordvalues = new float[vfds.numPicks(prev_func)];

    // reset the number of picks on new function to number of previous function
    vfds.resetNumPicks(new_func, vfds.numPicks(prev_func));

    // get pick data from prev_func and copy to new_func
    vfds.getAbscissaArray(prev_func, absvalues);
    vfds.getOrdinateArray(prev_func, ordvalues);
    int type = vfds.getDefaultType(prev_func);
    vfds.replaceAllPicks(new_func, absvalues, ordvalues, type);

    delete [] absvalues;
    delete [] ordvalues;
}

void VaSemblancePlot::copyPicksFromSourceFunc(VfDataset &vfds, long nearfun,
                                              long actfun) {

    // copies the abscissa and ordinate arrays from function "nearfun" of
    // the specified velocity dataset to function "actfun" of the same
    // dataset

    // get temp arrays to hold abscissa and ordinate arrays
    float *absvalues = new float[vfds.numPicks(nearfun)];
    float *ordvalues = new float[vfds.numPicks(nearfun)];

    // delete the picks in the current active function
    vfds.deleteAllVelocityFunctionPicks(actfun);

    // reset the number of picks on active function to number of near function
    vfds.resetNumPicks(actfun, vfds.numPicks(nearfun));

    // get pick data from nearfun and copy to actfun
    vfds.getAbscissaArray(nearfun, absvalues);
    vfds.getOrdinateArray(nearfun, ordvalues);
    int type = vfds.getDefaultType(nearfun);
    vfds.replaceAllPicks(actfun, absvalues, ordvalues, type);

    delete [] absvalues;
    delete [] ordvalues;
}

void VaSemblancePlot::copyPicksFromComparisonFunc(VfDataset &compvfds,
                                                  long nearfun,
                                                  VfDataset &vfds,
                                                  long actfun) {

    // copies the abscissa and ordinate arrays from function "nearfun" of
    // the comparison velocity dataset to function "actfun" of the active
    // dataset

    // get temp arrays to hold abscissa and ordinate arrays
    float *absvalues = new float[compvfds.numPicks(nearfun)];
    float *ordvalues = new float[compvfds.numPicks(nearfun)];

    // delete the picks in the current active function
    vfds.deleteAllVelocityFunctionPicks(actfun);

    // reset the number of picks on active function to number of near function
    vfds.resetNumPicks(actfun, compvfds.numPicks(nearfun));

    // get pick data from nearfun and copy to actfun
    compvfds.getAbscissaArray(nearfun, absvalues);
    compvfds.getOrdinateArray(nearfun, ordvalues);
    int type = compvfds.getDefaultType(nearfun);
    vfds.replaceAllPicks(actfun, absvalues, ordvalues, type);

    delete [] absvalues;
    delete [] ordvalues;
}


void VaSemblancePlot::registerEtaPlot(VaEtaPlot *eta)
{
VaSemblancePicks *picks = (VaSemblancePicks *)_va_picks;
  picks->registerEtaPlot(eta);
}


void VaSemblancePlot::showEtaOverlays()
{
VaSemblancePicks *picks = (VaSemblancePicks *)_va_picks;

  picks->showEtaOverlays();
}

void VaSemblancePlot::hideEtaOverlays()
{
VaSemblancePicks *picks = (VaSemblancePicks *)_va_picks;

  picks->hideEtaOverlays();
}

void VaSemblancePlot::copyPrevPanelVelocityFunc(long prev_func, long new_func) {

    VfDataset *actDset = _vf_manager->activeDataset();
    if (!actDset) {
        fprintf(stdout,"No active velocity dataset\n");
        return;
    }
    long actDsetIndx = actDset->indexOfThisDataset();
    long actfun = actDset->getActiveVelocityFunction();

    // the new_func should already have been set active
    assert(actfun == new_func);

    // handle the copy
    if (_gui->isAutoCopySelected() && prev_func >= 0) {
        if (prev_func >= 0) {
            copyPicksFromPrevPanelFunc(*actDset, prev_func, new_func);
        }
    }
}

void VaSemblancePlot::copySourceVelocityFunc(long option) {

    VfDataset *actDset = _vf_manager->activeDataset();
    if (!actDset) {
        fprintf(stdout,"No active velocity dataset\n");
        return;
    }

    long actDsetIndx = actDset->indexOfThisDataset();
    long actfun = actDset->getActiveVelocityFunction();
    long nearfun;
    if (option == SEMB_COPY_COMPARISON) {
        VfDataset *compDset;
        nearfun = getComparisonVelocityFuncIndex(option, compDset);
        if (nearfun >= 0) {
            fprintf(stdout,"Copy comparison function %ld to %ld\n",
                    nearfun, actfun);
            copyPicksFromComparisonFunc(*compDset, nearfun, *actDset, actfun);
        }
    }
    else {
        nearfun = getSourceVelocityFuncIndex(option);
        if (nearfun >= 0) {
            fprintf(stdout,"Copy function %ld to %ld\n",nearfun, actfun);
            copyPicksFromSourceFunc(*actDset, nearfun, actfun);
        }
    }
}

long VaSemblancePlot::getSourceVelocityFuncIndex(long option) {

    VfDataset *actDset = _vf_manager->activeDataset();
    if (!actDset) {
        fprintf(stdout,"No active velocity dataset\n");
        return -1L;
    }

    long actDsetIndx = actDset->indexOfThisDataset();
    long actfun = actDset->getActiveVelocityFunction();
    long nearfun;

    switch(option) {
        case SEMB_COPY_PREV_INLINE:
            nearfun = actDset->findPrevXloc();
            break;
        case SEMB_COPY_NEXT_INLINE:
            nearfun = actDset->findNextXloc();
            break;
        case SEMB_COPY_PREV_XLINE:
            nearfun = actDset->findPrevYloc();
            break;
        case SEMB_COPY_NEXT_XLINE:
            nearfun = actDset->findNextYloc();
            break;
        case SEMB_COPY_REFERENCE:
            nearfun = actDset->getReferenceVelocityFunction();
            break;
        default:
            assert(0);
    }
    return nearfun;
}

long VaSemblancePlot::getComparisonVelocityFuncIndex(long option,
                                                     VfDataset* &compDset) {

    VfDataset *actDset = _vf_manager->activeDataset();
    compDset = (VfDataset *) 0;
    if (!actDset) {
        fprintf(stdout,"No active velocity dataset\n");
        return -1L;
    }

    long actDsetIndx = actDset->indexOfThisDataset();
    long actfun = actDset->getActiveVelocityFunction();

    long numEditDsets = _vf_manager->numEditableDatasets();
    long numDsets = _vf_manager->numDatasets();
    assert(numDsets > numEditDsets);

    // get the comparison dataset index. since the dataset indices are
    // a zero based index, and the editable datasets are first in the
    // array of datasets, the index of the first comparison dataset is
    // numEditDsets, so it should be the one to use. but when a comparison
    // dataset is added, the reference dataset index in the manager is
    // set to point at that reference dataset. so use the member function
    // on the manager to get the reference (comparison) dataset. this is
    // not to be confused with the reference function set on the Control
    // Panel dialog, which refers to a velocity function from the active
    // editable dataset, not the comparison dataset.
    long compDsetIndx = _vf_manager->getReferenceDatasetIndex();
    compDset = _vf_manager->dataset(compDsetIndx);
    if (!compDset) {
        fprintf(stdout,"No comparison dataset with index %ld\n",
                compDsetIndx);
        return -1L;
    }

    // find the velocity function in the comparison dataset that
    // is closest to the active editable dataset
    float activeXloc = actDset->getXloc(actfun);
    float activeYloc = actDset->getYloc(actfun);
    long nearfun = compDset->findMatchingVelfun(activeXloc, activeYloc);
    if (nearfun < 0) {
        fprintf(stdout,"Failed to find matching velocity function in ");
        fprintf(stdout, "comparison dataset\n");
        compDset = (VfDataset *) 0;
        return -1L;
    }

    return nearfun;
}

void VaSemblancePlot::activateSnapMode(float vel_halfwin, float time_halfwin,
                                       float semb_t_min, float semb_t_max,
                                       float semb_v_min, float semb_v_max) {

    // pass info about snap parameters from gui class to pick class

    ((VaSemblancePicks *) _va_picks)->activateSnapMode(vel_halfwin,
                                                       time_halfwin,
                                                       semb_t_min, semb_t_max,
                                                       semb_v_min, semb_v_max);
}

void VaSemblancePlot::deactivateSnapMode() {

    // pass info about snap parameters from gui class to pick class

    ((VaSemblancePicks *) _va_picks)->deactivateSnapMode();
}
