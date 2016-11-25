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
// $Id: va_eta_overlays.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include <string.h>
#include "vaplots/va_eta_overlays.hh"
#include "vaplots/va_iso_picks.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_gvs_picks.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_xh_trans.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"
#include "sp/seis_plot.hh"

#include <assert.h>





//!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Some of this is temporary until the vel file format supports eta vals
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*********** Kludge until we have eta in main manager *****************

VaEtaOverlayData::VaEtaOverlayData(VfManager *eta_manager,  VaPicks *picks,
                                 VfManager *main_manager, VaVectColors *colors,
                                 SeisPlot *sp, VaPlot *semblance_plot)
                                 : VaPickData(eta_manager, picks),
                                  _colors(colors), _changing(0), 
                                  _alloc4change(0), _doing_post(0),
                                  _doing_total_changes(0), 
                                  _doing_rem_ins_func(0),
                                   _rem_ins_func(0),_hold_update(0),
                                  _eta_manager(eta_manager),
                                  _main_manager(main_manager),
                                   _sp(sp),_semblance_plot(semblance_plot)
{
  /* just initializers */
}

VaEtaOverlayData::~VaEtaOverlayData()
{
  if (_alloc4change)
    {
      delete [] _before ;
      delete [] _after  ;
      delete [] _changed;
    }
}

int VaEtaOverlayData::getNumPts(long /*id*/)
{
  float bin_tolerance = 1.0;
  float xloc, yloc;
  long eta_func;
  long ifun = _main_manager->activeDataset()->getActiveVelocityFunction();

  if(ifun < 0 || _hold_update || _doing_total_changes ) return 0;


  xloc = _main_manager->activeDataset()->getXloc(ifun);
  yloc = _main_manager->activeDataset()->getYloc(ifun);

  eta_func = _eta_manager->activeDataset()->findMatchingVelfun (xloc, yloc);
  if(eta_func != -1)
    {
      //The following can happen if the user scans to a location where there
      //are no semblance picks. The active function will be the location
      //before the scan so the eta_func will be for that location.
      //Also I am using my own tolerance of 1.0 for roundoff errors because
      //I believe that the semblance locations and the eta locations should
      //be close to equal in all cases?
      if(xloc - bin_tolerance <= _semblance_plot->getDisplayedXbin() &&
         xloc + bin_tolerance >= _semblance_plot->getDisplayedXbin() &&
         yloc - bin_tolerance <= _semblance_plot->getDisplayedYbin() &&
         yloc + bin_tolerance >= _semblance_plot->getDisplayedYbin()    )
        return _eta_manager->activeDataset()->numPicks(eta_func) * 2;
      else
        return 0;
    } 
  else
    {
      return 0;
    }

}

float VaEtaOverlayData::getX(int i, long /*id*/)
{
  int mod;

  mod = i % 2;

  if(!mod)
    return _sp->gridX1();
  else
    return _sp->gridX2();
}

float VaEtaOverlayData::getY(int i, long /*id*/)
{
  long ifun = _eta_manager->activeDataset()->getActiveVelocityFunction();

  return _eta_manager->activeDataset()->getAbscissa(ifun, i / 2, VTIN); 

}

int VaEtaOverlayData::getAltMarkerColor(int i, long /*id*/)
{
  int retval;

  VfDataset *ds = _eta_manager->activeDataset();

  retval = VaVectColors::SEL_COL;

  return retval;
}

int VaEtaOverlayData::isChanging()
{
  return _changing;
}

int VaEtaOverlayData::ignoreOther()
{
  return _doing_total_changes || _doing_rem_ins_func;
}

void VaEtaOverlayData::beforeChanges()
{
  _changing = 1;

  modIndicesBefore(0, getNumPts());
}

void VaEtaOverlayData::afterChanges()
{
  assert( _changing );

  /*
   * This if takes away a purify UMR if calling with
   * _rem_ins_func = 0 and _func_index, _func_nrem,  &_func_nins unset.
   */
  modIndicesAfter(0, getNumPts());
  modDone();
  _changing = 0;
}

void VaEtaOverlayData::preTotalChanges(VfDataset *dataset)
{
  assert(!_rem_ins_func);

  if (_eta_manager->activeDataset() == dataset)
    {
      int numPts = getNumPts();
      modIndicesBefore(0, numPts);
      _using_mod = 1;

      /*
       * For recoloring in afterChanges
       */
      _rem_ins_func = 1     ;
      _func_index   = 0     ;
      _func_nrem    = numPts;

      _doing_total_changes = 1;
    }
}

void VaEtaOverlayData::postTotalChanges(VfDataset *dataset)
{
  if (_eta_manager->activeDataset() == dataset)
    {
      int numPts = getNumPts();
      modIndicesAfter(0, numPts);
      modDone();
      _rem_ins_func = 0;
      /*
       * For recoloring in afterChanges
       */
      _func_nins = numPts;
      _doing_total_changes = 0;
    }
}

void VaEtaOverlayData::preNewActiveVelocityFunction(VfDataset * /*dataset*/)
{
  /* do it in afterChanges */
}

void VaEtaOverlayData::postNewActiveVelocityFunction(VfDataset *dataset)
{
  long eta_func;
  long ifun = _main_manager->activeDataset()->getActiveVelocityFunction();

  if(ifun < 0)
    { 
      return;
    }

  float xloc = _main_manager->activeDataset()->getXloc(ifun);
  float yloc = _main_manager->activeDataset()->getYloc(ifun);

  eta_func = _eta_manager->activeDataset()->findMatchingVelfun (xloc, yloc);

  if(eta_func >= 0)
    {
    _hold_update = 1;
    _eta_manager->activeDataset()->setActiveVelocityFunction(eta_func);
    _hold_update = 0;
    getNumPts();
    }
}


void VaEtaOverlayData::changeActiveVelocityFunction()
{
  postNewActiveVelocityFunction(NULL);
}

void VaEtaOverlayData::preRemoveInsertVelocityFunctions(VfDataset *dataset,
                                                        long ifun, long nrem, long /*nins*/)
{
  assert(!_rem_ins_func);

  if (_eta_manager->activeDataset() == dataset)
    {
      if (nrem)
        {
          modIndicesBefore((int) ifun, (int) nrem);
          _using_mod = 1;
        }

      /*
       * For recoloring in afterChanges
       */
      _rem_ins_func = 1;
      _func_index = (int) ifun;
      _func_nrem  = (int) nrem;

      _doing_rem_ins_func = 1;
    }
}

void VaEtaOverlayData::postRemoveInsertVelocityFunctions(VfDataset *dataset,
                                                         long ifun, long nrem, long nins)
{
  if (_eta_manager->activeDataset() == dataset)
    {
      if (nrem)
        {
          modIndicesAfter((int) ifun, (int) nins);
          modDone();
        }
      else
        {
          /*
           * Since no line, no need to erase anything if
           * only inserting functions.
           */
          modAttributes((int) ifun, (int) nins);
        }

      /*
       * For recoloring in afterChanges
       */
      _func_nins = (int) nins;
      _rem_ins_func = 0;
      _doing_rem_ins_func = 0;
    }
}




void VaEtaOverlayData::preNewActiveDataset()
{
  preTotalChanges(_eta_manager->activeDataset());
}

void VaEtaOverlayData::postNewActiveDataset()
{
  postTotalChanges(_eta_manager->activeDataset());
}



