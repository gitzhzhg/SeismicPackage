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



//=============================================================================
//=          Eta picking class                                                =
//=          Author Michael L. Sherrill 09/28/01                              =
//=============================================================================

// $Id: va_eta_picks.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

//!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Some of this is temporary until the vel file format supports eta vals
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#include <string.h>
#include "vaplots/va_eta_picks.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_eta_plot.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"

#include <assert.h>
#include <math.h>



#define FALLBACK "mouse*VA_ETA_PICK: BTN#1: Edit or Insert Pick, BTN#2: Delete Pick,  ETA PICKING MODE ACTIVE"


VaEtaPickData::VaEtaPickData(VfManager *manager, VaPicks *picks)
     : VaPickData(manager, picks), _num_points(0)
{

}

VaEtaPickData::~VaEtaPickData()
{
  /* do nothing */
}

int VaEtaPickData::getNumPts(long /*junk*/)
{
  return _num_points;
}







//============================================================================
//================= VaEtaPicker ==============================================
//============================================================================
VaEtaPicker::VaEtaPicker(PlotBase *plot, VfManager *manager,
                         VaPicks *picks, VectorLinkedList *vectors,
                         SeisPlot *semblance_plot)
  : VaPicker(plot, "", "VA_ETA_PICK", FALLBACK,
             manager, picks, vectors)
{
  _eta_picks = (VaEtaPicks *)picks;
  _eta_plot  = (VaEtaPlot *)picks->getPlot();
  _yval      = _eta_plot->getPickYval();
  _sp        = semblance_plot;
}

VaEtaPicker::~VaEtaPicker()
{
  /* do nothing */
}


//Temporarily overwrite the base class so we can check the temporary dataset
//Instead of the main
int VaEtaPicker::canEdit()
{
  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();
  return ds->isEditable() && ds->notLocked();
}


void VaEtaPicker::bracketTime(float time, int vel_type, int deleting)
{
  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();
  long       ifun     = ds->getActiveVelocityFunction();
  int        numPicks = (int) ds->numPicks(ifun);

  float time_tol;

  /*
   * If y-axis is depth instead of time, I should do
   * some conversion here.  But for now I'll let it go.
   */
  
  float mmx, mmy;
  _sp->MM2WC(&mmx, &mmy);//This is the semblance plot
  time_tol = _manager->utilities()->getPickingTolerance() * mmy;

  float pick_time;

  /*
   * Init i_first_bigger to numPicks, then if none are
   * bigger we are left with the correct result.
   */
  int i_first_bigger = numPicks;

  for (_index = 0; _index < numPicks; _index++)
    {
      pick_time = ds->getAbscissa(ifun, (long) _index, vel_type);

      if (fabs((double) (pick_time - time)) <= time_tol)
        {
          _insert = 0;

          if (_index == 0)
            {
              _use_t_min = 0;
            }
          else
            {
              _use_t_min = 1;
              _t_min = ds->getAbscissa(ifun,
                                       (long) (_index - 1), vel_type) +
                time_tol;
            }

          if (_index == numPicks - 1)
            {
              _use_t_max = 0;
            }
          else
            {
              _use_t_max = 1;
              _t_max = ds->getAbscissa(ifun,
                                       (long) (_index + 1), vel_type) -
                time_tol;
            }

          return;
        }
      else if ((i_first_bigger == numPicks)
               && (pick_time      >      time))
        {
          i_first_bigger = _index;
        }
    }


  if(deleting && _index == numPicks)//did not find pick on delete
    {
     _index = -1;
     return;
    }

  /*
   * If we get this far, we are inserting a new pick.
   */
  _insert = 1;

  _index = i_first_bigger;

  if (_index == 0)
    {
      _use_t_min = 0;
    }
  else
    {
      _use_t_min = 1;
      _t_min = ds->getAbscissa(ifun,
                               (long) (_index - 1), vel_type) +
        time_tol;
    }

  if (_index == numPicks)
    {
      _use_t_max = 0;
    }
  else
    {
      _use_t_max = 1;
      _t_max = ds->getAbscissa(ifun, (long) _index, vel_type)
        - time_tol;
    }
}

void VaEtaPicker::deletePick()
{
  long old_index = _index;
  float xval = _eta_plot->getDeleteXval();
  float yval = _eta_plot->getDeleteYval();

  _eta_plot->removeNmo();

  bracketTime(yval, VTIN, 1);

  if(_index == -1)
    {
      doBeep();
      return;
    }

  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();

  long ifun = ds->getActiveVelocityFunction();

  ds->removePick(ifun, (long) _index);

  _eta_plot->pickingCompleted();

  _index = old_index;

  _eta_plot->applyNmo();
}


void VaEtaPicker::noModButtonOnePress(int x, int y)
{
  _started = 0;
  float pick_tol  = _manager->utilities()->getPickingTolerance();
  _vhor = getPlot()->xWC(x);
  _vnmo = getPlot()->yWC(y);


  


  if(!_eta_picks->haveVector())
    {
      _eta_picks->insertStart(-1, _vhor, _vnmo);
    }


  
  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();
  long       ifun = ds->getActiveVelocityFunction();

  if (ifun == -1)
    {
      assert(!canEdit());
      doBeep();
      ignoreActions();
    }
  else
    {
      _yval = _eta_plot->getPickYval();

      bracketTime(_yval, VTIN, 0);

      if (canEdit())
        {
         
	/*
	 * Remember PickWatch is a PickBase.
	 * Do not want PickBase changing in middle
	 * of action.
	 */
          Bool watch_ok = PlotBase::watchOK();
          if (watch_ok)
            PlotBase::setWatchOK(False);

          if ((int) ds->getActivePick(ifun) != _index)
            ds->setActivePick(ifun, (long) _index);

          _eta_picks->replaceDrag(_index, _vhor, _vnmo);

          if (watch_ok)
            PlotBase::setWatchOK(True );

        }
      else
        {
          if (_insert)
            {
              doBeep();
            }
          else
            {
              Bool watch_ok = PlotBase::watchOK();
              if (watch_ok)
                PlotBase::setWatchOK(False);

              ds->setActivePick(ifun, (long) _index);

              if (watch_ok)
                PlotBase::setWatchOK(True );
            }

          ignoreActions();
        }
    }

	
}


void VaEtaPicker::noModButtonOneMotion(int x1, int x2,
				       int y1, int y2)
{
  _vhor = getPlot()->xWC(x2);
  _vnmo = getPlot()->yWC(y2);

  if (!canEdit())
    {
      assert(!_insert);
      return;
    }


  _eta_picks->replaceDrag(_index, _vhor, _vnmo);

}


void VaEtaPicker::noModButtonOneRelease(int /*x1*/, int x2,
					int /*y1*/, int y2)
{

  _eta_plot->removeNmo();

  _vhor = getPlot()->xWC(x2);
  _vnmo = getPlot()->yWC(y2);
  _eta = 0.5F * ((_vhor * _vhor) / (_vnmo * _vnmo) - 1.0F);

  printf("Eta = %f, vhoriz = %f, vnmo = %f, time = %f\n\n",_eta,_vhor,_vnmo,_yval);


  //Temporary
  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();


  long ifun = ds->getActiveVelocityFunction();
  int        numPicks = (int) ds->numPicks(ifun);

  if (_insert)
    {
      //_picks->broadcastInsDone(  _index, _eta, _yval);
      //For now if this is the 1st pick make sure we first insert
      //a pick at zero time with an eta of zero
      if(_index == 0 && numPicks == 0)
        {
          ds->insertPick(ifun, (long) _index, 0.0F, 0.0F, VTIN);
          ++_index;
        }
      ds->insertPick(ifun, (long) _index, _yval, _eta, VTIN);
    }
  else
    {
      //_picks->broadcastRepDone(  _index, _eta, _yval);
      ds->replacePick(ifun, (long) _index, _yval, _eta, VTIN);
    }

  //Temporarily we save the picks after every pick action
  _eta_plot->pickingCompleted(&_vnmo);

  _eta_plot->applyNmo();

}

void VaEtaPicker::noModButtonTwoPress(int x, int y)
{
  _eta_plot->removeNmo();


  //Temporary
  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();

  long ifun = ds->getActiveVelocityFunction();
  
  ds->removePick(ifun, (long) _index);

}

void VaEtaPicker::noModButtonTwoMotion(int /*x1*/, int /*x2*/,
				       int /*y1*/, int /*y2*/)
{
  /* do nothing */
}

void VaEtaPicker::noModButtonTwoRelease(int /*x1*/, int /*x2*/,
					int /*y1*/, int /*y2*/)
{
  //Temporary
  VfDataset *ds = _eta_plot->getTempVfManager()->activeDataset();
  //VfDataset *ds = _manager->activeDataset();
  long ifun = ds->getActiveVelocityFunction();
  _eta_picks->cleanupPick(0, _yval, _eta);

   //Temporarily we save the picks
  _eta_plot->pickingCompleted();

  _eta_plot->applyNmo();
}

VaEtaPicks::VaEtaPicks(VfManager *manager, class  VfHorizons *horizons,
                       VaPlot *plot, SeisPlot *sp, VaVectColors *colors,
                       SeisPlot *semblance_plot)
  : VaPicks(manager, horizons, plot, sp, colors, 1)

{
  _rbn_data = NULL;
  _eta_data = new VaEtaPickData(_manager, this);
  _white_rbn_data = NULL;
  _semblance_plot = semblance_plot;
}

VaEtaPicks::~VaEtaPicks()
{
  /*
   * _data is deleted in base class destructor.
	 */
  if (_picker)
    delete _picker;

  _white_rbn_vectors->remove(_white_rbn_vector);
  delete _white_rbn_data; 
	
  if(_eta_data)
    delete _eta_data;
}


void VaEtaPicks::deletePick(SeisPlot *sp)
{
  VaEtaPicker *picker = 
                new VaEtaPicker(sp, _manager, this, _editable_vectors, sp);
  picker->deletePick();
  delete picker;
}


VaEtaPicker* VaEtaPicks::getPicker()
{
  return (VaEtaPicker *)_picker;
}

void VaEtaPicks::init(SeisPlot *sp)
{
  float white_x, white_y;

  _picker = new VaEtaPicker(sp, _manager, this, _editable_vectors,
                            _semblance_plot);

  VaEtaPlot *eta_plot = (VaEtaPlot *)getPlot();
  white_x = eta_plot->getLargestHnmo();
  white_y = eta_plot->getLargestVnmo();
  if(_white_rbn_data == NULL)
    {
      _white_rbn_data = new VectData(1, &white_x, &white_y);

      _white_rbn_vector = _rbn_vectors->add(_white_rbn_data,
                             "white",
                              _colors->semblanceWidth (),
                              False,
                              _colors->semblanceLineStyle(),
                              _colors->semblanceActiveFuncMarker(),
                              _colors->semblanceActiveFuncMarkerSize(),
                              _colors->semblanceActiveFuncMarkerLineWidth());
    }
  _white_rbn_vector->makeVisible();
}


VaPicker *VaEtaPicks::newSeparateWindow(SeisPlot *sp)
{
  assert(0);
  return new VaEtaPicker(sp, _manager, this, _editable_vectors,_semblance_plot);
}




void VaEtaPicks::insertStart(int index, float x, float y)
{

        
  if (index == -1)
    {
      _rbn_data = new VectData(1, &x, &y);

      _rbn_vector = _rbn_vectors->add(_rbn_data,
                                 _colors->semblanceRbnColor(),
                                 _colors->semblanceWidth (),
                                 False,
                                 _colors->semblanceLineStyle(),
                                 _colors->semblanceActiveFuncMarker(),
                                 _colors->semblanceActiveFuncMarkerSize(),
                                 _colors->semblanceActiveFuncMarkerLineWidth());

      _rbn_vector->makeVisible();
      _eta_data->setNumPoints(1);
    }
  else if (index == 0)
    {
      float xs[3];
      float ys[3];

      xs[0] = _eta_data->getX(0);
      ys[0] = _eta_data->getY(0);
      xs[1] = x;
      ys[1] = y;
      _rbn_data = new VectData(2, xs, ys);

      _rbn_vector = _rbn_vectors->add(_rbn_data,
                                      _colors->semblanceRbnColor(),
                                      _colors->semblanceWidth   (), True);
      _eta_data->setNumPoints(1);
    }
  else
    {
      _rbn_data = (VectData *) NULL;
      _eta_data->setNumPoints(0);
    }
        
}




void VaEtaPicks::replaceDrag(int /*index*/, float x, float y)
{
  int numPts = _eta_data->getNumPts(0);

  if (numPts > 0)
    _rbn_data->replace(0, 1, &x, &y);
}

void VaEtaPicks::cleanupPick(int /*index*/, float /*x*/, float /*y*/)
{
  if (_eta_data->getNumPts(0))
    {
      _rbn_vectors->remove(_rbn_vector);
      delete _rbn_data; _rbn_data = NULL;
      _eta_data->setNumPoints(0);
    }
  //Since this is called everytime the user selects a new location
  //on the semblance plot to run the eta we will post the highest
  //semblance on the eta plot here
  VaEtaPlot *eta_plot = (VaEtaPlot *)getPlot();
  float white_x = eta_plot->getLargestHnmo();
  float white_y = eta_plot->getLargestVnmo();
  if(_white_rbn_data == NULL)
    {
      _white_rbn_data = new VectData(1, &white_x, &white_y);

      _white_rbn_vector = _rbn_vectors->add(_white_rbn_data,
                             "white",
                              _colors->semblanceWidth (),
                              False,
                              _colors->semblanceLineStyle(),
                              _colors->semblanceActiveFuncMarker(),
                              _colors->semblanceActiveFuncMarkerSize(),
                              _colors->semblanceActiveFuncMarkerLineWidth());
      _white_rbn_vector->makeVisible();    
    }
  else
    {
      _white_rbn_data->replace(0, 1, &white_x, &white_y);
    }
  
}


char *VaEtaPicks::getClassName()
{
  static char *retval = VA_ETA_PICKS;

  return retval;
}

Boolean VaEtaPicks::haveVector()
{
  Boolean stat;

  stat =  _rbn_data != NULL ? True : False;
  if(stat)
    _eta_data->setNumPoints(1);
  else
    _eta_data->setNumPoints(0);
  return stat;
}
