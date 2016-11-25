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



//---------------------- vf_horizons.cc -------------------------//
//---------------------- vf_horizons.cc -------------------------//
//---------------------- vf_horizons.cc -------------------------//

//        implementation file for the VfHorizons class
//                   not derived from any class
//                          subdirectory vf


#include "vf/vf_horizons.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_horizonio.hh"
#include "oprim/grid_transform.hh"
#include "oprim/horizon_array.hh"
#include "oprim/horizon.hh"
#include "oprim/generic_cards.hh"
#include "oprim/generic_decode.hh"
#include "oprim/backup_base.hh"
#include "oprim/rmod_layers.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>


#define  HORIZON_FILE     "horizon file"
#define  RMOD_FILE        "RMOD file"
#define  MODSPEC_FILE     "MODSPEC file"
#define  NBUF  222


//---------------------- constructor ---------------------------//
//---------------------- constructor ---------------------------//
//---------------------- constructor ---------------------------//


VfHorizons::VfHorizons(VfInformer *informer)
           :
                  _informer   (informer),
                  _transform  (NULL),
                  _array      (NULL)
{
  assert(_informer);
  _transform = new GridTransform();
  _array     = new HorizonArray ();
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//


VfHorizons::~VfHorizons()
{
  delete _transform;
  delete _array;
}



                 //------ public functions ------//
                 //------ public functions ------//
                 //------ public functions ------//
                 //------ public functions ------//
                 //------ public functions ------//
                 //------ public functions ------//
                 //------ public functions ------//
                 //------ public functions ------//
                 //------ public functions ------//
                 //------ public functions ------//
                 //------ public functions ------//



//----------------------- get values --------------------------//
//----------------------- get values --------------------------//
//----------------------- get values --------------------------//

      // public.

long VfHorizons::numHorizons()  const
{
  return _array->numHorizons();
}


long VfHorizons::getActiveHorizonIndex()  const
{
  return _array->getActiveHorizonIndex();
}



//------------------------ set values --------------------------------//
//------------------------ set values --------------------------------//
//------------------------ set values --------------------------------//

     // public.

void VfHorizons::setActiveHorizonIndex(long ihorizon)
{
  informer()->preNewActiveHorizon();
  _array->setActiveHorizonIndex(ihorizon);
  informer()->showMessage("new active horizon");
  informer()->postNewActiveHorizon();
}


void VfHorizons::selectAllHorizons(int selected)
{
  informer()->preNewSelectedHorizons();
  _array->selectAllHorizons(selected);
  informer()->postNewSelectedHorizons();
}



//--------------------- delete active horizon ----------------------------//
//--------------------- delete active horizon ----------------------------//
//--------------------- delete active horizon ----------------------------//

   // public.
   // called at user prerogative (when active horizon is no longer needed).

void VfHorizons::deleteActiveHorizon()
{
  char msg[NBUF];
  if(numHorizons() == 0)
      {
      strcpy(msg, "there are no horizons to delete");
      informer()->ringBell();
      informer()->showMessage(msg);
      return;
      }
  long ihorizon = getActiveHorizonIndex();
  informer()->preRemoveInsertHorizons(ihorizon, 1, 0);
  _array->deleteHorizon(ihorizon);
  informer()->showMessage("active horizon successfully deleted");
  informer()->postRemoveInsertHorizons(ihorizon, 1, 0);
}



//------------------------ get segment-specific values --------------------//
//------------------------ get segment-specific values --------------------//
//------------------------ get segment-specific values --------------------//

     // public.

long  VfHorizons::numSegments            (long ihorizon)            const
{
  return _array->numSegments(ihorizon);
}


long  VfHorizons::startingIndexOfSegment (long ihorizon, long iseg)   const
{
  return _array->startingIndexOfSegment(ihorizon, iseg);
}


long  VfHorizons::numPicksInSegment      (long ihorizon, long iseg)   const
{
  return _array->numPicksInSegment(ihorizon, iseg);
}


long  VfHorizons::getSegmentNumber      (long ihorizon, long ipick)   const
{
  return _array->getSegmentNumber(ihorizon, ipick);
}



float VfHorizons::getXlocInSegment
                            (long ihorizon, long iseg, long ipick)  const
{
  return _array->getXlocInSegment(ihorizon, iseg, ipick);
}


float VfHorizons::getYlocInSegment
                            (long ihorizon, long iseg, long ipick)  const
{
  return _array->getYlocInSegment(ihorizon, iseg, ipick);
}


float VfHorizons::getTimeInSegment
                            (long ihorizon, long iseg, long ipick)  const
{
  return _array->getTimeInSegment(ihorizon, iseg, ipick);
}


float VfHorizons::getShotpointInSegment
                             (long ihorizon, long iseg, long ipick)  const
{
  return _array->getShotpointInSegment(ihorizon, iseg, ipick);
}


float VfHorizons::getLineNumberInSegment
                            (long ihorizon, long iseg, long ipick)  const
{
  return _array->getLineNumberInSegment(ihorizon, iseg, ipick);
}



                   //------- pass thru -------//
                   //------- pass thru -------//
                   //------- pass thru -------//
                   //------- pass thru -------//
                   //------- pass thru -------//
                   //------- pass thru -------//
                   //------- pass thru -------//
                   //------- pass thru -------//
                   //------- pass thru -------//
                   //------- pass thru -------//



//------------------------ get values ----------------------------------//
//------------------------ get values ----------------------------------//
//------------------------ get values ----------------------------------//

      // public.

long  VfHorizons::numPicks      (long ihorizon)       const
{
  return _array->numPicks(ihorizon);
}


long  VfHorizons::getActivePick (long ihorizon)       const
{
  return _array->getActivePick(ihorizon);
}


int   VfHorizons::isSelected    (long ihorizon)       const
{
  return _array->isSelected(ihorizon);
}


const char *VfHorizons::getName   (long ihorizon)       const
{
  return _array->getName(ihorizon);
}


const char *VfHorizons::getColor  (long ihorizon)       const
{
  return _array->getColor(ihorizon);
}


const char *VfHorizons::getPicktype  (long ihorizon)       const
{
  return _array->getPicktype(ihorizon);
}


const char *VfHorizons::getUnits  (long ihorizon)       const
{
  return _array->getUnits(ihorizon);
}


int VfHorizons::readError  (long ihorizon)       const
{
  return _array->readError(ihorizon);
}



float VfHorizons::getXloc       (long ihorizon, long ipick)  const
{
  return _array->getXloc(ihorizon, ipick);
}


float VfHorizons::getYloc       (long ihorizon, long ipick)  const
{
  return _array->getYloc(ihorizon, ipick);
}


float VfHorizons::getTime       (long ihorizon, long ipick)  const
{
  return _array->getTime(ihorizon, ipick);
}


float VfHorizons::getShotpoint  (long ihorizon, long ipick)  const
{
  return _array->getShotpoint(ihorizon, ipick);
}


float VfHorizons::getLineNumber (long ihorizon, long ipick)  const
{
  return _array->getLineNumber(ihorizon, ipick);
}


float VfHorizons::minimumXloc(long ihorizon)  const
{
  return _array->minimumXloc(ihorizon);
}


float VfHorizons::minimumYloc(long ihorizon)  const
{
  return _array->minimumYloc(ihorizon);
}


float VfHorizons::minimumTime(long ihorizon)  const
{
  return _array->minimumTime(ihorizon);
}


float VfHorizons::minimumShotpoint(long ihorizon)  const
{
  return _array->minimumShotpoint(ihorizon);
}


float VfHorizons::minimumLineNumber(long ihorizon)  const
{
  return _array->minimumLineNumber(ihorizon);
}


float VfHorizons::maximumXloc(long ihorizon)  const
{
  return _array->maximumXloc(ihorizon);
}


float VfHorizons::maximumYloc(long ihorizon)  const
{
  return _array->maximumYloc(ihorizon);
}


float VfHorizons::maximumTime(long ihorizon)  const
{
  return _array->maximumTime(ihorizon);
}


float VfHorizons::maximumShotpoint(long ihorizon)  const
{
  return _array->maximumShotpoint(ihorizon);
}


float VfHorizons::maximumLineNumber(long ihorizon)  const
{
  return _array->maximumLineNumber(ihorizon);
}



float VfHorizons::getXgrid      (long ihorizon, long ipick)  const
{
  float xloc = _array->getXloc(ihorizon, ipick);
  float yloc = _array->getYloc(ihorizon, ipick);
  return (float)_transform->getXgridCoord(xloc, yloc);
}


float VfHorizons::getYgrid      (long ihorizon, long ipick)  const
{
  float xloc = _array->getXloc(ihorizon, ipick);
  float yloc = _array->getYloc(ihorizon, ipick);
  return (float)_transform->getYgridCoord(xloc, yloc);
}


//------------------------ set values ----------------------------------//
//------------------------ set values ----------------------------------//
//------------------------ set values ----------------------------------//

      // public.

void VfHorizons::setActivePick (long ihorizon, long active)
{
  informer()->preNewActiveHorizonPick(ihorizon);
  _array->setActivePick(ihorizon, active);
  informer()->showMessage("new active horizon pick");
  informer()->postNewActiveHorizonPick(ihorizon);
}


void VfHorizons::setSelected   (long ihorizon, int selected)
{
  informer()->preNewSelectedHorizons();
  _array->setSelected(ihorizon, selected);
  informer()->postNewSelectedHorizons();
}


void VfHorizons::setColor  (long ihorizon, const char *color)
{
  informer()->preNewHorizonColor(ihorizon);
  _array->setColor(ihorizon, color);
  informer()->postNewHorizonColor(ihorizon);
}



           //------ overriding virtual functions ------//
           //------ overriding virtual functions ------//
           //------ overriding virtual functions ------//
           //------ overriding virtual functions ------//
           //------ overriding virtual functions ------//
           //------ overriding virtual functions ------//
           //------ overriding virtual functions ------//
           //------ overriding virtual functions ------//
           //------ overriding virtual functions ------//
           //------ overriding virtual functions ------//



//-------------------- do validate input -------------------------//
//-------------------- do validate input -------------------------//
//-------------------- do validate input -------------------------//


int VfHorizons::doValidateInput(VfHorizonio *horizonio,
                                const char *filename,
                                char *filetype, char *info)
{
  strcpy(filetype, HORIZON_FILE);
  Horizon *hor = new Horizon("horizon");
  int error = hor->validateFile(horizonio, filename, info);
  delete hor;
  if(!error) return FALSE;

  char info2[NBUF];
  strcpy(info2, "");

  RmodLayers *rmod = new RmodLayers();
  error = rmod->validateFile(filename, info2);
  delete rmod;
  if(!error)
      {
      horizonio->clear();
      strcpy(filetype, RMOD_FILE);
      strcpy(info, info2);
      return FALSE;
      }

/************* TO DO:
  error = validateModspecFile(filename,info2);
  if(!error)
      {
      horizonio->clear();
      strcpy(filetype, MODSPEC_FILE);
      strcpy(info, info2);
      return FALSE;
      }
  return FALSE;
************/

  return FALSE;
}



//--------------------- do prepare import -----------------------------//
//--------------------- do prepare import -----------------------------//
//--------------------- do prepare import -----------------------------//


int VfHorizons::doPrepareImport(VfHorizonio *horizonio,
                                const char * /*filename*/,
                                const char *filetype, char *msg)
{
  if(strcmp(filetype, RMOD_FILE) == 0)
      {
      strcpy(msg, "importing RMOD file");
      return FALSE;
      }
  if(strcmp(filetype, MODSPEC_FILE) == 0)
      {
      strcpy(msg, "importing MODSPEC file");
      return FALSE;
      }
  return horizonio->verifyParameters(msg);
}


//--------------------- do import -----------------------------//
//--------------------- do import -----------------------------//
//--------------------- do import -----------------------------//


int VfHorizons::doImport(VfHorizonio *horizonio,
                         const char *filename,
                         const char *filetype, char *msg)
{
  if(strcmp(filetype, RMOD_FILE) == 0)
      {
      informer()->beforeChanges();
      int error = importRmodLayers(filename, msg);
      informer()->showMessage(msg);
      informer()->afterChanges();
      return error;
      }
/********* TO DO:
  if(strcmp(filetype, MODSPEC_FILE) == 0)
      {
      informer()->beforeChanges();
      int error = importModspecFile(filename, msg);
      informer()->showMessage(msg);
      informer()->afterChanges();
      return error;
      }
**********/

  long ihorizon = _array->numHorizons();
  informer()->preRemoveInsertHorizons(ihorizon, 0, 1);

  long ihorizon2 = _array->createHorizon();
  assert(ihorizon2 == ihorizon);
  _array->setName (ihorizon, BackupBase::getProperFilename(filename));
  _array->setColor(ihorizon, "red");

  int error = _array->importHorizonFile(ihorizon, horizonio, filename, msg);

  informer()->showMessage(msg);
  informer()->postRemoveInsertHorizons(ihorizon, 0, 1);
  return error;
}



//------------------------- import rmod layers --------------------------//
//------------------------- import rmod layers --------------------------//
//------------------------- import rmod layers --------------------------//

     // private.

int VfHorizons::importRmodLayers(const char *filename, char* msg)
{
  RmodLayers *rmod = new RmodLayers();
  int error = rmod->readFile(filename, msg);
  int nhor = rmod->numHorizons();
  int nval = rmod->numValues();
  if(nhor == 0 || nval == 0)
      {
      delete rmod;
      return error;
      }
  int  prev_hor = -1;
  int  prev_seg = -1;
  int  ihor     = -1;
  long index    = -1;
  for(int ival = 0; ival < nval; ival++)
      {
      float xval = rmod->getXval        (ival);
      float yval = rmod->getYval        (ival);
      float zval = rmod->getZval        (ival);
      int   seg  = rmod->getSegmentIdent(ival);
      int   hor  = rmod->getHorizonIdent(ival);
      if(ival > 0 && hor != prev_hor)
           {
           finishNewHorizon(index);
           }
      if(ival == 0 || hor != prev_hor)
           {
           ihor = rmod->getHorizonIndex(ival);
           index = startNewHorizon(rmod->horizonName(ihor),
                                   rmod->horizonColor(ihor));
           }
      int new_segment = (hor == prev_hor && seg != prev_seg);
      supplyNewPick(index, xval, yval, zval, new_segment);
      prev_hor = hor;
      prev_seg = seg;
      }
  finishNewHorizon(index);
  delete rmod;
  return error;
}



//----------------- start and finish new horizon -----------------------//
//----------------- start and finish new horizon -----------------------//
//----------------- start and finish new horizon -----------------------//

     // private.

long VfHorizons::startNewHorizon(const char *name, const char *color)
{
  assert(name && color);
  long ihorizon = _array->numHorizons();
  informer()->preRemoveInsertHorizons(ihorizon, 0, 1);

  long ihorizon2 = _array->createHorizon();
  assert(ihorizon2 == ihorizon);
  _array->setName           (ihorizon, name);
  _array->setColor          (ihorizon, color);
  _array->setNumPicks       (ihorizon, 0);
  return ihorizon;
}



void VfHorizons::supplyNewPick
        (long ihorizon, float xloc, float yloc, float zloc, int new_segment)
{
  assert(ihorizon >= 0);
  _array->appendNilRow(ihorizon);
  _array->setLastValue(ihorizon, Horizon::XLOC, xloc);
  _array->setLastValue(ihorizon, Horizon::YLOC, yloc);
  _array->setLastValue(ihorizon, Horizon::TIME, zloc);
  _array->setLastValue(ihorizon, Horizon::SHOT, xloc);  // new 8/25/99
  _array->setLastValue(ihorizon, Horizon::LINE, yloc);  // new 8/25/99
  if(new_segment)
      {
      long ipick = _array->numPicks(ihorizon) - 1;
      _array->addSegment(ihorizon, ipick);
      }
}



void VfHorizons::finishNewHorizon(long ihorizon)
{
  assert(ihorizon >= 0);
  informer()->postRemoveInsertHorizons(ihorizon, 0, 1);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

