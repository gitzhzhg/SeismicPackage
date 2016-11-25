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

//----------------------- horizon_array.cc -------------------------//
//----------------------- horizon_array.cc -------------------------//
//----------------------- horizon_array.cc -------------------------//

//         implementation file for the HorizonArray class
//                     not derived from any class
//                         subdirectory oprim


#include "oprim/horizon_array.hh"
#include "oprim/horizon.hh"
#include "oprim/generic_cards.hh"
#include "oprim/void_array.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


HorizonArray::HorizonArray()
               :
                  _array    (NULL)
{
  _array = new VoidArray();
}



//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//


HorizonArray::~HorizonArray()
{
  deleteAllHorizons();
  delete _array;
}



//----------------------- private functions --------------------------//
//----------------------- private functions --------------------------//
//----------------------- private functions --------------------------//

     // private.

Horizon *HorizonArray::horizon(long ihorizon)  const
{
  return (Horizon*)_array->element(ihorizon);
}



//------------------------ get values ------------------------------------//
//------------------------ get values ------------------------------------//
//------------------------ get values ------------------------------------//

   // public.

long HorizonArray::numHorizons()  const
{
  return _array->numElements();
}


long HorizonArray::getActiveHorizonIndex()  const
{
  return _array->getActiveIndex();
}


long HorizonArray::findMatchingHorizonIndex(const char *name)  const
{
  long num = numHorizons();
  for(long ihorizon = 0; ihorizon < num; ihorizon++)
      {
      const char *name2 = horizon(ihorizon)->getName();
      if(strcmp(name2, name) == 0) return ihorizon;
      }
  return -1;
}



//-------------------- set values ----------------------------------//
//-------------------- set values ----------------------------------//
//-------------------- set values ----------------------------------//

       // public.

void HorizonArray::setActiveHorizonIndex(long ihorizon)
{
  _array->setActiveIndex(ihorizon);
}



void HorizonArray::selectAllHorizons(int selected)
{
  long num = numHorizons();
  for(long ihorizon = 0; ihorizon < num; ihorizon++)
      {
      horizon(ihorizon)->setSelected(selected);
      }
}



void HorizonArray::deleteAllHorizons()
{
  long num = numHorizons();
  for(long ihorizon = 0; ihorizon < num; ihorizon++)
      {
      delete horizon(ihorizon);
      }
  _array->clearElements();
}



long HorizonArray::createHorizon()
{
  Horizon *element = new Horizon();
  return _array->appendElement(element);
}



void HorizonArray::deleteActiveHorizon()
{
  long ihorizon = getActiveHorizonIndex();
  if(ihorizon) delete horizon(ihorizon);
  _array->removeActiveElement();
}



void HorizonArray::deleteHorizon(long ihorizon)
{
  delete horizon(ihorizon);
  _array->removeElement(ihorizon);
}



//------------------------ get segment-specific values --------------------//
//------------------------ get segment-specific values --------------------//
//------------------------ get segment-specific values --------------------//

     // public.

long  HorizonArray::numSegments            (long ihorizon)            const
{
  return horizon(ihorizon)->numSegments();
}


long  HorizonArray::startingIndexOfSegment (long ihorizon, long iseg)   const
{
  return horizon(ihorizon)->startingIndexOfSegment(iseg);
}


long  HorizonArray::numPicksInSegment      (long ihorizon, long iseg)   const
{
  return horizon(ihorizon)->numPicksInSegment(iseg);
}


long  HorizonArray::getSegmentNumber      (long ihorizon, long ipick)   const
{
  return horizon(ihorizon)->getSegmentNumber(ipick);
}



float HorizonArray::getXlocInSegment
                            (long ihorizon, long iseg, long ipick)  const
{
  return horizon(ihorizon)->getXlocInSegment(iseg, ipick);
}


float HorizonArray::getYlocInSegment
                            (long ihorizon, long iseg, long ipick)  const
{
  return horizon(ihorizon)->getYlocInSegment(iseg, ipick);
}


float HorizonArray::getTimeInSegment
                            (long ihorizon, long iseg, long ipick)  const
{
  return horizon(ihorizon)->getTimeInSegment(iseg, ipick);
}


float HorizonArray::getShotpointInSegment
                             (long ihorizon, long iseg, long ipick)  const
{
  return horizon(ihorizon)->getShotpointInSegment(iseg, ipick);
}


float HorizonArray::getLineNumberInSegment
                            (long ihorizon, long iseg, long ipick)  const
{
  return horizon(ihorizon)->getLineNumberInSegment(iseg, ipick);
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



//--------------------- get values -------------------------------------//
//--------------------- get values -------------------------------------//
//--------------------- get values -------------------------------------//

      // public.

long  HorizonArray::numPicks      (long ihorizon)       const
{
  return horizon(ihorizon)->numPicks();
}


long  HorizonArray::getActivePick (long ihorizon)       const
{
  return horizon(ihorizon)->getActivePick();
}


int   HorizonArray::isSelected    (long ihorizon)       const
{
  return horizon(ihorizon)->isSelected();
}


const char *HorizonArray::getName   (long ihorizon)       const
{
  return horizon(ihorizon)->getName();
}


const char *HorizonArray::getColor  (long ihorizon)       const
{
  return horizon(ihorizon)->getColor();
}


const char *HorizonArray::getPicktype  (long ihorizon)       const
{
  return horizon(ihorizon)->getPicktype();
}


const char *HorizonArray::getUnits  (long ihorizon)       const
{
  return horizon(ihorizon)->getUnits();
}


int HorizonArray::readError  (long ihorizon)       const
{
  return horizon(ihorizon)->readError();
}



float HorizonArray::getXloc       (long ihorizon, long ipick)  const
{
  return horizon(ihorizon)->getXloc(ipick);
}


float HorizonArray::getYloc       (long ihorizon, long ipick)  const
{
  return horizon(ihorizon)->getYloc(ipick);
}


float HorizonArray::getTime       (long ihorizon, long ipick)  const
{
  return horizon(ihorizon)->getTime(ipick);
}


float HorizonArray::getShotpoint  (long ihorizon, long ipick)  const
{
  return horizon(ihorizon)->getShotpoint(ipick);
}


float HorizonArray::getLineNumber (long ihorizon, long ipick)  const
{
  return horizon(ihorizon)->getLineNumber(ipick);
}


float HorizonArray::minimumXloc(long ihorizon)  const
{
  return horizon(ihorizon)->minimumXloc();
}


float HorizonArray::minimumYloc(long ihorizon)  const
{
  return horizon(ihorizon)->minimumYloc();
}


float HorizonArray::minimumTime(long ihorizon)  const
{
  return horizon(ihorizon)->minimumTime();
}


float HorizonArray::minimumShotpoint(long ihorizon)  const
{
  return horizon(ihorizon)->minimumShotpoint();
}


float HorizonArray::minimumLineNumber(long ihorizon)  const
{
  return horizon(ihorizon)->minimumLineNumber();
}


float HorizonArray::maximumXloc(long ihorizon)  const
{
  return horizon(ihorizon)->maximumXloc();
}


float HorizonArray::maximumYloc(long ihorizon)  const
{
  return horizon(ihorizon)->maximumYloc();
}


float HorizonArray::maximumTime(long ihorizon)  const
{
  return horizon(ihorizon)->maximumTime();
}


float HorizonArray::maximumShotpoint(long ihorizon)  const
{
  return horizon(ihorizon)->maximumShotpoint();
}


float HorizonArray::maximumLineNumber(long ihorizon)  const
{
  return horizon(ihorizon)->maximumLineNumber();
}





//-------------------------- set values ------------------------------//
//-------------------------- set values ------------------------------//
//-------------------------- set values ------------------------------//

     // public.

void HorizonArray::setNumPicks  (long ihorizon, long npicks)
{
  horizon(ihorizon)->setNumPicks(npicks);
}


void HorizonArray::setActivePick (long ihorizon, long active)
{
  horizon(ihorizon)->setActivePick(active);
}


void HorizonArray::setSelected   (long ihorizon, int selected)
{
  horizon(ihorizon)->setSelected(selected);
}


void HorizonArray::setName   (long ihorizon, const char *name)
{
  horizon(ihorizon)->setName(name);
}


void HorizonArray::setColor  (long ihorizon, const char *color)
{
  horizon(ihorizon)->setColor(color);
}


void HorizonArray::appendNilRow  (long ihorizon)
{
  horizon(ihorizon)->appendNilRow();
}


void HorizonArray::setLastValue  (long ihorizon, long icol, float value)
{
  horizon(ihorizon)->setLastValue((int)icol, value);
}


void HorizonArray::addSegment  (long ihorizon, long ipick)
{
  horizon(ihorizon)->addSegment(ipick);
}



//------------------ read and write disk file ------------------------//
//------------------ read and write disk file ------------------------//
//------------------ read and write disk file ------------------------//

     // public.

int HorizonArray::importHorizonFile
                          (long ihorizon, FloatioWrapper *floatio,
                           const char *filename, char *msg)
{
  return horizon(ihorizon)->importHorizonFile (floatio, filename, msg);
}


int HorizonArray::exportHorizonFile
                          (long ihorizon, FloatioWrapper *floatio,
                           const char *filename, char *msg)
{
  return horizon(ihorizon)->exportHorizonFile (floatio, filename, msg);
}


int HorizonArray::readHorizonFile
                          (long ihorizon, const char *filename, char *msg)
{
  return horizon(ihorizon)->readHorizonFile(filename, msg);
}


int HorizonArray::saveHorizonFile
                          (long ihorizon, const char *filename, char *msg)
{
  return horizon(ihorizon)->saveHorizonFile(filename, msg);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

