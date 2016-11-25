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

//---------------------- statgui_reference.cc ------------------------//
//---------------------- statgui_reference.cc ------------------------//
//---------------------- statgui_reference.cc ------------------------//

//          implementation file for the StatguiReference class
//                derived from the SLSmartForm class
//                        subdirectory statgui


#include "statgui/statgui_reference.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/slp_text.hh"
#include "sl/slp_label.hh"
#include "sl/sl2_arrows.hh"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//------------------------ static functions -------------------------//
//------------------------ static functions -------------------------//
//------------------------ static functions -------------------------//


static void arrows_num1_trap(void *data, long newvar)
{
  StaticManager *manager = (StaticManager*)data;
  manager->setReferenceDatasetIndex((int)newvar - 1);
}


static long arrows_num1_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return manager->getReferenceDatasetIndex() + 1;
}


static long arrows_num2_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return manager->numDatasets();
}


static long arrows_sense_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  return (manager->numDatasets() > 1);
}


static char *info_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  static char buffer[60];
  int nx = manager->referenceDataset()->getNx();
  int ny = manager->referenceDataset()->getNy();
  int nn = manager->referenceDataset()->numNilValues();
/*
  int ns = manager->referenceDataset()->numSelections();
  sprintf(buffer, "#x %d  #y %d  #nil %d  #sel %d", nx, ny, nn, ns);
  sprintf(buffer, "%dx %dy %dnil %dsel", nx, ny, nn, ns);
*/
  sprintf(buffer, "nx %d  ny %d  %d nils", nx, ny, nn);
  return buffer;
}



//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//


StatguiReference::StatguiReference  (SLDelay       *slparent,
                                     StaticManager *manager,
                                     int            wide)
       : SLSmartForm(slparent, "statgui_reference", NULL, FALSE, FALSE),
                 _manager     (manager)
{
  assert(_manager);
  constructorHelper(wide);
  if(slparent->made()) make();
}



StatguiReference::StatguiReference  (Widget         wparent,
                                     StaticManager *manager,
                                     int            wide)
       : SLSmartForm(wparent, "statgui_reference", NULL, FALSE, FALSE),
                 _manager     (manager)
{
  assert(_manager);
  constructorHelper(wide);
  make();
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


StatguiReference::~StatguiReference()
{
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        ".borderWidth:         2",
        "*background:          wheat",
            NULL };



//------------------- constructor helper ----------------------//
//------------------- constructor helper ----------------------//
//------------------- constructor helper ----------------------//

        // private.

void StatguiReference::constructorHelper(int wide)
{
  setFallbackResources(defres);

  SL2Arrows  *arrows  = new SL2Arrows(this, "arrows", "reference dataset", 2);
  SLpLabel   *info    = new SLpLabel (this, "info");

   arrows ->registerNum1Trap   (arrows_num1_trap   , manager());
   arrows ->registerNum1Update (arrows_num1_update , manager());
   arrows ->registerNum2Update (arrows_num2_update , manager());
   arrows ->registerSenseUpdate(arrows_sense_update, manager());
   info   ->setupCvarFun       (info_update        , manager());

///                     left     right     top       bottom
  if(wide)
      {
      attach( arrows ,  this   ,  NULL   , this    ,  this, 6, 0, 6, 6);
      attach( info   ,  arrows ,  this   , this    ,  this, 6, 6, 6, 6);
      }
  else
      {
      attach( arrows ,  this   ,  NULL   , this    ,  NULL, 6, 6, 6, 0);
      attach( info   ,  this   ,  this   , arrows  ,  this, 6, 6, 0, 0);
      }
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

