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

//---------------------- tp_mute_info_gui.cc ------------------------//
//---------------------- tp_mute_info_gui.cc ------------------------//
//---------------------- tp_mute_info_gui.cc ------------------------//

//          implementation file for the TpMuteInfoGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_mute_info_gui.hh"
#include "cprim.h"
#include "pick/tp_mute_pair.hh"
#include "pick/tp_resources.hh"
#include "sl/slp_label.hh"
#include "stdio.h"
#include "string.h"


//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//

static char *info1_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  float xmin    = pair->getXmin();
  float ymin    = pair->getYmin();
  float zmin    = pair->getZmin();
  float xmax    = pair->getXmax();
  float ymax    = pair->getYmax();
  float zmax    = pair->getZmax();
  char info1[80], info2[80], info3[80];
  static char info[300];
  sprintf(info1, "min/max offsets: %.0f %.0f\n", xmin, xmax);
  sprintf(info2, "min/max inline bins: %.0f %.0f\n", ymin, ymax);
  sprintf(info3, "min/max xline bins: %.0f %.0f\n", zmin, zmax);
  strcpy(info, info1);
  strcat(info, info2);
  strcat(info, info3);
  return info;
}



static char *info2_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  float ylatest = pair->getLatestYbinUpdated();
  float zlatest = pair->getLatestZbinUpdated();
  char info1[80], info2[80];
  static char info[200];
  sprintf(info1, "latest inline bin updated: %.0f\n", ylatest);
  sprintf(info2, "latest xline bin updated: %.0f\n", zlatest);
  strcpy(info, info1);
  strcat(info, info2);
  return info;
}



static char *info3_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  float ydisp1  = pair->getFirstDisplayedYbin();
  float ydisp2  = pair->getLastDisplayedYbin();
  float zdisp1  = pair->getFirstDisplayedZbin();
  float zdisp2  = pair->getLastDisplayedZbin();
  char info1[80], info2[80];
  static char info[200];
  if(ydisp1 == ydisp2)
     {
     sprintf(info1, "displayed inline bin: %.0f\n", ydisp1);
     }
  else
     {
     sprintf(info1, "displayed inline bins: %.0f - %.0f\n", ydisp1, ydisp2);
     }
  if(zdisp1 == zdisp2)
     {
     sprintf(info2, "displayed xline bin: %.0f\n", zdisp1);
     }
  else
     {
     sprintf(info2, "displayed xline bins: %.0f - %.0f\n", zdisp1, zdisp2);
     }
  strcpy(info, info1);
  strcat(info, info2);
  return info;
}



static long info_sense_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  return pair->fileIsLoaded();
}




//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//

static String defres[]= {
    "*fontList: 8x13bold",
    NULL };


TpMuteInfoGui::TpMuteInfoGui(SLDelay *slparent, TpMutePair *pair)
       : SLSmartForm(slparent, "tp_mute_info_gui", NULL, FALSE)
{
  assert(slparent && pair);

  setDefaultResources(TpResources::getDisplay(), "tpm_info1", defres);
  setDefaultResources(TpResources::getDisplay(), "tpm_info2", defres);
  setDefaultResources(TpResources::getDisplay(), "tpm_info3", defres);

  SLpLabel *info1 = new SLpLabel(this, "tpm_info1", 0, "xx\nxx\nxx");
  SLpLabel *info2 = new SLpLabel(this, "tpm_info2", 0, "xx\nxx");
  SLpLabel *info3 = new SLpLabel(this, "tpm_info3", 0, "xx\nxx");

  info1->setupCvarFun  (info1_update     , pair);
  info2->setupCvarFun  (info2_update     , pair);
  info3->setupCvarFun  (info3_update     , pair);

  info1->setupSenseFun (info_sense_update, pair);
  info2->setupSenseFun (info_sense_update, pair);
  info3->setupSenseFun (info_sense_update, pair);

  attach(info1, this, this,  this,  NULL, 0, 0, 10,  0);
  attach(info2, this, this, info1, info3, 0, 0,  0,  0);
  attach(info3, this, this,  NULL,  this, 0, 0,  0, 10);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpMuteInfoGui::~TpMuteInfoGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
