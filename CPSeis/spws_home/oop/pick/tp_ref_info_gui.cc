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

//---------------------- tp_ref_info_gui.cc ------------------------//
//---------------------- tp_ref_info_gui.cc ------------------------//
//---------------------- tp_ref_info_gui.cc ------------------------//

//         implementation file for the TpRefInfoGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_ref_info_gui.hh"
#include "pick/tp_ref_pair.hh"
#include "pick/tp_resources.hh"
#include "sl/slp_label.hh"
#include "cprim.h"
#include "stdio.h"
#include "string.h"


//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//

static char *info_update(void *data)
{
  TpRefPair *pair = (TpRefPair*)data;
  long  first = pair->getFirstProfile();
  long   last = pair->getLastProfile();
  long latest = pair->getLatestProfileUpdated();
  long  disp1 = pair->getFirstDisplayedProfile();
  long  disp2 = pair->getLastDisplayedProfile();
  static char info1[80], info2[80], info3[80], info4[80], info[300];
  sprintf(info1, "first shot profile: %d\n"     , first);
  sprintf(info2, "last shot profile: %d\n"      ,  last);
  sprintf(info3, "latest profile updated: %d\n" , latest);
  if(disp1 == disp2) sprintf(info4, "displayed profile: %d", disp1);
  else         sprintf(info4, "displayed profiles: %d - %d", disp1, disp2);
  strcpy(info, info1);
  strcat(info, info2);
  strcat(info, info3);
  strcat(info, info4);
  return info;
}


static long info_sense_update(void *data)
{
  TpRefPair *pair = (TpRefPair*)data;
  return pair->fileIsLoaded();
}




//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//

static String defres[]= {
    "*fontList: 8x13bold",
    NULL };


TpRefInfoGui::TpRefInfoGui(SLDelay *slparent, TpRefPair *pair)
       : SLSmartForm(slparent, "tp_ref_info_gui", NULL, FALSE)
{
  assert(slparent && pair);

  setDefaultResources(TpResources::getDisplay(), "info", defres);

  SLpLabel *info = new SLpLabel(this, "info", 0, "xx\nxx\nxx\nxx");

  info->setupCvarFun  (info_update      , pair);
  info->setupSenseFun (info_sense_update, pair);

  attach(info, this, this, this, this, 0, 0, 0, 0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpRefInfoGui::~TpRefInfoGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
