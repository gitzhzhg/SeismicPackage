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

//---------------------- tp_gen_info_gui.cc ------------------------//
//---------------------- tp_gen_info_gui.cc ------------------------//
//---------------------- tp_gen_info_gui.cc ------------------------//

//          implementation file for the TpGenInfoGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_gen_info_gui.hh"
#include "pick/tp_statfile_pair.hh"
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
  TpStatfilePair *pair = (TpStatfilePair*)data;
  float late1 = pair->getFirstLatestYbinUpdated();
  float late2 = pair->getLastLatestYbinUpdated();
  float disp1 = pair->getFirstDisplayedYbin();
  float disp2 = pair->getLastDisplayedYbin();
  static char info1[80], info2[80], info3[80], info4[80], info[300];
  if(late1 == late2)
     {
     sprintf(info1, "latest Y bin updated:\n");
     sprintf(info2, "%.2f\n", late1);
     }
  else
     {
     sprintf(info1, "latest Y bins updated:\n");
     sprintf(info2, "%.2f - %.2f\n", late1, late2);
     }
  if(disp1 == disp2)
     {
     sprintf(info3, "displayed Y bin:\n");
     sprintf(info4, "%.2f", disp1);
     }
  else
     {
     sprintf(info3, "displayed Y bins:\n");
     sprintf(info4, "%.2f - %.2f", disp1, disp2);
     }
  strcpy(info, info1);
  strcat(info, info2);
  strcat(info, info3);
  strcat(info, info4);
  return info;
}


static long info_sense_update(void *data)
{
  TpStatfilePair *pair = (TpStatfilePair*)data;
  return pair->fileIsLoaded();
}




//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//

static String defres[]= {
    "*fontList: 8x13bold",
    NULL };


TpGenInfoGui::TpGenInfoGui(SLDelay *slparent, TpStatfilePair *pair)
       : SLSmartForm(slparent, "tp_gen_info_gui", NULL, FALSE)
{
  assert(slparent && pair);

  setDefaultResources(TpResources::getDisplay(), "tpg_info", defres);

  SLpLabel *info = new SLpLabel(this, "tpg_info", 0, "xx\nxx\nxx\nxx");

  info->setupCvarFun  (info_update      , pair);
  info->setupSenseFun (info_sense_update, pair);

  attach(info, this, this, this, this, 0, 0, 0, 0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpGenInfoGui::~TpGenInfoGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
