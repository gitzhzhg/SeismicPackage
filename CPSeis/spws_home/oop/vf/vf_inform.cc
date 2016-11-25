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

//-------------------------- vf_inform.cc ------------------------//
//-------------------------- vf_inform.cc ------------------------//
//-------------------------- vf_inform.cc ------------------------//

//           implementation file for the VfInform class
//                   not derived from any class
//                         subdirectory vf


#include "vf/vf_inform.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_informer.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//

VfInform::VfInform(VfManager *manager, int printit)
       :
             _manager        (manager),
             _printit        (printit),
             _disabled       (FALSE)
{
  assert(_manager);
  _manager->informer()->addInformObject(this);
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


VfInform::~VfInform()
{
  if(_manager) _manager->informer()->removeInformObject(this);
}



//------------------- private helper functions ------------------------//
//------------------- private helper functions ------------------------//
//------------------- private helper functions ------------------------//

#define RETURN  if(!_printit) return;
#define INDEX   dataset->indexOfThisDataset() + 1
#define TYPE    _manager->utilities()->typeSymbol(type)


static char prefix [111];    // set by     front       and used by dispose.
static char suffix [111];    // set by style functions and used by dispose.


void VfInform::dispose()
{
  printf("%s  %s\n", prefix, suffix);
}



void VfInform::front(const char *string, VfDataset *dataset)
{
  static char *format_pre   = "%s %s";
  static char *format_post  = "%s%s";
  static char *format_other = "%s    %s";
  char which[8];
  if(dataset) sprintf(which, "%2d ", INDEX);
  else        sprintf(which,  "   ");
  char *format;
  if     (strncmp(string, "pre" , 3) == 0) format = format_pre;
  else if(strncmp(string, "post", 4) == 0) format = format_post;
  else                                     format = format_other;
  sprintf(prefix, format, which, string);
}



void VfInform::style0 (const char *string, VfDataset *dataset)
{
  RETURN
  front(string, dataset);
  strcpy(suffix, "");
  dispose();
}


void VfInform::style1 (const char *string, VfDataset *dataset,
                         long ihorizon)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "%d", ihorizon);
  dispose();
}


void VfInform::style1 (const char *string, VfDataset *dataset,
                         const char *msg)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "%s", msg);
  dispose();
}


void VfInform::style2 (const char *string, VfDataset *dataset,
                         long ifun, long nchng)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "ifun=%d  nchng=%d", ifun, nchng);
  dispose();
}


void VfInform::style3 (const char *string, VfDataset *dataset,
                         long index, long nrem, long nins)
{
  RETURN
  front(string, dataset);
  char buffer[8];
  if(dataset) strcpy(buffer, "nfun");
  else        strcpy(buffer, "index");
  sprintf(suffix, "%s=%d  nrem=%d  nins=%d", buffer, index, nrem, nins);
  dispose();
}


void VfInform::style4 (const char *string, VfDataset *dataset,
                         long ifun, int type, long ipick, long nrem)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "ifun=%d  type=%s  ipick=%d  nrem=%d",
                              ifun, TYPE, ipick, nrem);
  dispose();
}


void VfInform::style5 (const char *string, VfDataset *dataset,
                        long ifun, int type, long ipick, long nrem, long nins)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "ifun=%d  type=%s  ipick=%d  nrem=%d  nins=%d",
                              ifun, TYPE, ipick, nrem, nins);
  dispose();
}


void VfInform::style7 (const char *string, VfDataset *dataset,
                         const char *code, const char *msg,
                         long i1, long i2, long i3, long i4, long i5)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "%s  %s  %d %d %d %d %d",
                                  code, msg, i1, i2, i3, i4, i5);
  dispose();
}



//-------------------- general use functions -------------------------//
//-------------------- general use functions -------------------------//
//-------------------- general use functions -------------------------//


void VfInform::beforeChanges()
{
  style0(" ");
  style0("beforeChanges");
}


void VfInform::afterChanges()
{
  style0("afterChanges");
  style0(" ");
}


void VfInform::beginSlowOperations()
{
  style0("beginSlowOperations");
}


void VfInform::endSlowOperations()
{
  style0("endSlowOperations");
}



void VfInform::ringBell()
{
  style0("ringBell");
}


void VfInform::showMessage(const char *msg)
{
  style1("showMessage", NULL, msg);
}


void VfInform::sendMessage(const char *code, const char *msg,
                           long i1, long i2, long i3, long i4, long i5)
{
  style7("sendMessage", NULL, code, msg, i1, i2, i3, i4, i5);
}


void VfInform::findActiveVelocityFunction(VfDataset *dataset)
{
  style0("findActiveVelocityFunction", dataset);
}



//------------------------- limited use functions ------------------//
//------------------------- limited use functions ------------------//
//------------------------- limited use functions ------------------//


void VfInform::returningToEventLoop()
{
  style0("returningToEventLoop");
}


void VfInform::dataGoingAway()
{
  style0("dataGoingAway");
}


void VfInform::newHistoryCard(VfDataset *dataset)
{
  style0("newHistoryCard", dataset);
}



//----------------------- data change notifications ------------------//
//----------------------- data change notifications ------------------//
//----------------------- data change notifications ------------------//


void VfInform::preChangeBinTolerances()
{
  style0("preChangeBinTolerances");
}

void VfInform::postChangeBinTolerances()
{
  style0("postChangeBinTolerances");
}



void VfInform::preNewActiveDataset()
{
  style0("preNewActiveDataset");
}

void VfInform::postNewActiveDataset()
{
  style0("postNewActiveDataset");
}



void VfInform::preNewReferenceDataset()
{
  style0("preNewReferenceDataset");
}

void VfInform::postNewReferenceDataset()
{
  style0("postNewReferenceDataset");
}



void VfInform::preSelectDataset(VfDataset *dataset)
{
  style0("preSelectDataset", dataset);
}

void VfInform::postSelectDataset(VfDataset *dataset)
{
  style0("postSelectDataset", dataset);
}



void VfInform::preUnselectDataset(VfDataset *dataset)
{
  style0("preUnselectDataset", dataset);
}

void VfInform::postUnselectDataset(VfDataset *dataset)
{
  style0("postUnselectDataset", dataset);
}



void VfInform::preTotalChanges(VfDataset *dataset)
{
  style0("preTotalChanges", dataset);
}

void VfInform::postTotalChanges(VfDataset *dataset)
{
  style0("postTotalChanges", dataset);
}


void VfInform::preRemoveInsertDatasets (long index, long nrem, long nins)
{
  style3("preRemoveInsertDatasets", NULL, index, nrem, nins);
}

void VfInform::postRemoveInsertDatasets (long index, long nrem, long nins)
{
  style3("postRemoveInsertDatasets", NULL, index, nrem, nins);
}



void VfInform::preChangeDataLock(VfDataset *dataset)
{
  style0("preChangeDataLock", dataset);
}

void VfInform::postChangeDataLock(VfDataset *dataset)
{
  style0("postChangeDataLock", dataset);
}



void VfInform::dataNeedsSavingFlagTurnedOn(VfDataset *dataset)
{
  style0("dataNeedsSavingFlagTurnedOn", dataset);
}

void VfInform::dataNeedsSavingFlagTurnedOff(VfDataset *dataset)
{
  style0("dataNeedsSavingFlagTurnedOff", dataset);
}



void VfInform::preChangeHeaderWords(VfDataset *dataset)
{
  style0("preChangeHeaderWords", dataset);
}

void VfInform::postChangeHeaderWords(VfDataset *dataset)
{
  style0("postChangeHeaderWords", dataset);
}



void VfInform::preChangeMoveoutOrder(VfDataset *dataset)
{
  style0("preChangeMoveoutOrder", dataset);
}

void VfInform::postChangeMoveoutOrder(VfDataset *dataset)
{
  style0("postChangeMoveoutOrder", dataset);
}



void VfInform::preChangeUnits(VfDataset *dataset)
{
  style0("preChangeUnits", dataset);
}

void VfInform::postChangeUnits(VfDataset *dataset)
{
  style0("postChangeUnits", dataset);
}



void VfInform::preNewActiveVelocityFunction(VfDataset *dataset)
{
  style0("preNewActiveVelocityFunction", dataset);
}

void VfInform::postNewActiveVelocityFunction(VfDataset *dataset)
{
  style0("postNewActiveVelocityFunction", dataset);
}



void VfInform::preNewReferenceVelocityFunction(VfDataset *dataset)
{
  style0("preNewReferenceVelocityFunction", dataset);
}

void VfInform::postNewReferenceVelocityFunction(VfDataset *dataset)
{
  style0("postNewReferenceVelocityFunction", dataset);
}



void VfInform::preNewNeighbors(VfDataset *dataset)
{
  style0("preNewNeighbors", dataset);
}

void VfInform::postNewNeighbors(VfDataset *dataset)
{
  style0("postNewNeighbors", dataset);
}



void VfInform::preRemoveInsertVelocityFunctions
                       (VfDataset *dataset, long ifun, long nrem, long nins)
{
  style3("preRemoveInsertVelocityFunctions",
                                      dataset, ifun, nrem, nins);
}

void VfInform::postRemoveInsertVelocityFunctions
                       (VfDataset *dataset, long ifun, long nrem, long nins)
{
  style3("postRemoveInsertVelocityFunctions",
                                      dataset, ifun, nrem, nins);
}



void VfInform::preChangeSelections(VfDataset *dataset, long ifun, long nchng)
{
  style2("preChangeSelections", dataset, ifun, nchng);
}

void VfInform::postChangeSelections(VfDataset *dataset, long ifun, long nchng)
{
  style2("postChangeSelections", dataset, ifun, nchng);
}



void VfInform::preChangeCoords(VfDataset *dataset, long ifun, long nchng)
{
  style2("preChangeCoords", dataset, ifun, nchng);
}

void VfInform::postChangeCoords(VfDataset *dataset, long ifun, long nchng)
{
  style2("postChangeCoords", dataset, ifun, nchng);
}




void VfInform::preNewDefaultTypes(VfDataset *dataset, long ifun, long nchng)
{
  style2("preNewDefaultTypes", dataset, ifun, nchng);
}

void VfInform::postNewDefaultTypes(VfDataset *dataset, long ifun, long nchng)
{
  style2("postNewDefaultTypes", dataset, ifun, nchng);
}




void VfInform::preModifyStrings(VfDataset *dataset, long ifun, long nchng)
{
  style2("preModifyStrings", dataset, ifun, nchng);
}

void VfInform::postModifyStrings(VfDataset *dataset, long ifun, long nchng)
{
  style2("postModifyStrings", dataset, ifun, nchng);
}




void VfInform::preNewActivePicks(VfDataset *dataset, long ifun, long nchng)
{
  style2("preNewActivePicks", dataset, ifun, nchng);
}

void VfInform::postNewActivePicks(VfDataset *dataset, long ifun, long nchng)
{
  style2("postNewActivePicks", dataset, ifun, nchng);
}




void VfInform::preChangePickSelections(VfDataset *dataset, long ifun)
{
  style1("preChangePickSelections", dataset, ifun);
}

void VfInform::postChangePickSelections(VfDataset *dataset, long ifun)
{
  style1("postChangePickSelections", dataset, ifun);
}




void VfInform::preModifyPicks
      (VfDataset *dataset, long ifun, int type, long ipick, long nrem)
{
  style4("preModifyPicks", dataset, ifun, type, ipick, nrem);
}

void VfInform::postModifyPicks
      (VfDataset *dataset, long ifun, int type, long ipick, long nrem,
                                                            long nins)
{
  style5("postModifyPicks", dataset, ifun, type, ipick, nrem, nins);
}



//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//


void VfInform::preNewActiveHorizon()
{
  style0("preNewActiveHorizon");
}

void VfInform::postNewActiveHorizon()
{
  style0("postNewActiveHorizon");
}



void VfInform::preRemoveInsertHorizons (long index, long nrem, long nins)
{
  style3("preRemoveInsertHorizons", NULL, index, nrem, nins);
}

void VfInform::postRemoveInsertHorizons (long index, long nrem, long nins)
{
  style3("postRemoveInsertHorizons", NULL, index, nrem, nins);
}



void VfInform::preNewSelectedHorizons()
{
  style0("preNewSelectedHorizons");
}

void VfInform::postNewSelectedHorizons()
{
  style0("postNewSelectedHorizons");
}



void VfInform::preNewActiveHorizonPick(long ihorizon)
{
  style1("preNewActiveHorizonPick", NULL, ihorizon);
}

void VfInform::postNewActiveHorizonPick(long ihorizon)
{
  style1("postNewActiveHorizonPick", NULL, ihorizon);
}



void VfInform::preNewHorizonColor(long ihorizon)
{
  style1("preNewHorizonColor", NULL, ihorizon);
}

void VfInform::postNewHorizonColor(long ihorizon)
{
  style1("postNewHorizonColor", NULL, ihorizon);
}



void VfInform::preNewHorizonTransform()
{
  style0("preNewHorizonTransform");
}

void VfInform::postNewHorizonTransform()
{
  style0("postNewHorizonTransform");
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
