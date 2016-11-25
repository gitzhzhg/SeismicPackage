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

//---------------------- static_inform.cc ------------------------//
//---------------------- static_inform.cc ------------------------//
//---------------------- static_inform.cc ------------------------//

//         implementation file for the StaticInform class
//                   not derived from any class
//                        subdirectory stat


#include "stat/static_inform.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "stat/static_informer.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream.h>
#include <assert.h>



//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//

StaticInform::StaticInform(StaticManager *manager, int printit)
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


StaticInform::~StaticInform()
{
  if(_manager) _manager->informer()->removeInformObject(this);
}



//------------------- private helper functions ------------------------//
//------------------- private helper functions ------------------------//
//------------------- private helper functions ------------------------//

#define RETURN  if(!_printit) return;
#define INDEX   dataset->indexOfThisDataset() + 1


static char prefix [111];    // set by     front       and used by dispose.
static char suffix [111];    // set by style functions and used by dispose.


void StaticInform::dispose()
{
  printf("%s  %s\n", prefix, suffix);
}



void StaticInform::front(const char *string, StaticDataset *dataset)
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



void StaticInform::style0 (const char *string, StaticDataset *dataset)
{
  RETURN
  front(string, dataset);
  strcpy(suffix, "");
  dispose();
}


void StaticInform::style1 (const char *string, StaticDataset *dataset,
                           const char *msg)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "%s", msg);
  dispose();
}


void StaticInform::style3 (const char *string, StaticDataset *dataset,
                           int index, int nrem, int nins)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "index=%d  nrem=%d  nins=%d", index, nrem, nins);
  dispose();
}


void StaticInform::style4 (const char *string, StaticDataset *dataset,
                           int ix, int iy, int nxchng, int nychng)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "ix=%d  iy=%d  nxchng=%d  nychng=%d",
                              ix, iy, nxchng, nychng);
  dispose();
}



void StaticInform::style7 (const char *string, StaticDataset *dataset,
                           const char *code, const char *msg,
                           int i1, int i2, int i3, int i4, int i5)
{
  RETURN
  front(string, dataset);
  sprintf(suffix, "%s  %s  %d %d %d %d %d",
                                  code, msg, i1, i2, i3, i4, i5);
  dispose();
}



//------------------- general use functions ---------------------------//
//------------------- general use functions ---------------------------//
//------------------- general use functions ---------------------------//


void StaticInform::beforeChanges()
{
  style0(" ");
  style0("beforeChanges");
}


void StaticInform::afterChanges()
{
  style0("afterChanges");
  style0(" ");
}


void StaticInform::beginSlowOperations()
{
  style0("beginSlowOperations");
}


void StaticInform::endSlowOperations()
{
  style0("endSlowOperations");
}



void StaticInform::ringBell()
{
  style0("ringBell");
}


void StaticInform::showMessage(const char *msg)
{
  style1("showMessage", NULL, msg);
}


void StaticInform::sendMessage(const char *code, const char *msg,
                               int i1, int i2, int i3, int i4, int i5)
{
  style7("sendMessage", NULL, code, msg, i1, i2, i3, i4, i5);
}


void StaticInform::findActiveGroundPosition(StaticDataset *dataset)
{
  style0("findActiveGroundPosition", dataset);
}



//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//
//--------------------- limited use functions -----------------------//


void StaticInform::returningToEventLoop()
{
  style0("returningToEventLoop");
}


void StaticInform::dataGoingAway()
{
  style0("dataGoingAway");
}



//-------------------- data change notifications ---------------------//
//-------------------- data change notifications ---------------------//
//-------------------- data change notifications ---------------------//


void StaticInform::preRemoveInsertDatasets(int index, int nrem, int nins)
{
  style3("preRemoveInsertDatasets", NULL, index, nrem, nins);
}

void StaticInform::postRemoveInsertDatasets(int index, int nrem, int nins)
{
  style3("postRemoveInsertDatasets", NULL, index, nrem, nins);
}



void StaticInform::preNewActiveDataset()
{
  style0("preNewActiveDataset");
}

void StaticInform::postNewActiveDataset()
{
  style0("postNewActiveDataset");
}



void StaticInform::preNewReferenceDataset()
{
  style0("preNewReferenceDataset");
}

void StaticInform::postNewReferenceDataset()
{
  style0("postNewReferenceDataset");
}



void StaticInform::dataNeedsSavingFlagTurnedOn(StaticDataset *dataset)
{
  style0("dataNeedsSavingFlagTurnedOn", dataset);
}

void StaticInform::dataNeedsSavingFlagTurnedOff(StaticDataset *dataset)
{
  style0("dataNeedsSavingFlagTurnedOff", dataset);
}



void StaticInform::preSelectDataset(StaticDataset *dataset)
{
  style0("preSelectDataset", dataset);
}

void StaticInform::postSelectDataset(StaticDataset *dataset)
{
  style0("postSelectDataset", dataset);
}



void StaticInform::preUnselectDataset(StaticDataset *dataset)
{
  style0("preUnselectDataset", dataset);
}

void StaticInform::postUnselectDataset(StaticDataset *dataset)
{
  style0("postUnselectDataset", dataset);
}



void StaticInform::preChangeDataLock(StaticDataset *dataset)
{
  style0("preChangeDataLock", dataset);
}

void StaticInform::postChangeDataLock(StaticDataset *dataset)
{
  style0("postChangeDataLock", dataset);
}



void StaticInform::preTotalChanges(StaticDataset *dataset)
{
  style0("preTotalChanges", dataset);
}

void StaticInform::postTotalChanges(StaticDataset *dataset)
{
  style0("postTotalChanges", dataset);
}



void StaticInform::preNewActiveGroundPosition(StaticDataset *dataset)
{
  style0("preNewActiveGroundPosition", dataset);
}

void StaticInform::postNewActiveGroundPosition(StaticDataset *dataset)
{
  style0("postNewActiveGroundPosition", dataset);
}



void StaticInform::preChangeSelections(StaticDataset *dataset,
                  int ix, int iy, int nxchng, int nychng)
{
  style4("preChangeSelections", dataset, ix, iy, nxchng, nychng);
}

void StaticInform::postChangeSelections(StaticDataset *dataset,
                  int ix, int iy, int nxchng, int nychng)
{
  style4("postChangeSelections", dataset, ix, iy, nxchng, nychng);
}




void StaticInform::preChangeStaticValues(StaticDataset *dataset,
                  int ix, int iy, int nxchng, int nychng)
{
  style4("preChangeStaticValues", dataset, ix, iy, nxchng, nychng);
}

void StaticInform::postChangeStaticValues(StaticDataset *dataset,
                  int ix, int iy, int nxchng, int nychng)
{
  style4("postChangeStaticValues", dataset, ix, iy, nxchng, nychng);
}



void StaticInform::preChangeStattype(StaticDataset *dataset)
{
  style0("preChangeStattype", dataset);
}

void StaticInform::postChangeStattype(StaticDataset *dataset)
{
  style0("postChangeStattype", dataset);
}



void StaticInform::preChangeHeaderWords(StaticDataset *dataset)
{
  style0("preChangeHeaderWords", dataset);
}

void StaticInform::postChangeHeaderWords(StaticDataset *dataset)
{
  style0("postChangeHeaderWords", dataset);
}



void StaticInform::preTransformGroundPositions(StaticDataset *dataset)
{
  style0("preTransformGroundPositions", dataset);
}

void StaticInform::postTransformGroundPositions(StaticDataset *dataset)
{
  style0("postTransformGroundPositions", dataset);
}



//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//
//-------------------- data user messages ---------------------------//






//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
