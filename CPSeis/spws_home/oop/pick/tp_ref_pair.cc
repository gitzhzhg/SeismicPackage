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

//----------------------- tp_ref_pair.cc -----------------------//
//----------------------- tp_ref_pair.cc -----------------------//
//----------------------- tp_ref_pair.cc -----------------------//

//         implementation file for the TpRefPair class
//               derived from the TpPairBase class
//                       subdirectory pick


#include "pick/tp_ref_pair.hh"
#include "pick/tp_popup_base.hh"
#include "sp/seis_plot.hh"
#include "oprim/refraction_data.hh"
#include "sl/shell_watch.hh"
#include "plot/pick_watch.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <string.h>
#include <assert.h>


//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//


static const long NLARGE = 500;  // show watch cursor when
                                 // reading or saving more
                                 // than this many picks

static const char * const FILETYPE  = "SCRS pickfile";
static const char * const EXTENSION = "cst";

static const Boolean      REQUIRED1 = FALSE;
static const Boolean      REQUIRED2 = FALSE;



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpRefPair::TpRefPair(SLDelay *slparent,
                          TpPopupBase *pop, SeisPlot* /*dummy*/)
       : TpPairBase(slparent, "ref_pair",
                         FILETYPE, EXTENSION, REQUIRED1, REQUIRED2),
                  _pop               (pop),
/*
                  _sp                (sp),
*/
                  _refdata           (NULL),
                  _align             (CHANNEL),
                  _selgrp            (0),
                  _firstgrp          (0),
                  _lastgrp           (0),
                  _typegp            (SEQU),
                  _latest            (0),
                  _disp1             (0),
                  _disp2             (0),
                  _pickhead          (0)
{
/*
  assert(_pop && _sp);
*/
  assert(_pop);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpRefPair::~TpRefPair()
{
  if(_refdata) delete _refdata;
}



//------------------------ do validate ------------------------//
//------------------------ do validate ------------------------//
//------------------------ do validate ------------------------//

            // overrides SLFilePairPlus
            // called from private SLFilePairPlus::validateTrap
            //       which is called from SLFilePair
            //       which is called from make_filepair

void TpRefPair::doValidate (const char *filename1,
                           const char *filename2,
                           long *valid1, long *valid2,
                           char *info1, char *info2,
                           long *same_datasets)
{
  // cout << "am in TpRefPair::doValidate" << endl;
  RefractionData::checkValidities((char*)filename1, (char*)filename2,
                                  valid1, valid2,
                                  info1, info2,
                                  same_datasets);
}



//-------------------- do open --------------------------------//
//-------------------- do open --------------------------------//
//-------------------- do open --------------------------------//

            // overrides SLFilePairPlus
            // called from public SLFilePairPlus::openFiles()

int TpRefPair::doOpen (long /*status*/,
                            const char *filename1,
                            const char *filename2,
                            Boolean required1, Boolean required2,
                            FppWorkingMessage *working_message_trap,
                            void              *working_message_data,
                            char *msg)
{
  // cout << "am in TpRefPair::doOpen" << endl;
  if(_refdata) delete _refdata;
  SeisPlot *sp = _pop->getSeisPlot();        // new 9/2/97.
  _refdata = new RefractionData(
        filename1, filename2, required1, required2,
        working_message_trap, working_message_data, _typegp,
/*
        _sp->filename(),
*/
        sp->filename(),
        (void*(*)(char*,long*,long*))tracefile_open,
        (long (*)(void *, float *))tracefile_read_header,
        (long (*)(void *))tracefile_rewind,
        (long (*)(void *))tracefile_close, _pickhead);

  if(_refdata->constructionError())
     {
     strcpy(msg, _refdata->constructionMessage());
     doClose();
     return 1;   // error
     }

  _selgrp   = 1;
  _firstgrp = _refdata->firstShotRecord();
  _lastgrp  = _refdata->lastShotRecord();
  _latest   = _refdata->latestModifiedShotRecord();
  _disp1    = 0;
  _disp2    = 0;
  return 0;   // no error
}



//---------------------- do close ----------------------------//
//---------------------- do close ----------------------------//
//---------------------- do close ----------------------------//

            // overrides SLFilePairPlus
            // called from public SLFilePairPlus::closeFiles()

void TpRefPair::doClose()
{
  // cout << "am in TpRefPair::doClose" << endl;
  if(_refdata) delete _refdata;
  _refdata = NULL;
  _selgrp   = 0;
  _firstgrp = 0;
  _lastgrp  = 0;
  _latest   = 0;
  _disp1    = 0;
  _disp2    = 0;
}



//-------------------- class RefWatch ------------------------//
//-------------------- class RefWatch ------------------------//
//-------------------- class RefWatch ------------------------//

     // This class creates watch cursors on shells, and on
     // plots with active picking.  This class has been written
     // because the HP does not like the following code:
     //           if(n > NLARGE) ShellWatch watch1;
     //           if(n > NLARGE) PickWatch  watch2;

class RefWatch
  {
  ShellWatch *watch1;
  PickWatch  *watch2;
  public:
  RefWatch(long n); 
  ~RefWatch();
  };

RefWatch::RefWatch(long n) : watch1(NULL), watch2(NULL)
{
  if(n > NLARGE)
      {
      watch1 = new ShellWatch();
      watch2 = new PickWatch ();
      }
}

RefWatch::~RefWatch()
{
  if(watch1) delete watch1;
  if(watch2) delete watch2;
}



//-------------------- do read current picks -------------------//
//-------------------- do read current picks -------------------//
//-------------------- do read current picks -------------------//

void TpRefPair::doReadCurrentPicks(float *picks,
                         const float *head, long nwords, long n)
{
  assert(_refdata);
  RefWatch watch(n);
  _refdata->readPicks(head, nwords, n, picks);
  _disp1 = NearestInteger(head[8]);
  _disp2 = NearestInteger(head[8 + (n - 1) * nwords]);
}



//-------------------- do read previous picks -------------------//
//-------------------- do read previous picks -------------------//
//-------------------- do read previous picks -------------------//

void TpRefPair::doReadPreviousPicks(float *picks,
                         const float *head, long nwords, long n)
{
  assert(_refdata);
  RefWatch watch(n);
  _refdata->readPrevPicks(head, nwords, n, picks, _align);
}



//-------------------- do read next picks -------------------//
//-------------------- do read next picks -------------------//
//-------------------- do read next picks -------------------//

void TpRefPair::doReadNextPicks(float *picks,
                         const float *head, long nwords, long n)
{
  assert(_refdata);
  RefWatch watch(n);
  _refdata->readNextPicks(head, nwords, n, picks, _align);
}



//-------------------- do read selected picks -------------------//
//-------------------- do read selected picks -------------------//
//-------------------- do read selected picks -------------------//

void TpRefPair::doReadSelectedPicks(float *picks,
                         const float *head, long nwords, long n)
{
  assert(_refdata);
  RefWatch watch(n);
  _refdata->readSelPicks(head, nwords, n, picks, _align, _selgrp);
}



//-------------------- do save current picks -------------------//
//-------------------- do save current picks -------------------//
//-------------------- do save current picks -------------------//

void TpRefPair::doSaveCurrentPicks(float *picks,
                 const float *head, long nwords, long n, long /*action*/)
{
  assert(_refdata);
  RefWatch watch(n);
  _refdata->writePicks(head, nwords, n, picks);
  _latest = _refdata->latestModifiedShotRecord();
}


//-------------------- do update file --------------------------//
//-------------------- do update file --------------------------//
//-------------------- do update file --------------------------//

void TpRefPair::doUpdateFile()
{
  ///////// Nothing needs to be done here, since the data
  ///////// are written to the file by doSaveCurrentPicks.
}



//-------------------- set variables -----------------------//
//-------------------- set variables -----------------------//
//-------------------- set variables -----------------------//


void TpRefPair::setTypeGP(long typegp)
{
  if(typegp == _typegp) return;
  if(fileIsLoaded()) return;
  _typegp = typegp;
  char msg[100];
  strcpy(msg, "type of ground positions changed to\n");
  switch(_typegp)
     {
     case SEQU : strcat(msg, "sequential"); break;
     case GRID : strcat(msg, "grid"      ); break;
     default: assert(FALSE);
     }
  _pop->showMessage(msg);
}



void TpRefPair::setOverlayAlignment(long align)
{
  if(align == _align) return;
  _align = align;
  _pop->updatePrevNextSelVectors();
  char msg[100];
  strcpy(msg, "overlay alignment changed to\n");
  switch(_align)
     {
     case CHANNEL : strcat(msg, "matching channel" ); break;
     case OFFSET  : strcat(msg, "matching offset"  ); break;
     case RECEIVER: strcat(msg, "matching receiver"); break;
     default: assert(FALSE);
     }
  _pop->showMessage(msg);
}



void TpRefPair::setSelectedProfile(long selgrp)
{
  selgrp = ConstrainValue(selgrp, _firstgrp, _lastgrp);
  if(selgrp == _selgrp) return;
  _selgrp = selgrp;
  _pop->updateSelVector();
  _pop->showMessage("new selected profile\nchosen");
}



void TpRefPair::setPickhead(int pickhead)
{
  _pickhead = ConstrainValue(pickhead, 0, 999);
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
