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

//----------------------- tp_sisc_pair.cc -----------------------//
//----------------------- tp_sisc_pair.cc -----------------------//
//----------------------- tp_sisc_pair.cc -----------------------//

//            implementation file for the TpSiscPair class
//               derived from the TpStatfilePair class
//                       subdirectory pick

                  // also used by FISH and CC3D

#include "pick/tp_sisc_pair.hh"
#include "pick/tp_popup_base.hh"
#include "stat/static_kernal.hh"
#include "stat/statutil_wrapper.hh"
#include "oprim/history_cards.hh"
#include "sp/seis_plot.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <string.h>
#include <assert.h>


//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpSiscPair::TpSiscPair(SLDelay *slparent,
                          TpPopupBase *pop, SeisPlot *sp,
                          const char * const filetype,
                          const char * const extension,
                          const Boolean      required1,
                          const Boolean      required2,
                          const char * const program,
                          const char * const default_stattype)
       : TpStatfilePair(slparent, pop, sp, 
                        filetype, extension, required1, required2,
                        program, default_stattype)
{
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpSiscPair::~TpSiscPair()
{
}



//--------------- convert picks to or from file ----------------//
//--------------- convert picks to or from file ----------------//
//--------------- convert picks to or from file ----------------//


void TpSiscPair::convertPicksToOrFromFile(float *picks,
                         const float *head, long /*nwords*/, long n)
{
  float offset  = ConstrainValue(head[27], 0.001, 0.900);
  float znil    = ZNIL;
  float missing = MISSING;
  for(int i = 0; i < n; i++)
      {
      if(picks[i] != znil && picks[i] != missing)
          {
          picks[i] = offset - picks[i];
          }
      }
}



//----------------- adjust picks by action ---------------------//
//----------------- adjust picks by action ---------------------//
//----------------- adjust picks by action ---------------------//

    // sets picks to nil if correlation trace is dead and
    // action is either AUTOMATIC or SNAP.

void TpSiscPair::adjustPicksByAction(float *picks,
                  const float *head, long nwords, long n, long action)
{
  if(action != AUTOMATIC && action != SNAP) return;
  float znil    = ZNIL;
  float missing = MISSING;
  for(int i = 0; i < n; i++)
      {
      if(picks[i] != missing && picks[i] != znil)
          {
          if(head[4 + nwords * i] == 0.0) picks[i] = znil;
          }
      }
}



//--------------------- do read current picks ------------------//
//--------------------- do read current picks ------------------//
//--------------------- do read current picks ------------------//


void TpSiscPair::doReadCurrentPicks(float *picks,
                         const float *head, long nwords, long n)
{
  TpStatfilePair::doReadCurrentPicks(picks, head, nwords, n);
  convertPicksToOrFromFile          (picks, head, nwords, n);
}



//--------------------- do save current picks ------------------//
//--------------------- do save current picks ------------------//
//--------------------- do save current picks ------------------//


void TpSiscPair::doSaveCurrentPicks(float *picks,
                   const float *head, long nwords, long n, long action)
{
  convertPicksToOrFromFile          (picks, head, nwords, n);
  adjustPicksByAction               (picks, head, nwords, n, action);
  TpStatfilePair::doSaveCurrentPicks(picks, head, nwords, n, action);
}



//-------------------- do read previous picks -------------------//
//-------------------- do read previous picks -------------------//
//-------------------- do read previous picks -------------------//

          // used only by SISC to get integrated picks

void TpSiscPair::doReadPreviousPicks(float *picks,
                         const float *head, long nwords, long n)
{
  char msg[111];
  long  nx   = getNx();
  long  ny   = getNy();
  float xrun = 51.0;
  float yrun = 1.0;
  long  trim = 0;
  assert(ny == 1);

  float *keep = new float[nx];
  _kernal->history()->disableHistoryCardAdditions ();
  _kernal           ->getStaticValues             (keep);
  _kernal           ->integrateStaticValues       (FALSE, msg);
  _kernal           ->removeRunningAverage (FALSE, StaticKernal::OPTION_REMOVE,
                                            trim, xrun, yrun,
                                            StaticKernal::ENDFLAG_N, msg);
  doReadCurrentPicks                              (picks, head, nwords, n);
  _kernal           ->setStaticValues             (keep);
  _kernal->history()->enableHistoryCardAdditions  ();
  delete [] keep;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
