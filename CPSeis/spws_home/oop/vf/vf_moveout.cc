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

//-------------------------- vf_moveout.cc ---------------------------//
//-------------------------- vf_moveout.cc ---------------------------//
//-------------------------- vf_moveout.cc ---------------------------//

//            implementation file for the VfMoveout class
//                    not derived from any class
//                         subdirectory vf


#include "vf/vf_moveout.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "oprim/nmopure_wrapper.hh"
#include "trslib.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//
//------------------ constructor and destructor ---------------------//


VfMoveout::VfMoveout()
{
}



VfMoveout::~VfMoveout()
{
}



//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//
//-------------------------- set values ----------------------------//



//---------------------- get moveout function --------------------------//
//---------------------- get moveout function --------------------------//
//---------------------- get moveout function --------------------------//


static void get_moveout_function
                (int *order, VfDataset *dataset, long ifun, VfDataset *etaset,
                 float *tpicks2, float *vpicks2, long *npicks2,
                 float *tpicks4, float *vpicks4, long *npicks4)
{
  assert(dataset);
  *npicks2 = 0;
  *npicks4 = 0;

  float nhosign = dataset->getNhosign();
  float nhoexp  = dataset->getNhoexp ();

  assert( (nhosign >  0.9 && nhoexp < 2.1) ||
          (nhosign < -0.9 && nhoexp > 3.9) );

  int is_second_order = (nhosign >  0.9 && nhoexp < 2.1);
  int is_fourth_order = (nhosign < -0.9 && nhoexp > 3.9);

  if(*order == VfMoveout::ORDER_2_OR_4)
      {
      if(is_fourth_order) *order = VfMoveout::ORDER_4;
      else                *order = VfMoveout::ORDER_2;
      }

  switch(*order)
      {
      case VfMoveout::ORDER_2          : assert(is_second_order); break;
      case VfMoveout::ORDER_4          : assert(is_fourth_order); break;
      case VfMoveout::ORDER_ETA4       : assert(is_second_order); break;
      case VfMoveout::ORDER_ETA        : assert(is_second_order); break;
      case VfMoveout::ORDER_2_PLUS_4   : assert(is_second_order); break;
      case VfMoveout::ORDER_2_PLUS_ETA4: assert(is_second_order); break;
      case VfMoveout::ORDER_2_PLUS_ETA : assert(is_second_order); break;
      default               : assert(FALSE);
      }

  if(*order == VfMoveout::ORDER_4)
      {
      *npicks4 = dataset->numPicks(ifun);
      dataset->getTimeArray(ifun, tpicks4);
      dataset->getVnmoArray(ifun, vpicks4);
      return;
      }

  *npicks2 = dataset->numPicks(ifun);
  dataset->getTimeArray(ifun, tpicks2);
  dataset->getVnmoArray(ifun, vpicks2);

  if(*order == VfMoveout::ORDER_2) return;

  float xloc = dataset->getXloc(ifun);
  float yloc = dataset->getYloc(ifun);

  assert(etaset);

  long ifun4 = etaset->findMatchingVelfun(xloc, yloc);

  if(ifun4 < 0)
      {
      *order = VfMoveout::ORDER_2;
      return;
      }

  *npicks4 = etaset->numPicks(ifun);
  etaset->getTimeArray(ifun, tpicks4);

  if(*order == VfMoveout::ORDER_2_PLUS_4)
      {
      etaset->getVnmoArray(ifun, vpicks4);
      }
  else
      {
      etaset->getVintArray(ifun, vpicks4);
      }
}



//-------------------------- get doppler mute -----------------------//
//-------------------------- get doppler mute -----------------------//
//-------------------------- get doppler mute -----------------------//

       // Note: This function is called for only one trace at a
       // time since it is called by a vector data getY function.
       // I've decided to enforce this with an assert.
       // Eventually this function will not be needed since the
       // mute times are now being put into trace headers.

       // public.
       // returns doppler mute times corresponding to specified offsets.
       // dataset = velocity dataset to use.
       // ifun    = index of velocity function to use.
       // tmax    = maximum mute time (seconds) to search for.
       // dt      = desired resolution in mute time (seconds) (e.g. 0.016).
       // doppler = CPS doppler mute parameter (usual value 1.7).
       // offsets[number] = array of offsets            (input).
       // times  [number] = array of doppler mute times (output).
       // these arrays can be used for drawing a doppler mute line.


void VfMoveout::getDopplerMute(VfDataset *dataset, long ifun,
                      float tmax, float dt, float doppler,
                      int number, const float *offsets, float *times,
                      VfDataset *etaset, int order)
{
  assert(number == 1);

  float tmin  = 0.0;
  long  nsamp = 1 + NearestInteger((tmax - tmin) / dt);
  int   mode  = 1;
  int   nwh   = 64;
  int   i;

  float *hhhh = new float [nwh];
  float *ffff = new float [nsamp];

  for(i = 0; i < nwh; i++)
      {
      hhhh[i] = 0.0;
      }
  for(i = 0; i < nsamp; i++)
      {
      ffff[i] = 0.0;   // will not be used.
      }

  hhhh[1] = 1;                 // header 2.
  hhhh[5] = offsets[0];        // header 6.
  hhhh[63] = nsamp;            // header 64.

  doMoveout(dataset, ifun, mode, doppler, ffff, hhhh,
            nsamp, nwh, number, tmin, dt, etaset, order);

  times[0] = hhhh[30];         // header 31.

  delete [] hhhh;
  delete [] ffff;

/******************
  if(number < 1) return;

  if(dt <= 0.0)
      {
      for(int i = 0; i < number; i++) { times[i] = 0.004; }
      return;
      }

  long  npicks2;
  float tpicks2[MAXPICKS];
  float vpicks2[MAXPICKS];
  long  npicks4;
  float tpicks4[MAXPICKS];
  float vpicks4[MAXPICKS];

  int do_eta_moveout = get_moveout_function (dataset, ifun, etaset,
                                             tpicks2, vpicks2, &npicks2,
                                             tpicks4, vpicks4, &npicks4);
  if(npicks2 == 0)
      {
      for(int i = 0; i < number; i++) { times[i] = 0.004; }
      return;
      }

  float nhosign = dataset->getNhosign();
  float nhoexp  = dataset->getNhoexp ();

  int error;
  char msg[111];

  float tstrt     = 0.0;
  int   ndpt      = 1 + NearestInteger((tmax - tstrt) / dt);
  int   action    = NmopureWrapper::ACTION_FORWARD;
  int   order     = NmopureWrapper::ORDER_2;
  int   terpmode  = NmopureWrapper::TERPMODE_CUBIC;
  int   tracemute = FALSE;

  if(nhoexp > 3.9) order = NmopureWrapper::ORDER_4;

  NmopureWrapper *nmopure = new NmopureWrapper(ndpt, tstrt, dt,
                                               action, order, terpmode,
                                               doppler, tracemute,
                                               &error, msg);

  nmopure->velfun(npicks, tpicks, vpicks, &error, msg);

  float *ffff = new float [ndpt];
  for(int i2 = 0; i2 < ndpt; i2++)
      {
      ffff[i2] = 0.0;   // will not be used.
      }

  for(int i = 0; i < number; i++)
      {
      float offset  = offsets[i];
      float offnew  = 0.0;
      int   mtop    = 1.0;
      int   mbottom = ndpt;
      nmopure->apply(&offset, &offnew, ffff, &mtop, &mbottom);
      times[i] = tstrt + (mtop - 1) * dt;
      }
  delete [] ffff;
  delete nmopure;
******************/
}



//------------------------ get moveout times -------------------------//
//------------------------ get moveout times -------------------------//
//------------------------ get moveout times -------------------------//

       // public.
       // returns moveout times corresponding to specified offsets.
       // dataset = velocity dataset to use.
       // ifun    = index of velocity function to use.
       // ipick   = index of desired velocity-time pick to use.
       // offsets[number] = array of offsets       (input).
       // times  [number] = array of moveout times (output).
       // these arrays can be used for drawing a moveout hyperbola.
       // dataset is needed for getting nhosign and nhoexp.
       // dataset is also needed for getting tpick and vpick
       //   from indices ifun and ipick.

void VfMoveout::getMoveoutTimes(VfDataset *dataset,
                      long ifun, long ipick,
                      int number, const float *offsets, float *times)  const
{
  assert(dataset);
  float tpick   = dataset->getTime(ifun, ipick);
  float vpick   = dataset->getVnmo(ifun, ipick);
  getMoveoutTimes(dataset, tpick, vpick, number, offsets, times);
}



//------------------------ get moveout times -------------------------//
//------------------------ get moveout times -------------------------//
//------------------------ get moveout times -------------------------//

       // public.
       // returns moveout times corresponding to specified offsets.
       // dataset = velocity dataset to use.
       // tpick   = zero-offset time                 (input).
       // vpick   = corresponding NMO velocity       (input).
       // offsets[number] = array of offsets       (input).
       // times  [number] = array of moveout times (output).
       // these arrays can be used for drawing a moveout hyperbola.
       // dataset is needed only for getting nhosign and nhoexp.

void VfMoveout::getMoveoutTimes(VfDataset *dataset,
                      float tpick, float vpick,
                      int number, const float *offsets, float *times)  const
{
  assert(dataset);
  float nhosign = dataset->getNhosign();
  float nhoexp  = dataset->getNhoexp ();
  float nhoexp2 = ConstrainValue(nhoexp, 0.1, 8.0);
  float tpick2  = tpick * tpick;
  float vpick2  = pow(vpick, nhoexp2);
  if(vpick2 < 100.0) vpick2 = 100.0;

  for(int i = 0; i < number; i++)
       {
       float offset2 = AbsoluteValue(offsets[i]);
       if(offset2 < 1.0) offset2 = 1.0;
       float arg = tpick2 + nhosign * pow(offset2, nhoexp2) / vpick2;
       if(arg < 0.0) arg = 0.0;
       times[i] = sqrt(arg);
       }
}



//-------------------------- get pick from two points ------------------//
//-------------------------- get pick from two points ------------------//
//-------------------------- get pick from two points ------------------//

       // public.
       // get time-velocity pair from two points on a moveout hyperbola.
       // dataset = velocity dataset to use.
       // offset1 and time1 define   one   point on hyperbola.
       // offset2 and time2 define another point on hyperbola.
       // tpick = zero-offset time           (returned).
       // vpick = corresponding NMO velocity (returned).
       // dataset is needed only for getting nhosign and nhoexp.

static float vmin =   100.0;
static float vmax = 50000.0;

void VfMoveout::getPickFromTwoPoints(VfDataset *dataset,
                                     float offset1, float time1,
                                     float offset2, float time2,
                                     float *tpick, float *vpick)  const
{
  assert(dataset);
  float nhosign = dataset->getNhosign();
  float nhoexp  = dataset->getNhoexp ();
  float nhoexp2 = ConstrainValue(nhoexp, 0.1, 8.0);

  float off1e = 0.0;
  float off2e = 0.0;
  if(offset1 > 0.0) off1e = pow(offset1, nhoexp2);
  if(offset2 > 0.0) off2e = pow(offset2, nhoexp2);

  if(time1 == time2)
       {
       *vpick = vmax;
       }
  else
       {
       float vpick2 =
               nhosign * (off1e - off2e) / (time1 * time1 - time2 * time2);
       if(vpick2 > 0.0)
            {
            *vpick = pow(vpick2, 1.0 / nhoexp2);
            if(*vpick < vmin) *vpick = vmin;
            if(*vpick > vmax) *vpick = vmax;
            }
       else
            {
            *vpick = vmax;
            }
       }

  float vpick2 = pow(*vpick, nhoexp2);
  float tpick2 = time1 * time1 - nhosign * off1e / vpick2;
  if(tpick2 > 0.0)
       {
       *tpick = sqrt(tpick2);
       }
  else
       {
       *tpick = 0.0;
       }
}



//------------------------ do moveout (byte values) --------------------//
//------------------------ do moveout (byte values) --------------------//
//------------------------ do moveout (byte values) --------------------//

// this routine is no longer used (and is out of date) but must not be
// removed yet because it will become an unsatisfied external.

int VfMoveout::doMoveout(VfDataset *dataset, long ifun,
                         int mode, float doppler,
                         unsigned char *bbbb, float *hhhh,
                         long nsamp, long nwh, long ntot, float tmin, float dt,
                         VfDataset *etaset, int order)
{
                  assert(FALSE);
                  assert(FALSE);
                  assert(FALSE);
                  assert(FALSE);
  return TRUE;
}



//------------- do moveout (float values) (using prodlib) -------------------//
//------------- do moveout (float values) (using prodlib) -------------------//
//------------- do moveout (float values) (using prodlib) -------------------//

     // public.
     // do non-hyperbolic nmo correction on float arrays.
     // returns error = FALSE if all ok and error = TRUE if fails.
     // byte arrays are unchanged if fails.
     // dataset = velocity dataset to use.
     // ifun    = index of velocity function to use.
     // mode    =  >= 1 for forward nmo, <= -1 for reverse nmo.
     //            == 0 is not allowed.
     // doppler = doppler mute (1.7 to allow stretch up to factor 1.7).
     // ffff[nsamp * ntot] = trace float values for all traces in the image.
     // hhhh[nwh   * ntot] = trace header words for all traces in the image.
     // nsamp   = #byte values on each trace in bbbb[].
     // nwh     = #words in each trace header in hhhh[] (offset = 6th word).
     // ntot    = #traces (and trace headers) in the image.
     // tmin    = minimum time on trace (i.e. time of first trace sample).
     // dt      = sample rate on trace (seconds).
     // dataset is needed for getting nhosign and nhoexp.
     // dataset is also needed for getting velocity function using ifun.


int VfMoveout::doMoveout(VfDataset *dataset, long ifun,
                         int mode, float doppler,
                         float *ffff, float *hhhh,
                         long nsamp, long nwh, long ntot, float tmin, float dt,
                         VfDataset *etaset, int order)
{
  assert(dataset);
  assert(ffff);
  assert(hhhh);
  if(mode == 0 || dt <= 0.0 || ntot < 1 || nwh < 6) return TRUE;

  long  npicks2;
  float tpicks2[MAXPICKS];
  float vpicks2[MAXPICKS];
  long  npicks4;
  float tpicks4[MAXPICKS];
  float vpicks4[MAXPICKS];

  get_moveout_function (&order, dataset, ifun, etaset,
                        tpicks2, vpicks2, &npicks2,
                        tpicks4, vpicks4, &npicks4);
  if(npicks2 == 0)
      {
      for(int i = 0; i < ntot; i++)
          {
          int j = i * nwh;
          hhhh[j + 29] = 1;        // put new mute index into header 30.
          hhhh[j + 30] = tmin;     // put new mute time  into header 31.
          }
      return TRUE;
      }

  int error;
  char msg[111];

  int action    = NmopureWrapper::ACTION_FORWARD;
  int terpmode  = NmopureWrapper::TERPMODE_CUBIC;
  int tracemute = FALSE;

  if(mode <= -1)  action = NmopureWrapper::ACTION_REVERSE;

  NmopureWrapper *nmopure = new NmopureWrapper(nsamp, tmin, dt,
                                               action, order, terpmode,
                                               doppler, tracemute,
                                               &error, msg);

  nmopure->velfun(npicks2, tpicks2, vpicks2,
                  npicks4, tpicks4, vpicks4, &error, msg);

  for(int i = 0; i < ntot; i++)
      {
      int j = i * nwh;
      int k = i * nsamp;
      float offset  = (float)hhhh[j +  5];  // offset header 6.
      float offnew  = (float)hhhh[j + 31];  // scratch header 32 (partial NMO).
/***
      int   mtop    = NearestInteger(hhhh[j +  1]);   // top mute header 2.
      int   mbottom = NearestInteger(hhhh[j + 63]);   // bottom header 64.
***/
      int   mtop    = 1;      // so headers 30 and 31 will not be influenced
      int   mbottom = nsamp;  // by the original trace mutes.

      nmopure->apply(&offset, &offnew, &ffff[k], &mtop, &mbottom);

      hhhh[j + 29] = mtop;               // put new mute index into header 30.
      hhhh[j + 30] = tmin + (mtop-1)*dt; // put new mute time  into header 31.
      }
  delete nmopure;
  return error;
}


//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

