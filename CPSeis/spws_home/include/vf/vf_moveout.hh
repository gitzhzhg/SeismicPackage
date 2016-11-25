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

//-------------------------- vf_moveout.hh -------------------------//
//-------------------------- vf_moveout.hh -------------------------//
//-------------------------- vf_moveout.hh -------------------------//

//                 header file for the VfMoveout class
//                    not derived from any class
//                          subdirectory vf


      // This class contains the algorithm for doing normal moveout.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_MOVEOUT_HH_
#define _VF_MOVEOUT_HH_

#include "named_constants.h"


class VfMoveout
{

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

public:  // these must match NmopureWrapper enums (except for ORDER_2_OR_4).

  enum { ORDER_2           = 1,
         ORDER_4           = 2,
         ORDER_ETA4        = 3,
         ORDER_ETA         = 4,
         ORDER_2_PLUS_4    = 5,
         ORDER_2_PLUS_ETA4 = 6,
         ORDER_2_PLUS_ETA  = 7,
         ORDER_2_OR_4      = 8  };

private:

//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//

public:    // constructor and destructor.

           VfMoveout ();
  virtual ~VfMoveout ();

public:    // get values.


public:    // set values.


public: // get doppler mute times corresponding to specified offsets.
        // dataset = velocity dataset to use.
        // ifun    = index of velocity function to use.
        // tmax    = maximum mute time (seconds) to search for.
        // dt      = desired resolution in mute time (seconds) (e.g. 0.16).
        // doppler = CPS doppler mute parameter (usual value 1.7).
        // offsets[number] = array of offsets            (input).
        // times  [number] = array of doppler mute times (output).
        // these arrays can be used for drawing a doppler mute line.
        // if order == ORDER_2           dataset is used.
        // if order == ORDER_4           dataset is used.
        // if order == ORDER_ETA4        dataset and etaset (eta) are used.
        // if order == ORDER_ETA         dataset and etaset (eta) are used.
        // if order == ORDER_2_PLUS_4    dataset and etaset (4th) are used.
        // if order == ORDER_2_PLUS_ETA4 dataset and etaset (eta) are used.
        // if order == ORDER_2_PLUS_ETA  dataset and etaset (eta) are used.
        // if order == ORDER_2_OR_4      dataset is used (with nhosign,nhoexp).
        // etaset is a dataset containing eta values for anisotropic NMO.
        // in one case above, etaset contains traditional 4th order parameters.
        // etaset can be NULL if order is ORDER_2 or ORDER_4 or ORDER_2_OR_4.
        // nhosign,nhoexp are used to get order if order is ORDER_2_OR_4.

  void getDopplerMute (class VfDataset *dataset, long ifun,
                       float tmax, float dt, float doppler,
                       int number, const float *offsets, float *times,
                       class VfDataset *etaset = NULL,
                       int order = ORDER_2_OR_4);

public:   // get moveout times corresponding to specified offsets.
          // dataset = velocity dataset to use.
          // ifun    = index of velocity function to use.
          // ipick   = index of desired velocity-time pick to use.
          // tpick   = zero-offset time                 (input).
          // vpick   = corresponding NMO velocity       (input).
          // offsets[number] = array of offsets       (input).
          // times  [number] = array of moveout times (output).
          // these arrays can be used for drawing a moveout hyperbola.
          // dataset is needed for getting nhosign and nhoexp.
          // in the first function, dataset is also needed for getting
          //   tpick and vpick from indices ifun and ipick.

  void getMoveoutTimes (class VfDataset *dataset,
                       long ifun, long ipick,
                       int number, const float *offsets, float *times)  const;

  void getMoveoutTimes (class VfDataset *dataset,
                       float tpick, float vpick,
                       int number, const float *offsets, float *times)  const;

public:   // get time-velocity pair from two points on a moveout hyperbola.
          // dataset = velocity dataset to use.
          // offset1 and time1 define   one   point on hyperbola.
          // offset2 and time2 define another point on hyperbola.
          // tpick = zero-offset time           (returned).
          // vpick = corresponding NMO velocity (returned).
          // dataset is needed only for getting nhosign and nhoexp.

  void getPickFromTwoPoints (class VfDataset *dataset,
                             float offset1, float time1,
                             float offset2, float time2,
                             float *tpick, float *vpick)  const;

public: // do non-hyperbolic nmo correction on byte or float arrays.
        // returns error = FALSE if all ok and error = TRUE if fails.
        // byte or float arrays are unchanged if fails.
        // dataset = velocity dataset to use.
        // ifun    = index of velocity function to use for forward nmo
        //             (this function is saved for later reverse nmo)
        //             (ifun is irrelevant for reverse nmo).
        // mode    =  >= 1 for forward nmo, <= -1 for reverse nmo.
        //            == 0 for reverse nmo followed by forward nmo.
        // doppler = doppler mute (1.7 to allow stretch up to factor 1.7).
 // bbbb[nsamp * ntot] = trace byte values  for all traces in the image.
 // ffff[nsamp * ntot] = trace float values for all traces in the image.
 // hhhh[nwh   * ntot] = trace header words for all traces in the image.
        // nsamp   = #byte  values on each trace in bbbb[].
        // nsamp   = #float values on each trace in ffff[].
        // nwh     = #words in each trace header in hhhh[] (offset = 6th word).
        // ntot    = #traces (and trace headers) in the image.
        // tmin    = minimum time on trace (i.e. time of first trace sample).
        // dt      = sample rate on trace (seconds).
        // dataset is needed for getting nhosign and nhoexp.
        // for forward nmo, dataset is also needed for getting velocity
        //   function using ifun; this function is then saved for doing
        //   reverse nmo later.
        // if order == ORDER_2           dataset is used.
        // if order == ORDER_4           dataset is used.
        // if order == ORDER_ETA4        dataset and etaset (eta) are used.
        // if order == ORDER_ETA         dataset and etaset (eta) are used.
        // if order == ORDER_2_PLUS_4    dataset and etaset (4th) are used.
        // if order == ORDER_2_PLUS_ETA4 dataset and etaset (eta) are used.
        // if order == ORDER_2_PLUS_ETA  dataset and etaset (eta) are used.
        // if order == ORDER_2_OR_4      dataset is used (with nhosign,nhoexp).
        // etaset is a dataset containing eta values for anisotropic NMO.
        // in one case above, etaset contains traditional 4th order parameters.
        // etaset can be NULL if order is ORDER_2 or ORDER_4 or ORDER_2_OR_4.
        // nhosign,nhoexp are used to get order if order is ORDER_2_OR_4.

  int doMoveout (class VfDataset *dataset, long ifun,
                 int mode, float doppler,
                 float *ffff, float *hhhh,
                 long nsamp, long nwh, long ntot, float tmin, float dt,
                 class VfDataset *etaset = NULL, int order = ORDER_2_OR_4);

   //// the following function is obsolete but still needed to satisfy
   //// unsatisfied externals:

  int doMoveout (class VfDataset *dataset, long ifun,
                 int mode, float doppler,
                 unsigned char *bbbb, float *hhhh,
                 long nsamp, long nwh, long ntot, float tmin, float dt,
                 class VfDataset *etaset = NULL, int order = ORDER_2_OR_4);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
