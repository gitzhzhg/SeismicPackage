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

//-------------------------- nmopure_wrapper.hh ----------------------------//
//-------------------------- nmopure_wrapper.hh ----------------------------//
//-------------------------- nmopure_wrapper.hh ----------------------------//

//              header file for the NmopureWrapper class
//                      not derived from any class
//                          subdirectory oprim


#ifndef _NMOPURE_WRAPPER_HH_
#define _NMOPURE_WRAPPER_HH_

#include "c2f_interface.h"
#include "named_constants.h"


class NmopureWrapper
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//

public:  // these must match moveout.f90 and dynsamp.f90 parameters.

  enum { ACTION_FORWARD = 1,
         ACTION_REVERSE = 2,
         ACTION_PARTIAL = 3,
         ACTION_VNMO    = 4,
         ACTION_VINT    = 5  };

  enum { ORDER_2           = 1,
         ORDER_4           = 2,
         ORDER_ETA4        = 3,
         ORDER_ETA         = 4,
         ORDER_2_PLUS_4    = 5,
         ORDER_2_PLUS_ETA4 = 6,
         ORDER_2_PLUS_ETA  = 7  };

  enum { TERPMODE_CUBIC  = 1,
         TERPMODE_LINEAR = 2,
         TERPMODE_FFT2   = 3,
         TERPMODE_FFT4   = 4,
         TERPMODE_FFT8   = 5  };

private:

  F90Pointer  _fpoint;
  int         _ndpt;

//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//
//----------------------------- functions ---------------------------------//

public:

           NmopureWrapper (int ndpt, float tstrt, float dt, int action,
                           int order, int terpmode, float doppler,
                           int tracemute, int *error, char *msg);
  virtual ~NmopureWrapper ();

  void     velfun  (int npicks2, const float *tpicks2, const float *vpicks2,
                    int npicks4, const float *tpicks4, const float *vpicks4,
                    int *error, char *msg);

  void     apply   (float *offset, float *offnew, float *tr,
                    int *mtop, int *mbottom);

//--------------------------- information ------------------------------//
//--------------------------- information ------------------------------//
//--------------------------- information ------------------------------//

     //            ndpt = number of trace values.
     //           tstrt = trace starting time (seconds).
     //              dt = trace sample interval (seconds).
     //          action = normal moveout action to perform (enum).
     //           order = normal moveout order (enum).
     //        terpmode = trace interpolation option (enum).
     //         doppler = doppler stretch factor (>1.0).
     //       tracemute = whether to mute the trace (true or false).
     //           error = error flag (true if an error occurred).
     //             msg = message for possible printing.

     //          npicks = number of time/velocity picks.
     //  tpicks(npicks) = time picks.
     //  vpicks(npicks) = velocity picks.

     //          offset = offset of trace (changed if doing partial NMO).
     //          offnew = new offset of trace (if doing partial NMO).
     //        tr(ndpt) = trace values.
     //            mtop = head mute index of trace (used and changed).
     //         mbottom = tail mute index of trace (used and changed).

 // OFFSET and OFFNEW are swapped when doing partial NMO.
 // Otherwise, OFFSET is not changed and OFFNEW is not used.

 // MTOP and MBOTTOM will first be adjusted by the same amount as the
 // corresponding trace sample moves.  Then they will be adjusted downward
 // and upward, respectively, to exclude regions of index reversals.
 // Finally, if DOPPLER is specified and is > 1.0, regions of excessive
 // stretch will also be excluded.

 // If TRACEMUTE is true, the trace will be muted above MTOP and below MBOTTOM,
 // and a mute taper will be applied at the top for forward NMO correction.

//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

};

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
