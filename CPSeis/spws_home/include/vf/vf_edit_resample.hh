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

//-------------------------- vf_edit_resample.hh -------------------------//
//-------------------------- vf_edit_resample.hh -------------------------//
//-------------------------- vf_edit_resample.hh -------------------------//

//              header file for the VfEditResample class
//                  derived from the VfEditBase class
//                          subdirectory vf

      // This class contains the algorithm for resampling
      // velocity function picks, and the parameters needed
      // for controlling the resampling algorithm.

      // The variable _type must have values defined by an enum statement
      // in the vf_constants.hh header file.  However, velocity function
      // type versus layer thickness is not allowed.

      // See implementation file for further documentation.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_EDIT_RESAMPLE_HH_
#define _VF_EDIT_RESAMPLE_HH_

#include "vf/vf_edit_base.hh"


class VfEditResample : public VfEditBase
{

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

public:

  enum {
       TERPFLAG_LINEAR  =  1,  // linear interpolation with flat    extrap.
       TERPFLAG_LINEAR2 = -1,  // linear interpolation with sloping extrap.
       TERPFLAG_CUBIC   =  2,  // four-point cubic interpolation.
       TERPFLAG_SPLINE  =  3,  // relaxed spline interpolation.
       TERPFLAG_POLY    =  4,  // least-squares polynomial fit.
       TERPFLAG_RUN     =  5,  // linear interp + flat    + run av smoothing.
       TERPFLAG_RUN2    = -5,  // linear interp + sloping + run av smoothing.
       TERPFLAG_EARTH   =  6,  // fill in points without changing earth model.
       TERPFLAG_TOP     =  7,  // reset top velocity function pick.
       TERPFLAG_BOTTOM  =  8   // reset bottom velocity function pick.
       };


  enum {
       STEPFLAG_DELTA   = 1,   // use top and bottom sampling increments.
       STEPFLAG_NUMBER  = 2,   // use specified number of samples.
       STEPFLAG_RETAIN  = 3,   // retain current abscissae.
       STEPFLAG_ARRAY   = 4    // use abscissae specified in an array.
       };


  enum {
       ENDFLAG_TRUNCATED = 1,  // running av truncated end range.
       ENDFLAG_SHIFTED   = 2,  // running av shifted   end range.
       ENDFLAG_EXTENDED  = 3,  // running av extended  end range.
       ENDFLAG_NARROWED  = 4   // running av narrowed  end range (graded ends).
       };

private:

  class VfUtilities *_utilities;

  int   _type;             // velocity function type to resample   (enum).
  int   _terpflag;         // the type of interpolation/resampling (enum).
  int   _stepflag;         // how to choose the time/depth values  (enum).
  int   _endflag;          // end range option for running average (enum).
  int   _reset_timedepth;  // TRUE or FALSE when resetting bottom pick.
  int   _reset_velocity;   // TRUE or FALSE when resetting bottom pick.
  float _bottom_time;      // value to use when resetting bottom pick.
  float _bottom_depth;     // value to use when resetting bottom pick.
  float _bottom_velocity;  // value to use when resetting bottom pick.
  float _top_velocity;     // value to use when resetting top pick.
  long  _nwant;            // desired number of picks.
  long  _ncoef;            // number of coefficients for polynomial.
  long  _nrun;             // length of running average.
  float _dx1;              // resampling depth increment at top.
  float _dx2;              // resampling depth increment at bottom.
  float _dt1;              // resampling time increment at top.
  float _dt2;              // resampling time increment at bottom.
  float _dv;               // average velocity deviation for spline fit.

//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//

public:    // constructor and destructor.

           VfEditResample (VfUtilities *utilities);
  virtual ~VfEditResample ();

private:   // verify valid parameter.
           // these assert if parameter is invalid.

  static void  verifyValidType      (int     type);
  static void  verifyValidTerpflag  (int terpflag);
  static void  verifyValidStepflag  (int stepflag);
  static void  verifyValidEndflag   (int  endflag);

public:    // get values.

  int    getResampleType   ()  const  { return _type;            }
  int    getTerpflag       ()  const  { return _terpflag;        }
  int    getStepflag       ()  const  { return _stepflag;        }
  int    getEndflag        ()  const  { return _endflag;         }
  int    getResetTimedepth ()  const  { return _reset_timedepth; }
  int    getResetVelocity  ()  const  { return _reset_velocity;  }
  float  getBottomTime     ()  const  { return _bottom_time;     }
  float  getBottomDepth    ()  const  { return _bottom_depth;    }
  float  getBottomVelocity ()  const  { return _bottom_velocity; }
  float  getTopVelocity    ()  const  { return _top_velocity; }
  long   getNwant          ()  const  { return _nwant;           }
  long   getNcoef          ()  const  { return _ncoef;           }
  long   getNrun           ()  const  { return _nrun;            }
  float  getDx1            ()  const  { return _dx1;             }
  float  getDx2            ()  const  { return _dx2;             }
  float  getDt1            ()  const  { return _dt1;             }
  float  getDt2            ()  const  { return _dt2;             }
  float  getDv             ()  const  { return _dv;              }

public:    // set values.

  void   setResampleType   (int   value);
  void   setTerpflag       (int   value);
  void   setStepflag       (int   value);
  void   setEndflag        (int   value);
  void   setResetTimedepth (int   value);
  void   setResetVelocity  (int   value);
  void   setBottomTime     (float value);
  void   setBottomDepth    (float value);
  void   setBottomVelocity (float value);
  void   setTopVelocity    (float value);
  void   setNwant          (long  value);
  void   setNcoef          (long  value);
  void   setNrun           (long  value);
  void   setDx1            (float value);
  void   setDx2            (float value);
  void   setDt1            (float value);
  void   setDt2            (float value);
  void   setDv             (float value);

public:    // check whether parameter value is allowed.
           // or check whether parameter is needed.
           // depends on current values of other parameters.
           // these return TRUE or FALSE.

  int   allowType          (int value)  const;
  int   allowTerpflag      (int value)  const;
  int   allowStepflag      (int value)  const;
  int   needStepflag       ()           const;
  int   needEndflag        ()           const;
  int   needResetTimedepth ()           const;
  int   needResetTime      ()           const;
  int   needResetDepth     ()           const;
  int   needResetVelocity  ()           const;
  int   needBottomTime     ()           const;
  int   needBottomDepth    ()           const;
  int   needBottomVelocity ()           const;
  int   needTopVelocity    ()           const;
  int   needNwant          ()           const;
  int   needNcoef          ()           const;
  int   needNrun           ()           const;
  int   needDx             ()           const;
  int   needDt             ()           const;
  int   needDv             ()           const;

public:   // overriding virtual functions.

  virtual int virtualCheck   (class VfKernal *kernal, char *msg);
  virtual int virtualEdit    (class VfKernal *kernal, char *msg);

private:

  int  resampleOneFunction (class VfFunction *velfun,
                            float last_time, float last_depth);

  void resetBottomPick     (long n, float *x, float *v,
                            long *nn, float *xx, float *vv);

  void resetTopPick        (long n, float *x, float *v,
                            long *nn, float *xx, float *vv);

  int  resampleHelper      (float last_time, float last_depth,
                            long n, float *x, float *v,
                            long *nn, float *xx, float *vv);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
