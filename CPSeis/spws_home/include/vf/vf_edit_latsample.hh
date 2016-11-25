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

//-------------------------- vf_edit_latsample.hh -------------------------//
//-------------------------- vf_edit_latsample.hh -------------------------//
//-------------------------- vf_edit_latsample.hh -------------------------//

//                 header file for the VfEditLatsample class
//                  derived from the VfEditBase class
//                          subdirectory vf


      // This class contains the algorithm for resampling velocity
      // functions laterally, and the parameters needed for controlling
      // this algorithm.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_EDIT_LATSAMPLE_HH_
#define _VF_EDIT_LATSAMPLE_HH_

#include "vf/vf_edit_base.hh"


class VfEditLatsample : public VfEditBase
{

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

public:

  enum {
       TERPFLAG_LINEAR  =  1,  // use linear interpolation.
       TERPFLAG_CUBIC   =  2,  // use four-point cubic interpolation.
       TERPFLAG_SPLINE  =  3,  // use spline interpolation.
       TERPFLAG_POLY    =  4,  // use least-squares polynomial fit.
       TERPFLAG_RUN     =  5   // use running average smoothing.
       };

  enum {
       STEPFLAG_CHANGE  = 1,   // change the X and/or Y function locations.
       STEPFLAG_RETAIN  = 2    // retain the current function locations.
       };

  enum {
       ENDFLAG_TRUNCATED = 1,  // use truncated running average end option.
       ENDFLAG_SHIFTED   = 2,  // use shifted   running average end option.
       ENDFLAG_EXTENDED  = 3,  // use extended  running average end option.
       ENDFLAG_NARROWED  = 4   // use narrowed  end option (graded ends).
       };

  enum {
       DIRFLAG_X_ONLY   = 1,  // resample in X direction only.
       DIRFLAG_Y_ONLY   = 2,  // resample in Y direction only.
       DIRFLAG_X_THEN_Y = 3,  // resample in X direction first, then Y.
       DIRFLAG_Y_THEN_X = 4   // resample in Y direction first, then X.
       };

private:

  class VfUtilities *_utilities;

  int   _type;        // resampling velocity function type    (enum).
  int   _terpflag;    // the type of interpolation/resampling (enum).
  int   _stepflag;    // how to choose the time/depth values  (enum).
  int   _endflag;     // end range option for running average (enum).
  int   _dirflag;     // resampling direction                 (enum).
  float _x1;          // desired first X location.
  float _y1;          // desired first Y location.
  float _xinc;        // resampling X increment.
  float _yinc;        // resampling Y increment.
  long  _nx;          // desired number of X locations.
  long  _ny;          // desired number of Y locations.
  int   _smooth;      // whether to smooth only below specified time/depth.
  float _tsmooth;     // smooth only below this time.
  float _dsmooth;     // smooth only below this depth.
  float _dv;          // average spline velocity error to accept.
  long  _ncoef;       // number of coefficients in polynomial fit.
  long  _nrun;        // number of functions in running average.


//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//

public:    // constructor and destructor.

           VfEditLatsample (VfUtilities *utilities);
  virtual ~VfEditLatsample ();

private:   // verify valid parameter.
           // these assert if parameter is invalid.

  static void  verifyValidType      (int     type);
  static void  verifyValidTerpflag  (int terpflag);
  static void  verifyValidStepflag  (int stepflag);
  static void  verifyValidEndflag   (int  endflag);
  static void  verifyValidDirflag   (int  dirflag);

public:    // get values.

  int    getResampleType   ()  const  { return _type;      }
  int    getTerpflag       ()  const  { return _terpflag;  }
  int    getStepflag       ()  const  { return _stepflag;  }
  int    getEndflag        ()  const  { return _endflag;   }
  int    getDirflag        ()  const  { return _dirflag;   }
  float  getX1             ()  const  { return _x1;        }
  float  getY1             ()  const  { return _y1;        }
  float  getXinc           ()  const  { return _xinc;      }
  float  getYinc           ()  const  { return _yinc;      }
  long   getNx             ()  const  { return _nx;        }
  long   getNy             ()  const  { return _ny;        }
  float  getXend           ()  const  { return _x1 + (_nx-1) * _xinc;  }
  float  getYend           ()  const  { return _y1 + (_ny-1) * _yinc;  }
  int    getSmooth         ()  const  { return _smooth;    }
  float  getTsmooth        ()  const  { return _tsmooth;   }
  float  getDsmooth        ()  const  { return _dsmooth;   }
  float  getDv             ()  const  { return _dv;        }
  long   getNcoef          ()  const  { return _ncoef;     }
  long   getNrun           ()  const  { return _nrun;      }

public:    // set values.

  void   setResampleType   (int   value);
  void   setTerpflag       (int   value);
  void   setStepflag       (int   value);
  void   setEndflag        (int   value);
  void   setDirflag        (int   value);
  void   setX1             (float value);
  void   setY1             (float value);
  void   setXinc           (float value);
  void   setYinc           (float value);
  void   setNx             (long  value);
  void   setNy             (long  value);
  void   setXend           (float value);
  void   setYend           (float value);
  void   setSmooth         (int   value);
  void   setTsmooth        (float value);
  void   setDsmooth        (float value);
  void   setDv             (float value);
  void   setNcoef          (long  value);
  void   setNrun           (long  value);

public:    // check whether parameter value is allowed.
           // or check whether parameter is needed.
           // depends on current values of other parameters.
           // these return TRUE or FALSE.

  int   allowType          (int value)  const;
  int   allowTerpflag      (int value)  const;
  int   allowStepflag      (int value)  const;
  int   needStepflag       ()           const;
  int   needEndflag        ()           const;
  int   needXlocations     ()           const;
  int   needYlocations     ()           const;
  int   needSmooth         ()           const;
  int   needTsmooth        ()           const;
  int   needDsmooth        ()           const;
  int   needDv             ()           const;
  int   needNcoef          ()           const;
  int   needNrun           ()           const;

public:   // overriding virtual functions.

  virtual int  virtualCheck   (class VfKernal *kernal, char *msg);
  virtual int  virtualEdit    (class VfKernal *kernal, char *msg);

private:

  int resample  (VfKernal *kernal, char *msg, int dir);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
