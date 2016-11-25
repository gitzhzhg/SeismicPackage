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

//-------------------------- vf_edit_nearsample.hh -------------------------//
//-------------------------- vf_edit_nearsample.hh -------------------------//
//-------------------------- vf_edit_nearsample.hh -------------------------//

//              header file for the VfEditNearsample class
//                  derived from the VfEditBase class
//                          subdirectory vf


      // This class contains the algorithm for resampling velocity
      // functions laterally, and the parameters needed for controlling
      // this algorithm.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_EDIT_NEARSAMPLE_HH_
#define _VF_EDIT_NEARSAMPLE_HH_

#include "vf/vf_edit_base.hh"


class VfEditNearsample : public VfEditBase
{

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

private:

  class VfUtilities *_utilities;

  int   _type;             // resampling velocity function type    (enum).
  int   _which_abscissae;  // VfKernal::NEAREST_ or RESTRICTED_ABSCISSAE.
  float _x1;               // desired first X location.
  float _y1;               // desired first Y location.
  float _xinc;             // resampling X increment.
  float _yinc;             // resampling Y increment.
  long  _nx;               // desired number of X locations.
  long  _ny;               // desired number of Y locations.


//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//

public:    // constructor and destructor.

           VfEditNearsample (VfUtilities *utilities);
  virtual ~VfEditNearsample ();

public:    // get values.

  int    getResampleType   ()  const  { return _type;      }
  int    getWhichAbscissae ()  const  { return _which_abscissae;    }
  float  getX1             ()  const  { return _x1;        }
  float  getY1             ()  const  { return _y1;        }
  float  getXinc           ()  const  { return _xinc;      }
  float  getYinc           ()  const  { return _yinc;      }
  long   getNx             ()  const  { return _nx;        }
  long   getNy             ()  const  { return _ny;        }
  float  getXend           ()  const  { return _x1 + (_nx-1) * _xinc;  }
  float  getYend           ()  const  { return _y1 + (_ny-1) * _yinc;  }

public:    // set values.

  void   setResampleType   (int   value);
  void   setWhichAbscissae (int   value);
  void   setX1             (float value);
  void   setY1             (float value);
  void   setXinc           (float value);
  void   setYinc           (float value);
  void   setNx             (long  value);
  void   setNy             (long  value);
  void   setXend           (float value);
  void   setYend           (float value);

public:   // overriding virtual functions.

  virtual int  virtualCheck   (class VfKernal *kernal, char *msg);
  virtual int  virtualEdit    (class VfKernal *kernal, char *msg);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
