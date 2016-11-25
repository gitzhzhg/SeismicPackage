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

//-------------------------- vf_edit_raytrace.hh -------------------------//
//-------------------------- vf_edit_raytrace.hh -------------------------//
//-------------------------- vf_edit_raytrace.hh -------------------------//

//                 header file for the VfEditRaytrace class
//                  derived from the VfEditBase class
//                          subdirectory vf


      // This class contains the algorithm for raytracing velocity
      // functions, and the parameters needed for controlling
      // this algorithm.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_EDIT_RAYTRACE_HH_
#define _VF_EDIT_RAYTRACE_HH_

#include "vf/vf_edit_base.hh"


class VfEditRaytrace : public VfEditBase
{

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

public:

enum { HOW_FORWARD     = 1, // raytrace to get NMO from RMS velocities.
       HOW_INVERSE     = 2, // inverse raytrace to get RMS from NMO velocities.
       HOW_RESET_NMO   = 3, // reset NMO velocities equal to RMS velocities.
       HOW_RESET_RMS   = 4  // reset RMS velocities equal to NMO velocities.
     };

private:

  int   _how;              // how to process the functions       (enum).

//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//

public:    // constructor and destructor.

           VfEditRaytrace ();
  virtual ~VfEditRaytrace ();

public:    // get values.

  int    getHow    ()  const  { return _how;             }

public:    // set values.

  void   setHow    (int   value);

public:   // overriding virtual functions.

  virtual int  virtualCheck (class VfKernal *kernal, char *msg);
  virtual int  virtualEdit  (class VfKernal *kernal, char *msg);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
