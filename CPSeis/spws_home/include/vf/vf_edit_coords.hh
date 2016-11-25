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

//-------------------------- vf_edit_coords.hh -------------------------//
//-------------------------- vf_edit_coords.hh -------------------------//
//-------------------------- vf_edit_coords.hh -------------------------//

//                 header file for the VfEditCoords class
//                  derived from the VfEditBase class
//                          subdirectory vf


      // This class contains the algorithm for resetting velocity
      // function headers, and the parameters needed for controlling
      // this algorithm.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_EDIT_COORDS_HH_
#define _VF_EDIT_COORDS_HH_

#include "vf/vf_edit_base.hh"


class VfEditCoords : public VfEditBase
{

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

public:

  enum { DIRECTION_FORWARD = 1, DIRECTION_REVERSE, INTEGERIZE,
         SWAP_COORDS, SWAP_COORDS_AND_HEADERS };

private:

  class GridTransform *_transform;    // owned by this class.
  int                  _direction;    // one of the above enums.


//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//

public:    // constructor and destructor.

           VfEditCoords ();
  virtual ~VfEditCoords ();

public:    // get values.

  class GridTransform *transform()  const  { return _transform; }

  int               getDirection()  const  { return _direction; }

public:    // set values.

  void        setDirection (int direction);

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
