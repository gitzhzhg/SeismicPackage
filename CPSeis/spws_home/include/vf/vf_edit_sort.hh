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

//-------------------------- vf_edit_sort.hh -------------------------//
//-------------------------- vf_edit_sort.hh -------------------------//
//-------------------------- vf_edit_sort.hh -------------------------//

//                 header file for the VfEditSort class
//                  derived from the VfEditBase class
//                          subdirectory vf


      // This class contains the algorithm for sorting velocity
      // functions, and the parameters needed for controlling
      // this algorithm.


//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//
//--------------------- start of coding ------------------------------//

#ifndef _VF_EDIT_SORT_HH_
#define _VF_EDIT_SORT_HH_

#include "vf/vf_edit_base.hh"


class VfEditSort : public VfEditBase
{

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

public:

enum { DIR_ASCENDING  = 1, // sort to ascending order.
       DIR_DESCENDING = 2, // sort to descending order.
       DIR_EITHER     = 3  // sort to order implied by first/last values.
     };

private:

  int   _xdirwant;       // direction to sort the X bins (above enum).
  int   _ydirwant;       // direction to sort the Y bins (above enum).
  int   _xfast;          // true to sort X bins fastest (false for Y bins).

//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//
//---------------------- functions ------------------------------------//

public:    // constructor and destructor.

           VfEditSort ();
  virtual ~VfEditSort ();

public:    // get values.

  int    getXdirection ()  const  { return _xdirwant; }
  int    getYdirection ()  const  { return _ydirwant; }
  int    getXfast      ()  const  { return _xfast; }

public:    // set values.

  void   setXdirection (int   value);
  void   setYdirection (int   value);
  void   setXfast      (int   value)  { _xfast = value; }

public:   // overriding virtual functions.

  virtual int  virtualCheck   (class VfKernal *kernal, char *msg);
  virtual int  virtualEdit    (class VfKernal *kernal, char *msg);

private:

  int  checkSort (class VfKernal *kernal, int *xdir, int *ydir);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
