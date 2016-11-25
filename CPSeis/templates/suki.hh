/****
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
!!!
!!!           Header File Template for Fortran to C++ Interfaces
!!!
!!!-----------------------------------------------------------------------------
!!!                   REVISION HISTORY FOR THIS TEMPLATE
!!!
!!!     Date        Author     Description
!!!     ----        ------     -----------
!!!  3. 2002-10-23  Stoeckley  Minor history doc improvements.
!!!  2. 2002-05-16  Stoeckley  Add brief doc section and this template
!!!                             revision history.
!!!  1. 2001-10-23  Stoeckley  Initial version.
!!!
!!!-----------------------------------------------------------------------------
!!!
!!! This file can be used as a template for Fortran to C++ interfaces.
!!! See c2f_interface.h (and suki_wrapper.f90) for details. 
!!!
!!!-----------------------------------------------------------------------------
****/


/****
!<CPS_v1 type="HEADER_FILE"/>
****/
//------------------------------- suki.hh --------------------------------//
//------------------------------- suki.hh --------------------------------//
//------------------------------- suki.hh --------------------------------//

       // other files are:  suki.cc  suki_wrapper.f90  suki_crou.cc

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                      C P S   H E A D E R   F I L E
!
! Name       : SUKI
! Category   : --> should match the main file of this primitive.
! Written    : 2001-01-01   by: NNNN
! Revised    : 2001-01-01   by: NNNN
! Maturity   : beta
! Purpose    : --> should match the main file of this primitive.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2001-01-01  NNNN       Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//
//--------------------------- start of coding ------------------------------//


#ifndef _SUKI_HH_
#define _SUKI_HH_


#define SUKI_NFFF 4
#define SUKI_NCCC 21


class Suki
{

//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//
//--------------------------------- data -----------------------------------//

private:

  int    _iii;
  float  _fff[SUKI_NFFF];
  double _ddd;
  char   _ccc[SUKI_NCCC];

//----------------------------- functions ----------------------------------//
//----------------------------- functions ----------------------------------//
//----------------------------- functions ----------------------------------//

public:

           Suki  ();
  virtual ~Suki  ();
  int      solve (float fff[], double ddd, char *ccc);

//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//

} ;

#endif

//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//
//--------------------------------- end -------------------------------------//

