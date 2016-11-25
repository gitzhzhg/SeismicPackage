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

//------------------------ tpbox_angle_table.hh --------------------------//
//------------------------ tpbox_angle_table.hh --------------------------//
//------------------------ tpbox_angle_table.hh --------------------------//

//            header file for the TpboxAngleTable class
//                derived from the SLDatabox class
//                      subdirectory pick

 
#ifndef _TPBOX_ANGLE_TABLE_HH_
#define _TPBOX_ANGLE_TABLE_HH_

#include "sl/sl_databox.hh"


class TpboxAngleTable  :  public SLDatabox
{

//----------------------------- data -------------------------------//
//----------------------------- data -------------------------------//
//----------------------------- data -------------------------------//

private:

  class SeisAvast *_so;
  int              _nangles;       // number of angles in list.
  float           *_angles;        // array of angles.
  int             *_select;        // array of select flags.

//------------------------ functions ------------------------------//
//------------------------ functions ------------------------------//
//------------------------ functions ------------------------------//

public:          // constructors and destructor

  TpboxAngleTable (SLDelay *slparent, class SeisAvast *so);
  virtual ~TpboxAngleTable();

  int    numAngles  ()           const  { return _nangles; }
  float  getAngle   (int index)  const;
  int    isSelected (int index)  const;        // returns TRUE/FALSE.

  void   setAngles        (int nangles, float *angles);
  void   setSelection     (int index, int select);      // select = TRUE/FALSE.
  void   toggleSelection  (int index);
  void   toggleSelections ();

private:

  void makeHelper();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
