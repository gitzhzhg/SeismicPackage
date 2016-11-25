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

//-------------------------- vf_horizonio.hh ----------------------------//
//-------------------------- vf_horizonio.hh ----------------------------//
//-------------------------- vf_horizonio.hh ----------------------------//

//               header file for the VfHorizonio class
//               derived from the FloatioWrapper class
//                          subdirectory vf


       // This class provides self defining ascii file I/O for
       // a horizon data object containing five arrays with
       // field names xcoord, ycoord, pick, shotpoint, and line.


#ifndef _VF_HORIZONIO_HH_
#define _VF_HORIZONIO_HH_

#include "oprim/floatio_wrapper.hh"


class VfHorizonio : public FloatioWrapper
{

//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//

private:


//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//

public:  // io must be 0 for input and 1 for output.

           VfHorizonio (int io);
  virtual ~VfHorizonio ();


//---------------- virtual functions overriding FloatioWrapper -----------//
//---------------- virtual functions overriding FloatioWrapper -----------//
//---------------- virtual functions overriding FloatioWrapper -----------//

public:

  virtual int   showFieldtypes      ()  const  { return FALSE; }
  virtual int   showUnits           ()  const  { return FALSE; }
  virtual int   showHdrs            ()  const  { return FALSE; }
  virtual int   showDefaults        ()  const  { return FALSE; }
  virtual int   showComments        ()  const  { return FALSE; }
  virtual int   showWidths          ()  const  { return TRUE ; }
  virtual int   showConverters      ()  const  { return TRUE ; }
  virtual int   showMaxchars        ()  const  { return TRUE ; }


//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

};

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
