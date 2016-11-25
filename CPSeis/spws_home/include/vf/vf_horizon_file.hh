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

//-------------------------- vf_horizon_file.hh ---------------------//
//-------------------------- vf_horizon_file.hh ---------------------//
//-------------------------- vf_horizon_file.hh ---------------------//

//               header file for the VfHorizonFile class
//                 derived from the FileBase class
//                         subdirectory vf


#ifndef _VF_HORIZON_FILE_HH_
#define _VF_HORIZON_FILE_HH_

#include "oprim/file_base.hh"


class VfHorizonFile  :  public FileBase
{

//------------------------------ data -------------------------------//
//------------------------------ data -------------------------------//
//------------------------------ data -------------------------------//

private:

  class VfManager   *_manager;    // pointer provided to constructor.
  class VfHorizons  *_horizons;   // pointer provided to constructor.
  class VfHorizonio *_horizonio;  // pointer provided to constructor.


//---------------------------- functions ------------------------------//
//---------------------------- functions ------------------------------//
//---------------------------- functions ------------------------------//

public:  // constructor and destructor.

           VfHorizonFile (VfManager *manager, VfHorizons *horizons,
                          VfHorizonio *horizonio, Intent intent);
  virtual ~VfHorizonFile ();

private:  // virtual functions overriding FileBase.

  virtual Validity virtualValidate    (const char *filename, char *info);
  virtual Prepare  virtualPrepareRead (const char *filename, char *msg);
  virtual Result   virtualRead        (const char *filename, char *msg);


//---------------------------- end of functions -----------------------//
//---------------------------- end of functions -----------------------//
//---------------------------- end of functions -----------------------//

} ;

#endif

//---------------------------------- end --------------------------------//
//---------------------------------- end --------------------------------//
//---------------------------------- end --------------------------------//
