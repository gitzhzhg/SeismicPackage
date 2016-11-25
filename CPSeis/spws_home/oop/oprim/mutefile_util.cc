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

//-------------------------- mutefile_util.cc ----------------------------//
//-------------------------- mutefile_util.cc ----------------------------//
//-------------------------- mutefile_util.cc ----------------------------//

//            implementation file for the MutefileUtil class
//                     not derived from any class
//                         subdirectory oprim


#include "oprim/mutefile_util.hh"
#include "mutefile.h"
#include "xyzdim.h"
#include "xydim.h"
#include "xdim.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


MutefileUtil::MutefileUtil()
{
}



//--------------------------- destructor ------------------------------//
//--------------------------- destructor ------------------------------//
//--------------------------- destructor ------------------------------//

MutefileUtil::~MutefileUtil()
{
}
/*****
*****/



//----------------------- add constant -------------------------------//
//----------------------- add constant -------------------------------//
//----------------------- add constant -------------------------------//

         // public static function.


int MutefileUtil::addConstant(const char *input_filename,
                              const char *output_filename,
                              float constant, char *msg)
{
/////////////// create mutefile object and read input file.

  MuteStruct *ss = mutefile_create();
  int error = mutefile_read_file(ss, (char*)input_filename, msg);
  if(error)
      {
      mutefile_destroy(ss);
      return error;
      }

/////////////// add constant to the mute values.

  float nil = mutefile_get_nil();

  int nz = xyzdim_get_nz(ss->xyzdim);
  for(int iz = 0; iz < nz; iz++)
      {
      int ny = xyzdim_get_ny(ss->xyzdim, iz);
      for(int iy = 0; iy < ny; iy++)
          {
          int nx = xyzdim_get_nx(ss->xyzdim, iy, iz);
          for(int ix = 0; ix < nx; ix++)
              {
              if(ss->xyzdim->xydim[iz]->xdim[iy]->varray[ix] != nil)
                 ss->xyzdim->xydim[iz]->xdim[iy]->varray[ix] += constant;
              }
          }
      }

/////////////// save output file and delete mutefile object.

  error = mutefile_save_file(ss, (char*)output_filename, msg);
  mutefile_destroy(ss);
  return error;
}



//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//

