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

//-------------------------- vf_constants.hh -------------------------//
//-------------------------- vf_constants.hh -------------------------//
//-------------------------- vf_constants.hh -------------------------//

//                          subdirectory vf

//  This header file contains constants which are needed by several
//  classes related to velocity analysis.  The constants are isolated
//  here, rather than being part of a class, in order to minimize
//  the necessity to include class header files when the only
//  requirement would be to gain access to these constants.


#ifndef _VF_CONSTANTS_HH_
#define _VF_CONSTANTS_HH_


//----------------------- miscellaneous ----------------------------//
//----------------------- miscellaneous ----------------------------//
//----------------------- miscellaneous ----------------------------//


enum { MAXPICKS = 10000, MAXVELPICKS = 200 };
           // maximum allowed number of velocity function picks.
           // (CPS constraint)


//----------------- velocity function types ---------------------//
//----------------- velocity function types ---------------------//
//----------------- velocity function types ---------------------//


                     //     ordinate             abscissa
                     //     --------             --------
enum { VTNM =  0,        // NMO velocity      vs 2-way time
       VTRM =  1,        // RMS velocity      vs 2-way time
       VZRM =  2,        // RMS velocity      vs depth
       VLRM =  3,        // RMS velocity      vs layer thickness
       VTAV =  4,        // average velocity  vs 2-way time
       VZAV =  5,        // average velocity  vs depth
       VLAV =  6,        // average velocity  vs layer thickness
       VTIN =  7,        // interval velocity vs 2-way time
       VZIN =  8,        // interval velocity vs depth
       VLIN =  9,        // interval velocity vs layer thickness
       VTDP = 10         // depth             vs 2-way time
     };

enum { FIRSTTYPE = 0, LASTTYPE = 10, NUMTYPES = 11 };


//------------------- selection flags ---------------------------//
//------------------- selection flags ---------------------------//
//------------------- selection flags ---------------------------//


enum { SELECT_MAYBE  = ' ',     // unspecified selection (depends on
                                //   whether flanked by SELECT_TOP and
                                //   SELECT_BOTTOM).
       SELECT_YES    = 'Y',     // yes definitely selected.
       SELECT_NO     = 'n',     // no definitely not selected.
       SELECT_TOP    = 'T',     // top of selected range.
       SELECT_BOTTOM = 'B'      // bottom of selected range.
     };


//----------------------- error flags ---------------------------//
//----------------------- error flags ---------------------------//
//----------------------- error flags ---------------------------//


enum { ERROR_NONE  =  ' ',   // velocity function has no errors.
       ERROR_ZERO  =  'N',   // velocity function has zero or one pick.
       ERROR_NILS  =  'E'    // an error has occurred (some picks are nil).
     };


//--------------------- raytrace flags ---------------------------//
//--------------------- raytrace flags ---------------------------//
//--------------------- raytrace flags ---------------------------//


enum { RAYTRACE_NO       = ' ',   // type VNMO and VRMS are the same
                                  //   (raytracing is not done).
       RAYTRACE_FORWARD  = 'R',   // type VNMO was obtained by raytracing
                                  //   the velocity model.
       RAYTRACE_INVERSE  = 'I'    // the velocity model was obtained
                                  //   from VNMO by inverse raytracing.
     };


//------------- flag when reading velocity file from disk -----------//
//------------- flag when reading velocity file from disk -----------//
//------------- flag when reading velocity file from disk -----------//


enum { READ_REPLACE   = 1, // replace all velocity functions in memory
                           //  (delete all existing functions first).
       READ_ADD       = 2, // add to velocity functions in memory
                           //  (keep all existing functions).
       READ_NEW       = 3, // put data into new comparison dataset
                           //  (create comparison dataset first).
       READ_NOTHING   = 4  // do not read the file.
     };


//------------- flag when saving velocity file to disk -------------//
//------------- flag when saving velocity file to disk -------------//
//------------- flag when saving velocity file to disk -------------//


enum { SAVE_SELECTED   = 1,  // save selected velocity functions.
       SAVE_ALL        = 2,  // save all velocity functions.
       SAVE_ACTIVE     = 3   // save active velocity function.
     };


//------------------------- units ----------------------------------//
//------------------------- units ----------------------------------//
//------------------------- units ----------------------------------//

enum { FEET_PER_SECOND   = 1,  // velocity units (depth units are feet).
       METERS_PER_SECOND = 2,  // velocity units (depth units are meters).
       UNSPECIFIED_UNITS = 3   // velocity units are not specified.
     };

                 // time units are always seconds.


//---------------------- end of constants --------------------------//
//---------------------- end of constants --------------------------//
//---------------------- end of constants --------------------------//

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//

