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

//------------------------ fg_traces.hh ---------------------//
//------------------------ fg_traces.hh ---------------------//
//------------------------ fg_traces.hh ---------------------//

//             header file for the FgTraces class
//                 not derived from any class
//                     subdirectory geom


    // This class contains an array of seismic traces.


#ifndef _FG_TRACES_HH_
#define _FG_TRACES_HH_

class FgInformer;
class FgGroup;
class FieldFlag;

class FgTraces
{
  friend class FgTraceValues;
  friend class Midpoints;
  friend class FgTeData;	/* ehs */

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  FgInformer *_informer;
  long        _ntraces;  // number of traces in the survey.
  FgGroup   **_grps;     // pointers to  source           for each trace.
  FieldFlag **_rflags;   // pointers to receiver          for each trace.
  long       *_xmids;    // array of X midpoint locations for each trace.
  long       *_ymids;    // array of Y midpoint locations for each trace.
  long       *_offsets;  // array of offsets              for each trace.
  char       *_dead;     // dead trace codes              for each trace.
  float       _offmin;   // minimum offset.
  float       _offmax;   // maximum offset.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  FgTraces (FgInformer *informer);
  virtual ~FgTraces();

  long   numTraces                ()  const  { return _ntraces; }
  float  getMinimumOffset         ()  const  { return _offmin; }
  float  getMaximumOffset         ()  const  { return _offmax; }

  void  clearTraces               ();  // deallocates all arrays.
  void  clearCoords               ();  // deallocates _xmids _ymids _offsets.
  void  clearDeadTraceCodes       ();  // deallocates _dead.

  void  createTraces  (long ntraces);  // allocates _grps and _rflags.
  void  createCoords              ();  // allocates _xmids _ymids _offsets.
  void  createDeadTraceCodes      ();  // allocates _dead.
  void  findOffsetRange           ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
