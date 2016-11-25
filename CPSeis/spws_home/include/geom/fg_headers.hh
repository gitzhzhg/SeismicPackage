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

//------------------------ fg_headers.hh ----------------------------//
//------------------------ fg_headers.hh ----------------------------//
//------------------------ fg_headers.hh ----------------------------//

//            header file for the FgHeaders class
//                 not derived from any class
//                    subdirectory geom


//  This class derives CPS header words from the FieldGeometry class.
//  See the implementation file for documentation.


#ifndef _FG_HEADERS_HH_
#define _FG_HEADERS_HH_

#include "cprim.h"
#include "named_constants.h"


class FgHeaders
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class FgTraceValues       *_tv;
  const class GridTransform *_transform;
  double                     _head[CPS_MAX_HEADERS + 1];
  double                     _zero;
  double                     _one;
  int                        _headers_are_nil;

//------------------ functions ----------------------------------//
//------------------ functions ----------------------------------//
//------------------ functions ----------------------------------//

public:      // constructor and destructor.

  FgHeaders(FgTraceValues *tv, const GridTransform *transform);
  virtual ~FgHeaders();

public:   // calculate header words for one trace.
          // returns error flag (TRUE if error, FALSE if no error).
          // itrace = sequential trace number (starting with 1).

  void   startHeadersFromScratch ();
  int    calculateHeaderWords    (long itrace, int more);

public:   // get information calculated above.
          // all but the last must follow call to calculateHeaderWords.
          // ihead = header word number (1-64).

  int         getHeaderErrorFlag         ()           const;
  long        getHeaderTraceNumber       ()           const;
  long        getHeaderSourceLineIndex   ()           const;
  long        getHeaderSourceFlagIndex   ()           const;
  long        getHeaderReceiverLineIndex ()           const;
  long        getHeaderReceiverFlagIndex ()           const;
  double      getHeaderWordValue         (int ihead)  const;
  const char *getHeaderWordDescription   (int ihead)  const;

/*
public:   // this function does not require calculateHeaderWords first.

  double      getHeaderWordValue  (long itrace, int ihead);
*/

private:

  void   setLong   (int i, long   x)  { if(x != INIL) _head[i] = x; }
  void   setFloat  (int i, float  x)  { if(x != FNIL) _head[i] = x; }
  void   setDouble (int i, double x)  { if(x != DNIL) _head[i] = x; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
