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

//---------------------- static_work_io.hh ---------------------//
//---------------------- static_work_io.hh ---------------------//
//---------------------- static_work_io.hh ---------------------//

//              header file for the StaticWorkIO class
//                  not derived from any class
//                      subdirectory stat


       // manages static file I/O for static workfiles.
       // owned by (and called only by) StaticKernal.
       // moves data to and from StatStruct (which is in StaticKernal).


#ifndef _STATIC_WORK_IO_HH_
#define _STATIC_WORK_IO_HH_

#include "cprim.h"
#include <stdio.h>


class StaticWorkIO
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  StatStruct  *_ss;

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor.

           StaticWorkIO (StatStruct *ss);
  virtual ~StaticWorkIO ();

public:     // adds one history card if add is TRUE.

  int  saveFile      (const char *filename, char *msg, int add = 1);
  int  readFile      (const char *filename, char *msg, int add = 1);
  long validateFile  (const char *filename, char *info);

private:

  int privateSaveHeader (FILE *stream, int add);
  int privateSaveCards  (FILE *stream, int add);
  int privateSaveValues (FILE *stream);

  int privateReadHeader (FILE *stream, long *ncards, int *values);
  int privateReadCards  (FILE *stream, long  ncards, int  add);
  int privateReadValues (FILE *stream,               int  values);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
