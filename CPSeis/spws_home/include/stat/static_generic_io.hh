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

//---------------------- static_generic_io.hh ---------------------//
//---------------------- static_generic_io.hh ---------------------//
//---------------------- static_generic_io.hh ---------------------//

//              header file for the StaticGenericIO class
//                  not derived from any class
//                      subdirectory stat


       // manages static file I/O for generic static files.
       // owned by (and called only by) StaticKernal.
       // moves data to and from StatStruct (which is in StaticKernal).


#ifndef _STATIC_GENERIC_IO_HH_
#define _STATIC_GENERIC_IO_HH_

#include "cprim.h"
#include <stdio.h>


class StaticGenericIO
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  StatStruct          *_ss;
  class StaticDecode  *_decode;
  char                *_keepname;

private:  // these are nil if _keepname or howReadGenericFile changes
          // or the file has not been scanned.

  float    _xmin;
  float    _xmax;
  float    _ymin;
  float    _ymax;
  float    _vmin;
  float    _vmax;

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

           StaticGenericIO (StatStruct *ss);
  virtual ~StaticGenericIO ();

  void        howReadGenericFile (const char *how);
  const char *howReadGenericFile ()  const;
  int         badGenericReadCode ()  const;

  int  scanGenericFile (char *msg);
                              // gets bin limits from last file validated.

  float  genericXmin     ()  const  { return _xmin; }
  float  genericXmax     ()  const  { return _xmax; }
  float  genericYmin     ()  const  { return _ymin; }
  float  genericYmax     ()  const  { return _ymax; }
  float  genericVmin     ()  const  { return _vmin; }
  float  genericVmax     ()  const  { return _vmax; }

  int  saveFile      (const char *filename, char *msg);
  int  readFile      (const char *filename, char *msg,
                                StatStruct *other, const char *how);
  long validateFile  (const char *filename, char *info);

private:

  int  supplyRequiredHeaderInfo (StatStruct *other);
  int  privateSaveHeader        (FILE *stream);
  int  privateSaveCards         (FILE *stream);
  int  privateSaveValues        (FILE *stream);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
