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

//------------------------ binary_file.hh ---------------------//
//------------------------ binary_file.hh ---------------------//
//------------------------ binary_file.hh ---------------------//

//            header file for the BinaryFile class
//                 not derived from any class
//                    subdirectory oprim

// Class for binary file I/O.
// Can be used as a base class or as is.
// The file is opened in the constructor.
// The file is closed in the destructor.

// The I/O will stop, and the file will be closed, after an error occurs,
// but read and write functions can still be called until a convenient time
// to check for an error.  If a previous write error has occurred, a write
// function will do nothing.  If a read error (or EOF) occurs, or a
// previous read error (or EOF) occurred, a zero (or an empty string
// or an array filled with zeroes) will be returned.

// The length of a numeric array (nelements) or a string array (nchars)
// can be zero, in which case nothing is read or written.

// The length of a string array (nchars) is the number of characters read
// or written, and must be long enough to include the null termination
// character, but not longer than the memory which contains the string.
// The actual length of the string (up to the null termination) is not
// relevant.

// Files written on big-endian machines can be read on little-endian machines
// (and vice versa).  Also, files written on 64-bit machines can be read
// on 32-bit machines (and vice versa) even if long variable types (which
// are different on the two machine types) are written to the file.

// This class always writes the file without byte swapping, which is more
// efficient when the file is usually written and read on the same type of
// machine.  This class writes the integer 1 near the beginning of the file
// so that it can determine later whether to byte-swap while reading the file.
// This class also saves sizeof(long) near the beginning of the file so that
// a subsequent read can handle long values correctly in case the file was
// written with 32-bit longs and read on a 64-bit machine or vice versa.

#ifndef _BINARY_FILE_HH_
#define _BINARY_FILE_HH_

#include <stdio.h>

class BinaryFile
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  char  *_filename;
  FILE  *_stream;    // NULL if an error occurred.
  int    _swap;      // whether to swap bytes when reading the file.
  int    _fix;       // whether to fix long types when reading the file.
  char  *_msg;       // message corresponding to success or error.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:  // inout must be "input" or "output".

  BinaryFile (const char *filename, const char *inout);

  virtual ~BinaryFile();

  int         err ()  const  { return !_stream; }
  const char *msg ()  const  { return _msg; }

  char   readChar         ();
  short  readShort        ();
  int    readInt          ();
  long   readLong         ();
  float  readFloat        ();
  double readDouble       ();
  void   readString       (char  *string, int nchars);
  void   readCharArray    (char   *array, int nelements);
  void   readShortArray   (short  *array, int nelements);
  void   readIntArray     (int    *array, int nelements);
  void   readLongArray    (long   *array, int nelements);
  void   readFloatArray   (float  *array, int nelements);
  void   readDoubleArray  (double *array, int nelements);

  void   writeChar        (char   value);
  void   writeShort       (short  value);
  void   writeInt         (int    value);
  void   writeLong        (long   value);
  void   writeFloat       (float  value);
  void   writeDouble      (double value);
  void   writeString      (const char  *string, int nchars);
  void   writeCharArray   (const char   *array, int nelements);
  void   writeShortArray  (const short  *array, int nelements);
  void   writeIntArray    (const int    *array, int nelements);
  void   writeLongArray   (const long   *array, int nelements);
  void   writeFloatArray  (const float  *array, int nelements);
  void   writeDoubleArray (const double *array, int nelements);

private:

  void   privateRead  (void       *values, int nbytes, int nelements, const char *ident);
  void   privateWrite (const void *values, int nbytes, int nelements, const char *ident);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
