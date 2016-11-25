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

//---------------------- binary_file.cc -----------------------//
//---------------------- binary_file.cc -----------------------//
//---------------------- binary_file.cc -----------------------//

//         implementation file for the BinaryFile class
//                 not derived from any class
//                     subdirectory oprim

#include "oprim/binary_file.hh"
#include "named_constants.h"
#include "swap.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//

BinaryFile::BinaryFile (const char *filename, const char *inout)
           :
             _filename  (NULL),
             _stream    (NULL),
             _swap      (FALSE),   // unused when writing file.
             _fix       (FALSE),   // unused when writing file.
             _msg       (NULL)
{
  assert(sizeof(char)    == 1);
  assert(sizeof(short)   == 2);
  assert(sizeof(int)     == 4);
  assert(sizeof(long)    == 4 || sizeof(long) == 8);
  assert(sizeof(int32_t) == 4);
  assert(sizeof(int64_t) == 8);
  assert(sizeof(float)   == 4);
  assert(sizeof(double)  == 8);
  assert(inout);
  assert(strcmp(inout, "input") == 0 || strcmp(inout, "output") == 0);

                 ///////////////////

  if(filename == NULL || filename[0] == '\0' || strcmp(filename, "None") == 0
                                             || strcmp(filename, "NONE") == 0
                                             || strcmp(filename, "none") == 0)
      {
      _msg = new char [33];
      sprintf(_msg, "unspecified %s filename", inout);
      return;
      }

  _filename = new char [strlen(filename) + 1];
  _msg      = new char [strlen(filename) + 99];
  strcpy(_filename, filename);
  sprintf(_msg, "%s file %s open error", inout, _filename);  // preset message.

                 ///////////////////

  if(strcmp(inout, "input") == 0)
      {
      _stream = fopen(_filename, "r");

      char header[11];
      int swap_test;
      int long_size;

      privateRead (header,     sizeof(char), 11, "header");
      privateRead (&swap_test, sizeof(int),   1, "swap_test"); // no byte swapping here.
      _swap = (swap_test != 1);
      privateRead (&long_size, sizeof(int),   1, "long_size"); // possible byte swapping here.
      _fix = (long_size != sizeof(long));
      }

                 ///////////////////

  else
      {
      _stream = fopen(_filename, "w");

      int swap_test = 1;
      int long_size = sizeof(long);

      privateWrite ("BinaryFile", sizeof(char), 11, "header");
      privateWrite (&swap_test,   sizeof(int),   1, "swap_test");
      privateWrite (&long_size,   sizeof(int),   1, "long_size");
      }

                 ///////////////////

  if(_stream) sprintf(_msg, "%s file %s opened", inout, _filename);
}

//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

BinaryFile::~BinaryFile()
{
  if(_stream)   fclose(_stream);
  if(_filename) delete [] _filename;
  if(_msg)      delete [] _msg;
}

//------------------------ private read --------------------------//
//------------------------ private read --------------------------//
//------------------------ private read --------------------------//

void BinaryFile::privateRead(void *values, int nbytes, int nelements, const char *ident)
{
  if(nbytes == 0 || nelements == 0) return;
  if(!_stream)
      {
      memset(values, 0, nbytes * nelements);
      return;
      }
  int nreturn = fread(values, nbytes, nelements, _stream);
  if(feof(_stream))
      {
      sprintf(_msg, "file %s EOF while reading %d %s", _filename, nelements, ident);
      fclose(_stream);
      _stream = NULL;
      memset(values, 0, nbytes * nelements);
      }
  else if(ferror(_stream))
      {
      sprintf(_msg, "file %s error while reading %d %s", _filename, nelements, ident);
      fclose(_stream);
      _stream = NULL;
      memset(values, 0, nbytes * nelements);
      }
  else if(nreturn != nelements)
      {
      sprintf(_msg, "file %s read %d %s (instead of %d)", _filename, nreturn, ident, nelements);
      fclose(_stream);
      _stream = NULL;
      memset(values, 0, nbytes * nelements);
      }
  else if(_swap && (nbytes == 2 || nbytes == 4 || nbytes == 8))
      {
      swap_unk_cvec(values, &nbytes, &nelements);
      }
}

//------------------------ private write --------------------------//
//------------------------ private write --------------------------//
//------------------------ private write --------------------------//

void BinaryFile::privateWrite(const void *values, int nbytes, int nelements, const char *ident)
{
  if(!_stream || nbytes == 0 || nelements == 0) return;
  int nreturn = fwrite(values, nbytes, nelements, _stream);
  if(ferror(_stream))
      {
      sprintf(_msg, "file %s error while writing %d %s", _filename, nelements, ident);
      fclose(_stream);
      _stream = NULL;
      }
  else if(nreturn != nelements)
      {
      sprintf(_msg, "file %s wrote %d %s (instead of %d)", _filename, nreturn, ident, nelements);
      fclose(_stream);
      _stream = NULL;
      }
}

//---------------------- read value -------------------------//
//---------------------- read value -------------------------//
//---------------------- read value -------------------------//

char BinaryFile::readChar()
{
  char value;
  privateRead(&value, sizeof(char), 1, "char");
  return value;
}


short BinaryFile::readShort()
{
  short value;
  privateRead(&value, sizeof(short), 1, "short");
  return value;
}


int BinaryFile::readInt()
{
  int value;
  privateRead(&value, sizeof(int), 1, "int");
  return value;
}


long BinaryFile::readLong()
{
  if(_fix && sizeof(long) == 4)   // reading 64bits on 32bit machine.
      {
      int64_t value;
      privateRead(&value, sizeof(int64_t), 1, "int64");
      return (long)value;
      }

  if(_fix && sizeof(long) == 8)   // reading 32bits on 64bit machine.
      {
      int32_t value;
      privateRead(&value, sizeof(int32_t), 1, "int32");
      return (long)value;
      }

  long value;
  privateRead(&value, sizeof(long), 1, "long");
  return value;
}


float BinaryFile::readFloat()
{
  float value;
  privateRead(&value, sizeof(float), 1, "float");
  return value;
}


double BinaryFile::readDouble()
{
  double value;
  privateRead(&value, sizeof(double), 1, "double");
  return value;
}

//---------------------- read string -------------------------//
//---------------------- read string -------------------------//
//---------------------- read string -------------------------//

void BinaryFile::readString(char *string, int nchars)
{
  privateRead(string, sizeof(char), nchars, "string");
}

//---------------------- read array -------------------------//
//---------------------- read array -------------------------//
//---------------------- read array -------------------------//

void BinaryFile::readCharArray(char *array, int nelements)
{
  privateRead(array, sizeof(char), nelements, "chars");
}


void BinaryFile::readShortArray(short *array, int nelements)
{
  privateRead(array, sizeof(short), nelements, "shorts");
}


void BinaryFile::readIntArray(int *array, int nelements)
{
  privateRead(array, sizeof(int), nelements, "ints");
}


void BinaryFile::readLongArray(long *array, int nelements)
{
  if(_fix && sizeof(long) == 4)   // reading 64bits on 32bit machine.
      {
      int64_t *temparray = new int64_t [nelements];
      privateRead(temparray, sizeof(int64_t), nelements, "int64s");
      for(int i = 0; i < nelements; i++) { array[i] = (long)temparray[i]; }
      delete [] temparray;
      return;
      }

  if(_fix && sizeof(long) == 8)   // reading 32bits on 64bit machine.
      {
      int32_t *temparray = new int32_t [nelements];
      privateRead(temparray, sizeof(int32_t), nelements, "int32s");
      for(int i = 0; i < nelements; i++) { array[i] = (long)temparray[i]; }
      delete [] temparray;
      return;
      }

  privateRead(array, sizeof(long), nelements, "longs");
}


void BinaryFile::readFloatArray(float *array, int nelements)
{
  privateRead(array, sizeof(float), nelements, "floats");
}


void BinaryFile::readDoubleArray(double *array, int nelements)
{
  privateRead(array, sizeof(double), nelements, "doubles");
}

//---------------------- write value -------------------------//
//---------------------- write value -------------------------//
//---------------------- write value -------------------------//

void BinaryFile::writeChar(char value)
{
  privateWrite(&value, sizeof(char), 1, "char");
}


void BinaryFile::writeShort(short value)
{
  privateWrite(&value, sizeof(short), 1, "short");
}


void BinaryFile::writeInt(int value)
{
  privateWrite(&value, sizeof(int), 1, "int");
}


void BinaryFile::writeLong(long value)
{
  privateWrite(&value, sizeof(long), 1, "long");
}


void BinaryFile::writeFloat(float value)
{
  privateWrite(&value, sizeof(float), 1, "float");
}


void BinaryFile::writeDouble(double value)
{
  privateWrite(&value, sizeof(double), 1, "double");
}

//---------------------- write string -------------------------//
//---------------------- write string -------------------------//
//---------------------- write string -------------------------//

void BinaryFile::writeString(const char *string, int nchars)
{
  privateWrite(string, sizeof(char), nchars, "string");
}

//---------------------- write array -------------------------//
//---------------------- write array -------------------------//
//---------------------- write array -------------------------//

void BinaryFile::writeCharArray(const char *array, int nelements)
{
  privateWrite(array, sizeof(char), nelements, "chars");
}


void BinaryFile::writeShortArray(const short *array, int nelements)
{
  privateWrite(array, sizeof(short), nelements, "shorts");
}


void BinaryFile::writeIntArray(const int *array, int nelements)
{
  privateWrite(array, sizeof(int), nelements, "ints");
}


void BinaryFile::writeLongArray(const long *array, int nelements)
{
  privateWrite(array, sizeof(long), nelements, "longs");
}


void BinaryFile::writeFloatArray(const float *array, int nelements)
{
  privateWrite(array, sizeof(float), nelements, "floats");
}


void BinaryFile::writeDoubleArray(const double *array, int nelements)
{
  privateWrite(array, sizeof(double), nelements, "doubles");
}

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
