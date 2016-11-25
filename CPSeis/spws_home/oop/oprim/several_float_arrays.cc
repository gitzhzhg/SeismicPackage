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


//---------------------- several_float_arrays.cc -------------------------//
//---------------------- several_float_arrays.cc -------------------------//
//---------------------- several_float_arrays.cc -------------------------//

//         implementation file for the SeveralFloatArrays class
//                      not derived from any class
//                         subdirectory oprim


#include "oprim/several_float_arrays.hh"
#include "oprim/active_index_keeper.hh"
#include "oprim/simple_float_array.hh"
#include "oprim/floatio_wrapper.hh"
#include "named_constants.h"
#include "str.h"
#include "swapio.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <ctype.h>


#define  NBUF        222
#define  NUM_CARDS   80

static char *IDENTIFIER = "SeveralFloatArrays";


//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//


SeveralFloatArrays::SeveralFloatArrays(const int ncolumns,
                                       const char *filetype,
                                       const float tolerance)
     :
       _ncolumns   (ncolumns),
       _filetype   (str_newstr(filetype)),
       _rerror     (FALSE),
       _werror     (FALSE),
       _arrays     (NULL),
       _nseg       (0),
       _first      (NULL),
       _active     (NULL)
{
  assert(_ncolumns >= 1);
  assert(filetype);
  assert(strlen(_filetype) > 0);

  _active = new ActiveIndexKeeper();
  _arrays = new SimpleFloatArray * [_ncolumns];
  for(int icol = 0; icol < _ncolumns; icol++)
      {
      _arrays[icol] = new SimpleFloatArray(LONG_MAX, tolerance);
      }
}



//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//


SeveralFloatArrays::~SeveralFloatArrays()
{
  delete _active;
  for(int icol = 0; icol < _ncolumns; icol++)
      {
      delete _arrays[icol];
      }
  delete [] _arrays;
  if(_first) delete [] _first;
}



//-------------------------------- array ------------------------------//
//-------------------------------- array ------------------------------//
//-------------------------------- array ------------------------------//

   // private.

SimpleFloatArray *SeveralFloatArrays::array(int icol)  const
{
  assert(icol >= 0 && icol < _ncolumns);
  return _arrays[icol];
}



//------------------------------ get values -----------------------------//
//------------------------------ get values -----------------------------//
//------------------------------ get values -----------------------------//

   // public.

long SeveralFloatArrays::numElements()  const
{
  long nelements1 = _active ->numElements();
  long nelements2 = array(0)->numElements();
  long nelements3 = array(_ncolumns-1)->numElements();
  assert(nelements1 == nelements2);
  assert(nelements1 == nelements3);
  return nelements1;
}


long SeveralFloatArrays::getActiveIndex()  const
{
  return _active->getActiveIndex();
}


float SeveralFloatArrays::getValue(int icol, long index)  const
{
  return array(icol)->getValue(index);
}


float SeveralFloatArrays::minimumValue(int icol)  const
{
  return array(icol)->minimumValue();
}


float SeveralFloatArrays::maximumValue(int icol)  const
{
  return array(icol)->maximumValue();
}


long SeveralFloatArrays::numNilValues(int icol)  const
{
  return array(icol)->numNilValues();
}


long SeveralFloatArrays::numLiveValues(int icol)  const
{
  return array(icol)->numLiveValues();
}


int SeveralFloatArrays::isAscending(int icol)  const
{
  return array(icol)->isAscending();
}


int SeveralFloatArrays::isDescending(int icol)  const
{
  return array(icol)->isDescending();
}


int SeveralFloatArrays::isEquallySpaced(int icol)  const
{
  return array(icol)->isEquallySpaced();
}



//----------------------------- set values -------------------------------//
//----------------------------- set values -------------------------------//
//----------------------------- set values -------------------------------//

   // public.

void SeveralFloatArrays::setNumElements(long nelements)
{
  _active->resetNumElementsAndClear(nelements);
  for(int icol = 0; icol < _ncolumns; icol++)
      {
      array(icol)->resetNumElementsAndClear(nelements);
      }
  deleteSegments();
}


void SeveralFloatArrays::setActiveIndex(long active)
{
  _active->setActiveIndex(active);
}


void SeveralFloatArrays::setValue(int icol, long index, float value)
{
  _active    ->setValue(index);
  array(icol)->setValue(index, value);
}


void SeveralFloatArrays::setLastValue(int icol, float value)
{
  _active    ->setLastValue();
  array(icol)->setLastValue(value);
}


void SeveralFloatArrays::multiplyByConstant(int icol, float constant)
{
  array(icol)->multiplyByConstant(constant);
}


void SeveralFloatArrays::addConstant(int icol, float constant)
{
  array(icol)->addConstant(constant);
}



//------------------------ set or append one value -----------------------//
//------------------------ set or append one value -----------------------//
//------------------------ set or append one value -----------------------//

    // public.

void SeveralFloatArrays::setOrAppendValue(int icol, long index, float value)
{
  if(index == numElements()) appendNilRow();
  _active    ->setValue(index);
  array(icol)->setValue(index, value);
}



//------------------------ insert or remove one row ----------------------//
//------------------------ insert or remove one row ----------------------//
//------------------------ insert or remove one row ----------------------//

    // public.

void SeveralFloatArrays::appendNilRow ()
{
  insertNilRow(numElements());
}


void SeveralFloatArrays::appendRow    (int icol, float value)
{
  appendNilRow();
  _active    ->setLastValue();
  array(icol)->setLastValue(value);
}


void SeveralFloatArrays::insertNilRow (long index)
{
  _active->insertElement(index);
  for(int icol = 0; icol < _ncolumns; icol++)
      {
      array(icol)->insertNilElement(index);
      }
}


void SeveralFloatArrays::insertRowFromBuffers   (long index)
{
  _active->insertElement(index);
  for(int icol = 0; icol < _ncolumns; icol++)
      {
      array(icol)->insertElementFromBuffer(index);
      }
}


void SeveralFloatArrays::insertRow  (int icol, long index, float value)
{
  insertNilRow(index);
  _active    ->setValue(index);
  array(icol)->setValue(index, value);
}


void SeveralFloatArrays::removeRow    (long index)
{
  _active->removeElement(index);
  for(int icol = 0; icol < _ncolumns; icol++)
      {
      array(icol)->removeElement(index);
      }
}


void SeveralFloatArrays::removeRowToBuffers  (long index)
{
  _active->removeElement(index);
  for(int icol = 0; icol < _ncolumns; icol++)
      {
      array(icol)->removeElementToBuffer(index);
      }
}



//------------------------ get segment-specific values --------------------//
//------------------------ get segment-specific values --------------------//
//------------------------ get segment-specific values --------------------//

     // public.

long  SeveralFloatArrays::numSegments ()   const
{
  if(_nseg == 0 && numElements() > 0) return 1;
  return _nseg;
}


long  SeveralFloatArrays::startingIndexOfSegment (long iseg)   const
{
  if(_nseg <= 1) return 0;
  assert(_first);
  assert(iseg >= 0 && iseg < _nseg);
  return _first[iseg];
}


long  SeveralFloatArrays::numElementsInSegment (long iseg)   const
{
  long nelements = numElements();
  if(_nseg <= 1) return nelements;
  assert(_first);
  assert(iseg >= 0 && iseg < _nseg);
  if(iseg == _nseg - 1) return nelements - _first[iseg];
  return _first[iseg + 1] - _first[iseg];
}



long  SeveralFloatArrays::getSegmentNumber (long index)   const
{
  assert(index >= 0 && index < numElements());
  if(_nseg <= 1) return 1;
  assert(_first);
  for(long iseg = 1; iseg < _nseg; iseg++)
      {
      if(index < _first[iseg]) return iseg;
      }
  return _nseg;
}



float SeveralFloatArrays::getValueInSegment
                               (long iseg, long icol, long index)  const
{
  if(_nseg <= 1) return getValue((int)icol, index);
  assert(_first);
  assert(iseg >= 0 && iseg < _nseg);
  long actual = _first[iseg] + index;
  return getValue((int)icol, actual);
}



//--------------------- set segment-specific values ---------------------//
//--------------------- set segment-specific values ---------------------//
//--------------------- set segment-specific values ---------------------//

   // public.

void SeveralFloatArrays::deleteSegments()
{
  _nseg = 0;
  if(_first) delete [] _first;
  _first = NULL;
}



void SeveralFloatArrays::addSegment (long index)
{
  long nelements = numElements();
  assert(index >= 0 && index < nelements);
  if(index == 0) return;
  assert(nelements > 0);
  if(_first == NULL)
      {
      _nseg = 2;
      _first = new long [_nseg];
      _first[0] = 0;
      _first[1] = index;
      }
  else
      {
      assert(_nseg >= 2);
      _nseg++;
      long *new_first = new long [_nseg];
      memcpy(new_first, _first, (int)(_nseg - 1) * sizeof(long));
      delete [] _first;
      _first = new_first;
      _first[_nseg - 1] = index;
      assert(_first[_nseg - 1] > _first[_nseg - 2]);
      }
}



//----------------------------- validate file -------------------------------//
//----------------------------- validate file -------------------------------//
//----------------------------- validate file -------------------------------//

   // public.

int SeveralFloatArrays::validateFile (FloatioWrapper *floatio,
                                      const char *filename, char *info)
{
////////////////////// validate file:

  assert(floatio && filename && info);

  int error = floatio->validateFile(filename, info);
  if(error) return TRUE;

////////////////////// let the derived class validate the header:

  strcpy(info, "");
  return virtualValidateHeader(floatio, info);
}



//----------------------------- import file -------------------------------//
//----------------------------- import file -------------------------------//
//----------------------------- import file -------------------------------//

   // public.

int SeveralFloatArrays::importFile (FloatioWrapper *floatio,
                                    const char *filename, char *msg)
{
////////////////////// open file:

  assert(floatio && filename && msg);

  int error = floatio->openInputFile(filename, msg);
  if(error)
      {
      _rerror = TRUE;
      return TRUE;
      }

////////////////////// let the derived class validate the header:

  sprintf(msg, "error while importing %s file header", _filetype);
  error = virtualImportHeader(floatio, msg);
  if(error)
      {
      floatio->closeFile();
      _rerror = TRUE;
      return TRUE;
      }

////////////////////// before read columns:

  setNumElements(0);
  error = FALSE;
  float *values = new float [_ncolumns];
  int err;

////////////////////// read columns:

  while(TRUE)
      {
      floatio->readLine(&err, msg, values, _ncolumns);
      if(err == FloatioWrapper::STATUS_EOF)
          {
          break;
          }
      else if(err == FloatioWrapper::STATUS_ERROR)
          {
          error = TRUE;
          break;
          }
      appendNilRow();
      for(int icol = 0; icol < _ncolumns; icol++)
          {
          if(values[icol] != FNIL)
              {
              setLastValue(icol, values[icol]);
              }
          }
      }

////////////////////// after read columns:

  delete [] values;

////////////////////// close file:

  floatio->closeFile();
  if(!error)
      {
      sprintf(msg, "%s file successfully imported (%d data lines)",
                                             _filetype, numElements());
      }
  _rerror = error;
  return error;
}



//----------------------------- export file -------------------------------//
//----------------------------- export file -------------------------------//
//----------------------------- export file -------------------------------//

   // public.

int SeveralFloatArrays::exportFile (FloatioWrapper *floatio,
                                    const char *filename, char *msg)
{
////////////////////// let the derived class validate the header:

  assert(floatio && filename && msg);

  sprintf(msg, "error while exporting header of %s file", _filetype);
  int error = virtualExportHeader(floatio, msg);
  if(error)
      {
      _werror = TRUE;
      return TRUE;
      }

////////////////////// open file:

  error = floatio->openOutputFile(filename, msg);
  if(error)
      {
      _werror = TRUE;
      return TRUE;
      }

////////////////////// before write columns:

  error = FALSE;
  long nelements = numElements();
  float *values = new float [_ncolumns];
  int err;

////////////////////// write columns:

  for(long index = 0; index < nelements; index++)
      {
      for(int icol = 0; icol < _ncolumns; icol++)
          {
          values[icol] = array(icol)->getValue(index);
          }
      floatio->writeLine(&err, msg, values, _ncolumns);
      if(err == FloatioWrapper::STATUS_ERROR)
          {
          error = TRUE;
          break;
          }
      }

////////////////////// after write columns:

  delete [] values;

////////////////////// close file:

  floatio->closeFile();
  if(!error)
      {
      sprintf(msg, "%s file successfully exported", _filetype);
      }
  _werror = error;
  return error;
}


//---------------------------- get export value ----------------------------//
//---------------------------- get export value ----------------------------//
//---------------------------- get export value ----------------------------//


float SeveralFloatArrays::getExportValue (int icol, long index)
{
  return array(icol)->getValue(index);
}


//------------------------- read binary file ------------------------------//
//------------------------- read binary file ------------------------------//
//------------------------- read binary file ------------------------------//

   // public.

int SeveralFloatArrays::readBinaryFile
                          (const char *filename, char *msg, int method)
{
////////////////////// open file:

  assert(filename && msg);
  if(filename[0] == '\0')
      {
      sprintf(msg, "blank filename when trying to read %s file", _filetype);
      _rerror = TRUE;
      return TRUE;
      }
  FILE *stream = fopen(filename, "rb");
  if(stream == NULL)
      {
      sprintf(msg, "open error when trying to read %s file", _filetype);
      _rerror = TRUE;
      return TRUE;
      }

////////////////////// read generic header:

  int swapped = FALSE;
  long num;
  char buffer1[NBUF];
  char buffer2[NBUF];
  int num1 = swapio_fread_char( buffer1, NBUF, stream);
  int num2 = swapio_fread_char( buffer2, NBUF, stream);
  int num3 = swapio_fread_long(&num    , 1   , stream);
  if(num1 != NBUF || num2 != NBUF || num3 != 1)
      {
      sprintf(msg, "error while reading generic header of %s file", _filetype);
      fclose(stream);
      _rerror = TRUE;
      return TRUE;
      }

  if (num < 0 || num > 999999)
      {
      swapio_toggle_read_swapping_action ();
      swapio_do_swap_long (&num, 1);
      swapped = TRUE;
      }

  if(strncmp(buffer1, IDENTIFIER, NBUF) != 0)
      {
      sprintf(msg, "wrong identifier in generic header of %s file", _filetype);
      fclose(stream);
      _rerror = TRUE;
      if(swapped) swapio_toggle_read_swapping_action ();
      return TRUE;
      }
  if(strcmp(buffer2, _filetype) != 0)
      {
      sprintf(msg, "wrong filetype in generic header of %s file", _filetype);
      fclose(stream);
      _rerror = TRUE;
      if(swapped) swapio_toggle_read_swapping_action ();
      return TRUE;
      }
  if(num < 0 || num > 999999)
      {
      sprintf(msg,
         "read error in %s file - might come from different machine",
                                                                  _filetype);
      fclose(stream);
      _rerror = TRUE;
      if(swapped) swapio_toggle_read_swapping_action ();
      return TRUE;
      }
  setNumElements(num);

////////////////////// read segments:

  int num4 = swapio_fread_long(&_nseg, 1, stream);
  if(num4 != 1 || _nseg < 0 || _nseg > num)
      {
      _nseg = 0;
      sprintf(msg, "error while reading #segments from %s file", _filetype);
      fclose(stream);
      _rerror = TRUE;
      if(swapped) swapio_toggle_read_swapping_action ();
      return TRUE;
      }
  if(_nseg >= 2)
      {
      _first = new long [_nseg];
      int num5 = swapio_fread_long(_first, (int)_nseg, stream);
      if(num5 != _nseg)
          {
          delete [] _first;
          _first = NULL;
          _nseg = 0;
          sprintf(msg, "error while reading segments in %s file", _filetype);
          fclose(stream);
          _rerror = TRUE;
          if(swapped) swapio_toggle_read_swapping_action ();
          return TRUE;
          }
      int error = FALSE;
      if(_first[0] != 0) error = TRUE;
      for(long iseg = 1; iseg < _nseg; iseg++)
          {
          if(_first[iseg] <= _first[iseg - 1]) error = TRUE;
          if(_first[iseg] >= num)              error = TRUE;
          }
      if(error)
          {
          delete [] _first;
          _first = NULL;
          _nseg = 0;
          sprintf(msg, "error in segment values in %s file", _filetype);
          fclose(stream);
          _rerror = TRUE;
          if(swapped) swapio_toggle_read_swapping_action ();
          return TRUE;
          }
      }

////////////////////// read special header:

  sprintf(msg, "error while reading special header of %s file", _filetype);
  int error = virtualReadBinaryHeader(stream, msg);
  if(error)
      {
      _rerror = TRUE;
      if(swapped) swapio_toggle_read_swapping_action ();
      return TRUE;
      }

  if(method == SEPARATE)   //////////method/////////////
  {                        //////////method/////////////

////////////////////// read each array:

  for(int icol = 0; icol < _ncolumns; icol++)
      {
      int error = array(icol)->readFromBinaryFile(stream, msg);
      if(error)
          {
          fclose(stream);
          _rerror = TRUE;
          if(swapped) swapio_toggle_read_swapping_action ();
          return TRUE;
          }
      }

  }                        //////////method/////////////
  else                     //////////method/////////////
  {                        //////////method/////////////

////////////////////// read columns:

  for(long index = 0; index < num; index++)
      {
      for(int icol = 0; icol < _ncolumns; icol++)
          {
          float value;
          int num = swapio_fread_float(&value, 1, stream);
          if(num != 1)
              {
              sprintf(msg, "error reading %s value (index %d column %d)",
                                      _filetype, index+1, icol+1);
              fclose(stream);
              _rerror = TRUE;
              if(swapped) swapio_toggle_read_swapping_action ();
              return TRUE;
              }
          array(icol)->setValue(index, value);
          }
      }
  }                        //////////method/////////////

////////////////////// close file:

  fclose(stream);
  sprintf(msg, "%s file successfully read", _filetype);
  _rerror = FALSE;
  if(swapped) swapio_toggle_read_swapping_action ();
  return FALSE;
}



//-------------------------- save binary file ------------------------------//
//-------------------------- save binary file ------------------------------//
//-------------------------- save binary file ------------------------------//

   // public.

int SeveralFloatArrays::saveBinaryFile
                           (const char *filename, char *msg, int method)
{
////////////////////// open file:

  assert(filename && msg);
  if(filename[0] == '\0')
      {
      sprintf(msg, "blank filename when trying to save %s file", _filetype);
      _werror = TRUE;
      return TRUE;
      }
  FILE *stream = fopen(filename, "wb");
  if(stream == NULL)
      {
      sprintf(msg, "open error when trying to save %s file", _filetype);
      _werror = TRUE;
      return TRUE;
      }

////////////////////// write generic header:

  long nelements = numElements();
  char buffer1[NBUF];
  char buffer2[NBUF];
  memset(buffer1, 0, NBUF);
  memset(buffer2, 0, NBUF);
  strcpy(buffer1, IDENTIFIER);
  strcpy(buffer2, _filetype);
  int num1 = swapio_fwrite_char( buffer1  , NBUF, stream);
  int num2 = swapio_fwrite_char( buffer2  , NBUF, stream);
  int num3 = swapio_fwrite_long(&nelements, 1   , stream);
  if(num1 != NBUF || num2 != NBUF || num3 != 1)
      {
      sprintf(msg, "error while writing generic header of %s file", _filetype);
      fclose(stream);
      _werror = TRUE;
      return TRUE;
      }

////////////////////// write segments:

  int num4 = swapio_fwrite_long(&_nseg, 1, stream);
  if(num4 != 1)
      {
      sprintf(msg, "error while writing segments to %s file", _filetype);
      fclose(stream);
      _werror = TRUE;
      return TRUE;
      }
  if(_nseg >= 2)
      {
      int num5 = swapio_fwrite_long(_first, (int)_nseg, stream);
      if(num5 != _nseg)
          {
          sprintf(msg, "error while writing segments to %s file", _filetype);
          fclose(stream);
          _werror = TRUE;
          return TRUE;
          }
      }

////////////////////// write special header:

  sprintf(msg, "error while writing special header of %s file", _filetype);
  int error = virtualSaveBinaryHeader(stream, msg);
  if(error)
      {
      _werror = TRUE;
      return TRUE;
      }

  if(method == SEPARATE)   //////////method/////////////
  {                        //////////method/////////////

////////////////////// write each array:

  for(int icol = 0; icol < _ncolumns; icol++)
      {
      int error = array(icol)->saveToBinaryFile(stream, msg);
      if(error)
          {
          fclose(stream);
          _werror = TRUE;
          return TRUE;
          }
      }

  }                        //////////method/////////////
  else                     //////////method/////////////
  {                        //////////method/////////////

////////////////////// write columns:

  for(long index = 0; index < nelements; index++)
      {
      for(int icol = 0; icol < _ncolumns; icol++)
          {
          float value = array(icol)->getValue(index);
          int num = swapio_fwrite_float(&value, 1, stream);
          if(num != 1)
              {
              sprintf(msg, "error writing %s value (index %d column %d)",
                                      _filetype, index+1, icol+1);
              fclose(stream);
              _werror = TRUE;
              return TRUE;
              }
          }
      }

  }                        //////////method/////////////

////////////////////// close file:

  fclose(stream);
  sprintf(msg, "%s file successfully saved", _filetype);
  _werror = FALSE;
  return FALSE;
}



//---------------------------------- end -----------------------------------//
//---------------------------------- end -----------------------------------//
//---------------------------------- end -----------------------------------//

