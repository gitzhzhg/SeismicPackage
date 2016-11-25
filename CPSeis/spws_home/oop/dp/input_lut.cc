// input_lut.cc:  implementation file for InputLUT object
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

#include "dp/input_lut.hh"
#include "dp/decoder.hh"

InputLUT::InputLUT (Decoder *decoder, int column):
  _decoder             (decoder),
  _tempLUTs            (0),
  _LUTs                (0),
  _indices             (0),
  _bin_count           (0),
  _column              (0),
  _column_size         (0),
  _inserted_lut_count  (0),
  _undefined_method    (MAXIMUM_CODE_VALUE_UNDEFINED),
  _inverseLUT          (0),
  _min_lut_code        (0),
  _max_lut_code        (255),
  _min_byte_value      (0),
  _max_byte_value      (255),
  _not_defined_byte    (255),
  _min_lut_value       (0),
  _max_lut_value       (255),
  _min_float_value     (0),
  _max_float_value     (254),
  _not_defined_float   (-FLT_MAX),
  _gain                (1),
  _bias                (0),
  _error_status        (IL_SUCCESSFUL)
{
  if (!_decoder) {
    _error_status = IL_BAD_INPUTS;
    return;
  }
  _not_defined_byte = _decoder->findMaximum();

  update (column);
}

InputLUT::~InputLUT ()
{
  if (_tempLUTs) delete _tempLUTs; // kill array of pointers
  if (_LUTs) delete _LUTs;  // intended to only kill array of pointers!
  if (_indices) delete [] _indices;
  if (_inverseLUT) delete [] _inverseLUT;
}

// whenever the decoder object changes, this call must to be checked
int InputLUT::update (int column)
{
  _bin_count = _decoder->getBinCount ();
  _column_size = _decoder->getDecoderSize ();
  if (column >= _bin_count || _column_size < 2) {
    _error_status = IL_BAD_INPUTS;
    return 0;
  }

  _column = column;

  establishExtremaBytes ();
  establishExtremaFloats ();

  if (!establishUndefinedCodes (_undefined_method, _not_defined_float))
    return 0;

  return initializeExtrema ();
}

int InputLUT::establishUndefinedCodes (long which_method,
  float not_defined_float)
{
  if (which_method == MINIMUM_CODE_VALUE_UNDEFINED) {
    if (not_defined_float >= _min_float_value &&
        not_defined_float <= _max_float_value   ) {
      _error_status = IL_BAD_INPUTS;
      return 0;
    }
    _not_defined_byte  = _min_byte_value;
    _not_defined_float = not_defined_float;
  }
  else /* if (which_method == MAXIMUM_CODE_VALUE_UNDEFINED) */ {
    if (not_defined_float >= _min_float_value &&
        not_defined_float <= _max_float_value   ) {
      _error_status = IL_BAD_INPUTS;
      return 0;
    }
    _not_defined_byte  = _max_byte_value;
    _not_defined_float = not_defined_float;
  }
  _undefined_method = which_method;
  return 1;
}

int InputLUT::establishTwoCodeValues (unsigned char code0, float value0,
  unsigned char code1, float value1)
{
  if (code0 == code1) {
    _error_status = IL_BAD_INPUTS;
    return 0;
  }

  float gain, bias, float_value0, float_value1;

  gain = (value1 - value0) / (float)(code1 - code0);
  bias = value0 - _gain * (float)code0;

  if (_undefined_method == MINIMUM_CODE_VALUE_UNDEFINED) {
    float_value0 = gain * (float)(_min_byte_value + 1) + bias;
    float_value1 = gain * (float) _max_byte_value      + bias;
  }
  else /* (_undefined_method == MAXIMUM_CODE_VALUE_UNDEFINED) */ {
    float_value0 = gain * (float) _min_byte_value      + bias;
    float_value1 = gain * (float)(_max_byte_value - 1) + bias;
  }

  if (float_value1 < float_value0) {
    _min_float_value = float_value1;
    _max_float_value = float_value0;
  }
  else {
    _min_float_value = float_value0;
    _max_float_value = float_value1;
  }

  if (_not_defined_float >= _min_float_value &&
      _not_defined_float <= _max_float_value   ) {
    _error_status = IL_BAD_INPUTS;
    return 0;
  }

  _gain = gain;
  _bias = bias;
  return 1;
}

unsigned char InputLUT::getCode (int element) const
{
  int k2;
  for (k2 = 0; k2 < _inserted_lut_count; k2++) {
    if (_LUTs[k2]) element = (int)_LUTs[k2]->getCode (element);
  }

  if (element > _column_size)
    element = _column_size;
  else if (element < 0)
    element = 0;

  int index = element * _bin_count + _column;
  return _decoder->element (index);
}

int InputLUT::getElementFromCode (unsigned char code)
{
  if (code > _max_lut_code)
    code = _max_lut_code;
  else if (code < _min_lut_code)
    code = _min_lut_code;

  if (!_inverseLUT) initializeInverseLUT ();
  int element = _inverseLUT[code];

  int k2;
  for (k2 = _inserted_lut_count; k2 > 0; k2--)
    if(_LUTs[k2])element=_LUTs[k2]->getElementFromCode((unsigned char)element);
  return element;
}

int InputLUT::insertLUT (InputLUT *lut, int index)
{
  if (index < 0 || index > _inserted_lut_count) {
    _error_status = IL_BAD_INPUTS;
    return 0;
  }

  if (index == _inserted_lut_count) {
    if (_inserted_lut_count > 0) {
      if (!_LUTs) {
        _error_status = IL_INITIALIZATION_ERROR;
        return 0;
      }
// add one more LUT in
// create a temporary _LUTs array
      _tempLUTs = new InputLUT *[_inserted_lut_count+1];
      if (!_tempLUTs) {
         _error_status = IL_MEMORY_ALLOCATION_ERROR;
         return 0;
      }

// copy current _LUTs array to temporary _LUTs array
      int k2;
      for (k2 = 0; k2 < _inserted_lut_count; k2++)
        _tempLUTs[k2] = _LUTs[k2];
// insert the incoming LUT into the temporary _LUTs array
      _tempLUTs[_inserted_lut_count] = lut;
// delete current _LUTs array
      delete _LUTs;
// rename temporary _LUTs array to _LUTs array
      _LUTs = _tempLUTs;
      _tempLUTs = 0;
    }
    else {
      if (_LUTs) {
        _error_status = IL_INITIALIZATION_ERROR;
        return 0;
      }
// create a new _LUTs array with a single entry
      _LUTs = new InputLUT *[1];
      if (!_LUTs) {
        _error_status = IL_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
      _LUTs[0] = lut;
    }
    _inserted_lut_count++;
  }
  else {
// replace an existing _LUTs entry
    if (!_LUTs) {
      _error_status = IL_INITIALIZATION_ERROR;
      return 0;
    }
    _LUTs[index] = lut;
  }
  return initializeExtrema ();
}

int InputLUT::removeInsertedLUT (int index)
{
  if (index < 0 || index >= _inserted_lut_count) {
    _error_status = IL_BAD_INPUTS;
    return 0;
  }
  _LUTs[index] = 0;
  return initializeExtrema ();
}

int InputLUT::removeAllInsertedLUTs ()
{
  if (_LUTs) delete _LUTs, _LUTs = 0;
  _inserted_lut_count = 0;
  return initializeExtrema ();
}

int InputLUT::failed ()
{
  return (int)(_error_status != IL_SUCCESSFUL);
}

void InputLUT::establishExtremaBytes ()
{
  _min_byte_value = _decoder->findMinimum ();
  _max_byte_value = _decoder->findMaximum ();
}

void InputLUT::establishExtremaFloats ()
{
  if (_undefined_method == MINIMUM_CODE_VALUE_UNDEFINED) {
    _min_float_value = _gain * (float)(_min_byte_value + 1) + _bias;
    _max_float_value = _gain * (float) _max_byte_value      + _bias;
  }
  else /* if (_undefined_method == MAXIMUM_CODE_VALUE_UNDEFINED) */ {
    _min_float_value = _gain * (float) _min_byte_value      + _bias;
    _max_float_value = _gain * (float)(_max_byte_value - 1) + _bias;
  }
}

int InputLUT::initializeExtrema ()
{
  if (!findCodeExtrema()) return 0;
  if (getValue(_min_lut_code) > getValue(_max_lut_code)) {
    _min_lut_value = getValue (_max_lut_code);
    _max_lut_value = getValue (_min_lut_code);
  }
  else {
    _min_lut_value = getValue (_min_lut_code);
    _max_lut_value = getValue (_max_lut_code);
  }

  if (_inverseLUT) delete [] _inverseLUT, _inverseLUT = 0;

  return 1;
}

int InputLUT::findCodeExtrema ()
{
  int k2;
  _min_lut_code = 255;
  _max_lut_code = 0;
  unsigned char code;

  for (k2 = 0; k2 < _column_size; k2++) {
    code = getCode(k2);
    if (code != _not_defined_byte) {
      if (code < _min_lut_code) _min_lut_code = code;
      if (code > _max_lut_code) _max_lut_code = code;
    }
  }
  return (int)(_min_lut_code  <= _max_lut_code);
}

// the inverse LUT stuff only works for monotonically increasing functions
int InputLUT::initializeInverseLUT ()
{
  if (!_inverseLUT) delete [] _inverseLUT, _inverseLUT = 0;

  int num_of_indices = _max_lut_code + 1;
  if (num_of_indices < 1) return 0;

  _indices = new int[num_of_indices];
  if (!_indices) {
    _error_status = IL_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  int num_of_codes = _column_size;
  int code;
// initialize indices array to all -1's
  for (code = 0; code < num_of_indices; code++)
    _indices[code] = -1;

  int index, gcode, current_index = 0;
// in the indices array, place as many codes as possible, these are
//   invertible points
  for (code = 0; code < num_of_indices; code++) {
    gcode = -1;
    for (index = current_index; index < num_of_codes && gcode < code; index++){
      gcode = (int) getCode (index);
    }
    if (gcode == code) {
      _indices[code] = --index;
      current_index = index;
    }
    else
      current_index = --index;
  }

// where code indices are undefined (-1's), interpolate
  int first_di, second_di, uic;
  float gain, bias;

  index = 0;
  code = 0;

// loop throught the indices array
  while (code < num_of_indices) {

// look for an occurence of an undefined index and note the corresponding code
    for (uic = code; uic < num_of_indices && _indices[uic] != -1; uic++) {}

// did the end of loop occur?
    if (uic < num_of_indices) {

// for the code associated with the undefined index, find out which two codes
//   encompass it and also not these two codes' corresponding indices.
      for (second_di = index;    second_di < num_of_codes   &&
                                 (int)getCode(second_di) < uic; second_di++) {}
      for (first_di = second_di; first_di > -1              &&
                                 (int)getCode(first_di)  > uic; first_di-- ) {}

// set up gain and bias to interpolate the undefined index
      if (first_di < 0)
        gain = 0;
      else
        gain = (float)(second_di - first_di)
             / (float)(getCode(second_di) - getCode(first_di));

      bias = (float)first_di - gain * (float)getCode(first_di) + 0.5;

      _indices[uic] = (int)(gain * (float)uic + bias);

// set up to resume the loop where it left off
      code = uic + 1;
      index = first_di;
    }
    else
// did not find an occurence of a -1 before getting to end of loop; quit loop
      code = uic;
  }

  _inverseLUT = new unsigned char[num_of_indices];
  if (!_inverseLUT) {
    _error_status = IL_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  for (code = 0; code < num_of_indices; code++)
    _inverseLUT[code] = (unsigned char) _indices[code];

  delete [] _indices, _indices = 0;

  return 1;
}
