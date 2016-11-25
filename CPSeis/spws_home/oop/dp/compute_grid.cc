// Base class for manipulating grids
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

#include "dp/compute_grid.hh"
#include "dp/uchar_grid.hh"
#include "dp/multi_attributes.hh"
#include "dp/input_lut.hh"
#include "dp/output_lut.hh"
#include "dp/uchar_grid_accessor.hh"
#include "dp/float_to_uchar.hh"
#include "dp/float_grid.hh"
#include "dp/float_grid_accessor.hh"
#include "dp/compute_grid_constants.hh"
#include <float.h>

#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))

ComputeGrid::ComputeGrid (int number_of_inputs):
  _number_of_inputs        (number_of_inputs),
  _which_result            (DO_NOTHING),
  _compressed_inputs_flag  (DONT_COMPRESS_INPUTS),
  _rescaled_result_flag    (DONT_RESCALE_RESULT),
  _input                   (0),
  _inp                     (0),
  _res                     (0),
  _inps                    (0),
  _ftuc                    (0),
  _ucg                     (0),
  _ma                      (0),
  _LUTs                    (0),
  _result_LUT              (0),
  _result_out_of_date      (0),
  _inputs_out_of_date      (0),
  _compression_out_of_date (0),
  _percent_of_B            (50),
  _error_status            (CG_SUCCESSFUL)
{
}

ComputeGrid::~ComputeGrid ()
{
  if (_input)  deleteInput ();
  if (_inp)    delete _inp;
  if (_res)    delete _res;
  if (_inps)   delete _inps;
  if (_ftuc)   delete _ftuc;

  deleteLUTObjects ();
}

void ComputeGrid::deleteLUTObjects ()
{
  if (_ucg)         delete _ucg,        _ucg        = 0;
  if (_ma)          delete _ma,         _ma         = 0;
  if (_result_LUT)  delete _result_LUT, _result_LUT = 0;
  if (_LUTs) {
    int k2;
    for (k2 = 0; k2 < _number_of_inputs; k2++) delete _LUTs[k2];
    delete _LUTs, _LUTs = 0;
  }
}

int ComputeGrid::compressionStateIsSet ()
{
  return (int)(_compressed_inputs_flag == COMPRESS_INPUTS);
}

int ComputeGrid::enoughGridsAreValid ()
{
  int retval = 1;
  int k2;
  for (k2 = 0; k2 < _number_of_inputs && retval; k2++)
    retval = (int)(retval && getInputFloatGrid(k2));
  return retval;
}

int ComputeGrid::resultIsDisplayed ()
{
  return (int)(_which_result != DO_NOTHING);
}

int ComputeGrid::rescaleResultStateIsSet ()
{
  return (int)(_rescaled_result_flag == RESCALE_RESULT);
}

int ComputeGrid::setCompressionState (long compressed_inputs_flag)
{
// determine if a transition is occurring
  int stays_on  = (int)(_compressed_inputs_flag == COMPRESS_INPUTS      &&
                         compressed_inputs_flag == COMPRESS_INPUTS        );
  int stays_off = (int)(_compressed_inputs_flag == DONT_COMPRESS_INPUTS &&
                         compressed_inputs_flag == DONT_COMPRESS_INPUTS   );
  _compressed_inputs_flag = compressed_inputs_flag;

  FloatGrid *result = getResultFloatGrid ();
  if (compressed_inputs_flag == COMPRESS_INPUTS) {
    if (stays_on && !_compression_out_of_date)
      return 1;
    if (_ma && result && _result_LUT && !_compression_out_of_date) {
// copy multi-attribute resultant array to the result array
      if (!_ma->initialize()) {
        _error_status = _ma->errorStatus ();
        return 0;
      }
      if (resultIsDisplayed())
        if (!byLUT(_which_result)) {
          return 0;
	}
    }
    else {
      if (!enoughGridsAreValid()) {
        return 0;
      }
      else if (result) {

// reinitialize the resultant grid
        if (!initializeResultantGrid()) return 0;

// create a local unsigned char grid big enough to hold the necessary
//   inputs, each at the size of the result
        deleteLUTObjects ();
        _ucg = new UCharGrid (result->getNumYBins(), _number_of_inputs,
          result->getNumXBins(), result->getDoAbort());
        if (!_ucg) {
          _error_status = CG_MEMORY_ALLOCATION_ERROR;
          return 0;
        }
        if (_ucg->failed()) {
          _error_status = _ucg->errorStatus ();
          return 0;
        }
// move the inputs into the unsigned char grid
        if (!moveInputsToCompress ()) {
          return 0;
	}
// create a multi-attribute/decoder object and initialize, the result array
//   will hold the encoded data
        _ma = new MultiAttributes (_ucg, getNumColors(), result);
        if (!_ma) {
          _error_status = CG_MEMORY_ALLOCATION_ERROR;
          return 0;
        }
        if (_ma->failed()) {
          _error_status = _ma->errorStatus ();
          return 0;
        }
        if (!_ma->initialize()) {
          _error_status = _ma->errorStatus ();
          return 0;
        }
// using this object:
// create the necessary number of input LUT objects for manipulation
        int k2;
        if (_number_of_inputs > 0) _LUTs = new InputLUT *[_number_of_inputs];
        float amin, amax;
        for (k2 = 0; k2 < _number_of_inputs; k2++) {
          _LUTs[k2] = new InputLUT ((Decoder *)_ma, k2);
          if (!_LUTs[k2]) {
            _error_status = CG_MEMORY_ALLOCATION_ERROR;
            return 0;
          }
          if (_LUTs[k2]->failed()) {
            _error_status = _LUTs[k2]->errorStatus ();
            return 0;
	  }
//        amin = getInputFloatGrid(k2)->findStatisticalMinimum (2.5);
//        amax = getInputFloatGrid(k2)->findStatisticalMaximum (2.5);
          amax = getInputFloatGrid(k2)->findMaximum ();
          amin = getInputFloatGrid(k2)->findMinimum ();
          if (!_LUTs[k2]->establishTwoCodeValues ((unsigned char)0, amin,
            (unsigned char)254, amax)) {
            _error_status = _LUTs[k2]->errorStatus ();
            return 0;
          }
        }
// create an output LUT object for showing the results
        _result_LUT = new OutputLUT (_ma->getDecoderSize());
        if (!_result_LUT) {
          _error_status = CG_MEMORY_ALLOCATION_ERROR;
          return 0;
        }
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }

        if (resultIsDisplayed()) {
          if (!byLUT(_which_result)) {
            return 0;
	  }
	}
        _compression_out_of_date = 0;
      }
    }
  }
  else /* if (compressed_inputs_flag == DONT_COMPRESS_INPUTS) */ {
    if (stays_off && !_inputs_out_of_date)
      return 1;
    else if (result) {
      if (!byGrid(_which_result)) {
        return 0;
      }
    }
  }
  _inputs_out_of_date = 0;
  _result_out_of_date = 0;
  return 1;
}

int ComputeGrid::setResultState (long which_result)
{
  _result_out_of_date = which_result != _which_result;
  if (!_result_out_of_date) return 1;
  _which_result = which_result;
  if (!getResultFloatGrid()) return 0;
  else {
    if (compressionStateIsSet()) {
      if (!byLUT(which_result)) return 0;
    }
    else {
      if (_inputs_out_of_date) {
        if (!setCompressionState(_compressed_inputs_flag)) return 0;
      }
      else if (!byGrid(which_result)) return 0;
    }
  }
  return 1;
}

int ComputeGrid::changeProportion () {
  if (!getResultFloatGrid()) return 0;
  else {
    if (compressionStateIsSet()) {
      if (!byLUT(_which_result)) return 0;
    }
    else {
      if (_inputs_out_of_date) {
        if (!setCompressionState(_compressed_inputs_flag)) return 0;
      }
      else if (!byGrid(_which_result)) return 0;
    }
  }
  return 1;
}

// perform the requested result using the multi-attribute decoder
int ComputeGrid::byLUT (long which_result)
{
// set the undefined value to be a large negative number
  if (!_result_LUT->setRange(-FLT_MAX,min((float)0,_result_LUT->getMinimum()),
    max((float)0,_result_LUT->getMaximum()))) {
    _error_status = _result_LUT->errorStatus ();
    return 0;
  }

// display the decoding LUT
  if (!_result_LUT->fill((float)0)) {
    _error_status = _result_LUT->errorStatus ();
    return 0;
  }

  float factor;
  switch (which_result) {
    case DO_NOTHING:
      break;
    case A_ONLY:
      *_result_LUT += _LUTs[0];
      if (_result_LUT->failed()) {
        _error_status = _result_LUT->errorStatus ();
        return 0;
      }
      break;
    case B_ONLY:
      *_result_LUT += _LUTs[1];
      if (_result_LUT->failed()) {
        _error_status = _result_LUT->errorStatus ();
        return 0;
      }
      break;
    case A_MINUS_B:
       if (_percent_of_B <= 99) {
        *_result_LUT += _LUTs[0];
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B != 50 && _percent_of_B >= 1 && _percent_of_B <= 99) {
        factor = (100 - _percent_of_B) / _percent_of_B;
        *_result_LUT *= factor;
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B >= 1) {
        *_result_LUT -= _LUTs[1];
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B < 50 && _percent_of_B >= 1 && _percent_of_B <= 99) {
        *_result_LUT /= factor;
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      break;
    case A_PLUS_B:
      if (_percent_of_B <= 99) {
        *_result_LUT += _LUTs[0];
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B != 50 && _percent_of_B >= 1 && _percent_of_B <= 99) {
        factor = (100 - _percent_of_B) / _percent_of_B;
        *_result_LUT *= factor;
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B >= 1) {
        *_result_LUT += _LUTs[1];
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B < 50 && _percent_of_B >= 1 && _percent_of_B <= 99) {
        *_result_LUT /= factor;
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      break;
    case A_DIVIDED_BY_B:
      if (_percent_of_B <= 0) {
        if (!_result_LUT->fill(_result_LUT->getUndefined())) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      else if (_percent_of_B <= 99) {
        *_result_LUT += _LUTs[0];
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
        if (_percent_of_B != 50) {
          factor = (100 - _percent_of_B) / _percent_of_B;
          *_result_LUT *= factor;
          if (_result_LUT->failed()) {
            _error_status = _result_LUT->errorStatus ();
            return 0;
          }
        }
        *_result_LUT /= _LUTs[1];
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      break;
    case A_TIMES_B:
      if (_percent_of_B >= 1 && _percent_of_B <= 99) {
        *_result_LUT += _LUTs[0];
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
        if (_percent_of_B != 50) {
          factor = (100 - _percent_of_B) * _percent_of_B / 2500;
          *_result_LUT *= factor;
          if (_result_LUT->failed()) {
            _error_status = _result_LUT->errorStatus ();
            return 0;
          }
        }
        *_result_LUT -= _LUTs[1];
        if (_result_LUT->failed()) {
          _error_status = _result_LUT->errorStatus ();
          return 0;
        }
      }
      break;
    default:
      return 0;
  }
  if (rescaleResultStateIsSet())
    if (!rescaleLUT()) return 0;
  if (!displayLUT()) return 0;  // pure virtual
  return 1;
}

// perform the requested result on the inputs using the resultant grid that has
//   already been allocated and prepared for display
int ComputeGrid::byGrid (long which_result)
{
// reinitialize the resultant grid
  if (!initializeResultantGrid()) return 0;

  FloatGrid *result = getResultFloatGrid ();

  float factor;
  switch (which_result) {
    case DO_NOTHING:
      break;
    case A_ONLY:
      _input = prepareInput (0);
      if (!_input) return 0;
      result = prepareResult (0);
      if (!result) return 0;
      *result += *_input;
      if (result->failed()) {
        _error_status = result->errorStatus ();
        return 0;
      }
      resetInput (0);
      break;
    case B_ONLY:
      _input = prepareInput (1);
      if (!_input) return 0;
      result = prepareResult (1);
      if (!result) return 0;
      *result += *_input;
      if (result->failed()) {
        _error_status = result->errorStatus ();
        return 0;
      }
      resetInput (1);
      break;
    case A_MINUS_B:
      if (_percent_of_B <= 99) {
        _input = prepareInput (0);
        if (!_input) return 0;
        result = prepareResult (0);
        if (!result) return 0;
        *result += *_input;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B != 50 && _percent_of_B >= 1 && _percent_of_B <= 99){
        factor = (100 - _percent_of_B) / _percent_of_B;
        *result *= factor;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
      }
      resetInput (0);

      if (_percent_of_B >= 1) {
        _input = prepareInput (1);
        if (!_input) return 0;
        result = prepareResult (1);
        if (!result) return 0;
        *result -= *_input;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B < 50 && _percent_of_B >= 1 && _percent_of_B <= 99) {
        *result /= factor;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
      }
      resetInput (1);
      break;
    case A_PLUS_B:
      if (_percent_of_B <= 99) {
        _input = prepareInput (0);
        if (!_input) return 0;
        result = prepareResult (0);
        if (!result) return 0;
        *result += *_input;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B != 50 && _percent_of_B >= 1 && _percent_of_B <= 99){
        factor = (100 - _percent_of_B) / _percent_of_B;
        *result *= factor;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
      }
      resetInput (0);

      if (_percent_of_B >= 1) {
        _input = prepareInput (1);
        if (!_input) return 0;
        result = prepareResult (1);
        if (!result) return 0;
        *result += *_input;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
      }
      if (_percent_of_B < 50 && _percent_of_B >= 1 && _percent_of_B <= 99) {
        *result /= factor;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
      }
      resetInput (1);
      break;
    case A_DIVIDED_BY_B:
      if (_percent_of_B <= 0) {
        _input = prepareInput (0);
        if (!_input) return 0;
        result = prepareResult (0);
        if (!result) return 0;
        if (!result->fill(result->getUndefined())) {
          _error_status = result->errorStatus ();
          return 0;
        }
        resetInput (0);
      }
      else if (_percent_of_B <= 99) {
        _input = prepareInput (0);
        if (!_input) return 0;
        result = prepareResult (0);
        if (!result) return 0;
        *result += *_input;
        if (_percent_of_B != 50) {
          factor = (100 - _percent_of_B) / _percent_of_B;
          *result *= factor;
          if (result->failed()) {
            _error_status = result->errorStatus ();
            return 0;
          }
        }
        resetInput (0);

        _input = prepareInput (1);
        result = prepareResult (1);
        if (!result) return 0;
        *result /= *_input;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
        resetInput (1);
      }      
      break;
    case A_TIMES_B:
      if (_percent_of_B >= 1 && _percent_of_B <= 99) {
        _input = prepareInput (0);
        if (!_input) return 0;
        result = prepareResult (0);
        if (!result) return 0;
        *result += *_input;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
        if (_percent_of_B != 50) {
          factor = (100 - _percent_of_B) * _percent_of_B / 2500;
          *result *= factor;
          if (result->failed()) {
            _error_status = result->errorStatus ();
            return 0;
          }
        }
        resetInput (0);

        _input = prepareInput (1);
        if (!_input) return 0;
        result = prepareResult (1);
        if (!result) return 0;
        *result *= *_input;
        if (result->failed()) {
          _error_status = result->errorStatus ();
          return 0;
        }
        resetInput (1);
      }
      break;
    default:
      return 0;
  }
  if (rescaleResultStateIsSet())
    if (!rescaleGrid()) return 0;

// set the undefined value to be a bit smaller than the minimum
  if (result->gridIsDefined()) {
    float a_bit = 0.001 * (result->findMaximum() - result->findMinimum());
    if (a_bit < FLT_EPSILON) a_bit = FLT_EPSILON;
    if (!result->resetUndefined(result->findMinimum()-a_bit)) return 0;
  }

  if (!displayGrid()) return 0;  // pure virtual
  return 1;
}

int ComputeGrid::setRescaleResultState (long rescaled_result_flag)
{
  if ((rescaled_result_flag == RESCALE_RESULT      &&
       rescaleResultStateIsSet()                     ) ||
      (rescaled_result_flag == DONT_RESCALE_RESULT &&
       !rescaleResultStateIsSet()                    )   )
    return 1;

  FloatGrid *result = getResultFloatGrid ();
  _rescaled_result_flag = rescaled_result_flag;
  if (resultIsDisplayed()) {
    if (compressionStateIsSet() && result && _result_LUT && _LUTs) {
      if (!byLUT(_which_result)) return 0;
    }
    else if (result) {
      if (!byGrid(_which_result)) return 0;
    }
  }
  return 1;
}

// cause a redisplay of the result
int ComputeGrid::updateResult ()
{
// store current state locally and then clear variables 
  long compressed_inputs_flag, which_result;

  if (_compressed_inputs_flag == COMPRESS_INPUTS)
    compressed_inputs_flag = COMPRESS_INPUTS;
  else
    compressed_inputs_flag = DONT_COMPRESS_INPUTS;
  _compressed_inputs_flag = DONT_COMPRESS_INPUTS;

  if (_which_result != DO_NOTHING)
    which_result = _which_result;
  else
    which_result = DO_NOTHING;
  _which_result = DO_NOTHING;

// now set the display back the way it was, this should cause a redisplay
  if (!setCompressionState(compressed_inputs_flag)) return 0;
  if (!setResultState(which_result)) return 0;
  return 1;
}

void ComputeGrid::setPercentOfB (float percent)
{
  if (percent < 0) percent = 0;
  else if (percent > 100) percent = 100;
  _percent_of_B = percent;
}

float ComputeGrid::percentOfB ()
{
  return _percent_of_B;
}

float *ComputeGrid::percentOfBLoc ()
{
  return &_percent_of_B;
}

int ComputeGrid::failed ()
{
  return (int)(_error_status != CG_SUCCESSFUL);
}

// one or more inputs changed so rather than automatically updating the
//   result, set an inputs out of date indicator so as to avoid unwanted
//   computation
void ComputeGrid::inputsChanged ()
{
  _inputs_out_of_date = 1;
  _compression_out_of_date = 1;
  if (!compressionStateIsSet()) deleteLUTObjects ();  // avoid unwanted storage
}

int ComputeGrid::validateIndex (int index)
{
// ensure valid input index
  if (index < 0)
    return (int)0;
  else if (index >= _number_of_inputs)
    return _number_of_inputs - 1;
  else
    return index;
}

// move the input grids to the unsigned char grid where compression can happen
//   any necessary nonrotational (X-Y) resampling and spatial shifting is done
int ComputeGrid::moveInputsToCompress ()
{
  _inps = new UCharGridAccessor ();
  if (!_inps) {
    _error_status = CG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (_inps->failed()) {
    _error_status = _inps->errorStatus ();
    return 0;
  }
  if (!_inps->specifyData (_ucg)) {
    _error_status = _inps->errorStatus ();
    return 0;
  }
  if (!_inps->setZBinData (resultX0(), resultX1())) {
    _error_status = _inps->errorStatus ();
    return 0;
  }
  if (!_inps->setXBinData (resultY0(), resultY1())) {
    _error_status = _inps->errorStatus ();
    return 0;
  }
  if (!_inps->setYBinData ((float)(_number_of_inputs-1), (float)0)) {
    _error_status = _inps->errorStatus ();
    return 0;
  }

// set the multi-spectral grid to be undefined assuming that the subregion
//   has been maximized
  if (!_ucg->fill(_ucg->getUndefined())) {
    _error_status = _ucg->errorStatus ();
    return 0;
  }

// instantiate the object to move the float grid to the byte multi-spectral
//   grid
  _ftuc = new FloatToUChar ();
  if (!_ftuc) {
    _error_status = CG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (_ftuc->failed()) {
    _error_status = _ftuc->errorStatus ();
    return 0;
  }

  float amin, amax, gain, bias;
  long arr_index;
  int k2;
  for (k2 = 0; k2 < _number_of_inputs; k2++) {

// prepare the input's spatial scaling
    _input = prepareInput (k2);
    if (!_input) return 0;

// determine and store amplitude rescaling parameters that will convert input
//   to 8-bit data
//  amin = getInputFloatGrid(k2)->findStatisticalMinimum (2.5);
//  amax = getInputFloatGrid(k2)->findStatisticalMaximum (2.5);
    amax = getInputFloatGrid(k2)->findMaximum ();
    amin = getInputFloatGrid(k2)->findMinimum ();
    if (amax-amin > FLT_EPSILON) {
      gain = (float)254 / (amax - amin);
      bias = -gain * amin;
    }
    else {
      _error_status = CG_AMPLITUDE_SCALE_ERROR;
      return 0;
    }

// set up the region of interest in _ucg based on the starting user
//   coordinates of the input. assumes that _ucg's subsize is maximized
    arr_index = _ucg->getIndex (_inps->getGridX(inputY0(k2)),
                                _inps->getGridY((float)k2),
                                _inps->getGridZ(inputX0(k2))     );
    if (!_ucg->setSubStart(_ucg->getX(arr_index), _ucg->getY(arr_index),
                           _ucg->getZ(arr_index)                        )) {
      _error_status = _ucg->errorStatus ();
      return 0;
    }
    arr_index = _ucg->getIndex (_inps->getGridX(inputY1(k2)),
                                _inps->getGridY((float)k2),
                                _inps->getGridZ(inputX1(k2))     );
    if (!_ucg->setSubEnd(_ucg->getX(arr_index), _ucg->getY(arr_index),
                         _ucg->getZ(arr_index)                        )) {
      _error_status = _ucg->errorStatus ();
      return 0;
    }

// move the input float grid into the resultant unsigned char grid
    if (!_ftuc->move(_input,_ucg,gain,bias)) {
      _error_status = _ftuc->errorStatus ();
      return 0;
    }
    resetInput (k2);
  }

// don't waste classification vectors on undefined vectors
  _ftuc->simplifyUndefinedVectors (_ucg);

// clean up
  delete _ftuc, _ftuc = 0;
  delete _inps, _inps = 0;

  return 1;
}

FloatGrid *ComputeGrid::prepareInput (int index)
{
  if (!prepareAllOfResult()) return 0;

  if (!prepareAllOfInput(index)) return 0;

// set up the region of interest in the input based on the user coordinates of
//   the result
  FloatGrid *input = getInputFloatGrid (index);
  long arr_index;
  arr_index = input->getIndex (_inp->getGridX(resultX0()),
                               _inp->getGridY(resultY0()));
  if (!input->setSubStart(input->getX(arr_index),input->getY(arr_index))) {
    _error_status = input->errorStatus();
    return 0;
  }
  arr_index = input->getIndex (_inp->getGridX(resultX1()),
                               _inp->getGridY(resultY1()));
  if (!input->setSubEnd (input->getX(arr_index),input->getY(arr_index))) {
    _error_status = input->errorStatus();
    return 0;
  }

// get the input and result bin ranges
  float x_scale_factor = _inp->getXBinSize() / _res->getXBinSize();
  float y_scale_factor = _inp->getYBinSize() / _res->getYBinSize();
  int inp_num_x_bins   = input->getSubXBins();
  int inp_num_y_bins   = input->getSubYBins();
  int res_num_x_bins   = (int)(x_scale_factor * (float)(inp_num_x_bins - 1)
    + 1.5);
  int res_num_y_bins   = (int)(y_scale_factor * (float)(inp_num_y_bins - 1)
    + 1.5);
    
  if (inp_num_x_bins != res_num_x_bins || inp_num_y_bins != res_num_y_bins) {
// the result num of bins are not equal to the input number of bins

// resample the temporary float grid so the bin sizes will match the resultants
    _input = new FloatGrid (res_num_x_bins, res_num_y_bins,
      getResultFloatGrid()->getDoAbort());
    if (!_input) {
      _error_status = CG_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_input->failed()) {
      _error_status = _input->errorStatus ();
      return 0;
    }
    if (!input->resample (_input)) {
      _error_status = input->errorStatus ();
      return 0;
    }
  }
  else {
    _input = input;
    if (!_input) {
      _error_status = CG_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
  }

  if (!_input->resetUndefined(getResultFloatGrid()->getUndefined())) {
    _error_status = _input->errorStatus ();
    return 0;
  }

  delete _inp,   _inp = 0;
  delete _res,   _res = 0;

  return _input;
}

int ComputeGrid::prepareAllOfInput (int index)
{
// set up the input grid accessor
  _inp = new FloatGridAccessor ();
  if (!_inp) {
    _error_status = CG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  else if (_inp->failed()) {
    _error_status = _inp->errorStatus();
    return 0;
  }
  if (!_inp->specifyData (getInputFloatGrid(index))) {
    _error_status = _inp->errorStatus();
    return 0;
  }
  if (!_inp->setXBinData (inputX0(index), inputX1(index))) {
    _error_status = _inp->errorStatus();
    return 0;
  }
  if (!_inp->setYBinData (inputY0(index), inputY1(index))) {
    _error_status = _inp->errorStatus();
    return 0;
  }
  return 1;
}

// remove any temporary float grids that are unnecessary
void ComputeGrid::resetInput (int index)
{
  if (_input) {
    if (_input != getInputFloatGrid(index)) delete _input;
    _input = 0;
  }
}

void ComputeGrid::deleteInput ()
{
  int k2, found = 0;
  if (_input) {
    for (k2 = 0; k2 < _number_of_inputs && !found; k2++)
      found = (int)(_input == getInputFloatGrid(k2));
    if (!found) delete _input;
  }
  _input = 0;
}

FloatGrid *ComputeGrid::prepareResult (int index)
{
  if (!prepareAllOfResult()) return 0;

// set up the region of interest in the result based on the user coordinates of
//   the given input
  FloatGrid *result = getResultFloatGrid ();
  long arr_index;
  arr_index = result->getIndex (_res->getGridX(inputX0(index)),
                                _res->getGridY(inputY0(index)));
  if (!result->setSubStart(result->getX(arr_index),result->getY(arr_index))) {
    _error_status = result->errorStatus();
    return 0;
  }
  arr_index = result->getIndex (_res->getGridX(inputX1(index)),
                                _res->getGridY(inputY1(index)));
  if (!result->setSubEnd(result->getX(arr_index),result->getY(arr_index))) {
    _error_status = result->errorStatus();
    return 0;
  }

// assume any place where overlap does not occur is undefined
  if (!result->fillOutside(result->getUndefined())) {
    _error_status = result->errorStatus();
    return 0;
  }
  return result;
}

int ComputeGrid::prepareAllOfResult ()
{
  _res = new FloatGridAccessor ();
  if (!_res) {
    _error_status = CG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  else if (_res->failed()) {
    _error_status = _res->errorStatus();
    return 0;
  }
  if (!_res->specifyData (getResultFloatGrid())) {
    _error_status = _res->errorStatus();
    return 0;
  }
  if (!_res->setXBinData (resultX0(), resultX1())) {
    _error_status = _res->errorStatus();
    return 0;
  }
  if (!_res->setYBinData (resultY0(), resultY1())) {
    _error_status = _res->errorStatus();
    return 0;
  }
  return 1;
}

int ComputeGrid::rescaleLUT ()
{
  float rmax = resultMaxAmp ();
  float rmin = resultMinAmp ();
  float gmax = _result_LUT->findStatisticalMaximum ();
  float gmin = _result_LUT->findStatisticalMinimum ();

  if (rmax-rmin < FLT_EPSILON) {
    _error_status = CG_AMPLITUDE_SCALE_ERROR;
    return 0;
  }
  if (gmax-gmin < FLT_EPSILON) {
    _error_status = CG_AMPLITUDE_SCALE_ERROR;
    return 0;
  }
  float gain = (rmax - rmin) / (gmax - gmin);
  float bias = rmin - gain * gmin;

  if (!_result_LUT->setRange(_result_LUT->getUndefined(),rmin,rmax)) {
    _error_status = _result_LUT->errorStatus ();
    return 0;
  }

  if (!_result_LUT->setRescaleParameters(gain,bias)) {
    _error_status = _result_LUT->errorStatus ();
    return 0;
  }

  if (!_result_LUT->rescaleWithClip()) {
    _error_status = _result_LUT->errorStatus ();
    return 0;
  }
  return 1;
}

int ComputeGrid::rescaleGrid ()
{
  FloatGrid *result = getResultFloatGrid ();
  float rmax = resultMaxAmp ();
  float rmin = resultMinAmp ();
  float gmax = result->findStatisticalMaximum ();
  float gmin = result->findStatisticalMinimum ();

  if (rmax-rmin < FLT_EPSILON) {
    _error_status = CG_AMPLITUDE_SCALE_ERROR;
    return 0;
  }
  if (gmax-gmin < FLT_EPSILON) {
    _error_status = CG_AMPLITUDE_SCALE_ERROR;
    return 0;
  }
  float gain = (rmax - rmin) / (gmax - gmin);
  float bias = rmin - gain * gmin;

  if (!result->setRange(result->getUndefined(),rmin,rmax)) {
    _error_status = result->errorStatus ();
    return 0;
  }

  if (!result->setRescaleParameters(gain,bias)) {
    _error_status = result->errorStatus ();
    return 0;
  }

  if (!result->rescaleWithClip()) {
    _error_status = result->errorStatus ();
    return 0;
  }
  return 1;
}

int ComputeGrid::initializeResultantGrid ()
{
  FloatGrid *result = getResultFloatGrid ();

// maximize the region of interest in the result
  if (!result->setSubStart((int)0,(int)0)) {
    _error_status = result->errorStatus ();
    return 0;
  }
  if (!result->setSubSize(result->getNumXBins(),result->getNumYBins())) {
    _error_status = result->errorStatus ();
    return 0;
  }

// set the undefined value to be a large negative number
  if (!result->setRange(-FLT_MAX,min((float)0,result->getMinimum()),
    max((float)0,result->getMaximum()))) {
    _error_status = result->errorStatus ();
    return 0;
  }

// zero out the result
  if (!result->fill((float)0)) {
    _error_status = result->errorStatus ();
    return 0;
  }
  return 1;
}
