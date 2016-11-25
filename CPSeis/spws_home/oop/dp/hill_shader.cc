// hill_shader.hh:  Implementation file for HillShader class
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
#include "dp/hill_shader.hh"
#include "dp/float_grid.hh"
#include "sp/do_abort.hh"
#include <math.h>
#include <float.h>
#include <assert.h>

// The incoming (N x M) structure and attribute arrays must have an extra
//   necessary border pixel.
//   Thus the hill shade operation is begun at index (1,1), and carried out to
//   index (N-1, M-1).  Consequently, the resulting encoded hill shade array
//   alters only an (N-2 x M-2) array at the appropriate position.  The need
//   for a border pixel arises from the 3 x 3 convolution required in this
//   process.
// This class only alters the incoming structure and attribute  objects'
//   subregion.

#define EPSILON 1e-35
#define PARTIALS_STDEV 20.0
#define MIX_PARTIALS 0
#define MIN_CLASSIFIED 30000
#define PCNT_CLASSIFIED 0.2

#ifndef PI
#define PI 3.14159265358979323846
#endif

HillShader::HillShader (int coding_levels):
  DisplayProcessorBase (coding_levels)
{
// initialize pointers to NULL
  _structure = 0;
  _attribute = 0;
  _hill_shade_array = 0;
  _hill_shade_array_float = 0;
  _s = 0;
  _a = 0;
  _dx = 0;
  _dy = 0;
  _a_dx_dy = 0;
  _ca = _sa = _ce = _se = (float)0.7071067811865;

// after pointers are NULLified, check for failure in ancestors instantiantion
  if (DisplayProcessorBase::failed()) return;

// initialize some pointers and constants
  _degrees_to_radians = (double)PI / (double)180;
  _type = COLOR_ATTRIBUTE;        // do color attribute hill-shaded displays
  _max_level = MAX_INTENSITY;
}

HillShader::~HillShader ()
{
  reclaimMemory ();
}

// Initialize constants that effect hill shading on the hill shade array
//   at the user specified azimuth.
int HillShader::setAzimuth (float azimuth_degrees)
{
// the initialize function must be called prior to this function
  if (_needs_initializing) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return 0;
  }
  
// determine sine and cosine of azimuth angle
  double azm_rad = (double)azimuth_degrees *_degrees_to_radians;
  _ca = (float) cos (azm_rad);
  _sa = (float) sin (azm_rad);
  return setIllumination ();
}

// Initialize constants that effect hill shading on the hill shade array
//   at the user specified elevation.
int HillShader::setElevation (float elevation_degrees)
{
// the initialize function must be called prior to this function
  if (_needs_initializing) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return 0;
  }
  
// determine sine and cosine of elevation angle
  double elv_rad = (double)elevation_degrees *_degrees_to_radians;
  _ce = (float) cos (elv_rad);
  _se = (float) sin (elv_rad);
  return setIllumination ();
}

void HillShader::setColorAttribute ()
{
  _type = COLOR_ATTRIBUTE; // do color attribute hill-shaded displays
}

void HillShader::setColorAttributeContours ()
{
  _type = COLOR_ATTRIBUTE_CONTOURS; // do a color attribute contour display
}

void HillShader::setBlackAndWhiteAttribute ()
{
  _type = B_W_ATTRIBUTE; // do B&W attribute hill-shaded displays
}

void HillShader::setBlackAndWhiteStructure ()
{
  _type = B_W_STRUCTURE; // do B&W structural hill-shaded displays
}

void HillShader::setBlackAndWhiteAttributeContours ()
{
  _type = B_W_ATTRIBUTE_CONTOURS; // do a B&W attribute contour display
}

// Initially execute the hill-shader for the entire structure and attribute
int HillShader::initialize (FloatGrid *structure, FloatGrid *attribute)
{
// ensure that the structure is acceptable
  if (!
    (structure                    &&
     structure->getSubXBins() > 2 &&
     structure->getSubYBins() > 2   )) {
    _error_status = DPB_BAD_INPUTS;
    return 0;
  }
  _structure = structure;

  if (!attribute) 
    _attribute = structure;
  else {
// ensure that the sizes of the structure and attribute are equal
    if (attribute->getNumXBins() != structure->getNumXBins() ||
        attribute->getNumYBins() != structure->getNumYBins()   ) {
      _error_status = DPB_BAD_INPUTS;
      return 0;
    }
    attribute->setSubSize (structure->getSubXBins(), structure->getSubYBins());
    _attribute = attribute;
  }

// Process the structure and attribute
  execute (1, 1, _structure->getNumXBins()-2, _structure->getNumYBins()-2);
  _needs_initializing = 0; // turn off initialization switch
  return 1;
}

float *HillShader::getResult ()
{
  if (_needs_initializing) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return (float *)0;
  }

// initialize and return the entire hill shade array
  if (!_hill_shade_array_float) {
    _hill_shade_array_float = new float[_structure->getArraySize()];
    if (!_hill_shade_array_float) {
      _error_status = DPB_MEMORY_ALLOCATION_ERROR;
      return (float *)0;
    }
    for (long k2 = 0; k2 < _structure->getArraySize(); k2++)
      _hill_shade_array_float[k2] = (float) _hill_shade_array[k2];
  }
  return _hill_shade_array_float;
}

int HillShader::prepareUse ()
{

// if Z's have changed, see if they have changed to a trivial constant value
  int variable_new_z = newZVaries();

// if Z's have changed search out the mapping between the Z's in the given
//   RGBZ LUT and the Z's in the decoder
  if (_z_changed && variable_new_z) {

    if (!ZLUTAvailable(_coding_levels)) {
      _error_status = RGBZGenerator::errorStatus ();
      return 0;
    }

    float inv_z_gain;
    float inv_z_bias;
    if (fabs((double)_z_gain) < (double)EPSILON) {
      inv_z_gain = (float)1;
      inv_z_bias = (float)0;
    }
    else {
      inv_z_gain = (float)1 / _z_gain;
      inv_z_bias = -_z_bias / _z_gain;
    }
    float decoder_z;
    int found, k2, k3, k4 = 0;
    for (k2 = 0; k2 < _coding_levels*3; k2+=3) {
      found = 0;
      decoder_z = inv_z_gain * (float)_decoder[k2] + inv_z_bias;
      for (k3 = 1; (k3 < _rgbz_count) && !found; k3++)
        found = (decoder_z > _newz[k3-1] && decoder_z <= _newz[k3]);
      if (found)
        _z_lut[k4] = (k3 - 2) * 4;  // str the indx of the red in the RGBZ LUT
      else /* if (!found) */ {
        if (decoder_z <= _newz[0])
          _z_lut[k4] = 0; // use index of first red in RGBZ LUT
        else /* if (decoder_z > _rgbz[_rgbz_count*4-1]) */
          _z_lut[k4] = (_rgbz_count - 1) * 4; // use index of last red in RGBZ
      }
      k4++;
    }
    _z_changed = 0;

// store the non-constant new Z's
    k3 = 3;
    for (k2 = 0; k2 < _rgbz_count; k2++) {
      _rgbz[k3] = _newz[k2];
        k3 += 4;
    }
  }

  else if (_z_changed && !variable_new_z) {

    if (!_z_lut) { // the Z-LUT has to exist before a constant new Z occurs
      _error_status = DPB_INITIALIZATION_ERROR;
      return 0;
    }

    float inv_z_gain;
    float inv_z_bias;
    if (fabs((double)_z_gain) < (double)EPSILON) {
      inv_z_gain = (float)1;
      inv_z_bias = (float)0;
    }
    else {
      inv_z_gain = (float)1 / _z_gain;
      inv_z_bias = -_z_bias / _z_gain;
    }
    float decoder_z;
    int found, k3, k4 = 0;
    for (int k2 = 0; k2 < _coding_levels*3; k2+=3) {
      found = 0;
      decoder_z = inv_z_gain * (float)_decoder[k2] + inv_z_bias;
      for (k3 = 7; (k3 < _rgbz_count*4) && !found; k3+=4)
        found = (decoder_z > _rgbz[k3-4] && decoder_z <= _rgbz[k3]);
      if (found)
        _z_lut[k4] = k3 - 7 - 4;  // store the index of the red in the RGBZ LUT
      else /* if (!found) */ {
        if (decoder_z <= _rgbz[3])
          _z_lut[k4] = 0; // use index of first red in RGBZ LUT
        else /* if (decoder_z > _rgbz[_rgbz_count*4-1]) */
          _z_lut[k4] = _rgbz_count * 4 - 4; // use index of last red in RGBZ
      }
      k4++;
    }
    _z_changed = 0;
  }
  return 1;
}

void HillShader::getOneCode (int index, float *red, float *green,
  float *blue, float *attribute)
{
// Provide a hill-shaded RGB element
  _z_lut_offset = index;
  _offset = index * 4;
  _decoder_offset = index * 3 + 1;
  computeElement (red, green, blue, attribute);
}

void HillShader::getNextCode (float *red, float *green, float *blue,
  float *attribute)
{
// Provide the next hill-shaded RGB element
  _z_lut_offset++;
  _offset += 4;
  _decoder_offset += 3;
  computeElement (red, green, blue, attribute);
}

int HillShader::execute (int first_column, int first_row,
  int column_count, int row_count)
{
  if (!_needs_initializing) // ensure that the region is within
    if (!
      (first_column >= 0                                       &&
       first_column + column_count < _structure->getNumXBins() &&
       first_row >= 0                                          &&
       first_row + row_count       < _structure->getNumYBins()   )) {
      _error_status = DPB_BAD_INPUTS;
      return 0;
    }

  if (!(preModify (first_column, first_row, column_count, row_count)))
    return 0;
  if (!modify ()) return 0;

// update the float array IF required
  if (_hill_shade_array_float) {
    DoAbort *do_abort = _structure->getDoAbort ();
    long index;
    long index_offset = (long)first_column * (long)_structure->getNumYBins() +
      (long)first_row;
    int k2, k3;
    for (k2 = 0; k2 < column_count; k2++) {
      index = index_offset;
      for (k3 = 0; k3 < row_count; k3++) {
        _hill_shade_array_float[index] = (float) _hill_shade_array[index];
        index++;
        if (do_abort) {
	  if (!(index % (long)5000)) {
	    if (do_abort->userAbort()) {
              _error_status = DPB_USER_ABORTED;
              return 0;
            }
          }
        }
      }
      index_offset += _structure->getNumYBins();
    }
  }
  return 1;
}

void HillShader::reset ()
{
  reclaimMemory ();
  _structure->resetErrorStatus ();
  _attribute->resetErrorStatus ();
  DisplayProcessorBase::reset ();
}

// Initialize constants that effect hill shading on the hill shade array
//   at the user specified illumination vector.
int HillShader::setIllumination ()
{
  if (fabs((double)_dx_gain) < (double)EPSILON) {
    _inv_dx_gain = (float)0;
    _inv_dx_bias = (float)0;
  }
  else /* if (fabs((double)_dx_gain) >= (double)EPSILON) */ {
    _inv_dx_gain = (float)1 / _dx_gain;
    _inv_dx_bias = -_dx_bias / _dx_gain;
  }

  if (fabs((double)_dy_gain) < (double)EPSILON) {
    _inv_dy_gain = (float)0;
    _inv_dy_bias = (float)0;
  }
  else /* if (fabs((double)_dy_gain) >= (double)EPSILON) */ {
    _inv_dy_gain = (float)1 / _dy_gain;
    _inv_dy_bias = -_dy_bias / _dy_gain;
  }

  if (MIX_PARTIALS) {
// without mixing we want  _xc = -_ce * _ca;
// without mixing we want  _yc = -_ce * _sa;
// but due to mixing we have
    float c45 = (float)0.7071067811865;
    float s45 = (float)0.7071067811865;
    _xc = -_ce * (c45 * _ca - s45 * _sa);
    _yc = -_ce * (c45 * _sa + s45 * _ca);
  }
  else {
    _xc = -_ce * _ca;
    _yc = -_ce * _sa;
  }

  float dx, dy, norm, hsr, min_hsr, max_hsr;
  double dtmp, ave_hsr;
  unsigned char *dx_ptr = _decoder + 1;
  unsigned char *dy_ptr = _decoder + 2;
  for (int k2 = 0; k2 < _coding_levels*3; k2+=3) {
    dx   = _inv_dx_gain * (float)dx_ptr[k2] + _inv_dx_bias;
    dy   = _inv_dy_gain * (float)dy_ptr[k2] + _inv_dy_bias;
    dtmp = (double)1 - (double)dx * (double)dx - (double)dy * (double)dy; 
    if (dtmp < (double)EPSILON) {
      norm = (float)0;
    }
    else if (dtmp >= (double)1) {
      norm = (float)1;
    }
    else {
      norm = (float)(sqrt (dtmp));
    }
    hsr  = _xc * dx + _yc * dy + _se * norm;
    if (k2 == 0) {
      min_hsr = hsr;
      max_hsr = hsr;
      ave_hsr = hsr;
    }
    else {
      if (min_hsr > hsr) min_hsr = hsr;
      if (max_hsr < hsr) max_hsr = hsr;
      ave_hsr += hsr;
    }
  }
  ave_hsr = ave_hsr / (double)_coding_levels;

// convert the extrema to zero mean
  max_hsr -= ave_hsr;
  min_hsr -= ave_hsr;

// Find a gain and bias to convert the reflection coeficient (RC) data to
//   multipliers from 0.5 to 1.5 where the average RC is translated to 1.0.
//   Note that if the average RC is not in the middle of the extrema, a linear
//   attempt is made to maintain that proportionality about 1.0.
  if (fabs((double)(max_hsr - min_hsr)) < (double)EPSILON) {
                                                           // max_hsr ~ min_hsr
    _hsr_gain = (float)0;
    _hsr_bias = (float)1;  // don't alter the RGBZ LUT
  }
  else /* if (fabs((double)(max_hsr - min_hsr)) >= (double)EPSILON) */ {
// max_hsr !=  min_hsr
    if (max_hsr > -min_hsr) {
// the distance from average to maximum is greater than to minimum
      _hsr_gain = (float).5 / max_hsr;
    }
    else /* if (max_hsr <= -min_hsr) */ {
// the distance from average to minimum is greater than to maximum
      _hsr_gain = -(float).5 / min_hsr;
    }
    _hsr_bias = (float)1 - _hsr_gain * ave_hsr;
  }
  return 1;
}

// Initialize constants that effect black and white hill shading on the hill
//   shade array at the user specified illumination vector.
int HillShader::setBlackAndWhite ()
{
  if (fabs((double)_z_gain) < (double)EPSILON) {
    _inv_z_gain = (float)0;
    _inv_z_bias = (float)0;
  }
  else /* if (fabs((double)_dz_gain) >= (double)EPSILON) */ {
    _inv_z_gain = (float)1 / _z_gain;
    _inv_z_bias = -_z_bias / _z_gain;
  }

  float z, min_z, max_z;
  double ave_z;
  unsigned char *z_ptr = _decoder;
  for (int k2 = 0; k2 < _coding_levels*3; k2+=3) {
    z = _inv_z_gain * (float)z_ptr[k2] + _inv_z_bias;
    if (k2 == 0) {
      min_z = z;
      max_z = z;
      ave_z = z;
    }
    else {
      if (min_z > z) min_z = z;
      if (max_z < z) max_z = z;
      ave_z += z;
    }
  }
  ave_z = ave_z / (double)_coding_levels;
  
// convert the extrema to zero mean z
  max_z -= ave_z;
  min_z -= ave_z;

// Find a gain and bias to convert the Z- data to
//   multipliers from 0.5 to 1.5 where the average z is translated to 1.0.
//   Note that if the average z is not in the middle of the extrema, a linear
//   attempt is made to maintain that proportionality about 1.0.
  if (fabs((double)(max_z - min_z)) < (double)EPSILON) {
                                                           // max_hsr ~ min_hsr
    _bw_gain = (float)0;
    _bw_bias = (float)1;  // don't alter the RGBZ LUT
  }
  else /* if (fabs((double)(max_z - min_z)) >= (double)EPSILON) */ {
// max_z !=  min_z
    if (max_z > -min_z) {
// the distance from average to maximum is greater than to minimum
      _bw_gain = (float).5 / max_z;
    }
    else /* if (max_z <= -min_z) */ {
// the distance from average to minimum is greater than to maximum
      _bw_gain = -(float).5 / min_z;
    }
    _bw_bias = (float).5 - _bw_gain * ave_z;
  }
  return 1;
}

void HillShader::computeElement (float *red, float *green, float *blue,
  float *attribute)
{
  float fac, dx, dy, norm;
  double dtmp, r, g, b;
  double root_of_3 = 1.7320508075689;
  int z_index = _z_lut[_z_lut_offset];
  *attribute  = _rgbz[z_index+3]; // not a monotonic function of attribute

  if (_type == COLOR_ATTRIBUTE_CONTOURS) {
    if (_rgbz_remapper) {
      *red   = _rgbz_remapper->getValue (z_index);
      *green = _rgbz_remapper->getValue (z_index+1);
      *blue  = _rgbz_remapper->getValue (z_index+2);
    }
    else {
      *red   = _rgbz[z_index];
      *green = _rgbz[z_index+1];
      *blue  = _rgbz[z_index+2];
    }
  }
  else if (_type == B_W_ATTRIBUTE_CONTOURS) {
// determine the z parameter weighting
    fac = _bw_gain * (_inv_z_gain * (float)_decoder[_decoder_offset-1]
          + _inv_z_bias) + _bw_bias;
    if (_rgbz_remapper) {
// When the rgbz remapper in invoked, use it with the rgbz table to find an 
//  additional normalized weighting.  Otherwise, only use the z parameter
//  weighting
      r = (float)_rgbz[z_index] > (float)EPSILON ?
          (double)_rgbz_remapper->getValue (z_index)
          / (double)_rgbz[z_index] :
          (double)0;
      g = (float)_rgbz[z_index+1] > (float)EPSILON ?
          (double)_rgbz_remapper->getValue (z_index+1)
          / (double)_rgbz[z_index+1] :
          (double)0;
      b = (float)_rgbz[z_index+2] > (float)EPSILON ?
          (double)_rgbz_remapper->getValue (z_index+2)
          / (double)_rgbz[z_index+2] :
          (double)0;
      dtmp = r*r + g*g + b*b;
      fac *= (float)(sqrt (dtmp) / root_of_3);
    }
    *red   = fac;
    *green = fac;
    *blue  = fac;
  }
  else {
// first compute the hill-shader weighting
    dx   = _inv_dx_gain * (float)_decoder[_decoder_offset]   + _inv_dx_bias;
    dy   = _inv_dy_gain * (float)_decoder[_decoder_offset+1] + _inv_dy_bias;
    dtmp = (double)1 - (double)dx * (double)dx - (double)dy * (double)dy;
    if (dtmp <= (double)EPSILON) {
      norm = (float)0;
    }
    else if (dtmp >= (double)1) {
      norm = (float)1;
    }
    else {
      norm = (float)(sqrt (dtmp));
    }
    fac  = _hsr_gain * (_xc * dx + _yc * dy + _se * norm) + _hsr_bias;

    if (_type == COLOR_ATTRIBUTE) {
      if (_rgbz_remapper) {
// If the rgbz remapper is on, apply the hill-shader weighting to it
        *red   = fac * _rgbz_remapper->getValue (z_index);
        *green = fac * _rgbz_remapper->getValue (z_index+1);
        *blue  = fac * _rgbz_remapper->getValue (z_index+2);
      }
      else {
// apply the hill-shader weighting directly to the rgbz table
        *red   = fac * _rgbz[z_index];
        *green = fac * _rgbz[z_index+1];
        *blue  = fac * _rgbz[z_index+2];
      }
    }
    else if (_type == B_W_ATTRIBUTE) {
// determine the z parameter weighting
      fac *= _bw_gain * (_inv_z_gain * (float)_decoder[_decoder_offset-1]
             + _inv_z_bias) + _bw_bias;
      if (_rgbz_remapper) {
// When the rgbz remapper in invoked, use it with the rgbz table to find an 
//  additional normalized weighting.  Otherwise, only use the z parameter
//  weighting
        r = (float)_rgbz[z_index] > (float)EPSILON ?
            (double)_rgbz_remapper->getValue (z_index)
            / (double)_rgbz[z_index] :
            (double)0;
        g = (float)_rgbz[z_index+1] > (float)EPSILON ?
            (double)_rgbz_remapper->getValue (z_index+1)
	    / (double)_rgbz[z_index+1] :
            (double)0;
        b = (float)_rgbz[z_index+2] > (float)EPSILON ?
            (double)_rgbz_remapper->getValue (z_index+2)
            / (double)_rgbz[z_index+2] :
            (double)0;
        dtmp = r*r + g*g + b*b;
        fac *= (float)(sqrt (dtmp) / root_of_3);
      }
      *red   = fac;
      *green = fac;
      *blue  = fac;
    }
    else /* if (_type == B_W_STRUCTURE) */ {
      if (_rgbz_remapper) {
// When the rgbz remapper in invoked, use it with the rgbz table as an 
//  additional normalized weighting.  Otherwise, only use the hill-shader
//  weighting
        r = (float)_rgbz[z_index] > (float)EPSILON ?
            (double)_rgbz_remapper->getValue (z_index)
            / (double)_rgbz[z_index] :
            (double)0;
        g = (float)_rgbz[z_index+1] > (float)EPSILON ?
            (double)_rgbz_remapper->getValue (z_index+1)
	    / (double)_rgbz[z_index+1] :
            (double)0;
        b = (float)_rgbz[z_index+2] > (float)EPSILON ?
            (double)_rgbz_remapper->getValue (z_index+2)
            / (double)_rgbz[z_index+2] :
            (double)0;
        dtmp = r*r + g*g + b*b;
        fac *= (float)(sqrt (dtmp) / root_of_3);
      }
      *red   = fac;
      *green = fac;
      *blue  = fac;
    }
  }

// restrict rgb to lie between (000 and 111)
  *red   = *red   < (float)0 ? (float)0 : *red;
  *red   = *red   > (float)1 ? (float)1 : *red;
  *green = *green < (float)0 ? (float)0 : *green;
  *green = *green > (float)1 ? (float)1 : *green;
  *blue  = *blue  < (float)0 ? (float)0 : *blue;
  *blue  = *blue  > (float)1 ? (float)1 : *blue;
}

// set the structure sub region for modifying the hill-shaded result
//   attempt to establish convolution padding
int HillShader::preModify (int first_column, int first_row, int column_count,
  int row_count)
{
  int col1 = first_column;
  int sub_x_bins = column_count;
  if (first_column > 0) {
    sub_x_bins++;
    col1--;
  }
  if (col1 + sub_x_bins < _structure->getNumXBins()) sub_x_bins++;

  int row1 = first_row;
  int sub_y_bins = row_count;
  if (first_row > 0) {
    sub_y_bins++;
    row1--;
  }
  if (row1 + sub_y_bins < _structure->getNumYBins()) sub_y_bins++;

  if (!(_structure->setSubSize (sub_x_bins, sub_y_bins))) {
    _error_status = _structure->errorStatus ();
    return 0;
  }
  if (!(_structure->setSubBinStart (col1, row1))) {
    _error_status = _structure->errorStatus ();
    return 0;
  }
  if (_attribute != _structure) {
    if (!(_attribute->setSubSize (sub_x_bins, sub_y_bins))) {
      _error_status = _attribute->errorStatus ();
      return 0;
    }
    if (!(_attribute->setSubBinStart (col1, row1))) {
      _error_status = _attribute->errorStatus ();
      return 0;
    }
  }
  return 1;
}

// execute the hill-shader on the structure's  and attribute's subregion
int HillShader::modify ()
{
// the initialize function must be called prior to this function

// initialize a _s structure with convolution padding
  DoAbort *do_abort = _structure->getDoAbort ();
  _s = new FloatGrid (_structure->getSubXBins(), _structure->getSubYBins(),
    do_abort);
  if (!_s) {
    _error_status = DPB_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (_s->failed()) {
    _error_status = _s->errorStatus ();
    return 0;
  }
  *_s = *_structure;
  float s_min = _s->findMinimum();
  float s_max = _s->findMaximum();
  if (_s->failed()) {
    _error_status = _s->errorStatus ();
    return 0;    
  }
  if (!(_s->setRange (_s->getUndefined(), s_min, s_max))) {
    _error_status = _s->errorStatus ();
    return 0;    
  }
  if (!(_s->resetUndefined ((float)(-FLT_MAX)))) {
    _error_status = _s->errorStatus ();
    return 0;    
  }
  if (!(_s->setSubSize (_structure->getSubXBins()-2,
    _structure->getSubYBins()-2))) {
    _error_status = _s->errorStatus ();
    return 0;    
  }

  if (!(_s->setSubBinStart (_structure->getSubXBinStart()+1,
    _structure->getSubYBinStart()+1))) {
    _error_status = _s->errorStatus ();
    return 0;    
  }

// initialize the dx/ds grid
  _dx = new FloatGrid (_structure->getSubXBins(),
    _structure->getSubYBins(), do_abort);
  if (!_dx) {
    _error_status = DPB_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (_dx->failed()) {
    _error_status = _dx->errorStatus ();
    return 0;
  }
  if (!_dx->fill(_structure->getUndefined())) {
    _error_status = _dx->errorStatus ();
    return 0;
  }
  if (!_dx->setSubSize(_structure->getSubXBins()-2,
    _structure->getSubYBins()-2)) {
    _error_status = _dx->errorStatus ();
    return 0;
  }
  if (!_dx->setSubStart(1,1)) {
    _error_status = _dx->errorStatus ();
    return 0;
  }
  if (!(_dx->setRange (-FLT_MAX, s_min, s_max))) {
    _error_status = _dx->errorStatus ();
    return 0;
  }

// define the dx/ds convolution kernel
  if (!(_s->initializeSearchGridPattern (6, 6))) {
    _error_status = _s->errorStatus ();
    return 0;
  }
  int y = -1;
  int k2;
  for (k2 = 0; k2 < 3; k2++) {
    if (!(_s->setSearchGridPatternOffset (k2, -1, y++))) {
      _error_status = _s->errorStatus ();
      return 0;
    }
    if (!(_s->setSearchGridPatternWeight (k2, -1))) {
      _error_status = _s->errorStatus ();
      return 0;
    }
  }
  y = -1;
  for (k2 = 3; k2 < 6; k2++) {
    if (!(_s->setSearchGridPatternOffset (k2, +1, y++))) {
      _error_status = _s->errorStatus ();
      return 0;
    }
    if (!(_s->setSearchGridPatternWeight (k2, +1))) {
      _error_status = _s->errorStatus ();
      return 0;
    }
  }

// compute the dx/ds grid
  *_dx = conv (*_s);
  if (_dx->failed()) {
    _error_status = _dx->errorStatus ();
    return 0;
  }

// initialize the dy/ds grid
  _dy = new FloatGrid (_structure->getSubXBins(),
    _structure->getSubYBins(), do_abort);
  if (!_dy) {
    _error_status = DPB_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (_dy->failed()) {
    _error_status = _dy->errorStatus ();
    return 0;
  }
  if (!_dy->fill(_structure->getUndefined())) {
    _error_status = _dy->errorStatus ();
    return 0;
  }
  if (!_dy->setSubSize(_structure->getSubXBins()-2,
    _structure->getSubYBins()-2)) {
    _error_status = _dy->errorStatus ();
    return 0;
  }
  if (!_dy->setSubStart(1,1)) {
    _error_status = _dy->errorStatus ();
    return 0;
  }
  if (!(_dy->setRange (-FLT_MAX, s_min, s_max))) {
    _error_status = _dy->errorStatus ();
    return 0;
  }

// define the dy/ds convolution kernel
  int x = -1;
  for (k2 = 0; k2 < 3; k2++) {
    if (!(_s->setSearchGridPatternOffset (k2, x++, -1))) {
      _error_status = _s->errorStatus ();
      return 0;
    }
    if (!(_s->setSearchGridPatternWeight (k2, -1))) {
      _error_status = _s->errorStatus ();
      return 0;
    }
  }
  x = -1;
  for (k2 = 3; k2 < 6; k2++) {
    if (!(_s->setSearchGridPatternOffset (k2, x++, +1))) {
      _error_status = _s->errorStatus ();
      return 0;
    }
    if (!(_s->setSearchGridPatternWeight (k2, +1))) {
      _error_status = _s->errorStatus ();
      return 0;
    }
  }

// compute the dy/ds grid
  *_dy = conv (*_s);
  if (_dy->failed()) {
    _error_status = _dy->errorStatus ();
    return 0;
  }

  float a_min, a_max;
  if (_structure == _attribute) {
    _a = _s;
    a_min = s_min;
    a_max = s_max;
  }

  else {
// initialize a _a structure for combining with _dx and _dy

    _a = new FloatGrid (_structure->getSubXBins(), _structure->getSubYBins(),
      do_abort);
    if (!_a) {
      _error_status = DPB_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_a->failed()) {
      _error_status = _a->errorStatus ();
      return 0;
    }
    *_a = *_attribute;
    a_min = _a->findMinimum();
    a_max = _a->findMaximum();
    if (_a->failed()) {
      _error_status = _a->errorStatus ();
      return 0;    
    }
    if (!(_a->setRange (_a->getUndefined(), a_min, a_max))) {
      _error_status = _a->errorStatus ();
      return 0;    
    }
    if (!(_a->resetUndefined ((float)(-FLT_MAX)))) {
      _error_status = _a->errorStatus ();
      return 0;    
    }
    if (!(_a->setSubSize (_structure->getSubXBins()-2,
      _structure->getSubYBins()-2))) {
      _error_status = _a->errorStatus ();
      return 0;    
    }

    if (!(_a->setSubBinStart (_structure->getSubXBinStart()+1,
      _structure->getSubYBinStart()+1))) {
      _error_status = _a->errorStatus ();
      return 0;
    }
  }

// determine the rescaling parameters to rescale the _a attribute to lie from
//   1.0 to the number of coding levels-1.  Set the undefined value to be 0.0

// check to see if the _a data is constant
  if (_needs_initializing) {
    if (fabs ((double)a_max - (double)a_min) < (double)EPSILON) {
      if (fabs((double)a_min) > (double)EPSILON)
        _z_gain = (_max_level - (float)1) / a_min;
      else
        _z_gain = (float)0;
    }

// _a data is not constant
    else {
      _z_gain = (_max_level - (float)1) / (a_max - a_min);
    }
    _z_bias = (float)1.5 - _z_gain * a_min;
  }

// Reset the subregion on the Attribute-array so that the rescale process
//   treats the whole incoming array!  This is because the Attribute-array
//   could have extreme values outside the subregion.
  if (!(_a->setSubSize (_structure->getSubXBins(),
    _structure->getSubYBins()))) {
    _error_status = _a->errorStatus ();
    return 0;    
  }
  if (!(_a->setSubBinStart (_structure->getSubXBinStart(),
    _structure->getSubYBinStart()))) {
    _error_status = _a->errorStatus ();
    return 0;    
  }

// rescale the amplitudes for the _a attribute
  assert (_a->setRescaleParameters (_z_gain, _z_bias));
  if (_needs_initializing) {

    if (!(_a->rescale ())) {
      _error_status = _a->errorStatus ();
      return 0;
    }
  }
  else {
    if (!(_a->setRange (-FLT_MAX, (float)1, _max_level))) {
      _error_status = _a->errorStatus ();
      return 0;
    }
    if (!(_a->rescaleWithClip ())) {
      _error_status = _a->errorStatus ();
      return 0;
    }
  }
  if (!(_a->resetUndefined ((float)0))) {
    _error_status = _a->errorStatus ();
    return 0;
  }

// initialize the magnitude grid
  FloatGrid *magn = new FloatGrid (_structure->getSubXBins(),
    _structure->getSubYBins(), do_abort);
  if (!magn) {
    _error_status = DPB_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (magn->failed()) {
    _error_status = magn->errorStatus ();
    return 0;
  }
  if (!magn->setSubSize(_structure->getSubXBins(),
    _structure->getSubYBins())) {
    _error_status = magn->errorStatus ();
    return 0;
  }
  if (!magn->setSubStart(0,0)) {
    _error_status = magn->errorStatus ();
    return 0;
  }
  if (!(magn->setRange (-FLT_MAX, s_min, s_max))) {
    _error_status = magn->errorStatus ();
    return 0;
  }
// initialize the temporary square variable
  FloatGrid *sqr = new FloatGrid (_structure->getSubXBins(),
    _structure->getSubYBins(), do_abort);
  if (!sqr) {
    _error_status = DPB_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (sqr->failed()) {
    _error_status = sqr->errorStatus ();
    return 0;
  }
  if (!sqr->setSubSize(_structure->getSubXBins(),
    _structure->getSubYBins())) {
    _error_status = sqr->errorStatus ();
    return 0;
  }
  if (!sqr->setSubStart(0,0)) {
    _error_status = sqr->errorStatus ();
    return 0;
  }
  if (!(sqr->setRange (-FLT_MAX, s_min, s_max))) {
    _error_status = sqr->errorStatus ();
    return 0;
  }

// determine the gradient magnitude image
  *sqr   = *_dx;          // set square: sqr = dx
  *sqr  *= *_dx;          // set square: sqr = dx**2
  *magn  = *sqr;          // set magnitude:  magn = dx**2
  *sqr   = *_dy;          // set square: sqr = dy
  *sqr  *= *_dy;          // set square: sqr = dy**2
  *magn += *sqr;          // set magnitude: magn = dx**2 + dy**2
  *magn += (float)1;      // set magnitude: magn = dx**2 + dy**2 + 1
  *magn  = sqrt (*magn);  // set magnitude: magn = (dx**2 + dy**2 + 1)**(1/2)

// normalize the dx/ds and dy/ds images by the gradient magnitude image
  *_dx   /= *magn;        // set dx: dx = dx / magn
  *_dy   /= *magn;        // set dy: dy = dy / magn

  if (MIX_PARTIALS) {
// mix these two results together to help the classification step
    FloatGrid *dx45 = sqr;
    FloatGrid *dy45 = magn;
    float c45 = (float)0.7071067811865;
    float s45 = (float)0.7071067811865;

    *dx45 = *_dx;
    *dx45 *= c45;
    *dy45  = *_dy;
    *dy45 *= s45;

    *_dx  = *dx45;
    *_dx -= *dy45;  // dx = cos(45)*dx - sin(45)*dy

    *_dy  = *dx45;
    *_dy += *dy45;  // dy = sin(45)*dx + cos(45)*dy

    delete dx45;
    delete dy45;
  }
  else {
    delete sqr;
    delete magn;
  }

// determine the rescaling parameters to rescale the dx/ds grid to lie between
//   1.0 and the number of coding levels.  Set the undefined value to be 0.0

// check to see if the _dx data is constant
  if (_needs_initializing) {
    float dx_min = _dx->findStatisticalMinimum (PARTIALS_STDEV);
    float dx_max = _dx->findStatisticalMaximum (PARTIALS_STDEV);
    if (_dx->failed()) {
      _error_status = _dx->errorStatus ();
      return 0;
    }
    if (fabs ((double)dx_max-(double)dx_min) < (double)EPSILON) {
      if (fabs((double)dx_min) > (double)EPSILON)
        _dx_gain = (_max_level - (float)1) / dx_min;
      else
        _dx_gain = 0;
    }

// _dx data is not constant
    else {
      _dx_gain = (_max_level - (float)1) / (dx_max - dx_min);
    }
    _dx_bias = (float)1.5 - _dx_gain * dx_min;
  }

// rescale the amplitudes for the _dx grid
  if (!(_dx->setRescaleParameters (_dx_gain, _dx_bias))) {
    _error_status = _dx->errorStatus ();
    return 0;
  }
  if (!(_dx->setRange (-FLT_MAX, (float)1, _max_level))) {
    _error_status = _dx->errorStatus ();
    return 0;
  }
  if (!(_dx->rescaleWithClip ())) {
    _error_status = _dx->errorStatus ();
    return 0;
  }
  if (!(_dx->resetUndefined ((float)0))) {
    _error_status = _dx->errorStatus ();
    return 0;
  }

// determine the rescaling parameters to rescale the dy/ds grid to lie between
//   1.0 and the number of coding levels.  Set the undefined value to be 0.0

// check to see if the _dy data is constant
  if (_needs_initializing) {
    float dy_min = _dy->findStatisticalMinimum (PARTIALS_STDEV);
    float dy_max = _dy->findStatisticalMaximum (PARTIALS_STDEV);
    if (_dy->failed()) { 
      _error_status = _dy->errorStatus ();
      return 0;
    }
    if (fabs ((double)dy_max-(double)dy_min) < (double)EPSILON) {
      if (fabs((double)dy_min) > (double)EPSILON)
        _dy_gain = (_max_level - (float)1) / dy_min;
      else
        _dy_gain = 0;
    }

// _dy data is not constant
    else {
      _dy_gain = (_max_level - (float)1) / (dy_max - dy_min);
    }
    _dy_bias = (float)1.5 - _dy_gain * dy_min;
  }

// rescale the amplitudes for the _dy grid
  if (!(_dy->setRescaleParameters (_dy_gain, _dy_bias))) {
    _error_status = _dy->errorStatus ();
    return 0;
  }
  if (!(_dy->setRange (-FLT_MAX, (float)1, _max_level))) {
    _error_status = _dy->errorStatus ();
    return 0;
  }
  if (!(_dy->rescaleWithClip ())) {
    _error_status = _dy->errorStatus ();
    return 0;
  }
  if (!(_dy->resetUndefined ((float)0))) {
    _error_status = _dy->errorStatus ();
    return 0;
  }

// Organize _a, _dx, and _dy together as an attribute interleaved unsigned
//   char array.   Trim off the border pixels.
  long count = (_a->getNumXBins () - 2) * (_a->getNumYBins () - 2);
  _a_dx_dy = new unsigned char[3*count];
  if (!_a_dx_dy) {
    _error_status = DPB_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  unsigned char *a_dx_dy_ptr = _a_dx_dy;
  int increment = 2;
  float *a_ptr  = _a ->getArray () + _a->getNumYBins () - 2;
  float *dx_ptr = _dx->getArray () + _a->getNumYBins () - 2;
  float *dy_ptr = _dy->getArray () + _a->getNumYBins () - 2;

  int k3;
  long acount = 0;
  for (k2 = 1; k2 < _a->getNumXBins()-1; k2++) {
    a_ptr  += increment;
    dx_ptr += increment;
    dy_ptr += increment;
    for (k3 = 1; k3 < _a->getNumYBins()-1; k3++) {

       a_ptr++;
      *a_dx_dy_ptr = (unsigned char) *a_ptr;
       a_dx_dy_ptr++;

       dx_ptr++;
      *a_dx_dy_ptr = (unsigned char) *dx_ptr;
       a_dx_dy_ptr++;

       dy_ptr++;
      *a_dx_dy_ptr = (unsigned char) *dy_ptr;
       a_dx_dy_ptr++;

      if (do_abort) {
        acount++;
	if (!(acount % (long)5000)) {
          if (do_abort->userAbort()) {
            _error_status = DPB_USER_ABORTED;
            return 0;
          }
        }
      }
    }
  }

  delete _dx, _dx = 0;
  delete _dy, _dy = 0;
  delete _s,  _s  = 0;
  if (_structure != _attribute) delete _a;
  _a = 0;

  if (_needs_initializing) {

// encode the three scaled arrays together to obtain the encoded array
//   along with the accompanying decoder array
    long max_nodes = MAX_NODES;
    long max_classified;
    if ((long)((float)PCNT_CLASSIFIED*(float)count+(float).5)
      > (long)MIN_CLASSIFIED) {
      max_classified = (long)((float)PCNT_CLASSIFIED * (float)count
        + (float).5);
    }
    else if (count > (long)MIN_CLASSIFIED) {
      max_classified = (long)MIN_CLASSIFIED;
    }
    else {
      max_classified = count;
    }
    _n_tree = new NTree (_a_dx_dy, count, 3, max_nodes, max_classified,
      0, 0, do_abort);
    if (!_n_tree) {
      _error_status = DPB_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_n_tree->failed()) {
      _error_status = DPB_COMPRESSION_ERROR;  // loses connction with NTree err
      return 0;
    }
    if (!(_n_tree->classify ())) {
      _error_status = DPB_COMPRESSION_ERROR;  // loses connction with NTree err
      return 0;
    }

// initialize _decoder array
    if (!arrayInitializer((int)3,_coding_levels)) return 0;

    int levels = _n_tree->reduce (_coding_levels, _decoder);
    _coding_levels = levels;

// create the hill shaded array
    _hill_shade_array = new unsigned char[_structure->getArraySize()];
    if (!_hill_shade_array) {
      _error_status = DPB_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    int zero = 0;
    if (!memset ((void *)_hill_shade_array, (unsigned char)zero,
      (size_t)_structure->getArraySize()*sizeof(unsigned char))) {
      _error_status = DPB_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (!setBlackAndWhite()) {
      _error_status = DPB_INITIALIZATION_ERROR;
      return 0;
    }
  }

// create a temporary array
  _temp_array = new unsigned char[count];
  if (!_temp_array) {
    _error_status = DPB_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

// encode the combined scaled arrays
  _n_tree->assign (_a_dx_dy, count, _decoder, _temp_array);

// insert the temporary array into the hill shaded array at the right location
  unsigned char *tap  = _temp_array;
  unsigned char *hsap = _hill_shade_array + _structure->getSubXBinStart()
    * _structure->getNumYBins() + _structure->getSubYBinStart()
    + _structure->getSubYBins() - 2;
  increment = _structure->getNumYBins() - _structure->getSubYBins() + 2;
  acount = 0;
  for (k2 = 1; k2 < _structure->getSubXBins()-1; k2++) {
    hsap += increment;
    for (k3 = 1; k3 < _structure->getSubYBins()-1; k3++) {
       hsap++;
      *hsap = *tap;
       tap++;

      if (do_abort) {
        acount++;
	if (!(acount % (long)5000)) {
	  if (do_abort->userAbort()) {
            _error_status = DPB_USER_ABORTED;
            return 0;
          }
        }
      }
    }
  }

  delete [] _a_dx_dy,     _a_dx_dy     = 0;
  delete [] _temp_array,  _temp_array  = 0;

  return 1;
}

void HillShader::reclaimMemory ()
{
  if (_s)       delete    _s,       _s       = 0;
  if (_dx)      delete    _dx,      _dx      = 0;
  if (_dy)      delete    _dy,      _dy      = 0;
  if (_a_dx_dy) delete [] _a_dx_dy, _a_dx_dy = 0;

  if (_structure != _attribute) delete _a;
  _a = 0;

  if (_hill_shade_array) delete [] _hill_shade_array, _hill_shade_array = 0;
  if (_hill_shade_array_float)
    delete [] _hill_shade_array_float, _hill_shade_array_float = 0;
}
