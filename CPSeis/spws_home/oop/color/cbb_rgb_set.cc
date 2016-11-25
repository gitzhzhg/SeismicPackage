// class that creates a color bar builder RGB set object
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
#include "color/cbb_rgb_set.hh"
#include "color/color_descriptor.hh"
#include "color/amplitude_processor.hh"
#include <math.h>
#include <float.h>


CBBRGBSet::CBBRGBSet (AmplitudeProcessor *amp):
  DisplayProcessorBase (amp->levels()),
  _amp  (amp)
{
// the amplitude processor provides a level of abstraction so that any
//   crazy amplitude scheme can be insulated from this more general class

  if (!failed()) {
    arrayInitializer ();
  }
}

CBBRGBSet::~CBBRGBSet ()
{
}

int CBBRGBSet::prepareUse ()
{
  int levs = _coding_levels * _num_bins;
// if Z's have changed, see if they have changed to a trivial constant value
  int variable_new_z = newZVaries();

// if Z's have changed search out the mapping between the Z's in the given
//   RGBZ LUT and the Z's in the decoder
  if (_z_changed && variable_new_z) {

    if (!ZLUTAvailable(_coding_levels)) {
      _error_status = RGBZGenerator::errorStatus ();
      return 0;
    }

    float decoder_z;
    int found, k2, k3, k4 = 0;
    for (k2 = 0; k2 < levs; k2++) {
      found = 0;
      decoder_z = _amp->middleOfLevel (k2);
      for (k3 = 1; (k3 < _rgbz_count) && !found; k3++)
        found = (decoder_z > _newz[k3-1] && decoder_z <= _newz[k3]);
      if (found)
        _z_lut[k4] = (k3 - 1) * 4;  // str the indx of the red in the RGBZ LUT
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

    float decoder_z;
    int found, k3, k4 = 0;
    for (int k2 = 0; k2 < levs; k2++) {
      found = 0;
      decoder_z = _amp->middleOfLevel (k2);
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

int CBBRGBSet::initializeLUTs (ColorDescriptor *color_info)
{
  int levels = _amp->levels ();
  int cells = _amp->cellsPerLevel ();

  if (!color_info || !color_info->get() || _coding_levels != levels
    && _num_bins != 1) {
    _error_status = DPB_BAD_INPUTS;
    return 0;
  }
  if (color_info->numColors() != levels) {
    _error_status = DPB_BAD_INPUTS;
    return 0;
  }

// clear decoder array
  memset (_decoder, (int)0, (size_t)(_coding_levels)); 

  int k2, k3 = 3;
  for (k2 = 0; k2 < _coding_levels; k2++) {
    _decoder[k2] = (unsigned char)color_info->get()->pix[k2];
    _rgbz[k3] = _amp->maximumOfLevel (k2);
    k3 += 4;
  }
  _decoder_offset = 0;
  return 1;
}

void CBBRGBSet::getOneCode (int index, float *red, float *green, float *blue,
  float *attribute)
{
// Provide an RGB element
  _z_lut_offset = index;
  _offset = index * 4;
  _decoder_offset = index;  // will give pixel value
  computeElement (red, green, blue, attribute);
}

void CBBRGBSet::getNextCode (float *red, float *green, float *blue,
  float *attribute)
{
// Provide the next RGB element
  _z_lut_offset++;
  _offset += 4;
  _decoder_offset++;  // will provide pixel value
  computeElement (red, green, blue, attribute);
}

int CBBRGBSet::getIndex (float attribute)
{
// _z_gain, _z_bias take the Z-values to indices
  return _amp->levelOfValue (attribute);
}

int CBBRGBSet::getPixel (int index)
{
  return (int)_decoder[index];
}

float CBBRGBSet::getAttribute (int index)
{
  return _amp->maximumOfLevel (index);
}

void CBBRGBSet::computeElement (float *red, float *green, float *blue,
  float *attribute)
{
  int z_index = _z_lut[_z_lut_offset];
  *attribute = _rgbz[z_index+3];

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
