// color_lut.cc:  Implementation file for ColorLUT class
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

#include "dp/color_lut.hh"
#include "sp/seis_plot.hh"
#include "dp/output_lut.hh"
#include "dp/decoder.hh"
#include "sp/seis_color.hh"
#include <assert.h>

ColorLUT::ColorLUT () :
  RGBZGenerator (),
  _result_lut    (0),
  _sp            (0),
  _sc            (0),
  _color_ampmin  (0),
  _color_ampmax  (0),
  _error_status  (CL_SUCCESSFUL)
{
}

ColorLUT::~ColorLUT ()
{
  if (_sc) {
    resetSeisPlotLUT ();
    delete _sc;
  }
}

int ColorLUT::setSeisPlot (SeisPlot *sp)
{
  if (!sp) {
    _error_status = CL_BAD_INPUTS;
    return 0;
  }

  _sp = sp;

  if (_sc) delete _sc, _sc = 0;
  _sc = new SeisColor (sp);
  if (!_sc) {
    _error_status = CL_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

// remember the color amplitudes for _sp for later restoration
  _color_ampmin = _sp->minColorAmp ();
  _color_ampmax = _sp->maxColorAmp ();

  return getSeisPlotLUT ();
}

int ColorLUT::setResultLUT (OutputLUT *lut, Decoder *decoder)
{
  if (!lut) {
    _error_status = CL_BAD_INPUTS;
    return 0;
  }

  if (!ZLUTAvailable(decoder->getDecoderSize())) {
    _error_status = decoder->errorStatus ();
    return 0;
  }

  _result_lut = lut;

// based on the given result LUT, define the Z-LUT
  return reloadResultLUT ();
}

int ColorLUT::reloadResultLUT ()
{
  _z_changed = 1;       // assume a change in the Z's occured
  clearZs ();           // force a change of the Z's in the RGBZ array
  return prepareUse (); // redefine the Z-LUT
}

int ColorLUT::getMaximumCode ()
{
  return (int)(_result_lut->getSize () - 1);
}

// pure virtual function defined
int ColorLUT::prepareUse ()
{
// If Z's have changed.  Search out the mapping between the Z's in the given
//   RGBZ LUT and the values in the LUT.
  float z_to_check;
  int found, k2, k3;

  if (_z_changed && newZVaries()) {

    for (k2 = 0; k2 < _z_lut_size; k2++) {
      if (k2 == 0)
        z_to_check = _result_lut->getElement (k2);
      else
        z_to_check = _result_lut->nextElement ();
      found = 0;
      for (k3 = 1; (k3 < _rgbz_count) && !found; k3++)
        found = (z_to_check > _newz[k3-1] && z_to_check <= _newz[k3]);
      if (found)
        _z_lut[k2] = (k3 - 2) * 4;  // store index of red value in RGBZ LUT
      else /* if (!found) */ {
        if (z_to_check <= _newz[0])
          _z_lut[k2] = 0; // use index of first red in RGBZ LUT
        else /* if (lut_z > _newz[_rgbz_count-1]) */
          _z_lut[k2] = (_rgbz_count - 1) * 4; // use indx of last red in RGBZ
      }
    }
    _z_changed = 0;

// store the non-constant new Z's
    k3 = 3;
    for (k2 = 0; k2 < _rgbz_count; k2++) {
      _rgbz[k3] = _newz[k2];
        k3 += 4;
    }
  }

  else if (_z_changed && !newZVaries()) {

    if (!ZVaries()) {
// the Z's in _rgbz do not vary, so interpolate them from the minimum LUT value
//   to the maximum LUT value
      float z_value_incr =
        (_result_lut->findMaximum() - _result_lut->findMinimum())
        / (float)(_rgbz_count - 1);
      z_to_check = _result_lut->findMinimum (); 
      for (k2 = 3; k2 < _rgbz_count*4; k2+=4) {
        _rgbz[k2] = z_to_check;
        z_to_check += z_value_incr;
      }
    }

    for (k2 = 0; k2 < _z_lut_size; k2++) {
      if (k2 == 0)
        z_to_check = _result_lut->getElement (k2);
      else
        z_to_check = _result_lut->nextElement ();
      found = 0;
      for (k3 = 7; (k3 < _rgbz_count*4) && !found; k3+=4)
        found = (z_to_check > _rgbz[k3-4] && z_to_check <= _rgbz[k3]);
      if (found)
        _z_lut[k2] = k3 - 7 - 4;  // store index of red value in RGBZ LUT
      else /* if (!found) */ {
        if (z_to_check <= _rgbz[3])
          _z_lut[k2] = 0; // use index of first red in RGBZ LUT
        else /* if (z_to_check > _rgbz[_rgbz_count*4-1]) */
          _z_lut[k2] = (_rgbz_count - 1) * 4; // use indx of last red in RGBZ
      }
    }
    _z_changed = 0;
  }
  return 1;
}

// pure virtual function defined
void ColorLUT::getOneCode (int index, float *red, float *green,
  float *blue, float *attribute)
{
  _z_lut_offset = index;
  int z_index   = _z_lut[_z_lut_offset];
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
  *attribute = _rgbz[z_index+3];
}

// pure virtual function defined
void ColorLUT::getNextCode (float *red, float *green, float *blue,
  float *attribute)
{
  _z_lut_offset++;
  int z_index   = _z_lut[_z_lut_offset];
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
  *attribute = _rgbz[z_index+3];
}

// get the color look-up-table but do not get the previous Z's
int ColorLUT::getSeisPlotLUT ()
{
  int num_colors = _sc->getNumColors ();
  if (!setSize(num_colors)) return 0;

  float red, green, blue, attribute;
  _sc->getAColor (0, &red, &green, &blue, &attribute);
  setOne (0, red, green, blue, (float)0);  // clear out old Z's
  int k2;
  for (k2 = 1; k2 < num_colors; k2++) {
    _sc->getAColor (k2, &red, &green, &blue, &attribute);
    setNext (red, green, blue, (float)0);  // clear out old Z's
  }
  return 1;
}

int ColorLUT::notify (SeisColor *sc, Boolean reploted)
{
  assert (sc);
  if (reploted) {} // talk to M. Sherril about handling a replotted SeisPlot
  if (!changeSeisPlotLUT(sc)) return 0;
  setResult ();
  return 1;
}


int ColorLUT::changeSeisPlotLUT (SeisColor *sc)
{
  int num_colors = sc->getNumColors ();
  if (!setSize(num_colors)) return 0;

  float red, green, blue, attribute;
  sc->getAColor (0, &red, &green, &blue, &attribute);
  setOne (0, red, green, blue, attribute);
  int k2;
  for (k2 = 1; k2 < num_colors; k2++) {
    sc->getAColor (k2, &red, &green, &blue, &attribute);
    setNext (red, green, blue, attribute);
  }

  if (!prepareUse()) return 0;

  return 1;
}

int ColorLUT::setResult ()
{
  int num_colors = (int)_result_lut->getSize ();
  if (num_colors < 1) {
    _error_status = CL_BAD_INPUTS;
    return 0;
  }
  _sc->setNumColors (num_colors);

  float red, green, blue, attribute;
  getOneCode (0, &red, &green, &blue, &attribute);
  _sc->setAColor (0, red, green, blue, attribute);
  for (int k2 = 1; k2 < num_colors; k2++) {
    getNextCode (&red, &green, &blue, &attribute);
    _sc->setAColor (k2, red, green, blue, attribute);
  }
  _sp->setMinColorAmp (getMinimumCode());
  _sp->setMaxColorAmp (getMaximumCode());
  _sc->prepareColors ();
  return 1;
}

void ColorLUT::resetSeisPlotLUT ()
{
  int num_colors = getSize ();
  if (num_colors < 1) {
    _error_status = CL_BAD_INPUTS;
    return;
  }
  _sc->setNumColors (num_colors);
  float red, green, blue, attribute;
  getOne (0, &red, &green, &blue, &attribute);
  _sc->setAColor (0, red, green, blue, attribute);
  for (int k2 = 1; k2 < num_colors; k2++) {
    getNext (&red, &green, &blue, &attribute);
    _sc->setAColor (k2, red, green, blue, attribute);
  }

  _sp->setMinColorAmp (_color_ampmin);
  _sp->setMaxColorAmp (_color_ampmax);
  _sc->prepareColors ();
}

int ColorLUT::failed ()
{
  return (int)(_error_status != CL_SUCCESSFUL || RGBZGenerator::failed());
}

GridErrorCodes ColorLUT::errorStatus ()
{
  if (RGBZGenerator::failed())
    return RGBZGenerator::errorStatus ();
  else
    return _error_status;
}
