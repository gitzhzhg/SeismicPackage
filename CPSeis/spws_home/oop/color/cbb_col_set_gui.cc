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
// class that creates the color set GUI for color bar builder
#include "color/cbb_col_set_gui.hh"
#include "color/color_bar_builder_pop.hh"
#include "color/color_descriptor.hh"
#include "color/cbb_rgb_set.hh"
#include "color/cbb_col_sys_gui.hh"
#include "color/cbb_attr_rng_gui.hh"
#include "color/cbb_col_fill_gui.hh"
#include "color/cbb_col_ext_gui.hh"
#include "color/cbb_col_ro_gui.hh"
#include "color/cbb_color_picker.hh"
#include "color/cbb_levels_gui.hh"
#include "color/cbb_cps_amp_proc.hh"
#include "color/color_selector_pop.hh"
#include "color/rgb_to_bhs.hh"
#include "color/resampler.hh"
#include "color/cbb_vect_data.hh"
#include "vect/ll_vector.hh"
#include <stdlib.h>
#include <math.h>

CBBColSetGui::CBBColSetGui (Widget parent, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  CBBColorSet (parent, name, hctx, XmVERTICAL),
  _cbb_pop         (cbb_pop),
  _color_info      (0),
  _rgb             (0),
  _rgb_to_bhs      (0),
  _amp             (0),
  _prev_rgb        (0),
  _prev_levels     (0),
  _CC0_resampler   (0),
  _CC1_resampler   (0),
  _CC2_resampler   (0),
  _range_vect_data (0),
  _range_vect_mngr (0),
  _range_vect      (0),
  _range_vect_ys   (0),
  _use_prev_rgb    (False),
  _active          (False),
  _been_activated  (False)
{
}

CBBColSetGui::CBBColSetGui (SLDelay *container, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  CBBColorSet (container, name, hctx, XmVERTICAL),
  _cbb_pop         (cbb_pop),
  _color_info      (0),
  _rgb             (0),
  _rgb_to_bhs      (0),
  _amp             (0),
  _prev_rgb        (0),
  _prev_levels     (0),
  _CC0_resampler   (0),
  _CC1_resampler   (0),
  _CC2_resampler   (0),
  _range_vect_data (0),
  _range_vect_mngr (0),
  _range_vect_ys   (0),
  _use_prev_rgb    (False),
  _active          (False),
  _been_activated  (False)
{
}

CBBColSetGui::~CBBColSetGui ()
{
  CBBColorSet::deactivate ();
  if (_color_info) delete _color_info, _color_info   = 0;
  if (_rgb)        delete _rgb,        _rgb          = 0;
  if (_rgb_to_bhs) delete _rgb_to_bhs, _rgb_to_bhs   = 0;
  if (_amp)        delete _amp,        _amp          = 0;

  if (_prev_rgb)   free (_prev_rgb),   _prev_rgb     = 0;

  if (_CC0_resampler) delete _CC0_resampler, _CC0_resampler = 0;
  if (_CC1_resampler) delete _CC1_resampler, _CC1_resampler = 0;
  if (_CC2_resampler) delete _CC2_resampler, _CC2_resampler = 0;

  if (_range_vect_data) delete _range_vect_data, _range_vect_data = 0;
  if (_range_vect_mngr) delete _range_vect_mngr, _range_vect_mngr = 0;
  if (_range_vect_ys)   free (_range_vect_ys),   _range_vect_ys   = 0;
}

void CBBColSetGui::establishPickInfo (int x1, int x2, int y1, int y2,
  int /* button */, PickBase::Action action, PickBase::Modifier modifier)
{
  if (modifier == PickBase::none) {
    if (action == PickBase::press) {
      establishFirstIndex (x1, y1);
      establishSecondIndex (x1, y1);
    }
    else if (action == PickBase::motion) {
      establishSecondIndex (x2, y2);
    }
    else if (action == PickBase::release) {
      establishSecondIndex (x2, y2);
    }
  }
  else if (modifier == PickBase::cntl) {
    if (action == PickBase::press) {
      establishFirstIndex (x1, y1);
      captureRGB ();
      establishSecondIndex (x1, y1);
    }
    else if (action == PickBase::motion) {
      establishSecondIndex (x2, y2);
      interpolateColors (1);
    }
    else if (action == PickBase::release) {
      if (_prev_rgb) free (_prev_rgb), _prev_rgb = 0;
      _use_prev_rgb = False;
    }
  }
  else if (modifier == PickBase::shft) {
    if (action == PickBase::press) {
      establishFirstIndex (x1, y1);
      captureRGB ();
      establishSecondIndex (x1, y1);
    }
    else if (action == PickBase::motion) {
      establishSecondIndex (x2, y2);
      extrapolateColors (1);
    }
    else if (action == PickBase::release) {
      if (_prev_rgb) free (_prev_rgb), _prev_rgb = 0;
      _use_prev_rgb = False;
    }
  }
}

void CBBColSetGui::indexEstablished (int ident)
{
  float attribute;

  if (ident == CBBColorSet::ESTABLISHED_FIRST_INDEX   ) {

// the first cell has been selected, rember the colors of it
    attribute = _amp->middleOfLevel (firstIndex());
    _cbb_pop->_attr_rng_gui->setFrom (attribute);

    int from = _rgb->getIndex (attribute);
    _rgb->getOne (from+1, &_red_p1, &_green_p1, &_blue_p1, &attribute);
    _rgb->getOne (from-1, &_red_m1, &_green_m1, &_blue_m1, &attribute);
    redefineRange ();
  }
  else if (ident == CBBColorSet::ESTABLISHED_SECOND_INDEX   ) {

// one or a range of cells have been selected, update the attribute range
    attribute = _amp->middleOfLevel (secondIndex());
    _cbb_pop->_attr_rng_gui->setTo (attribute);
    redefineRange ();
  }
}

void CBBColSetGui::valueEstablished (int ident)
{
  if (ident == CBBColorSet::ESTABLISHED_FIRST_INDEX   ) {
// the from value has been selected, remember the colors of it
    float attribute;
    int from = _rgb->getIndex (_cbb_pop->_attr_rng_gui->from());
    _rgb->getOne (from+1, &_red_p1, &_green_p1, &_blue_p1, &attribute);
    _rgb->getOne (from-1, &_red_m1, &_green_m1, &_blue_m1, &attribute);
    redefineRange ();
  }
  else if (ident == CBBColorSet::ESTABLISHED_SECOND_INDEX   ) {
// the to value has been selected, update the attribute range
    redefineRange ();
  }
}

void CBBColSetGui::managing ()
{
  if (_use_prev_rgb) {
    initialize (_prev_levels, _prev_rgb);
    _use_prev_rgb = False;
  }
  else {
    float *rgbz = _cbb_pop->getLUT ();
    int levels = _cbb_pop->getLUTSize ();
    initialize (levels, rgbz);
    free (rgbz); 
  }
  activate ();
}

void CBBColSetGui::unmanaging ()
{
  deactivate ();
}

void CBBColSetGui::activate ()
{
  CBBColorSet::activate ();
  picker()->enable ();
  _cbb_pop->_col_ro_gui->setShowRGB (True);
  _active = True;
  _been_activated = True;
}

void CBBColSetGui::deactivate ()
{
  picker()->disable ();
  CBBColorSet::deactivate ();
  _cbb_pop->_col_ro_gui->setShowRGB (False);
  _active = False;
}

void CBBColSetGui::levelsChanged ()
{
  if (!_been_activated) return;

// grab the current colors with Z's
  _prev_levels   = _amp->levels ();
  int new_levels = _cbb_pop->_levels_gui->levels ();

  if (_prev_levels != new_levels) {

    if (_prev_rgb) free (_prev_rgb);
    _prev_rgb = 0;
    _use_prev_rgb = False;

    int levs;
    if (new_levels > _prev_levels) {
      levs = new_levels;
    }
    else {
      levs = _prev_levels;
    }
    _prev_rgb = (float *)calloc ((size_t)(4*levs), sizeof(float));
    if (!_prev_rgb) return;

    Boolean prev_active = False;

    if (_active) {
// deactivate color set if it is currently active
      deactivate ();
      prev_active = True;
    }
    int k2, k3 = 0;
    for (k2 = 0; k2 < _prev_levels; k2++) {
      _rgb->getOne (k2, &_prev_rgb[k3], &_prev_rgb[k3+1],
        &_prev_rgb[k3+2], &_prev_rgb[k3+3]);
      k3 += 4;
    }

// transfer colors from the previous color LUT to the current color LUT
//   as much as is possible
    if (_prev_levels < new_levels) {
// put additional levels where the largest Z range is and split the
//   difference
      int k4, k5, lrgst_indx, cur_levels = _prev_levels;
      float diff, lrgst_diff;
      for (k2 = 0; k2 < new_levels-_prev_levels; k2++) {
        lrgst_diff = _prev_rgb[7] - _prev_rgb[3];
        lrgst_indx = 1;
        k4 = 11;
        for (k3 = 2; k3 < cur_levels; k3++) {
          diff = _prev_rgb[k4] - _prev_rgb[k4-4];
          if (diff > lrgst_diff) {
            lrgst_diff = diff;
            lrgst_indx = k3;
          }
          k4 += 4;
        }
        k4 = (cur_levels + 1) * 4 - 1;
        for (k3 = cur_levels; k3 > lrgst_indx; k3--) {
          for (k5 = k4; k5 > k4-4; k5--) _prev_rgb[k5] = _prev_rgb[k5-4];
          k4 -= 4;
        }
        cur_levels++;
        _prev_rgb[(lrgst_indx+1)*4-1] = _prev_rgb[lrgst_indx*4-1] +
          lrgst_diff / 2;
      }
    }
    else if (_prev_levels > new_levels) {
// remove levels where the smallest Z range is and fuse the outside Z's
      int k4, k5, smlst_indx, cur_levels = _prev_levels;
      float diff, smlst_diff;
      for (k2 = 0; k2 < _prev_levels-new_levels; k2++) {
        smlst_diff = _prev_rgb[7] - _prev_rgb[3];
        smlst_indx = 1;
        k4 = 11;
        for (k3 = 2; k3 < cur_levels; k3++) {
          diff = _prev_rgb[k4] - _prev_rgb[k4-4];
          if (diff < smlst_diff) {
            smlst_diff = diff;
            smlst_indx = k3;
          }
          k4 += 4;
        }
        cur_levels--;
        k4 = smlst_indx * 4;
        for (k3 = smlst_indx; k3 < cur_levels; k3++) {
          for (k5 = k4; k5 < k4+4; k5++) _prev_rgb[k5] = _prev_rgb[k5+4];
          k4 += 4;
        }
      }
    }

// initialize the color objects
    initialize (new_levels, _prev_rgb);
    free (_prev_rgb), _prev_rgb = 0;
    _use_prev_rgb = False;

// activate color set if it was previously active
    if (prev_active) activate ();
  }
}

void CBBColSetGui::changeRGB ()
{
  if (!_been_activated) return;

  Boolean prev_active = False;

  if (_active) {
// deactive color set if it is currently active
    deactivate ();
    prev_active = True;
  }

  _prev_levels = _cbb_pop->getLUTSize ();
  _cbb_pop->_levels_gui->setLevels (_prev_levels);

  if (_prev_rgb) free (_prev_rgb), _prev_rgb = 0;
  _prev_rgb = _cbb_pop->getLUT ();

  if (_prev_rgb) initialize (_prev_levels, _prev_rgb);

// activate color set if it was previously active
  if (prev_active) activate ();

  free (_prev_rgb), _prev_rgb = 0;
  _use_prev_rgb = False;
}

Boolean CBBColSetGui::RGBNotEqualToFile (char *filename)
{
  return _cbb_pop->LUTNotEqualToFile (filename, _rgb);
}

Boolean CBBColSetGui::RGBChanged ()
{
  return _cbb_pop->LUTChanged (_rgb);
}

void CBBColSetGui::newRGBSet (float *rgb)
{
  if (!_been_activated) return;

  Boolean prev_active = False;

  if (_active) {
// deactivate color set if it is currently active
    deactivate ();
    prev_active = True;
  }

  if (_prev_rgb) free (_prev_rgb), _prev_rgb = 0;

// assume that the _levels_gui object currently has the correct # of levels
  _prev_levels = _cbb_pop->_levels_gui->levels ();

// internally capture the given rgbz array
  _prev_rgb = (float *)calloc ((size_t)(4*_prev_levels), sizeof(float));
  int k2;
  for (k2 = 0; k2 < _prev_levels*4; k2++) {
    _prev_rgb[k2] = rgb[k2];
  }

// initialize the color objects
  initialize (_prev_levels, _prev_rgb);

// activate color set if it was previously active
  if (prev_active) activate ();

  free (_prev_rgb), _prev_rgb = 0;
  _use_prev_rgb = False;
}

void CBBColSetGui::storeRGBSet ()
{
  if (!_rgb) return;

  if (_prev_rgb) free (_prev_rgb), _prev_rgb = 0;

  _prev_levels = _rgb->getSize ();

// internally capture the given rgbz array
  _prev_rgb = (float *)calloc ((size_t)(4*_prev_levels), sizeof(float));
  int k2, k3 = 0;
  for (k2 = 0; k2 < _prev_levels; k2++) {
    _rgb->getOne (k2, &_prev_rgb[k3], &_prev_rgb[k3+1],
      &_prev_rgb[k3+2], &_prev_rgb[k3+3]);
    k3 += 4;
  }
  _use_prev_rgb = True;  // set flag for proper reactivation
}

void CBBColSetGui::insertColor ()
{
// put the color bar builder's active color at the color set index
//   associated with the "From"  attribute
  float red, green, blue, attribute;

  attribute = _cbb_pop->_attr_rng_gui->from ();

// find the RGB index closest to the attribute
  int index = _rgb->getIndex (attribute);

// get the active color
  _cbb_pop->_col_sel_pop->activeColor (&red, &green, &blue);

// put the color at the index using the previous attribute
  _rgb->setOne (index, red, green, blue, _rgb->getAttribute(index));

  recolor ();
}

Widget CBBColSetGui::drawingArea ()
{
  return getWidget ();
}

// for a given (x,y) pair in window coordinates, return the red, green,
//   blue, and weighted attribute
void CBBColSetGui::RGB (int x, int y, float *red, float *green,
  float *blue, float *attribute)
{
  float findex;
  findex = weightedIndex (x, y);
  int index = (int)findex;
  _rgb->getOne (index, red, green, blue, attribute);
  *attribute = _amp->valueOfLevel (findex);
}

void CBBColSetGui::interpolateColors (int continuous)
{
// using the settings of the color bar builder and the color set,
//   interpolate colors over a portion of the color set

// from the "From" attribute, find the color set index to start from
  float attribute;
  int from, to;

  attribute = _cbb_pop->_attr_rng_gui->from ();

// find the RGB index closest to the attribute
  from = _rgb->getIndex (attribute);

// from the "To" attribute, find the color set index to interpolate to
  attribute = _cbb_pop->_attr_rng_gui->to ();

// find the RGB index closest to the attribute
  to = _rgb->getIndex (attribute);

  if (from == to) {
    if (continuous) resetExternal (from, to);
    return;
  }

  if         (_cbb_pop->_col_sys_gui->BHSSelected())
    interpByBHS (from, to, continuous);
  else    if (_cbb_pop->_col_sys_gui->GRAYSelected ())
    interpByGRAY (from, to, continuous);
  else /* if (_cbb_pop->_col_sys_gui->RGBSelected ()) */
    interpByRGB (from, to, continuous);

  recolor ();
}

void CBBColSetGui::extrapolateColors (int continuous)
{
// using the settings of the color bar builder and the color set,
//   extrapolate colors over a portion of the color set

// from the "From" attribute, find the color set index to start from
  float attribute;
  int from, to;

  attribute = _cbb_pop->_attr_rng_gui->from ();

// find the RGB index closest to the attribute
  from = _rgb->getIndex (attribute);

// from the "To" attribute, find the color set index to extrapolate to
  attribute = _cbb_pop->_attr_rng_gui->to ();

// find the RGB index closest to the attribute
  to = _rgb->getIndex (attribute);

  if (from == to) {
    insertColor ();
    if (continuous) resetExternal (from, to);
    return;
  }

  if         (_cbb_pop->_col_sys_gui->BHSSelected ())
    extrapByBHS (from, to, continuous);
  else    if (_cbb_pop->_col_sys_gui->GRAYSelected())
    extrapByGRAY (from, to, continuous);
  else /* if (_cbb_pop->_col_sys_gui->RGBSelected ()) */
    extrapByRGB (from, to, continuous);

  recolor ();
}

void CBBColSetGui::extractColor (float *red, float *green, float *blue)
{
// using the color set return the color associated with the "From" attribute
  float attribute = _cbb_pop->_attr_rng_gui->from ();

// find the RGB index closest to the attribute
  int index = _rgb->getIndex (attribute);

// return the color
  _rgb->getOne (index, red, green, blue, &attribute);
}

void CBBColSetGui::initialize (int levels, float *rgbz)
{
// create the amplitude processor
  if (_amp) delete _amp, _amp = 0;
  _amp = new CBBCPSAmpProc ();
  _amp->setMinimum (_cbb_pop->_amp->maximumOfLevel(0));
  _amp->setMaximum (_cbb_pop->_amp->maximumOfLevel(_cbb_pop->_max_levels-2));
  _amp->setLevels (levels);

  initNonlinearity (levels, rgbz);

  Dimension hei = DAHeight ();

  int cells = (int)((float)hei / (float)levels);
  if (2*((int)(cells/2)) != cells) cells--; // make sure # cells is even
  _amp->setCellsPerLevel (cells);

// create the color LUT
  if (_rgb) delete _rgb, _rgb = 0;
  _rgb = new CBBRGBSet (_amp);

// create the color descriptor
  if (_color_info) delete _color_info, _color_info = 0;
  _color_info = new ColorDescriptor (W(), levels);
  _color_info->allocateColorCells ();

// pass the color information to the set of colors
  CBBColorSet::initialize (_color_info, _rgb);

// update attribute range
  _cbb_pop->_attr_rng_gui->redoAttributeRange ();
  redefineRange ();

// finish initializing including the transfer of all the colors and
//   amplitudes to the color LUT
  _rgb->setSize (levels);
  _rgb->initializeLUTs (_color_info);

  int k2, k3 = 0;
  for (k2 = 0; k2 < levels; k2++) {
    _rgb->setOne (k2, rgbz[k3], rgbz[k3+1], rgbz[k3+2],
      _amp->maximumOfLevel(k2));
    k3 += 4;
  }

  _rgb->prepareUse ();
}

void CBBColSetGui::transferPreviousColors ()
{
// transfer colors from the temporary color LUT to the permanent color LUT
//   as much as is possible

// since nonlinear color levels are now allowed, in the future, one may want
//   to modify this routine so as to stick new levels in where previous level
//   widths were biggest or remove levels where previous level widths were
//   smallest for finc < 1 or finc > 1, respectively.  A level width is
//   defined as the attribute change from one level to the next.

  int k2, k3 = 0;
  float flev = 0, finc;
  int new_levels = _amp->levels ();
  finc = (float)_prev_levels / (float)new_levels;
  for (k2 = 0; k2 < new_levels; k2++) {
    _rgb->setOne (k2, _prev_rgb[k3], _prev_rgb[k3+1], _prev_rgb[k3+2],
      _rgb->getAttribute(k2));
    flev += finc;
    k3 = (int)flev * 3;
  }
  free (_prev_rgb), _prev_rgb = 0;
  _use_prev_rgb = False;
}

void CBBColSetGui::usePreviousRGBSet ()
{
// transfer all the colors and amplitudes from the temporary color LUT to the
//   permanent color LUT as much as is possible
  int k2, k3 = 0;
  for (k2 = 0; k2 < _prev_levels; k2++) {
    _rgb->setOne (k2, _prev_rgb[k3], _prev_rgb[k3+1], _prev_rgb[k3+2],
      _prev_rgb[k3+3]);
    k3 += 4;
  }
  free (_prev_rgb), _prev_rgb = 0;
  _use_prev_rgb = False;
}

void CBBColSetGui::interpByRGB (int from, int to, int continuous)
{
  float attribute, del;
  float from_red, from_green, from_blue;
  float to_red, to_green, to_blue;
  int k2, count;
  Resampler::Type type;

  count = from > to ? from - to + 1 : to - from + 1;

  _rgb->getOne (from, &from_red, &from_green, &from_blue, &attribute);
  if (continuous) {
    if (from > to) {
      to_red   = _red_m1;
      to_green = _green_m1;
      to_blue  = _blue_m1;
    }
    else {
      to_red   = _red_p1;
      to_green = _green_p1;
      to_blue  = _blue_p1;
    }
  }
  else {
    _rgb->getOne (to,   &to_red,   &to_green,   &to_blue,   &attribute);
  }

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC0))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC0))
        */
    type = Resampler::EXPONENTIAL;

  if (count > 1) {
    del = (to_red - from_red) / (float)(count - 1);
  }
  else
    del = 0;

  if (_CC0_resampler) delete _CC0_resampler, _CC0_resampler = 0;
  _CC0_resampler = new Resampler (from_red, del, count, type);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC1))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC1))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC1))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC1))
        */
    type = Resampler::EXPONENTIAL;

  if (count > 1) {
    del = (to_green - from_green) / (float)(count - 1);
  }
  else
    del = 0;

  if (_CC1_resampler) delete _CC1_resampler, _CC1_resampler = 0;
  _CC1_resampler = new Resampler (from_green, del, count, type);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC2))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC2))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC2))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC2))
        */
    type = Resampler::EXPONENTIAL;

  if (count > 1) {
    del = (to_blue - from_blue) / (float)(count - 1);
  }
  else
    del = 0;

  if (_CC2_resampler) delete _CC2_resampler, _CC2_resampler = 0;
  _CC2_resampler = new Resampler (from_blue, del, count, type);

  _rgb->setOne (from, _CC0_resampler->value(0),
                      _CC1_resampler->value(0),
                      _CC2_resampler->value(0),
                      _rgb->getAttribute(from));
  if (to < from) {
    for (k2 = from-1; k2 >= to; k2--) {
      _rgb->setOne (k2, _CC0_resampler->nextValue(),
                        _CC1_resampler->nextValue(),
                        _CC2_resampler->nextValue(),
                        _rgb->getAttribute(k2));
    }
  }
  else {
    for (k2 = from+1; k2 <= to; k2++) {
      _rgb->setOne (k2, _CC0_resampler->nextValue(),
                        _CC1_resampler->nextValue(),
                        _CC2_resampler->nextValue(),
                        _rgb->getAttribute(k2));
    }
  }

  delete _CC0_resampler, _CC0_resampler = 0;
  delete _CC1_resampler, _CC1_resampler = 0;
  delete _CC2_resampler, _CC2_resampler = 0;

  if (continuous) resetExternal (from, to);
}

void CBBColSetGui::extrapByRGB (int from, int to, int continuous)
{
  float attribute, del, cycles;
  float from_red, from_green, from_blue;
  int k2, count;
  Resampler::Type type;

  count = from > to ? from - to + 1 : to - from + 1;

  _rgb->getOne (from, &from_red, &from_green, &from_blue, &attribute);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC0))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC0))
        */
    type = Resampler::EXPONENTIAL;

  cycles = _cbb_pop->_col_ext_gui->cycles (ColorBarBuilderPop::CC0);

  if (count > 0)
    del = (float)1 / (float)(count - 1);
  else
    del = 0;

  if (_cbb_pop->_col_ext_gui->decreasingSelected (ColorBarBuilderPop::CC0))
    del *= -1;

  if (_CC0_resampler) delete _CC0_resampler, _CC0_resampler = 0;
  _CC0_resampler = new Resampler (from_red, del, count, type, cycles);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC1))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC1))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC1))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC1))
        */
    type = Resampler::EXPONENTIAL;

  cycles = _cbb_pop->_col_ext_gui->cycles (ColorBarBuilderPop::CC1);

  if (count > 0)
    del = (float)1 / (float)(count - 1);
  else
    del = 0;

  if (_cbb_pop->_col_ext_gui->decreasingSelected (ColorBarBuilderPop::CC1))
    del *= -1;

  if (_CC1_resampler) delete _CC1_resampler, _CC1_resampler = 0;
  _CC1_resampler = new Resampler (from_green, del, count, type, cycles);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC2))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC2))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC2))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC2))
        */
    type = Resampler::EXPONENTIAL;

  cycles = _cbb_pop->_col_ext_gui->cycles (ColorBarBuilderPop::CC2);

  if (count > 0)
    del = (float)1 / (float)(count - 1);
  else
    del = 0;

  if (_cbb_pop->_col_ext_gui->decreasingSelected (ColorBarBuilderPop::CC2))
    del *= -1;

  if (_CC2_resampler) delete _CC2_resampler, _CC2_resampler = 0;
  _CC2_resampler = new Resampler (from_blue, del, count, type, cycles);
 
  _rgb->setOne (from, _CC0_resampler->value(0),
                      _CC1_resampler->value(0),
                      _CC2_resampler->value(0),
                      _rgb->getAttribute(from));
  if (to < from) {
    for (k2 = from-1; k2 >= to; k2--) {
      _rgb->setOne (k2, _CC0_resampler->nextValue(),
                        _CC1_resampler->nextValue(),
                        _CC2_resampler->nextValue(),
                        _rgb->getAttribute(k2));
    }
  }
  else {
    for (k2 = from+1; k2 <= to; k2++) {
      _rgb->setOne (k2, _CC0_resampler->nextValue(),
                        _CC1_resampler->nextValue(),
                        _CC2_resampler->nextValue(),
                        _rgb->getAttribute(k2));
    }
  }

  delete _CC0_resampler, _CC0_resampler = 0;
  delete _CC1_resampler, _CC1_resampler = 0;
  delete _CC2_resampler, _CC2_resampler = 0;

  if (continuous) resetExternal (from, to);
}

void CBBColSetGui::interpByBHS (int from, int to, int continuous)
{
  float attribute, del, pi, red, green, blue;
  float from_brightness, from_hue, from_saturation;
  float to_brightness, to_hue, to_saturation;
  int k2, count;
  Resampler::Type type;

  pi = (float)(fabs(atan2((double)0,(double)-1)));

  count = from > to ? from - to + 1 : to - from + 1;

  if (!_rgb_to_bhs) _rgb_to_bhs = new RGBToBHS ();

  _rgb->getOne (from, &red, &green, &blue, &attribute);
  _rgb_to_bhs->setRGB (red, green, blue);
  _rgb_to_bhs->BHS (&from_brightness, &from_hue, &from_saturation);

  if (continuous) {
    if (from > to) {
      red   = _red_m1;
      green = _green_m1;
      blue  = _blue_m1;
    }
    else {
      red   = _red_p1;
      green = _green_p1;
      blue  = _blue_p1;
    }
    _rgb_to_bhs->setRGB (red, green, blue);
    _rgb_to_bhs->BHS (&to_brightness, &to_hue, &to_saturation);
  }
  else {
    _rgb->getOne (to, &red, &green, &blue, &attribute);
    _rgb_to_bhs->setRGB (red, green, blue);
    _rgb_to_bhs->BHS (&to_brightness, &to_hue, &to_saturation);
  }

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC0))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC0))
        */
    type = Resampler::EXPONENTIAL;

  if (count > 1)
    del = (to_brightness - from_brightness) / (float)(count - 1);
  else
    del = 0;

  if (_CC0_resampler) delete _CC0_resampler, _CC0_resampler = 0;
  _CC0_resampler = new Resampler (from_brightness, del, count, type);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC1))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC1))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC1))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC1))
        */
    type = Resampler::EXPONENTIAL;

  if (count > 1)
    del = (to_hue - from_hue) / (float)(count - 1);
  else
    del = 0;

  if (_CC1_resampler) delete _CC1_resampler, _CC1_resampler = 0;
  _CC1_resampler = new Resampler (from_hue, del, count, type, 1, pi, -pi);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC2))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC2))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC2))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC2))
        */
    type = Resampler::EXPONENTIAL;

  if (count > 1)
    del = (to_saturation - from_saturation) / (float)(count - 1);
  else
    del = 0;

  if (_CC2_resampler) delete _CC2_resampler, _CC2_resampler = 0;
  _CC2_resampler = new Resampler (from_saturation, del, count, type);
 
  _rgb_to_bhs->setBHS (_CC0_resampler->value(0),
                       _CC1_resampler->value(0),
                       _CC2_resampler->value(0));
  _rgb_to_bhs->RGB (&red, &green, &blue);
  _rgb->setOne (from, red, green, blue, _rgb->getAttribute(from));

  if (to < from) {
    for (k2 = from-1; k2 >= to; k2--) {
      _rgb_to_bhs->setBHS (_CC0_resampler->nextValue(),
                           _CC1_resampler->nextValue(),
                           _CC2_resampler->nextValue());
      _rgb_to_bhs->RGB (&red, &green, &blue);
      _rgb->setOne (k2, red, green, blue, _rgb->getAttribute(k2));
    }
  }
  else {
    for (k2 = from+1; k2 <= to; k2++) {
      _rgb_to_bhs->setBHS (_CC0_resampler->nextValue(),
                           _CC1_resampler->nextValue(),
                           _CC2_resampler->nextValue());
      _rgb_to_bhs->RGB (&red, &green, &blue);
      _rgb->setOne (k2, red, green, blue, _rgb->getAttribute(k2));
    }
  }

  delete _CC0_resampler, _CC0_resampler = 0;
  delete _CC1_resampler, _CC1_resampler = 0;
  delete _CC2_resampler, _CC2_resampler = 0;

  if (continuous) resetExternal (from, to);
}

void CBBColSetGui::extrapByBHS (int from, int to, int continuous)
{
  float attribute, del, cycles, pi, red, green, blue;
  float from_brightness, from_hue, from_saturation;
  int k2, count;
  Resampler::Type type;

  pi = (float)(fabs (atan2((double)0,(double)-1)));

  count = from > to ? from - to + 1 : to - from + 1;

  if (!_rgb_to_bhs) _rgb_to_bhs = new RGBToBHS ();

  _rgb->getOne (from, &red, &green, &blue, &attribute);
  _rgb_to_bhs->setRGB (red, green, blue);
  _rgb_to_bhs->BHS (&from_brightness, &from_hue, &from_saturation);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC0))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC0))
        */
    type = Resampler::EXPONENTIAL;

  cycles = _cbb_pop->_col_ext_gui->cycles (ColorBarBuilderPop::CC0);

  if (count > 0)
    del = (float)1 / (float)(count - 1);
  else
    del = 0;

  if (_cbb_pop->_col_ext_gui->decreasingSelected (ColorBarBuilderPop::CC0))
    del *= -1;

  if (_CC0_resampler) delete _CC0_resampler, _CC0_resampler = 0;
  _CC0_resampler = new Resampler (from_brightness, del, count, type,
    cycles);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC1))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC1))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC1))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC1))
        */
    type = Resampler::EXPONENTIAL;

  cycles = _cbb_pop->_col_ext_gui->cycles (ColorBarBuilderPop::CC1);

  if (count > 1) {
    del = (float)(2 * pi) / (float)(count - 1);
  }
  else {
    del = 0;
  }

  if (_cbb_pop->_col_ext_gui->decreasingSelected (ColorBarBuilderPop::CC1)){
    del *= -1;
  }

  if (_CC1_resampler) delete _CC1_resampler, _CC1_resampler = 0;
  _CC1_resampler = new Resampler (from_hue, del, count, type, cycles,
    pi, -pi);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC2))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC2))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC2))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC2))
        */
    type = Resampler::EXPONENTIAL;

  cycles = _cbb_pop->_col_ext_gui->cycles (ColorBarBuilderPop::CC2);

  if (count > 0)
    del = (float)1 / (float)(count - 1);
  else
    del = 0;

  if (_cbb_pop->_col_ext_gui->decreasingSelected (ColorBarBuilderPop::CC2))
    del *= -1;

  if (_CC2_resampler) delete _CC2_resampler, _CC2_resampler = 0;
  _CC2_resampler = new Resampler (from_saturation, del, count, type,
    cycles);
 
  _rgb_to_bhs->setBHS (_CC0_resampler->value(0),
                       _CC1_resampler->value(0),
                       _CC2_resampler->value(0));
  _rgb_to_bhs->RGB (&red, &green, &blue);
  _rgb->setOne (from, red, green, blue, _rgb->getAttribute(from));
  if (to < from) {
    for (k2 = from-1; k2 >= to; k2--) {
      _rgb_to_bhs->setBHS (_CC0_resampler->nextValue(),
                           _CC1_resampler->nextValue(),
                           _CC2_resampler->nextValue());
      _rgb_to_bhs->RGB (&red, &green, &blue);
      _rgb->setOne (k2, red, green, blue, _rgb->getAttribute(k2));
    }
  }
  else {
    for (k2 = from+1; k2 <= to; k2++) {
      _rgb_to_bhs->setBHS (_CC0_resampler->nextValue(),
                           _CC1_resampler->nextValue(),
                           _CC2_resampler->nextValue());
      _rgb_to_bhs->RGB (&red, &green, &blue);
      _rgb->setOne (k2, red, green, blue, _rgb->getAttribute(k2));
    }
  }

  delete _CC0_resampler, _CC0_resampler = 0;
  delete _CC1_resampler, _CC1_resampler = 0;
  delete _CC2_resampler, _CC2_resampler = 0;

  if (continuous) resetExternal (from, to);
}

void CBBColSetGui::interpByGRAY (int from, int to, int continuous)
{
  float attribute, del, gray_value;
  float from_red, from_green, from_blue, from_gray;
  float to_red, to_green, to_blue, to_gray;
  int k2, count;
  Resampler::Type type;

  count = from > to ? from - to + 1 : to - from + 1;

  _rgb->getOne (from, &from_red, &from_green, &from_blue, &attribute);
  if (continuous) {
    if (from > to) {
      to_red   = _red_m1;
      to_green = _green_m1;
      to_blue  = _blue_m1;
    }
    else {
      to_red   = _red_p1;
      to_green = _green_p1;
      to_blue  = _blue_p1;
    }
  }
  else {
    _rgb->getOne (to,   &to_red,   &to_green,   &to_blue,   &attribute);
  }

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC0))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC0))
        */
    type = Resampler::EXPONENTIAL;

  from_gray = (float)sqrt ((double)(from_red*from_red)
    + (double)(from_green*from_green)
    + (double)(from_blue*from_blue));
  to_gray = (float)sqrt ((double)(to_red*to_red)
    + (double)(to_green*to_green)
    + (double)(to_blue*to_blue));

  if (count > 1)
    del = (to_gray - from_gray) / (float)(count - 1);
  else
    del = 0;

  if (_CC0_resampler) delete _CC0_resampler, _CC0_resampler = 0;
  _CC0_resampler = new Resampler (from_gray, del, count, type);

  gray_value = _CC0_resampler->value (0);
  _rgb->setOne (from, gray_value, gray_value, gray_value,
    _rgb->getAttribute(from));

  if (to < from) {
    for (k2 = from-1; k2 >= to; k2--) {
      gray_value = _CC0_resampler->nextValue ();
      _rgb->setOne (k2, gray_value, gray_value, gray_value,
        _rgb->getAttribute(k2));
    }
  }
  else {
    for (k2 = from+1; k2 <= to; k2++) {
      gray_value = _CC0_resampler->nextValue ();
      _rgb->setOne (k2, gray_value, gray_value, gray_value,
        _rgb->getAttribute(k2));
    }
  }

  delete _CC0_resampler, _CC0_resampler = 0;

  if (continuous) resetExternal (from, to);
}

void CBBColSetGui::extrapByGRAY (int from, int to, int continuous)
{
  float attribute, del, gray_value, cycles;
  float from_red, from_green, from_blue, from_gray;
  float to_red, to_green, to_blue, to_gray;
  int k2, count;
  Resampler::Type type;

  count = from > to ? from - to + 1 : to - from + 1;

  _rgb->getOne (from, &from_red, &from_green, &from_blue, &attribute);
  _rgb->getOne (to,   &to_red,   &to_green,   &to_blue,   &attribute);

  if (_cbb_pop->_col_fill_gui->constantSelected (ColorBarBuilderPop::CC0))
    type = Resampler::CONSTANT;
  else if (_cbb_pop->_col_fill_gui->linearSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LINEAR;
  else if (_cbb_pop->_col_fill_gui->logarithmicSelected (ColorBarBuilderPop::CC0))
    type = Resampler::LOGARITHMIC;
  else /*
    if (_cbb_pop->_col_fill_gui->exponentialSelected (ColorBarBuilderPop::CC0))
        */
    type = Resampler::EXPONENTIAL;

  from_gray = (float)sqrt ((double)(from_red*from_red)
    + (double)(from_green*from_green)
    + (double)(from_blue*from_blue));
  to_gray = (float)sqrt ((double)(to_red*to_red)
    + (double)(to_green*to_green)
    + (double)(to_blue*to_blue));

  cycles = _cbb_pop->_col_ext_gui->cycles (ColorBarBuilderPop::CC0);

  if (count > 0)
    del = (float)1 / (float)(count - 1);
  else
    del = 0;

  if (_cbb_pop->_col_ext_gui->decreasingSelected (ColorBarBuilderPop::CC0))
    del *= -1;

  if (_CC0_resampler) delete _CC0_resampler, _CC0_resampler = 0;
  _CC0_resampler = new Resampler (from_gray, del, count, type, cycles);

  gray_value = _CC0_resampler->value (0);
  _rgb->setOne (from, gray_value, gray_value, gray_value,
    _rgb->getAttribute(from));

  if (to < from) {
    for (k2 = from-1; k2 >= to; k2--) {
      gray_value = _CC0_resampler->nextValue ();
      _rgb->setOne (k2, gray_value, gray_value, gray_value,
        _rgb->getAttribute(k2));
    }
  }
  else {
    for (k2 = from+1; k2 <= to; k2++) {
      gray_value = _CC0_resampler->nextValue ();
      _rgb->setOne (k2, gray_value, gray_value, gray_value,
        _rgb->getAttribute(k2));
    }
  }

  delete _CC0_resampler, _CC0_resampler = 0;

  if (continuous) resetExternal (from, to);
}

void CBBColSetGui::initNonlinearity (int levels, float *rgbz)
{
  if (levels < 3 || !rgbz) return;

  if (nonlinear(levels, rgbz)) {
    _amp->setLevelsUnique ();
    int k2, k3 = 3;
    for (k2 = 0; k2 < levels-1; k2++) {
      _amp->setLevelMinimum (k2+1, rgbz[k3]);
      k3 += 4;
    }
  }
}

int CBBColSetGui::nonlinear (int levels, float *rgbz)
{
// find out if the Z spacings are approximately linear (i.e. +/-4%)
  int k2, k3 = 7;
  float ave_del = 0;
  for (k2 = 1; k2 < levels; k2++) {
    ave_del += rgbz[k3] - rgbz[k3-4];
    k3 += 4;
  }
  ave_del /= (float)(levels - 1);

  float tolerance;
  if (ave_del < 0) {
    tolerance = -0.04 * ave_del;
  }
  else {
    tolerance =  0.04 * ave_del;
  }

  float del;
  k3 = 7;
  for (k2 = 1; k2 < levels; k2++) {
    del = rgbz[k3] - rgbz[k3-4];
    if (del > ave_del+tolerance ||
        del < ave_del-tolerance   ) {
      return 1;
    }
    k3 += 4;
  }
  return 0;
}

void CBBColSetGui::captureRGB ()
{
  if (!_been_activated) return;

// grab the current colors with Z's
  _prev_levels   = _amp->levels ();

  if (_prev_rgb) free (_prev_rgb), _prev_rgb = 0;

  _prev_rgb = (float *)calloc ((size_t)(4*_prev_levels), sizeof(float));
  if (!_prev_rgb) return;

  int k2, k3 = 0;
  for (k2 = 0; k2 < _prev_levels; k2++) {
    _rgb->getOne (k2, &_prev_rgb[k3], &_prev_rgb[k3+1],
      &_prev_rgb[k3+2], &_prev_rgb[k3+3]);
    k3 += 4;
  }

// make the active selection color equal to where the first index is
  float red, green, blue;
  extractColor (&red, &green, &blue);
  _cbb_pop->_col_sel_pop->setActiveColor (red, green, blue);
}

void CBBColSetGui::resetExternal (int from, int to)
{
  int k2, k3;
  k3 = 0;
  if (from > to) {
    for (k2 = 0; k2 < to; k2++) {
      _rgb->setOne (k2, _prev_rgb[k3], _prev_rgb[k3+1], _prev_rgb[k3+2],
        _prev_rgb[k3+3]);
      k3 += 4;
    }
    k3 = (from + 1) * 4;
    for (k2 = from+1; k2 < _prev_levels; k2++) {
      _rgb->setOne (k2, _prev_rgb[k3], _prev_rgb[k3+1], _prev_rgb[k3+2],
        _prev_rgb[k3+3]);
      k3 += 4;
    }
  }
  else {
    for (k2 = 0; k2 < from; k2++) {
      _rgb->setOne (k2, _prev_rgb[k3], _prev_rgb[k3+1], _prev_rgb[k3+2],
        _prev_rgb[k3+3]);
      k3 += 4;
    }
    k3 = (to + 1) * 4;
    for (k2 = to+1; k2 < _prev_levels; k2++) {
      _rgb->setOne (k2, _prev_rgb[k3], _prev_rgb[k3+1], _prev_rgb[k3+2],
        _prev_rgb[k3+3]);
      k3 += 4;
    }
  }
}

void CBBColSetGui::redefineRange ()
{
  if (!_range_vect_ys) {
// create the vector data array
    _range_vect_ys = (float *)malloc (sizeof(float)*2);
  }

// at what level is the origin of the range vector
  float level = (float)amp()->levelOfValue (_cbb_pop->_attr_rng_gui->from());
// convert level to a center pixel Y-coordinate
  _range_vect_ys[0] = YPixel (level+.5);
// at what level is the end of the range vector
  level = (float)amp()->levelOfValue (_cbb_pop->_attr_rng_gui->to());
// convert level to a center pixel Y-coordinate
  _range_vect_ys[1] = YPixel (level+.5);
// find the midpoint of the color bar in X-coordinate pixels
  float x = XPixel ();

  if (!_range_vect_data) {
// instantiate the vector data
    _range_vect_data = new CBBVectData (2, x, _range_vect_ys);
  }
  else {
// modify the vector data
    _range_vect_data->setXConst (x);
    _range_vect_data->replace (0, 2, _range_vect_ys);
  }

  if (!_range_vect_mngr) {
// create the vector manager
    _range_vect_mngr = new VectorLinkedList ();
    _range_vect = _range_vect_mngr->add (_range_vect_data,
      BaseData::defaultId, "white", 3, False, Vector::SolidLine,
      Vector::FilledSquareMarker, 6);
// establish where to plot the vector
    _range_vect_mngr->addPlot (this);
// set arrow on
    _range_vect->arrowsOn (10, -1, 0, 0.5);
    _range_vect->makeVisible ();
  }
}

void CBBColSetGui::refreshGraphics (int x, int y, int width, int height)
{
  if (_range_vect) {
    _range_vect->repair (this, x, y, width, height);
  }
}
