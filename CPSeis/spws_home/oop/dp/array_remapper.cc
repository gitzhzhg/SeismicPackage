// array_remapper.cc: implementation file for the ArrayRemapper object
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

#include "dp/array_remapper.hh"
#include <math.h>

ArrayRemapper::ArrayRemapper (const float *array, int coding_levels,
  int num_bins) :
  Decoder (coding_levels),
  _compression   (0),
  _gain          (1),
  _error_status  (AR_SUCCESSFUL)
{
  if (Decoder::failed()) {
    _error_status = Decoder::errorStatus ();
    return;
  }

  if (!resetArray(array,coding_levels,num_bins)) return;
}

ArrayRemapper::~ArrayRemapper ()
{
}

int ArrayRemapper::resetArray (const float *array, int coding_levels,
  int num_bins)
{
// check arguments
  if (!array || coding_levels < (int)1 || coding_levels > (int)256 ||
    num_bins < (int)1) {
    _error_status = AR_BAD_INPUTS;
    return (int)0;
  }

// initialize variables
  _array = array;
  _ar_num_bins = num_bins;

  if (!arrayInitializer((int)1,coding_levels)) {
    _error_status = Decoder::errorStatus ();
    return (int)0;
  }

// apply current compression
  compress ();

  return (int)1;
}

int ArrayRemapper::verifyOffset (int offset)
{
  int level = offset / _ar_num_bins;
  int bin = offset - level * _ar_num_bins;
  return verifyLevel(level) * _ar_num_bins + verifyBin(bin);
}

int ArrayRemapper::verifyLevel (int level)
{
  if (level < (int)0)
    return (int)0;
  else if (level >= _coding_levels)
    return _coding_levels - (int)1;
  else
    return level;
}

int ArrayRemapper::verifyBin (int bin)
{
  if (bin < (int)0)
    return (int)0;
  else if (bin >= _ar_num_bins)
    return _ar_num_bins - (int)1;
  else
    return bin;
}

// warning:  no verification of offset done here
float ArrayRemapper::getValue (int offset)
{
  int level = offset / _ar_num_bins;
  int bin = offset - level * _ar_num_bins;
  offset = (int)element(level) * _ar_num_bins + bin;
  return _gain * _array[offset];
}

// warning:  no verification of offset done here
void ArrayRemapper::getLevel (int offset, float *elem)
{
  int k2, level = offset / _ar_num_bins;
  offset = (int)element(level) * _ar_num_bins;
  for (k2 = 0; k2 < _ar_num_bins; k2++) elem[k2] = _gain * _array[offset++];
}

void ArrayRemapper::compress ()
{
  int k2;
  if (_compression == (int)0) {

// use a simple pass through mapping
    for (k2 = 0; k2 < _coding_levels; k2++) _decoder[k2] = (unsigned char)k2;
  }

  else {

// use Paul Hauge's reindexing scheme to compress or expand a look-up-table
    int range = _coding_levels / (int)2;
    int odd   = _coding_levels - (int)2 * range;
    double the_log = log ((double)2) * (double)_compression / (double)30;
    double scale_factor = exp (the_log); // 2 ** (compression/30)
    double max_scale_factor = (double)32;
    double min_scale_factor = (double)1 / max_scale_factor; 
    if (scale_factor < min_scale_factor) scale_factor = min_scale_factor;
    if (scale_factor > max_scale_factor) scale_factor = max_scale_factor;

    int j2_beg, k2_beg = range - (int)1;
    double midpoint, dx2, x2_beg;
    if (odd) {
      j2_beg = range + (int)1;
      midpoint = (double)range;
      dx2 = (double)1 / midpoint;
      x2_beg = dx2 - (double).001;
      _decoder[range] = (unsigned char)range;
    }
    else {
      j2_beg = range;
      midpoint = (double)range - (double).5;
      dx2 = (double)1 / midpoint;
      x2_beg = (double).5 * (dx2 - (double).001);
    }

    int k2 = k2_beg;
    int j2 = j2_beg;
    int i2;
    double pct_dev, x2 = x2_beg;
    for (i2 = 0; i2 < range; i2++) {
      pct_dev = ((double)1 - x2) / ((double)1 + x2);
      pct_dev = pow (pct_dev, scale_factor);
      pct_dev = ((double)1 - pct_dev) / ((double)1 + pct_dev);
      _decoder[k2--]
        = (unsigned char)(midpoint - pct_dev * midpoint + (double).5);
      _decoder[j2++]
        = (unsigned char)(midpoint + pct_dev * midpoint + (double).5);
      x2 += dx2;
    } 

  }
}

void ArrayRemapper::setCompression (int compression)
{
// note:  expect compression to vary from -100 to 100
  if (_compression != compression) {
    _compression = compression;
    compress ();
  }
}

int ArrayRemapper::failed ()
{
  return (int) (_error_status != AR_SUCCESSFUL);
}
