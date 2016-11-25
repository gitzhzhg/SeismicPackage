// decoder.cc:  Decoder class
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

#include "dp/decoder.hh"

Decoder::Decoder (int coding_levels):
  _coding_levels   (coding_levels),
  _decoder         (0),
  _decoder_offset  (0),
  _num_bins        (0),
  _error_status    (D_SUCCESSFUL)
{
// Check for valid inputs
  if (_coding_levels <   1 ||
      _coding_levels > 256   ) {
    _error_status = D_BAD_INPUTS;
    return;
  }
}

Decoder::~Decoder ()
{
  reclaimMemory ();
}

int Decoder::arrayInitializer (int num_bins, int coding_levels)
{
  if (num_bins < 1) {
    _error_status = D_BAD_INPUTS;
    return 0;
  }
  _num_bins = num_bins;

  if (coding_levels > 0 && coding_levels < 257) _coding_levels = coding_levels;

  if (_decoder) delete [] _decoder, _decoder = 0;
  _decoder = new unsigned char[_coding_levels*_num_bins];
  if (!_decoder) {
    _error_status = D_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  return 1;
}

unsigned char Decoder::findMinimum ()
{
  int count = _coding_levels * _num_bins;
  int k2;
  unsigned char retval = 255;
  for (k2 = 0; k2 < count; k2++)
    if (_decoder[k2] < retval) retval = _decoder[k2];
  return retval;
}

unsigned char Decoder::findMaximum ()
{
  int count = _coding_levels * _num_bins;
  int k2;
  unsigned char retval = 0;
  for (k2 = 0; k2 < count; k2++)
    if (_decoder[k2] > retval) retval = _decoder[k2];
  return retval;
}

int Decoder::failed ()
{
  return (int) (_error_status != D_SUCCESSFUL);
}

void Decoder::reset ()
{
  reclaimMemory ();
  _num_bins = 0;
  _error_status = D_SUCCESSFUL;
}

void Decoder::reclaimMemory ()
{
  if (_decoder) delete [] _decoder, _decoder = 0;
}
