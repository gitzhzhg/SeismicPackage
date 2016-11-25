// histogram.cc:  Implementation for class Histogram
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
// This class provides a means to histogram byte data using a tree
//   and provides probability and entropy queries.
#include <assert.h>
#include <math.h>
#include "dp/histogram.hh"
#include "dp/tree.hh"

// Constructor
Histogram::Histogram (unsigned char *data, long count, unsigned char offset,
  unsigned char increment)
{
// intialize pointers to NULL
  _Lhistog = 0;
  _cum_histog = 0;
  _inverse_cum_histog = 0;
  _ch = 0;
  _DataTree = 0;
  _treeNodes = 0;
  _treeNode_counts = 0;

  if (!(count > 0)) {
    _error_status = H_BAD_INPUTS;
    return;
  }

  if (count < 256) {

// Sparse data case...
// Build the tree
    if (!(buildTree (data, count, offset, increment))) return;
// Get tree nodes
    if (!(getTreeNodes ())) return;
// Generate the cumulative and inverse cumulative histograms
    if (!(generateCumulative ())) return;
// Free the tree node storage
    freeTreeNodes ();
  }
  else {

// Dense data case...
// Generate cumulative and inverse cumulative histograms using a look-up-table
    if (!(buildLUTHistograms (data, count, offset, increment))) return;
  }
  _error_status = H_SUCCESSFUL;
}

// Destructor
Histogram::~Histogram ()
{
  freeTreeNodes ();
  if (_cum_histog) delete [] _cum_histog;
  if (_inverse_cum_histog) delete [] _inverse_cum_histog;
  if (_Lhistog) delete [] _Lhistog;
  if (_ch) delete [] _ch;
}

// Generate cumulative and inverse cumulative histograms using a look-up-table
int Histogram::buildLUTHistograms (unsigned char *data, long count,
  unsigned char offset, unsigned char increment)
{
// allocate space
  _Lhistog = new long[256];
  if (!_Lhistog) {
    _error_status = H_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

// zero array
  long k2;
  for (k2 = 0; k2 < 256; k2++)
    _Lhistog[k2] = 0;

// generate histogram
  unsigned char *dptr = data+offset;
  for (k2 = 0; k2 < count; k2++) {
    _Lhistog[*dptr]++;
    dptr += increment;
  }

// determine entropy
  double prob_k;
  _entropy = 0.0;
  for (k2 = 0; k2 < 256; k2++) {
    prob_k = (double) _Lhistog[k2] / (double) count;
    if (prob_k > 0) _entropy += -prob_k * log (prob_k) / log (2);
  }

// generate cumulative histogram and scale so as not to exceed 255
  long sum = 0;
  _cum_histog = new unsigned char[256];
  if (!_cum_histog) {
    _error_status = H_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  float value, gain = 255.0 / (float) count;
  for (k2 = 0; k2 < 256; k2++) {
    sum += _Lhistog[k2];
    value = gain * (float) sum + 0.5;
    _cum_histog[k2] = (unsigned char) value;
  }

// determine the inverse cumulative histogram and scale so as not to exceed 255
  sum = 0;
  int previous_prob = 0, current_prob, k3;
  _inverse_cum_histog = new unsigned char[256];
  if (!_inverse_cum_histog) {
    _error_status = H_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  for (k2 = 0; (k2 < 256) && (previous_prob < 256); k2++) {
    sum += _Lhistog[k2];
    value = gain * (float) sum + 0.5;
    current_prob = (int) value;
    for (k3 = previous_prob; k3 <= current_prob; k3++)
      _inverse_cum_histog[k3] = (unsigned char)k2;
    previous_prob = current_prob + 1;
  }

// ensure that the zeroth element of the inverse cumulative
//   histogram does in fact reflect the minimum value
  int found = 0;
  for (k2 = 0; (k2 < 256) && (!found); k2++)
    if (_Lhistog[k2] > 0) {
      found = 1;
      _inverse_cum_histog[0] = (unsigned char)k2;
    }

// ensure that the 255th element of the inverse cumulative
//   histogram does in fact reflect the maximum value
  found = 0;
  for (k2 = 255; (k2 > -1) && (!found); k2--)
    if (_Lhistog[k2] > 0) {
      found = 1;
      _inverse_cum_histog[255] = (unsigned char)k2;
    }

// free the long histog array
  delete [] _Lhistog, _Lhistog = 0;

  _error_status = H_SUCCESSFUL;
  return 1;
}

// Build a tree
int Histogram::buildTree (unsigned char *data, long count,
  unsigned char offset, unsigned char increment)
{
  _DataTree = new Tree (data, count, (short)offset, (long)increment);
  if (!_DataTree) {
    _error_status = H_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (_DataTree->failed()) {
    _error_status = _DataTree->errorStatus ();
    return 0;
  }
  if (!(_DataTree->classify ())) {
    _error_status = _DataTree->errorStatus ();
    return 0;
  }
  _error_status = H_SUCCESSFUL;
  return 1;
}

// Get the tree nodes
int Histogram::getTreeNodes ()
{
  _number_of_tree_nodes = _DataTree->getNumberOfLeafNodes ();
  if (_DataTree->failed()) {
    _error_status = _DataTree->errorStatus ();
    return 0;
  }
  _treeNodes       = new unsigned char[_number_of_tree_nodes];
  _treeNode_counts = new long[_number_of_tree_nodes];
  if (!(_treeNodes && _treeNode_counts)) {
    _error_status = H_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  if (!(_DataTree->getLeafNodes (_treeNodes, _treeNode_counts))) {
    _error_status = _DataTree->errorStatus ();
    return 0;
  }

  _error_status = H_SUCCESSFUL;
  return 1;
}

// return the minimum data value
unsigned char Histogram::minValue ()
{
  return _inverse_cum_histog[0];
}

// return the maximum data value
unsigned char Histogram::maxValue ()
{
  return _inverse_cum_histog[255];
}
 
// Determine the entropy in gray levels rounded up to nearest whole gray level
short Histogram::entropyInGrayLevels (double pct_of_gray_levels)
{
  double D_gray_levels = pct_of_gray_levels * pow (2, _entropy);
  short retval = short (D_gray_levels);
  if (retval < D_gray_levels)
    retval++;                // round up to nearest whole gray level
  if (!(retval <= 256)) {
    _error_status = H_UNEXPECTED_RESULT;
    return 0;
  }
  _error_status = H_SUCCESSFUL;
  return retval;
}

// Determine the entropy in bits rounded up to nearest whole bit
unsigned char Histogram::entropyInBits (float pct_of_bits)
{
  float bits = pct_of_bits * (float)_entropy;
  unsigned char retval = (unsigned char) bits;
  if (retval < bits)
    retval++;              // round up to nearest whole bit
  if (!(retval <= 8)) {
    _error_status = H_UNEXPECTED_RESULT;
    return 0;
  }
  _error_status = H_SUCCESSFUL;
  return retval;
}

// Generate a cumulative histogram using the tree nodes
//   and then the inverse cumulative histogram
int Histogram::generateCumulative ()
{
  _ch = new long[256];
  if (!_ch) {
    _error_status = H_SUCCESSFUL;
    return 0;
  }
  long *cht = _ch;
  unsigned char *tn = _treeNodes;
  long *tnc = _treeNode_counts;
  long count;
  short node_count = 0, total_node_count = _number_of_tree_nodes;
  long total_count = 0;

// From the tree nodes fill out a cumulative histogram that will allow
//   indexes for all possible data values from 0 to 255.  The returned
//   values will be equal to the sum of all data values up to that index.

  short k2;
  for (k2 = 0; k2 < 256; k2++) {
    if ((k2 == *tn) && (node_count < total_node_count)) {
      tn++;
      node_count++;
      count = *(tnc++);
      *cht = total_count + count;
      total_count = *(cht++);
    }
    else
      *(cht++) = total_count;
  }

// determine entropy
  double prob_k;
  _entropy = 0.0;
  for (k2 = 0; k2 < _number_of_tree_nodes; k2++) {
    prob_k = (double) _treeNode_counts[k2] / (double) total_count;
    _entropy += -prob_k * log (prob_k) / log (2);
  }

// Rescale the cumulative histogram so that the returned values are not
//   data counts but values from 0 to 255.  Store the rescaled cumulative
//   histogram into an unsigned char array

  float value;
  float gain = 255. / total_count;
  cht = _ch;
  _cum_histog = new unsigned char[256];
  if (!_cum_histog) {
    _error_status = H_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  unsigned char *ch_ = _cum_histog;
  
  for (k2 = 0; k2 < 256; k2++) {
    value = *(cht++) * gain + 0.5;
    *(ch_++) = (unsigned char) value;
  }

// From this cumulative histogram, given a data value ranging from 0 to
//   255, a probability number can be returned that also ranges from 0 to 255

// Eliminate the scratch cumulative histogram array.

  delete [] _ch, _ch = 0;

// From the tree nodes fill out an inverse cumulative histogram that will
//   allow indexes for probability numbers ranging from 0 to 255.  The returned
//   values will only be values that appear as a tree nodes.

  long sum = 0;
  int previous_prob = 0, current_prob, k3;
  gain = 255.0 / (float) total_count;
  _inverse_cum_histog = new unsigned char[256];
  if (!_inverse_cum_histog) {
    _error_status = H_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  for (k2 = 0; (k2 < total_node_count) && (previous_prob < 256); k2++) {
    sum += _treeNode_counts[k2];
    value = gain * (float) sum  + 0.5;
    current_prob = (int) value;
    for (k3 = previous_prob; k3 <= current_prob; k3++)
      _inverse_cum_histog[k3] = _treeNodes[k2];
    previous_prob = current_prob + 1;
  }

// From this inverse cumulative histogram, given a probability number ranging
//   from 0 to 255, a data value ranging from 0 to 255 that was stored in the
//   tree is returned.

  _error_status = H_SUCCESSFUL;
  return 1;

}

// Given a specified probability (in fractional form), return the corresponding
//   data value using the inverse cumulative histogram
unsigned char Histogram::probableValue (float probability)
{
  if (!((probability >= 0) && (probability <= 1))) {
    _error_status = H_BAD_INPUTS;
    return (unsigned char)0;
  }
  float value = probability * 255.0 + 0.5;
  int prob = (int) value;
  _error_status = H_SUCCESSFUL;
  return _inverse_cum_histog[prob];
}

// Given a specified value range and a probability, return the corresponding
//   data value as follows:
//     1) use the cumulative histogram to establish a probability
//        range from the given data value range.
//     2) use the given probability to establish another probability
//        applied to the probability range found in (1).
//     3) use the inverse cumulative histogram and the probability from
//        (2) to return the data value required.
unsigned char Histogram::probableValueByRange (unsigned char begin,
  unsigned char end, float probability)
{
  if (!(
          (end >= begin)     &&
          (probability >= 0) &&
          (probability <= 1)    )) {
    _error_status = H_BAD_INPUTS;
    return 0;
  }
  int begin_prob, end_prob, new_prob;
  begin_prob = (int) _cum_histog[begin];
  end_prob   = (int) _cum_histog[end];
  float temp = (float)(end_prob - begin_prob) * probability + begin_prob + 0.5;
  new_prob = (int) temp;
  _error_status = H_SUCCESSFUL;
  return _inverse_cum_histog[new_prob];
}

// Return midpoint of range
unsigned char Histogram::midPointOfRange (unsigned char begin,
  unsigned char end)
{
  return (unsigned char) (((short)begin + (short)end)>>1);
}

// Free the tree node storage
void Histogram::freeTreeNodes ()
{
  if (_DataTree)         delete    _DataTree       , _DataTree = 0;
  if (_treeNodes)        delete [] _treeNodes      , _treeNodes = 0;
  if (_treeNode_counts) delete [] _treeNode_counts, _treeNode_counts = 0;
}

// return a cumulative histogram given a maximum expected number
//   of gray levels.
short Histogram::getCumulativeHistogram (unsigned char *distribution,
  short gray_levels)
{
  if (!(gray_levels <= 256)) {
    _error_status = H_BAD_INPUTS;
    return 0;
  }

// assume maximum possible gray levels

  short found_gray_levels = 256;

// look for when the right-most cumulative  distribution value dips

  short k2, match = 1;
  for (k2 = 254; (k2 > -1) && match; k2--) {
    if (_cum_histog[k2] != _cum_histog[k2+1])
      match = 0;
    else
      found_gray_levels--;
  }

// make sure that the found gray levels are equivalent to what was expected

  if (found_gray_levels > gray_levels) {
    _error_status = H_UNEXPECTED_RESULT;
    return 0;
  }
  else {

// output the unique part of the cumulative distribution function
//   scaled so as not to exceed gray_levels - 1

    float value;
    float gain = (float) (gray_levels - 1) /
      (float) _cum_histog[found_gray_levels-1];

    for (k2 = 0; k2 < found_gray_levels; k2++) {
      value = gain * (float) _cum_histog[k2];
      distribution[k2] = (unsigned char) value;
    }

// fill out array if necessary

    for (k2 = found_gray_levels; k2 < gray_levels; k2++)
      distribution[k2] = (unsigned char) value;

    _error_status = H_SUCCESSFUL;
    return 1;
  }
}

int Histogram::failed ()
{
  return (int) (_error_status != H_SUCCESSFUL);
}
