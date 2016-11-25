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
// histogram.hh:  User interface for class Histogram
// This class provides a means to histogram byte data using a n-tree
//   after John Cristy (for sparse data) or a look-up-table for dense
//   data and provides probability and entropy queries.  Sparse data is
//   assumed to be when counts < 256, otherwise dense is assumed.
#ifndef HISTOGRAM_HH
#define HISTOGRAM_HH

#include "dp/n_tree_error_codes.hh"

class Tree;

class Histogram {

public:
  Histogram					// constructor
    (unsigned char *data, long count,		// pointer to data & size
     unsigned char offset,			// data starting index
     unsigned char increment);			// data increment

  ~Histogram ();

  unsigned char minValue ();			// return minimum data value

  unsigned char maxValue ();			// return maximum data value

  unsigned char entropyInBits			// return entropy in bits
    (float pct_of_bits);			// fraction of entropy to use

  short entropyInGrayLevels			// return entropy in gray levls
    (double pct_of_gray_levels);		// fraction of entropy to use

  unsigned char probableValue			// return probable data value
    (float probability);			//   at given probability fract

  unsigned char probableValueByRange		// return probable data value
    (unsigned char begin,			//   given begining data value,
     unsigned char end,				//   ending data value,
     float probability);			//   and probability fraction

  unsigned char midPointOfRange			// return mid point of range
    (unsigned char begin,
     unsigned char end);

  short getCumulativeHistogram			// return the cumulative
    (unsigned char *distribution,		//   histogram
     short gray_levels);			//   max gray levels to find

  int failed ();				// return 1 if unsuccessful

  NTreeErrorCodes errorStatus ()		// return error status flag
    { return _error_status; }

private:
  int buildLUTHistograms			// build a tree with dense
    (unsigned char *data,			//   data
     long count, unsigned char offset,
     unsigned char increment);

  int buildTree (unsigned char *data,		// build a tree with sparse
    long count, unsigned char offset,		//   data
    unsigned char increment);

  int getTreeNodes ();				// get the data tree nodes

  int generateCumulative ();			// build histogram +/- LUT's

  void freeTreeNodes ();			// remove the data tree

  Tree
    *_DataTree;					// pointer to data tree created

  short
    _number_of_tree_nodes;			// number of tree leaf nodes

  long
    *_ch,					// temp long histogram array
    *_Lhistog,					// temp long histogram array
    *_treeNode_counts;				// pointer to histogram counts

  double
    _entropy;					// entropy in bits

  unsigned char
    *_treeNodes,				// pointer to histogram values
    *_cum_histog,				// pointer to cumulative histog
    *_inverse_cum_histog;			// pointer to inverse cum hist

  NTreeErrorCodes
    _error_status;				// error status flag

};
#endif
