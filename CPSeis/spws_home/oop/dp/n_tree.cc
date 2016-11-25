// n_tree.cc:  Implementation file for NTree class
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
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <iostream.h>
#include <math.h>
#include "dp/histogram.hh"
#include "dp/n_tree.hh"
#include "sp/do_abort.hh"

enum {MEDIAN, AVERAGE, SELECT};

// Constructor
NTree::NTree (const unsigned char *data, long count, unsigned char nchans,
  long max_nodes, long max_count_classified, unsigned char vlu_dsp,
  int allow_insert_vectors, DoAbort *do_abort):
  _data                     ((unsigned char *)data),
  _count                    ((long)count),
  _nchans                   ((unsigned char)nchans),
  _vlu_dsp                  (vlu_dsp), // specifies how all N-Tree nodes are md
  _allow_insert_vectors     (allow_insert_vectors),
  _do_abort                 (do_abort)
{
// intialize pointer to NULL
  _depths = 0;
  _histogram = 0;
  _cube_root = 0;
  _node = 0;
  _next = 0;
  _previous = 0;
  _predecessor = 0;
  _l_shifts = 0;
  _r_shifts = 0;
  _unique_codes = 0;
  _look_up_table = 0;

  if (max_nodes == -1)
    _max_tree_node_count = MAX_NODES;
  else
    _max_tree_node_count = max_nodes;

  if (max_count_classified == -1)
    _max_count_classified = _count;
  else
    _max_count_classified = max_count_classified;

  _pruning_threshold = 0;
  _next_pruning_threshold = MAX_INTEGER;
  _tree_node_count = 0;
  _highest_tree_node_count = 0;
  _times_exceeded_max_nodes = 0;
  _reduce_iteration_count = 0;
  _actual_count = 0;
  _max_count = 0;
  _unique_count = 0;
  _depths = new unsigned char[_nchans];
  _histogram  = new Histogram *[_nchans];
  if (!(_histogram && _depths)) {
    _error_status = NT_MEMORY_ALLOCATION_ERROR;
    return;
  }

short k2;
/*
// initialize pointers to NULL
  for (k2 = 0; k2 < _nchans; k2++ )
    _histogram[k2] = 0;
*/

  _cube_root  = new NTreeNode (_nchans, 0);
  if (!_cube_root) {
    _error_status = NT_MEMORY_ALLOCATION_ERROR;
    return;
  }
  if (_cube_root->failed()) {
    _error_status = _cube_root->errorStatus ();
    return;
  }
  _tree_depth = 0;
  _last_pct_of_bits = 1;
  for (k2 = 0; k2 < _nchans; k2++) {
    _histogram[k2] = new Histogram (_data, _count, k2, _nchans);
/*  if (!_histogram[k2]) {
      _error_status = NT_MEMORY_ALLOCATION_ERROR;
      return;
    } */
    if (_histogram[k2]->failed()) {
      _error_status = _histogram[k2]->errorStatus ();
      return;
    }
    _cube_root->_mins[k2]   = _histogram[k2]->minValue ();
    _cube_root->_maxs[k2]   = _histogram[k2]->maxValue ();
    _depths[k2]             = _histogram[k2]->entropyInBits(_last_pct_of_bits)
                              +1;  // experimentally suggested
    if (_histogram[k2]->failed()) {
      _error_status = _histogram[k2]->errorStatus ();
      return;
    }
    if (_depths[k2] > _tree_depth)
      _tree_depth = _depths[k2];
  }
  _cube_root->_pixel_count = _count; // intialize the pixel count for root

// determine some shift arrays that can be used to encode data values not
//   covered in the analysis using getCodeFromDataPoint
  _l_shifts = new int[_nchans];
  _r_shifts = new int[_nchans];
  if (!(_l_shifts && _r_shifts)) {
    _error_status = NT_MEMORY_ALLOCATION_ERROR;
    return;
  }
  int nbits = (int) (16. / (float)_nchans);
  if (nbits > 8) nbits = 8;
  int mbits = 8 - nbits;
  int tbits = 16;
  for (k2 = 0; k2 < _nchans-1; k2++) {
    _l_shifts[k2] = tbits - nbits;
    _r_shifts[k2] = mbits;
    tbits -= nbits; 
  }
  _l_shifts[_nchans-1] = 0;
  if (tbits < 16)
    _r_shifts[_nchans-1] = 8 - tbits;
  else
    _r_shifts[_nchans-1] = 0;  // handle the single channel trivial case

  _error_status = NT_SUCCESSFUL;
}

// Destructor
NTree::~NTree ()
{
// delete the N-tree nodes
  deleteNodes ();
// delete each of the histogram objects
  if (_histogram) {
    for (short k2 = 0; k2 < _nchans; k2++)
      if (_histogram[k2]) delete _histogram[k2];
  }
// delete arrays
  if (_depths) delete [] _depths;
  if (_histogram) delete [] _histogram;
  if (_l_shifts) delete [] _l_shifts;
  if (_r_shifts) delete [] _r_shifts;
  if (_look_up_table) delete [] _look_up_table;
  if (_unique_codes) delete [] _unique_codes;
}

int NTree::classify ()
{
  unsigned char *s = _data;

// figure a sampling of the source data
  long k3 = _count, s_incr = 1, k4;
  for (k4 = 0; (k4 < _count) && (k3 > _max_count_classified); k4++) {
    s_incr++;
    k3 = _count / s_incr;
  }

  k3 = _count / s_incr;
  s_incr *= _nchans;

  return classifyPoints (s, k3, s_incr, _vlu_dsp);
}

// classify a set of data points
int NTree::classifyPoints (unsigned char *s, long count, long s_incr,
  unsigned char vlu_dsp)
{
  do {

// Prune one level if the vector tree is too large.

    if (_tree_node_count > _max_tree_node_count) {
      pruneLevel (_cube_root); 

// Seek to reduce the deepest allowable tree depth down one while reducing
//   the same percent entropy for each channel

      unsigned char old_tree_depth = _tree_depth;
      float pct_of_bits;
      for (pct_of_bits = _last_pct_of_bits;
        (pct_of_bits > 0.0) && (_tree_depth >= old_tree_depth);
        pct_of_bits -= 0.05) {
        _tree_depth = 0;
        for (short k2 = 0; k2 < _nchans; k2++) {
          _depths[k2] = _histogram[k2]->entropyInBits(pct_of_bits);
          if (_depths[k2] > _tree_depth)
            _tree_depth = _depths[k2];
        }
      }
      if (!(pct_of_bits > 0)) {
        _error_status = NT_UNEXPECTED_RESULT;
        return 0;
      }
      _last_pct_of_bits = pct_of_bits;
      _times_exceeded_max_nodes++;
    }

// Start at the root and proceed level by level.
    
    _node = _cube_root;
    for (unsigned char level = 1; level <= _tree_depth; level++) {

// If nothing on level, create new level.

      if (_node->_child == 0) {

// Create new level.

        _next = new NTreeNode (_nchans, level, _vlu_dsp);
        if (!_next) {
          _error_status = NT_MEMORY_ALLOCATION_ERROR;
          return 0;
        }
        if (_next->failed()) {
          _error_status = _next->errorStatus ();
          return 0;
        }
        _tree_node_count++;
        if (_tree_node_count > _highest_tree_node_count)
          _highest_tree_node_count = _tree_node_count;
        _next->_parent = _node;
        _node->_child  = _next;

// Define this first node so it will contain the current vector.

        for (short k2 = 0; k2 < _nchans; k2++) {

// If a channel has already reached its maximum tree depth then
//   simply inherit the previous mins and maxs

          if (_depths[k2] < level) {
            _next->_mins[k2] = _node->_mins[k2];
            _next->_maxs[k2] = _node->_maxs[k2];
	  }
          
          else {

// Establish new mins and maxs

//            unsigned char mid = _histogram[k2]->probableValueByRange
//              (_node->_mins[k2], _node->_maxs[k2], 0.5);
            unsigned char mid = _histogram[k2]->midPointOfRange
              (_node->_mins[k2], _node->_maxs[k2]);
            if (*(s+k2) <= mid) {
              _next->_mins[k2] = _node->_mins[k2];
              _next->_maxs[k2] = mid;
            }
            else {
              _next->_mins[k2] = mid + 1;
              _next->_maxs[k2] = _node->_maxs[k2];
            }
          }
        }
      }

// Assign current node to the child

      _node = _node->_child;
      while (_node != 0) {

// Search sibling at this level to see if the data vector will fit therein

        _previous = _node;
        short is_in_there = 1;
        for (short k2 = 0; (k2 < _nchans) && is_in_there; k2++)
          is_in_there = (*(s+k2) >= _node->_mins[k2]) &&
                        (*(s+k2) <= _node->_maxs[k2]);

// Break the while loop if the vector is wholly contained in the node

        if (is_in_there) break;

// Otherwise look through the brothers and sisters

        _node = _node->_sibling;
      }

// If no node was found to contain this vector, create a new sibling.

      if (_node == 0) {
        _node = new NTreeNode (_nchans, level, _vlu_dsp);
        if (!_node) {
          _error_status = NT_MEMORY_ALLOCATION_ERROR;
          return 0;
        }
        if (_node->failed()) {
          _error_status = _node->errorStatus ();
          return 0;
        }
        _tree_node_count++;
        if (_tree_node_count > _highest_tree_node_count)
          _highest_tree_node_count = _tree_node_count;
        _previous->_sibling = _node;
        _previous = _previous->_parent;
        _node->_parent = _previous;

// Define this sibbling node so it will contain the current vector.

        for (short k2 = 0; k2 < _nchans; k2++) {

// If a channel has already reached its maximum tree depth then
//   simply inherit the previous mins and maxs

          if (_depths[k2] < level) {
            _node->_mins[k2] = _previous->_mins[k2];
            _node->_maxs[k2] = _previous->_maxs[k2];
	  }
          
          else {  

// Establish new mins and maxs

//            unsigned char mid = _histogram[k2]->probableValueByRange
//              (_previous->_mins[k2], _previous->_maxs[k2], 0.5);
            unsigned char mid = _histogram[k2]->midPointOfRange
              (_previous->_mins[k2], _previous->_maxs[k2]);
            if (*(s+k2) <= mid) {
              _node->_mins[k2] = _previous->_mins[k2];
              _node->_maxs[k2] = mid;
            }
            else {
              _node->_mins[k2] = mid + 1;
              _node->_maxs[k2] = _previous->_maxs[k2];
            }
          }
        }
      }

// Current node now points to an existing node representing the pixel's vector.
//   increment the pixel count for this node

      _node->_pixel_count++;
    }

// New leaf.  Increment the count of the number of pixels whose vector
//   is not represented at a lower depth in the tree.

    _node->_pixel_count_not_lower++;

    if (_node->_vlu_dsp) {
      if ((long)_node->_vlu_dsp == AVERAGE && (long)vlu_dsp == AVERAGE)
// sum currnt pnt
        for (short k2 = 0; k2 < _nchans; k2++) _node->_vlus[k2] += *(s+k2);

      else if (_node->_vlu_dsp && (long)vlu_dsp == SELECT) {
// select currnt pnt
        for (short k2 = 0; k2 < _nchans; k2++) _node->_vlus[k2] = *(s+k2);
        _node->_vlu_dsp = (unsigned char)SELECT;
// from now on only allow pnt selection
      }
    }

    count--;     // count down the number of input pixel vectors.
    if (_do_abort) {
      if (!(count % (long)1000)) {
	if (_do_abort->userAbort()) {
          _error_status = NT_USER_ABORTED;
          return 0;
        }
      }
    }
    s += s_incr; // increment to next pixel vector.
  } 
  while (count > 0);

  _error_status = NT_SUCCESSFUL;
  return 1;
}

// Repeatedly prune the N tree until the number of nodes whose pixel count
//   is greater than zero [of vectors that are not represented lower in the
//   tree] is less than or equal to the maximum number of vectors allowed in
//   the output image.
//
// On any given iteration over the tree, select those nodes
//   whose pixel count [of vectors contained in the N cube represented by
//   the nodes] are minimal for pruning and merge their vector statistics
//   upward.
//
int NTree::reduce (int number_vectors_allowed,
  unsigned char *vectors)
{

// Reduce the classification tree until the number of nodes with vectors that
//   are not represented lower in the tree is less than or equal to the
//   allowed number of vectors in the output image.  Return the actual
//   number of vectors allocated.

  _max_count = number_vectors_allowed;
  _pruning_threshold = 0;  // The first time through, only get an actual count
                           //   subsequent times through you will have the
                           //   smallest group size
  do {
    _actual_count = 0;
    _next_pruning_threshold = MAX_INTEGER;
    reduceSub (_cube_root);
    _pruning_threshold = _next_pruning_threshold;
    _reduce_iteration_count++;
    if (_do_abort) {
      if (_do_abort->userAbort()) {
        _error_status = NT_USER_ABORTED;
        return 0;
      }
    }
  }
  while (_actual_count > (long)number_vectors_allowed);

// Initialize vectors LUT array.

  _vectors = vectors;  // set the internal LUT pointer equal to external
  _actual_count = 0;
  createVectorLUT (_cube_root);
  computeUniqueLUTCount (vectors);
  return (int)_actual_count;
}

// if the vector is unique and there are still LUT entry locations
//   available, insert the given vector into the N-tree.  it is assumed that
//   the N-tree has already been "reduce"d.  the intent of this routine is
//   to build a resultant N-tree out to the maximum count possible.  further
//   "reduce"ing to the N-tree would be less valid because the statistical
//   significance of the N-tree is compromised in this operation
int NTree::insertVector (const unsigned char *vector, unsigned char *vectors,
  unsigned char fxd_vlu)
{
// check if any more vectors are allowed
  if (_actual_count >= _max_count && _max_count > 0) {
    _error_status = NT_SUCCESSFUL;
    return 0;
  }

// check to see if unique codes have been determined
  if (!_unique_codes) {
    _error_status = NT_SUCCESSFUL;
    return 0;
  }

// check if incoming vector is unique
  if (pointEqualToLUTEntry(vector,vectors)) {
    _error_status = NT_SUCCESSFUL;
    return 0;
  }

// find the closest node associated with the incoming vector
  unsigned char code = getCodeFromDataPoint (vector, vectors, !SELECT);

// check to make sure that the code is unique
  if (!codeUnique(code)) {
    _error_status = NT_SUCCESSFUL;
    return 0;
  }

  NTreeNode *node = getNodeFromCode (vector, code);

// check to make sure that it is legal to change the values for the node found
  if (node->_fxd_vlu) {
    _error_status = NT_SUCCESSFUL;
    return 0;
  }

// check to make sure that the node found has no child which contains a code
//   or points to siblings that could have codes.  this is because intuition
//   would indicate that splitting an existing node could mess up searches
//   that progress downward
  if (node->_child != 0)
    if (node->_child->_pixel_count_not_lower > 0 ||
      node->_child->_sibling != 0) {
      _error_status = NT_SUCCESSFUL;
      return 0;
    }

// check to make sure that the vector is completely contained between the
//   look-up-table vector and the minimum boundary or between the
//   look-up-table vector and the maximum boundary
  _vectors = &(vectors[node->_code*_nchans]);
  int in_min_side = 1;
  int k2;
  for (k2 = 0; k2 < _nchans && in_min_side; k2++)
    in_min_side = (vector[k2] >= node->_mins[k2] &&
                   vector[k2] <= _vectors[k2]      );

  int in_max_side = 1;
  if (in_min_side) in_max_side = 0;
  for (k2 = 0; k2 < _nchans && in_max_side; k2++)
    in_max_side = (vector[k2] >= _vectors[k2]    &&
                   vector[k2] <= node->_maxs[k2]   );

  if (!(in_min_side || in_max_side)) {
    _error_status = NT_SUCCESSFUL;
    return 0;
  }

// figure out how to split the mins and maxs to store incoming vector
  float temp, upper_diff = 0, extrema_diff = 0;
  for (k2 = 0; k2 < _nchans; k2++) {
    temp = node->_maxs[k2] - vector[k2];
    upper_diff += temp * temp;
    temp = node->_maxs[k2] - node->_mins[k2];
    extrema_diff += temp * temp;
  }

  upper_diff   = (float) sqrt ((double)upper_diff);
  extrema_diff = (float) sqrt ((double)extrema_diff);

// create a new sibling node to contain the incoming vector in such a way
//   as to minimize the exposure of the new node to data assignments.
//   this is done because no statistics have been used to create the new node.
  int new_vector_unique = 0;
  if (extrema_diff <= 0) return 0;  // this should not happen but don't abort
  float node_fraction = upper_diff / extrema_diff;
  float pknt   = (float) node->_pixel_count;
  float pkntnl = (float) node->_pixel_count_not_lower;

  float average;
  long ltmp;
  if (in_min_side) {
// don't let the new sibling node contain the incoming vector
    NTreeNode *sibling = new NTreeNode (_nchans, node->_level, node->_vlu_dsp);
    if (!sibling) {
      _error_status = NT_MEMORY_ALLOCATION_ERROR;
      return 0;
    }

    sibling->_parent = node->_parent;
    sibling->_sibling = node->_sibling;
    ltmp = (long)(node_fraction * pknt + 0.5);
    sibling->_pixel_count = ltmp > (long)1 ? ltmp : (long)1;
    ltmp = (long)(node_fraction * pkntnl + 0.5);
    sibling->_pixel_count_not_lower = ltmp > (long)1 ? ltmp : (long)1;
    sibling->_code = node->_code;

    if (!node->_vlu_dsp) {
      node->_vlus = new float[_nchans];
      if (!node->_vlus) {
        _error_status = NT_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
      node->_vlu_dsp = SELECT;
    }

    _actual_count++;
    node->_code = (unsigned char)_actual_count - 1;
    node->_fxd_vlu = fxd_vlu;
    ltmp = (long)pkntnl - sibling->_pixel_count_not_lower;
    node->_pixel_count_not_lower = ltmp > (long)1 ? ltmp : (long)1;
    node->_sibling = sibling;
    ltmp = (long)pknt - sibling->_pixel_count;
    node->_pixel_count = ltmp > (long)1 ? ltmp : (long)1;

    for (k2 = 0; k2 < _nchans; k2++) {
      sibling->_maxs[k2] = node->_maxs[k2];

      if (!new_vector_unique              &&
          vector[k2] < sibling->_maxs[k2] &&
          vector[k2] < _vectors[k2]         ) {
        sibling->_mins[k2] = vector[k2] + 1;  // ensure vector not in sibling
        new_vector_unique = 1; // make the minimum change possible
      }
      else
        sibling->_mins[k2] = vector[k2];

      if (sibling->_vlu_dsp == AVERAGE) {
        average = node->_vlus[k2] / pkntnl;
        sibling->_vlus[k2] = (float)sibling->_pixel_count_not_lower * average;
      }
      else if (sibling->_vlu_dsp == SELECT) {
        if (node->_vlus[k2] < sibling->_mins[k2])
          sibling->_vlus[k2] = sibling->_mins[k2];
        else
          sibling->_vlus[k2] = node->_vlus[k2];
      }

      node->_maxs[k2] = vector[k2];
      node->_vlus[k2] = vector[k2];
    }
  }
  else /* if (in_max_side) */ {
// let the new sibling node contain the incoming vector
    NTreeNode *sibling = new NTreeNode (_nchans, node->_level, SELECT);
    if (!sibling) {
      _error_status = NT_MEMORY_ALLOCATION_ERROR;
      return 0;
    }

    sibling->_parent = node->_parent;
    sibling->_sibling = node->_sibling;
    ltmp = (long)(node_fraction * pknt + 0.5);
    sibling->_pixel_count = ltmp > (long)1 ? ltmp : (long)1;
    ltmp = (long)(node_fraction * pkntnl + 0.5);
    sibling->_pixel_count_not_lower = ltmp > (long)1 ? ltmp : (long)1;
    sibling->_fxd_vlu = fxd_vlu;

    _actual_count++;
    sibling->_code = (unsigned char)(_actual_count - 1);
    ltmp = (long)pkntnl - sibling->_pixel_count_not_lower;
    node->_pixel_count_not_lower = ltmp > (long)1 ? ltmp : (long)1;
    node->_sibling = sibling;
    ltmp = (long)pknt - sibling->_pixel_count;
    node->_pixel_count = ltmp > (long)1 ? ltmp : (long)1;

    for (k2 = 0; k2 < _nchans; k2++) {
      sibling->_vlus[k2] = vector[k2];
      sibling->_maxs[k2] = node->_maxs[k2];
      sibling->_mins[k2] = vector[k2];

      if (!new_vector_unique         &&
        vector[k2] > node->_mins[k2] &&
        vector[k2] > _vectors[k2]      ) {
        node->_maxs[k2] = vector[k2] - 1; // ensure vector not in node
        new_vector_unique = 1;  // make the minimum change necessary
      }
      else
        node->_maxs[k2] = vector[k2];

      if (node->_vlu_dsp == AVERAGE) {
        average = node->_vlus[k2] / pkntnl;
        node->_vlus[k2] = (float)node->_pixel_count_not_lower * average;
      }
      else if (node->_vlu_dsp == SELECT) {
        if (node->_vlus[k2] > node->_maxs[k2])
          node->_vlus[k2] = node->_maxs[k2];
      }
    }
  }

  _vectors = &(vectors[(_actual_count-1)*_nchans]);
  for (k2 = 0; k2 < _nchans; k2++)
    _vectors[k2] = vector[k2];
  _unique_codes[_actual_count-1] = (unsigned char)(_actual_count - 1);
  _unique_count++;

  _error_status = NT_SUCCESSFUL;
  return 1;
}

// Generate the encoded image from the input data.  The encoded image
//   is made up of pixels where each pixel is an index into the vector
//   look-up-table array.  It is assumed that the N-Tree has a code
//   for every input data point.
int NTree::assignUsingTree (const unsigned char *data, long count,
  unsigned char *encoded_data)
{
  unsigned char *s = (unsigned char *)data;
  unsigned char *q = encoded_data;
  long i = count;
  short is_in_there, k2;

// Loop through the entire input image

  while (i > 0) {

// Start at the root of the tree

    _previous = _cube_root;
    _node = _cube_root;

// Go down through as many nodes as is necessary

    do {
      _node = _node->_child; 

// Find the node where the input data vector is located

      do {
        is_in_there = 1;
        for (k2 = 0; (k2 < _nchans) && is_in_there; k2++)
          is_in_there = (*(s+k2) >= _node->_mins[k2]) &&
                        (*(s+k2) <= _node->_maxs[k2]);

// Break the do loop if the data vector is wholly contained in the current
//   node and set the current node to the previous node

        if (is_in_there) {
          _previous = _node;
          break;
	}

// Otherwise look through the siblings

        _node = _node->_sibling;
      }

// Keep this up until you have examined every sibling

      while (_node != 0);

// If you have selected a sibling then break the loop

      if (_previous != _node)
        break;  
    }

// Keep this up until you have gone past the lowest possible level in the tree

    while (_node->_child != 0);

// Assign the code of the previous node to the output encoded image

    *q = _previous->_code;

// Increment the counters and pointers

    i--;
    if (_do_abort) {
      if (!(i % (long)1000)) {
	if (_do_abort->userAbort()) {
          _error_status = NT_USER_ABORTED;
          return 0;
        }
      }
    }
    s += _nchans;   // increment to next pixel vector.
    q++;
  }
  return 1;
}

// Generate the encoded image from the input data.  The encoded image
//   is made up of pixels where each pixel is an index into the vector
//   look-up-table array.  The vector LUT may not have a code for every
//   input data point.
int NTree::assign (const unsigned char *data, long count,
  unsigned char *vectors, unsigned char *encoded_data)
{
  unsigned char *s = (unsigned char *)data;
  unsigned char *q = encoded_data;
  long i = count;

  unsigned char vlu_dsp;  
  if (LUTEntriesAreUnique())
    vlu_dsp = 0;                      // no need to do anymore selection
  else
    vlu_dsp = (unsigned char)SELECT;  // strive for unique entries

// Loop through the entire input image

  while (i > 0) {

// Assign the output encoded image pixel to an index in the vectors
//   look-up-table array

    *q = getCodeFromDataPoint (s, vectors, vlu_dsp);

// Increment the counters and pointers

    i--;
    if (_do_abort) {
      if (!(i % (long)1000)) {
	if (_do_abort->userAbort()) {
          _error_status = NT_USER_ABORTED;
          return 0;
        }
      }
    }
    s += _nchans;   // increment to next pixel vector.
    q++;
  }
  return 1;
}

// Given a multi-channeled data point, return an encoded data value that
//   is in a distance sense as close as possible to an existing
//   classified vector.  The look_up_table array is initialized with all
//   negative values and is of size 65536 (i.e. 0 to 65535).
// As an option (vlu_dsp = SELECT), adjustments can be made to the
//   classified vector to ensure uniqness of the vector.
unsigned char NTree::getCodeFromDataPoint (const unsigned char *data,
  unsigned char *vectors, unsigned char vlu_dsp)
{
  int is_in_there, k2;
// Search through the classification tree for the coded value in hopes that
//   the given data point was in the data already classified

// Start at the root of the tree

  _previous = _cube_root;
  _node = _cube_root;

// Go down through as many nodes as is necessary

  do {
    _node = _node->_child; 

// Find the node where the input data vector is located

    do {
      is_in_there = 1;
      for (k2 = 0; (k2 < _nchans) && is_in_there; k2++)
        is_in_there = (data[k2] >= _node->_mins[k2]) &&
                      (data[k2] <= _node->_maxs[k2]);

// Break the do loop if the data vector is wholly contained in the current
//   node and set the current node to the previous node

      if (is_in_there) {
        _previous = _node;
        break;
      }

// Otherwise look through the siblings

      _node = _node->_sibling;
    }

// Keep this up until you have examined every sibling

    while (_node != 0);

// If you have selected a sibling then break the loop

    if (_previous != _node)
      break;  
  }

// Keep this up until you have gone past the lowest possible level in the tree

  while (_node->_child != 0);

// If the data point was encapsulated in the classification tree where a
//   valid vector entry is located, return the code

  if (_previous->_pixel_count_not_lower > 0) {
    if ((long)vlu_dsp != SELECT || !_unique_codes)
      return _previous->_code;
    else {

// this attempts to ensure uniqueness of each LUT entry
      int code = (int)_previous->_code;
      if (_unique_codes[code] != code) { // attempt to make LUT more unique
        if (!pointEqualToLUTEntry(data,(const unsigned char *)vectors)) {
          _vectors = &(vectors[code*_nchans]);
          for (k2 = 0; k2 < _nchans; k2++)
            _vectors[k2] = data[k2];
          _unique_codes[code] = code;
          _unique_count++;
        }
      }
      else if (_allow_insert_vectors && insertVector(data,vectors))
// attempt to maximize LUT
        code = getCodeFromDataPoint (data, vectors, !SELECT); // get new code
      return (unsigned char)code;
    }
  }

// At this point it is obvious that the data point was not found in the
//   classification tree, see if a previous data point that was hashed down
//   to encompass that previous data point resembles the current data point
//   hashed value and is stored in the look-up-table array
  unsigned short hash_value = 0;
  for (k2 = 0; k2 < _nchans; k2++)
    hash_value |= (data[k2] >> _r_shifts[k2]) << _l_shifts[k2];

// create a coded look-up-table if one has not already been created
  if (!_look_up_table) {
    _look_up_table = new short[65536];
    if (!_look_up_table) {
      _error_status = NT_MEMORY_ALLOCATION_ERROR;
      return (unsigned char)0;
    }
    for (k2 = 0; k2 < 65536; k2++)
      _look_up_table[k2] = -1;
  }

// with this hashed value, look and see, if a code has been stored yet
  short code = _look_up_table[hash_value];

// positive values indicate that a code has previously been stored in the
//   look up table for a data point giving the current hashed result
  if (code >= 0)
    return (unsigned char) code;

// having failed the two earlier attempts, search through each value
//   in the vector table to find the one closest in a distance sense to
//   the current data point
  unsigned char *v = (unsigned char *)vectors;
  long min_distance = MAX_INTEGER, chan_distance, distance;
  int k3, count = (int)_actual_count;
  for (k2 = 0; k2 < count; k2++) {
    distance = 0;
    for (k3 = 0; k3 < _nchans; k3++) {
      chan_distance = (long) *v;
      distance += chan_distance * chan_distance;
      v++;
    }
    if (distance < min_distance) {
      min_distance = distance;
      code = (short) k2; // the codes are simply the sequential vector numbers
    }
  }

// record the code associated with the minimum vector distance at the
//   computed hash value for the current data point and return the code
  _look_up_table[hash_value] = code;
  return (unsigned char) code;
}

// Decode an encoded image
int NTree::decode (const unsigned char *encoded_data,
  long count, unsigned char *decoded_data, const unsigned char *vectors)
{
  unsigned char *q = (unsigned char *)encoded_data;
  unsigned char *d = decoded_data;
  long i = count;

// Loop through the entire encoded image

  while (i > 0) {

// Determine the index into the look-up-table

    short index = *q * _nchans;

// Transfer the vector into the decoded image

    for (short k2 = 0; k2 < _nchans; k2++) {
      *d = vectors[index];
      index++;
      d++;
    }

// Increment the counters and pointers

    i--;
    if (_do_abort) {
      if (!(i % (long)1000)) {
	if (_do_abort->userAbort()) {
          _error_status = NT_USER_ABORTED;
          return 0;
        }
      }
    }
    q++;
  }
  return 1;
}

// Compute the coding errors between an input image and an encoded image
int NTree::codingError (const unsigned char *data,
  long count, const unsigned char *encoded_data,
  const unsigned char *vectors, float *ave_err_per_pxl,
  float *norm_ave_err_per_pxl, float *norm_max_err)
{
  unsigned char *s = (unsigned char *)data;
  unsigned char *q = (unsigned char *)encoded_data;
  long i = count;

  float distance, x, total_error = 0;
  float maximum_error = 0;

// Loop through the entire input image

  while (i > 0) {

// Determine the index into the look-up-table

    short index = *q * _nchans;

// Find the difference between the input image and the decoded image

    distance = 0;
    for (short k2 = 0; k2 < _nchans; k2++) {
      x = *s - vectors[index];
      distance += x * x;
      index++;
      s++;
    }
    total_error += distance;
    if (distance > maximum_error)
      maximum_error = distance;

// Increment the counters and pointers

    i--;
    if (_do_abort) {
      if (!(i % (long)1000)) {
	if (_do_abort->userAbort()) {
          _error_status = NT_USER_ABORTED;
          return 0;
        }
      }
    }
    q++;
  }

// compute final error statistics.

  *ave_err_per_pxl = total_error / count;
  *norm_ave_err_per_pxl =
    *ave_err_per_pxl / (_nchans * MAX_INTENSITY * MAX_INTENSITY);
  *norm_max_err = maximum_error / (_nchans * MAX_INTENSITY * MAX_INTENSITY);
  return 1;
}

void NTree::printStatistics()
{
  cout << "Actual number of vectors quantized:  " << _actual_count
       << endl;
  cout << "Final percent of data entropy allowed:  " << _last_pct_of_bits
       << endl;

  char chr[1];
  for (short k2 = 0; k2 < _nchans; k2++) {
    sprintf (chr, "%d", _depths[k2]);
    cout << "Final tree depth for channel # "<< k2+1 << ":  " << chr
         << endl;
  }
  cout << "Final number of nodes in the NTree:  " << _tree_node_count
       << endl;
  cout << "Highest number of nodes in the NTree:  " << _highest_tree_node_count
       << endl;
  cout << "Minimum number allowed in a single node:  "
       << _pruning_threshold << endl;
  cout << "Number of times exceeded maximum nodes allowed:  "
       << _times_exceeded_max_nodes << endl;
  cout << "Number of iterations required to get to # of vectors quantized:  "
       << _reduce_iteration_count << endl;
  
}

int NTree::computeUniqueLUTCount (unsigned char *vectors)
{
  unsigned char equal;
  int k2, k3, repeat_count, test = 0, count;

  count = (int)_actual_count;

// remove old unique codes array
  if (_unique_codes) delete [] _unique_codes, _unique_codes = 0;

  int *temp1 = new int[_max_count];
  if (!temp1) {
    _error_status = NT_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

// check for the situation where the current vector points to an earlier code
//   but the earlier code points to yet an earlier code!
//   i.e. when the determination of uniqueness is not self-consistent
  int code;
  for (k2 = 0; k2 < count; k2++) {
    code = (int) getCodeFromDataPoint (&(vectors[k2*_nchans]),
      (unsigned char *)vectors, !SELECT);
    if (code < k2 && temp1[code] != code) {
// since the current vector would successfully point to an earlier code when
//   in fact that earlier code's vector failed to point to its own self, do a
//   swap
      swapLUTEntries (vectors, (unsigned char)code, (unsigned char)k2);
      temp1[k2] = temp1[code];
      temp1[code] = code;
    }
    else
      temp1[k2] = code;
  }

// sort the unique codes array into a temporary array
  int *temp2 = new int[count];
  for (k2 = 0; k2 < count; k2++)
    temp2[k2] = temp1[k2];

  int itmp;
  for (k2 = 0; k2 < count; k2++)
    for (k3 = k2+1; k3 < count; k3++)
      if (temp2[k2] > temp2[k3]) {
        itmp = temp2[k2];
        temp2[k2] = temp2[k3];
        temp2[k3] = itmp;
      }

// determine the unique count
  _unique_count = count;
  for (k2 = 0; k2 < count; k2++) {
    repeat_count = 0;
    equal = 1;
    for (k3 = k2+1; k3 < count && equal; k3++) {
      equal = temp2[k2] == temp2[k3];
      if (equal) repeat_count++;
    }
    if (repeat_count > 0) {
      if (test)
        printf ("LUT entry #%d is repeated %d times\n", k2, repeat_count);
      _unique_count--;
    }
  }

  delete [] temp2;

  _unique_codes = new int[_max_count];
  if (!_unique_codes) {
    _error_status = NT_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  for (k2 = 0; k2 < count; k2++)
    _unique_codes[k2] = temp1[k2];

  delete [] temp1;

  if (test)
    printf ("Out of %d, there are %d unique LUT entries\n", _actual_count,
      _unique_count);
  _error_status = NT_SUCCESSFUL;
  return 1;
}

int NTree::LUTEntriesAreUnique ()
{
  return (int)((long)_unique_count == _actual_count);
}

int NTree::pointEqualToLUTEntry (const unsigned char *data,
  const unsigned char *vectors)
{
  unsigned char equal;
  int k2, k3, count = (int)_actual_count;
  for (k2 = 0; k2 < count; k2++) {
    _vectors = (unsigned char *)&(vectors[k2*_nchans]);
    equal = 1;
    for (k3 = 0; k3 < _nchans && equal; k3++)
      equal = data[k3] == _vectors[k3];
    if (equal) return (int)equal;
  }
  return 0;
}

// swap codes and corresponding vector look-up-table and unique id entries
int NTree::swapLUTEntries (unsigned char *vectors, unsigned char code1,
  unsigned char code2)
{
  if (code1 == code2) {
    _error_status = NT_SUCCESSFUL;
    return 1;
  }

  unsigned char *vect1 = &(vectors[(int)((int)code1*(int)_nchans)]);
  unsigned char *vect2 = &(vectors[(int)((int)code2*(int)_nchans)]); 
  NTreeNode *node1 = getNodeFromCode (vect1, code1);
  NTreeNode *node2 = getNodeFromCode (vect2, code2);

  if (node1 == 0 || node2 == 0) {
    _error_status = NT_UNEXPECTED_RESULT;
    return 0;
  }

// remove unique ids array because swap will make it invalid
  if (_unique_codes) delete [] _unique_codes, _unique_codes = 0;

// swap the codes
  unsigned char temp = node1->_code;
  node1->_code = node2->_code;
  node2->_code = temp;

// make the vector look-up-table consistent with the code swap

  for (int k2 = 0; k2 < _nchans; k2++) {
    temp = vect1[k2];
    vect1[k2] = vect2[k2];
    vect2[k2] = temp;
  }

  _error_status = NT_SUCCESSFUL;
  return 1;
}

int NTree::failed ()
{
  return (int) (_error_status != NT_SUCCESSFUL);
}

// deletes all nodes at the bottom level of the vector tree merging their
//   vector statistics into their parent node.
void NTree::pruneLevel (NTreeNode *node)
{

// recursively get down to the current tree depth "nuc" the subtree.

  if (node->_child != 0)
    pruneLevel (node->_child);
  if (node->_sibling != 0)
    pruneLevel (node->_sibling);
  if (node->_level == _tree_depth)
    pruneSubtree (node);
}

// Delete the given node and all of its children.
//   Steps in deleting an individuual node are:
//
//   o  Delete all children.
//   o  Merge vector statistics into parent node.
//   o  If the node is first on sibling list, update parent's child pointer
//        to the next sibling.
//   o  Delete the node from its sibling list.
//
void NTree::pruneSubtree (NTreeNode *node)
{

// Prune all children of this node. 

  while (node->_child != 0)
    pruneSubtree (node->_child);

// Merge vector statistics into parent. 

  _predecessor = node->_parent;

  int k2;
  if ((long)_predecessor->_vlu_dsp == AVERAGE) {
    if ((long)node->_vlu_dsp == AVERAGE) // both child & parent are summing
      for (k2 = 0; k2 < _nchans; k2++)
        _predecessor->_vlus[k2] += node->_vlus[k2];
    else if ((long)node->_vlu_dsp == SELECT) { // child vlu slct'd but not prnt
      for (k2 = 0; k2 < _nchans; k2++)
        _predecessor->_vlus[k2] = node->_vlus[k2]; // accept the child as is
      _predecessor->_vlu_dsp = (unsigned char)SELECT;
// only select point from now on
    }
  }

  else if (((long)_predecessor->_vlu_dsp == SELECT) &&
           (              node->_vlu_dsp == SELECT) &&
           ((node->_pixel_count_not_lower + node->_pixel_count_not_lower) >
            _predecessor->_pixel_count)               )
// Both child and parent have selected values.  If the child is more
//   significant than the parent then use the child's values.  The child is
//   deemed to be more significant than the parent if it has more than twice
//   the number of counts not represented lower to itself as compared to what
//   the parent has in the entire N-cube that the parent represents.
//   Otherwise, retain the parent's selected values.
        for (k2 = 0; k2 < _nchans; k2++)
          _predecessor->_vlus[k2] = node->_vlus[k2]; // accept the child as is

  _predecessor->_pixel_count_not_lower += node->_pixel_count_not_lower;

// Remove node from its sibling list or from its parent's child.

  _next = node->_sibling;
  _previous = _predecessor->_child;

// If node is head of sibling list, change parent's child pointer

  if (_predecessor->_child == node) {

// Make parent's child point to the next sibling.

    _predecessor->_child = _next;
  }
  else {

// Find node's predecessor on sibling list and queue around node.

    while (_previous->_sibling != node)
      _previous = _previous->_sibling;
    _previous->_sibling = _next;
  }
  _tree_node_count--;
  delete node, node = 0;     // I think that now that either a) the parent's
                             //   child is no longer node, so node can be
                             //   freed, or b) the sibling pointers have
                             //   queued around node, so node can be
                             //   freed.  I believe delete was not in the
                             //   original code because they used a "Page
                             //   class" to allocate and free memory.
                             //   The present code has moved away from "Page".
                             //   It could well be that the "Page" approach
                             //   would be a much smarter way to manage
                             //   memory, i.e. keep things from getting
                             //   too fragmented.
}

// Traverse the vector description tree.  Each node in the tree that has
//   the number of vectors not represented lower in the tree greater than
//   zero is placed in the vectors look-up-table.
void NTree::createVectorLUT (NTreeNode *node)
{
  short k2;

// Traverse any children or siblings.

  if (node->_child != 0)
    createVectorLUT (node->_child);
  if (node->_sibling != 0)
    createVectorLUT (node->_sibling);
  if (node->_pixel_count_not_lower > 0) {

// Node is a vectors look-up-table array entry.

    if (!node->_vlu_dsp) {

// Determine the median value within the node for each channel.

      for (k2 = 0; k2 < _nchans; k2++) {
        *_vectors = _histogram[k2]->probableValueByRange
          (node->_mins[k2], node->_maxs[k2], 0.5);
        _vectors++;
      }
    }

    else if ((long)node->_vlu_dsp == AVERAGE) {

// The values stored are a sum to be averaged by the count in order to
//   determine what the vectors look-up-table will use

      for (k2 = 0; k2 < _nchans; k2++) {
        *_vectors = (unsigned char)(node->_vlus[k2]
                    / node->_pixel_count_not_lower + 0.5);
        _vectors++;
      }
    }

    else /* if ((long)node->_vlu_dsp == SELECT) */ {

// The values stored are precisely what the vectors look-up-table will use

      for (k2 = 0; k2 < _nchans; k2++) {
        *_vectors = (unsigned char)node->_vlus[k2];
        _vectors++;
      }
    }

    node->_code = (unsigned char)_actual_count++;
  }
}

// Select those nodes whose whose pixel count [of vectors contained in the
//   N cube represented by the nodes (n1)] are minimal for pruning and merge
//   their vector statistics upward.  Use a pruning threshold (np) to govern
//   node selection as follows:
//
//     Prune all nodes such that n1 <= np.
//     Set np to minimum n1 for remaining nodes.
void NTree::reduceSub (NTreeNode *node)
{

// Reduce any children or siblings.

  if (node->_child != 0)
    reduceSub (node->_child);
  if (node->_sibling != 0)
    reduceSub (node->_sibling);
                                           // I don't understand this
                                           // unless shift is always negative?
  int shift = _tree_depth - node->_level;  // ?why divide by 2 when node level
                                           // is less than depth. 
  if (node->_pixel_count <= (_pruning_threshold >> shift)) {

// Node has a sub_threshold pixel count.  Increment actual pixel vector
//   count if node has a nonzero number of pixels whose vector is not
//   represented at lower levels and its parent does not.  Prune the node and
//   its children since the parent already has a low number.

    if ((node->_pixel_count_not_lower > 0) &&
      (node->_parent->_pixel_count_not_lower == 0))
      _actual_count++;  // I don't understand this since you are pruning nxt!
    pruneSubtree (node);
  }
  else {

// Increment pixel vector count if node has a nonzero number of pixels whose
//   vector is not represented at lower levels.  Find the minimum pruning
//   threshold.

    if (node->_pixel_count_not_lower > 0)
      _actual_count++;
    if (node->_pixel_count < (_next_pruning_threshold >> shift))
      _next_pruning_threshold=node->_pixel_count << shift;  // set up for nxt
  }
}

// Delete the N tree one level at a time until its gone
void NTree::deleteNodes ()
{

  if (!_cube_root) return;

// delete levels 1 to the tree depth

  unsigned char old_tree_depth = _tree_depth;
  for (unsigned char k2 = old_tree_depth; k2 > 0; k2--) {
    _tree_depth = k2;
    pruneLevel (_cube_root);
  }

// delete the tree root

  delete _cube_root, _cube_root = 0;

}

// Given a code, return the N-Tree Node associated with it.  If one can not
//   be found, return NULL.
NTreeNode *NTree::getNodeFromCode (const unsigned char *data,
  unsigned char code)
{
  int is_in_there, contains_code = 0, k2;

// Check for the case that the code is located at the root.  Be careful
//   to make sure that the cube root is a valid node
  if (_cube_root->_pixel_count_not_lower > 0) {
    if (_cube_root->_code == code) return _cube_root;
  }

// First find the location in the classification tree where the data is
//   located

// Start at the root of the tree

  _previous = _cube_root;
  _node = _cube_root;

// Go down through as many nodes as is necessary

  do {
    _node = _node->_child; 

// Find the node where the input data is located

    do {

// See if the data is contained within this node
      is_in_there = 1;
      for (k2 = 0; (k2 < _nchans) && is_in_there; k2++)
        is_in_there = (data[k2] >= _node->_mins[k2]) &&
                      (data[k2] <= _node->_maxs[k2]);

      if (is_in_there) {
// Check to see if this node also contains the given code and is a valid
//   vector entry
        if (_node->_pixel_count_not_lower > 0)
          contains_code = code == _node->_code;
      }

// Break the do loop if the input data is contained in the current
//   node and set the current node to the previous node

      if (is_in_there) {
        _previous = _node;
        break;
      }

// Otherwise look through the siblings

      _node = _node->_sibling;
    }

// Keep this up until you have examined every sibling

    while (_node != 0);

// If you have selected a sibling or the node containing the code has been
//   found then break the loop

    if (_previous != _node || contains_code)
      break;  
  }

// Keep this up until you have gone past the lowest possible level in the tree

  while (_node->_child != 0);

// If the code was found in the classification tree where a
//   valid vector entry is located, return the code

  if (contains_code)
    return _previous;
  else
    return 0;
}

int NTree::codeUnique (unsigned char code)
{
  if (!_unique_codes) return 0;

  int retval = 1; // start out assuming unique
  int icode = (int) code;
  for (int k2 = 0; k2 < _actual_count && retval; k2++) // when not unique->done
    retval = ((icode == k2 && _unique_codes[k2] == icode) ||
              (icode != k2 && _unique_codes[k2] != icode)   );

  return retval;
}

/*
At some point in the future, you might consider an int (32 bit) version of
a similar class that could save up to a user specified N entries from inputs
from several
different sources.  Because eventually, most displays want no more than 256
entries, you need to encode the incoming attributes 0 to N-1, run a N
level histogram, select the 256 most populous entries determine where the
other used (of the N-256) entries are closest to the selected 256 entries. To
facilitate this, along with the N level look-up-table, store several
other tables that identify for each entry which of the other N entries is
1st closest, 2nd closest, 3rd closest, ....  Also consider storing a vector
length array.  When data comes in, is encoded, and the 256th most populous
entries are determined, any of the encoded values not in the 256 selected
entries can be selected with these auxillary LUT's.  Research will
show how many levels of these "closest" LUT's are needed in order to minimize
most on-the-fly closest vector calculations.

The advantage of this approach is to 1) defer some of the clustering until
later thereby retaining more precision, 2) not have to cluster all of the data
originally, 3) have more generalized data encoding to begin with.

For example note that 2**16 = 2**5 * 2**6 * 2**5.  One could simply have a
single 65536 RGB LUT that could be used to take any colorized result and apply
the above algorithm to.

The disadvantage is that the computer time required to encode data from the N
levels to 256 although interactive is not real-time because the partially
clustered data in general is more that one 8-bit byte requiring a move.
*/
