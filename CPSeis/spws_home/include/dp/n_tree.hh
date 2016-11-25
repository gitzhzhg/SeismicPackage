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
// n_tree.hh:  User interface for class NTree
#ifndef NTREE_HH
#define NTREE_HH

#include "dp/n_tree_node.hh"
#include "dp/n_tree_error_codes.hh"

#define MAX_COUNT            32 * 1024
#define MAX_NODES            512 * 528
#define MAX_INTENSITY        255
#define MIN_INTENSITY	     0
#define MAX_INTEGER          0x7fffffff

class NTreeNode;
class Histogram;
class DoAbort;

class NTree {

public:
  NTree						// constructor
    (const unsigned char *data,			//   vector image
     long count,				//   number of vector pixels
     unsigned char nchans,			//   number of vector elements
     long max_nodes = -1,			//   maximum tree node count
     long max_count_classified = -1,		//   max data count classified
     unsigned char vlu_dsp = 0,			//   value disposition flag
     int allow_insert_vectors = 0,		//   enter 1-automatic inserts
     DoAbort *do_abort = 0);			//   object to allow user abort
     
  ~NTree ();					// destructor

  int classify ();				// classify the vector image

  int classifyPoints				// classify a given data set
    (unsigned char *s,				//   pointer to data set array
     long count,				//   number of points to clssfy
     long s_incr,				//   increment btwn data points
     unsigned char vlu_dsp);			//   value disposition flag

  int reduce				        // reduce the vector tree
    (int number_vectors_allowed,	        //   maximum size of vector LUT
     unsigned char *vectors);			//   vector look-up-table

  int insertVector				// rtn 1-able to insert vector
    (const unsigned char *vector,		//   vector to try to insert
     unsigned char *vectors,			//   vector look-up-table
     unsigned char fxd_vlu = 0);		//   1-make this a fixed vlu

  int assignUsingTree				// make encoded image
    (const unsigned char *data,			//   pointer to input data
     long count,				//   size of input data array
     unsigned char *encoded_data);		//   pointer to encoded data

  int assign					// make encoded image
    (const unsigned char *data,			//   pointer to input data
     long count,				//   size of input data array
     unsigned char *vectors,			//   vector look-up-table
     unsigned char *encoded_data);		//   pointer to encoded data

  unsigned char getCodeFromDataPoint		// rtn a code given a data pnt
    (const unsigned char *data,			//   multi-channel data point
     unsigned char *vectors,			//   vector look-up-table
     unsigned char vlu_dsp);			//   point value disposition

  int decode					// decode an encoded image
    (const unsigned char *encoded_data,		//   pointer to input data
     long count,				//   size of input data array
     unsigned char *decoded_data,		//   pointer to decoded data
     const unsigned char *vectors);		//   vector look-up-table

  int codingError				// determine coding error
    (const unsigned char *data,			//   pointer to input data
     long count,				//   size of input data array
     const unsigned char *encoded_data,		//   pointer to encoded data
     const unsigned char *vectors,		//   vector look-up-table
     float *ave_err_per_pxl,			//   average error per pixel
     float *norm_ave_err_per_pxl,		//   normalized ^
     float *norm_max_err);			//   normalized maximum error
						//    normalized =
						//     value / (nchans*255*255)

  void printStatistics();			// print out process stats

  int computeUniqueLUTCount			// finds # of unique LUT entrys
    (unsigned char *vectors);			//   vector look-up-table

  int LUTEntriesAreUnique ();			// rtn 1 if all LUT entrys uniq

  int pointEqualToLUTEntry			// rtn 1-data pnt is a LUT pnt
    (const unsigned char *data,			//   pointer to input data
     const unsigned char *vectors);		//   vector look-up-table

  int swapLUTEntries				// swap codes and LUT entries
    (unsigned char *vectors,			//   vector look-up-table
     unsigned char code1,			//   1st code & entry to swap
     unsigned char code2);			//   2nd code & entry to swap

  int failed ();				// return 1 if unsuccessful

  NTreeErrorCodes errorStatus ()		// return error status flag
    { return _error_status; }
  
private:
  void pruneLevel (NTreeNode *node);		// prune a level in the NTree

  void pruneSubtree (NTreeNode *node);		// prune a subtree

  void createVectorLUT (NTreeNode *node);	// create vector LUT

  void reduceSub (NTreeNode *node);		// subfunction for reduce

  void deleteNodes ();				// delete the tree nodes

  NTreeNode *getNodeFromCode			// return node containing code
    (const unsigned char *data,			//   pointer to input data
     unsigned char code);			//   input code

  int codeUnique				// rtrns 1 if for sure unique
    (unsigned char code);			//   code to test uniqueness on

  NTreeNode
    *_cube_root,				// root of tree
    *_node,					// current node
    *_next,					// next node
    *_previous,					// previous node
    *_predecessor;				// parent node

  DoAbort
    *_do_abort;					// object to allow user abort

  int
    _allow_insert_vectors,			// 1 to auto insert vec to max
    _unique_count,				// # of unique pixel vectors
    *_l_shifts,					// left shift array for hashing
    *_r_shifts,					// right shift array for hashin
    *_unique_codes;				// array of unique codes

  long
    _count,					// number of pixel vectors
    _tree_node_count,				// number of tree nodes
    _pruning_threshold,				// prune nodes with vectors <
    _next_pruning_threshold,			// next pruning threshold
    _max_count_classified,			// max data count classified
    _max_tree_node_count,			// maximum number of nodes
    _highest_tree_node_count,			// highest number of nodes
    _times_exceeded_max_nodes,			// times exceeded max_tree_nod
    _reduce_iteration_count,			// times had to reduce down
    _actual_count,				// final # of pixel vectors
    _max_count;					// max # of pixel vectors

  short
    *_look_up_table;				// encoded hash look-up-table

  float
    _last_pct_of_bits;				// percent of entropy allowed

  unsigned char
    *_data,					// pointer to input image
    *_depths,					// pntr to max depth levels
    *_vectors,					// internal vectors LUT ptr
    _nchans,					// number of channels
    _tree_depth,				// lowest depth of tree
    _vlu_dsp;					// value disposition flag

  Histogram
    **_histogram;				// pointer to histograms array

  NTreeErrorCodes
    _error_status;				// error status flag

};

#endif
