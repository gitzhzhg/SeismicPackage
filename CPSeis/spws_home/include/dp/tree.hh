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
// tree.hh:  User interface for class Tree
#ifndef TREE_HH
#define TREE_HH

#include "dp/n_tree_node.hh"
#include "dp/n_tree_error_codes.hh"

#define TREE_DEPTH      8
#define MIN_INTENSITY   0
#define MAX_INTENSITY 255

class NTreeNode;

class Tree {

public:
  Tree						// constructor
    (const unsigned char *data,			// data values
     const long count,				// number of data values
     const short offset,			// offset into data of 1st val
     const long increment);			// increment from 1st to 2nd

  ~Tree ();					// destructor

  int classify	();				// classify the data values

  short getNumberOfLeafNodes ();		// return number of leaf nodes

  int getLeafNodes				// return leaf node arrays
    (unsigned char *values,			// data values for leaf nodes
     long *counts);				// data counts for leaf nodes

  void printStatistics();			// print out process stats

  int failed ();				// return 1 if unsuccessful

  NTreeErrorCodes errorStatus ()		// return error status flag
    { return _error_status; }
  
private:
  int createUnsortedLeafNodes			// create an unsorted leaf node
    (NTreeNode *node);

  void sortValuesAndCounts			// sort the leaf node arrays
    (short left,				// index to begin sort
     short right);				// inex to end sort

  short compareValues				// compare to values (-1,0,+1)
    (unsigned char value1,			// 1st value to compare
     unsigned char value2);			// 2nd value to compare

  void swapValues				// swap two data values
    (short k2,					// index of 1st value
     short k3);					// index of 2nd value

  void swapCounts				// swap two count values
    (short k2,					// index of 1st count
     short k3);					// index of 2nd count

  void deleteNodes ();				// delete the tree nodes

  void pruneLevel (NTreeNode *node);		// prune a level in the NTree

  void pruneSubtree (NTreeNode *node);		// prune a subtree

  NTreeNode
    *_cube_root,				// root of tree
    *_node,					// current node
    *_next,					// next node
    *_previous,					// previous node
    *_predecessor;				// parent node

  short
    _only_count,				// only count T/F flag
    _offset;					// offset to 1st data value

  long
    _count,					// number of data values
    _tree_node_count,				// number of tree nodes
    *_counts,					// internal counts array ptr
    _increment,					// increment from 1st to 2nd
    _actual_count;				// size of leaf node arrays

  unsigned char
    *_data,					// pointer to input image
    _tree_depth,				// lowest depth of tree
    *_values;					// internal values array ptr

  NTreeErrorCodes
    _error_status;				// error status flag

};

#endif
