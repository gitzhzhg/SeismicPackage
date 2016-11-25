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
// n_tree_node.hh:  User interface for class NTreeNode
#ifndef N_TREE_NODE_HH
#define N_TREE_NODE_HH

#include "dp/n_tree_error_codes.hh"

class NTreeNode {
  friend class NTree; // This class is intended to be used by NTree
  friend class Tree;  // This class is intended to be used by Tree

public:
  NTreeNode					// constructor
    (unsigned char nchans,			// number of channels in node
     unsigned char level,			// level in tree of node
     unsigned char vlu_dsp=0);			// given vlaue disposition flag

  ~NTreeNode ();				// destructor

  int failed ();				// returns 1 if successful call

  NTreeErrorCodes errorStatus ()		// returns error status flag
    { return _error_status; }

private:
  unsigned char
    _nchans,					// number of channels
    _level,					// current deepest level
    _vlu_dsp,					// vlu disposition flg (0,1,2)
                                                // 0->mdn, 1->avrg, 2->select
    _fxd_vlu,					// 0->OK to change _vlus array
    _code,					// vector code for this node
    *_mins,  			    		// pointer to minimums
    *_maxs;					// pointer to maximums

  long
    _pixel_count,				// number of pixels whose
						//   vector is contained in
						//   the N cube which this
						//   represents
    _pixel_count_not_lower;			// number of pixels whose
						//   vector is not represented
						//   at lower depth in the tree
  float
    *_vlus;					// pointer to values

  NTreeNode
    *_parent,					// pointer to parent node
    *_child,					// pointer to child node
    *_sibling;					// pointer to sibling node

  NTreeErrorCodes
    _error_status;				// error status flag

};

#endif
