// n_tree_node.cc:  Implementation for class NTreeNode
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
#include "dp/n_tree_node.hh"

// Constructor
NTreeNode::NTreeNode (unsigned char nchans, unsigned char level,
  unsigned char vlu_dsp):
  _nchans                 (nchans),
  _level                  (level),
  _vlu_dsp                (vlu_dsp),
  _pixel_count            (0),
  _pixel_count_not_lower  (0),
  _parent                 (0),
  _child                  (0),
  _sibling                (0),
  _mins                   (0),
  _maxs                   (0),
  _vlus                   (0),
  _fxd_vlu                (0)

{

  _mins = new unsigned char[_nchans];
  if (!_mins) {
    _error_status = NTN_MEMORY_ALLOCATION_ERROR;
    return;
  }

  _maxs = new unsigned char[_nchans];
  if (!_mins) {
    _error_status = NTN_MEMORY_ALLOCATION_ERROR;
    return;
  }

  if (_vlu_dsp) {
    _vlus = new float[_nchans];
    if (!_vlus) {
      _error_status = NTN_MEMORY_ALLOCATION_ERROR;
      return;
    }
    for (int k2 = 0; k2 < _nchans; k2++) _vlus[k2] = 0; // clear the array
  }

  _error_status = NTN_SUCCESSFUL;
}

// Destructor
NTreeNode::~NTreeNode ()
{
  if (_mins) delete [] _mins;
  if (_maxs) delete [] _maxs;
  if (_vlus) delete [] _vlus;
}

int NTreeNode::failed ()
{
  return (int) (_error_status != NTN_SUCCESSFUL);
}
