// tree.cc:  Implementation file for Tree class
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
#include <iostream.h>
#include "dp/tree.hh"

// Constructor
Tree::Tree (const unsigned char *data, const long count,
  const short offset, const long increment):
  _data           ((unsigned char *)data),
  _count          ((long)count),
  _offset         ((short)offset),
  _increment      ((long)increment)
{
  _tree_node_count = 0;
  _cube_root  = new NTreeNode (1, 0);
  if (!_cube_root) {
    _error_status = T_MEMORY_ALLOCATION_ERROR;
    return;
  }
  if (_cube_root->failed()) {
    _error_status = _cube_root->errorStatus ();
    return;
  }
  _tree_depth = TREE_DEPTH;
  _cube_root->_mins[0] = MIN_INTENSITY;
  _cube_root->_maxs[0] = MAX_INTENSITY;
  _cube_root->_pixel_count = _count; // intialize the pixel count for root
  _error_status = T_SUCCESSFUL;
}

// Destructor
Tree::~Tree ()
{
// delete the Tree nodes
  deleteNodes ();
}

int Tree::classify ()
{
  unsigned char *s = _data + _offset;
  long k3 = _count;

  do {

// Start at the root and proceed level by level.
    
    _node = _cube_root;
    for (unsigned char level = 1; level <= _tree_depth; level++) {

// If nothing on level, create new level.

      if (_node->_child == 0) {

// Create new level.

        _next = new NTreeNode (1, level);
        if (!_next) {
          _error_status = T_MEMORY_ALLOCATION_ERROR;
          return 0;
        }
        if (_next->failed()) {
          _error_status = _next->errorStatus ();
          return 0;
        }
        _tree_node_count++;
        _next->_parent = _node;
        _node->_child  = _next;

// Define this first node so it will contain the current value.

// Establish new mins and maxs

        unsigned char mid =
          (unsigned char) (((short)_node->_mins[0] + (short)_node->_maxs[0])
          >> 1);               // half the sum of min and max
        if (*s <= mid) {
          _next->_mins[0] = _node->_mins[0];
          _next->_maxs[0] = mid;
        }
        else {
          _next->_mins[0] = mid + 1;
          _next->_maxs[0] = _node->_maxs[0];
        }
      }

// Assign current node to the child

      _node = _node->_child;
      while (_node != 0) {

// Search sibling at this level to see if the data value will fit therein

        _previous = _node;
        short is_in_there = (*s >= _node->_mins[0]) &&
          (*s <= _node->_maxs[0]);

// Break the while loop if the value is contained in the node

        if (is_in_there) break;

// Otherwise look through the brothers and sisters

        _node = _node->_sibling;
      }

// If no node was found to contain this value, create a new sibling.

      if (_node == 0) {
        _node = new NTreeNode (1, level);
        if (!_node) {
          _error_status = T_MEMORY_ALLOCATION_ERROR;
          return 0;
        }
        if (_node->failed()) {
          _error_status = _node->errorStatus ();
          return 0;
        }
        _tree_node_count++;
        _previous->_sibling = _node;
        _previous = _previous->_parent;
        _node->_parent = _previous;

// Define this sibbling node so it will contain the current value.

// Establish new mins and maxs

        unsigned char mid =
          (unsigned char) (((short)_previous->_mins[0] +
          (short)_previous->_maxs[0])
          >> 1);               // half the sum of min and max
        if (*s <= mid) {
          _node->_mins[0] = _previous->_mins[0];
          _node->_maxs[0] = mid;
        }
        else {
          _node->_mins[0] = mid + 1;
          _node->_maxs[0] = _previous->_maxs[0];
        }

      }

// Current node now points to an existing node representing the pixel's value.
//   increment the pixel count for this node

      _node->_pixel_count++;
    }

// New leaf.  Increment the count of the number of pixels whose value
//   is not represented at a lower depth in the tree.

    _node->_pixel_count_not_lower++;
    k3--;              // count down the number of input pixel values.
    s += _increment;   // increment to next pixel.
  } 
  while (k3 > 0);

  _error_status = T_SUCCESSFUL;
  return 1;
}

// Return values array, and counts array for the leaf nodes
int Tree::getLeafNodes (unsigned char *values, long *counts)
{
  _values = values;  // init the internal values pointer equal to external
  _counts = counts;  // init the internal counts pointer equal to external
  _actual_count = 0;
  _only_count = 0;
  if (!(createUnsortedLeafNodes (_cube_root))) return 0;
  _values = values;  // reinit the internal values pointer equal to external
  _counts = counts;  // reinit the internal counts pointer equal to external
  sortValuesAndCounts (0, (short)_actual_count-1);
  _error_status = T_SUCCESSFUL;;
  return 1;
}

// Return the size of the values and counts arrays
short Tree::getNumberOfLeafNodes ()
{
  _actual_count = 0;
  _only_count = 1;
  if (!(createUnsortedLeafNodes (_cube_root))) return (short)0;
  return _actual_count;
}

// Traverse the value description tree.  Each node in the tree that has
//   the number of values not represented lower in the tree greater than
//   zero is placed in the values array.
int Tree::createUnsortedLeafNodes (NTreeNode *node)
{
// Traverse any children or siblings.

  if (node->_child != 0)
    if (!(createUnsortedLeafNodes (node->_child))) return 0;
  if (node->_sibling != 0)
    if (!(createUnsortedLeafNodes (node->_sibling))) return 0;
  if (node->_pixel_count_not_lower > 0) {

// Node is a values array entry.  Store value and count.

    if (!_only_count) {
      if (!(node->_mins[0] == node->_maxs[0])) {
        _error_status = T_BAD_NODE_DATA;
        return 0;
      }
      *_values = node->_mins[0];  // min and max should always equal at this
                                  // level!
      _values++;
      *_counts = node->_pixel_count;
      _counts++;
    }
    _actual_count++;
  }
  _error_status = T_SUCCESSFUL;
  return 1;
}

// Sort the leaf node arrays
void Tree::sortValuesAndCounts (short left, short right)
{
  short k2, last;

  if (left >= right) // return if array is trivial
    return;
  swapValues (left, (left + right) >> 1);
  swapCounts (left, (left + right) >> 1);
  last = left;
  for (k2 = left+1; k2 <= right; k2++)
    if (compareValues (_values[k2], _values[left]) < 0) {
      swapValues (++last, k2);
      swapCounts (  last, k2);
    }
  swapValues (left, last);
  swapCounts (left, last);
  sortValuesAndCounts (left,   last-1);
  sortValuesAndCounts (last+1, right);
}

short Tree::compareValues (unsigned char value1, unsigned char value2)
{
  if (value1 < value2)
    return -1;
  else if (value1 > value2)
    return 1;
  else
    return 0;
}

void Tree::swapValues (short k2, short k3)
{
  unsigned char temp = _values[k2];
  _values[k2]        = _values[k3];
  _values[k3]        = temp;
}


void Tree::swapCounts (short k2, short k3)
{
  long temp   = _counts[k2];
  _counts[k2] = _counts[k3];
  _counts[k3] = temp;
}

// Delete the N tree one level at a time until its gone
void Tree::deleteNodes ()
{

// delete levels 1 to the tree depth

  unsigned char old_tree_depth = _tree_depth;
  for (unsigned char k2 = old_tree_depth; k2 > 0; k2--) {
    _tree_depth = k2;
    pruneLevel (_cube_root);
  }

// delete the tree root

  delete _cube_root;

}

// deletes all nodes at the bottom level of the value tree
void Tree::pruneLevel (NTreeNode *node)
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
//   o  If the node is first on sibling list, update parent's child pointer
//        to the next sibling.
//   o  Delete the node from its sibling list.
void Tree::pruneSubtree (NTreeNode *node)
{

// Prune all children of this node. 

  while (node->_child != 0)
    pruneSubtree (node->_child);

  _predecessor = node->_parent;

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
  delete node;
}

void Tree::printStatistics()
{
  cout << "Actual number of values:  " << _actual_count
       << endl;
  cout << "Final number of nodes in the Tree:  " << _tree_node_count
       << endl;
}

int Tree::failed ()
{
  return (int) (_error_status != T_SUCCESSFUL);
}
