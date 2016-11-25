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
#ifndef _cube_slab_h
#define _cube_slab_h

#include "cubeio/scube.hh"

//- CubeSlab
class CubeSlab : public SimpleCube {
   //. Class derived from SimpleCube that is designed to hold
   //. a sequence of slices from a, in general, larger cube.

public:
   CubeSlab(int axis, int I, int esiz, int n1, int n2, int n, char *data);
   ~CubeSlab();

protected:

  int axis();
   //. Indicates the cube axis number of the slab normal.

  int index();
   //. Indicates the start position of the slab along the cube axis.

  void set_index(int i);
   //. Sets the start slab index along the cube axis.

  void set_axis(int i);
   //. Set the axis number(1, 2, or 3) for the slab.
   //. The slab normal should point along this axis.

  int count();
   //. Returns the slab thickness(i.e. the number of slices).

  void set_count(int thick);
   //. Sets the size of the slab thickness.

  int axis_;
  int index_;
  int count_;
     
  private:
};

  inline int CubeSlab::axis() { return axis_; }
  inline int CubeSlab::index() { return index_; }
  inline int CubeSlab::count() { return count_; }
  inline void CubeSlab::set_index(int index) { index_ = index; }
  inline void CubeSlab::set_axis(int axis) { axis_ = axis; }
  inline void CubeSlab::set_count(int thick) { count_ = thick; }

#endif

