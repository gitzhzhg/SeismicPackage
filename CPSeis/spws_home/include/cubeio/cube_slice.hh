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
#ifndef _cube_slice_h
#define _cube_slice_h

#include "cubeio/scube.hh"

//- CubeSlice
class CubeSlice : public SimpleCube { 
//. Class derived from SimpleCube to hold a 2-D plane of data
//. of type T. This class always owns its data.

public:
   CubeSlice(int axis, int index,int esize, int n1, int n2);
  ~CubeSlice();

  inline int axis();
   //. Indicates the cube axis number of the slice normal.

  inline int index();
   //. Indicates the position of the slice along the cube axis.
   //. Index starts counting at 0.

  inline void set_index(int i);
   //. Sets the slice index along the cube axis.

  void set_axis(int i);
   //. Set the axis number(1, 2, or 3) for the slice.
   //. The slice normal should point along this axis.


protected:
   int axis_;     // axis 1,2, or 3 of the cube
   int index_;    // index of the slice along the axis

private:
};

  inline int CubeSlice::axis() { return axis_; }
  inline int CubeSlice::index() { return index_; }
  inline void CubeSlice::set_index(int index) { index_ = index; }
  inline void CubeSlice::set_axis(int axis) { axis_ = axis; }


#endif
