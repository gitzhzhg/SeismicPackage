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
#ifndef _cube_brick_h
#define _cube_brick_h

#include "cubeio/scube.hh"

//- CubeBrick
class CubeBrick : public SimpleCube {
   //. A CubeBrick is a SimpleCube that knows its address in a big cube
   //. that is composed of SimpleCube data element blocks.

public:
   CubeBrick(int A[], int n[], int esize,char *data);
  ~CubeBrick();

  int index(int axis);
   //. Return the address of the Brick along the specified axis(1,2, or 3).

  void set_index(int I, int axis);
   //. Set the Brick index for one of the axis.

  virtual void print();
   //. Print some Brick information to stdout.

protected:
   int index1_;   // index of the brick along the axis 1
   int index2_;   // index of the brick along the axis 2
   int index3_;   // index of the brick along the axis 3

private:
};

#endif
