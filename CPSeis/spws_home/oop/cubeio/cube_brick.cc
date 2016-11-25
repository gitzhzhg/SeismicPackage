#include "cubeio/cube_brick.hh"
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
#include <stdio.h>

/*****************************************************************************
 * Methods for the CubeBrick class
 *****************************************************************************/

CubeBrick::CubeBrick(
   int A[], int L[] ,int esize, char *data)
: SimpleCube(L,esize,data)  {
// Set up the Brick parameters
   index1_ = A[0];
   index2_ = A[1];
   index3_ = A[2];
}

CubeBrick::~CubeBrick() { 
}

void CubeBrick::set_index(int I, int axis) {
   if(axis==1) index1_ = I;
   if(axis==2) index2_ = I;
   if(axis==3) index3_ = I;
}

int CubeBrick::index(int axis) {
//. return index for axis direction = axis
 if(axis==1) return index1_;
 if(axis==2) return index2_;
 if(axis==3) return index3_;
 return 0;
}

void CubeBrick::print() {
//. print out the Brick parameters
 printf( "Brick: n1=%d     n2=%d      n3=%d\n",l1_,l2_,l3_);
 printf( "Brick: index1=%d index-2=%d index-3=%d\n",
        index1_,index2_,index3_);
 if(owns_data_)
  printf("Brick: owns its data\n");
 else
  printf("Brick: does not own its data\n");
}

