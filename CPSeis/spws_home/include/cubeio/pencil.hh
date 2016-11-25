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
#ifndef _pencil_h
#define _pencil_h

class SimpleCube;

//- Pencil
class Pencil {
//. A Pencil is a stack of SimpleCubes .
//. It is a memory resident object.
//. Two strorage orders are supported
//.   1.  N SimpleCubes of size l1*l2*l3
//.    -or-  a bundle of traces
//.   2.  (N*l1)xl2xl3    .... axis=1
//.       (N*l2)xl1xl3    .... axis=2
//.       (N*l3)xl2xl1    .... axis=3

public:

   Pencil(int N, int axis, int *A,
     int *L,int esize, int opt);
    //. N = number of sub-cubes in pencil.
    //. axis = orientation of the pencil(i.e. 1,2, or 3).
    //. A[3] = triplet address of 1st sub-cube of the pencil.
    //. L[3] = triplet sub-cube size = L[0]*L[1]*L[2] .
    //. esize = number of bytes in each element.
    //. opt > 0 bypasses allocation of trace buffer.

  ~Pencil();

  inline int blocks() { return N_; }
   //. size of cube buffer in cubes.

  inline int axis()   { return axis_; }
   //. indicates the axis orientation of the pencil.

  inline int index(int axis) { return A_[axis]; }
   //. return index of 1st sub-cube for axis 1,2, or 3.

  void set_index(int *A) { A_[0]=A[0]; A_[1]=A[1]; A_[2]=A[2];}
   //. set the 1st sub cube address for all 3 axis.

  virtual void print();
  char *get_trace(int i, int j); 
   //. return pointer to trace(i,j) in pencil.

  char *get_trace(int nth);
   //. return pointer to nth sequential trace in pencil.

  void set_trace(int i, int j,int wdo, char *trace);
   //. copy data to trace(i,j) in pencil.

  void set_trace(int nth,int wdo, char *trace);
   //. copy data to nth trace in pencil.

  void set_cube_data(int n,int woff, int wdo, char *dat);
   //. copy data to nth sub-cube of pencil

  char *get_cube_data(int n,int woff);
   //. return pointer to nth sub-cube of pencil

  int permute();
   //. swap the data in the sub-cube and trace buffers

  int trace_count();
   //. returns the number of traces in the traces buffer

protected:
   int N_;         // Number of Simple Cubes
   int axis_;      // axis orientation of the pencil(1,2,3)
   int A_[3];      // start address of 1st sub-cube.
   int ntraces_;
   SimpleCube **stack_;
   SimpleCube *traces_;

private:
};

inline int Pencil::trace_count() {return ntraces_;}

#endif

