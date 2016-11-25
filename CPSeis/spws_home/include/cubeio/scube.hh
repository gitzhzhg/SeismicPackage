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
#ifndef _scube_h
#define _scube_h

#include "cubeio/defs.hh"


//- SimpleCube
class SimpleCube {
  //. SimpleCube is a class which defines a cube size and a data pointer.
  //. The data ownership by SimpleCube is optional.

public:

  SimpleCube(int L[],int esize, char *data);
  SimpleCube(int l1,int l2, int l3,int esize, char *data);
  ~SimpleCube();

  void  set_data(char *data);
   //. Set or reset the data pointer.

  void  set_data(int n, char *d); 
   //. Set the nth element of the data object to d, where n>=0.

  void  set_data(int off,int n, char *d);
   //. Set n elements of the data object starting with data[off].

  virtual void  set_data(int i1, int i2, int i3, int n, char *d);
   //. Set n elements of the data object starting from (i1,i2,i3).

  inline char *data();
   //. Return a pointer to the data object.

  char *data(int n);
   //. Return the nth element of the data object where n>=0.

  virtual char *data(int i1, int i2, int i3);
   //. Return element (i1,i2,i3) of the data object.
   //. Indices start from 0.

  inline int size();
   //. Return the number of elements in the data object.

  int esize();
   //. Return the size of 1 element in the data object.

  int length(int axis);
   //. Return the element count along axis 1,2, or 3.

  bool owns_data();
   //. Tells whether SimpleCube owns the data or not. SimpleCube
   //. deletes data that it owns when its destruction operator is invoked.

  void set_ownership(bool b);
   //. Force a setting on the ownership flag for the data object.

  SimpleCube *permute(SimpleCube *in);
   //. Reorder the storage of the data elements.

  virtual void  print();
   //. Print some info about SimpleCube to stdout.

protected:
   int l1_;
   int l2_;
   int l3_;
   int esize_;
   char *data_;
   bool owns_data_;
private:
};

  inline char *SimpleCube::data() { return data_; }
  inline char *SimpleCube::data(int n) { return data_+n*esize_;}
  inline bool SimpleCube::owns_data() { return owns_data_; }
  inline void SimpleCube::set_ownership(bool b) { owns_data_ = b; }
  inline int SimpleCube::size() { return l1_*l2_*l3_; }
  inline int SimpleCube::esize() { return esize_; }

#endif
