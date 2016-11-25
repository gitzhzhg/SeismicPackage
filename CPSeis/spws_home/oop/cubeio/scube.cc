#include "cubeio/scube.hh"
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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*****************************************************************************
 * Methods for the SimpleCube class
 *****************************************************************************/

/*
int main()
{int i;
 float data[512],*xx,v;
 SimpleCube<float> *sc;
 int L[3];
 L[0]=8; L[1]=8; L[2]=8;

 for(i=0;i<512;i++) data[i]=i;

 sc = new SimpleCube(L,4,data);
 sc->print();
 xx =  (float *) sc->data();
 printf("data value=%f\n",xx[0]);
 
 data[8]=3.45;
 sc->set_data(0,0,0,10,data);
 v =   sc->data(8);
 printf("data value=%f\n",v);

 delete sc;
 exit(0);
}
*/

SimpleCube::SimpleCube(int L[] ,int esize, char *data)
: data_(nil), l1_(1), l2_(1), l3_(1), esize_(esize) {
// Save the simple cube parameters
   l1_ = (L[0] < 1) ? 1 : L[0];
   l2_ = (L[1] < 1) ? 1 : L[1];
   l3_ = (L[2] < 1) ? 1 : L[2];
   esize_ = (esize < 1) ? 1 : esize;
   owns_data_ = false;

   set_data(data);
}

SimpleCube::SimpleCube(int l1, int l2, int l3 ,int esize, char *data)
: data_(nil), l1_(1), l2_(1), l3_(1), esize_(esize) {
// Save the simple cube parameters
   l1_ = (l1 < 1) ? 1 : l1;
   l2_ = (l2 < 1) ? 1 : l2;
   l3_ = (l3 < 1) ? 1 : l3;
   esize_ = (esize < 1) ? 1 : esize;
   owns_data_ = false;

   set_data(data);
}


SimpleCube::~SimpleCube() { 
   if( owns_data_ && data_) delete [] data_;
   data_ = nil ;
}

void SimpleCube::set_data(char *data) {
//. Change the data held by the SimpleCube
//. Will delete old data if it owns the old data.
//. Set pointer to the new data or allocate memory
//. if the input argument is nil.
  if( owns_data_ && data_) {
      delete data_;
      data_ = nil;
  }

   if( data ) {
      data_ = data;
      owns_data_ = false ;
   } else {
      data_ = new char[l1_*l2_*l3_*esize_];
      owns_data_ = true ;
   }
}

void  SimpleCube::set_data(int n, char *d) { 
 memcpy(data(n), d, esize_);
}

void SimpleCube::set_data(int i1, int i2, int i3, int n, char *d) {
  int n0 = i3*l1_*l2_ + i2*l1_ + i1;
  if(data_) memcpy(data_+n0*esize_,d,n*esize_);
}

void SimpleCube::set_data(int woff, int n, char *d) {
//. indices start at 0
  if(data_) memcpy(data_+woff*esize_,d,n*esize_);
}

char *SimpleCube::data(int i1, int i2, int i3) {
//. indices start at (0,0,0)
  int n = i3*l1_*l2_ + i2*l1_ + i1;
  if(data_) return data_+n*esize_;
  else return 0;
}

int SimpleCube::length(int axis) {
//. return the size of SimpleCube in direction=axis
  if(axis==1) return l1_;
  if(axis==2) return l2_;
  if(axis==3) return l3_;
  return 0;
}

SimpleCube *SimpleCube::permute(SimpleCube *in) {
//.  Reorder a(L1,L2,L3) to b(L1,L3,L2)
  int L[3];
  if(!in) return nil;

  char *a = in->data();
  if(!a) return nil;

  L[0] = in->length(1);
  L[1] = in->length(2);
  L[2] = in->length(3);
  SimpleCube *out = new SimpleCube(L,esize_,nil);
  char * b = out->data();
  if( b == nil ) { 
     delete out; 
     return 0;
  }

  int io = 0;
  for (int j2 = 0; j2 < L[1]; ++j2) {
    for (int j3 = 0; j3 < L[2]; ++j3) {
      int ii = j3 * L[0] * L[1] + j2 * L[0];
      memcpy( b +( io )*esize_ , a + (ii )*esize_, L[0]*esize_);;
      io += L[0];
     }
  }
  return out;
}

void SimpleCube::print() {
 printf( "\nSimpleCube: n1=%d n2=%d n3=%d\n",l1_,l2_,l3_);
 printf( "SimpleCube: size of type=%d\n",esize_);
 if(owns_data_)
  printf("SimpleCube: owns its data\n");
 else
  printf("SimpleCube: does not own its data\n");
}
