#include "cubeio/pencil.hh"
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
#include "cubeio/scube.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*****************************************************************************
 * Methods for the Pencil class
 *****************************************************************************/

/* Main to test Pencil class
int main()
{int i;
 Pencil *fc;
 int L[3], A[3], esiz=4;
 float tr[80];

 L[0] = 8; L[1] = 8; L[2] = 8;
 A[0] = 0; A[1] = 2; A[2] = 3;
 for(i=0;i<80;i++) tr[i]=i;
 fc = new Pencil(10,1,A,L,esiz,1);
 fc->print();
 for(i=0;i<80;i++)
  fc->set_trace(i,80,tr);
 fc->permute();
 char *bl = fc->get_cube_data(0,0);
 printf("dat=%f %f\n",bl[0],bl[7]);

 delete fc;
 exit(0);
}
*/

Pencil::Pencil(int N, int axis, int *A,
  int *L,int esiz,int opt) : axis_(axis) , N_(N) {
  // opt determines which buffers get allocated

 set_index(A);

 stack_ = new SimpleCube *[N];
 int i;
 for(i=0;i<N;i++)
  {
   stack_[i] = new SimpleCube(L,esiz,0); 
   stack_[i]->set_data(0); // forces memory allocation
  }

 ntraces_=0;
 if(opt<1) return;

 if(axis==1) {
   traces_ = new SimpleCube(N*L[0],L[1],L[2],esiz,0);
   ntraces_= L[1]*L[2];
 }
 if(axis==2) {
   traces_ = new SimpleCube(N*L[1],L[0],L[2],esiz,0);
   ntraces_= L[0]*L[2];
 }
 if(axis==3) {
   traces_ = new SimpleCube(N*L[3],L[1],L[0],esiz,0);
   ntraces_= L[1]*L[0];
 }
 traces_->set_data(0); // forces memory allocation 

}

Pencil::~Pencil() { 
 if(traces_) delete traces_;
 if(stack_)  delete [] stack_;
}

char *Pencil::get_trace(int i, int j) { 
// return pointer to the (i,j)th trace.
 if(!traces_) return 0;
 int li = traces_->length(2);
 int lj = traces_->length(3);
 int off= traces_->length(1)*( i + j*li)*traces_->esize();
 if(i < 0 || i > li-1) return 0;
 if(j < 0 || j > lj-1) return 0;
 return traces_->data() + off;
}

char *Pencil::get_trace(int nth) { 
// return pointer to the nth trace.
 if(!traces_) return 0;
 int li = traces_->length(2);
 int lj = traces_->length(3);
 int off= traces_->length(1)*traces_->esize()*nth;
 if(nth < 0 || nth > li*lj-1) return 0;
 return traces_->data() + off;
}

void Pencil::set_trace(int i, int j,int wdo, char *trace) { 
// set wdo elements of the (i,j)th trace
 char * buf;
 if(!traces_) return;
 int li = traces_->length(2);
 int lj = traces_->length(3);
 int off= traces_->length(1)*traces_->esize()*( i + j*li);
 if(i < 0 || i > li-1) return;
 if(j < 0 || j > lj-1) return;
 buf = traces_->data() + off;
 memcpy(buf,trace,wdo*traces_->esize());
}

void Pencil::set_trace(int nth,int wdo, char *trace) { 
// set wdo elements of the nth trace
 char * buf;
 if(!traces_) return;
 int li = traces_->length(2);
 int lj = traces_->length(3);
 int off= traces_->length(1)*traces_->esize()*nth;
 if(off < 0 || nth > li*lj-1) return;
 buf = traces_->data() + off;
 memcpy(buf,trace,wdo*traces_->esize());
}

void Pencil::set_cube_data(int n,int woff, int wdo, char *dat) { 
// set wdo elements of cube n starting at woff
 if(n<0 || n> N_-1) return;
 if(!stack_[n] ) return;
 memcpy(stack_[n]->data()+woff*stack_[0]->esize(),dat,wdo*stack_[0]->esize());
}

char *Pencil::get_cube_data(int n,int woff) { 
// return pointer to element of cube n and offset woff
 if(n<0 || n> N_-1) return 0;
 if(!stack_[n] ) return 0;
 return stack_[n]->data()+woff*stack_[n]->esize();
}

void Pencil::print() {
//. print out the Pencil parameters
 printf( "Pencil: Number of cubes=%d\n",N_);
 printf( "Pencil: Axis orientation=%d\n",axis_);
 printf( "Pencil: size of type=%d\n\n",traces_->esize());
 if(traces_) {
   printf("Pencil: parameters for traces-buffer\n");
   traces_->print();
 }
 if(stack_) {
   printf("\nPencil: parameters for stack-buffer\n");
   stack_[0]->print();
 }
}

/*
 * Permute Pencil Elements. The contents of the stack and
 * trace buffers of the Pencil are swapped.
 * The stack buffer has storage order = disk order
 * The trace buffer has storage order = memory order
 * axis.....pencil axis flag. axis=1,2,3
 * buf1.....buffer holding sub-cubes along axis 1,2, or 3. 
 *          Storage order in a SimpleCube is ==> c(l1xl2xl3) 
 * buf2.....buffer with elements stored as "traces".
 *          axis=1  ===> buf2(N*l1,l2,l3)
 *          axis=2  ===> buf2(N*l2,l1,l3)
 *          axis=3  ===> buf2(N*l3,l2,l1)
 */
int Pencil::permute() { 

  char  t,*buf1,*buf2;
  float ft,*fbuf1,*fbuf2;
  int  wrds,b,l,m, esiz;
  int  i1,i2,i3,ib;
  int  L,l1,l2,l3;

  if(!traces_) return 0;
  if(!stack_) return 0;
  esiz=traces_->esize();
  if(esiz != 1 && esiz != sizeof(float)) return 0;
  wrds = stack_[0]->size();
  l1 = stack_[0]->length(1);
  l2 = stack_[0]->length(2);
  l3 = stack_[0]->length(3);
  L  = traces_->length(1);
  buf2 = traces_->data();
  fbuf2= (float *) buf2;

  l=0;
  if (axis_ == 1) {
    /* traces along axis 1. lv1 x l2 x l3 */
    for (ib = 0; ib < N_; ++ib) {/* sub-cubes along 1 axis */
      buf1 = stack_[ib]->data();
      fbuf1= (float *) buf1;
      for (i3 = 0; i3 < l3; ++i3) {
        m = (i3*l2*L + ib*l1);
        if(esiz==1) {
	  for (i2 = 0; i2 < l2; ++i2) {
            m += i2*L;
            for (i1 = 0; i1 < l1; ++i1, ++l) {
              t = buf2[m + i1];
              buf2[m + i1] = buf1[l];
              buf1[l] = t;
            }
          }
        } else {
	  for (i2 = 0; i2 < l2; ++i2) {
            m += i2*L;
            for (i1 = 0; i1 < l1; ++i1, ++l) {
              ft = buf2[m + i1];
              fbuf2[m + i1] = fbuf1[l];
              fbuf1[l] = ft;
            }
          }
        }
      }
    }
  } else if (axis_ == 3) { 
    /* traces along axis 3. lv3 x l2 x l1 */
    int incm = L * l2;
    for (ib = 0; ib < N_; ++ib) { /* sub-cubes along 3 axis */
      buf1 = stack_[ib]->data();
      fbuf1= (float *) buf1;
      for (i3 = 0; i3 < l3; ++i3) {
        for (i2 = 0; i2 < l2; ++i2) {
          m = (ib*l3 + i2*L + i3);
          l = (ib*wrds + i3*l1*l2 + i2*l1);
          if(esiz==1) {
            for (i1 = 0; i1 < l1; ++i1) {
              t = buf2[m];
              buf2[m] = buf1[l];
              buf1[l] = t;
              m += incm;
              l++;
            }
          } else {
            for (i1 = 0; i1 < l1; ++i1) {
              ft = fbuf2[m];
              fbuf2[m] = fbuf1[l];
              fbuf1[l] = ft;
              m += incm;
              l++;
            }
          }
        }
       }
    }
  } else if (axis_ == 2) {
    /* traces along axis 2. lv2 x l1 x l3 */
    for (ib = 0; ib < N_; ++ib) { /* sub-cubes along 2 axis */
      buf1 = stack_[ib]->data();
      fbuf1= (float *) buf1;
      for (i3 = 0; i3 < l3; ++i3) {
        for (i2 = 0; i2 < l2; ++i2, ++l) {
          m = (i3*L*l1 + i2 + ib*l2);
          l = (ib*wrds + i3*l1*l2 + i2*l1);
          if(esiz==1) {
            for (i1 = 0; i1 < l1; ++i1) {
              t = buf2[m];
              buf2[m] = buf1[l];
              buf1[l] = t;
              m += L;
              l++;
            }
          } else {
            for (i1 = 0; i1 < l1; ++i1) {
              ft = buf2[m];
              fbuf2[m] = fbuf1[l];
              fbuf1[l] = ft;
              m += L;
              l++;
            }
          }
        }
      }
    }
  }

  return 1;
}

