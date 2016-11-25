#include <stdio.h>
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
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "cubeio/cube_brick.hh"
#include "cubeio/cube_slice.hh"
#include "cubeio/brick_slice.hh"

BrickSlice::BrickSlice() {
}

BrickSlice::~BrickSlice() {
}

bool BrickSlice::intersects(CubeBrick *b, CubeSlice *s) {
// Check that brick indices are within range of the slice.
// Also check for compatible word sizes.
 int index,axis,I;
 int axis1=0,axis2=1,axis3=2;
 int bl[3],bi[3],n1,n2;
 if(!b) return false;
 if(!s) return false;
 bl[0] = b->length(1);
 bl[1] = b->length(2);
 bl[2] = b->length(3);
 bi[0] = b->index(1);
 bi[1] = b->index(2);
 bi[2] = b->index(3);
 n1 = s->length(1);
 n2 = s->length(2);
 index = s->index();
 axis  = s->axis();
 I = bi[axis-1];

 if(b->esize() != s->esize()) return false;
 if(index< I*bl[axis-1] || index > (I+1)*bl[axis-1]-1) return false;

 if(bi[0]<0 || bi[1]<0 || bi[2]<0) return false;

 switch(axis) {
  case 1:
   if(bi[axis2]*bl[axis2] > n1) return false;
   if(bi[axis3]*bl[axis3] > n2) return false;
   break;
  case 2:
   if(bi[axis1]*bl[axis1] > n1) return false;
   if(bi[axis3]*bl[axis3] > n2) return false;
   break;
  case 3:
   if(bi[axis1]*bl[axis1] > n1) return false;
   if(bi[axis2]*bl[axis2] > n2) return false;
   break;
 default:
   return false;
 }
 return true;
}

// This method involves data motion in memory.
// broadcast data from a sub-cube (I1,I2,I3) into a slice

bool BrickSlice::brick_to_slice(CubeBrick *brick, CubeSlice *slice) {

 char *brick_data=0;
 char *slice_data=0;

   if(!intersects(brick,slice)) return false;
   int esiz = brick->esize();
   int l1 = brick->length(1);
   int l2 = brick->length(2);
   int l3 = brick->length(3);
   int I1 = brick->index(1);
   int I2 = brick->index(2);
   int I3 = brick->index(3);
   int index = slice->index();
   int axis  = slice->axis();
   int n1    = slice->length(1);
   int n2    = slice->length(2);

   if(axis == 1) {
     // slice axis are  along axis 2 & 3 of cube.
      int I = index/l1;
      int i = index - I*l1;
      int so = I3*l3*n1 + I2*l2;
      int lim2 = ((I2+1)*l2 > n1) ?  (I2+1)*l2 - n1 : l2;
      int lim3 = ((I3+1)*l3 > n2) ?  (I3+1)*l3 - n2 : l3;
      for( int i3=0; i3<lim3; i3++) {
         brick_data = brick->data() + (i + i3*l1*l2)*esiz;
         slice_data = slice->data() + (so + i3*n1)*esiz; 
         if(esiz==1) {
          for( int i2=0; i2<lim2; i2++) {
            slice_data[i2] = brick_data[i2*l1];
          }
         } else {
          for( int i2=0; i2<lim2; i2++) {
            memcpy(slice_data, brick_data, esiz);
            brick_data += l1*esiz;
            slice_data += esiz;
          }
         }
      }

      return true;
   }

   if(axis == 2) {
     // slice axis are along axis 1 & 3 of cube.
      brick_data = brick->data();
      // jj = I3*_l3*_n1 + i3*_n1  + I1*_l1 + i1;
      int I = index/l2;
      int i = index - I*l2;
      int so = I3*l3*n1 + I1*l1;
      int lim1 = ((I1+1)*l1 > n1) ?  (I1+1)*l1 - n1 : l1;
      int lim3 = ((I3+1)*l3 > n2) ?  (I3+1)*l3 - n2 : l3;
      brick_data = brick->data() + i*l1*esiz; 
      slice_data = slice->data() + so*esiz;
      for( int i3=0; i3<lim3; i3++) {
         memcpy(slice_data+i3*n1*esiz, brick_data+i3*l1*l2*esiz, lim1*esiz);
      }
      return true;
   }

   if(axis == 3) {
     // slice axis are along axis 1 & 2 of cube.
      //    jj = I1*l1 + i1 + I2*l2*n1 + i2*n1;
      int I = index/l3;
      int i = index - I*l3;
      int so = I2*l2*n1 + I1*l1;
      int lim1 = ((I1+1)*l1 > n1) ?  (I1+1)*l1 - n1 : l1;
      int lim2 = ((I2+1)*l2 > n2) ?  (I2+1)*l2 - n2 : l2;
      brick_data = brick->data() + i*l1*l2*esiz;
      slice_data = slice->data() + so*esiz;
      for( int i2=0; i2 < lim2; i2++) {
         memcpy(slice_data+i2*n1*esiz, brick_data+i2*l1*esiz, lim1*esiz);
      }
      return true;
   }

 return false ;
}
 

// data motion in memory.
// broadcast a brick into a slab
// the slab normal is along axis with dimensions sl[].
// brick address =  I1,I2,I3

bool BrickSlice::brick_to_slab(CubeBrick *brick, int axis,
   char *slab, int sl[]) {
 int   i1,i2,i3,m,ii,m0,inc;
 int   bi[3],bl[3],axis1=0,axis2=1,axis3=2,n1,n2;
 int   lim1,lim2,lim3;
 int   b, esiz;
 char *scube=0;

 if(!brick || !slab) return false;
 scube = brick->data();
 if(!scube) return false;
 // get the brick indices and lengths
 bi[axis1] = brick->index(1);
 bi[axis2] = brick->index(2);
 bi[axis3] = brick->index(3);
 lim1 = bl[axis1] = brick->length(1);
 lim2 = bl[axis2] = brick->length(2);
 lim3 = bl[axis3] = brick->length(3);
 esiz = brick->esize();

 n1   = sl[axis1];
 n2   = sl[axis2];
 if(axis==1) {
   // slab index = I1 ; size = l1_ slices that are n2_ x n3_
   // m = i1*n2_*n3_ + I3*l3_*n2_ + i3*n2_  + I2*l2_ + i2;
    lim1 = sl[axis3];
    if(bl[axis2]*(bi[axis2]+1) > n1) lim2 = n1 - bi[axis2]*bl[axis2];
    if(bl[axis3]*(bi[axis3]+1) > n2) lim3 = n2 - bi[axis3]*bl[axis3];
    inc = n1*n2*esiz;
    m0 = bi[axis2]*bl[axis2] + bi[axis3]*bl[axis3]*n1;
    for( i3=0; i3 < lim3; i3++) {
      for( i2=0; i2 < lim2; i2++) {
       for(b=0;b<esiz;b++) {
          m = b+esiz*(m0 + i3*n2 + i2);
          ii= b+esiz*(i3*bl[axis1]*bl[axis2] + i2*bl[axis1]);
          for( i1=0; i1 < lim1*esiz; i1+= esiz) {
             slab[m] = scube[ii +i1];
             m += inc;
          }
       }
      }
    }
 } else if(axis==2) {
    // slab index = I2 ; size = l2_ slices that are n1_ x n3_
    // m = i2*n1*n3_ + I3*_l3*n1_ + i3*n1_  + I1*_l1 + i;
    lim2 = sl[axis3];
    if(bl[axis1]*(bi[axis1]+1) > n1) lim1 = n1 - bi[axis1]*bl[axis1];
    if(bl[axis3]*(bi[axis3]+1) > n2) lim3 = n2 - bi[axis3]*bl[axis3];
    m0 = bi[axis1]*bl[axis1] + bi[axis3]*bl[axis3]*n1;
    for( i3=0; i3 < lim3; i3++) {
      for(i2=0; i2 < lim2; i2++) {
       for(b=0;b<esiz;b++) {
          m = b+esiz*(m0 + i2*n1*n2 + i3*n1);
          ii= b+esiz*(i3*bl[axis1]*bl[axis2] + i2*bl[axis1]);
          for( i1=0; i1 < lim1*esiz; i1+= esiz) {
             slab[i1 + m] = scube[i1 + ii];
          }
       }
      }
    }
 } else if(axis==3) {
    // slab index = I3 ; size = l3_ slices that are n1_ x n2_
    // m = i3*n2_*n1_ + I1*l1_ + i  + I2*l2_*n1_ + i2*n1_;
    lim3 = sl[axis3];
    if(bl[axis1]*(bi[axis1]+1) > n1) lim1 = n1 - bi[axis1]*bl[axis1];
    if(bl[axis2]*(bi[axis2]+1) > n2) lim2 = n2 - bi[axis2]*bl[axis2];
    m0 = bi[axis1]*bl[axis1] + bi[axis2]*bl[axis2]*n1;
    for( i3=0; i3 < lim3; i3++) {
      for( i2=0; i2 < lim2; i2++) {
       for(b=0;b<esiz;b++) {
          m = b+esiz*(m0 + i3*n2*n1 + i2*n1);
          ii= b+esiz*(i3*bl[axis1]*bl[axis2] + i2*bl[axis1]);
          for( i1=0; i1 < lim1*esiz; i1+= esiz) {
             slab[m+i1] = scube[ii+i1];
          }
       }
      }
    }
 } else {
   return false;
 }
 
 return true;
}

// extract a CubeBrick, from a slab
CubeBrick *BrickSlice::brick_from_slab(int bi[],int bl[],  int axis,
           int esiz, int sl[], char *slab) {
 int   i,i1,i2,i3,m,ii,m0,inc,b;
 int   lim1,lim2,lim3,axis1=0,axis2=1,axis3=2,n1,n2;
 char *scube;

 CubeBrick * brick = new CubeBrick(bi,bl,esiz,nil);
 if(!brick) return nil;
 brick->set_data(nil);
 scube = brick->data();
 lim1 = bl[axis1];
 lim2 = bl[axis2];
 lim3 = bl[axis3];
 n1 = sl[0];
 n2 = sl[1];
 esiz = brick->esize();
 for(i=0;i<esiz*brick->size();i++) { scube[i]=0; }
 if(axis==1) {
    // slab index = I1 ; size = l1_ slices that are n2_ x n3_
    // m = i*n2_*n3_ + I3*l3_*n2_ + i3*n2_  + I2*l2_ + i2;
    lim1 = sl[axis3];
    if(bl[axis2]*(bi[axis2]+1) > n1) lim2 = n1 - bi[axis2]*bl[axis2];
    if(bl[axis3]*(bi[axis3]+1) > n2) lim3 = n2 - bi[axis3]*bl[axis3];
    inc = n1*n2*esiz;
    m0 = bi[axis2]*bl[axis2] + bi[axis3]*bl[axis3]*n1;
    for( i3=0; i3 < lim3; i3++) {
      for( i2=0; i2 < lim2; i2++) {
       for(b=0;b<esiz;b++) {
          m = b+esiz*(m0 + i3*n2 + i2);
          ii= b+esiz*(i3*bl[axis1]*bl[axis2] + i2*bl[axis1]);
          for( i1=0; i1 < lim1*esiz; i1+= esiz) {
             scube[ii+i1] = slab[m];
             m += inc;
          }
       }
      }
    }
    return brick;
 } else if(axis==2) {
      // slab index = I2 ; size = l2_ slices that are n1_ x n3_
      // m = i2*n1_*n3_ + I3*l3_*n1_ + i3*n1_  + I1*l1_ + i1;
    lim2 = sl[axis3];
    if(bl[axis1]*(bi[axis1]+1) > n1) lim1 = n1 - bi[axis1]*bl[axis1];
    if(bl[axis3]*(bi[axis3]+1) > n2) lim3 = n2 - bi[axis3]*bl[axis3];
    m0 = bi[axis1]*bl[axis1] + bi[axis3]*bl[axis3]*n1;
    for( i3=0; i3 < lim3; i3++) {
      for(i2=0; i2 < lim2; i2++) {
       for(b=0;b<esiz;b++) {
          m = b+esiz*(m0 + i2*n1*n2 + i3*n1);
          ii= b+esiz*(i3*bl[axis1]*bl[axis2] + i2*bl[axis1]);
          for( i1=0; i1 < lim1*esiz; i1+= esiz) {
            scube[i1 + ii] = slab[i1 + m];
          }
       }
      }
    }
    return brick;
 } else if(axis==3) {
      // slab index = I3 ; size = l3_ slices that are n1_ x n2_
      // m = i3*n2_*n1_ + I1*l1_ + i1  + I2*l2_*n1_ + i2*n1_;
    lim3 = sl[axis3];
    if(bl[axis1]*(bi[axis1]+1) > n1) lim1 = n1 - bi[axis1]*bl[axis1];
    if(bl[axis2]*(bi[axis2]+1) > n2) lim2 = n2 - bi[axis2]*bl[axis2];
    m0 = bi[axis1]*bl[axis1] + bi[axis2]*bl[axis2]*n1;
    for( i3=0; i3 < lim3; i3++) {
      for( i2=0; i2 < lim2; i2++) {
       for(b=0;b<esiz;b++) {
          m = b+esiz*(m0 + i3*n2*n1 + i2*n1);
          ii= b+esiz*(i3*bl[axis1]*bl[axis2] + i2*bl[axis1]);
          for( i1=0; i1 < lim1*esiz; i1+= esiz) {
             scube[ii+i1] = slab[m+i1];
          }
       }
      }
    }
    return brick;
 }
 delete brick;
 return nil;
}
