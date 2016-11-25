#include <string.h>
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
#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/file.h>
#include "cubeio/cube_slice.hh"
#include "cubeio/cubeio.hh"
#include "cubeio/pencil.hh"
#include "wrdcnvrt.h"
#include "tfdefs.h"
#include "tf3d.h"

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

void build_syn_trace(float dt, int nt,Pt3Df *src, Pt3Df *rec,
     int np, Pt3Df *dif, float *tr, float *hd, float *lav);
int cube_3dfft(CubeIO *ccube, int w1, int w2);

#ifdef __cplusplus
}                   // for C++
#endif

int  main(unsigned int argc, char **argv)

{ CubeIO *ccube=0;
  CubeSlice *cslice1, *cslice2, *cslice3;
  Grid3DDesc *h;
  Pt3Di N,br,bl;
  Pt3Df *dif, src,rec;
  char  *dbuf;

  int   wdtyp=WIEEE, ftyp = BRICK_TYPE;
  int   l1=16,l2=8,l3=8,nchan=1;
  int   n1=200,n2=160,n3=100;
  int   X=2,Y=3,Z=1;
  float o1=0.0,o2=30.,o3=100.0;
  float d1=0.008,d2=12.5,d3=25.0;
  char  lab1[32],lab2[32],lab3[32];
  char  hfile[80],dfile[80],pname[16],oname[16];

  int i, ndif=3, bytes_per_element=4;
  int woff;

// Create arrays to hold a synthetic trace and header
  float lav,*trace,*hd;
  int num=n1;
  trace = new float[n1];
  hd    = new float[64];
  strcpy(lab1,"TIME");
  strcpy(lab2,"XBASEMENT");
  strcpy(lab3,"YBASEMENT");
  strcpy(pname,"Seismic");
  strcpy(oname,"TestBrick");
  strcpy(hfile,"newcube.br");
  strcpy(dfile,"newcube.brdat");
  h = tf3d_create_desc(hfile , dfile, pname, oname,
      o1,o2,o3,n1,n2,n3,
      d1,d2,d3,
      lab1,lab2,lab3,
      X,Y,Z, ftyp, wdtyp);
  br.v[0]= l1; br.v[1]=l2; br.v[2]= l3;
  tf3d_setBrick(h,&br);
  bl.v[0]= (n1-1)/l1 +1;
  bl.v[1]= (n2-1)/l2 +1;
  bl.v[2]= (n3-1)/l3 +1;
  tf3d_setBlock(h,&bl);

// Create coordinates for ndif point difractors
  dif = new Pt3Df[ndif];
  dif[0].v[0]=250.0;
  dif[0].v[1]=o2;
  dif[0].v[2]=o3;
  dif[1].v[0]=350.0;
  dif[1].v[1]=o2+ 600.0;
  dif[1].v[2]=o3+ 1250.0;
  dif[2].v[0]=300.0;
  dif[2].v[1]=o2;
  dif[2].v[2]=o3+2000.0;
// Set the dimensions of the cube we want to generate
 N.v[0]=n1;
 N.v[1]=n2;
 N.v[2]=n3;
// Create a char cube to hold the data on disk
 if(wdtyp==WBYTE) bytes_per_element=1;
 ccube = new CubeIO(dfile, N.v, br.v,bytes_per_element,true, nchan);

// Write the header file to disk.
  if(ccube->cube_wrhdr( h) ==0) {
    printf("error writing header file\n");
    exit(1);
  }

// Create a cslice to hold a line of traces
 cslice1 = new CubeSlice(3,0,ccube->esize(),n1,n2);
// Create a series of pseudo lines
 src.v[0] = 0.0;            //source z coordinate
 rec.v[0] = 0.0;            //receiver z coordinate
 for(int i3=0;i3<n3;i3++) { //loop over cross line index
    src.v[2] = o3 + i3*d3;  //source y coordinate
    rec.v[2] = src.v[2];    //receiver y coordinate
    dbuf = cslice1->data(); //data buffer for in line slice
    for(int i2=0;i2<n2;i2++) { //loop over in line index
      src.v[1] = o2 + i2*d2;   //source x coordinate
      rec.v[1] = src.v[1];     //receiver x coordinate

      // Generate a syntehtic trace
      build_syn_trace(d1, n1,&src, &rec, ndif, dif, trace, hd, &lav);
      // store trace in buffer dbuf
      woff = i2*n1*ccube->esize();;
      if(ccube->esize()==1) {
       float_to_byt_(trace,&num,(unsigned char *) (dbuf+woff), &num, &lav);
      } else {
       memcpy(dbuf+woff,trace,n1*ccube->esize());
      }
    }
     
    // compute the integer line index
    float f_index = (hd[17]-o3)/d3;
    int line_index  = (f_index+0.1);
    if(line_index>=0 && line_index < n3) {
      cslice1->set_index(line_index);
      ccube->slice_wr(cslice1); //Write the slice to the cube
    }
 }
 delete cslice1;

 printf("start ffts\n");
 i = cube_3dfft(ccube, 0, 60);
// Recall a slice
 cslice1 = ccube->slice_rd(1,5);
 cslice1->print();
 cslice2 = ccube->slice_rd(2,45);
 cslice2->print();
 cslice3 = ccube->slice_rd(3,15);
 cslice3->print();
 delete []trace;
 delete []hd;
 delete cslice1;
 delete cslice2;
 delete cslice3;
 delete ccube;
 exit(0);
}

int cube_3dfft(CubeIO *ccube, int w1, int w2)
{ int n1,n2,n3,i1,i2,i3;
  int i,j,off,nw=w2-w1+1;
  float *dbuf;
  CubeSlice *cslice=0;
 if(!ccube) return -1;
 if(ccube->esize() != sizeof(float)) return -1;
 n1 = ccube->grid_points(1);
 if(2*nw > n1) return -1; //not enough room to store frquencies
// Read the cube back in for 3D fft
 float *fftbuf = new float[2*4096];
 int   n1_fft = 256;
// trace and inline ffts
 for(i3=0;i3<ccube->grid_points(3);i3++) {
   printf("fft for slice=%d\n",i3);
   cslice = ccube->slice_rd(3,i3);
   dbuf = (float *) cslice->data();   //data buffer for in line slice
   n1 = cslice->length(1);  // trace length
   n2 = cslice->length(2);  // number of traces
   // fft over n1 and n2
   for(i2=0;i2<n2;i2++) { // loop over traces in slice
     // get a trace
     for(i=0;i<2*n1_fft;i++) fftbuf[i]=0.0;
     memcpy(fftbuf,dbuf+i2*n1,n1*cslice->esize());
     // call fft routine to transform fftbuf
     // put some freq. back into slice
     memcpy(dbuf+i2*n1,fftbuf+2*w1,2*nw*cslice->esize());
   }
   for(i1=0;i1<nw;i1++) {
     off = 2*i1;
     for(j=0;j<n2;j+=2) {
       fftbuf[j]   = dbuf[off];
       fftbuf[j+1] = dbuf[off+1];
       off += n1;
     }
    // call fft routine
    // put data back to buffer
     off = 2*i1;
     for(j=0;j<n2;j+=2) {
       dbuf[off] = fftbuf[j];
       dbuf[off+1] = fftbuf[j+1];
       off += n1;
     }
   }
   ccube->slice_wr(cslice);
   delete cslice;
 }
// cross line ffts
   Pencil *P;
   int A[3],L[3],axis=3,it,ix,p_off=0,p_num;
   float *trace;
   A[0]=0; L[0]=ccube->sub_cube_size(1);
   A[1]=0; L[1]=ccube->sub_cube_size(2);
   A[2]=0; L[2]=ccube->sub_cube_size(3);
   P = new Pencil(ccube->cube_blocks(3),axis,
       A,L, ccube->esize(),1);
   p_num = ccube->cube_block_size();
   for(it=0;it<ccube->cube_blocks(1);it++) { // blocks along axis-1
     for(ix=0;ix<ccube->cube_blocks(2);ix++) { // blocks along axis-2
       A[0] = it; A[1] = ix; A[2] = 0;
       P->set_index(A);
       ccube->cube_rd_p(P,p_off,p_num);
       P->permute();
       printf("fft for it=%d ix=%d\n",it,ix);
       for(i=0;i<P->trace_count();i++) { // loop over vec. in pencil
         trace = (float *) P->get_trace(i);
       }
     }
   }
}

void build_syn_trace(float dt, int nt,Pt3Df *src, Pt3Df *rec,
  int np, Pt3Df *dif, float *tr, float *hd, float *lav)
{
// output traces and headers
 float  d1,d2,d3, v0=1500, t=0, r=0;
 double d;
 int    i,index;

 *lav=0;
 if(!src || !rec || dt<=0) return;
 if(np<1) return;
 for(i=0;i<nt;i++) tr[i]=0.0;

 hd[12] = src->v[0];
 hd[10] = src->v[1];
 hd[11] = src->v[2];

 hd[15] = rec->v[0];
 hd[13] = rec->v[1];
 hd[14] = rec->v[2];

 hd[16] = 0.5*(hd[10] + hd[13]);
 hd[17] = 0.5*(hd[11] + hd[14]);
 hd[18] = 0.5*(hd[12] + hd[15]);

 for(i=0;i<np;i++) { // loop over difractors
   r=0;
   t=0;
   d1 = src->v[0]-dif[i].v[0];
   d2 = src->v[1]-dif[i].v[1];
   d3 = src->v[2]-dif[i].v[2];
   d  = d1*d1 + d2*d2 + d3*d3;
   d  = sqrt(d);
   r  += d;
   t  += d/v0;

   d1 = rec->v[0]-dif[i].v[0];
   d2 = rec->v[1]-dif[i].v[1];
   d3 = rec->v[2]-dif[i].v[2];
   d  = d1*d1 + d2*d2 + d3*d3;
   d  = sqrt(d);
   r  += d;
   t  += d/v0;

   index = t/dt;
   if(index<nt) {
     tr[index] += 1.0/r;
     tr[index-1] += 1.0/r;
   }
 }
 *lav = fabs(tr[0]);
 for(i=0;i<nt;i++)
   *lav = (fabs(tr[i]) > *lav) ? fabs(tr[i]) : *lav;

}
