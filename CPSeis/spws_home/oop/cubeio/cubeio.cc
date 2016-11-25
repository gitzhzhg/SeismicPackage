#include <unistd.h> /* for access, read */
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
#include <fcntl.h>  /* for open, close */
#include <sys/types.h> 
#include <ctype.h>
#include <limits.h>  /* for _POSIX_PATH_MAX */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "cubeio/cubeio.hh"
#include "cubeio/brick_slice.hh"
#include "cubeio/pencil.hh"
#include "cubeio/cube_brick.hh"
#include "cubeio/cube_slice.hh"
#include "cubeio/dataio.hh"
#include "wrdcnvrt.h"
#include "tfdefs.h"
 

/*****************************************************************************/

CubeIO::CubeIO(
   const char *name, int *C, int *c, int esiz, bool open_now,int nchan) :
   nchan_(nchan), esiz_(esiz)  {
   setup(name,C,c,nchan);
// Open the IO chanels for the data
   if(open_now)
    if(!cube_open(nil)) return;
   cube_prnt();
}

CubeIO::CubeIO( int fd0,
   const char *name, int *C, int *c, int esiz) :
   nchan_(1), esiz_(esiz)  {
   int fd = fd0;
   setup(name,C,c,1);
// Open the IO chanels for the data
   if(!cube_open(&fd)) return;
   cube_prnt();
}

CubeIO::CubeIO(char *header, bool open_now) : nchan_(1) {
   Grid3DDesc *h=0;
   Pt3Di *I,*L;
   char *dfile;
   fio_=nil;
   if(!header) return;
   h = cube_rdhdr(header);
   if(!h) return;
   
   dfile = tf3d_getDataFile(h);
   esiz_ = tf3d_getEsize(h);
   I = tf3d_getN(h);
   L = tf3d_getBrick(h);
   setup(dfile,I->v,L->v,1);
// Open the IO chanels for the data
   if(open_now)
    if(!cube_open(nil)) return;
   cube_prnt();
}

CubeIO::~CubeIO() {

 if(name_) free(name_);
 name_ = 0;
 if( coff_ ) { delete [] coff_; coff_=0; }

 if( fio_ ) { delete [] fio_; }
}

bool CubeIO::isopen() {
 if(!fio_) return false;
 return fio_[0]->open();
}

void CubeIO::setup(const char *name, int *C, int *c, int nchan) {
// Set up the cube parameters
   fio_  = nil;
   nchan_= nchan;
   coff_ = nil ;
   name_ = (char *) malloc(strlen(name)+2);
   strcpy(name_,name);
   if(nchan_<1) nchan_=1;
   n1_ = C[0]; n2_ = C[1]; n3_ = C[2];
   l1_ = c[0]; l2_ = c[1]; l3_ = c[2];
   cube_sl(n1_, n2_, n3_, l1_, l2_, l3_);
}

/*
 * decide how the cube will be partitioned on disk.
 * n1,n2,n3...initial  dimensions          (input)
 * l1,l2,l3...no. of points/sub-block in the 1,2,3 directions.
 * lv1,lv2,lv3..1,2,3 cube dimensions on disk.    (output)
 * ns1,ns2,ns3..nO. of sub-blocks in x,y,z directions.(output)
 */

int CubeIO::cube_sl(int n1, int n2, int n3, int l1, int l2, int l3) {
  int ret_val=1;

// Save the true dimensions of the cube before we pad
  n1_= n1;
  n2_= n2;
  n3_= n3;

// User requested sub-cell size.
  l1_ = l1;
  l2_ = l2;
  l3_ = l3;
  if(l1_ < 8) ret_val=1;
  if(l2_ < 8) ret_val=1;
  if(l3_ < 8) ret_val=1;
  if(l1_ > n1_) l1_ = n1_;

// Number of sub-divisions for axis i,j,k.

  ns1_ = ((n1_-1)/l1_ + 1);
  ns2_ = ((n2_-1)/l2_ + 1);
  ns3_ = ((n3_-1)/l3_ + 1);

// Padded axis size
  lv1_ = ns1_ * l1_;
  lv2_ = ns2_ * l2_;
  lv3_ = ns3_ * l3_;

  cube_disk_offset();

// cube_prnt();
  return ret_val;
} 

// Open files to handle the io
bool CubeIO::cube_open(int *fd0) {
 int fd;
 char mode[4];
 if(fd0) nchan_=1;
 fio_ = new DataIO *[nchan_] ;
 assert(fio_);

 for(int i=0; i < nchan_; i++) { 
    char lname[_POSIX_PATH_MAX];
    strcpy(lname,name_);
    if(nchan_ > 1) sprintf(lname,"%s_%d",name_,i);
    int flags, pmode=0666;
    if( access(lname,F_OK) ==0 ) {
      strcpy(mode,"r+");
      flags = O_RDWR;
    } else {
      strcpy(mode,"w+");
      flags = O_CREAT | O_RDWR | O_EXCL;
    }
    if(fd0==nil) {
      fd = open(lname,flags,pmode);
      fio_[i] = new DataIO(fd,lname,mode,true);
    } else {
      fd = *fd0;
      fio_[i] = new DataIO(fd,lname,mode,false);
    }
    if(!fio_[i]) return false;
 }

  return true ;
}

/*
 * Compute the disk offsets(in samples) for all sub-cubes
 */

void CubeIO::cube_disk_offset() {
   int i,j,k;
   coff_ = new int[ns1_*ns2_*ns3_] ;
   assert(coff_) ;

   int *off = new int[nchan_] ;
   for(i=0; i<nchan_; i++) off[i]=0;
  
   int incr = cube_block_size();
   for(k=0;k<ns3_;k++) {
     for(j=0;j<ns2_;j++) {
       for(i=0;i<ns1_;i++) { 
           /* m = sub-cube index, n = io channel index */
           int m = address(i,j,k);
           int n = get_nd(i,j,k);
  
           coff_[m] = off[n];
           off[n] += incr;
        }
      }
   }
   delete off ;
}

bool CubeIO::cube_init() {

  DataIO *fio;
  const int woff=0;
  int minic,nd,eoff;
  int edo = cube_block_size();
  char *buf  = new char[edo*esiz_];
  if(!buf) return false;

  for( int j=1; j <= ns3_; j++) {
     for( int k=1; k <= ns2_; k++) {
       for( int i=1; i <= ns1_; i++) {
           minic=address(i,k,j);
           nd  = get_nd(i,k,j);
           fio = fio_[nd];
           eoff = coff_[minic] + woff + 1;
           write_wa(fio,buf,eoff, edo);
       }
     }
  }
  delete []buf;
  return true ;
}

void CubeIO::cube_locate(int i1, int i2, int i3, int *B, int *b)
{/*
  * given : a cube element (i1,i2,i3) ( Counting from 0)
  * find  : the corresponding brick and point inside the brick
  */
 B[0] = 1;
 B[1] = 1;
 B[2] = 1;
 b[0] = 0;
 b[1] = 0;
 b[2] = 0;
 if(i1 >= 0 && i1 < n1_)
   { B[0] = i1/l1_; 
     b[0] = i1 - B[0]*l1_;
   }
 if(i2 >= 0 && i2 < n2_)
   { B[1] = i2/l2_; 
     b[1] = i2 - B[1]*l2_;
   }
 if(i3 >= 0 && i3 < n3_)
   { B[2] = i3/l3_; 
     b[2] = i3 - B[2]*l3_;
   }
}

/* Output the cube parameters to stdo */
void CubeIO::cube_prnt() {
   printf("#cube_prnt:   3d cube parameters\n");
   printf("#cube_prnt: padded cube size  : A1=%d, A2=%d A3=%d\n",lv1_,lv2_,lv3_);
   printf("#cube_prnt: unpadded cube size:n1=%d, n2=%d, n3=%d\n",n1_,n2_,n3_);
   printf("#cube_prnt: No. of mini cubes :ns1=%d, ns2=%d, ns3=%d\n",ns1_,ns2_,ns3_);
   printf("#cube_prnt: mini cube size :l1=%d l2=%d l3=%d\n",l1_,l2_,l3_);
   printf("#cube_prnt: element size:%d\n",esiz_);
}

/*
 * Write the ascii header for a Bricket
 */
int CubeIO::cube_wrhdr(Grid3DDesc *h) {
 char *hfile=0,*dfile=0;
 int   fd,nby,nw;
 char *str=0;
 str = tf3d_bld_vox(h,&nby,2,3,1);
 if(!str) return 0;
 hfile = tf3d_getHeaderFile(h);
 tf3d_setDataFile(h,name_);
 dfile = tf3d_getDataFile(h);
 if(strcmp(dfile,hfile)==0 || strcmp(dfile,"SAME")==0)
  {/* header and data are in same file */
   free(str); return 0;
  }
 else
  {/* header file is seperate from data file*/
   int PMODE=0666;
   fd=open(hfile, O_CREAT|O_RDWR|O_EXCL ,PMODE);
   lseek(fd,0,0);
   nby = strlen(str) + 1;
   nw = write(fd,str,nby);
   close(fd);
  }
 if(str) free(str);
 return 1;
}

Grid3DDesc *CubeIO::cube_rdhdr(char *file) {
 Grid3DDesc *h=0;
 int   type=0,fd,nby=4096,nr;
 char *str=0,*hit1,*hit2;
 char *sp;
 
 if(!file) return h;
 str = new char[nby];
 fd=open(file, O_RDONLY ,0);
 if(fd<0) return h;
 lseek(fd,0,0);
 nr = read(fd,str,nby);
 close(fd);
 if(nr>0) {
  str[nr-1]='\0';
  sp = str;
  while(sp[0]) {  sp[0] = toupper(sp[0]); sp++; }
  if( (hit1 = strstr(sp,"GOCAD ")) != 0)
   { if( (hit2=strstr(hit1,"BRICK ")) !=0){
       if(hit2-hit1 < 60)  type=3;
     }
   }
  if(type>0) 
    h = tf3d_create_desc_from_gostr(file,BRICK_TYPE,str);
 }
 if(str) delete []str;
 return h;
}

// transpose
/*
int CubeIO::cube_tp(char *a, char *b, int n1, int n2) {
    int i,j,k;
    for(k=0;k<esiz_;k++) {
     for (j = 0; j < n2 ; ++j) {
        for (i = 0; i < n1 ; ++i) {
             b[j + i * n2] = a[i + j * n1];
        }
     }
     a++; b++;
    }
    return 0;
}
*/

/*************************************************************************/
/* Brick management */

//  Allocate storage and read in a brick from disk.
CubeBrick *CubeIO::brick_rd(int i1,int i2, int i3) {
   int I[3],l[3];
   int nd   = get_nd(i1, i2, i3);
   int minic= address(i1, i2, i3);
   int off  = coff_[minic];
   int wdo  = cube_block_size();
   DataIO *fio = fio_[nd];

   I[0]= i1; I[1]= i2; I[2] = i3;
   l[0]= l1_; l[1]= l2_; l[2] = l3_;
   CubeBrick * brick = new CubeBrick(I,l,esiz_,nil);
   if(!brick) return nil;
   brick->set_data(nil);
  
   read_wa(fio, brick->data(), off, wdo);
   fio->wait();
   
   return brick;
}

char *CubeIO::brick_rd(int i1,int i2, int i3,int wdo, int woff) {
// get brick data form disk and return in a buffer.
   int nd   = get_nd(i1, i2, i3);
   int minic= address(i1, i2, i3) ;
   int off  = coff_[minic] + woff;
   DataIO *fio = fio_[nd];
   if( wdo > cube_block_size()) wdo = cube_block_size();
   char *buf = new char[cube_block_size()*esiz_];
   if(!buf) return 0;
   read_wa(fio, buf, off , wdo);
   fio->wait();
   return buf;
}

/*  Write the requested brick or component.
 *  W wdo words of brick starting from word woff.
 *  Issue wait() call to synchronize the IO.
 */

bool CubeIO::brick_wr(CubeBrick *brick) {
  if(!brick) return false;
  int nd   = get_nd(brick->index(1),brick->index(2),brick->index(3));
  int minic= address(brick->index(1),brick->index(2),brick->index(3));
  int off  = coff_[minic];
  DataIO *fio = fio_[nd];
  int wdo  = brick->size();
  write_wa(fio, brick->data(), off, wdo);
  fio->wait();
  return true ;
}

bool CubeIO::brick_wr(CubeBrick *brick, int woff, int wdo) {
  if(!brick) return false;
  int nd   = get_nd(brick->index(1),brick->index(2),brick->index(3));
  int minic= address(brick->index(1),brick->index(2),brick->index(3));
  int off  = coff_[minic] + woff;
  DataIO *fio = fio_[nd];

  if( wdo > brick->size()) wdo = brick->size();

  write_wa(fio, brick->data()+woff*esiz_, off , wdo);
  fio->wait();
  return true ;
}

/* ccc
 * read/write a pencil of data from a 3d cube. the cube is 
 * composed of ns1*ns2*ns3 Bricks, where a Brick is l1*l2*l3 words.
 * the pencils along axis 1,2,3 are composed of ns1,ns2,ns3 sub-cubes
 * respectively.
 * (i1,i2,i3)....index of 1st sub-cube in the pencil 
 * axis................axis of pencil orientation 1,2,3 
 * buf.................array containing-receiving the data.
 * wdo.................no. of words from each Brick.
 * woff................number of words of offset into a Brick.
 *                     (best if wdo & woff are sector multiples)
 * wskp................no. of words (memory) between transfers of wdo
 */

// minic  = sequential label for a Brick with a label
//          (i1,i2,i3) (0<-->ns1*ns2*ns3-1) 

void CubeIO::address(int seq, int *i1, int *i2, int *i3)
{// given sequential address return the triplet address.
 int n12;
 n12 = ns1_ *ns2_;
 *i3 = (int) (seq / n12);
 *i2 = (int) (seq %n12) / ns1_;
 *i1 = (int) ((seq %n12) % ns1_);
}


bool CubeIO::cube_rd_p(Pencil *pen,int eoff, int edo) {
    int i1,i2,i3,axis,nd,N;
    char *buf=0;
   // nd associates a Brick with an IO channel
   // The index for axis will start at 0
   axis = pen->axis();
   i1 = pen->index(1);
   i2 = pen->index(2);
   i3 = pen->index(3);
   N = pen->blocks();
   int minic=0;
   nd = get_nd(i1,i2,i3); // starting chanel number
   for (int i = 0; i < N; ++i) {
      if(axis==1) minic = address(i1+i,i2,i3) ;
      if(axis==2) minic = address(i1,i2+i,i3) ;
      if(axis==3) minic = address(i1,i2,i3+i) ;
      int lrecr = coff_[minic] + eoff;
      DataIO *fio = fio_[nd];
      buf = pen->get_cube_data(i,eoff);
      read_wa(fio, buf, lrecr, edo);
      nd = (nd + 1) % nchan_;
   }
   cube_wt_all();
   return true ;
}

bool CubeIO::cube_wr_p(
  char *buf, int axis,int i1,int i2,int i3, 
  int woff,int wdo, int wskp) {
  int minic= 0;
  int lptr = 0;
  int nd   = get_nd(i1,i2,i3);
  int ns = cube_blocks(axis) ;
  for (int i = 0; i < ns; ++i) {
      if(axis==1) minic = address(i,i2,i3) ;
      if(axis==2) minic = address(i1,i,i3) ;
      if(axis==3) minic = address(i1,i2,i) ;
      int lrecw = coff_[minic] + woff;
      DataIO *fio = fio_[nd];
      write_wa(fio, &buf[lptr], lrecw, wdo);
      lptr += (wdo + wskp)*esiz_;
      nd = (nd + 1) % nchan_;
  }
  cube_wt_all();
  return  true ;
}

/********************************************************************/
/* Slice management */
/*
 * Retrieve slice at the index position along axis.
 * axis = 1,2,3 : 0 <= index <= n1_,_n2,_n3
 */

CubeSlice *CubeIO::slice_rd(int axis, int index) {
 CubeBrick     *brick;
 CubeSlice *cslice;
 BrickSlice bs;

 if(axis==1) {
    int i = index/l1_;
    cslice = new CubeSlice(axis,index,esiz_, n2_, n3_);
    for( int i3=0; i3 < ns3_; i3++) { 
      for(int i2=0;i2 < ns2_;i2++) {
           brick = brick_rd(i, i2, i3);
           bs.brick_to_slice(brick, cslice);
           delete brick;
       }
    }
    return cslice;
 }

 if(axis==2) {
    int i = index/l2_;
    cslice = new CubeSlice(axis, index, esiz_, n1_, n3_);
    for( int i3=0; i3 < ns3_;i3++) {
      for(int i1=0;i1 < ns1_;i1++) {
           brick = brick_rd(i1, i, i3);
           bs.brick_to_slice(brick, cslice);
           delete brick;
         }
    }
    return cslice;
  }

 if(axis==3) {
    int i = index/l3_;
    cslice = new CubeSlice(axis, index,esiz_,  n1_, n2_);
    for(int i2=0; i2 < ns2_; i2++) {
      for(int i1=0; i1 < ns1_; i1++) {
           brick = brick_rd(i1, i2, i);
           bs.brick_to_slice(brick, cslice);
           delete brick;
      }
    }
    return cslice;
  }
  return nil  ;
}

int CubeIO::slice_wr(CubeSlice *cslice) {

 char *brd=0;
 int wdo,woff,off,minic,nd;
 int b;
 if(!cslice) return 0;
 int ax = cslice->axis();
 int ind= cslice->index();

 if(ax==1) {
   int n2 = cslice->length(1);
   int n3 = cslice->length(2);
   int I1 = ind/l1_ ;
   int i1 = ind - I1*l1_;
   for(int I3=0;I3<ns3_;I3++)
     {for(int I2=0;I2<ns2_;I2++)
        {
         CubeBrick *br = brick_rd(I1,I2,I3);
         char *dat = cslice->data() + esiz_*(I3*l3_*n2 + I2*l2_);
         int lim2 = (I2==ns2_-1) ?  l2_ - (lv2_-n2) : l2_;
         int lim3 = (I3==ns3_-1) ?  l3_ - (lv3_-n3) : l3_;
         for(b=0;b<esiz_;b++) {
         for(int i3=0;i3<lim3;i3++)
           {brd = br->data() + esiz_*(i1 + i3*l1_*l2_) + b;
            for(int i2=0;i2<lim2;i2++)
              {
               *brd = dat[esiz_*(i3*n2+i2) + b];
               brd += l1_*esiz_;
              }
           }
         }
          brick_wr(br);
          delete br;
        }
     }
  return 1;
 }

 if(ax==2) {
   int n1 = cslice->length(1);
   int n3 = cslice->length(2);
   int I2 = ind/l2_;
   int i2 = ind - I2*l2_;
   for(int I3=0;I3<ns3_;I3++)
     {for(int I1=0;I1<ns1_;I1++)
        {
         CubeBrick *br = brick_rd(I1,I2,I3);
         char *dat = cslice->data() + esiz_*(I3*l3_*n1 + I1*l1_);
         int lim1 = (I1==ns1_-1) ?  l1_ - (lv1_-n1) : l1_;
         int lim3 = (I3==ns3_-1) ?  l3_ - (lv3_-n3) : l3_;
         for(int i3=0;i3<lim3;i3++)
           { 
            brd = br->data() + esiz_*(i3*l1_*l2_ + i2*l1_);
            memcpy(brd, dat+i3*n1*esiz_, lim1*esiz_);
           }
          brick_wr(br);
          delete br;
        }
     }
  return 1;
 }

 if(ax==3) {
   char *buf = new char[l1_*l2_*esiz_];
   int I3 = ind/l3_;
   int i3 = ind - I3*l3_;
   wdo    = l1_*l2_;
   woff   = i3*wdo;
   int n1 = cslice->length(1);
   int n2 = cslice->length(2);
   for(int I2=0;I2<ns2_;I2++)
     {for(int I1=0;I1<ns1_;I1++)
        {
         char *dat = cslice->data() + esiz_*(I2*l2_*n1 + I1*l1_);
         int lim1 = (I1==ns1_-1) ?  l1_ - (lv1_-n1) : l1_;
         int lim2 = (I2==ns2_-1) ?  l2_ - (lv2_-n2) : l2_;
         for(int i2=0;i2<lim2;i2++)
           memcpy(buf+i2*l1_*esiz_, dat+i2*n1*esiz_, lim1*esiz_);
         nd   = get_nd(I1, I2, I3);
         minic= address(I1, I2, I3) ;
         off  = coff_[minic] + woff;
         DataIO *fio = fio_[nd];
         write_wa(fio,buf,off,wdo);
        }
     }
   delete []buf; return 1;
 }
 return 0;
}

char *CubeIO::cube_slab(int axis, int slab) {
  char *bslab;
  int   i,j,k,L[3],A[3];
  int   nwrd, wdo ,woff=0, soff=0;

 L[0]=l1_; L[1]=l2_; L[2]=l3_;

 if( axis==1 ) {
     // Slab whoose normal is axis 1, 2-3 planes
     // Slab is lv2_ by lv3_ by l1_
     if( slab<0 || slab>ns1_-1) return 0;
     A[0] = slab; A[1] = 0; A[2] = 0;
     Pencil *pen = new Pencil(ns2_,2,A,L,esiz_,0);
     nwrd =  cube_block_size() * ns2_;
     if(!pen) return nil;

     bslab = new char[esiz_ * lv2_ * lv3_ * l1_] ;
     if( bslab == nil ) { 
        delete pen; return nil ;
     }

     wdo = l1_*l2_*l3_;
     for( j=0; j < ns3_; j++) {
       A[0] = slab; A[1] = 0; A[2] = j;
       pen->set_index(A);
       cube_rd_p(pen,woff,wdo);
       pen->permute();
       for(k=0;k<l1_;k++) {
        for(i=0;i<l2_;i++) {
         soff = (j*l2_ + i + k*lv3_)*lv2_*esiz_;
         memcpy(bslab+soff,pen->get_trace(k,i),lv2_*esiz_);
        }
       }
     }
     delete pen;
     return bslab;
  }

  if(axis==2) {
      // Slab whoose normal is axis 2, 1-3 planes
     // Slab is lv1_ by lv3_ by l2_
     if( slab < 0 || slab > ns2_-1 ) return 0;
     A[0] = 0; A[1] = slab; A[2] = 0;
     Pencil *pen = new Pencil(ns1_,1,A,L,esiz_,0);
     if(!pen) return nil;

     bslab = new char[esiz_ * lv1_ * lv3_ * l2_] ;
     if( bslab == nil ) {
       delete pen; return nil ;
     }

     for( k=0; k<ns3_; k++) {
       A[0] = 0; A[1] = slab; A[2] = k;
       pen->set_index(A);
       cube_rd_p(pen,woff,wdo);
       pen->permute();
       for(i=0;i<l3_;i++) {
         for(j=0;j<l2_;j++) {
          soff = (k*l3_ + i + j*lv3_)*lv1_*esiz_;
          memcpy(bslab+soff,pen->get_trace(j,i),lv1_*esiz_);
         }
       }
     }
     delete pen;
     return bslab;
  }

  if(axis==3) {
     // Slab whoose normal is axis 3, 2-1 planes
     // Slab is lv1_ by lv2_ by l3_ 
     if( slab < 0 || slab > ns3_-1 ) return 0;
     A[0] = 0; A[1] = 0; A[2] = slab;
     Pencil *pen = new Pencil(ns1_,1,A,L,esiz_,0);
     if(!pen) return nil;
  
     bslab = new char[esiz_*lv1_ * lv2_ * l3_] ;
     if( bslab == nil ) {
        delete pen ;
        return nil ;
     }
  
     for( i=0; i < ns2_; i++) {
       A[0] = 0; A[1] = i; A[2] = slab;
       pen->set_index(A);
       cube_rd_p(pen,woff,wdo);
       pen->permute();
       for(j=0;j<l3_;j++) {
        soff = (i *l2_*lv1_ + j*lv1_*lv2_)*esiz_;
        memcpy(bslab+soff,pen->get_trace(0,j),l2_*lv1_*esiz_);
       }
     }
     delete pen;
     return bslab;
  }
  return nil ;
}

/********************************************************************/
/* Utilities */

// synchronize the io with a wait call.
void CubeIO::cube_wt_all() {
  for( int i=0; i<nchan_; i++) fio_[i]->wait();
} 

int CubeIO::cube_wr_buf(char *tbuf, int line, int I2) {
// Dump an inline trace buffer to disk(s) 
// tbuf is l1_ traces by n1_ samples.
  int I1,I3,i2,i3;
  int edo,nd,minic,eoff;
  char *buf=0;
  DataIO *fio;

    I3 = (line - 1) / l3_;
    i3 = (line -I3*l3_ -1);
    edo = l1_ * l2_;

    buf = new char[edo*esiz_];
    for(I1=0;I1<ns1_;I1++) {
      int lim1 = (I1==ns1_-1) ?  l1_ - (lv1_-n1_) : l1_;
      for(i2=0;i2<l2_;i2++) {
        memcpy(&buf[i2*l1_*esiz_],&tbuf[(I1*l1_ + i2*n1_)*esiz_],lim1*esiz_);
      }
      nd   = get_nd(I1,I2,I3);
      minic= address(I1,I2,I3);
      eoff  = coff_[minic] + i3*edo;
      fio = fio_[nd];
      write_wa(fio,buf,eoff,edo);
      fio->wait();
    }
    delete []buf;
    return 0;
} 

/*
 * Word addressable IO routines
 */

void CubeIO::read_wa(DataIO *fio, char *buf, int eoff, int edo) { 
    long byoff = eoff * esiz_;
    int byget = edo * esiz_;
    fio->seek(byoff);
    int n_read = fio->read((char *) buf, byget);
}

void CubeIO::write_wa(DataIO *fio,char *buf, int eoff, int edo) { 
    long byoff = eoff * esiz_;
    int byput = edo * esiz_;
    fio->seek(byoff);
    int n_write = fio->write((char *) buf, byput);
}


void cubeio_wrsl3(void **cio, int *p, int *s, int *more, float *trace, float *lav)
{CubeIO *pntr = *(CubeIO **) cio;
 static CubeSlice *cslice1;
 int  i,n1,n2,off;
 int  num;
 char *dbuf;
 if(!pntr) return;
 if(!cslice1) 
   {
    n1 = pntr->grid_points(1);
    n2 = pntr->grid_points(2);
    cslice1 = new CubeSlice(3,*s,1,n1,n2);
   }

 if(*more==0) 
   {pntr->slice_wr(cslice1);
    delete cslice1;
    cslice1=0;
    delete pntr;
    return;
   }
 n1 = pntr->grid_points(1);
 if(*s != cslice1->index())
  {pntr->slice_wr(cslice1);
   cslice1->index();
  }
 *lav = fabs(trace[0]);
 for(i=0;i<n1;i++)
   *lav = (fabs(trace[i]) > *lav) ? fabs(trace[i]) : *lav;
 num = n1;
 dbuf = cslice1->data();
 off = *p * n1;
 float_to_byt_(trace,&num,(unsigned char *) (dbuf+off), &num, lav);
}

void CubeIO::cube_getp(int *Npad,int *bl) {
 // return some of the cube parameters
    Npad[0] = lv1_;
    Npad[1] = lv2_;
    Npad[2] = lv3_;
    bl[0] = l1_;
    bl[1] = l2_;
    bl[2] = l3_;
}

int CubeIO::cube_blocks(int axis) {
 //. Return the number of bricks along an axis.
 if(axis==1) return ns1_;
 if(axis==2) return ns2_;
 if(axis==3) return ns3_;
 return 0;
}

int CubeIO::length_axis(int axis) {
 //. Return the length of the padded axis on disk.
    if(axis==1)  return lv1_;
    if(axis==2)  return lv2_;
    if(axis==3)  return lv3_;
    return 0;
}

int CubeIO::sub_cube_size(int axis) {
 //. Return the brick size along the direction=axis
    if(axis==1) return l1_;
    if(axis==2) return l2_;
    if(axis==3) return l3_;
    return 0;
}

int CubeIO::grid_points(int axis) {
 //. Return the number of grid points in the direction=axis.
    if(axis==1)  return n1_;
    if(axis==2)  return n2_;
    if(axis==3)  return n3_;
    return 0;
}



