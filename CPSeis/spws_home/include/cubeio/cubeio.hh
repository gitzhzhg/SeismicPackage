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
#ifndef _cubeio_h
#define _cubeio_h

#include "cubeio/defs.hh"
#include "tf3d.h"
class CubeSlice;
class CubeSlab;
class CubeBrick;
class Pencil;



#ifdef __cplusplus
extern "C" {                 // for C++
#endif
/* wrappers for access by C */
void cubeio_wrsl3(void **cio, int *p, int *s, int *more,
     float *trace, float *lav);

#ifdef __cplusplus
}                   // for C++
#endif

class DataIO;

//- CubeIO
class CubeIO {
   //. The cube axis are classified as 1,2 and 3. The cube is of 
   //. dimemsion n1, n2 and n3.
   //. The cube is broken into sub-cubes(bricks) with size l1*l2*l3.
   //. On disk the cube is padded so that they are an integer number of
   //. bricks along each axis.
public:
   CubeIO(const char *name, int *C, int *c, int esiz,
          bool open_now, int nchan=1);
      //. Creates a CubeIO given the size of the cube and the size
      //. of the bricks. Axis sizes give by C[], and c[]
   CubeIO(int fd, const char *name, int *C, int *c, int esiz);
      //. Uses the IO descriptor provided.
   CubeIO(char *header, bool open_now);
      //. For opening an existing Brick file for reading.

  ~CubeIO();
   bool isopen();

   int  esize();
    //. size in bytes of a data element

   void cube_getp(int *Npad,int *bl);
      //. Retrieves the dimensions of the cube(padded on disk), and
      //. the dimension of the bricks.

   //- Bricks
   CubeBrick *brick_rd(int i1,int i2, int i3);
       //. Reads a brick from disk
   char *brick_rd(int i1,int i2, int i3,int wdo, int woff);
       //. Reads portion of brick data from disk
   bool brick_wr(CubeBrick *brick);
       //. write a brick to disk
   bool brick_wr(CubeBrick *brick, int woff, int wdo);
       //. Writes a Brick or portion of it to disk.
       //. wdo elements of brick starting from element woff.

   //- Slices
   CubeSlice *slice_rd(int axis, int slice);
       //. Create a slice reading it from disk, using bricks.
   int slice_wr(CubeSlice *cslice);
       //. Store a slice to a cube on disk.

   //- Slabs
   //. Slabs are fat slice through the cube. The axis for the slab
   //. is its normal vector. One can have a slab of bricks or a slab
   //. of slices. 
   char *cube_slab(int dir, int slab);

   //- Pencil
   //. A Pencil is a stack of bricks along one of the
   //. cube axis.
   bool cube_rd_p(Pencil *pen, int eoff, int edo);
   //. Pencil encapsulates the request and eoff and edo specify
   //. data to read with in each sub-cube
   bool cube_wr_p(
       char *buf, int axis, int ins1, int ins2, int ins3,
       int woff,int wdo, int wskp
   );
      //. read/write a pencil of data from a 3d cube. 
      //. edo is the no. of words from each Brick.
      //. eoff is the number of words of offset into a Brick.

   void cube_prnt();

   int cube_wrhdr( Grid3DDesc *h );
   Grid3DDesc *cube_rdhdr( char *file );

//   static int cube_tp(char *, char *, int , int);

   int cube_blocks(int axis);
    //. Number of bricks along the requested axis.

   int length_axis(int axis);
    //. Cube grid size on disk for a requested axis(1,2 or 3).

   int sub_cube_size(int axis);
    //. Length of the brick for the requested axis.

   int grid_points(int axis);
    //. Grid point count for a brick for the requested axis.

   void cube_locate(int i1, int i2, int i3, int *B, int *b);
    //. given global triplet element address (i1,i2,i3), find the
    //. the brick and sub-brick triplets B[3], b[3].

   int  cube_block_size();
    //. number of elements in a brick.

   void address(int seq, int *i1, int *i2, int *i3);
    //. Given a brick sequential address return a brick triplet address.

   int  address(int i1,int i2,int i3);
    //. Given a brick triplet address return a brick sequential address.
    //. triplet numbers start from 0

protected:
   void setup(const char *name, int *C, int *c, int nchan);
   bool cube_open(int *fd);
    //. Creates the DataIO objects that handle all IO.
    //. Pass nil if you want cube to open files itself.

   bool cube_init();
    //. Initializes cube on disk.

   void cube_wt_all();
   int  cube_wr_buf(char *tbuf, int line, int I2);

   void read_wa(DataIO *fio,char *buf, int offset, int length);
   void write_wa(DataIO *fio,char *buf, int offset, int length);

   int  cube_sl(int n1, int n2, int n3,int li, int lj, int lk);
    //. Compute all dependent variables from input variables

   void cube_disk_offset();
    //. computes the element offset on disk for all bricks

   int get_nd(int i1,int i2,int i3);
    //. The io channel number which handles brick (i1,i2,i3)
    //. Not a descriptor! May support multiple streams in future!

protected:
   char *name_;
   int esiz_;
   int n1_, n2_, n3_;      // cube size(unpadded) along each axis
   int l1_, l2_, l3_ ;     // Size of SimpleCube s
   int nchan_;             // No. of IO channels
   int lv1_, lv2_, lv3_ ;  // cube size(padded) cube along each axis
   int ns1_, ns2_, ns3_ ;  // Number of sub-cubes along each axis
   int *coff_;             // disk offsets for sub-cubes
   DataIO **fio_;          // Handles the IO for the cube
private:

};

/*____________________________________________________________________________*/


 inline int  CubeIO::address(int i1,int i2,int i3) {
  return  i1  +  ns1_ * ( i2 ) + ns1_ * ns2_ * (i3 ) ;}
 inline int CubeIO::get_nd(int i1,int i2,int i3) {
  return (i1 + i2 + i3 ) % nchan_;}
 inline int CubeIO::cube_block_size() {return l1_*l2_*l3_;}
 inline int CubeIO::esize() { return esiz_;}



#endif

