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
#ifndef brick_slice_h
#define brick_slice_h

#include "cubeio/defs.hh"
class CubeSlice;
class CubeSlab;
class CubeBrick;

#ifdef __cplusplus
extern "C" {                 // for C++
#endif
/* wrappers for access by C */

#ifdef __cplusplus
}                   // for C++
#endif


//- BrickSlice
class BrickSlice {
   //. This class pushes transfers data form CubeBrick to CubeSlice
   //. and CubeSlab, and vice versa.
   //. A cube has axis 1,2, and 3 with an arbitrary element size.
   //. A CubeSlice contains a 2D data array, and a CubeSlab has
   //. a data array that is essentially a stack od CubeSlices.

public:
   BrickSlice();
    //. A helper class for the CubeIO class.
  ~BrickSlice();

   bool intersects(CubeBrick *b, CubeSlice *s);
    //. Determine if there is any intersection of brick and slice
    //. along the major axis which is determined by slice.
   inline void set_brick(CubeBrick *b);
   inline void set_slice(CubeSlice *s);

   bool brick_to_slice(CubeBrick *b, CubeSlice *s);
   //. Move data from the brick to the receiving slice.
   //. The CubeBrick and CubeSlice must come from the same Cube.
   //. I.E. they have same element size, ...

   //- Slabs
   //. Slabs are fat slice through the cube. The axis for the slab
   //. is its normal vector.
   bool brick_to_slab(CubeBrick *brick, int axis,char *slab, int sl[]);
   //. Copy the brick data to the slab pointer.
   //. sl[] is a 3 element array with the dimensions of the slab array.

   CubeBrick *brick_from_slab(int bi[], int bl[],int axis, int esiz,
     int sl[], char *slab);
   //. bi[] is a 3 element array of the brick indeces(address).
   //. bl[] is a 3 element array of the brick dimensions.
   //. element size in chars of the brick and slab.
   //. axis is 1,2, or 3 and is the slabs normal or slow axis.
   //. sl[] is a 3 element array with the slabs dimensions.
   //. slab is the buffer containing the slab data.

protected:

private:

};

/*____________________________________________________________________________*/


#endif

