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
#ifndef INTERPOLATOR_2D_HH
#define INTERPOLATOR_2D_HH

/// Class Interpolator2D performs 2 dimensional interpolation.
///   Typically X is 0-dimension and Y is 1-dimension.
///   When getVal is called with random locations, efficiency suffers.

typedef void (*InterpConversionFunction)(void *, void *, float);

class Interpolator2D
{
public:
  Interpolator2D       /* Constuctor */
    (float *first      /* array of size 2 containing the first coordinate in
                            each dimension */,
     float *last       /* array of size 2 containing the last coordinate in
                            each dimension */,
     int *size	       /* array of size 2 containing the size of each
                            dimension */,
     float nil_val     /* value recognized as nil */,
     int stype	       /* flag indicating how to resample */,
     float *values = 0 /* a one dimensional array organized in 1-dimension
			    then in 0-dimension (0 implies supply later) */,
     int allow_lt_2D = 1 /* flag permitting a size of a dimension < 2 */);

  ~Interpolator2D (); /* Destructor */

  void clearBlock (); /* clear the internal array or pointer */

  void moveBlock      /* store the array to an internal buffer */
    (float *values);  /* a one dimensional array organized in 1-dimension
			   then in 0-dimension */

  void pointToBlock   /* store the given pointer NOT the array */
    (float *values);  /* a one dimensional array organized in 1-dimension
			   then in 0-dimension */

  void getSpecs     /* return specifications for the interpolator */
    (float **values /* pointer to a one dimensional array organized in
		         1-dimension then in 0-dimension */,
     float *first   /* array of size 2 containing the first coordinate in
                         each dimension */,
     float *last    /* array of size 2 containing the last coordinate in
                         each dimension */,
     int *size	    /* array of size 2 containing the size of each
                         dimension */) const;

  int getValues    /* return a 2D interpolated array organized in
		       1-dimension then in 0-dimension */
    (void *values  /* a one dimensional array written to in 1-dimension then
                        in 0-dimension */,
     float *first  /* array of size 2 containing the first coordinate in
                        each dimension */,
     float *last   /* array of size 2 containing the last coordinate in
                        each dimension */,
     int *size	   /* array of size 2 containing the size of each
                        dimension */) const;

  void getValues   /* return a 1D interpolated array organized in
		       1-dimension then in 0-dimension */
    (int dim       /* which dimension to interpolate */,
     float *values /* the interpolated 1D array */,
     float *first  /* array of size 2 containing the first coordinate in
                        each dimension */,
     float *last   /* array of size 2 containing the last coordinate in
                        each dimension */,
     int *size	   /* array of size 2 containing the size of each
                        dimension */) const;

  void getVal         /* compute an interpolated value */
    (void *retval     /* computed interpolated value */,
     const float *loc /* array of size 2 containing the coordinates at which
                           to interpolate */);

  void setConversionFunction /* register a converter with the interpolator */
    (InterpConversionFunction func /* conversion function */,
     void *obj                   /* object associated with the function */,
     int num_bytes = 4           /* number of bytes in result */);

  enum {   /* flags describing method of interpolation */
    NN = 0 /* nearest neighbor interpolation in both dimensions */,
    LIN    /* bilinear interpolation */,
    LIN_NN /* linear interpolation in 0-dimension, nearest neighbor
	        in 1-dimension */,
    NN_LIN /* nearest neighbor interpolation in 0-dimension, linear
	        in 1-dimension */
  };

protected:
  int cellIndex  /* returns the cell index */
    (int dim     /* which dimension of the given coordinate */,
     float coord /* coordinate from which to find the index */) const;

  int cellIndex  /* returns the cell index */
    (float first /* first coordinate */,
     float del   /* length of the cell */,
     float coord /* coordinate from which to find the index */) const;

  void assignValues ();  /* assign pointers along the 0-dimension */

  int illDefined /* returns 0 if cell has four non-nil corner values */
    (int id0     /* cell index in 0-dimension */,
     int id1     /* cell index in 1-dimension */) const;

  static void defaultConversionFunction /* dummy conversion function */
    (void *obj    /* object associated with the function */,
     void *result /* resultant value returned */,
     float value  /* given float value to operate on */);

  int indexInValidRange /* return 0 if index is not in a valid range */
    (int dim      /* dimension index is associated with */,
     int *id      /* index to check */) const;

  InterpConversionFunction
    _conversion_function /* conversion function registerd with interpolator */;

  void
    *_conversion_obj /* object associated with the conversion function */;

  int
    _stype            /* flag describing method of interpolation */,
    _allow_lt_2D      /* flag permitting a size of a dimension < 2 */,
    _size[2]          /* size of each dimension */,
    _loc_set[2]       /* flags indicating if coordinates have been defined
			 in each dimension */,
    _wt_set[2]        /* flags indicating if weights have been defined
			 in each dimension */,
    _id_set[2]        /* flags indicating if cell indices have been defined
			 in each dimension */,
    _0_values_set     /* flag indicating if intermediate values have been
			 defined in 0-dimension */,
    _1_values_set     /* flag indicating if intermediate values have been
			 defined in 1-dimension */,
    _prev_ill_defined /* 0 if previous cell had four non-nil corner values */,
    _block_owned      /* 0 if block is not stored internally */,
    _block_unowned    /* 0 if block is not stored externally */,
    _conversion_value_size /* number of bytes in a converted value */,
    _prev_id[2]       /* previous cell indices used in each dimension */;

  float
    _nil_val          /* value recognized as nil */,
    *_block           /* pointer to 2D array block */,
    **_values         /* 2D array of pointers for 2D array block */,
    _first[2]         /* first coordinate in each dimension */,
    _last[2]          /* last coordinate in each dimension */,
    _del[2]           /* size of cell in each dimension */,
    _prev_loc[2]      /* previous coordinates used in each dimension */,
    _prev_wt[2]       /* previous weights used in each dimension */,
    _prev_0_values[2] /* previous intermediate values used in 0-dimension */,
    _prev_1_values[2] /* previous intermediate values used in 1-dimension */;
};

#endif
