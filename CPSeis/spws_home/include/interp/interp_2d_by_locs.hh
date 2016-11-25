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
#ifndef INTERP_2D_BY_LOCS_HH
#define INTERP_2D_BY_LOCS_HH

#include "interp/interpolator_2d.hh"

// written for efficiency. allows the user to interpolate between user
//   specified locations in a given dimension regardless of the constructor's
//   orignal data knots

typedef void (*InterpCorrectionFunction)(void *, float *, float, int);

class Interp2DByLocs : public Interpolator2D {

public:
  Interp2DByLocs       /* Constuctor */
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

  ~Interp2DByLocs (); /* Destructor */

  int setLocs      /* define the dimension and locations to use as knots */
    (int dim     /* dimension to which the locations correspond */,
     float *locs /* location of knots array */,
     int *size   /* size of the locs array and the other dimension */);

  int getValues    /* return a 2D interpolated array organized in
		        1-dimension then in 0-dimension */
    (void *values /* a one dimensional array written to in 1-dimension then
                        in 0-dimension */,
     float *first  /* array of size 2 containing the first coordinate in
                        each dimension */,
     float *last   /* array of size 2 containing the last coordinate in
                        each dimension */,
     int *size	   /* array of size 2 containing the size of each
                        dimension */);

  void setCorrectionFunction /* register a correcter with the interpolator */
    (InterpCorrectionFunction func /* correction function */,
     void *obj                     /* object associated with the function */);

  enum { /* flags for amplitude correction */
    LOC0 /* 0th location flag - amplitude correction function */,
    LOC1 /* 1st location flag - amplitude correction function */
  };

protected:
  int getValuesByLoc /* return a 2D interpolated array organized in
			  1-dimension then in 0-dimension */
    (int dim       /* dimension to do the interpolation */,
     void *values  /* a one dimensional array written to in 1-dimension then
                        in 0-dimension */,
     float *first  /* array of size 2 containing the first coordinate in
                        each dimension */,
     float *last   /* array of size 2 containing the last coordinate in
                        each dimension */,
     int *size	   /* array of size 2 containing the size of each
                        dimension */);

  int getValuesByNN /* do nearest neighbor (NN) interpolation between
			 _locX_values if not a repeat return 0 */
    (float loc     /* location along 1-_loc_dim to do interpolation */,
     float *values /* returned interpolated values */,
     int size      /* size of values array */);

  int getValuesByLIN /* do linear (LIN) interpolation between _locX_values
		          if not a repeat return 0 */
    (float loc     /* location along 1-_loc_dim to do interpolation */,
     float *values /* returned interpolated values */,
     int size      /* size of values array */);

  void setIllDefined /* populate the defined array based on values */
    (int *defined    /* output defined array - 0's where nil values occur */,
     float *values   /* array containing values that could be nil */,
     int size        /* size of both arrays */);

  static void defaultCorrectionFunction /* dummy correction function */
    (void *obj     /* object associated with the function */,
     float *result /* resultant value returned */,
     float value   /* given float value to operate on */,
     int which_loc /* flag indicating which location to correct */);

  void correctValues /* perform a value correction on a given array */
    (float *values   /* given array to correct */,
     int size        /* size of array */,
     int which_loc   /* which location to use as a basis for correction */);

  InterpCorrectionFunction
    _correction_function /* correction functn registrd with interp2DByLocs */;

  void
    *_correction_obj /* object associated with the correction function */;

  int
    _loc_dim   /* dimension to which the locations correspond */,
    _locs_size /* size of the _locs array */,
    _loc0_set  /* flag indicating that the _loc0_values are defined */,
    _loc1_set  /* flag indicating that the _loc1_values are defined */,
    _loc0      /* current _locs index corresponding to _loc0_values */,
    _loc1      /* current _locs index corresponding to _loc1_values */,
    _last_loc0 /* previous _locs index */,
    _last_loc1 /* previous _locs index */,
    *_loc0_defined /* flags indicating defined values in _loc0_values */,
    *_loc1_defined /* flags indicating defined values in _loc1_values */;

  float
    *_locs        /* array of locations given by user & sorted internally
		      in ascending order */,
    *_loc0_values /* array of values corresponding to _locs[_loc0] */,
    *_loc1_values /* array of values corresponding to _locs[_loc1] */,
    _last_wt1     /* previous weighting for the _locs1_values array */,
    *_prev_words  /* pointer to the previous interpolated four byte values */;

  unsigned char
    *_prev_bytes /* pointer to the previous interpolated byte values */;
};

#endif
