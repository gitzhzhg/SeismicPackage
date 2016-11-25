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
#ifndef GRIDDER_3D_HH
#define GRIDDER_3D_HH

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>

/// Class Index3D is a converter from 1D and 3D indices

class Index3D {

public:
  Index3D     /* constructor */
    (int *dim /* 3 element array with size of each dimension */);

  ~Index3D (); /* destructor */

  int totalSize () const; /* returns the total size of the array in 1D */

  int to1D     /* convert from a 3D set of indices to a 1D index */
    (int *i_3D /* a given 3 element array of indices */) const;

  void from1D   /* convert from a 1D index to a set of 3D indices */
    (int i_1D  /* a given 1D index into the array */,
     int *i_3D /* a returned 3 element array of indices */) const;

protected:
  int
    _dim [3]      /* 3 element array containing size of each dimension */,
    _span[3]      /* 3 element array containing a span for each dimension */,
    _total_size /* the total size of the array in 1D */;

};

/// Class Points3D is a container for data points.  Each point is an
/// 3-dimensional location and a value.

class Points3D
{
	public:

		/// Constructor
		Points3D ();

		/// Destructor
		~Points3D ();

		/// Adds a point to the container, returns the pointer or 0
		int add(const float *loc	/** array of dimension 3
						which specifies the data point
						location */,
			 float val		/** value at the given
						location */);

		/// Add the ith point from Points points, rtns the ptr or 0
		int add(const Points3D *points	/** source Points container */,
			 int i			/** index into points */);

		/// Returns the num of points in the container.
		int num() const;

		/// Gets the location of the ith point.
		void  getLoc(	int i		/** input scalar point index */,
				float *loc	/** output array of dimension
						3 containing location */
			) const;

		/// Gets the value at the ith point.
		float getVal(int i /** point index */) const;

	protected:

	private:

		enum { ALLOC_INC = 10 };

		int _num, _num_alloc;
		float *_loc_block, **_loc, *_val;
};

/// Class Gridder performs 3 dimensional gridding.
/**
 * Control points are input with addPoint.  Gridded points are extracted
 * with getVal.  getVal extracts a gridded points by collecting all points
 * within the normalized reach distance of the extraction location.  These
 * points are averaged, each weighted by its reciprocal distance from the
 * desired extraction point location.  Gridder bins input points in an grid
 * of dimension 3.  Each bin size is the reach in each dimension,
 * therefore, when extracting points the bin containing the extracting point
 * location and all immediately adjacent bins are all that have to be checked.
 * If there are no input points within the normalized reach distance of the
 * extracting point, nil_value is returned.
 */

class Gridder3D
{
	public:

		/// Constructor
		Gridder3D(	float *min	/** array of size 3
						containing grid minimums */,
				float *max	/** array of size 3
						containing grid maximums */,
				float *reach	/** array of size 3
						containing grid reachs */,
				float nil_val	/** nil value */,
				int stype	/** resampling type */
			);

		/// Destructor
		~Gridder3D();

		/// Add a control point, returns the pointer or NULL
		int addPoint(	const float *loc	/** input array of size
							3 containing the
							control pnt location */,
				float val		/** value at control
							pnt location */
			);

		/// Returns grid specifications.
		void getSpecs(  float *min	  /** array of size ndims
						  containing grid minimums */,
				float *max	  /** array of size ndims 
						  containing grid maximums */,
				float *reach	  /** array of size ndims
						  containing grid reachs */,
				float *nil_val = 0/** nil value */,
				int *stype = 0	  /** resampling type */
			) const;

		/// Returns value at extracted grid point.
		float getVal(const float *loc	/** input array of size 3
						containing the desired
						extraction pnt location */
				) const;

		void setResampleType (int stype  /** resampling type */
			);

		void getPoints (          /** returns size of Points array */
 				 Points3D ***points
				                 /** array of size grid_size
						   containing control pnts */,
				 int *grid_size  /** array of size 3
						   containing sz of points */
			) const;

		int findRangeOfPossibleChange (  /** returns 0 if no change */
			   const Points3D **points
			                         /** array of size total_size
                                                   containing control pnts */,
				 int *grid_size  /** array of size 3
						    grid sizes             */,
				 float *mins	 /** array of size 3
						   containing change mins  */,
				 float *maxs	 /** array of size 3
						   containing change maxs  */
			) const;

		/// Extracts value array.
		int extractValues (       /** returns size of values array */
			    float *values /** array of size as needed below
					    contains values: if NULL count */,
				 float *mins	 /** array of size 3
						   containing extract mins */,
				 float *maxs	 /** array of size 3
						   containing extract maxs */,
				 float *incs	 /** array of size 3
						   containing extract incs */
			) const;


		enum {
		  WTD = 0  /** weighted resampling */,
		  NN       /** nearest neighbor resampling */
		};

	protected:

	private:

		enum {
		  MIN      /** justify the answer to be minimum */,
                  MAX      /** justify the answer to be maximum */,
		  CENTER   /** justify the answer to be centered */
		};
  
		int _stype, _grid_size[3];
		float _min[3], _max[3], _reach[3];
		float _nil_val;
		Points3D **_points;
                Index3D *_index;

		/// Converts a single dimension's distance to bin index.
		int gridIndex(float min, float reach, float loc) const;

		/// Converts a single dimension's bin index to distance.
		float gridLocation(float min, float reach, int index,
			int just) const;

		/// Returns number of control points at location.
		int sameLocation(const float *loc, float *sum) const;

		/// Returns number of control points within reach.
		int withinReach (const float *loc, float *weighted_sum,
			float *factor_sum) const;

		/// Returns reach normalized square of distance between two
		/// locations.
		float normalizedDistanceSquared(
			const float *loc1, const float *loc2) const;

		/// Returns 0 if given point is same as internal point at
		/// given 1D index
		int compar(
			int i_1D, const Points3D *point) const;

};

#endif
