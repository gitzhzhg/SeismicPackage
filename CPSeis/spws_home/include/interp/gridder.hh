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
#ifndef GRIDDER_HH
#define GRIDDER_HH

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>

/// Class Index converts between multi-dimensional and one-dimensional array
/// indices.

/**
 * Class Index is necessary because class Gridder must handle any dimension
 * of data.  Gridder copes with this by using a one-dimensional array and
 * class Index to reference the one-dimensional array as a mult-dimensional
 * array.
 */

class Index
{
	public:

		/// Constructor
		/**
		 * For example, for a 2D, 20 by 10 array:
		 * ndims = 2, dim = { 20, 10 }.
		 * Index models C style multi-dimension arrays, the last
		 * dimension index increments most quickly.
		 */
		Index(	int ndims	/** number of dimensions */,
			int *dim 	/** array of size ndims containing
					the array size in each dimension */);

		/// Destructor
		~Index();

		/// Returns number of dimensions as set in constructor.
		int numDims() const;

		/// Returns total number of elements in multi-dimension array.
		int totalSize() const;

		/// Converts from multi-D indices to 1-D index.
		/**
		 * Given array of size ndims containing the multi-dimensional
		 * indices, returns the corresponding one-dimensional index.
		 * Returns -1 if one or more of the multi-dimensional indices
		 * are out of the dimensioned ranges specified in the
		 * constructor.
		 */
		int to1D(int *i_multiD	/** input array of size ndims containing
					the indices in each dimension */) const;

		/// Converts from 1-D index to multi-D indices.
		/**
		 * Given a one-dimensional index, returns the corresponding
		 * array of size ndims containing the multi-dimensional
		 * indices.  Returns -1 for all indices if the one-dimensional
		 * index is out of the dimensioned range specified in the
		 * constructor.
		 */
		void from1D(	int i_1d	/** input scalar 1D index*/,
				int *i_multiD	/** output array of size ndims
						containing the indices in
						each dimension */) const;

	protected:

	private:

		int _ndims;
		int *_dim, *_span;
		int _total_size;
};

/// Class Points is a container for data points.  Each point is an n-dimensional
/// location and a value.

class Points
{
	public:

		/// Constructor
		Points(int ndims /** total number of dimensions */);

		/// Destructor
		~Points();

		/// Returns the number of dimensions as specified in the
		/// constructor.
		int numDims() const;

		/// Adds a point to the container, returns the pointer or 0
		long add(const float *loc	/** array of dimension ndims
						which specifies the data point
						location */,
			 float val		/** value at the given
						location */);

		/// Add the ith point from Points points, rtns the ptr or 0
		long add(const Points *points	/** source Points container */,
			 int i			/** index into points */);

		/// Returns the num of points in the container.
		int num() const;

		/// Gets the location of the ith point.
		void  getLoc(	int i		/** input scalar point index */,
				float *loc	/** output array of dimension
						ndims containing location */
			) const;

		/// Gets the value at the ith point.
		float getVal(int i /** point index */) const;

	protected:

	private:

		/// Nested class Point represents a single data point.
		/**
		 * Point's dimensionality is obtained from its containing
		 * Points object.
		 */
		class Point
		{
			public:

				/// Constuctor
				Point(	Points *container
					/** containing Points object */,
					const float *loc
					/** input array of size
					container->numDims() containing
					location */,
					float val /** value at data point */);

				/// Destructor
				~Point();

				/// Returns point location.
				void  getLoc(float *loc /** output array of size
							container->numDims()
							containing location */
					) const;

				/// Returns value.
				float getVal(          ) const;

			private:

				Points *_container;
				float *_loc;
				float  _val;
		};

		enum { ALLOC_INC = 100 };

		int _ndims, _num, _num_alloc;
		Point **_point;
};

/// Class Gridder performs to to 1 to n dimensional gridding.
/**
 * Control points are input with addPoint.  Gridded points are extracted
 * with getVal.  getVal extracts a gridded points by collecting all points
 * within the normalized reach distance of the extraction location.  These
 * points are averaged, each weighted by its reciprocal distance from the
 * desired extraction point location.  Gridder bins input points in an grid
 * of dimension ndims.  Each bin size is the reach in each dimension,
 * therefore, when extracting points the bin containing the extracting point
 * location and all immediately adjacent bins are all that have to be checked.
 * If there are no input points within the normalized reach distance of the
 * extracting point, nil_value is returned.
 */

class Gridder
{
	public:

		/// Constructor
		Gridder(	int ndims	/** number of dimensions */,
				float *min	/** array of size ndims
						containing grid minimums */,
				float *max	/** array of size ndims
						containing grid maximums */,
				float *reach	/** array of size ndims
						containing grid reachs */,
				float nil_val	/** nil value */,
				int stype	/** resampling type */
			);

		/// Destructor
		~Gridder();

		/// Add a control point, returns the pointer or NULL
		long addPoint(	const float *loc     /** input array of size
						     ndims containing the
						     control pnt location */,
				float val	     /** value at control
						     pnt location */
			);

		/// Returns grid extrema.
		void getSpecs(	int   *ndims	  /** number of dimensions */,
				float *min	  /** array of size ndims
						  containing grid minimums */,
				float *max	  /** array of size ndims 
						  containing grid maximums */,
				float *reach	  /** array of size ndims
						  containing grid reachs */,
				float *nil_val = 0/** nil value */,
				int *stype = 0	  /** resampling type */
			) const;

		/// Returns value at extracted grid point.
		float getVal(const float *loc	/** input array of size ndims
						containing the desired
						extraction pnt location */
				) const;

		void setResampleType (int stype  /** resampling type */
			);

		int getPoints (          /** returns size of Points array */
 				 Points ***points /** array of size total_size
						    containing control pnts */
			) const;

		int findRangeOfPossibleChange (  /** returns 0 if no change */
			   const Points **points /** array of size total_size
                                                   containing control pnts */,
				 int total_size  /** size of Points array */,
				 float *mins	 /** array of size ndims
						   containing change mins */,
				 float *maxs	 /** array of size ndims
						   containing change maxs */
			) const;

		/// Extracts value array.
		int extractValues (       /** returns size of values array */
			    float *values /** array of size as needed below
					    contains values: if NULL count */,
				 float *mins	 /** array of size ndims
						   containing extract mins */,
				 float *maxs	 /** array of size ndims
						   containing extract maxs */,
				 float *incs	 /** array of size ndims
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
  
		int _ndims, _stype, *_grid_size;
		float *_min, *_max, *_reach;
		float _nil_val;
		Index *_index;
		Points **_points;

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

		/// Uses recursive do loop to enable searching to be done in
		/// any dimensionality.
		int withinReach (const int *i_min, const int *i_max,
			int *i_cur, int cur_dim, const float *loc, 
			float *weighted_sum, float *factor_sum,
			float *min_dist2) const;

		/// Returns reach normalized square of distance between two
		/// locations.
		float normalizedDistanceSquared(
			const float *loc1, const float *loc2) const;

		/// Returns 0 if given point is same as internal point at
		/// given 1D index
		int compar(
			int i_1D, const Points *point) const;

		/// Uses recursive for loop to enable range calculation to be
		/// done in any dimensionality
		void findLocationRange(
			int *count, const int *i_min, const int *i_max,
			int *i_cur, int cur_dim, float *mins,
			float *maxs) const;

		/// Uses recursive for loop to enable value extraction to be
		/// done in any dimensionality
		void extractValues (
				 int   *index	 /** current value index   */,
			    float *values /** array of size as needed below
					    contains values: if NULL count */,
				 float *mins	 /** array of size ndims
						   containing extract mins */,
				 int   *i_max	 /** array of size ndims
						   containing index maxs */,
				 float *incs	 /** array of size ndims
						   containing extract incs */,
				 int   *i_cur	 /** array of size ndims
						   containing cur indices  */,
				 float *locs	 /** array of size ndims
						   containing extract loc  */,
				 int   dim	 /** current dimension     */
			) const;
};

#endif
