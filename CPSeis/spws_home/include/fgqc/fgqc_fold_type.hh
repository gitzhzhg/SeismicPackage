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
//**************************************************************************
//                Author Michael L. Sherrill  08/95
//                   Elevation plot type class
//**************************************************************************

#ifndef FG_QC_FOLD_H
#define FG_QC_FOLD_H

#include "fgqc/fgqc_plot_type.hh"

class UCharGrid;
class UCharGridAccessor;
class DistributionSlicer;
class DistrSlicerPop;
class FloatGridAccessor;

class FgQcFoldType : public FgQcPlotType {

  private:
    double getCmpTraceAzimuth			// get the azimuth for gvn trc
      (long ixcmp,				//   Common MidPoint number
       long ixfold);				//   trace w/in CMP gather

    int setFoldGathers				// set up to gather folds
      (int num_zval_ranges);			//   # of Z-value ranges to get

    int gatherFolds				// gather folds within a region
      (float xmin,				//   minimum X-coordinate
       float xmax,				//   maximum X-coordinate
       float ymin,				//   minimum Y-coordinate
       float ymax);				//   maximum Y-coordinate

    int quantizeFoldsInitially ();		// init quantzn gathered folds

    int quantizeFoldsSubsequently ();		// curnt quantzn gathered folds

    Boolean checkGridStatus			// check grid obj error status
      (int check_type,				//   identity of object to chk
       int dont_post=0);			//   enter 1 to not post error

    void extendROI				// extend region of interest
      (float *xmin,				//   X-minimum of region addr
       float *xmax,				//   X-maximum of region addr
       float *ymin,				//   Y-minimum of region addr
       float *ymax,				//   Y-maximum of region addr
       float amount);				//   fraction to extend ROI

    void findGridROI				// find ROI in grid coords
      (float xmin,				//   X-minimum of region
       float xmax,				//   X-maximum of region
       float ymin,				//   Y-minimum of region
       float ymax);				//   Y-maximum of region

    void findLocBinData				// find survey bin sizes
      (float xmin,				//   X-minimum of region
       float xmax,				//   X-maximum of region
       float ymin,				//   Y-minimum of region
       float ymax);				//   Y-maximum of region

    void findLocROI				// find ROI in survey coords
      (float xgmin,				//   Grid X-min of region
       float xgmax,				//   Grid X-max of region
       float ygmin,				//   Grid Y-min of region
       float ygmax);				//   Grid Y-max of region

    int computeNumGathers			// find number of active gathrs
      (float xmin,				//   minimum X-coordinate
       float xmax,				//   maximum X-coordinate
       float ymin,				//   minimum Y-coordinate
       float ymax);				//   maximum Y-coordinate

    int createCmpList ();			// create arry of active gathrs

    void storeCmp				// store active CMP gather
      (long ixcmp);				//   CMP gather index

    int sortCmpList ();				// sort arry of active gathrs

    static int compareCmps			// compare CMP gather indices
      (const void *ixcmp1,			//   First CMP gather index
       const void *ixcmp2);			//   Second CMP gather index

    int cmpIndex				// return array index given
      (long ixcmp);				//   CMP gather index

    void destroyCmpList ();			// destroy active CMP list

    int createLiveFoldOfStack ();		// create live fold array

    long grabLiveFoldOfStack			// return live fold given
      (long ixcmp);				//   CMP gather index

    void destroyLiveFoldOfStack ();		// destroy live fold array

    int createCmpTraceOffsets ();		// create CMP trace offsets

    float grabCmpTraceOffset			// return trace offset given
      (long ixcmp,				//   CMP gather index
       long ixfold);				//   index of trace in fold

    void destroyCmpTraceOffsets ();		// destroy CMP trace offsets

    int createCmpTraceAzimuths ();		// create CMP trace azimuths

    double grabCmpTraceAzimuth			// return trace azimuth given
      (long ixcmp,				//   CMP gather index
       long ixfold);				//   index of trace in fold

    void destroyCmpTraceAzimuths ();		// destroy CMP trace azimuths

    UCharGrid
      *_uchar_grid;				// unsigned char grid objct ptr

    UCharGridAccessor
      *_ugar,					// resultant grid accessor
      *_uga;					// unsgned char grid obj acces

    FloatGridAccessor
      *_tga,					// temporary grid accessor
      *_fga;					// float grid accessor

    DistributionSlicer
      *_ds;					// distr slicer object pointer

    DistrSlicerPop
      *_dsp;					// distr slicer popup obj ptr 

    double
      _deg_per_radian;				// multiplier for azimuth calc

    float
      *_cmp_trace_offsets,			// array of CMP trace offsets
      **_cmp_offset_pntrs,			// array of pntrs to offsets
      *_cmp_trace_azimuths,			// array of CMP trace azimuths
      **_cmp_azimuth_pntrs,			// array of pntrs to azimuths
      _xperbin,					// findLocBinData's X-bin size
      _yperbin,					// findLocBinData's Y-bin size
      _xlmin,					// findLocROI's survey X-min
      _xlmax,					// findLocROI's survey X-max
      _ylmin,					// findLocROI's survey Y-min
      _ylmax,					// findLocROI's survey Y-max
      _min_zval,				// minimum Z-value result
      _max_zval;				// maximum Z-value result

    Boolean
      _veto_extended;                           // false if DYNAMIC_LIMITS OK

    int
      *_cmp_list,				// array of active CMP #s
      _cmp_list_size,				// size of _cmp_list
      _cmp_count,				// number of active CMP #s
      _total_traces,				// total # of active traces
      _max_live_fold,				// maximum live fold of active
      _previous_ixcmp,				// previous ixcmp requested
      _previous_cmp_index,			// previous CMP index
      _num_x_loc_bins,				// findLocBinData's num X-bins
      _num_y_loc_bins,				// findLocBinData's num Y-bins
      _xgmin,					// findGridROI's grid X-min
      _xgmax,					// findGridROI's grid X-max
      _ygmin,					// findGridROI's grid Y-min
      _ygmax,					// findGridROI's grid Y-max
      _user_xgmin,				// user's grid X-min
      _user_xgmax,				// user's grid X-max
      _user_ygmin,				// user's grid Y-min
      _user_ygmax,				// user's grid Y-max
      _uchar_clear,				// unsgnd grid is "zeroed" out
      *_live_fold_of_stack;                     // int array of live folds/CMP

  protected:
    void computeNumPoints(float xmin, float xmax, float ymin, float ymax);

  public:
    FgQcFoldType( FgQcPlot *fgqc_plot );
    virtual ~FgQcFoldType();
    int plot();
    int editData();
    int ValuesChanged(FieldGeometry *fg, long ixl,
                      int ident,         long index, 
                      long nrem,         long nins);
    int gatherData				// get distribution of data
      (int num_fold_groups,			//   number of fold bins wanted
       int num_zval_groups,			//   number of zval bins wanted
       float xmin,				//   X-coordinate minimum bound
       float xmax,				//   X-coordinate maximum bound
       float ymin,				//   Y-coordinate minimum bound
       float ymax);				//   Y-coordinate maximum bound
};

#endif
