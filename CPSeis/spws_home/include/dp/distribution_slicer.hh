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
// distribution_slicer.hh:  User Interface file for DistributionSlicer class
#ifndef DISTRIBUTION_SLICER_HH
#define DISTRIBUTION_SLICER_HH

#include "dp/display_processor_base.hh"

class UCharGrid;

class DistributionSlicer : public DisplayProcessorBase {

public:
  DistributionSlicer				// constructor
    (int coding_levels,				//   maximum number of codes
     float min_bin,				//   min value assgned to a bin
     float max_bin,				//   max value assgned to a bin
     int min_bin_sum,				//   min bin count sum
     int max_bin_sum,				//   max bin count sum
     float *upper_bin_limit,			//   init upper bin limit vlu
     float *lower_bin_limit);			//   init lower bin limit vlu

  virtual ~DistributionSlicer ();		// destructor

  void setUpperBinLimit				// set an upper bin value limit
    (float upper_bin_limit);			//   value set

  void setLowerBinLimit				// set a lower bin value limit
    (float lower_bin_limit);			//   value set

  float getUpperBinLimit ()			// rtrn the uppr bin valu limit
    {return _upper_bin_limit;}

  float getLowerBinLimit ()			// rtrn the lowr bin valu limit
    {return _lower_bin_limit;}

  void setValueDisposition			// set how to select decoder
    (long value_disposition)			//   0-Medn, 1-Averg, 2-Selct
    { _value_disposition =
      (unsigned char)value_disposition; }

  void setInsertVectorFlag			// set whether to allow max vec
    (int allow_insert_vector)			//   1->attemp to use max vects
    { _allow_insert_vector =
      allow_insert_vector; }

  int getMinimumUpperBinLimit ();		// return a min upper Bin limit

  int getMaximumUpperBinLimit ();		// return a max upper Bin limit

  int getMinimumLowerBinLimit ();		// return a min lower Bin limit

  int getMaximumLowerBinLimit ();		// return a max lower Bin limit

  int quantizeBinCounts ();			// quantize the counts in bins

  int initialize				// assign a UCharGrid object
    (UCharGrid *grid);				//   object to assign

  UCharGrid *getResult ()			// returns UCharGrid result *
    {return _distr_slicer_grid;}

// pure virtual function
  virtual unsigned char *getArray ();		// returns unsigned char array*

// pure virtual function
  virtual int prepareUse ();			// prepare the Z-LUT for use

// pure virtual function
 virtual void getOneCode			// returns one RGBZ decoder elm
    (int index,					//   RGBZ decoder index to get
     float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute);				//   attribute decoder value

// pure virtual function
  virtual void getNextCode			// returns nxt RGBZ decoder elm
    (float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute);				//   attribute decoder value

// pure virtual function
  virtual int execute				// distr slice a subregn of grd
    (int min_z_bin,				//   first column to modify
     int min_x_bin,				//   first row to modify
     int num_z_bins,				//   # of columns to modify
     int num_x_bins);				//   # of rows to modify

private:
  int findBinCountInfo ();			// find data about cnts in bins

  float snapUpperBinLimit			// rtrns valid upper bin limit
    (float upper_bin_limit);			//   given upper bin limit

  float validateUpperBinLimit ();		// makes sure upper bin lmt OK

  float snapLowerBinLimit			// rtrns valid lower bin limit
    (float lower_bin_limit);			//   given lower bin limit

  float validateLowerBinLimit ();		// makes sure lower bin lmt OK

  int ZLUTSize ();				// rtrns rqrd size of Z-LUT

  int sortDecoder ();				// incr sort of decoder by maxs

// pure virtual function
  virtual void computeElement			// compute a decoder element
    (float *red,				//   address of red value
     float *green,				//   address of green value
     float *blue,				//   address of blue value
     float *attribute);				//   address of attribute value

// pure virtual function
  virtual int preModify				// set a subregion of grid
    (int min_z_bin,				//   first column to modify
     int min_x_bin,				//   first row to modify
     int num_z_bins,				//   # of columns to modify
     int num_x_bins);				//   # of rows to modify

// pure virtual function
  virtual int modify ();			// operate distr slicer on grid

  UCharGrid
    *_grid,					// unsigned char grid to analyz
    *_distr_slicer_grid;			// encoded distrib slicer grid

  int
    _min_bin_count,				// minimum count in a bin
    _max_bin_count,				// maximum count in a bin
    _bin_count_incr,				// quantized bin count incr
    _min_bin_sum,				// minimum bin count sum
    _max_bin_sum,				// maximum bin count sum
    _num_bins,					// # of distribtution bins
    _allow_insert_vector;			// 1->attempt to use max vects

  float
    _bin_width,					// width of a bin
    _min_bin,					// minimum valu assigned to bin
    _max_bin,					// maximum valu assigned to bin
    _upper_bin_limit,				// current upper bin limit
    _lower_bin_limit,				// current lower bin limit
    _epsilon;					// min bin limit diff allowed

  unsigned char
    _value_disposition;				// how to select decoder vlus

};

#endif
