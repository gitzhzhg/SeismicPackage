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
// hill_shader_mag.hh:  User interface file for HillShader class
#ifndef HILL_SHADER_MAG_HH
#define HILL_SHADER_MAG_HH

#include "dp/display_processor_base.hh"

class FloatGrid;

class HillShader : public DisplayProcessorBase {

public:
    enum Type {
      COLOR_ATTRIBUTE,
      COLOR_ATTRIBUTE_CONTOURS,
      B_W_ATTRIBUTE,
      B_W_STRUCTURE,
      B_W_ATTRIBUTE_CONTOURS
    };

  HillShader					// constructor
    (int coding_levels);			//   number of levels for codin

  virtual ~HillShader ();			// destructor

  int setAzimuth				// sets hill-shade azimuth
    (float azimuth_degrees=45);			//   illumination azimuth (deg)

  int setElevation				// sets hill-shade elevation
    (float elevation_degrees=45);		//   illumination elevatn (deg)

  void setColorAttribute ();			// do color attribute HS dsplys

  void setColorAttributeContours ();		// do color contour display

  void setBlackAndWhiteAttribute ();		// do B&W attribute HS displays

  void setBlackAndWhiteStructure ();		// do B&W structure HS displays

  void setBlackAndWhiteAttributeContours ();	// do B&W attrib contour disply

  int initialize				// assign input grids
    (FloatGrid *structure,			//   structure grid
     FloatGrid *attribute=0);			//   attribute grid

  float *getResult ();				// returns pntr to HS array

// pure virtual function
  unsigned char *getArray ()			// returns pntr to HS array
    { return _hill_shade_array; }

// pure virtual function
  virtual int prepareUse ();			// prepare use of the RGBZ LUT

// pure virtual function
  virtual void getOneCode			// returns one RGBZ decoder elm
    (int index,					//   RGBZ decoder index to get
     float *red,				//   red       decoder value
     float *green,				//   green     decoder value
     float *blue,				//   blue      decoder value
     float *attribute);				//   attribute decoder value

// pure virtual function
  virtual void getNextCode			// returns nxt RGBZ decoder elm
    (float *red,				//   red       decoder value
     float *green,				//   green     decoder value
     float *blue,				//   blue      decoder value
     float *attribute);				//   attribute decoder value

  virtual int execute				// hill-shade a subregn of grid
    (int first_column,				//   first column to modify
     int first_row,				//   first row to modify
     int column_count,				//   # of columns to modfiy
     int row_count);				//   # of rows to modify

  virtual void reset ();			// reset the hill shader

  int type ()					// return hill shader type
    { return _type; }

  void setType					// return hill shader type
    (Type type)					//   set hill shader type
    { _type = type; }

  int attributeAndStructureDiffer ()		// do attrib and struct differ
    { return _attribute != _structure; }

private:
  int setIllumination ();			// sets hill-shade illumination

  int setBlackAndWhite ();			// initializes B&W attributes

  virtual void computeElement			// compute the decoder values
    (float *red,				//   red       decoder value
     float *green,				//   green     decoder value
     float *blue,				//   blue      decoder value
     float *attribute);				//   attribute decoder value

  virtual int preModify				// set a subregion of grid
    (int first_column,				//   first column to modify
     int first_row,				//   first row to modify
     int column_count,				//   # of columns to modfiy
     int row_count);				//   # of rows to modify

  virtual int modify ();			// operate hill-shader on grid

  void reclaimMemory ();			// delete arrays and objects

  FloatGrid
    *_s,					// temp grid for incoming strct
    *_a,					// temp grid for incoming attrb
    *_dx,					// temp grid for dx/ds
    *_dy,					// temp grid for dy/ds
    *_structure,				// pntr to incoming structure
    *_attribute;				// pntr to incoming attribute

  Type
    _type;					// type of display to do

  float
    _hsr_gain,					// gain to take HS reslt to 0-1
    _hsr_bias,					// bias to take HS reslt to 0-1
    _bw_gain,					// gain to take z to 0-1
    _bw_bias,					// bias to take z to 0-1
    _inv_z_gain,				// gain to take z from byte
    _inv_z_bias,				// bias to take z from byte
    _inv_dx_gain,				// gain to take dx from byte
    _inv_dx_bias,				// bias to take dx from byte
    _inv_dy_gain,				// gain to take dy from byte
    _inv_dy_bias,				// bias to take dy from byte
    *_hill_shade_array_float,			// float encoded HS array optn
    _max_level,					// original _coding_levels-1
    _z_gain,					// gain takes z from float->UC
    _z_bias,					// bias takes z from float->UC
    _dx_gain,					// gain takes dx from float->UC
    _dx_bias,					// bias takes dx from float->UC
    _dy_gain,					// gain takes dy from float->UC
    _dy_bias,					// bias takes dy from float->UC
    _ca,					// cosine of illumination azim
    _sa,					// sine of illumination azimuth
    _ce,					// cosine of illumination elev
    _se,					// sine of illumination elevat
    _xc,					// constant x-illumin rotation
    _yc;					// constant y-illumin rotation

  double
    _degrees_to_radians;			// convert degrees to radians

  unsigned char
    *_a_dx_dy,					// temp array to hold 3 attrib
    *_hill_shade_array;				// encoded hill shade array

};

#endif
