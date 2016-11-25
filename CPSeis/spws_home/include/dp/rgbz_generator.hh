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
// rgbz_generator.hh:  User interface file for RGBZGenerator base class
#ifndef RGBZ_GENERATOR_HH
#define RGBZ_GENERATOR_HH

#include "dp/array_remapper.hh"
#include "dp/grid_error_codes.hh"

class RGBZGenerator {

public:
  RGBZGenerator	();				// constructor

  virtual ~RGBZGenerator ();			// destructor

  int setSize					// set the RGBZ LUT size to use
    (int rgbz_count);				//   number of RGBZ entries

  void setOne					// set one RGBZ LUT element
    (int index,					//   RGBZ element to set
     float red,					//   red value
     float green,				//   green value
     float blue,				//   blue value
     float attribute);				//   attribute value

  void setNext					// set next RGBZ LUT element
    (float red,					//   red value
     float green,				//   green value
     float blue,				//   blue value
     float attribute);				//   attribute value

// pure virtual function
  virtual int prepareUse () = 0;		// prepare use of the RGBZ LUT

  int getSize ()				// get the RGBZ LUT size used
    { return _rgbz_count; }

  void getOne					// get one RGBZ LUT element
    (int index,					//   RGBZ element to get
     float *red,				//   red value
     float *green,				//   green value
     float *blue,				//   blue value
     float *attribute);				//   attribute value

  void getNext					// get next RGBZ LUT element
    (float *red,				//   red value
     float *green,				//   green value
     float *blue,				//   blue value
     float *attribute);				//   attribute value

// pure virtual function
  virtual void getOneCode			// returns one RGBZ decoder elm
    (int index,					//   RGBZ decoder index to get
     float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute) = 0;			//   attribute decoder value

// pure virtual function
  virtual void getNextCode			// returns nxt RGBZ decoder elm
    (float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute) = 0;			//   attribute decoder value

  int setIntensity				// set RGB desired gain factor
    (long intensity);				//   times 10 here

  int setCompression				// set RGB compression factor
    (long compression);				//   here (0 -> no compression)

  int failed ();				// return 1 if unsuccessful

  virtual GridErrorCodes errorStatus ()			// return error status flag
    { return _error_status; }

protected:
  virtual void reset ();			// initialize object

  int newZVaries ();				// rtns 1 if newZ's are unequal

  int ZVaries ();				// rtns 1 if rgbZ's are unequal

  int ZLUTAvailable				// rtns 1 if ZLUT array exists
    (int z_lut_size);				//   size necessary for ZLUT

  void clearZs ();				// sets Z's in RGBZ array to 0

  ArrayRemapper
    *_rgbz_remapper;				// optional RGBZ remapper objct

  int
    _z_changed,					// switch set when Z's change
    *_z_lut,					// Z-LUT between RGBZ & decoder
    _z_lut_size,				// current size of Z-LUT
    _offset,					// current offset for RGBZ arrs
    _z_lut_offset,				// current offset for Z-LUT
    _rgbz_count;				// # of entries in rgbz LUT

  float
    *_rgbz,					// red, green, blue, Z LUT
    *_newz;					// incoming new Z's

  long
    _intensity,					// desired RGB gain times 10
    _compression;				// compression factr 0 -> none

private:
  GridErrorCodes
    _error_status;				// current error status flag
  
  void reclaimMemory ();			// delete arrays and objects
};

#endif
