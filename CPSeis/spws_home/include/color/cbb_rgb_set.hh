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
// class that creates a color bar builder RGB set object
#ifndef CBB_RGB_SET_HH
#define CBB_RGB_SET_HH

#include "dp/display_processor_base.hh"

class ColorDescriptor;
class AmplitudeProcessor;

class CBBRGBSet : public DisplayProcessorBase {

public:
  CBBRGBSet					// constructor
    (AmplitudeProcessor *amp);			//   amplitude processor

  ~CBBRGBSet ();				// destructor

  virtual int prepareUse ();			// prepare Z-LUT for use

  int initializeLUTs				// initialize the LUTs
    (ColorDescriptor *color_info);		//   color info descriptor

  virtual void getOneCode			// returns one RGBZ coder elem
    (int index,					//   RGBZ decoder index to get
     float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute);				//   attribute decoder value

  virtual void getNextCode			// returns nxt RGBZ coder elem
    (float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute);				//   attribute decoder value

  int getIndex					// return the index assoc w/
    (float attribute);				//   the given attribute

  int getPixel					// return pixel value given
    (int index);				//   RGBZ decoder index

  float getAttribute				// return attribute given
    (int index);				//   RGBZ decoder index

protected:
  virtual void computeElement			// returns nxt RGBZ coder elem
    (float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute);				//   attribute decoder value

private:
  AmplitudeProcessor
    *_amp;					// amplitude processor

};

#endif
