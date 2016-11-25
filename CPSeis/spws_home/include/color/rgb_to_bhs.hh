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
// class to transform RGB values to BHS and reverse
#ifndef RGB_TO_BHS_HH
#define RGB_TO_BHS_HH

class RGBToBHS {

public:
  RGBToBHS ();					// constructor

  void setRGB					// set current to given RGB
    (float red,					//   current red
     float green,				//   current green
     float blue);				//   current blue

  void setBHS					// set current to given BHS
    (float brightness,				//   current brightness
     float hue,					//   current hue
     float saturation);				//   current saturation

  void RGB					// return current RGB
    (float *red,				//   current red
     float *green,				//   current green
     float *blue);				//   current blue

  void BHS					// return current BHS
    (float *brightness,				//   current brightness
     float *hue,				//   current hue (radians)
     float *saturation);			//   current saturation

  float distance				// return distance from currnt
    (float red,					//   given red
     float green,				//   given green
     float blue);				//   given blue

private:
  double
    _sqrt3,					// sqrt (3)
    _red,					// current red
    _green,					// current green
    _blue;					// current blue

};

#endif
