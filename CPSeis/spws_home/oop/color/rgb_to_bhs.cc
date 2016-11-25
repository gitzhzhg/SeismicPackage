// class to transform RGB values to BHS and reverse
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
#include "color/rgb_to_bhs.hh"
#include <math.h>

RGBToBHS::RGBToBHS ()
{
  _sqrt3 = sqrt ((double)3);
}

void RGBToBHS::setRGB (float red, float green, float blue)
{
// store whatever is given (no checks on ranges)
//   valid ranges should be 0 <= red, green, blue <= 1
  _red   = (double)red;
  _green = (double)green;
  _blue  = (double)blue;
}

void RGBToBHS::setBHS (float brightness, float hue, float saturation)
{
// convert whatever is given into red, green blue (no checks on ranges)
//   valid ranges should be 0 <= brightness, saturation <= 1
//   and -pi <= hue <= +pi
  _red   = (double)brightness + (double)2 / (double)3 * (double)saturation *
    cos ((double)hue);

  _green = (double)brightness + (double)saturation / (double)3 *
    cos ((double)hue) * (_sqrt3 * tan ((double)hue) - (double)1);

  _blue  = (double)brightness - (double)saturation / (double)3 *
    cos ((double)hue) * (_sqrt3 * tan ((double)hue) + (double)1);
}

void RGBToBHS::RGB (float *red, float *green, float *blue)
{
// return current red, green, and blue within acceptable ranges
//   valid ranges should be 0 <= red, green, blue <= 1
  float r, g, b;

  r      = _red < (double)0   ? (float)0 : (float)_red;
  *red   =    r > (float) 1   ? (float)1 : r;
 
  g      = _green < (double)0 ? (float)0 : (float)_green;
  *green =      g > (float) 1 ? (float)1 : g;

  b      = _blue < (double)0  ? (float)0 : (float)_blue;
  *blue  =     b > (float) 1  ? (float)1 : b;
}

void RGBToBHS::BHS (float *brightness, float *hue, float *saturation)
{
// convert current red, green, and blue to brightness, hue, and saturation
//   within acceptable ranges
//   valid ranges should be 0 <= brightness, saturation <= 1
//   and -pi <= hue <= +pi
  double x, y;
  float s, b;

  x = _red - (_green + _blue) / (double)2;

  y = _sqrt3 / (double)2 * (_green - _blue);

  b           = (float)((_red + _green + _blue) / (double)3);
  b           = b < (float)0 ? (float)0 : b;
  *brightness = b > (float)1 ? (float)1 : b; 

  *hue = (float)atan2 (y, x);

  s           = (float)sqrt (x*x + y*y);
  *saturation = s > (float)1 ? (float)1 : s;
}

float RGBToBHS::distance (float red, float green, float blue)
{
  double d;

  d = (red   - _red)   * (red   - _red)
    + (green - _green) * (green - _green)
    + (blue  - _blue)  * (blue  - _blue);

  return (float)(sqrt(d));
}
