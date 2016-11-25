#include <stdio.h>
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
#include <assert.h>
#include "hardcopy/hardcopy_color.hh"
#include "cgm.h"



#define True  1
#define False 0



HardCopyColor::HardCopyColor()
{
  for(int i=0; (i<MaxColors); _colors[i++]= NULL);
  defineColorIndex(1.0, 1.0, 1.0); 
  defineColorIndex(0.0, 0.0, 0.0); 
}

HardCopyColor::~HardCopyColor()
{
  for(int i=0; (i<MaxColors); i++)
        if (_colors[i]) delete _colors[i];
  

}


int HardCopyColor::defineColorIndex(float percent_red,
                                    float percent_green,
                                    float percent_blue)
{
  int retval= 0;
  int found= False;
  for(int i=0; (i<MaxColors && !found); i++) {
          if (_colors[i]) {
                if (_colors[i]->red   == percent_red &&
                    _colors[i]->blue  == percent_blue &&
                    _colors[i]->green == percent_green ) {
                             found= True; 
                             retval= i;
                             //printf("found: %f %f %f\n", _colors[i]->red,
                             //                            _colors[i]->green,
                             //                            _colors[i]->blue );
                }

          }
          else {
                cgmGscr(1, i, percent_red, percent_green,
                                 percent_blue);
                _colors[i]= new ColorValues;
                _colors[i]->red=   percent_red;
                _colors[i]->green= percent_green;
                _colors[i]->blue=  percent_blue;
                found= True; 
                retval= i;
                //printf("added: %f %f %f\n", _colors[i]->red,
                //                            _colors[i]->green,
                //                            _colors[i]->blue );

          }
  }
  assert(found);

  return retval;
}
