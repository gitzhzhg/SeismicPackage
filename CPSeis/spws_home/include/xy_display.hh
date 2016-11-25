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
/*-------------------------------------------------------------------------
 *USER DOC
 * File        : xy_display_.hh
 * Author      : Michael L. Sherrill 
 * Date        : 11/1/91 (cc version 4/97)
 *
 *
 *NOTES:
 * 1. Header for mouse readout class.
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *
 *END DOC
 * -------------------------------------------------------------------------*/

#ifndef __XYDISPLAY
#define __XYDISPLAY

#include <X11/Intrinsic.h>
#include "plot_image.hh"


/*------------------------------------------------------------------------
 This class controls the xy output as the mouse moves
 -----------------------------------------------------------------------*/
class XYdisp {         
  public:
    Widget           xloc;
    Widget           yloc;
    Widget           zloc;
    char             *override_x_str;
    char             *override_y_str;
    int              status;
    Boolean          init;
    long             mouse_readout_type;
    int              _x_readout_header;
    int              _alt_y_readout_header;
    ImageXYOutputFunction outfunc;/* called with text widget write */
    void             *outdata;/* data for function */
    Boolean          interpolate_readout;
    float            not_defined_value;
    XYdisp() : _x_readout_header(-1), _alt_y_readout_header(-1) {};
    ~XYdisp(){};
    int getMouseReadoutType(){return mouse_readout_type;}
    void setMouseReadoutInterpolation(Boolean b, float v)
                {interpolate_readout = b; not_defined_value = v;}
    void    setXReadoutHeader(int h) {_x_readout_header= h;}
    int     getXReadoutHeader() {return _x_readout_header;}
    void    setAltYReadoutHeader(int h) {_alt_y_readout_header= h;}
    int     getAltYReadoutHeader() {return _alt_y_readout_header;}

};



#endif
