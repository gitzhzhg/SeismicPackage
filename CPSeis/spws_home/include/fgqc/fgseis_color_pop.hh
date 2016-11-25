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
//author Michael L. Sherrill 09/95
//class that creates color processing menu for use with fgqc plots
#ifndef FGSEISCOLORPOP_H
#define FGSEISCOLORPOP_H

#include "sp/seis_color_pop.hh"

class HillShaderPop;
class DistrSlicerPop;
class ColorLUT;
class FgQcPlotType;
class SLArrowTitle;

class FgSeisColorPop :  public SeisColorPop {
 private:

 protected:
       HillShaderPop  *_hsp;
       DistrSlicerPop *_dsp;
       ColorLUT       *_clt;
       virtual void   DoAction();
       SLRadioBox     *_texturebox;
       int            _texture_state;
       SLArrowTitle   *_point_sizer;
       int            _point_size;
       FgQcPlotType   *_plot_class;

 public:

       enum { SMOOTH_DATA, INSERT_DATA };

       FgSeisColorPop( Widget               p,
                       char                 *name,
                       SeisPlot             *sp,
                       HelpCtx              hctx);
       ~FgSeisColorPop();
       void manage();
       virtual void reloadDefaults(Boolean do_method = True);
       virtual void reloadSystemDefaults(Boolean do_method = True);
       void assignHillShaderPop (HillShaderPop  *hsp){_hsp = hsp;} 
       void assignDistrSlicerPop(DistrSlicerPop *dsp){_dsp = dsp;} 
       void assignColorLUT      (ColorLUT           *clt){_clt = clt;}
       int  colorLUTIsSet () {return (int)(_hsp || _dsp || _clt);}
       void disableOptions();
       void enableOptions();
       int Texture ();
       int Pointsize ();
       void setPlotClass (FgQcPlotType *plot_class=0);
};




#endif
