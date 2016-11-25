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
#ifndef HARDCOPY_SEIS_POP_HH 
#define HARDCOPY_SEIS_POP_HH

#include "hardcopy/hardcopy_pop.hh"

class SLTextBox;


class HardCopySeisPop : public HardCopyPop 
{
   private:
        float         _is;
        float         _ti;
        SLTextBox    *_scale;

   public:
        HardCopySeisPop(Widget        p,
                        char         *name,
                        HelpCtx       hctx,
                        SeisPlot     *sp,
			SLpFileData  *slp_file_data,
                        float        marker_scale_factor = 0.0,
                        Boolean      allow_scale_change  = False);
        ~HardCopySeisPop();
        virtual Boolean notifyComplex(SLDelay*, int ident);
        virtual Widget  make(Widget p);
        virtual void computeWidthHeight( float *width, float *height);
        virtual void unitChange(SeisPlot*, int);
        virtual void managing();
};
#endif
