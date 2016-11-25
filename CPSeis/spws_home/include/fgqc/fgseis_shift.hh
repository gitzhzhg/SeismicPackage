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
#ifndef FGSHIFT_HH
#define FGSHIFT_HH


#include "pick/seis_shift.hh"
#include "fgqc/fgqc_ovjd_pop.hh"
//type of last shift
#define LINEAR_SHIFT 0
#define NONLINEAR_SHIFT 1


//relative menu class
class SeisShiftPop;
class FieldGeometry;
class SeisPlot;

class FgSeisShift : public SeisShift {

   private:
     FgSeisOvjdPop  *_fop;
     FieldGeometry  *_fg;

   protected:

   public:
     FgSeisShift(SeisPlot *sp, FgSeisOvjdPop *fop,
                 FieldGeometry  *fg, float velocity = 5000.0, int header = 6);

       // for velocity type shifting
     Boolean linearShift(Boolean forward = True, Boolean make_plot = True);

       // for header word or other independent type shifting
     Boolean nonlinearShift(Boolean forward, float *shifts_in = NULL, 
                            Boolean make_plot = True);

};



#endif










