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
//author Michael L. Sherrill 11/93
//used for color bar popup
#ifndef SEISCBARPOP_H
#define SEISCBARPOP_H

#include "sl/sl_scale.hh"
#include "sl/sl_form_pop.hh"
#include "sp/seis_cbar.hh"
#include "sp/seis_color.hh"
#include "sp/seis_inform.hh"
#include "sp/seis_scale.hh"

class CbarInform;


class SeisCbarPop :  public SLFPopSep {

    protected:
       Boolean          _first_time;
       SeisPlot         *_sp;
       int              _color_num;
       SeisCbar         *_cbar;
       ColorInfo        _col;
       float            *_rgb;
       IntensityScale   *_intensityscale;
       CompressionScale *_compressionscale;
       virtual void    DoAction();
     public:
       SeisCbarPop( Widget               p,
                    char                 *name,
                    SeisPlot             *sp,
                    HelpCtx              hctx);
       virtual ~SeisCbarPop();
       CbarInform   *_ci;
       virtual Widget make(Widget p);
       virtual void manage();
       friend  class IntensityScale;
       friend  class CompressionScale;
       void addSP(SeisPlot *sp);
       void removeSP(SeisPlot *sp);
};



class CbarInform : public SeisInform {

   private:
      
   protected:
     SeisCbar   *_cb;

   public:
     CbarInform( SeisPlot *sp)
               : SeisInform(sp), _cb(NULL) {};
     virtual ~CbarInform() {};
     virtual void newPlot(SeisPlot *sp);
     virtual void postScan(SeisPlot *sp, SeisPlot::ScanDir);
     virtual void notCurrentInWindow(SeisPlot *sp);
     void setCbar(SeisCbar *cb) {_cb = cb;}
   };





#endif





