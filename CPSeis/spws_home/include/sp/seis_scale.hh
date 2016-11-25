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
#ifndef SEIS_SCALE_H
#define SEIS_SCALE_H

#include "sl/sl_scale.hh"
#include "sp/seis_color.hh"




class IntensityScale : public SLScaleDrag {

 protected:
       SeisColor       *_scp;
       virtual void    ScaleAction(int val);
       virtual void    ScaleActionDrag(int val);
       virtual void    ScaleActionVC(int val);

 public:
       IntensityScale(  SLDelay       *contain,
                        char          *name,
                        HelpCtx       hctx,
                        int           *valptr,
                        SeisColor     *scp);

       virtual ~IntensityScale();

};


class CompressionScale : public SLScaleDrag {

 protected:
       SeisColor       *_scp;
       virtual void    ScaleAction(int val);
       virtual void    ScaleActionDrag(int val);
       virtual void    ScaleActionVC(int val);

     public:
       CompressionScale(SLDelay       *contain,
                        char          *name,
                        HelpCtx       hctx,
                        int           *valptr,
                        SeisColor     *scp);

       virtual ~CompressionScale();

     };

#endif





