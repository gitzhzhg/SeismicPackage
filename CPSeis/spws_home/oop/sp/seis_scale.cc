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
#include "sp/seis_scale.hh"



IntensityScale::IntensityScale(SLDelay       *contain,
                        char          *name,
                        HelpCtx       hctx,
                        int           *valptr,
                        SeisColor     *scp)
       : SLScaleDrag(contain,name,hctx,valptr),_scp(scp)
{

}

CompressionScale::CompressionScale(SLDelay       *contain,
                                   char          *name,
                                   HelpCtx       hctx,
                                   int           *valptr,
                                   SeisColor     *scp)
	 : SLScaleDrag(contain,name,hctx,valptr),_scp(scp)
{

}


IntensityScale::~IntensityScale()
{
}


CompressionScale::~CompressionScale()
{
}


void IntensityScale::ScaleAction(int /*val*/)
{
}

void IntensityScale::ScaleActionDrag(int val)
{
  _scp->setIntensity(val);
}

void IntensityScale::ScaleActionVC(int val)
{
  _scp->setIntensity(val);
}

void CompressionScale::ScaleAction(int /*val*/)
{
}

void CompressionScale::ScaleActionDrag(int val)
{
  _scp->setCompression(val);
}

void CompressionScale::ScaleActionVC(int val)
{
  _scp->setCompression(val);
}
