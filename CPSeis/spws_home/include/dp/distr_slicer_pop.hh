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
// Works with the DistributionSlicer object to allow the user to interactively
//   change the upper and lower bin limits in real-time and allow setting
//   intensity
#ifndef DISTR_SLICER_POP_HH
#define DISTR_SLICER_POP_HH

#include <assert.h>
#include "sl/sl_scale.hh"
#include "dp/rgbz_pop.hh"

class DSUpperLimitScale;
class DSLowerLimitScale;
class DistributionSlicer;
class SeisPlot;
class SeisColor;

class DistrSlicerPop : public RGBZPop {

protected:
  DistributionSlicer
    *_ds;					// given DS object ptr

  DSUpperLimitScale
    *_dsupperscale;				// created DS upper lmt scl obj

  DSLowerLimitScale
    *_dslowerscale;				// created DS lower lmt scl obj

public:
  DistrSlicerPop				// constructor
    (Widget p,					//   parent widget
     char *name,				//   name of pop up
     DistributionSlicer *ds,			//   gvn DistriSlicer objct ptr
     SeisPlot *sp,				//   gvn SeisPlot object ptr
     HelpCtx hctx);				//   context sensitive help

  virtual ~DistrSlicerPop ();			// destructor

  virtual Widget make				// makes pop up
    (Widget p);					// gvn widget to attach pop up

  friend class DSUpperLimitScale;			// let this one get DSP's data

  friend class DSLowerLimitScale;			// let this one get DSP's data

};


class DSUpperLimitScale : public SLScaleDrag {

public:
  DSUpperLimitScale				// constructor
    (SLDelay *contain,				//   parent container of scale
     char *name,				//   name of scale
     HelpCtx hctx,				//   context sensitive help
     int *valptr,				//   value pointer
     DistrSlicerPop *dsp);			//   DSP object ptr

protected:
  virtual void ScaleAction			// call back on scale action
    (int val);					//   value incoming

  virtual void ScaleActionDrag			// call back on scale dragging
    (int val);					//   value incoming

  virtual void ScaleActionVC			// call back on variable chngin
    (int val);					//   value incoming

  DistrSlicerPop
    *_dsp;					// pointer to calling DSP obj

};


class DSLowerLimitScale : public SLScaleDrag {

public:
  DSLowerLimitScale				// constructor
    (SLDelay *contain,				//   parent container of scale
     char *name,				//   name of scale
     HelpCtx hctx,				//   context sensitive help
     int *valptr,				//   value pointer
     DistrSlicerPop *dsp);			//   DSP object ptr

protected:
  virtual void ScaleAction			// call back on scale action
    (int val);					//   value incoming

  virtual void ScaleActionDrag			// call back on scale dragging
    (int val);					//   value incoming

  virtual void ScaleActionVC			// call back on variable chngin
    (int val);					//   value incoming

  DistrSlicerPop
    *_dsp;					// pointer to calling DSP obj

};

#endif
