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
// Used to work with a HillShader object to allow the user to interactively
//   change the illumination angles in real time and allow setting intensity
#ifndef HILL_SHADER_POP_HH
#define HILL_SHADER_POP_HH

#include <assert.h>
#include "sl/sl_scale.hh"
#include "dp/rgbz_pop.hh"

class HillShaderAzimuth;
class HillShaderElevation;
class HillShader;
class SeisPlot;
class SeisColor;
class SLRadioBox;

class HillShaderPop :  public RGBZPop {

public:
  HillShaderPop
    (Widget p,					//   parent widget
     char *name,				//   name of pop up
     HillShader *hs,			        //   gvn HillShader objct ptr
     SeisPlot *sp,				//   gvn SeisPlot object ptr
     HelpCtx hctx);				//   context sensitive help

  virtual ~HillShaderPop ();			// destructor

  virtual Widget make				// makes pop up
    (Widget p);					// gvn widget to attach pop up

  virtual int getSeisPlotLUT ();		// grabs the current SP clr LUT

  friend class HillShaderAzimuth;			// let this one get HSP's data

  friend class HillShaderElevation;			// let this one get HSP's data

private:
  virtual int initializeHelper ();		// helper routine for intialize

  static void typeAction			// HS type on radio action
    (void *data,				//   object passed in
     long which);				//   selection

  void adjustSensitivity ();

  HillShader
    *_hs;					// given HS object ptr

  HillShaderAzimuth
    *_hillshaderazimuth;			// HS azimuth scale 

  HillShaderElevation
    *_hillshaderelevation;			// HS elevation scale

  SLRadioBox
    *_typerbox;					// radio box to select HS type

};





class HillShaderAzimuth : public SLScaleDrag {

public:
  HillShaderAzimuth				// constructor
    (SLDelay *contain,				//   parent container of scale
     char *name,				//   name of scale
     HelpCtx hctx,				//   context sensitive help
     int *valptr,				//   value pointer
     HillShaderPop *hsp);			//   HSP object ptr

protected:
  virtual void ScaleAction			// call back on scale action
    (int val);					//   value incoming

  virtual void ScaleActionDrag			// call back on scale dragging
    (int val);					//   value incoming

  virtual void ScaleActionVC			// call back on variable chngin
    (int val);					//   value incoming

  HillShaderPop
    *_hsp;					// pointer to calling HSP obj

};





class HillShaderElevation : public SLScaleDrag {

public:
  HillShaderElevation				// constructor
    (SLDelay *contain,				//   parent container of scale
     char *name,				//   name of scale
     HelpCtx hctx,				//   context sensitive help
     int *valptr,				//   value pointer
     HillShaderPop *hsp);			//   HSP object ptr

protected:
  virtual void ScaleAction			// call back on scale action
    (int val);					//   value incoming

  virtual void ScaleActionDrag			// call back on scale dragging
    (int val);					//   value incoming

  virtual void ScaleActionVC			// call back on variable chngin
    (int val);					//   value incoming

  HillShaderPop
    *_hsp;					// pointer to calling HSP obj

};

#endif
