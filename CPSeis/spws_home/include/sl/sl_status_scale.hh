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
#ifndef SL_STATUS_SCALE_HH
#define SL_STATUS_SCALE_HH

#include "sl/sl_form_pop.hh"
#include "sl/sl_scale.hh"

class SLScale;

class SLStatusScale : public SLFPopSep {

public:
  SLStatusScale					// constructor
    (Widget p,					//   parent widget
     char *name,				//   name of instance
     HelpCtx hctx = 0,				//   context sensitive help
     char *title = 0);				//   title

  virtual ~SLStatusScale ();			// destructor

  virtual Widget make 				// make the graphic
    (Widget p = 0);				//   parent widget
    
  void setPercent				// set status scale percent
    (float percent);				//   value from 0 to 100

  SLScale *getScale(){ return _scale;}


protected:
  virtual void DoAction ();			// do action function

  virtual void managing ();			// called by manage()

  SLScale
    *_scale;					// scale object

  char
    *_title;					// title for container

  float
    _percent;					// value from 0 to 100
};

#endif
