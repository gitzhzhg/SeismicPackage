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
//author Michael L. Sherrill
//class that creates menu for making underlay images
#ifndef SEISUNDERPOP_H
#define SEISUNDERPOP_H

#include "seis_select.hh"
#include "sl/sl_form.hh"

class SeisPlot;
class SLTextBox;
class SLTogBox;
class UnderInform;



class SeisUnderPop :  public SeisSelect {

  protected:
       virtual void    DoAction();
       virtual Boolean ValidInput();
       SLTextBox       *_headers;
       SLTextBox       *_umovie;
       SLForm          *_umovie_form;
       SLTogBox        *_ifumovie;
       long            _uframes;          
       long            _doumovie;
       long            _mheader;
       SeisPlot        *_overlay_sp;
       UnderInform     *_inform;
       void            preManage();
       virtual SeisPlot *getSP();
         
  public:
       SeisUnderPop( Widget    p,
                     char      *name,
                     HelpCtx   hctx,
                     SeisPlot  *underlay_sp,
                     SeisPlot  *overlay_sp,
                     Boolean   allow_selector = False);
                   
       virtual ~SeisUnderPop();
       Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean);
       virtual void setFromOther(SeisPlot *othersp =NULL);
       virtual void notCurrentInWindow(SeisPlot *sp);

       virtual void getTracePattern
         (long *nplt,
	  long *iskp,
	  long *ndo,
	  long *nskp,
	  long *frames,
	  long *domovie);

       virtual void setNumMovieFrames
         (long num_frames);

       virtual void setMovie
         (Boolean domovie);

       friend class UnderInform;
};
#endif

