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
#ifndef SEISSELECTPOP_H
#define SEISSELECTPOP_H

#include "seis_select.hh"
#include "sl/sl_form.hh"

class SeisPlot;
class SLTextBox;
class SLTogBox;

class SeisSelectPop :  public SeisSelect {

  private:
       static void togChanged(void *data, long ident, Boolean set);
       static void movieChanged(void *data, long);
  protected:
       virtual void    DoAction();
       virtual void    UndoInput();
       virtual Boolean ValidInput();

       SLTextBox *_movie;
       SLForm    *_movie_form;
       SLTogBox  *_ifmovie;


  public:
       SeisSelectPop( Widget    p,
                      char      *name,
                      HelpCtx   hctx,
                      SeisPlot  *sp,
                      SeisPlot  *info_sp =NULL,
                      Boolean   allow_selector = False);
       virtual ~SeisSelectPop();

       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean);
       virtual void setNumMovieFrames(long num_frames);
       virtual void setMovie (Boolean domovie);
};

#endif
