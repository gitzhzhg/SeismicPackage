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
#ifndef SEISMOVIEPOP_H
#define SEISMOVIEPOP_H

#include "sl/SLFormPop.h"
#include "sl/SLTextBox.h"
#include "sl/PsuedoWidget.h"

class SeisPlot;


class SeisAnnoPop : public SLFPopSep {

  private:
  protected:
       virtual void    DoAction();
       virtual void    UndoInput();
       virtual Boolean ValidInput();

       SeisPlot *_sp;
       long     frames, skip_frames;
  public:
        SeisMoviePop( SLDelay   *contain,
                      char      *name,
                      HelpCtx   hctx,
                      SeisPlot  *sp );
        SeisMoviePop( Widget    p,
                      char      *name,
                      HelpCtx   hctx,
                      SeisPlot  *sp );
        virtual ~SeisMoviePop();
        void init();
        virtual Widget make(Widget p);
        virtual void reloadDefaults(Boolean do_method= True);
        void         reloadSystemDefaults();
        virtual void manage();
};



#endif
