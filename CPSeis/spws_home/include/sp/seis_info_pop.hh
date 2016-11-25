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
//class that displays a user's plotting parameters
#ifndef SEISINFOPOP_H
#define SEISINFOPOP_H

#include "sl/sl_scale.hh"
#include "sl/sl_form_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_inform.hh"

class SeisInfoPopInform;

class SeisInfoPop :  public SLFPopSep {

 private:

 protected:
       SeisPlot        *_sp;
       Boolean         _first_time;
       char            *_str;
       Widget          _pinfo;

 public:
       friend class SeisInfoPopInform;
       SeisInfoPop(  Widget               p,
                     char                 *name,
                     SeisPlot             *sp,
                     HelpCtx              hctx);
       virtual ~SeisInfoPop();
       SeisInfoPopInform *_sipi; 
       virtual Widget make(Widget p);
       virtual void manage();
       void getPlotInfo( Boolean inform_is_calling);
};




class SeisInfoPopInform : public SeisInform {

 private:

 protected:
     SeisInfoPop   *_sip;

   public:
     SeisInfoPopInform( SeisPlot *sp, SeisInfoPop *sip)
       : SeisInform(sp), _sip(sip){};
     virtual ~SeisInfoPopInform();
     virtual void newPlot(SeisPlot *sp);
     virtual void postZoom(SeisPlot *, SeisPlot::ZoomDir );
     virtual void postScan(SeisPlot *, SeisPlot::ScanDir );
     virtual void unitChange(SeisPlot *sp, int);
     virtual void notCurrentInWindow(SeisPlot *);
   };
#endif



