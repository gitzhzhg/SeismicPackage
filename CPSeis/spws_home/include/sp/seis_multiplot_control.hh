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
#ifndef SEISMULTIPLOTCONTROL_HH
#define SEISMULTIPLOTCONTROL_HH

#include "sl/sl_form_pop.hh"
#include "sp/seis_inform.hh"


class SeisPlot;
class SeisPlotTie;
class SeisPlotUnder;
class SeisSelectPop;
class SeisTiePop;
class SLPushBox;
class SLArrowScale;
class SLForm;
class SeisColorPop;
class SeisDifferencePop;
class SeisZoomOpPop;
class SeisCbarPop;
class SeisOvjd;
class SeisLavPop;

class SeisMultiPlotControl : public SeisInform, public SLFPopSep {

    public:
        SeisMultiPlotControl( Widget          p,
                              char           *name,
                              HelpCtx         hctx,
                              SeisPlotTie    *sp,
                              SeisPlot       *tie_sp,
                              SeisSelectPop  *select_pop,
                              SeisTiePop     *tie_pop,
                              SeisColorPop   *color_pop,
                              SeisZoomOpPop  *_zoom_pop,
                              SeisCbarPop    *cbar_pop,
                              SeisOvjd       *ovjd,
                              SeisLavPop     *lav_pop);

        void addNewMain();
        void addNewTie();
        void delMain();
        void delTie();

        virtual ~SeisMultiPlotControl();
        virtual Widget make(Widget p);
        virtual Boolean notifyComplex(SLDelay*, int);
        virtual void    managing();

        virtual void newPlot(SeisPlot * );
        virtual void addingTie(SeisPlotTie *, SeisPlot * );
        virtual void removingTie(SeisPlotTie *, SeisPlot *);
        virtual void notCurrentInWindow(SeisPlot *);
        virtual void newSeisPlotCreatedInWindow(SeisPlot *);
        virtual SeisPlot *getSeisPlot(char *filename);


    protected:
        SeisPlotTie       *_sp;
        SeisPlotTie       *_master_sp;
        SeisPlot          *_tie_sp;
        SLPushBox         *_main_push;
        SLPushBox         *_diff_push;
        SLPushBox         *_tie_push;
        SLArrowScale      *_main_control;
        SLArrowScale      *_tie_control;
        SeisDifferencePop *_difference_control;
        Widget            _main_filelab;
        Widget            _tie_filelab;
        int               _prev_main_value;
        int               _prev_tie_value;
        SeisSelectPop     *_select_pop;
        SeisTiePop        *_tie_pop;
        SeisColorPop      *_color_pop;
        SeisZoomOpPop     *_zoom_pop;
        SeisCbarPop       *_cbar_pop;
        SeisOvjd          *_ovjd;
        SeisLavPop        *_lav_pop;

        void changeMain(int val);
        void changeTie(int val);
        void setFilename(Widget w, SeisPlot *sp);
        void setMovieOrderFilenames();
        void setCurrentSP(class SeisWinMan *mgr, int val);



};




#endif
