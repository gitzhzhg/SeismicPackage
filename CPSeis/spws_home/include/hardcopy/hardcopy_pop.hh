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
#ifndef HARDCOPY_POP_HH 
#define HARDCOPY_POP_HH

#include "sl/sl_form_fc_pop.hh"
#include "sp/seis_inform.hh"


class SeisPlot;
class SLTextBox;
class SLRadioBox;
class SLTogBox;
class SLOptionMenu;


class HardCopyPop : public SLFormFCPop , public SeisInform {


   protected:
        float         _def_width;
        float         _def_height;
        float         _width;
        float         _height;
        float         _marker_scale_factor;
        long          _num_copies;
        Boolean       _do_anno;
        Boolean       _make_symetrical;
        Boolean       _using_symetrical;
        Boolean       _first_time_man;
        Boolean       _using_pip;
        char         *_submit_script;
        SeisPlot     *_sp;
        SLTextBox    *_plot_dim;
        SLTextBox    *_scale_input;
        SLTextBox    *_copies_text;
        SLRadioBox   *_submit_action;
        SLRadioBox   *_movie_style;
        SLTogBox     *_extra_input;
        SLOptionMenu *_queue;
        Widget        _total_dim;

        virtual void    DoAction();
        virtual Boolean ValidInput();
        virtual void    managing();
        Boolean scriptExist(Boolean always_show_error);
        void setTotalDim();
        enum {WIDTH, HEIGHT };


   public:

        enum PopType { PIP=0, SYMETRICAL, NONE}; 
        HardCopyPop(Widget            p, 
                    char              *name, 
                    HelpCtx           hctx, 
                    SeisPlot          *sp,
		    class SLpFileData *slp_file_data,
                    PopType           ptype= HardCopyPop::PIP,
                    Boolean           show_movie_style= True,
                    float             marker_scale_factor = 0.0,
                    Boolean           allow_scale_change  = False);
        ~HardCopyPop();
        virtual Widget  make(Widget p);
        virtual Boolean notifyComplex(SLDelay*, int ident);
        virtual void computeWidthHeight( float *width, float *height);
        virtual void notCurrentInWindow(SeisPlot *sp);
        virtual void newPlot(SeisPlot * );
        void setWidth(float w);
        void setHeight(float h);
        void setSeisPlot( SeisPlot *sp);
};
#endif
