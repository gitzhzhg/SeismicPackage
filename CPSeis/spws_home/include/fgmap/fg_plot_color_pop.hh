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
#ifndef FGPLOTCOLORPOP_HH
#define FGPLOTCOLORPOP_HH

#include "sl/sl_form_pop.hh"

class SLRowColumn;
class SLColorOption;
class SLOptionMenu;
class SLpToggle;
class FgSeisPlotList;


class FgPlotColorPop : public SLFPopSep 
{
   private:
       FgSeisPlotList *_fgsp_list;
       SLRowColumn    *_rc_flags;
       SLRowColumn    *_rc_lines;
       SLRowColumn    *_rc_plot;

       SLColorOption  *_plot_background;
       SLOptionMenu   *_flag_mode;

       SLColorOption  *_active_flag;
       SLColorOption  *_selected_flag;
       SLColorOption  *_computed_flag;
       SLColorOption  *_default_flag;

       SLColorOption  *_active_line;
       SLColorOption  *_selected_line;
       SLColorOption  *_has_rec_line;
       SLColorOption  *_has_source_line;
       SLColorOption  *_has_both_line;
       SLColorOption  *_default_line;

       SLpToggle      *_use_active_flag;
       SLpToggle      *_use_selected_flag;
       SLpToggle      *_use_computed_flag;
       SLpToggle      *_use_default_flag;

       SLpToggle      *_use_active_line;
       SLpToggle      *_use_selected_line;
       SLpToggle      *_use_rec_line;
       SLpToggle      *_use_source_line;
       SLpToggle      *_use_both_line;
       SLpToggle      *_use_default_line;

       Boolean         _must_tidy_options;

   protected:
       virtual void  DoAction();
       virtual void  extraButton(int ident);
       void          setColors();
       virtual void  managing();
   public:
       FgPlotColorPop(Widget          p,
                      char           *name,
                      HelpCtx         hctx,
                      FgSeisPlotList *list);
       ~FgPlotColorPop();
       virtual Widget make(Widget p);
       virtual Boolean notifyComplex(SLDelay*, int ident);

};
#endif



