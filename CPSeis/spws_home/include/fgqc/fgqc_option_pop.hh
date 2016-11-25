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
//The following class allows the option menu to set it's buttons attachements
//which prevents circular dependency warnings

#include "fgqc_plot.hh"

class FgQcOptionPop : public SLFPopSep{

      private:
        FgQcPlot     *_qc_plot;
        SLPushBox    *_buttons;     
        Boolean       _menu_op;
        Boolean       _color_op;
        Boolean       _cbar_op;
        Boolean       _zoomupseparate;
        Boolean       _zoomup;
        Boolean       _zoomdown;
        Boolean       _zoomoriginal;
        Boolean       _hill_shader;
        Boolean       _map_overlay;
        Boolean       _picking_menu;

      public :
        FgQcOptionPop(FgQcPlot *p, char *name,HelpCtx  hctx);
  ~FgQcOptionPop(){};
        Widget make(Widget p);
        void setOptions( int ident, Boolean state, Boolean set_all = False);    
};
