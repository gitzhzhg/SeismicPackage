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
#ifndef SEISPLOTTIE_H
#define SEISPLOTTIE_H

#include "sp/seis_plot.hh"
#include "sl/wlist.hh"
#include <Xm/ScrollBar.h>


class ViewWin;
class WinCtl;
class TieInform;
class SLScrollWin;
class PaperPlot;
class HardCopyPlot;


class SeisPlotTie : public SeisPlot {

  private:
      static void scrollCallback(Widget w, XtPointer udata, 
                          XmScrollBarCallbackStruct  *CBdata );
  protected:
      void scroll(Widget w, XtPointer udata, XmScrollBarCallbackStruct *CBdata);
      void addAllCallbacks(Widget w);
      void removeAllCallbacks(Widget w);
      SeisPlot   *_tiesp;
      SeisPlot   *_header_graph;
      ViewWin    *_primsw;
      //SLScrollWin  *_slsw;
      Widget     _prim_vsb;  //, _tie_vsb;
      //WinCtl     *_winctl;
      //Wlist       _time_list;
      TieInform  *_inform;
      long        _lock_pt;
      //Boolean     _lock_sb;
      int         _old_prim_sb_value;
      int         _old_tie_sb_value;
      int         _whose_moving;
  public:
      SeisPlotTie( const Widget  p,
                   const char    *name ="plot",
		   const char    *colorset_name =NULL,
                   const int     ptype =PlotImage::PlotWFILL);
      SeisPlotTie( SeisPlotTie *sp);
      ~SeisPlotTie();
      virtual Widget W();
      virtual SeisPlot& operator=(SeisPlot& sp);
      void addTie(SeisPlot *sp);
      void delTie();
      void addHeaderGraph(SeisPlot *header_plot);
      void delHeaderGraph();
      Boolean doingTie();
      Boolean doingHeader();
      void showit();
      void showBar(Boolean bar =True);
      void addOutputWidget(Widget w);
      void delOutputWidget(Widget w);
      void displayTimeOffset();
      void resetTimeOffset();
      void scrollBarsLocked(Boolean lock =True);

      virtual void zoomUp();                   // zoom in place
      virtual int plot();
     // virtual SeisPlotZoom *zoomUpSeparateWin();
      virtual void zoomDown();                 // zoom in place
      virtual void originalSize();             // goto orginal size

      friend class TieInformSeisZoomer;
      friend class TieInform;
      friend class SeisHardCopy;

      virtual int writeHardCopy(SeisHardCopy *shc);
      virtual void computeHardWidthHeight(float   seismic_width,
                                          float   seismic_height,
                                          float  *tot_width,
                                          float  *tot_height,
                                          Boolean left_cbar,
                                          int     frame      = 1,
                                          Boolean panel      = False);


      virtual void computePaneledHardWidthHeight(float   seismic_width,
                                                 float   seismic_height,
                                                 float  *tot_width,
                                                 float  *tot_height,
                                                 Boolean left_cbar);

};




#endif
