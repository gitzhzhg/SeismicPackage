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
#ifndef VACPSHAREDACTIONS_HH
#define VACPSHAREDACTIONS_HH

#include <Xm/Xm.h>
#include "oprim/general_object.hh"
#include "sp/seis_inform.hh"

class VaVectColors;
class VaCmpPlot;
class VaSemblancePlot;
class VaGvsPlot;

class VaCpShareActions : public SeisInform, public GeneralObject {

   private:
      VaVectColors  *_vect_colors;
      VaCmpPlot     *_cmp_plot;
      Widget         _w;

      static void gotClient(void *);
      
   public : 
      VaCpShareActions(Widget          w, 
                       VaVectColors    *vect_colors, 
                       VaCmpPlot       *cmp_plot,
                       VaSemblancePlot *semb_plot,
                       VaGvsPlot       *gvs_plot);
      ~VaCpShareActions();

  virtual void prePlot(SeisPlot *);
      void showOverlays(Boolean show);
      Boolean  applyNMOSensitive();
      Boolean  removeNMOSensitive();
      void applyNMO();
      void removeNMO();
      virtual void newPlot(SeisPlot *sp);
      virtual void noPlotDisplayed(SeisPlot *sp);
      virtual void postMovie(SeisPlot *, SeisPlot::MovieDir );
      virtual void notCurrentInWindow(SeisPlot *);
};



#endif
