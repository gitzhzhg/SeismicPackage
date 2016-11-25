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
#ifndef FGMAPCONTORLPOP_HH
#define FGMAPCONTORLPOP_HH


#include "sl/sl_form_pop.hh"

class FgMap;
class SLRadioBox;
class SLPushBox;



class FgMapControlPop : public SLFPopSep {

  protected:
     //virtual void removeButton();
     SLRadioBox   *_pick_type;
     SLRadioBox   *_plot_type;
     SLRadioBox   *_display_type;
     SLPushBox    *_buttons;
     FgMap        *_fgmap;
     Boolean      _changing;

  public:
     FgMapControlPop(SLDelay *contain, char *name, FgMap *fgmap);
     ~FgMapControlPop();
     virtual Widget make(Widget p);
     virtual Boolean notifyComplex(SLDelay*,int);
     void changeSelectedPicking(int ident);
     void changeSelectedPlot(int ident);
     void changeSelectedDisplay(int ident);
     int currentPicking();
     int currentSelectedPlot();
     int currentDisplay();

};
#endif

