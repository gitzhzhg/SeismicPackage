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
#ifndef FGSEISPLOTLIST_HH
#define FGSEISPLOTLIST_HH


#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include "fgxp/fgxp_constants.hh"
#include "wproc.h"
#include <X11/Intrinsic.h>

class FgSeisPlot;
class SeisZoomOpPop;
class FieldGeometry;
class FgMapToFlag;
class FgControlPop;
class TransformPop;
class FgPlotColorPop;
class ContainerList;



class FgSPElement : public Element
{
  friend class FgSeisPlotList;
  protected:
     FgSeisPlot *_sp;

     FgSPElement(FgSeisPlot *sp) : _sp(sp) {}
     ~FgSPElement() {}
     int operator ==(void * const sp) const 
                               { return((FgSeisPlot*) sp == _sp); }
     virtual void print() const {}
};



class FgSeisPlotList : public BaseLinkedList
{
  public:
   enum BackingStoreType {ALWAYS, NEVER, FOLLOW_SUGGESTION };
  private:
   FieldGeometry   *_fg;
   FgMapToFlag     *_xlat;
   SeisZoomOpPop   *_zoomop;
   HelpCtx          _hctx;
   FgControlPop    *_dcp;
   TransformPop    *_tp;
   ContainerList   *_all_pops;
   FgPlotColorPop  *_plot_colors;
   Boolean          _status_showing;
   BackingStoreType _bs_type;

   Pixel            _background_pixel;
   Boolean          _new_pixel;
   char            *_active_flag;
   char            *_selected_flag;
   char            *_computed_flag;
   char            *_default_flag;

   char            *_active_line;
   char            *_selected_line;
   char            *_has_rec_line;
   char            *_has_source_line;
   char            *_has_both_line;
   char            *_default_line;

   Boolean         _use_active_flag;
   Boolean         _use_selected_flag;
   Boolean         _use_computed_flag;
   Boolean         _use_active_line;
   Boolean         _use_selected_line;
   Boolean         _use_rec_line;
   Boolean         _use_source_line;
   Boolean         _use_both_line;

   FlagMode        _flagMode;

  public:
   FgSeisPlotList(Widget         w, 
                  FieldGeometry *fg, 
                  HelpCtx        hctx,
                  FgControlPop  *dcp =NULL,
                  TransformPop  *tp  =NULL,
                  ContainerList *all_pops =NULL);
   ~FgSeisPlotList();
   void add(FgSeisPlot *sp);
   SeisZoomOpPop   *zoomOpPop();
   FieldGeometry   *fieldGeometry();
   HelpCtx          helpCtx();
   FgMapToFlag     *translator();
   FgControlPop    *getDCP();
   TransformPop    *getTP();
   ContainerList   *getAllPops();
   FgPlotColorPop  *getPlotColorPop();
   void showMessageArea();
   void hideMessageArea();

   Pixel plotBgPixel();
   char *activeFlagColor();
   char *selectedFlagColor();
   char *computedFlagColor();
   char *defaultFlagColor();
   char *activeLineColor();
   char *selectedLineColor();
   char *receiverLineColor();
   char *sourceLineColor();
   char *bothLineColor();
   char *defaultLineColor();

   Boolean useActiveFlag();
   Boolean useSelectedFlag();
   Boolean useComputedFlag();
   Boolean useActiveLine();
   Boolean useSelectedLine();
   Boolean useReceiverLine();
   Boolean useSourceLine();
   Boolean useBothLine();

   void setPlotBgPixel(Pixel _background_pixel);
   void setActiveFlagColor(   char  *_active_flag,     Boolean use_it);
   void setSelectedFlagColor( char  *_selected_flag,   Boolean use_it);
   void setComputedFlagColor( char  *_computed_flag,   Boolean use_it);
   void setDefaultFlagColor(  char  *_default_flag);
   void setActiveLineColor(   char  *_active_line,     Boolean use_it);
   void setSelectedLineColor( char  *_selected_line,   Boolean use_it);
   void setReceiverLineColor( char  *_has_rec_line,    Boolean use_it);
   void setSourceLineColor(   char  *_has_source_line, Boolean use_it);
   void setBothLineColor(     char  *_has_both_line,   Boolean use_it);
   void setDefaultLineColor(  char  *_default_line );

   FlagMode flagMode();
   void setFlagMode(FlagMode flagMode);

   void updateColorsOnPlots();

   void setBackingStoreMode(BackingStoreType bs_type);
   BackingStoreType backingStoreMode();

   void remove(FgSeisPlot *sp);
   FgSeisPlot *top(void **ptr = (void **) 0);
   FgSeisPlot *find(FgSeisPlot *sp);
   FgSeisPlot *bottom(void **ptr = (void **) 0);
   FgSeisPlot *next(void **ptr = (void **) 0);
   FgSeisPlot *prev(void **ptr = (void **) 0);
   FgSeisPlot *current(void **ptr = (void **) 0);
};

#endif
