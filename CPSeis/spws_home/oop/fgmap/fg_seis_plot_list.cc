#include "fgmap/fg_seis_plot_list.hh"
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
#include "fgmap/fg_seis_plot.hh"
#include "fgmap/fg_map_to_flag.hh"
#include "fgmap/fg_plot_color_pop.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "sp/seis_zoomop_pop.hh"
#include "vect/vector.hh"
#include "oprim/static_utils.hh"
#include "cprim.h"

#define LIST_UNDEFINED_PIXEL 3443UL


FgSeisPlotList::FgSeisPlotList(Widget         w, 
                               FieldGeometry *fg, 
                               HelpCtx        hctx,
                               FgControlPop  *dcp,
                               TransformPop  *tp,
                               ContainerList *all_pops) :
   _fg(fg), _hctx(hctx), _dcp(dcp), _tp(tp), _all_pops(all_pops),
   _status_showing(True),
   _active_flag(NULL), _selected_flag(NULL), _computed_flag(NULL),
   _default_flag(NULL), _active_line(NULL), _selected_line(NULL),
   _has_rec_line(NULL), _has_source_line(NULL), _has_both_line(NULL),
   _default_line(NULL), _use_active_flag(True), _use_selected_flag(True),
   _use_computed_flag(True), _use_active_line(True), _use_selected_line(True),
   _use_rec_line(True), _use_source_line(True), _use_both_line(True),
   _flagMode(ShowAll), _new_pixel(False), 
   _background_pixel(LIST_UNDEFINED_PIXEL), _bs_type(FOLLOW_SUGGESTION)
{
  Widget shell_child=  get_shell_child(get_toplevel_shell(w));
  _xlat= new FgMapToFlag(_fg, 30);
  _zoomop= new SeisZoomOpPop(shell_child, "zoomop", _hctx);
  _plot_colors= new FgPlotColorPop(shell_child, "plot_colors", hctx, this);

  if (!_active_flag)     _active_flag=      newstr("blue");
  if (!_selected_flag)   _selected_flag=    newstr("green");
  if (!_computed_flag)   _computed_flag=    newstr("yellow");
  if (!_default_flag)    _default_flag=     newstr("red");
  if (!_active_line)     _active_line=      newstr("blue");
  if (!_selected_line)   _selected_line=    newstr("green");
  if (!_has_both_line)   _has_both_line=    newstr("purple");
  if (!_has_rec_line)    _has_rec_line=     newstr("magenta");
  if (!_has_source_line) _has_source_line=  newstr("cyan");
  if (!_default_line)    _default_line=     newstr("red");

  struct COLOR_SCELL cell= { "light gray", "gray54", False};
  Colormap cmap;
  XtVaGetValues(w, XmNcolormap, &cmap, NULL);
  _background_pixel= alloc_scell(XtDisplay(w), cmap, &cell, True, NULL, NULL);

}

FgSeisPlotList::~FgSeisPlotList()
{
   if (_active_flag)     free(_active_flag);
   if (_selected_flag)   free(_selected_flag);
   if (_computed_flag)   free(_computed_flag);
   if (_default_flag)    free(_default_flag);
   if (_active_line)     free(_active_line);
   if (_selected_line)   free(_selected_line);
   if (_has_rec_line)    free(_has_rec_line);
   if (_has_source_line) free(_has_source_line);
   if (_has_both_line)   free(_has_both_line);
   if (_default_line)    free(_default_line);


}

SeisZoomOpPop   *FgSeisPlotList::zoomOpPop()       {return _zoomop;}
FieldGeometry   *FgSeisPlotList::fieldGeometry()   {return _fg;}
HelpCtx          FgSeisPlotList::helpCtx()         {return _hctx;}
FgMapToFlag     *FgSeisPlotList::translator()      {return _xlat;}
FgControlPop    *FgSeisPlotList::getDCP()          {return _dcp;}
TransformPop    *FgSeisPlotList::getTP()           {return _tp;}
ContainerList   *FgSeisPlotList::getAllPops()      {return _all_pops;}
FgPlotColorPop  *FgSeisPlotList::getPlotColorPop() {return _plot_colors;}


Pixel FgSeisPlotList::plotBgPixel()        {return _background_pixel;}
char *FgSeisPlotList::activeFlagColor()    {return _active_flag;}
char *FgSeisPlotList::selectedFlagColor()  {return _selected_flag;}
char *FgSeisPlotList::computedFlagColor()  {return _computed_flag;}
char *FgSeisPlotList::defaultFlagColor()   {return _default_flag;}
char *FgSeisPlotList::activeLineColor()    {return _active_line;}
char *FgSeisPlotList::selectedLineColor()  {return _selected_line;}
char *FgSeisPlotList::receiverLineColor()  {return _has_rec_line;}
char *FgSeisPlotList::sourceLineColor()    {return _has_source_line;}
char *FgSeisPlotList::bothLineColor()      {return _has_both_line;}
char *FgSeisPlotList::defaultLineColor()   {return _default_line;}

Boolean FgSeisPlotList::useActiveFlag()    {return _use_active_flag;}
Boolean FgSeisPlotList::useSelectedFlag()  {return _use_selected_flag;}
Boolean FgSeisPlotList::useComputedFlag()  {return _use_computed_flag;}
Boolean FgSeisPlotList::useActiveLine()    {return _use_active_line;}
Boolean FgSeisPlotList::useSelectedLine()  {return _use_selected_line;}
Boolean FgSeisPlotList::useReceiverLine()  {return _use_rec_line;}
Boolean FgSeisPlotList::useSourceLine()    {return _use_source_line;}
Boolean FgSeisPlotList::useBothLine()      {return _use_both_line;}

FlagMode FgSeisPlotList::flagMode()        { return _flagMode; }

void FgSeisPlotList::setPlotBgPixel(Pixel background_pixel)
{
  if (_background_pixel != background_pixel) {
          _background_pixel= background_pixel;
          _new_pixel= True;
  }
}

void FgSeisPlotList::setActiveFlagColor(char *active_flag, Boolean use_it)
{
  if (_active_flag)     free(_active_flag);
  _active_flag= newstr(active_flag);
  _use_active_flag= use_it;
}
void FgSeisPlotList::setSelectedFlagColor(char *selected_flag, Boolean use_it)
{
  if (_selected_flag)   free(_selected_flag);
  _selected_flag= newstr(selected_flag);
  _use_selected_flag= use_it;
}
void FgSeisPlotList::setComputedFlagColor(char *computed_flag, Boolean use_it)
{
  if (_computed_flag)   free(_computed_flag);
  _computed_flag= newstr(computed_flag);
  _use_computed_flag= use_it;
}
void FgSeisPlotList::setDefaultFlagColor(char *default_flag)
{
  if (_default_flag)    free(_default_flag);
  _default_flag= newstr(default_flag);
}
void FgSeisPlotList::setActiveLineColor(char *active_line, Boolean use_it)
{
  if (_active_line)     free(_active_line);
  _active_line= newstr(active_line);
  _use_active_line= use_it;
}
void FgSeisPlotList::setSelectedLineColor(char *selected_line, Boolean use_it)
{
  if (_selected_line)     free(_selected_line);
  _selected_line= newstr(selected_line);
  _use_selected_line= use_it;
}
void FgSeisPlotList::setReceiverLineColor(char *has_rec_line, Boolean use_it)
{
  if (_has_rec_line)    free(_has_rec_line);
  _has_rec_line= newstr(has_rec_line);
  _use_rec_line= use_it;
}
void FgSeisPlotList::setSourceLineColor(char *has_source_line, Boolean use_it)
{
  if (_has_source_line) free(_has_source_line);
  _has_source_line= newstr(has_source_line);
  _use_source_line= use_it;
}
void FgSeisPlotList::setBothLineColor(char *has_both_line, Boolean use_it)
{
  if (_has_both_line)   free(_has_both_line);
  _has_both_line= newstr(has_both_line);
  _use_both_line= use_it;
}
void FgSeisPlotList::setDefaultLineColor(char *default_line )
{
  if (_default_line)    free(_default_line);
  _default_line= newstr(default_line);
}
void FgSeisPlotList::setFlagMode(FlagMode flagMode)
{
  _flagMode = flagMode;
}


void FgSeisPlotList::updateColorsOnPlots()
{
  FgSeisPlot *q;
  void       *x;
  FgXpPlotLinkedList *xplist;

  if (_new_pixel) SU::holdVectors();

  for(q= top(&x); (q); q= next(&x) ){
       xplist= q->getXpList();
       if (xplist) {
         // set the colors on the cross plot list
              xplist->setLineColors( _default_line, _has_source_line,
                                     _has_rec_line, _has_both_line, 
                                     _selected_line, _active_line);
              xplist->setFlagColors( _default_flag, _computed_flag, 
                                     _selected_flag, _active_flag);
              xplist->setLinePrecedence(_use_active_line, _use_selected_line,
                                        _use_both_line  , _use_rec_line,
                                        _use_source_line);
              xplist->setFlagPrecedence(_use_active_flag, _use_selected_flag,
                                        _use_computed_flag);
              xplist->setFlagMode(_flagMode);
 
       }
       if (_new_pixel) {
              if (q->plotType() || q->plottedPlotType())
                   q->setGridColor(_background_pixel);
              if ( (XtWindow(q->W())) ) {
                   if (q->plottedPlotType() == PlotImage::PlotGRID)
                       q->plot();
              } // end if
       } // end if
  } // end loop
  if (_new_pixel) SU::flushVectors();

  _new_pixel= False;

}

void FgSeisPlotList::showMessageArea()
{
   FgSeisPlot *q;
   void *x;
   for(q= top(&x); (q); q= next(&x)) q->showMessageArea();
   _status_showing= True;
}
void FgSeisPlotList::hideMessageArea()
{
   FgSeisPlot *q;
   void *x;
   for(q= top(&x); (q); q= next(&x)) q->hideMessageArea();
   _status_showing= False;
}

void FgSeisPlotList::setBackingStoreMode(BackingStoreType bs_type)
{
 _bs_type= bs_type;
}

FgSeisPlotList::BackingStoreType FgSeisPlotList::backingStoreMode()
{
 return _bs_type;
}

void FgSeisPlotList::add(FgSeisPlot *sp)
{ 
   FgSPElement *theElement = new FgSPElement(sp);
   BaseLinkedList::add((Element *) theElement);
   _zoomop->addControl(sp);
   if (_status_showing) sp->showMessageArea();
   else                 sp->hideMessageArea();
   if (_background_pixel == LIST_UNDEFINED_PIXEL)
           _background_pixel= sp->getGridColor();
}

void FgSeisPlotList::remove(FgSeisPlot *sp)
{ 
   BaseLinkedList::remove((void*)sp);
   _zoomop->removeControl(sp);
}

FgSeisPlot *FgSeisPlotList::top(void **ptr)
{
   FgSPElement* q= (FgSPElement*)BaseLinkedList::top(ptr);
   return (q ? q->_sp : NULL);
}

FgSeisPlot *FgSeisPlotList::find(FgSeisPlot *sp)
{
  FgSPElement* q= (FgSPElement*)BaseLinkedList::find((void*)sp);
  return (q ? q->_sp : NULL);
}

FgSeisPlot *FgSeisPlotList::bottom(void **ptr)
{
   FgSPElement* q= (FgSPElement*)BaseLinkedList::bottom(ptr);
   return (q ? q->_sp : NULL);
}

FgSeisPlot *FgSeisPlotList::next(void **ptr)
{
   FgSPElement* q= (FgSPElement*)BaseLinkedList::next(ptr);
   return (q ? q->_sp : NULL);
}

FgSeisPlot *FgSeisPlotList::prev(void **ptr)
{
   FgSPElement* q= (FgSPElement*)BaseLinkedList::prev(ptr);
   return (q ? q->_sp : NULL);
}

FgSeisPlot *FgSeisPlotList::current(void **ptr)
{
   FgSPElement* q= (FgSPElement*)BaseLinkedList::current(ptr);
   return (q ? q->_sp : NULL);
}
