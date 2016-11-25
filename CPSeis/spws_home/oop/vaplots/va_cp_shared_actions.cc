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
// $Id: va_cp_shared_actions.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include "vaplots/va_cp_shared_actions.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_gvs_plot.hh"
#include "sl/sl_client_message.hh"





VaCpShareActions::VaCpShareActions(Widget          w,
                                   VaVectColors    *vect_colors, 
                                   VaCmpPlot       *cmp_plot,
                                   VaSemblancePlot *semb_plot,
                                   VaGvsPlot       *gvs_plot) :
                    GeneralObject("VaCpShareActions"), 
                    _vect_colors(vect_colors),
                    _cmp_plot(cmp_plot),
                    _w(w)
{
 addSeisPlot(cmp_plot->SP());
 addSeisPlot(semb_plot->SP());
 addSeisPlot(gvs_plot->SP());
}

VaCpShareActions::~VaCpShareActions()
{
}

void VaCpShareActions::showOverlays(Boolean show)
{
  _vect_colors->setEnableShowFuncs(show);
  doPostAction();
}

Boolean VaCpShareActions::applyNMOSensitive()
{
  return _cmp_plot->SP()->isPlotDisplayed();
}

Boolean VaCpShareActions::removeNMOSensitive()
{
  return _cmp_plot->nmcApplied();
}

void VaCpShareActions::applyNMO()
{
  _cmp_plot->applyForwardMoveout();
}

void VaCpShareActions::removeNMO()
{
  _cmp_plot->applyReverseMoveout();
}


void VaCpShareActions::newPlot(SeisPlot *)
{
  new SLClientMessage(_w, "cm", gotClient, this);
}
void VaCpShareActions::noPlotDisplayed(SeisPlot *)
{
  new SLClientMessage(_w, "cm", gotClient, this);
}
void VaCpShareActions::postMovie(SeisPlot *, SeisPlot::MovieDir )
{
  new SLClientMessage(_w, "cm", gotClient, this);
}
void VaCpShareActions::notCurrentInWindow(SeisPlot *sp)
{
  new SLClientMessage(_w, "cm", gotClient, this);
  addSeisPlot(sp->currentSPInWindow());
}

void VaCpShareActions::prePlot(SeisPlot *)
{
  doPreAction();
}

void VaCpShareActions::gotClient(void *data)
{
  VaCpShareActions *obj= (VaCpShareActions*)data;

    obj->doPostAction();

}
