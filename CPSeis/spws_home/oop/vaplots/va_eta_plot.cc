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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================



//=============================================================================
//=          Eta plot class                                                   =
//=          Author Michael L. Sherrill 09/28/01                              =
//=          The processEta method is based on Stephen Chiu's work            =
//=          using the equation from Geophysics Vol 63 no 3 1998 by           =
//=          Vladimir Grechka and IIya Tsvankin of Colorado School of mines   =
//=                                                                           = 
//= For each input CMP, this class performs a semblance stacking velocity     =
//= analysis in a window centered at the time associated with the specified   =
//= semblance velocity pick. For each CMP a single trace is output whose      =
//= sample values are semblance values for different velocity scans.          =
//= These output traces are then rendered as a semblance image for the user   =
//= to pick the eta at a particular velocity location.                        =
//=============================================================================

// $Id: va_eta_plot.cc,v 1.3 2004/10/12 21:09:38 cornkc Exp $
// $Name: 12Oct04 $




//!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Some of this is temporary until the vel file format supports eta vals
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#include <math.h>
#include "vaplots/va_plot.hh"
#include "vaplots/va_eta_plot.hh"
#include "vaplots/va_eta_gui.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_common_params.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_eta_picks.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_horizons.hh"
#include "vf/vf_read_save.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_loc_out.hh"
#include "sl/sl_error_pop.hh"

#ifndef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif

enum{ACCEPT_AUTO = 11};

static String  defres[]= 
    {
    ".resizePolicy:              RESIZE_NONE",
    "*Active.background:         red",
    NULL
    };


VaEtaPlot::VaEtaPlot(            VfManager         *vf_manager, 
                                 VfHorizons        *horizons,
                                 Widget w,          char *name,
                                 HelpCtx            hctx,
                                 VaPlotCommonParams *common_params,
                                 VaVectColors       *vect_colors,
                                 int                numcolors,
                                 VaPlot             *semblance_plot,
                                 VaPlot             *cmp_plot)
                                 : VaPlot(vf_manager, w, name),
                                   SLFormHelpPop(w, name, 
                                                 FP_DOREMOVE | FP_DOHELP,
	                                         hctx, True, 2, False, 
                                                 True, numcolors, False),
                                 _gui_has_been_managed(False),
                                 _been_managed(False),
                                 _activated(False), 
                                 _semblance_plot(semblance_plot),
                                 _cmp_plot(cmp_plot),
                                 _amps_out(NULL), _horizons(horizons),
                                 _vect_colors(vect_colors),
                                 _velocity_pick(1000.0F),
                                 _amp_min(0.0F), _amp_max(1.0)
{

  _plot_type = ETA;

  setDefaultResources( w, name, defres);

  //Temp manager until va's main manager supports eta columns
  _temp_manager  = new VfManager(1,1, "", 1);

  addSeisPlot(_semblance_plot->SP());

  ((VaCmpPlot *)_cmp_plot)->setEtaManager(_temp_manager, this);

  addExtraButton("Accept Auto Pick", ACCEPT_AUTO);

  make(w);
}

VaEtaPlot::~VaEtaPlot()
{
  delete _gui;
  delSeisPlot(_esp);
  delete _esp;
  delete _temp_manager;
  if(_amps_out != NULL) free (_amps_out);
}


Widget VaEtaPlot::make(Widget p)
{
  if ( made() ) return topWidget();
  Widget parent = p ? p : wParent();

  SLFormHelpPop::make(parent);

  setTitle("Eta Plot");

  _esp = new SeisPlot(topWidget(), "Eta");

  _va_picks = new VaEtaPicks(_vf_manager, _horizons, this, _esp, _vect_colors,
                             _semblance_plot->SP());

  _esp->shareColorsWith(_semblance_plot->SP());
  _esp->shareContourColors(_semblance_plot->SP()->getContourColorPtr());

  addSeisPlot(_esp);

  _loc = new SeisLocOut(topWidget(),"eta_loc_out", _esp, getHelpCtx());

  _esp->initArrayTypePlot();
  _esp->cancelArrayTypeData();

  _gui = new VaEtaGui(topWidget(), "Eta_Gui", getHelpCtx(), this,
                      (VaCmpPlot *)_cmp_plot);

  return topWidget();
}


void VaEtaPlot::manage()
{
  int pixels_wide, pixels_high;
  int hpixels_per_inch, vpixels_per_inch;
  SeisPlot *cmpsp = _cmp_plot->SP();

  //The following attachements were put into this area because we get
  //a circular dependency error if it is destroyed and never managed
  //and the attachments have been registered.
  if(_been_managed == False)
    { 

      XtVaSetValues( topWidget(), XmNresizePolicy, XmRESIZE_NONE, NULL);

      //Set the window to be the approximate size of the plot
      hpixels_per_inch = _esp->getHorizontalPixelsPerInch(
                              XtDisplay(cmpsp->getWidget()), 
                              DefaultScreen(XtDisplay(cmpsp->getWidget())) );
      pixels_wide = (int)((_gui->getPlotWidth() * hpixels_per_inch) + 
                          _esp->leftBorder() + _esp->rightBorder());
      pixels_wide += 15;//add pixels for scroll bar size
  
      vpixels_per_inch = _esp->getVerticalPixelsPerInch(
                                XtDisplay(cmpsp->getWidget()), 
                                DefaultScreen(XtDisplay(cmpsp->getWidget())) );
      pixels_high = (int)((_gui->getPlotHeight() * vpixels_per_inch) +
                          _esp->topBorder() + _esp->bottomBorder());
      pixels_high += 120;//add pixels for the widget readout height

      pixels_wide = min(pixels_wide, 1200);
      pixels_wide = max(700, pixels_wide);//so all buttons will show on bottom
      pixels_high = min(pixels_high, 1000);

      XtVaSetValues(topWidget(),XmNwidth,pixels_wide,XmNheight,pixels_high,NULL);


      XtVaSetValues( _esp->W(), XmNleftAttachment,   XmATTACH_FORM,
                     XmNrightAttachment,  XmATTACH_FORM,
                     XmNtopAttachment,    XmATTACH_FORM,
                     XmNbottomAttachment, XmATTACH_WIDGET,
                     XmNbottomWidget,     bottomSeparator(), 
                     NULL);

      XtVaSetValues(_loc->W(), XmNrightAttachment,  XmATTACH_FORM,
                    XmNbottomAttachment, XmATTACH_FORM,
                    NULL);

      XtVaSetValues(buttonContainer(),
                    XmNmarginHeight,     5,
                    XmNleftAttachment,   XmATTACH_FORM,
                    XmNrightAttachment,  XmATTACH_WIDGET,
                    XmNrightWidget,      _loc->W(),
                    XmNrightOffset,      5,
                    NULL);


      _been_managed = True;

    }//end never been managed


  SLFormHelpPop::manage();

  XRaiseWindow(XtDisplay(topWidget()), XtWindow(XtParent(topWidget())));

  XSync(XtDisplay(topWidget()),False);

}

void VaEtaPlot::setActive(Boolean active)
{
  VaSemblancePlot *semb_plot = (VaSemblancePlot *)_semblance_plot;
  clearNmo();

  _activated = active;

  if(!_activated)
    {
      unmanage();
      vfDataset()->unlockData();
      semb_plot->hideEtaOverlays();
      _bottom_control->manageEta(False);
    }
  else
    {
      vfDataset()->lockData();
      semb_plot->showEtaOverlays();
    }
}


//=========================================================================
//================== New seis plot in window  =============================
//=========================================================================
void VaEtaPlot::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
  _esp = sp->currentSPInWindow();
  _gui->seisPlotChanged();
  //may need to notify picker via _va_picks here
}


//=========================================================================
//=== If the semblance plot has changed and we are managed, go away =======
//=========================================================================
void VaEtaPlot::newPlot(SeisPlot *sp)
{
  if(sp == _semblance_plot->SP() && managed())
    unmanage();
}


//===========================================================================
//============= Return ptr to the dialog                       ==============
//===========================================================================
SLShellContainer *VaEtaPlot::getDialog()
{
  return _gui;
}

void VaEtaPlot::manageGui()
{
  _gui->makeAndManage();
  _gui_has_been_managed = True;
}


void VaEtaPlot::updateGui(Boolean update_file_limits)
{
  _gui->updateParams(update_file_limits);
}

int VaEtaPlot::plot()
{
  int stat;
  VaEtaPicks *picks = (VaEtaPicks *)_va_picks;

  if(!_activated) return PlotImage::PlotSuccess;

  picks->cleanupPick(0,0,0);
 
  if(!needsReplotting()) return PlotImage::PlotSuccess;  

  setPlotParameters();


  stat = _esp->plot();
  if(stat == PlotImage::PlotSuccess)
    {
      _esp->backingStore(True);
      markNmo();
      manage();
    }
     
  return stat;

}


void VaEtaPlot::markNmo()
{
  Display *dpy  = XtDisplay(_esp->imageGraphic());
  Pixmap pixmap = _esp->imagePixmap(0);  
  Screen *scr   = XtScreen(_esp->imageGraphic());
  Window rootw  = RootWindowOfScreen(scr);
  GC gc         = XCreateGC( dpy, rootw, None, NULL);
  Pixel back_pix= _esp->getImageBackgroundPixel();
  Pixel fore_pix= _esp->getImageForegroundPixel();
  int x1, y1, x2, y2;


  _esp->setRedrawAction(SeisPlot::Redraw);

  XSetBackground(dpy, gc, fore_pix);
  XSetForeground(dpy, gc, back_pix);
  XSetLineAttributes( dpy, gc, 1, LineOnOffDash, CapButt, JoinMiter);

  x1 = _esp->leftBorder();
  x2 = x1 + _esp->imageWidth();
  y1 = y2 = _esp->plottedHeight() / 2 - 1;
  XDrawLine(dpy, pixmap, gc, x1, y1, x2, y2);
  _esp->redraw(x1, y1, x2 - x1, 1);


  y1 = _esp->topBorder();
  x1 = x2 = _esp->plottedWidth() / 2 - 1;
  y2 = y1 + _esp->imageHeight();
  XDrawLine(dpy, pixmap, gc, x1, y1, x2, y2);
  _esp->redraw(x1, y1, 1, y2 - y1);
  
}


//===========================================================================
//============= Get trace file information                     ==============
//===========================================================================
int VaEtaPlot::getFileParameters(char *filename)
{
  long stat = PlotImage::PlotSuccess;
  return stat;  
}



void VaEtaPlot::setPlotParameters()
{

  _gui->setPlotParameters();

}

//=========================================================================
//============= Change movie panel or read from file            ===========
//=========================================================================
void VaEtaPlot::changePanel(float x, float y, 
                            float /*time*/, float /*velocity*/)
{
  int stat = PlotImage::PlotSuccess;
  int frame;

  _esp->blankImage(x,y);
  _blank_image = True;


}


//=========================================================================
//============= Get xlocation of a gather in the file     =================
//=========================================================================
const float VaEtaPlot::getPanelXlocation(long i)
{
  return getXlocation(i);
}

//=========================================================================
//============= Get xlocation of a gather in the file     =================
//=========================================================================
const float VaEtaPlot::getPanelYlocation(long i)
{
  return getYlocation(i);
}

void VaEtaPlot::clearNmo()
{
  VaCmpPlot *cmp = (VaCmpPlot *)_cmp_plot;
  cmp->applyReverseMoveout();
}

void VaEtaPlot::removeNmo()
{
  VaCmpPlot *cmp = (VaCmpPlot *)_cmp_plot;
  cmp->externalModifyReverseMoveout();
}


void VaEtaPlot::applyNmo()
{
  VaCmpPlot *cmp = (VaCmpPlot *)_cmp_plot;
  cmp->externalModifyForwardMoveout();
}

//=========================================================================
//============= Receive pick from semblance plot          =================
//=========================================================================
void VaEtaPlot::receivePick(int action, float x, float y, float func_x, 
                            float func_y, long index)
{
  Boolean ok = False;
  int stat = 0;
  SLErrorPop *errpop;
  long nsamp, total_traces;
  long func, active_func;
  VfDataset *dataset= _temp_manager->activeDataset();//Temporary

  //Set the eta data set to the same headers as the main dataset
  dataset->setNhx(vfDataset()->getNhx());
  dataset->setNhy(vfDataset()->getNhy());


  if(!_activated) return;

  _index = index;
  _yWC   = y;

  func = dataset->findMatchingVelfun(func_x, func_y );
  active_func= dataset->getActiveVelocityFunction();
  if (func == -1)
    {
    long getfunc = dataset->findOrInsertVelfun( func_x, func_y );
    }
  else if (active_func != func)
    {
    dataset->setActiveVelocityFunction(func);
    }

  if(action == INSERT_PICK)
    {
      _pick_xval = x;
      _pick_yval = y;
      _velocity_pick = x;
      nsamp = (_gui->endingNmoVelocity() - _gui->startingNmoVelocity()) /
        _gui->nmoVelocityIncrement() + 1.5; 
      total_traces= (_gui->endingHorzVelocity() - 
        _gui->startingHorzVelocity()) /
        _gui->horzVelocityIncrement() + 1.5; 
      if(_amps_out != NULL) free(_amps_out);
      _amps_out  = (float *) calloc( 1,(int)(nsamp * total_traces *
                                             sizeof(float)));
      if(_amps_out == NULL)
        {
          errpop = new SLErrorPop(_top_widget, "Error",
                                  "Not enough memory for ETA data array");
          return;
        }
      stat = _esp->initArrayTypeData(1, 1, total_traces, nsamp, _amps_out);
      if(!stat)
        {
          errpop = new SLErrorPop(_top_widget, "Error",
                                  "Not enough memory for ETA plot");
          return;
        }
      manage();
      removeNmo();
      ok = processEta(x, y);
      if(ok)
        {
          stat = plot();
          if(stat != PlotImage::PlotSuccess)
            errpop = new SLErrorPop(_top_widget, "Error",
                                    "Error plotting the ETA plot");
        }      
      else
        {
          errpop = new SLErrorPop(_top_widget, "Error",
                                  "Error in running the ETA ");
        }
    }
  else
    {
    removeNmo();
    _delete_xval = x;
    _delete_yval = y;
    VaEtaPicks *picks = (VaEtaPicks *)_va_picks;
    SeisPlot *sp = _semblance_plot->SP();
    picks->deletePick(sp);
    }
}


//=============================================================================
//=== Perform the horizontal anisotrophy analyis                            ===
//=== This is from fortran code by Stephen Chiu.                            ===
//=== After this is working we should make a c primitive of it and put it   ===
//=== into cps                                                              ===
//=============================================================================
Boolean VaEtaPlot::processEta(float x, float y)
{
  SeisPlot *cmp_sp;
  Boolean ok = False;
  long ivn, ivh, it;
  float vi, v2, vh, vh2;
  long nvnmo, nhor;
  float x1, x2, xpwr;
  float *scr; 
  long nwin, i, j, k, ix;
  long ntr;
  float *hd_in, *hd_out;
  long trace_offset, header_offset;
  float *trace_in;
  int offset_hdr = 5;
  float tx, fac1, fac2;
  float dt, tzero, t0, xindex;
  float coef = 1.2, velocity;
  long indx;
  float trm, trp, s1;
  float d, temp;

  cmp_sp    = _cmp_plot->SP();

  hd_in     = cmp_sp->firstMemoryHeaderDataForUpdate();

  hd_out    = _esp->firstMemoryHeaderDataForUpdate();

  trace_in  = cmp_sp->firstMemoryFloatTraceDataForUpdate();

  trace_offset  = cmp_sp->currentFrame() * cmp_sp->originalTraces() * 
    cmp_sp->samplesPerTrace();
  header_offset = cmp_sp->currentFrame() * cmp_sp->originalTraces() * 
    cmp_sp->numHeaders();

  ntr   = cmp_sp->displayedTraces(cmp_sp->currentFrame());

  nvnmo = (_gui->endingNmoVelocity() - _gui->startingNmoVelocity()) /
    _gui->nmoVelocityIncrement() + 1.5; 

  nhor  = (_gui->endingHorzVelocity() - _gui->startingHorzVelocity()) /
    _gui->horzVelocityIncrement() + 1.5; 

  nwin  = _gui->windowLength() / cmp_sp->srval();

  dt    = cmp_sp->sampleRate();

  tzero = y - float(nwin/2.0F) * dt;

  scr = (float *)calloc(1, (int)(nwin * sizeof(float))); 
  if(scr == NULL) return ok;




  //Start of semblance calculation - velocity loop

  for(ivn = 0; ivn < nvnmo; ivn++)
    {
      vi= _gui->startingNmoVelocity() + (ivn *_gui->nmoVelocityIncrement());
      v2 = vi * vi; 

      for(ivh = 0; ivh < nhor; ivh++)
        {
          vh= _gui->startingHorzVelocity()+(ivh*_gui->horzVelocityIncrement());
          vh2 = vh * vh;

          xpwr = 0.0;
          for(i = 0; i < nwin; i++) scr[i] = 0.0;

          for(ix = 0; ix < ntr; ix++) // offset loop
            {
              x1= hd_in[ix * cmp_sp->numHeaders() + offset_hdr + header_offset];
              x2 = x1 * x1;
              fac1 = (vh2-v2) * x2 * x2 / v2;

              for(it = 0; it < nwin; it++) //  time loop  
                {
                  t0 = tzero + float(it) * dt; 
                  fac2 = (t0 * t0 * v2 * v2) + (coef * vh2 * x2);

                  tx = sqrt(t0*t0 + x2/v2 - fac1/fac2); 

                  //Compute delta-t, move sample from trace & accumulate
                  xindex = tx / dt; 
                  indx = (long)xindex;
                  indx = max(indx,0); 
                  indx = min(indx,cmp_sp->samplesPerTrace()-2);
                  d    = xindex - indx;
                  trm  = (1.0F - d) * 
                   trace_in[ix*cmp_sp->samplesPerTrace()+indx + trace_offset];
                  trp  = d * 
                   trace_in[ix*cmp_sp->samplesPerTrace()+indx+1 + trace_offset];
                  scr[it] = scr[it] + trm + trp;
                  xpwr = xpwr + trm*trm + trp*trp;
                }//end time loop
            }//end offset loop 

          //Compute semblance
          s1 = 0.0;
          for(k = 0; k < nwin; k++)
            s1 += scr[k] * scr[k];
          if (xpwr <= 0.0)
            {
              _amps_out[ivh * nvnmo + ivn] = 0.0;
            }
          else
            { 
              _amps_out[ivh * nvnmo + ivn] = s1/xpwr/float(ntr);
              _amps_out[ivh * nvnmo + ivn] *= _amps_out[ivh * nvnmo + ivn];
            }

        }//end ivh

    }//end ivn

  //Output
  //Normalize the output, may want to make this a user option later
  //Shift and scale output amplitudes
  _amp_max = 0.0;
  _amp_min = 1.e25F;
     
  for(i = 0; i < nhor; i++)
    {
      for(j = 0; j < nvnmo; j++)
        {
          _amp_min = min(_amps_out[i*nvnmo+j],_amp_min); 
          _amp_max = max(_amps_out[i*nvnmo+j],_amp_max);
        }
    }

  //Normalize, may need this as an option. Produces amp range 0.0-1.0
  _amp_max = _amp_max - _amp_min; 
  if (_amp_max > 0.0)
    {
      for(i = 0; i < nhor; i++)
        {
          for(j = 0; j < nvnmo; j++)
            {
              _amps_out[i*nvnmo+j] =
                    (_amps_out[i*nvnmo+j] -_amp_min) / _amp_max;
              //Mark the largest value
              if(_amps_out[i*nvnmo+j] == 1.0)
                {
                  _largest_vnmo = j * _gui->nmoVelocityIncrement() +
                                      _gui->startingNmoVelocity();
                  _largest_hnmo = i * _gui->horzVelocityIncrement() +
                                      _gui->startingHorzVelocity();
                }
            }
        }
      _amp_min = 0.0;
      _amp_max = 1.0;
    }
  //End normalize option
 

  //Set header info
  for(j = 0; j < nhor; j++)
    {
      hd_out[j * cmp_sp->numHeaders()] = j + 1;//Sequential trace num
      velocity = _gui->startingHorzVelocity() +
        j * _gui->horzVelocityIncrement();
      hd_out[5 + j * cmp_sp->numHeaders()] = velocity; //Velocity header
      hd_out[6] = 0.0F;//Y bin header
      hd_out[24 + j * cmp_sp->numHeaders()] = _amp_max; //Lav header
    }


  free(scr);
      
  return (ok = True);

}




void VaEtaPlot::extraButton(int ident)
{
  acceptAutoPick();
}


//==========================================================================
//==================== Temporary kludge ====================================
//==========================================================================
void VaEtaPlot::pickingCompleted(float *new_velocity)
{
  float eta;
  long ifun;
  VfDataset *dataset= _vf_manager->activeDataset();
  VfDataset *etaset = _temp_manager->activeDataset();
  long eta_ifun = etaset->getActiveVelocityFunction();
  VaEtaPicks *picks = (VaEtaPicks *)_va_picks;
 
  if(eta_ifun != -1)
    {
      long activepick = etaset->getActivePick(eta_ifun);
      eta = etaset->getOrdinate(eta_ifun, activepick, VTIN);
    }
  else
    {
      eta = 0.0;
    }

  dataset->unlockData();

  //Update the nmo velocity if a eta pick was made
  if(new_velocity != NULL)
    {
      ifun = dataset->getActiveVelocityFunction();
      dataset->replacePick(ifun, _index, _yWC, *new_velocity, VTNM);
      //Put the eta value in the mouse readout
      _esp->setLocationOutputType(PlotImage::MOUSE_AUX, "ETA:", 
                                 (double) eta);
    }

  dataset->lockData();

  _gui->updateFile();
}



void VaEtaPlot::acceptAutoPick()
{
  long ifun, index;
  int numPicks;
  VaEtaPicks *picks = (VaEtaPicks *)_va_picks;
  VaEtaPicker *picker = picks->getPicker();

  removeNmo();

  float eta = 0.5F * ((_largest_hnmo * _largest_hnmo) / 
               (_largest_vnmo * _largest_vnmo) - 1.0F);

  printf("Eta = %f, vhoriz = %f, vnmo = %f, time = %f\n\n",eta,
         _largest_hnmo, _largest_vnmo, _yWC);
  
  VfDataset *ds= _temp_manager->activeDataset();//Temporary

  ifun = ds->getActiveVelocityFunction();
  numPicks = (int) ds->numPicks(ifun); 


  picker->bracketTime( _yWC, VTIN, 0);

  index = picker->index();

  if (picker->inserting())
    {
      //For now if this is the 1st pick make sure we first insert
      //a pick at zero time with an eta of zero
      if(index == 0 && numPicks == 0)
        {
          ds->insertPick(ifun, index, 0.0F, 0.0F, VTIN);
          ++index;
        }
      ds->insertPick(ifun, index, _yWC, eta, VTIN);
    }
  else
    {
      ds->replacePick(ifun, index, _yWC, eta, VTIN);
    }

  //Temporarily we save the picks after every pick action
  pickingCompleted(&_largest_vnmo);
  
  applyNmo();
}
