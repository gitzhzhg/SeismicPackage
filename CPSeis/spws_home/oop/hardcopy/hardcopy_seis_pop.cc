#include <stdlib.h>
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
#include <Xm/Label.h>
#include "hardcopy/hardcopy_seis_pop.hh"
#include "hardcopy/hardcopy_plot.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_text_box.hh"

static String  defres[]= {
    "*tiL.labelString:  Traces / Inch:",
    "*isL.labelString:  Inches / Sec:",
    NULL };


enum {IS, TI};


HardCopySeisPop::HardCopySeisPop(Widget        p,
                                 char         *name,
                                 HelpCtx       hctx,
                                 SeisPlot     *sp,
				 SLpFileData  *slp_file_data,
                                 float        marker_scale_factor,
                                 Boolean      allow_scale_change) :
               HardCopyPop(p,name,hctx, sp, slp_file_data, HardCopyPop::PIP,
			   True, marker_scale_factor, allow_scale_change)
        
{
  static SLText dim_text[]  = {
   {"ti",   "range:0.0 *,default:20.00000", NULL, SLType_float, TI},
   {"is",   "range:0.0 *,default:2.00000",  NULL, SLType_float, IS},
  };
  dim_text[0].target=&_ti;
  dim_text[1].target=&_is;

  setDefaultResources( p, name, defres);
  _scale= new SLTextBox( this, "scale", hctx, dim_text, XtNumber(dim_text));
}

HardCopySeisPop::~HardCopySeisPop()
{
  delete _scale;
}



Widget HardCopySeisPop::make(Widget p) 
{
   if ( made() ) return topWidget();
   HardCopyPop::make(p);
   XtVaSetValues( _scale->W(), XmNleftAttachment, XmATTACH_WIDGET,
                               XmNleftOffset,     10,
                               XmNleftWidget,     _plot_dim->W(),
                               XmNtopAttachment,  XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,      _plot_dim->W(),
                               NULL );

   return topWidget();
}


void HardCopySeisPop::managing()
{
  HardCopyPop::managing();
  unitChange(NULL,0);
}


Boolean HardCopySeisPop::notifyComplex(SLDelay *obj, int ident)
{
  if (obj == _plot_dim) {
         if (ident == HEIGHT) {
             _scale->SetValue(IS, 
              _plot_dim->GetFloat(HEIGHT) / (_sp->tmax() - _sp->tmin())  );
         } 
         else if (ident == WIDTH) {
             _scale->SetValue(TI, 
                    _sp->plottedNplt() / _plot_dim->GetFloat(WIDTH) );
         }
  } // end if
  else if (obj == _scale) {
         if (ident == IS) {
               _plot_dim->SetValue(HEIGHT, 
                (_sp->tmax() - _sp->tmin()) * _scale->GetFloat(IS) );
         }
         else if (ident == TI) {
               _plot_dim->SetValue(WIDTH, 
                       _sp->plottedNplt() / _scale->GetFloat(TI) );
         }

  }  // end else if
  HardCopyPop::notifyComplex(obj,ident);

  return True;
}

void HardCopySeisPop::computeWidthHeight( float *width, float *height)
{
    if (_first_time_man) {
         _scale->SetValue(TI, _sp->ti() );
         _scale->SetValue(IS, _sp->is() );
    }
    notifyComplex(_scale, TI); 
    notifyComplex(_scale, IS); 
     
    *width=   _plot_dim->GetFloat(WIDTH);
    *height=  _plot_dim->GetFloat(HEIGHT);
}


void HardCopySeisPop::unitChange(SeisPlot*, int)
{
  if (made()) {
     if (_sp->units() == PlotMetric) {
        wprocShowMsg( _plot_dim->LabW(HardCopyPop::WIDTH), 
                     "Length Plot CMs" );
        wprocShowMsg( _plot_dim->LabW(HardCopyPop::HEIGHT), 
                     "Height Plot CMs" );
        wprocShowMsg( _scale->LabW(TI), "Traces / CM:" );
        wprocShowMsg( _scale->LabW(IS), "CM / Sec:" );
   
     }
     else {
        wprocShowMsg( _plot_dim->LabW(HardCopyPop::WIDTH), 
                     "Length Plot Inches" );
        wprocShowMsg( _plot_dim->LabW(HardCopyPop::HEIGHT), 
                     "Height Plot Inches" );
        wprocShowMsg( _scale->LabW(TI), "Traces / Inch:" );
        wprocShowMsg( _scale->LabW(IS), "Inches / Sec:" );
     }

     setTotalDim();//This will update the total size label widget
  }
}
