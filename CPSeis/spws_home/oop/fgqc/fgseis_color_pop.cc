//author: Michael L. Sherrill 09/95
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
//Creates color processing menu for use with fgqc plots

#include "fgqc/fgseis_color_pop.hh"
#include "dp/hill_shader_pop.hh"
#include "dp/distr_slicer_pop.hh"
#include "dp/color_lut.hh"
#include "fgqc/fgqc_plot_type.hh"
#include "sl/sl_arrow_label.hh"
#include <Xm/Label.h>

#define BaseParentClass SLFPopSep
#define ParentClass SeisColorPop

static String defres[]= {
    "*smoothdata.labelString:         Smooth Data",
    "*insertdata.labelString:         Insert Data",
    "*texturebox_Frame.topPosition:   5",
    "*texturebox_Frame.leftPosition:  20",
    "*pointsize.labelString:          Point Size",
    NULL};

FgSeisColorPop::FgSeisColorPop( Widget            p,
                                char              *name,
                                SeisPlot          *sp,
                                HelpCtx           hctx)
       :SeisColorPop(p,name,sp,hctx),
        _texture_state  (SMOOTH_DATA),
        _point_size     (1),
        _plot_class     (0)

{
  _hsp = 0;
  _dsp = 0;
  _clt = 0;

  static SLRadio ttypes[] =
    {
      { "smoothdata", SMOOTH_DATA },        
      { "insertdata", INSERT_DATA },
    };

    setDefaultResources (p, name, defres);

    _texturebox = new SLRadioBox (this, "texturebox", getHelpCtx(), ttypes,
                                  XtNumber(ttypes),
                                  NULL, True, False);
    _texturebox->SetRadio (SMOOTH_DATA);

    _point_sizer = new SLArrowTitle (this, "pointsize", getHelpCtx(),
       &_point_size);
}


FgSeisColorPop::~FgSeisColorPop()
{
}

void FgSeisColorPop::DoAction()
{
 SeisPlot *lsp;
 BaseParentClass::DoAction();

 Boolean regather_data = False;
 _texture_state = (int)_texturebox->WhichSelected();
 _point_size    = _point_sizer->arrowLabel()->getValue ();
 if (_plot_class) {
   if (_texture_state != _plot_class->Texture() ||
       _point_size    != _plot_class->Pointsize() ) {
     regather_data = True;
   }
 }

 for(lsp = _list.top(); lsp; lsp = _list.next() ) { //loop thru all seis plots

  switch( _ctypebox->WhichSelected() )
    {
    case RAMTYPE:
                   lsp->setDoMedian(True);
                   lsp->setDoColor(True);
                   lsp->setDoPercent(False);
                   lsp->setDoAmplitude(False);
                   break;
    case GRAYTYPE:
                   lsp->setDoMedian(False);
                   lsp->setDoColor(True);
                   break;
    case COLORTYPE:
                   lsp->setDoMedian(False);
                   lsp->setDoColor(True);
                   break;
    }


  switch( _amptypebox->WhichSelected() )
    {
    case BARVALS:  
                   if(_cbar->minRGBAmp() == 0.0 && _cbar->maxRGBAmp() == 0.0)
		      {
                      _amptypebox->SetRadio(PCNTVALS);
                      lsp->setDoPercent(True);
                      lsp->setDoAmplitude(False); 
                      break;
		      }
                   lsp->setDoPercent(False);
                   lsp->setDoAmplitude(False);
                   _minamp = _cbar->minRGBAmp();
                   _maxamp = _cbar->maxRGBAmp();
                   lsp->setMinColorAmp(_minamp);
                   lsp->setMaxColorAmp(_maxamp); 
                   break;
    case AMPVALS:
                   lsp->setDoPercent(False);
                   lsp->setDoAmplitude(True);                              
                   lsp->setMinColorAmp(_minamp);
                   lsp->setMaxColorAmp(_maxamp); 
                   break;
    case PCNTVALS:
                   lsp->setDoPercent(True);
                   lsp->setDoAmplitude(False);
                   lsp->setMinColorAmp(_minamp);
                   lsp->setMaxColorAmp(_maxamp);  
                   break;
    }


   lsp->setGradeVert((Boolean)_grad_vert);
   lsp->setGradeHorz((Boolean)_grad_horz);
   lsp->setPNC( (int)(_pncscale->GetScaleValue() *.10));
   lsp->setPPC( (int)(_ppcscale->GetScaleValue() *.10));
   if(_hsp) {
     lsp->setMinColorAmp(_hsp->getMinimumCode());
     lsp->setMaxColorAmp(_hsp->getMaximumCode());
   }
   else if (_dsp) {
     lsp->setMinColorAmp(_dsp->getMinimumCode());
     lsp->setMaxColorAmp(_dsp->getMaximumCode());
   }
   else if (_clt) {
     lsp->setMinColorAmp(_clt->getMinimumCode());
     lsp->setMaxColorAmp(_clt->getMaximumCode());
   }
   else if (_been_managed == False && _sp->imageIsDisplayed() == False){
     lsp->setMinColorAmp(_minamp);
     lsp->setMaxColorAmp(_maxamp);
   }

 } //end loop

  if(_sp->imageIsDisplayed() == True)
    _dont_plot_yet = False;
  else
    _dont_plot_yet = True;

  if(!_first_time) 
    {
    if(_hsp == 0 && _dsp == 0 && _clt == 0) _cbar->loadToSeisPlot();
    if(_dont_plot_yet && whichButton() == FP_OK) 
       {
       _dont_plot_yet = False;
       return;
       }
    if(_dont_plot_yet && whichButton() == FP_APPLY) 
       {
       _dont_plot_yet = False;
       return;
       }
    if(_hsp)
      {
      _hsp->notifyColor(_cbar, False);
      }
    else if(_dsp)
      {
      _dsp->notifyColor(_cbar, False);
      }
    else if(_clt)
      {
      _clt->notify(_cbar, False);
      }
    else
      {
      if(_been_managed)
        {
	if(regather_data)
          {
          _plot_class->setTexture(_texture_state);
          _plot_class->setPointsize(_point_size);
          _plot_class->plot();
          }
        else
          {
          for(lsp = _list.top(); lsp; lsp = _list.next() )
            if( lsp->isPlotDisplayed() &&
                lsp->plotType() >= PlotImage::PlotCOLOR )
                  lsp->plot();
	  }
        }
      }
    }
  else
    {
    _first_time = False;
    if(_dsp)
      {
      _dsp->notifyColor(_cbar, False);
      }
    else if(_clt)
      {
      _clt->notify(_cbar, False);
      }
    }


}

void FgSeisColorPop::manage()
{


  if (_been_managed == False) {
    XtVaSetValues (_texturebox->W(),
                              XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                              XmNtopWidget,        _ctypebox->W(),
                              XmNleftAttachment,   XmATTACH_WIDGET,
                              XmNleftWidget,       _ctypebox->W(),
                              XmNleftOffset,       5,
                              NULL);

    XtVaSetValues (_point_sizer->W(),
                              XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                              XmNtopWidget,        _texturebox->W(),
                              XmNleftAttachment,   XmATTACH_WIDGET,
                              XmNleftWidget,       _texturebox->W(),
                              XmNleftOffset,       5,
                              NULL);

    ParentClass::manage();
  }

 /*
  * the first time through we get these fields values from the 
  * the app defaults file.
  * Later we set it based on previous values
  */
 if (_new_appdefaults == False)
   {
   _amp1box->SetValue( MINAMP, _sp->minColorAmp() );
   _amp2box->SetValue( MAXAMP, _sp->maxColorAmp() );
   _gradebox->SetTog( GRAD_VERT, (Boolean)_sp->gradeVert() );
   _gradebox->SetTog( GRAD_HORZ, (Boolean)_sp->gradeHorz() );
   }

 if(_been_managed == False && _sp->imageIsDisplayed())
   {
   _amp1box->SetValue( MINAMP, _sp->minColorAmp() );
   _amp2box->SetValue( MAXAMP, _sp->maxColorAmp() );
   }

 if(_been_managed == False && _do_amps == True)
   {
   _amp1box->SetValue( MINAMP, _minamp );
   _amp2box->SetValue( MAXAMP, _maxamp );
   _amptypebox->SetRadio(AMPVALS);
   }

 if(_been_managed == False && _grading == False)
   {
   _gradebox->SetTog( GRAD_VERT, False );
   _gradebox->SetTog( GRAD_HORZ, False );
   }

 if(_cbar->minRGBAmp() == 0.0 && _cbar->maxRGBAmp() == 0.0)
     {
     if(_amptypebox->WhichSelected() == BARVALS)_amptypebox->SetRadio(PCNTVALS);
     XtSetSensitive( _amptypebox->GetRadioWidget(BARVALS), False);
     }


   if(_hsp || _dsp || _clt)
     disableOptions(); //disable some color options when RGBZ decoding
   else /* if(!_hsp && !_dsp && !_clt) */
     enableOptions();  // enable all color options when not RGBZ decoding


//For the time being disable grading options
  _gradebox->SetTog(GRAD_VERT, False);
  _gradebox->SetTog(GRAD_HORZ, False);
  XtSetSensitive( _gradebox->TogW(GRAD_VERT), False);
  XtSetSensitive( _gradebox->TogW(GRAD_HORZ), False);



  XtManageChild(topWidget()); 
 
  _been_managed = True;
}

//Do sensitive and hill shader specific code here
void FgSeisColorPop::disableOptions()
{

  XtSetSensitive( _gradebox->TogW(GRAD_VERT), False);
  XtSetSensitive( _gradebox->TogW(GRAD_HORZ), False);
  XtSetSensitive( _ctypebox->GetRadioWidget(RAMTYPE), False);
  XtSetSensitive( _amptypebox->GetRadioWidget(AMPVALS), False);
  XtSetSensitive( _amptypebox->GetRadioWidget(PCNTVALS), False);
  XtSetSensitive( _amp1box->TxtW(MINAMP), False);
  XtSetSensitive( _amp2box->TxtW(MAXAMP), False);
  XtSetSensitive( _pncscale->W(), False);
  XtSetSensitive( _ppcscale->W(), False);
  XtSetSensitive( _texturebox->GetRadioWidget(SMOOTH_DATA), False);
  XtSetSensitive( _texturebox->GetRadioWidget(INSERT_DATA), False);
  XtSetSensitive( _point_sizer->arrowLabel()->W(), False);

}

//Enable specific options  here
void FgSeisColorPop::enableOptions()
{

  XtSetSensitive( _ctypebox->GetRadioWidget(RAMTYPE), True);
  XtSetSensitive( _amptypebox->GetRadioWidget(AMPVALS), True);
  XtSetSensitive( _amptypebox->GetRadioWidget(PCNTVALS), True);
  XtSetSensitive( _amp1box->TxtW(MINAMP), True);
  XtSetSensitive( _amp2box->TxtW(MAXAMP), True);
  XtSetSensitive( _pncscale->W(), True);
  XtSetSensitive( _ppcscale->W(), True);
  XtSetSensitive( _texturebox->GetRadioWidget(SMOOTH_DATA), True);
  XtSetSensitive( _texturebox->GetRadioWidget(INSERT_DATA), True);
  XtSetSensitive( _point_sizer->arrowLabel()->W(), True);

}

void FgSeisColorPop::reloadDefaults (Boolean do_method)
{
  if (made()) {
    _texturebox->reloadDefaults();
    SeisColorPop::reloadSystemDefaults (do_method);
  }
}

void FgSeisColorPop::reloadSystemDefaults (Boolean do_method)
{
  _texturebox->SetRadio (SMOOTH_DATA);
  SeisColorPop::reloadSystemDefaults (do_method);
}

int FgSeisColorPop::Texture ()
{
  return _texture_state;
}

int FgSeisColorPop::Pointsize ()
{
  return _point_size;
}

void FgSeisColorPop::setPlotClass (FgQcPlotType *plot_class)
{
  _plot_class = plot_class;
}
