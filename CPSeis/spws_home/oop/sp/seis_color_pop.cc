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
//author: Michael L. Sherrill 11/93
//Creates color processing menu
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include "sp/seis_color_pop.hh"
#include "sp/seis_ctype.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_winman.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/paintset_collection.hh"
#include "sl/colorset_collection.hh"

#define    AMPMIN_DEF    0.0F
#define    AMPMAX_DEF    1.0F
#define    GRAD_VERT_DEF True
#define    GRAD_HORZ_DEF True
#define    HI_RES_DEF    False
#define    COLOR_DEF     True
#define    PCNT_DEF      True


static String  defres[]= {
    "*scp_popup.title:               Color Processing Menu",
//    ".height:                        600",
//    ".width:                         745",
    "*processlab.labelString:        Amplitude Processing Parameters",
    "*selectlab.labelString:         Color Selection Parameters",
    "*min_lab.labelString:           MIN",
    "*max_lab.labelString:           MAX",
    "*graytype.labelString:          Gray Scale",
    "*colortype.labelString:         Color",
    "*ramtype.labelString:           Median Ram",
    "*colortype.set:                 True",
    "*gradvert.labelString:          Grade Vertical",
    "*gradhorz.labelString:          Grade Horizontal",
    "*hires.labelString:             Hi Res (see help)",
    "*gradvert.set:                  True",
    "*gradhorz.set:                  True",
    "*hires.set:                     False",
    "*barvals.labelString:           Bar Values",
    "*ampvals.labelString:           Amplitudes->",
    "*pcntvals.labelString:          Percentiles->",
    "*pnc_lab.labelString:           PNC",
    "*pncscale.minimum:              0",
    "*pncscale.maximum:              1000",
    "*pncscale.decimalPoints:        1",
    "*ppc_lab.labelString:           PPC",
    "*ppcscale.minimum:              0",
    "*ppcscale.maximum:              1000",
    "*ppcscale.decimalPoints:        1",
    "*sep_1.topPosition:             25",
    "*sep_2.topPosition:             50",
    "*sep_1.leftOffset:              10",
    "*sep_2.leftOffset:              10",
    "*sep_1.rightOffset:             10",
    "*sep_2.rightOffset:             10",
    "*sep_1.bottomOffset:            10",
    "*sep_2.bottomOffset:            10",
    "*ctypebox_Frame.topPosition:    5",
    "*ctypebox_Frame.leftPosition:   3",
    "*gradebox_Frame.topPosition:    5",
    "*gradebox_Frame.leftPosition:   50",
    "*processlab.fontList:           -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*processlab.topPosition:        55",
    "*processlab.leftOffset:         10",
    "*processlab.rightOffset:        5",
    "*selectlab.fontList:            -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*selectlab.topPosition:         25",
    "*selectlab.leftOffset:          10",
    "*selectlab.rightOffset:         5",
    "*Usedef.set:                    True",
    "*amptypebox_Frame.topPosition:  61",
    "*amptypebox_Frame.leftPosition: 3",
    "*pcntvals.set:                  True",
    "*amp1box.topPosition:           65",
    "*amp1box.leftPosition:          30",
    "*amp2box.topPosition:           65",
    "*amp2box.leftPosition:          55",
    "*min_lab.topPosition:           66",
    "*max_lab.topPosition:           66",
    "*max_lab.leftOffset:            10",
    "*pnc_lab.topPosition:           70",
    "*ppc_lab.topPosition:           70",
    "*pncscale.topPosition:          71",
    "*pncscale.leftPosition:         30",
    "*ppcscale.topPosition:          71",
    "*ppcscale.leftPosition:         55",
    "*pncscale..value:               1000",
    "*ppcscale..value:               1000",
    "*minamp.value:                  0.0",
    "*maxamp.value:                  1.0",
    "*centerpercent.labelString:     Center amplitudes (see help)",
    "*centerpercent.set:             False",
    NULL};




#define ParentClass SLFPopSep


SeisColorPop::SeisColorPop( Widget            p,
                            char              *name,
                            SeisPlot          *sp,
                            HelpCtx           hctx,
                            Boolean           allow_data_processing,
                            Boolean           plot_on_doaction) 

       : SLFPopSep(p,name,FP_DOALL,hctx,False,False),
         SeisInform(sp),
                            _plot_on_doaction(plot_on_doaction),
                            _new_file(True),
                            _use_file_defaults(False), _new_appdefaults(True),
                            _sp(sp), _first_time(True), 
                            _color_num(1),
                            _allow_data_processing(allow_data_processing)

{

   static SLText texts[]  = 
     {
     {"minamp", NULL, NULL, SLType_float, MINAMP },
     {"maxamp", NULL, NULL, SLType_float, MAXAMP },
     };
   _minamp = 0.0;
   _maxamp = 1.0;
   _minstart = _minamp;
   _maxstart = _maxamp;
   texts[0].target= &_minamp;
   texts[1].target= &_maxamp;
     
   static SLTog togs[]  = 
     {
       { "gradvert", NULL, GRAD_VERT },
       { "gradhorz", NULL, GRAD_HORZ },
       { "hires",    NULL, HI_RES },
     };
   togs[0].target= &_grad_vert;
   togs[1].target= &_grad_horz;
   togs[2].target= &_hi_res;

   static SLRadio ctypes[]  = 
     {
       { "graytype",     GRAYTYPE },
       { "colortype",    COLORTYPE },
       { "ramtype",      RAMTYPE },
     };

   static SLRadio amptypes[]  = 
     {
       { "barvals",     BARVALS },
       { "ampvals",     AMPVALS },
       { "pcntvals",    PCNTVALS },
     };

   static SLTog center_tog[]  = 
     {
       { "centerpercent", NULL, CENTER_PERCENT },
     };
   center_tog[0].target= &_center_percent;


   setDefaultResources( p, name, defres);

   _change_button = False;
   
   _ctypebox= new SLRadioBox(this, "ctypebox",getHelpCtx(),ctypes,
                             XtNumber(ctypes),
                            NULL, True, False );

   _gradebox= new SLTogBox( this,"gradebox", getHelpCtx(), togs, XtNumber(togs),
                            True, False );

   _amptypebox= new SLRadioBox(this,"amptypebox",getHelpCtx(),amptypes,
                               XtNumber(amptypes), NULL, True, False );
   _amptypebox->setAltChoiceAction( (SLRadioButtonfunc)barAmpAction, this );

   _amp1box= new SLTextBox( this, "amp1box", getHelpCtx(),
                            &texts[MINAMP], 1, False, 1, False, False );

   _amp2box= new SLTextBox( this, "amp2box", getHelpCtx(),
                            &texts[MAXAMP], 1, False, 1, False, False );

   _pncscale= new PercentScale( this, "pncscale", getHelpCtx(), NULL, this );

   _ppcscale= new PercentScale( this, "ppcscale", getHelpCtx(), NULL, this );

   _centerbox= new SLTogBox(this,"centerbox", getHelpCtx(), center_tog,
                            XtNumber(center_tog),
                            True, False );


   _ctypebox->setAltChoiceAction( (SLRadioButtonfunc)ChoiceAction, this );

   _amp1box->setAltLosingAction( (SLTextfunc)AmpLosingFocusAction, this );

   _amp2box->setAltFocusAction ( (SLTextfunc)AmpFocusAction, this );

   _dont_plot_yet = False;

   _last_button = FP_OK;

   addSP(_sp);

   _cbar = NULL;
   DoAction();     

   _been_managed = False;
   _do_amps = False;
   _grading= True;

   _seiscolortype = NULL;
}


SeisColorPop::~SeisColorPop()
{
  if(_cbar) delete _cbar;
  if(_seiscolortype) delete _seiscolortype;
}


Widget SeisColorPop::make(Widget p)
{

   if ( made() ) return topWidget();
   p= p ? p : wParent();
   ShellStatMsg  bld_info(p,"Building Color Popup...");
   SLFPopSep::make(p);

///////////////// new ////////////////////////
   // put the colormap associated with the parent on the top widget since
   //   the top widget will be the ancester to _cbar and _seiscolortype
   Colormap colormap, old_colormap;
   XtVaGetValues (wParent(), XmNcolormap, &colormap, NULL);
   XtVaGetValues (topWidget(), XmNcolormap, &old_colormap, NULL);
   XtVaSetValues (topWidget(), XmNcolormap, colormap, NULL);
///////////////// new ////////////////////////

   _cbar = new SeisCbar(topWidget(), "cbar", _sp, True, False, True);

   _seiscolortype = new SeisCtypePop(topWidget(), "seiscolortype", NULL,
                                     _sp,_cbar,True,this);
///////////////// new ////////////////////////
   // put the old colormap associated with the top widget back
   XtVaSetValues (topWidget(), XmNcolormap, old_colormap, NULL);
///////////////// new ////////////////////////

   if(!_allow_data_processing)_amptypebox->SetRadio(AMPVALS);

   setTitle("Color Options");

   DoAction();

   return topWidget();

}


Boolean SeisColorPop::ValidInput()
{
 Boolean stat;


 if (made()) 
   {
   stat= ParentClass::ValidInput();

   if ( (_minamp >= _maxamp) && (stat) ) 
      {
      _amp1box->popError( "max amplitude must be greater than min");
      stat= False;
      } 
   }
 else
   { 
   stat= True;
   }


 return (stat); 
}


void SeisColorPop::DoAction()
{ // first
 SeisPlot *lsp;
 ParentClass::DoAction();

 int read_only_color = ColorsetCollection::readOnly (_sp->W());
 int do_action;

 for (lsp = _list.top(); lsp; lsp = _list.next() ) { //loop all seis plots

   do_action = !ColorsetCollection::recolorCurrentOnly() ||
               lsp->isCurrentInWindow ();
   // Kirk Wallace requested that the changes only apply to the current
   //   SeisPlot in the 24-bit color model for cbyt only

   if (do_action) {
     switch (_ctypebox->WhichSelected()) {
     case RAMTYPE:
       lsp->setDoMedian    (True);
       lsp->setDoColor     (True);
       lsp->setDoPercent   (False);
       lsp->setDoAmplitude (False);
       break;
     case GRAYTYPE:
       lsp->setDoMedian (False);
       lsp->setDoColor  (True);
       break;
     case COLORTYPE:
       lsp->setDoMedian (False);
       lsp->setDoColor  (True);
       break;
     }


     switch (_amptypebox->WhichSelected()) {
     case BARVALS:  
       if (_cbar->minRGBAmp() == 0.0 && _cbar->maxRGBAmp() == 0.0) {
	 _amptypebox->SetRadio (PCNTVALS);
	 lsp->setDoPercent     (True);
	 lsp->setDoAmplitude   (False); 
	 break;
       }
       lsp->setDoPercent (False);
       lsp->setDoAmplitude (False);
       _minamp = _cbar->minRGBAmp ();
       _maxamp = _cbar->maxRGBAmp ();
       break;
     case AMPVALS:
       lsp->setDoPercent   (False);
       lsp->setDoAmplitude (True);                              
       break;
     case PCNTVALS:
       lsp->setDoPercent   (True);
       lsp->setDoAmplitude (False); 
       break;
     }


     lsp->setGradeVert ((Boolean)_grad_vert);
     lsp->setGradeHorz((Boolean)_grad_horz);
     if (_center_percent) {//Need hi resolution mode for best result
       lsp->setHiResolution (1);
     }
     else {
       lsp->setHiResolution ((int)_hi_res);
     }
     lsp->setPNC ((int)(_pncscale->GetScaleValue() * .10));
     lsp->setPPC ((int)(_ppcscale->GetScaleValue() * .10));
     lsp->setMinColorAmp   (_minamp);
     lsp->setMaxColorAmp   (_maxamp);
     lsp->setCenterPercent (_center_percent);
   }
 } //end loop

  if(_sp->imageIsDisplayed() == True)
    _dont_plot_yet = False;
  else
    _dont_plot_yet = True;
  

  if (!_first_time) {
    _cbar->loadToSeisPlot();

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


    if(_plot_on_doaction) {

	if (_cbar) {
	  _cbar->updateAmps (_sp);
	  _cbar->redraw ();
	}

      for (lsp = _list.top(); lsp; lsp = _list.next())
	if (lsp->isPlotDisplayed() &&
            lsp->plotType() >= PlotImage::PlotCOLOR) {
          if (lsp->isCurrentInWindow()) {
            lsp->plot();
	  }
	  else if (!ColorsetCollection::recolorCurrentOnly() &&
                    ColorsetCollection::readOnly(lsp->W())     ) {
   // Kirk Wallace requested that the changes only apply to the current
   //   SeisPlot in the 24-bit color model for cbyt only
	    lsp->setColorRequiredPlotFlag (TRUE);
	  }
	}
      }
    }
  else
    {
    _first_time = False;
    }

}



void SeisColorPop::ChoiceAction( void *data, long which )
{
 SeisColorPop *obj = (SeisColorPop *)data;

   obj->_change_button = False;

   switch(which)
     {
     case GRAYTYPE:
          obj->_seiscolortype->setPredef(PlotImage::GRAY);
          XtSetSensitive(obj->_seiscolortype->getColorScaleWidget(), True);
          XtSetSensitive(obj->_amptypebox->W(), True); 
          XtSetSensitive(obj->_amp1box->W(), True);
          XtSetSensitive(obj->_amp2box->W(), True);
          XtSetSensitive(obj->_pncscale->W(),True);
          XtSetSensitive(obj->_ppcscale->W(),True);
          break;
     case RAMTYPE:
          obj->_seiscolortype->setPredef(PlotImage::MEDRAM);
          obj->_amptypebox->SetRadio(BARVALS);
          XtSetSensitive(obj->_seiscolortype->getColorScaleWidget(), False);
          XtSetSensitive(obj->_amptypebox->W(), False); 
          XtSetSensitive(obj->_amp1box->W(), False);
          XtSetSensitive(obj->_amp2box->W(), False);
          XtSetSensitive(obj->_pncscale->W(),False);
          XtSetSensitive(obj->_ppcscale->W(),False);
          break;
     case COLORTYPE:
          if(obj->_seiscolortype->whichSelected() == SeisCtype::USEDEF)
             obj->_seiscolortype->setPredef(obj->_color_num);
          else
             obj->_seiscolortype->setColorType(SeisCtype::CBAR);
          XtSetSensitive(obj->_seiscolortype->getColorScaleWidget(), True);
          XtSetSensitive(obj->_amptypebox->W(), True); 
          XtSetSensitive(obj->_amp1box->W(), True);
          XtSetSensitive(obj->_amp2box->W(), True);
          XtSetSensitive(obj->_pncscale->W(),True);
          XtSetSensitive(obj->_ppcscale->W(),True);
          break;
     }

   obj->_change_button = True;
}


void SeisColorPop::barAmpAction( void *data, long which )
{
 SeisColorPop *obj = (SeisColorPop *)data;

  if(!obj->made()) return;

   switch(which)
     {
      case  BARVALS:
          XtSetSensitive(obj->_amp1box->W(), False);
          XtSetSensitive(obj->_amp2box->W(), False);
          XtSetSensitive(obj->_pncscale->W(),False);
          XtSetSensitive(obj->_ppcscale->W(),False);
          XtSetSensitive(obj->_centerbox->W(),False);
          obj->_centerbox->SetTog(CENTER_PERCENT, False);
          break;
     case AMPVALS:
          XtSetSensitive(obj->_amp1box->W(), True);
          XtSetSensitive(obj->_amp2box->W(), True);
          XtSetSensitive(obj->_pncscale->W(),False);
          XtSetSensitive(obj->_ppcscale->W(),False);
          obj->_amp1box->SetValue(MINAMP,obj->_minamp);
          obj->_amp2box->SetValue(MAXAMP,obj->_maxamp);
          XtSetSensitive(obj->_centerbox->W(),False);
          obj->_centerbox->SetTog(CENTER_PERCENT, False);
          break;
     case PCNTVALS:
          XtSetSensitive(obj->_amp1box->W(), False);
          XtSetSensitive(obj->_amp2box->W(), False);
          XtSetSensitive(obj->_pncscale->W(),True);
          XtSetSensitive(obj->_ppcscale->W(),True);
          XtSetSensitive(obj->_centerbox->W(),True);
          break;
     }

}


void SeisColorPop::AmpFocusAction( void *data, long )
{
 SeisColorPop *obj = (SeisColorPop *)data;

  obj->_minstart = obj->_minamp;
  obj->_maxstart = obj->_maxamp;

}


void SeisColorPop::AmpLosingFocusAction( void *data, long )
{
 SeisColorPop *obj = (SeisColorPop *)data;

 if(obj->_minstart != obj->_minamp || obj->_maxstart != obj->_maxamp)
    obj->_amptypebox->SetRadio(AMPVALS);
}


void PercentScale::ScaleAction(int)
{
  _scp->_amptypebox->SetRadio(SeisColorPop::PCNTVALS);
}


void SeisColorPop::manage()
{


//The following attachements moved to this manage function from the make
//function in order to resolve circular dependancy warnings if this widget
//is never managed.
 if(_been_managed == False)
   {
   XtVaSetValues( _cbar->W(), XmNtopAttachment,    XmATTACH_FORM,
                              XmNtopOffset,        2,
                              XmNleftAttachment,   XmATTACH_POSITION,
                              XmNleftPosition,     77,
                              XmNrightAttachment,  XmATTACH_FORM,
                              XmNrightOffset,      2,
                              XmNbottomAttachment, XmATTACH_WIDGET,
                              XmNbottomWidget, bottomSeparator(),
                              XmNbottomOffset,     5, 
                              XmNheight,           440,
                              XmNwidth,            150, NULL);


   XtVaSetValues (_gradebox->W(),
		  XmNtopAttachment,  XmATTACH_POSITION,
		  XmNtopPosition,    5,                 // KC added
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition,   50,                // KC added
		  NULL);

// int top_posn, left_posn;
// XtVaGetValues(_gradebox->W(), XmNtopPosition,  &top_posn,
//                               XmNleftPosition, &left_posn, NULL);
// printf ("gradebox T,L should be 5,50 is %d,%d\n", top_posn, left_posn);


   XtVaSetValues (_ctypebox->W(),
		  XmNtopAttachment,  XmATTACH_POSITION,
		  XmNtopPosition,    5,                 // KC added
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition,   3,                 // KC added
		  NULL);

// XtVaGetValues(_ctypebox->W(), XmNtopPosition,  &top_posn,
//		                 XmNleftPosition, &left_posn, NULL);
// printf ("ctypebox T,L should be 5,3 is %d,%d\n", top_posn, left_posn);

 
   XtVaSetValues (_amptypebox->W(),
		  XmNtopAttachment,  XmATTACH_POSITION,
		  XmNtopPosition,    61,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition,   3,
		  NULL);

// XtVaGetValues(_amptypebox->W(), XmNtopPosition,  &top_posn,
//		                   XmNleftPosition, &left_posn, NULL);
// printf ("amptypebox T,L should be 61,3 is %d,%d\n", top_posn, left_posn);


   XtVaSetValues (_amp1box->W(),
		  XmNtopAttachment,  XmATTACH_POSITION,
		  XmNtopPosition,    65,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition,   30,
		  NULL);

   XtVaSetValues (_amp2box->W(),
		  XmNtopAttachment, XmATTACH_POSITION,
		  XmNtopPosition,    65,
		  XmNleftAttachment,XmATTACH_POSITION,
		  XmNleftPosition,   55,
		  NULL);

   XtVaSetValues (_pncscale->W(),
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNtopPosition,    71,
		  XmNtopAttachment,  XmATTACH_POSITION,
		  XmNleftPosition,   30,
		  NULL );

   XtVaSetValues (_ppcscale->W(),
		  XmNtopAttachment,  XmATTACH_POSITION,
		  XmNtopPosition,    71,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition,   55,
		  NULL );

   Widget min_lab= XtVaCreateManagedWidget ("min_lab",
		   xmLabelWidgetClass, topWidget(),
		   XmNtopAttachment,   XmATTACH_POSITION,
		   XmNtopPosition,     66,
		   XmNleftAttachment,  XmATTACH_WIDGET,
                   XmNleftWidget,      _amptypebox->W(),
		   NULL );

   Widget max_lab= XtVaCreateManagedWidget ("max_lab",
                   xmLabelWidgetClass, topWidget(),
                   XmNtopAttachment,   XmATTACH_POSITION,
		   XmNtopPosition,     66,
                   XmNleftAttachment,  XmATTACH_WIDGET,
                   XmNleftWidget,      _amp1box->W(),
		   XmNleftOffset,      10,
		   NULL );

   Widget pnc_lab= XtVaCreateManagedWidget ("pnc_lab",
                   xmLabelWidgetClass, topWidget(),
		   XmNtopAttachment,   XmATTACH_WIDGET,
		   XmNtopWidget,       _amp1box->W(),
                   XmNleftAttachment,  XmATTACH_WIDGET,
                   XmNleftWidget,      _amptypebox->W(),
		   NULL );

   Widget ppc_lab= XtVaCreateManagedWidget( "ppc_lab", xmLabelWidgetClass,
                                            topWidget(),
                             XmNtopAttachment,  XmATTACH_WIDGET,
                             XmNtopWidget,        _amp2box->W(),
                             XmNleftAttachment,   XmATTACH_WIDGET,
                             XmNleftWidget,       _pncscale->W(),    NULL );

   _selectlab= XtVaCreateManagedWidget ("selectlab",
                    xmLabelWidgetClass, topWidget(),
                    XmNtopAttachment,   XmATTACH_POSITION,
		    XmNtopPosition,     25,
                    XmNleftAttachment,  XmATTACH_FORM,
		    XmNleftOffset,      10,
                    XmNrightAttachment, XmATTACH_WIDGET,
                    XmNrightWidget,     _cbar->W(),
		    XmNrightOffset,     5,
                    NULL);

   XtVaSetValues (_seiscolortype->W(),   
		    XmNleftAttachment,   XmATTACH_FORM,
                    XmNleftOffset,       2,
                    XmNrightAttachment,  XmATTACH_WIDGET,
                    XmNrightWidget,      _cbar->W(),
                    XmNtopAttachment,    XmATTACH_WIDGET,
                    XmNtopWidget,        _selectlab,
                    NULL);

   _processlab= XtVaCreateManagedWidget ("processlab",
                    xmLabelWidgetClass, topWidget(),
                    XmNtopAttachment,   XmATTACH_POSITION,
		    XmNtopPosition,     55,
                    XmNleftAttachment,  XmATTACH_FORM,
		    XmNleftOffset,      10,
                    XmNrightAttachment, XmATTACH_WIDGET,
                    XmNrightWidget,     _cbar->W(),
		    XmNrightOffset,     5,
                    NULL);

   Widget sep_1 = XtVaCreateManagedWidget("sep_1", xmSeparatorWidgetClass,
                             topWidget(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _selectlab,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNrightAttachment,  XmATTACH_WIDGET,
                             XmNrightWidget,      _cbar->W(), NULL);

   Widget sep_2 = XtVaCreateManagedWidget("sep_2", xmSeparatorWidgetClass,
                             topWidget(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _processlab,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNrightAttachment,  XmATTACH_WIDGET,
                             XmNrightWidget,      _cbar->W(), NULL); 


   XtVaSetValues( _centerbox->W(),  XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                             XmNleftWidget,     _amptypebox->W(),
                             XmNtopAttachment,  XmATTACH_WIDGET,
                             XmNtopWidget,      _ppcscale->W(),
                             XmNtopOffset,      10,
                             NULL ); 
   
   Widget tmp=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                         XmNtopAttachment, XmATTACH_WIDGET,
                                         XmNtopWidget,     _centerbox->W(),
                                         XmNbottomAttachment, XmATTACH_WIDGET,
                                         XmNbottomWidget,    bottomSeparator(),
                                         XmNleftAttachment,  XmATTACH_FORM,
                                         XmNleftOffset,       5,
                                         XmNtopOffset,        5,
                                         NULL);
  
   if(!_allow_data_processing)
     {
     XtUnmanageChild(_ctypebox->GetRadioWidget(RAMTYPE));
     XtUnmanageChild(_amptypebox->GetRadioWidget(PCNTVALS));
     XtUnmanageChild(_pncscale->topWidget());
     XtUnmanageChild(_ppcscale->topWidget());
     XtUnmanageChild(_centerbox->W());
     XtUnmanageChild(pnc_lab);
     XtUnmanageChild(ppc_lab);
     _amptypebox->SetRadio(AMPVALS);
     }

   }//end of 1st time manage attachements

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
   _gradebox->SetTog( HI_RES, _sp->useHiResolution());
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

 SLBase::manage();
 
 _been_managed = True;
}

void SeisColorPop::reloadDefaults(Boolean)
{

  if (made()) {
      SLFPopSep::reloadDefaults();
      _amp1box->reloadDefaults();
      _amp2box->reloadDefaults();
      _gradebox->reloadDefaults();
      _ctypebox->reloadDefaults();
      _amptypebox->reloadDefaults();
      _pncscale->reloadDefaults();
      _ppcscale->reloadDefaults();
      _dont_plot_yet= True;
      DoAction();
  }
}


void SeisColorPop::reloadSystemDefaults(Boolean do_method)
{
 SLFPopSep::reloadSystemDefaults(do_method);

  _amp1box->SetValue(MINAMP, AMPMIN_DEF);
  _amp2box->SetValue(MAXAMP, AMPMAX_DEF);
    
  _gradebox->SetTog(GRAD_VERT, GRAD_VERT_DEF);
  _gradebox->SetTog(GRAD_HORZ, GRAD_HORZ_DEF);    
  _gradebox->SetTog(HI_RES, HI_RES_DEF);

  _ctypebox->SetRadio(COLORTYPE); 
  _amptypebox->SetRadio(PCNTVALS);

  _pncscale->setScaleValue(100);
  _ppcscale->setScaleValue(100);

  DoAction();
}




void SeisColorPop::notCurrentInWindow(SeisPlot *sp)
{
  if (_sp == sp) 
    {
    _sp= sp->currentSPInWindow();
    addSeisPlot(_sp);
    if (!_list.find(_sp)) addSP(_sp);
    _cbar->setSeisPlot(_sp);
    if(_sp->imageIsDisplayed())
        setAmplitudes(_sp->minColorAmp(), _sp->maxColorAmp());
    }
}

void SeisColorPop::destroyed(SeisPlot *sp)
{
  if (_list.find(sp)) _list.remove(sp);
}

void SeisColorPop::setAmplitudes(float amp_min, float amp_max)
{
  _minamp = _minstart = amp_min;
  _maxamp = _maxstart = amp_max;
  _amp1box->SetValue( MINAMP, _minamp );
  _amp2box->SetValue( MAXAMP, _maxamp );
  _amptypebox->SetRadio(AMPVALS);
}

void SeisColorPop::updateSeisPlotParameters()
{
   DoAction(); 
}


void SeisColorPop::addSP(SeisPlot *newsp)
{
  if (_list.top()) {
    // if this isn't the first one, then share with the first one
    newsp->shareColorsWith (_list.top());
  }
  _list.add(newsp);
  addSeisPlot(newsp);
}

void SeisColorPop::removeSP(SeisPlot *sp)
{
//////////////// new ///////////////////
  if (_list.count() > 1) {
    // if this isn't the first one, then unshare
    // sp->shareColorsWith (NULL); // seems necessary for symmetry!
    // BUT THIS CAUSES A CRASH... when trying to reassign _col
  }
//////////////// new ///////////////////
  _list.remove(sp);
}


//This is called when the user changes the predefined color bar slider
void SeisColorPop::newColorBar()
{
  _color_num = (int)_cbar->predef();

  //Warning color bars with values should always be >= SECTOR
  if(_color_num >= PlotImage::SECTOR)
    {
    XtSetSensitive( _amptypebox->GetRadioWidget(BARVALS), True);
    assert(_cbar->minRGBAmp() != 0.0 ||_cbar->maxRGBAmp() != 0.0 );
    }
  else
    {
    XtSetSensitive( _amptypebox->GetRadioWidget(BARVALS), False);
    }
}


//=======================================================================
//==== Method to transfer this classes parameters to a SeisPlot =========
//==== Example of usage is when the MultiPlotControl makes a    =========
//==== SeisPlot and needs to get the parameters from this menu  =========
//==== before plotting. Added 01-30-02 MLS                      =========
//=======================================================================
void SeisColorPop::setColorOnSeisPlot(SeisPlot *sp)
{

  switch( _ctypebox->WhichSelected() )
    {
    case RAMTYPE:
                   sp->setDoMedian(True);
                   sp->setDoColor(True);
                   sp->setDoPercent(False);
                   sp->setDoAmplitude(False);
                   break;
    case GRAYTYPE:
                   sp->setDoMedian(False);
                   sp->setDoColor(True);
                   break;
    case COLORTYPE:
                   sp->setDoMedian(False);
                   sp->setDoColor(True);
                   break;
    }


  switch( _amptypebox->WhichSelected() )
    {
    case BARVALS:  
                   if(_cbar->minRGBAmp() == 0.0 && _cbar->maxRGBAmp() == 0.0)
		      {
                      _amptypebox->SetRadio(PCNTVALS);
                      sp->setDoPercent(True);
                      sp->setDoAmplitude(False); 
                      break;
		      }
                   sp->setDoPercent(False);
                   sp->setDoAmplitude(False);
                   _minamp = _cbar->minRGBAmp();
                   _maxamp = _cbar->maxRGBAmp();
                   break;
    case AMPVALS:
                   sp->setDoPercent(False);
                   sp->setDoAmplitude(True);                              
                   break;
    case PCNTVALS:
                   sp->setDoPercent(True);
                   sp->setDoAmplitude(False); 
                   break;
    }


   sp->setGradeVert((Boolean)_grad_vert);
   sp->setGradeHorz((Boolean)_grad_horz);
   sp->setHiResolution((int)_hi_res);
   sp->setPNC( (int)(_pncscale->GetScaleValue() * .10) );
   sp->setPPC( (int)(_ppcscale->GetScaleValue() * .10));
   sp->setMinColorAmp(_minamp);
   sp->setMaxColorAmp(_maxamp);

   _cbar->loadToSeisPlot();

}






//=============================================================================
//======================== Internal SeisCtypePop class ========================
//=============================================================================
void SeisCtypePop::typeChange(int which)
{

  if(_scp->_change_button) _scp->_ctypebox->SetRadio(SeisColorPop::COLORTYPE);

  if(which == USEDEF && 
     _scp->_amptypebox->WhichSelected() == SeisColorPop::BARVALS) 
       _scp->_amptypebox->SetRadio(SeisColorPop::PCNTVALS);

  if(_scp->_cbar->minRGBAmp() == 0.0 && _scp->_cbar->maxRGBAmp() == 0.0)
     {
     //_scp->_amptypebox->SetRadio(PCNTVALS);
     XtSetSensitive( 
               _scp->_amptypebox->GetRadioWidget(SeisColorPop::BARVALS), False);
     }
  else
     {
     XtSetSensitive(
               _scp->_amptypebox->GetRadioWidget(SeisColorPop::BARVALS), True);
     }


  _scp->_color_num = (int)_cbar->predef();
}

void SeisCtypePop::newColorBar()
{
  //Notify the SeisColorPop that the predefined color slider has changes
  _scp->newColorBar();
}




