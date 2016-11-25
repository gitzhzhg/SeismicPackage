//********************************************************
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
//Author Michael L. Sherrill 10/95
//Class that creates sub menus of the main qc pop up menu
//********************************************************

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include "geom/field_geometry.hh"
#include "fgqc/fgqc_pop_sub_menu.hh"
#include "fgqc/fgqc_pop.hh"
#include "fgqc/fgqc_plot.hh"
#include "sl/sl_push_box.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_radio_box.hh"
#include "fgqc/fgqc_plot_constants.hh"




#define ParentClass SLFPopSep


FgQcPopSubMenu::FgQcPopSubMenu( Widget              p,
                                char                *name,
                                HelpCtx             hctx, 
                                FgQcPop             *fgqc_pop)
                  : SLFPopSep(p,name,FP_DOOK|FP_DOHELP,hctx,True,False),
                  _plot_on_doaction(True), _use_file_defaults(False), 
                  _new_appdefaults(True),  _first_time(True), 
                  _hctx(hctx), _fgqc_pop(fgqc_pop), 
                  _active_plot(fgqc_pop->_active_plot)

{


}




FgQcPopSubMenu::~FgQcPopSubMenu()
{

}

//========================================================================
//================ Offset/Azimuth Distribution Sub Menu ==================
//========================================================================
static String  offset_defres[]= {
    "*offset_menu_popup.title:         Offset Distribution",
    "*offminL.labelString:             Minimum Offset",
    "*offmaxL.labelString:             Maximum Offset",
    "*fixedlimits.labelString:         Fixed Limits",
    "*dynamiclimits.labelString:       Dynamic Limits",
    "*limittyperadiobox_Frame.top      5",
    "*limittyperadiobox_Frame.left     5",
    ".width:                           280",
    ".height:                          250",
    NULL};

static String  azimuth_defres[]= {
    "*azimuth_menu_popup.title:        Azimuthal Distribution",
    "*azmminL.labelString:             Minimum Azimuth",
    "*azmmaxL.labelString:             Maximum Azimuth",
    "*offminL.labelString:             Minimum Offset",
    "*offmaxL.labelString:             Maximum Offset",
    "*fixedlimits.labelString:         Fixed Limits",
    "*dynamiclimits.labelString:       Dynamic Limits",
    "*limittyperadiobox_Frame.top      5",
    "*limittyperadiobox_Frame.left     5",
    ".width:                           280",
    ".height:                          250",
    NULL};

enum {OFFSET_MIN, OFFSET_MAX, AZIMUTH_MIN, AZIMUTH_MAX};

static SLRadio limittypes[]= {
    {"Fixed Limits",    FIXED_LIMITS},
    {"Dynamic Limits",  DYNAMIC_LIMITS},
  };

OffsetDistributionMenu::OffsetDistributionMenu( Widget              p,
                                                char                *name,
                                                HelpCtx             hctx, 
                                                FgQcPop             *fgqc_pop,
                                                long                menu_type)
                                       : FgQcPopSubMenu (p,name,hctx,fgqc_pop),
                                         _menu_type (menu_type)


{
  if (_menu_type == AZIMUTHALD) {
    static SLText azimuth_texts[]  = {
      {"offmin",  NULL, NULL, SLType_float, OFFSET_MIN},
      {"offmax",  NULL, NULL, SLType_float, OFFSET_MAX},
      {"azmmin",  NULL, NULL, SLType_float, AZIMUTH_MIN},
      {"azmmax",  NULL, NULL, SLType_float, AZIMUTH_MAX},
    };
  
    azimuth_texts[0].target= &_offset_min;
    azimuth_texts[1].target= &_offset_max;
    azimuth_texts[2].target= &_azimuth_min;
    azimuth_texts[3].target= &_azimuth_max;
    

    setDefaultResources( p, name, azimuth_defres);

    _offset_box = new SLTextBox( this, "azimuth_box", _hctx,
                                azimuth_texts, XtNumber(azimuth_texts), 
                                True, 1, True, False );
  }
  else /* if(_menu_type == OFFSETD) */ {
    static SLText offset_texts[]  = {
      {"offmin",  NULL, NULL, SLType_float, OFFSET_MIN},
      {"offmax",  NULL, NULL, SLType_float, OFFSET_MAX},
    };
  
    offset_texts[0].target= &_offset_min;
    offset_texts[1].target= &_offset_max;

    setDefaultResources( p, name, offset_defres);

    _offset_box = new SLTextBox( this, "offset_box", _hctx,
                                offset_texts, XtNumber(offset_texts), 
                                True, 1, True, False );
  }
  _offset_box->setAltLosingAction( (SLTextfunc)LosingFocusAction, this );

  _limittyperadiobox = new SLRadioBox (this, "limittyperadiobox", getHelpCtx(),
    limittypes, XtNumber(limittypes), NULL, True, False);
  _limittyperadiobox->setAltChoiceAction
    ((SLRadioButtonfunc)LimitTypeAction, this);
  _limit_type = FIXED_LIMITS;

}




OffsetDistributionMenu::~OffsetDistributionMenu()
{

}


Widget OffsetDistributionMenu::make(Widget p)
{

  SLFPopSep::make(p);

  XtVaSetValues( _offset_box->W(), XmNtopAttachment,  XmATTACH_FORM,
                                   XmNtopOffset,      20,
                                   XmNleftAttachment, XmATTACH_FORM, 
                                   XmNleftOffset,     5, 
                                   XmNrightAttachment,XmATTACH_FORM, 
                                   XmNrightOffset,    5,NULL);

  _limittyperadiobox->SetRadio (_limit_type);

  XtVaSetValues (_limittyperadiobox->W(),
                                   XmNtopAttachment,  XmATTACH_WIDGET,
                                   XmNtopWidget,      _offset_box->W(),
                                   XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,     _offset_box->W(),
                                   XmNleftOffset,     15,
                                   XmNtopOffset,      5, NULL);

  return(topWidget());
}


void OffsetDistributionMenu::manage()
{
  if(_first_time)
    {
    _offset_box->SetValue(OFFSET_MIN, _fgqc_pop->fg()->getMinimumOffset());
    _offset_box->SetValue(OFFSET_MAX, _fgqc_pop->fg()->getMaximumOffset());
    if(_menu_type == AZIMUTHALD)
      {
      _offset_box->SetValue(AZIMUTH_MIN, (float)  0.0);
      _offset_box->SetValue(AZIMUTH_MAX, (float)360.0);
      }
    _first_time = False;
    }

  ParentClass::manage();

}

void OffsetDistributionMenu::LosingFocusAction(void *data,long /*which*/)
{
OffsetDistributionMenu *obj = (OffsetDistributionMenu *)data;

  obj->_active_plot->setOffsets(obj->_offset_min, obj->_offset_max);
  obj->_active_plot->setAzimuths(obj->_azimuth_min, obj->_azimuth_max);
}

void OffsetDistributionMenu::LimitTypeAction (void *data, long which)
{
  OffsetDistributionMenu *obj = (OffsetDistributionMenu *)data;
  obj->_limit_type = which;
  obj->_active_plot->setLimitType (obj->_limit_type);
  if (obj->_limit_type == DYNAMIC_LIMITS) {
    obj->_fgqc_pop->_extendedcolorsbox->SetTog (EXTENDED_COLORS, True);
    XtSetSensitive
      (obj->_fgqc_pop->_extendedcolorsbox->TogW(EXTENDED_COLORS), False);
  }
  else /* if (obj->_limit_type == FIXED_LIMITS) */ {
    XtSetSensitive
      (obj->_fgqc_pop->_extendedcolorsbox->TogW(EXTENDED_COLORS), True);
  }
}





//========================================================================
//================ Normalized Bin Sub Menu ===============================
//========================================================================
static String  normalized_defres[]= {
    "*normalized_bin_menu_popup.title: Normalized Bin",
    "*percentL.labelString:            Percent Of Data",
    ".width:                           280",
    ".height:                          135",
    NULL};

enum {BIN_PERCENT};

NormalizedBinMenu::NormalizedBinMenu( Widget              p,
                                      char                *name,
                                      HelpCtx             hctx, 
                                      FgQcPop             *fgqc_pop)
                                    : FgQcPopSubMenu (p,name,hctx,fgqc_pop)
{

static SLText percent_texts[] = 
  {
    {"percent",  NULL, NULL, SLType_float, BIN_PERCENT},
  };
  percent_texts[0].target= &_percent;

  _percent = 100.0;

  setDefaultResources( p, name, normalized_defres);

  _percent_box = new SLTextBox( this, "percent_box", _hctx,
                                percent_texts, XtNumber(percent_texts), 
                                True, 1, True, False );

  _percent_box->setAltLosingAction( (SLTextfunc)LosingFocusAction,
                                    this );

  _percent = 100.0;
  _percent_box->SetValue(BIN_PERCENT, _percent);
}

NormalizedBinMenu::~NormalizedBinMenu()
{

}


Widget NormalizedBinMenu::make(Widget p)
{

  SLFPopSep::make(p);
  XtVaSetValues( _percent_box->W(), XmNtopAttachment,  XmATTACH_FORM,
                                    XmNtopOffset,      10,
                                    XmNleftAttachment, XmATTACH_FORM, 
                                    XmNleftOffset,     5, 
                                    XmNrightAttachment,XmATTACH_FORM, 
                                    XmNrightOffset,    5,NULL);
  return(topWidget());
}


void NormalizedBinMenu::LosingFocusAction(void *data,long/*which*/)
{
NormalizedBinMenu *obj = (NormalizedBinMenu *)data;

  if(obj->_percent < 1.0)
    obj->_percent_box->SetValue(BIN_PERCENT, (float)1.0);
  if(obj->_percent > 100.0) 
    obj->_percent_box->SetValue(BIN_PERCENT, (float)100.0);
  obj->_active_plot->setPercent(obj->_percent);
}
