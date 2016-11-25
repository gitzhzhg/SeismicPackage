#include "fgmap/fg_plot_color_pop.hh"
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
#include "fgmap/fg_seis_plot_list.hh"
#include "fgxp/fgxp_constants.hh"
#include "sl/sl_color_option.hh"
#include "sl/sl_option_menu.hh"
#include "sl/sl_row_column.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_label.hh"
#include "sl/shell_stat_msg.hh"
#include <Xm/Label.h>



enum { RED, CYAN, MAGENTA, PURPLE, BLUE, YELLOW, GREEN,
       FOREST_GREEN, ORANGE, WHITE, LIGHT_GRAY, SLATE_GRAY,
       SALMON, CHOCOLATE, BLACK };

static SLPush colors_ary[] = {
                { "red",          RED},
                { "cyan",         CYAN},
                { "magenta",      MAGENTA},
                { "purple",       PURPLE},
                { "blue",         BLUE},
                { "yellow",       YELLOW},
                { "green",        GREEN},
                { "forest green", FOREST_GREEN},
                { "orange",       ORANGE},
                { "white",        WHITE},
                { "light gray",   LIGHT_GRAY},
                { "slate gray",   SLATE_GRAY},
                { "salmon",       SALMON},
                { "chocolate",    CHOCOLATE},
                { "black",        BLACK},
        };

/*
 * Keep longest option in flag_mode_ary shorter than longest option
 * in colors_ary.  Temporarily add the longest option from colors_ary
 * so the option menus will be the same size and the popup lines up
 * nicely.  In the managing virtual function, delButton of the longest
 * option from colors_ary.
 */
static SLPush flag_mode_ary[] = {
                { "show all",  (long) ShowAll},
                { "srcs only", (long) ShowSrcsOnly},
                { "rcvs only", (long) ShowRcvsOnly},
                { "no comp.",  (long) HideComputed},
                { "no unass.", (long) HideUnassigned},
                { "forest green", 999},	/* delButton in managing */
        };

static String  defres[]= {
     "_popup.title:                    Plot Color Control",
     "*flagt.labelString:              Flag Colors",
     "*linet.labelString:              Line Colors",
     "*flagt.fontList:        -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
     "*linet.fontList:        -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
     "*plot_backgroundL.labelString:   Background:",
     "*flag_modeL.labelString:         Flag Mode:",
     "*active_flagL.labelString:       Active Flag:",
     "*selected_flagL.labelString:     Selected Flag:",
     "*computed_flagL.labelString:     Computed Flag:",
     "*default_flagL.labelString:      Default Flag:",
     "*active_lineL.labelString:       Active Line:",
     "*selected_lineL.labelString:     Selected Line:",
     "*has_rec_lineL.labelString:      Line w/ rec.:",
     "*has_source_lineL.labelString:   Line w/ sources:",
     "*has_both_lineL.labelString:     Line w/ both:",
     "*default_lineL.labelString:      Default Line:",
     "*XmToggleButton.labelString:     Use Color",
     "*reset_def.labelString:          Reset",
     "*XmToggleButton.set:             True",
     "*rc_plot.orientation:            VERTICAL",
     "*rc_plot.packing:                PACK_COLUMN",
     "*rc_plot.spacing:                0",
     "*rc_plot.numColumns:             3",
     "*rc_plot.adjustMargin:           False",
     "*rc_flags.orientation:           VERTICAL",
     "*rc_flags.packing:               PACK_COLUMN",
     "*rc_flags.spacing:               0",
     "*rc_flags.numColumns:            3",
     "*rc_flags.adjustMargin:          False",
     "*rc_lines.orientation:           VERTICAL",
     "*rc_lines.packing:               PACK_COLUMN",
     "*rc_lines.spacing:               0",
     "*rc_lines.numColumns:            3",
     "*rc_lines.adjustMargin:          False",
     "*plot_background.value:          light gray",
     "*flag_mode.value:                show all",
     "*active_flag.value:              blue",
     "*selected_flag.value:            green",
     "*computed_flag.value:            yellow",
     "*default_flag.value:             red",
     "*active_line.value:              blue",
     "*selected_line.value:            green",
     "*has_rec_line.value:             magenta",
     "*has_source_line.value:          cyan",
     "*has_both_line.value:            purple",
     "*default_line.value:             red",
    NULL, };

enum { RESET_DEF= 100 };


FgPlotColorPop::FgPlotColorPop(Widget          p,
                               char           *name,
                               HelpCtx         hctx,
                               FgSeisPlotList *list) :
                    SLFPopSep(p,name, FP_DOSTANDARD, hctx, False, False),
                    _fgsp_list(list), _must_tidy_options(True)
{

  setDefaultResources(XtDisplay(p), name, defres);
  _rc_plot= new SLRowColumn(this, "rc_plot",NULL,True);
  _rc_flags= new SLRowColumn(this, "rc_flags",NULL,True);
  _rc_lines= new SLRowColumn(this, "rc_lines",NULL,True);

  // labels
  new SLpLabel(_rc_plot,  "plot_backgroundL");
  new SLpLabel(_rc_plot,  "flag_modeL");
  new SLpLabel(_rc_flags, "active_flagL");
  new SLpLabel(_rc_flags, "selected_flagL");
  new SLpLabel(_rc_flags, "computed_flagL");
  new SLpLabel(_rc_flags, "default_flagL");

  new SLpLabel(_rc_lines, "active_lineL");
  new SLpLabel(_rc_lines, "selected_lineL");
  new SLpLabel(_rc_lines, "has_both_lineL");
  new SLpLabel(_rc_lines, "has_rec_lineL");
  new SLpLabel(_rc_lines, "has_source_lineL");
  new SLpLabel(_rc_lines, "default_lineL");

  // option menus
  int numc= XtNumber(colors_ary);
  _plot_background= new SLColorOption(_rc_plot, "plot_background", 
                                                          colors_ary, numc );
  _flag_mode=       new SLOptionMenu (_rc_plot, "flag_mode", 
                                                       _rc_plot->getHelpCtx(),
                                                       flag_mode_ary,
                                                       XtNumber(flag_mode_ary));
  _active_flag=     new SLColorOption(_rc_flags, "active_flag", 
                                                          colors_ary, numc );
  _selected_flag=   new SLColorOption(_rc_flags, "selected_flag", 
                                                          colors_ary, numc );
  _computed_flag=   new SLColorOption(_rc_flags, "computed_flag", 
                                                          colors_ary, numc );
  _default_flag=    new SLColorOption(_rc_flags, "default_flag", 
                                                          colors_ary, numc );

  _active_line=     new SLColorOption(_rc_lines, "active_line", 
                                                          colors_ary, numc );
  _selected_line=   new SLColorOption(_rc_lines, "selected_line", 
                                                          colors_ary, numc );
  _has_both_line=   new SLColorOption(_rc_lines, "has_both_line", 
                                                          colors_ary, numc );
  _has_rec_line=    new SLColorOption(_rc_lines, "has_rec_line", 
                                                          colors_ary, numc );
  _has_source_line= new SLColorOption(_rc_lines, "has_source_line", 
                                                          colors_ary, numc );
  _default_line=    new SLColorOption(_rc_lines, "default_line", 
                                                          colors_ary, numc );

  _active_flag->setComplexNotify(this);
  _selected_flag->setComplexNotify(this);
  _computed_flag->setComplexNotify(this);
  _active_line->setComplexNotify(this);
  _selected_line->setComplexNotify(this);
  _has_rec_line->setComplexNotify(this);
  _has_source_line->setComplexNotify(this);
  _has_both_line->setComplexNotify(this);

  addExtraButton("reset_def", RESET_DEF);


  // toggle buttons
  _use_active_flag  = new SLpToggle(_rc_flags, "use_active_flag");
  _use_selected_flag= new SLpToggle(_rc_flags, "use_selected_flag");
  _use_computed_flag= new SLpToggle(_rc_flags, "use_computed_flag");

  _use_active_line  = new SLpToggle(_rc_lines, "use_active_line");
  _use_selected_line= new SLpToggle(_rc_lines, "use_selected_line");
  _use_both_line    = new SLpToggle(_rc_lines, "use_both_line");
  _use_rec_line     = new SLpToggle(_rc_lines, "use_rec_line");
  _use_source_line  = new SLpToggle(_rc_lines, "use_source_line");

  new SLpLabel(_rc_flags, "");
  new SLpLabel(_rc_lines, "");

  defaultButton(FP_OK);
  setColors();
}

FgPlotColorPop::~FgPlotColorPop()
{
  delete _rc_flags;
  delete _rc_lines;
  delete _rc_plot;

  delete _plot_background;
  delete _flag_mode;
  delete _active_flag;
  delete _selected_flag;
  delete _computed_flag;
  delete _default_flag;

  delete _active_line;
  delete _selected_line;
  delete _has_rec_line;
  delete _has_source_line;
  delete _has_both_line;
  delete _default_line;

  delete _use_active_flag;
  delete _use_selected_flag;
  delete _use_computed_flag;
  delete _use_default_flag;

  delete _use_rec_line;
  delete _use_source_line;
  delete _use_both_line;
  delete _use_default_line;
}


Widget FgPlotColorPop::make(Widget p)
{

   if ( made() ) return topWidget();
   ShellStatMsg  bld_info(wParent(),"Building Plot Color Popup...");
   SLFPopSep::make(p);
   Widget flagt;
   Widget linet;

   XtVaSetValues(_rc_plot->W(),  XmNleftAttachment, XmATTACH_FORM,
                                 XmNtopAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,     5,
                                 XmNtopOffset,      5, NULL);

   flagt=  XtVaCreateManagedWidget( "flagt", xmLabelWidgetClass, topWidget(),
                                     XmNtopAttachment,    XmATTACH_WIDGET,
                                     XmNtopWidget,        _rc_plot->W(),
                                     XmNleftAttachment,   XmATTACH_FORM,
                                     XmNrightAttachment,  XmATTACH_FORM,
                                     XmNtopOffset,        8,
                                     XmNleftOffset,       5,
                                     XmNrightOffset,      5,
                                     NULL);

   XtVaSetValues(_rc_flags->W(), XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                 XmNtopAttachment,  XmATTACH_WIDGET,
                                 XmNleftWidget,     _rc_plot->W(),
                                 XmNtopWidget,      flagt,
                                 XmNtopOffset,      5, NULL);

   linet=  XtVaCreateManagedWidget( "linet", xmLabelWidgetClass, topWidget(),
                                     XmNtopAttachment,    XmATTACH_WIDGET,
                                     XmNtopWidget,        _rc_flags->W(),
                                     XmNleftAttachment,   XmATTACH_FORM,
                                     XmNrightAttachment,  XmATTACH_FORM,
                                     XmNtopOffset,        8,
                                     XmNleftOffset,       5,
                                     XmNrightOffset,      5,
                                     NULL);

   XtVaSetValues(_rc_lines->W(), XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                 XmNtopAttachment,  XmATTACH_WIDGET,
                                 XmNleftWidget,     _rc_plot->W(),
                                 XmNtopWidget,      linet,
                                 XmNtopOffset,      5, NULL);

   Widget tmp2=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                     XmNleftAttachment,   XmATTACH_FORM,
                                     XmNleftOffset,       5,
                                     XmNtopAttachment,    XmATTACH_WIDGET,
                                     XmNtopWidget,        _rc_lines->W(),
                                     XmNbottomAttachment, XmATTACH_WIDGET,
                                     XmNbottomWidget,     bottomSeparator(),
                                     NULL);

   Widget tmp3=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                     XmNleftAttachment,   XmATTACH_WIDGET,
                                     XmNleftOffset,       5,
                                     XmNleftWidget,       _rc_flags->W(),
                                     XmNtopAttachment,    XmATTACH_FORM,
                                     XmNtopOffset,        5,
                                     XmNrightAttachment,  XmATTACH_FORM,
                                     NULL);

   return topWidget();
}

void  FgPlotColorPop::managing()
{
	if (_must_tidy_options && XtWindow(_flag_mode->topWidget()))
	{
		Widget rc;

		XtVaGetValues(_flag_mode->topWidget(),
			XmNsubMenuId, &rc,
			NULL);

		XtVaSetValues(rc,
			XmNresizeWidth, False,
			NULL);

		_flag_mode->delButton(999);

		_must_tidy_options = False;
	}
}

void  FgPlotColorPop::extraButton(int ident)
{
  if (ident == RESET_DEF) {
     _plot_background->setButton(LIGHT_GRAY);
     _flag_mode->setButton(ShowAll);
     _active_flag->setButton(BLUE); 
     _selected_flag->setButton(GREEN);
     _computed_flag->setButton(YELLOW);
     _default_flag->setButton(RED);
     _active_line->setButton(BLUE);
     _selected_line->setButton(GREEN);
     _has_both_line->setButton(PURPLE);
     _has_rec_line->setButton(MAGENTA);
     _has_source_line->setButton(CYAN);
     _default_line->setButton(RED);

     _use_active_flag->setToggleValue(True);
     _use_selected_flag->setToggleValue(True);
     _use_computed_flag->setToggleValue(True);
     _use_active_line->setToggleValue(True);
     _use_selected_line->setToggleValue(True);
     _use_both_line->setToggleValue(True);
     _use_rec_line->setToggleValue(True);
     _use_source_line->setToggleValue(True);

  }
}

Boolean FgPlotColorPop::notifyComplex(SLDelay *obj, int)
{
  if (obj== _active_flag)         _use_active_flag->setToggleValue(True);
  else if (obj== _selected_flag)  _use_selected_flag->setToggleValue(True);
  else if (obj== _computed_flag)  _use_computed_flag->setToggleValue(True);
  else if (obj== _active_line)    _use_active_line->setToggleValue(True);
  else if (obj== _selected_line)  _use_selected_line->setToggleValue(True);
  else if (obj== _has_rec_line)   _use_rec_line->setToggleValue(True);
  else if (obj== _has_source_line)_use_source_line->setToggleValue(True);
  else if (obj== _has_both_line)  _use_both_line->setToggleValue(True);

  return True;
}

void FgPlotColorPop::setColors()
{
  char *str;
  
  if (_plot_background->made())
         _fgsp_list->setPlotBgPixel( _plot_background->currentPixel() );

  if (_flag_mode->made())
         _fgsp_list->setFlagMode( (FlagMode) _flag_mode->whichSelected() );

  str= _active_flag->currentColor();
  _fgsp_list->setActiveFlagColor( str, 
                                  (Boolean)_use_active_flag->toggleValue());
  free(str);

  str= _selected_flag->currentColor();
  _fgsp_list->setSelectedFlagColor( str, 
                            (Boolean)_use_selected_flag->toggleValue());
  free(str);

  str= _computed_flag->currentColor();
  _fgsp_list->setComputedFlagColor( str, 
                            (Boolean)_use_computed_flag->toggleValue());
  free(str);

  str= _default_flag->currentColor();
  _fgsp_list->setDefaultFlagColor( str);
  free(str);


  str= _active_line->currentColor();
  _fgsp_list->setActiveLineColor( str, 
                            (Boolean)_use_active_line->toggleValue());
  free(str);

  str= _selected_line->currentColor();
  _fgsp_list->setSelectedLineColor( str, 
                            (Boolean)_use_selected_line->toggleValue());
  free(str);

  str= _has_rec_line->currentColor();
  _fgsp_list->setReceiverLineColor( str, 
                                    (Boolean)_use_rec_line->toggleValue());
  free(str);

  str= _has_source_line->currentColor();
  _fgsp_list->setSourceLineColor( str, 
                                  (Boolean)_use_source_line->toggleValue());
  free(str);

  str= _has_both_line->currentColor();
  _fgsp_list->setBothLineColor( str, (Boolean)_use_both_line->toggleValue());
  free(str);

  str= _default_line->currentColor();
  _fgsp_list->setDefaultLineColor( str);
  free(str);

}

void FgPlotColorPop::DoAction()
{
  setColors();
  _fgsp_list->updateColorsOnPlots();
}
