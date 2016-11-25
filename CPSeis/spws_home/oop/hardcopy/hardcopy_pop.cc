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
#include <unistd.h>
#include <Xm/Label.h>
#include "hardcopy/hardcopy_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_hard_copy.hh"
#include "sl/sl_text_box.hh"
#include "sl/slp_push.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_option_menu.hh"
#include "sl/error_handler.hh"
#include "sl/shell_watch.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/error_handler.hh"
#include "sl/slp_file.hh"
#include "plot/pick_watch.hh"

static String  defres[]= {
    "_popup.title:                  Plot Hard Copy",
    "*file.labelString:             Plot File:",
    "*file.annoType:                Label",
    "*file.fileDescription:         Plot File",
    "*nosubmit.set:                 False",
    "*submit.set:                   False",
    "*submit_del.set:               True",
    "*widthL.labelString:           Length Plot Inches:",
    "*heightL.labelString:          Height Plot Inches:",
    "*file.fileFlags:               IsRequired Writable AddExtAlways",
    "*nosubmit.labelString:         Don't Submit plot file",
    "*submit.labelString:           Submit plot - save plot file",
    "*submit_del.labelString:       Submit plot - delete plot file",
    "*scaleL.labelString:           Obj. Scale Fact.",
    "*hard_symetrical.set:          True",
    "*hard_annotation.set:          True",
    "*use_pip.set:                  True",
    "*frame_only.set:               True",
    "*hard_symetrical.labelString:  Enforce Symetrical Size",
    "*hard_annotation.labelString:  Show Annotation",
    "*cbar_both.labelString:        Color Bar on both sides",
    "*use_pip.labelString:          Use Pip Extension",
    "*panel.labelString:            Panel all Frames of Movie",
    "*frame_only.labelString:       Plot only current frame",
    "*copiesL.labelString:          # Copies   :",
    "*copies.value:                 1",
    NULL };

#define MIN_DIMENSION 10
#define SCRIPT_NAME "spws_hardcopy_submit"

extern "C" int wpSystem(char *command);

enum {NOSUBMIT, SUBMIT, SUBMIT_DEL, SCALE, SYM, ANNO, CBAR_BOTH, USE_PIP,
      PANEL, FRAME_ONLY, COPIES};

static SLRadio sub_rads[]  = {
         { "nosubmit",   NOSUBMIT },
         { "submit",     SUBMIT },
         { "submit_del", SUBMIT_DEL },
       };

static SLRadio panel_rads[]  = {
         { "panel",       PANEL },
         { "frame_only",  FRAME_ONLY },
       };

static SLTog extra_tog[]  = {
    { "hard_annotation",    NULL,     ANNO },
    { "cbar_both",          NULL,     CBAR_BOTH},
    { "hard_symetrical",    NULL,     SYM },  // put this on last
       };

static SLTog extra_tog2[]  = {
    { "hard_annotation",    NULL,     ANNO },
    { "cbar_both",          NULL,     CBAR_BOTH},
    { "use_pip",            NULL,     USE_PIP},
       };



extern "C" int addext_rep_( char name[], char ext[], int *istat );



HardCopyPop::HardCopyPop(Widget        p,
                         char         *name,
                         HelpCtx       hctx,
                         SeisPlot     *sp,
			 SLpFileData  *slp_file_data,
                         PopType      ptype,
                         Boolean      show_movie_style,
                         float        marker_scale_factor,
                         Boolean      allow_scale_change) :
        SLFormFCPop(p,name, FP_DOSTANDARD, slp_file_data, hctx, False, False),
        SeisInform(sp),
        _sp(sp), _submit_script(NULL),
        _total_dim(NULL),
        _marker_scale_factor(marker_scale_factor), _scale_input(NULL),
        _extra_input(NULL), _first_time_man(True), _movie_style(NULL)
{

 setDefaultResources( p, name, defres);

 static SLText dim_text[]  = {
   {"width",   "range:1.0 *,default:10.0",  NULL,  SLType_float,   WIDTH},
   {"height",  "range:1.0 *,default:10.0",  NULL,  SLType_float,   HEIGHT},
  };
 dim_text[0].target=&_width;
 dim_text[1].target=&_height;

 static SLText scale_text[]  = {
   { "scale",   "range:0.1 *,default:2.0",  NULL,  SLType_float,   SCALE },
 };
 scale_text[0].target=&_marker_scale_factor; 

 static SLText copies_text[]  = {
   { "copies",   "range:1 100,default:1",  NULL,  SLType_int,   COPIES },
 };
 copies_text[0].target = &_num_copies; 


  assert(marker_scale_factor > -0.25 );  // greater or = to 0

  _using_symetrical= (ptype == SYMETRICAL);
  _using_pip=        (ptype == PIP);
  _make_symetrical= False;


  _plot_dim= new SLTextBox( this, "plot_dim", hctx, 
                            dim_text, XtNumber(dim_text));
  if (allow_scale_change) {
        _scale_input= new SLTextBox( this, "scale_input", hctx, 
                                  scale_text, XtNumber(scale_text));
        _scale_input->SetRange( (int)SCALE, 0.1, 20.0,
          (double)marker_scale_factor, (long)1);
        _scale_input->SetValue( SCALE, marker_scale_factor);
        
  }
  if (_using_symetrical) {
       _extra_input= new SLTogBox( this, "symetrical", hctx,
                                   extra_tog, XtNumber(extra_tog), True);
       _make_symetrical= _extra_input->IsSelected(SYM);
  }
  else if (_using_pip) {
       _extra_input= new SLTogBox( this, "symetrical", hctx,
                                   extra_tog2, XtNumber(extra_tog2), True);
  }
  else {
       _extra_input= new SLTogBox( this, "symetrical", hctx,
                                   extra_tog, XtNumber(extra_tog)-1, True);
  }
  _submit_action= new SLRadioBox( this, "submit_action", getHelpCtx(),
                                  sub_rads, XtNumber(sub_rads), NULL, True);
  if (show_movie_style)
        _movie_style= new SLRadioBox( this, "movie_style", getHelpCtx(),
                                      panel_rads, XtNumber(panel_rads), 
                                      NULL, True);
  _queue= new SLOptionMenu(this, "queue", NULL, NULL, 0);
  _queue->setLabel("Plot Queue:");   

  _copies_text= new SLTextBox( this, "copies_text", hctx, 
                               copies_text, XtNumber(copies_text));


}

HardCopyPop::~HardCopyPop()
{
  if (_submit_script) free(_submit_script);
  delete _plot_dim;
  delete _submit_action;
  delete _queue;
  delete _scale_input;

}


Widget HardCopyPop::make(Widget p)
{
   if ( made() ) return topWidget();
   p= p ? p : wParent();
   ShellStatMsg  bld_info(p,"Building Hardcopy plot Popup...");
   SLFormFCPop::make(p);

   XtVaSetValues (_slp_file->W(),
		  XmNtopAttachment,    XmATTACH_FORM,
		  XmNtopOffset,        5,
		  XmNleftAttachment,   XmATTACH_FORM,
		  XmNleftOffset,       5,
		  XmNrightAttachment,  XmATTACH_FORM,
		  XmNrightOffset,      5,
		  NULL);

   XtVaSetValues( _plot_dim->W(), XmNleftAttachment, XmATTACH_FORM,
                                  XmNleftOffset,     10,
                                  XmNtopAttachment,  XmATTACH_WIDGET,
                                  XmNtopOffset,      15,
                                  XmNtopWidget,      _slp_file->W(),
                                  NULL );
   _total_dim= XtVaCreateManagedWidget("total_dim", xmLabelWidgetClass, 
                               topWidget(),
                               XmNleftAttachment,  XmATTACH_FORM,
                               XmNrightAttachment, XmATTACH_FORM,
                               XmNrightOffset,     10,
                               XmNleftOffset,      10,
                               NULL);
   XtVaSetValues( _extra_input->W(),
                  XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                  XmNleftWidget,     _plot_dim->W(),
                  XmNtopAttachment,  XmATTACH_WIDGET,
                  XmNtopWidget,      _plot_dim->W(),
                  XmNtopOffset,      10,
                  NULL );
   XtVaSetValues( _total_dim,
                  XmNtopAttachment, XmATTACH_WIDGET,
                  XmNtopOffset,     10,
                  XmNtopWidget,     _extra_input,    NULL);

   XtVaSetValues( _submit_action->W(), XmNleftAttachment, XmATTACH_FORM,
                                       XmNleftOffset,     10,
                                       XmNtopAttachment,  XmATTACH_WIDGET,
                                       XmNtopOffset,      15,
                                       XmNtopWidget,      _total_dim,
                                       NULL );

   if (_movie_style) 
        XtVaSetValues( _movie_style->W(), XmNleftAttachment, XmATTACH_FORM,
                                          XmNleftOffset,     10,
                                          XmNtopAttachment,  XmATTACH_WIDGET,
                                          XmNtopOffset,      10,
                                          XmNtopWidget,      _submit_action,
                                          NULL );

   XtVaSetValues( _queue->W(), XmNleftAttachment, XmATTACH_WIDGET,
                               XmNleftWidget,     _submit_action->W(),
                               XmNleftOffset,     10,
                               XmNtopAttachment,  XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,      _submit_action->W(),
                               NULL );

   XtVaSetValues( _copies_text->W(), XmNleftAttachment, XmATTACH_WIDGET,
                               XmNleftWidget,     _submit_action->W(),
                               XmNleftOffset,     10,
                               XmNtopAttachment,  XmATTACH_WIDGET,
                               XmNtopWidget,      _queue->W(),
                               NULL );


   if (_scale_input) {
        XtVaSetValues( _scale_input->W(), 
                             XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                             XmNleftWidget,     _queue->W(),
                             XmNtopAttachment,  XmATTACH_WIDGET,
                             XmNtopWidget,      _copies_text->W(),
                             XmNtopOffset,      10,
                             NULL);
   } // end if

   Widget tmp_attach= _movie_style ? _movie_style->W() : _submit_action->W();
   
   Widget tmp=  XtVaCreateManagedWidget("", xmLabelWidgetClass, topWidget(),
                                XmNtopAttachment,    XmATTACH_WIDGET,
                                XmNtopWidget,        tmp_attach,
                                XmNbottomAttachment, XmATTACH_WIDGET,
                                XmNbottomWidget,     bottomSeparator(),
                                XmNleftAttachment,   XmATTACH_FORM,
                                XmNleftOffset,       5,
                                XmNtopOffset,        5,
                                XmNbottomOffset,     25,
                                NULL);
   Widget tmp2= XtVaCreateManagedWidget("", xmLabelWidgetClass, topWidget(),
                                XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                                XmNtopWidget,        _queue->W(),
                                XmNrightAttachment,   XmATTACH_FORM,
                                XmNleftAttachment,   XmATTACH_WIDGET,
                                XmNleftWidget,       _queue->W(),
                                XmNrightOffset,       15,
                                XmNleftOffset,        1,
                                NULL);

   return topWidget();

}

void HardCopyPop::setTotalDim()
{
  float tot_width, tot_height;
  float width, height;

  width  = _width;
  height = _height; 

  if(_sp->units() != PlotEnglish)
    {
    width  *= .3937F;
    height *= .3937F;
    } 
    
  if (_sp->movie() && _movie_style && 
      _movie_style->WhichSelected() == PANEL ) {
     _sp->computePaneledHardWidthHeight(width,      height, 
                                        &tot_width, &tot_height,
                                        _extra_input->IsSelected(CBAR_BOTH) );
  }
  else {
     _sp->computeHardWidthHeight(width, height, &tot_width, &tot_height,
                              _extra_input->IsSelected(CBAR_BOTH) );
  }

  if(_sp->units() != PlotEnglish)
    {
    tot_width  /= .3937F;
    tot_height /= .3937F;
    if (_total_dim)
         wprocVAShowMsg(_total_dim, 
                     "Total Dimensions: %3.1f CM long by %3.1f CM high", 
                                  tot_width, tot_height);
    }
  else
    {
    if (_total_dim)
         wprocVAShowMsg(_total_dim, 
                "Total Dimensions: %3.1f Inches long by %3.1f Inches high", 
                                  tot_width, tot_height);
    }
}

void HardCopyPop::managing()
{
  Boolean stat= True;
  int istat;
  char filename[400];


  computeWidthHeight( &_def_width, &_def_height);
  assert (_sp); // Geopress may manage w/o a SeisPlot further work needed
  setTotalDim();
  if (_sp->isPlotDisplayed()) {
        if (_first_time_man)  {
             _plot_dim->SetRange(WIDTH, SLTextBox::same, 
                                  SLTextBox::same, _def_width);
             _plot_dim->SetRange(HEIGHT, SLTextBox::same, 
                                  SLTextBox::same, _def_width);
             _plot_dim->SetValue(WIDTH, _def_width);
             _plot_dim->SetValue(HEIGHT, _def_height);
             _first_time_man= False;
             setTotalDim();
        }
  }
  else {
       ErrorHandler err= W();
       err.deliverError(
             "You may not do a hardcopy until you have a plot displayed.");
       stat= False;
  }
  stat= scriptExist(True);
  if (stat) {
       strcpy(filename, _sp->filename());
       if ( (strlen(filename) == 0) || (filename[0] == ' ') ) {
             exp_file(filename,"plotfile.cgm");
       }
       else {
             addext_rep_(filename, "cgm", &istat);
             assert(istat == 0);
       }
       _slp_file->setFilename (filename);
  }


  if(_movie_style && made()) 
    XtSetSensitive(_movie_style->W(), (_sp->plottedFrames() > 1) );

}




void HardCopyPop::computeWidthHeight( float *width, float *height)
{
  Display *dpy= XtDisplay(W());
  int screen_num= XScreenNumberOfScreen( XtScreen(W()) );

  *width=  _sp->plottedWidth() / 
                 _sp->getHorizontalPixelsPerInch(dpy, screen_num);
  *height= _sp->plottedHeight() / 
                 _sp->getVerticalPixelsPerInch(dpy, screen_num);

}



Boolean HardCopyPop::ValidInput()
{
  Boolean stat = True;
  ErrorHandler err= W();
  if (!_sp->isPlotDisplayed()) {
       err.deliverError(
             "You may not do a hardcopy until you have a plot displayed.");
       stat= False;
  }
  //if (_sp->movie()) {
  //     err.deliverError(
  //         "You currently may not make a hardcopy from a movie loop... yet.");
  //     stat= False;
  //}
  if (stat) stat = SLFormFCPop::ValidInput ();
  if (stat) stat= scriptExist(False);
  if (stat) notifyComplex(_plot_dim, WIDTH);
  return stat;
}

void HardCopyPop::DoAction()
{
  ShellWatch sw;
  PickWatch pw;
  char submit_script[500];
  char keep_or_del[40];
  float tot_width, tot_height;
  int submit_stat;
  float width, height;

  width  = _width;
  height = _height; 

  if(_sp->units() != PlotEnglish)
    {
    width  *= .3937F;
    height *= .3937F;
    }

  if (_sp->movie() && _movie_style && 
      _movie_style->WhichSelected() == PANEL ) {
     _sp->computePaneledHardWidthHeight(width,      height, 
                                        &tot_width, &tot_height,
                                        _extra_input->IsSelected(CBAR_BOTH) );
  }
  else {
     _sp->computeHardWidthHeight(width, height, &tot_width, &tot_height,
                                 _extra_input->IsSelected(CBAR_BOTH) );
  }
  _do_anno= _extra_input->IsSelected(ANNO);


  /*
   * create CGM file
   */
  SeisHardCopy shc(_slp_file->filename()); 
  shc.setWidthHeight(tot_width, tot_height);
  shc.setMarkerScaleFactor(_marker_scale_factor);
  shc.setDoAnnotation(_do_anno);
  if (_using_pip) shc.setUsePipExtension( _extra_input->IsSelected(USE_PIP) );
  shc.setColorBarBothSides( _extra_input->IsSelected(CBAR_BOTH) );
  if (_movie_style) 
           shc.setPanelMovie( _movie_style->WhichSelected() == PANEL );
  shc.setFrame( (int)_sp->currentFrame() + 1 );
  _sp->writeHardCopy(&shc);

  /*
   * submit file
   */
  if ((_submit_action->WhichSelected() != NOSUBMIT)  &&  made()) {// plot it
        ShellStatMsg sub_info(W(),"Submitting CGM file to plotter...");
        char *select_queue= _queue->whichSelectedString();
        if (_submit_action->WhichSelected() == SUBMIT_DEL)
                   strcpy(keep_or_del, "-delete");
        else
                   strcpy(keep_or_del, "-keep");
        sprintf(submit_script, "%s -queue %s %s %s %d",
                SCRIPT_NAME, select_queue, keep_or_del, _slp_file->filename(),
		_num_copies);
        submit_stat= wpSystem(submit_script);
        if (!submit_stat) {
               ErrorHandler eh(topWidget());
               eh.deliverError("Plot could not be submitted.");
        }
        printf("queue selected: %s\n", select_queue);
        free(select_queue);
  } // end submit the file
}


Boolean HardCopyPop::notifyComplex(SLDelay *obj, int ident)
{
  if (obj==_plot_dim                           || 
     (obj==_extra_input && ident == SYM)       ||
     (obj==_extra_input && ident == CBAR_BOTH) ||
      obj==_movie_style )
    {
         float height;
         long ptype= _sp->plotType();
         if (_using_symetrical) 
                     _make_symetrical= _extra_input->IsSelected(SYM);

         if (  _make_symetrical       &&  // a very complex 'if' that makes
               _sp->isPlotDisplayed() &&  // sure everything is OK for 
              ( (ident == WIDTH)      ||  // computing a height from the width 
                (ident == SYM) )      && 
              ( ( ptype == PlotImage::PlotGRID)    ||
                ( ptype == PlotImage::PlotHEADER)  || 
                ( ptype == PlotImage::PlotARRAY) ) ) {
             height= _sp->getSymetricalSize(False, 
                                            _sp->gridX1(), _sp->gridX2(),
                                            _sp->gridY1(), _sp->gridY2(),
                                            &_width );
             _plot_dim->SetValue(HEIGHT,height);
         } // end complex if
         
    }
  else if(obj == _submit_action)
    {
    XtSetSensitive(_copies_text->W(), 
                     (_submit_action->WhichSelected() != NOSUBMIT) );
    XtSetSensitive(_queue->W(), 
                     (_submit_action->WhichSelected() != NOSUBMIT) );  
    }

  if (_sp) setTotalDim();

  return True;
}

Boolean HardCopyPop::scriptExist(Boolean always_show_error)
{
  int stat= wpAccess(SCRIPT_NAME);
  Boolean retval= True;

  if (stat != 0) {
         ErrorHandler err= W();
         char outmsg[300];
         retval= False;
         sprintf( outmsg, "%s%s%s%s",
                     "The submission script ", SCRIPT_NAME,
                     " is not accessible -\n", 
                     "You may write CGM files but not submit them.");
         if (_submit_action->WhichSelected() == NOSUBMIT) { 
              if (always_show_error) {
                       err.deliverError(outmsg);
              } // end if
              else {
                       retval= True;
              } // end else
         }
         else {
              err.deliverError(outmsg);
         }
         _submit_action->SetRadio(NOSUBMIT);
         if (_submit_action->made()) {
            XtSetSensitive(_submit_action->GetRadioWidget(SUBMIT), False);
            XtSetSensitive(_submit_action->GetRadioWidget(SUBMIT_DEL), False);
         } // end if
  } // end if
  else {
     if (_submit_action->made()) {
        XtSetSensitive(_submit_action->GetRadioWidget(SUBMIT), True);
        XtSetSensitive(_submit_action->GetRadioWidget(SUBMIT_DEL), True);
     } // end if
  } // end else
  return retval;

}

void HardCopyPop::setSeisPlot( SeisPlot *sp)
{
  _sp= sp;
  if (W()) 
    {
    if (XtIsManaged(W())) managing();
    if(_movie_style && made()) XtSetSensitive(_movie_style->W(), 
                                     (_sp->plottedFrames() > 1) );
    }
}

void HardCopyPop::notCurrentInWindow(SeisPlot *sp)
{
   if (_sp == sp) {
       _sp= _sp->currentSPInWindow();
       if(_movie_style && made()) XtSetSensitive(_movie_style->W(), 
                                       (_sp->plottedFrames() > 1) );
       if (!find(_sp)) {
           addSeisPlot(_sp);
       }
       else if (made() && (XtIsManaged(W()))) {
           managing();
       }
   }
}

void HardCopyPop::newPlot(SeisPlot *)
{
  if (made() && (XtIsManaged(W()))) {
        managing();
  }

  if(made() && _movie_style)
    XtSetSensitive(_movie_style->W(), (_sp->plottedFrames() > 1) );
}
