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
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include "sl/slp_file.hh"
#include "sp/seis_select.hh"
#include "sp/seis_anno_pop.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_winman.hh"
#include "sp/trace_selector_pop.hh"
#include "sp/trace_selector_ndo_pop.hh"
#include "sl/psuedo_widget.hh"
#include "sl/shell_watch.hh"
#include "sl/error_handler.hh"
#include "sl/sl_scale.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/slp_option.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_error_pop.hh"
#include "net/net_env.hh"
#include "trace_selector.hh"
#include "trciof77.h"
#include "workstation.h"

#define IDENT 0
#define LABEL "Filename..."
#define FILETYPE "Seismic File"
#define EXTENSION "*"


static String  defres[]= {
     "*doselector.labelString:   Use Trace Selector -->",
     "*selectorbutton.labelString:  Selector...",
     "*ntotL.labelString:        Nplt:",
     "*iskpL.labelString:        Initial Skip:",
     "*ndoL.labelString:         Ndo:",
     "*nskpL.labelString:        Nskp:",
     "*tminL.labelString:        Tmin:",
     "*tmaxL.labelString:        Tmax:",
     "*tdecL.labelString:        Tdec:",
     "*trp.labelString:          Reverse Polarity",
     "*rd.labelString:          Plot Right to Left",
     "*invert.labelString:      Invert Y Axis",
     "*gs.labelString:          Variable Density",
     "*wonly.labelString:       Wigl Only",
     "*norm.labelString:        Normalize",
     "*ext.labelString:         Scale to Amp",
     "*stf.labelString:         Scale to File",
     "*stp.labelString:         Scale to Panel",
     "*wfill.labelString:       Variable Area Pos",
     "*wfilln.labelString:      Variable Area Neg",
     "*ampL.labelString:        Amplitude:",
     "*ctL.labelString:         Ct:",
     "*infile.labelString:      Filename...",
     "*infile.fileDescription:  Seismic file",
     "*infile.fileExtension:    *",
     "*infile.fileFlags:        MustExist IsRequired",
     "*infile.annoType:         pushbutton",
     "*infile*Fileshell*dirMask: *.*",
     "*XmText.columns:           8",
     "*action_push.packing:      PACK_COLUMN",
     "*anno.labelString:         Annotation...",
     "*vd.labelString:           Color Options...",
     "*underlay.labelString:     Underlay...",
     "*maxim.labelString:        Maximize",
     "*sellab.labelString:       Data Selection Parameters",
     "*dislab.labelString:       Data Display Parameters",
     "*dislab.fontList:          -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
     "*sellab.fontList:          -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
     "*data_sel1.leftAttachment: ATTACH_POSITION",
     "*data_sel1.leftPosition:   10",
     "*traceparams.leftOffset:   20",
     "*ptype.topOffset:          30",
     "*XmText.marginHeight:      1",
     "*XmText.marginWidth:       1",
     "*ntot.value:               100",
     "*iskp.value:               0",
     "*ndo.value:                1",
     "*nskp.value:               0",
     "*tmin.value:               0.00",
     "*tmax.value:               1.00",
     "*tdec.value:               1",
     "*ti.value:                 20.00",
     "*is.value:                 2.00",
     "*ct.value:                 4.00",
     "*wfill.set:                True",
     "*stp.set:                  True",
     "*sp-port:                  6090",
     //"*sp-host:                  poepsn03.po.dupont.com",
    NULL};


#define INFILE_NAME  "infile"

static SLRadio norm_rads[]  = {
         { "stp",  PlotImage::PANELNORM },
         { "norm", PlotImage::NORM },
         { "stf",  PlotImage::FILENORM },
         { "ext",  PlotImage::EXTERNALNORM },
       };

static SLRadio rads[]  = {
         { "gs",    PlotImage::PlotGS },
         { "wonly", PlotImage::PlotWONLY },
         { "wfill", PlotImage::PlotWFILL },
         { "wfilln",SeisSelect::NEGATIVE_FILL },
       };

static SLPush buttons[]  = {
 { "anno",    SeisSelect::ANNO },
 { "maxim",   SeisSelect::MAXIM },
 { "vd",      SeisSelect::VD },
};



/*
static void host_ops_trap (void *data, long ident, long oldvar,
  long newvar)
{
  SeisSelect *seis_select = (SeisSelect *)data;
  int ident = (int)newvar;
  seis_select->getDataFrom (ident);
}


static long host_ops_upfun (void *data)
{
  SeisSelect *seis_select = (SeisSelect *)data;
  if (seis_select->netConnectOk ()) {
    return (long)SeisSelect::CRAY_SP;
  }
  else {
    return (long)SeisSelect::LOCAL;
 }
}
*/

SeisSelect::SeisSelect( Widget    p,
                        char      *name,
                        HelpCtx   hctx,
                        SeisPlot  *sp,
                        SeisPlot  *info_sp,
                        Boolean   allow_selector)
              : SLFPopSep(p,name,FP_DOALL,hctx,False,False), 
                SeisInform(sp), _sp(sp), 
                _info_sp(info_sp), _first_time(True), _docolor(True),
                _color_pop(NULL), _first_sp(sp), _infile(NULL),
                _infile_valid(False), _infile_failstr(NULL),
                _external_amp(1.0), _netenv(NULL),
                _allow_selector(allow_selector), _sav_frames(0), 
                _nplt(100),_iskp(0),_ndo(1),_nskp(0), 
                _previous_initialized(False), _frames(0), _domovie(0),
                _selector_button(0), _selector_ndo_pop(NULL),
                _selector_pop(NULL)
{
_alt_access.objptr=NULL;

static SLTog stog[]  = {
    { "doselector", NULL,   DOSELECTOR },
};
stog[0].target=&_doselector;

static SLPush selectorbutton[]  = {
 { "selectorbutton",    DOSELECTOR },
};


static SLText Select_text1[]  = {
 { "ntot",   NULL,  NULL,  SLType_int,   NTOT },
 { "iskp",   NULL,  NULL,  SLType_int,   ISKP },
 { "ndo",    NULL,  NULL,  SLType_int,   NDO },
 { "nskp",   NULL,  NULL,  SLType_int,   NSKP },
};
Select_text1[0].target=&_nplt; 
Select_text1[1].target=&_iskp; 
//Select_text1[2].target=&_ndo; 
//Select_text1[3].target=&_nskp; 

static SLText Select_text2[]  = {
 { "tmin",   NULL,  NULL,  SLType_float, TMIN },
 { "tmax",   NULL,  NULL,  SLType_float, TMAX },
 { "tdec",   NULL,  NULL,  SLType_int,   TDEC },
       };
Select_text2[0].target=&_tmin; 
Select_text2[1].target=&_tmax; 
Select_text2[2].target=&_tdec; 

static SLText Traceparams_text[]  = {
 { "ti",   "range:0.0 *,default:20.00000",  NULL,  SLType_float,   TI },
 { "is",   "range:0.0 *,default:2.00000",   NULL,  SLType_float,   IS },
 { "ct",   NULL,  NULL,  SLType_float,   CT },
       };
Traceparams_text[0].target=&_ti; 
Traceparams_text[1].target=&_is; 
Traceparams_text[2].target=&_ct; 

static SLTog psettings_togs[]  = {
    { "trp",    NULL,     RP },
    { "rd",     NULL,     RD },
    { "invert", NULL, INVERT },
       };
psettings_togs[0].target=&_rp; 
psettings_togs[1].target=&_rd; 
psettings_togs[2].target=&_invert; 

static SLText amp_text[]  = {
 { "amp",   "range:-9.99999e+30 9.99999e+30,default:1.00000",  NULL,
    SLType_gfloat,   PlotImage::EXTERNALNORM },
};
amp_text[0].target = &_external_amp;








  _infile = new SLpFile (this, "infile", IDENT, LABEL, FILETYPE, EXTENSION);
  _infile->setCtrap (fileCallback, this);

  char *tfile=  _pw_topw->childFileChoiceDef (INFILE_NAME);
  if (tfile) {
    strcpy (_bytfile, tfile);
    _infile->setFilename (_bytfile);
  }
  else {
    _bytfile[0]= '\0';
  }


  setDefaultResources( p, name, defres);

  /*
  _host_ops = new SLpOption (this, "host_ops", 0, "Get Data From: ");
  _host_ops->addOption ("Local"    , SeisSelect::LOCAL  );
  _host_ops->addOption ("Cray - sp", SeisSelect::CRAY_SP);

  _host_ops->setItrap     (host_ops_trap , this);
  _host_ops->setupIvarFun (host_ops_upfun, this);
  */

  if(_allow_selector)
    {
    _selector_tog= new SLTogBox(this, "selectortog", getHelpCtx(), 
                           stog,XtNumber(stog));
    _selector_tog->setAltChoiceAction(selectorChanged, (void*)this);
    _selector_button = new SLPushBox(this, "selectorbutton", getHelpCtx(),
                                selectorbutton, XtNumber(selectorbutton));
    _selector_button->setComplexNotify(this);
    }

  _norm_form= new SLSmartForm(this, "norm_form", getHelpCtx(), True); 
  _norm_type= new SLRadioBox( _norm_form, "norm_type", getHelpCtx(),
                              norm_rads, XtNumber(norm_rads), NULL);
  _ext_amp= new SLTextBox( _norm_form,"_ext_amp",getHelpCtx(),
                           amp_text,XtNumber(amp_text), False);
  _ext_amp->SetRange (PlotImage::EXTERNALNORM, SLTextBox::same,
    SLTextBox::same, SLTextBox::same, 6);
  _ext_amp->setWidth (120);

  _ptype= new SLRadioBox( this, "ptype", getHelpCtx(),
                          rads, XtNumber(rads), NULL, True );

  _psettings= new SLTogBox( this, "psettings", getHelpCtx(),
                            psettings_togs, XtNumber(psettings_togs), True);

  _data_sel1= new SLTextBox( this,"data_sel1",getHelpCtx(),
                           Select_text1,XtNumber(Select_text1), True);

  _data_sel2= new SLTextBox( this,"data_sel2",getHelpCtx(),
                           Select_text2,XtNumber(Select_text2), True);

  _traceparams= new SLTextBox( this,"traceparams",getHelpCtx(),
                         Traceparams_text,XtNumber(Traceparams_text), True );
  _traceparams->setComplexNotify(this);

  _annopop= new SeisAnnoPop(this, "anno", getHelpCtx(), sp, info_sp);


//  _mform= new SLForm(this, "mform", NULL, True);
//  _extra_sp= new SLPushBox(_mform, "extra_sp", NULL, sp_buttons,
//                           XtNumber(sp_buttons));
//  _extra_sp->setComplexNotify(this);
//  _which_sp= new SLScale(_mform, "which_sp", NULL);
//  _which_sp->setComplexNotify(this);


  _but = new SLPushBox(this, "action_push", getHelpCtx(),
                                buttons, XtNumber(buttons));
  _but->setComplexNotify(this);
  _norm_type->setComplexNotify(this);
  _ext_amp->setComplexNotify(this);
}


/*
Boolean SeisSelect::netConnectOk ()
{
  if (_netenv == NULL) {
    return False;
  }
  else if (_netenv->networkStatus() == NetEnv::Good) {
    return True;
  }
  else {
    return False;
  }
}
*/

Widget SeisSelect::make(Widget p)
{
   if ( made() ) return topWidget();
   SLFPopSep::make(p);
   p= wParent();


   _sellab= XtVaCreateManagedWidget( "sellab", xmLabelWidgetClass, topWidget(),
                                     XmNtopAttachment,   XmATTACH_FORM,
                                     XmNtopOffset,       15,
                                     XmNleftAttachment,  XmATTACH_FORM,
                                     XmNleftOffset,      5,
                                     XmNrightAttachment, XmATTACH_FORM, 
                                     XmNrightOffset,     5,
                                     NULL);

   XtVaSetValues (_infile->W(),
                            XmNtopAttachment,   XmATTACH_WIDGET,
                            XmNtopWidget,       _sellab,
                            XmNtopOffset,       15,
                            XmNleftAttachment,  XmATTACH_FORM,
                            XmNleftOffset,      20,
                            XmNrightAttachment, XmATTACH_FORM,
                            XmNrightOffset,     20,
                            XmNfileTargetAddr,  _bytfile,  NULL );




  if (strlen(_sp->filename()) > 0) {
    // if possible set the FileChoice filename
    _infile->setFilename (_sp->filename());
  }
  else if (_infile->filename() && strlen(_infile->filename()) > 0 &&
    strcmp(_infile->filename(),"NONE")) {
    // if possible set the SeisPlot filename
    _sp->setFilename (_infile->filename());
    _infile_valid = True;
  }



   

   

   XtVaSetValues( _traceparams->W(), XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,     _psettings->W(),
                               XmNleftAttachment, XmATTACH_WIDGET,
                               XmNleftWidget,    _psettings->W(), NULL);

   Widget tmp1=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                               XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,     _data_sel2->W(),
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNrightOffset,       5,
                               XmNleftAttachment, XmATTACH_WIDGET,
                               XmNleftWidget,     _data_sel2->W(),
                               XmNleftOffset,       10,
                               NULL);
   wprocShowMsg(tmp1,"");


   Widget tmp2=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                               XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,     _traceparams->W(),
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNrightOffset,       5,
                               XmNleftAttachment, XmATTACH_WIDGET,
                               XmNleftWidget,     _traceparams->W(),
                               XmNleftOffset,       10,
                               NULL);
   wprocShowMsg(tmp2,"");


   /*
   XtVaSetValues( _host_ops->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                                  XmNtopWidget,      _infile->W(),
                                  XmNtopOffset,       20,
                                  XmNleftAttachment,  XmATTACH_FORM,
                                  XmNleftOffset,      30,
                                  NULL);
   */

   XtVaSetValues( _data_sel1->W(), XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopWidget,     _infile->W(),
                                   XmNtopOffset,     10, 
                                   NULL);   



   if(_allow_selector)
     {

     XtVaSetValues( _selector_tog->W(), XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopWidget,     _data_sel1->W(),
                                   XmNtopOffset,     10, 
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNleftOffset,    45,
                                   NULL);

     XtVaSetValues( _selector_button->W(), XmNtopAttachment, XmATTACH_WIDGET,
                                   XmNtopWidget,      _data_sel1->W(),
                                   XmNtopOffset,      7, 
                                   XmNleftAttachment, XmATTACH_WIDGET,
                                   XmNleftWidget,     _selector_tog->W(),
                                   XmNleftOffset,     20,
                                   NULL);   

     XtSetSensitive(_selector_tog->W(), False);
     XtSetSensitive(_selector_button->W(), False);

     _information= XtVaCreateManagedWidget( "information", xmLabelWidgetClass, 
                                           topWidget(),
                                           XmNtopAttachment, XmATTACH_WIDGET,
                                           XmNtopWidget, _selector_button->W(),
                                           XmNtopOffset,       10,
                                           XmNleftAttachment,  XmATTACH_FORM,
                                           XmNleftOffset,      5,
                                           XmNrightAttachment, XmATTACH_FORM, 
                                           XmNrightOffset,     5,
                                           NULL);
   
     }
   else
     {
     _information= XtVaCreateManagedWidget( "information", xmLabelWidgetClass, 
                                           topWidget(),
                                           XmNtopAttachment, XmATTACH_WIDGET,
                                           XmNtopWidget,     _data_sel1->W(),
                                           XmNtopOffset,       10,
                                           XmNleftAttachment,  XmATTACH_FORM,
                                           XmNleftOffset,      5,
                                           XmNrightAttachment, XmATTACH_FORM, 
                                           XmNrightOffset,     5,
                                           NULL);
     }


  wprocShowMsg(_information,"   ");

  Widget sep= make_attached_sep(topWidget(), "sep");
  XtVaSetValues( sep, XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget,     _information, 
                       XmNtopOffset,     15, NULL);


  _dislab= XtVaCreateManagedWidget( "dislab", xmLabelWidgetClass, 
                                            topWidget(),
                                           XmNtopAttachment,   XmATTACH_WIDGET,
                                           XmNtopWidget,       sep,
                                           XmNtopOffset,       15,
                                           XmNleftAttachment,  XmATTACH_FORM,
                                           XmNleftOffset,      5,
                                           XmNrightAttachment, XmATTACH_FORM, 
                                           XmNrightOffset,     5,
                                           NULL);


   XtVaSetValues( _psettings->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                                   XmNtopOffset,      15,
                                   XmNleftAttachment, XmATTACH_FORM,
                                   XmNleftOffset,     20,
                                   XmNtopWidget,      _dislab, NULL);

   

   XtVaSetValues( _data_sel2->W(), XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                                   XmNtopWidget,     _data_sel1->W(),
                                   XmNleftAttachment, XmATTACH_WIDGET,
                                   XmNleftOffset,     20,
                                   XmNrightOffset,    20,
                                   XmNleftWidget,    _data_sel1->W(), NULL);

   XtVaSetValues( _ptype->W(), XmNtopAttachment, XmATTACH_WIDGET,
                               XmNtopWidget,     _psettings->W(),
                               XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,    _psettings->W(), 
                               XmNtopOffset,     15, NULL);
   _ptype->setComplexNotify(this);

   XtVaSetValues( _norm_form->W(), XmNtopAttachment,     XmATTACH_WIDGET,
                                   XmNtopWidget,        _ptype->W(),
                                   XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                                   XmNleftWidget,        _ptype->W(), 
                                   XmNtopOffset,     15, NULL);

   XtVaSetValues( _norm_type->W(), XmNtopAttachment,   XmATTACH_FORM,
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   NULL);

   XtVaSetValues( _ext_amp->W(),   XmNtopAttachment,   XmATTACH_WIDGET,
                                   XmNtopWidget,       _norm_type->W(),
                                   XmNleftAttachment,  XmATTACH_FORM,
                                   XmNrightAttachment, XmATTACH_FORM,
                                   NULL);

   _bottom_tmp= XtVaCreateManagedWidget( "", 
                                         xmLabelWidgetClass, topWidget(),
                                         XmNtopAttachment, XmATTACH_WIDGET,
                                         XmNtopWidget,     _norm_form->W(),
                                         XmNbottomAttachment, XmATTACH_WIDGET,
                                         XmNbottomWidget,    bottomSeparator(),
                                         XmNbottomOffset,    20,
                                         XmNleftAttachment,  XmATTACH_FORM,
                                         XmNleftOffset,       5,
                                         XmNtopOffset,        5,
                                         NULL);

   wprocShowMsg(_bottom_tmp,"");


   XtVaSetValues( _but->W(), XmNtopAttachment,  XmATTACH_WIDGET,
                             XmNtopWidget,      _traceparams->W(),
                             XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                             XmNleftWidget,     _traceparams->W(), 
                             XmNtopOffset,      4,
                             XmNnumColumns,     2, NULL);



   _traceparams->SetRange ((int)TI, (double)0.00001, (double)99999999.0,
     (double)20.0, (long)5);

   // call set range just go for 3 decimal places
   _data_sel2->SetRange ((int)TMIN, (double)0.0, (double)100.0, (double)1.0,
     (long)3);
   _data_sel2->SetRange ((int)TMAX, (double)0.0, (double)100.0, (double)1.0,
     (long)3);

   setColorPop(_color_pop);
   if (_docolor) colorOpEnabled( _sp->colorCapable() ); 
   else          colorOpEnabled(False);
   setUnits();
   setFromOther(_info_sp);
   ValidInput();

   

   return topWidget();
}


SeisSelect::~SeisSelect()
{
   delete _psettings;
   delete _ptype;
   delete _ext_amp;
   delete _norm_type;
   delete _norm_form;
   delete _data_sel1;
   delete _data_sel2;
   delete _traceparams;
   delete _but;
   delete _infile;
}



void SeisSelect::selectorChanged(void *data, long, Boolean set)
{
   SeisSelect *obj= (SeisSelect*)data;
   if (set) { 
     XtSetSensitive(obj->_selector_button->W(), True);
   }
   else {
     XtSetSensitive(obj->_selector_button->W(), False);
   }
}


void SeisSelect::setUnits()
{
  if (made()) {
     if (_sp->units() == PlotMetric) {
        wprocShowMsg( _traceparams->LabW(TI), "Trace / cm:" );
        wprocShowMsg( _traceparams->LabW(IS), "cm / Sec:" );
   
     }
     else {
        wprocShowMsg( _traceparams->LabW(TI), "Trace / Inch:" );
        wprocShowMsg( _traceparams->LabW(IS), "Inches / Sec:" );
     }
  }

}

void SeisSelect::unitChange(SeisPlot*, int)
{
  setUnits();
}


void SeisSelect::pushAction(long ident)
{

  switch (ident) {
       case MAXIM:
             _sp->setTmin( _tmin );
             _sp->setTmax( _tmax );
             _sp->setNPlt( _nplt );
             _sp->maximizeScale( (int)_sp->plotType() );
             _data_sel1->SetValue(NTOT, _sp->nplt() );
             _data_sel2->SetValue(TMAX, _sp->tmax() );
             _traceparams->SetValue(TI, _sp->ti() );
             _traceparams->SetValue(IS, _sp->is() );
             break;

       case ANNO:
             //Make sure annotation popup has correct SeisPlot
             _annopop->setSeisPlot(_sp);
             _annopop->makeAndManage(XtParent(W()));
                break;
       case VD:
             _color_pop->dontPlotYet(True);
             _color_pop->makeAndManage();
             _ptype->SetRadio(PlotImage::PlotGS);
             //XtSetSensitive(_norm_type->W(), True);
                break;
       case UNDERLAY:
                break;
  }

}


Boolean SeisSelect::notifyComplex(SLDelay *obj, int ident)
{

  if (obj == _ptype) {
     
       int which= _ptype->WhichSelected();
       if (which == PlotImage::PlotCOLOR) {
             //XtSetSensitive(_norm_type->W(), False);
             if (_sp->canScaleToFile()) 
                    _norm_type->SetRadio(PlotImage::FILENORM);
             else
                    _norm_type->SetRadio(PlotImage::PANELNORM);
       } // end if
       else {
              _traceparams->SetRange( TI, (float)SLTextBox::same, 
                               (float)_sp->maxWigglesPerInch() );
             //XtSetSensitive(_norm_type->W(), True);
       } // end else
        
  }
  else if (obj == _but) {
          pushAction(ident);
  }
  else if (obj == _norm_type) {
       if (ident ==  PlotImage::EXTERNALNORM) {
               _ext_amp->SetValue(PlotImage::EXTERNALNORM, 
                                  (float)_external_amp );
               setToExtAmp(True);
       }
       else {
               setToExtAmp(False);
       }
  }
  else if (obj == _ext_amp) {
      setToExtAmp(True);
  }
  else if (obj == _data_sel1) {
      if(checkForPatternChange())
        {
        turnOffSelector();
        checkTracePattern();
        }
  }
  else if (obj == _selector_button) {
    //  _selector_pop->manage();
    _selector_ndo_pop->manage ();
  }  
  else if (obj == _traceparams){
      //checkTracesPerInch();
  }

  return True;
}


Boolean SeisSelect::checkForPatternChange()
{
Boolean changed = False;

  if(_previous_initialized)
    {
    if(_previous_nplt != _data_sel1->GetInt(NTOT) ||
       _previous_iskp != _data_sel1->GetInt(ISKP) ||
       _previous_ndo  != _data_sel1->GetInt(NDO)  ||
       _previous_nskp != _data_sel1->GetInt(NSKP)    )
      changed = True;
    }
  else//This else added for Geopress since it goes thru a different
    { //path and the checkTracePattern would not get called when needed.
      //See notifyComplex method. MLS 05/02
    changed = True;
    }

  _previous_nplt = _data_sel1->GetInt(NTOT);
  _previous_iskp = _data_sel1->GetInt(ISKP);
  _previous_ndo  = _data_sel1->GetInt(NDO);
  _previous_nskp = _data_sel1->GetInt(NSKP);

  _previous_initialized = True;

  return changed;
}

void SeisSelect::checkTracePattern()
{
SLErrorPop *error_pop = NULL;
long ndo              = _data_sel1->GetInt(NDO);
long nskp             = _data_sel1->GetInt(NSKP);
long iskp             = _data_sel1->GetInt(ISKP);
long old_nplt         = _data_sel1->GetInt(NTOT);
long traces_in_file   = _sp->totalTraces();
int new_nplt;

  //If user has requested more than what is in file we dont
  //need to do anything because that case will be handled elsewhere
  if( _data_sel1->GetInt(NTOT) > traces_in_file)
    return;

  //If the user is doing more than one trace at a time in his pattern
  //make sure that his number to skip is set also
  if(ndo > 1 && nskp < 1) 
    {
    nskp = _nskp = 1;
    _data_sel1->SetValue(NSKP,_nskp);
    }

  int pattern_len = ndo + nskp;

  new_nplt = (traces_in_file - iskp) /  pattern_len * ndo;

  int left_over = (traces_in_file - iskp) % pattern_len;

  if(left_over > ndo)
    new_nplt += ndo;
  else
    new_nplt += left_over;

  if(new_nplt < 1) 
    {
    _data_sel1->SetValue(NTOT,traces_in_file);
    _data_sel1->SetValue(NDO, 1L);
    _data_sel1->SetValue(NSKP,0L);
    _data_sel1->SetValue(ISKP,0L);
    error_pop = new SLErrorPop(topWidget(),"Trace Pattern Error",
          "Sorry the trace pattern requested will not plot\n\
           any traces... please start over");
    return;
    }

  if(new_nplt < old_nplt)
    {
    error_pop = new SLErrorPop(topWidget(),"Trace Pattern Warning",
                               "Total traces to plot reset");
    _nplt = new_nplt;
    _data_sel1->SetValue(NTOT,_nplt);
    }

}



void SeisSelect::setToExtAmp(Boolean set)
{
   if (set) {
         _norm_type->SetRadio(PlotImage::EXTERNALNORM);
   }
   else {
         _ext_amp->clear(PlotImage::EXTERNALNORM);
   }
}

void SeisSelect::fileCallback (void *data, long ident, char *oldvar,
  char *newvar)
{
  SeisSelect *obj = (SeisSelect *)data;
  obj->filein (ident, oldvar, newvar);
}

#define BADFILE " %s is not a seismic file."
#define OLDFILE " %s has not been modified."
#define NEWFILE 1
#define UNCHANGEDFILE -1
#define BLNKFILE 0

void SeisSelect::filein (long /*ident*/, char *oldvar, char *newvar)
{
  static char wkstr[200];

  int file_stat, error;
  time_t time_stamp;
  if (newvar && strlen(newvar)) {
    time_stamp = _infile->timeStamp (newvar);
    file_stat = NEWFILE;
  }
  else {
    file_stat = BLNKFILE;
  }
  if (file_stat == NEWFILE) {
    if (oldvar) {
      if (!strcmp(oldvar,newvar) && !fileModified(time_stamp)) {
	file_stat = UNCHANGEDFILE;
      }
    }
  }

  if (file_stat == UNCHANGEDFILE) {
    Widget tw = _data_sel1->TxtW (NTOT);
    XmProcessTraversal (tw, XmTRAVERSE_CURRENT);
    sprintf (wkstr, OLDFILE, newvar);
    _infile_failstr = wkstr;
    _infile_valid   = False;
  }
  else if (file_stat == NEWFILE) {
    //workstation_force_jseis_close (newvar);
    if (_sp->setFilename(newvar)) {
      _infile->setFilename (newvar);
      if (_allow_selector) {
	if (!_selector_ndo_pop->clearOrSetDefaults(_sp)) {
	  turnOffSelector (); //Because the input file couldn't be set
	  XtSetSensitive (_selector_tog->W(), True);
	}
	else {
	  useSelector (_selector_ndo_pop->selector(),
            _selector_ndo_pop->results());
	}
      }

      //Force rechecking of trace patterns
      _previous_nplt = -1;

      wprocVAShowMsg (_information, 
        "Total Nplt: %d,    Tmin: %4.3f,   Tmax: %4.3f   Srval: %4.4f",
	_sp->totalTraces(), _sp->minTmin(), _sp->maxTmax(), _sp->srval());
   
      _data_sel1->SetRange (NTOT, 1, _sp->totalTraces(), 
        (_sp->totalTraces() < 100) ? _sp->totalTraces() : 100);

      if (_nplt > _sp->totalTraces()) {
	_data_sel1->SetValue (NTOT, _sp->totalTraces());
      }
      if ((_tmax > _sp->maxTmax()) || (_tmax < _sp->minTmin()) ||
	_first_time) { 
	_data_sel2->SetValue (TMAX, _sp->maxTmax());
      }
      if ((_tmin > _sp->maxTmax()) || (_tmin < _sp->minTmin()) ||
	_first_time) {
	_data_sel2->SetValue (TMIN, _sp->minTmin());
      }
      if (_tmin > _tmax) {
	_data_sel2->SetValue (TMAX, _sp->maxTmax());
	_data_sel2->SetValue (TMIN, _sp->minTmin());
      }
   
      //If we have something like a time slice that is sampled in
      //feet we will try to set a reasonable inches per second
      if (_sp->srval() > 0.1F && _sp->is() > 0.1F) {
	_traceparams->SetValue(IS, (_sp->is() / 100.0F));
      }

      _data_sel2->SetRange (TMIN, _sp->minTmin(), _sp->maxTmax(),  
	_sp->minTmin(), 3);
      _data_sel2->SetRange (TMAX, _sp->minTmin(), _sp->maxTmax(),  
        _sp->maxTmax(), 3);

      if (_sp->canScaleToFile()) {
	XtSetSensitive( _norm_type->GetRadioWidget(PlotImage::FILENORM),
          True);
      }
      else {
	XtSetSensitive (_norm_type->GetRadioWidget(PlotImage::FILENORM),
          False);
	if (_norm_type->WhichSelected() == PlotImage::FILENORM) {
	  _norm_type->SetRadio(PlotImage::PANELNORM);
	}
      } 
          
      _first_time = False;
      _data_sel1->highlight (NTOT);
      _infile_failstr = NULL;
      _infile_valid   = True;
      _time_stamp     = time_stamp;

    }
    else {
      sprintf (wkstr, BADFILE, newvar);
      _infile_failstr = wkstr;
      _infile_valid   = False;
      if (_allow_selector) {
	XtSetSensitive (_selector_tog   ->W(), False);
	XtSetSensitive (_selector_button->W(), False);
        _selector_pop->unmanage ();
	_selector_ndo_pop->unmanage ();
      }
    }
  }
  else {
    Widget tw = _data_sel1->TxtW (NTOT);
    XmProcessTraversal (tw, XmTRAVERSE_CURRENT);
    sprintf (wkstr, BADFILE, newvar);
    _infile_failstr = wkstr;
    _infile_valid   = False;
  }

  if (!_infile_valid) {
    ErrorHandler err = W();
    err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
    char *errstr = _infile_failstr;
    err.deliverError (errstr);
  }

  //netConnectGood();
  _sp->setNetEnv(NULL);
}

void SeisSelect::closeJSFileIfChanged ()
{
  Boolean changed;

  char *fname = _infile->filename ();
  time_t time_stamp = _infile->timeStamp (fname);

  changed = fileModified (time_stamp);
  if (changed) {
    //workstation_force_jseis_close (fname);
    _time_stamp = time_stamp;
  }
}

Boolean SeisSelect::fileModified (time_t time_stamp)
{
  return time_stamp != _time_stamp;
}

void SeisSelect::setFromOther(SeisPlot *othersp)
{
 SeisPlot *sp= othersp ? othersp : _info_sp;


 if (sp) {
     _data_sel1->SetValue(NTOT, sp->nplt() );
     _data_sel1->SetValue(ISKP, sp->iskp() );
     _data_sel1->SetValue(NDO,  sp->ndo() );
     _data_sel1->SetValue(NSKP, sp->nskp() );
     _data_sel2->SetValue(TMIN, sp->tmin() );
     _data_sel2->SetValue(TMAX, sp->tmax() );
     _data_sel2->SetValue(TDEC, sp->tdec() );
     _traceparams->SetValue(TI, sp->ti() );
     _traceparams->SetValue(IS, sp->is() );
     _traceparams->SetValue(CT, sp->ct() );

     _psettings->SetTog(RP,     (Boolean)sp->rp() );
     _psettings->SetTog(RD,     (Boolean)sp->rToL() );
     _psettings->SetTog(INVERT, (Boolean)sp->invert() );

     if(sp->plotType() == PlotImage::PlotWFILL && sp->negativeFill())
          _ptype->SetRadio(NEGATIVE_FILL);
     else
          _ptype->SetRadio( sp->plotType() );
     _norm_type->SetRadio( sp->norm() );
     _external_amp= sp->externalAmp();
     
     if (sp->norm() == PlotImage::EXTERNALNORM) {
         _ext_amp->SetValue(PlotImage::EXTERNALNORM, (float)_external_amp);
     }
     else {
         _ext_amp->clear(PlotImage::EXTERNALNORM);
     }

     /*
     if (_sp->netEnv() == NULL) {
             _host_ops->setOptionValue (LOCAL);
             getDataFrom(LOCAL);
     }
     else {
             _host_ops->setOptionValue (CRAY_SP);
             getDataFrom(CRAY_SP);
     }
     */

     _sp->setNetEnv(NULL);

     if (made())
            wprocVAShowMsg(_information, 
                "Total Nplt: %d,    Tmin: %4.3f,   Tmax: %4.3f   Srval: %4.4f",
                 _sp->totalTraces(), _sp->minTmin(), _sp->maxTmax(), 
                 _sp->srval());

 } // End if

}

void SeisSelect::colorOpEnabled(Boolean enable)
{
 if (made() ) {
    if (enable) {
        XtManageChild(_ptype->GetRadioWidget(PlotImage::PlotGS));
        if (_color_pop) XtManageChild(_but->pushW(VD));
    }
    else {
        XtUnmanageChild(_ptype->GetRadioWidget(PlotImage::PlotGS));
        XtUnmanageChild(_but->pushW(VD));
    }
 }
 _docolor= enable;
}



void SeisSelect::addNewSeisPlot()
{
   SeisWinMan *widget_manager= _sp->getSeisWinMan();
   SeisPlot *oldsp= _sp;
   _sp= new SeisPlot(_first_sp);
   setFromOther(oldsp);
   _infile->setFilename ("");
   widget_manager->setCurrentSP(_sp);
   addSeisPlot(_sp);
}


void SeisSelect::notCurrentInWindow(SeisPlot *sp)
{
   SeisPlot *oldsp= _sp;
   if (_sp == sp) {
       _sp= getSP();
       if (!find(_sp)) {
           addSeisPlot(_sp);
           setFromOther(oldsp);
           if (_infile) {
	     _infile->setFilename (_sp->filename());
	   }
           applyParams();
       }
       else if (XtIsManaged(W())) {
           setFromOther(_sp);
           if (_infile) {
	     _infile->setFilename (_sp->filename());
	   }
       }

// No longer want to turn off selector when the current SeisPlot changes
//  The application will now decide that when a new file is selected

       if(_allow_selector) {
	 _selector_pop->seisPlotChanged (_sp);
	 _selector_ndo_pop->seisPlotChanged (_sp);
       }


   }
}

SeisPlot *SeisSelect::getSP()
{
  return  _sp->currentSPInWindow();
}

void SeisSelect::incrementSeisPlot()
{
   void *x;
   SeisWinMan *widget_manager= _sp->getSeisWinMan();
   if (widget_manager->count() > 1) {
       //SeisPlot *oldsp= _sp;
       assert(widget_manager->find(_sp, &x));
       _sp= widget_manager->next(&x); 
       if (!_sp) _sp= widget_manager->top();
       _infile->setFilename (_sp->filename());
       setFromOther(_sp);
       widget_manager->setCurrentSP(_sp);
   }
}




void SeisSelect::manage()
{
  Widget w;
  char *fname;
  SLBase::manage();

  
  //wprocFileChoiceRetWidgets(_infile, NULL, &w, NULL, NULL);
  w = _infile->W();
  XmProcessTraversal(w, XmTRAVERSE_CURRENT);
  wprocVAShowMsg(_information, 
                "Total Nplt: %d,    Tmin: %4.3f,   Tmax: %4.3f   Srval: %4.4f",
                 _sp->totalTraces(), _sp->minTmin(), _sp->maxTmax(), 
                 _sp->srval());

  if ((!_first_time) && (_sp->plotType() != PlotImage::PlotGRID)) {
           setFromOther(_sp);
/*
//// temporarily removed for testing:
*/
	   _infile->setFilename (_sp->filename());
  }

  if (_first_time) {
    fname = _infile->filename ();
    if (fname && strlen(fname) && strcmp(fname,"NONE")) {
/*
 *     wprocFileChoiceSetFlags(_infile, wprocMustExistMask);
 *     wprocFileChoiceValidate(_infile,True);
 *     wprocFileChoiceSetFlags(_infile, wprocMustExistMask|wprocIsRequiredMask);
*/
       _first_time= False;
    }
//  XtFree(fname);
  }
  //netConnectGood();
  _sp->setNetEnv(NULL);
}


/*
Boolean SeisSelect::netConnectGood()
{
 Boolean retval= True;
 if (_netenv != NULL && _netenv->networkStatus() == NetEnv::Bad) {
        ErrorHandler err= W();
        err.deliverError(
               "Lost Connection to remote machine\nResetting to local.");
        _host_ops->setOptionValue (LOCAL);
        getDataFrom(LOCAL);
        retval= False;
 }
 return retval;
}
*/


Boolean SeisSelect::checkTracesPerInch()
{
Boolean ok = True;
SLErrorPop *errorpop;
char errormsg[80];

  //Verify traces per inch is within hardware range.
  Display *dpy= XtDisplay(W());
  int  scrn= XScreenNumberOfScreen( XtScreen(W()));
  float  mt= ((float) DisplayWidth(dpy,scrn)) /
                                (0.0394 * DisplayWidthMM(dpy, scrn));
  _color_ti_max = mt;
  _wiggle_ti_max = (float)_sp->maxWigglesPerInch();

 if(_ptype->WhichSelected() ==  PlotImage::PlotCOLOR)
   {
   if(_traceparams->GetFloat(TI) > _color_ti_max)
     {
     _traceparams->SetValue(TI, _color_ti_max);
     sprintf(errormsg,
     "The maximum traces per inch in color is %3.2f, and for wiggle is %3.2f\n",
         _color_ti_max, _wiggle_ti_max);
     errorpop = new SLErrorPop(topWidget(),"Error", errormsg);
     return (ok = False);
     }
   }
 else
   {
   if(_traceparams->GetFloat(TI) > _wiggle_ti_max)
     {
     _traceparams->SetValue(TI, _wiggle_ti_max);
     sprintf(errormsg,
     "The maximum traces per inch in color is %3.2f, and for wiggle is %3.2f\n",
         _color_ti_max, _wiggle_ti_max);
     errorpop = new SLErrorPop(topWidget(),"Error", errormsg);
     return (ok = False);
     }
   }

 return ok;

}

Boolean SeisSelect::selectorsMade ()
{
  Boolean retval = _selector_tog     != NULL &&
                   _selector_pop     != NULL &&
                   _selector_ndo_pop != NULL   ;
  return retval;
}

Boolean SeisSelect::updateSelectors ()
{
  Boolean stat = True;

  if (_allow_selector && selectorsMade()) {
     if (  _selector_tog->IsSelected(DOSELECTOR)     &&
         (!_selector_pop->tracesFound()              &&
          !_selector_ndo_pop->results()->getTraces()   )) {
       stat = False;
       SLErrorPop *error_pop = new SLErrorPop(topWidget(),
          "Trace Selector Error",
          "You have chosen the Trace Selector but no traces have been found");
       return stat;
     }

     if (_selector_tog->IsSelected(DOSELECTOR) &&
         _selector_pop->tracesFound()) {
       _sp->setSelectorParameters (1,
         _selector_pop->_primary_header,
         _selector_pop->_primary_min,
         _selector_pop->_primary_max,
         _selector_pop->_primary_inc,
         _selector_pop->_secondary_header,
         _selector_pop->_secondary_min,
         _selector_pop->_secondary_max,
         _selector_pop->_secondary_inc,
         _selector_pop->_tertiary_header,
         _selector_pop->_tertiary_min,
         _selector_pop->_tertiary_max,
         _selector_pop->_tertiary_inc,
         _selector_pop->_traces_found,
         _selector_pop->_frames_found,
         _selector_pop->_traces_in_last_frame,
         _selector_pop->_dosecondary,
         _selector_pop->_dotertiary);
     }
     else if (_selector_tog->IsSelected(DOSELECTOR) &&
         _selector_ndo_pop->results()->getTraces() > 0) {
       _sp->setNDoSelectorParameters (1,
         _selector_ndo_pop->selector(),
         _selector_ndo_pop->results());
     }
     else {
       _sp->setSelectorParameters (0);
       _sp->setNDoSelectorParameters (0);
     }
  }
  return stat;
}

Boolean SeisSelect::ValidInput()
{
 Boolean stat = True;

 if (made()) {
   if (stat && XtIsManaged(topWidget())) {
     if (!_infile_valid) {
       ErrorHandler err = W();
       err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
       char *errstr = _infile_failstr;
       err.deliverError (errstr);
       if (_allow_selector) {
	 XtSetSensitive (_selector_tog   ->W(), False);
	 XtSetSensitive (_selector_button->W(), False);
	 _selector_pop->unmanage ();
	 _selector_ndo_pop->unmanage ();
       }
     }
   }
 }

 stat = checkTracesPerInch();
 if (!stat) return stat;

 if (stat) stat = _data_sel1  ->validate();
 if (stat) stat = _data_sel2  ->validate();
 if (stat) stat = _traceparams->validate();
 //if (!netConnectGood()) stat= False;

 if (stat) {
   stat = updateSelectors ();
 }
 return stat;
}

void SeisSelect::applyParams() 
{
  int norm= _norm_type->WhichSelected();
  if(_ptype->WhichSelected() == NEGATIVE_FILL)
    {
    _sp->setPlotType(PlotImage::PlotWFILL);
    _sp->setNegativeFill(True);
    }
  else
    {
    _sp->setPlotType( _ptype->WhichSelected() );
    _sp->setNegativeFill(False);
    }
  _sp->setNorm( norm );
  if (norm == PlotImage::EXTERNALNORM)_sp->setExternalAmp(_external_amp);
  _sp->setNPlt(_nplt);
  _sp->setISkp(_iskp);
  _sp->setNdo( _data_sel1->GetInt(NDO) );
  _sp->setNSkp(_data_sel1->GetInt(NSKP));
  _sp->setTminTmax(_tmin,_tmax);
  _sp->setTdec(_tdec);
  _sp->setTI(_ti);
  _sp->setIS(_is);
  _sp->setCT(_ct);
  _sp->setRP(_rp);
  _sp->setRtoL(_rd);
  _sp->setInvert(_invert);
}


void SeisSelect::DoAction() 
{ 
  ShellWatch sw;
  Boolean succ;

  applyParams();
  _sp->disableErrors(); 
  if (_color_pop) {
          _color_pop->dontPlotYet(True);
          _color_pop->make();
          _color_pop->dontPlotYet(False);
  }

  closeJSFileIfChanged ();
  succ= _sp->plot();
  if (!succ) {
         manage();
         ErrorHandler err= W();
         err.setEtype(ErrorHandler::Error, ErrorHandler::CUI, False);
         char *errstr= _sp->lastError();
         err.deliverError(errstr);
         free(errstr);
  }
  //netConnectGood();
  _sp->setNetEnv(NULL);

  _sp->enableErrors(); 

}

void SeisSelect::reloadDefaults(Boolean)
{
  if (made()) {
     _infile->reloadDefaults ();
     char *fname = _infile->filename ();
     if (strlen(fname)) {
       _first_time= False;
/*
 *     wprocFileChoiceSetFlags (_infile, wprocMustExistMask);
 *     wprocFileChoiceValidate (_infile, True);
 *     wprocFileChoiceSetFlags (_infile, 
 *       wprocMustExistMask|wprocIsRequiredMask);
*/
     }
//   XtFree(fname);
  }
  _psettings->reloadDefaults();
  _ptype->reloadDefaults();
  _data_sel1->reloadDefaults();
  _data_sel2->reloadDefaults();
  _traceparams->reloadDefaults();
}

void SeisSelect::reloadSystemDefaults(Boolean)
{
  _data_sel1->SetValue(NTOT, 100L );
  _data_sel1->SetValue(ISKP, 0L );
  _data_sel1->SetValue(NDO,  1L );
  _data_sel1->SetValue(NSKP, 0L );
  _data_sel2->SetValue(TMIN, (float)0.0 );
  _data_sel2->SetValue(TMAX, (float)1.0 );
  _data_sel2->SetValue(TDEC, 1L );
  _traceparams->SetValue(TI, (float)20.0 );
  _traceparams->SetValue(IS, (float)2.0 );
  _traceparams->SetValue(CT, (float)4.0 );

  _psettings->SetTog(RP,     False );
  _psettings->SetTog(RD,     False );
  _psettings->SetTog(INVERT, False );
  _ptype->SetRadio( PlotImage::PlotWFILL );
}

void SeisSelect::unmangeIS()
{
  XtUnmanageChild(_traceparams->TxtW(IS));
  XtUnmanageChild(_traceparams->LabW(IS));
}

void SeisSelect::unmangeMAX()
{
  XtUnmanageChild(_but->pushW(MAXIM));
}

void SeisSelect::unmanagePTYPE()
{
  XtUnmanageChild(_ptype->W());
}

void SeisSelect::unmanageCT()
{
  XtUnmanageChild(_traceparams->TxtW(CT));
  XtUnmanageChild(_traceparams->LabW(CT));
}

void SeisSelect::unmanageScale()
{
  _norm_form->unmanage();
  XtUnmanageChild(_bottom_tmp);
}

void SeisSelect::UndoInput()
{
  _sp->resetPreviousFilename();
}



void SeisSelect::setAnnoPop(SLFormPop  *pop)
{
 if (_anno_pop) delete _anno_pop;
 _anno_pop= pop;

 if (_anno_pop) XtManageChild(_but->pushW(ANNO)); 
 else           XtUnmanageChild(_but->pushW(ANNO)); 
}

void SeisSelect::setColorPop(SeisColorPop  *pop)
{
 _color_pop= pop;

 if (made()) {
     if ((_color_pop)&&(_docolor)) XtManageChild(_but->pushW(VD)); 
     else                          XtUnmanageChild(_but->pushW(VD)); 
 }
}


void SeisSelect::setUnderPop(SLFormPop  *pop)
{
 if (_under_pop) delete _under_pop;
 _under_pop= pop;

 if (_under_pop) XtManageChild(_but->pushW(UNDERLAY)); 
 else            XtUnmanageChild(_but->pushW(UNDERLAY)); 
}

/*
void SeisSelect::getDataFrom(int where)
{
   if (where == CRAY_SP) {

       PsuedoWidget res_pw= PsuedoWidget( W(), NULL );
       int port=   res_pw.anyIntDef("sp-port", "Sp-Port");
       char *host= res_pw.anyDef(   "sp-host", "Sp-Host");

       _netenv= NetEnv::getNetEnv(host, port);

       if (_netenv->networkStatus() == NetEnv::Bad) _netenv->redoConnection();
       if (_netenv->networkStatus() == NetEnv::Good) {
              _sp->setNetEnv(_netenv);
              setNetEnvPtr(&_netenv);
       }
       else {
              _host_ops->setOptionValue (LOCAL);
              ErrorHandler err= W();
              err.deliverError(
                 "Cannot establish connection to remote machine.");
       }
   }
   else if (where == LOCAL) {
       _sp->setNetEnv(NULL);
       if ( _alt_access.objptr) *_alt_access.objptr= NULL;
   }
   else 
       assert(0);
}

void SeisSelect::setNetEnvPtr(NetEnv **netenv)
{
  _alt_access.objptr=           netenv;
  _alt_access.access_func=      NetEnv::access;
  _alt_access.exp_tilde_func=   NetEnv::expTilde;
  _alt_access.mod_type_func=    NetEnv::modTime;
  _alt_access.exp_file_func=    NetEnv::expFile;
  _alt_access.get_dir_func=     NetEnv::dir;
  _alt_access.get_dir_dir_func= NetEnv::dirOfDirectories;
  _sp->setNetEnv(*netenv);
  if (made())
        wprocFileChoiceSetAltFileAccess(_infile, &_alt_access);

}
*/

void SeisSelect::resetDataSelectionParameters( long  ntot, long  iskp,
                                               long  ndo,  long  nskp,
                                               long  tdec, float tmin,
                                               float tmax              )
{
  _data_sel1->SetValue(NTOT,   ntot );
  _data_sel1->SetValue(ISKP,   iskp );
  _data_sel1->SetValue(NDO,    ndo  );
  _data_sel1->SetValue(NSKP,   nskp );
  _data_sel2->SetValue(TDEC,   tdec );
  _data_sel2->SetValue(TMIN,   tmin );
  _data_sel2->SetValue(TMAX,   tmax );
  wprocVAShowMsg(_information,
       "Total Nplt: %d,    Tmin: %4.3f,   Tmax: %4.3f,   Srval %4.4f",
            ntot,              tmin,          tmax,    _sp->srval());
 
}


void SeisSelect::useSelector(int   primary_header,
                             float primary_min,
                             float primary_max,
                             float primary_inc,
                             int   secondary_header,
                             float secondary_min,
                             float secondary_max,
                             float secondary_inc,
                             int   tertiary_header,
                             float tertiary_min,
                             float tertiary_max,
                             float tertiary_inc,
                             long  traces_found,
                             long  frames_found,
                             long  traces_in_last_frame,
                             long  secondary_search,
                             long  tertiary_search)
{
  _selector_tog->SetTog(DOSELECTOR, True);
  _sp->setSelectorParameters(1,
                             primary_header,
                             primary_min,
                             primary_max,
                             primary_inc,
                             secondary_header,
                             secondary_min,
                             secondary_max,
                             secondary_inc,
                             tertiary_header,
                             tertiary_min,
                             tertiary_max,
                             tertiary_inc,
                             traces_found,
                             frames_found,
                             traces_in_last_frame,
                             secondary_search,
                             tertiary_search);
}


void SeisSelect::useSelector (NDoTraceSelection *select,
                              NDoTraceFind *results)
{
  _selector_tog->SetTog (DOSELECTOR, True);
  _sp->setNDoSelectorParameters (1, select, results);
}

void SeisSelect::turnOffSelector()
{
  if(_allow_selector)
    _selector_tog->SetTog(DOSELECTOR, False);
  _sp->setSelectorParameters (0);
  _sp->setNDoSelectorParameters (0);
}

void SeisSelect::getTracePattern(long *nplt, long *iskp, long *ndo, long *nskp,
                                 long *frames, long *domovie)
{
  *nplt    = _nplt;
  *iskp    = _iskp;
  *ndo     = _ndo;
  *nskp    = _nskp;
  *frames  = _frames;
  *domovie = _domovie;
}

void SeisSelect::setTracePattern(long nplt, long iskp, long ndo, long nskp,
                                 long frames, long domovie)
{
  _data_sel1->SetValue (NTOT, nplt);
  _data_sel1->SetValue (ISKP, iskp);
  _data_sel1->SetValue (NDO,  ndo);
  _data_sel1->SetValue (NSKP, nskp);
  setNumMovieFrames (frames);
  setMovie ((Boolean)domovie);
}

void SeisSelect::setNumTracesToPlot(long nplt)
{
  _previous_nplt = nplt;//So checkForPatternChange will not be called
  _data_sel1->SetValue(NTOT, nplt);
}

void SeisSelect::setNumMovieFrames(long /*num_frames*/)
{
}


void SeisSelect::postScan(SeisPlot *sp, SeisPlot::ScanDir )
{
  _data_sel1->SetValue(ISKP, sp->iskp());
  _data_sel1->SetValue(NTOT, sp->nplt());
} 

void SeisSelect::setMovie (Boolean domovie)
{
}
