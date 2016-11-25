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
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include "vu/seis_avast_pop.hh"
#include "vu/seis_avast.hh"
#include "vu/tpbox_angle_table.hh"
#include "sp/seis_plot.hh"
#include "sl/slp_file.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/shell_watch.hh"
#include "sl/error_handler.hh"
#include <math.h>

#define INFILE_NAME  "infile"
#define    CHANGE_ANGLE  30L


static String defres[]= {
     ".width:                         600",
     "_popup.title:             AVA Angles",
     "*infile.labelString:      Filename...",
     "*infile.fileDescription:  Avast file",
     "*infile.fileExtension:     ",
     "*infile.fileFlags:        MustExist IsRequired",
     "*infile.annoType:         pushbutton",
     "*infile*Fileshell*dirMask: *avast",
    NULL};


enum {ANGLE};



static char *picking_mode = "Mode: Pick AVAST Trace";
static const char * const help_token = "AVAST";
static char *avasthelp= "mouse*AVAST:  BTN#1: Select location to modify, \
BTN#2: None, BTN#3: None";


#define ParentClass SLFPopSep


SeisAvastPop::SeisAvastPop( Widget            p,
                          char              *name,
                          SeisPlot          *sp,
                          SeisAvast          *so,
                          HelpCtx           hctx) 

       : SLFPopSep(p,name,FP_DOALL,hctx,True,False),
                            _plot_on_doaciton(False), _new_file(True),
                            _use_file_defaults(False), _new_appdefaults(True),
                            _sp(sp),_so(so), 
                            _new_angle(so->getAngle()),
	                    _first_time(True), _infile(NULL),
                            _infile_valid(False), _infile_failstr(NULL)

{

  static SLText textb2[]  = {
    {"new_angle", NULL, NULL, SLType_float, ANGLE},
  };
  textb2[0].target= &_new_angle;


   _new_avastfile[0]= '\0';

   setDefaultResources( p, name, defres);

   
   _angle_box= new TpboxAngleTable( this, _so);


   _trace_selected = False;
   _tindex = 0;
   _so->_sop = this;
   _pick_avast = NULL;

   _infile = new SLpFile (this, INFILE_NAME);

   DoAction();     
}



//Following done to allow customization of widget creation
SeisAvastPop::SeisAvastPop( Widget            p,
                          char              *name,
                          SeisPlot          *sp,
                          SeisAvast          *so,
                          HelpCtx           hctx,
                          Boolean           /*custom_widgets*/) 

       : SLFPopSep(p,name,FP_DOALL,hctx,True,False),
                            _plot_on_doaciton(False), _new_file(True),
                            _use_file_defaults(False), _new_appdefaults(True),
                            _sp(sp),_so(so), 
                            _new_angle(so->getAngle()),
	                    _first_time(True), _infile(NULL),
                            _infile_valid(False), _infile_failstr(NULL)
{
  _infile = new SLpFile (this, INFILE_NAME);
}



SeisAvastPop::~SeisAvastPop()
{
  delete _infile;
}


Widget SeisAvastPop::make(Widget p)
{

   if ( made() ) return topWidget();
   SLFPopSep::make(p);

  _infile->setCtrap (fileCallback, this);

   XtVaSetValues (_infile->W(),
                            XmNtopAttachment,   XmATTACH_FORM,
                            XmNtopOffset,       15,
                            XmNleftAttachment,  XmATTACH_FORM,
                            XmNleftOffset,      20,
                            XmNrightAttachment, XmATTACH_FORM,
		            XmNrightOffset,     20,             NULL );

   XtVaSetValues( _angle_box->W(), XmNleftAttachment,   XmATTACH_FORM,
                                   XmNleftOffset,       200,
                                   XmNtopAttachment,    XmATTACH_WIDGET,
                                   XmNtopOffset,        15,
                                   XmNtopWidget,        _infile->W(),
                                   XmNbottomAttachment, XmATTACH_WIDGET,
                                   XmNbottomOffset,     15,
                                   XmNbottomWidget,     bottomSeparator(),
                                   NULL);

   return topWidget();

}


#define BADFILE " %s is not a AVAST file."

void SeisAvastPop::filein (long ident, char *oldvar, char *newvar)
{
  static char wkstr[200];

  long nangles;
  float *angles;

  int newfile;
  if (oldvar) {
    if (newvar && strcmp(oldvar,newvar) && strlen(newvar)) {
      newfile = 1;
    }
    else {
      newfile = 0;
    }
  }
  else if (newvar && strlen(newvar)) {
    newfile = 1;
  }
  else {
    newfile = 0;
  }

  if (newfile) {
    if (_so->setFilename(newvar)) {
      nangles = _so->getAngleCount();
      angles = new float [nangles];
      _so->getAngles(angles);
      _angle_box->setAngles((int) nangles, angles);
      delete [] angles;
      _infile_failstr = NULL;
      _infile_valid   = True;
    }
    else {
      sprintf (wkstr, BADFILE, newvar);
      _infile_failstr = wkstr;
      _infile_valid   = False;
    }
  }

  if (!_infile_valid) {
    ErrorHandler err = W();
    err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
    char *errstr = _infile_failstr;
    err.deliverError (errstr);
  }
}



void SeisAvastPop::fileCallback (void *data, long ident, char *oldvar,
  char *newvar)
{

  SeisAvastPop *obj = (SeisAvastPop *)data;
  obj->filein (ident, oldvar, newvar);
}


Boolean SeisAvastPop::ValidInput()
{
 Boolean stat;


  if (made())
     stat= ParentClass::ValidInput();
  else
     stat= True;

  return (stat); 
}


void SeisAvastPop::UndoInput()
{
  if(_sp->imageIsDisplayed())_so->makeInvisible();
  if(_sp->imageIsDisplayed()) {
     _so->makeInvisible();
     activityNotify(SLShellContainer::EndingActivity);
  }
  SLFormPop::UndoInput();
  _sp->redraw();
  _trace_selected = False;
}


void SeisAvastPop::DoAction()
{
  ParentClass::DoAction();
  Boolean replot = False;
  ShellWatch watch1;

  if(!_sp->imageIsDisplayed())return; 

  if (_so->checkForNewFile()) {
     replot = True;
  }

  if(_so->getAngle() != _new_angle) 
     {
     _so->changeAngle(_new_angle);
     replot = True;
     }

  if(!_first_time)
     {
     switch (whichButton())
        {
        case FP_OK:
        case FP_APPLY:
                   if(_sp->imageIsDisplayed())
                      {
                      _so->makeVisible();
                      activityNotify(SLShellContainer::StartingActivity);
                      replot = True;
		      }
                   break;
        }
     }


  if(replot) 
     {
     changeHeaders();
     _sp->redraw();
     _so->post(_sp);
     }

}


void SeisAvastPop::manage()
{
  XtManageChild(topWidget()); 
  _first_time = False;

}

void SeisAvastPop::reloadDefaults(Boolean)
{
  SLFPopSep::reloadDefaults();
  DoAction();
}


void SeisAvastPop::reloadSystemDefaults(Boolean do_method)
{
  SLFPopSep::reloadSystemDefaults(do_method);
  DoAction();
}


void SeisAvastPop::original( void *data, long /*button*/)
{
  SeisAvastPop *obj = (SeisAvastPop *)data;

  obj->_so->loadHeaders(obj->_sp);
  obj->_sp->redraw();
  obj->_so->post(obj->_sp);
  obj->_so->_headers_modified = False;
}


void SeisAvastPop::newData()
{
  if(!_sp->imageIsDisplayed())return;
  if(_first_time) return;

  _tindex = 0;
  _so->_headers_modified = False;
  _trace_selected = False;
}


void SeisAvastPop::changeHeaders()
{
  Boolean *trace_ary;
  const float *sphd = _sp->headers();

  if(!_trace_selected) 
    {
    _so->loadHeaders(_sp);
    return;
    }

  trace_ary = (Boolean *)calloc(1, (unsigned int)(_sp->plottedNplt() 
                                   * sizeof(Boolean)));
  if(trace_ary == NULL) {printf("handle trace_ary error here\n"); return;}

  _so->loadHeaders(_sp);

 free(trace_ary);
}


void SeisAvastPop::selected( void *data, long /*button*/)
{
  SeisAvastPop *obj = (SeisAvastPop *)data;
  if(obj->_pick_avast == NULL) obj->_pick_avast = new PickAvast(obj->_sp, obj);
}


//******************** Avast's PickBase ********************************

PickAvast::PickAvast (SeisPlot    *sp, 
                    SeisAvastPop *sop)
                    : PickBase(sp, picking_mode, help_token, avasthelp,
                                   XC_tcross, allow, True), _sop(sop)
{

}

void PickAvast::buttonAny(int ev_x1, int /*ev_x2*/, int /*ev_y1*/, 
                         int /*ev_y2*/, int button, Action action, 
                         Modifier /*modifier*/)
{
 long trace;
 const float *sphd;


 if(!_sop->_sp->imageIsDisplayed())return;
 sphd = _sop->_sp->headers();
 long THDR = _sop->_sp->header1() - 1;


 if(button == 1 && action == press)
    {
    trace = (long)(_sop->_sp->xWC(ev_x1) + .5);
    if(trace < 1) trace = 1;
    if(trace > _sop->_sp->plottedNplt()) trace = _sop->_sp->plottedNplt(); 
    _sop->_tindex = (trace-1) * _sop->_sp->numHeaders();
    _sop->_trace_selected = True;
    _sop->_pick_avast = NULL;  
    delete this;
    }

}
