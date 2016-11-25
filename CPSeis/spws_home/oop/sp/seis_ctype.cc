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
#include "sp/seis_ctype.hh"
#include "sp/seis_cbar.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_scale.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_push_box.hh"
#include "sl/slp_file.hh"
#include "sl/error_handler.hh"
#include "color/seis_cbb_pop.hh"
#include "color/cbb_col_set_gui.hh"
#include "color/seis_color_file_io.hh"

#include "exptilde_crou.h"


enum {CBAR,USEDEF,CBLDR};

#define CFILE_NAME  "cfile"
#define CFILE_IDENT 0
#define CFILE_LABEL "Color File:  "
#define CFILE_TYPE  "Color Bar File"
#define CFILE_EXT   "rgb"


static SLRadio radios[]  = {
                     { "Cbar",   CBAR, },
                     { "Usedef", USEDEF, },
                };


static String  defres[]= {
    "*Cbar.labelString:        Color Bar   -->",
    "*Usedef.labelString:      Use Defined -->",
    "*scale_lab.labelString:   Color Number:",
    "*clab.labelString:        Color Selection Parameters",
    "*cfile.labelString:       Color File:",
    "*cfile.fileDescription:   Color Bar File",
    "*cfile*Fileshell*dirMask: *.rgb",
    "*cnum.minimum:            1",
    "*cnum.maximum:            21",//Change this when adding to defined colors
    "*Usedef.set:              True",
    "*cbldr.labelString:       Color Builder...",
    NULL};

static SLPush button[] = {
  { "cbldr",  CBLDR },
};



class Cscale :  public SLScaleDrag {
               protected:
                 virtual void    ScaleAction(int);
                 class SeisCtype *_csel;
                 long            _last_val;
               public :
                 Cscale(  SLDelay       *contain,
                          char          *name,
                          HelpCtx       hctx,
                          class SeisCtype *csel,
                          int           *valptr = NULL) :
                 SLScaleDrag(contain, name, hctx, valptr),
                 _csel(csel), _last_val(-1) {};
};


void Cscale::ScaleAction(int val)
{

 //If we do not have enough colors for the 64 color bars reset the range
 if(_csel->_sp->getLoadableColors() < 64 && _last_val == -1)
   {
   setRange(1, PlotImage::SAAT - 1);
   }

 if (val != _last_val) {
      _csel->_cbar->setPredef(val);
      _csel->_c_choice->SetRadio( USEDEF);
      if (_csel->_cbldr->colSetGui()->RGBChanged())
        _csel->_cbldr->changeRGB ();
      _csel->_new_color= True;
      _csel->newColorBar();
 } // End if
 _last_val= val;

}






static char *failstr= " %s is not a valid rgb file.";

SeisCtype::SeisCtype( Widget   p,
                      char     *name,
                      HelpCtx  hctx,
                      SeisPlot *sp,
                      SeisCbar *cbar,
                      Boolean  make_now) 
          : SLForm(p,name,hctx,False,False,True),
            _sp(sp), _cbar(cbar), _new_color(False), _cfile_failstr(NULL),
	    _cfile_valid(False), _cbldr(NULL), _first_time(True)
{
  setDefaultResources( p, name, defres);
  _cfile[0] = '\0';
  init();
  if (make_now) make(p);
  else          supportUnmadeDefaults(p);
}


SeisCtype::SeisCtype( SLDelay  *contain,
                      char     *name,
                      HelpCtx  hctx,
                      SeisPlot *sp,
                      SeisCbar *cbar,
                      Boolean  make_if_can)
          : SLForm(contain,name,hctx,False,True,True),
	    _sp(sp), _cbar(cbar), _new_color(False), _cfile_failstr(NULL),
	    _cfile_valid(False), _cbldr(NULL), _first_time(True)
{
  _cfile[0] = '\0';
  if ((contain->made())&&(make_if_can))  {
    setDefaultResources( contain->topWidget(), name, defres);
    init();
    make(contain->topWidget());
  }
  else {
    supportUnmadeDefaults(contain->pW());
    setDefaultResources( contain->pW()->display(), name, defres);
    init();
  }
}


void SeisCtype::init ()
{

  _cnum= new Cscale( this, "cnum", getHelpCtx(), this);
  _c_choice= new SLRadioBox( this, "pchoice", getHelpCtx(),
                              radios, XtNumber(radios), NULL, True );
  _c_choice->setAltChoiceAction( (SLRadioButtonfunc)doCtog, (void *)this);

  int max_colors;
  if (_sp) {
    max_colors = _sp->getLoadableColors ();
  }
  else {
    max_colors = 64;
  }

  _fio   = new SeisColorFileIO (this);
  _cbldr = new SeisCBBPop (this, "cbldr", getHelpCtx(), _sp,
    max_colors, _fio, _cbar);

  _but = new SLPushBox (this, "cbldr_push", getHelpCtx(), button,
    XtNumber(button), True);

  _but->setComplexNotify (this);

  _cfileo = new SLpFile (this, CFILE_NAME, CFILE_IDENT, CFILE_LABEL,
    CFILE_TYPE, CFILE_EXT);
  _cfileo->setCtrap (cfilecb, this);

  unmade_filesetup ();
}

SeisCtype::~SeisCtype ()
{
  if (_but)      delete _but,      _but      = 0;
  if (_cnum)     delete _cnum,     _cnum     = 0;
  if (_c_choice) delete _c_choice, _c_choice = 0;
  if (_cbldr)    delete _cbldr,    _cbldr    = 0;
  if (_fio)      delete _fio,      _fio      = 0;
  if (_cfileo)   delete _cfileo,   _cfileo   = 0;
}



Widget SeisCtype::make(Widget   p)
{

  if (made()) return topWidget();
  SLForm::make(p);


  XtVaSetValues( _c_choice->W(), XmNleftAttachment, XmATTACH_FORM,
                                  XmNtopAttachment,  XmATTACH_FORM, NULL );

  XtVaSetValues( _cfileo->W(), XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,       _c_choice->W(),
		               XmNleftAttachment,  XmATTACH_WIDGET,
		               XmNleftWidget,      _c_choice->W(),
		               XmNrightAttachment,  XmATTACH_FORM, NULL);

  XtVaSetValues( _cnum->W(),  XmNtopAttachment, XmATTACH_WIDGET,
                              XmNtopWidget,       _cfileo->W(),
                              XmNleftAttachment,  XmATTACH_WIDGET,
                              XmNleftWidget,      _c_choice->W(),   NULL);

  XtVaSetValues( _but->W(),   XmNtopAttachment,   XmATTACH_WIDGET,
                              XmNtopWidget,       _c_choice->W(),
                              XmNtopOffset,       10,
                              XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                              XmNleftWidget,      _c_choice->W(),
                              NULL);


  if ( _c_choice->WhichSelected() == CBAR) {
        if (_cfile_valid) {
            _cbar->readColorFile (_cfile);
            _cbldr->setFileIn (_cfile);
        }
        else
            _c_choice->SetRadio( USEDEF);
  }
  if ( _c_choice->WhichSelected() == USEDEF)
           _cbar->setPredef( _cnum->GetScaleValue() );







 return topWidget();
}


void SeisCtype::pushAction (long ident)
{
  if (ident == CBLDR) {
    _cbldr->makeAndManage (XtParent(W()));
  }
}


Boolean SeisCtype::notifyComplex (SLDelay *obj, int ident)
{
  if (obj == _but) {
    pushAction (ident);
  }
  return True;
}

long SeisCtype::readColorFile (char *filename)
{

  static char *failstr_toomany= " has too many colors.\n\n\
You may only use a color bar file with %1d or less colors.";
  static char wkstr[200];

  long retval;

  retval = _cbar->readColorFile (filename);
  if (retval == SeisColor::CbarSucc) {
    _cfileo->setFilename (filename);
    _cfile_valid = True;
    if (_c_choice->WhichSelected() != CBAR) _c_choice->SetRadio (CBAR);
    _cfile_failstr = NULL;
    _cbldr->setFileIn (filename);
    strcpy (_cfile, filename);
  }
  else if (retval == SeisColor::CbarTooMany)  {
    sprintf (wkstr, failstr_toomany, _cbar->availableColors());
    _cfile_failstr = wkstr;
    _cfile_valid = False;
    _cfile[0] = '\0';
  }
  else if (retval == SeisColor::CbarInvalid) {
    sprintf (wkstr, failstr, filename);
    _cfile_failstr = wkstr;
    _cfile_valid = False;
    _cfile[0] = '\0';
  } // End else
  return retval;
}




void SeisCtype::cfilecb (void *data, long /*ident*/, char *oldvar,
  char *newvar)
{
  static char *failstr_mustexist= " must exist\n\
Enter a color file or use a\npredefined color.";

  SeisCtype *obj = (SeisCtype *)data;
  long stat;

  int newfile;
  if (oldvar) {
    if (newvar && strcmp(oldvar,newvar)) {
      newfile = 1;
    }
    else {
      newfile = 0;
    }
  }
  else if (newvar) {
    newfile = 1;
  }
  else {
    newfile = 0;
  }

  int use_none = !strcmp(newvar,"NONE") || !strcmp(newvar,"none");
  if (newfile) {

    if (strlen(newvar) == 0 || use_none) {
      if (obj->_c_choice->WhichSelected() == USEDEF || use_none) {
	obj->_c_choice->SetRadio (USEDEF);
	obj->_cfile_failstr = NULL;
	obj->_cfile_valid = True;
	obj->_cfileo->setFilename ("NONE");
      } // End if
      else {
	obj->_cfile_failstr = failstr_mustexist;
	obj->_cfile_valid = False;
	obj->_cfile[0] = '\0';
      }
    }
    else {
      char fname[300];
      exptilde_crou2 (fname, newvar);
      stat = obj->readColorFile (fname);
    } // End else
  }
  else {
    if (use_none) {
	obj->_c_choice->SetRadio (USEDEF);
	obj->_cfile_failstr = NULL;
	obj->_cfile_valid = True;
	obj->_cfile[0] = '\0';
    } // End if
    else {
      obj->_cfile_failstr = failstr_mustexist;
      obj->_cfile_valid = False;
      obj->_cfile[0] = '\0';
    }
  }

  if (!obj->_cfile_valid) {
    ErrorHandler err = obj->W();
    err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
    char *errstr = obj->_cfile_failstr;
    err.deliverError (errstr);
  }

  obj->_new_color = True;
}

void SeisCtype::doCtog(SeisCtype *obj,
                       long      whichButton)
{
 Boolean stat;

  switch (whichButton) {
       case USEDEF :
                    obj->_cbar->setPredef( obj->_cnum->GetScaleValue() );
                    if (obj->_cbldr->colSetGui()->RGBChanged())
                      obj->_cbldr->changeRGB ();
                    break;
       case CBAR :
                    stat = obj->_cfile_valid;
                    if (!stat) {
                      obj->_c_choice->SetRadio(USEDEF);
		    }
                    else if (obj->okToReadColorFile()) {
                      obj->readColorFile (obj->_cfile);
		    }
                    break;
  } // End Switch
  obj->_new_color= True;
  obj->typeChange((int)whichButton);

}




Boolean SeisCtype::okToReadColorFile ()
{
  Boolean retval;

  if (_first_time) {
    if (strlen(_cfile) > 0) {
      _first_time = False;
      retval = True;
    }
    else {
      retval = False;
    }
  }
  else {
    retval = True;
  }
  return retval;
}




void SeisCtype::unmade_filesetup()
{

  char *tfile;

  tfile= _pw_topw->childFileChoiceDef( CFILE_NAME);

  if (tfile) {
    strcpy (_cfile, tfile);
    _cfileo->setFilename (_cfile);
    _cfile_valid = True;
  }
  else {
    _cfile[0]= '\0';
  }

  if ( _c_choice->WhichSelected() == CBAR) {
    if (_cbar->readColorFile(_cfile) == SeisColor::CbarInvalid)
              _cbar->setPredef( _cnum->GetScaleValue() );
  }

  if ( _c_choice->WhichSelected() == USEDEF)
              _cbar->setPredef( _cnum->GetScaleValue() );
}




void SeisCtype::setFileName(char *fname) 
{
  if (_cbldr) {
    _cbldr->setFileIn (fname);
    cfilecb (this, 0, 0, fname);
  }
}

char *SeisCtype::getFileName() 
{
  return _cfileo->filename ();
}

void SeisCtype::setColorType(int which)
{
 doCtog(this,which);
}



int SeisCtype::whichSelected()
{
  return _c_choice->WhichSelected();
}

void SeisCtype::setPredef(int which) 
{ 
    setColorType(USEDEF); 
    _cnum->setScaleValue(which);
}


int SeisCtype::getPredef() 
{ 
   return _cnum->GetScaleValue();
}

Widget SeisCtype::getColorScaleWidget()
{
  return _cnum->W();
}


void SeisCtype::newColorBar()
{

}
