#include <Xm/Xm.h>
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
#include <Xm/Text.h>
#include "cube/cube_select.hh"
#include "cube/cube_master.hh"
#include "cube/cube_display.hh"
#include "cube/cube_section_gui.hh"
#include "cube/cube_annotation_gui.hh"
#include "cube/cube_amplitude_gui.hh"
#include "cube/cube.hh"
#include "cube/cube_plot_error.hh"
#include "sl/psuedo_widget.hh"
#include "sl/error_handler.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_text_box.hh"
#include "sl/slp_option.hh"
#include "sl/slp_file.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_color_pop.hh"
#include "sp/do_abort_with_status.hh"
#include "net/net_env.hh"
#include "cube.h"

#define INFILE_NAME  "infile"
#define REPEATFILE   " has already been selected\nfor this window."
#define CUBENOTSET   " could not be set\nas a cube file."
#define BADCUBE      " was rejected as a\ncube file."
#define FILEMISMATCH "Cube File Invalid:  File\nsize does not match\
\nprevious cube(s)."
#define ISNOTAFILE   "Cube File Invalid:  Check\nto see if file has been\
\nset."
#define ISNOTVALIDSIZE "Cube File Invalid:  File\nhas one or more\
\ndimensions of unity."
#define CUBEALLOC  "Percent of sample-slice cube allocation complete:"
#define CUBECREATE "Percent of sample-slice cube creation complete:"

#define INFILE_IDENT    0
#define INFILE_LABEL    "Filename..."
#define INFILE_TYPE     "Cube File"
#define INFILE_EXT      "t*"


enum {
  DISPLAY, ANNOTATE, COLOR, LNHW, XLHW, LOCAL, CRAY_SP, AMPLITUDE
};

static String defres[] = {
  "*sectyplab.labelString:          Section Type",
  "*totlab.labelString:             Total",
  "*hslab.labelString:              Horizontal Slices:",
  "*lnlab.labelString:              Lines:",
  "*xllab.labelString:              Crosslines:",
  "*lavlab.labelString:             LAV:",
  "*lnhw.labelString:               Line Header: ",
  "*xlhw.labelString:               Crossline Header: ",
  "*lnhw.value:                     7",
  "*xlhw.value:                     8",
  "*hstot.labelString:              0",
  "*lntot.labelString:              0",
  "*xltot.labelString:              0",
  "*lav.labelString:                0",
  "*add.labelString:                Add",
  "*display.labelString:            Display...",
  "*annotate.labelString:           Annotation...",
  "*amplitude.labelString:          Scaling...",
  "*color.labelString:              Color...",
  "*infile.labelString:             Filename...",
  "*infile.width:                   350",
  "*infile.fileDescription:         Cube file",
  "*infile.fileExtension:           cube",
  "*infile.fileFlags:               MustExist IsRequired",
  "*infile.annoType:                pushbutton",
  "*infile*Fileshell*dirMask:       *.t*",
  "*action_push.packing:            PACK_COLUMN",
  "*action_push.numColumns:         2",
  "*sp-port:                        6090",
  "*sp-host:                        poepsn03.po.dupont.com",

  NULL
};

static SLPush buttons[] = {
  { "display",  DISPLAY },
  { "annotate", ANNOTATE },
  { "color",    COLOR },
  { "amplitude",AMPLITUDE}
};

/*
static void host_ops_trap (void *data, long ident, long oldvar,
  long newvar)
{
  CubeSelect *cube_select = (CubeSelect *)data;
  int ident = (int)newvar;
  cube_select->getDataFrom (ident);
}

static long host_ops_upfun (void *data)
{
  CubeSelect *cube_select = (CubeSelect *)data;
  if (cube_select->netConnectOk ()) {
    return (long)CubeSelect::CRAY_SP;
  }
  else {
    return (long)CubeSelect::LOCAL;
 }
}
*/

CubeSelect::CubeSelect (Widget p, char *name, HelpCtx hctx,
  CubeDisplay *cube_display, CubeSectionGui *cube_section,
  CubeAnnotationGui *cube_annotation, CubeAmplitudeGui *cube_amplitude) :
  SLFPopSep (p, name, FP_DOALL, hctx, False, False),
  CubeInform (cube_display->currentDisplayedCube()),
  _cube_display     (cube_display),
  _cube_section     (cube_section),
  _cube_annotation  (cube_annotation),
  _cube_amplitude   (cube_amplitude),
  _first_time       (True),
  _hstot            (0),
  _lntot            (0),
  _xltot            (0),
  _coord_hwds       (0),
  _lav              (0),
  _netenv           (NULL),
  _changing_file    (False),
  _deleting_file    (False),
  _cube_file_state  (NONE)
{
  _infile = new SLpFile (this, INFILE_NAME, INFILE_IDENT, INFILE_LABEL,
    INFILE_TYPE, INFILE_EXT);
  _infile->setCtrap (fileCallback, this);

  char *tfile = _pw_topw->childFileChoiceDef (INFILE_NAME);
  if (tfile) {
    strcpy (_cube_file, tfile);
    _infile->setFilename (_cube_file);
  }
  else {
    _cube_file[0] = '\0';
  }

  static SLText Coord_hwds_text[] = {
    {"lnhw", "range:1 9999999, default:7", NULL, SLType_int, LNHW},
    {"xlhw", "range:1 9999999, default:8", NULL, SLType_int, XLHW},
  };

  Coord_hwds_text[0].target=&_lnhw;
  Coord_hwds_text[1].target=&_xlhw;

  _added_cube = NULL;

  //_alt_access.objptr=NULL;

  setDefaultResources (p, name, defres);

  //_host_ops = new SLpOption (this, "host_ops", 0, "Get Data From: ");
  //_host_ops->addOption ("Local"    , CubeSelect::LOCAL  );
  //_host_ops->addOption ("Cray - sp", CubeSelect::CRAY_SP);

  //_host_ops->setItrap     (host_ops_trap , this);
  //_host_ops->setupIvarFun (host_ops_upfun, this);

  _but = new CubeSelectPush (this, "action_push", getHelpCtx(), buttons,
    XtNumber(buttons), this);

  _coord_hwds = new SLTextBox (this, "coord_hwds", getHelpCtx(),
    Coord_hwds_text, XtNumber(Coord_hwds_text));

  Cube *cube = _cube_display->currentDisplayedCube ();
  if (cube) {
    cube->transformHeaders (&_lnhw, &_xlhw);
    _coord_hwds->SetValue ((int)LNHW, (long)_lnhw);
    _coord_hwds->SetValue ((int)XLHW, (long)_xlhw);
    _cube_annotation->setLineHeader      (_lnhw);
    _cube_annotation->setCrosslineHeader (_xlhw);
  }

  CubeMaster::instance()->addInformer (this);
}

CubeSelect::~CubeSelect ()
{
  CubeMaster::instance()->delInformer (this);
  delete _but;
  delete _coord_hwds;
  delete _infile;
}

Boolean CubeSelect::netConnectOk ()
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

Widget CubeSelect::make (Widget p)
{
  if (made()) return topWidget ();
  SLFPopSep::make (p);
  p = wParent ();

    XtVaSetValues (_but->W(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     bottomSeparator(),
                             XmNbottomOffset,     5,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       100,
                             XmNnumColumns,       2,
                             NULL);


  XtVaSetValues (_coord_hwds->W(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _but->W(),
                             XmNbottomOffset,     20,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       20,
                             NULL);

  /*
  XtVaSetValues (_host_ops->W(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _coord_hwds->W(),
                             XmNbottomOffset,     15,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       20,
                             NULL);
  */


  Widget lavlab = XtVaCreateManagedWidget (
                             "lavlab",            xmLabelWidgetClass,
                             topWidget(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _coord_hwds->W(),
                             XmNbottomOffset,     15,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       20,
                             NULL);

  Widget hslab = XtVaCreateManagedWidget (
                             "hslab",             xmLabelWidgetClass,
                             topWidget(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     lavlab,
                             XmNbottomOffset,     15,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       20,
                             NULL);

  Widget xllab = XtVaCreateManagedWidget (
                             "xllab",             xmLabelWidgetClass,
                             topWidget(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     hslab,
                             XmNbottomOffset,     5,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       20,
                             NULL);

  Widget lnlab = XtVaCreateManagedWidget (
                             "lnlab",             xmLabelWidgetClass,
                             topWidget(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     xllab,
                             XmNbottomOffset,     5,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       20,
                             NULL);

  Widget sectyplab = XtVaCreateManagedWidget (
                             "sectyplab",         xmLabelWidgetClass,
                             topWidget(),
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     lnlab,
                             XmNbottomOffset,     5,
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       20,
                             NULL);

  _lav = XtVaCreateManagedWidget (
                             "lav",               xmLabelWidgetClass,
                             topWidget(),
                             XmNleftAttachment,   XmATTACH_WIDGET,
                             XmNleftWidget,       lavlab,
                             XmNleftOffset,       125,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _coord_hwds->W(),
                             XmNbottomOffset,     20,
                             NULL);

  Widget phantomlabR = XtVaCreateManagedWidget (
                             "",                 xmLabelWidgetClass,
                             topWidget(),
                             XmNleftAttachment,  XmATTACH_WIDGET,
                             XmNleftWidget,      _lav,
                             XmNleftOffset,      50,
                             XmNrightAttachment, XmATTACH_FORM,
                             NULL);

  _hstot = XtVaCreateManagedWidget (
                             "hstot",             xmLabelWidgetClass,
                             topWidget(),
                             XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                             XmNleftWidget,       _lav,
                             XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                             XmNrightWidget,      _lav,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _lav,
                             XmNbottomOffset,     15,
                             NULL);

  _xltot = XtVaCreateManagedWidget (
                             "xltot",            xmLabelWidgetClass,
                             topWidget(),
                             XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                             XmNleftWidget,      _lav,
                             XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
                             XmNrightWidget,     _lav,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _hstot,
                             XmNbottomOffset,     5,
                             NULL);

  _lntot = XtVaCreateManagedWidget (
                             "lntot",            xmLabelWidgetClass,
                             topWidget(),
                             XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                             XmNleftWidget,      _lav,
                             XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
                             XmNrightWidget,     _lav,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _xltot,
                             XmNbottomOffset,     5,
                             NULL);

  Widget totlab = XtVaCreateManagedWidget (
                             "totlab",           xmLabelWidgetClass,
                             topWidget(),
                             XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
                             XmNleftWidget,      _lav,
                             XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
                             XmNrightWidget,     _lav,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _lntot,
                             XmNbottomOffset,     5,
                             NULL);
  /*
  Widget tmp = XtVaCreateManagedWidget (
                             "",                 xmLabelWidgetClass,
                             topWidget(),
                             XmNleftAttachment,  XmATTACH_FORM,
                             XmNleftOffset,      5,
                             XmNtopAttachment,   XmATTACH_FORM,
                             XmNtopOffset,       50,
                             NULL);
  */

  XtVaSetValues (_infile->W(),
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       5,
                             XmNrightAttachment,  XmATTACH_FORM,
                             XmNrightOffset,      5,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     totlab,
                             XmNbottomOffset,     50,
                             XmNfileTargetAddr,  _cube_file,
                             NULL);

  Cube *cube = _cube_display->currentDisplayedCube ();
  if (strlen(cube->primaryFilename()) > 0) {
    _infile->setFilename (cube->primaryFilename());
  }
  
  Widget phantomlabT = XtVaCreateManagedWidget (
                             "",                  xmLabelWidgetClass,
                             topWidget(),
                             XmNtopAttachment,    XmATTACH_FORM,
                             XmNtopOffset,        25,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _infile->W(),
                             XmNbottomOffset,     25,
                             NULL);
  
  set ();
  ValidInput ();
  return topWidget ();
  
}

void CubeSelect::manage ()
{
  Widget w;
  char *fname;

  Boolean deleting_file = deletingCube ();
  if (!deleting_file) SLBase::manage();

  if(!_first_time)
    {
    if(!_changing_file && !deleting_file)
      {
      _previous_cube = _cube_display->currentDisplayedCube ();
      _added_cube = _cube_display->newCube ();
      _previous_cube->callCubeIsNolongerCurrent (_added_cube);
      }
    }


  XmProcessTraversal (_infile->W(), XmTRAVERSE_CURRENT);

  if (!_first_time && !deleting_file)
    set (_cube_display->currentDisplayedCube());

  else if (_first_time) {
    fname = _infile->filename ();
    if (strlen(fname)) {
/*
 *     wprocFileChoiceSetFlags (_infile, wprocMustExistMask);
 *     wprocFileChoiceValidate (_infile, True);
 *     wprocFileChoiceSetFlags (_infile,
 *     wprocMustExistMask|wprocIsRequiredMask);
 */
       _first_time= False;
    }
 //    XtFree(fname);
  }
  netConnectGood();
}

void CubeSelect::cubeIsNolongerCurrent (Cube * /*cube*/, Cube *newcube)
{
  if (newcube && !find(newcube)) addCube (newcube);
  if (newcube == _cube_display->currentDisplayedCube()) change ();
}

void CubeSelect::destroyed (Cube *cube)
{
  if (cube == _cube_display->currentDisplayedCube()) {
    set ();
  }
}

void CubeSelect::change ()
{
  set (_cube_display->currentDisplayedCube());
}

void CubeSelect::set (Cube *cube)
{
  Widget textw/*, annow, fsbpopw, errpop*/;
  if (cube) {
    if (!_first_time) {
      if (strcmp(_infile->filename(),cube->primaryFilename())) {
/*
 *      wprocFileChoiceSetFile (_infile, cube->primaryFilename(), False);
 *      wprocFileChoiceRetWidgets (_infile, &textw, &annow, &fsbpopw, &errpop);
 */
	textw = _infile->text()->W();
        XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
      }
    }
    if (_hstot) wprocVAShowMsg (_hstot, "%d", cube->totalTimeSlices());
    if (_lntot) wprocVAShowMsg (_lntot, "%d", cube->totalLines());
    if (_xltot) wprocVAShowMsg (_xltot, "%d", cube->totalCrossLines());
    if (_lav)   wprocVAShowMsg (_lav,   "%f", (float)cube->getMaxAmp());
    cube->transformHeaders (&_lnhw, &_xlhw);
    _coord_hwds->SetValue ((int)LNHW, (long)_lnhw);
    _coord_hwds->SetValue ((int)XLHW, (long)_xlhw);
    _cube_annotation->setLineHeader      (_lnhw);
    _cube_annotation->setCrosslineHeader (_xlhw);
 
    /*
    if (cube->netEnv() == NULL) {
      _host_ops->setOptionValue (LOCAL);
       getDataFrom(LOCAL);
    }
    else {
      _host_ops->setOptionValue (CRAY_SP);
       getDataFrom(CRAY_SP);
       }
    */
  }
  else {
    if (!_first_time) {
      _infile->setFilename ("");
      textw = _infile->text()->W();
      XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
    }
    int zero = 0;
    if (_hstot) wprocVAShowMsg (_hstot, "%d", zero);
    if (_lntot) wprocVAShowMsg (_lntot, "%d", zero);
    if (_xltot) wprocVAShowMsg (_xltot, "%d", zero);
    if (_lav)   wprocVAShowMsg (_lav,   "%f", (float)zero);
  }


  if (made()) {
    wprocShowMsg (_coord_hwds->LabW(LNHW), "Line Header: ");
    wprocShowMsg (_coord_hwds->LabW(XLHW), "Crossline Header: ");
  }
}

void CubeSelect::filein (long ident, char *oldvar, char *newvar)
{
  if (!unique (_infile->filename())) {
    strcpy (_fail_str, REPEATFILE);
    _cube_file_state = REPEATED;
  }
  else {
    Cube *cube = _cube_display->currentDisplayedCube ();
    if (!cube && _cube_display->count() == 0) {
      cube = _cube_display->newCube ();
    }
    if (cube) {
      cube->setCubeTrcioHeaders(_lnhw, _xlhw);
      if (cube->setPrimaryFilename(_infile->filename(),False) &&
          cubeTrcioIsOk(cube)) {
        set (cube);
        _first_time = False;
	_fail_str[0] = '\0';
	_cube_file_state = GOOD;
      }
      else {
	strcpy (_fail_str, CUBENOTSET);
	_cube_file_state = NOTSET;
      }
    }
    else {
      strcpy (_fail_str, BADCUBE);
      _cube_file_state = BAD;
    }
  }

  netConnectGood();
}

Boolean CubeSelect::ValidInput ()
{
  Boolean retval;
  char error_string[360];

  if (made() && !_first_time) {
    if (XtIsManaged(topWidget())) {
      switch (_cube_file_state) {
      case BAD :
      case NOTSET :
      case NONE :
      case REPEATED :
	if (_infile->filename() && strlen(_infile->filename())) {
	  strcpy (error_string, _infile->filename());
	  if (strlen(_fail_str)) {
	    strcat (error_string, "\n ");
	    strcat (error_string, _fail_str);
	  }
	}
	else if (strlen(_fail_str)) {
	  strcpy (error_string, _fail_str);
	}
	retval = False;
	break;
      case GOOD :
	retval = True;
	break;
      default:
	assert (0);
      }
      if (!retval) {
	ErrorHandler err = W();
	err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
	err.deliverError (error_string);
	set ();
      }
    }
    else {
      retval = True;
    }
  }
  else {
    retval = True;
  }
  return retval;
}

Boolean CubeSelect::ValidSize (Cube *cube)
{
  Boolean stat = True;

  if (cube->totalLines()      <= 1 ||
      cube->totalCrossLines() <= 1 ||
      cube->totalTimeSlices() <= 1   ) stat = False;

  return stat;
}

void CubeSelect::cancelButton()
{
Cube *cube = _cube_display->currentDisplayedCube ();

  // See if we are adding a cube. If so make sure it is ready.
  if (_added_cube == cube) {
    if (_previous_cube) {
      _previous_cube->resetCubeSlice ();
    }
    delete _added_cube;
  }
  _added_cube = NULL;
  if (_previous_cube == NULL) _first_time = True;
  
  SLFPopSep::cancelButton();
}

Boolean CubeSelect::deletingCube ()
{
  Boolean retval;

  if (_deleting_file) {
    retval = True;
    _deleting_file = False;
  }
  else {
    retval = False;
  }
  return retval;
}

void CubeSelect::DoAction ()
{
  Cube *cube = _cube_display->currentDisplayedCube ();

  // See if we are adding a cube. If so make sure it is ready.
  if(cube && _added_cube == cube)
    {
    if (!cube->validCubeFile()) 
      {
      delete _added_cube; _added_cube = NULL;
      return;
      }
    }
  _added_cube = NULL;

// check if the current cube is valid
  if (!cube->validCubeFile()) {
    char error_string[60];
    sprintf (error_string, ISNOTAFILE);
    ErrorHandler err = W();
    err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
    err.deliverError (error_string);
    return;
  }

// check if the current cube dimensions are valid
  if (!ValidSize(cube)) {
    char error_string[60];
    sprintf (error_string, ISNOTVALIDSIZE);
    ErrorHandler err = W();
    err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
    err.deliverError (error_string);
    return;
  }

// check if the current cube matches the other cubes within the cube display
  Boolean matches = _cube_display->cubeMatches (cube);
  if (!matches) {
    char error_string[64];
    sprintf (error_string, FILEMISMATCH);
    ErrorHandler err = W();
    err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
    err.deliverError (error_string);
    _cube_display->displayPrevCube ();
    delete cube;
    return;
  }

// set the annotation text
  _cube_annotation->setLineHeader      (_lnhw);
  _cube_annotation->setCrosslineHeader (_xlhw);

// set the cube coordindate headers, scaling and annotation parameters
  cube->setTransformHeaders (_lnhw, _xlhw);
  _cube_section->setParams ();
  _cube_amplitude->setParams ();
  _cube_annotation->setParams ();

  SeisColorPop *color_pop = CubeMaster::instance()->colorPop (_cube_display);
  if (!color_pop->beenManaged()) {
// intialize the color amplitude extrema for the cube display (for TimeSlices)
    SeisPlot *sp = 0;
    if (cube->inlineSP())         { sp = cube->inlineSP();    }
    else if (cube->crosslineSP()) { sp = cube->crosslineSP(); }
    if (sp) {
      color_pop->setAmplitudes(-sp->maxDataAmp(),sp->maxDataAmp());
      cube->setMinColorAmp (-sp->maxDataAmp());
      cube->setMaxColorAmp ( sp->maxDataAmp());
    }
  }

// attempt to plot the current cube
  ShellWatch sw;
  Boolean successful = cube->plot ();
  if (!successful) {
    manage ();
    CubePlotError error (W(), cube);
  }

  netConnectGood();
}

void CubeSelect::fileCallback (void *data, long ident, char *oldvar,
  char *newvar)
{
  CubeSelect *cube_select = (CubeSelect *)data;
  cube_select->filein (ident, oldvar, newvar);
}

Boolean CubeSelect::unique (char *filename)
{
  void *x;
  Cube *cube;
  for (cube = _cube_display->top(&x); cube; cube = _cube_display->next(&x)) {
    if (!strcmp (cube->primaryFilename(), filename)) {
      //      return False;
      return True;
    }
  }
  return True;
}

int CubeSelect::cubeTrcioIsOk (Cube *cube)
{
  int retval;
  char *fn;
  size_t fnl;

  if (cube && cube->primaryFilename()) {

    if (!cube_trcio_supported (cube->primaryFilename())) {
      return 1; // don't penalize the obsolete file types
    }

// cube exists and has been set, continue...
    CubeTrcio *cubetrcio = cube->inlineSP()->getCubeTrcio ();

    if (cubetrcio) {
// an instance of CubeTrcio exists, continue...
      if (!cube_trcio_csvFileExists(cubetrcio)) {

// the auxiliary file does not exist where the data is so
//   get a copy of the auxiliary file name
        fnl = strlen (cube_trcio_csvName(cubetrcio)) + 1;
        fn  = (char *)malloc (fnl);
        strcpy (fn, cube_trcio_csvName(cubetrcio));

// reset the csv_name to be located in CPSDATA/ and continue...
        cube_trcio_setCsvNamePath(cubetrcio, "~/cpsdata/");

        if (!cube_trcio_csvFileExists(cubetrcio)) {
// the auxiliary file does not exist there either so try to create the
//   file where the data is
          cube_trcio_setCsvName (cubetrcio, (const char *)fn);
          free (fn);

// register the DoAbortWithStatus function with CubeTrcio
          _do_abort_w_status = new DoAbortWithStatus (topWidget(),
            getHelpCtx());
	  cube_trcio_setStatusFunction (cubetrcio,
            (StatusFunction)
            DoAbortWithStatus::altUserAbortAndStatus,
            (void *)_do_abort_w_status);

          _do_abort_w_status->setNewAction (CUBEALLOC);
          if (!cube_trcio_initializeCsvFile(cubetrcio)) {
// priveledges and space are available for the auxiliary file where the
//   data is so write the data
            _do_abort_w_status->actionComplete ();
            _do_abort_w_status->setNewAction (CUBECREATE);
            if (!cube_trcio_writeCsvData(cubetrcio)) {

// the data was successfully written
              retval = 1;
            }

            else {
// an error was returned when trying to write the data
              retval = 0;
            }
            _do_abort_w_status->actionComplete ();
          }

          else {
// either priveledges or space were not available for the auxiliary file
//   where the data is so try to create the file in CPSDATA/
            _do_abort_w_status->actionComplete ();
            cube_trcio_setCsvNamePath (cubetrcio, "~/cpsdata/");

            _do_abort_w_status->setNewAction (CUBEALLOC);
            if (!cube_trcio_initializeCsvFile(cubetrcio)) {
// priveledges and space were available for the auxiliary file in CPSDATA/
//   so write the data
              _do_abort_w_status->actionComplete ();
              _do_abort_w_status->setNewAction (CUBECREATE);
              if (!cube_trcio_writeCsvData(cubetrcio)) {

// the data was successfully written
                retval = 1;
              }

              else {
// an error was returned when trying to write the data
                retval = 0;
              }
              _do_abort_w_status->actionComplete ();
            }
            else {
              _do_abort_w_status->actionComplete ();
            }
          }
	  cube_trcio_setStatusFunction (cubetrcio,
            (StatusFunction)
            DoAbortWithStatus::altUserAbortAndStatus, (void *)0);
          delete _do_abort_w_status;
        }
        else {
// the auxiliary file exists in CPSDATA/
          if (cube_trcio_matchesCsvFile(cubetrcio)) {
// the auxiliary file matches the TRCIO data
            retval = 1;
          }
          else {
// the auxiliary file does not match the TRCIO data
            retval = 0;
          }
        }
      }
      else {
// the auxiliary file exists where the data is
        if (cube_trcio_matchesCsvFile(cubetrcio)) {
// the auxiliary file matches the TRCIO data
          retval = 1;
        }
        else {
// the auxiliary file does not match the TRCIO data
          retval = 0;
        }
      }
    }
    else {
// the instance of CubeTrcio is NULL
      retval = 0;
    }
  }
  else {
// cube is NULL or the primary filename has not been set
    retval = 0;
  }
  return retval;
}



Boolean CubeSelect::netConnectGood()
{
 Boolean retval= True;
 /*
 if (_netenv != NULL && _netenv->networkStatus() == NetEnv::Bad) {
        ErrorHandler err= W();
        err.deliverError(
               "Lost Connection to remote machine\nResetting to local.");
        _host_ops->setOptionValue (LOCAL);
        getDataFrom(LOCAL);
        retval= False;
 }
 */
 return retval;
}


void CubeSelect::getDataFrom(int where)
{
  /*
   Cube *cube = _cube_display->currentDisplayedCube ();
   if (where == CRAY_SP) {
       PsuedoWidget res_pw= PsuedoWidget( W(), NULL );
       int port=   res_pw.anyIntDef("sp-port", "Sp-Port");
       char *host= res_pw.anyDef(   "sp-host", "Sp-Host");

       _netenv= NetEnv::getNetEnv(host, port);

       if (_netenv->networkStatus() == NetEnv::Bad) _netenv->redoConnection();
       if (_netenv->networkStatus() == NetEnv::Good) {
              cube->setNetEnv(_netenv);
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
       cube->setNetEnv(NULL);
       if ( _alt_access.objptr) *_alt_access.objptr= NULL;
   }
   else
       assert(0);
  */
}

void CubeSelect::deleteFile ()
{
  Cube *cube = _cube_display->currentDisplayedCube ();
  assert (cube != NULL);
  _changing_file = True;
  if (_cube_display->count() == 1) cube->ignoreAnnotationRequest (True);
  delete cube;
  _deleting_file = True;
  if (_cube_display->count() < 1) {
    _changing_file = False;
  }
}


void CubeSelect::setNetEnvPtr(NetEnv **netenv)
{
  Cube *cube = _cube_display->currentDisplayedCube ();
/*
 *_alt_access.objptr=           netenv;
 *_alt_access.access_func=      NetEnv::access;
 *_alt_access.exp_tilde_func=   NetEnv::expTilde;
 *_alt_access.mod_type_func=    NetEnv::modTime;
 *_alt_access.exp_file_func=    NetEnv::expFile;
 *_alt_access.get_dir_func=     NetEnv::dir;
 *_alt_access.get_dir_dir_func= NetEnv::dirOfDirectories;
 */
  cube->setNetEnv(*netenv);
//if (made()) wprocFileChoiceSetAltFileAccess(_infile, &_alt_access);
}


void CubeSelectPush::pushAction (long ident)
{
  
  if (ident == DISPLAY) {
    _cube_select->_cube_section->dontPlotYet ();
    _cube_select->_cube_section->makeAndManage (XtParent(W()));
  }
  else if (ident == ANNOTATE) {
    _cube_select->_cube_annotation->dontPlotYet ();
    _cube_select->_cube_annotation->makeAndManage (XtParent(W()));
  }
  else if (ident == COLOR) {
    _cube_select->_cube_display->colorPop()->dontPlotYet (True);
    _cube_select->_cube_display->colorPop()->makeAndManage (XtParent(W()));
  }
  else if (ident == AMPLITUDE) {
    _cube_select->_cube_amplitude->dontPlotYet ();
    _cube_select->_cube_amplitude->makeAndManage (XtParent(W()));
  }
}
