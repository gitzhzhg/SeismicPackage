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
// class that creates the file I/O gui for a color bar builder

#include "color/cbb_col_fio_gui.hh"
#include "color/cbb_col_set_gui.hh"
#include "color/color_bar_builder_pop.hh"
#include "color/color_file_io.hh"
#include "sl/psuedo_widget.hh"
#include "sl/error_handler.hh"
#include "sl/slp_file.hh"

#include "wproc.h"
#include "cprim.h"
#include "exptilde_crou.h"
#include "inquire.h"

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <sys/stat.h>
#include <unistd.h>

#define INFILE_NAME  "infile"
#define OUTFILE_NAME "outfile"
#define BADFILE      " was rejected as a\ncolor bar file."
#define FILENOTSET   " could not be set\nas a color bar file."

#define TITLE_LABEL   "File I/O"
#define INFILE_IDENT  0
#define INFILE_LABEL  "Input File...  "
#define INFILE_TYPE   "Input Color Bar File"
#define INFILE_EXT    "rgb"
#define OUTFILE_IDENT 1
#define OUTFILE_LABEL "Output File:"
#define OUTFILE_TYPE  "Output Color Bar File"
#define OUTFILE_EXT   "rgb"

// The following is not used currently
static String defres[] = {
  "*title.labelString:              File I/O",
  "*infile.labelString:             Input File...",
  "*infile.fileDescription:         Input Color Bar File",
  "*infile.fileExtension:           rgb",
  "*infile.fileFlags:               MustExist",
  "*infile.annoType:                pushbutton",
  "*infile*Fileshell*dirMask:       *.*rgb*",
  "*outfile.labelString:            Output File...",
  "*outfile.fileDescription:        Output Color Bar File",
  "*outfile.fileExtension:          rgb",
  "*outfile.fileFlags:              Writable",
  "*outfile.annoType:               label",
  "*outfile*Fileshell*dirMask:      *.*rgb*",
  0};

CBBColFIOGui::CBBColFIOGui (Widget parent, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (parent, name, hctx, True),
  _in_file_state    (NONE),
  _out_file_state   (NONE),
  _first_time       (True),
  _cbb_pop          (cbb_pop),
  _infile           (NULL),
  _outfile          (NULL)
{
  setDefaultResources (parent, name, defres);
  init ();
}

CBBColFIOGui::CBBColFIOGui (SLDelay *container, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (container, name, hctx, True),
  _in_file_state    (NONE),
  _out_file_state   (NONE),
  _first_time       (True),
  _cbb_pop          (cbb_pop),
  _infile           (NULL),
  _outfile          (NULL)
{
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
    make (container->topWidget());
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
  init ();
}

void CBBColFIOGui::init ()
{
  _infile = new SLpFile (this, INFILE_NAME, INFILE_IDENT, INFILE_LABEL,
    INFILE_TYPE, INFILE_EXT);

  _infile->setCtrap (fileCallback, this);

  char *infile = _pw_topw->childFileChoiceDef (INFILE_NAME);
  if (infile) {
    strcpy (_input_file, infile);
    setInFile (_input_file);
  }
  else {
    _input_file[0] = '\0';
    setInFile (0);
  }

  _outfile = new SLpFile (this, OUTFILE_NAME, OUTFILE_IDENT, OUTFILE_LABEL,
    OUTFILE_TYPE, OUTFILE_EXT);

  _outfile->setCtrap (fileCallback, this);

  char *outfile = _pw_topw->childFileChoiceDef (OUTFILE_NAME);
  if (outfile) {
    strcpy (_output_file, outfile);
    setOutFile (_output_file);
  }
  else {
    _output_file[0] = '\0';
    setOutFile (0);
  }

// setInFilename (0);
//setOutFilename (0);
}

CBBColFIOGui::~CBBColFIOGui ()
{
  delete _infile;
  delete _outfile;
}

Widget CBBColFIOGui::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make (parent);

  if (strlen( _in_filename) > 0)  _infile->setFilename ( _in_filename);
  if (strlen(_out_filename) > 0) _outfile->setFilename (_out_filename);

  Widget title = XtVaCreateManagedWidget ("File I/O",
                             xmLabelWidgetClass,  topWidget(),
			     XmNtopAttachment,    XmATTACH_FORM,
			     XmNtopOffset,        1,
                             XmNleftAttachment,   XmATTACH_FORM,
			     XmNleftOffset,       25,
                             XmNrightAttachment,  XmATTACH_FORM,
			     XmNrightOffset,      25,
                             NULL);

  XtVaSetValues (_infile->W(),
		             XmNtopAttachment,    XmATTACH_WIDGET,
		             XmNtopWidget,        title,
			     XmNtopOffset,        1,
                             XmNleftAttachment,   XmATTACH_FORM,
		             XmNleftOffset,       10,
                             XmNrightAttachment,  XmATTACH_FORM,
		             XmNrightOffset,      10,
                             NULL);

  XtVaSetValues (_outfile->W(),
		             XmNtopAttachment,    XmATTACH_WIDGET,
		             XmNtopWidget,        _infile->W(),
		             XmNtopOffset,        1,
		             XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
		             XmNleftWidget,       _infile->W(),
                             XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
		             XmNrightWidget,      _infile->W(),
                             NULL);
/*
  Widget title = XtVaCreateManagedWidget ("title",
                             xmLabelWidgetClass,  topWidget(),
			     XmNtopAttachment,    XmATTACH_FORM,
                             XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                             XmNleftWidget,       _outfile,
                             XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                             XmNrightWidget,      _outfile,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _infile->W(),
                             XmNbottomOffset,     10,
                             NULL);

  XtVaSetValues (_infile->W(),
                             XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                             XmNleftWidget,       _outfile,
                             XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                             XmNrightWidget,      _outfile,
                             XmNbottomAttachment, XmATTACH_WIDGET,
                             XmNbottomWidget,     _outfile,
                             XmNbottomOffset,     10,
                             NULL);

  XtVaSetValues (_outfile->W(),
                             XmNleftAttachment,   XmATTACH_FORM,
                             XmNleftOffset,       5,
                             XmNrightAttachment,  XmATTACH_FORM,
                             XmNrightOffset,      5,
                             XmNbottomAttachment, XmATTACH_FORM,
                             XmNbottomOffset,     10,
                             NULL);
*/
  set ();
  ValidInput ();
  return topWidget ();
}

void CBBColFIOGui::manage ()
{
  Widget widget;
  char *fname;
  SLBase::manage();

  XmProcessTraversal (_infile->W(), XmTRAVERSE_CURRENT);

  if (!_first_time)
    set ();

  else if (_first_time) {
    fname = _infile->filename ();
    if (strlen(fname)) {
/*
 *     wprocFileChoiceSetFlags (_infile, wprocMustExistMask);
 *     wprocFileChoiceValidate (_infile, True);
 *     wprocFileChoiceSetFlags (_infile, wprocMustExistMask);
 */
       _first_time = False;
    }
    fname = _outfile->filename ();
    if (strlen(fname)) {
/*
 *     wprocFileChoiceSetFlags (_outfile, wprocWritableMask);
 *     wprocFileChoiceValidate (_outfile, True);
 */
       _first_time = False;
    }
    // XtFree (fname);
  }
}

char *CBBColFIOGui::outputFile ()
{
  if (_out_file_state != BAD) return _out_filename;
  else                        return (char *)0;
}

Boolean CBBColFIOGui::setInFilename (char *infile)
{
  Boolean retval;
  char fname[300];
  struct stat file_stats;
  long status;
  char msg[300];

  if (infile) {
    int new_file;
    if (inFileNew(infile)) {
      new_file = 1;
    }
    else if (inFileDifferent(infile)) {
      new_file = 1;
    }
    else {
      new_file = 0;
    }

    if (new_file) {
      exptilde_crou2 (fname, infile);
      strcpy (_in_filename, fname);

      status = inquire_input (fname, msg);

      if (status != FILE_ERROR) {
// file is accessible
	stat (fname, &file_stats);
	_file_mod_time = file_stats.st_mtime;
// here read the data if possible and apply it
	retval = _cbb_pop->fileIO()->readColorFile (_cbb_pop, _in_filename);
	if (retval) {
          _in_file_state = GOOD;
	  strcpy (_input_file, infile);
	}
      }
      else if (fname[0] == '\0' || !strcmp(fname,"NONE") ||
        !strcmp(fname,"none") || !strcmp(fname,"None")) {
	_in_file_state = NONE;
	_input_file[0] = '\0';
	retval = True;
      }
      else {
	char error_string[60];
	sprintf (error_string,
          "CBBColFIOGui::setInFilename:  Could not find file");
	ErrorHandler err = W();
	err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
	err.deliverError (error_string);
	_in_file_state = BAD;
	retval = False;
      }
    }
    else {
      retval = _in_file_state == GOOD;
    }
  }
  else {
    _in_filename[0] = '\0';
    _in_file_state = NONE;
    _input_file[0] = '\0';
    retval = True;
  }
  return retval;
}

Boolean CBBColFIOGui::setInFile (char *filename)
{
  Widget textw;

  if (setInFilename(filename)) {
    if (strlen(_in_filename) > 0) {
      if (_infile) {
        if (!_infile->filename()                  ||
          strcmp(_infile->filename(),_in_filename)  ) {
	  _infile->setFilename (_in_filename);
        }
        if (made() && XtIsManaged(topWidget())) {
	  textw = _infile->text()->W();
          XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
        }
      }
    }
    else {
      if (_infile) {
	_infile->setFilename ("NONE");
        if (made() && XtIsManaged(topWidget())) {
	  textw = _infile->text()->W();
          XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
        }
      }
    }
    return True;
  }
  else return False;
}

Boolean CBBColFIOGui::inFileNew (char *check_file)
{
  Boolean is_new;
  char fname[300];
  struct stat file_stats;

  if (check_file) {
    exptilde_crou2 (fname, check_file);
 
    if (_in_filename && strcmp(fname,_in_filename) == 0) {
      if (stat(fname,&file_stats) == 0) {
	if (file_stats.st_mtime != _file_mod_time) {
          is_new = True;
	} // end if
        else {
          is_new = False;
	} // end else
      } // end if stat
      else {
        is_new = True;
      }
    } // end if strcmp
    else {
      is_new = True;
    }
  } // end if infile
  else {
    is_new = False;
  }
  return is_new;
}

Boolean CBBColFIOGui::inFileDifferent (char *check_file)
{
  Boolean is_different = False;
  char fname[300];

  if (check_file) {
    exptilde_crou2 (fname, check_file);
    is_different = _cbb_pop->colSetGui()->RGBNotEqualToFile(fname);
  }
  return is_different;
}

Boolean CBBColFIOGui::good (Boolean is_output)
{
  Boolean retval;

  if (is_output) {
    retval = _out_file_state == GOOD;
  }
  else {
    retval = _in_file_state == GOOD;
  }
  return retval;
}

Boolean CBBColFIOGui::none (Boolean is_output)
{
  Boolean retval;

  if (is_output) {
    retval = _out_file_state == NONE;
  }
  else {
    retval = _in_file_state == NONE;
  }
  return retval;
}

void CBBColFIOGui::set ()
{
  Widget textw;
  
  if (_in_filename) {
    if (!_first_time) {
      if (_infile && _infile->filename()        &&
        strcmp(_infile->filename(),_in_filename)  ) {
	_infile->setFilename (_in_filename);
	textw = _infile->text()->W();
        XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
      }
    }
  }
  else {
    if (!_first_time) {
      _infile->setFilename ("NONE");
      textw = _infile->text()->W();
      XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
    }
  }
  
  if (_out_filename) {
    if (!_first_time) {
      if (_outfile && _outfile->filename()        &&
        strcmp(_outfile->filename(),_out_filename)  ) {
	_outfile->setFilename (_out_filename);
	textw = _outfile->text()->W();
        XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
      }
    }
  }
  else {
    if (!_first_time) {
      _outfile->setFilename ("NONE");
      textw = _outfile->text()->W();
      XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
    }
  }
}

void CBBColFIOGui::filein (long ident, char *oldvar, char *newvar)
{
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

  if (ident == INFILE_IDENT) {
    if (newfile && setInFilename(newvar)) {
      set ();
      _first_time = False;
    }
  }
  else if (ident == OUTFILE_IDENT) {
    if (newfile && setOutFilename(newvar)) {
      set ();
      _first_time = False;
    }
  }
}

Boolean CBBColFIOGui::ValidInput ()
{
  Boolean retval;
  char error_string[360];

  if (made() && !_first_time && XtIsManaged(topWidget())) {
    if (_in_file_state == BAD) {
      if (_infile->filename() && strlen(_infile->filename())) {
	sprintf (error_string, "%s must be readable!", _infile->filename());
      }
      else {
	sprintf (error_string, "Input file is invalid!");
      }
      ErrorHandler err = W();
      err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
      err.deliverError (error_string);
      set ();
      retval = False;
    }
    else {
      retval = True;
    }

    if (_out_file_state == BAD) {
      if (_outfile->filename() && strlen(_outfile->filename())) {
	sprintf (error_string, "%s must be writable!", _outfile->filename());
      }
      else {
	sprintf (error_string, "Output file is invalid!");
      }
      ErrorHandler err = W();
      err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
      err.deliverError (error_string);
      set ();
      retval = False;
    }
  }
  else {
    retval = True;
  }
  return retval;
}

void CBBColFIOGui::fileCallback (void *data, long ident, char *oldvar,
  char *newvar)
{
  CBBColFIOGui *fio = (CBBColFIOGui *)data;
  fio->filein (ident, oldvar, newvar);
}

Boolean CBBColFIOGui::setOutFilename (char *outfile)
{
  Boolean retval;
  char fname[300];
  struct stat file_stats;
  long status;
  char msg[300];

  if (outfile) {
    exptilde_crou2 (fname, outfile);
 
    if (_out_file_state != NONE && strcmp(fname,_out_filename) == 0) {
      // A previously used filename
      status = inquire_output (fname, msg);
      if (status != FILE_ERROR) {
	stat (fname, &file_stats);
	_out_file_state = GOOD;
	strcpy (_output_file, outfile);
	retval = True;
      }
      else {
	_out_file_state = BAD;
	retval = False;
      }
    }
    else if (fname[0] == '\0' || !strcmp(fname,"NONE") ||
      !strcmp(fname,"none")) {
      _out_file_state  = NONE;
      strcpy (_out_filename, "NONE");
      strcpy (_output_file,  "NONE");
      retval = True;
    }
    else {
      // An unused filename
      _out_file_state = GOOD;
      strcpy (_out_filename, fname);
      strcpy (_output_file, outfile);
      retval = True;
    }
  }
  else {
    _out_file_state  = NONE;
    _out_filename[0] = '\0';
    _output_file[0] = '\0';
    retval = True;
  }
  return retval;
}

Boolean CBBColFIOGui::setOutFile (char *filename)
{
  Widget textw;

  if (setOutFilename(filename)) {
    if (strlen(_out_filename) > 0) {
      if (_outfile && _outfile->filename() && _out_filename) {
        if (strcmp(_outfile->filename(),_out_filename)) {
	  _outfile->setFilename (_out_filename);
        }
        if (made() && XtIsManaged(topWidget())) {
	  textw = _outfile->text()->W();
          XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
        }
      }
    }
    else {
      if (_outfile) {
	_outfile->setFilename ("NONE");
        if (made() && XtIsManaged(topWidget())) {
	  textw = _outfile->text()->W();
          XmTextSetInsertionPosition (textw, XmTextGetLastPosition (textw));
        }
      }
    }
    return True;
  }
  else return False;
}
