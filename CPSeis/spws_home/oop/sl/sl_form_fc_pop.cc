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
#include "sl/sl_form_fc_pop.hh"
#include "sl/error_handler.hh"
#include "sl/slp_file_data.hh"
#include "sl/slp_push.hh"
#include "sl/slp_label.hh"

#include "exptilde_crou.h"
#include "inquire.h"

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Text.h>
#include <X11/Xatom.h>

static String  defres[] = {
  "*file.topAttachment:     ATTACH_POSITION",
  "*file.topPosition:       10",
  "*file.leftAttachment:    ATTACH_FORM",
  "*file.rightAttachment:   ATTACH_FORM",
  "*file.leftOffset:        5",
  "*file.rightOffset:       5",
  NULL
};

SLFormFCPop::SLFormFCPop (Widget p,  char *name, unsigned long buttons, 
  SLpFileData *slp_file_data, HelpCtx hctx, Boolean small_on_dpi,
  Boolean make_now) :
  SLFPopSep (p, name, buttons, hctx, small_on_dpi, False),
  _slp_file       (NULL),
  _state          (NONE),
  _first_time     (True),
  _slp_file_data  (slp_file_data)
{
  init (XtDisplay(p));

  _slp_file = new SLpFile (this, _slp_file_data);
  _slp_file->setCtrap (FileSuccessCallback, this);

  if (make_now) {
    make (p);
  }
  else {
    supportUnmadeDefaults (p);
  }

  setFilename (0);
}

Widget SLFormFCPop::make (Widget p)
{
  if (!made()) {
    setDefaultResources (XtDisplay(wParent()), _name, defres);
    SLFPopSep::make (p);

    SLpPush  *push  = _slp_file->push  ();
    SLpLabel *label = _slp_file->label ();
    Widget text     = _slp_file->text  ()->W();
    if (getHelpCtx()) {
                 add_HELP (text      , helper, getHelpCtx());
      if (push ) add_HELP ( push->W(), helper, getHelpCtx());
      if (label) add_HELP (label->W(), helper, getHelpCtx());
    }
  }
  return topWidget ();
}

void SLFormFCPop::manage ()
{
  SLBase::manage ();

  XmProcessTraversal (_slp_file->W(), XmTRAVERSE_CURRENT);

  if (_first_time) {
    if (strlen(_slp_file->filename())) _first_time = False;
  }
  setTitle ((char *)_slp_file_data->filetype());
}

Boolean SLFormFCPop::ValidInput ()
{
  Boolean retval;
  char error_string[360];

  if (made() && !_first_time && XtIsManaged(topWidget())) {
    if (_state == BAD) {
      if (_slp_file->filename() && strlen(_slp_file->filename())) {
	if (_slp_file->type() == SLpFile::_INPUT) {
	  sprintf (error_string, "%s must be readable!",
            _slp_file->filename());
	}
	else {
	  sprintf (error_string, "%s must be writable!",
            _slp_file->filename());
	}
      }
      else {
	if (_slp_file->type() == SLpFile::_INPUT) {
	  sprintf (error_string, "Input file is invalid!");
	}
	else {
	  sprintf (error_string, "Output file is invalid!");
	}
      }
      if (made()) {
        ErrorHandler err = W();
        err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
        err.deliverError (error_string);
      }
      retval = False;
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

Boolean SLFormFCPop::setFilename (char *filename)
{
  Boolean retval;
  char fname[300];

  if (filename) {
    exptilde_crou2 (fname, filename);

    if (validateFile(fname)) {
      // file is accessible
      _state = GOOD;
      _slp_file->setFilename (fname);
      retval = True;
    }
    else if (fname[0] == '\0' || !strcmp(fname,"NONE") ||
      !strcmp(fname,"none") || !strcmp(fname,"None")) {
      // file is not specified
      _state = NONE;
      _slp_file->setFilename (fname);
      retval = False;
    }
    else {
      // file is inaccessible
      char error_string[60];
      sprintf (error_string,
        "SLFormFCPop::setFilename:  Could not access file\n %s",
        filename);
      if (made()) {
        ErrorHandler err = W();
        err.setEtype (ErrorHandler::Error, ErrorHandler::CUI, False);
        err.deliverError (error_string);
      }
      _state = BAD;
      retval = False;
    }
  }
  else {
    _state = NONE;
    _slp_file->setFilename ("NONE");
    retval = True;
  }
  return retval;
}

SLFormFCPop::~SLFormFCPop ()
{
  delete _slp_file;
}

void SLFormFCPop::init (Display *)
{
  // only do things in derived classes as necessary
}

Boolean SLFormFCPop::validateFile (char *filename)
{
  Boolean retval;
  long status;
  char msg[300];

  if (_slp_file->type() == SLpFile::_INPUT) {
    // specifying a file to read
    status = inquire_input (filename, msg);
  }
  else if (_slp_file->type() == SLpFile::_OUTPUT) {
    // specifying a file to save
    status = inquire_output (filename, msg);
  }

  if (status == FILE_ERROR) {
    retval = False;
  }
  else {
    retval = True;
  }
  return retval;
}

void SLFormFCPop::FileSuccessCallback (void *data, long ident, char *oldvar,
  char *newvar)
{

 SLFormFCPop *obj = (SLFormFCPop *)data;
 obj->FileSuccess (ident, oldvar, newvar);
}

void SLFormFCPop::FileSuccess (long ident, char *oldvar, char *newvar)
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

  if (newfile && setFilename(newvar)) {
    _first_time = False;
  }
}
