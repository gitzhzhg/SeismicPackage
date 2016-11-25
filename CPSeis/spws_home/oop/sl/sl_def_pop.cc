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
#include "sl/sl_def_pop.hh"
#include "sl/slp_file_data.hh"
#include "sl/sl_file_selection_pop.hh"

#include "wproc.h"


static String  defres[]= {
  NULL,
  NULL,
  "*file.labelString:                Defaults File:",
  "*file.fileDescription:            Defaults file",
  "*file.fileExtension:              ad",
  "*file*XmFileSelectionBox.dirMask: *.ad",
  "_popup.title:              Defaults Popup",
  ".width:                  550",
  ".height:                 150",
  NULL };


SLDefPop::SLDefPop (Widget p, char *name, HelpCtx hctx,
  SLpFileData *slp_file_data, void *data, Boolean make_now) :
  SLFormFCPop (p, name, FP_DOALL, slp_file_data, hctx, True, False),
  _data  (data)
{
  if (make_now) make (p);
  else          supportUnmadeDefaults (p);
}

SLDefPop::~SLDefPop ()
{
}

Widget SLDefPop::make (Widget p)
{
 
  if (!made()) {
    SLFormFCPop::make (p);
    SLFileSelectionPop *filebox = _slp_file->filebox ();
    if (filebox) {
      // specify the directory
      char defdir[200];
      Deffile_path ( "", defdir);
      filebox->setDirectory (defdir);
    }

    XtVaSetValues (_slp_file->W(),
		   XmNtopAttachment,    XmATTACH_FORM,
		   XmNtopOffset,        10,
		   XmNleftAttachment,   XmATTACH_FORM,
		   XmNleftOffset,       5,
		   XmNrightAttachment,  XmATTACH_FORM,
		   XmNrightOffset,      5,
		   XmNbottomAttachment, XmATTACH_WIDGET,
		   XmNbottomWidget,     bottomSeparator(),
		   XmNbottomOffset,     10,
		   NULL);

    defaultButtonOK (True);
    ValidInput ();
  }
  return parentOfChildren ();
}

char *SLDefPop::defFile ()
{
  char *retval;
  if (_slp_file) {
    retval = _slp_file->filename ();
  }
  else {
    retval = NULL;
  }
  return retval;
}

void SLDefPop::DoAction ()
{
 defFile ();
 if (_slp_file->type() == SLpFile::_OUTPUT) {
   saveFile (_slp_file->filename(), _data);
 }
 else {
   getFile (_slp_file->filename(), _data);
 }
}
