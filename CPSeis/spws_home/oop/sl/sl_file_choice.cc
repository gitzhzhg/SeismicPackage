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

//---------------------- sl_file_choice.cc ------------------------//
//---------------------- sl_file_choice.cc ------------------------//
//---------------------- sl_file_choice.cc ------------------------//

//         implementation file for the SLFileChoice class
//               derived from the SLSmartForm class
//                       subdirectory sl


#include "sl/sl_file_choice.hh"
#include "sl/slp_file.hh"
#include "sl/slp_label.hh"
#include "sl/sl_shell_container.hh"
#include "sl/sl_error_pop.hh"
#include "sl/sl_quest_pop.hh"
#include "oprim/file_base.hh"
#include "cprim.h"
#include <X11/StringDefs.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>




//------------------- shorthand macros -----------------------------//
//------------------- shorthand macros -----------------------------//
//------------------- shorthand macros -----------------------------//


#define III XtRInt   , sizeof(int   )
#define PPP XtRPixel , sizeof(Pixel )
#define SSS XtRString, sizeof(String)
#define JJ  XtRImmediate
#define SS  XtRString
#define RRR(n,i,p,j,c) \
   { n, "SLFileChoiceResources", i, XtOffsetOf(SLFileChoice,p), j, c },



//-------------------- get any widget ---------------------------//
//-------------------- get any widget ---------------------------//
//-------------------- get any widget ---------------------------//

      // SLDelay does not have to be made.

static Widget get_any_widget(SLDelay *gui)
{
  assert(gui);
  if(gui->W      ()) return gui->W      ();
  if(gui->wParent()) return gui->wParent();
  return get_any_widget(gui->slParent());
}



//------------------ set fallback resources -------------------//
//------------------ set fallback resources -------------------//
//------------------ set fallback resources -------------------//

          // private.

static char *defres1[] = {
   //   ".recomputeSize:  FALSE",
        ".fontList:       8x13bold",
   //   ".labelString:    X\nX",
   //   ".foreground:     dark orange",
        NULL };

static char *defres3[] = {
        ".foreground:     brown",
        NULL };

//// apparently, recomputeSize must be set in code AFTER
//// a label with the desired number of lines has been set.


void SLFileChoice::setFallbackResources()
{
  Widget any = get_any_widget(this);
  assert(any);

  setDefRes(XtDisplay(any), "file_choice_label_1", defres1);
  setDefRes(XtDisplay(any), "file_choice_label_3", defres3);

  static XtResource resources[] = {
      RRR("sl_file_choice_red"  , PPP, _pixel_red  , SS, (caddr_t)"red")
      RRR("sl_file_choice_green", PPP, _pixel_green, SS, (caddr_t)"green4")
      };

  XtGetApplicationResources(any, this, resources,
                                XtNumber(resources), NULL, 0);
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


SLFileChoice::SLFileChoice(SLDelay    *slparent,
                           char       *name,
                           int         io,
                           FileBase   *file,
                           const char *label,
                           HelpCtx     hctx,
                           Boolean     doframe,
                           Boolean     make_if_can,
                           Boolean     manage_now,
                           int         omit)
     : SLSmartForm(slparent, name, hctx, doframe, make_if_can, manage_now),
               _io           (io),
               _file         (file),
               _label        (NULL),
               _filefield    (NULL),
               _msg1field    (NULL),
               _msg2field    (NULL),
               _msg3field    (NULL),
               _dialog       (NULL),
               _fp_value     (FP_NONE),
               _pixel_red    (0),
               _pixel_green  (0)
{
  assert(_io == SLpFile::_INPUT || _io == SLpFile::_OUTPUT);
  assert(_file);

  setFallbackResources();
  if(label) _label = newstr(label);

  const char *filetype  = _file->getFileType();
  const char *extension = _file->getExtension();

  _filefield = new SLpFile     (this, "file", 0, label,
                                       filetype, extension, _io);

  if(!omit) _msg1field = new SLpLabel    (this, "file_choice_label_1");
            _msg2field = new SLpLabel    (this, "file_choice_label_1");
  if(!omit) _msg3field = new SLpLabel    (this, "file_choice_label_3");

  _filefield->setCtrap(fileTrap, this);


                //     LEFT    RIGHT      TOP       BOTTOM
  if(omit)
    {
    attach(_filefield, this  , this  ,    this    ,  NULL, 10, 10, 10);
    attach(_msg2field, this  , this  ,  _filefield,  this,  0,  0,  0,  5);
    }
  else
    {
    attach(_filefield, this  , this  ,    this    ,  NULL, 10, 10, 10);
    attach(_msg1field, this  , this  ,  _filefield,  NULL);
    attach(_msg2field, this  , this  ,  _msg1field,  NULL);
    attach(_msg3field, this  , this  ,  _msg2field,  this,  0,  0,  0,  5);
    }
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


SLFileChoice::~SLFileChoice()
{
  if(_label) free(_label);
}



//----------------------- file trap ----------------------------//
//----------------------- file trap ----------------------------//
//----------------------- file trap ----------------------------//

     // private static function.
     // called when user enters filename into file choice widget.

void SLFileChoice::fileTrap(void *data, long /*ident*/,
               char* /*oldvar*/, char* /*newvar*/)
{
  SLFileChoice   *THIS = (SLFileChoice*)data;
/*
  const char *filename = THIS->_filefield->cvar();
  if(THIS->_io == SLpFile::_INPUT)
          THIS->_file->setInputFilename (filename);
  else    THIS->_file->setOutputFilename(filename);
*/
/*******
          THIS->_file->setInputFilename (newvar);    // equivalent.
  else    THIS->_file->setOutputFilename(newvar);    // equivalent.
*******/
  THIS->updateFields();
  THIS->newFilenameEntered();   // virtual function.
  int status;
  if(THIS->_io == SLpFile::_INPUT) status = THIS->_file->inputStatus();
  else                             status = THIS->_file->outputStatus();
  THIS->callNotifyComplex(status);
}



//------------------ reset extension from file base --------------------//
//------------------ reset extension from file base --------------------//
//------------------ reset extension from file base --------------------//


void SLFileChoice::resetExtensionFromFileBase()
{
  const char *extension = _file->getExtension();
  _filefield->resetExtension(extension);
}



//-------------------- update fields from file base --------------------//
//-------------------- update fields from file base --------------------//
//-------------------- update fields from file base --------------------//

      // public.
      // first gets the filename from the FileBase object, and puts
      //   it into the file choice widget.
      // updates the various label fields which are displayed
      //   with the file choice widget.

void SLFileChoice::updateFieldsFromFileBase()
{
  const char *filename;
  if(_io == SLpFile::_INPUT) filename = _file->inputFilename();
  else                       filename = _file->outputFilename();
  _filefield->setCvar((char*)filename);
  privateUpdateFields();
}



//-------------------- update fields ----------------------------//
//-------------------- update fields ----------------------------//
//-------------------- update fields ----------------------------//

      // public.
      // first gets the filename from the file choice widget,
      //   and sets the filename into the FileBase object
      //   (just to make sure that the current filename is used).
      // updates the various label fields which are displayed
      //   with the file choice widget.
      // optionally called when the dialog box is managed, and
      //   any other time the status of the files on disk might
      //   have changed.
      // automatically called by this object when a new filename
      //   has been chosen, and after files are read or written.

   // NOTE: The commented code had to be replaced by making a local
   // copy of the filename.  This is because the name returned by cvar()
   // is a pointer to memory which is reallocated whenever cvar() is
   // called on any SLp... object.  It turns out that setInputFilename
   // and setOutputFilename call virtual functions which might cause
   // cvar() to be called before the filename is used.  In the case
   // where this behavior was found, setInputFilename was calling
   // a virtual function which in turn called beginSlowOperations(),
   // which in turn apparently called cvar() somewhere, but I have
   // not been able to find out where.

   // NOTE: cvar() can return NULL.

void SLFileChoice::updateFields()
{
  char filename[222];
  char *cvar = _filefield->cvar();
  if(cvar) strcpy(filename, cvar);
  else     strcpy(filename, "");
/****
  const char *filename = _filefield->cvar();
****/
  if(_io == SLpFile::_INPUT) _file->setInputFilename (filename);
  else                       _file->setOutputFilename(filename);
  privateUpdateFields();
}



//-------------------- private update fields ----------------------------//
//-------------------- private update fields ----------------------------//
//-------------------- private update fields ----------------------------//

      // private.
      // called by updateFields and updateFieldsFromFileBase.
      // assumes that the filename is in both the FileBase object and
      //   the file choice widget, and virtualValidate has already been
      //   called (when the filename was put into the FileBase object).
      // updates the various label fields which are displayed
      //   with the file choice widget.

void SLFileChoice::privateUpdateFields()
{
  const char *msg1;
  const char *msg2;
  const char *msg3;
  int error;
  if(_io == SLpFile::_INPUT)
      {
      msg1 = _file->inputStatusMsg();
      msg2 = _file->inputValidityInfo();
      msg3 = _file->lastFileReadMsg();
      error = (_file->inputStatus() == FileBase::INPUT_INVALID);
      }
  else
      {
      msg1 = _file->outputStatusMsg();
      msg2 = _file->outputValidityInfo();
      msg3 = _file->lastFileSavedMsg();
      error = (_file->outputStatus() == FileBase::OUTPUT_INVALID);
      }
  if(_msg1field) _msg1field->setCvar((char*)msg1);
  if(_msg2field) _msg2field->setCvar((char*)msg2);
  if(_msg3field) _msg3field->setCvar((char*)msg3);

  Pixel pixel;
  if(error) pixel = _pixel_red;
  else      pixel = _pixel_green;
  Widget w1 = NULL;
  Widget w2 = NULL;
  if(_msg1field) w1 = _msg1field->W();
  if(_msg2field) w2 = _msg2field->W();
  if(w1) XtVaSetValues(w1, XmNforeground, pixel, NULL);
  if(w2)
      {
      if(strlen(msg2) <= 1)
          {
          XtVaGetValues(w2, XmNbackground, &pixel, NULL);
          }
      XtVaSetValues(w2, XmNforeground, pixel, NULL);
      }
}



//------------------------ take action ------------------------//
//------------------------ take action ------------------------//
//------------------------ take action ------------------------//

    // public.
    // normally called from an OK or Apply pushbutton trap,
    //   and optionally from other similar traps (such as Cancel
    //   or Remove).
    // dialog should be the popup which will be unmanaged if the
    //   action on an OK button is successful.
    // dialog should also be the popup which will be notified of
    //   the final action (complex notify):
    //               FP_OK    if successful OK action.
    //               FP_APPLY if successful Apply action.
    //               FP_NONE  if unsuccessful OK or Apply action.
    //               fp_value if no action taken (anything else).
    // fp_value should be FP_OK         if the  OK   button was pressed.
    // fp_value should be FP_APPLY      if the Apply button was pressed.
    // fp_value should be anything else if any other button was pressed.
    //
    // initial action:
    //   (0) if fp_value is not FP_OK or FP_APPLY:
    //           (a) calls dialog->callNotifyComplex(fp_value), and
    //           (b) RETURNS to caller.
    //   (1) checks whether to read or write.
    //   (2) if prohibited:
    //           (a) calls dialog->callNotifyComplex(FP_NONE),
    //           (b) displays error popup, and
    //           (c) RETURNS to caller.
    //   (3) if caution:
    //           (a) displays question popup, and
    //           (b) RETURNS to caller.
    //           (c) waits for YES or NO to be pressed.
    //   (4) unmanages dialog box if fp_value is FP_OK.
    //   (5) tries to read or write.
    //   (6) if error:
    //           (a) calls dialog->callNotifyComplex(FP_NONE),
    //           (b) re-manages dialog box if fp_value is FP_OK,
    //           (c) displays error popup, and
    //           (d) RETURNS to caller.
    //   (7) calls dialog->callNotifyComplex(fp_value).
    //   (8) RETURNS to caller.
    //
    // if YES is pressed on question popup:
    //   (1) unmanages dialog box if fp_value is FP_OK.
    //   (2) tries to read or write.
    //   (3) if error:
    //           (a) calls dialog->callNotifyComplex(FP_NONE),
    //           (b) re-manages dialog box if fp_value is FP_OK,
    //           (c) displays error popup, and
    //           (d) RETURNS to caller.
    //   (4) calls dialog->callNotifyComplex(fp_value).
    //   (5) RETURNS to question popup.
    //
    // if NO is pressed on question popup:
    //   (1) calls dialog->callNotifyComplex(FP_NONE).
    //   (2) RETURNS to question popup.


void SLFileChoice::takeAction(SLShellContainer *dialog, int fp_value)
{
  if(fp_value != FP_OK && fp_value != FP_APPLY)
      {
      dialog->callNotifyComplex(fp_value);
      return;
      }
  _dialog   = dialog;
  _fp_value = fp_value;
  char msg[FileBase::MSIZE];
  FileBase::Prepare prepare;
  if(_io == SLpFile::_INPUT) prepare = _file->prepareToRead(msg);
  else                       prepare = _file->prepareToSave(msg);
  updateFields();
  if(prepare == FileBase::PROHIBIT)
      {
  //  if(_dialog) _dialog->callNotifyComplex(FP_NONE);
      _dialog->callNotifyComplex(FP_NONE);
      new SLErrorPop(this, "Error", msg);
      _dialog   = NULL;
      _fp_value = FP_NONE;
      return;
      }
  else if(prepare == FileBase::CAUTION)
      {
      new SLQuestPop(this, "Question", msg, FALSE, yesTrap, noTrap, this);
      return;
      }
  accessFile();
}



//--------------------- yes and no traps -----------------------------//
//--------------------- yes and no traps -----------------------------//
//--------------------- yes and no traps -----------------------------//

     // private static functions.
     // called from question popup.

void SLFileChoice::yesTrap(void *data)
{
  SLFileChoice *THIS = (SLFileChoice*)data;
  THIS->accessFile();
}

void SLFileChoice::noTrap(void *data)
{
  SLFileChoice *THIS = (SLFileChoice*)data;
//if(THIS->_dialog) THIS->_dialog->callNotifyComplex(FP_NONE);
  THIS->_dialog->callNotifyComplex(FP_NONE);
  THIS->_dialog   = NULL;
  THIS->_fp_value = FP_NONE;
}



//----------------------- access file ---------------------------//
//----------------------- access file ---------------------------//
//----------------------- access file ---------------------------//

    // private.
    // read or save the file.
    // called from takeAction or yesTrap.
    // if successful, and if dialog not NULL, pops down dialog.

void SLFileChoice::accessFile()
{
  if(_fp_value == FP_OK) _dialog->unmanage();
//if(_dialog) _dialog->unmanage();
  char msg[FileBase::MSIZE];
  FileBase::Result result;
  if(_io == SLpFile::_INPUT) result = _file->readFile(msg);
  else                       result = _file->saveFile(msg);
  updateFields();
  if(result == FileBase::SUCCESS)
      {
 ///  if(_dialog) _dialog->unmanage();
 //   if(_dialog) _dialog->callNotifyComplex(FP_OK);
      _dialog->callNotifyComplex(_fp_value);
      }
  else
      {
 //   if(_dialog) _dialog->manage();
 //   if(_dialog) _dialog->callNotifyComplex(FP_NONE);
      if(_fp_value == FP_OK) _dialog->manage();
      _dialog->callNotifyComplex(FP_NONE);
      new SLErrorPop(this, "Error", msg);
      }
  _dialog   = NULL;
  _fp_value = FP_NONE;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

