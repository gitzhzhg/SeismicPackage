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

//---------------------- sl_file_pair_plus.cc ------------------------//
//---------------------- sl_file_pair_plus.cc ------------------------//
//---------------------- sl_file_pair_plus.cc ------------------------//

//         implementation file for the SLFilePairPlus class
//               derived from the SLSmartForm class
//                        subdirectory sl


     // To derive from this class:
     //   (1) Override the virtual functions doValidate, doOpen,
     //          and doClose.
     //   (2) Add methods to read and write data.  Be sure to
     //          disable reads and writes if fileIsLoaded() returns
     //          FALSE, and be sure to disable writes if
     //          fileIsReadOnly() returns TRUE.
     //   (3) Add methods to set and get any required variables.

     // To use this class without derivation:
     //   (1) Write three functions matching the typedefs FppValidateTrap,
     //          FppOpenTrap, and FppCloseTrap.  These can be C-style
     //          nonmember functions, or static member functions of
     //          some other class.
     //   (2) Register these three functions by calling the methods
     //          registerValidateTrap, registerOpenTrap, and
     //          registerCloseTrap.  The user data registered with
     //          these calls must contain all information required
     //          by the registered functions.
     //   (3) Do all reads and writes outside of the SLFilePairPlus
     //          class.

     // Any one of the following methods might be used to deal with
     // a data object which accesses the file:
     //   (1) The data object can be created in the constructor
     //          and deleted in the destructor.
     //   (2) A pointer to the data object can be supplied to
     //          the constructor, which keeps a copy of the pointer.
     //   (3) The data object can be created in the doOpen virtual
     //          function (or in the registered _open_trap function),
     //          and deleted in the doClose virtual function (or
     //          in the registered _close_trap function).
     //   (4) The data object can be both created and deleted in
     //          each method which reads or writes data.

     // The file can be opened and closed in the virtual functions
     // doOpen and doClose respectively (or in the registered
     // _open_trap and _close_trap functions).  Or the file can be
     // opened and closed within each method which reads or writes data.
     // The file to be used is the one returned by workingFilename().


#include "sl/sl_file_pair_plus.hh"
#include "sl/sl_error_pop.hh"
#include "sl/sl_file_pair.hh"
#include "sl/slp_text.hh"
#include "sl/shell_watch.hh"
#include "plot/pick_watch.hh"
#include "cprim.h"
#include "inquire.h"


//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//

      // SLDelay does not have to be made

static Widget get_any_widget(SLDelay *gui)
{
  assert(gui);
  if(gui->W      ()) return gui->W      ();
  if(gui->wParent()) return gui->wParent();
  return get_any_widget(gui->slParent());
}




//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


SLFilePairPlus::SLFilePairPlus (SLDelay *slparent, char *name,
                  HelpCtx hctx, Boolean doframe,
                  const char * const filetype,
                  const char * const extension,
                  const Boolean required1,
                  const Boolean required2)
       : SLSmartForm(slparent, name, hctx, doframe),
                  _pair              (NULL),
                  _text              (NULL),
                  _loaded            (FALSE),
                  _read_only         (FALSE),
                  _validate_trap     (NULL),
                  _validate_data     (NULL),
                  _open_trap         (NULL),
                  _open_data         (NULL),
                  _close_trap        (NULL),
                  _close_data        (NULL)
{
  constructorHelper(filetype, filetype, extension, extension,
                    required1, required2, "", "");
}



SLFilePairPlus::SLFilePairPlus (SLDelay *slparent, char *name,
                  HelpCtx hctx, Boolean doframe,
                  const char * const filetype1,
                  const char * const filetype2,
                  const char * const extension1,
                  const char * const extension2,
                  const Boolean required1,
                  const Boolean required2,
                  const char * const suffix1,
                  const char * const suffix2)
       : SLSmartForm(slparent, name, hctx, doframe),
                  _pair              (NULL),
                  _text              (NULL),
                  _loaded            (FALSE),
                  _read_only         (FALSE),
                  _validate_trap     (NULL),
                  _validate_data     (NULL),
                  _open_trap         (NULL),
                  _open_data         (NULL),
                  _close_trap        (NULL),
                  _close_data        (NULL)
{
  constructorHelper(filetype1, filetype2, extension1, extension2,
                    required1, required2, suffix1, suffix2);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


SLFilePairPlus::~SLFilePairPlus()
{
}



//--------------------- constructor helper -------------------------//
//--------------------- constructor helper -------------------------//
//--------------------- constructor helper -------------------------//


void SLFilePairPlus::constructorHelper(const char * const filetype1,
                                       const char * const filetype2,
                                       const char * const extension1,
                                       const char * const extension2,
                                       const Boolean required1,
                                       const Boolean required2,
                                       const char * const suffix1,
                                       const char * const suffix2)
{
  assert(filetype1 && filetype2 && extension1 && extension2);
/*
  Widget widget = get_any_widget(this);
///////  create_watch_cursor(widget);
*/

  _pair = new SLFilePair(this , "filepair",
                         filetype1, filetype2, extension1, extension2,
                         privateValidateTrap, this,
                         "", "", required1, required2, suffix1, suffix2);
  _text = new SLpText   (this , "text");

  _text->showLabelAppearance();

  attach(_pair, this, this,  this, NULL, 10, 10, 10,  0);
  attach(_text, this, this, _pair, this, 10, 10,  0, 10);

  _text->setCvar("NO FILE LOADED");
  _pair->setSensitivity(TRUE);
  _pair->updateFilePair();
}



//--------------------- maybe update ----------------------------//
//--------------------- maybe update ----------------------------//
//--------------------- maybe update ----------------------------//

        // Sets both file names to basename with the proper
        // extension, but only if basename has changed since
        // the previous call to this method, and only if no
        // file is currently loaded.

        // If basename already has an extension, that extension
        // will not be used.

        // To enforce uniformity of use of this class, this method
        // is deliberately the only way for the application to set
        // the file names.  The user, of course, can type (or select)
        // any file name (with any extension) she/he wishes.

        // To enforce uniformity, this function should be called
        // each time the dialog box containing this object is popped up.

        // If the files associated with this object are related to
        // traces being displayed by SeisPlot or by the image library,
        // the basename should be the name of the trace file (usually
        // a byte file).

void SLFilePairPlus::maybeUpdate(char *basename)
{
  if(!_loaded) _pair->updateFilePairIfNewBaseName(basename);
}



//------------------------ open file ----------------------------//
//------------------------ open file ----------------------------//
//------------------------ open file ----------------------------//

       // calls doOpen (a virtual function to override)

long SLFilePairPlus::openFile()
{
///////  start_watch_cursor();
  ShellWatch watch1;
  PickWatch  watch2;
  if(_loaded) closeFile();
  char msg[300];

  long status = _pair->updateFilePair(msg);
  if(status == FILE_ERROR)
     {
     new SLErrorPop(this, "Error", msg);
///////     stop_watch_cursor();
     return FILE_ERROR;
     }

  const char *filename1 = _pair->inputFile();
  const char *filename2 = _pair->outputFile();
  Boolean     required1 = _pair->inputRequired();
  Boolean     required2 = _pair->outputRequired();

  strcpy(msg, "unspecified error occurred");    // preset value

  int error = doOpen(status, filename1, filename2,
                     required1, required2,
                     SLpText::displayMessage, _text, msg);
  if(error)
     {
     _text->setCvar("NO FILE LOADED");
     new SLErrorPop(this, "Error", msg);
///////     stop_watch_cursor();
     return FILE_ERROR;
     }
  _loaded    = TRUE;
  _read_only = (status == FILE_READ_ONLY);
  strcpy(msg, "FILE LOADED: ");
  strcat(msg, workingFilename());
  _text->setCvar(msg);
  _pair->setSensitivity(FALSE);
///////  stop_watch_cursor();
  return status;
}



//----------------------- close file ----------------------------//
//----------------------- close file ----------------------------//
//----------------------- close file ----------------------------//

       // calls doClose (a virtual function to override)

void SLFilePairPlus::closeFile()
{
  if(!_loaded) return;
  doClose();
  _loaded    = FALSE;
  _read_only = FALSE;
  _text->setCvar("NO FILE LOADED");
  _pair->setSensitivity(TRUE);
  _pair->updateFilePair();
}



//---------------------- working filename -----------------------//
//---------------------- working filename -----------------------//
//---------------------- working filename -----------------------//


const char *SLFilePairPlus::workingFilename()  const
{
  static char blank_name[] = "";
  if(!_loaded) return blank_name;
  if(_read_only) return _pair->inputFile();
  return _pair->outputFile();
}


const char *SLFilePairPlus::inputFilename()  const
{
  return _pair->inputFile();
}


const char *SLFilePairPlus::outputFilename()  const
{
  return _pair->outputFile();
}



//----------------- functions to register traps -------------------//
//----------------- functions to register traps -------------------//
//----------------- functions to register traps -------------------//


void SLFilePairPlus::registerValidateTrap(FppValidateTrap *trap, void *data)
{
  _validate_trap = trap;
  _validate_data = data;
}


void SLFilePairPlus::registerOpenTrap(FppOpenTrap *trap, void *data)
{
  _open_trap = trap;
  _open_data = data;
}


void SLFilePairPlus::registerCloseTrap(FppCloseTrap *trap, void *data)
{
  _close_trap = trap;
  _close_data = data;
}



//-------------------- virtual functions to override ------------------//
//-------------------- virtual functions to override ------------------//
//-------------------- virtual functions to override ------------------//

      // any function not overridden must have a trap registered

void SLFilePairPlus::doValidate(const char *filename1,
                                const char *filename2,
                                long *valid1, long *valid2,
                                char *info1, char *info2,
                                long *same_datasets)
{
  assert(_validate_trap);
  _validate_trap(_validate_data, filename1, filename2, valid1, valid2,
                                 info1, info2, same_datasets);
}



int SLFilePairPlus::doOpen(long status,
                           const char *filename1,
                           const char *filename2,
                           Boolean required1, Boolean required2,
                           FppWorkingMessage *working_message_trap,
                           void              *working_message_data,
                           char *msg)
{
  assert(_open_trap);
  return _open_trap(_open_data, status, filename1, filename2,
                                required1, required2,
                                working_message_trap,
                                working_message_data,
                                msg);
}



void SLFilePairPlus::doClose()
{
  assert(_close_trap);
  _close_trap(_close_data);
}



//--------------------- private validate trap ----------------------//
//--------------------- private validate trap ----------------------//
//--------------------- private validate trap ----------------------//

       // calls doValidate (a virtual function to override)

void SLFilePairPlus::privateValidateTrap(void *data,
                                  char *filename1, char *filename2,
                                  long *valid1, long *valid2,
                                  char *info1, char *info2,
                                  long *same_datasets)
{
  SLFilePairPlus *gui = (SLFilePairPlus*)data;
  gui->doValidate(filename1, filename2, valid1, valid2,
                              info1, info2, same_datasets);
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
