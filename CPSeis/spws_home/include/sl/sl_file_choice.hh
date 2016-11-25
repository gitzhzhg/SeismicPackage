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

//---------------------- sl_file_choice.hh ---------------------------//
//---------------------- sl_file_choice.hh ---------------------------//
//---------------------- sl_file_choice.hh ---------------------------//

//             header file for the SLFileChoice class
//               derived from the SLSmartForm class
//                       subdirectory sl

         // This class is comprized of a file choice widget
         // (encapsulated in SLpFile) plus several label
         // widgets which supply information about the
         // file.  This class interacts with the FileBase
         // base class.


#ifndef _SL_FILE_CHOICE_HH_
#define _SL_FILE_CHOICE_HH_

#include "sl/sl_smart_form.hh"


class SLFileChoice : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int               _io;         // SLpFile::_INPUT or SLpFile::_OUTPUT.
  class FileBase   *_file;       // file base object to work with.
  char             *_label;      // for label or pushbutton.
  class SLpFile    *_filefield;  // owned by this class.
  class SLpLabel   *_msg1field;  // owned by this class.
  class SLpLabel   *_msg2field;  // owned by this class.
  class SLpLabel   *_msg3field;  // owned by this class.
  class SLShellContainer *_dialog;  // to pop down upon success if FP_OK.
  int    _fp_value;  // pass this (or FP_NONE) to dialog->callNotifyComplex.
  Pixel             _pixel_red;
  Pixel             _pixel_green;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  SLFileChoice( SLDelay    *slparent,
                char       *name,
                int         io,
                FileBase   *file,
                const char *label       = NULL,
                HelpCtx     hctx        = NULL,
                Boolean     doframe     = TRUE,
                Boolean     make_if_can = TRUE,
                Boolean     manage_now  = TRUE,
                int         omit        = FALSE); // omit first & third fields.

  virtual ~SLFileChoice();

  int       getInputOutputFlag ()  const  { return _io; }
  FileBase *getFileBase        ()  const  { return _file; }
  SLpFile  *getFileField       ()  const  { return _filefield; }
  SLpLabel *getMsg1Field       ()  const  { return _msg1field; }
  SLpLabel *getMsg2Field       ()  const  { return _msg2field; }
  SLpLabel *getMsg3Field       ()  const  { return _msg3field; }

public:      // major actions.
             // see the implementation file for documentation.

  void resetExtensionFromFileBase();  // SLpFile::resetExtension does not work.
  void updateFieldsFromFileBase  ();
  void updateFields              ();
  void takeAction   (SLShellContainer *dialog, int fp_value);

protected:   // virtual function to optionally override.
             // called after user enters a new filename into the file
             //   choice widget (after that filename is placed into
             //   the FileBase object).

  virtual void newFilenameEntered()  {}

private:

  void        privateUpdateFields  ();
  void        setFallbackResources ();
  static void fileTrap   (void *data, long ident, char *oldvar, char *newvar);
  static void yesTrap    (void *data);
  static void noTrap     (void *data);
  void        accessFile ();
  
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
