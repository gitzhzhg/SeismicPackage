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

//------------------------ file_base.hh ----------------------------//
//------------------------ file_base.hh ----------------------------//
//------------------------ file_base.hh ----------------------------//

//             header file for the FileBase class
//                 not derived from any class
//                     subdirectory oprim


//    This is a base class for file I/O classes.
//    See the implementation file for documentation.


#ifndef _FILE_BASE_HH_
#define _FILE_BASE_HH_

#include <stdio.h>
#include <time.h>


class FileBase
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

public:

  enum        { MSIZE = 1001 };   // safe length of errmsg arrays.
  enum        { LSIZE =  101 };   // safe length of info   arrays.

  enum Intent  { USE_FOR_INPUT, USE_FOR_OUTPUT, USE_FOR_INPUT_OR_OUTPUT };

  enum Standing   { STANDING_BLANK,            STANDING_NOT_FOUND,
                    STANDING_NOT_CREATEABLE,   STANDING_FOUND,
                    STANDING_NOT_READABLE,     STANDING_NOT_WRITEABLE,
                    STANDING_NOT_READ_WRITE };

  enum Validity { VALID_YES, VALID_NO, VALID_MAYBE, VALID_YESBUT, VALID_NOBUT };

  enum What { BLANK_FILENAME, MISSING_FILE, NEW_FILE,
              PREVIOUS_GOOD_FILE, PREVIOUS_BAD_FILE };

  enum InputStatus  { INPUT_VALID, INPUT_INVALID };
  enum OutputStatus { OUTPUT_CREATE, OUTPUT_OVERWRITE, OUTPUT_INVALID };

  enum Prepare      { PROHIBIT, CAUTION, GODSPEED };
  enum Result       { CANCELLED, SUCCESS, FAILURE };

private:

  char     *_filetype;   // short string which describes the type of file.
  char     *_extension;  // default extension for file names.
  const int _intent;     // whether used for input, output, or both/either.

  int    _io;   // index 0 for input; index 1 for output.
                // set to 0 or 1 each time a function is called.

  char  *_filename1;   // name of input  file to use for next read.
  char  *_filename2;   // name of output file to use for next save.
  char  *_prevname1;   // name of last input  file read.
  char  *_prevname2;   // name of last output file saved.

  int    _validated1;  // whether _filename1 has already been checked
                       //   by virtualValidate (TRUE/FALSE).
  int    _validated2;  // whether _filename2 has already been checked
                       //   by virtualValidate (TRUE/FALSE).

  time_t       _modtime1;    // last modification time of input file.
  time_t       _modtime2;    // last modification time of output file.
  Standing     _standing1;   // standing of input  file.
  Standing     _standing2;   // standing of output file.
  Validity     _validity1;   // validity of input  file.
  Validity     _validity2;   // validity of output file.
  InputStatus  _status1;     // status of input  file.
  OutputStatus _status2;     // status of output file.
  int          _preverror1;  // whether last read had an error (TRUE/FALSE).
  int          _preverror2;  // whether last save had an error (TRUE/FALSE).

  char  *_standing1_msg;  // description of standing of input  file.
  char  *_standing2_msg;  // description of standing of output file.
  char  *_validity1_info; // information about valid input  file.
  char  *_validity2_info; // information about valid output file.
  char  *_status1_msg;    // description of status of input  file.
  char  *_status2_msg;    // description of status of output file.
  char  *_prevname1_msg;  // message regarding last input  file read.
  char  *_prevname2_msg;  // message regarding last output file saved.

  int _validate_only_when_changed;


//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:    // constructor and destructor.

  FileBase (const char *filetype, const char *extension,
                             Intent intent = USE_FOR_INPUT_OR_OUTPUT,
                             int validate_only_when_changed = 0);
  virtual ~FileBase();

public:   // get values.

  const char   *getFileType       ()  const  { return _filetype; }
  const char   *getExtension      ()  const  { return _extension; }
  int           getIntent         ()  const  { return _intent; }

  int           ioIndex           ()  const  { return _io; }

  const char   *inputFilename               ()  const  { return _filename1; }
  const char   *outputFilename              ()  const  { return _filename2; }
  const char   *lastInputFilenameRead       ()  const  { return _prevname1; }
  const char   *lastOutputFilenameSaved     ()  const  { return _prevname2; }

  int           lastInputFilenameValidated  ()  const  { return _validated1; }
  int           lastOutputFilenameValidated ()  const  { return _validated2; }

  time_t        inputModTime      ()  const  { return _modtime1; }
  time_t        outputModTime     ()  const  { return _modtime2; }
  Standing      inputStanding     ()  const  { return _standing1; }
  Standing      outputStanding    ()  const  { return _standing2; }
  Validity      inputValidity     ()  const  { return _validity1; }
  Validity      outputValidity    ()  const  { return _validity2; }
  InputStatus   inputStatus       ()  const  { return _status1; }
  OutputStatus  outputStatus      ()  const  { return _status2; }
  int           lastReadHadError  ()  const  { return _preverror1; }
  int           lastSaveHadError  ()  const  { return _preverror2; }

  const char   *inputStandingMsg  ()  const  { return _standing1_msg; }
  const char   *outputStandingMsg ()  const  { return _standing2_msg; }
  const char   *inputValidityInfo ()  const  { return _validity1_info; }
  const char   *outputValidityInfo()  const  { return _validity2_info; }
  const char   *inputStatusMsg    ()  const  { return _status1_msg; }
  const char   *outputStatusMsg   ()  const  { return _status2_msg; }
  const char   *lastFileReadMsg   ()  const  { return _prevname1_msg; }
  const char   *lastFileSavedMsg  ()  const  { return _prevname2_msg; }

public:   // set values.
          // functions returning int return changed = TRUE or FALSE.

  void  resetFileType           (const char *filetype);
  int   setInputFilename        (const char *filename);
  int   setOutputFilename       (const char *filename);
  int   replaceInputExtension   (const char *extension);
  int   replaceOutputExtension  (const char *extension);
  void  resetDefaultExtension   (const char *extension);
  void  recheckFilenames        ();
  void  setInformationLabel     (char *label);

public:   // convenience function which does not use any member variables.
          // returns TRUE if filename has the specified extension.
          // returns FALSE if filename has a different (or no) extension.
          // returns FALSE if the filename or extension is NULL or blank.

  static int hasSpecifiedExtension
                      (const char *filename, const char *extension);

public:   // actions to take.
          // these assert if calling them violates _intent.

  Prepare prepareToRead    (char *errmsg = NULL);
  Prepare prepareToSave    (char *errmsg = NULL);
  Result  readFile         (char *errmsg = NULL);
  Result  saveFile         (char *errmsg = NULL);

protected:   // virtual functions to override.
             // functions violating _intent are not called.

  virtual Prepare  virtualPrepareRead (const char *filename, char *errmsg);
  virtual Prepare  virtualPrepareSave (const char *filename, char *errmsg);
  virtual Result   virtualRead        (const char *filename, char *errmsg);
  virtual Result   virtualSave        (const char *filename, char *errmsg);
  virtual Validity virtualValidate    (const char *filename, char *info);
  virtual void     preNewFilename     ()  {}
  virtual void     postNewFilename    ()  {}

     // if the above virtualValidate function is not overridden, the
     // following functions are called instead:

  virtual Validity virtualValidateInput
                               (const char *filename, char *info, What what);
  virtual Validity virtualValidateOutput
                               (const char *filename, char *info, What what);
     // what:
     // BLANK_FILENAME    : the filename is blank.
     // MISSING_FILE      : the file cannot be read.
     // NEW_FILE          : a readable file has not yet been validated.
     // PREVIOUS_GOOD_FILE: a previous validation of the same file was good.
     // PREVIOUS_BAD_FILE : a previous validation of the same file was bad.

private:

  time_t       getModificationTime (const char *filename);
  Standing     inquireFile         (const char *filename, char *msg);
  Validity     validateFile        (const char *filename, char *info);
  InputStatus  inquireInputFile    (Standing, Validity,   char *msg);
  OutputStatus inquireOutputFile   (Standing, Validity,   char *msg);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
