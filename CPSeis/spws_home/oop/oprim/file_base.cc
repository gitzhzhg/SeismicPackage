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


//---------------------- file_base.cc ------------------------//
//---------------------- file_base.cc ------------------------//
//---------------------- file_base.cc ------------------------//

//            implementation file for the FileBase class
//                    not derived from any class     
//                        subdirectory oprim


#include "oprim/file_base.hh"
#include "oprim/backup_base.hh"
#include "named_constants.h"
#include "str.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>


#define NBUF    200   // used for member string variables.
                      // probably only need about 80.


//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//
//------------------- documentation ----------------------------//



//--------------- character string information ------------------//
//--------------- character string information ------------------//
//--------------- character string information ------------------//


// Error messages set by the following public functions:
//           prepareToRead     readFile
//           prepareToSave     saveFile
// should have a size of at least [MSIZE] to be safe (unless the
// argument is NULL, of course).  These messages may be several
// lines long, with intervening \n characters.  They are intended
// to be displayed in error popup boxes or question popup boxes.


// The following protected virtual functions:
//           virtualPrepareRead    virtualRead
//           virtualPrepareSave    virtualSave
// should not try to create a message larger than [MSIZE] (including
// the terminating \0 character).  This will be the same message
// returned by the corresponding public function (see above).


// The following protected virtual functions:
//                  virtualValidate
//                  virtualValidateInput
//                  virtualValidateOutput
// should not try to create a message larger than [LSIZE] (including
// the terminating \0 character).  This message is intended for
// single-line display and should not contain any \n characters.
// It will be the message returned by the public functions
// inputValidityInfo and outputValidityInfo.


// The messages whose pointers are returned by the following public
// functions:
//           inputStandingMsg      outputStandingMsg
//           inputValidityInfo     outputValidityInfo
//           inputStatusMsg        outputStatusMsg
//           lastFileReadMsg       lastFileSavedMsg
// are intended for display in single-line text or label widgets,
// and do not include any \n characters.



//--------------- public function information -------------------//
//--------------- public function information -------------------//
//--------------- public function information -------------------//


// The following public functions should be called whenever you wish
// to set a new file name:
//
//        void   setInputFilename       (const char *filename)
//        void   setOutputFilename      (const char *filename)
//        void   replaceInputExtension  (const char *extension)
//        void   replaceOutputExtension (const char *extension)
//
// These are normally to be called from a file choice trap, and can
// also be called at any other time.  These functions first set the
// specified file name to the new value, and then they call
// recheckFilenames() which interrogates both files and sets
// various status flags and messages.



// The following public function should be called whenever you wish
// to make sure all information about the input and output files
// is up-to-date.
//
//           void   recheckFilenames()
//
// This is normally to be called when the associated dialog box is
// popped up, and anytime else when you think the information about
// the files might become out-of-date.  This function is executed
// automatically whenever you call the following functions:
//
//       setInputFilename        prepareToRead      readFile
//       setOutputFilename       prepareToSave      saveFile
//       replaceInputExtension
//       replaceOutputExtension



// The following public functions should be called BEFORE trying
// to read or save a file:
//
//      Prepare prepare = prepareToRead (char *errmsg = NULL)
//      Prepare prepare = prepareToSave (char *errmsg = NULL)
//
// They return the following values:
//
// prepare   description                       errmsg
// -------   -----------                       ------
// PROHIBIT  not allowed to read/save file     display in error popup
// CAUTION   warning about impending read/save display in question popup
// GODSPEED  go ahead and read/save file       ignore



// The following public functions must be called to actually read
// or save a file:
//
//         Result result = readFile (char *errmsg = NULL)
//         Result result = saveFile (char *errmsg = NULL)
//
// They return the following values:
//
// result     description                       errmsg
// ------     -----------                       ------
// CANCELLED  not allowed to read/save file     display in error popup
// FAILURE    a read/write error occurred       display in error popup
// SUCCESS    file was successfully read/saved  ignore
//
// CANCELLED means that accessing the file has not yet been started,
//   and no change has occurred on the file or in memory.
// FAILURE means that a problem occurred while the file was being
//   accessed, such that a portion of the data on the input file may
//   have gotten into memory, or the output file might be corrupted.
//
// NOTE: These two functions call prepareToRead or prepareToSave before
// actually starting to read or save the file, for protection in case
// the user does not do so.  The following actions will occur:
//   returned by prepareToRead/Save    returned by readFile/saveFile
//   ------------------------------    -----------------------------
//             PROHIBIT                          CANCELLED
//             CAUTION                CANCELLED or FAILURE or SUCCESS
//             GODSPEED               CANCELLED or FAILURE or SUCCESS



// See the header file to discover a bunch of public functions which
// can be called to obtain information about the status of the input
// and output files, and the last file read or saved.  Both integer-type
// variables and corresponding message strings are available.  This
// information will be up-to-date after calling any public function
// (documented above), or anytime thereafter, as long as nothing else
// changes the files or their status on disk.  The values of the
// returned variables can be as follows (listed under the name of the
// function which returns the value):



// inputStatus()     description
// -------------     -----------
// INPUT_VALID       file is the correct type, or might be the
//                     correct type, and can be read.
// INPUT_INVALID     filename is blank, or file is not readable,
//                     or file not found, or file not the correct type.
//
// outputStatus()    description
// --------------    -----------
// OUTPUT_CREATE     file does not exist but can be created.
// OUTPUT_OVERWRITE  file already exists and is the correct type,
//                     and is readable and writeable, and it has
//                     been determined that it is OK to overwrite it.
// OUTPUT_INVALID    filename is blank, or file is not readable in
//                     order to be checked, or is not writeable,
//                     or cannot be created, or the file already 
//                     exists and is either not the correct type,
//                     or is maybe the correct type, or should not
//                     be overwritten even if it is the correct type.



// inputStanding()
// outputStanding()         description
// ----------------         -----------
// STANDING_BLANK           filename is blank ("NONE" or "" or NULL pointer).
// STANDING_FOUND           file exists and is readable/writeable.
// STANDING_NOT_READABLE    file exists but is not readable.
// STANDING_NOT_WRITEABLE   file exists but is not writeable.
// STANDING_NOT_READ_WRITE  file exists but is not readable/writeable.
// STANDING_NOT_CREATEABLE  file does not exist and is not writeable.
// STANDING_NOT_FOUND       file does not exist but is writeable.



// inputValidity()
// outputValidity()  description
// ----------------  -----------
// VALID_YES         file is known to be a valid file of appropriate type.
// VALID_YESBUT      file is known to be a valid file of appropriate type.
// VALID_NO          file is known NOT to be a valid file of appropriate type.
// VALID_NOBUT       do not know and do not care.
// VALID_MAYBE       do not know and do not care.
//
// If the file is to be read, it will be read only if <valid> is
//   VALID_YES or VALID_YESBUT or VALID_NOBUT or VALID_MAYBE.
// If the file is to be overwritten, it will be overwritten only
//   if <valid> is VALID_YES or VALID_NOBUT.
// VALID_NO will be returned if there is any kind of error encountered
//   when the file is looked at while trying to determine its
//   validity.
//
// inputValidity()     inputStatus()       outputStatus()
// outputValidity()    for a found file    for a found file
// ----------------    ----------------    ----------------
// VALID_YES           INPUT_VALID         OUTPUT_OVERWRITE         
// VALID_YESBUT        INPUT_VALID         OUTPUT_INVALID             
// VALID_NO            INPUT_INVALID       OUTPUT_INVALID             
// VALID_NOBUT         INPUT_VALID         OUTPUT_OVERWRITE         
// VALID_MAYBE         INPUT_VALID         OUTPUT_INVALID             




//------------------ virtual function information -----------------//
//------------------ virtual function information -----------------//
//------------------ virtual function information -----------------//


// The following protected virtual functions should be overridden
// by derived classes:
//
//   Prepare  virtualPrepareRead   (const char *filename, char *errmsg)
//   Prepare  virtualPrepareSave   (const char *filename, char *errmsg)
//   Result   virtualRead          (const char *filename, char *errmsg)
//   Result   virtualSave          (const char *filename, char *errmsg)
//   Validity virtualValidate      (const char *filename, char *info)
//   Validity virtualValidateInput (const char *filename, char *info, What what)
//   Validity virtualValidateOutput(const char *filename, char *info, What what)
//
// The first four virtual functions return the same variables and
// error messages as the corresponding public functions which call
// them.  The error messages are preset to a reasonable string, and
// are never NULL, even if the errmsg argument in the corresponding
// public function is NULL.  If there is no error, the error message
// will get reset to "-" after the virtual function returns.
//
// The "Validate" virtual functions returns the validity of the file, plus
// an optional single line of information which describes the file if
// it appears to be a valid file of the appropriate type.  The info
// argument is preset to "-" and is never NULL.  If it is important
// to know whether the input or output file is being validated,
// the function ioIndex(), which returns 0 for input or 1 for output,
// can be used, or the specific input/output functions can be overridden
// instead.



//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//
//------------------ constructor and destructor ------------------//



//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


FileBase::FileBase(const char *filetype, const char *extension, Intent intent,
                   int validate_only_when_changed)
          :
            _filetype        (str_newstr(filetype)),
            _extension       (str_newstr(extension)),
            _intent          (intent),
            _io              (0),

            _filename1       (new char [NBUF]),
            _filename2       (new char [NBUF]),
            _prevname1       (new char [NBUF]),
            _prevname2       (new char [NBUF]),
            _validated1      (FALSE),
            _validated2      (FALSE),

            _modtime1        (0),
            _modtime2        (0),
            _standing1       (STANDING_BLANK),
            _standing2       (STANDING_BLANK),
            _validity1       (VALID_NO),
            _validity2       (VALID_NO),
            _status1         (INPUT_INVALID),
            _status2         (OUTPUT_INVALID),
            _preverror1      (FALSE),
            _preverror2      (FALSE),

            _standing1_msg   (new char [NBUF]),
            _standing2_msg   (new char [NBUF]),
            _validity1_info  (new char [NBUF]),
            _validity2_info  (new char [NBUF]),
            _status1_msg     (new char [NBUF]),
            _status2_msg     (new char [NBUF]),
            _prevname1_msg   (new char [NBUF]),
            _prevname2_msg   (new char [NBUF]),

            _validate_only_when_changed   (validate_only_when_changed)
{
  assert(_intent == USE_FOR_INPUT  ||
         _intent == USE_FOR_OUTPUT ||
         _intent == USE_FOR_INPUT_OR_OUTPUT);
  strcpy(_filename1, "");
  strcpy(_filename2, "");
  strcpy(_prevname1, "");
  strcpy(_prevname2, "");
  recheckFilenames();
    // The following two lines are needed because recheckFilenames
    // calls the virtual function virtualValidate, which is not
    // the derived class's function since it is called from this
    // base class constructor.  And since recheckFilenames resets
    // _validated1 and _validated2 to TRUE (under the assumption
    // that the derived class's virtualValidate had been called),
    // we must reset them to FALSE here.
  _validated1 = FALSE;
  _validated2 = FALSE;
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


FileBase::~FileBase()
{
  if(_filetype)       free(_filetype);
  if(_extension)      free(_extension);

  if(_filename1)      delete [] _filename1;
  if(_filename2)      delete [] _filename2;

  if(_standing1_msg)  delete [] _standing1_msg;
  if(_standing2_msg)  delete [] _standing2_msg;
  if(_validity1_info) delete [] _validity1_info;
  if(_validity2_info) delete [] _validity2_info;
  if(_status1_msg)    delete [] _status1_msg;
  if(_status2_msg)    delete [] _status2_msg;

  if(_prevname1)      delete [] _prevname1;
  if(_prevname2)      delete [] _prevname2;
  if(_prevname1_msg)  delete [] _prevname1_msg;
  if(_prevname2_msg)  delete [] _prevname2_msg;
}




//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//
//------------------- static functions --------------------------//



//---------------------- set filename ----------------------------//
//---------------------- set filename ----------------------------//
//---------------------- set filename ----------------------------//

      // returns changed == TRUE if the filename has changed.
      // returns changed == FALSE if the filename remains the same.

static int set_filename(char *filename, const char *inputname, int reset)
{
  char newname[NBUF];
  if(inputname == NULL || strcmp(inputname, "NONE") == 0)
        {
        strcpy(newname, "");
        }
  else
        {
        str_remove_all_blanks(newname, (char*)inputname);
        }
  int changed = (strcmp(filename, newname) != 0);
  if(reset) strcpy(filename, newname);
  return changed;
}



//---------------------- check prev ---------------------------//
//---------------------- check prev ---------------------------//
//---------------------- check prev ---------------------------//

static void check_prev(const char *filename, const char *prevname,
                       int preverror, const char *action,
                       char *prevname_msg)
{
  if(strlen(prevname) == 0)
      {
      strcpy(prevname_msg, "NO FILE PREVIOUSLY ");
      strcat(prevname_msg, action);
      }
  else if(!strcmp(filename, prevname))
      {
                    strcpy(prevname_msg, "ABOVE FILE HAS BEEN ");
                    strcat(prevname_msg, action);
      if(preverror) strcat(prevname_msg, " (with errors)");
      }
  else
      {
                    strcpy(prevname_msg, "FILE LAST ");
                    strcat(prevname_msg, action);
      if(preverror) strcat(prevname_msg, " (with errors)");
                    strcat(prevname_msg, ": ");
                    strcat(prevname_msg, prevname);
      }
}





//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//
//-------------------- public functions -------------------------//



//--------------------- reset file type -----------------------------//
//--------------------- reset file type -----------------------------//
//--------------------- reset file type -----------------------------//

      // public.
      // call this from virtualValidate if you are dealing with
      //   more than one type of file and you want the displayed
      //   message to reflect the current file type which has just
      //   been validated.

void FileBase::resetFileType(const char *filetype)
{
  if(filetype != NULL && filetype[0] != '\0' && filetype[0] != ' ')
      {
      if(_filetype) free(_filetype);
      _filetype = str_newstr(filetype);
      }
}



//----------------------- has specified extension --------------------//
//----------------------- has specified extension --------------------//
//----------------------- has specified extension --------------------//

   // static convenience function which does not use any member variables.
   // returns TRUE if filename has the specified extension.
   // returns FALSE if filename has a different (or no) extension.
   // returns FALSE if the filename or extension is NULL or blank.

int FileBase::hasSpecifiedExtension
                        (const char *filename, const char *extension)
{
  if(filename == NULL || extension == NULL) return FALSE;
  if(filename [0] == ' ' || filename [0] == '\0') return FALSE;
  if(extension[0] == ' ' || extension[0] == '\0') return FALSE;

  char buffer[99];
  strcpy(buffer, ".");
  strcat(buffer, extension);

  int nf = strlen(filename);
  int ne = strlen(buffer);
  if(nf < ne) return FALSE;
  if(memcmp(&filename[nf - ne], buffer, ne) == 0) return TRUE;
  return FALSE;
}



//------------------- set input or output filename ------------------//
//------------------- set input or output filename ------------------//
//------------------- set input or output filename ------------------//

    // public.

int FileBase::setInputFilename(const char *filename)
{
  assert(_intent != USE_FOR_OUTPUT);
  _io = 0;
  int changed = set_filename(_filename1, filename, FALSE);
  if(changed)
      {
      _validated1 = FALSE;
      preNewFilename();
      set_filename(_filename1, filename, TRUE);
      recheckFilenames();
      postNewFilename();
      }
/*
  preNewFilename();
  int changed = set_filename(_filename1, filename);
  if(changed) _validated1 = FALSE;
  recheckFilenames();
  postNewFilename();
*/
  return changed;
}


int FileBase::setOutputFilename(const char *filename)
{
  assert(_intent != USE_FOR_INPUT);
  _io = 1;
  int changed = set_filename(_filename2, filename, FALSE);
  if(changed)
      {
      _validated2 = FALSE;
      preNewFilename();
      set_filename(_filename2, filename, TRUE);
      recheckFilenames();
      postNewFilename();
      }
/*
  preNewFilename();
  int changed = set_filename(_filename2, filename);
  if(changed) _validated2 = FALSE;
  recheckFilenames();
  postNewFilename();
*/
  return changed;
}



//------------------- replace input or output extension ------------------//
//------------------- replace input or output extension ------------------//
//------------------- replace input or output extension ------------------//

    // public.

int FileBase::replaceInputExtension(const char *extension)
{
  assert(_intent != USE_FOR_OUTPUT);
  if(inputStanding() == STANDING_BLANK) return FALSE;
  char filename[NBUF];
  strcpy(filename, inputFilename());
  BackupBase::replaceExtension(filename, extension);
  return setInputFilename(filename);
}


int FileBase::replaceOutputExtension(const char *extension)
{
  assert(_intent != USE_FOR_INPUT);
  if(outputStanding() == STANDING_BLANK) return FALSE;
  char filename[NBUF];
  strcpy(filename, outputFilename());
  BackupBase::replaceExtension(filename, extension);
  return setOutputFilename(filename);
}


void FileBase::resetDefaultExtension(const char *extension)
{
  assert(extension);
  if(_extension) free(_extension);
  _extension = str_newstr(extension);
}



//------------------- recheck filenames -----------------------------//
//------------------- recheck filenames -----------------------------//
//------------------- recheck filenames -----------------------------//

    // public.
    // needs the following variables:
    //    _filename1   _validated1   _prevname1   _preverror1
    //    _filename2   _validated2   _prevname2   _preverror2
    // sets the following variables:
    //    _validated1
    //    _validated2
    //    _modtime1   _standing1      _validity1       _status1 
    //    _modtime2   _standing2      _validity2       _status2 
    //    _standing1_msg  _validity1_info  _status1_msg  _prevname1_msg 
    //    _standing2_msg  _validity2_info  _status2_msg  _prevname2_msg 

void FileBase::recheckFilenames()
{
  int remember = _io;
  time_t modtime1 = getModificationTime (_filename1);
  time_t modtime2 = getModificationTime (_filename2);
  if(modtime1 != _modtime1) _validated1 = FALSE;
  if(modtime2 != _modtime2) _validated2 = FALSE;
  _modtime1 = modtime1;
  _modtime2 = modtime2;

                    //                  i             o
  _standing1 = inquireFile         (_filename1, _standing1_msg);
  _standing2 = inquireFile         (_filename2, _standing2_msg);

/******
                             //           i             o
  _io = 0; _validity1 = validateFile (_filename1, _validity1_info);
  _io = 1; _validity2 = validateFile (_filename2, _validity2_info);

                    //              i            i           o
  _status1 = inquireInputFile  (_standing1, _validity1, _status1_msg);
  _status2 = inquireOutputFile (_standing2, _validity2, _status2_msg);
******/

       /// the above has been replaced by the below so that resetFileType
       /// will work for both input and output files.

  _io = 0;
                      //              i             o
  if(!_validate_only_when_changed || !_validated1)
     { _validity1 = validateFile (_filename1, _validity1_info); }
  _status1   = inquireInputFile  (_standing1, _validity1, _status1_msg);
                      //              i            i           o

  _io = 1;
                      //              i             o
  if(!_validate_only_when_changed || !_validated2)
     { _validity2 = validateFile (_filename2, _validity2_info); }
  _status2   = inquireOutputFile (_standing2, _validity2, _status2_msg);
                      //              i            i           o

       //          i           i           i          i          o
  check_prev (_filename1, _prevname1, _preverror1, "READ" , _prevname1_msg);
  check_prev (_filename2, _prevname2, _preverror2, "SAVED", _prevname2_msg);

  _validated1 = TRUE;
  _validated2 = TRUE;
  _io = remember;
}



//----------------- prepare to read -------------------------------//
//----------------- prepare to read -------------------------------//
//----------------- prepare to read -------------------------------//

    // public.
    // calls virtualPrepareRead.
    // to be called before trying to read the file.
    // returns prepare = PROHIBIT or CAUTION or GODSPEED.
    // checks whether we really want to read data.
    // sets errmsg if not NULL.

FileBase::Prepare
FileBase::prepareToRead(char *errmsg)
{
  assert(_intent != USE_FOR_OUTPUT);
  _io = 0;
  time_t      modtime  = _modtime1;
  Standing    standing = _standing1;
  Validity    validity = _validity1;
  InputStatus status   = _status1;
  char       *info     = str_newstr(_validity1_info);

  recheckFilenames();

  int changed = FALSE;
  if(_modtime1  != modtime)                 changed = TRUE;
  if(_standing1 != standing)                changed = TRUE;
  if(_validity1 != validity)                changed = TRUE;
  if(_status1   != status)                  changed = TRUE;
  if(strcmp(info, _validity1_info))         changed = TRUE;
  free(info);

  if(changed)
      {
      if(errmsg)
          {
          if(_standing1 == STANDING_BLANK)
              {
              strcpy(errmsg, "No input file specified.");
              }
          else
              {
              strcpy(errmsg, "The status of the input file\n");
              strcat(errmsg, "has changed.\n----\n");
              strcat(errmsg, "Check it before continuing.");
              }
          }
      return PROHIBIT;
      }

  if(_status1 == INPUT_INVALID)
      {
      if(errmsg) strcpy(errmsg, _status1_msg);
      return PROHIBIT;
      }

  char buffer[MSIZE];
  strcpy(buffer, "");
  Prepare prepare = virtualPrepareRead(_filename1, buffer);

  if(!errmsg) return prepare;

  if(strlen(buffer) > 0)
      {
      strcpy(errmsg, buffer);
      strcat(errmsg, "\n----\n");
      }
  else
      {
      strcpy(errmsg, "");
      }

  switch(prepare)
      {
      case PROHIBIT:
          strcat(errmsg, "You cannot read the file at this time.");
          break;
      case CAUTION:
          strcat(errmsg, "Are you sure you want");
          strcat(errmsg, " to read the file at this time?");
          break;
      case GODSPEED:
          strcpy(errmsg, "-");
          break;
      default: assert(FALSE);
      }
  return prepare;
}



/*
  switch(prepare)
      {
      case PROHIBIT:
          if(errmsg)
              {
              if(strlen(buffer) == 0)
                  {
                  strcpy(errmsg, "You are not allowed\n");
                  strcat(errmsg, "to read the file\nat this time.");
                  }
              else
                  {
                  strcpy(errmsg, buffer);
                  }
              }
          break;
      case CAUTION:
          if(errmsg)
              {
              if(strlen(buffer) == 0)
                  {
                  strcpy(errmsg, "Are you sure you want\n");
                  strcat(errmsg, "to read the file\nat this time?");
                  }
              else
                  {
                  strcpy(errmsg, buffer);
                  }
              }
          break;
      case GODSPEED:
          if(errmsg)
              {
              strcpy(errmsg, "-");
              }
          break;
      default: assert(FALSE);
      }
  return prepare;
}
*/



//----------------- prepare to save -------------------------------//
//----------------- prepare to save -------------------------------//
//----------------- prepare to save -------------------------------//

    // public.
    // calls virtualPrepareSave.
    // to be called before trying to save the file.
    // returns prepare = PROHIBIT or CAUTION or GODSPEED.
    // checks whether we really want to save data.
    // sets errmsg if not NULL.

FileBase::Prepare
FileBase::prepareToSave(char *errmsg)
{
  assert(_intent != USE_FOR_INPUT);
  _io = 1;
  time_t       modtime  = _modtime2;
  Standing     standing = _standing2;
  Validity     validity = _validity2;
  OutputStatus status   = _status2;
  char        *info     = str_newstr(_validity2_info);

  recheckFilenames();

  int changed = FALSE;
  if(_modtime2  != modtime)                 changed = TRUE;
  if(_standing2 != standing)                changed = TRUE;
  if(_validity2 != validity)                changed = TRUE;
  if(_status2   != status)                  changed = TRUE;
  if(strcmp(info, _validity2_info))         changed = TRUE;
  free(info);

  if(changed)
      {
      if(errmsg)
          {
          if(_standing2 == STANDING_BLANK)
              {
              strcpy(errmsg, "No output file specified.");
              }
          else
              {
              strcpy(errmsg, "The status of the output file\n");
              strcat(errmsg, "has changed.\n----\n");
              strcat(errmsg, "Check it before continuing.");
              }
          }
      return PROHIBIT;
      }

  if(_status2 == OUTPUT_INVALID)
      {
      if(errmsg) strcpy(errmsg, _status2_msg);
      return PROHIBIT;
      }

  char buffer[MSIZE];
  strcpy(buffer, "");
  Prepare prepare = virtualPrepareSave(_filename2, buffer);

/*
  //// added 2/19/02:
  if(strlen(buffer) == 0 && _status2 == OUTPUT_OVERWRITE
                         && prepare == GODSPEED) prepare = CAUTION;
*/
  if(prepare == GODSPEED)
      {
      strcpy(buffer, "");
      if(_status2 == OUTPUT_OVERWRITE) prepare = CAUTION;
      }

  if(!errmsg) return prepare;

  if(strlen(buffer) > 0)
      {
      strcpy(errmsg, buffer);
      strcat(errmsg, "\n----\n");
      }
  else
      {
      strcpy(errmsg, "");
      }

  switch(prepare)
      {
      case PROHIBIT:
          strcat(errmsg, "You cannot save the file at this time.");
          break;
      case CAUTION:
          if(_status2 == OUTPUT_OVERWRITE)
              {
              strcat(errmsg, "The file you are about to save\n");
              strcat(errmsg, "will overwrite this existing file:\n");
              strcat(errmsg, "----\n");
              strcat(errmsg, _filename2);
              strcat(errmsg, "\n----\n");
              }
          strcat(errmsg, "Are you sure you want");
          strcat(errmsg, " to save the file at this time?");
          break;
      case GODSPEED:
          strcpy(errmsg, "-");
          break;
      default: assert(FALSE);
      }
  return prepare;
}



/*
  switch(prepare)
      {
      case PROHIBIT:
          if(errmsg)
              {
              if(strlen(buffer) == 0)
                  {
                  strcpy(errmsg, "You are not allowed\n");
                  strcat(errmsg, "to save the file\nat this time.");
                  }
              else
                  {
                  strcpy(errmsg, buffer);
                  }
              }
          break;
      case CAUTION:
          if(errmsg)
              {
              if(strlen(buffer) > 0)
                  {
                  strcpy(errmsg, buffer);
                  strcat(errmsg, "\n----\n");
                  }
              if(_status2 == OUTPUT_OVERWRITE)
                  {
                  strcat(errmsg, "The file you are about to save\n");
                  strcat(errmsg, "will overwrite this existing file:\n");
                  strcat(errmsg, "----\n");
                  strcat(errmsg, _filename2);
                  strcat(errmsg, "\n----\nAre you sure you want\n");
                  strcat(errmsg, "to save the file\nat this time?");
                  }
              else
                  {
                  strcpy(errmsg, "Are you sure you want\n");
                  strcat(errmsg, "to save the file\nat this time?");
                  }
              }
/ *
              if(strlen(buffer) == 0)
                  if(_status2 == OUTPUT_OVERWRITE)
                      {
                      strcpy(errmsg, "The file you are about to save\n");
                      strcat(errmsg, "will overwrite this existing file:\n");
                      strcat(errmsg, "----\n");
                      strcat(errmsg, _filename2);
                      strcat(errmsg, "\n----\nAre you sure you want\n");
                      strcat(errmsg, "to save the file\nat this time?");
                      }
                  else
                      {
                      strcpy(errmsg, "Are you sure you want\n");
                      strcat(errmsg, "to save the file\nat this time?");
                      }
                  }
              else
                  {
                  if(_status2 == OUTPUT_OVERWRITE)
                      {
                      strcpy(errmsg, buffer);
                      strcat(errmsg, "\n----\n");
                      strcat(errmsg, "The file you are about to save\n");
                      strcat(errmsg, "will overwrite this existing file:\n");
                      strcat(errmsg, "----\n");
                      strcat(errmsg, _filename2);
                      strcat(errmsg, "\n----\nAre you sure you want\n");
                      strcat(errmsg, "to save the file\nat this time?");
                      }
                  else
                      {
                      strcpy(errmsg, buffer);
                      strcat(errmsg, "\n----\n");
                      strcat(errmsg, "Are you sure you want\n");
                      strcat(errmsg, "to save the file\nat this time?");
                      }
                  }
              }
          break;
      case GODSPEED:
          if(errmsg)
              {
              strcpy(errmsg, "-");
              }
          break;
      default: assert(FALSE);
      }
  return prepare;
}
*/



//--------------------- read file -----------------------------//
//--------------------- read file -----------------------------//
//--------------------- read file -----------------------------//

    // public.
    // calls virtualRead.
    // returns result = CANCELLED or SUCCESS or FAILURE.
    // sets errmsg if not NULL.

FileBase::Result
FileBase::readFile(char *errmsg)
{
  assert(_intent != USE_FOR_OUTPUT);
  _io = 0;
  Prepare prepare = prepareToRead(errmsg);
  if(prepare == PROHIBIT) return CANCELLED;

  char buffer[MSIZE];
  strcpy(buffer, "");
  Result result = virtualRead(_filename1, buffer);

  switch(result)
      {
      case CANCELLED:
          if(errmsg)
              {
              if(strlen(buffer) == 0)
                  {
                  strcpy(errmsg, "You are not allowed\n");
                  strcat(errmsg, "to read the file\nat this time.");
                  }
              else
                  {
                  strcpy(errmsg, buffer);
                  }
              }
          return CANCELLED;
      case FAILURE:
          if(errmsg)
              {
              if(strlen(buffer) == 0)
                  {
                  strcpy(errmsg, "Error encountered\n");
                  strcat(errmsg, "while reading the file.\n");
                  }
              else
                  {
                  strcpy(errmsg, buffer);
                  }
              }
          _preverror1 = TRUE;
          break;
      case SUCCESS:
          if(errmsg)
              {
              strcpy(errmsg, "-");
              }
          _preverror1 = FALSE;
          break;
      default: assert(FALSE);
      }
  strcpy(_prevname1, _filename1);
  recheckFilenames();
  return result;
}



//--------------------- save file -----------------------------//
//--------------------- save file -----------------------------//
//--------------------- save file -----------------------------//

    // public.
    // calls virtualSave.
    // returns result = CANCELLED or SUCCESS or FAILURE.
    // sets errmsg if not NULL.

FileBase::Result
FileBase::saveFile(char *errmsg)
{
  assert(_intent != USE_FOR_INPUT);
  _io = 1;
  Prepare prepare = prepareToSave(errmsg);
  if(prepare == PROHIBIT) return CANCELLED;

  char buffer[MSIZE];
  strcpy(buffer, "");
  Result result = virtualSave(_filename2, buffer);

  switch(result)
      {
      case CANCELLED:
          if(errmsg)
              {
              if(strlen(buffer) == 0)
                  {
                  strcpy(errmsg, "You are not allowed\n");
                  strcat(errmsg, "to save the file\nat this time.");
                  }
              else
                  {
                  strcpy(errmsg, buffer);
                  }
              }
          return CANCELLED;
      case FAILURE:
          if(errmsg)
              {
              if(strlen(buffer) == 0)
                  {
                  strcpy(errmsg, "Error encountered\n");
                  strcat(errmsg, "while saving the file.\n");
                  }
              else
                  {
                  strcpy(errmsg, buffer);
                  }
              }
          _preverror2 = TRUE;
          break;
      case SUCCESS:
          if(errmsg)
              {
              strcpy(errmsg, "-");
              }
              _preverror2 = FALSE;
          break;
      default: assert(FALSE);
      }
  strcpy(_prevname2, _filename2);
  recheckFilenames();
  return result;
}



//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//
//-------------------- virtual functions --------------------------//

          // protected.
          // to override in derived classes.

FileBase::Prepare
FileBase::virtualPrepareRead (const char* /*filename*/, char* /*errmsg*/)
{
  return CAUTION;
}


FileBase::Prepare
FileBase::virtualPrepareSave (const char* /*filename*/, char* /*errmsg*/)
{
  if(outputStatus() == OUTPUT_OVERWRITE) return CAUTION;
  return GODSPEED;
}


FileBase::Result
FileBase::virtualRead      (const char* /*filename*/, char* /*errmsg*/)
{
  return CANCELLED;
}


FileBase::Result
FileBase::virtualSave      (const char* /*filename*/, char* /*errmsg*/)
{
  return CANCELLED;
}



FileBase::Validity
FileBase::virtualValidate  (const char* filename, char* info)
{
  What what;
//////////////////
  if(_io == 0)
      {
      if(_standing1 == STANDING_BLANK)
          {
          what = BLANK_FILENAME;
          }
      else if(_standing1 != STANDING_FOUND &&
              _standing1 != STANDING_NOT_WRITEABLE)
          {
          what = MISSING_FILE;
          }
      else if(!_validated1)
          {
          what = NEW_FILE;
          }
      else if(_validity1 == VALID_YES || _validity1 == VALID_YESBUT)
          {
          what = PREVIOUS_GOOD_FILE;
          strcpy(info, _validity1_info);
          }
      else
          {
          what = PREVIOUS_BAD_FILE;
          strcpy(info, _validity1_info);
          }
      return virtualValidateInput (filename, info, what);
      }
//////////////////
  if(_standing2 == STANDING_BLANK)
      {
      what = BLANK_FILENAME;
      }
  else if(_standing2 != STANDING_FOUND)
      {
      what = MISSING_FILE;
      }
  else if(!_validated2)
      {
      what = NEW_FILE;
      }
  else if(_validity2 == VALID_YES || _validity2 == VALID_YESBUT)
      {
      what = PREVIOUS_GOOD_FILE;
      strcpy(info, _validity2_info);
      }
  else
      {
      what = PREVIOUS_BAD_FILE;
      strcpy(info, _validity2_info);
      }
  return virtualValidateOutput(filename, info, what);
//////////////////
}



FileBase::Validity
FileBase::virtualValidateInput
                    (const char* /*filename*/, char* /*info*/, What /*what*/)
{
  return VALID_MAYBE;
}



FileBase::Validity
FileBase::virtualValidateOutput
                    (const char* /*filename*/, char* /*info*/, What /*what*/)
{
  return VALID_MAYBE;
}




//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//
//------------------- protected functions ------------------------//



//--------------------------- err msg -------------------------------//
//--------------------------- err msg -------------------------------//
//--------------------------- err msg -------------------------------//

//////////////////// not yet implemented.

/***********
void wprocVAShowMsg( Widget w, char *format, ...)
{
#define BUFLEN 2000
 char str[BUFLEN];
 va_list   args;
 int cnt;

 va_start(args, format);
 cnt= vsprintf(str, format, args);
 if (cnt >= BUFLEN) printf("Warning: wprocVAShowMsg: Buffer overflow\n");
 wprocShowMsg(w,str);
 va_end(args);
}
 these functions:   vfprintf  vprintf  vsprintf
 are equivalent to:  fprintf   printf   sprintf
 except the former use the args argument instead of a variable argument list.
***********/




//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//
//-------------------- private functions -------------------------//



//------------------ get modification time -----------------------//
//------------------ get modification time -----------------------//
//------------------ get modification time -----------------------//

    // private.
    // returns the last modification time of the file.
    // returns 0 if there is an error.

time_t
FileBase::getModificationTime(const char *filename)
{
  struct stat file_stats;
  int error = stat(filename, &file_stats);
  if(error) return 0;
  return file_stats.st_mtime;
}



//-------------------- inquire file ---------------------------//
//-------------------- inquire file ---------------------------//
//-------------------- inquire file ---------------------------//

    // private.
    // the access function returns 0 if successful and -1 if error.
    // the access function will always claim that a non-existent
    //   function is not writeable, even when you can open it
    //   and write to it.

    // returns standing = STANDING_BLANK           STANDING_NOT_FOUND 
    //                    STANDING_NOT_CREATEABLE  STANDING_FOUND       
    //                    STANDING_NOT_READABLE    STANDING_NOT_WRITEABLE
    //                    STANDING_NOT_READ_WRITE

FileBase::Standing
FileBase::inquireFile(const char *filename, char *msg)
{
  if(strlen(filename) == 0)
      {
      strcpy(msg, "no filename specified");
      return STANDING_BLANK;
      }
  int exists    = 1 + access(filename, F_OK);
  int readable  = 1 + access(filename, R_OK);
  int writeable = 1 + access(filename, W_OK);
  if(exists && readable && writeable)
      {
      strcpy(msg, "file found");
      return STANDING_FOUND;
      }
  if(exists && writeable)
      {
      strcpy(msg, "file is writeable but not readable");
      return STANDING_NOT_READABLE;
      }
  if(exists && readable)
      {
      strcpy(msg, "file is readable but not writeable");
      return STANDING_NOT_WRITEABLE;
      }
  if(exists)
      {
      strcpy(msg, "file is not readable or writeable");
      return STANDING_NOT_READ_WRITE;
      }
  FILE *stream = fopen(filename, "w+");
  if(stream == NULL)
      {
      strcpy(msg, "file does not exist and cannot be created");
      return STANDING_NOT_CREATEABLE;
      }
  fclose(stream);
  remove(filename);
  strcpy(msg, "file not found but can be created");
  return STANDING_NOT_FOUND;
}



//-------------------- validate file -------------------------------//
//-------------------- validate file -------------------------------//
//-------------------- validate file -------------------------------//

  // private.
  // calls virtualValidate.
  // returns validity = VALID_YES or VALID_NO or VALID_MAYBE or
  //                                   VALID_YESBUT or VALID_NOBUT.
  // sets info.
  // this function is called even if we already know that the file
  //   is not valid, because overriding classes might be doing other
  //   things to update their own data within virtualValidate.  This
  //   is the only way they will know that a new filename has been set.

FileBase::Validity
FileBase::validateFile(const char *filename, char *info)
{
  if(_io == 0 && _intent == USE_FOR_OUTPUT) return VALID_NO;
  if(_io == 1 && _intent == USE_FOR_INPUT ) return VALID_NO;
////// this change made 11/13/98 allows the overriding class to have access
////// to the previous validation info:
  char info2[NBUF];
  strcpy(info2, "-");
  Validity validity = virtualValidate(filename, info2);
  strcpy(info, info2);
/****
  strcpy(info, "-");
  Validity validity = virtualValidate(filename, info);
****/

/////// added 2/21/02 to remove second info line when file is not found:
  char *word = strstr(info,"input");
  if(word) strcpy(info, "-");

  switch(validity)
      {
      case VALID_YES:    break;
      case VALID_NO:     break;
      case VALID_MAYBE:  break;
      case VALID_YESBUT: break;
      case VALID_NOBUT : break;
      default: assert(FALSE);
      }
  return validity;
}



//--------------------- inquire input file --------------------------//
//--------------------- inquire input file --------------------------//
//--------------------- inquire input file --------------------------//

    // private.
    // needs   _standing1 and _validity1.
    // returns _status1 = INPUT_VALID or INPUT_INVALID.
    // sets    _status1_msg.
    // for clarity, all input and output values (except for
    //   _filetype) are passed thru the argument list or returned.

FileBase::InputStatus
FileBase::inquireInputFile(Standing standing, Validity validity, char *msg)
{
  InputStatus status;
  if(standing == STANDING_NOT_WRITEABLE )  standing = STANDING_FOUND;
  if(standing == STANDING_NOT_READ_WRITE)  standing = STANDING_NOT_READABLE;
  if(standing == STANDING_NOT_CREATEABLE)  standing = STANDING_NOT_FOUND;

  if(standing == STANDING_BLANK)
       {
       strcpy(msg, "no input file specified");
       status = INPUT_INVALID;
       }
  else if(standing == STANDING_NOT_FOUND)
       {
       strcpy(msg, "input file not found");
       status = INPUT_INVALID;
       }
  else if(standing == STANDING_NOT_READABLE)
       {
       strcpy(msg, "input file not readable");
       status = INPUT_INVALID;
       }
  else if(standing == STANDING_FOUND && validity == VALID_NO)
       {
       sprintf(msg, "the input file is not a valid %s", _filetype);
       status = INPUT_INVALID;
       }
  else if(standing == STANDING_FOUND && validity == VALID_NOBUT)
       {
       sprintf(msg, "input %s found", _filetype);
       status = INPUT_VALID;
       }
  else if(standing == STANDING_FOUND && validity == VALID_YES)
       {
       sprintf(msg, "input %s found", _filetype);
       status = INPUT_VALID;
       }
  else if(standing == STANDING_FOUND && validity == VALID_YESBUT)
       {
       sprintf(msg, "input %s found", _filetype);
       status = INPUT_VALID;
       }
  else if(standing == STANDING_FOUND && validity == VALID_MAYBE)
       {
       strcpy(msg, "input file found");
       status = INPUT_VALID;
       }
  else
       {
       strcpy(msg, "illegal value of 'standing' or 'validity'");
       status = INPUT_INVALID;
       }
  return status;
}



//----------------------- inquire output file ----------------------//
//----------------------- inquire output file ----------------------//
//----------------------- inquire output file ----------------------//

    // private.
    // needs   _standing2 and _validity2.
    // returns _status2 = OUTPUT_CREATE or OUTPUT_OVERWRITE or OUTPUT_INVALID.
    // sets    _status2_msg.
    // for clarity, all input and output values (except for
    //   _filetype) are passed thru the argument list or returned.

FileBase::OutputStatus
FileBase::inquireOutputFile(Standing standing, Validity validity, char *msg)
{
  OutputStatus status;

  if(standing == STANDING_BLANK)
       {
       strcpy(msg, "no output file specified");
       status = OUTPUT_INVALID;
       }
  else if(standing == STANDING_NOT_FOUND)
       {
       strcpy(msg, "output file does not yet exist");
       status = OUTPUT_CREATE;
       }
  else if(standing == STANDING_NOT_READABLE)
       {
       strcpy(msg, "output file is not readable and cannot be checked");
       status = OUTPUT_INVALID;
       }
  else if(standing == STANDING_NOT_WRITEABLE)
       {
       strcpy(msg, "output file is not writeable ");
       status = OUTPUT_INVALID;
       }
  else if(standing == STANDING_NOT_CREATEABLE)
       {
       strcpy(msg, "output file cannot be created");
       status = OUTPUT_INVALID;
       }
  else if(standing == STANDING_NOT_READ_WRITE)
       {
       strcpy(msg, "output file is not readable or writeable");
       status = OUTPUT_INVALID;
       }
  else if(standing == STANDING_FOUND && validity == VALID_NO)
       {
       if(strncmp(_filetype, "SCRS", 4) == 0)
         sprintf(msg, "existing output file is NOT an %s", _filetype);
       else
         sprintf(msg, "existing output file is NOT a %s", _filetype);
       status = OUTPUT_INVALID;
       }
  else if(standing == STANDING_FOUND && validity == VALID_NOBUT)
       {
       sprintf(msg, "existing %s will be overwritten", _filetype);
       status = OUTPUT_OVERWRITE;
       }
  else if(standing == STANDING_FOUND && validity == VALID_YES)
       {
       sprintf(msg, "existing %s will be overwritten", _filetype);
       status = OUTPUT_OVERWRITE;
       }
  else if(standing == STANDING_FOUND && validity == VALID_YESBUT)
       {
       sprintf(msg, "existing %s must not be overwritten", _filetype);
       status = OUTPUT_INVALID;
       }
  else if(standing == STANDING_FOUND && validity == VALID_MAYBE)
       {
       strcpy(msg, "output file of unknown type already exists");
       status = OUTPUT_INVALID;
       }
  else
       {
       strcpy(msg, "illegal value of 'standing' or 'validity'");
       status = OUTPUT_INVALID;
       }
  return status;
}


//---------------------------------------------------------------------
//------- The following is intended to allow setting the file   -------
//------- label by an external class                            -------
//---------------------------------------------------------------------
void FileBase::setInformationLabel(char *label)
{
  strcpy(_validity1_info , label);
  recheckFilenames();
}


//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//
//------------------------------- end --------------------------------//


