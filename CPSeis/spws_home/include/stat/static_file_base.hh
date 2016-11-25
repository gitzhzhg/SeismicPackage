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

//------------------------ static_file_base.hh ----------------------------//
//------------------------ static_file_base.hh ----------------------------//
//------------------------ static_file_base.hh ----------------------------//

//             header file for the StaticFileBase class
//                 derived from the FileBase class
//                        subdirectory stat


//    This class reads and writes CPS static files.
//    This class accesses the StaticDataset class.
//    Separate instances should be used for input and output.


#ifndef _STATIC_FILE_BASE_HH_
#define _STATIC_FILE_BASE_HH_

#include "oprim/file_base.hh"
#include "named_constants.h"


class StaticFileBase  :  public FileBase
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

     // also int io = ioIndex() from FileBase base class.

public:

  enum { READ_ACTIVE    = 1, // read static file into the active dataset
                             //  (replacing the data in the active dataset).
         READ_REF       = 2, // read static file into the reference dataset
                             //  (replacing the data in the reference dataset).
         READ_NEW       = 3, // read static file into a new dataset
                             //  (create new dataset first).
         READ_NEWACTIVE = 4, // read static file into a new dataset and
                             //  make it active (create new dataset first).
         READ_NEWREF    = 5, // read static file into a new dataset and
                             //  make it reference (create new dataset first).
         READ_NOTHING   = 6  // do not read the static file (already read).
       };

private:

  int                  _read_choice; // above read choice (public enum).
  class StaticManager *_manager;     // pointer to external object.
  class StatioWrapper *_statio;      // pointer to external object.
  class StatioWrapper *_statio2;     // pointer to external (file to overwrite).
  void                *_doer;        // pointer to external object.


//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:   // for input specify doer and statio.
          // for output specify statio and statio2.

  StaticFileBase (StaticManager *manager, void *doer,
                  StatioWrapper *statio, StatioWrapper *statio2 = NULL);
  virtual ~StaticFileBase();

  StaticManager *getStaticManager  ()  const  { return _manager    ; }
  StatioWrapper *getStatioWrapper  ()  const  { return _statio     ; }
  int            getReadChoice     ()  const  { return _read_choice; }

  void           setReadChoice     (int read_choice);

public:  // call this when the active kernal changes, or when something
         //  changes in the kernal which might affect the pickle jar
         //  parameters, or when saving a static file.

  void updatePjarFromKernal (int skiphist);


//------------- virtual functions overriding FileBase --------------------//
//------------- virtual functions overriding FileBase --------------------//
//------------- virtual functions overriding FileBase --------------------//

protected:

  virtual Prepare  virtualPrepareRead (const char *filename, char *errmsg);
  virtual Prepare  virtualPrepareSave (const char *filename, char *errmsg);
  virtual Result   virtualRead        (const char *filename, char *errmsg);
  virtual Result   virtualSave        (const char *filename, char *errmsg);
  virtual Validity virtualValidate    (const char *filename, char *info);
  virtual void     preNewFilename     ();
  virtual void     postNewFilename    ();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
