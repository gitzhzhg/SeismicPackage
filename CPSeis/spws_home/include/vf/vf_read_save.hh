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

//-------------------------- vf_read_save.hh ------------------------------//
//-------------------------- vf_read_save.hh ------------------------------//
//-------------------------- vf_read_save.hh ------------------------------//

//               header file for the VfReadSave class
//                    not derived from any class
//                         subdirectory vf

        // This class contains all of the information and
        // algorithms needed for validating, reading, and
        // writing velocity files of different types.

        // Separate instances should be used for input and output.

        // If fill_kernal_while_validating is true, the entire file is
        // read into a storage kernal when validating the input file.
        // Then when asked to read the file, this data is transferred
        // from the storage kernal to the specified kernal instead of
        // reading the file again.  Under this circumstance, changes
        // made to the pickle jar between validating and reading will
        // have no effect.  This option should be taken only when the
        // velocity file does not require customized reading.  This
        // option is required if certain min/max information about the
        // file is needed as soon as the file is validated.


//--------------------------- start of coding -----------------------------//
//--------------------------- start of coding -----------------------------//
//--------------------------- start of coding -----------------------------//

#ifndef _VF_READ_SAVE_HH_
#define _VF_READ_SAVE_HH_

#include "vf/vf_constants.hh"


class VfReadSave
{

//------------------------------- data ------------------------------------//
//------------------------------- data ------------------------------------//
//------------------------------- data ------------------------------------//

private:

  class VfInformer   *_informer;  // pointer provided to this class.
  class VfDiskfile   *_diskfile;  // velocity file I/O with VfKernal.
  class VfKernal     *_storage;   // storage for validated input file.
  class HistoryCards *_hstorage;  // storage for validated input file.

  int  _fill_kernal_while_validating;   // true or flase.

  int  _read_choice;    // how to read velocity functions to memory (enum).
  int  _save_choice;    // which velocity functions to save         (enum).

  // _read_choice = READ_REPLACE, READ_ADD, READ_NEW, READ_NOTHING.
  // _save_choice = SAVE_SELECTED, SAVE_ALL, SAVE_ACTIVE.


//--------------------------- functions -----------------------------------//
//--------------------------- functions -----------------------------------//
//--------------------------- functions -----------------------------------//

public:

           VfReadSave (class VfInformer  *informer,
                       class VfUtilities *utilities,
                       int fill_kernal_while_validating, int io,
                       const char *defname = "velocity");
  virtual ~VfReadSave ();

  class VfDiskfile *diskfile()  const  { return _diskfile; }

  int fillKernalWhileValidating()  const
                        { return _fill_kernal_while_validating; }


//---------------------------- get values -------------------------------//
//---------------------------- get values -------------------------------//
//---------------------------- get values -------------------------------//

public:  // get control values.

  int                getReadChoice    ()  const  { return _read_choice; }
  int                getSaveChoice    ()  const  { return _save_choice; }

public:  // get file type from encoding.

  const char        *getFiletype      ();

public:  // get values for properly reading or saving generic file.
         // these pass thru to _diskfile.

  int                getType          ()  const;

public:  // get values which might not be on generic input file.
         // these pass thru to _storage if fill_kernal_while_validating true.
         // these pass thru to _diskfile if fill_kernal_while_validating false.

  int         getNhx             ()  const;
  int         getNhy             ()  const;
  int         getMoveoutOrder    ()  const;

public:  // get minimum and maximum values of all velocity functions
         //   in the validated input file to be read.
         // valid only when fill_kernal_while_validating is true.
         // these return 0.0 if fill_kernal_while_validating is false.
         // these return 0.0 if there are no velocity functions.
         // these pass thru to _storage.

  float minimumXloc       ()  const;
  float maximumXloc       ()  const;
  float minimumYloc       ()  const;
  float maximumYloc       ()  const;

    // the following return 0.0 if there are no velocity functions, or if
    //   all velocity functions have no picks or only nil picks.
    // otherwise, nil picks are not used when getting minima and maxima.

  float minimumAbscissa  (int type)  const;
  float maximumAbscissa  (int type)  const;
  float minimumOrdinate  (int type)  const;
  float maximumOrdinate  (int type)  const;


//---------------------------- set values -------------------------------//
//---------------------------- set values -------------------------------//
//---------------------------- set values -------------------------------//

public:  // set control values.

  void        setReadChoice           (int      read_choice);
  void        setSaveChoice           (int      save_choice);

public:  // set values for properly reading or saving generic file.
         // these pass thru to _diskfile.

  void        setType                 (int             type);


//----------------------- validate a velocity file ------------------------//
//----------------------- validate a velocity file ------------------------//
//----------------------- validate a velocity file ------------------------//

public:  // these return an error flag TRUE or FALSE.
         // to be called from a FileBase object.

  int  validateInputFile   (const char *filename, char *info);
  int  validateOutputFile  (const char *filename, char *info);


//----------------- prepare to read or save a velocity file ----------------//
//----------------- prepare to read or save a velocity file ----------------//
//----------------- prepare to read or save a velocity file ----------------//

public:  // these return an error flag TRUE or FALSE.
         // to be called from a FileBase object.

  int      prepareReadFile (char *msg);
  int      prepareSaveFile (char *msg);


//---------------------- read or save a velocity file ---------------------//
//---------------------- read or save a velocity file ---------------------//
//---------------------- read or save a velocity file ---------------------//

public:  // these return an error flag TRUE or FALSE.
         // to be called from VfDataset.
         // the VfKernal argument keeps users from calling these functions,
         //   since users do not have access to the VfKernal object.

  int      readVelocityFile (const char *filename, char *msg,
                             class VfKernal *kernal,
                             class HistoryCards *history,
                             int *replaced_all);

  int      saveVelocityFile (const char *filename, char *msg,
                             class VfKernal *kernal,
                             class HistoryCards *history,
                             int *saved_all);


//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//

} ;

#endif

//------------------------------- end -------------------------------------//
//------------------------------- end -------------------------------------//
//------------------------------- end -------------------------------------//

