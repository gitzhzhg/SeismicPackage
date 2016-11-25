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

//---------------------------- vf_file_base.hh ----------------------------//
//---------------------------- vf_file_base.hh ----------------------------//
//---------------------------- vf_file_base.hh ----------------------------//

//               header file for the VfFileBase class
//                 derived from the FileBase class
//                         subdirectory vf


//    This is a class for reading, writing, and validating velocity
//    files of various types.  This class is needed by SLFileChoice.
//    This class need not be overridden.

//    This class accesses the VfManager and VfDataset classes.
//    This class also accesses the VfKernal class when reading and
//    saving files.


#ifndef _VF_FILE_BASE_HH_
#define _VF_FILE_BASE_HH_

#include "oprim/file_base.hh"
#include "named_constants.h"


class VfFileBase  :  public FileBase
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//

     // also int io = ioIndex() from FileBase base class.
     // the required enums are in the vf_constants.hh header file.

private:

  class VfManager    *_manager;     // pointer to external object.
  class VfReadSave   *_readsave;    // owned by this class.
  class VelioWrapper *_velio2;      // owned by this class (file to overwrite).
  class VfEditSort   *_edit_sort;   // owned by this class.
  class VfEditNames  *_edit_names;  // owned by this class.

  int  _sort_choice;   // whether to sort velocity functions while saving.
  int  _names_choice;  // whether to reset blank function names while saving.


//-------------------------- functions ----------------------------------//
//-------------------------- functions ----------------------------------//
//-------------------------- functions ----------------------------------//

public:

           VfFileBase (const char *filetype, const char *extension,
                       VfManager *manager, Intent intent = USE_FOR_INPUT,
                       int fill_kernal_while_validating = TRUE);
  virtual ~VfFileBase ();


//---------------------------- get values ---------------------------------//
//---------------------------- get values ---------------------------------//
//---------------------------- get values ---------------------------------//

public:  // variables in this class.

  VfManager     *manager        ()  const  { return _manager; }
  VfReadSave    *readsave       ()  const  { return _readsave; }
  VelioWrapper  *velio2         ()  const  { return _velio2; }
  int            getSortChoice  ()  const  { return _sort_choice; }
  int            getNamesChoice ()  const  { return _names_choice; }

public:  // get control values.
         // these pass thru to VfReadSave.

  int                getReadChoice    ()  const;
  int                getSaveChoice    ()  const;

public:  // get values for properly reading or saving generic file.
         // these pass thru to VfReadSave.

  int                getType                  ()           const;

public:  // get values which might not be on generic input file.
         // the unit/attribute items also will not be on old style file.
         // these pass thru to VfReadSave.

  int         getNhx             ()  const;
  int         getNhy             ()  const;
  int         getMoveoutOrder    ()  const;

public:  // get minimum and maximum values from all velocity functions
         //   in the validated input file to be read.
         // these loop thru all velocity functions to get returned value.
         // these return 0.0 if fill_kernal_while_validating is false.
         // these return 0.0 if there are no velocity functions.
         // these pass thru to VfReadSave.

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


//----------------------------- set values -------------------------------//
//----------------------------- set values -------------------------------//
//----------------------------- set values -------------------------------//

public:  // variables in this class.

  void   setSortChoice   (int value)  { _sort_choice  = value; }
  void   setNamesChoice  (int value)  { _names_choice = value; }

public:  // set control values.
         // these pass thru to VfReadSave.

  void   setReadChoice   (int value);
  void   setSaveChoice   (int value);

public:  // set values for properly reading or saving generic file.
         // irrelevant for input if fill_kernal_while_validating is true.
         // these pass thru to VfReadSave.

  void        setType                  (int         value);


//------------------------- virtual functions -----------------------------//
//------------------------- virtual functions -----------------------------//
//------------------------- virtual functions -----------------------------//

protected:   // virtual functions overriding FileBase.

  virtual Validity virtualValidateInput
                               (const char *filename, char *info, What what);

  virtual Validity virtualValidateOutput
                               (const char *filename, char *info, What what);

  virtual void     preNewFilename     ();
  virtual void     postNewFilename    ();
  virtual Prepare  virtualPrepareRead (const char *filename, char *msg);
  virtual Prepare  virtualPrepareSave (const char *filename, char *msg);
  virtual Result   virtualRead        (const char *filename, char *msg);
  virtual Result   virtualSave        (const char *filename, char *msg);


//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

} ;

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
