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

//--------------------------- static_helper.hh --------------------------//
//--------------------------- static_helper.hh --------------------------//
//--------------------------- static_helper.hh --------------------------//

//              header file for the StaticHelper class
//                  not derived from any class
//                      subdirectory stat


#ifndef _STATIC_HELPER_HH_
#define _STATIC_HELPER_HH_


class StaticHelper
{

//---------------------------- data -----------------------------------//
//---------------------------- data -----------------------------------//
//---------------------------- data -----------------------------------//

private:

  int  _locked;        // whether data changes are locked.
  int  _needs_saving;  // whether data has changed since last saved.
  int  _backed_up;     // whether data has been backed up since last changed.

  class StaticInformer  *_informer;  // not owned by this object (can be NULL).
  class StaticDataset   *_dataset;   // not owned by this object.
  class StaticUndo      *_undo;      // owned by this object.
  class StaticBackup    *_backup;    // owned by this object.

//------------------------- functions --------------------------------//
//------------------------- functions --------------------------------//
//------------------------- functions --------------------------------//

public:     // constructor and destructor.

           StaticHelper (class StaticInformer *informer,
                         class StaticDataset  *dataset,
                         class StaticKernal   *kernal);
  virtual ~StaticHelper ();

private:

  void  turnOnDataNeedsSavingFlag  ();
  void  turnOffDataNeedsSavingFlag ();
  int   dataChangesAreLocked       ();
  int   errorHappened              (const char *msg, int error);

//---------------------- public functions ----------------------------//
//---------------------- public functions ----------------------------//
//---------------------- public functions ----------------------------//

public:   // miscellaneous functions.
          // functions returning int return TRUE or FALSE.

  int      isLocked              ()  const  { return  _locked; }
  int      notLocked             ()  const  { return !_locked; }
  int      dataNeedsSaving       ()  const  { return  _needs_saving; }
  int      dataBackedUp          ()  const  { return  _backed_up; }
  void     lockData              ();
  void     unlockData            ();

//----------------------- pre/post functions -------------------------//
//----------------------- pre/post functions -------------------------//
//----------------------- pre/post functions -------------------------//

public:    // some of these return error = TRUE or FALSE.

  void   preSelectDataset    ();
  void  postSelectDataset    ();

  void   preUnselectDataset  ();
  void  postUnselectDataset  ();

  void   preChangeSelections (int ix, int iy, int nxchng, int nychng);
  void  postChangeSelections (int ix, int iy, int nxchng, int nychng);

  void   preNewActiveGroundPosition();
  void  postNewActiveGroundPosition();

  int    preRead   (void *doer, const char *working, char *msg);
  void  postRead   (const char *filename, const char *msg, int error);

  void   preSave   (const char *msg);
  void  postSave   (const char *filename, const char *msg, int error);

  int    preCopy   (void *doer, const char *msg);
  void  postCopy               (const char *msg);

  int    preTotalChanges (void *doer = 0, const char *msg = 0, int error = 0);
  void  postTotalChanges                 (const char *msg = 0, int error = 0);

  int    preChangeStaticValues (void *doer, const char *msg, int error,
                                int ix, int iy, int nxchng, int nychng);
  void  postChangeStaticValues             (const char *msg, int error,
                                int ix, int iy, int nxchng, int nychng);

  int    preChangeStattype ();
  void  postChangeStattype ();

  int    preChangeHeaderWords ();
  void  postChangeHeaderWords ();

  int    preTransformGroundPositions (void *doer = 0,
                                      const char *msg = 0, int error = 0);
  void  postTransformGroundPositions (const char *msg = 0, int error = 0);

//-------------------- interact with disk files -------------------------//
//-------------------- interact with disk files -------------------------//
//-------------------- interact with disk files -------------------------//

public:   // interact with backup file or undo file.
          // doer must match for maybe... functions.

  int  allowReadDeleteUndoFile (void *doer)  const;
  void maybeDeleteUndoFile     (void *doer);
  void maybeReadUndoFile       (void *doer);
  void saveUndoFile            (void *doer);
  void saveBackupFile          ();

public:    // learn name of last file read or saved.
           // "(errors) " preceding name if error occurred.

  const char *lastFileSaved       ()  const;
  const char *lastFileRead        ()  const;
  const char *lastBackupFileSaved ()  const;

//---------------------- end of functions ----------------------------//
//---------------------- end of functions ----------------------------//
//---------------------- end of functions ----------------------------//

} ;

#endif

//-------------------------------- end --------------------------------//
//-------------------------------- end --------------------------------//
//-------------------------------- end --------------------------------//
