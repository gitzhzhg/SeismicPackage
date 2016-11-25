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

//--------------------------- vf_helper.hh --------------------------//
//--------------------------- vf_helper.hh --------------------------//
//--------------------------- vf_helper.hh --------------------------//

//                header file for the VfHelper class
//                    not derived from any class
//                         subdirectory vf


      // This class helps VfDataset by taking care of several
      // flags needed by VfDataset, and by taking over various
      // duties.  This keeps a bunch of private functions out
      // of VfDataset, and thereby reduces the need to recompile
      // lots of files which rely on the VfDataset header file
      // when changes are made to these duties.


//------------------------ start of coding ---------------------------//
//------------------------ start of coding ---------------------------//
//------------------------ start of coding ---------------------------//


#ifndef _VF_HELPER_HH_
#define _VF_HELPER_HH_


class VfHelper
{

//--------------------------- data -----------------------------------//
//--------------------------- data -----------------------------------//
//--------------------------- data -----------------------------------//

public:

  enum { EDIT_BUT_SUPPRESS_BACKUPS = 999 };

private:

  class VfInformer       *_informer;    // pointer provided to this class.
  class VfDataset        *_dataset;     // pointer provided to this class.
  class VfKernal         *_kernal;      // pointer provided to this class.
  class VfBackup         *_backup;      // owned by this class.
  class VfUndo           *_undo;        // owned by this class.
  class HistoryCards     *_history;     // owned by this class.
 
  int   _needs_saving;  // whether data has changed since last saved.
  int   _backed_up;     // whether data has been backed up since last changed.
  int   _locked;        // whether data changes are locked.
  const int _editable;  // whether dataset is editable:
                        //   FALSE means not editable.
                        //   not FALSE means editable.
                        //   EDIT_BUT_SUPPRESS BACKUPS means suppress backups.
 

//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:   // constructor and destructor.

           VfHelper (int editable, VfInformer *informer,
                     VfDataset *dataset, VfKernal *kernal,
                     const char *progname);
  virtual ~VfHelper ();

public:   // get values.

  HistoryCards *history ()  const  { return _history; }

  int    dataNeedsSaving     ()  const  { return _needs_saving; }  // TF
  int    dataBackedUp        ()  const  { return    _backed_up; }  // TF
  int    isLocked            ()  const  { return       _locked; }  // TF
  int    notLocked           ()  const  { return      !_locked; }  // TF
  int    isEditable          ()  const  { return     _editable; }  // TF
  int    notEditable         ()  const  { return    !_editable; }  // TF

  const char *lastFileRead        ()  const;
  const char *lastFileSaved       ()  const;
  const char *lastBackupFileSaved ()  const;

public:   // set values.

  void   lockData            ();
  void   unlockData          ();

public:    // test whether an action is forbidden.

  int  dataChangesAreForbidden       ();
  int  readVelocityFileIsForbidden   (char *msg);
  int  saveVelocityFileIsForbidden   (char *msg);

public:   // interaction with backup and undo files.

  void  saveBackupFile          ();
  void  saveUndoFile            (void *doer);
  int   allowReadDeleteUndoFile (void *doer)  const;   // TF
  void  maybeReadUndoFile       (void *doer);
  void  maybeDeleteUndoFile     (void *doer);


//---------------------- private functions -------------------------//
//---------------------- private functions -------------------------//
//---------------------- private functions -------------------------//

private:

  void turnOnDataNeedsSavingFlag  ();
  void turnOffDataNeedsSavingFlag ();
  void broadcastMessage           (const char *msg);
  void broadcastBell              (int error);
  void broadcastHistory           (const char *card);


//------------------- pre and post operations ----------------------//
//------------------- pre and post operations ----------------------//
//------------------- pre and post operations ----------------------//

public:

  void  preSelectDataset      ();
  void postSelectDataset      ();
  void  preUnselectDataset    ();
  void postUnselectDataset    ();
  void  preTotalChanges       ();
  void postTotalChanges       ();

  void  preEdit(class VfEditBase *edit, const char *msg, int error, void *doer);
  void postEdit(class VfEditBase *edit, const char *msg, int error);

  void  preRead       (void *doer);
  void postRead       (const char *filename, const char *msg, int error,
                                                         int replaced_all);
  void  preSave       ();
  void postSave       (const char *filename, const char *msg, int error,
                                                         int saved_all);

  void  preChangeHeaderWords             ();
  void postChangeHeaderWords             ();
  void  preChangeMoveoutOrder            ();
  void postChangeMoveoutOrder            ();
  void  preChangeUnits                   ();
  void postChangeUnits                   ();
  void  preNewActiveVelocityFunction     (long ifun);
  void postNewActiveVelocityFunction     ();
  void  preNewReferenceVelocityFunction  (long ifun);
  void postNewReferenceVelocityFunction  ();
  void  preRemoveInsertVelocityFunctions (long ifun, long nrem, long nins,
                                                         int always = 0);
  void postRemoveInsertVelocityFunctions (long ifun, long nrem, long nins);
  void  preChangeSelections     (long ifun);
  void postChangeSelections     (long ifun);
  void  preModifyPicks          (long ifun, int type, long ipick, long nrem);
  void postModifyPicks          (long ifun, int type, long ipick);
  void  preChangeCoords         (long ifun, long nchng);
  void postChangeCoords         (long ifun, long nchng);
  void  preNewActivePicks       (long ifun, long nchng, long ipick);
  void postNewActivePicks       (long ifun, long nchng);
  void  preChangePickSelections (long ifun);
  void postChangePickSelections (long ifun);
  void  preNewDefaultTypes      (long ifun, long nchng);
  void postNewDefaultTypes      (long ifun, long nchng);
  void  preModifyStrings        (long ifun, long nchng);
  void postModifyStrings        (long ifun, long nchng);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
