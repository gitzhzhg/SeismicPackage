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
// $Id: vf_dataset.hh,v 1.2 2004/06/07 13:28:00 wjdone Exp $
// $Name:  $

//--------------------------- vf_dataset.hh --------------------------//
//--------------------------- vf_dataset.hh --------------------------//
//--------------------------- vf_dataset.hh --------------------------//

//                header file for the VfDataset class
//                    not derived from any class
//                         subdirectory vf


// This VfDataset class contains a velocity function dataset, including
// routines to manipulate the dataset and copy it to and from disk files.
// Data users are informed when any data changes.

// EDITABLE and LOCKED attributes:
// This dataset may be either editable or uneditable.  This attribute is
//   set when the dataset is created, and cannot be changed.  Uneditable
//   datasets can be replaced (by reading a file or by copying from another
//   dataset) or cleared.  Uneditable datasets cannot be edited or saved.
// Editable datasets can be either locked or unlocked.  This attribute
//   can be set at any time.  No data can be modified (replaced or edited
//   or cleared) when the dataset is locked.  Uneditable datasets are
//   considered unlocked, but with the restrictions described above.
// Editable datasets are automatically backed up as needed, unless the
//   editable parameter is set to EDIT_BUT_SUPPRESS_BACKUPS.

// INDICES:
// Some arguments and returned values are indices:
//     index = index of this dataset in an array of datasets.
//     ifun  = index of velocity function in an array of functions.
//     ipick = index of time/velocity pick in a velocity function.
//     ifun  >= 0 and ifun  < numVelocityFunctions().
//     ipick >= 0 and ipick < numPicks(ifun).
// Indices (and number of items) are of type long.
// Most functions assert if an index argument is out of range.

// ENUM CONSTANTS:
// Some arguments and returned values are enum constants:
//     type     = velocity function type.
//     select   = velocity function selection flag.
//     error    = velocity function error flag.
//     raytrace = velocity function raytrace flag.
// Enum constants are defined in the vf_constants.hh header file.
// Enum constants are of type int.
// Most functions assert if an enum argument is illegal.
// Some functions below are labelled ENUM if an argument or returned
//   value is an enum constant.

// TRUE/FALSE CONSTANTS:
// Some arguments and returned values are true/false constants.
// True/false constants are of type int.
// Some functions below are labelled TF if an argument or returned
//   value is a true/false constant.

// ACTIVE ITEMS:
// There is exactly one "active" velocity function at any given time.
//   The active function can be changed at any time.
// There is exactly one "active" time/velocity pick in each velocity
//   function at any given time.  The active pick is normally the
//   last pick inserted/removed/modified, but can be changed at any
//   time.
// The index of an active item is -1 if the list of items is empty.
// Items can be made active or inactive whether or not the dataset is
//   editable or locked.

// REFERENCE ITEMS:
// There is exactly one "reference" velocity function at any given time.
//   The reference function can be changed at any time.
// The index of a reference item is -1 if the list of items is empty.
// Items can be made reference or not reference whether or not the dataset is
//   editable or locked.

// SELECTED ITEMS:
// There are any number of selected velocity functions at any given
//   time, from zero up to numVelocityFunctions().
// Items can be selected or unselected whether or not the dataset is
//   editable or locked.

// VELOCITY FUNCTION TYPE:
// The default type (or the function argument "type") is used to
//   determine what the independent variables of the velocity function
//   are.  The independent variables (abscissa and ordinate) are as
//   shown in the vf_constants.h header file.
// In any functions which contain the argument "type" with a default value
//   of -1, if the argument is omitted (or set to -1), the default type
//   is used.  All functions assert if "type" is out-of-range (and not -1).
// The default type of a velocity function is important only in these
//   circumstances:
//   (1) When the "type" argument is omitted (or set to -1) in a function call.
//   (2) When the "mixed types" option is used when saving a velocity file.
// A velocity function type gets changed when you change the columns used
//   while editing the velocity function in a table.

// VELOCITY FUNCTION PICKS:
// The time/velocity picks for all velocity types are always available,
//   regardless of the default type.
// To keep all time/velocity arrays up-to-date, all functions which
//   insert/remove/modify picks (or read a velocity function from a file,
//   or create a velocity function) call the VfFunction function updatePicks
//   afterwards.  Therefore, if you wish to make several such calls in
//   succession, it is best to use functions which will accomplish all
//   modifications at once, if possible, or at least to minimize the
//   number of functions called, in order to eliminate unnecessary calls
//   to updatePicks.
// Some pick values might have a nil value at any time.
// The value of nil is returned by the constant FNIL in named_constants.h.



//------------------------ start of coding ---------------------------//
//------------------------ start of coding ---------------------------//
//------------------------ start of coding ---------------------------//


#ifndef _VF_DATASET_HH_
#define _VF_DATASET_HH_

#include "named_constants.h"

class VfDataset
{

//--------------------------- data -----------------------------------//
//--------------------------- data -----------------------------------//
//--------------------------- data -----------------------------------//

public:

  enum { BYTE_ARRAY, FLOAT_ARRAY };
  enum { EDIT_BUT_SUPPRESS_BACKUPS = 999 };

private:

  long   _index;     // index of this dataset within VfManager.
  int    _active;    // whether this dataset is active.
  int    _reference; // whether this dataset is the reference dataset.
  int    _selected;  // whether this dataset is selected.

  class VfInformer    *_informer;     // direct access by user allowed.
  class VfUtilities   *_utilities;    // direct access by user allowed.
  class VfKernal      *_kernal;       // owned by this class.
  class VfHelper      *_helper;       // owned by this class.
 
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:   // functions to be called only by VfDatasetArray.

  VfDataset (int editable, VfInformer *informer, VfUtilities *utilities,
                                         const char *progname = "");
  virtual ~VfDataset ();

public:   // these are to be called only by the designated class.

  void setIndexByVfDatasetArrayOnly         (long index) { _index  =  index; }
  void setActiveFlagByVfDatasetArrayOnly    (int active) { _active = active; }
  void setReferenceFlagByVfDatasetArrayOnly (int ref)    { _reference = ref; }
  void newBinTolerancesByVfManagerOnly      ();

public:   // get values.

  long   indexOfThisDataset  ()  const  { return        _index; }
  int    isActive            ()  const  { return       _active; }  // TF
  int    isReference         ()  const  { return    _reference; }  // TF
  int    isSelected          ()  const  { return     _selected; }  // TF
  int    notActive           ()  const  { return      !_active; }  // TF
  int    notReference        ()  const  { return   !_reference; }  // TF
  int    notSelected         ()  const  { return    !_selected; }  // TF

  class VfInformer     *informer   ()  const  { return _informer; }
  class VfUtilities    *utilities  ()  const  { return _utilities; }
  class VfKernal       *kernal     ()  const  { return _kernal; }
  class HistoryCards   *history    ()  const;

  int    dataNeedsSaving     ()  const;    // TF
  int    dataBackedUp        ()  const;    // TF
  int    isLocked            ()  const;    // TF
  int    notLocked           ()  const;    // TF
  int    isEditable          ()  const;    // TF
  int    notEditable         ()  const;    // TF

  const char *lastFileRead        ()  const;
  const char *lastFileSaved       ()  const;
  const char *lastBackupFileSaved ()  const;

public:   // set values.
          // lock/unlockData do nothing if dataset is uneditable.

  void   selectThisDataset   ();
  void   unselectThisDataset ();
  void   lockData            ();
  void   unlockData          ();


//-------------------- take various major actions -----------------------//
//-------------------- take various major actions -----------------------//
//-------------------- take various major actions -----------------------//

   // an undo file is saved if doer is not NULL.
   // an undo file can also be saved at any time by calling saveUndoFile.
   // the change can be undone by calling maybeReadUndoFile with same doer.
   // after an undo file is read, it is deleted.
   // whenever an undo file is saved, the previous undo file (if any)
   //   is deleted.

   // a backup file is saved (if necessary) after the change is made.
   // a backup file can also be saved at any time by calling saveBackupFile.

public:   // miscellaneous edits of data in memory.
          // this function uses parameters and algorithms in classes
          //   derived from VfEditBase.
          // this function does nothing if dataset is uneditable or locked.

  void  editDataset  (class VfEditBase *edit, void *doer);


public:  // read or save a velocity file.
         // these functions return error TRUE or FALSE.
         // these functions return a message string.
         // readVelocityFile:
         //    does nothing if dataset is locked.
         // saveVelocityFile:
         //    does nothing if dataset is uneditable, unless force is TRUE.
         // the second set of functions create their own temporary VfReadSave
         //    with default parameters _read_choice = READ_REPLACE and
         //    _save_choice = SAVE_ALL.

  int readVelocityFile    (const char *filename, char *msg,
                           class VfReadSave *readsave, void *doer);

  int saveVelocityFile    (const char *filename, char *msg,
                           class VfReadSave *readsave, int force = 0);

  int readVelocityFile    (const char *filename, char *msg, void *doer);
  int saveVelocityFile    (const char *filename, char *msg, int force = 0);

public: // interact with backup file or undo file.
        // a backup file is not saved if no changes have occurred.
        // doer must match for maybe... functions.
        // see oprim/undo_base.hh for further information.
        // a backup file is saved after reading the undo file.
        // saveBackupFile and saveUndoFile do nothing if dataset is uneditable.
        // maybeReadUndoFile does nothing if dataset is uneditable or locked.

  void  saveBackupFile          ();
  void  saveUndoFile            (void *doer);
  int   allowReadDeleteUndoFile (void *doer)  const;   // TF
  void  maybeReadUndoFile       (void *doer);
  void  maybeDeleteUndoFile     (void *doer);


//-------------------- pass thru to VfKernal --------------------------//
//-------------------- pass thru to VfKernal --------------------------//
//-------------------- pass thru to VfKernal --------------------------//

public:   // replace or append velocity functions from storage.
          // also copies _nhx, _nhy, _order, _nhosign, and _nhoexp.
          // also resets  active   velocity function if previously empty.
          // also resets reference velocity function if previously empty.
          // does NOT copy the _informer and _utilities pointers.

  void  replaceVelocityFunctions (const VfDataset *storage);
  void  appendVelocityFunctions  (const VfDataset *storage);


public:   // get values.

  int          getNhx                     ()  const;
  int          getNhy                     ()  const;
  int          getMoveoutOrder            ()  const;
  float        getNhosign                 ()  const;
  float        getNhoexp                  ()  const;
  int          getDistanceUnits           ()  const;
  const char  *getSymbolFromUnits         ()  const;
  const char  *getName                    ()  const;
  const char  *getAttributeName           ()  const;
  const char  *getAttributeUnits          ()  const;
  const char  *getTimeDepthUnits          ()  const;

public:   // set values.
          // these do nothing if dataset is uneditable or locked.

  void         setNhx                     (int   value);
  void         setNhy                     (int   value);
  void         setMoveoutOrder            (int   value);
  void         setDistanceUnits           (int   value);
  void         setUnitsFromSymbol         (const char *value);
  void         setName                    (const char *value);
  void         setAttributeName           (const char *value);
  void         setAttributeUnits          (const char *value);
  void         setTimeDepthUnits          (const char *value);

public: // get minimum and maximum values of all velocity functions.
        // these return 0.0 if there are no velocity functions.
        // the second set of functions adjust the returned value to bin center.

  float minimumXloc     ();
  float maximumXloc     ();
  float minimumYloc     ();
  float maximumYloc     ();

  float minimumXbinCenter ();   // uses xcenter and xwidth in VfUtilities.
  float maximumXbinCenter ();   // uses xcenter and xwidth in VfUtilities.
  float minimumYbinCenter ();   // uses ycenter and ywidth in VfUtilities.
  float maximumYbinCenter ();   // uses ycenter and ywidth in VfUtilities.

    // the following return 0.0 if there are no velocity functions, or if
    //   all velocity functions have no picks or only nil picks.
    // otherwise, nil picks are not used when getting minima and maxima.

  float minimumAbscissa  (int type);
  float maximumAbscissa  (int type);
  float minimumOrdinate  (int type);
  float maximumOrdinate  (int type);

  float minimumDepth     ();    float maximumDepth     ();
  float minimumTime      ();    float maximumTime      ();
  float minimumVrms      ();    float maximumVrms      ();
  float minimumVav       ();    float maximumVav       ();
  float minimumVint      ();    float maximumVint      ();
  float minimumVnmo      ();    float maximumVnmo      ();
  float minimumThickness ();    float maximumThickness ();
  float minimumOffset    ();    float maximumOffset    ();

  float getMinimumTime  (float depth)  const;
  float getMaximumTime  (float depth)  const;
  float getMinimumDepth (float  time)  const;
  float getMaximumDepth (float  time)  const;

public: // find matching velocity function.
        // returns index of matching velocity function.
        // xloc and yloc designate coordinates within the desired bin.
        // if there are two or more functions in matching bin, returns
        //   the one closest to xloc in the x direction.
        // returns -1 if there are no velocity functions in the matching bin.
        // returns -1 if there are no velocity functions at all.

  long findMatchingVelfun (float xloc, float yloc)  const;

public: // find nearest velocity function.
        // returns index of velocity function nearest to (xloc,yloc).
        // returns -1 if there are no velocity functions.
        // uses xperpix and yperpix to normalize x and y distance scales by
        //   dividing by (utilities()->getXwidth() * xperpix) (and same for y).

  long findNearestVelfun (float xloc, float yloc,
                          float xperpix, float yperpix)  const;

public: // returns indices of four velocity functions nearest to (xloc,yloc)
        //   (one function in each of four directions), and the corresponding
        //   weights to use for interpolating between these functions to create
        //   a function at (xloc,yloc).
        // velocity functions with errors corresponding to the specified type
        //   are ignored.
        // if there are no usable velocity functions, sets all indices to -1
        //   and all weights to zero.  otherwise, always sets all indices >= 0
        //   with weights which add to 1.0.
        // if there are less than four useable velocity functions, some of the
        //   indices will be duplicated and some of the weights will be zero.
        // uses xnorm and ynorm to normalize x and y distance scales by
        //   dividing by them.

  void findNearestVelfuns (float xloc, float yloc,
                           float xnorm, float ynorm,
                           long *ifun1, long *ifun2,
                           long *ifun3, long *ifun4,
                           float *weight1, float *weight2,
                           float *weight3, float *weight4,
                           int type)  const;

public: // check the sorted order of the velocity functions.
        // sets *xdir to  2 if xbins are sorted to ascending order.
        // sets *xdir to -2 if xbins are sorted to descending order.
        // sets *xdir to  1 if xbins are not sorted but initially ascending.
        // sets *xdir to -1 if xbins are not sorted but initially descending.
        // sets *xdir to  0 if no velocity functions or all xbins are the same.
        // sets *ydir to  2 if ybins are sorted to ascending order.
        // sets *ydir to -2 if ybins are sorted to descending order.
        // sets *ydir to  1 if ybins are not sorted but initially ascending.
        // sets *ydir to -1 if ybins are not sorted but initially descending.
        // sets *ydir to  0 if no velocity functions or all ybins are the same.
        // returns sorted = TRUE if both x and y are sorted.
        // otherwise returns sorted = FALSE.
        // it is assumed that x changes faster than y.
        // the second and third functions are alternatives to checkSort.

  int checkSort       (int *xdir, int *ydir)  const;    // returns sorted.
  int checkXdirection ()                      const;    // returns xdir.
  int checkYdirection ()                      const;    // returns ydir.

public: // find velocity function at next or previous x or y location.
        // ifun = index of current velocity function location.
        // direction >= 0 returns index of   next   velocity function.
        // direction <  0 returns index of previous velocity function.
        // findNearbyXloc returns index of next/prev x location with same ybin.
        // findNearbyYloc returns index of next/prev y location with same xbin.
        // returns -1 if no such location is found.
        // the velocity functions can be in any order.

  long findNearbyXloc (long ifun, int direction)  const;
  long findNearbyYloc (long ifun, int direction)  const;

public: // find velocity function at next or previous x or y location.
        // ifun = index of current velocity function location.
        // findNextXloc returns index of   next   function in x direction.
        // findPrevXloc returns index of previous function in x direction.
        // findNextYloc returns index of   next   function in y direction.
        // findPrevYloc returns index of previous function in y direction.
        // returns -1 if no such location is found.
        // if ifun == -2, searches from the active velocity function.
        // if ifun >= 0 and ifun < nfun, searches from location ifun.
        // otherwise (e.g. if fun == -1) uses xloc and yloc to find ifun first.
        // the velocity functions can be in any order.
        // these are convenience functions which call findMatchingVelfun,
        //   checkXdirection, checkYdirection, findNearbyXloc, and
        //   findNearbyYloc.
        // if searching from the active function, these simply return
        //   the value of a variable, and therefore are fast (updating
        //   the variable first if something has changed which might cause
        //   the nearest neighbors to the active function to change).

  long findNextXloc (long ifun = -2, float xloc = 0.0, float yloc = 0.0);
  long findPrevXloc (long ifun = -2, float xloc = 0.0, float yloc = 0.0);
  long findNextYloc (long ifun = -2, float xloc = 0.0, float yloc = 0.0);
  long findPrevYloc (long ifun = -2, float xloc = 0.0, float yloc = 0.0);

public: // find where to insert a velocity function to maintain sort.
        // returns index where new velocity function should be inserted.
    // xflag TRUE  means to create ascending  x-order if either is possible.
    // xflag FALSE means to create descending x-order if either is possible.
    // recommend setting xflag to semblance file order for new velocity file.

  long findWhereToInsert (float xloc, float yloc, int xflag = TRUE)  const;

public: // find existing velfun, or create new one.
        // also insert new velfun if none exists at the given coordinates.
        // then set this velfun to be the active one.
        // returns index of velocity function which was found or successfully
        //   inserted.  this will also be the active function.
        // returns -1 if an attempt to insert a velfun fails.
    // xflag TRUE  means to create ascending  x-order if either is possible.
    // xflag FALSE means to create descending x-order if either is possible.
    // recommend setting xflag to semblance file order for new velocity file.

  long findOrInsertVelfun (float xloc, float yloc, int xflag = TRUE);

public:   // routines to get list of velocity functions.
          // list contains functions from xmin thru xmax at given ybin.
          // list is returned in ascending or descending order.
          // duplicate locations in the same xybin are eliminated.
          // if xmin and xmax are FNIL, all xbins are allowed.
          // if ybin_choice is FNIL, all ybins are allowed, and the ybin
          //   width is considered infinite, so that all y values are lumped
          //   into the same ybin.  since duplicate locations are eliminated,
          //   this means that only one function will be returned for each
          //   xbin.
          // list should be allocated with enough space equal to the total
          //   number of velocity functions.

  void getVelfunInline    (float xmin, float xmax,
                           float ybin_choice, long *list, long *nlist)  const;
  void getVelfunCrossline (float ymin, float ymax,
                           float xbin_choice, long *list, long *nlist)  const;

public:  // convert velocities into an xbin timeslice array.
         // ymin is mapped to index [0].
         // ymax is mapped to index [nsamp-1].
         // sets velocity to zero if velfun has no picks or some nil picks.
         // if vint_grade is FALSE, does special interval velocity
         //   interpolation.

  void xbinToFloat (float ymin, float ymax,
                    float xbin_choice, int type, float time,
                    long nsamp, float *array, int vint_grade = TRUE)  const;

public:  // get interpolated velocity (ordinate) at specified time or
         //   depth (abscissa) and at interpolated (xloc,yloc) location.
         // type must not have abscissa = layer thickness.
         // if type is VTDP, getInterpolatedVelocity actually returns depth.
         // getInterpolatedDepth is a special case of getInterpolatedVelocity
         //   for type = VTDP.
         // these functions return an ordinate interpolated to the
         //   specified (xloc,yloc) from the nearest velocity functions
         //   in each of four directions.
         // if vint_grade is FALSE, does special interval velocity
         //   interpolation.
         // uses xnorm and ynorm to normalize the x and y distance scales
         //   (by dividing by them) when calculating interpolation weights.

  float getInterpolatedVelocity
           (float xloc, float yloc, float abscissa,
            int type, int vint_grade = TRUE,
            float xnorm = 1.0, float ynorm = 1.0)  const;

  float getInterpolatedTime
           (float xloc, float yloc, float depth,
            float xnorm = 1.0, float ynorm = 1.0)  const;

  float getInterpolatedDepth
           (float xloc, float yloc, float time,
            float xnorm = 1.0, float ynorm = 1.0)  const;

public: // get velocity function at interpolated (xloc,yloc) location.
        // type must not have abscissa = layer thickness.
        // this function returns a velocity function interpolated to the
        //   specified (xloc,yloc) from the nearest velocity functions
        //   (without errors in the specified type) in each of four directions.
        // if vint_grade is FALSE, does special interval velocity
        //   interpolation.
        // uses xnorm and ynorm to normalize the x and y distance scales
        //   (by dividing by them) when calculating interpolation weights.
        // the number of picks in the abscissa and ordinate arrays is returned.
        // if there are no velocity functions, returns npicks = 0.
        //
        // if the npicks argument is zero:
        //  - the abscissae and ordinates are returned.
        //  - the abscissae include the number of picks, and the time or depth
        //      values, from the nearest velocity function which has no errors.
        //  - npicks will of course never exceed MAXPICKS.
        //
        // if the npicks argument is greater than zero:
        //  - the abscissae[npicks] are input and the ordinates are returned.
        //  - npicks can have any value (e.g. npicks > MAXPICKS is OK).
        //  - if abscissae[] and ordinates[] are the same address, the
        //      abscissae will be replaced by the ordinates.
        //
        // warning regarding the npicks argument being zero:
        //  - this option is acceptable for re-gridding a velocity function
        //      file, and has the advantage that a new function falling at
        //      the location of an old function will have the same picks.
        //  - this option should NOT be used for creating very closely spaced
        //      velocity functions (e.g. trace-by-trace for generating an
        //      iso-velocity underlay, or for time-to-depth conversion),
        //      because discontinuities will occur at locations where the
        //      set of velocity functions changes (i.e. midway between
        //      velocity functions).

  long getInterpolatedVelocityFunction
                   (long npicks, float *abscissae, float *ordinates,
                    float xloc, float yloc,
                    int type, int vint_grade = TRUE,
                    float xnorm = 1.0, float ynorm = 1.0)  const;

public: // this function returns velocity picks interpolated to the
        //   specified (xloc,yloc) from the nearest velocity functions
        //   (without errors in the specified type) in each of four directions.
        // puts evenly-sampled velocity picks into ordinates[npicks].
        // tmin (minimum time or depth) is mapped to ordinates[0].
        // tmax (maximum time or depth) is mapped to ordinates[npicks-1].
        // fills ordinates[] with zeroes if there are no velocity functions.
        // fills ordinates[] with zeroes all functions have a type error.
        // type must not have abscissa = layer thickness.
        // if vint_grade is FALSE, does special interval velocity
        //   interpolation.

  void velToFloat (float xloc, float yloc, float tmin, float tmax,
                   long npicks, float *ordinates,
                   int type, int vint_grade = TRUE)  const;

public: // convert a seismic trace to or from time or depth.
        // tmin and dmin (minimum time and depth) are mapped to array[0].
        // tmax and dmax (maximum time and depth) are mapped to array[nsamp-1].
        // array_type is one of the above enums (BYTE_ARRAY or FLOAT_ARRAY).

  void timeToDepth (float xloc, float yloc, float tmin, float tmax,
                    long nsamp, void *array, int array_type,
                    float dmin, float dmax)  const;

  void depthToTime (float xloc, float yloc, float tmin, float tmax,
                    long nsamp, void *array, int array_type,
                    float dmin, float dmax)  const;

public: // start and stop multiple interpolations.
        // you can optionally call startMultipleInterpolations before
        //   a series of calls to one of these functions:
        //        getInterpolatedVelocityFunction
        //        velToFloat
        //        timeToDepth
        //        depthToTime
        //   and then call stopMultipleOperations when finished.
        // this is simply a speed-up procedure which might be useful
        //   when calling the function hundreds or thousands of times.
        // the following arguments must be the same in all calls:
        //       npicks (or nsamp) (and > 1).
        //       tmin, tmax, dmin, dmax, abscissae, type, vint_grade.

  void startMultipleInterpolations (long npicks);
  void stopMultipleInterpolations  ();


//-------------------- pass thru to VfFunctionArray ---------------------//
//-------------------- pass thru to VfFunctionArray ---------------------//
//-------------------- pass thru to VfFunctionArray ---------------------//

public:   // delete velocity functions.

  void deleteAllVelocityFunctions      ();
  void deleteSelectedVelocityFunctions ();
  void deleteEmptyVelocityFunctions    ();

public:   // get values.

  long         numVelocityFunctions                 ()  const;
  long         numVelocityFunctionsWithErrors       ()  const;
  long         numRaytracedVelocityFunctions        ()  const;
  long         numVelocityFunctionsWithBlankNames   ()  const;

  long         numVelocityFunctionsWithTypeErrors (int type)  const;

  long         getActiveVelocityFunction            ()  const;
  long         getReferenceVelocityFunction         ()  const;

public:   // set values.
          // setActiveVelocityFunction saves a backup file if necesary;
          //   this is useful when scanning to the next velocity function.

  void         setActiveVelocityFunction     (long ifun);
  void         setReferenceVelocityFunction  (long ifun);

public:   // insert or remove one velocity function.
          // appendVelocityFunction and insertVelocityFunction insert
          //   an empty function containing no picks.
          // these do nothing if dataset is uneditable or locked.

  void  appendVelocityFunction           ();
  void  insertVelocityFunction           (long ifun);
  void  insertVelocityFunctionFromBuffer (long ifun);
  void  removeVelocityFunction           (long ifun);
  void  removeVelocityFunctionToBuffer   (long ifun);


//-------------------- pass thru to VfFunctionSelect --------------------//
//-------------------- pass thru to VfFunctionSelect --------------------//
//-------------------- pass thru to VfFunctionSelect --------------------//

public:   // get select flags.

  long numSelectedVelocityFunctions      ()           const;
  int  velocityFunctionIsSelected        (long ifun)  const;   // TF
  int  getSelectFlag                     (long ifun)  const;   // ENUM

public:   // set select flags.

  void clearSelectFlags                  ();
  void incrementSelectFlag               (long ifun);
  void setSelectFlag                     (long ifun, int select);  // ENUM

public:   // before and after setting select flags.

    // these should be called before and after SEVERAL (more than one)
    //   consecutive calls to incrementSelectFlag or setSelectFlag.
    // these should not be called at any other time.

  void beforeSettingSeveralSelectFlags   ();
  void afterSettingSeveralSelectFlags    ();


//-------------------- pass thru to VfFunction --------------------------//
//-------------------- pass thru to VfFunction --------------------------//
//-------------------- pass thru to VfFunction --------------------------//

   // updates of velocity function picks use parameters in VfUtilities.

public:   // reallocate and reset picks.
          // these do nothing if dataset is uneditable or locked.

  void deleteAllVelocityFunctionPicks (long ifun);

  void resetNumPicks (long ifun, long npicks, int type = -1);

  void resetNumPicks (long ifun, long npicks, float *abscissae,
                                              float *ordinates, int type = -1);

public:   // get and set all velocity function contents.
          // get... copies contents into user-owned function.
          // set... copies contents from user-owned function.

  void getVelocityFunctionContents
                 (long ifun,       class VfFunction *function)  const;
  void setVelocityFunctionContents
                 (long ifun, const class VfFunction *function);

public:   // get strings corresponding to velocity function type.

  const char     *getTypeSymbol       (long ifun, int type = -1)  const;
  const char     *getTypeDescription  (long ifun, int type = -1)  const;

public:   // get general values.

  float        getXloc         (long ifun)  const;
  float        getYloc         (long ifun)  const;
  long         numPicks        (long ifun)  const;
  long         getActivePick   (long ifun)  const;
  int          getDefaultType  (long ifun)  const;                 // ENUM
  int          getErrorFlag    (long ifun)  const;                 // ENUM
  int          getRaytraceFlag (long ifun)  const;                 // ENUM
  int          typeError       (long ifun, int type = -1)  const;  // T/F

  const char  *getVfid       (long ifun)  const;
  const char  *getProject    (long ifun)  const;
  const char  *getLine       (long ifun)  const;
  const char  *getRdate      (long ifun)  const;
  const char  *getPdate      (long ifun)  const;
  const char  *getUserid     (long ifun)  const;
  const char  *getComment    (long ifun)  const;
  float        getElevation  (long ifun)  const;  // from comment field.
  float        getWaterDepth (long ifun)  const;  // from comment field.
                // elevation or water depth is nil if not present or decodable.

public:   // get individual pick values.

  float        getDepth     (long ifun, long ipick)  const;
  float        getTime      (long ifun, long ipick)  const;
  float        getThickness (long ifun, long ipick)  const;
  float        getVrms      (long ifun, long ipick)  const;
  float        getVav       (long ifun, long ipick)  const;
  float        getVint      (long ifun, long ipick)  const;
  float        getVnmo      (long ifun, long ipick)  const;
  float        getOffset    (long ifun, long ipick)  const;

  float        getAbscissa  (long ifun, long ipick, int type = -1)  const;
  float        getOrdinate  (long ifun, long ipick, int type = -1)  const;

public:   // get arrays of pick values.

  void         getDepthArray     (long ifun, float *values)  const;
  void         getTimeArray      (long ifun, float *values)  const;
  void         getThicknessArray (long ifun, float *values)  const;
  void         getVrmsArray      (long ifun, float *values)  const;
  void         getVavArray       (long ifun, float *values)  const;
  void         getVintArray      (long ifun, float *values)  const;
  void         getVnmoArray      (long ifun, float *values)  const;
  void         getOffsetArray    (long ifun, float *values)  const;

  void    getAbscissaArray  (long ifun, float *values, int type = -1)  const;
  void    getOrdinateArray  (long ifun, float *values, int type = -1)  const;

public:  // find bracketing pick indices ia and ib, where normally ib = ia+1.
         // the values checked are the abscissae (time or depth) for the
         //   specified type (type should not have abscissa = layer thickness).
         // returns ia = last  pick index with value <= specified value.
         // returns ib = first pick index with value >= specified value.
         // returns ia = ib = -1 for any of these reasons:
         //   there are no picks.
         //   the specified abscissa value (time or depth) is nil.
         //   ANY abscissa values are nil.
         //   the abscissa values are not in strictly ascending order.
         // special cases:
         // if specified value < first abscissa value,
         //                   returns ia = -1 and ib = 0.
         // if specified value >  last abscissa value,
         //                   returns ia = numPicks()-1 and ib = numPicks().
         // if specified value matches an abscissa value,
         //                   returns ia = ib = index of matching abscissa.
         // values are assumed to match if their difference is <= tolerance
         //   as given in VfUtilities.

  void  findBracketingAbscissae
              (long ifun, float value, int type, long *ia, long *ib)  const;

public:  // get interpolated velocity (ordinate) at specified time or
         //   depth (abscissa).
         // type should not have abscissa = layer thickness.
         // if vint_grade is FALSE, does special interval velocity
         //   interpolation.
         // if type is VTDP, getInterpolatedVelocity actually returns depth.
         // getInterpolatedDepth is a special case of getInterpolatedVelocity
         //   for type = VTDP.

  float getInterpolatedVelocity
     (long ifun, float abscissa, int type = -1, int vint_grade = TRUE)  const;

  float getInterpolatedTime  (long ifun, float depth)  const;
  float getInterpolatedDepth (long ifun, float time )  const;

public:  // puts evenly-sampled velocity picks into ordinates[npicks].
         // tmin (minimum time or depth) is mapped to ordinates[0].
         // tmax (maximum time or depth) is mapped to ordinates[npicks-1].
         // fills ordinates[] with zeroes if this type has a type error.
         // type must not have abscissa = layer thickness.
         // if vint_grade is FALSE, does special interval velocity
         //   interpolation.

  void velToFloat (long ifun, float tmin, float tmax,
                   long npicks, float *ordinates,
                   int type = -1, int vint_grade = TRUE)  const;

public:   // set general values.
          // these do nothing if dataset is uneditable or locked.

  void  setXloc        (long ifun, float value);
  void  setYloc        (long ifun, float value);
  void  setActivePick  (long ifun, long  value);
  void  setDefaultType (long ifun, int   value);

  void  setVfid        (long ifun, const char *value);
  void  setProject     (long ifun, const char *value);
  void  setLine        (long ifun, const char *value);
  void  setRdate       (long ifun, const char *value);
  void  setPdate       (long ifun, const char *value);
  void  setUserid      (long ifun, const char *value);
  void  setComment     (long ifun, const char *value);
  void  setElevation   (long ifun, float   elevation);   // sets comment field.
  void  setWaterDepth  (long ifun, float water_depth);   // sets comment field.

public:   // set individual pick values.
          // these functions reset the active pick.
          // these do nothing if dataset is uneditable or locked.

  void  setDepth     (long ifun, long ipick, float value,    int type = -1);
  void  setTime      (long ifun, long ipick, float value,    int type = -1);
  void  setThickness (long ifun, long ipick, float value,    int type = -1);
  void  setVrms      (long ifun, long ipick, float value,    int type = -1);
  void  setVav       (long ifun, long ipick, float value,    int type = -1);
  void  setVint      (long ifun, long ipick, float value,    int type = -1);
  void  setVnmo      (long ifun, long ipick, float value,    int type = -1);
  void  setOffset    (long ifun, long ipick, float value,    int type = -1);

  void  setAbscissa  (long ifun, long ipick, float abscissa, int type = -1);
  void  setOrdinate  (long ifun, long ipick, float ordinate, int type = -1);
  void  replacePick  (long ifun, long ipick, float abscissa,
                                             float ordinate, int type = -1);

public:   // set arrays of pick values.
          // these do nothing if dataset is uneditable or locked.

  void  setDepthArray     (long ifun, const float *values,    int type = -1);
  void  setTimeArray      (long ifun, const float *values,    int type = -1);
  void  setThicknessArray (long ifun, const float *values,    int type = -1);
  void  setVrmsArray      (long ifun, const float *values,    int type = -1);
  void  setVavArray       (long ifun, const float *values,    int type = -1);
  void  setVintArray      (long ifun, const float *values,    int type = -1);
  void  setVnmoArray      (long ifun, const float *values,    int type = -1);
  void  setOffsetArray    (long ifun, const float *values,    int type = -1);

  void  setAbscissaArray  (long ifun, const float *abscissae, int type = -1);
  void  setOrdinateArray  (long ifun, const float *ordinates, int type = -1);
  void  replaceAllPicks   (long ifun, const float *abscissae,
                                      const float *ordinates, int type = -1);

public:   // insert or remove one time/velocity pick.
          // these functions reallocate the arrays.
          // these functions reset the active pick.
          // these do nothing if dataset is uneditable or locked.

  void  appendNilPick        (long ifun,                      int type = -1);
  void  insertNilPick        (long ifun, long ipick,          int type = -1);
  void  insertPickFromBuffer (long ifun, long ipick,          int type = -1);
  void  removePick           (long ifun, long ipick,          int type = -1);
  void  removePickToBuffer   (long ifun, long ipick,          int type = -1);

  void  appendPick  (long ifun,              float abscissa,
                                             float ordinate,  int type = -1);
  void  insertPick  (long ifun, long ipick,  float abscissa,
                                             float ordinate,  int type = -1);

public:   // invoke or cancel ray tracing.
          // if _raytrace is RAYTRACE_NO     , cancelRayTracing does nothing.
          // if _raytrace is RAYTRACE_FORWARD, invokeRayTracing does nothing.
          // if _raytrace is RAYTRACE_INVERSE, invokeRayTracing does nothing.
          // these functions reset the _raytrace variable in VfFunction.
          // these do nothing if dataset is uneditable or locked.

  void  invokeRayTracing (long ifun, int type = -1);
  void  cancelRayTracing (long ifun, int type = -1);


public:   // get pick select flags.

  long        numSelectedPicks    (long ifun)              const;
  int         pickIsSelected      (long ifun, long ipick)  const;   // TF
  int         getPickSelectFlag   (long ifun, long ipick)  const;   // ENUM
  const char *getPickSelectString (long ifun, long ipick)  const;

public:   // set pick select flags.

  void beforeSettingSeveralPickSelectFlags (long ifun);
  void setOneOfSeveralPickSelectFlags      (long ifun, long ipick, int select);
  void afterSettingSeveralPickSelectFlags  (long ifun);

  void clearPickSelectFlags      (long ifun);
  void incrementPickSelectFlag   (long ifun, long ipick);
  void setPickSelectFlag         (long ifun, long ipick, int select);  // ENUM
  void toggleAllPickSelections   (long ifun);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
