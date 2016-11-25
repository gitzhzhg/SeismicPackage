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

//--------------------------- static_dataset.hh --------------------------//
//--------------------------- static_dataset.hh --------------------------//
//--------------------------- static_dataset.hh --------------------------//

//              header file for the StaticDataset class
//                  not derived from any class
//                      subdirectory stat


#ifndef _STATIC_DATASET_HH_
#define _STATIC_DATASET_HH_


class StaticDataset
{

//---------------------------- data -----------------------------------//
//---------------------------- data -----------------------------------//
//---------------------------- data -----------------------------------//

public:

  enum { ENDFLAG_N = 1,   // running average narrowed end range (graded ends).
         ENDFLAG_T = 2,   // running average truncated end range.
         ENDFLAG_E = 3,   // running average extended end range.
         ENDFLAG_S = 4    // running average shifted end range.
       };

  enum { INTERP_NEAR = 1, // while resampling: use nearest values
                          //         (nils preserved).
         INTERP_TERP = 2  // while resampling: interpolate among non-nil
                          //         values (some nils might survive).
       };

  enum { EXTRAP_EDGE = 1, // while resampling: extrapolate with edge value.
         EXTRAP_NILS = 2, // while resampling: extrapolate with nils.
         EXTRAP_ZERO = 3  // while resampling: extrapolate with zero.
       };

private:

  int     _index;         // index of this StaticDataset.
  int     _active;        // whether this StaticDataset is active.
  int     _reference;     // whether this StaticDataset is reference.
  int     _selected;      // whether this StaticDataset is selected.

  class StaticInformer  *_informer;  // not owned by this object (can be NULL).
  class StaticKernal    *_kernal;        // owned by this object.
  class StaticHelper    *_helper;        // owned by this object.

//------------------------- functions --------------------------------//
//------------------------- functions --------------------------------//
//------------------------- functions --------------------------------//

public:     // constructor and destructor.

           StaticDataset (const char *progname = "unknown",
                          StaticInformer *informer = 0);

  virtual ~StaticDataset ();

public:    // to be called only by StaticDatasetArray.

  void  setIndexByDatasetArrayOnly         (int index)  { _index = index; }
  void  setActiveFlagByDatasetArrayOnly    (int active) { _active = active; }
  void  setReferenceFlagByDatasetArrayOnly (int ref)    { _reference = ref; }

//---------------------- misc functions ----------------------------//
//---------------------- misc functions ----------------------------//
//---------------------- misc functions ----------------------------//

public:  // find out whether file size (nx and ny) can be changed.
         // returns TRUE or FALSE.
         // file size can be changed only if all values are nil.

  int  allowChangingFileSize ()  const;

public:   // miscellaneous functions.
          // functions returning int return TRUE or FALSE.

  class StaticInformer *informer ()  const  { return _informer; }
  class HistoryCards   *history  ()  const;

  int      indexOfThisDataset    ()  const  { return  _index; }
  int      isActive              ()  const  { return  _active; }
  int      notActive             ()  const  { return !_active; }
  int      isReference           ()  const  { return  _reference; }
  int      notReference          ()  const  { return !_reference; }
  int      isSelected            ()  const  { return  _selected; }
  int      notSelected           ()  const  { return !_selected; }
  int      isLocked              ()  const;
  int      notLocked             ()  const;
  int      dataNeedsSaving       ()  const;
  int      dataBackedUp          ()  const;
  void     lockData              ();
  void     unlockData            ();
  void     selectThisDataset     ();
  void     unselectThisDataset   ();

//------------------------- get values ---------------------------//
//------------------------- get values ---------------------------//
//------------------------- get values ---------------------------//

public:   // indices  ix   range from 0 thru           getNx()-1.
          // indices  iy   range from 0 thru           getNy()-1.
          // indices ival  range from 0 thru       numValues()-1.
          // functions using an index assert if an index is out of range.
          // getMatching... functions assert if xbin or ybin is out of range.
          // getNearest... functions use indices nearest the specified bins.
          // getUnconstrained... functions might return indices out of range.
          // getCenterValue returns nil if xbin or ybin is out of range.
          // getTerpValue returns nil if any needed adjacent values are nil.
          // interp and extrap must be one of the above enums.

  const char *getStattype     ()  const;
  int         getNhx          ()  const;
  int         getNhy          ()  const;
  int         getNhx2         ()  const;
  int         getNhy2         ()  const;
  float       getX1           ()  const;
  float       getY1           ()  const;
  float       getXinc         ()  const;
  float       getYinc         ()  const;
  int         getNx           ()  const;
  int         getNy           ()  const;
  float       getXend         ()  const;
  float       getYend         ()  const;

  int         numValues       ()  const;
  int         numNilValues    ()  const;
  int         numLiveValues   ()  const;
  float       minimumValue    ()  const;
  float       maximumValue    ()  const;
  float       sumValues       ()  const;
  float       averageValue    ()  const;

  float       getActiveXbin   ()  const;
  float       getActiveYbin   ()  const;
  int         getActiveIx     ()  const;
  int         getActiveIy     ()  const;

  int         numSelections   ()                const;
  int         isSelected      (int ival)        const;    // returns T/F.
  int         isSelected      (int ix, int iy)  const;    // returns T/F.

  int         getMatchingIx         (float xbin)    const;
  int         getMatchingIy         (float ybin)    const;
  int         getNearestIx          (float xbin)    const;
  int         getNearestIy          (float ybin)    const;
  int         getUnconstrainedIx    (float xbin)    const;
  int         getUnconstrainedIy    (float ybin)    const;
  float       getXbin               (int   ix)      const;
  float       getYbin               (int   iy)      const;
  float       getMatchingXbinCenter (float xbin)    const;
  float       getMatchingYbinCenter (float ybin)    const;
  float       getNearestXbinCenter  (float xbin)    const;
  float       getNearestYbinCenter  (float ybin)    const;

  float       getValue          (int ival)                 const;
  float       getValue          (int ix, int iy)           const;
  float       getMatchingValue  (float xbin, float ybin)   const;
  float       getNearestValue   (float xbin, float ybin)   const;
  float       getCenterValue    (float xbin, float ybin)   const;
  float       getTerpValue      (float xbin, float ybin)   const;
  float       getResampledValue (float xbin, float ybin,
                                 int interp, int extrap)   const;

  void        getStaticValues   (float *values)            const;

  float       getWeight (int ival,       int xdist, int ydist)  const;
  float       getWeight (int ix, int iy, int xdist, int ydist)  const;

//------------------------- set values ---------------------------//
//------------------------- set values ---------------------------//
//------------------------- set values ---------------------------//

public:   // indices ix   range from 0 thru     getNx()-1.
          // indices iy   range from 0 thru     getNy()-1.
          // indices ival range from 0 thru numValues()-1.
          // setValue         asserts if  ival        is out of range.
          // setValue         asserts if  ix  or  iy  is out of range.
          // setMatchingValue asserts if xbin or ybin is out of range.
          // setNx, setNy, setXend, and setYend assert unless
          //   allowChangingFileSize() returns TRUE (i.e. array is NULL).

  void   setStattype      (const char *stattype);
  void   setNhx           (int   nhx);
  void   setNhy           (int   nhy);
  void   setNhx2          (int   nhx2);
  void   setNhy2          (int   nhy2);
  void   setX1            (float x1);
  void   setY1            (float y1);
  void   setXinc          (float xinc);
  void   setYinc          (float yinc);
  void   setNx            (int   nx);     // cannot use if non-nil data exists.
  void   setNy            (int   ny);     // cannot use if non-nil data exists.
  void   setXend          (float xend);   // cannot use if non-nil data exists.
  void   setYend          (float yend);   // cannot use if non-nil data exists.

  void   setActiveXbin    (float xbin);
  void   setActiveYbin    (float ybin);
  void   setActiveIx      (int   ix);
  void   setActiveIy      (int   iy);

  void   setSelections    (int ix, int iy, int nxsel, int nysel, int value);
  void   clearSelections  ();

  void   setValue         (int   ival,             float value);
  void   setValue         (int   ix  , int   iy  , float value);
  void   setMatchingValue (float xbin, float ybin, float value);
  void   setStaticValues  (void *doer, const float *values);
  void   copyData         (void *doer, const StaticDataset *other);

//----------------------- major edits -------------------------------//
//----------------------- major edits -------------------------------//
//----------------------- major edits -------------------------------//

public:  // an undo file is saved (identified by the doer).
         // endflag, interp, and extrap must be one of the above enums.

  void  gradeStaticValues              (void *doer, float xmin, float xmax,
                                                    float ymin, float ymax);

  void  gradeStaticValues              (void *doer, int ixmin, int ixmax,
                                                    int iymin, int iymax);

  void  removeRunningAverage           (void *doer, int trim, float xrun,
                                                    float yrun, int endflag);

  void  smoothStaticValues             (void *doer, int trim, float xrun,
                                                    float yrun, int endflag);

  void  integrateStaticValues          (void *doer);
  void  clearStaticValues              (void *doer);
  void  randomizeStaticValues          (void *doer);
  void  multiplyStaticValuesByConstant (void *doer, float constant);
  void  addConstantToStaticValues      (void *doer, float constant);
  void  replaceNilsWithSpecifiedValue  (void *doer, float constant);
  void  replaceRangeOfValuesWithNils   (void *doer, float range1, float range2);
  void  replaceNilsWithTerpValues      (void *doer);
  void  interpNilsInXdirection         (void *doer);
  void  interpNilsInYdirection         (void *doer);
  void  interpNilsFromNearby           (void *doer, int ixdist,
                                                    int iydist, int require);

  void  transformGroundPositions       (void *doer,
                        float x1old, float x2old, float y1old, float y2old,
                        float x1new, float x2new, float y1new, float y2new);

  void  resampleStaticValues           (void *doer, int interp, int extrap,
                                                    float x1, float y1,
                                                    float xinc, float yinc,
                                                    int nx, int ny);

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

public:  // read or save a file.
         // an undo file is saved (identified by the doer).
         // these functions return a message.
         // these functions return error = TRUE or FALSE.

         // when reading a file, the pickle jar contains all required
         // header info (including how-to-read info) to use (obtained by
         // an earlier validate and/or scan and/or user input).

         // when saving a file, the pickle jar contains optional information
         // (such as encoding and fields) to use (obtained by an earlier
         // user input); additional header info is added from the static
         // dataset.

  int  readForeign (void *doer, const char *filename,
                    class StatioWrapper *statio, char *msg);

  int  saveFile    (const char *filename,
                    class StatioWrapper *statio, char *msg);

public:  // augment pickle jar with information from the data object
         // which will be used when saving the file.  this is a
         // convenience for gui displays where the user might want
         // to see the information prior to saving the file.
         // the pickle jar is not cleared first.

  void updatePjar (class StatioWrapper *statio, int skiphist);

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

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
