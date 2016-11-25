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

//-------------------------- static_kernal.hh -------------------------//
//-------------------------- static_kernal.hh -------------------------//
//-------------------------- static_kernal.hh -------------------------//

//              header file for the StaticKernal class
//                  not derived from any class
//                      subdirectory stat


   // This class maintains the kernal (guts) of a static dataset.


#ifndef _STATIC_KERNAL_HH_
#define _STATIC_KERNAL_HH_

#include "named_constants.h"


class StaticKernal
{

//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//
//--------------------------- data ----------------------------------//

public:

  enum { OPTION_REMOVE = 1,  // remove running average from the data.
         OPTION_SMOOTH = 2   // smooth the data with a running average.
       };

  enum { ENDFLAG_N = 1,   // running average narrowed end range (graded ends).
         ENDFLAG_T = 2,   // running average truncated end range.
         ENDFLAG_E = 3,   // running average extended end range.
         ENDFLAG_S = 4    // running average shifted end range.
       };                 // these must be same as in StatutilWrapper.

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

  class LogicArray      *_select;        // owned by this object.
  class LimitsKeeper    *_limits;        // owned by this object.
  class HistoryCards    *_history;       // owned by this object.

  int     _ixactive;           // active X ground position index.
  int     _iyactive;           // active Y ground position index.

    float *_pointer;           // pointer to array of static values.

    char   _stattype[60];      // type of static file.
    int    _nhx         ;      // x header word number.       
    int    _nhy         ;      // y header word number.       
    int    _nhx2        ;      // second x header word number.
    int    _nhy2        ;      // second y header word number.
    float  _x1          ;      // first x bin center.       
    float  _y1          ;      // first y bin center.       
    float  _xinc        ;      // x bin increment.         
    float  _yinc        ;      // y bin increment.         
    int    _nx          ;      // number of x bins.        
    int    _ny          ;      // number of y bins.        

//----------------------------- functions ---------------------------//
//----------------------------- functions ---------------------------//
//----------------------------- functions ---------------------------//

public:      // constructor and destructor.

           StaticKernal (const char *progname);
  virtual ~StaticKernal ();

//---------------------- private functions -------------------------//
//---------------------- private functions -------------------------//
//---------------------- private functions -------------------------//

private:

  int    allStaticValuesAreNil    (char *msg)  const;
  int    allStaticValuesAreLive   (char *msg)  const;
  void   freePointer              ();
  void   allocPointerIfNecessary  ();
  void   findLimits               ();
  void   updateLimits             (float old_value, float new_value);
  void   startReset               ();
  void   finishReset              ();
  void   doReset                  ();

  int    privateReadForeign (const char *filename,
                             class StatioWrapper *statio, char *msg,
                             int skipstat = FALSE);

  int    privateSaveFile    (const char *filename,
                             class StatioWrapper *statio, char *msg,
                             int skiphist = FALSE);

//---------------------- public functions -------------------------//
//---------------------- public functions -------------------------//
//---------------------- public functions -------------------------//

public:  // find out whether file size (nx and ny) can be changed.
         // returns TRUE or FALSE.
         // file size can be changed only if all values are nil.

  int  allowChangingFileSize ()  const;

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
          // adjustment == 0 rounds up or down to nearest bin.
          // adjustment >  0 rounds up to next bin.
          // adjustment <  0 rounds down to previous bin.

  HistoryCards *history()  const  { return _history; }

  const char *getStattype     ()  const  { return _stattype; }
  int         getNhx          ()  const  { return _nhx     ; }
  int         getNhy          ()  const  { return _nhy     ; }
  int         getNhx2         ()  const  { return _nhx2    ; }
  int         getNhy2         ()  const  { return _nhy2    ; }
  float       getX1           ()  const  { return _x1      ; }
  float       getY1           ()  const  { return _y1      ; }
  float       getXinc         ()  const  { return _xinc    ; }
  float       getYinc         ()  const  { return _yinc    ; }
  int         getNx           ()  const  { return _nx      ; }
  int         getNy           ()  const  { return _ny      ; }
  float       getXend         ()  const  { return _x1 + (_nx - 1) * _xinc; }
  float       getYend         ()  const  { return _y1 + (_ny - 1) * _yinc; }

  int         numValues       ()  const  { return _nx * _ny; }
  int         numNilValues    ()  const;
  int         numLiveValues   ()  const;
  float       minimumValue    ();
  float       maximumValue    ();
  float       sumValues       ()  const;
  float       averageValue    ()  const;

  float       getActiveXbin   ()  const;
  float       getActiveYbin   ()  const;
  int         getActiveIx     ()  const  { return _ixactive; }
  int         getActiveIy     ()  const  { return _iyactive; }

  int         numSelections   ()                  const;
  int         isSelected      (int  ival)         const;    // returns T/F.
  int         isSelected      (int  ix, int  iy)  const;    // returns T/F.

  int         getMatchingIx         (float xbin)                      const;
  int         getMatchingIy         (float ybin)                      const;
  int         getNearestIx          (float xbin, int adjustment = 0)  const;
  int         getNearestIy          (float ybin, int adjustment = 0)  const;
  int         getUnconstrainedIx    (float xbin)                      const;
  int         getUnconstrainedIy    (float ybin)                      const;
  float       getXbin               (int  ix)                         const;
  float       getYbin               (int  iy)                         const;
  float       getMatchingXbinCenter (float xbin)                      const;
  float       getMatchingYbinCenter (float ybin)                      const;
  float       getNearestXbinCenter  (float xbin, int adjustment = 0)  const;
  float       getNearestYbinCenter  (float ybin, int adjustment = 0)  const;

  float       getValue              (int  ival)                const;
  float       getValue              (int  ix, int  iy)         const;
  float       getMatchingValue      (float xbin, float ybin)   const;
  float       getNearestValue       (float xbin, float ybin)   const;
  float       getCenterValue        (float xbin, float ybin)   const;
  float       getTerpValue          (float xbin, float ybin)   const;
  float       getResampledValue     (float xbin, float ybin,
                                     int interp, int extrap)   const;

  void        getStaticValues       (float *values)            const;

  float       getWeight (int  ival,        int  xdist, int  ydist)  const;
  float       getWeight (int  ix, int  iy, int  xdist, int  ydist)  const;

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
          // history cards are added (non-redundantly) by these routines.

  void   clear            ();

  void   setStattype      (const char *stattype);
  void   setNhx           (int              nhx);
  void   setNhy           (int              nhy);
  void   setNhx2          (int             nhx2);
  void   setNhy2          (int             nhy2);
  void   setX1            (float             x1);
  void   setY1            (float             y1);
  void   setXinc          (float           xinc);
  void   setYinc          (float           yinc);
  void   setNx            (int               nx);
  void   setNy            (int               ny);
  void   setXend          (float           xend);
  void   setYend          (float           yend);

  void   setActiveXbin    (float xbin);
  void   setActiveYbin    (float ybin);
  void   setActiveIx      (int  ix);
  void   setActiveIy      (int  iy);

  void   setSelections    (int  ix, int  iy, int  nxsel, int  nysel, int value);
  void   clearSelections  ();

  void   setValue         (int   ival,             float value);
  void   setValue         (int     ix, int     iy, float value);
  void   setMatchingValue (float xbin, float ybin, float value);
  void   setStaticValues  (const float *values);
  void   copyData         (const StaticKernal *other);

//----------------------- major edits -------------------------------//
//----------------------- major edits -------------------------------//
//----------------------- major edits -------------------------------//

public:  // these return error (TRUE or FALSE) and msg.
         // option, endflag, interp, and extrap must be one of the above enums.
         // history cards are added by these routines.

         // if testing is TRUE:
         //   the operation will not be performed.
         //   error is FALSE if the operation can be performed.
         //   error is TRUE if there are parameter errors.
         //   error is TRUE if the the operation will be a do-nothing.
         //   sets msg to working message or error message.

         // if testing is FALSE:
         //   the operation will be performed if possible.
         //   error is FALSE if the operation has been performed.
         //   error is TRUE if the operation was not performed.
         //   sets msg to success message or error message.

  int  gradeStaticValues       (int testing, int  ixmin, int  ixmax,
                                int  iymin, int  iymax, char *msg);

  int  removeRunningAverage    (int testing, int option, int trim, float xrun,
                                float yrun, int endflag, char *msg);

  int  integrateStaticValues          (int testing, char *msg);
  int  clearStaticValues              (int testing, char *msg);
  int  randomizeStaticValues          (int testing, char *msg);
  int  multiplyStaticValuesByConstant (int testing, float constant, char *msg);
  int  addConstantToStaticValues      (int testing, float constant, char *msg);
  int  replaceNilsWithSpecifiedValue  (int testing, float constant, char *msg);

  int  replaceRangeOfValuesWithNils   (int testing, float range1,
                                       float range2, char *msg);

  int  replaceNilsWithTerpValues      (int testing, char *msg);
  int  interpNilsInXdirection         (int testing, char *msg);
  int  interpNilsInYdirection         (int testing, char *msg);

  int  interpNilsFromNearby     (int testing, int  ixdist, int  iydist,
                                 int require, char *msg);

  int  transformGroundPositions (int testing,
                 float x1old, float x2old, float y1old, float y2old,
                 float x1new, float x2new, float y1new, float y2new,
                 int *reversing, char *msg);

  int  resampleStaticValues    (int testing, int interp, int extrap,
                 float x1, float y1, float xinc, float yinc,
                 int  nx, int  ny, char *msg);

//-------------------- interact with disk files -------------------------//
//-------------------- interact with disk files -------------------------//
//-------------------- interact with disk files -------------------------//

public:  // read or save a file.
         // these functions return a message.
         // these functions return error = TRUE or FALSE.

         // when reading a file, the pickle jar contains all required
         // header info (including how-to-read info) to use (obtained by
         // an earlier validate and/or scan and/or user input).

         // when saving a file, the pickle jar contains optional information
         // (such as encoding and fields) to use (obtained by an earlier
         // user input); additional header info is added from the static
         // dataset.

         // set SKIPSTAT to TRUE to skip reading the static values.
         // when SKIPSTAT is TRUE, the static file is not actually opened
         // because the required header information is already in statio.

  int  readForeign (const char *filename,
                    class StatioWrapper *statio, char *msg,
                    int skipstat = FALSE);

  int  saveFile    (const char *filename,
                    class StatioWrapper *statio, char *msg);

public:  // update pickle jar with information from the data object
         // which will be used when saving the file.  this is a
         // convenience for gui displays where the user might want
         // to see the information prior to saving the file.
         // the pickle jar is not cleared first.

  void updatePjar (class StatioWrapper *statio, int skiphist);

public:  // read or save a file (convenience functions).
         // these functions return a message.
         // these functions return error = TRUE or FALSE.
         // these functions are intended primarily for batch programs,
         //  or for interactive programs which treat the file as a black
         //  box without opportunity to manipulate the I/O with a GUI.
         // set SKIPHIST to TRUE to skip adding a last history card
         //  (intended for undo files).
         // set SKIPSTAT to TRUE to skip reading the static values.

  int  readFile   (const char *filename, char *msg, int skipstat = FALSE);
  int  saveFile   (const char *filename, char *msg,
                   const char *encoding = NULL, int skiphist = FALSE);


//---------------------- end of functions ---------------------------//
//---------------------- end of functions ---------------------------//
//---------------------- end of functions ---------------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
