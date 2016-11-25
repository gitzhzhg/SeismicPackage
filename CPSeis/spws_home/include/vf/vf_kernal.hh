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

//--------------------------- vf_kernal.hh --------------------------//
//--------------------------- vf_kernal.hh --------------------------//
//--------------------------- vf_kernal.hh --------------------------//

//                header file for the VfKernal class
//                    not derived from any class
//                         subdirectory vf

   // This class maintains the kernal (guts) of a velocity dataset.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _VF_KERNAL_HH_
#define _VF_KERNAL_HH_

#include "named_constants.h"

class VfKernal
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

public:

  enum { NEAREST_ABSCISSAE      =  0,
         RESTRICTED_ABSCISSAE   = -1,
         UNRESTRICTED_ABSCISSAE = -2 };

  enum { BYTE_ARRAY, FLOAT_ARRAY };

private:

  class VfInformer       *_informer;   // pointer provided to this class.
  class VfUtilities      *_utilities;  // pointer provided to this class.
  class VfFunctionArray  *_array;      // owned by this class.
  class VfFunctionSelect *_select;     // owned by this class.
 
  int   _nhx;      // header word for X location.
  int   _nhy;      // header word for Y location.
  int   _order;    // order    of moveout (2 or 4).
  float _nhosign;  // sign     of moveout (normally 1.0 or -1.0).
  float _nhoexp;   // exponent of moveout (normally 2.0 or  4.0).
  int   _dunits;    // FEET_PER_SECOND, METERS_PER_SECOND, UNSPECIFIED_UNITS.

  char *_name;                 // a name associated with this dataset.
  char *_attname;
  char *_attunits;
  char *_tdunits;

  long  _xprev;    // index of previous (to active) function in X direction.
  long  _xnext;    // index of   next   (to active) function in X direction.
  long  _yprev;    // index of previous (to active) function in Y direction.
  long  _ynext;    // index of   next   (to active) function in Y direction.

/****************
  int    _MULTIPLE;      // used with start/stopMultipleInterpolations.
****************/
  long   _NPICKS;        // used with start/stopMultipleInterpolations.
  long   _IFUN1;         // used with start/stopMultipleInterpolations.
  long   _IFUN2;         // used with start/stopMultipleInterpolations.
  long   _IFUN3;         // used with start/stopMultipleInterpolations.
  long   _IFUN4;         // used with start/stopMultipleInterpolations.
  float *_ORDINATES1;    // used with start/stopMultipleInterpolations.
  float *_ORDINATES2;    // used with start/stopMultipleInterpolations.
  float *_ORDINATES3;    // used with start/stopMultipleInterpolations.
  float *_ORDINATES4;    // used with start/stopMultipleInterpolations.


//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:   // constructor and destructor.

           VfKernal (VfInformer *informer, VfUtilities *utilities);
  virtual ~VfKernal ();

  void  newBinTolerancesByVfDatasetOnly();

public:   // replace or append velocity functions from storage.
          // also copies the following variables:
          //   _nhx, _nhy, _order, _nhosign, _nhoexp.
          //   _dunits, _name, _attname, _attunits, _tdunits.
          // also resets  active   velocity function if previously empty.
          // also resets reference velocity function if previously empty.
          // does NOT copy the _informer and _utilities pointers.
          // if working is not NULL, shows working message during progress.
          // working is a message like "validating velocity function".

  void  replaceVelocityFunctions (const char *working,
                                  const VfKernal *storage);
  void  appendVelocityFunctions  (const char *working,
                                  const VfKernal *storage);

public:   // get values.

  VfInformer      *informer          ()  const  { return _informer; }
  VfUtilities     *utilities         ()  const  { return _utilities; }
  VfFunctionArray *array             ()  const  { return _array; }
  int              getNhx            ()  const  { return _nhx; }
  int              getNhy            ()  const  { return _nhy; }
  int              getMoveoutOrder   ()  const  { return _order; }
  float            getNhosign        ()  const  { return _nhosign; }
  float            getNhoexp         ()  const  { return _nhoexp; }
  int              getDistanceUnits  ()  const  { return _dunits; }
  const char      *getSymbolFromUnits()  const;
  const char      *getName           ()  const  { return _name; }
  const char      *getAttributeName  ()  const  { return _attname; }
  const char      *getAttributeUnits ()  const  { return _attunits; }
  const char      *getTimeDepthUnits ()  const  { return _tdunits; }

public:   // set values.

  void         setNhx            (int nhx);
  void         setNhy            (int nhy);
  void         setMoveoutOrder   (int order);
  void         setMoveoutOrder   (float nhosign, float nhoexp);
  void         setDistanceUnits  (int dunits);
  void         setUnitsFromSymbol(const char *symbol);
  void         setName           (const char *name);
  void         setAttributeName  (const char *attname);
  void         setAttributeUnits (const char *attunits);
  void         setTimeDepthUnits (const char *tdunits);

public: // get minimum and maximum values of all velocity functions.
        // these return 0.0 if there are no velocity functions.
        // the second set of functions adjust the returned value to bin center.

  float minimumXloc       ();
  float maximumXloc       ();
  float minimumYloc       ();
  float maximumYloc       ();

  float minimumXbinCenter ();   // uses xcenter and xwidth from VfUtilities.
  float maximumXbinCenter ();   // uses xcenter and xwidth from VfUtilities.
  float minimumYbinCenter ();   // uses ycenter and ywidth from VfUtilities.
  float maximumYbinCenter ();   // uses ycenter and ywidth from VfUtilities.

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
        // xloc and yloc designate the desired coordinates.
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

  int checkSort       (int *xdir, int *ydir, int *xfast)  const;
                                                        // returns sorted.
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
        // if searching from the active function, these simply return _xprev,
        //   _xnext, _yprev, or _ynext (updating them first if necessary).

  long findNextXloc (long ifun = -2, float xloc = 0.0, float yloc = 0.0);
  long findPrevXloc (long ifun = -2, float xloc = 0.0, float yloc = 0.0);
  long findNextYloc (long ifun = -2, float xloc = 0.0, float yloc = 0.0);
  long findPrevYloc (long ifun = -2, float xloc = 0.0, float yloc = 0.0);

private:

  long findHelper (long ifun, float xloc, float yloc);

public: // find where to insert a velocity function to maintain sort.
        // returns index where new velocity function should be inserted.
    // xflag TRUE  means to create ascending  x-order if either is possible.
    // xflag FALSE means to create descending x-order if either is possible.
    // recommend setting xflag to semblance file order for new velocity file.

  long findWhereToInsert (float xloc, float yloc, int xflag = TRUE)  const;

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
                    long nsamp, float *array, int vint_grade)  const;

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
        // if the npicks argument is NEAREST_ABSCISSAE:
        //  - the abscissae and ordinates are returned.
        //  - the abscissae include the number of picks, and the time or depth
        //      values, from the nearest velocity function which has no errors.
        //  - the returned npicks will of course never exceed MAXPICKS.
        //
        // if the npicks argument is RESTRICTED_ABSCISSAE:
        //  - the abscissae and ordinates are returned.
        //  - the abscissae include the number of picks, and the time or depth
        //      values, from all of the picks in all of the velocity functions
        //      which have no errors and are used in the interpolation.
        //      (duplicate time or depth values are eliminated.)
        //      (if there are >MAXPICKS abscissae, they are culled.)
        //      (points are culled from farthest functions and latest times.)
        //  - the returned npicks will never exceed MAXPICKS.
        //
        // if the npicks argument is UNRESTRICTED_ABSCISSAE:
        //  - the abscissae and ordinates are returned.
        //  - the abscissae include the number of picks, and the time or depth
        //      values, from all of the picks in all of the velocity functions
        //      which have no errors and are used in the interpolation.
        //      (duplicate time or depth values are eliminated.)
        //  - the returned npicks might exceed MAXPICKS.
        //
        // if the npicks argument is greater than zero:
        //  - the abscissae[npicks] are input and the ordinates are returned.
        //  - npicks can have any value (e.g. npicks > MAXPICKS is OK).
        //  - if abscissae[] and ordinates[] are the same address, the
        //      abscissae will be replaced by the ordinates.
        //  - the returned npicks will equal the input npicks.
        //  - the returned npicks might exceed MAXPICKS.
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
                    float xnorm = 1.0, float ynorm = 1.0);

/***************************
//////////// idea:

       // abscissae is dimensioned [npicks].
       // ordinates is dimensioned [npicks * ntraces] (returned).
       // xloc      is dimensioned [ntraces].
       // yloc      is dimensioned [ntraces].

  long getInterpolatedVelocityFunctions
                   (long ntraces, long npicks,
                    const float *abscissae, float *ordinates,
                    float *xloc, float *yloc,
                    int type, int vint_grade = TRUE,
                    float xnorm = 1.0, float ynorm = 1.0);
***************************/

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
                   int type, int vint_grade = TRUE);

/***************************
//////////// idea:

       // ordinates is dimensioned [npicks * ntraces] (returned).
       // xloc      is dimensioned [ntraces].
       // yloc      is dimensioned [ntraces].

  void velsToFloats (float *xloc, float *yloc, float tmin, float tmax,
                     long npicks, long ntraces, float *ordinates,
                     int type, int vint_grade = TRUE);
***************************/

public: // convert a seismic trace to or from time or depth.
        // tmin and dmin (minimum time and depth) are mapped to array[0].
        // tmax and dmax (maximum time and depth) are mapped to array[nsamp-1].
        // array_type is one of the above enums (BYTE_ARRAY or FLOAT_ARRAY).

  void timeToDepth (float xloc, float yloc, float tmin, float tmax,
                    long nsamp, void *array, int array_type,
                    float dmin, float dmax);

  void depthToTime (float xloc, float yloc, float tmin, float tmax,
                    long nsamp, void *array, int array_type,
                    float dmin, float dmax);

/***************************
//////////// idea:

       // array is dimensioned [nsamp * ntraces] (converted).
       // xloc  is dimensioned [ntraces].
       // yloc  is dimensioned [ntraces].

  void timesToDepths (float *xloc, float *yloc, float tmin, float tmax,
                      long nsamp, void *array, int array_type,
                      float dmin, float dmax);

  void depthsToTimes (float *xloc, float *yloc, float tmin, float tmax,
                      long nsamp, void *array, int array_type,
                      float dmin, float dmax);
***************************/

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


//-------------------- pass thru to VfFunctionArray --------------------//
//-------------------- pass thru to VfFunctionArray --------------------//
//-------------------- pass thru to VfFunctionArray --------------------//

public: // clearing and resetting functions.
        // resetNumVelocityFunctions reallocates the array and creates
        //   empty velocity functions.
        // appendNumVelocityFunctions creates empty velocity functions
        //   and appends them to the existing functions.
        // deleteAllVelocityFunctions deletes each velocity function,
        //   frees the array, and sets the array pointer to NULL.
        // deleteSelectedVelocityFunctions deletes each selected function.
        // deleteEmptyVelocityFunctions deletes each function with no picks.
        // clearEverything deletes all velocity functions and resets several
        //   other values to defaults.

  void clearEverything                 ();
  void resetNumVelocityFunctions       (long nfun);
  void appendNumVelocityFunctions      (long nappend);
  void deleteAllVelocityFunctions      ();
  void deleteSelectedVelocityFunctions ();
  long deleteEmptyVelocityFunctions    ();

public:   // get values.
          // getActiveVelocityFunction    returns  -1  if array is empty.
          // getReferenceVelocityFunction returns  -1  if array is empty.
          // activeVelfun                 returns NULL if array is empty.
          // referenceVelfun              returns NULL if array is empty.

  long               numVelocityFunctions                 ()  const;
  long               numVelocityFunctionsWithErrors       ()  const;
  long               numRaytracedVelocityFunctions        ()  const;
  long               numVelocityFunctionsWithBlankNames   ()  const;

  long               numVelocityFunctionsWithTypeErrors (int type)  const;

  long               getActiveVelocityFunction            ()  const;
  long               getReferenceVelocityFunction         ()  const;

  class VfFunction  *velfun                      (long ifun)  const;
  class VfFunction  *activeVelfun                         ()  const;
  class VfFunction  *referenceVelfun                      ()  const;

public:   // set values.

  void         setActiveVelocityFunction     (long ifun);
  void         setReferenceVelocityFunction  (long ifun);

public:   // insert or remove one velocity function.
          // appendVelocityFunction and insertVelocityFunction insert
          //   an empty function containing no picks.

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
  int  velocityFunctionIsSelected        (long ifun)  const;
  int  getSelectFlag                     (long ifun)  const;
  int  severalSelectionsInProgress       ()           const;  // TRUE/FALSE

public:   // set select flags.

  void clearSelectFlags                  ();
  void incrementSelectFlag               (long ifun);
  void setSelectFlag                     (long ifun, int select);

public:   // before and after setting select flags.

    // these should be called before and after SEVERAL (more than one)
    //   consecutive calls to incrementSelectFlag or setSelectFlag.
    // these MUST be called before and after any of these activities:
    //   (1) sorting velocity functions.
    //   (2) setting the select flag directly in velocity functions
    //         instead of calling clearSelectFlags or incrementSelectFlag
    //         or setSelectFlag in this class.
    // these should not be called at any other time.

  void beforeSettingSeveralSelectFlags   ();
  void afterSettingSeveralSelectFlags    ();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
