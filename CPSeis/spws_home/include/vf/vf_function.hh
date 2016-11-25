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
// $Id: vf_function.hh,v 1.2 2004/06/07 13:28:00 wjdone Exp $
// $Name:  $

//-------------------------- vf_function.hh -------------------------//
//-------------------------- vf_function.hh -------------------------//
//-------------------------- vf_function.hh -------------------------//

//               header file for the VfFunction class
//                    not derived from any class
//                          subdirectory vf

           // this class is a single velocity function.

   // All functions which insert/remove/modify picks call updatePicks
   // afterwards (with the exceptions noted below).  Therefore, if you
   // wish to make several calls in succession, it is best to use functions
   // which will accomplish all modifications at once, if possible, or at
   // least to minimize the number of functions called, in order to eliminate
   // unnecessary calls to updatePicks.

   // Every time a single time/velocity pick is modified, inserted, or
   // deleted, the index of the active pick is reset to that location.
   // The index of the active pick can also be reset at any time.
   // In all cases, there is always exactly one active pick
   // (unless there are no picks).


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _VF_FUNCTION_HH_
#define _VF_FUNCTION_HH_

#include "vf_constants.hh"
#include "named_constants.h"
#include <stdio.h>

// Choose whether to use SimpleShortArray or SimpleFloatArray:

#define MY_SIMPLE_ARRAY SimpleShortArray
#define USING_SIMPLE_SHORT_ARRAY
/*
#define MY_SIMPLE_ARRAY SimpleFloatArray
#define USING_SIMPLE_FLOAT_ARRAY
*/

class VfFunction
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  class VfUtilities     *_utilities;
  class VfFunctionArray *_array;

  float _xloc;         // X coordinate of velocity function.
  float _yloc;         // Y coordinate of velocity function.
  int   _type;         // default velocity function type            (enum).
  int   _select;       // selection flag for this velocity function (enum).
  int   _error;        // error     flag for this velocity function (enum).
  int   _raytrace;     // raytrace  flag for this velocity function (enum).
  int   _several;      // TRUE while several operations are being performed.

  enum {    VFID_LENGTH =  8,
         PROJECT_LENGTH = 10,
            LINE_LENGTH = 10,
           RDATE_LENGTH =  5,
           PDATE_LENGTH =  5,
          USERID_LENGTH =  3,
         COMMENT_LENGTH = 15 };

  char _vfid    [   VFID_LENGTH + 1];
  char _project [PROJECT_LENGTH + 1];
  char _line    [   LINE_LENGTH + 1];
  char _rdate   [  RDATE_LENGTH + 1];
  char _pdate   [  PDATE_LENGTH + 1];
  char _userid  [ USERID_LENGTH + 1];
  char _comment [COMMENT_LENGTH + 1];

  class ActiveIndexKeeper *_active;
  class MY_SIMPLE_ARRAY  *_depth;   // depth.
  class MY_SIMPLE_ARRAY  *_time;    // 2-way time.
  class MY_SIMPLE_ARRAY  *_vnmo;    // NMO velocity.
  class MY_SIMPLE_ARRAY  *_vrms;    // RMS velocity.
  class MY_SIMPLE_ARRAY  *_vav;     // average velocity.
  class MY_SIMPLE_ARRAY  *_vint;    // interval velocity.
  class MY_SIMPLE_ARRAY  *_thick;   // layer thickness.
  class MY_SIMPLE_ARRAY  *_offset;  // raytraced offsets.
  class SelectionsKeeper  *_selkeep; // array of pick select flags.

  long   _NPICKS;      // this is -1 if not in remembering mode.
  float *_ORDINATES;   // remembered ordinates while in remembering mode.


//---------------------- general functions -----------------------------//
//---------------------- general functions -----------------------------//
//---------------------- general functions -----------------------------//

public:   // constructor and destructor.

           VfFunction (VfUtilities *utilities);
  virtual ~VfFunction ();

public:  // whether or not to notify a VfFunctionArray before and after
         //   any change in this object by calling before/afterVelfunChange.
         // set argument to array pointer to turn on notifications
         //   (to be done when this object is inserted into the array)
         //   (not necessarily when this object is created).
         // set argument to NULL to turn off notifications
         //   (to be done when this object is removed from the array)
         //   (not necessarily when this object is deleted).

  void  pleaseNotifyArray (VfFunctionArray *array)  { _array = array; }

public:  // call these before and after several calls which change any of
         //   the time/depth/velocity picks in the data.
         // these postpone updates which otherwise are performed each time
         //   a change is made to the picks.
         // the type argument should be the type to be used for updating
         //   the velocity function.  this will normally be the same type
         //   used in calls to intervening functions (between before...
         //   and after...).  if no intervening functions take the type
         //   argument, the default type (or -1 or omitted) should be used.
         // these are optional calls, but will improve efficiency significantly
         //   when a number of changes are being made consecutively.

  void beforeSeveralOperations ();
  void afterSeveralOperations  (int type = -1);

private:

  void              beforeVelfunChange           ();
  void              afterVelfunChange            ();
  MY_SIMPLE_ARRAY  *abscissaArray                (int type)  const;
  MY_SIMPLE_ARRAY  *ordinateArray                (int type)  const;
  void              privateClear                 ();
  void              privateDeletePicks           ();
  void              privateResetNumPicksAndClear (long npicks);

public:   // reallocate and reset picks.

  void clearVelocityFunction          ();    // also clears picks and buffer.
  void deleteAllVelocityFunctionPicks ();    // also clears buffer.

  void resetNumPicks (long npicks, int type = -1);

  void resetNumPicks (long npicks, float *abscissae,
                                   float *ordinates, int type = -1);

  void convertFeetToMeters ();
  void convertMetersToFeet ();
  void removeWaterVelocity (float vwater);

  void copyVelocityFunctionContents  (const VfFunction *velfun);

public:    // save to binary file or read from binary file.
           // these return error TRUE or FALSE.

  int  saveToBinaryFile   (FILE *stream, int type = -1)  const;
  int  readFromBinaryFile (FILE *stream);


//------------------------------- get values ---------------------------//
//------------------------------- get values ---------------------------//
//------------------------------- get values ---------------------------//

public:   // get strings corresponding to velocity function type.

  const char     *getTypeSymbol       (int type = -1)  const;
  const char     *getTypeDescription  (int type = -1)  const;

public:   // get general values.
          // typeError returns TRUE if there is an error in the abscissae
          //   or ordinates of the specified type.

  float        getXloc         ()  const  { return _xloc; }
  float        getYloc         ()  const  { return _yloc; }
  long         numPicks        ()  const;
  int          getDefaultType  ()  const  { return _type    ; }
  int          getSelectFlag   ()  const  { return _select  ; }
  int          getErrorFlag    ()  const  { return _error   ; }
  int          getRaytraceFlag ()  const  { return _raytrace; }
  long         getActivePick   ()  const;
  int          vfidIsBlank     ()  const;

  int          typeError       (int type = -1)  const;

  const char  *getVfid       ()  const  { return _vfid   ; }
  const char  *getProject    ()  const  { return _project; }
  const char  *getLine       ()  const  { return _line   ; }
  const char  *getRdate      ()  const  { return _rdate  ; }
  const char  *getPdate      ()  const  { return _pdate  ; }
  const char  *getUserid     ()  const  { return _userid ; }
  const char  *getComment    ()  const  { return _comment; }
  float        getElevation  ()  const;  // from comment field.
  float        getWaterDepth ()  const;  // from comment field.
                // elevation or water depth is nil if not present or decodable.

public:   // get individual pick values.

  float        getDepth     (long ipick)  const;
  float        getTime      (long ipick)  const;
  float        getVrms      (long ipick)  const;
  float        getVav       (long ipick)  const;
  float        getVint      (long ipick)  const;
  float        getVnmo      (long ipick)  const;
  float        getThickness (long ipick)  const;
  float        getOffset    (long ipick)  const;

  float        getAbscissa  (long ipick, int type = -1)  const;
  float        getOrdinate  (long ipick, int type = -1)  const;

public:   // get arrays of pick values.

  void         getDepthArray     (float *values)  const;
  void         getTimeArray      (float *values)  const;
  void         getVrmsArray      (float *values)  const;
  void         getVavArray       (float *values)  const;
  void         getVintArray      (float *values)  const;
  void         getVnmoArray      (float *values)  const;
  void         getThicknessArray (float *values)  const;
  void         getOffsetArray    (float *values)  const;

  void         getAbscissaArray  (float *values, int type = -1)  const;
  void         getOrdinateArray  (float *values, int type = -1)  const;

/*
public:   // get pointers to arrays of pick values.

  const float *getDepthArrayPointer     ()  const;
  const float *getTimeArrayPointer      ()  const;
  const float *getVrmsArrayPointer      ()  const;
  const float *getVavArrayPointer       ()  const;
  const float *getVintArrayPointer      ()  const;
  const float *getVnmoArrayPointer      ()  const;
  const float *getThicknessArrayPointer ()  const;
  const float *getOffsetArrayPointer    ()  const;

  const float *getAbscissaArrayPointer  (int type = -1)  const;
  const float *getOrdinateArrayPointer  (int type = -1)  const;
*/

  float *createAbscissaArrayPointer  (int type = -1)  const;
  float *createOrdinateArrayPointer  (int type = -1)  const;

  void   deleteAbscissaArrayPointer  (float *values, int type = -1)  const;
  void   deleteOrdinateArrayPointer  (float *values, int type = -1)  const;

public:  // get minimum and maximum values.
         // these return FNIL if there are no picks, or all picks are nil.
         // otherwise, nil picks are not used when getting minima and maxima.

  float  minimumDepth     ()  const;
  float  maximumDepth     ()  const;
  float  minimumTime      ()  const;
  float  maximumTime      ()  const;
  float  minimumVrms      ()  const;
  float  maximumVrms      ()  const;
  float  minimumVav       ()  const;
  float  maximumVav       ()  const;
  float  minimumVint      ()  const;
  float  maximumVint      ()  const;
  float  minimumVnmo      ()  const;
  float  maximumVnmo      ()  const;
  float  minimumThickness ()  const;
  float  maximumThickness ()  const;
  float  minimumOffset    ()  const;
  float  maximumOffset    ()  const;

  float  minimumAbscissa  (int type = -1)  const;
  float  maximumAbscissa  (int type = -1)  const;
  float  minimumOrdinate  (int type = -1)  const;
  float  maximumOrdinate  (int type = -1)  const;

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
                 (float value, int type, long *ia, long *ib)  const;

public:  // get interpolated velocity (ordinate) at specified time or
         //   depth (abscissa).
         // type should not have abscissa = layer thickness.
         // if vint_grade is FALSE, does special interval velocity
         //   interpolation.
         // if type is VTDP, getInterpolatedVelocity actually returns depth.
         // getInterpolatedDepth is a special case of getInterpolatedVelocity
         //   for type = VTDP.

  float getInterpolatedVelocity
           (float abscissa, int type = -1, int vint_grade = TRUE) const;

  float getInterpolatedTime  (float depth)  const;
  float getInterpolatedDepth (float time )  const;

public: // returns ordinates interpolated to the specified abscissae.
        // abscissae[npicks] are input and ordinates[npicks] are returned.
        // if abscissae[] and ordinates[] are in fact the same array,
        //   the abscissae will be replaced by the ordinates.
        // fills ordinates[] with zeroes if this type has a type error.
        // type must not have abscissa = layer thickness.
        // if vint_grade is FALSE, does special interval velocity
        //   interpolation.

  void getInterpolatedVelocityFunction
                   (long npicks, const float *abscissae, float *ordinates,
                    int type = -1, int vint_grade = TRUE);

public:  // puts evenly-sampled velocity picks into ordinates[npicks].
         // tmin (minimum time or depth) is mapped to ordinates[0].
         // tmax (maximum time or depth) is mapped to ordinates[npicks-1].
         // fills ordinates[] with zeroes if this type has a type error.
         // type must not have abscissa = layer thickness.
         // if vint_grade is FALSE, does special interval velocity
         //   interpolation.

  void velToFloat (float tmin, float tmax,
                   long npicks, float *ordinates,
                   int type = -1, int vint_grade = TRUE);

public:  // remember and forget interpolated ordinates.
         // if rememberInterpolatedOrdinates is called, then the ordinates
         //   returned by the next call to getInterpolatedVelocityFunction
         //   and velToFloat are remembered, so that repeated calls to
         //   these functions will simply return the remembered values.
         // during this remembering mode, it is assumed that all subsequent
         //   calls to getInterpolatedVelocityFunction or velToFloat are
         //   requesting the same ordinates, and no checks are made to
         //   verify this (except to assert if npicks changes).  the
         //   velocity function should not be changed in any way during
         //   this remembering mode.
         // if deleteInterpolatedOrdinates is called, the remembered
         //   ordinates (if any) will be deleted, but the remembering
         //   mode will remain in effect.
         // if forgetInterpolatedOrdinates is called, the remembered
         //   ordinates (if any) will be deleted, and the remembering
         //   mode will be cancelled.
         // because _NPICKS and _ORDINATES are set during the remembering
         //   mode, getInterpolatedVelocityFunction and velToFloat cannot
         //   be const functions.

  void rememberInterpolatedOrdinates (long npicks);
  void deleteInterpolatedOrdinates   ();
  void forgetInterpolatedOrdinates   ();


//------------------------------- set values ---------------------------//
//------------------------------- set values ---------------------------//
//------------------------------- set values ---------------------------//

public:   // set general values.

  void  setXloc        (float value);
  void  setYloc        (float value);
  void  setDefaultType (int   value);
  void  setSelectFlag  (int   value); // to be called only by VfFunctionSelect.
  void  setActivePick  (long  value);

  void  setVfid        (const char *value);
  void  setProject     (const char *value);
  void  setLine        (const char *value);
  void  setRdate       (const char *value);
  void  setPdate       (const char *value);
  void  setUserid      (const char *value);
  void  setComment     (const char *value);
  void  setElevation   (float   elevation);     // sets comment field.
  void  setWaterDepth  (float water_depth);     // sets comment field.

public:   // set individual pick values.
          // these functions reset the active pick.

  void  setDepth     (long ipick, float value, int type = -1);
  void  setTime      (long ipick, float value, int type = -1);
  void  setVrms      (long ipick, float value, int type = -1);
  void  setVav       (long ipick, float value, int type = -1);
  void  setVint      (long ipick, float value, int type = -1);
  void  setVnmo      (long ipick, float value, int type = -1);
  void  setThickness (long ipick, float value, int type = -1);

  void  setAbscissa  (long ipick, float abscissa, int type = -1);
  void  setOrdinate  (long ipick, float ordinate, int type = -1);
  void  replacePick  (long ipick, float abscissa,
                                  float ordinate, int type = -1);

public:   // set arrays of pick values.

  void  setDepthArray     (const float *values, int type = -1);
  void  setTimeArray      (const float *values, int type = -1);
  void  setVrmsArray      (const float *values, int type = -1);
  void  setVavArray       (const float *values, int type = -1);
  void  setVintArray      (const float *values, int type = -1);
  void  setVnmoArray      (const float *values, int type = -1);
  void  setOffsetArray    (const float *values, int type = -1);
  void  setThicknessArray (const float *values, int type = -1);

  void  setAbscissaArray  (const float *abscissae, int type = -1);
  void  setOrdinateArray  (const float *ordinates, int type = -1);
  void  replaceAllPicks   (const float *abscissae,
                           const float *ordinates, int type = -1);


//--------------------------- take action ------------------------------//
//--------------------------- take action ------------------------------//
//--------------------------- take action ------------------------------//

public:   // insert or remove one time/velocity pick.
          // these functions reallocate the arrays.
          // these functions reset the active pick.

  void  appendNilPick                                          (int type = -1);
  void  insertNilPick                              (long ipick, int type = -1);
  void  insertPickFromBuffer                       (long ipick, int type = -1);
  void  removePick                                 (long ipick, int type = -1);
  void  removePickToBuffer                         (long ipick, int type = -1);

  void  appendPick             (float abscissa, float ordinate, int type = -1);
  void  insertPick (long ipick, float abscissa, float ordinate, int type = -1);

public:   // invoke or cancel ray tracing.
          // if _raytrace is RAYTRACE_NO     , cancelRayTracing does nothing.
          // if _raytrace is RAYTRACE_FORWARD, invokeRayTracing does nothing.
          // if _raytrace is RAYTRACE_INVERSE, invokeRayTracing does nothing.
          // these functions reset _raytrace.

  void  invokeRayTracing (int type = -1);
  void  cancelRayTracing (int type = -1);

private:  // update picks for all velocity types.
          // uses parameters in VfUtilities and VfUpdate
          //   and VhelpOffsets.
          // this function is called after any changes to picks.
          // this function is called when changing ray tracing mode.
          // this function needs to be called by the user ONLY after calling
          //   replaceOneOfSeveralPicks as many times as needed.
          // if _raytrace is RAYTRACE_NO, ray tracing is not performed
          //         (VNMO and VRMS will be the same).
          // otherwise, does forward ray tracing if type != VNMO,
          //         and sets _raytrace to RAYTRACE_FORWARD.
          // otherwise, does inverse ray tracing if type == VNMO,
          //         and sets _raytrace to RAYTRACE_INVERSE.
          // resets error flag in velocity function.
          // error message can be obtained by calling updatePicksErrorMessage.

  void  privateUpdatePicks (int type);


//------------------- pass thru to SelectionsKeeper ------------------------//
//------------------- pass thru to SelectionsKeeper ------------------------//
//------------------- pass thru to SelectionsKeeper ------------------------//

public:   // get select flags.

  long        numSelectedPicks    ()            const;
  int         pickIsSelected      (long ipick)  const;   // TF
  int         getPickSelectFlag   (long ipick)  const;   // ENUM
  const char *getPickSelectString (long ipick)  const;

public:   // set select flags.

  void beforeSettingSeveralPickSelectFlags ();
  void setOneOfSeveralPickSelectFlags      (long index, int select);
  void afterSettingSeveralPickSelectFlags  ();

  void clearPickSelectFlags      ();
  void incrementPickSelectFlag   (long ipick);
  void setPickSelectFlag         (long ipick, int select);  // ENUM
  void toggleAllPickSelections   ();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
