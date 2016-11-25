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

//------------------------ vf_function_array.hh --------------------------//
//------------------------ vf_function_array.hh --------------------------//
//------------------------ vf_function_array.hh --------------------------//

//            header file for the VfFunctionArray class
//                derived from the SmartArray class
//                         subdirectory vf

   // This class maintains an array of velocity functions which it owns.
   // Velocity functions in the array (or in the buffer) cannot be NULL.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _VF_FUNCTION_ARRAY_HH_
#define _VF_FUNCTION_ARRAY_HH_


#include "oprim/smart_array.hh"


class VfFunctionArray : public SmartArray
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  class VfUtilities *_utilities;
  long   _nerr;        // number of velocity functions with errors.
  long   _nray;        // number of raytraced velocity functions.
  long   _nblank;      // number of velocity functions with blank names.
  int    _might;       // true if active velfun neighbors might have changed.

  class LimitsKeeper *  _xloc_limits;
  class LimitsKeeper *  _yloc_limits;
  class LimitsKeeper * _depth_limits;
  class LimitsKeeper *  _time_limits;
  class LimitsKeeper *  _vrms_limits;
  class LimitsKeeper *   _vav_limits;
  class LimitsKeeper *  _vint_limits;
  class LimitsKeeper *  _vnmo_limits;
  class LimitsKeeper * _thick_limits;
  class LimitsKeeper *_offset_limits;

//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:  // constructor and destructor.

           VfFunctionArray (VfUtilities *utilities);
  virtual ~VfFunctionArray ();

  class VfFunction *velfun(long ifun)  const
                         { return (VfFunction*)arrayElement(ifun); }

  class VfFunction *activeVelfun()  const
                         { return (VfFunction*)activeElement(); }

  class VfFunction *referenceVelfun()  const
                         { return (VfFunction*)referenceElement(); }

  long  numVelocityFunctionsWithErrors     ()  const  { return _nerr; }
  long  numRaytracedVelocityFunctions      ()  const  { return _nray; }
  long  numVelocityFunctionsWithBlankNames ()  const  { return _nblank; }

  float minimumXloc     ();
  float maximumXloc     ();
  float minimumYloc     ();
  float maximumYloc     ();

  float minimumAbscissa  (int type);
  float maximumAbscissa  (int type);
  float minimumOrdinate  (int type);
  float maximumOrdinate  (int type);

  float minimumDepth     ();  float maximumDepth     ();
  float minimumTime      ();  float maximumTime      ();
  float minimumVrms      ();  float maximumVrms      ();
  float minimumVav       ();  float maximumVav       ();
  float minimumVint      ();  float maximumVint      ();
  float minimumVnmo      ();  float maximumVnmo      ();
  float minimumThickness ();  float maximumThickness ();
  float minimumOffset    ();  float maximumOffset    ();

private:

  class LimitsKeeper *abscissaLimits (int type)  const;
  class LimitsKeeper *ordinateLimits (int type)  const;

public:  // neighborsMightHaveChanged() is to be called after anything
         //   changes which might cause the nearest neighbors to the
         //   active velocity function to change:
         //   (by VfFunction when xloc and yloc coordinates change)
         //   (by this class when velocity functions are sorted)
         //   (by VfKernal when the active velocity function changes)
         //   (by VfKernal when a velocity function is inserted or removed)
         //   (by VfKernal [indirectly from VfDataset and VfManager]
         //      when the bin tolerances in VfUtilities change)
         // neighborsAreUpdated() should be called (by VfKernal) after
         //   the nearest neighbors have been updated.

  void  neighborsMightHaveChanged   ()         { _might = 1; }
  void  neighborsAreUpdated         ()         { _might = 0; }
  int   neighborsNeedUpdating       ()  const  { return _might; }

public:   // to be called only by VfFunction objects in this array,
          //   before and after changes to these variables:
          //     - error and raytrace flags.
          //     - velocity function name.
          //     - xloc and yloc coordinates.

  void  beforeVelfunChange (VfFunction *object);
  void   afterVelfunChange (VfFunction *object);

private:  // overriding virtual functions.

  virtual void  switchElements        (long index1, long index2);
  virtual void *doCreateObject        ();
  virtual void  doDeleteObject        (void *object);
  virtual void  objectHasBeenInserted (long ifun);
  virtual void  objectWillBeRemoved   (long ifun);
  virtual void  doCopyToBuffer        (void *object, void *buffer);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
