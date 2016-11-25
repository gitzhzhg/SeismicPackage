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

//------------------------ horizon_array.hh -----------------------//
//------------------------ horizon_array.hh -----------------------//
//------------------------ horizon_array.hh -----------------------//

//                header file for the HorizonArray class
//                     not derived from any class
//                         subdirectory oprim

                // used for both horizons and faults.

 // This class maintains an array of seismic horizons (or faults).
 // This class owns the array and the horizons in the array.
 // The individual horizons are accessible only via pass-thru functions.


//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//
//------------------- start of coding ---------------------------//


#ifndef _HORIZON_ARRAY_HH_
#define _HORIZON_ARRAY_HH_

#include <stdio.h>


class HorizonArray
{

//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//
//--------------------------- data ------------------------------//

private:

  class VoidArray        *_array;         // owned by this class.

//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//
//-------------------------- functions ---------------------------------//

public:

           HorizonArray ();
  virtual ~HorizonArray ();

private:

  class Horizon *horizon (long ihorizon)  const;

public:    // get values.

  long             numHorizons              ()                  const;
  long             getActiveHorizonIndex    ()                  const;
  long             findMatchingHorizonIndex (const char *name)  const;

public:    // set values.
           // createHorizon returns index of created horizon.

  void  setActiveHorizonIndex (long ihorizon);
  void  selectAllHorizons     (int selected);   // set TRUE or FALSE.
  void  deleteAllHorizons     ();
  long  createHorizon         ();
  void  deleteActiveHorizon   ();
  void  deleteHorizon         (long index);

public:   // get segment-specific values.
          // numSegments is zero if numPicks is zero.
          // 0 <= sipick < numPicksInSegment.
          // 0 <=  ipick < numPicks.
          // getSegmentNumber returns iseg+1 for the specified pick.

  long   numSegments            (long ihorizon)               const;
  long   startingIndexOfSegment (long ihorizon, long iseg)    const;
  long   numPicksInSegment      (long ihorizon, long iseg)    const;
  long   getSegmentNumber       (long ihorizon, long ipick)   const;

  float  getXlocInSegment       (long ihorizon, long iseg, long sipick)  const;
  float  getYlocInSegment       (long ihorizon, long iseg, long sipick)  const;
  float  getTimeInSegment       (long ihorizon, long iseg, long sipick)  const;
  float  getShotpointInSegment  (long ihorizon, long iseg, long sipick)  const;
  float  getLineNumberInSegment (long ihorizon, long iseg, long sipick)  const;


//--------------------- pass thru to individual horizons ---------------//
//--------------------- pass thru to individual horizons ---------------//
//--------------------- pass thru to individual horizons ---------------//

public:   // get values.

  long        numPicks      (long ihorizon)       const;
  long        getActivePick (long ihorizon)       const;
  int         isSelected    (long ihorizon)       const;
  const char *getName       (long ihorizon)       const;
  const char *getColor      (long ihorizon)       const;
  const char *getPicktype   (long ihorizon)       const;
  const char *getUnits      (long ihorizon)       const;
  int         readError     (long ihorizon)       const;

  float       getXloc       (long ihorizon, long ipick)  const;
  float       getYloc       (long ihorizon, long ipick)  const;
  float       getTime       (long ihorizon, long ipick)  const;
  float       getShotpoint  (long ihorizon, long ipick)  const;
  float       getLineNumber (long ihorizon, long ipick)  const;

  float       minimumXloc       (long ihorizon)  const;
  float       minimumYloc       (long ihorizon)  const;
  float       minimumTime       (long ihorizon)  const;
  float       minimumShotpoint  (long ihorizon)  const;
  float       minimumLineNumber (long ihorizon)  const;

  float       maximumXloc       (long ihorizon)  const;
  float       maximumYloc       (long ihorizon)  const;
  float       maximumTime       (long ihorizon)  const;
  float       maximumShotpoint  (long ihorizon)  const;
  float       maximumLineNumber (long ihorizon)  const;

public:   // set values.

  void        setNumPicks   (long ihorizon, long npicks);
  void        setActivePick (long ihorizon, long active);
  void        setSelected   (long ihorizon, int selected);
  void        setName       (long ihorizon, const char *name);
  void        setColor      (long ihorizon, const char *color);
  void        appendNilRow  (long ihorizon);
  void        setLastValue  (long ihorizon, long icol, float value);
  void        addSegment    (long ihorizon, long ipick);


//------------------------ read and write disk files --------------------//
//------------------------ read and write disk files --------------------//
//------------------------ read and write disk files --------------------//

public:   // read and write disk file.
          // import/export... access a text file.
          // read/save... access a binary file.
          // import... reads file using parameters in floatio.
          // export... writes file using parameters in floatio.
          // these set msg.
          // these return error TRUE or FALSE.

  int  importHorizonFile    (long ihorizon, class FloatioWrapper *floatio,
                             const char *filename, char *msg);
  int  exportHorizonFile    (long ihorizon, class FloatioWrapper *floatio,
                             const char *filename, char *msg);

  int  readHorizonFile      (long ihorizon, const char *filename, char *msg);
  int  saveHorizonFile      (long ihorizon, const char *filename, char *msg);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
