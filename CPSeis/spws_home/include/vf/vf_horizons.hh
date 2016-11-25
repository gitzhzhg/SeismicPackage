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

//---------------------- vf_horizons.hh -------------------------//
//---------------------- vf_horizons.hh -------------------------//
//---------------------- vf_horizons.hh -------------------------//

//            header file for the VfHorizons class
//                  not derived from any class
//                       subdirectory vf


#ifndef _VF_HORIZONS_HH_
#define _VF_HORIZONS_HH_


class VfHorizons
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class VfInformer     *_informer;  // provided to this class.
  class GridTransform  *_transform; // owned by this class.
  class HorizonArray   *_array;     // owned by this class.

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:    // constructor and destructor.

           VfHorizons (VfInformer *informer);
  virtual ~VfHorizons ();

public:    // get values.

  class VfInformer       *informer     ()  const  { return _informer; }
  class GridTransform    *transform    ()  const  { return _transform; }

  long numHorizons                     ()  const;
  long getActiveHorizonIndex           ()  const;

public:    // set values.

  void setActiveHorizonIndex    (long ihorizon);
  void selectAllHorizons        (int selected);   // set TRUE or FALSE.
  void deleteActiveHorizon      ();

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

public:   // get transformed coordinates.

  float       getXgrid      (long ihorizon, long ipick)  const;
  float       getYgrid      (long ihorizon, long ipick)  const;

public:   // set values.

  void        setNumPicks   (long ihorizon, long npicks);
  void        setActivePick (long ihorizon, long active);
  void        setSelected   (long ihorizon, int selected);
  void        setColor      (long ihorizon, const char *color);

public:   // access disk files.

  virtual int doValidateInput  (class VfHorizonio *horizonio,
                                const char *filename,
                                      char *filetype, char *info);

  virtual int doPrepareImport  (class VfHorizonio *horizonio,
                                const char *filename,
                                const char *filetype, char *msg);

  virtual int doImport         (class VfHorizonio *horizonio,
                                const char *filename,
                                const char *filetype, char *msg);

private:

  int  importRmodLayers (const char *filename, char* msg);
  long startNewHorizon  (const char *name, const char *color);
  void supplyNewPick    (long index, float xloc, float yloc, float zloc,
                                                      int new_segment);
  void finishNewHorizon (long ihorizon);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
