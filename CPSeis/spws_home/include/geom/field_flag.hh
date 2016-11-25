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

//---------------------- field_flag.hh ---------------------//
//---------------------- field_flag.hh ---------------------//
//---------------------- field_flag.hh ---------------------//

//              header file for the FieldFlag class
//                  not derived from any class
//                      subdirectory geom

    // This class contains all data associated with a single
    // surveyed flag position used in seismic data acquisition.

    // The incremental distance from the previous flag might be
    // horizontal (based on xloc and yloc) or sloping (based on xloc
    // and yloc and elev).  Other classes deal with setting these
    // values correctly.

    // Recommended units of distance are in feet or meters.
    // Recommended units of time are in seconds or milliseconds.


#ifndef _FIELD_FLAG_HH_
#define _FIELD_FLAG_HH_

#include "geom/fg_constants.hh"
#include <stdio.h>

#define  TURN_ON_FLAG(ident)  ( _depend |=  (1 << ident) )
#define TURN_OFF_FLAG(ident)  ( _depend &= ~(1 << ident) )
#define    FLAG_IS_ON(ident)  ( _depend &   (1 << ident) )


class FieldFlag
{
  friend class FgTraceValues;

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class SeisLine *_line;  // pointer to opaque object containing this flag.

  class IntegerList *_sss; // list of  sources  at this flag.
  class IntegerList *_rrr; // list of receivers at this flag.

  long    _ixf;          // index of this flag in the line.
  int     _depend;       // each bit TRUE if value is dependent.
  float   _shotpoint;    // shotpoint of surveyed flag.
  double  _dist;         // distance of flag from previous flag on line.
  double  _xloc;         // surveyed X coordinate of surveyed flag.
  double  _yloc;         // surveyed Y coordinate of surveyed flag.
  float   _elev;         // surveyed  elevation   of surveyed flag.
  float   _hd;           // default hole depth  of sources at this flag.
  float   _tuh;          // default uphole time of sources at this flag.
  float   _rstat;        // receiver static at this surveyed flag.
  float   _sstat;        // source   static at this surveyed flag.
  float   _xskid;        // receiver  inline   skid at this surveyed flag.
  float   _yskid;        // receiver crossline skid at this surveyed flag.
  float   _eskid;        // receiver elevation skid at this surveyed flag.
  double  _cum; // newloc// distance of flag from start of seismic line.
  float   _select;       // selection flag (cast from char).
//double  _cum; // oldloc// distance of flag from start of seismic line.
  float   _sina;         // sine of azimuth.
  float   _cosa;         // cosine of azimuth.
  int     _sdead;        // dead  source  code (from ZT1 cards).
  int     _rdead;        // dead receiver code (from ZT2 cards).

      // note: moving cum to new location (12/6/96) shaved
      //       two words off the size of the structure.

private:   // speedup values used only by FgTraceValues.

  long    _ixf_rec_closest;
  double  _rec_xloc;
  double  _rec_yloc;
  long    _rec_mgp;
  long    _rec_mgp_closest;
  int     _cmp_error;

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

  FieldFlag (class SeisLine *line);
  virtual ~FieldFlag();

//--------------- pass-thru to source and receiver lists -------------//
//--------------- pass-thru to source and receiver lists -------------//
//--------------- pass-thru to source and receiver lists -------------//

   // ixs2 = index of desired  source  (group number) at this flag.
   // ixr2 = index of desired receiver (trace number) at this flag.

public:          // get source-receiver values

  long   numSourcesAtFlag         ()           const;
  long   numReceiversAtFlag       ()           const;
  int    flagHasSource            ()           const;
  int    flagHasReceiver          ()           const;
  long   sourceGroupNumber        (long ixs2)  const;
  long   receiverTraceNumber      (long ixr2)  const;

public:          // set source-receiver values

  void   addSourceToFlag          (long sgroup);
  void   addReceiverToFlag        (long rtrace);
  void   removeSourcesFromFlag    ();
  void   removeReceiversFromFlag  ();
  void   trimSourceAllocation     ();              // optional
  void   trimReceiverAllocation   ();              // optional

//----------------- public access to this flag -----------------//
//----------------- public access to this flag -----------------//
//----------------- public access to this flag -----------------//

public:      // get flag values

  SeisLine *getLinePointer ()  const  { return _line; }
  long      getFlagIndex   ()  const  { return _ixf; }

  double    distanceToFlag        (double xloc, double yloc)  const;
  double    distanceSquaredToFlag (double xloc, double yloc)  const;

  int       flagValueIsDependent (int ident)  const
                                     { return FLAG_IS_ON(ident); }
  double    getFlagValue         (int ident)  const;

  float     getShotpoint        ()  const  { return _shotpoint; }
  double    getIncrDistance     ()  const  { return _dist; }
  double    getXloc             ()  const  { return _xloc; }
  double    getYloc             ()  const  { return _yloc; }
  float     getElevation        ()  const  { return _elev; }
  float     getHoleDepth        ()  const  { return _hd; }
  float     getUpholeTime       ()  const  { return _tuh; }
  float     getReceiverStatic   ()  const  { return _rstat; }
  float     getSourceStatic     ()  const  { return _sstat; }
  float     getReceiverXskid    ()  const  { return _xskid; }
  float     getReceiverYskid    ()  const  { return _yskid; }
  float     getReceiverEskid    ()  const  { return _eskid; }
  double    getCumDistance      ()  const  { return _cum; }
  float     getAzimuth          ()  const;
  float     getSineOfAzimuth    ()  const  { return _sina; }
  float     getCosineOfAzimuth  ()  const  { return _cosa; }
  int       getDeadSourceCode   ()  const  { return _sdead; }
  int       getDeadReceiverCode ()  const  { return _rdead; }
  int       sourceMaybeDead     ()  const;    // from ZT4 cards.
  int       receiverMaybeDead   ()  const;    // from ZT4 cards.
  float defaultSourceDatumStatic   (float ref, float ve)  const;
  float defaultReceiverDatumStatic (float ref, float ve)  const;
  int       receiverIsSkidded   ()  const;

public:      // set flag values

  // setFlagValue (and other set... functions):
  //     sets value.
  //     sets dependency flag FALSE.
  // setDependentFlagValue:
  //     sets value.
  //     sets dependency flag TRUE.

  void setFlagValue           (int ident, double value);
  void setDependentFlagValue  (int ident, double value);
/*
  void setSimpleFlagValue     (int ident, double value);
*/
  void setFlagDependencyTrue  (int ident)  { TURN_ON_FLAG (ident); }
  void setFlagDependencyFalse (int ident)  { TURN_OFF_FLAG(ident); }

  void setShotpoint       (float  value);
  void setIncrDistance    (double value);
  void setXloc            (double value);
  void setYloc            (double value);
  void setElevation       (float  value);
  void setHoleDepth       (float  value);
  void setUpholeTime      (float  value);
  void setReceiverStatic  (float  value);
  void setSourceStatic    (float  value);
  void setReceiverXskid   (float  value);
  void setReceiverYskid   (float  value);
  void setReceiverEskid   (float  value);

  void setDependentIncrDistance (double value) { _dist = value;
                                                 TURN_ON_FLAG(FG_DIST); }
  void setDependentXloc         (double value) { _xloc = value;
                                                 TURN_ON_FLAG(FG_XLOC); }

public:     // called only by SeisLine.

  void set2Shotpoint       (float  value) { _shotpoint = value; }
  void set2IncrDistance    (double value) { _dist      = value; }
  void set2Xloc            (double value) { _xloc      = value; }
  void set2Yloc            (double value) { _yloc      = value; }
  void set2Elevation       (float  value) { _elev      = value; }
  void set2HoleDepth       (float  value) { _hd        = value; }
  void set2UpholeTime      (float  value) { _tuh       = value; }
  void set2ReceiverStatic  (float  value) { _rstat     = value; }
  void set2SourceStatic    (float  value) { _sstat     = value; }

  void setIndexOfThisFlag   (long   value) { _ixf   = value; }
  void setSineOfAzimuth     (float  value) { _sina  = value; }
  void setCosineOfAzimuth   (float  value) { _cosa  = value; }
  void setDeadSourceCode    (int    value) { _sdead = value; }
  void setDeadReceiverCode  (int    value) { _rdead = value; }
  void setSourceMaybeDead   (int    value);    // from ZT4 cards.
  void setReceiverMaybeDead (int    value);    // from ZT4 cards.

  void updateCumDist (FieldFlag *prev, FieldFlag *next);

  char getFlagSelectValue ()       const  { return (char)_select; }
  void setFlagSelectValue (char select)   { _select = (float)select; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
