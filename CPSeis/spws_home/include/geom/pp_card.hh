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

//---------------------- pp_card.hh ---------------------//
//---------------------- pp_card.hh ---------------------//
//---------------------- pp_card.hh ---------------------//

//              header file for the PpCard class
//                  not derived from any class
//                      subdirectory geom

    // This class contains all data associated with a single
    // profile pattern card (PP card).


#ifndef _PP_CARD_HH_
#define _PP_CARD_HH_

#define  TURN_ON_PP(ident)  ( _depend |=  (1 << ident) )
#define TURN_OFF_PP(ident)  ( _depend &= ~(1 << ident) )
#define    PP_IS_ON(ident)  ( _depend &   (1 << ident) )


class PpCard
{
  friend class FgTraceValues;

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int    _depend;   // each bit TRUE if value is dependent.
  long   _file;     // first original field file number on card.
  float  _sshot;    // shotpoint   of first source   on card.
  long   _sline;    // line number of first source   on card.
  float  _rshot;    // shotpoint   of first receiver in pattern.
  long   _rline;    // line number of first receiver in pattern.
  long   _pattern;  // receiver pattern number.
  float  _xskid;    // source inline    skid.
  float  _yskid;    // source crossline skid.
  long   _hold;     // how many sources to hold skid.
  float  _elev;     // new source elevation.
  float  _hd;       // new source hole depth.
  float  _tuh;      // new source uphole time.
  long   _smove;    // source   moveup in FieldFlag indices.
  long   _rmove;    // receiver moveup in FieldFlag indices.
  long   _ngroups;  // number of groups described by this card.
  long   _ntraces;  // number of traces described by this card.
  long   _thru_gr;  // through group number for this card.
  long   _thru_tr;  // through trace number for this card.

private:    // speedup values used only by FgTraceValues.

  long   _first_group;       // source
  long   _ixl_pp_source;     // source
  long   _ixf_pp_source;     // source
  int    _source_error;

  long   _first_trace;       // receiver
  long   _nchan;             // receiver
  long   _ixl_pp_rec;        // receiver
  long   _ixf_pp_rec;        // receiver
  long   _ixrp_first;        // receiver
  long   _ixrp_last;         // receiver
  long   _ixrp_first_irreg;  // receiver
  long   _ixrp_last_irreg;   // receiver
  long   _ixrp_chan1;        // receiver
  long   _rp_line_chan1;     // receiver
  long   _rp_mgp_chan1;      // receiver
  int    _rec_error;

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

  PpCard();
  virtual ~PpCard();


//----------------- public access to this flag -----------------//
//----------------- public access to this flag -----------------//
//----------------- public access to this flag -----------------//

public:      // get card values

  int       ppValueIsDependent (int ident)  const
                                   { return PP_IS_ON(ident); }
/*
  float     getPpValue         (int ident)  const;
*/
  double    getPpValue         (int ident)  const;

  long   getFirstFileNumber   ()  const  { return _file; }
  long   getThruFileNumber    ()  const;
  float  getSourceShotpoint   ()  const  { return _sshot; }
  float  getReceiverShotpoint ()  const  { return _rshot; }
  long   getSourceLine        ()  const  { return _sline; }
  long   getReceiverLine      ()  const  { return _rline; }
  long   getPatternNumber     ()  const  { return _pattern; }
  float  getSourceXskid       ()  const  { return _xskid; }
  float  getSourceYskid       ()  const  { return _yskid; }
  long   getSkidHold          ()  const  { return _hold; }
  float  getNewElevation      ()  const  { return _elev; }
  float  getNewHoleDepth      ()  const  { return _hd; }
  float  getNewUpholeTime     ()  const  { return _tuh; }
  long   getSourceMove        ()  const  { return _smove; }
  long   getReceiverMove      ()  const  { return _rmove; }
  long   getNumGroupsOnCard   ()  const  { return _ngroups; }
  long   getNumTracesOnCard   ()  const  { return _ntraces; }
  long   getNumChannelsOnCard ()  const;
  long   getFirstGroupNumber  ()  const;
  long   getFirstTraceNumber  ()  const;
  long   getThruGroupNumber   ()  const  { return _thru_gr; }
  long   getThruTraceNumber   ()  const  { return _thru_tr; }

  void   getSourceSkids     (long group, float *inline_skid,
                                         float *crossline_skid)  const;
  void   getSourceSkidsQuickly(long ixg, float *inline_skid,
                                         float *crossline_skid)  const;
  void   getGroupAndChannel(long trace, long *group, long *channel)  const;

public:      // set card values

  // setPpValue (and other set... functions):
  //     sets value.
  //     sets dependency flag FALSE.
  // setDependentPpValue (and the second setNumTracesOnCard):
  //     sets value.
  //     sets dependency flag TRUE.

/*
  void   setPpValue           (int ident, float value);
  void   setDependentPpValue  (int ident, float value);
*/
  void   setPpValue           (int ident, double value);
  void   setDependentPpValue  (int ident, double value);
/*
  void   setSimplePpValue     (int ident, float value);
*/
  void   setPpDependencyTrue  (int ident)  { TURN_ON_PP (ident); }
  void   setPpDependencyFalse (int ident)  { TURN_OFF_PP(ident); }


  void   setFirstFileNumber   (long  value);
  void   setSourceShotpoint   (float value);
  void   setReceiverShotpoint (float value);
  void   setSourceLine        (long  value);
  void   setReceiverLine      (long  value);
  void   setPatternNumber     (long  value);
  void   setSourceXskid       (float value);
  void   setSourceYskid       (float value);
  void   setSkidHold          (long  value);
  void   setNewElevation      (float value);
  void   setNewHoleDepth      (float value);
  void   setNewUpholeTime     (float value);
  void   setSourceMove        (long  value);
  void   setReceiverMove      (long  value);
  void   setNumGroupsOnCard   (long  value);
  void   setNumTracesOnCard   (long  value);
  void   setThruGroupNumber   (long  value);
  void   setThruTraceNumber   (long  value);

  long   updateThruGroupNumber (long  prev_thru_gr);
  long   updateThruTraceNumber (long  prev_thru_tr);

  void   setNumTracesOnCard   (class FgConnect *connect);

public:        // called only by PpCards.

  void   set2SourceShotpoint   (float value)  { _sshot   = value; }
  void   set2ReceiverShotpoint (float value)  { _rshot   = value; }
  void   set2SourceLine        (long  value)  { _sline   = value; }
  void   set2ReceiverLine      (long  value)  { _rline   = value; }
  void   set2PatternNumber     (long  value)  { _pattern = value; }
  void   set2SourceMove        (long  value)  { _smove   = value; }
  void   set2ReceiverMove      (long  value)  { _rmove   = value; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
