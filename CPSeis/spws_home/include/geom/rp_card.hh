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

//---------------------- rp_card.hh ---------------------//
//---------------------- rp_card.hh ---------------------//
//---------------------- rp_card.hh ---------------------//

//              header file for the RpCard class
//                  not derived from any class
//                      subdirectory geom

    // This class contains all data associated with a single
    // receiver pattern card (RP card).


#ifndef _RP_CARD_HH_
#define _RP_CARD_HH_


class RpCard
{
  friend class FgTraceValues;

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  long  _pattern;     // receiver pattern number.
  long  _nchannels;   // number of channels in entire pattern (all cards).
  long  _cumchannels; // number of channels up thru this card.
  int   _rpflag;      // receiver pattern flag (enum).
  float _shot;        // shotpoint   of first receiver in pattern.
  long  _line;        // line number of first receiver in pattern.
  long  _nx;          // number of       receivers in    inline direction.
  long  _xinc;        // spacing between receivers in    inline direction.
  long  _ny;          // number of       receivers in crossline direction.
  long  _yinc;        // spacing between receivers in crossline direction.
  float _xskid;       // pattern skid in    inline direction.
  float _yskid;       // pattern skid in crossline direction.
  float _eskid;       // pattern skid in elevation.

private:    // speedup values used only by FgTraceValues.

  long  _ixl;         // index of line corresponding to _line.
  long  _ixf;         // index of flag corresponding to _shot.

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

  RpCard();
  virtual ~RpCard();

//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//

public:      // get card values

  long   getRpPatternNumber ()  const  { return _pattern; }
  long   getRpNumChannels   ()  const  { return _nchannels; }
  long   getRpCumChannels   ()  const  { return _cumchannels; }
  int    getRpFlag          ()  const  { return _rpflag; }
  float  getRpShotpoint     ()  const  { return _shot; }
  long   getRpLineNumber    ()  const  { return _line; }
  long   getRpNumX          ()  const  { return _nx; }
  long   getRpXinc          ()  const  { return _xinc; }
  long   getRpNumY          ()  const  { return _ny; }
  long   getRpYinc          ()  const  { return _yinc; }
  float  getRpXskid         ()  const  { return _xskid; }
  float  getRpYskid         ()  const  { return _yskid; }
  float  getRpEskid         ()  const  { return _eskid; }

  void   getRpChannelsOnCard (long *first_channel, long *last_channel)  const;

  void   getRpIncrements (long channel, long *xinc,  long *yinc,
                                        long *incrx, long *incry)  const;

public:      // set card values

  void   setAllValues       (RpCard *other);
  void   setRpPatternNumber (long  value);
  void   setRpNumChannels   (long  value);
  void   setRpCumChannels   (long  value);
  void   setRpFlag          (int   value);
  void   setRpShotpoint     (float value);
  void   setRpLineNumber    (long  value);
  void   setRpNumX          (long  value);
  void   setRpXinc          (long  value);
  void   setRpNumY          (long  value);
  void   setRpYinc          (long  value);
  void   setRpXskid         (float value);
  void   setRpYskid         (float value);
  void   setRpEskid         (float value);

public:  // update cumulative number of channels.
         // num = number of channels thru previous RP card of same pattern.

  long   updateRpCumChannels(long num);        // returns updated num.

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
