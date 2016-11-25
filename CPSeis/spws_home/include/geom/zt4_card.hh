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

//---------------------- zt4_card.hh ---------------------//
//---------------------- zt4_card.hh ---------------------//
//---------------------- zt4_card.hh ---------------------//

//              header file for the Zt4Card class
//                  not derived from any class
//                      subdirectory geom

    // This class contains all data associated with a single
    // ZT4 card (zero receivers).


#ifndef _ZT4_CARD_HH_
#define _ZT4_CARD_HH_


class Zt4Card
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int   _code;         // code number (enum).
  float _from_sshot;   // from source shotpoint.
  float _to_sshot;     // to source shotpoint.
  long  _sline;        // source line number.
  float _from_rshot;   // from receiver shotpoint.
  float _to_rshot;     // to receiver shotpoint.
  long  _rline;        // receiver line number.

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

  Zt4Card();
  virtual ~Zt4Card();

//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//

public:      // get card values

  int    getZt4Code                  ()  const  { return _code; }
  float  getZt4FromSourceShotpoint   ()  const  { return _from_sshot; }
  float  getZt4ToSourceShotpoint     ()  const  { return _to_sshot; }
  long   getZt4SourceLineNumber      ()  const  { return _sline; }
  float  getZt4FromReceiverShotpoint ()  const  { return _from_rshot; }
  float  getZt4ToReceiverShotpoint   ()  const  { return _to_rshot; }
  long   getZt4ReceiverLineNumber    ()  const  { return _rline; }

  int    getMatchingZt4Code (long source_line, float source_shot,
                             long    rec_line, float    rec_shot)  const;

  int    sourceMaybeDead   (long source_line, float source_shot)  const;
  int    receiverMaybeDead (long    rec_line, float    rec_shot)  const;

public:      // set card values

  void   setAllValues                (Zt4Card *other);
  void   setZt4Code                  (int   value);
  void   setZt4FromSourceShotpoint   (float value);
  void   setZt4ToSourceShotpoint     (float value);
  void   setZt4SourceLineNumber      (long  value);
  void   setZt4FromReceiverShotpoint (float value);
  void   setZt4ToReceiverShotpoint   (float value);
  void   setZt4ReceiverLineNumber    (long  value);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
