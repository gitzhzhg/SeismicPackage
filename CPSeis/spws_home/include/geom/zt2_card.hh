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

//---------------------- zt2_card.hh ---------------------//
//---------------------- zt2_card.hh ---------------------//
//---------------------- zt2_card.hh ---------------------//

//              header file for the Zt2Card class
//                  not derived from any class
//                      subdirectory geom

    // This class contains all data associated with a single
    // ZT2 card (zero receivers).


#ifndef _ZT2_CARD_HH_
#define _ZT2_CARD_HH_


class Zt2Card
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int   _code;        // code number (enum).
  float _from_shot;   // from receiver shotpoint.
  float _to_shot;     // to receiver shotpoint.
  long  _line;        // receiver line number.

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

  Zt2Card();
  virtual ~Zt2Card();

//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//

public:      // get card values

  int    getZt2Code                  ()  const  { return _code; }
  float  getZt2FromReceiverShotpoint ()  const  { return _from_shot; }
  float  getZt2ToReceiverShotpoint   ()  const  { return _to_shot; }
  long   getZt2ReceiverLineNumber    ()  const  { return _line; }

  int    getMatchingZt2Code (long rec_line, float rec_shot)  const;

public:      // set card values

  void   setAllValues                (Zt2Card *other);
  void   setZt2Code                  (int   value);
  void   setZt2FromReceiverShotpoint (float value);
  void   setZt2ToReceiverShotpoint   (float value);
  void   setZt2ReceiverLineNumber    (long  value);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
