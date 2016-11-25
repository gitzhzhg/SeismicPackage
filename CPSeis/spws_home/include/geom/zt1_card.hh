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

//---------------------- zt1_card.hh ---------------------//
//---------------------- zt1_card.hh ---------------------//
//---------------------- zt1_card.hh ---------------------//

//              header file for the Zt1Card class
//                  not derived from any class
//                      subdirectory geom

    // This class contains all data associated with a single
    // ZT1 card (zero sources).


#ifndef _ZT1_CARD_HH_
#define _ZT1_CARD_HH_


class Zt1Card
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int   _code;        // code number (enum).
  float _from_shot;   // from source shotpoint.
  float _to_shot;     // to source shotpoint.
  long  _line;        // source line number.

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

  Zt1Card();
  virtual ~Zt1Card();

//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//

public:      // get card values

  int    getZt1Code                ()  const  { return _code; }
  float  getZt1FromSourceShotpoint ()  const  { return _from_shot; }
  float  getZt1ToSourceShotpoint   ()  const  { return _to_shot; }
  long   getZt1SourceLineNumber    ()  const  { return _line; }

  int    getMatchingZt1Code (long source_line, float source_shot)  const;

public:      // set card values

  void   setAllValues              (Zt1Card *other);
  void   setZt1Code                (int   value);
  void   setZt1FromSourceShotpoint (float value);
  void   setZt1ToSourceShotpoint   (float value);
  void   setZt1SourceLineNumber    (long  value);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
