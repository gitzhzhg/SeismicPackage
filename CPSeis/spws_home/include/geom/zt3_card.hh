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

//---------------------- zt3_card.hh ---------------------//
//---------------------- zt3_card.hh ---------------------//
//---------------------- zt3_card.hh ---------------------//

//              header file for the Zt3Card class
//                  not derived from any class
//                      subdirectory geom

    // This class contains all data associated with a single
    // ZT3 card (zero traces in groups).


#ifndef _ZT3_CARD_HH_
#define _ZT3_CARD_HH_


class Zt3Card
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  int   _code;         // code number (enum).
  long  _from_group;   // from group number.
  long  _to_group;     // to group number.
  long  _from_trace;   // from trace (channel) number.
  long  _to_trace;     // to trace (channel) number.

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

  Zt3Card();
  virtual ~Zt3Card();

//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//
//----------------- public access to this card -----------------//

public:      // get card values

  int    getZt3Code            ()  const  { return _code; }
  long   getZt3FromGroupNumber ()  const  { return _from_group; }
  long   getZt3ToGroupNumber   ()  const  { return _to_group; }
  long   getZt3FromTraceNumber ()  const  { return _from_trace; }
  long   getZt3ToTraceNumber   ()  const  { return _to_trace; }

  int    getMatchingZt3Code (long group, long channel)  const;
  int    groupPartlyDead    (long group)                const;

public:      // set card values

  void   setAllValues          (Zt3Card *other);
  void   setZt3Code            (int   value);
  void   setZt3FromGroupNumber (long  value);
  void   setZt3ToGroupNumber   (long  value);
  void   setZt3FromTraceNumber (long  value);
  void   setZt3ToTraceNumber   (long  value);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
