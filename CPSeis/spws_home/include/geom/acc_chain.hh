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

//------------------------ acc_chain.hh ---------------------//
//------------------------ acc_chain.hh ---------------------//
//------------------------ acc_chain.hh ---------------------//

//            header file for the AccChain class
//              derived from the AccBase class
//                    subdirectory geom

     // This class calculates chaining values in array elements
     //   in the SeisLine class (derived from SmartArray).
     // See the implementation file for documentation.


#ifndef _ACC_CHAIN_HH_
#define _ACC_CHAIN_HH_

#include "oprim/acc_base.hh"


class AccChain : public AccBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class SeisLine *_seis_line;

  int  _chaining;        // chaining parameter.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  AccChain (SeisLine *seis_line, int chaining);
  virtual ~AccChain();

protected:   // virtual functions overriding AccBase

  virtual void getRange (long n, long *index1, long *index2);
  virtual void fixRange (long n, long  index1, long  index2);

public:

  void  notifyReverseDirection ();
  int   setChaining            (int chaining);

  int   needsValues         (int ident)  const;
  int   changesValues       (int ident)  const;
  int   getChaining         ()           const  { return _chaining; }

  void  preChange2 (long index, long nrem, long nins);
  void  postChange2(long index, long nrem, long nins);

private:

  void  horizontalChainingUpdate (long n);
  void  slopeChainingUpdate      (long n);
  void  noChainingUpdate         (long n);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
