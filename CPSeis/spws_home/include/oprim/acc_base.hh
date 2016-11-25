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

//------------------------ acc_base.hh ---------------------//
//------------------------ acc_base.hh ---------------------//
//------------------------ acc_base.hh ---------------------//

//              header file for the AccBase class
//                 not derived from any class
//                    subdirectory oprim

     // The prefix "Acc" means "array column control".

     // This class manages the setting of values in array
     //   elements in a class derived from SmartArray.
     // One value in each array element (identified with an
     //   ident, and analogous to a column of numbers in a table)
     //   can be managed by this class.
     // This is a base class, which will act as a simple
     //   pass-thru (no changes to the values) if used without
     //   derivation.
     // See the implementation file for more documentation.

     // The "preChange" and "postChange" methods are called before
     // and after a change occurs.  This class knows whether to
     // change any values based on information it receives.  The
     // range specified by <index, nrem, nins> correspond to the
     // range of values of the corresponding ident which will
     // change or have changed.  This class has the option of
     // resetting some or all of these values over this range,
     // or over a larger range.

     // This class reports the range of values actually changed
     // (which may exceed the range specified by <index, nrem, nins>)
     // by calling the virtual functions "valuesWillChange" and
     // "valuesHaveChanged" on SmartArray.  These calls can be suppressed
     // by setting _send to FALSE.  This should be done if some other
     // class or mechanism deals with reporting the changes, or if
     // the reporting is not necessary.


#ifndef _ACC_BASE_HH_
#define _ACC_BASE_HH_

#include "oprim/smart_array.hh"


class AccBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

protected:

  class SmartArray * const _array;
  const int                _ident;   // ident to manage (any integer).

private:

  const int _send;   // whether to send values-changed msgs  (TRUE or FALSE).
  int   _between;    // whether we are between pre and post  (TRUE or FALSE).
  int   _freeze;     // whether to freeze updates            (TRUE or FALSE).
  int   _need;       // whether changes are needed           (TRUE or FALSE).
  int   _accum;      // whether a range is being accumulated (TRUE or FALSE).
  long  _index_keep; // valid after pre and post; <= index.
  long  _nrem_keep;  // valid after pre and post; >= nrem.
  long  _nins_keep;  // valid after pre and post; >= nins.
  long  _n_new;      // new n for testing.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  AccBase (SmartArray *array, int ident, int send = 1);    // TRUE

  virtual ~AccBase();

  void  regRange     (long index, long nrem, long nins);
  void  regRange     (AccBase *other_acc_base);
  void  preChange    (long index, long nrem, long nins);
  void  preChange    (AccBase *other_acc_base);
  void  post1Change  ();
  void  post2Change  ();
  void  postChange   ();

  void  freezeUpdates ();
  void  resumeUpdates ();   // do only what is needed since freeze.
  void  performUpdates();   // do everything.

  int   updatesFrozen ()  const  { return _freeze; }
  int   updatesNeeded ()  const  { return _need; }
  long  modifiedIndex ()  const  { return _index_keep; }
  long  modifiedNrem  ()  const  { return _nrem_keep; }
  long  modifiedNins  ()  const  { return _nins_keep; }


protected:   // virtual functions to override.
             // no values are reset if not overridden.

     // getRange might be called if n == 0 or index2 == index1 - 1.
     // fixRange might be called if n == 0 or index2 == index1 - 1.

  virtual void getRange (long /*n*/, long* /*index1*/, long* /*index2*/) {}
  virtual void fixRange (long /*n*/, long  /*index1*/, long  /*index2*/) {}


public:   // virtual function to override.
          // no values or flags are reset if not overridden.

  virtual void adjustDependencyFlags() {}


private:

  void  preChangeHelper   ();
  void  post1ChangeHelper ();


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
