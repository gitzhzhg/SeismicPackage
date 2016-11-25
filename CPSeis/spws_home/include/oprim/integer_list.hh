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

//---------------------- integer_list.hh ---------------------//
//---------------------- integer_list.hh ---------------------//
//---------------------- integer_list.hh ---------------------//

//          header file for the SourcesReceivers class
//                  not derived from any class
//                      subdirectory geom

    // This class maintains a list of long integers.


#ifndef _INTEGER_LIST_HH_
#define _INTEGER_LIST_HH_


class IntegerList
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  long         _n;          // number of integers in list.
  long         _nalloc;     // allocated space for integers.
  long        *_array;      // array of integers.

  static long  _first_step; // first   allocation step.
  static long  _min_step;   // minimum allocation step (after first step).
  static long  _max_step;   // maximum allocation step (after first step).

   // set _min_step == _max_step to make all subsequent steps constant.

//------------------------- functions -----------------------//
//------------------------- functions -----------------------//
//------------------------- functions -----------------------//

public:      // constructor and destructor

  IntegerList           ();
  virtual ~IntegerList  ();

  long   numElements    ()              const  { return _n; }
  long  *getListPointer ()              const  { return _array; }
  long   getElement     (long index)    const;
  void   addElement     (long element);
  void   clearList      ();
  void   trimAllocation ();

  static void setAllocationSteps (long first_step =  1,
                                  long min_step   =  1,
                                  long max_step   = 50);

public:  // static routines with same functionality as above.
         // the IntegerList argument can be NULL (except for getElement).
         // addElement creates the object if it is NULL, and returns
         //    the (possibly modified) pointer to the object.
         // clearList deletes the object and returns NULL.

  static long         numElements    (IntegerList *list);
  static long        *getListPointer (IntegerList *list);
  static long         getElement     (IntegerList *list, long index);
  static IntegerList *addElement     (IntegerList *list, long element);
  static IntegerList *clearList      (IntegerList *list);
  static void         trimAllocation (IntegerList *list);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
