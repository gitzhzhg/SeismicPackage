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

//----------------------- active_index_keeper.hh ---------------------------//
//----------------------- active_index_keeper.hh ---------------------------//
//----------------------- active_index_keeper.hh ---------------------------//

//             header file for the ActiveIndexKeeper class
//                    not derived from any class
//                        subdirectory oprim

  // This class maintains the "active" index for one or more linked
  // arrays.  For this class to work properly, the appropriate
  // function must be called each time a similar function is called
  // on the arrays to insert, remove, or replace any array elements,
  // or to reset the number of array elements.


//------------------------ start of coding --------------------------------//
//------------------------ start of coding --------------------------------//
//------------------------ start of coding --------------------------------//


#ifndef _ACTIVE_INDEX_KEEPER_HH_
#define _ACTIVE_INDEX_KEEPER_HH_


class ActiveIndexKeeper
{

//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//
//-------------------------------- data -----------------------------------//

public:

  enum Which { MOVE_WITH_VALUE, SHOW_LAST_CHANGE };

private:

  const Which _which;      // one of the above enums.
  long        _nelements;  // number of array elements.
  long        _active;     // active index (or -1 if no elements).


//------------------------------- functions -------------------------------//
//------------------------------- functions -------------------------------//
//------------------------------- functions -------------------------------//

public:

           ActiveIndexKeeper (const Which which = MOVE_WITH_VALUE);
  virtual ~ActiveIndexKeeper ();

  long  numElements              ()  const  { return _nelements; }
  long  getActiveIndex           ()  const  { return _active; }

  void  setActiveIndex           (long index);
  void  setValue                 (long index);
  void  setLastValue             ();

  void  setOrAppendValue         (long index);

  void  appendElement            ();
  void  insertElement            (long index);
  void  removeElement            (long index);

  void  deleteAllElements        ();
  void  resetNumElements         (long nelements);
  void  resetNumElementsAndClear (long nelements);
  void  copyAllElements          (const ActiveIndexKeeper *object);


//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//
//--------------------------- end of functions ----------------------------//

} ;

#endif

//--------------------------------- end -----------------------------------//
//--------------------------------- end -----------------------------------//
//--------------------------------- end -----------------------------------//
