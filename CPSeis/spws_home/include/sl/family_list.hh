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

//------------------- family_list.hh -----------------------------//
//------------------- family_list.hh -----------------------------//
//------------------- family_list.hh -----------------------------//

//         header file for FamilyElement and FamilyList
//           derived from Element and BaseLinkedList
//                      subdirectory sl    

//        supports linked lists of functions to call

//     Currently needed to tie updating of dfp_base classes
//     with updating of slp_base classes.  Also can be used
//     for many other purposes.  Can be eliminated when dfp_base
//     classes are no longer wrappers around windowbox routines.

//   Functions returning int return an error of 0 if found,
//                              and an error of 1 if not found.
//   After getting an error of 0, the user must then call
//     getCurrentFun() and getCurrentData() to get the function
//     and data.
//   And/or after getting an error of 0, the user must then call
//     callCurrentFun() to call the function.
//   Or the user may simply call CallFamily() to call all functions
//     in the list.


#ifndef _FAMILY_LIST_HH_
#define _FAMILY_LIST_HH_

#include "oprim/element.hh"
#include "oprim/ll_base.hh"


//-------------------- FamilyElement ------------------------//
//-------------------- FamilyElement ------------------------//
//-------------------- FamilyElement ------------------------//

class FamilyElement : public Element
{
private:

  void (*_fun)(void *data);
  void *_data;

  friend class FamilyList;

  FamilyElement    (void (*fun)(void *data), void *data);
  ~FamilyElement   () {}
  int  operator == (void * const element) const;
  void print       () const;
};


//------------------- FamilyList ----------------------------//
//------------------- FamilyList ----------------------------//
//------------------- FamilyList ----------------------------//

class FamilyList : public BaseLinkedList
{
public:

  FamilyList           ();
  ~FamilyList          () {}
  void     add         (void (*fun)(void *data), void *data);
  void     remove      (void (*fun)(void *data), void *data);
  int      find        (void (*fun)(void *data), void *data);
  int      top         ();
  int      bottom      ();
  int      next        ();
  int      prev        ();
  int      current     ();
  void *getCurrentFun  ();
  void *getCurrentData ();
  int   callCurrentFun ();
  void  callFamily     ();
};

//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//

#endif

//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

