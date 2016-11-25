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

//------------------- static_inform_list.hh -----------------------------//
//------------------- static_inform_list.hh -----------------------------//
//------------------- static_inform_list.hh -----------------------------//

//    header file for StaticInformElement and StaticInformList
//            derived from Element and BaseLinkedList
//                       subdirectory stat

//        supports linked lists of StaticInform objects


#ifndef _STATIC_INFORM_LIST_HH_
#define _STATIC_INFORM_LIST_HH_

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <assert.h>

class StaticInform;


//------------------- StaticInformElement -----------------------//
//------------------- StaticInformElement -----------------------//
//------------------- StaticInformElement -----------------------//

class StaticInformElement : public Element
{
private:

  StaticInform *_inform;

  friend class StaticInformList;

   StaticInformElement (StaticInform *inform);
  ~StaticInformElement () {}
  int operator ==      (void * const inform) const;
  void print           () const;
};


//---------------------- StaticInformList ------------------------------//
//---------------------- StaticInformList ------------------------------//
//---------------------- StaticInformList ------------------------------//

class StaticInformList : public BaseLinkedList
{
public:

           StaticInformList  ();
  virtual ~StaticInformList  () {}
  void          add          (StaticInform *inform);
  void          remove       (StaticInform *inform);
  StaticInform *find         (StaticInform *inform);
  StaticInform *top          (void **p = 0);      // NULL
  StaticInform *bottom       (void **p = 0);      // NULL
  StaticInform *next         (void **p = 0);      // NULL
  StaticInform *prev         (void **p = 0);      // NULL
  StaticInform *current      (void **p = 0);      // NULL
};


//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//

#endif

//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

