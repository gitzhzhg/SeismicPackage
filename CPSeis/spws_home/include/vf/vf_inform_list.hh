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

//------------------- vf_inform_list.hh -----------------------------//
//------------------- vf_inform_list.hh -----------------------------//
//------------------- vf_inform_list.hh -----------------------------//

//        header file for VfInformElement and VfInformList
//            derived from Element and BaseLinkedList
//                       subdirectory vf

//        supports linked lists of VfInform objects


#ifndef _VF_INFORM_LIST_HH_
#define _VF_INFORM_LIST_HH_

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <assert.h>

class VfInform;


//------------------- VfInformElement -----------------------//
//------------------- VfInformElement -----------------------//
//------------------- VfInformElement -----------------------//

class VfInformElement : public Element
{
private:

  VfInform *_inform;

  friend class VfInformList;

  VfInformElement  (VfInform *inform);
  ~VfInformElement () {}
  int operator ==  (void * const inform) const;
  void print       () const;
};


//---------------------- VfInformList ------------------------------//
//---------------------- VfInformList ------------------------------//
//---------------------- VfInformList ------------------------------//

class VfInformList : public BaseLinkedList
{
public:

  VfInformList           ();
  virtual ~VfInformList  () {}
  void      add          (VfInform *inform);
  void      remove       (VfInform *inform);
  VfInform *find         (VfInform *inform);
  VfInform *top          (void **p = 0);      // NULL
  VfInform *bottom       (void **p = 0);      // NULL
  VfInform *next         (void **p = 0);      // NULL
  VfInform *prev         (void **p = 0);      // NULL
  VfInform *current      (void **p = 0);      // NULL
};


//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//

#endif

//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

