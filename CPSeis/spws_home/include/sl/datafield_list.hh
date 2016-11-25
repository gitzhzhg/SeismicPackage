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

//------------------ datafield_list.hh -----------------------------//
//------------------ datafield_list.hh -----------------------------//
//------------------ datafield_list.hh -----------------------------//

//        header file for DatafieldElement and DatafieldList
//              derived from Element and BaseLinkedList
//                       subdirectory sl

//        supports linked lists of DFpBase objects


#ifndef _DATAFIELD_LIST_HH_
#define _DATAFIELD_LIST_HH_

#include "oprim/element.hh"
#include "oprim/ll_base.hh"

class DFpBase;


//--------------------- DatafieldElement ------------------------//
//--------------------- DatafieldElement ------------------------//
//--------------------- DatafieldElement ------------------------//

class DatafieldElement : public Element
{
private:

  DFpBase *_gui;

  friend class DatafieldList;

  DatafieldElement  (DFpBase *gui);
  ~DatafieldElement () {}
  int operator ==   (void * const gui) const;
  void print        () const;
};


//-------------------- DatafieldList ----------------------//
//-------------------- DatafieldList ----------------------//
//-------------------- DatafieldList ----------------------//

class DatafieldList : public BaseLinkedList
{
public:

  DatafieldList    ();
  ~DatafieldList   () {}
  void     add     (DFpBase *gui);
  void     remove  (DFpBase *gui);
  DFpBase *find    (DFpBase *gui);
  DFpBase *top     ();
  DFpBase *bottom  ();
  DFpBase *next    ();
  DFpBase *prev    ();
  DFpBase *current ();
};

//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//

#endif

//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

