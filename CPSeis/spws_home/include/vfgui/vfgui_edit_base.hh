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

//---------------------- vfgui_edit_base.hh ---------------------------//
//---------------------- vfgui_edit_base.hh ---------------------------//
//---------------------- vfgui_edit_base.hh ---------------------------//

//             header file for the VfguiEditBase class
//               derived from the SLSmartForm class
//                       subdirectory vfgui


         // This is the base class for various guis which are
         // used to edit data residing in a class derived from
         // VfEditBase.

         // The derived class must create the _edit object in its
         // constructor, and then must call supplyEditObject from
         // its constructor to allow this base class to set _edit
         // to the address of the _edit object.  This base class
         // deletes _edit in its destructor.


#ifndef _VFGUI_EDIT_BASE_HH_
#define _VFGUI_EDIT_BASE_HH_

#include "sl/sl_smart_form.hh"


class VfguiEditBase : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class VfEditBase    *_edit;        // owned by this class.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

           VfguiEditBase (SLDelay *slparent);
  virtual ~VfguiEditBase ();

  VfEditBase   *edit    ()  const  { return _edit; }

protected:  // to be called from constructor of derived class, after the
            //   derived class creates this object derived from VfEditBase.

  void supplyEditObject (VfEditBase *edit);


//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
