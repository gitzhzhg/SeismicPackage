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

//---------------------- statgui_nils2.hh ---------------------------//
//---------------------- statgui_nils2.hh ---------------------------//
//---------------------- statgui_nils2.hh ---------------------------//

//             header file for the StatguiNils2 class
//                 derived from the SLSmartForm class
//                        subdirectory statgui



#ifndef _STATGUI_NILS2_HH_
#define _STATGUI_NILS2_HH_

#include "sl/sl_smart_form.hh"


class StatguiNils2 : public SLSmartForm
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class StaticManager *_manager;

  long    _choice;
  float   _rconstant;
  float   _range1;
  float   _range2;
  int     _ixdist;
  int     _iydist;
  int     _require;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  StatguiNils2(SLDelay *slparent, class StaticManager *manager);

  virtual ~StatguiNils2();

  class StaticManager *manager       ()  const  { return _manager; }

  void takeAction();

public:  // get and set values.

  int         getChoice           ()  const  { return _choice; }
  float       getReplaceConstant  ()  const  { return _rconstant; }
  float       getRange1           ()  const  { return _range1; }
  float       getRange2           ()  const  { return _range2; }
  int         getIxdist           ()  const  { return _ixdist; }
  int         getIydist           ()  const  { return _iydist; }
  int         getRequire          ()  const  { return _require; }

  void        setReplaceConstant  (float value);
  void        setRange1           (float value);
  void        setRange2           (float value);
  void        setIxdist           (int   value);
  void        setIydist           (int   value);
  void        setRequire          (int   value);

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
