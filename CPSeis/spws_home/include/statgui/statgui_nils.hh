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

//---------------------------- statgui_nils.hh -----------------------//
//---------------------------- statgui_nils.hh -----------------------//
//---------------------------- statgui_nils.hh -----------------------//

//                header file for the StatguiNils class
//                 derived from the SLSmartForm class
//                       subdirectory statgui


#ifndef _STATGUI_NILS_HH_
#define _STATGUI_NILS_HH_

#include "sl/sl_smart_form.hh"
#include <X11/Intrinsic.h>


class StatguiNils : public SLSmartForm
{

//---------------------------- data -----------------------------------//
//---------------------------- data -----------------------------------//
//---------------------------- data -----------------------------------//

private:

  class StaticManager  *_manager;
  void                 *_doer4;
  void                 *_doer44;
  void                 *_doer5;
  void                 *_doer5x;
  void                 *_doer5y;
  void                 *_doer5n;
  float                 _rconstant;
  float                 _range1;
  float                 _range2;
  int                   _ixdist;
  int                   _iydist;
  int                   _require;

//------------------------ functions --------------------------------//
//------------------------ functions --------------------------------//
//------------------------ functions --------------------------------//

public:

  StatguiNils(SLDelay *slparent, class StaticManager *manager);
  virtual ~StatguiNils();

  class StaticManager *manager       ()  const  { return _manager; }
  class StaticDataset *activeDataset ()  const;

  void          *getDoer4    ()  const  { return _doer4; }
  void          *getDoer44   ()  const  { return _doer44; }
  void          *getDoer5    ()  const  { return _doer5; }
  void          *getDoer5x   ()  const  { return _doer5x; }
  void          *getDoer5y   ()  const  { return _doer5y; }
  void          *getDoer5n   ()  const  { return _doer5n; }

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

  void cancelUndo();

//-------------------------- end of functions -----------------------//
//-------------------------- end of functions -----------------------//
//-------------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
