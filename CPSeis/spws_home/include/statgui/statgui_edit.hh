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

//---------------------------- statgui_edit.hh ---------------------------//
//---------------------------- statgui_edit.hh ---------------------------//
//---------------------------- statgui_edit.hh ---------------------------//

//                header file for the StatguiEdit class
//                 derived from the SLSmartForm class
//                       subdirectory statgui


#ifndef _STATGUI_EDIT_HH_
#define _STATGUI_EDIT_HH_

#include "sl/sl_smart_form.hh"
#include <X11/Intrinsic.h>


class StatguiEdit : public SLSmartForm
{

//---------------------------- data -----------------------------------//
//---------------------------- data -----------------------------------//
//---------------------------- data -----------------------------------//

private:

  class StaticManager  *_manager;
  void                 *_doer1;
  void                 *_doer2;
  void                 *_doer3;
  void                 *_doer6;
  void                 *_doer7;
  void                 *_doer8;
  void                 *_doer9;
  float                 _xmin;
  float                 _xmax;
  float                 _ymin;
  float                 _ymax;
  float                 _mconstant;
  float                 _aconstant;
  int                   _trim;
  float                 _xrun;
  float                 _yrun;
  int                   _endflag;

//------------------------ functions --------------------------------//
//------------------------ functions --------------------------------//
//------------------------ functions --------------------------------//

public:

  StatguiEdit(SLDelay *slparent, class StaticManager *manager);
  virtual ~StatguiEdit();

  class StaticManager *manager       ()  const  { return _manager; }
  class StaticDataset *activeDataset ()  const;

  void          *getDoer1    ()  const  { return _doer1; }
  void          *getDoer2    ()  const  { return _doer2; }
  void          *getDoer3    ()  const  { return _doer3; }
  void          *getDoer6    ()  const  { return _doer6; }
  void          *getDoer7    ()  const  { return _doer7; }
  void          *getDoer8    ()  const  { return _doer8; }
  void          *getDoer9    ()  const  { return _doer9; }

  float       getXmin             ()  const  { return _xmin; }
  float       getXmax             ()  const  { return _xmax; }
  float       getYmin             ()  const  { return _ymin; }
  float       getYmax             ()  const  { return _ymax; }
  float       getMultiplyConstant ()  const  { return _mconstant; }
  float       getAddConstant      ()  const  { return _aconstant; }
  int         getTrimPercentage   ()  const  { return _trim; }
  float       getXrun             ()  const  { return _xrun; }
  float       getYrun             ()  const  { return _yrun; }
  int         getEndFlag          ()  const  { return _endflag; }

  void        setXmin             (float value);
  void        setXmax             (float value);
  void        setYmin             (float value);
  void        setYmax             (float value);
  void        setMultiplyConstant (float value);
  void        setAddConstant      (float value);
  void        setTrimPercentage   (int   value);
  void        setXrun             (float value);
  void        setYrun             (float value);
  void        setEndFlag          (int   value);

  void cancelUndo();

//-------------------------- end of functions -----------------------//
//-------------------------- end of functions -----------------------//
//-------------------------- end of functions -----------------------//

} ;

#endif

//-------------------------------- end --------------------------------//
//-------------------------------- end --------------------------------//
//-------------------------------- end --------------------------------//
