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

//---------------------- statgui_several.hh ---------------------------//
//---------------------- statgui_several.hh ---------------------------//
//---------------------- statgui_several.hh ---------------------------//

//             header file for the StatguiSeveral class
//               derived from the SLSmartForm class
//                       subdirectory statgui

#ifndef _STATGUI_SEVERAL_HH_
#define _STATGUI_SEVERAL_HH_

#include "sl/sl_smart_form.hh"
#include "stat/static_inform.hh"


class StatguiSeveral : public SLSmartForm, public StaticInform
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

public:

  enum { XDIRECTION = 1, YDIRECTION, XDIRECTION_ALL, YDIRECTION_ALL };

private:       // also manager() in StaticInform.

  class StatboxSeveral *_table;
  int                  _direction;         // one of the above enums.
  int                  _chosen_xindex;
  int                  _chosen_yindex;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  StatguiSeveral(SLDelay *slparent, class StaticManager *manager);

  virtual ~StatguiSeveral();

  StatboxSeveral *getStatboxSeveral ()  const  { return _table; }
  int             getDirection      ()  const  { return _direction; }
  int             getChosenXindex   ()  const;
  int             getChosenYindex   ()  const;

  void setStatboxSeveral (StatboxSeveral *table)  { _table = table; }
  void setDirection      (int direction)          { _direction = direction; }
  void setChosenXindex   (int ix);
  void setChosenYindex   (int iy);

  static void   freezeChosenIndices();
  static void unfreezeChosenIndices();

private:

  virtual void postNewActiveGroundPosition(StaticDataset *dataset);
  virtual void    findActiveGroundPosition(StaticDataset *dataset);

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
