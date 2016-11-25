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

//---------------------- statgui_transform.cc ------------------------//
//---------------------- statgui_transform.cc ------------------------//
//---------------------- statgui_transform.cc ------------------------//

//         implementation file for the StatguiTransform class
//               derived from the SLSmartForm class
//                      subdirectory statgui

#include "statgui/statgui_transform.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_sep.hh"
#include "sl/radio_list.hh"
#include "sl/slp_radio.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include <stdio.h>
#include <string.h>



//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//

        // public.

#define SET(_x1old, setX1old)                          \
void  StatguiTransform::setX1old (float value)         \
{                                                      \
  _x1old = value;                                      \
}

/*
  takeProposedAction();                                \
*/

SET(_x1old, setX1old)
SET(_x2old, setX2old)
SET(_y1old, setY1old)
SET(_y2old, setY2old)
SET(_x1new, setX1new)
SET(_x2new, setX2new)
SET(_y1new, setY1new)
SET(_y2new, setY2new)



//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//


#define TRAP(x1old_trap, setX1old)                          \
static void x1old_trap(void *data, long /*ident*/,          \
                        float /*oldvar*/, float newvar)     \
{                                                           \
  StatguiTransform *gui = (StatguiTransform*)data;          \
  gui->setX1old(newvar);                                    \
}

TRAP(x1old_trap, setX1old)
TRAP(x2old_trap, setX2old)
TRAP(y1old_trap, setY1old)
TRAP(y2old_trap, setY2old)
TRAP(x1new_trap, setX1new)
TRAP(x2new_trap, setX2new)
TRAP(y1new_trap, setY1new)
TRAP(y2new_trap, setY2new)



//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//

/*
      // takeProposedAction() is temporary.
*/

#define UPDATE(x1old_update, getX1old)                \
static float x1old_update(void *data)                 \
{                                                     \
  StatguiTransform *gui = (StatguiTransform*)data;    \
  return gui->getX1old();                             \
}


/*
  gui->takeProposedAction();                          \
*/

UPDATE(x1old_update, getX1old)
UPDATE(x2old_update, getX2old)
UPDATE(y1old_update, getY1old)
UPDATE(y2old_update, getY2old)
UPDATE(x1new_update, getX1new)
UPDATE(x2new_update, getX2new)
UPDATE(y1new_update, getY1new)
UPDATE(y2new_update, getY2new)


static char *warning_update(void *data)
{
  StatguiTransform *gui = (StatguiTransform*)data;
  static char *buffer0 = "-";
  static char *buffer1 =
                 "warning: both old X ground positions cannot be the same";
  static char *buffer2 =
                 "warning: both old Y ground positions cannot be the same";
  static char *buffer3 =
                 "warning: both new X ground positions cannot be the same";
  static char *buffer4 =
                 "warning: both new Y ground positions cannot be the same";
  static char *buffer5 =
          "note: these parameters currently define a do-nothing operation";
  static char *buffer6 =
     "note: these parameters will cause the static dataset to be reversed";
  float x1old = gui->getX1old();
  float x2old = gui->getX2old();
  float y1old = gui->getY1old();
  float y2old = gui->getY2old();
  float x1new = gui->getX1new();
  float x2new = gui->getX2new();
  float y1new = gui->getY1new();
  float y2new = gui->getY2new();
  if(x1old == x2old) return buffer1;
  if(y1old == y2old) return buffer2;
  if(x1new == x2new) return buffer3;
  if(y1new == y2new) return buffer4;
  float xslope = (x2new - x1new) / (x2old - x1old);
  float yslope = (y2new - y1new) / (y2old - y1old);
  float old_x1   = gui->manager()->activeDataset()->getX1();
  float old_y1   = gui->manager()->activeDataset()->getY1();
  float old_xinc = gui->manager()->activeDataset()->getXinc();
  float old_yinc = gui->manager()->activeDataset()->getYinc();
  float new_x1   = x1new + (old_x1 - x1old) * xslope;
  float new_y1   = y1new + (old_y1 - y1old) * yslope;
  float new_xinc = old_xinc * xslope;
  float new_yinc = old_yinc * yslope;
  if(old_x1   == new_x1   && old_y1   == new_y1   &&
     old_xinc == new_xinc && old_yinc == new_yinc) return buffer5;
  if(xslope < 0.0 || yslope < 0.0) return buffer6;
  return buffer0;
}



//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        ".borderWidth:           2",
        "*background:            gray80",
        ".sep.separatorType:     SINGLE_LINE",
        ".sep.height:            6",
        ".warning.foreground:    red",
            NULL };



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


StatguiTransform::StatguiTransform(SLDelay *slparent, StaticManager *manager)
/*
StatguiTransform::StatguiTransform(SLDelay *slparent, StaticManager *manager,
                                                      StaticDataset *proposed)
*/
       : SLSmartForm(slparent, "statgui_transform"),
              _manager   (manager),
/*
              _proposed  (proposed),
*/
              _x1old     (1.0),
              _x2old     (2.0),
              _y1old     (1.0),
              _y2old     (2.0),
              _x1new     (1.0),
              _x2new     (2.0),
              _y1new     (1.0),
              _y2new     (2.0)
{
  assert(_manager);
/*
  assert(_manager && _proposed);
*/
  setFallbackResources(defres);

  SLpLabel *warning = new SLpLabel (this, "warning");
  SLpLabel *lab1    = new SLpLabel (this, "label", 0,
                            "any two current X ground positions:");
  SLpLabel *lab2    = new SLpLabel (this, "label", 0,
                            "corresponding desired X ground positions:");
  SLpLabel *lab3    = new SLpLabel (this, "label", 0,
                            "any two current Y ground positions:");
  SLpLabel *lab4    = new SLpLabel (this, "label", 0,
                            "corresponding desired Y ground positions:");

  SLpText  *x1old = new SLpText (this, "text", 0, SLpText::_FLOAT, 8, 3);
  SLpText  *x2old = new SLpText (this, "text", 0, SLpText::_FLOAT, 8, 3);
  SLpText  *y1old = new SLpText (this, "text", 0, SLpText::_FLOAT, 8, 3);
  SLpText  *y2old = new SLpText (this, "text", 0, SLpText::_FLOAT, 8, 3);
  SLpText  *x1new = new SLpText (this, "text", 0, SLpText::_FLOAT, 8, 3);
  SLpText  *x2new = new SLpText (this, "text", 0, SLpText::_FLOAT, 8, 3);
  SLpText  *y1new = new SLpText (this, "text", 0, SLpText::_FLOAT, 8, 3);
  SLpText  *y2new = new SLpText (this, "text", 0, SLpText::_FLOAT, 8, 3);

  warning->setupCvarFun(warning_update, this);

  x1old->setFtrap(x1old_trap, this);
  x2old->setFtrap(x2old_trap, this);
  y1old->setFtrap(y1old_trap, this);
  y2old->setFtrap(y2old_trap, this);
  x1new->setFtrap(x1new_trap, this);
  x2new->setFtrap(x2new_trap, this);
  y1new->setFtrap(y1new_trap, this);
  y2new->setFtrap(y2new_trap, this);

  x1old->setupFvarFun(x1old_update, this);
  x2old->setupFvarFun(x2old_update, this);
  y1old->setupFvarFun(y1old_update, this);
  y2old->setupFvarFun(y2old_update, this);
  x1new->setupFvarFun(x1new_update, this);
  x2new->setupFvarFun(x2new_update, this);
  y1new->setupFvarFun(y1new_update, this);
  y2new->setupFvarFun(y2new_update, this);

         /////       LEFT     RIGHT    TOP      BOTTOM

  attach(lab1      , this  ,  x1old  , this   ,  NULL ,  2, 2,10);
  attach(x1old     , NULL  ,  x2old  , this   ,  NULL ,  2, 2,10);
  attach(x2old     , NULL  ,  this   , this   ,  NULL ,  2, 2,10);
  attach(lab2      , this  ,  x1new  , x1old  ,  NULL ,  2, 2, 0);
  attach(x1new     , NULL  ,  x2new  , x1old  ,  NULL ,  2, 2, 0);
  attach(x2new     , NULL  ,  this   , x1old  ,  NULL ,  2, 2, 0);
  attach(warning   , this  ,  this   , x1new  ,  NULL ,  0, 0,10);
  attach(lab3      , this  ,  y1old  , warning,  NULL ,  2, 2,10);
  attach(y1old     , NULL  ,  y2old  , warning,  NULL ,  2, 2,10);
  attach(y2old     , NULL  ,  this   , warning,  NULL ,  2, 2,10);
  attach(lab4      , this  ,  y1new  , y1old  ,  this ,  2, 2, 0,8);
  attach(y1new     , NULL  ,  y2new  , y1old  ,  this ,  2, 2, 0,8);
  attach(y2new     , NULL  ,  this   , y1old  ,  this ,  2, 2, 0,8);

/*
  takeProposedAction();
*/
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


StatguiTransform::~StatguiTransform()
{
}



/*
//------------------------- update proposed dataset --------------------//
//------------------------- update proposed dataset --------------------//
//------------------------- update proposed dataset --------------------//

        // public.

void StatguiTransform::updateProposedDataset()
{
  StaticDataset *dataset = manager()->activeDataset();
  _proposed->setX1  (dataset->getX1());
  _proposed->setY1  (dataset->getY1());
  _proposed->setXinc(dataset->getXinc());
  _proposed->setYinc(dataset->getYinc());
}



//--------------------- take proposed action ------------------------//
//--------------------- take proposed action ------------------------//
//--------------------- take proposed action ------------------------//

        // private.

void StatguiTransform::takeProposedAction()
{
  updateProposedDataset();
  _proposed->transformGroundPositions(NULL, _x1old, _x2old, _y1old, _y2old,
                                            _x1new, _x2new, _y1new, _y2new);
}
*/



//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//

        // public.

void StatguiTransform::takeAction()
{
  StaticDataset *dataset = manager()->activeDataset();
  dataset->transformGroundPositions(this, _x1old, _x2old, _y1old, _y2old,
                                          _x1new, _x2new, _y1new, _y2new);
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
