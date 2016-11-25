
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
//---------------------- line_create_pop.cc ------------------------------//
//---------------------- line_create_pop.cc ------------------------------//
//---------------------- line_create_pop.cc ------------------------------//

//           implementation file for the LineCreatePop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to create a grid of seismic
      // lines and flags.


#include "fggui/line_create_pop.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_constants.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_text.hh"
#include "sl/slp_push.hh"
#include "cprim.h"
#include "named_constants.h"
#include <math.h>
#include <iostream.h>
#include <assert.h>


#define NCHAR  12


//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//


void LineCreatePop::setLineIncrement(long   value)
{
  if(value == 0) return;
  _line_increment = value;
}


void LineCreatePop::setShotIncrement(float  value)
{
  if(value == 0.0) return;
  _shot_increment = value;
}


void LineCreatePop::setXlocIncrement(double value)
{
  if(value == 0.0) return;
  _xloc_increment = value;
}


void LineCreatePop::setYlocIncrement(double value)
{
  if(value == 0.0) return;
  _yloc_increment = value;
}


void LineCreatePop::setNumLines     (long   value)
{
  if(value <= 0) return;
  _num_lines = value;
}


void LineCreatePop::setNumFlags     (long   value)
{
  if(value <= 0) return;
  _num_flags = value;
}




//---------------------- variable traps -----------------------------//
//---------------------- variable traps -----------------------------//
//---------------------- variable traps -----------------------------//


#define TRAP1(first_line_trap, long2, setFirstLine)                  \
static void first_line_trap(void *data, long, long2, long2 newvar)   \
{                                                                    \
  LineCreatePop *pop = (LineCreatePop*)data;                         \
  pop->setFirstLine(newvar);                                         \
}

TRAP1 (first_line_trap, long  , setFirstLine)
TRAP1 (first_shot_trap, float , setFirstShot)
TRAP1 (first_xloc_trap, double, setFirstXloc)
TRAP1 (first_yloc_trap, double, setFirstYloc)

TRAP1 ( incr_line_trap, long  , setLineIncrement)
TRAP1 ( incr_shot_trap, float , setShotIncrement)
TRAP1 ( incr_xloc_trap, double, setXlocIncrement)
TRAP1 ( incr_yloc_trap, double, setYlocIncrement)

TRAP1 ( num_lines_trap, long  , setNumLines)
TRAP1 ( num_flags_trap, long  , setNumFlags)
TRAP1 (     angle_trap, double, setAngle)


#define TRAP2(last_trap, long2, getFirst, getIncrement, setNumLines)  \
static void last_trap(void *data, long, long2, long2 newvar)          \
{                                                                     \
  LineCreatePop *pop = (LineCreatePop*)data;                          \
  double first = (double)pop->getFirst();                             \
  double incr  = (double)pop->getIncrement();                         \
  long num_lines = NearestInteger((newvar - first) / incr) + 1;       \
  pop->setNumLines(num_lines);                                        \
}

TRAP2 (last_line_trap, long  , getFirstLine, getLineIncrement, setNumLines)
TRAP2 (last_shot_trap, float , getFirstShot, getShotIncrement, setNumFlags)
TRAP2 (last_xloc_trap, double, getFirstXloc, getXlocIncrement, setNumFlags)
TRAP2 (last_yloc_trap, double, getFirstYloc, getYlocIncrement, setNumLines)



//---------------------- variable update functions ------------------//
//---------------------- variable update functions ------------------//
//---------------------- variable update functions ------------------//


#define UPDATE1(first_line_update, long2, getFirstLine)    \
static long2 first_line_update(void *data)                 \
{                                                          \
  LineCreatePop *pop = (LineCreatePop*)data;               \
  return pop->getFirstLine();                              \
}

UPDATE1 (first_line_update, long  , getFirstLine)
UPDATE1 (first_shot_update, float , getFirstShot)
UPDATE1 (first_xloc_update, double, getFirstXloc)
UPDATE1 (first_yloc_update, double, getFirstYloc)

UPDATE1 ( incr_line_update, long  , getLineIncrement)
UPDATE1 ( incr_shot_update, float , getShotIncrement)
UPDATE1 ( incr_xloc_update, double, getXlocIncrement)
UPDATE1 ( incr_yloc_update, double, getYlocIncrement)

UPDATE1 ( num_lines_update, long  , getNumLines)
UPDATE1 ( num_flags_update, long  , getNumFlags)
UPDATE1 (     angle_update, double, getAngle)


#define UPDATE2(last_update, long2, getFirst, getIncrement, getNumLines)  \
static long2 last_update(void *data)                                      \
{                                                                         \
  LineCreatePop *pop = (LineCreatePop*)data;                              \
  long2    first = pop->getFirst();                                       \
  long2    incr  = pop->getIncrement();                                   \
  long num_lines = pop->getNumLines();                                    \
  return (first + (num_lines - 1) * incr);                                \
}

UPDATE2 (last_line_update, long  , getFirstLine, getLineIncrement, getNumLines)
UPDATE2 (last_shot_update, float , getFirstShot, getShotIncrement, getNumFlags)
UPDATE2 (last_xloc_update, double, getFirstXloc, getXlocIncrement, getNumFlags)
UPDATE2 (last_yloc_update, double, getFirstYloc, getYlocIncrement, getNumLines)




//----------------------- cancel trap ------------------------//
//----------------------- cancel trap ------------------------//
//----------------------- cancel trap ------------------------//

        // called by CANCEL pushbutton.

static void cancel_trap(void *data, long /*ident*/)
{
  LineCreatePop *pop = (LineCreatePop*)data;
  pop->unmanage();
}



//----------------------- ok trap ----------------------------//
//----------------------- ok trap ----------------------------//
//----------------------- ok trap ----------------------------//

        // called by OK and APPLY pushbuttons.
        // ident == 1 means  ok   button.
        // ident == 2 means apply button.

static void ok_trap(void *data, long ident)
{
  LineCreatePop *pop = (LineCreatePop*)data;
  FieldGeometry *fg  = pop->getFieldGeometry();
  if(!fg->allowModifyingLdCards()) return;
  pop->createSeismicLines();
  if(ident == 1) pop->unmanage();
}



//------------------ ok sense upfun ---------------------------//
//------------------ ok sense upfun ---------------------------//
//------------------ ok sense upfun ---------------------------//

        // used by OK and APPLY pushbuttons.

static long ok_sense_upfun(void *data)
{
  LineCreatePop *pop = (LineCreatePop*)data;
  FieldGeometry *fg  = pop->getFieldGeometry();
  if(!fg->allowModifyingLdCards()) return FALSE;
  return TRUE;
}



//---------------------- create seismic lines -------------------//
//---------------------- create seismic lines -------------------//
//---------------------- create seismic lines -------------------//


void LineCreatePop::createSeismicLines()
{
  assert(_fg->allowModifyingLdCards());
  int chaining = _fg->getChaining();
  if(chaining != NO_CHAINING)
      {
      if(!_fg->allowChangeChaining()) _fg->resumeDependentUpdates();
      assert(_fg->allowChangeChaining());
      _fg->setChaining(NO_CHAINING);
      }

  _fg->freezeDependentUpdates();
  _fg->showMessage("creating seismic lines...");
  _fg->preMultipleOperations();

  double sina  = sin(RADIANS_PER_DEGREE * _angle);
  double cosa  = cos(RADIANS_PER_DEGREE * _angle);
  double xinc_cosa = _xloc_increment * cosa;
  double yinc_sina = _yloc_increment * sina;
  double yinc_cosa = _yloc_increment * cosa;
  double xinc_sina = _xloc_increment * sina;

  for(long iy = 0; iy < _num_lines; iy++)
      {
      long line = _first_line + iy * _line_increment;
      long ixl  = _fg->findMatchingLineNumber(line);
      if(ixl != -1) continue;
      ixl =_fg->appendNewLine();
      if(ixl == -1) break;
      _fg->setLineNumber(ixl, line);
      for(long ix = 0; ix < _num_flags; ix++)
          {
          long ixf =_fg->appendNewFlagToLine(ixl);
          if(ixf == -1) break;
          if(ix <= 1)
              {
              float  shot = _first_shot + ix * _shot_increment;
              double xloc = _first_xloc + ix * xinc_cosa - iy * yinc_sina;
              double yloc = _first_yloc + ix * xinc_sina + iy * yinc_cosa;
              _fg->setShotpoint(ixl, ixf, shot);
              _fg->setXloc     (ixl, ixf, xloc);
              _fg->setYloc     (ixl, ixf, yloc);
              }
          }
      }

  _fg->postMultipleOperations();
  _fg->resumeDependentUpdates();
  _fg->setChaining(chaining);
  _fg->showMessage("finished creating seismic lines");
}





//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


LineCreatePop::LineCreatePop(SLDelay *slparent, char *name, HelpCtx hctx,
                                               FieldGeometry *fg)
            : SLDialog(slparent, name, hctx, TRUE),
                   _fg              (fg),
                   _first_line      (0),
                   _first_shot      (0.0),
                   _first_xloc      (0.0),
                   _first_yloc      (0.0),
                   _line_increment  (1),
                   _shot_increment  (1.0),
                   _xloc_increment  (100.0),
                   _yloc_increment  (100.0),
                   _num_lines       (1),
                   _num_flags       (1),
                   _angle           (0.0)
{
  assert(_fg);

  setTitle("Create Seismic Lines");

  SLSmartForm *work = workArea();
  
/////// create text objects:

  SLpText *a0 = new SLpText (work, "a0", 0, SLpText::_CHAR  , NCHAR);
  SLpText *a1 = new SLpText (work, "a1", 0, SLpText::_CHAR  , NCHAR);
  SLpText *a2 = new SLpText (work, "a2", 0, SLpText::_CHAR  , NCHAR);
  SLpText *a3 = new SLpText (work, "a3", 0, SLpText::_CHAR  , NCHAR);
  SLpText *a4 = new SLpText (work, "a4", 0, SLpText::_CHAR  , NCHAR);

  SLpText *b0 = new SLpText (work, "b0", 0, SLpText::_CHAR  , NCHAR);
  SLpText *b1 = new SLpText (work, "b1", 0, SLpText::_LONG  , NCHAR);
  SLpText *b2 = new SLpText (work, "b2", 0, SLpText::_LONG  , NCHAR);
  SLpText *b3 = new SLpText (work, "b3", 0, SLpText::_LONG  , NCHAR);
  SLpText *b4 = new SLpText (work, "b4", 0, SLpText::_LONG  , NCHAR);

  SLpText *c0 = new SLpText (work, "c0", 0, SLpText::_CHAR  , NCHAR);
  SLpText *c1 = new SLpText (work, "c1", 0, SLpText::_FLOAT , NCHAR);
  SLpText *c2 = new SLpText (work, "c2", 0, SLpText::_FLOAT , NCHAR);
  SLpText *c3 = new SLpText (work, "c3", 0, SLpText::_FLOAT , NCHAR);
  SLpText *c4 = new SLpText (work, "c4", 0, SLpText::_LONG  , NCHAR);

  SLpText *d0 = new SLpText (work, "d0", 0, SLpText::_CHAR  , NCHAR);
  SLpText *d1 = new SLpText (work, "d1", 0, SLpText::_DOUBLE, NCHAR);
  SLpText *d2 = new SLpText (work, "d2", 0, SLpText::_DOUBLE, NCHAR);
  SLpText *d3 = new SLpText (work, "d3", 0, SLpText::_DOUBLE, NCHAR);
  SLpText *d4 = new SLpText (work, "d4", 0, SLpText::_LONG  , NCHAR);

  SLpText *e0 = new SLpText (work, "e0", 0, SLpText::_CHAR  , NCHAR);
  SLpText *e1 = new SLpText (work, "e1", 0, SLpText::_DOUBLE, NCHAR);
  SLpText *e2 = new SLpText (work, "e2", 0, SLpText::_DOUBLE, NCHAR);
  SLpText *e3 = new SLpText (work, "e3", 0, SLpText::_DOUBLE, NCHAR);
  SLpText *e4 = new SLpText (work, "e4", 0, SLpText::_LONG  , NCHAR);

  SLpText *f0 = new SLpText (work, "f0", 0, SLpText::_CHAR  , NCHAR);
  SLpText *f1 = new SLpText (work, "f1", 0, SLpText::_DOUBLE, NCHAR);

/////// set labels:

  a0->showLabelAppearance();
  a1->showLabelAppearance();
  a2->showLabelAppearance();
  a3->showLabelAppearance();
  a4->showLabelAppearance();

  a0->setupCvarValue(" ");
  a1->setupCvarValue("first");
  a2->setupCvarValue("increment");
  a3->setupCvarValue("last");
  a4->setupCvarValue("how many");

  b0->showLabelAppearance();
  c0->showLabelAppearance();
  d0->showLabelAppearance();
  e0->showLabelAppearance();
  f0->showLabelAppearance();

  b0->setupCvarValue("line number:");
  c0->setupCvarValue("  shotpoint:");
  d0->setupCvarValue("       xloc:");
  e0->setupCvarValue("       yloc:");
  f0->setupCvarValue("      angle:");

/////// register traps:

  b1->setItrap(first_line_trap, this);
  b2->setItrap( incr_line_trap, this);
  b3->setItrap( last_line_trap, this);
  b4->setItrap( num_lines_trap, this);

  c1->setFtrap(first_shot_trap, this);
  c2->setFtrap( incr_shot_trap, this);
  c3->setFtrap( last_shot_trap, this);
  c4->setItrap( num_flags_trap, this);

  d1->setDtrap(first_xloc_trap, this);
  d2->setDtrap( incr_xloc_trap, this);
  d3->setDtrap( last_xloc_trap, this);
  d4->setItrap( num_flags_trap, this);

  e1->setDtrap(first_yloc_trap, this);
  e2->setDtrap( incr_yloc_trap, this);
  e3->setDtrap( last_yloc_trap, this);
  e4->setItrap( num_lines_trap, this);

  f1->setDtrap(     angle_trap, this);

/////// register update functions:

  b1->setupIvarFun(first_line_update, this);
  b2->setupIvarFun( incr_line_update, this);
  b3->setupIvarFun( last_line_update, this);
  b4->setupIvarFun( num_lines_update, this);

  c1->setupFvarFun(first_shot_update, this);
  c2->setupFvarFun( incr_shot_update, this);
  c3->setupFvarFun( last_shot_update, this);
  c4->setupIvarFun( num_flags_update, this);

  d1->setupDvarFun(first_xloc_update, this);
  d2->setupDvarFun( incr_xloc_update, this);
  d3->setupDvarFun( last_xloc_update, this);
  d4->setupIvarFun( num_flags_update, this);

  e1->setupDvarFun(first_yloc_update, this);
  e2->setupDvarFun( incr_yloc_update, this);
  e3->setupDvarFun( last_yloc_update, this);
  e4->setupIvarFun( num_lines_update, this);

  f1->setupDvarFun(     angle_update, this);

/////// attachments:

                //  left  right  top  bottom

  work->attach (a0, work, NULL, work, NULL);
  work->attach (a1, a0  , NULL, work, NULL);
  work->attach (a2, a1  , NULL, work, NULL);
  work->attach (a3, a2  , NULL, work, NULL);
  work->attach (a4, a3  , work, work, NULL);

  work->attach (b0, work, NULL, a1  , NULL);
  work->attach (b1, a0  , NULL, a1  , NULL);
  work->attach (b2, a1  , NULL, a1  , NULL);
  work->attach (b3, a2  , NULL, a1  , NULL);
  work->attach (b4, a3  , work, a1  , NULL);

  work->attach (c0, work, NULL, b1  , NULL);
  work->attach (c1, a0  , NULL, b1  , NULL);
  work->attach (c2, a1  , NULL, b1  , NULL);
  work->attach (c3, a2  , NULL, b1  , NULL);
  work->attach (c4, a3  , work, b1  , NULL);

  work->attach (d0, work, NULL, c1  , NULL);
  work->attach (d1, a0  , NULL, c1  , NULL);
  work->attach (d2, a1  , NULL, c1  , NULL);
  work->attach (d3, a2  , NULL, c1  , NULL);
  work->attach (d4, a3  , work, c1  , NULL);

  work->attach (e0, work, NULL, d1  , NULL);
  work->attach (e1, a0  , NULL, d1  , NULL);
  work->attach (e2, a1  , NULL, d1  , NULL);
  work->attach (e3, a2  , NULL, d1  , NULL);
  work->attach (e4, a3  , work, d1  , NULL);

  work->attach (f0, work, NULL, e1  , NULL);
  work->attach (f1, a0  , NULL, e1  , work);

/////// bottom buttons:

  SLpPush *ok    = addBottomOK     (1, ok_trap    , this);
  SLpPush *apply = addBottomApply  (2, ok_trap    , this);
                   addBottomCancel (0, cancel_trap, this);
                   addBottomHelp   ("CREATE_LINES");

  ok   ->setupSenseFun(ok_sense_upfun, this);
  apply->setupSenseFun(ok_sense_upfun, this);
 
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


LineCreatePop::~LineCreatePop()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
