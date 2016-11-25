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

//------------------------ ld_operation_gui.hh ----------------------------//
//------------------------ ld_operation_gui.hh ----------------------------//
//------------------------ ld_operation_gui.hh ----------------------------//

//              header file for the LdOperationGui class
//                  derived from the SLSmartForm class
//                         subdirectory fggui

             // This class is used to choose an operation
             // to be performed on seismic lines.

#ifndef _LD_OPERATION_GUI_HH_
#define _LD_OPERATION_GUI_HH_

#include "sl/sl_smart_form.hh"
#include <X11/Intrinsic.h>


class LdOperationGui : public SLSmartForm
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

public:

enum { DELETE_SHOTPOINTS = 1,  CLEAR_SELECTED_FLAGS,
       INSERT_BETWEEN,    INSERT_BEFORE,       INSERT_AFTER,
       FILL_MISSING,      ADD_CARDS_BEGINNING, ADD_CARDS_END,
       REVERSE_DIRECTION, ADD_SHOTS_BEGINNING, ADD_SHOTS_END };

private:

  class FieldGeometry *_fg; // pointer to field geometry.

  long   _operation;      // which operation to perform (enum).

  float  _delete_sp1;     // first shotpoint to delete.
  float  _delete_sp2;     // last  shotpoint to delete.
  long   _insert_between; // number of cards to insert between cards.
  long   _insert_before;  // number of cards to insert before _sp_before.
  long   _insert_after;   // number of cards to insert after  _sp_after.
  float  _sp_before;      // shotpoint to insert cards before.
  float  _sp_after;       // shotpoint to insert cards after.
  float  _sp_interval;    // shotpoint interval for filling in missing cards.
  long   _ncards1;        // #cards to add at beginning;
  long   _ncards2;        // #cards to add at end;
  float  _sp1;            // shotpoint to start  with at beginning;
  float  _sp2;            // shotpoint to finish with at end;
  float  _sp1_interval;   // shotpoint interval to start  with at beginning;
  float  _sp2_interval;   // shotpoint interval to finish with at end;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:

  LdOperationGui(SLDelay *slparent, char *name, HelpCtx hctx,
                                       class FieldGeometry *fg);
  virtual ~LdOperationGui();

  FieldGeometry *getFieldGeometry ()    const  { return _fg; }

  long   getOperation     ()    const  { return _operation; }

  float  getDeleteSp1     ()    const  { return _delete_sp1; }
  float  getDeleteSp2     ()    const  { return _delete_sp2; }
  long   getInsertBetween ()    const  { return _insert_between; }
  long   getInsertBefore  ()    const  { return _insert_before; }
  long   getInsertAfter   ()    const  { return _insert_after; }
  float  getSpBefore      ()    const  { return _sp_before; }
  float  getSpAfter       ()    const  { return _sp_after; }
  float  getSpInterval    ()    const  { return _sp_interval; }
  long   getNcards1       ()    const  { return _ncards1; }
  long   getNcards2       ()    const  { return _ncards2; }
  float  getSp1           ()    const  { return _sp1; }
  float  getSp2           ()    const  { return _sp2; }
  float  getSp1Interval   ()    const  { return _sp1_interval; }
  float  getSp2Interval   ()    const  { return _sp2_interval; }

  void   setDeleteSp1     (float v)  {              _delete_sp1     = v; }
  void   setDeleteSp2     (float v)  {              _delete_sp2     = v; }
  void   setInsertBetween (long  v)  { if(v >    0) _insert_between = v; }
  void   setInsertBefore  (long  v)  { if(v >    0) _insert_before  = v; }
  void   setInsertAfter   (long  v)  { if(v >    0) _insert_after   = v; }
  void   setSpBefore      (float v)  {              _sp_before      = v; }
  void   setSpAfter       (float v)  {              _sp_after       = v; }
  void   setSpInterval    (float v)  { if(v != 0.0) _sp_interval    = v; }
  void   setNcards1       (long  v)  { if(v >    0) _ncards1        = v; }
  void   setNcards2       (long  v)  { if(v >    0) _ncards2        = v; }
  void   setSp1           (float v)  {              _sp1            = v; }
  void   setSp2           (float v)  {              _sp2            = v; }
  void   setSp1Interval   (float v)  { if(v != 0.0) _sp1_interval   = v; }
  void   setSp2Interval   (float v)  { if(v != 0.0) _sp2_interval   = v; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
