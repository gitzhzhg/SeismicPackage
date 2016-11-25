
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
//---------------------- ld_operation_gui.cc -----------------------//
//---------------------- ld_operation_gui.cc -----------------------//
//---------------------- ld_operation_gui.cc -----------------------//

//          implementation file for the LdOperationGui class
//                 derived from the SLSmartForm class
//                       subdirectory fggui

             // This class is used to choose an operation
             // to be performed on seismic lines.


#include "fggui/ld_operation_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/radio_list.hh"
#include "sl/slp_push.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_radio.hh"
#include "sl/sl_sep.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <assert.h>



//------------------ sense update functions -----------------------//
//------------------ sense update functions -----------------------//
//------------------ sense update functions -----------------------//


/*
static long delete_sense_upfun(void *data)
{
  LdOperationGui *THIS = (LdOperationGui*)data;
  FieldGeometry  *fg   = THIS->getFieldGeometry();
  int sense = fg->allowModifyingLdCards();
  THIS->setSensitivity(sense);   // sensitivity of entire object.
  if(!fg->allowDeletingData()) return FALSE;
  if(fg->numLines   () == 0) return FALSE;
  if(fg->totNumFlags() == 0) return FALSE;
  return TRUE;
}
         ////// with this routine, sometimes the text objects
         ////// go insensitive and remain so indefinitely.
*/


static long delete_sense_upfun1(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  int sense = fg->allowModifyingLdCards();
  if(!sense) return FALSE;
  if(!fg->allowDeletingData()) return FALSE;
  if(fg->numLines   () == 0) return FALSE;
  if(fg->totNumFlags() == 0) return FALSE;
  return TRUE;
}


static long delete_sense_upfun2(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->allowModifyingLdCards();
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


LdOperationGui::LdOperationGui(SLDelay *slparent, char *name, HelpCtx hctx,
                                               FieldGeometry *fg)
            : SLSmartForm(slparent, name, hctx, TRUE),
                   _fg              (fg),
                   _operation       (ADD_CARDS_BEGINNING),
                   _delete_sp1      (FNIL),
                   _delete_sp2      (FNIL),
                   _insert_between  (0),
                   _insert_before   (0),
                   _insert_after    (0),
                   _sp_before       (FNIL),
                   _sp_after        (FNIL),
                   _sp_interval     (1.0),
                   _ncards1         (0),
                   _ncards2         (0),
                   _sp1             (FNIL),
                   _sp2             (FNIL),
                   _sp1_interval    (1.0),
                   _sp2_interval    (1.0)
{
  assert(_fg);

  RadioList *r = new RadioList();
  SLSep  *sep1 = new SLSep(this, "sep");
  SLSep  *sep2 = new SLSep(this, "sep");
  SLSep  *sep3 = new SLSep(this, "sep");
  SLSep  *sep4 = new SLSep(this, "sep");

  SLpRadio *r1  = r->addRadio(this, "r1" , DELETE_SHOTPOINTS   );
  SLpRadio *r2  = r->addRadio(this, "r2" , CLEAR_SELECTED_FLAGS);
  SLpRadio *r3  = r->addRadio(this, "r3" , INSERT_BETWEEN      );
  SLpRadio *r4  = r->addRadio(this, "r4" , INSERT_BEFORE       );
  SLpRadio *r5  = r->addRadio(this, "r5" , INSERT_AFTER        );
  SLpRadio *r6  = r->addRadio(this, "r6" , FILL_MISSING        );
  SLpRadio *r7  = r->addRadio(this, "r7" , ADD_CARDS_BEGINNING );
  SLpRadio *r8  = r->addRadio(this, "r8" , ADD_CARDS_END       );
  SLpRadio *r9  = r->addRadio(this, "r9" , REVERSE_DIRECTION   );
  SLpRadio *r10 = r->addRadio(this, "r10", ADD_SHOTS_BEGINNING );
  SLpRadio *r11 = r->addRadio(this, "r11", ADD_SHOTS_END       );

  r->setLabel(DELETE_SHOTPOINTS   , "delete shotpoints");
  r->setLabel(CLEAR_SELECTED_FLAGS, "clear selected flags");
  r->setLabel(INSERT_BETWEEN      , "insert");
  r->setLabel(INSERT_BEFORE       , "insert");
  r->setLabel(INSERT_AFTER        , "insert");
  r->setLabel(FILL_MISSING, "fill in missing cards using shotpoint interval");
  r->setLabel(ADD_CARDS_BEGINNING , "add");
  r->setLabel(ADD_CARDS_END       , "add");
  r->setLabel(REVERSE_DIRECTION   , "reverse direction of line");
  r->setLabel(ADD_SHOTS_BEGINNING , "add shotpoints at beginning");
  r->setLabel(ADD_SHOTS_END       , "add shotpoints at end");

  r->setupIvarPoint(&_operation);  // do not need trap or update function.

/*
  r->setupSenseFun(DELETE_SHOTPOINTS, delete_sense_upfun, this);
*/
  r->setupSenseFun(DELETE_SHOTPOINTS   , delete_sense_upfun1, fg);
  r->setupSenseFun(CLEAR_SELECTED_FLAGS, delete_sense_upfun2, fg);
  r->setupSenseFun(INSERT_BETWEEN      , delete_sense_upfun2, fg);
  r->setupSenseFun(INSERT_BEFORE       , delete_sense_upfun2, fg);
  r->setupSenseFun(INSERT_AFTER        , delete_sense_upfun2, fg);
  r->setupSenseFun(FILL_MISSING        , delete_sense_upfun2, fg);
  r->setupSenseFun(ADD_CARDS_BEGINNING , delete_sense_upfun2, fg);
  r->setupSenseFun(ADD_CARDS_END       , delete_sense_upfun2, fg);
  r->setupSenseFun(REVERSE_DIRECTION   , delete_sense_upfun2, fg);
  r->setupSenseFun(ADD_SHOTS_BEGINNING , delete_sense_upfun2, fg);
  r->setupSenseFun(ADD_SHOTS_END       , delete_sense_upfun2, fg);

  SLpText *d1 = new SLpText(this, "d1", 0, SLpText::_FLOAT, 7);
  SLpText *d2 = new SLpText(this, "d2", 0, SLpText::_FLOAT, 7);
  SLpText *i0 = new SLpText(this, "i0", 0, SLpText::_LONG , 5);
  SLpText *ib = new SLpText(this, "ib", 0, SLpText::_LONG , 5);
  SLpText *ia = new SLpText(this, "ia", 0, SLpText::_LONG , 5);
  SLpText *sb = new SLpText(this, "sb", 0, SLpText::_FLOAT, 7);
  SLpText *sa = new SLpText(this, "sa", 0, SLpText::_FLOAT, 7);
  SLpText *si = new SLpText(this, "si", 0, SLpText::_FLOAT, 7);
  SLpText *n1 = new SLpText(this, "n1", 0, SLpText::_LONG , 5);
  SLpText *n2 = new SLpText(this, "n2", 0, SLpText::_LONG , 5);
  SLpText *s1 = new SLpText(this, "s1", 0, SLpText::_FLOAT, 7);
  SLpText *s2 = new SLpText(this, "s2", 0, SLpText::_FLOAT, 7);
  SLpText *t1 = new SLpText(this, "t1", 0, SLpText::_FLOAT, 7);
  SLpText *t2 = new SLpText(this, "t2", 0, SLpText::_FLOAT, 7);

  d1->setupFvarPoint(&_delete_sp1);
  d2->setupFvarPoint(&_delete_sp2);
  i0->setupIvarPoint(&_insert_between);
  ib->setupIvarPoint(&_insert_before);
  ia->setupIvarPoint(&_insert_after);
  sb->setupFvarPoint(&_sp_before);
  sa->setupFvarPoint(&_sp_after);
  si->setupFvarPoint(&_sp_interval);
  n1->setupIvarPoint(&_ncards1);
  n2->setupIvarPoint(&_ncards2);
  s1->setupFvarPoint(&_sp1);
  s2->setupFvarPoint(&_sp2);
  t1->setupFvarPoint(&_sp1_interval);
  t2->setupFvarPoint(&_sp2_interval);

  d1->setupSenseFun(delete_sense_upfun1, fg);
  d2->setupSenseFun(delete_sense_upfun1, fg);
  i0->setupSenseFun(delete_sense_upfun2, fg);
  ib->setupSenseFun(delete_sense_upfun2, fg);
  ia->setupSenseFun(delete_sense_upfun2, fg);
  sb->setupSenseFun(delete_sense_upfun2, fg);
  sa->setupSenseFun(delete_sense_upfun2, fg);
  si->setupSenseFun(delete_sense_upfun2, fg);
  n1->setupSenseFun(delete_sense_upfun2, fg);
  n2->setupSenseFun(delete_sense_upfun2, fg);
  s1->setupSenseFun(delete_sense_upfun2, fg);
  s2->setupSenseFun(delete_sense_upfun2, fg);
  t1->setupSenseFun(delete_sense_upfun2, fg);
  t2->setupSenseFun(delete_sense_upfun2, fg);

  SLpLabel *r1a  = new SLpLabel(this, "r1a" , 0, "thru");
  SLpLabel *r3a  = new SLpLabel(this, "r3a" , 0,
                         "cards between each pair of present cards");
  SLpLabel *r4a  = new SLpLabel(this, "r4a" , 0, "cards before shotpoint");
  SLpLabel *r5a  = new SLpLabel(this, "r5a" , 0, "cards after shotpoint");
  SLpLabel *r7a  = new SLpLabel(this, "r7a" , 0, "cards at beginning");
  SLpLabel *r8a  = new SLpLabel(this, "r8a" , 0, "cards at end");
  SLpLabel *r10a = new SLpLabel(this, "r10a", 0, "starting with shotpoint");
  SLpLabel *r10b = new SLpLabel(this, "r10b", 0, "using shotpoint interval");
  SLpLabel *r11a = new SLpLabel(this, "r11a", 0, "finishing with shotpoint");
  SLpLabel *r11b = new SLpLabel(this, "r11b", 0, "using shotpoint interval");

  r1a ->setupSenseFun(delete_sense_upfun1, fg);
  r3a ->setupSenseFun(delete_sense_upfun2, fg);
  r4a ->setupSenseFun(delete_sense_upfun2, fg);
  r5a ->setupSenseFun(delete_sense_upfun2, fg);
  r7a ->setupSenseFun(delete_sense_upfun2, fg);
  r8a ->setupSenseFun(delete_sense_upfun2, fg);
  r10a->setupSenseFun(delete_sense_upfun2, fg);
  r10b->setupSenseFun(delete_sense_upfun2, fg);
  r11a->setupSenseFun(delete_sense_upfun2, fg);
  r11b->setupSenseFun(delete_sense_upfun2, fg);

    //  <r1> delete shotpoints [d1] thru [d2]                      r1a
    //  <r2> clear selected flags
    //  <r9> reverse direction of line
    //  --------------------------------------------------------------
    //  <r3> insert [i0] cards between each pair of present cards  r3a
    //  <r4> insert [ib] cards before shotpoint [sb]               r4a
    //  <r5> insert [ia] cards after shotpoint [sa]                r5a
    //  --------------------------------------------------------------
    //  <r6> fill in missing cards using shotpoint interval [si]
    //  --------------------------------------------------------------
    //  <r7> add [n1] cards at beginning                           r7a
    //  <r8> add [n2] cards at end                                 r8a
    //  --------------------------------------------------------------
    // <r10> add shotpoints at beginning
    //                             starting with shotpoint [s1]    r10a
    //                             using shotpoint interval [t1]   r10b
    // <r11> add shotpoints at end
    //                             finishing with shotpoint [s2]   r11a
    //                             using shotpoint interval [t2]   r11b

    //          LEFT  RIGHT   TOP   BOTTOM
  attach(r1  ,  this,  NULL,  this,  NULL,   0, 0, 5);
  attach(d1  ,  r1  ,  NULL,  this,  NULL,   0, 0, 5);
  attach(r1a ,  d1  ,  NULL,  this,  NULL,   0, 0, 5);
  attach(d2  ,  r1a ,  NULL,  this,  NULL,   0, 0, 5);

  attach(r2  ,  this,  NULL,  r1  ,  NULL,   0, 0, 5);

  attach(r9  ,  this,  NULL,  r2  ,  NULL,   0, 0, 5);

  attach(sep1,  this,  this,  r9  ,  NULL,   0, 0, 5);   // separator.

  attach(r3  ,  this,  NULL,  sep1,  NULL,   0, 0, 5);
  attach(i0  ,  r3  ,  NULL,  sep1,  NULL,   0, 0, 5);
  attach(r3a ,  i0  ,  this,  sep1,  NULL,   0, 5, 5);   // attached on right.

  attach(r4  ,  this,  NULL,  r3  ,  NULL,   0, 0, 5);
  attach(ib  ,  r4  ,  NULL,  r3  ,  NULL,   0, 0, 5);
  attach(r4a ,  ib  ,  NULL,  r3  ,  NULL,   0, 0, 5);
  attach(sb  ,  r4a ,  NULL,  r3  ,  NULL,   0, 0, 5);

  attach(r5  ,  this,  NULL,  r4  ,  NULL,   0, 0, 5);
  attach(ia  ,  r5  ,  NULL,  r4  ,  NULL,   0, 0, 5);
  attach(r5a ,  ia  ,  NULL,  r4  ,  NULL,   0, 0, 5);
  attach(sa  ,  r5a ,  NULL,  r4  ,  NULL,   0, 0, 5);

  attach(sep2,  this,  this,  r5  ,  NULL,   0, 0, 5);   // separator.

  attach(r6  ,  this,  NULL,  sep2,  NULL,   0, 0, 5);
  attach(si  ,  r6  ,  NULL,  sep2,  NULL,   0, 0, 5);

  attach(sep3,  this,  this,  r6  ,  NULL,   0, 0, 5);   // separator.

  attach(r7  ,  this,  NULL,  sep3,  NULL,   0, 0, 5);
  attach(n1  ,  r7  ,  NULL,  sep3,  NULL,   0, 0, 5);
  attach(r7a ,  n1  ,  NULL,  sep3,  NULL,   0, 0, 5);

  attach(r8  ,  this,  NULL,  r7  ,  NULL,   0, 0, 5);
  attach(n2  ,  r8  ,  NULL,  r7  ,  NULL,   0, 0, 5);
  attach(r8a ,  n2  ,  NULL,  r7  ,  NULL,   0, 0, 5);

  attach(sep4,  this,  this,  r8  ,  NULL,   0, 0, 5);   // separator.

  attach(r10 ,  this,  NULL,  sep4,  NULL,   0, 0, 5);
  attach(r10a,  this,  NULL,  r10 ,  NULL, 150, 0, 5);
  attach(s1  ,  r10a,  NULL,  r10 ,  NULL,   0, 0, 5);
  attach(r10b,  this,  NULL,  r10a,  NULL, 150, 0, 5);
  attach(t1  ,  r10b,  NULL,  r10a,  NULL,   0, 0, 5);

  attach(r11 ,  this,  NULL,  r10b,  NULL,   0, 0, 5);
  attach(r11a,  this,  NULL,  r11 ,  NULL, 150, 0, 5);
  attach(s2  ,  r11a,  NULL,  r11 ,  NULL,   0, 0, 5);
  attach(r11b,  this,  NULL,  r11a,  this, 150, 0, 5, 5);
  attach(t2  ,  r11b,  NULL,  r11a,  this,   0, 0, 5, 5);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


LdOperationGui::~LdOperationGui()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
