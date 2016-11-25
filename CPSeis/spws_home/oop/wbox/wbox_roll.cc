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

//---------------------- wbox_roll.cc -------------------------//
//---------------------- wbox_roll.cc -------------------------//
//---------------------- wbox_roll.cc -------------------------//

//          implementation file for the WboxRoll class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_roll.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_field.hh"
#include "wbox/wbox_link.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>




//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxRoll::WboxRoll(WboxBox *box)
         :
            _box           (box),
            _ksave         (1),
            _kscalar       (0)
{
  assert(box);
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxRoll::~WboxRoll()
{
}



//------------------------ get active field pointer ------------//
//------------------------ get active field pointer ------------//
//------------------------ get active field pointer ------------//

         // public.

WboxField *WboxRoll::getActiveFieldPointer()  const
{
  return _box->getFieldPointer(_ksave);
}



//-------------------- roll with arrows --------------------------//
//-------------------- roll with arrows --------------------------//
//-------------------- roll with arrows --------------------------//

    // private.
    // act on keys LEFT,RIGHT,UP,DOWN for choosing new _ksave.
    // makes sure the new _ksave does not violate switch value.
    // leaves _ksave unchanged if there is nowhere to go.
    // stays within same tab group.
    // 7/7/92 changed 99 to 188.
    // 8/6/92 changed 188 back to 99 for up and down.

void WboxRoll::rollWithArrows(const char *endkey)
{
  int        kmin   = _box->getKmin();
  int        kmax   = _box->getKmax();
  WboxField *active = _box->getFieldPointer(_ksave);
  int        krow   = active->getIrow();
  int        kcol   = active->getIcol();
  int        ktab   = active->getItab();
  int        idist  = 99999;
  for(int ifield = kmin; ifield <= kmax; ifield++)
      {
      WboxField *field = _box->getFieldPointer(ifield);
      int         irow = field->getIrow();
      int         icol = field->getIcol();
      int         svar = field->getEffectiveSwitch();
      int         itab = field->getItab();
      int        jdist = 32000;
      int        delta = AbsoluteValue(kcol - icol);
      if(itab != ktab || svar <= 0) continue;
      if(!strcmp(endkey, "UP"))
          {
          if     (irow < krow) jdist = 99 * (krow - irow) + delta;
          else if(irow > krow) jdist = 99 * (199  - irow) + delta;
          }
      else if(!strcmp(endkey, "DOWN"))
          {
          if     (irow > krow) jdist = 99 * (irow - krow) + delta;
          else if(irow < krow) jdist = 99 * (irow +  100) + delta;
          }
      else if(!strcmp(endkey, "LEFT"))
          {
          if(irow == krow && icol < kcol)
                               jdist = kcol - icol;
          else if(irow < krow) jdist = 188 * (krow - irow) - icol;
          else if(irow > krow) jdist = 188 * (199  - irow) - icol;
          else                 jdist = 188 * (399   - icol);
          }
      else if(!strcmp(endkey, "RIGHT"))
          {
          if(irow == krow && icol > kcol)
                               jdist = icol - kcol;
          else if(irow > krow) jdist = 188 * (irow - krow) + icol;
          else if(irow < krow) jdist = 188 * (irow +   100) + icol;
          else                 jdist = 188 * (300   + icol);
          }
      else if(!strcmp(endkey, "^UP") || !strcmp(endkey, "@UP"))
          {
          jdist = 99 * irow + delta;
          }
      else if(!strcmp(endkey, "^DOWN") || !strcmp(endkey, "@DOWN"))
          {
          jdist = 99 * (199 - irow) + delta;
          }
      else if(!strcmp(endkey, "^LEFT") || !strcmp(endkey, "@LEFT"))
          {
          if(irow == krow) jdist = icol;
          }
      else if(!strcmp(endkey, "^RIGHT") || !strcmp(endkey, "@RIGHT"))
          {
          if(irow == krow) jdist = 199 - icol;
          }
      else
          {
          assert(FALSE);
          }
      if(jdist < idist)
          {
          idist = jdist;
          _ksave = ifield;
          }
      }
}



//------------------- enforce switch value ----------------------//
//------------------- enforce switch value ----------------------//
//------------------- enforce switch value ----------------------//

  // private.
  // enforce switch value in case other smarter methods are not invoked.
  // changes _ksave if necessary so that its switch is on.
  // if the current switch is on, or all switches are off,
  //                                        _ksave is not changed.
  // tries to stay within the same tab group if possible.

void WboxRoll::enforceSwitchValue()
{
  int        kmin   = _box->getKmin();
  int        kmax   = _box->getKmax();
  WboxField *active = _box->getFieldPointer(_ksave);
  int        krow   = active->getIrow();
  int        kcol   = active->getIcol();
  int        ksvar  = active->getEffectiveSwitch();
  int        ktab   = active->getItab();
  if(ksvar > 0) return;
  int iflag = FALSE;
  while(TRUE)
     {
     int keep = 0;
     int idist = 99999;
     for(int ifield = kmin; ifield <= kmax; ifield++)
         {
         WboxField *field = _box->getFieldPointer(ifield);
         int         jrow = field->getIrow();
         int         jcol = field->getIcol();
         int         svar = field->getEffectiveSwitch();
         int         jtab = field->getItab();
         if(svar > 0 && (jtab == ktab || iflag == TRUE))
             {
             int jdist = AbsoluteValue(kcol - jcol)
                  + 99 * AbsoluteValue(krow - jrow);
             if(jdist < idist)
                 {
                 idist = jdist;
                 keep = ifield;
                 }
             }
         }
     if(keep != 0)
         {
         _ksave = keep;
         return;
         }
     else if(iflag == FALSE)
         {
         iflag = TRUE;
         }
     else
         {
         return;
         }
     }
}



//----------------------- roll with tab -----------------------------//
//----------------------- roll with tab -----------------------------//
//----------------------- roll with tab -----------------------------//

    // private.
    // changes _ksave to a different tab group.
    // should be called only if endkey is JUMP or $JUMP.

void WboxRoll::rollWithTab(const char *endkey)
{
  int         kmin  = _box->getKmin();
  int         kmax  = _box->getKmax();
  WboxField *active = _box->getFieldPointer(_ksave);
  int         ktab  = active->getItab();
  int         keep  = -1;
  for(int ifield = kmin; ifield <= kmax; ifield++)
      {
      int j = ifield;
      if(!strcmp(endkey, "$JUMP")) j = kmax + kmin - ifield;
      WboxField *field = _box->getFieldPointer(j);
      int        jsvar = field->getEffectiveSwitch();
      int        jtab  = field->getItab();
      if(jsvar > 0 && jtab != ktab)
          {
          if( (!strcmp(endkey, "JUMP" ) && jtab > ktab) ||
              (!strcmp(endkey, "$JUMP") && jtab < ktab && jtab > 0) )
                  {
                  WboxLink *link = _box->getLinkPointer(jtab);
                  _ksave = link->getKarray();
                  if(_ksave == 0) _ksave = j;
                  return;
                  }
          else if(keep == -1 || jtab <= 0)
                  {
                  keep = j;
                  }
          }
      }
  if(keep == -1) return;
  WboxField *field2 = _box->getFieldPointer(keep);
  ktab = field2->getItab();
  if(ktab <= 0)
      {
      _ksave = _kscalar;
      }
  else
      {
      WboxLink *link = _box->getLinkPointer(ktab);
      _ksave = link->getKarray();
      }
  if(_ksave == 0) _ksave = keep;
}





//------------- remember active datafield --------------------//
//------------- remember active datafield --------------------//
//------------- remember active datafield --------------------//

    // private.
    // remembers the current active datafield for its tab group.
    // for scalar datafields, sets _kscalar.
    // for array datafields, sets value in the appropriate linked array set.

void WboxRoll::rememberActiveDatafield()
{
  WboxField *active = _box->getFieldPointer(_ksave);
  WboxLink  *link   = active->getLinkPointer();
  if(link)
      {
      link->setKarray(_ksave);
      }
  else
      {
      _kscalar = _ksave;
      }
}




//--------------------- roll for arrays --------------------------//
//--------------------- roll for arrays --------------------------//
//--------------------- roll for arrays --------------------------//

  // private.
  // for arrays only (itab>0).
  // act on keys LEFT,RIGHT,UP,DOWN for choosing new _ksave (and maybe _ifirst).
  // act on keys ^UP,^DOWN,@UP,@DOWN for choosing new index.
  // stays within same tab group.
  // ksave_keep = old active datafield (before calling rollWithArrows).
  // _ksave     = new active datafield (calculated by rollWithArrows).

void WboxRoll::rollForArrays(const char *endkey, int ksave_keep)
{
  WboxField *active_keep = _box->getFieldPointer(ksave_keep);
  WboxField *active      = _box->getFieldPointer(_ksave);
  WboxLink  *link_keep   = active_keep->getLinkPointer();
  int        irow_keep   = active_keep->getIrow();
  int        icol_keep   = active_keep->getIcol();
  int        irow        = active     ->getIrow();
  int        icol        = active     ->getIcol();
  int        index_keep  = active_keep->getIndex();
  int        n_keep      = active_keep->getN    ();
  int        nmax_keep   = active_keep->getNmax ();
  int        last        = MinimumValue(n_keep + 1, nmax_keep);
  assert(link_keep);
  if(!strcmp(endkey, "UP") && irow_keep <= irow)
      {
      _ksave = ksave_keep;
      if(index_keep > 1)
          {
          link_keep->setIfirst(link_keep->getIfirst() - 1);
          }
      }
  else if(!strcmp(endkey, "DOWN") && irow_keep >= irow)
      {
      _ksave = ksave_keep;
      if(index_keep < last)
          {
          link_keep->setIfirst(link_keep->getIfirst() + 1);
          }
      }
  else if(!strcmp(endkey, "LEFT") && icol_keep <= icol
                                  && irow_keep <= irow)
      {
      if(index_keep == 1)
          {
          _ksave = ksave_keep;
          }
      if(index_keep < last)
          {
          _ksave += irow_keep - irow;
          link_keep->setIfirst(link_keep->getIfirst() - 1);
          }
      }
  else if(!strcmp(endkey, "RIGHT") && icol_keep >= icol
                                   && irow_keep >= irow)
      {
      if(index_keep == last)
          {
          _ksave = ksave_keep;
          }
      if(index_keep < last)
          {
          _ksave += irow_keep - irow;
          link_keep->setIfirst(link_keep->getIfirst() + 1);
          }
      }
  else if(!strcmp(endkey, "^UP"))
      {
      rollToNewIndex(index_keep - link_keep->getNarow());
      }
  else if(!strcmp(endkey, "^DOWN"))
      {
      rollToNewIndex(index_keep + link_keep->getNarow());
      }
  else if(!strcmp(endkey, "@UP"))
      {
      rollToNewIndex(1);
      }
  else if(!strcmp(endkey, "@DOWN"))
      {
      rollToNewIndex(last);
      }
}



//------------------------ roll to match -----------------------------//
//------------------------ roll to match -----------------------------//
//------------------------ roll to match -----------------------------//

  // private.
  // index and/or _ksave might get changed.
  // _ksave remains in the same array.
  // returns TRUE if index and/or _ksave is changed.
  // returns FALSE otherwise.

int WboxRoll::rollToMatch(const char *endkey, int index)
{
  int    ivar, ival;
  float  fvar, fval;
  double dvar, dval;
  char   cvar[200], cval[200];
  WboxField  *active = _box->getFieldPointer(_ksave);
  int n = active->getN();
  if(n <= 1)
      {
      _box->maybeShowMessage("nothing to look for");
      return FALSE;
      }
  active->getUserValue(index, &ival, &fval, &dval, cval);
  int index2 = 0;

///////////////// look for next matching value:

  if(!strcmp(endkey, "^M") || !strcmp(endkey, "^V"))
      {
      for(int i = 1; i <= n - 1; i++)
          {
          int i2 = i + index;
          if(i2 > n) i2 -= n;
          active->getUserValue(i2, &ivar, &fvar, &dvar, cvar);
          if     (active->isCvar()) { if(!strcmp(cvar, cval)) index2 = i2; }
          else if(active->isIvar()) { if(ivar == ival)        index2 = i2; }
          else if(active->isFvar()) { if(fvar == fval)        index2 = i2; }
          else if(active->isDvar()) { if(dvar == dval)        index2 = i2; }
          if(index2 == i2)
              {
              if(!strcmp(endkey, "^V"))
                  {
                  _box->maybeShowMessage
                     ("a value which matches another has been found");
                  rollToNewIndex(index);
                  }
              else
                  {
                  _box->maybeShowMessage("next matching value found");
                  rollToNewIndex(index2);
                  }
              return TRUE;
              }
          }
      if(!strcmp(endkey, "^V"))
          {
          return FALSE;
          }
      else
          {
          _box->maybeShowMessage("no other values match this one");
          }
      }

///////////////// look for next non-matching value:

  else if(!strcmp(endkey, "^N"))
      {
      for(int i = 1; i <= n - 1; i++)
          {
          int i2 = i + index;
          if(i2 > n) i2 -= n;
          active->getUserValue(i2, &ivar, &fvar, &dvar, cvar);
          if     (active->isCvar()) { if(strcmp(cvar, cval)) index2 = i2; }
          else if(active->isIvar()) { if(ivar != ival)       index2 = i2; }
          else if(active->isFvar()) { if(fvar != fval)       index2 = i2; }
          else if(active->isDvar()) { if(dvar != dval)       index2 = i2; }
          if(index2 == i2)
              {
              _box->maybeShowMessage("next non-matching value found");
              rollToNewIndex(index2);
              return TRUE;
              }
          }
      _box->maybeShowMessage("all values match");
      }

  _box->getAllboxPointer()->ringBell();
  return FALSE;
}




//---------------------- roll to new index -------------------------//
//---------------------- roll to new index -------------------------//
//---------------------- roll to new index -------------------------//

  // private.
  // the active datafield must first be set to be within the desired array.
  // nothing happens if the active datafield is a scalar.
  // first adjusts the specified index, if necessary, to make it valid.
  // then scrolls the linked arrays containing the active datafield, if
  //   necessary, to make the specified index visible.
  // then moves the active datafield to the specified index.
  // _ksave remains in the same array.

void WboxRoll::rollToNewIndex(int index)
{
  WboxField *active = _box->getFieldPointer(_ksave);
  WboxLink  *link   = active->getLinkPointer();
  if(link)
      {
      index = link->makeIndexVisible(index);
      _ksave += index - active->getIndex() ;
                       // guaranteed not to change ident.
      }
}



//------------------- goto new datafield --------------------------//
//------------------- goto new datafield --------------------------//
//------------------- goto new datafield --------------------------//

  // public.
  // finds new datafield number _ksave.
  // something happens only if endkey is one of these values:
  //     UP  DOWN  RIGHT  LEFT  BUTTON  JUMP  $JUMP
  //    ^UP ^DOWN ^RIGHT ^LEFT  ^M  ^N  ^V
  //    @UP @DOWN @RIGHT @LEFT  CONFIGUR
  // but in addition, the switch value is enforced, and the active
  //   datafield for the active tab group is remembered.

void WboxRoll::gotoNewDatafield(const char *endkey, int irow, int icol)
{
  WboxField  *active = _box->getFieldPointer(_ksave);
  WboxLink   *link   = active->getLinkPointer();

  if(link && !strcmp(endkey, "CONFIGUR"))
      {
      int index = active->getIndex();
      rollToNewIndex(index);
      }

  else if(!strcmp(endkey, "UP"    ) || !strcmp(endkey, "DOWN" ) ||
          !strcmp(endkey, "RIGHT" ) || !strcmp(endkey, "LEFT" ) ||
          !strcmp(endkey, "^UP"   ) || !strcmp(endkey, "^DOWN") ||
          !strcmp(endkey, "^RIGHT") || !strcmp(endkey, "^LEFT") ||
          !strcmp(endkey, "@UP"   ) || !strcmp(endkey, "@DOWN") ||
          !strcmp(endkey, "@RIGHT") || !strcmp(endkey, "@LEFT"))
      {
      int ksave_keep = _ksave;
      rollWithArrows(endkey);
      if(link)
          {
          rollForArrays(endkey, ksave_keep);
          }
      }

  else if(!strcmp(endkey, "BUTTON"))
      {
      int    min_switch = 1;
      WboxField *field = _box->findFieldPointer(irow, icol, min_switch);
      if(field) _ksave = field->getIfield();
      }

  else if(!strcmp(endkey, "JUMP") || !strcmp(endkey, "$JUMP"))
      {
      rollWithTab(endkey);
      }

  else if(link && (!strcmp(endkey, "^M") || !strcmp(endkey, "^N")))
      {
      int index = active->getIndex();
      rollToMatch(endkey, index);
      }

  else if(link && !strcmp(endkey, "^V"))
      {
      int n = active->getN();
      if(n <= 1)
          {
          _box->maybeShowMessage("nothing to look for");
          }
      else if(n > 200)
          {
          _box->maybeShowMessage("too many values to look through");
          }
      else
          {
          int index = active->getIndex();
          int index_changed = FALSE;
          for(int j = 1; j <= n; j++)
              {
              int index2 = j + index;
              if(index2 > n) index2 -= n;
              assert(index2 >= 1 && index2 <= n);
              index_changed = rollToMatch(endkey, index2);
              if(index_changed)
                  {
                  break;
                  }
              }
          if(!index_changed)
              {
              _box->maybeShowMessage
                          ("there are no values which match any other");
              _box->getAllboxPointer()->ringBell();
              }
          }
      }
  enforceSwitchValue();
  rememberActiveDatafield();
}



//------------------- goto specified datafield -------------------//
//------------------- goto specified datafield -------------------//
//------------------- goto specified datafield -------------------//

  // public.
  // finds new _ksave corresponding to ident and index.
  // does not change _ksave if ident is not found.
  // if scalar datafield is found:
  //     index is irrelevant.
  // if array datafield is found:
  //     if index is out-of-range, moves it within range.
  //     linked array set is scrolled to make index visible.

void WboxRoll::gotoSpecifiedDatafield(int ident, int index)
{
  int       kmin = _box->getKmin();
  int       kmax = _box->getKmax();
  for(int ifield = kmin; ifield <= kmax; ifield++)
      {
      WboxField *field = _box->getFieldPointer(ifield);
      WboxLink  *link  = field->getLinkPointer();
      if(ident == field->getIdent())
          {
          _ksave = ifield;
          if(link)
              {
              rollToNewIndex(index);
                  // changes _ksave, which is already in the correct array.
              }
          break;
          }
      }
  enforceSwitchValue();
  rememberActiveDatafield();
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

