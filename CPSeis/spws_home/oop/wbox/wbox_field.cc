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

//---------------------- wbox_field.cc -------------------------//
//---------------------- wbox_field.cc -------------------------//
//---------------------- wbox_field.cc -------------------------//

//          implementation file for the WboxField class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_field.hh"
#include "wbox/wbox_link.hh"
#include "wbox/wbox_vector.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_screen.hh"
#include "wbox/wbox_allbox.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>



#define NBUF  (UCHAR_MAX + 1)   // size of temporary character storage.


//----------------------- get values -------------------------//
//----------------------- get values -------------------------//
//----------------------- get values -------------------------//


#define PASSTHRU(int2, getIdent)      \
                                      \
int2 WboxField::getIdent() const      \
{                                     \
  return _vector->getIdent();         \
}


PASSTHRU (int      , getIdent        )
PASSTHRU (WboxBox* , getBoxPointer   )
PASSTHRU (WboxLink*, getLinkPointer  )
PASSTHRU (int      , getIcol         )
PASSTHRU (int      , getNchar        )
PASSTHRU (int      , getNdec         )
PASSTHRU (int      , getLength       )
PASSTHRU (int      , getItype        )
PASSTHRU (int      , getTraptype     )
PASSTHRU (int      , getUptype       )
PASSTHRU (int      , getSwtype       )
PASSTHRU (int      , isIvar          )
PASSTHRU (int      , isFvar          )
PASSTHRU (int      , isDvar          )
PASSTHRU (int      , isCvar          )
PASSTHRU (int      , isRadio         )
PASSTHRU (int      , isIndex         )
PASSTHRU (void*    , getUpdatePointer)
PASSTHRU (int      , getN            )
PASSTHRU (int      , getNmax         )
PASSTHRU (int      , getIbox         )
PASSTHRU (int      , getItab         )



int         WboxField::getIvar() const { return _var.getIvar(); }
float       WboxField::getFvar() const { return _var.getFvar(); }
double      WboxField::getDvar() const { return _var.getDvar(); }
const char* WboxField::getCvar() const { return _var.getCvar(); }
  


int
WboxField::getIfield()
const
{
  return _vector->getIfield(_irow);
}



int
WboxField::getIndex()
const
{
  return _vector->getIndex(_irow);
}



void
WboxField::getValue(int *ivar, float *fvar, double *dvar, char *cvar)
const
{
  _var.getValue(ivar, fvar, dvar, cvar);
}



//---------------- constructor -------------------------------//
//---------------- constructor -------------------------------//
//---------------- constructor -------------------------------//

WboxField::WboxField(WboxVector *vector, int irow)
         :
               _vector         (vector),
               _svar           (0),
               _irow           ((unsigned char)irow),
               _force          (TRUE)
{
  assert(irow >= 1 && irow <= 255 && irow <= UCHAR_MAX);

  switch(_vector->getItype())
     {
     case WboxVector::_IVAR:  _var.setIvar(); break;
     case WboxVector::_FVAR:  _var.setFvar(); break;
     case WboxVector::_DVAR:  _var.setDvar(); break;
     case WboxVector::_CVAR:  _var.setCvar(); break;
     default:                 assert(FALSE);
     }
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxField::~WboxField()
{
}




//-------------------- get user value ---------------------------//
//-------------------- get user value ---------------------------//
//-------------------- get user value ---------------------------//

   // previously getpoint_or_getfun_cvar.
   // the index argument need not necessarily match getIndex().

   // cvar is returned with length exactly _length,
   // including trailing blanks if necessary, followed
   // by a null-termination.

   // the string in user area, obtained with either a pointer
   // or an update function, may or may not be null-terminated,
   // and may be of any length.

void
WboxField::getUserCvar(int index, char *cvar)
const
{
  _vector->getUserCvar(index, cvar);
}



float
WboxField::getUserFvar(int index)
const
{
  return _vector->getUserFvar(index);
}



double
WboxField::getUserDvar(int index)
const
{
  return _vector->getUserDvar(index);
}



int
WboxField::getUserIvar(int index)
const
{
  return _vector->getUserIvar(index);
}



//------------------ decode value --------------------------//
//------------------ decode value --------------------------//
//------------------ decode value --------------------------//

     // previously cc2vv (copy textbuf to value).

int
WboxField::decodeValue(const char *textbuf,
          int *ivar, float *fvar, double *dvar, char *cvar)
const
{
  return _vector->decodeValue(textbuf, ivar, fvar, dvar, cvar);
}



//------------------ validate text --------------------------//
//------------------ validate text --------------------------//
//------------------ validate text --------------------------//


int
WboxField::validateText(const char *textbuf)
const
{
  return _vector->validateText(textbuf);
}



//---------------------- encode value ---------------------------//
//---------------------- encode value ---------------------------//
//---------------------- encode value ---------------------------//

       // previously vv2cc (copy local value to textbuf).

void
WboxField::encodeValue(char *textbuf)
const
{
  int         ivar =    0;
  float       fvar =  0.0;
  double      dvar =  0.0;
  const char *cvar = NULL;
  int        itype = _vector->getItype();
  if     (itype == WboxVector::_CVAR) cvar = getCvar();
  else if(itype == WboxVector::_DVAR) dvar = getDvar();
  else if(itype == WboxVector::_FVAR) fvar = getFvar();
  else                                ivar = getIvar();
  _vector->encodeValue(ivar, fvar, dvar, cvar, textbuf);
}



//---------------------- encode value ---------------------------//
//---------------------- encode value ---------------------------//
//---------------------- encode value ---------------------------//

       // previously vv2cc (copy specified value to textbuf).

void
WboxField::encodeValue
                   (int ivar, float fvar, double dvar, const char *cvar,
                    char *textbuf)
const
{
  _vector->encodeValue(ivar, fvar, dvar, cvar, textbuf);
}



//------------------ get effective switch -----------------------------//
//------------------ get effective switch -----------------------------//
//------------------ get effective switch -----------------------------//

   // if datafield is visible, returns user switch.
   // otherwise, returns -999 regardless of user switch.

int
WboxField::getEffectiveSwitch()
const
{
  if(_svar == -999) return _svar;
  return _vector->getUserSwitch(getIndex());
}



//------------------ get user switch -----------------------------//
//------------------ get user switch -----------------------------//
//------------------ get user switch -----------------------------//

   // previously getpoint_or_getfun_switch.
   // returns negative switch if index is out of range.

int
WboxField::getUserSwitch(int index)
const
{
  return _vector->getUserSwitch(index);
}



//------------------- get user value --------------------------//
//------------------- get user value --------------------------//
//------------------- get user value --------------------------//

//  previously pp2vv (copy pointee to value).
//  if index is outside range (1,n), value is set to nil or zero.

void
WboxField::getUserValue(int index,
             int *ivar, float *fvar, double *dvar, char *cvar)
const
{
  _vector->getUserValue(index, ivar, fvar, dvar, cvar);
}



//---------------------- put user value ---------------------------//
//---------------------- put user value ---------------------------//
//---------------------- put user value ---------------------------//

//   previously vv2pp (copy value to pointee).
//   copy local value to user area.

void
WboxField::putUserValue(int index)
const
{
  int n    = _vector->getN   ();
  int nmax = _vector->getNmax();
  if(index < 1 || index > n+1 || index > nmax ||
                     _vector->getUptype() != WboxVector::_POINT) return;
  int         ivar =    0;
  float       fvar =  0.0;
  double      dvar =  0.0;
  const char *cvar = NULL;
  int        itype = _vector->getItype();
  if     (itype == WboxVector::_CVAR) cvar = getCvar();
  else if(itype == WboxVector::_DVAR) dvar = getDvar();
  else if(itype == WboxVector::_FVAR) fvar = getFvar();
  else                                ivar = getIvar();
  _vector->putUserValue(index, ivar, fvar, dvar, cvar);
}



//---------------------- put user value ---------------------------//
//---------------------- put user value ---------------------------//
//---------------------- put user value ---------------------------//

//   previously vv2pp (copy value to pointee).
//   copy specified value to user area.
//   if index < 1, nothing is done.
//   if index > n+1 or nmax, nothing is done.
//   if utype is not _POINT, nothing is done.
//   for itype _CVAR:
//        (1) cvar may or may not be null-terminated, and
//               might be any length.
//        (2) exactly _length characters will be copied
//               to the user area, blank-filled if necessary, and
//               not null-terminated.
//  NOTE: copying to index n+1 is allowed if this does not exceed nmax.

void
WboxField::putUserValue(int index,
          int ivar, float fvar, double dvar, const char *cvar)
const
{
  _vector->putUserValue(index, ivar, fvar, dvar, cvar);
}



//---------------- decode and put user value -------------------//
//---------------- decode and put user value -------------------//
//---------------- decode and put user value -------------------//

     //  previously cc2pp (copy textbuf to pointee).

int
WboxField::decodeAndPutUserValue(int index, const char *textbuf)
const
{
  return _vector->decodeAndPutUserValue(index, textbuf);
}



//----------------- get and encode user value ------------------//
//----------------- get and encode user value ------------------//
//----------------- get and encode user value ------------------//

      //  previously pp2cc (copy pointee to textbuf).

void
WboxField::getAndEncodeUserValue(int index, char *textbuf)
const
{
  _vector->getAndEncodeUserValue(index, textbuf);
}



//--------------------- copy user value -------------------//
//--------------------- copy user value -------------------//
//--------------------- copy user value -------------------//

    // previously pp2pp (copy pointee to pointee).
    // copy user value from index1 to index2.
    // nothing is done if uptype is not _POINT.
    // nothing is done if index1 is outside range (1,n).
    // nothing is done if index2 is outside range (1,nmax or n+1).

void
WboxField::copyUserValue(int index1, int index2)
const
{
  _vector->copyUserValue(index1, index2);
}



//------------------ step user radio value --------------------//
//------------------ step user radio value --------------------//
//------------------ step user radio value --------------------//

      // previously step_radio_array_value.

void
WboxField::stepUserRadioValue(int index, const char *endkey)
const
{
  _vector->stepUserRadioValue(index, endkey);
}



//--------------------- compare ----------------------------//
//--------------------- compare ----------------------------//
//--------------------- compare ----------------------------//

         // compare value in user area with local copy.
         // then set local copy to value in user area.
         // return appropriate enum value.

//   compares user values (switch and variable) with local copies.
//   returns _NO_CHANGE      if neither the switch nor the variable changed.
//   returns _SWITCH_CHANGED if only the switch changed.
//   returns _VAR_CHANGED    if only the variable changed.
//   returns _BOTH_CHANGED   if both the variable and the switch changed.
//   also resets local copies to user values if changed.

WboxField::CHANGED
WboxField::compare()
{
  int index = getIndex();
  int changed_switch = FALSE;
  int changed_var    = FALSE;

  int svar = _vector->getUserSwitch(index);
  if(svar != _svar)
      {
      _svar = svar;
      changed_switch = TRUE;
      }

  int itype = _vector->getItype();
  if(itype == WboxVector::_CVAR)
      {
      char cvar[NBUF];
      _vector->getUserCvar(index, cvar);
      if(strcmp(cvar, _var.getCvar()))
          {
          _var.setCvar(cvar);
          changed_var = TRUE;
          }
      }
  else if(itype == WboxVector::_DVAR)
      {
      double dvar = _vector->getUserDvar(index);
      if(dvar != _var.getDvar())
          {
          _var.setDvar(dvar);
          changed_var = TRUE;
          }
      }
  else if(itype == WboxVector::_FVAR)
      {
      float fvar = _vector->getUserFvar(index);
      if(fvar != _var.getFvar())
          {
          _var.setFvar(fvar);
          changed_var = TRUE;
          }
      }
  else if(itype == WboxVector::_IVAR)
      {
      int ivar = _vector->getUserIvar(index);
      if(ivar != _var.getIvar())
          {
          _var.setIvar(ivar);
          changed_var = TRUE;
          }
      }
  else
      {
      assert(FALSE);
      }

  if(changed_switch == FALSE && changed_var == FALSE) return _NO_CHANGE;
  if(changed_switch == TRUE  && changed_var == FALSE) return _SWITCH_CHANGED;
  if(changed_switch == FALSE && changed_var == TRUE ) return _VAR_CHANGED;
  return _BOTH_CHANGED;
}



//--------------------- call trap ------------------------------//
//--------------------- call trap ------------------------------//
//--------------------- call trap ------------------------------//

         // call trap with local value.
         // endkey must be specified, and might get changed.
         // ident and index need not be specified, and will be
         //    set to the values for this or another datafield.

void
WboxField::callTrap(const char *textbuf, int nread, char *endkey,
                    int *ident, int *index)
const
{
  int         ivar       =   0;
  float       fvar       = 0.0;
  double      dvar       = 0.0;
  static char cvar[NBUF] = " ";         // needs to be static on the cray.
  int    itype      = _vector->getItype();
  if     (itype == WboxVector::_CVAR) strcpy(cvar,  getCvar());
  else if(itype == WboxVector::_DVAR)        dvar = getDvar();
  else if(itype == WboxVector::_FVAR)        fvar = getFvar();
  else                                       ivar = getIvar();
  _vector->callTrap(ivar, fvar, dvar, cvar,
           textbuf, nread, endkey, ident, index, _irow);
}



//--------------------- call trap ------------------------------//
//--------------------- call trap ------------------------------//
//--------------------- call trap ------------------------------//

         // call trap with specified value.
         // endkey must be specified, and might get changed.
         // ident and index need not be specified, and will be
         //    set to the values for this or another datafield.

    // if nread >= 1 and _uptype == _POINT, the value is put into
    //   the user area before the trap is called.
    // endkey can change with any trap.
    // ident and index are returned.
    // ident and index are preset to the values for this object.
    // ident and index can change with _OLDF,_SIMP,_EZED,_OLDC traps.

void
WboxField::callTrap(int ivar, float fvar, double dvar, const char *cvar,
                const char *textbuf, int nread, char *endkey,
                int *ident, int *index)
const
{
  _vector->callTrap(ivar, fvar, dvar, cvar,
           textbuf, nread, endkey, ident, index, _irow);
}



//---------------------- set values ------------------------------//
//---------------------- set values ------------------------------//
//---------------------- set values ------------------------------//


void WboxField::setSvar(int svar)
{
  _svar = svar;
}


void WboxField::setIrow(int irow)
{
  _irow = irow;
}



//----------------------------- erase field --------------------//
//----------------------------- erase field --------------------//
//----------------------------- erase field --------------------//

   // public.
   // erases datafield if it is not wanted by the user.
   // datafield will be erased if the user switch is -99 but the
   //   datafield was last drawn with a switch of a different value.
   // datafield with a switch value of -999 is ignored.  this was
   //   set by the reconfigure routine.
   // the datafield must be erased before updates are performed,
   //   in case another datafield will occupy same location.

void WboxField::eraseField()
{
  if(_svar != -999)
      {
      WboxBox    *box    = _vector->getBoxPointer();
      WboxScreen *screen = box->getScreenPointer();
      int index = getIndex();
      int suser = getUserSwitch(index);
      if(suser == -99 && _svar != -99)
                       screen->draw2(box, this, " ", -77);
      }
}



//------------------------ update field ---------------------------//
//------------------------ update field ---------------------------//
//------------------------ update field ---------------------------//

    // public.
    // irow,icol,irow2,icol2 are used only when endkey is EXPOSE.
    // if this datafield is not visible, the datafield is not updated
    //   and _NO_CHANGE is returned.
    // else if endkey is EXPOSE and this datafield falls in the exposed area,
    //   the datafield is always updated and _BOTH_CHANGED is returned.
    // else if this datafield is the active field, the datafield is always
    //   updated and _BOTH_CHANGED is returned.
    // else this datafield will be updated if a change occurred, and
    //   _NO_CHANGE, _VAR_CHANGED, or _BOTH_CHANGED is returned.

WboxField::CHANGED
WboxField::updateField(const char *endkey,
                          int irow, int icol, int irow2, int icol2)
{
  WboxBox    *box    = _vector->getBoxPointer();
  WboxScreen *screen = box->getScreenPointer();
  CHANGED    changed = _NO_CHANGE;

  if(_svar != -999)
      {
      changed = compare();
      if(!strcmp(endkey, "EXPOSE"))
          {
          if(_irow                                        >= irow  - 1 &&
             _irow                                        <= irow2 + 1 &&
             _vector->getIcol() + _vector->getNchar() - 1 >= icol  - 1 &&
             _vector->getIcol()                           <= icol2 + 1)
                            changed = _BOTH_CHANGED;
          }
      if(box->getActiveFieldPointer() == this) changed = _BOTH_CHANGED;
      if(changed != _NO_CHANGE)
          {
          char textbuf[200];
          encodeValue(textbuf);
          int draw_text_only = (changed == _VAR_CHANGED);
          screen->draw2(box, this, textbuf, _svar, draw_text_only);
          }
      }
  return changed;
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

