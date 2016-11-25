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

//---------------------- wbox_vector.cc -------------------------//
//---------------------- wbox_vector.cc -------------------------//
//---------------------- wbox_vector.cc -------------------------//

//          implementation file for the WboxVector class
//                  not derived from any class
//                       subdirectory wbox


  // this class maintains information about a variable (in user area)
  // which is displayed in one or more datafields.  this class also
  // maintains information which is common to all of the datafields
  // which display the variable.  all datafields which display this
  // variable have a pointer to the same instance of this class.

  // if the variable is a scalar, only one datafield is used.
  // if the variable is an array (a "vector"), a column of datafields
  //   is used.


#include "wbox/wbox_vector.hh"
#include "wbox/wbox_link.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_allbox.hh"
#include "str.h"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>



#define NBUF  (UCHAR_MAX + 1)   // size of temporary character storage.



//----------------------- get values -------------------------//
//----------------------- get values -------------------------//
//----------------------- get values -------------------------//


void*
WboxVector::getUpdatePointer()
const
{
  if(_uptype != _POINT) return NULL;
  return (void*)_update.ipoint;
}



int
WboxVector::getIfield(int irow)
const
{
  if(_link == NULL) return _ifield1;
  int iarow = _link->getIarow();
  assert(irow >= iarow);
  int ifield = _ifield1 + irow - iarow;
  return ifield;
}


int
WboxVector::getIndex(int irow)
const
{
  if(_link == NULL) return 1;
  return _link->getIndex(irow);
}


int
WboxVector::getN()
const
{
  if(_link == NULL) return 1;
  return _link->getN();
}


int
WboxVector::getNmax()
const
{
  if(_link == NULL) return 1;
  return _link->getNmax();
}


int
WboxVector::getIbox()
const
{
  return _box->getIbox();
}


int
WboxVector::getItab()
const
{
  if(_link == NULL) return 0;
  return _link->getItab();
}




//---------------- constructor -------------------------------//
//---------------- constructor -------------------------------//
//---------------- constructor -------------------------------//

     // link should be NULL for scalar datafields.

WboxVector::WboxVector(int ifield0, int ifield1, ITYPE itype, int ident,
                       WboxBox *box, WboxLink *link,
                       int icol, int nchar, int ndec, int length)
         :
               _ifield0        (ifield0),
               _ifield1        (ifield1),
               _itype          (itype),
               _traptype       (_NOTRAP),
               _uptype         (_NONE),
               _swtype         (_NONE),
               _box            (box),
               _link           (link),
  ////         _trap.ttrap     (NULL),      // doesn't work
  ////         _update.ipoint  (NULL),      // doesn't work
  ////         _sw.spoint      (NULL),      // doesn't work
               _ident          ((short)ident),
               _icol           ((unsigned char)icol),
               _nchar          ((unsigned char)nchar),
               _ndec           ((unsigned char)ndec),
               _length         ((unsigned char)length),
               _use_ipoint_int (FALSE),
               _use_spoint_int (FALSE)
{
  _trap.ttrap    = NULL;
  _update.ipoint = NULL;
  _sw.spoint     = NULL;

  assert(_itype == _IVAR || _itype == _FVAR ||
         _itype == _DVAR || _itype == _CVAR);

  assert(ident >= -32767   && ident <= 32767);
  assert(ident >= SHRT_MIN && ident <= SHRT_MAX);

  assert(_box);

  assert(icol   >= 1 && icol   <= 255 && icol   <= UCHAR_MAX);
  assert(nchar  >= 0 && nchar  <= 255 && nchar  <= UCHAR_MAX);
  assert(ndec   >= 0 && ndec   <= 255 && ndec   <= UCHAR_MAX);
  assert(length >= 0 && length <= 255 && length <= UCHAR_MAX);

  assert(length < NBUF && nchar < NBUF);

  if(itype == _CVAR)
      {
      assert(_length >= 1);
      if(_nchar <= 0) _nchar = length;
      }
  else
      {
      assert(_length == 0);
      if(_nchar <= 0) _nchar = 10;
      }

  if(itype != _FVAR && itype != _DVAR) { assert(_ndec == 0); }

/*
  if(_link) _link->addVectorPointer(this);
*/
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxVector::~WboxVector()
{
}





             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//
             //--------- registrations -------------//



    // the trap argument may be NULL.
    // the update-function or update-pointer argument may be NULL.

    // if the trap argument is NULL, or one of these functions
    // is not called, no trap will be called.

    // if both the update-function and update-pointer arguments are
    // NULL, the variable value will be 0 or blank (except for
    // uptype _INDEX, where the variable value is always index+1).

    // these functions can be called more than once, at any time
    // during the operation of the program.  some of these functions
    // will override other previously-called functions.



//--------------------- register traps ------------------------//
//--------------------- register traps ------------------------//
//--------------------- register traps ------------------------//

  // when calling any of these functions:
  //  (1) if the trap argument is NULL, any previously-registered
  //        trap is erased.
  //  (2) registerNoTrap is a convenience function equivalent to the
  //        other functions with a NULL argument.


void
WboxVector::registerNoTrap()
{
  _trap.ttrap = NULL;
  _traptype = _NOTRAP;
}



void
WboxVector::registerGenericTrap(WboxGenericTrap *trap, TRAPTYPE traptype)
{
  switch(traptype)
    {
    case _OLDF: registerFortranTrap   ((WboxFortranTrap*)trap); break;
    case _SIMP: registerSimpleTrap    ((WboxFortranTrap*)trap); break;
    case _EZED: registerEzedTrap      ((WboxEzedTrap   *)trap); break;
    case _OLDC: registerClanguageTrap ((WboxClangTrap  *)trap); break;
    default:    registerClanguageTrap ((WboxClangTrap  *)trap); break;
    }
}



void
WboxVector::registerFortranTrap(WboxFortranTrap *otrap)   // old-style trap.
{
  _trap.otrap = otrap;
  if(otrap) _traptype = _OLDF;
  else      _traptype = _NOTRAP;
}


void
WboxVector::registerSimpleTrap(WboxFortranTrap *otrap)   // old-style trap.
{
  _trap.otrap = otrap;
  if(otrap) _traptype = _SIMP;
  else      _traptype = _NOTRAP;
}


void
WboxVector::registerEzedTrap(WboxEzedTrap *etrap)   // old-style trap.
{
  _trap.etrap = etrap;
  if(etrap) _traptype = _EZED;
  else      _traptype = _NOTRAP;
}


void
WboxVector::registerClanguageTrap(WboxClangTrap *ttrap)   // old-style trap.
{
  _trap.ttrap = ttrap;
  if(ttrap) _traptype = _OLDC;
  else      _traptype = _NOTRAP;
}



void
WboxVector::registerIvarTrap(WboxIvarTrap *itrap)
{
  if(_itype != _IVAR) return;
  _trap.itrap = itrap;
  if(itrap) _traptype = _NEWC;
  else      _traptype = _NOTRAP;
}


void
WboxVector::registerFvarTrap(WboxFvarTrap *ftrap)
{
  if(_itype != _FVAR) return;
  _trap.ftrap = ftrap;
  if(ftrap) _traptype = _NEWC;
  else      _traptype = _NOTRAP;
}


void
WboxVector::registerDvarTrap(WboxDvarTrap *dtrap)
{
  if(_itype != _DVAR) return;
  _trap.dtrap = dtrap;
  if(dtrap) _traptype = _NEWC;
  else      _traptype = _NOTRAP;
}


void
WboxVector::registerCvarTrap(WboxCvarTrap *ctrap)
{
  if(_itype != _CVAR) return;
  _trap.ctrap = ctrap;
  if(ctrap) _traptype = _NEWC;
  else      _traptype = _NOTRAP;
}



//---------------------- register pointers -------------------------//
//---------------------- register pointers -------------------------//
//---------------------- register pointers -------------------------//


void
WboxVector::registerSwitchPointInt(int *spoint_int)
{
  _sw.spoint_int = spoint_int;
  if(spoint_int) _swtype = _POINT;
  else           _swtype = _NONE;
  _use_spoint_int = TRUE;
}


void
WboxVector::registerSwitchPoint(long *spoint)
{
  _sw.spoint = spoint;
  if(spoint) _swtype = _POINT;
  else       _swtype = _NONE;
  _use_spoint_int = FALSE;
}


void
WboxVector::registerIndexBehavior()
{
  if(_itype != _IVAR) return;
  _update.ipoint = NULL;
  _uptype = _INDEX;
}


void
WboxVector::registerRadioPointInt(int *ipoint_int)
{
  if(_itype != _IVAR) return;
  _update.ipoint_int = ipoint_int;
  if(ipoint_int) _uptype = _RPOINT;
  else           _uptype = _NONE;
  _use_ipoint_int = TRUE;
}


void
WboxVector::registerRadioPoint(long *ipoint)
{
  if(_itype != _IVAR) return;
  _update.ipoint = ipoint;
  if(ipoint) _uptype = _RPOINT;
  else       _uptype = _NONE;
  _use_ipoint_int = FALSE;
}


void
WboxVector::registerIvarPointInt(int *ipoint_int)
{
  if(_itype != _IVAR) return;
  _update.ipoint_int = ipoint_int;
  if(ipoint_int) _uptype = _POINT;
  else           _uptype = _NONE;
  _use_ipoint_int = TRUE;
}


void
WboxVector::registerIvarPoint(long *ipoint)
{
  if(_itype != _IVAR) return;
  _update.ipoint = ipoint;
  if(ipoint) _uptype = _POINT;
  else       _uptype = _NONE;
  _use_ipoint_int = FALSE;
}


void
WboxVector::registerFvarPoint(float *fpoint)
{
  if(_itype != _FVAR) return;
  _update.fpoint = fpoint;
  if(fpoint) _uptype = _POINT;
  else       _uptype = _NONE;
}


void
WboxVector::registerDvarPoint(double *dpoint)
{
  if(_itype != _DVAR) return;
  _update.dpoint = dpoint;
  if(dpoint) _uptype = _POINT;
  else       _uptype = _NONE;
}


void
WboxVector::registerCvarPoint(char *cpoint)
{
  if(_itype != _CVAR) return;
  _update.cpoint = cpoint;
  if(cpoint) _uptype = _POINT;
  else       _uptype = _NONE;
}



//----------------------- register update functions ----------------//
//----------------------- register update functions ----------------//
//----------------------- register update functions ----------------//


void
WboxVector::registerSwitchFun(WboxIupdateFun *sfun)
{
  _sw.sfun = sfun;
  if(sfun) _swtype = _FUN;
  else     _swtype = _NONE;
}


void
WboxVector::registerIvarFun(WboxIupdateFun *ifun)
{
  if(_itype != _IVAR) return;
  _update.ifun = ifun;
  if(ifun) _uptype = _FUN;
  else     _uptype = _NONE;
}


void
WboxVector::registerFvarFun(WboxFupdateFun *ffun)
{
  if(_itype != _FVAR) return;
  _update.ffun = ffun;
  if(ffun) _uptype = _FUN;
  else     _uptype = _NONE;
}


void
WboxVector::registerDvarFun(WboxDupdateFun *dfun)
{
  if(_itype != _DVAR) return;
  _update.dfun = dfun;
  if(dfun) _uptype = _FUN;
  else     _uptype = _NONE;
}


void
WboxVector::registerCvarFun(WboxCupdateFun *cfun)
{
  if(_itype != _CVAR) return;
  _update.cfun = cfun;
  if(cfun) _uptype = _FUN;
  else     _uptype = _NONE;
}



//------------------ copy no null or yes null --------------------//
//------------------ copy no null or yes null --------------------//
//------------------ copy no null or yes null --------------------//

  // private.
  // copy character string from cvar1 to cvar2.
  // return length excluding trailing blanks.
  // cvar1 may or may not be null-terminated and may be any length.
  // cvar2 will be exactly the specified length, with trailing blanks
  //     appended if necessary.
  // if cvar1 is NULL, it is treated as if filled with blanks.
  // with copyNoNull, cvar2 will NOT be null-terminated.
  // with copyYesNull, cvar2 will be null-terminated.
  // when copying to a text buffer which will be drawn, length should
  //   be set to _nchar.  otherwise, length should be set to _length.


int
WboxVector::copyYesNull
                 (char *cvar2, const char *cvar1, int length)
const
{
  int len = copyNoNull(cvar2, cvar1, length);
  cvar2[length] = '\0';
  return len;
}


int
WboxVector::copyNoNull
                 (char *cvar2, const char *cvar1, int length)
const
{
  assert(cvar2 && length >= 1);
  if(cvar1 == NULL)
      {
      memset(cvar2, ' ', length);
      return 0;
      }
  int ok_to_copy = TRUE;
  int len = 0;
  for(int i = 0; i < length; i++)
      {
      if(ok_to_copy)
          {
          if(cvar1[i] == '\0') ok_to_copy = FALSE;
          }
      if(ok_to_copy)
          {
          cvar2[i] = cvar1[i];
          if(cvar2[i] != ' ') len = i + 1;
          }
      else
          {
          cvar2[i] = ' ';
          }
      }
  return len;
}



//-------------- get user cvar ----------------------------//
//-------------- get user cvar ----------------------------//
//-------------- get user cvar ----------------------------//

   // previously getpoint_or_getfun_cvar.
   // the index argument need not necessarily match getIndex().

   // cvar is returned with length exactly _length,
   // including trailing blanks if necessary, followed
   // by a null-termination.

   // the string in user area, obtained with either a pointer
   // or an update function, may or may not be null-terminated,
   // and may be of any length.

void
WboxVector::getUserCvar(int index, char *cvar)
const
{
  assert(_itype == _CVAR);
  int n = getN();
  char *point;
  if(index < 1 || index > n)
      {
      point = NULL;
      }
  else if(_uptype == _POINT)
      {
      point = _update.cpoint + (index - 1) * (_length);
      }
  else if(_uptype == _FUN)
      {
      point = _update.cfun(_box->getUserData(), _ident, index - 1);
      }
  else
      {
      point = NULL;
      }
  copyYesNull(cvar, point, _length);
}



//------------------ get user fvar -----------------------------//
//------------------ get user fvar -----------------------------//
//------------------ get user fvar -----------------------------//

   // previously getpoint_or_getfun_fvar.
   // the index argument need not necessarily match getIndex().

float
WboxVector::getUserFvar(int index)
const
{
  assert(_itype == _FVAR);
  int n = getN();
  float fvar;
  if(index < 1 || index > n)
      {
      fvar = FNIL;
      }
  else if(_uptype == _POINT)
      {
      fvar = *(_update.fpoint + index - 1);
      }
  else if(_uptype == _FUN)
      {
      fvar = _update.ffun(_box->getUserData(), _ident, index - 1);
      }
  else
      {
      fvar = 0.0;
      }
  return fvar;
}



//------------------ get user dvar -----------------------------//
//------------------ get user dvar -----------------------------//
//------------------ get user dvar -----------------------------//

   // previously getpoint_or_getfun_dvar.
   // the index argument need not necessarily match getIndex().

double
WboxVector::getUserDvar(int index)
const
{
  assert(_itype == _DVAR);
  int n = getN();
  double dvar;
  if(index < 1 || index > n)
      {
      dvar = DNIL;
      }
  else if(_uptype == _POINT)
      {
      dvar = *(_update.dpoint + index - 1);
      }
  else if(_uptype == _FUN)
      {
      dvar = _update.dfun(_box->getUserData(), _ident, index - 1);
      }
  else
      {
      dvar = 0.0;
      }
  return dvar;
}



//------------------ get user ivar -----------------------------//
//------------------ get user ivar -----------------------------//
//------------------ get user ivar -----------------------------//

   // previously getpoint_or_getfun_ivar.
   // the index argument need not necessarily match getIndex().

   // NOTE: copying from index n+1 is allowed for toggle and radio
   //   buttons (for uptype == _POINT or _RPOINT only) if this does
   //   not exceed nmax.  purpose is to display toggle or radio button
   //   in either depressed or non-depressed position for index n+1.

int
WboxVector::getUserIvar(int index)
const
{
  assert(_itype == _IVAR);
  int n = getN();
  int ivar;

  if(index == n + 1 && (_uptype == _POINT || _uptype == _RPOINT))
      {
      int svar = getUserSwitch(index);
      int nmax = getNmax();
      if(index <= nmax && (svar == 3 || svar == -3 || svar == 4 || svar == -4))
          {
          n++;            // lie about n to get value at n+1 location.
          }
      }

  if(index < 1 || index > n)
      {
      ivar = INIL;
      }
  else if(_uptype == _POINT && _use_ipoint_int)
      {
      ivar = (int)*(_update.ipoint_int + index - 1);
      }
  else if(_uptype == _POINT)
      {
      ivar = (int)*(_update.ipoint + index - 1);
      }
  else if(_uptype == _RPOINT && _use_ipoint_int)
      {
      if(*_update.ipoint_int == index) ivar = 1;
      else                             ivar = 0;
      }
  else if(_uptype == _RPOINT)
      {
      if(*_update.ipoint == index) ivar = 1;
      else                         ivar = 0;
      }
  else if(_uptype == _FUN)
      {
      ivar = (int)_update.ifun(_box->getUserData(), _ident, index - 1);
      }
  else if(_uptype == _INDEX)
      {
      ivar = index;
      }
  else
      {
      ivar = 0;
      }
  return ivar;
}



//------------------ decode value --------------------------//
//------------------ decode value --------------------------//
//------------------ decode value --------------------------//

//  previously cc2vv (copy textbuf to value).
//  argument pointer can be NULL if it does not correspond to the type.
//  returns istat >=  1 if value is good.
//  returns istat ==  0 if value is nil.
//  returns istat <= -1 if value is error.

int
WboxVector::decodeValue(const char *textbuf,
          int *ivar, float *fvar, double *dvar, char *cvar)
const
{
                   if(ivar) *ivar    =    0;
                   if(fvar) *fvar    =  0.0;
                   if(dvar) *dvar    =  0.0;
                   if(cvar)  cvar[0] = '\0';
  int istat;
  if(_itype == _CVAR)
      {
      int len = copyYesNull(cvar, textbuf, _length);
      if(len == 0) istat = 0;
      else         istat = 1;
      }
  else if(_itype == _DVAR)
      {
      str_ss2dd((char*)textbuf,   dvar, &istat);
      }
  else if(_itype == _FVAR)
      {
      str_ss2ff((char*)textbuf,   fvar, &istat);
      }
  else
      {
      str_ss2ii((char*)textbuf,   ivar, &istat);
      }
  return istat;
}



//------------------ validate text --------------------------//
//------------------ validate text --------------------------//
//------------------ validate text --------------------------//

//  returns istat >=  1 if value is good.
//  returns istat ==  0 if value is nil.
//  returns istat <= -1 if value is error.

int
WboxVector::validateText(const char *textbuf)
const
{
  int    ivar;
  float  fvar;
  double dvar;
  char   cvar[NBUF];
  return decodeValue(textbuf,  &ivar, &fvar, &dvar, cvar);
}



//---------------------- encode value ---------------------------//
//---------------------- encode value ---------------------------//
//---------------------- encode value ---------------------------//

       // previously vv2cc (copy specified value to textbuf).

void
WboxVector::encodeValue
                   (int ivar, float fvar, double dvar, const char *cvar,
                    char *textbuf)
const
{
  char buffer[NBUF];
  int nchar2 = (int)_nchar;
  int ndec2  = (int)_ndec;
  if(_itype == _CVAR)
      {
      copyYesNull(textbuf, cvar, _nchar);
      }
  else if(_itype == _DVAR)
      {
      str_dd2ss  (dvar, buffer, nchar2, ndec2);
      copyYesNull(textbuf, buffer, _nchar);
      }
  else if(_itype == _FVAR)
      {
      str_ff2ss  (fvar, buffer, nchar2, ndec2);
      copyYesNull(textbuf, buffer, _nchar);
      }
  else if(_itype == _IVAR)                      // new 2001-08-08
      {                                         // new 2001-08-08
      str_ii2ss  (ivar, buffer, nchar2);        // new 2001-08-08
      copyYesNull(textbuf, buffer, _nchar);     // new 2001-08-08
      }                                         // new 2001-08-08
  else
      {
      assert(FALSE);
      }
}



//------------------ get user switch -----------------------------//
//------------------ get user switch -----------------------------//
//------------------ get user switch -----------------------------//

   // previously getpoint_or_getfun_switch.
   // returns negative switch if index is out of range.

int
WboxVector::getUserSwitch(int index)
const
{
  int svar;
  if(_swtype == _POINT && _use_spoint_int)
      {
      assert(_sw.spoint_int);
      svar = (int)*_sw.spoint_int;
      }
  else if(_swtype == _POINT)
      {
      assert(_sw.spoint);
      svar = (int)*_sw.spoint;
      }
  else if(_swtype == _FUN)
      {
      assert(_sw.sfun);
      svar = (int)_sw.sfun(_box->getUserData(), _ident, index - 1);
      }
  else
      {
      svar = 0;
      }
  if(svar > 0)
      {
      int n    = getN   ();
      int nmax = getNmax();
      if(index < 1 || index > n + 1 || index > nmax) svar = -svar;
      }
  return svar;
}



//------------------- get user value --------------------------//
//------------------- get user value --------------------------//
//------------------- get user value --------------------------//

//  previously pp2vv (copy pointee to value).
//  if index is outside range (1,n), value is set to nil or zero.
//  argument pointer can be NULL if it does not correspond to the type.

//  NOTE: copying from index n+1 is allowed for toggle and radio
//    buttons (for uptype == _POINT only) if this does not exceed
//    nmax.

void
WboxVector::getUserValue(int index,
             int *ivar, float *fvar, double *dvar, char *cvar)
const
{
                   if(ivar) *ivar    =    0;
                   if(fvar) *fvar    =  0.0;
                   if(dvar) *dvar    =  0.0;
                   if(cvar)  cvar[0] = '\0';
  switch(_itype)
      {
      case _CVAR     :         getUserCvar  (index, cvar); break;
      case _DVAR     : *dvar = getUserDvar  (index);       break;
      case _FVAR     : *fvar = getUserFvar  (index);       break;
      case _IVAR     : *ivar = getUserIvar  (index);       break;
      default        : assert(FALSE);
      }
}



//---------------------- put user value ---------------------------//
//---------------------- put user value ---------------------------//
//---------------------- put user value ---------------------------//

//   previously vv2pp (copy value to pointee).
//   copy specified value to user area.
//   if index < 1, nothing is done.
//   if index > n+1 or nmax, nothing is done.
//   if uptype is not _POINT, nothing is done.
//   for itype _CVAR:
//        (1) cvar may or may not be null-terminated, and
//               might be any length.
//        (2) exactly _length characters will be copied
//               to the user area, blank-filled if necessary, and
//               not null-terminated.
//  NOTE: copying to index n+1 is allowed if this does not exceed nmax.

void
WboxVector::putUserValue(int index,
          int ivar, float fvar, double dvar, const char *cvar)
const
{
  int n    = getN   ();
  int nmax = getNmax();
  if(index < 1 || index > n+1 || index > nmax) return;
  if(_uptype != _POINT && _uptype != _RPOINT) return;
  if(_itype == _CVAR)
      {
      char *point = _update.cpoint + (index - 1) * _length;
      copyNoNull(point, cvar, _length);
      }
  else if(_itype == _DVAR)
      {
      *(_update.dpoint + index - 1) = dvar;
      }
  else if(_itype == _FVAR)
      {
      *(_update.fpoint + index - 1) = fvar;
      }
  else if(_itype == _IVAR && _uptype == _POINT && _use_ipoint_int == FALSE)
      {
      *(_update.ipoint + index - 1) = (long)ivar;
      }
  else if(_itype == _IVAR && _uptype == _POINT && _use_ipoint_int == TRUE)
      {
      *(_update.ipoint_int + index - 1) = (int)ivar;
      }
  else if(_itype == _IVAR && _uptype == _RPOINT && _use_ipoint_int == FALSE)
      {
      if(ivar > 0) *_update.ipoint = (long)index;
      }
  else if(_itype == _IVAR && _uptype == _RPOINT && _use_ipoint_int == TRUE)
      {
      if(ivar > 0) *_update.ipoint_int = (int)index;
      }
}



//---------------- decode and put user value -------------------//
//---------------- decode and put user value -------------------//
//---------------- decode and put user value -------------------//

//  previously cc2pp (copy textbuf to pointee).
//  returns istat >=  1 if value is good.
//  returns istat ==  0 if value is nil.
//  returns istat <= -1 if value is error.

int
WboxVector::decodeAndPutUserValue(int index, const char *textbuf)
const
{
  int    ivar;
  float  fvar;
  double dvar;
  char   cvar[NBUF];
  int    istat = decodeValue (textbuf,  &ivar, &fvar, &dvar, cvar);
  putUserValue               (index,     ivar,  fvar,  dvar, cvar);
  return istat;
}



//----------------- get and encode user value ------------------//
//----------------- get and encode user value ------------------//
//----------------- get and encode user value ------------------//

      //  previously pp2cc (copy pointee to textbuf).

void
WboxVector::getAndEncodeUserValue(int index, char *textbuf)
const
{
  int    ivar;
  float  fvar;
  double dvar;
  char   cvar[NBUF];
  getUserValue (index, &ivar, &fvar, &dvar, cvar);
  encodeValue          (ivar,  fvar,  dvar, cvar,   textbuf);
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
WboxVector::copyUserValue(int index1, int index2)
const
{
  int n    = getN   ();
  int nmax = getNmax();
  if (_uptype != _POINT) return;
  if (index1 < 1 || index1 > n) return;
  if (index2 < 1 || index2 > nmax || index2 > n + 1) return;
  int    ivar;
  float  fvar;
  double dvar;
  char   cvar[NBUF];
  getUserValue(index1,   &ivar, &fvar, &dvar, cvar);
  putUserValue(index2,    ivar,  fvar,  dvar, cvar);
}



//------------------ step user radio value --------------------//
//------------------ step user radio value --------------------//
//------------------ step user radio value --------------------//


void
WboxVector::stepUserRadioValue(int index, const char *endkey)
const
{
  if(_itype != _IVAR) return;
  if(_uptype != _RPOINT) return;
  int ivar;
  if(_use_ipoint_int) ivar = (int)*_update.ipoint_int;
  else                ivar = (int)*_update.ipoint;
  if     (!strcmp(endkey, "REMOVE") && ivar >  index) ivar--;
  else if(!strcmp(endkey, "INSERT") && ivar >= index) ivar++;
  else if(!strcmp(endkey, "MAYBE" ) && ivar >  index) ivar++;
  else return;
  if(_use_ipoint_int) *_update.ipoint_int = (int)ivar;
  else                *_update.ipoint     = (long)ivar;
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
WboxVector::callTrap(int ivar, float fvar, double dvar, const char *cvar,
                const char *textbuf, int nread, char *endkey,
                int *ident, int *index, int irow)
const
{
  char textbuf2[200];
  strcpy(textbuf2, textbuf);
  *ident = _ident;
  *index = getIndex(irow);

  int debug = _box->getAllboxPointer()->getDebug();
  if(debug >= 2)
      {
      char buffer[8];
      if     (_traptype == _OLDF) strcpy(buffer, "OLDF");
      else if(_traptype == _SIMP) strcpy(buffer, "SIMP");
      else if(_traptype == _EZED) strcpy(buffer, "EZED");
      else if(_traptype == _OLDC) strcpy(buffer, "OLDC");
      else if(_traptype == _NEWC) strcpy(buffer, "NEWC");
      else                        strcpy(buffer, "NOTRAP");
      printf(
   "    %2d %-10s %-8s trap=%-6s ident=%-4d index=%-5d nread=%-3d\n",
         _box->getIbox(), _box->getBoxName(), endkey, buffer,
         _ident, *index, nread);
      }

  if(nread >= 1 && (_uptype == _POINT || _uptype == _RPOINT))
           putUserValue(*index, ivar, fvar, dvar, cvar);

  if(_traptype == _OLDF || _traptype == _SIMP || _traptype == _EZED)
      {
      int                     ibox    = _box->getIbox();
      WboxAllbox             *allbox  = _box->getAllboxPointer();
      WboxAllbox::WboxFortranTrapHandler
                             *handler = allbox->fortranTrapHandler();
      assert(handler);
      handler((WboxGenericTrap*)_trap.ttrap, _traptype,
                  ibox, ident, index,   textbuf2, nread, endkey);
      }

  else if(_traptype == _OLDC)
      {
      long ident2 = *ident;
      long index2 = *index;
      long nread2 = nread;
      _trap.ttrap(_box, &ident2, &index2,   textbuf2, &nread2, endkey);
                        //// the above is a c-language trap.
      *ident = (int)ident2;
      *index = (int)index2;
      }

  else if(_traptype == _NEWC)
      {
      void *data = _box->getUserData();
      if(_itype == _CVAR)
         {
         char cvar2[200];
         strcpy(cvar2, cvar);
         _trap.ctrap(data, _ident, *index - 1, cvar2, nread, endkey);
         }
      else if(_itype == _FVAR)
         {
         _trap.ftrap(data, _ident, *index - 1, fvar, nread, endkey);
         }
      else if(_itype == _DVAR)
         {
         _trap.dtrap(data, _ident, *index - 1, dvar, nread, endkey);
         }
      else
         {
         _trap.itrap(data, _ident, *index - 1, ivar, nread, endkey);
         }
      }
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

