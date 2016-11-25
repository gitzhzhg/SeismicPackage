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

//---------------------- wbox_box.cc -------------------------//
//---------------------- wbox_box.cc -------------------------//
//---------------------- wbox_box.cc -------------------------//

//          implementation file for the WboxBox class
//                  not derived from any class
//                       subdirectory wbox


#include "wbox/wbox_box.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_screen.hh"
#include "wbox/wbox_control.hh"
#include "wbox/wbox_vector.hh"
#include "wbox/wbox_field.hh"
#include "wbox/wbox_link.hh"
#include "oprim/array_bucket.hh"
#include "named_constants.h"
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/PushB.h>
#include <Xm/DrawingA.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>



//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxBox::WboxBox(WboxAllbox *allbox, int ibox,
                 WboxGenericTrap *default_trap, int default_traptype,
                 int omit, int nrow_init)
         :
            _ibox                   (ibox),
            _default_trap           (default_trap),
            _default_traptype       (default_traptype),

            _w                      (NULL),
            _parent                 (NULL),
            _shellchild             (NULL),
            _shell                  (NULL),
            _wtiny                  (NULL),
            _userdata               (NULL),

            _allbox                 (allbox),
            _screen                 (NULL),
            _control                (NULL),
            _vector_list            (NULL),
            _field_list             (NULL),
            _link_list              (NULL),
            _link_being_created     (NULL),
            _field_being_created    (NULL),
            _vector_being_created   (NULL),

            _creating               (TRUE),
            _irnext                 (1),
            _icnext                 (1),
            _jcnext                 (1),
            _icnext9                (1),

            _vmin                   (1),
            _vmax                   (0),
            _kmin                   (1),
            _kmax                   (0),
            _lmin                   (1),
            _lmax                   (0),
            _focusflag              (0),
            _managedflag            (-1),
            _nrow                   (0),
            _ncol                   (0),
            _starting_nrow          (0),
            _starting_ncol          (0),
            _omit                   (omit),
            _nrow_init              (nrow_init)
{
  assert(allbox);
  assert(ibox >= 1);
  strcpy(_boxname, " ");

  _field_list  = new ArrayBucket (50);
  _vector_list = new ArrayBucket (15);
  _link_list   = new ArrayBucket (5);
  _control     = new WboxControl (this);
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxBox::~WboxBox()
{
  for(int ifield = _kmin; ifield <= _kmax; ifield++)     // must kill first.
      {
      WboxField *field = getFieldPointer(ifield);
      delete field;
      }
  for(int ivector = _vmin; ivector <= _vmax; ivector++)  // must kill next.
      {
      WboxVector *vector = getVectorPointer(ivector);
      delete vector;
      }
  for(int itab = _lmin; itab <= _lmax; itab++)          // must kill last.
      {
      WboxLink *link = getLinkPointer(itab);
      delete link;
      }
  delete _field_list;
  delete _vector_list;
  delete _link_list;
  delete _control;
}



//----------------- create link -----------------------------------//
//----------------- create link -----------------------------------//
//----------------- create link -----------------------------------//

      // private.
      // this must be called before calling completeCanvas.
      // irow = the row number where the prompts will reside.

WboxLink *WboxBox::createLink(int irow, int numrow, int numrow_init)
{
  assert(_creating);
  if(irow <= 0) irow = _irnext;
  int itab = _link_list->numElements() + 1;
  WboxLink *link = new WboxLink(this, itab, irow + 1, numrow, numrow_init);
  _link_list->addElement(link);
  _lmax = _link_list->numElements();
  _link_being_created = link;
  _icnext9 = 1;
  return link;
}



//----------------- create linked array set -------------------//
//----------------- create linked array set -------------------//
//----------------- create linked array set -------------------//

      // public.
      // this must be called before calling completeCanvas.
      // irow = the row number where the prompts will reside.

void WboxBox::createLinkedArraySet(int irow, int numrow, int numrow_init)
{
  createLink(irow, numrow, numrow_init);
}


void WboxBox::createLinkedArraySet(int irow, int numrow,
                               long *npoint, long *nmaxpoint, int numrow_init)
{
  WboxLink *link = createLink(irow, numrow, numrow_init);
  link->registerNPoint   (npoint);
  link->registerNmaxPoint(nmaxpoint);
}


void WboxBox::createLinkedArraySet(int irow, int numrow,
                               int *npoint, int *nmaxpoint, int numrow_init)
{
  WboxLink *link = createLink(irow, numrow, numrow_init);
  link->registerNPointInt   (npoint);
  link->registerNmaxPointInt(nmaxpoint);
}


void WboxBox::createLinkedArraySet(int irow, int numrow,
               WboxNupdateFun *nfun, WboxNupdateFun *nmaxfun, int numrow_init)
{
  WboxLink *link = createLink(irow, numrow, numrow_init);
  link->registerNFun   (nfun);
  link->registerNmaxFun(nmaxfun);
}



//----------------- create single datafield --------------------------//
//----------------- create single datafield --------------------------//
//----------------- create single datafield --------------------------//

      // public.
      // these must be called before calling completeCanvas.

void WboxBox::createIvarDatafield (int ident, int irow, int icol,
                                   int nchar)
{
  WboxVector *vector = createVector(WboxVector::_IVAR, ident,
                                    NULL, icol, nchar, 0, 0);
  createDatafield(vector, irow);
}


void WboxBox::createFvarDatafield (int ident, int irow, int icol,
                                   int nchar, int ndec)
{
  WboxVector *vector = createVector(WboxVector::_FVAR, ident,
                                    NULL, icol, nchar, ndec, 0);
  createDatafield(vector, irow);
}


void WboxBox::createDvarDatafield (int ident, int irow, int icol,
                                   int nchar, int ndec)
{
  WboxVector *vector = createVector(WboxVector::_DVAR, ident,
                                    NULL, icol, nchar, ndec, 0);
  createDatafield(vector, irow);
}


void WboxBox::createCvarDatafield (int ident, int irow, int icol,
                                   int nchar, int length)
{
  WboxVector *vector = createVector(WboxVector::_CVAR, ident,
                                    NULL, icol, nchar, 0, length);
  createDatafield(vector, irow);
}



//----------------- create datafield pair ----------------------------//
//----------------- create datafield pair ----------------------------//
//----------------- create datafield pair ----------------------------//

      // public.
      // these must be called before calling completeCanvas.

void WboxBox::createIvarDatafieldPair (int ident, int irow, int icol,
                                       int ncharp, int lengthp,
                                       int nchar)
{
  createCvarDatafield(-ident, irow,icol, ncharp, lengthp);
  createIvarDatafield( ident,   -1,  -1, nchar );
}


void WboxBox::createFvarDatafieldPair (int ident, int irow, int icol,
                                       int ncharp, int lengthp,
                                       int nchar, int ndec)
{
  createCvarDatafield(-ident, irow,icol, ncharp,lengthp);
  createFvarDatafield( ident,   -1,  -1, nchar ,ndec);
}


void WboxBox::createDvarDatafieldPair (int ident, int irow, int icol,
                                       int ncharp, int lengthp,
                                       int nchar, int ndec)
{
  createCvarDatafield(-ident, irow,icol, ncharp,lengthp);
  createDvarDatafield( ident,   -1,  -1, nchar ,ndec);
}


void WboxBox::createCvarDatafieldPair (int ident, int irow, int icol,
                                       int ncharp, int lengthp,
                                       int nchar, int length)
{
  createCvarDatafield(-ident, irow,icol, ncharp,lengthp);
  createCvarDatafield( ident,   -1,  -1, nchar ,length );
}


void WboxBox::createIvarDatafieldRevPair (int ident, int irow, int icol,
                                          int ncharp, int lengthp,
                                          int nchar)
{
  if(icol > 0 && nchar == 2)   // changed 3/13/97
/*
  if(icol > 0)
*/
    {
            //// nchar should be 2 here.
    int nchar2 = nchar + ncharp + 1;
    int icol2  = icol  + nchar  + 1;
    createIvarDatafield( ident, irow,icol, nchar2);
    createCvarDatafield(-ident,  -1,icol2, ncharp,lengthp);
    }
  else
    {
    createIvarDatafield( ident, irow,icol, nchar);
    createCvarDatafield(-ident,   -1,  -1, ncharp,lengthp);
    }
}



//----------------- create datafield array -------------------------//
//----------------- create datafield array -------------------------//
//----------------- create datafield array -------------------------//

               // private.

void WboxBox::createDatafieldArray(int itype, int ident, int icol,
                int ncharp, int lengthp, int nchar, int ndec, int length)
{
  WboxLink *link = _link_being_created;
  assert(link);

  if(icol <= 0) icol = _icnext9;
  int iarow = link->getIarow();
  int narow = link->getNarow();

  WboxVector *vectorp = createVector(WboxVector::_CVAR, -ident,
                                     NULL, icol, ncharp, 0, lengthp);
  createDatafield(vectorp, iarow-1);

  WboxVector *vectora = createVector((WboxVector::ITYPE)itype, ident,
                                     link, icol, nchar, ndec, length);

  for(int i = 0; i < narow; i++)
     {
     createDatafield(vectora, iarow + i);
     }

  int iacol = link->getIacol();
  int nacol = link->getNacol();

  if(vectora->getNchar() > nchar) nchar = vectora->getNchar();
  if(vectorp->getNchar() > nchar) nchar = vectorp->getNchar();
  _irnext  = iarow + narow;
  _icnext  = 1;
  _jcnext  = 1;
  _icnext9 = icol + nchar + 1;
  if(iacol == 0) iacol = vectora->getIcol();
  if(iacol != 0)
      {
      int nacol2 = vectora->getIcol() + vectora->getNchar() - iacol;
      if(nacol < nacol2) nacol = nacol2;
      }
  link->setIacol(iacol);
  link->setNacol(nacol);
  if(_allbox->getDebug() >= 4)
             printf("fixreg: ibox=%d, itab=%d, iacol=%d, nacol=%d\n",
                    _ibox, link->getItab(), iacol, nacol);
}



//----------------- create datafield array ---------------------------//
//----------------- create datafield array ---------------------------//
//----------------- create datafield array ---------------------------//

      // public.
      // these must be called before calling completeCanvas.

void WboxBox::createIvarDatafieldArray (int ident, int icol,
                                        int ncharp, int lengthp,
                                        int nchar)
{
  createDatafieldArray(WboxVector::_IVAR, ident, icol,
                       ncharp, lengthp, nchar, 0, 0);
}



void WboxBox::createFvarDatafieldArray (int ident, int icol,
                                        int ncharp, int lengthp,
                                        int nchar, int ndec)
{
  createDatafieldArray(WboxVector::_FVAR, ident, icol,
                       ncharp, lengthp, nchar, ndec, 0);
}



void WboxBox::createDvarDatafieldArray (int ident, int icol,
                                        int ncharp, int lengthp,
                                        int nchar, int ndec)
{
  createDatafieldArray(WboxVector::_DVAR, ident, icol,
                       ncharp, lengthp, nchar, ndec, 0);
}



void WboxBox::createCvarDatafieldArray (int ident, int icol,
                                        int ncharp, int lengthp,
                                        int nchar, int length)
{
  createDatafieldArray(WboxVector::_CVAR, ident, icol,
                       ncharp, lengthp, nchar, 0, length);
}



//------------------------- create vector --------------------------//
//------------------------- create vector --------------------------//
//------------------------- create vector --------------------------//

      // private.
      // this must be called before calling completeCanvas.
      // link argument must be NULL for scalar datafields.

WboxVector *WboxBox::createVector(int itype, int ident, WboxLink *link,
                                  int icol, int nchar, int ndec, int length)
{
  assert(_creating);
  int icol_keep = icol;
  if(icol_keep <  0) icol = _jcnext;
  if(icol_keep == 0) icol = _icnext;
  int ifield0 = _field_list->numElements();      // prompt.
  int ifield1 = _field_list->numElements() + 1;  // first field in vector.
  if(!link) ifield0 = 0;

  WboxVector *vector = new WboxVector(ifield0, ifield1,
                                      (WboxVector::ITYPE)itype,
                                      ident, this, link,
                                      icol, nchar, ndec, length);

  _jcnext = icol + vector->getNchar() + 1;    // nchar might have changed.
  if(icol_keep >= 0) _icnext = icol;

  _vector_list->addElement(vector);
  _vmax = _vector_list->numElements();
  _vector_being_created = vector;

  if(link) link->addVector(_vmax);

  registerGenericTrap(ident, _default_trap);
  return vector;
}



//------------------------- create datafield -----------------------//
//------------------------- create datafield -----------------------//
//------------------------- create datafield -----------------------//

      // private.
      // this must be called before calling completeCanvas.

void WboxBox::createDatafield(WboxVector *vector, int irow)
{
  assert(_creating);

  int irow_keep = irow;
  if(irow_keep <  0) irow = _irnext - 1;
  if(irow_keep == 0) irow = _irnext;

  WboxField *field = new WboxField(vector, irow);

  _irnext = irow + 1;

  _field_list->addElement(field);
  _kmax = _field_list->numElements();
  _field_being_created = field;
}



//--------------------- get values -------------------------------//
//--------------------- get values -------------------------------//
//--------------------- get values -------------------------------//


Window  WboxBox::getWindow()  const
{
  return XtWindow(_w);
}


WboxField *WboxBox::getActiveFieldPointer()  const
{
  return _control->getActiveFieldPointer();
}


WboxVector *WboxBox::getVectorPointer(int ivector)  const
{
  return (WboxVector*)_vector_list->fetchElement(ivector - 1);
}


WboxField *WboxBox::getFieldPointer(int ifield)  const
{
  return (WboxField*)_field_list->fetchElement(ifield - 1);
}


WboxLink *WboxBox::getLinkPointer(int itab)  const
{
  return (WboxLink*)_link_list->fetchElement(itab - 1);
}


int WboxBox::numBottomRows()  const
{
  if(_omit) return 0;
  if(_lmax >= _lmin) return 2;
  return 1;
}



//---------------------- find vector pointer ----------------------//
//---------------------- find vector pointer ----------------------//
//---------------------- find vector pointer ----------------------//

      // searches in reverse direction to be efficient when the
      //   search occurs shortly after creation.
      // only one vector should exist with a given ident, except
      //   for ident == 0.
      // if there are more than one vector with the same ident,
      //   the most-recently-created vector will be returned.
      

WboxVector *WboxBox::findVectorPointer(int ident)  const
{
  for(int ivector = _vmax; ivector >= _vmin; ivector--)
     {
     WboxVector *vector = getVectorPointer(ivector);
     if(vector->getIdent() == ident) return vector;
     }
  return NULL;
}




//-------------------- find field pointer ------------------------//
//-------------------- find field pointer ------------------------//
//-------------------- find field pointer ------------------------//

  // public.
  // returns a pointer to the datafield at the specified row and column.
  // returns NULL if no datafield is found at that location.
  // min_switch = minimum value of switch to consider.
  // switch value of zero is never considered (regardless of min_switch).

WboxField *WboxBox::findFieldPointer(int irow, int icol, int min_switch)  const
{
  for(int ifield = _kmin; ifield <= _kmax; ifield++)
      {
      WboxField *field = getFieldPointer(ifield);
      int        jrow  = field->getIrow();
      int        jcol  = field->getIcol();
      int        nchar = field->getNchar();
      int        svar  = field->getSvar();
      if(irow == jrow && svar >= min_switch && svar != 0
          && icol >= jcol && icol < jcol + nchar)
            {
            return field;
            }
      }
  return NULL;
}



//----------------- register generic trap ------------------------//
//----------------- register generic trap ------------------------//
//----------------- register generic trap ------------------------//

   // public.
   // this trap is assumed to be the same type as _default_traptype.
   // if this trap is NULL, nothing is done, so that an earlier trap
   //   (such as the default trap) is not wiped out.

void  WboxBox::registerGenericTrap (int ident, WboxGenericTrap *trap)
{
  if(trap == NULL) return;
  WboxVector *vector = findVectorPointer(ident);
  if(vector == NULL) return;
  vector->registerGenericTrap(trap, (WboxVector::TRAPTYPE)_default_traptype);
}



//--------------------- register traps----------------------------//
//--------------------- register traps----------------------------//
//--------------------- register traps----------------------------//

   // public.


#define RTRAP(registerFortranTrap, WboxFortranTrap)                     \
                                                                        \
void  WboxBox::registerFortranTrap (int ident, WboxFortranTrap *trap)   \
{                                                                       \
  WboxVector *vector = findVectorPointer(ident);                        \
  if(vector == NULL) return;                                            \
  vector->registerFortranTrap(trap);                                    \
}


RTRAP(registerFortranTrap  , WboxFortranTrap)
RTRAP(registerSimpleTrap   , WboxFortranTrap)
RTRAP(registerEzedTrap     , WboxEzedTrap   )
RTRAP(registerClanguageTrap, WboxClangTrap  )
RTRAP(registerIvarTrap     , WboxIvarTrap   )
RTRAP(registerFvarTrap     , WboxFvarTrap   )
RTRAP(registerDvarTrap     , WboxDvarTrap   )
RTRAP(registerCvarTrap     , WboxCvarTrap   )



//--------------------- register pointers ----------------------//
//--------------------- register pointers ----------------------//
//--------------------- register pointers ----------------------//


#define NPOINT(registerNPoint, long2)                    \
                                                         \
void  WboxBox::registerNPoint (int ident, long2 *n)      \
{                                                        \
  WboxVector *vector = findVectorPointer(ident);         \
  if(vector                   == NULL) return;           \
  if(vector->getLinkPointer() == NULL) return;           \
  vector->getLinkPointer()->registerNPoint(n);           \
}


NPOINT(registerNPoint        , long  )
NPOINT(registerNmaxPoint     , long  )
NPOINT(registerNPointInt     , int   )
NPOINT(registerNmaxPointInt  , int   )



#define RPOINT(registerIvarPoint, long2)                    \
                                                            \
void  WboxBox::registerIvarPoint (int ident, long2 *ivar)   \
{                                                           \
  WboxVector *vector = findVectorPointer(ident);            \
  if(vector == NULL) return;                                \
  vector->registerIvarPoint(ivar);                          \
}


RPOINT(registerSwitchPoint     , long  )
RPOINT(registerRadioPoint      , long  )
RPOINT(registerIvarPoint       , long  )
RPOINT(registerFvarPoint       , float )
RPOINT(registerDvarPoint       , double)
RPOINT(registerCvarPoint       , char  )
RPOINT(registerSwitchPointInt  , int   )
RPOINT(registerRadioPointInt   , int   )
RPOINT(registerIvarPointInt    , int   )



void  WboxBox::registerIndexBehavior (int ident)
{
  WboxVector *vector = findVectorPointer(ident);
  if(vector == NULL) return;
  vector->registerIndexBehavior();
}



//--------------------- register update functions ---------------------//
//--------------------- register update functions ---------------------//
//--------------------- register update functions ---------------------//


void  WboxBox::registerNFun (int ident, WboxNupdateFun *nfun)
{
  WboxVector *vector = findVectorPointer(ident);
  if(vector                   == NULL) return;
  if(vector->getLinkPointer() == NULL) return;
  vector->getLinkPointer()->registerNFun(nfun);
}


void  WboxBox::registerNmaxFun (int ident, WboxNupdateFun *nmaxfun)
{
  WboxVector *vector = findVectorPointer(ident);
  if(vector                   == NULL) return;
  if(vector->getLinkPointer() == NULL) return;
  vector->getLinkPointer()->registerNmaxFun(nmaxfun);
}



#define RFUN(registerIvarFun, WboxIupdateFun)                         \
                                                                      \
void  WboxBox::registerIvarFun (int ident, WboxIupdateFun *ifun)      \
{                                                                     \
  WboxVector *vector = findVectorPointer(ident);                      \
  if(vector == NULL) return;                                          \
  if(vector->getIdent() == ident) vector->registerIvarFun(ifun);      \
}


RFUN(registerSwitchFun, WboxIupdateFun)
RFUN(registerIvarFun  , WboxIupdateFun)
RFUN(registerFvarFun  , WboxFupdateFun)
RFUN(registerDvarFun  , WboxDupdateFun)
RFUN(registerCvarFun  , WboxCupdateFun)



//------------------------ ident is visible -----------------------//
//------------------------ ident is visible -----------------------//
//------------------------ ident is visible -----------------------//

       // public.

int WboxBox::identIsVisible(int ident)  const
{
  for(int ifield = _kmin; ifield <= _kmax; ifield++)
     {
     WboxField *field = getFieldPointer(ifield);
     if(field->getIdent() == ident)
         {
         if(field->getSvar() != -999) return TRUE;
         }
     }
  return FALSE;
}



//---------------------- update sensitivity ----------------------//
//---------------------- update sensitivity ----------------------//
//---------------------- update sensitivity ----------------------//

       // public.

void WboxBox::updateSensitivity()
{
  if(_managedflag <= 0) return;

  int sense = FALSE;
  for(int ifield = _kmin; ifield <= _kmax; ifield++)
      {
      if(getFieldPointer(ifield)->getSvar() > 0) sense = TRUE;
      }

  if(XtIsSensitive(_wtiny)) { if(!sense) XtSetSensitive(_wtiny, FALSE); }
  else                      { if( sense) XtSetSensitive(_wtiny, TRUE ); }

  if(sense == FALSE && _focusflag == 1)
      {
      _focusflag = 0;
      _control->showWindowboxInfo();
      }
}



//------------------------- set focus ----------------------------//
//------------------------- set focus ----------------------------//
//------------------------- set focus ----------------------------//

       // public.

void WboxBox::setFocus(int ident, int index)
{
  _control->setFocus(ident, index);
}



//------------------------ send event ------------------------------//
//------------------------ send event ------------------------------//
//------------------------ send event ------------------------------//

       // public.

void WboxBox::sendClientMessageEvent(const char *endkey)  const
{
  _control->sendClientMessageEvent(endkey);
}



void WboxBox::sendImmediateEvent(const char *endkey)
{
  _control->sendImmediateEvent(endkey);
}



//----------------------- misc ------------------------------------//
//----------------------- misc ------------------------------------//
//----------------------- misc ------------------------------------//

     // public.

void  WboxBox::gainFocus()
{
  if (_wtiny && XtIsSensitive(_wtiny))
                 XmProcessTraversal(_wtiny, XmTRAVERSE_CURRENT);
}


void WboxBox::manage()
{
  if(_shellchild) XtManageChild(_shellchild);
}



void WboxBox::unmanage()
{
  if(_shellchild) XtUnmanageChild(_shellchild);
}



void WboxBox::destroy()
{
  if(_shell) XtDestroyWidget(_shell);
}



//--------------------- prepare canvas size --------------------------//
//--------------------- prepare canvas size --------------------------//
//--------------------- prepare canvas size --------------------------//

  // private.
  // sets member variables _nrow and _ncol.

void WboxBox::prepareCanvasSize()
{
  int debug   = _allbox->getDebug();
  int maxrows = _allbox->getMaxRows();
/*
  int nbottom = 1;
  if(_lmax >= _lmin) nbottom = 2;
  if(_omit) nbottom = 0;
*/
  int nbottom = numBottomRows();
  int nscroll = 3;                // room for scrollbar.
  _nrow = 0;
  _ncol = 0;

  for(int ifield = _kmin; ifield <= _kmax; ifield++)
      {
      WboxField *field = getFieldPointer(ifield);
      int ident = field->getIdent();
      int itab  = field->getItab();
      int irow  = field->getIrow();
      int icol  = field->getIcol();
      int nchar = field->getNchar();
      if(itab  <=    0) _nrow = MaximumValue(irow + nbottom, _nrow);
      if(ident != 9998) _ncol = MaximumValue(icol + nchar - 1, _ncol);
      if(debug >= 5) printf
              ("zreg:  ibox %d ifield %d irow %d icol %d nchar %d\n",
                       _ibox, ifield, irow, icol, nchar);
      }
  if(debug >= 4) printf
              ("zreg:  ibox %d nrow %d ncol %d\n", _ibox, _nrow, _ncol);

  for(int itab = _lmin; itab <= _lmax; itab++)
      {
      WboxLink *link = getLinkPointer(itab);
      int iarow      = link->getIarow();
      int narow      = link->getNarow();
      int narow_init = link->getNarowInit();
      int iacol      = link->getIacol();
      int nacol      = link->getNacol();
      narow_init     = MaximumValue(narow_init,MinimumValue(narow, maxrows));
      _nrow = MaximumValue(iarow + narow_init + nbottom - 1, _nrow);
      _ncol = MaximumValue(iacol + nacol + nscroll         , _ncol);
      if(debug >= 4) printf
            ("zreg:  ibox %d itab %d iarow %d iacol %d narow %d nacol %d\n",
                       _ibox, itab, iarow, iacol, narow, nacol);
      }
  if(debug >= 4) printf
              ("zreg:  ibox %d nrow %d ncol %d\n", _ibox, _nrow, _ncol);
  _ncol = MinimumValue(_ncol, UCHAR_MAX);
  _starting_nrow = _nrow;
  _starting_ncol = _ncol;
}



//--------------------- prepare drawing area -------------------------//
//--------------------- prepare drawing area -------------------------//
//--------------------- prepare drawing area -------------------------//

  // private.
  // if w is not NULL, drawing area already exists and parent is not used.
  // if w   is   NULL, drawing area is created as child of parent.
  // sets member variables _w _parent, _shell, and _shellchild.

void WboxBox::prepareDrawingArea(Widget parent, Widget w)
{
  Dimension width  = (_ncol + 2) * _screen->getCellx() + 4;
  Dimension height = (_nrow + 1) * _screen->getCelly() - 1;

  if     (_nrow_init >= 1) height = (_nrow_init + 1) * _screen->getCelly() - 1;
  else if(_nrow_init == 0) height = 1;

  if(_allbox->getDebug() >= 1)
            printf("wbox: %d %s nrow/ncol=%d %d width/height=%d %d\n",
                    _ibox, _boxname, _nrow, _ncol, width, height);

  Arg args[15];
  int i=0;
  XtSetArg (args[i], XmNwidth, width); i++;
  XtSetArg (args[i], XmNheight, height); i++;
  XtSetArg (args[i], XmNborderWidth, 5); i++;
  XtSetArg (args[i], XmNforeground, _screen->getBlackPixel()); i++;
  XtSetArg (args[i], XmNbackground, _screen->getBackground()); i++;
  XtSetArg (args[i], XmNresizePolicy, XmRESIZE_NONE); i++;
  if(w == NULL)
          {
          assert(parent != NULL);
          if(XtIsShell(parent))
                {  XtSetArg(args[i], XmNautoUnmanage, False); i++; }
          _w = XmCreateDrawingArea(parent, _boxname, args, i);
          }
  else
          {
          _w = w;
          XtSetValues(_w, args, i);
          }

  _parent     = XtParent(_w);
  _shell      = get_shell_widget(_w);
  _shellchild = get_shell_child (_w);
}



//--------------------- prepare tiny pushbutton ----------------------//
//--------------------- prepare tiny pushbutton ----------------------//
//--------------------- prepare tiny pushbutton ----------------------//

             // private.
             // sets member variable _wtiny.

void WboxBox::prepareTinyPushbutton()
{
  Arg args[15];
  int i=0;
  if (_allbox->getTinySize() <= 0)
          {
          XtSetArg (args[i], XmNwidth,  1); i++;
          XtSetArg (args[i], XmNheight, 1); i++;
          XtSetArg (args[i], XmNhighlightThickness, 0); i++;
          XtSetArg (args[i], XmNbackground, _screen->getBackground()); i++;
          }
  else
          {
          XtSetArg (args[i], XmNwidth,  _allbox->getTinySize()); i++;
          XtSetArg (args[i], XmNheight, _allbox->getTinySize()); i++;
          }
  XtSetArg (args[i], XmNx, 0); i++;
  XtSetArg (args[i], XmNy, 0); i++;
  XtSetArg (args[i], XmNleftOffset, 0); i++;
  XtSetArg (args[i], XmNtopOffset,  0); i++;
  XtSetArg (args[i], XmNleftAttachment, XmATTACH_FORM); i++;
  XtSetArg (args[i], XmNtopAttachment,  XmATTACH_FORM); i++;
  XtSetArg (args[i], XmNborderWidth,        0); i++;
  _wtiny = XmCreatePushButton(_w, "tinytiny", args, i);
  XtManageChild(_wtiny);
}


/************************
including the above code has the following consequences:
  1 - a tiny pushbutton shows up in the upper left corner; the highlight
         border around the pushbutton shows the windowbox to be active
         or inactive for keyboard events.
  2 - you can leave traversalOn TRUE on other primitive widgets
         (presumably including text widgets) in the same dialog box.
  3 - you can tab into and out of the windowbox to/from other widgets.
  4 - when tabbing out, you first flash to the next tab location that
         the windowbox is trying to go to; then when finally returning
         to the windowbox, that will be where you will be; this is
         somewhat distracting, but not serious; it can now be solved by
         optionally using CTRL-T, F20, or SHIFT-F20 to move around within
         the windowbox.
  4 - you can buttonpress to get out of the windowbox, but to get back
         in with a buttonpress, I set process traversal to the tiny
         pushbutton, so you do not have to buttonpress on the tiny
         pushbutton.
  5 - adopting the tiny pushbutton has allowed me to delete a
         lot of messy code written to deal with missing focus events;
         this messy code has never been able to solve all problems
         anyway.
  6 - keyboard arrow events when the pointer is in the scrollbar will
         no longer be interpreted by the scrollbar, but by the
         windowbox; with some programming, this could be changed,
         but it would probably not be worth it since similar functionality
         already exists in the windowbox.
  7 - the pushbutton receives all keyboard events, and the windowbox
         itself receives all button events.
  8 - resizePolicy must be RESIZE_NONE on the windowbox widget; this is
         probably a good idea all the time, so I added it to the regular
         code; this (and the other changes wrought by this experimental
         code) has made it possible to throw out protective code
         regarding the scrollbars.
  9 - autoUnmanage must be False on the shell child; this is probably a
         good idea all the time; I added this to the code for the case
         where I am creating the drawing area, and the drawing area is
         a child of the shell; however, since this resource cannot be
         set by XtSetValues, it will normally have to be put into a
         resource file.  (This would have to be False anyway if there
         are any other pushbuttons in the dialog box.)
**********************/



//--------------------- complete canvas --------------------------------//
//--------------------- complete canvas --------------------------------//
//--------------------- complete canvas --------------------------------//

     // public.
     // complete the canvas after registrations are finished.
     // sets member variables _screen, _boxname, _nrow, _ncol,
     //    _w, _parent, _shell, _shellchild, _wtiny,
     //    _iswi, _iswv, _index_keep, _value_keep.

void WboxBox::completeCanvas (WboxScreen *screen,
                   const char *boxname, Widget parent, Widget w,
                   HelpCtx hctx, Widget toplevel,
                   const char *helpfile, const char *helptitle)
{
  assert(_creating);
  assert(_kmin == 1 && _kmax >= 1);
  assert(_vmin == 1 && _vmax >= 1);
  assert(_lmin == 1 && _lmax >= 0);
  assert(screen);

  _screen = screen;
  strncpy(_boxname, boxname, WBOX_BSIZE - 1);
  _boxname[WBOX_BSIZE - 1] = '\0';

  prepareCanvasSize     ();
  prepareDrawingArea    (parent, w);
  prepareTinyPushbutton ();

  _control->completeCanvas(hctx, toplevel, helpfile, helptitle);

  _creating = FALSE;
  _link_being_created   = NULL;
  _field_being_created  = NULL;
  _vector_being_created = NULL;
}



//------------------------ reconfigure -------------------------------//
//------------------------ reconfigure -------------------------------//
//------------------------ reconfigure -------------------------------//

      // private.
      // react to a resized window.
      // irow2 and icol2 are the values obtained from the event handler.
      // resets _nrow and _ncol.
      // resets _svar             in some WboxField objects.
      // resets _narow and _nacol in some WboxLink  objects.

void WboxBox::reconfigure(int irow2, int icol2)
{
///////// blank out the message lines.

  if(!_omit)
  {
  _screen->draw                   (this, _nrow    , 1, " ", _ncol, -77);
  if(_lmax >= _lmin) _screen->draw(this, _nrow - 1, 1, " ", _ncol, -77);
  }

///////// set new size of windowbox.

  _nrow = MaximumValue(irow2 - 1, 1);
  _ncol = MaximumValue(icol2 - 1, 1);
  _ncol = MinimumValue(_ncol, UCHAR_MAX);

///////// blank out fields which will not be visible.
///////// set switch to -999 if a field will not be visible.
///////// set switch to -99 if a field has just become visible.

  int nlast = _nrow - numBottomRows();
/*
  int nlast;
  if(_lmax >= _lmin) nlast = _nrow - 2;
  else               nlast = _nrow - 1;
  if(_omit) nlast = _nrow;
*/
  int ifield, itab;
  for(ifield = _kmin; ifield <= _kmax; ifield++)
      {
      WboxField *field = getFieldPointer(ifield);
      int svar  = field->getSvar();
      int irow  = field->getIrow();
      int icol  = field->getIcol();
      int nchar = field->getNchar();
      int itab  = field->getItab();
      if(irow > nlast || icol + nchar - 1 > _ncol ||
                    (itab >= 1 && icol + nchar - 1 > _ncol - 4))
          {
          if(svar != -99 && svar != -77 && svar != -999)
              {
              _screen->draw2(this, field, " ", -77);
              }
          svar = -999;              // no longer visible.
          }
      else if(svar == -999)
          {
          svar = -99;               // visible again.
          }
      field->setSvar(svar);
      }
  _screen->eraseHighlightBox(this);

///////// set new size of array boxes.

  if(_lmin > _lmax) return;
  for(itab = _lmin; itab <= _lmax; itab++)
      {
      WboxLink *link = getLinkPointer(itab);
      link->setNarow(0);
      link->setNacol(0);
      }
  for(ifield = _kmin; ifield <= _kmax; ifield++)
      {
      WboxField *field = getFieldPointer(ifield);
      int svar  = field->getSvar();
      int irow  = field->getIrow();
      int icol  = field->getIcol();
      int nchar = field->getNchar();
      int itab  = field->getItab();
      if(itab >= 1 && svar != -999)
          {
          WboxLink *link = getLinkPointer(itab);
          int iarow = link->getIarow();
          int iacol = link->getIacol();
          int narow = link->getNarow();
          int nacol = link->getNacol();
          narow = MaximumValue(narow, irow             - iarow + 1);
          nacol = MaximumValue(nacol, icol + nchar - 1 - iacol + 1);
          link->setNarow(narow);
          link->setNacol(nacol);
          }
      }
}



//--------------------- erase fields -------------------------------//
//--------------------- erase fields -------------------------------//
//--------------------- erase fields -------------------------------//

void WboxBox::eraseFields()
{
  for(int ifield = _kmin; ifield <= _kmax; ifield++)
      {
      WboxField *field = getFieldPointer(ifield);
      field->eraseField();
      }
}



//---------------- update array visibilities -----------------------//
//---------------- update array visibilities -----------------------//
//---------------- update array visibilities -----------------------//


void WboxBox::updateArrayVisibilities()
{
  if(_managedflag <= 0) return;
  WboxLink *keep = NULL;
  for(int ifield = _kmin; ifield <= _kmax; ifield++)
      {
      WboxField *field = getFieldPointer(ifield);
      WboxLink  *link  = field->getLinkPointer();
      int        svar  = field->getSvar();
      if(svar != -999 && link != NULL && link != keep)
          {
          link->updateArrayVisibility();
          keep = link;
          }
      }
}



//------------------------ update scrollbars -----------------------//
//------------------------ update scrollbars -----------------------//
//------------------------ update scrollbars -----------------------//


void WboxBox::updateScrollbars()
{
  if(_managedflag <= 0) return;                        // added 6/13/96
  for(int itab = _lmin; itab <= _lmax; itab++)
      {
      WboxLink *link = getLinkPointer(itab);
      link->updateScrollbar();
      }
}



//------------ show message or maybe show message ------------------//
//------------ show message or maybe show message ------------------//
//------------ show message or maybe show message ------------------//

         // public.

void WboxBox::showMessage(const char *msg)
{
  _control->showMessage(msg);
}


void WboxBox::maybeShowMessage(const char *msg)
{
  _control->maybeShowMessage(msg);
}



//--------------------- save table ---------------------------------//
//--------------------- save table ---------------------------------//
//--------------------- save table ---------------------------------//

         // public.

void WboxBox::registerHardcopy (WboxHardcopy *hardcopy)
{
  _control->registerHardcopy(hardcopy);
}


void WboxBox::saveTable(const char *filename)  const
{
  _control->saveTable(filename);
}



//------------------------ update fields ---------------------------//
//------------------------ update fields ---------------------------//
//------------------------ update fields ---------------------------//

    // public.

void WboxBox::updateFields(const char *endkey,
                           int irow, int icol, int irow2, int icol2)
{
  _control->updateFields(endkey, irow, icol, irow2, icol2);
}



//------------------------- boxtrap -------------------------------//
//------------------------- boxtrap -------------------------------//
//------------------------- boxtrap -------------------------------//

         // public.

void WboxBox::boxtrap(const char *charr, const char *endkey,
                             int irow, int icol, int irow2, int icol2)
{
//---------we have changed the size of the screen.

  if(!strcmp(endkey, "CONFIGUR") ||
       (!strcmp(endkey, "EXPOSE") && _managedflag == -1))
      {
      reconfigure(irow2, icol2);
      if(!strcmp(endkey, "EXPOSE")) _managedflag = 1;
      }

//---------we have exposed a part of the screen.

  else if(!strcmp(endkey, "EXPOSE"))
      {
      _managedflag = 1;
      }
  else if(!strcmp(endkey, "UNMAP") || !strcmp(endkey, "DESTROY"))
      {
      _managedflag = 0;
      return;                 // do not call _control->boxtrap.
      }

//---------set focusflag depending on focusin and focusout.

  else if(!strcmp(endkey, "FOCUSOUT"))
      {
      _focusflag = 0;
      }
  else if(!strcmp(endkey, "FOCUSIN"))
      {
      _focusflag = 1;
      }

  _control->boxtrap(charr, endkey, irow, icol, irow2, icol2);
}



//----------------- restore previous user value ------------------//
//----------------- restore previous user value ------------------//
//----------------- restore previous user value ------------------//

       // public.

void WboxBox::restorePreviousUserValue(int *ident, int *index, int *istat)
{
  _control->restorePreviousUserValue(ident, index, istat);
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

