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

//---------------------- slp_text.cc ----------------------------------//
//---------------------- slp_text.cc ----------------------------------//
//---------------------- slp_text.cc ----------------------------------//

//           implementation file for the SLpText class
//                 derived from the SLpBase class
//                       subdirectory sl

#include "sl/slp_text.hh"
#include "sl/psuedo_widget.hh"
#include "str.h"
#include "named_constants.h"
#include <Xm/Text.h>
#include <iostream.h>
#include <assert.h>
 

//----------------- constructor helper --------------------------//
//----------------- constructor helper --------------------------//
//----------------- constructor helper --------------------------//

void SLpText::constructorHelper()
{
  switch(_type)
    {
    case _LONG:   createIvarResource();
                  createIminResource();
                  createImaxResource();
                  if(_nchar <= 0) _nchar = 6;
                  _ndec = 0;
                  setNewIvar(ivarDefault());
                  break;
    case _FLOAT:  createFvarResource();
                  createFminResource();
                  createFmaxResource();
                  if(_nchar <= 0) _nchar = 10;
                  _ndec = ConstrainValue(_ndec, 0, _nchar - 1);
                  setNewFvar(fvarDefault());
                  break;
    case _DOUBLE: createDvarResource();
                  createDminResource();
                  createDmaxResource();
                  if(_nchar <= 0) _nchar = 10;
                  _ndec = ConstrainValue(_ndec, 0, _nchar - 1);
                  setNewDvar(dvarDefault());
                  break;
    case _CHAR:   createCvarResource();
                  if(_nchar <= 0) _nchar = 0;
                  _ndec = 0;
                  setNewCvar(cvarDefault());
                  break;
    default:      cout << "Illegal type in SLpText constructor" << endl;
                  exit(0);
    }
  createSenseResource();
}



//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//
//--------------------- constructors -------------------------------//


SLpText::SLpText (SLDelay *slparent, char *name, long ident,
                  long type, long nchar, long ndec)
    : SLpBase(slparent, name, xmTextWidgetClass, ident, type),
      _flush       (FALSE),
      _show        (_NORMAL_APPEARANCE),
      _nchar       (nchar),
      _ndec        (ndec)
{
  constructorHelper();
  if(slparent->topWidget()) make();
}


SLpText::SLpText (Widget wparent, char *name, long ident,
                  long type, long nchar, long ndec)
    : SLpBase(wparent, name, xmTextWidgetClass, ident, type),
      _flush       (FALSE),
      _show        (_NORMAL_APPEARANCE),
      _nchar       (nchar),
      _ndec        (ndec)
{
  constructorHelper();
  make();
}


SLpText::SLpText (Widget w, long ident,
                  long type, long nchar, long ndec)
    : SLpBase(w, ident, type),
      _flush       (FALSE),
      _show        (_NORMAL_APPEARANCE),
      _nchar       (nchar),
      _ndec        (ndec)
{
  constructorHelper();
  make();
}



//------------------ destructor -------------------------//
//------------------ destructor -------------------------//
//------------------ destructor -------------------------//

SLpText::~SLpText(void)
{
}



//------------ set sensitivity ---------------------------//
//------------ set sensitivity ---------------------------//
//------------ set sensitivity ---------------------------//

void SLpText::setSenseResource(void)
{
  Widget w = topWidget();
  if(!w) return;
  int     thick;
  Boolean editable;
  int     width;
  switch(_show)
     {
     case _NORMAL_APPEARANCE:
               thick    = 2;
               editable = (Boolean)sense();
               width    = 0;
               break;
     case _LABEL_APPEARANCE:
               thick    = 0;
               editable = FALSE;
               width    = 0;
               break;
     default:    //     _FRAMED_LABEL_APPEARANCE
               thick    = 0;
               editable = FALSE;
               width    = 1;
               break;
     }
  XtVaSetValues(w, XmNshadowThickness      , thick,
                   XmNeditable             , editable,
                   XmNtraversalOn          , editable,
                   XmNcursorPositionVisible, editable,
                   XmNborderWidth          , width, NULL);
  SLBase::setSensitivity((Boolean)sense());
}



//----------------- set resources ---------------------------//
//----------------- set resources ---------------------------//
//----------------- set resources ---------------------------//


static void set_cvar_resource(Widget w, char *text)
{
  if(!w) return;
  if (text == NULL)
       {
       XmTextSetString(w, "");
       }
  else 
       {
   //  XtVaSetValues(w, XmNvalue, text, NULL);   // equivalent
       XmTextSetString(w, text);
       }
}



static void set_ivar_resource(Widget w, long value, long nchar)
{
  if(!w || nchar <= 0) return;
  char *text = new char[nchar + 1];
  int value2 = (int)value;
  int nchar2 = (int)nchar;
/*
  if(text) convert_ii2ss(&value2, text, &nchar2);
*/
  if(text) str_ii2ss(value2, text, nchar2);
  set_cvar_resource(w, text);
  delete [] text;
}



static void set_fvar_resource(Widget w, float value, long nchar, long ndec)
{
  if(!w || nchar <= 0) return;
  char *text = new char[nchar + 1];
  int nchar2 = (int)nchar;
  int ndec2  = (int)ndec;
/*
  if(text) convert_ff2ss(&value, text, &nchar2, &ndec2);
*/
  if(text) str_ff2ss(value, text, nchar2, ndec2);
  set_cvar_resource(w, text);
  delete [] text;
}


static void set_dvar_resource(Widget w, double value, long nchar, long ndec)
{
  if(!w || nchar <= 0) return;
  char *text = new char[nchar + 1];
  int nchar2 = (int)nchar;
  int ndec2  = (int)ndec;
/*
  if(text) convert_dd2ss(&value, text, &nchar2, &ndec2);
*/
  if(text) str_dd2ss(value, text, nchar2, ndec2);
  set_cvar_resource(w, text);
  delete [] text;
}



void SLpText::setIvarResource(void)
{
  set_ivar_resource(topWidget(), ivar(), _nchar);
  if(_flush && topWidget()) XFlush(XtDisplay(topWidget()));
}


void SLpText::setFvarResource(void)
{
  set_fvar_resource(topWidget(), fvar(), _nchar, _ndec);
  if(_flush && topWidget()) XFlush(XtDisplay(topWidget()));
}


void SLpText::setDvarResource(void)
{
  set_dvar_resource(topWidget(), dvar(), _nchar, _ndec);
  if(_flush && topWidget()) XFlush(XtDisplay(topWidget()));
}


void SLpText::setCvarResource(void)
{
  set_cvar_resource(topWidget(), cvar());
  if(_flush && topWidget()) XFlush(XtDisplay(topWidget()));
}



//--------------------- get resources ---------------------//
//--------------------- get resources ---------------------//
//--------------------- get resources ---------------------//


long SLpText::ivarResource(void) const
{
  char *text = cvarResource();
  int value;
  int istat;
/*
  convert_ss2ii(text, &value, &istat);
*/
  str_ss2ii(text, &value, &istat);
  return value;
}


float SLpText::fvarResource(void) const
{
  char *text = cvarResource();
  float value;
  int istat;
/*
  convert_ss2ff(text, &value, &istat);
*/
  str_ss2ff(text, &value, &istat);
  return value;
}


double SLpText::dvarResource(void) const
{
  char *text = cvarResource();
  double value;
  int istat;
/*
  convert_ss2dd(text, &value, &istat);
*/
  str_ss2dd(text, &value, &istat);
  return value;
}


char *SLpText::cvarResource(void) const
{
  if(!topWidget()) return NULL;
  char *text = XmTextGetString(topWidget());
  _textcopy = string_alloc(_textcopy, text, 0);
  XtFree(text);
  return _textcopy;
}



//--------------------- get defaults ---------------------//
//--------------------- get defaults ---------------------//
//--------------------- get defaults ---------------------//


long SLpText::ivarDefault(void) const
{
  char *text = cvarDefault();
  int value;
  int istat;
/*
  convert_ss2ii(text, &value, &istat);
*/
  str_ss2ii(text, &value, &istat);
  return value;
}

float SLpText::fvarDefault(void) const
{
  char *text = cvarDefault();
  float value;
  int istat;
/*
  convert_ss2ff(text, &value, &istat);
*/
  str_ss2ff(text, &value, &istat);
  return value;
}

double SLpText::dvarDefault(void) const
{
  char *text = cvarDefault();
  double value;
  int istat;
/*
  convert_ss2dd(text, &value, &istat);
*/
  str_ss2dd(text, &value, &istat);
  return value;
}

char *SLpText::cvarDefault(void) const
{
  if(!pW()) return NULL;
  return pW()->textDef();
}



//--------------- make ------------------------------------//
//--------------- make ------------------------------------//
//--------------- make ------------------------------------//

static String defres[]= {
    ".marginHeight:       0",
    NULL };


Widget SLpText::make(Widget p)
{
  if(!made())
     {
     Widget w = SLDelay::make(p);
     if(!w)
        {
        setDefaultResources(XtDisplay(wParent()), instanceName(), defres);
        Arg args[7];
        int i = 0;
        if(_nchar > 0)
            {
            XtSetArg(args[i], XmNcolumns  , (short)_nchar); i++;
            XtSetArg(args[i], XmNmaxLength, (int)_nchar  ); i++;
            }
        w = XmCreateText(wParent(), (char*)instanceName(), args, i);
        setTopWidget(w);
        XtManageChild(w);
        }
     XmRemoveTabGroup(w);
     setTraversal(_traversal);
     install_help();
     XtAddEventHandler (w, FocusChangeMask, FALSE,
                        (XtEventHandler)focusEventHandler, (XtPointer)this);
     XtAddCallback(w, XmNactivateCallback    , 
                                (XtCallbackProc)tCallback, (XtPointer)this);
     XtAddCallback(w, XmNfocusCallback       , 
                                (XtCallbackProc)tCallback, (XtPointer)this);
     XtAddCallback(w, XmNlosingFocusCallback , 
                                (XtCallbackProc)tCallback, (XtPointer)this);
     XtAddCallback(w, XmNmodifyVerifyCallback, 
                                (XtCallbackProc)tCallback, (XtPointer)this);
     XtAddCallback(w, XmNvalueChangedCallback, 
                                (XtCallbackProc)tCallback, (XtPointer)this);
     add_change_case_actions(w);
     if(updateSenseNeverActivated())
                          setupSenseValue(SLBase::sensitivity());
     updateSelf(TRUE);
     }
  return topWidget();
}



//------------------ display message ---------------------------//
//------------------ display message ---------------------------//
//------------------ display message ---------------------------//

void SLpText::displayMessage(void *data, char *msg)
{
  SLpText *gui = (SLpText*)data;
  assert(gui->_type == _CHAR);
  gui->setCvar(msg);
  if(!gui->_flush) XFlush(XtDisplay(gui->topWidget()));
}



//------------------------- check value ------------------------//
//------------------------- check value ------------------------//
//------------------------- check value ------------------------//

#define CHECK_VALUE(long2, imin, imax, ivar)                    \
long2 SLpText::checkValue(long2 value, int istat)               \
{                                                               \
  if(istat > 0 && imin() < imax())                              \
     {                                                          \
     if(value < imin() || value > imax())                       \
          {                                                     \
          value = (int)ConstrainValue(value, imin(), imax());   \
          XBell(XtDisplay(topWidget()), 50);                    \
          }                                                     \
     }                                                          \
  else if(istat < 0)                                            \
     {                                                          \
     value = (int)ivar();                                       \
     XBell(XtDisplay(topWidget()), 50);                         \
     }                                                          \
  return value;                                                 \
}

CHECK_VALUE(int   , imin, imax, ivar)
CHECK_VALUE(float , fmin, fmax, fvar)
CHECK_VALUE(double, dmin, dmax, dvar)



//----------------- get modified text -------------------------//
//----------------- get modified text -------------------------//
//----------------- get modified text -------------------------//

static char *get_modified_text(Widget w, XmTextVerifyCallbackStruct *call2)
{
  static char modified[201];

  char *text = XmTextGetString(w);
  size_t lentext = strlen(text);
  memcpy(modified, text, (unsigned int)call2->startPos);
  memcpy(modified + call2->startPos, call2->text->ptr,
                                           call2->text->length);
  memcpy(modified + call2->startPos + call2->text->length,
             text + call2->endPos, (unsigned int)(lentext - call2->endPos));
  modified[call2->startPos + call2->text->length + lentext
                                 - call2->endPos] = '\0';
  XtFree(text);
  return modified;
}



//--------------------- text callback ----------------------------//
//--------------------- text callback ----------------------------//
//--------------------- text callback ----------------------------//

           // Calls trap upon activate.
           // Calls trap upon losing focus (if value changed).

void SLpText::tCallback(Widget w, XtPointer user, XtPointer call)
{
  SLpText *gui = (SLpText*)user;
  XmTextVerifyCallbackStruct *call2 = (XmTextVerifyCallbackStruct*)call;
  static Boolean changed = FALSE;
  /////// not used: XmTextPosition position;
  char  *cvalue, *modified;
  int    ivalue, istat, length;
  float  fvalue;
  double dvalue;

  if(!call2->event) return;
  switch(call2->reason)
    {
    case XmCR_MODIFYING_TEXT_VALUE:
        cvalue   = XmTextGetString(w);
        modified = get_modified_text(w, call2);
        length   = strlen(modified);
        XtFree(cvalue);
        switch(gui->_type)
           {
/*
           case _LONG  : convert_ss2ii(modified, &ivalue, &istat); break;
           case _FLOAT : convert_ss2ff(modified, &fvalue, &istat); break;
           case _DOUBLE: convert_ss2dd(modified, &dvalue, &istat); break;
*/
           case _LONG  : str_ss2ii(modified, &ivalue, &istat); break;
           case _FLOAT : str_ss2ff(modified, &fvalue, &istat); break;
           case _DOUBLE: str_ss2dd(modified, &dvalue, &istat); break;
      //   default     : break;              // removed 4/7/95
           default     : istat = 1; break;   // added   4/7/95
           }
        if (istat == -1) call2->doit = FALSE;
        break;

    case XmCR_VALUE_CHANGED:
        changed = TRUE;
        break;

    case XmCR_FOCUS:
        cvalue = XmTextGetString(w);
        XmTextSetSelection(w, 0, strlen(cvalue), (Time)0);
        XtFree(cvalue);
        changed = FALSE;
        break;

    case XmCR_LOSING_FOCUS:
        XmTextClearSelection(w, (Time)0);
        if(!changed) break;

    case XmCR_ACTIVATE:
        cvalue = XmTextGetString(w);
        gui->setLastCallback((XmAnyCallbackStruct*)call);
        switch(gui->_type)
           {
           case _LONG:
/*
               convert_ss2ii(cvalue, &ivalue, &istat);
*/
               str_ss2ii(cvalue, &ivalue, &istat);
               ivalue = gui->checkValue(ivalue, istat);
               gui->callItrap(ivalue);
               break;
           case _FLOAT:
/*
               convert_ss2ff(cvalue, &fvalue, &istat);
*/
               str_ss2ff(cvalue, &fvalue, &istat);
               fvalue = gui->checkValue(fvalue, istat);
               gui->callFtrap(fvalue);
               break;
           case _DOUBLE:
/*
               convert_ss2dd(cvalue, &dvalue, &istat);
*/
               str_ss2dd(cvalue, &dvalue, &istat);
               dvalue = gui->checkValue(dvalue, istat);
               gui->callDtrap(dvalue);
               break;
           case _CHAR:
               gui->callCtrap(cvalue);
               break;
           default:
               break;
           }
        XtFree(cvalue);
        gui->updateEverything();
        gui->setLastCallback(NULL);
        changed = FALSE;
        break;
    }
}



//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
//------------------------ end ----------------------------------//
