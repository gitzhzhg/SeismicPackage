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

//---------------------- wbox_find.cc -------------------------//
//---------------------- wbox_find.cc -------------------------//
//---------------------- wbox_find.cc -------------------------//

//          implementation file for the WboxFind class
//                  not derived from any class
//                       subdirectory wbox


  // This class registers the fields activated by pressing
  //     the FIND key in an array field.
  // These fields are not activated unless the windowbox is already
  //     wide enough to accommodate these fields.
  // These fields use ident = 9999 and 9998.


#include "wbox/wbox_find.hh"
#include "wbox/wbox_box.hh"
#include "wbox/wbox_messages.hh"
#include "wbox/wbox_allbox.hh"
#include "wbox/wbox_field.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//
//------------------- constructor -------------------------------//


WboxFind::WboxFind(WboxBox *box, WboxMessages *messages)
         :
            _box                    (box),
            _messages               (messages),
            _index_field            (NULL),
            _value_field            (NULL),
            _field_keep             (NULL),
            _ident_keep             (0),
            _index_keep             (0),
            _n_keep                 (0),
            _nread_keep             (0),
            _iswi                   (-99),
            _iswv                   (-99)
{
  assert(_box && _messages);
  memset(_value_keep, '\0', WBOX_VSIZE);
  strcpy(_value_keep, " ");
  if(!_box->hasLinkedArrays()) return;
  if(_box->getOmitFlag()) return;

  int identi =  9999;
  int identv =  9998;
  int irow   =  1;
  int icoli  =  7;
  int icolv  = 30;
  int nchari =  5;
  int ncharv = 10;
  int length = 10;

  _box->createIvarDatafield(identi, irow, icoli, nchari);
  _index_field = _box->getFieldBeingCreated();
  _box->createCvarDatafield(identv, irow, icolv, ncharv, length);
  _value_field = _box->getFieldBeingCreated();

  _box->registerIvarPoint  (identi, &_index_keep);
  _box->registerCvarPoint  (identv,  _value_keep);
  _box->registerSwitchPoint(identi, &_iswi);
  _box->registerSwitchPoint(identv, &_iswv);
}



//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//
//------------------- destructor -----------------------------//


WboxFind::~WboxFind()
{
}



//--------------------------- use find fields -----------------------//
//--------------------------- use find fields -----------------------//
//--------------------------- use find fields -----------------------//

     // public.
     // all of the arguments must be valid upon entry.
     // any of the arguments might get changed by this routine.

void WboxFind::useFindFields(int *ident, int *index,
                             int *nread, char *endkey)
{
  if(_box->getOmitFlag()) return;
  if(!_box->hasLinkedArrays()) return;
  WboxField *field = _box->getActiveFieldPointer();

/////---------///// the FIND key has been pressed:

  if(!strcmp(endkey, "FIND") && field->getItab() >= 1)
      {
      _n_keep = field->getN();           // remember length of array.
      if(_n_keep == 0)
          {
          _messages->maybeShowMessage("NOTHING TO FIND");
          return;
          }
      int nrow = _box->getNrow();
      int ncol = _box->getNcol();
      _iswi = 1;                         // switch for index field.
      _index_field->setSvar(-99);        // forces index field to be drawn.
      _index_field->setIrow(nrow - 1);   // reset row# of index field.
      if(ncol >= 40)
          {
          _iswv = 1;                       // switch for value field.
          _value_field->setSvar(-99);      // forces value field to be drawn.
          _value_field->setIrow(nrow - 1); // reset row# of value field.
          }
      _field_keep = field;        // remember where find key was pressed.
      _ident_keep = *ident;       // remember correct ident.
      _index_keep = *index;       // initialize to current index.
      _messages->displayArrayInfo((int)_index_keep, (int)_n_keep, 1);
      *ident = 9999;                       // now go to index field.
      *index = 1;
      }

/////---------///// we are in the index field:
/////---------///// _index_keep might have been updated by the user.

  else if(*ident == 9999)
      {
      _iswi = -99;                       // switch for index field.
      _index_field->setSvar(-999);       // forces index field to be ignored.
      if(_iswv == 1)
          _value_field->setSvar(-99);    // forces value field to be drawn.
      *ident =      _ident_keep;
      *index = (int)_index_keep;         // updated by user if *nread > 0.
      if(*nread > 0 || _iswv == -99 ||
         !strcmp(endkey, "FOCUSIN") || !strcmp(endkey, "FOCUSOUT") ||
         !strcmp(endkey, "SCROLL"))
          {
          _iswv = -99;                     // switch for value field.
          _value_field->setSvar(-999);     // forces value field to be ignored.
          strcpy(endkey, " ");
          return;
          }
      strcpy(endkey, " ");
      _messages->displayArrayInfo((int)_index_keep, (int)_n_keep, 1);
      *ident = 9998;                       // now go to value field.
      *index = 1;
      }

/////---------///// we are in the value field:
/////---------///// _value_keep might have been updated by the user.

  else if(*ident == 9998)
      {
      _iswv = -99;                       // switch for value field.
      _value_field->setSvar(-999);       // forces value field to be ignored.
      *ident =      _ident_keep;
      *index = (int)_index_keep;
      strcpy(endkey, " ");
      if(_value_keep[0] == '\0' || _value_keep[0] == ' ') return;

      if(*nread == 0) *nread = _nread_keep;
      if(*nread >  0) _nread_keep = *nread;
      int found = findValue(*nread, index);
      if(!found)
          {
          _box->getAllboxPointer()->ringBell();
          strcpy(_value_keep, " ");
          }
      }

/////---------///// we are in some other field:

  else
      {
      strcpy(_value_keep, " ");
      }
}



//------------------------- find value ----------------------------------//
//------------------------- find value ----------------------------------//
//------------------------- find value ----------------------------------//

          // private.
          // uses _field_keep, _value_keep, and _n_keep.
          // uses argument nread.
          // uses and changes argument index.
          // returns TRUE if value found or FALSE if not found.

int WboxFind::findValue(int nread, int *index)
{
  int    inear2, inear3, ivalue, ivar;
  float  fnear2, fnear3, fvalue, fvar;
  double dnear2, dnear3, dvalue, dvar;
  char   cvar[200];
  WboxField *field = _field_keep;
  if(field->isCvar())                           // character variable.
      {
      int length = field->getLength();
      int nread2 = strlen(_value_keep);
      if(length    < nread2) nread2 = length;
      if(nread + 1 < nread2) nread2 = nread + 1;
      for(int m = nread2; m >= 1; m--)
          {
          char comp1[200];
          strncpy(comp1, _value_keep, m);
          comp1[m] = '\0';
          for(int i = 1; i <= _n_keep; i++)
              {
              char comp2[200];
              int i2 = i + *index;
              if(i2 > _n_keep) i2 -= _n_keep;
              field->getUserValue(i2, &ivar, &fvar, &dvar, cvar);
              strncpy(comp2, cvar  , m);
              comp2[m] = '\0';
              if(!strcmp(comp1, comp2))
                  {
                  *index = i2;
                  return TRUE;
                  }
              }
          }
      return FALSE;
      }
  else                           // not character variable.
      {
      int istat =
           field->decodeValue(_value_keep, &ivalue, &fvalue, &dvalue, NULL);
             // (char* argument NULL is not used since not character var)
      if(istat <= 0) return FALSE;     // nil value or error.
      int i3 = *index;
      field->getUserValue(i3, &ivar, &fvar, &dvar, cvar);
      if     (field->isDvar()) dnear3 = AbsValue(dvar - dvalue) + 1.e30;
      else if(field->isFvar()) fnear3 = AbsValue(fvar - fvalue) + 1.e30;
      else                     inear3 = AbsValue(ivar - ivalue) + 99999;
      for(int i = 1; i <= _n_keep; i++)
          {
          int i2 = i + *index;
          if(i2 > _n_keep) i2 -= _n_keep;
          field->getUserValue(i2, &ivar, &fvar, &dvar, cvar);
          if(field->isDvar())
              {
              dnear2 = AbsoluteValue(dvar - dvalue);
              if(dnear2 < dnear3) { i3 = i2; dnear3 = dnear2; }
              }
          else if(field->isFvar())
              {
              fnear2 = AbsoluteValue(fvar - fvalue);
              if(fnear2 < fnear3) { i3 = i2; fnear3 = fnear2; }
              }
          else
              {
              inear2 = AbsoluteValue(ivar - ivalue);
              if(inear2 < inear3) { i3 = i2; inear3 = inear2; }
              }
          }
      *index = i3;
      }
  return TRUE;
}



//------------------------- end -------------------------------//
//------------------------- end -------------------------------//
//------------------------- end -------------------------------//

