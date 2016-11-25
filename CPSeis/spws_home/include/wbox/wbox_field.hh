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

//----------------------- wbox_field.hh -------------------------//
//----------------------- wbox_field.hh -------------------------//
//----------------------- wbox_field.hh -------------------------//

//             header file for the WboxField class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_FIELD_HH_
#define _WBOX_FIELD_HH_

#include "oprim/local_value.hh"


class WboxBox;
class WboxLink;
class WboxVector;


class WboxField
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

public:

  enum CHANGED  { _NO_CHANGE, _SWITCH_CHANGED, _VAR_CHANGED, _BOTH_CHANGED };

private:

  WboxVector    *_vector;  // pointer to vector object.
  LocalValue     _var;     // local copy of variable.
  int            _svar;    // local copy of switch.
  unsigned char  _irow;    // row number of datafield.
  unsigned char  _force;   // force update if TRUE (might not need this).


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:    // constructor and destructor.

  WboxField (WboxVector *vector, int irow);

  virtual ~WboxField();

public:    // get values.

  int         getIdent         ()  const;
  WboxBox    *getBoxPointer    ()  const;
  WboxLink   *getLinkPointer   ()  const;
  WboxVector *getVectorPointer ()  const  { return _vector; }
  int         getIrow          ()  const  { return _irow; }
  int         getIcol          ()  const;
  int         getNchar         ()  const;
  int         getNdec          ()  const;
  int         getLength        ()  const;
  int         getSvar          ()  const  { return _svar; }
  int         getItype         ()  const;  // returns WboxVector enum.
  int         getTraptype      ()  const;  // returns WboxVector enum.
  int         getUptype        ()  const;  // returns WboxVector enum.
  int         getSwtype        ()  const;  // returns WboxVector enum.
  void       *getUpdatePointer ()  const;
  int         getIvar          ()  const;  // returns local copy.
  float       getFvar          ()  const;  // returns local copy.
  double      getDvar          ()  const;  // returns local copy.
  const char *getCvar          ()  const;  // returns local copy.
  int         getIfield        ()  const;  // references WboxVector.
  int         getIndex         ()  const;  // references WboxLink.
  int         getN             ()  const;  // references WboxLink.
  int         getNmax          ()  const;  // references WboxLink.
  int         getIbox          ()  const;
  int         getItab          ()  const;

  int         isIvar           ()  const;
  int         isFvar           ()  const;
  int         isDvar           ()  const;
  int         isCvar           ()  const;
  int         isRadio          ()  const;
  int         isIndex          ()  const;

  void getValue(int *ivar, float *fvar, double *dvar, char *cvar)  const;

public:  // get values from user area (using pointer or update function).
         // put values into user area (using pointer only).
         // the index need not be the actual index for this datafield.
         // this datafield is not changed.

  int    getEffectiveSwitch ()                       const;
  int    getUserSwitch      (int index)              const;
  void   getUserCvar        (int index, char *cvar)  const;
  int    getUserIvar        (int index)              const;
  float  getUserFvar        (int index)              const;
  double getUserDvar        (int index)              const;

  void getUserValue  (int index,
              int *ivar, float *fvar, double *dvar, char *cvar)  const;

  void putUserValue  (int index)  const;

  void putUserValue  (int index,
              int ivar, float fvar, double dvar, const char *cvar)  const;

  void copyUserValue (int index1, int index2)  const;

  void stepUserRadioValue (int index, const char *endkey)  const;

public:   // all of these are const functions except compare().

  void   encodeValue (char *textbuf)  const;
  void   encodeValue (int ivar, float fvar, double dvar, const char *cvar,
                      char *textbuf)  const;

  int    decodeValue  (const char *textbuf,
            int *ivar, float *fvar, double *dvar, char *cvar)  const;

  int    validateText (const char *textbuf)  const;

  void   getAndEncodeUserValue (int index,       char *textbuf)  const;
  int    decodeAndPutUserValue (int index, const char *textbuf)  const;

  CHANGED compare();

  void callTrap (int ivar, float fvar, double dvar, const char *cvar,
                 const char *textbuf, int nread, char *endkey,
                 int *ident, int *index)  const;

  void callTrap (const char *textbuf, int nread, char *endkey,
                 int *ident, int *index)  const;

  void setSvar (int svar);
  void setIrow (int irow);

  void    eraseField  ();
  CHANGED updateField (const char *endkey,
                    int irow, int icol, int irow2, int icol2);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


