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

//----------------------- wbox_vector.hh -------------------------//
//----------------------- wbox_vector.hh -------------------------//
//----------------------- wbox_vector.hh -------------------------//

//             header file for the WboxVector class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_VECTOR_HH_
#define _WBOX_VECTOR_HH_


class WboxBox;
class WboxLink;
class WboxPoint;


class WboxVector
{

//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//

public:

  typedef   void  WboxGenericTrap (void *box, long *ident, long *index,
                                    char *text, long *nread, char *endkey);
  typedef   void  WboxFortranTrap (int *ibox, int *ident, int *index,
                                    char *text, int *nread, char *endkey);
  typedef   void  WboxEzedTrap    (char *text, int *nread, int *index,
                                    int *ident, int *iexsw);
  typedef   void  WboxClangTrap   (void *box, long *ident, long *index,
                                    char *text, long *nread, char *endkey);
  typedef   void  WboxIvarTrap    (void *data, long ident, long index,
                                    long   ivar, long nread, char *endkey);
  typedef   void  WboxFvarTrap    (void *data, long ident, long index,
                                    float  fvar, long nread, char *endkey);
  typedef   void  WboxDvarTrap    (void *data, long ident, long index,
                                    double dvar, long nread, char *endkey);
  typedef   void  WboxCvarTrap    (void *data, long ident, long index,
                                    char  *cvar, long nread, char *endkey);

  typedef   char *WboxCupdateFun  (void *data, long ident, long index);
  typedef   long  WboxIupdateFun  (void *data, long ident, long index);
  typedef  float  WboxFupdateFun  (void *data, long ident, long index);
  typedef double  WboxDupdateFun  (void *data, long ident, long index);

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

public:

  enum ITYPE    { _IVAR, _FVAR, _DVAR, _CVAR };
  enum TRAPTYPE { _NOTRAP, _OLDF, _SIMP, _EZED, _OLDC, _NEWC };
  enum UPDATE   { _NONE, _POINT, _FUN, _RPOINT, _INDEX };

private:

  int         _ifield0;  // datafield number of prompt (or 0 if scalar).
  int         _ifield1;  // datafield number of first datafield in vector.
  ITYPE       _itype;    // type of variable displayed in datafield.
  TRAPTYPE    _traptype; // type of trap to call.
  UPDATE      _uptype;   // type of variable update to perform.
  UPDATE      _swtype;   // type of  switch  update to perform.

  WboxBox    *_box;      // pointer to windowbox structure.
  WboxLink   *_link;     // pointer to linked array set (NULL for scalars).

  union           // pointer to trap routine.
    {
    WboxFortranTrap *otrap;  // _traptype == _OLDF and _SIMP.
    WboxEzedTrap    *etrap;  // _traptype == _EZED.
    WboxClangTrap   *ttrap;  // _traptype == _OLDC.
    WboxIvarTrap    *itrap;  // _traptype == _NEWC, _itype == _IVAR.
    WboxFvarTrap    *ftrap;  // _traptype == _NEWC, _itype == _FVAR.
    WboxDvarTrap    *dtrap;  // _traptype == _NEWC, _itype == _DVAR.
    WboxCvarTrap    *ctrap;  // _traptype == _NEWC, _itype == _CVAR.
    } _trap;


  union              // pointer to variable or update routine.
    {
    int        *ipoint_int;  // _uptype == _POINT or _RPOINT, _itype == _IVAR.
    long           *ipoint;  // _uptype == _POINT or _RPOINT, _itype == _IVAR.
    float          *fpoint;  // _uptype == _POINT,  _itype == _FVAR.
    double         *dpoint;  // _uptype == _POINT,  _itype == _DVAR.
    char           *cpoint;  // _uptype == _POINT,  _itype == _CVAR.
    WboxIupdateFun *ifun;    // _uptype == _FUN,    _itype == _IVAR.
    WboxFupdateFun *ffun;    // _uptype == _FUN,    _itype == _FVAR.
    WboxDupdateFun *dfun;    // _uptype == _FUN,    _itype == _DVAR.
    WboxCupdateFun *cfun;    // _uptype == _FUN,    _itype == _CVAR.
    } _update;


  union              // pointer to switch value or update routine.
    {
    int        *spoint_int;  // _swtype == _POINT.
    long           *spoint;  // _swtype == _POINT.
    WboxIupdateFun *sfun;    // _swtype == _FUN.
    } _sw;

  short         _ident;  // variable identification.
  unsigned char _icol;   // column number of start of variable field.
  unsigned char _nchar;  // number of characters in variable field.
  unsigned char _ndec;   // maximum number of decimal places (float,double).
  unsigned char _length; // exact length of character variable in user area.

  unsigned char _use_ipoint_int;      // TRUE/FALSE.
  unsigned char _use_spoint_int;      // TRUE/FALSE.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:    // constructor and destructor.

  WboxVector (int ifield0, int ifield1, ITYPE itype, int ident,
              WboxBox *box, WboxLink *link,
              int icol, int nchar, int ndec = 0, int length = 0);

  virtual ~WboxVector();

public:    // get values.

  int         getIdent         ()  const  { return _ident   ; }
  WboxBox    *getBoxPointer    ()  const  { return _box     ; }
  WboxLink   *getLinkPointer   ()  const  { return _link    ; }
  int         getIcol          ()  const  { return _icol    ; }
  int         getNchar         ()  const  { return _nchar   ; }
  int         getNdec          ()  const  { return _ndec    ; }
  int         getLength        ()  const  { return _length  ; }
  int         getIfield0       ()  const  { return _ifield0 ; }
  int         getIfield1       ()  const  { return _ifield1 ; }
  ITYPE       getItype         ()  const  { return _itype   ; }
  TRAPTYPE    getTraptype      ()  const  { return _traptype; }
  UPDATE      getUptype        ()  const  { return _uptype  ; }
  UPDATE      getSwtype        ()  const  { return _swtype  ; }
  void       *getUpdatePointer ()  const;
  int         getIfield(int irow)  const;
  int         getIndex (int irow)  const;  // references linked array set.
  int         getN             ()  const;  // references linked array set.
  int         getNmax          ()  const;  // references linked array set.
  int         getIbox          ()  const;
  int         getItab          ()  const;

  int         isIvar           ()  const  { return (_itype == _IVAR ); }
  int         isFvar           ()  const  { return (_itype == _FVAR ); }
  int         isDvar           ()  const  { return (_itype == _DVAR ); }
  int         isCvar           ()  const  { return (_itype == _CVAR ); }
  int         isRadio          ()  const  { return (_uptype == _RPOINT); }
  int         isIndex          ()  const  { return (_uptype == _INDEX); }

public:     // registrations.

  void  registerNoTrap           ();
  void  registerGenericTrap      (WboxGenericTrap *trap, TRAPTYPE traptype);
  void  registerFortranTrap      (WboxFortranTrap *trap);
  void  registerSimpleTrap       (WboxFortranTrap *trap);
  void  registerEzedTrap         (WboxEzedTrap    *trap);
  void  registerClanguageTrap    (WboxClangTrap   *trap);
  void  registerIvarTrap         (WboxIvarTrap    *trap);
  void  registerFvarTrap         (WboxFvarTrap    *trap);
  void  registerDvarTrap         (WboxDvarTrap    *trap);
  void  registerCvarTrap         (WboxCvarTrap    *trap);

  void  registerSwitchPoint      (long            *point);
  void  registerIndexBehavior    ();
  void  registerRadioPoint       (long            *point);
  void  registerIvarPoint        (long            *point);
  void  registerFvarPoint        (float           *point);
  void  registerDvarPoint        (double          *point);
  void  registerCvarPoint        (char            *point);

  void  registerSwitchPointInt   (int             *point);
  void  registerRadioPointInt    (int             *point);
  void  registerIvarPointInt     (int             *point);

  void  registerSwitchFun        (WboxIupdateFun  *fun);
  void  registerIvarFun          (WboxIupdateFun  *fun);
  void  registerFvarFun          (WboxFupdateFun  *fun);
  void  registerDvarFun          (WboxDupdateFun  *fun);
  void  registerCvarFun          (WboxCupdateFun  *fun);

public:  // get values from user area (using pointer or update function).
         // put values into user area (using pointer only).

  int    getUserSwitch (int index)              const;
  void   getUserCvar   (int index, char *cvar)  const;
  int    getUserIvar   (int index)              const;
  float  getUserFvar   (int index)              const;
  double getUserDvar   (int index)              const;

  void getUserValue  (int index,
              int *ivar, float *fvar, double *dvar, char *cvar)  const;

  void putUserValue  (int index,
              int ivar, float fvar, double dvar, const char *cvar)  const;

  void copyUserValue (int index1, int index2)  const;

  void stepUserRadioValue (int index, const char *endkey)  const;

public:   // all of these are const functions.

  void   encodeValue (int ivar, float fvar, double dvar, const char *cvar,
                      char *textbuf)  const;

  int    decodeValue  (const char *textbuf,
            int *ivar, float *fvar, double *dvar, char *cvar)  const;

  int    validateText (const char *textbuf)  const;

  void   getAndEncodeUserValue (int index,       char *textbuf)  const;
  int    decodeAndPutUserValue (int index, const char *textbuf)  const;

  void callTrap (int ivar, float fvar, double dvar, const char *cvar,
                 const char *textbuf, int nread, char *endkey,
                 int *ident, int *index, int irow)  const;

private:

  int copyNoNull  (char *cvar2, const char *cvar1, int length)  const;
  int copyYesNull (char *cvar2, const char *cvar1, int length)  const;

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


