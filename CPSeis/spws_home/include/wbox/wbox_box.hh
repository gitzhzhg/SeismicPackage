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

//----------------------- wbox_box.hh -------------------------//
//----------------------- wbox_box.hh -------------------------//
//----------------------- wbox_box.hh -------------------------//

//              header file for the WboxBox class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_BOX_HH_
#define _WBOX_BOX_HH_

#include "wproc.h"


class WboxBox
{

//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//

  typedef const char *WboxHardcopy
                 (void *data, int numlines, const char *default_filename);

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

  typedef   long  WboxIupdateFun  (void *data, long ident, long index);
  typedef  float  WboxFupdateFun  (void *data, long ident, long index);
  typedef double  WboxDupdateFun  (void *data, long ident, long index);
  typedef   char *WboxCupdateFun  (void *data, long ident, long index);
  typedef   long  WboxNupdateFun  (void *data);

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  enum { WBOX_BSIZE = 50 };

  int               _ibox;
  WboxGenericTrap  *_default_trap;
  int               _default_traptype;   // enum in WboxVector class.

  Widget     _w;
  Widget     _parent;
  Widget     _shellchild;
  Widget     _shell;
  Widget     _wtiny;
  char       _boxname[WBOX_BSIZE];
  void      *_userdata;

  class WboxAllbox   *_allbox;
  class WboxScreen   *_screen;
  class WboxControl  *_control;
  class ArrayBucket  *_vector_list;
  class ArrayBucket  *_field_list;
  class ArrayBucket  *_link_list;
  class WboxLink     *_link_being_created;    // NULL if not being created.
  class WboxField    *_field_being_created;   // NULL if not being created.
  class WboxVector   *_vector_being_created;  // NULL if not being created.

  int    _creating;    // whether this box is in process of being created.
  int    _irnext;
  int    _icnext;
  int    _jcnext;
  int    _icnext9;

  int    _vmin;        // minimum index (>=1) of WboxVector object.
  int    _vmax;        // maximum index (>=1) of WboxVector object.
  int    _kmin;        // minimum index (>=1) of WboxField object.
  int    _kmax;        // maximum index (>=1) of WboxField object.
  int    _lmin;        // minimum index (>=1) of WboxLink object.
  int    _lmax;        // maximum index (>=1) of WboxLink object.
  int    _focusflag;   // whether box has the focus (1=yes, 0=no).
  int    _managedflag; // whether box is managed    (1=yes, 0=no, -1=no).
  int    _nrow;        // number of rows    (can change).
  int    _ncol;        // number of columns (can change).
  int    _starting_nrow;  // initial number of rows.
  int    _starting_ncol;  // initial number of columns.
  int    _omit;        // whether to omit the bottom one or two message rows.
  int    _nrow_init;   // restrict initial number of rows if >= 0.


//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:    // constructor and destructor.

  WboxBox (WboxAllbox *allbox, int ibox,
           WboxGenericTrap *default_trap, int default_traptype,
           int omit = 0, int nrow_init = -1);

  virtual ~WboxBox();


public:  // create new linked array sets.
         // these must be called before calling completeCanvas.

         // pointers to variables or update functions can be NULL.
         // pointers to variables or update functions can be registered
         //   separately, by calling WboxBox functions and specifying
         //   the ident of any array in the set.

  void createLinkedArraySet (int irow, int numrow, int numrow_init = 0);

  void createLinkedArraySet (int irow, int numrow,
                             long *npoint, long *nmaxpoint,
                             int numrow_init = 0);

  void createLinkedArraySet (int irow, int numrow,
                             int *npoint, int *nmaxpoint,
                             int numrow_init = 0);

  void createLinkedArraySet (int irow, int numrow,
                             WboxNupdateFun *nfun, WboxNupdateFun *nmaxfun,
                             int numrow_init = 0);


public:  // create new datafields.
         // these must be called before calling completeCanvas.
         // traps, pointers to variables or switches, and update functions
         //   are to be registered separately, by calling WboxBox functions
         //   and specifying the ident.

         // specified idents must normally be > 0 and must be unique
         //   within the WboxBox object.
         // the ident of a prompt is the negative of the specified ident.
         // one of more specified idents can be 0 only if they will not
         //   be referenced by any subsequent calls to WboxBox.  this
         //   usually is done when everything necessary is specified in
         //   the datafield creation call for a single character datafield
         //   with pointers to the string and the switch, and no trap.


  void createIvarDatafield       (int ident, int irow, int icol,
                                  int nchar);

  void createFvarDatafield       (int ident, int irow, int icol,
                                  int nchar, int ndec);

  void createDvarDatafield       (int ident, int irow, int icol,
                                  int nchar, int ndec);

  void createCvarDatafield       (int ident, int irow, int icol,
                                  int nchar, int length);



  void createIvarDatafieldPair   (int ident, int irow, int icol,
                                  int ncharp, int lengthp,
                                  int nchar);

  void createFvarDatafieldPair   (int ident, int irow, int icol,
                                  int ncharp, int lengthp,
                                  int nchar, int ndec);

  void createDvarDatafieldPair   (int ident, int irow, int icol,
                                  int ncharp, int lengthp,
                                  int nchar, int ndec);

  void createCvarDatafieldPair   (int ident, int irow, int icol,
                                  int ncharp, int lengthp,
                                  int nchar, int length);

  void createIvarDatafieldRevPair(int ident, int irow, int icol,
                                  int ncharp, int lengthp,
                                  int nchar);



  void createIvarDatafieldArray  (int ident, int icol,
                                  int ncharp, int lengthp,
                                  int nchar);

  void createFvarDatafieldArray  (int ident, int icol,
                                  int ncharp, int lengthp,
                                  int nchar, int ndec);

  void createDvarDatafieldArray  (int ident, int icol,
                                  int ncharp, int lengthp,
                                  int nchar, int ndec);

  void createCvarDatafieldArray  (int ident, int icol,
                                  int ncharp, int lengthp,
                                  int nchar, int length);


public:    // registrations.
           // the vector with matching ident will be affected.
  // (i.e. all datafields with matching ident will be affected).
  // (or the linked array set containing a vector with matching ident).

  // (if there is a default trap, it is registered at datafield creation time).
  // (the initial trap actually has type given by _default_traptype).
  // (if the initial trap is NULL, the previously-registered trap is retained.)

  void  registerHardcopy         (WboxHardcopy *hardcopy);

  void  registerGenericTrap      (int ident, WboxGenericTrap *ttrap );
  void  registerFortranTrap      (int ident, WboxFortranTrap *ttrap );
  void  registerSimpleTrap       (int ident, WboxFortranTrap *ttrap );
  void  registerEzedTrap         (int ident, WboxEzedTrap    *etrap );
  void  registerClanguageTrap    (int ident, WboxClangTrap   *ttrap );
  void  registerIvarTrap         (int ident, WboxIvarTrap    *itrap );
  void  registerFvarTrap         (int ident, WboxFvarTrap    *ftrap );
  void  registerDvarTrap         (int ident, WboxDvarTrap    *dtrap );
  void  registerCvarTrap         (int ident, WboxCvarTrap    *ctrap );

  void  registerNPoint           (int ident, long           *npoint);
  void  registerNmaxPoint        (int ident, long           *nmaxpoint);
  void  registerSwitchPoint      (int ident, long           *spoint);
  void  registerIndexBehavior    (int ident);
  void  registerRadioPoint       (int ident, long           *ipoint);
  void  registerIvarPoint        (int ident, long           *ipoint);
  void  registerFvarPoint        (int ident, float          *fpoint);
  void  registerDvarPoint        (int ident, double         *dpoint);
  void  registerCvarPoint        (int ident, char           *cpoint);

  void  registerNPointInt        (int ident, int            *npoint);
  void  registerNmaxPointInt     (int ident, int            *nmaxpoint);
  void  registerSwitchPointInt   (int ident, int            *spoint);
  void  registerRadioPointInt    (int ident, int            *ipoint);
  void  registerIvarPointInt     (int ident, int            *ipoint);

  void  registerNFun             (int ident, WboxNupdateFun *nfun  );
  void  registerNmaxFun          (int ident, WboxNupdateFun *nmaxfun);
  void  registerSwitchFun        (int ident, WboxIupdateFun *sfun  );
  void  registerIvarFun          (int ident, WboxIupdateFun *ifun  );
  void  registerFvarFun          (int ident, WboxFupdateFun *ffun  );
  void  registerDvarFun          (int ident, WboxDupdateFun *dfun  );
  void  registerCvarFun          (int ident, WboxCupdateFun *cfun  );


public:    // get values.

  Window       getWindow         ()  const;
  int          getIbox           ()  const  { return _ibox; }
  Widget       getW              ()  const  { return _w; }
  Widget       getParent         ()  const  { return _parent; }
  Widget       getShellChild     ()  const  { return _shellchild; }
  Widget       getShell          ()  const  { return _shell; }
  Widget       getTinyPushbutton ()  const  { return _wtiny; }
  const char  *getBoxName        ()  const  { return _boxname; }
  void        *getUserData       ()  const  { return _userdata; }

  WboxScreen  *getScreenPointer      () const { return _screen; }
  WboxAllbox  *getAllboxPointer      () const { return _allbox; }
  WboxLink    *getLinkBeingCreated   () const { return _link_being_created; }
  WboxField   *getFieldBeingCreated  () const { return _field_being_created; }

  WboxField   *getActiveFieldPointer     ()             const;
  WboxVector  *getVectorPointer          (int ivector)  const;
  WboxField   *getFieldPointer           (int ifield )  const;
  WboxLink    *getLinkPointer            (int itab   )  const;
  WboxVector  *findVectorPointer         (int ident  )  const;
  WboxField   *findFieldPointer  (int irow, int icol, int min_switch)  const;

  int          getVmin         ()  const  { return _vmin; }
  int          getVmax         ()  const  { return _vmax; }
  int          getLmin         ()  const  { return _lmin; }
  int          getLmax         ()  const  { return _lmax; }
  int          getKmin         ()  const  { return _kmin; }
  int          getKmax         ()  const  { return _kmax; }
  int          getNrow         ()  const  { return _nrow; }
  int          getNcol         ()  const  { return _ncol; }
  int          getStartingNrow ()  const  { return _starting_nrow; }
  int          getStartingNcol ()  const  { return _starting_ncol; }
  int          getOmitFlag     ()  const  { return _omit; }

  int          hasFocus        ()  const  { return (_focusflag   >= 1); }
  int          isManaged       ()  const  { return (_managedflag >= 1); }
  int          hasLinkedArrays ()  const  { return (_lmax >= _lmin); }
  int          numBottomRows   ()  const;

public:    // misc functions.

  void  setUserData  (void *userdata)  { _userdata = userdata; }

  int   identIsVisible         (int ident)                       const;
  void  updateSensitivity();
  void  setFocus               (int ident, int index);
  void  sendClientMessageEvent (const char *endkey)              const;
  void  sendImmediateEvent     (const char *endkey);

  void  gainFocus              ();
  void  manage                 ();
  void  unmanage               ();
  void  destroy                ();

  void  eraseFields            ();
  void  updateArrayVisibilities();
  void  updateScrollbars       ();
  void  showMessage            (const char *msg);
  void  maybeShowMessage       (const char *msg);

  void  saveTable               (const char *filename = NULL)  const;

  void  updateFields     (const char *endkey,
                          int irow, int icol, int irow2, int icol2);
  void  boxtrap          (const char *charr, const char *endkey,
                          int irow, int icol, int irow2, int icol2);

  void  restorePreviousUserValue (int *ident, int *index, int *istat);


public:    // complete the canvas after registrations are finished.
           // called only from WboxAllbox.

  void completeCanvas (WboxScreen *screen,
                       const char *boxname, Widget parent, Widget w,
                       HelpCtx hctx, Widget toplevel,
                       const char *helpfile, const char *helptitle);


private:

  WboxLink   *createLink  (int irow, int numrow, int numrow_init);

  WboxVector *createVector (int itype, int ident, WboxLink *link,
                            int icol, int nchar, int ndec, int length);

  void  createDatafield (WboxVector *vector, int irow);

  void  createDatafieldArray (int itype, int ident, int icol,
                  int ncharp, int lengthp, int nchar, int ndec, int length);

  void  prepareCanvasSize     ();
  void  prepareDrawingArea    (Widget parent, Widget w);
  void  prepareTinyPushbutton ();

  void  reconfigure      (int irow2, int icol2);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


