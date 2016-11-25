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

//------------------------ prim_support.hh ---------------------//
//------------------------ prim_support.hh ---------------------//
//------------------------ prim_support.hh ---------------------//

//             header file for the PrimSupport class
//       not derived from any class (except virtual derivations)
//                       subdirectory sl
//            this class should not be instantiated
//           to be inherited as a second inheritance

//------------------- general information ----------------//
//------------------- general information ----------------//
//------------------- general information ----------------//

//  The main job of this class is to support primitive GUIs,
//  which may be widgets or widgetless.  This includes maintaining
//  GUI resource values which are set by the application, and 
//  optionally automatically updated from program values.

//  Five resource objects are inherited by this class.  These are
//  the sensitivity resource (type long, although treated as a
//  Boolean), plus one resource of each of four data types (long,
//  float, double, and char*).  All five of these resources can be
//  used in any manner needed.

//  At most one resource in a GUI can be an interactive resource,
//  which must be one of the four resources (except sensitivity)
//  mentioned above.

//  Although the above five resources are inherited by this class,
//  derived classes must specifically allocate the ones they need.
//  Derived classes can also inherited and allocate additional
//  resources if more than one are needed with the same data type.

//  This class knows nothing about X-windows and Motif, or about 
//  the nature of the GUIs, although its base class SLDelay does
//  understand widgets, etc.

//  This class knows nothing about the resources it maintains;
//  it only knows the names of the resources and the types
//  (long, float, double, or char*).


//---------------------- instructions --------------------------//
//---------------------- instructions --------------------------//
//---------------------- instructions --------------------------//
          // showing examples for IvarGuiResource

// To activate one of the resources supported by this class, and
// if this resource will NOT be an interactive resource, you must
// do these things in the class derived from this class:
//   (1) Allocate the resource object in the constructor by calling
//          createIvarResource();
//   (2) Override the appropriate function setIvarResource().
// Step (2) can be omitted if you override updateSelf() and do
// not call this function from updateSelf().
//
// If this IS an interactive resource, you must also do these things: 
//   (3) Override the appropriate function ivarResource().
//   (4) Override the appropriate function ivarDefault().
// Step (4) can be omitted if there is no way to get a default value,
// and you simply want to retain the current value.


// To add a new resource, not supported by this class, to a derived
// class, you must first do the following things.  The new resource
// cannot be an interactive one.
//   (A) Inherit the resource from resource_collection.hh
//          as is done in this class for five resources.
//   (B) Follow steps (1) and (2) above.



//---------------------- start ----------------------------------//
//---------------------- start ----------------------------------//
//---------------------- start ----------------------------------//

#ifndef _PRIM_SUPPORT_HH_
#define _PRIM_SUPPORT_HH_

#include "sl/resource_collection.hh"

typedef void FocusFun (void*data, long ident);
typedef void AtrapFun (void*data, long ident);
typedef void ItrapFun (void*data, long ident, long   oldvar, long   newvar);
typedef void FtrapFun (void*data, long ident, float  oldvar, float  newvar);
typedef void DtrapFun (void*data, long ident, double oldvar, double newvar);
typedef void CtrapFun (void*data, long ident, char  *oldvar, char  *newvar);

typedef void FocusFunF(           long*ident);
typedef void AtrapFunF(           long*ident);
typedef void ItrapFunF(           long*ident, long  *oldvar, long  *newvar);
typedef void FtrapFunF(           long*ident, float *oldvar, float *newvar);
typedef void DtrapFunF(           long*ident, double*oldvar, double*newvar);
typedef void CtrapFunF(           long*ident, char  *oldvar, char  *newvar);

class FamilyList;
class SLDelay;

class PrimSupport : virtual public   ResourceList,
                    virtual public SenseGuiResource,
                    virtual public  IvarGuiResource,
                    virtual public  FvarGuiResource,
                    virtual public  DvarGuiResource,
                    virtual public  CvarGuiResource
{

//------------------------- data ----------------------------//
//------------------------- data ----------------------------//
//------------------------- data ----------------------------//

public:

  enum { _INACTIVE, _ACTIVE, _LONG, _FLOAT, _DOUBLE, _CHAR };

protected:

  long  _ident;   // numeric ID for this GUI.
  long  _type;    // type of GUI (one of the above enums).

private:

  static FamilyList *_family; // linked list of all update functions.

  enum { _STATIC, _NOTIFY, _FORTRAN };
                                // distinquishes within union _t.

  long _traptype;      // type of trap (one of the above enums).
  void *_data;         // pointer to user data for interactive trap.
  void *_focusindata;  // pointer to user data for focusin     trap.
  void *_focusoutdata; // pointer to user data for focusout    trap.
  FocusFun  *_focusintrap  ;
  FocusFun  *_focusouttrap ;
  FocusFunF *_focusintrapf ;
  FocusFunF *_focusouttrapf;
  union
    {
    AtrapFun  *_atrap ;
    ItrapFun  *_itrap ;
    FtrapFun  *_ftrap ;
    DtrapFun  *_dtrap ;
    CtrapFun  *_ctrap ;
    AtrapFunF *_atrapf;
    ItrapFunF *_itrapf;
    FtrapFunF *_ftrapf;
    DtrapFunF *_dtrapf;
    CtrapFunF *_ctrapf;
    SLDelay *_notify;
    } _t;

//---------------- constructor and destructor ----------------------//
//---------------- constructor and destructor ----------------------//
//---------------- constructor and destructor ----------------------//

protected:

  PrimSupport (long ident, long type); 
  virtual    ~PrimSupport (void);  

//-------- functions needing replacement in derived classes --------//
//-------- functions needing replacement in derived classes --------//
//-------- functions needing replacement in derived classes --------//

// Also, some inherited functions need to be overridden.  Examples:
//      setIvarResource  ivarResource  ivarDefault

protected:

  //  The following function must be overridden only if the resources
  //  are not independent or should be set simultaneously, or if
  //  additional resources are inherited:

  virtual void updateSelf (Boolean force = FALSE); // overrides ResourcePacket

private:

  //  The following function must be overridden if something is to
  //  be done if an interactive _STATIC or _FORTRAN trap has not
  //  been registered:

  virtual Boolean trapHelper (SLDelay * /*target*/) { return TRUE; }

  //  Template for the overridden function:
  //
  //  Boolean trapHelper(SLDelay *target)
  //  {
  //    Boolean doit = TRUE;
  //    if(target)          doit =     target->notify(this);
  //    else if(slParent()) doit = slParent()->notify(this);
  //    if(doit) doit = notify(NULL);
  //    return doit;
  //  }
  //
  //  The above cannot be in this class because this class does
  //  not have access to the correct this pointer, or to slParent().
  //  The notify function is a virtual function in SLDelay with
  //  this prototype:  virtual Boolean notify(SLPrim *gui);



//-------------------- other functions -----------------------//
//-------------------- other functions -----------------------//
//-------------------- other functions -----------------------//

//public:   // whether to allow reloading of defaults

//  void allowReloadDefault       (Boolean allow = TRUE)
//              { _allow_reload_default        = allow; }
//  void allowReloadSystemDefault (Boolean allow = TRUE)
//              { _allow_reload_system_default = allow; }

public:   // manage update functions

  static void addUpdateFunction    (void(*fun)(void *data), void *data);
  static void removeUpdateFunction (void(*fun)(void *data), void *data);
  static void callUpdateFunctions  (void);
  static void updateEverything     (void);

public:   // miscellaneous

  long   id               (void)  const  { return _ident; }
  long   type             (void)  const  { return _type; }
  void   reloadSelf       (Boolean do_method = TRUE);
  void   reloadSystemSelf (Boolean do_method = TRUE);

public:     // register traps

  void setNotify       (SLDelay *target);
  void setFocusinTrap  (FocusFun  *trap, void *data);
  void setFocusoutTrap (FocusFun  *trap, void *data);
  void setAtrap        (AtrapFun  *trap, void *data);
  void setItrap        (ItrapFun  *trap, void *data);
  void setFtrap        (FtrapFun  *trap, void *data);
  void setDtrap        (DtrapFun  *trap, void *data);
  void setCtrap        (CtrapFun  *trap, void *data);

  void setFocusinTrap  (FocusFunF *trap);
  void setFocusoutTrap (FocusFunF *trap);
  void setAtrap        (AtrapFunF *trap);
  void setItrap        (ItrapFunF *trap);
  void setFtrap        (FtrapFunF *trap);
  void setDtrap        (DtrapFunF *trap);
  void setCtrap        (CtrapFunF *trap);

public:   // optional convenience functions for dealing with label

  char *label                (void)  const       { return cvar(); }
  void  setLabel             (char *x)           { setCvar(x); }
  void  setupLabelValue      (char *x)           { setupCvarValue(x); }
  void  setupLabelPoint      (char *x, long n)   { setupCvarPoint(x,n); }
  void  setupLabelFun (char*(*f)(void*), void*d) { setupCvarFun(f,d); }

protected:   // call traps

  void   callFocusinTrap  (void);
  void   callFocusoutTrap (void);
  void   callAtrap        (void);
  void   callItrap        (long   value, Boolean do_method = TRUE);
  void   callFtrap        (float  value, Boolean do_method = TRUE);
  void   callDtrap        (double value, Boolean do_method = TRUE);
  void   callCtrap        (char*  value, Boolean do_method = TRUE);

//------------------- get and set and setup values -------------//
//------------------- get and set and setup values -------------//
//------------------- get and set and setup values -------------//

//  These are all public inline pass-thru functions inherited
//  from resource_collection.hh.  See that file for details.
//  Other similar functions for additional resources will be
//     automatically available if additional resources are
//     inherited by derived classes.

//----------------------- end of functions --------------------------//
//----------------------- end of functions --------------------------//
//----------------------- end of functions --------------------------//

};

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
