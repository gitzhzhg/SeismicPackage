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

//----------------------- resource_collection.hh ---------------------//
//----------------------- resource_collection.hh ---------------------//
//----------------------- resource_collection.hh ---------------------//

//                header file for resources for GUIs
//                         subdirectory oprim
//                 there is no implementation file

#ifndef _RESOURCE_COLLECTION_HH_
#define _RESOURCE_COLLECTION_HH_

#include <iostream.h>
#include "sl/resource_classes.hh"
#include "sl/resource_list.hh"

//------------------ general information --------------------------//
//------------------ general information --------------------------//
//------------------ general information --------------------------//

//  This file contains a class for each named resource which might
//  be used in a GUI.  An individual GUI class (specifically the class
//  GuiSupport, or one of its derived classes) must inherit one of these
//  classes for each resource it needs.

//  These classes know nothing about X-windows and Motif, or about
//  the nature of the resources, or the GUIs which inherit them.  The
//  true nature of any resource is determined by how it is used, and
//  by how the virtual functions are overridden by the GUI class
//  which inherits the resource.

//  Each class in this header file contains the following:
//    -- a pointer to a private GUI resource object.
//    -- associated protected virtual   functions.
//    -- associated protected pass-thru functions.
//    -- associated public    pass-thru functions.

//     class to be inherited    pointer to GUI resource object
//     ---------------------    ------------------------------
//       SenseGuiResource            LongGR _sense 
//        IvarGuiResource            LongGR _ivar
//        FvarGuiResource           FloatGR _fvar
//        DvarGuiResource          DoubleGR _dvar
//        CvarGuiResource          StringGR _cvar 
//        IminGuiResource            LongGR _imin 
//        ImaxGuiResource            LongGR _imax 
//       IstepGuiResource            LongGR _istep
//       IpageGuiResource            LongGR _ipage
//      SliderGuiResource            LongGR _slider
//        FminGuiResource           FloatGR _fmin 
//        FmaxGuiResource           FloatGR _fmax 
//        DminGuiResource          DoubleGR _dmin 
//        DmaxGuiResource          DoubleGR _dmax 


//---------------------- instructions --------------------------//
//---------------------- instructions --------------------------//
//---------------------- instructions --------------------------//
          // showing examples for IvarGuiResource

// To add one of these resources to a derived class, you must
// do the following things:
//
//   (1) Virtually inherit the resource.  This step can be done
//       for several resources, some of which may or may not be
//       actually needed, if this simplifies your efforts.  For
//       example, the methods are safe to use even if the resource
//       itself has not been allocated.
//
//   (2) Allocate the resource object in the constructor by calling
//       createIvarResource().  To save memory, this need not be done
//       unless you know that the resource is actually needed.
//
//   (3) Override setIvarResource().  This step can be omitted if
//       this is not an interactive resource, and you override
//       updateSelf() [in PrimSupport] and do not call this
//       function from updateSelf().
//
//   (4) If this is an interactive resource, you must override
//       ivarResource() if it is to be called from a callback
//       or event handler.
//
//   (5) If this is an interactive resource, you must override
//       ivarDefault() unless there is no way to get a default
//       value, and you simply want to retain the current value.
//
// NOTE: For string resources, the functions cvarResource()
// and cvarDefault() must return a pointer to static memory.


//------------------- public pass-thru functions ----------------//
//------------------- public pass-thru functions ----------------//
//------------------- public pass-thru functions ----------------//
         // showing examples for the first five resources

// Get and set system defaults:
//   The value returned by cvarSystemDefault() points to an
//   internally-allocated place which must not be de-allocated.
//   This place will be freed and reallocated on future calls to 
//   cvarSystemDefault().

   //  long    senseSystemDefault (void)   const;
   //  long    ivarSystemDefault  (void)   const;
   //  float   fvarSystemDefault  (void)   const;
   //  double  dvarSystemDefault  (void)   const;
   //  char   *cvarSystemDefault  (void)   const;

   //  void    setSenseSystemDefault (long   x);
   //  void    setIvarSystemDefault  (long   x);
   //  void    setFvarSystemDefault  (float  x);
   //  void    setDvarSystemDefault  (double x);
   //  void    setCvarSystemDefault  (char  *x);

// Get values:
//   The value returned by cvar() points to an internally-allocated
//   place which must not be de-allocated.  This place will be
//   freed and reallocated on future calls to cvar().

   //  long       sense (void)   const;
   //  long       ivar  (void)   const;
   //  float      fvar  (void)   const;
   //  double     dvar  (void)   const;
   //  char   *   cvar  (void)   const;

// Set values immediately:
//   These calls also deactivate future automatic updates
//      for this specific resource.
//   All resources for this GUI will be reset as necessary
//      (not just the resource in this argument list), since
//      this call also calls updateSelf().

   //  void setSense (long   x);
   //  void setIvar  (long   x);
   //  void setFvar  (float  x);
   //  void setDvar  (double x);
   //  void setCvar  (char  *x);

// Setup values for automatic updating:

   //  void  setupSenseValue (long   x);
   //  void  setupIvarValue  (long   x);
   //  void  setupFvarValue  (float  x);
   //  void  setupDvarValue  (double x);
   //  void  setupCvarValue  (char  *x);

   //  void  setupSensePoint (long   *x);
   //  void  setupIvarPoint  (long   *x);
   //  void  setupFvarPoint  (float  *x);
   //  void  setupDvarPoint  (double *x);
   //  void  setupCvarPoint  (char   *x);

   //  void  setupSenseFun (long   (*x)(void*));
   //  void  setupIvarFun  (long   (*x)(void*));
   //  void  setupFvarFun  (float  (*x)(void*));
   //  void  setupDvarFun  (double (*x)(void*));
   //  void  setupCvarFun  (char*  (*x)(void*));


//----------------- macro to define a class ---------------------//
//----------------- macro to define a class ---------------------//
//----------------- macro to define a class ---------------------//

#define ARG1   , long nvar
#define ARG2   , nvar
#define NARG

#define                                                            \
CLASS(      IvarGuiResource,   LongGR, _ivar, long2, long3, zero,  \
      createIvarResource,             changedIvar,                 \
         setIvarResource, setupIvarValue, setIvar,  setNewIvar,    \
            ivarResource, setupIvarPoint,    ivar,     prevIvar,   \
            ivarDefault , setupIvarFun, ARG1, ARG2,    oldIvar,    \
         updateIvarNeverActivated,                                 \
         setIvarSystemDefault,    ivarSystemDefault)               \
                                                                   \
class IvarGuiResource :  virtual public ResourceList               \
{                                                                  \
private:                                                           \
                                                                   \
  LongGR *_ivar;                                                   \
                                                                   \
protected:                                                         \
                                                                   \
  IvarGuiResource (void) :   _ivar(NULL)  {}                       \
                                                                   \
  virtual ~IvarGuiResource (void) { if(_ivar) delete _ivar; }      \
                                                                   \
  void createIvarResource()                                        \
         { _ivar = new LongGR(); add(_ivar); }                     \
                                                                   \
  void goof(char *name) const                                      \
         { if(!_ivar) cout << "calling " << name <<                \
               "() when the resource does not exist" << endl; }    \
                                                                   \
  virtual void  setIvarResource (void) {}                          \
                                                                   \
  virtual long2  ivarResource (void) const                         \
         { goof("ivarResource");  return ivar(); }                 \
                                                                   \
  virtual long2  ivarDefault  (void) const                         \
         { goof("ivarDefault");   return ivar(); }                 \
                                                                   \
  long2          ivarSystemDefault (void) const                    \
         { goof("ivarSystemDefault");                              \
           if(_ivar) return _ivar->getSystemDefault();             \
           else return zero; }                                     \
                                                                   \
  long2          prevIvar (void) const                             \
         { goof("prevIvar");                                       \
           if(_ivar) return _ivar->getPrev();                      \
           else return zero; }                                          \
                                                                   \
  void           setNewIvar(long2 value)                           \
         { goof("setNewIvar");                                     \
           if(_ivar) _ivar->setNewValue(value); }                  \
                                                                   \
  Boolean changedIvar(void) const                                  \
         { if(_ivar) return _ivar->changed();                      \
           else      return FALSE; }                               \
                                                                   \
  Boolean        updateIvarNeverActivated(void) const              \
         { goof("updateIvarNeverActivated");                       \
           if(_ivar) return _ivar->updateNeverActivated();         \
           else return TRUE; }                                     \
                                                                   \
public:                                                            \
                                                                   \
  long2          oldIvar (void)   const                            \
         { goof("oldIvar");                                        \
           if(_ivar) return _ivar->getOld();                       \
           else return zero; }                                     \
                                                                   \
  long2          ivar(void)   const                                \
         { goof("ivar");                                           \
           if(_ivar) return _ivar->getValue();                     \
           else return zero; }                                     \
                                                                   \
  void           setIvar(long2 value)                              \
         { goof("setIvar");                                        \
           if(_ivar) { _ivar->setupValue(value); updateSelf(); } } \
                                                                   \
  void           setupIvarValue (long2 value)                      \
         { goof("setupIvarValue");                                 \
           if(_ivar) _ivar->setupValue(value); }                   \
                                                                   \
  void           setupIvarPoint (long3 *point      ARG1)           \
         { goof("setupIvarPoint");                                 \
           if(_ivar) _ivar->setupPoint(point      ARG2); }         \
                                                                   \
  void           setupIvarFun   (long2 (*fun)(void*), void *data)  \
         { goof("setupIvarFun");                                   \
           if(_ivar) _ivar->setupFun  (fun, data); }               \
                                                                   \
  void           setIvarSystemDefault(long2 value)                 \
         { goof("setIvarSystemDefault");                           \
           if(_ivar) { _allow_reload_system_default = TRUE;        \
                       _ivar->setSystemDefault(value); } }         \
} ;


//------------------ classes to be inherited ------------------//
//------------------ classes to be inherited ------------------//
//------------------ classes to be inherited ------------------//

CLASS(      SenseGuiResource,   LongGR, _sense, long, long, 1,
      createSenseResource,              changedSense,
         setSenseResource, setupSenseValue, setSense, setNewSense,
            senseResource, setupSensePoint,    sense,    prevSense,
            senseDefault , setupSenseFun, NARG, NARG,    oldSense,
      updateSenseNeverActivated,
         setSenseSystemDefault,    senseSystemDefault)

CLASS(      IvarGuiResource,   LongGR, _ivar, long, long, 1,
      createIvarResource,             changedIvar, 
         setIvarResource, setupIvarValue, setIvar,  setNewIvar,
            ivarResource, setupIvarPoint,    ivar,     prevIvar,
            ivarDefault , setupIvarFun, NARG, NARG,    oldIvar,
      updateIvarNeverActivated,
         setIvarSystemDefault,    ivarSystemDefault)

CLASS(      FvarGuiResource,  FloatGR, _fvar, float, float, 0.0,
      createFvarResource,             changedFvar,
         setFvarResource, setupFvarValue, setFvar,  setNewFvar,
            fvarResource, setupFvarPoint,    fvar,     prevFvar,
            fvarDefault , setupFvarFun, NARG, NARG,    oldFvar,
      updateFvarNeverActivated,
         setFvarSystemDefault,    fvarSystemDefault)

CLASS(      DvarGuiResource, DoubleGR, _dvar, double, double, 0.0,
      createDvarResource,             changedDvar,
         setDvarResource, setupDvarValue, setDvar,  setNewDvar,
            dvarResource, setupDvarPoint,    dvar,     prevDvar,
            dvarDefault , setupDvarFun, NARG, NARG,    oldDvar,
      updateDvarNeverActivated,
         setDvarSystemDefault,    dvarSystemDefault)

CLASS(      CvarGuiResource, StringGR, _cvar, char*, char, NULL,
      createCvarResource,             changedCvar,
         setCvarResource, setupCvarValue, setCvar,  setNewCvar,
            cvarResource, setupCvarPoint,    cvar,     prevCvar,
            cvarDefault , setupCvarFun, ARG1, ARG2,    oldCvar,
      updateCvarNeverActivated,
         setCvarSystemDefault,    cvarSystemDefault)

CLASS(      IminGuiResource,   LongGR, _imin, long, long, 0,
      createIminResource,             changedImin,
         setIminResource, setupIminValue, setImin,  setNewImin,
            iminResource, setupIminPoint,    imin,     prevImin,
            iminDefault , setupIminFun, NARG, NARG,    oldImin,
      updateIminNeverActivated,
         setIminSystemDefault,    iminSystemDefault)

CLASS(      ImaxGuiResource,   LongGR, _imax, long, long, 0,
      createImaxResource,             changedImax,
         setImaxResource, setupImaxValue, setImax,  setNewImax,
            imaxResource, setupImaxPoint,    imax,     prevImax,
            imaxDefault , setupImaxFun, NARG, NARG,    oldImax,
      updateImaxNeverActivated,
         setImaxSystemDefault,    imaxSystemDefault)

CLASS(      IstepGuiResource,   LongGR, _istep, long, long, 0,
      createIstepResource,              changedIstep,
         setIstepResource, setupIstepValue, setIstep, setNewIstep,
            istepResource, setupIstepPoint,    istep,    prevIstep,
            istepDefault , setupIstepFun, NARG, NARG,    oldIstep,
      updateIstepNeverActivated,
         setIstepSystemDefault,    istepSystemDefault)

CLASS(      IpageGuiResource,   LongGR, _ipage, long, long, 0,
      createIpageResource,              changedIpage,
         setIpageResource, setupIpageValue, setIpage, setNewIpage,
            ipageResource, setupIpagePoint,    ipage,    prevIpage,
            ipageDefault , setupIpageFun, NARG, NARG,    oldIpage,
      updateIpageNeverActivated,
         setIpageSystemDefault,    ipageSystemDefault)

CLASS(      SliderGuiResource,   LongGR, _slider, long, long, 0,
      createSliderResource,               changedSlider,
         setSliderResource, setupSliderValue, setSlider, setNewSlider,
            sliderResource, setupSliderPoint,    slider,    prevSlider,
            sliderDefault , setupSliderFun, NARG, NARG,     oldSlider,
      updateSliderNeverActivated,
         setSliderSystemDefault,    sliderSystemDefault)

CLASS(      FminGuiResource,  FloatGR, _fmin, float, float, 0.0,
      createFminResource,             changedFmin,
         setFminResource, setupFminValue, setFmin,  setNewFmin,
            fminResource, setupFminPoint,    fmin,     prevFmin,
            fminDefault , setupFminFun, NARG, NARG,    oldFmin,
      updateFminNeverActivated,
         setFminSystemDefault,    fminSystemDefault)

CLASS(      FmaxGuiResource,  FloatGR, _fmax, float, float, 0.0,
      createFmaxResource,             changedFmax,
         setFmaxResource, setupFmaxValue, setFmax,  setNewFmax,
            fmaxResource, setupFmaxPoint,    fmax,     prevFmax,
            fmaxDefault , setupFmaxFun, NARG, NARG,    oldFmax,
      updateFmaxNeverActivated,
         setFmaxSystemDefault,    fmaxSystemDefault)

CLASS(      DminGuiResource, DoubleGR, _dmin, double, double, 0,
      createDminResource,             changedDmin,
         setDminResource, setupDminValue, setDmin,  setNewDmin,
            dminResource, setupDminPoint,    dmin,     prevDmin,
            dminDefault , setupDminFun, NARG, NARG,    oldDmin,
      updateDminNeverActivated,
         setDminSystemDefault,    dminSystemDefault)

CLASS(      DmaxGuiResource, DoubleGR, _dmax, double, double, 0,
      createDmaxResource,             changedDmax,
         setDmaxResource, setupDmaxValue, setDmax,  setNewDmax,
            dmaxResource, setupDmaxPoint,    dmax,     prevDmax,
            dmaxDefault , setupDmaxFun, NARG, NARG,    oldDmax,
      updateDmaxNeverActivated,
         setDmaxSystemDefault,    dmaxSystemDefault)



//--------------------- end of classes -------------------------//
//--------------------- end of classes -------------------------//
//--------------------- end of classes -------------------------//

#undef ARG1
#undef ARG2
#undef NARG
#undef CLASS

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//

