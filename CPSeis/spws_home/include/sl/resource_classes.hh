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

//---------------------- resource_classes.hh --------------------//
//---------------------- resource_classes.hh --------------------//
//---------------------- resource_classes.hh --------------------//

//            header file for the ResourcePacket class
//            header file for the     BaseGR     class
//            header file for the     LongGR     class
//            header file for the    FloatGR     class
//            header file for the   DoubleGR     class
//            header file for the   StringGR     class
//                     subdirectory oprim


//  The suffix "GR" means "GUI Resource".

//  The main job of the ResourcePacket class is to maintain a
//  linked list of all GUI resources used by a GUI.

//  The main job of each of the other classes is to maintain a GUI
//  resource value which is set by the application, and optionally
//  automatically updated from program values or from user 
//  interaction with the GUI.

//  These classes know nothing about X-windows and Motif, or about 
//  the nature of the GUI.

//  These classes know nothing about the resources they maintain;
//  they only know the types of the resources (long, float, double,
//  or char*).


#ifndef _RESOURCE_CLASSES_HH_
#define _RESOURCE_CLASSES_HH_


#include <X11/Intrinsic.h>
#include "cprim.h"


//------------------- resource base class -------------------//
//------------------- resource base class -------------------//
//------------------- resource base class -------------------//

class BaseGR
{
protected:
  void   *_data;
  long    _flag;
public:
  BaseGR                      (void);        
  virtual ~BaseGR             (void);  
  virtual void updateValue    (Boolean force) = 0;
  Boolean         changed     (void) const  { return (_flag < 0); }
  Boolean updateNeverActivated(void) const;
};



//-------------- long, float, and double classes ---------------//
//-------------- long, float, and double classes ---------------//
//-------------- long, float, and double classes ---------------//

#define CLASS_DEF(LongGR, Destructor, long2)                       \
class LongGR : public BaseGR                                       \
{                                                                  \
private:                                                           \
  long2   _var;                                                    \
  long2   _old;                                                    \
  long2   _prev;                                                   \
  long2   _sysdef;                                                 \
  union                                                            \
    {                                                              \
    long2   value;                                                 \
    long2  *point;                                                 \
    long2 (*fun)(void *data);                                      \
    } _u;                                                          \
public:                                                            \
  LongGR                  (void);                                  \
  Destructor              (void);                                  \
  void   setNewValue      (long2 value);                           \
  long2  getValue         (void)   const   { return _var;     }    \
  long2  getOld           (void)   const   { return _old;     }    \
  long2  getPrev          (void)   const   { return _prev;    }    \
  long2  getSystemDefault (void)   const   { return _sysdef;  }    \
  void   setSystemDefault (long2 value)    { _sysdef = value; }    \
  void   setupValue       (long2   uvalue);                        \
  void   setupPoint       (long2  *upoint);                        \
  void   setupFun         (long2 (*ufun)(void *data), void *data); \
  virtual void updateValue (Boolean force);                        \
};

CLASS_DEF(  LongGR,   ~LongGR, long  )
CLASS_DEF( FloatGR,  ~FloatGR, float )
CLASS_DEF(DoubleGR, ~DoubleGR, double)

#undef CLASS_DEF


//------------------------ char* class --------------------------//
//------------------------ char* class --------------------------//
//------------------------ char* class --------------------------//

class StringGR : public BaseGR
{
private:
  long    _nvar;               // number of characters in _upoint.
  char   *_var;                // allocated
  char   *_old;                // allocated
  char   *_prev;               // allocated
  char   *_sysdef;             // allocated
  union
    {
    char   *value;
    char   *point;
    char *(*fun)(void *data);
    } _u;
  static char *_varcopy;           // allocated copy of _var
  static char *_oldcopy;           // allocated copy of _old
  static char *_prevcopy;          // allocated copy of _prev
  static char *_sysdefcopy;        // allocated copy of _sysdef
public:
  StringGR       (void);
  ~StringGR      (void);  
  void   setNewValue (char *value);
  char  *getValue    (void)      const
       { return _varcopy    = string_alloc(_varcopy   , _var   , 0); }
  char  *getOld      (void)      const
       { return _oldcopy    = string_alloc(_oldcopy   , _old   , 0); }
  char  *getPrev     (void)      const
       { return _prevcopy   = string_alloc(_prevcopy  , _prev  , 0); }
  char  *getSystemDefault    (void)      const
       { return _sysdefcopy = string_alloc(_sysdefcopy, _sysdef, 0); }
  void   setSystemDefault (char *value)
       { _sysdef = string_alloc(_sysdef, value, 0); }
  void   setupValue  (char  *uvalue);
  void   setupPoint  (char  *upoint, long nvar);
  void   setupFun    (char *(*ufun)(void *data), void *data);
  virtual void updateValue (Boolean force);
};

//------------------------ end of classes ---------------------//
//------------------------ end of classes ---------------------//
//------------------------ end of classes ---------------------//

#endif

//---------------------------- end ---------------------------------//
//---------------------------- end ---------------------------------//
//---------------------------- end ---------------------------------//

