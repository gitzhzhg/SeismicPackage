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

//---------------------- resource_classes.cc ------------------------//
//---------------------- resource_classes.cc ------------------------//
//---------------------- resource_classes.cc ------------------------//

//        implementation file for the ResourcePacket class
//        implementation file for the     BaseGR     class
//        implementation file for the     LongGR     class
//        implementation file for the    FloatGR     class
//        implementation file for the   DoubleGR     class
//        implementation file for the   StringGR     class
//                     subdirectory oprim


#include "sl/resource_classes.hh"
#include "named_constants.h"


//--------------------- resource classes ----------------------//
//--------------------- resource classes ----------------------//
//--------------------- resource classes ----------------------//

enum { UPDATE_FROM_VALUE = 1, UPDATE_FROM_POINTER,
       UPDATE_FROM_FUNCTION,  UPDATE_DEACTIVATED,
       UPDATE_NEVER_ACTIVATED };

char *StringGR::_varcopy    = NULL;
char *StringGR::_oldcopy    = NULL;
char *StringGR::_prevcopy   = NULL;
char *StringGR::_sysdefcopy = NULL;


//------------------- constructors -------------------------------//
//------------------- constructors -------------------------------//
//------------------- constructors -------------------------------//

  BaseGR::BaseGR  (void) : 
            _data(NULL), _flag(UPDATE_NEVER_ACTIVATED) {}

  LongGR::LongGR  (void) :  BaseGR(),
            _var(1),    _old(0),    _prev(0),    _sysdef(1)    {}

 FloatGR::FloatGR (void) :  BaseGR(),
            _var(1.0),  _old(0.0),  _prev(0.0),  _sysdef(1.0)  {}

DoubleGR::DoubleGR(void) :  BaseGR(),
            _var(1.0),  _old(0.0),  _prev(0.0),  _sysdef(1.0)  {}

StringGR::StringGR(void) :  BaseGR(),
  _nvar(0), _var(NULL), _old(NULL), _prev(NULL), _sysdef(NULL) {}


//------------------------ destructors ----------------------------//
//------------------------ destructors ----------------------------//
//------------------------ destructors ----------------------------//

  BaseGR::~BaseGR   (void)   {}
  LongGR::~LongGR   (void)   {}
 FloatGR::~FloatGR  (void)   {}
DoubleGR::~DoubleGR (void)   {}

StringGR::~StringGR (void)
{
  string_free(_var );
  string_free(_old );
  string_free(_prev);
  string_free(_sysdef);
  if(_flag == UPDATE_FROM_VALUE) string_free(_u.value);
}


//----------------- update never activated ---------------------//
//----------------- update never activated ---------------------//
//----------------- update never activated ---------------------//

Boolean BaseGR::updateNeverActivated(void) const
{
  return (_flag == UPDATE_NEVER_ACTIVATED);
}



//------------- update value from application -------------------//
//------------- update value from application -------------------//
//------------- update value from application -------------------//

//  After calling this function, the following steps should be taken
//  if the value has been changed, or if you are doing a forced update:
//    (1) Find out whether the value changed by calling changed().
//    (2) If changed, get the new value by calling getValue().
//    (3) Reset the new value into the GUI by your own means.


#define UPDATE_VALUE(LongGR, long2)                              \
void LongGR::updateValue(Boolean force)                          \
{                                                                \
  long2 value;                                                   \
  _flag = AbsoluteValue(_flag);                                  \
  switch(_flag)                                                  \
     {                                                           \
     case UPDATE_FROM_VALUE:      _flag = UPDATE_DEACTIVATED;    \
                                  value = _u.value;              \
                                  break;                         \
                                                                 \
     case UPDATE_FROM_POINTER:    value = *_u.point;             \
                                  break;                         \
                                                                 \
     case UPDATE_FROM_FUNCTION:   value = _u.fun(_data);         \
                                  break;                         \
                                                                 \
     case UPDATE_DEACTIVATED:     if(force) _flag = -_flag;      \
                                  return;                        \
                                                                 \
     case UPDATE_NEVER_ACTIVATED: return;                        \
                                                                 \
     default:                     return;                        \
     }                                                           \
  if(force) _flag = -_flag;                                      \
  if(_var == value) return;                                      \
  _var = value;                                                  \
  _flag = -AbsoluteValue(_flag);                                 \
}

UPDATE_VALUE(  LongGR, long  )
UPDATE_VALUE( FloatGR, float )
UPDATE_VALUE(DoubleGR, double)


void StringGR::updateValue(Boolean force)
{
  char* value;
  long nvar;
  _flag = AbsoluteValue(_flag);
  switch(_flag)
     {      
     case UPDATE_FROM_VALUE:      _flag = UPDATE_DEACTIVATED; 
                                  value = _u.value;
                                  nvar  = 0;
                                  break;

     case UPDATE_FROM_POINTER:    value = _u.point;
                                  nvar  = _nvar;
                                  break;

     case UPDATE_FROM_FUNCTION:   value = _u.fun(_data);
                                  nvar  = 0;
                                  break;

     case UPDATE_DEACTIVATED:     if(force) _flag = -_flag;
                                  return;

     case UPDATE_NEVER_ACTIVATED: return;

     default:                     return;
     }
  if(force) _flag = -_flag;
/*
  if(!string_compare(_var, 0, value, nvar)) return;
  _var = string_alloc(_var, value, nvar);
  _flag = -AbsoluteValue(_flag);
  if(_flag == UPDATE_DEACTIVATED) string_free(_u.value);
*/
////// above commented code replaced by the following code 4/7/95
  if(string_compare(_var, 0, value, nvar))
      {
      _var = string_alloc(_var, value, nvar);
      _flag = -AbsoluteValue(_flag);
      }
  if(AbsoluteValue(_flag) == UPDATE_DEACTIVATED) string_free(_u.value);
}



//---------------- setup GUI values for later updates -------------//
//---------------- setup GUI values for later updates -------------//
//---------------- setup GUI values for later updates -------------//

// There are three ways to setup resources:
//   (1) supply a resource value directly.
//   (2) register a pointer to a user variable in which
//           the desired resource value will always reside.
//   (3) register a function which will always return the
//           desired resource value.

// When supplying a resource value directly, the automatic
//   setting of the resource will be deactivated after
//   updateValue() is called.
// Registering a NULL pointer or NULL function will
//   deactivate the automatic setting of the resource.

// Any of these functions can be called at any time, overriding
//   any previous updating arrangement made by a previous call.

//------------------ setup value -------------------------------//

#define SETUP_VALUE(LongGR, long2)            \
void LongGR::setupValue(long2 uvalue)         \
{                                             \
  _u.value = uvalue;                          \
  _flag = UPDATE_FROM_VALUE;                  \
}

SETUP_VALUE(  LongGR, long  )
SETUP_VALUE( FloatGR, float )
SETUP_VALUE(DoubleGR, double)


void StringGR::setupValue(char* uvalue) 
{                  
  if(_flag == UPDATE_FROM_VALUE) string_free(_u.value);
  _u.value = string_alloc(NULL, uvalue, 0);
  _flag = UPDATE_FROM_VALUE;
}



//------------------ setup pointer -------------------------------//

#define SETUP_POINT(LongGR, long2)            \
void LongGR::setupPoint(long2 upoint)         \
{                                             \
  _u.point = upoint;                          \
  if(_u.point) _flag = UPDATE_FROM_POINTER;   \
  else if(_flag != UPDATE_NEVER_ACTIVATED)    \
               _flag = UPDATE_DEACTIVATED;    \
}

SETUP_POINT(  LongGR, long*  )
SETUP_POINT( FloatGR, float* )
SETUP_POINT(DoubleGR, double*)


void StringGR::setupPoint(char* upoint, long nvar)
{                             
  if(_flag == UPDATE_FROM_VALUE) string_free(_u.value);
  _u.point = upoint;         
  if(_u.point) _flag = UPDATE_FROM_POINTER;
  else if(_flag != UPDATE_NEVER_ACTIVATED) 
               _flag = UPDATE_DEACTIVATED;
  _nvar = nvar;
}


//------------------ setup function -------------------------------//

#define SETUP_FUN(LongGR, long2)                             \
void LongGR::setupFun(long2(*ufun)(void *data), void *data)  \
{                                                            \
  _u.fun   = ufun;                                           \
  if(_u.fun) _flag = UPDATE_FROM_FUNCTION;                   \
  else if(_flag != UPDATE_NEVER_ACTIVATED)                   \
               _flag = UPDATE_DEACTIVATED;                   \
  _data = data;                                              \
}

SETUP_FUN(  LongGR, long  )
SETUP_FUN( FloatGR, float )
SETUP_FUN(DoubleGR, double)


void StringGR::setupFun(char*(*ufun)(void *data), void *data)
{                  
  if(_flag == UPDATE_FROM_VALUE) string_free(_u.value);
  _u.fun   = ufun;
  if(_u.fun) _flag = UPDATE_FROM_FUNCTION; 
  else if(_flag != UPDATE_NEVER_ACTIVATED) 
               _flag = UPDATE_DEACTIVATED;
  _data = data;
}



//----------------- set new value ------------------------------//
//----------------- set new value ------------------------------//
//----------------- set new value ------------------------------//

       // Normally called when user changes the value in a
       //    callback or event handler.
       // _var will contain the new value, which is placed into
       //    the pointee if the pointer is known.
       // _old will contain the previous value, which will equal
       //    the new value if the value has not been changed.
       // _prev will contain the previous value which was
       //    different from the new value.

    // Should also be called in the GUI constructor passing the
    // default value (from resource file) if the GUI has a
    // default resource value which can be obtained even if
    // the widget has not been created yet.  This way, the
    // value will correspond to the value the widget will have
    // when it is created.

    // Otherwise, a strange value can be set which will be a 
    // flag that the application has not tried to set the value;
    // this flag, if returned by getValue() when the widget 
    // is created, then means that the object is free to reset 
    // a value actually obtained from the widget.


#define SET_NEW_VALUE(LongGR, long2)                  \
void LongGR::setNewValue(long2 value)                 \
{                                                     \
  _old = _var;                                        \
  if(value == _var) return;                           \
  _prev = _var;                                       \
  _var = value;                                       \
  if(AbsoluteValue(_flag) == UPDATE_FROM_POINTER)     \
                                  *_u.point = _var;   \
}

SET_NEW_VALUE(  LongGR, long  )
SET_NEW_VALUE( FloatGR, float )
SET_NEW_VALUE(DoubleGR, double)


void StringGR::setNewValue(char *value)
{
  _old  = string_alloc(_old  , _var , 0);
  if(!string_compare(value, 0, _var, 0)) return;
  _prev = string_alloc(_prev , _var , 0);
  _var  = string_alloc(_var  , value, 0);
  if(AbsoluteValue(_flag) == UPDATE_FROM_POINTER)
                  string_copy(_u.point, _nvar, _var, 0);
}


//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
