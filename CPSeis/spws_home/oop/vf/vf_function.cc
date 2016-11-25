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
// $Id: vf_function.cc,v 1.2 2004/06/07 12:59:47 wjdone Exp spws $
// $Name:  $

//-------------------------- vf_function.cc --------------------------//
//-------------------------- vf_function.cc --------------------------//
//-------------------------- vf_function.cc --------------------------//

//          implementation file for the VfFunction class
//                    not derived from any class
//                         subdirectory vf



#include "vf/vf_function.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_function_array.hh"
#include "vf/vf_update.hh"
#include "oprim/active_index_keeper.hh"
#include "oprim/simple_short_array.hh"
#include "oprim/simple_float_array.hh"
#include "oprim/selections_keeper.hh"
#include "trslib.h"
#include "named_constants.h"
#include "swapio.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <math.h>


/*
#define FEET_PER_METER  3.28084   // now in named_constants.h
*/

#define BEFORE  beforeVelfunChange();
#define AFTER   afterVelfunChange();

#define MIGHT  if(_array) _array->neighborsMightHaveChanged();


//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//
//-------------------------- constructor ----------------------------//


VfFunction::VfFunction(VfUtilities *utilities)
            :
                _utilities  (utilities),
                _array      (NULL),
                _several    (FALSE),
                _NPICKS     (-1),
                _ORDINATES  (NULL)
{
  assert(_utilities);
  _active   = new ActiveIndexKeeper(ActiveIndexKeeper::SHOW_LAST_CHANGE);
#ifdef USING_SIMPLE_FLOAT_ARRAY
  _depth    = new MY_SIMPLE_ARRAY (MAXPICKS, 0.0);
  _time     = new MY_SIMPLE_ARRAY (MAXPICKS, 0.0);
  _vnmo     = new MY_SIMPLE_ARRAY (MAXPICKS, 0.0);
  _vrms     = new MY_SIMPLE_ARRAY (MAXPICKS, 0.0);
  _vav      = new MY_SIMPLE_ARRAY (MAXPICKS, 0.0);
  _vint     = new MY_SIMPLE_ARRAY (MAXPICKS, 0.0);
  _thick    = new MY_SIMPLE_ARRAY (MAXPICKS, 0.0);
  _offset   = new MY_SIMPLE_ARRAY (MAXPICKS, 0.0);
#else
  _depth    = new MY_SIMPLE_ARRAY (MAXPICKS);
  _time     = new MY_SIMPLE_ARRAY (MAXPICKS);
  _vnmo     = new MY_SIMPLE_ARRAY (MAXPICKS);
  _vrms     = new MY_SIMPLE_ARRAY (MAXPICKS);
  _vav      = new MY_SIMPLE_ARRAY (MAXPICKS);
  _vint     = new MY_SIMPLE_ARRAY (MAXPICKS);
  _thick    = new MY_SIMPLE_ARRAY (MAXPICKS);
  _offset   = new MY_SIMPLE_ARRAY (MAXPICKS);
#endif
  _selkeep  = new SelectionsKeeper ();
  privateClear();           // sets all other variables.
}





//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//
//--------------------------- destructor --------------------------//

VfFunction::~VfFunction()
{
  delete _active  ;
  delete _depth   ;
  delete _time    ;
  delete _vnmo    ;
  delete _vrms    ;
  delete _vav     ;
  delete _vint    ;
  delete _thick   ;
  delete _offset  ;
  delete _selkeep ;
  if(_ORDINATES) delete [] _ORDINATES;
}



//--------------------- before and after several operations --------------//
//--------------------- before and after several operations --------------//
//--------------------- before and after several operations --------------//

         // public.

void VfFunction::beforeSeveralOperations()
{
  assert(!_several);
  if(_array) _array->beforeVelfunChange(this);
  _several = TRUE;
}



void VfFunction::afterSeveralOperations(int type)
{
  assert(_several);
  _several = FALSE;
  privateUpdatePicks(type);
  if(_array) _array->afterVelfunChange(this);
}



//------------------------- private functions ---------------------//
//------------------------- private functions ---------------------//
//------------------------- private functions ---------------------//

         // private.

void VfFunction::beforeVelfunChange()          // BEFORE
{
  if(_several) return;
  if(_array) _array->beforeVelfunChange(this);
}


void VfFunction::afterVelfunChange()           // AFTER
{
  if(_several) return;
  if(_array) _array->afterVelfunChange(this);
}


MY_SIMPLE_ARRAY *VfFunction::abscissaArray(int type)  const
{
  if(type == -1) type = _type;
  MY_SIMPLE_ARRAY *array;
  switch(type)
      {
      case VTNM: array = _time ; break;
      case VTRM: array = _time ; break;
      case VZRM: array = _depth; break;
      case VLRM: array = _thick; break;
      case VTAV: array = _time ; break;
      case VZAV: array = _depth; break;
      case VLAV: array = _thick; break;
      case VTIN: array = _time ; break;
      case VZIN: array = _depth; break;
      case VLIN: array = _thick; break;
      case VTDP: array = _time ; break;
      default: assert(FALSE);
      }
  return array;
}


MY_SIMPLE_ARRAY *VfFunction::ordinateArray(int type)  const
{
  if(type == -1) type = _type;
  MY_SIMPLE_ARRAY *array;
  switch(type)
      {
      case VTNM: array = _vnmo ; break;
      case VTRM: array = _vrms ; break;
      case VZRM: array = _vrms ; break;
      case VLRM: array = _vrms ; break;
      case VTAV: array = _vav  ; break;
      case VZAV: array = _vav  ; break;
      case VLAV: array = _vav  ; break;
      case VTIN: array = _vint ; break;
      case VZIN: array = _vint ; break;
      case VLIN: array = _vint ; break;
      case VTDP: array = _depth; break;
      default: assert(FALSE);
      }
  return array;
}



void VfFunction::privateClear()
{
  _xloc      =             0.0;
  _yloc      =             0.0;
  _type      =            VTNM;
  _select    =    SELECT_MAYBE;
  _error     =      ERROR_ZERO;
  _raytrace  =     RAYTRACE_NO;

  strcpy (_vfid   , "none");
  strcpy (_project, "none");
  strcpy (_line   , "none");
  strcpy (_rdate  , "none");
  strcpy (_pdate  , "none");
  strcpy (_userid , "non" );
  strcpy (_comment, "none");
  MIGHT
}



void VfFunction::privateDeletePicks()
{
  _active  ->deleteAllElements();
  _depth   ->deleteAllElements();
  _time    ->deleteAllElements();
  _vnmo    ->deleteAllElements();
  _vrms    ->deleteAllElements();
  _vav     ->deleteAllElements();
  _vint    ->deleteAllElements();
  _thick   ->deleteAllElements();
  _offset  ->deleteAllElements();
  _selkeep ->deleteAllElements();
}



void VfFunction::privateResetNumPicksAndClear(long npicks)
{
  _active  ->resetNumElementsAndClear(npicks);
  _depth   ->resetNumElementsAndClear(npicks);
  _time    ->resetNumElementsAndClear(npicks);
  _vnmo    ->resetNumElementsAndClear(npicks);
  _vrms    ->resetNumElementsAndClear(npicks);
  _vav     ->resetNumElementsAndClear(npicks);
  _vint    ->resetNumElementsAndClear(npicks);
  _thick   ->resetNumElementsAndClear(npicks);
  _offset  ->resetNumElementsAndClear(npicks);
  _selkeep ->resetNumElementsAndClear(npicks);
}



//----------------------- clear velocity function ------------------//
//----------------------- clear velocity function ------------------//
//----------------------- clear velocity function ------------------//

        // public.

void VfFunction::clearVelocityFunction()
{
  BEFORE
  privateClear();
  privateDeletePicks();
  privateUpdatePicks(_type);
  AFTER
}



//---------------- delete all velocity function picks ---------------------//
//---------------- delete all velocity function picks ---------------------//
//---------------- delete all velocity function picks ---------------------//

        // public.

void VfFunction::deleteAllVelocityFunctionPicks()
{
  BEFORE
  privateDeletePicks();
  privateUpdatePicks(_type);
  AFTER
}



//------------------- reset num picks --------------------------------//
//------------------- reset num picks --------------------------------//
//------------------- reset num picks --------------------------------//

        // public.

void VfFunction::resetNumPicks(long npicks, int type)
{
  BEFORE
  privateResetNumPicksAndClear(npicks);
  privateUpdatePicks(type);
  AFTER
}



void VfFunction::resetNumPicks(long npicks, float *abscissae,
                                            float *ordinates, int type)
{
  BEFORE
  privateResetNumPicksAndClear(npicks);
  abscissaArray(type)->setAllValues(abscissae);
  ordinateArray(type)->setAllValues(ordinates);
  privateUpdatePicks(type);
  AFTER
}



void VfFunction::convertFeetToMeters()
{
  BEFORE
  float factor = 1.0 / FEET_PER_METER;
  _vnmo ->multiplyByConstant(factor);
  _vrms ->multiplyByConstant(factor);
  _vav  ->multiplyByConstant(factor);
  _vint ->multiplyByConstant(factor);
  _depth->multiplyByConstant(factor);
  _thick->multiplyByConstant(factor);
  AFTER
}



void VfFunction::convertMetersToFeet()
{
  BEFORE
  float factor = FEET_PER_METER;
  _vnmo ->multiplyByConstant(factor);
  _vrms ->multiplyByConstant(factor);
  _vav  ->multiplyByConstant(factor);
  _vint ->multiplyByConstant(factor);
  _depth->multiplyByConstant(factor);
  _thick->multiplyByConstant(factor);
  AFTER
}



void VfFunction::removeWaterVelocity(float vwater)
{
  BEFORE
  float vrms[MAXPICKS];
  _vrms->getAllValues(vrms);
  long npicks = numPicks();
  for(long ipick = 0; ipick < npicks; ipick++)
      {
      if(vrms[ipick] != FNIL)
          {
          vrms[ipick] = MaximumValue(vrms[ipick], vwater + 10.0);
          vrms[ipick] = sqrt(vrms[ipick] * vrms[ipick] - vwater * vwater);
          }
      }
  _vrms->setAllValues(vrms);
  privateUpdatePicks(VTRM);
  AFTER
}



//----------------- copy velocity function contents ------------//
//----------------- copy velocity function contents ------------//
//----------------- copy velocity function contents ------------//

    // public.
    // privateUpdatePicks not needed here.

void VfFunction::copyVelocityFunctionContents(const VfFunction *velfun)
{
  BEFORE
  _utilities = velfun->_utilities;
  _xloc      = velfun->_xloc;
  _yloc      = velfun->_yloc;
  _type      = velfun->_type;
  _select    = velfun->_select;
  _error     = velfun->_error;
  _raytrace  = velfun->_raytrace;
  memcpy(_vfid   , velfun->_vfid   ,    VFID_LENGTH + 1);
  memcpy(_project, velfun->_project, PROJECT_LENGTH + 1);
  memcpy(_line   , velfun->_line   ,    LINE_LENGTH + 1);
  memcpy(_rdate  , velfun->_rdate  ,   RDATE_LENGTH + 1);
  memcpy(_pdate  , velfun->_pdate  ,   PDATE_LENGTH + 1);
  memcpy(_userid , velfun->_userid ,  USERID_LENGTH + 1);
  memcpy(_comment, velfun->_comment, COMMENT_LENGTH + 1);
  _active  ->copyAllElements(velfun->_active  );
  _depth   ->copyAllElements(velfun->_depth   );
  _time    ->copyAllElements(velfun->_time    );
  _vnmo    ->copyAllElements(velfun->_vnmo    );
  _vrms    ->copyAllElements(velfun->_vrms    );
  _vav     ->copyAllElements(velfun->_vav     );
  _vint    ->copyAllElements(velfun->_vint    );
  _thick   ->copyAllElements(velfun->_thick   );
  _offset  ->copyAllElements(velfun->_offset  );
  _selkeep ->copyAllElements(velfun->_selkeep );
  MIGHT
  AFTER
}



//---------------------- structure for reading and saving ----------------//
//---------------------- structure for reading and saving ----------------//
//---------------------- structure for reading and saving ----------------//


struct Function
{
  long  npicks, active;
  float xloc, yloc;
  int   type, select, error, raytrace;
};

struct RD_FunctionA
{
  long npicks, active;
};

struct RD_FunctionA32_t
{
  int32_t npicks, active;
};

struct RD_FunctionA64_t
{
  int64_t npicks, active;
};

struct RD_FunctionB
{
  float xloc, yloc;
  int   type, select;
};

struct RD_FunctionC
{
  int error, raytrace;
};



//--------------------- static functions -----------------------------//
//--------------------- static functions -----------------------------//
//--------------------- static functions -----------------------------//


static int save_string(FILE *stream, int length, const char *string)
{
  int nobj = length + 1;
/*
  int n = fwrite(string, sizeof(char), nobj, stream);
*/
  int n = swapio_fwrite_char(string, nobj, stream);
  if(n != nobj) return TRUE;
  return FALSE;
}


static int save_array(FILE *stream, long npicks, MY_SIMPLE_ARRAY *array)
{
  float *values = array->createArrayPointer();
  int nobj = (int)npicks;
/*
  int n = fwrite(values, sizeof(float), nobj, stream);
*/
  int n = swapio_fwrite_float(values, nobj, stream);
  array->deleteArrayPointer(values);
  if(n != nobj) return TRUE;
  return FALSE;
}



static int read_string(FILE *stream, int length, char *string)
{
  int nobj = length + 1;
/*
  int n = fread(string, sizeof(char), nobj, stream);
*/
  int n = swapio_fread_char(string, nobj, stream);
  if(feof(stream) || ferror(stream) || n != nobj) return TRUE;
  string[length] = '\0';
  return FALSE;
}


static int read_array(FILE *stream, long npicks, MY_SIMPLE_ARRAY *array)
{
  float values[MAXPICKS];
  int nobj = (int)npicks;
/*
  int n = fread(values, sizeof(float), nobj, stream);
*/
  int n = swapio_fread_float(values, nobj, stream);
  if(feof(stream) || ferror(stream) || n != nobj) return TRUE;
  array->resetNumElements(values, npicks);
  return FALSE;
}



//----------------------- save to file -------------------------------//
//----------------------- save to file -------------------------------//
//----------------------- save to file -------------------------------//

         // public.
         // returns error = TRUE or FALSE.

         // if the number of picks is greater than zero,
         // the value of function.npicks on the file is set negative
         // as a flag that selkeep is saved to the file.

int VfFunction::saveToBinaryFile(FILE *stream, int type)  const
{
  if(type == -1) type = _type;
  Function function, function_io_only;
  function.npicks   = numPicks();
  function.xloc     = _xloc;
  function.yloc     = _yloc;
  function.type     = type;
  function.select   = _select;
  function.error    = _error;
  function.raytrace = _raytrace;
  function.active   = _active->getActiveIndex();

  memcpy(&function_io_only, &function, sizeof(Function));
  function_io_only.npicks = - function_io_only.npicks;
/*
  swapio_swap_before_write (&function_io_only, sizeof(float), 8);
*/
  swapio_swap_before_write (&function_io_only.npicks, sizeof(long),  2);
  swapio_swap_before_write (&function_io_only.xloc,   sizeof(float), 2);
  swapio_swap_before_write (&function_io_only.type,   sizeof(int),   4);
  int nobj = 1;
  int n = fwrite(&function_io_only, sizeof(Function), nobj, stream);
  if(n != nobj) return TRUE;

  int        error = save_string(stream,     VFID_LENGTH, _vfid);
  if(!error) error = save_string(stream,  PROJECT_LENGTH, _project);
  if(!error) error = save_string(stream,     LINE_LENGTH, _line);
  if(!error) error = save_string(stream,    RDATE_LENGTH, _rdate);
  if(!error) error = save_string(stream,    PDATE_LENGTH, _pdate);
  if(!error) error = save_string(stream,   USERID_LENGTH, _userid);
  if(!error) error = save_string(stream,  COMMENT_LENGTH, _comment);

  if(_error == ERROR_NONE && _raytrace == RAYTRACE_NO)
      {
      if(!error) error = save_array (stream, function.npicks,
                                                       abscissaArray(type));
      if(!error) error = save_array (stream, function.npicks,
                                                       ordinateArray(type));
      }
  else
      {
      if(!error) error = save_array (stream, function.npicks, _depth);
      if(!error) error = save_array (stream, function.npicks, _time);
      if(!error) error = save_array (stream, function.npicks, _vnmo);
      if(!error) error = save_array (stream, function.npicks, _vrms);
      if(!error) error = save_array (stream, function.npicks, _vav);
      if(!error) error = save_array (stream, function.npicks, _vint);
      if(!error) error = save_array (stream, function.npicks, _thick);
      if(!error) error = save_array (stream, function.npicks, _offset);
      }

  if(function.npicks > 0)
      {
      if(!error) error = _selkeep->binaryWrite(stream);
      }
  return error;
}



//----------------------- read from file -------------------------------//
//----------------------- read from file -------------------------------//
//----------------------- read from file -------------------------------//

         // public.
         // returns rerror = TRUE or FALSE.

         // if the value of function.npicks on the file is negative,
         // this is a flag that selkeep was saved to the file.

int VfFunction::readFromBinaryFile(FILE *stream)
{
  Function         function;
  RD_FunctionA     functionA;
  RD_FunctionA32_t functionA32;
  RD_FunctionA64_t functionA64;
  RD_FunctionB     functionB;
  RD_FunctionC     functionC;

  int read_selkeep = FALSE;
  int nobj = 1;
  char buf[33];
  buf[32] = '\0';
  int swapped = FALSE;

  int k2, n, rerror, serror, finished, alonglength;
  // read the first 32 bytes
  n = fread (buf, 32, nobj, stream);
  rerror = feof(stream) || ferror(stream) || n != nobj;
  if (rerror) return TRUE;
  // if the file contains 32 bit longs then you have all of Function
  // if the file contains 64 bit longs then you have all but last 8 bytes
  for (k2 = 0, finished = FALSE; !finished && k2 < 4; k2++) {
    alonglength = swapio_get_longlength ();
    if (alonglength == sizeof(long)) {
      memcpy (&functionA.npicks, &buf[0], 2*sizeof(long));
      swapio_swap_after_read_long (&functionA.npicks, 2);
      serror = functionA.npicks < -MAXPICKS ||
               functionA.npicks >  MAXPICKS   ;
      if (!serror) {
	function.npicks = functionA.npicks;
	function.active = functionA.active;
      }
    }
    else if (alonglength == 4 && sizeof(long) == 8) {
      memcpy (&functionA32.npicks, &buf[0], 8);
      swapio_swap_after_read_int ((int *)&functionA32.npicks, 2);
      serror = functionA32.npicks < -MAXPICKS ||
               functionA32.npicks >  MAXPICKS   ;
      if (!serror) {
	function.npicks = functionA32.npicks;
	function.active = functionA32.active;
      }
    }
    else if (alonglength == 8 && sizeof(long) == 4) {
      memcpy (&functionA64.npicks, &buf[0], 16);
      swapio_swap_after_read_int64 (&functionA64.npicks, 2);
      serror = functionA64.npicks < -MAXPICKS ||
               functionA64.npicks >  MAXPICKS   ;
      if (!serror) {
	function.npicks = functionA64.npicks;
	function.active = functionA64.active;
      }
    }
    else {
      if (swapped) {
	swapio_toggle_read_swapping_action ();
      }
      return TRUE;
    }
    if (!serror) {
      finished = TRUE;
    }
    else {
      switch (k2) {
      case 0 :
      case 2 :
	// first or third time through; had bad npicks value
	// toggle the swap state
	// try again
	swapio_toggle_read_swapping_action ();
	swapped = TRUE;
	break;
      case 1 :
	// second time through; had bad npicks value
	// change application long length
	// try again

	if (alonglength == 8) {
	  alonglength = 4;
	}
	else if (alonglength == 4) {
	  alonglength = 8;
	}
	else {
	  if (swapped) {
	    swapio_toggle_read_swapping_action ();
	  }
	  return TRUE;
	}
	swapio_use_longlength (&alonglength);
	break;
      case 3 :
	// fourth time through; had bad npicks value
	// out of ideas
	// try again
	if (swapped) {
	  swapio_toggle_read_swapping_action ();
	}
	return TRUE;
	break;
      default:
	assert (0);
      }
    }
  }

  if (!serror) {
    if (function.npicks < 0) {
      read_selkeep = TRUE;
      function.npicks = -function.npicks;
    }
    if (alonglength == 4) {
      memcpy (&functionB.xloc , &buf[8], 16);
      memcpy (&functionC.error, &buf[24], 8);
    }
    else /* if (alonglength == 8) */ {
      n = fread (&functionC, sizeof(functionC), nobj, stream);
      rerror = feof(stream) || ferror(stream) || n != nobj;
      if (rerror) {
	if (swapped) {
	  swapio_toggle_read_swapping_action ();
	}
	return TRUE;
      }
      memcpy (&functionB.xloc , &buf[16], 16);
/*
      memcpy (&functionC.error, &buf[32],  8);   // should not be called!
*/
    }

    swapio_swap_after_read_float (&functionB.xloc , 2);
    swapio_swap_after_read_int   (&functionB.type , 2);
    swapio_swap_after_read_int   (&functionC.error, 2);

    if (functionB.type < FIRSTTYPE || functionB.type > LASTTYPE) {
      if (swapped) {
	swapio_toggle_read_swapping_action ();
      }
      return TRUE;
    }

    function.xloc     = functionB.xloc    ;
    function.yloc     = functionB.yloc    ;
    function.type     = functionB.type    ;
    function.select   = functionB.select  ;
    function.error    = functionC.error   ;
    function.raytrace = functionC.raytrace;
  }
  else {
    if (swapped) {
      swapio_toggle_read_swapping_action ();
    }
    return TRUE;
  }

  BEFORE
  _xloc       = function.xloc;
  _yloc       = function.yloc;
  _type       = function.type;
  _select     = function.select;
  _error      = function.error;
  _raytrace   = function.raytrace;

             rerror = read_string(stream,     VFID_LENGTH, _vfid);
  if(!rerror) rerror = read_string(stream,  PROJECT_LENGTH, _project);
  if(!rerror) rerror = read_string(stream,     LINE_LENGTH, _line);
  if(!rerror) rerror = read_string(stream,    RDATE_LENGTH, _rdate);
  if(!rerror) rerror = read_string(stream,    PDATE_LENGTH, _pdate);
  if(!rerror) rerror = read_string(stream,   USERID_LENGTH, _userid);
  if(!rerror) rerror = read_string(stream,  COMMENT_LENGTH, _comment);

  if(_error == ERROR_NONE && _raytrace == RAYTRACE_NO)
      {
      if(!rerror) privateResetNumPicksAndClear(function.npicks);
      if(!rerror) rerror = read_array (stream, function.npicks,
                                                       abscissaArray(_type));
      if(!rerror) rerror = read_array (stream, function.npicks,
                                                       ordinateArray(_type));
      privateUpdatePicks(_type);
      }
  else
      {
      if(!rerror) rerror = read_array (stream, function.npicks, _depth);
      if(!rerror) rerror = read_array (stream, function.npicks, _time);
      if(!rerror) rerror = read_array (stream, function.npicks, _vnmo);
      if(!rerror) rerror = read_array (stream, function.npicks, _vrms);
      if(!rerror) rerror = read_array (stream, function.npicks, _vav);
      if(!rerror) rerror = read_array (stream, function.npicks, _vint);
      if(!rerror) rerror = read_array (stream, function.npicks, _thick);
      if(!rerror) rerror = read_array (stream, function.npicks, _offset);
      }

  _active ->resetNumElements        (function.npicks);
  _active ->setActiveIndex          (function.active);
  _selkeep->resetNumElements        (function.npicks);

  if(read_selkeep)
      {
      if(!rerror) rerror = _selkeep->binaryRead(stream);
      if(_selkeep->numElements() != function.npicks) rerror = TRUE;
      }
  MIGHT
  AFTER
  if (swapped) {
    swapio_toggle_read_swapping_action ();
  }
  return rerror;
}



//---------------- get string corresponding to type --------------------//
//---------------- get string corresponding to type --------------------//
//---------------- get string corresponding to type --------------------//

          // public.

const char *VfFunction::getTypeSymbol (int type)  const
{
  if(type == -1) type = _type;
  return _utilities->typeSymbol(type);
}


const char *VfFunction::getTypeDescription (int type)  const
{
  if(type == -1) type = _type;
  return _utilities->typeDescription(type);
}



//----------------------- get general values ---------------------//
//----------------------- get general values ---------------------//
//----------------------- get general values ---------------------//

        // public.

long VfFunction::numPicks()  const
{
  long nelements1 = _active->numElements();
  long nelements2 = _depth ->numElements();
  assert(nelements1 == nelements2);
  return nelements1;
}


long VfFunction::getActivePick()  const
{
  return _active->getActiveIndex();
}


int VfFunction::vfidIsBlank()  const
{
  if(strncmp(_vfid, "none", 4) == 0) return TRUE;
  if(strcmp (_vfid, ""       ) == 0) return TRUE;
  if(strncmp(_vfid, " "   , 1) == 0) return TRUE;
  return FALSE;
}



int VfFunction::typeError  (int type)  const
{
  long npicks = numPicks();
  if(npicks == 0) return TRUE;
  if(type == -1) type = _type;
  if(abscissaArray(type)->validateAscending() < npicks) return TRUE;
  if(abscissaArray(type)->getValue(0) != 0.0)           return TRUE;
  if(type == VTDP)
      {
      if(ordinateArray(type)->validateAscending() < npicks) return TRUE;
      if(ordinateArray(type)->getValue(0) != 0.0)           return TRUE;
      }
  else if(!_utilities->update()->velocitiesAreBogus())
      {
      if(ordinateArray(type)->validatePositive () < npicks) return TRUE;
      }
  return FALSE;
}



float VfFunction::getElevation()  const
{
  char buffer[80];
  float value;
  int num = sscanf(_comment, "%s %f", buffer, &value);
  if(num != 2) return FNIL;
  if(strcmp(buffer, "E") != 0) return FNIL;
  return value;
}


float VfFunction::getWaterDepth()  const
{
  char buffer[80];
  float value;
  int num = sscanf(_comment, "%s %f", buffer, &value);
  if(num != 2) return FNIL;
  if(strcmp(buffer, "W") != 0) return FNIL;
  return value;
}



//------------------ get individual pick values ----------------------//
//------------------ get individual pick values ----------------------//
//------------------ get individual pick values ----------------------//

        // public.

#define GET_PICK(getDepth, _depth)               \
float VfFunction::getDepth(long ipick)  const    \
{                                                \
  return _depth->getValue(ipick);                \
}


GET_PICK (getDepth    , _depth   )
GET_PICK (getTime     , _time    )
GET_PICK (getVrms     , _vrms    )
GET_PICK (getVav      , _vav     )
GET_PICK (getVint     , _vint    )
GET_PICK (getVnmo     , _vnmo    )
GET_PICK (getThickness, _thick   )
GET_PICK (getOffset   , _offset  )



float VfFunction::getAbscissa(long ipick, int type)  const
{
  return abscissaArray(type)->getValue(ipick);
}



float VfFunction::getOrdinate(long ipick, int type)  const
{
  return ordinateArray(type)->getValue(ipick);
}



//------------------ get arrays of pick values ----------------------//
//------------------ get arrays of pick values ----------------------//
//------------------ get arrays of pick values ----------------------//

        // public.

#define GET_PICKS(getDepthArray, _depth)                \
void VfFunction::getDepthArray(float *values)  const    \
{                                                       \
  _depth->getAllValues(values);                         \
}


GET_PICKS (getDepthArray    , _depth   )
GET_PICKS (getTimeArray     , _time    )
GET_PICKS (getVrmsArray     , _vrms    )
GET_PICKS (getVavArray      , _vav     )
GET_PICKS (getVintArray     , _vint    )
GET_PICKS (getVnmoArray     , _vnmo    )
GET_PICKS (getThicknessArray, _thick   )
GET_PICKS (getOffsetArray   , _offset  )



void VfFunction::getAbscissaArray(float *abscissae, int type)  const
{
  abscissaArray(type)->getAllValues(abscissae);
}



void VfFunction::getOrdinateArray(float *ordinates, int type)  const
{
  ordinateArray(type)->getAllValues(ordinates);
}




/*
//---------------- get pointers to arrays of pick values ----------------//
//---------------- get pointers to arrays of pick values ----------------//
//---------------- get pointers to arrays of pick values ----------------//

        // public.

#define GET_PICKS2(getDepthArrayPointer, _depth)          \
const float *VfFunction::getDepthArrayPointer()  const    \
{                                                         \
  return _depth->getArrayPointer();                       \
}


GET_PICKS2 (getDepthArrayPointer    , _depth   )
GET_PICKS2 (getTimeArrayPointer     , _time    )
GET_PICKS2 (getVrmsArrayPointer     , _vrms    )
GET_PICKS2 (getVavArrayPointer      , _vav     )
GET_PICKS2 (getVintArrayPointer     , _vint    )
GET_PICKS2 (getVnmoArrayPointer     , _vnmo    )
GET_PICKS2 (getThicknessArrayPointer, _thick   )
GET_PICKS2 (getOffsetArrayPointer   , _offset  )



const float *VfFunction::getAbscissaArrayPointer(int type)  const
{
  return abscissaArray(type)->getArrayPointer();
}



const float *VfFunction::getOrdinateArrayPointer(int type)  const
{
  return ordinateArray(type)->getArrayPointer();
}
*/


//---------------- get pointers to arrays of pick values ----------------//
//---------------- get pointers to arrays of pick values ----------------//
//---------------- get pointers to arrays of pick values ----------------//

        // public.

float *VfFunction::createAbscissaArrayPointer(int type)  const
{
  return abscissaArray(type)->createArrayPointer();
}



float *VfFunction::createOrdinateArrayPointer(int type)  const
{
  return ordinateArray(type)->createArrayPointer();
}


void VfFunction::deleteAbscissaArrayPointer(float *values, int type)  const
{
  abscissaArray(type)->deleteArrayPointer(values);
}



void VfFunction::deleteOrdinateArrayPointer(float *values, int type)  const
{
  ordinateArray(type)->deleteArrayPointer(values);
}



//--------------------- get minimum and maximum values ----------------//
//--------------------- get minimum and maximum values ----------------//
//--------------------- get minimum and maximum values ----------------//

    // public.
    // these return FNIL if there are no picks, or all picks are nil.
    // otherwise, nil values are not used when getting minima and maxima.

float VfFunction::minimumDepth    () const { return _depth ->minimumValue(); }
float VfFunction::maximumDepth    () const { return _depth ->maximumValue(); }

float VfFunction::minimumTime     () const { return _time  ->minimumValue(); }
float VfFunction::maximumTime     () const { return _time  ->maximumValue(); }

float VfFunction::minimumVrms     () const { return _vrms  ->minimumValue(); }
float VfFunction::maximumVrms     () const { return _vrms  ->maximumValue(); }

float VfFunction::minimumVav      () const { return _vav   ->minimumValue(); }
float VfFunction::maximumVav      () const { return _vav   ->maximumValue(); }

float VfFunction::minimumVint     () const { return _vint  ->minimumValue(); }
float VfFunction::maximumVint     () const { return _vint  ->maximumValue(); }

float VfFunction::minimumVnmo     () const { return _vnmo  ->minimumValue(); }
float VfFunction::maximumVnmo     () const { return _vnmo  ->maximumValue(); }

float VfFunction::minimumThickness() const { return _thick ->minimumValue(); }
float VfFunction::maximumThickness() const { return _thick ->maximumValue(); }

float VfFunction::minimumOffset   () const { return _offset->minimumValue(); }
float VfFunction::maximumOffset   () const { return _offset->maximumValue(); }



float VfFunction::minimumAbscissa  (int type)  const
{
  return abscissaArray(type)->minimumValue();
}



float VfFunction::maximumAbscissa  (int type)  const
{
  return abscissaArray(type)->maximumValue();
}



float VfFunction::minimumOrdinate  (int type)  const
{
  return ordinateArray(type)->minimumValue();
}



float VfFunction::maximumOrdinate  (int type)  const
{
  return ordinateArray(type)->maximumValue();
}



//------------------------ find bracketing abscissae ---------------------//
//------------------------ find bracketing abscissae ---------------------//
//------------------------ find bracketing abscissae ---------------------//

        // public.

void VfFunction::findBracketingAbscissae
                 (float value, int type, long *ia, long *ib)  const
{
#ifdef USING_SIMPLE_FLOAT_ARRAY
  abscissaArray(type)->findBracketingValues(value, ia, ib);
#else
  int ia2, ib2;
  abscissaArray(type)->findBracketingValues(value, &ia2, &ib2);
  *ia = ia2;
  *ib = ib2;
#endif
}



//---------------------- get interpolated velocity ------------------------//
//---------------------- get interpolated velocity ------------------------//
//---------------------- get interpolated velocity ------------------------//

      // public.
      // get interpolated velocity (ordinate) at specified time or
      //   depth (abscissa).
      // returns 0.0 if there are no picks, or some picks are nil.
      // type should not have abscissa = layer thickness.
      // if vint_grade is FALSE, does special interval velocity
      //   interpolation.

float VfFunction::getInterpolatedVelocity
                         (float abscissa, int type, int vint_grade)  const
{
  long npicks = numPicks();
  if(npicks == 0) return 0.0;
  if(type == -1) type = _type;
  assert(!VfUtilities::abscissaIsThickness(type));
  MY_SIMPLE_ARRAY *xarray = abscissaArray(type);
  MY_SIMPLE_ARRAY *varray = ordinateArray(type);
  if(xarray->numNilValues() > 0) return 0.0;
  if(varray->numNilValues() > 0) return 0.0;
  float *abscissae = xarray->createArrayPointer();
  float *ordinates = varray->createArrayPointer();
  if(vint_grade == FALSE && _utilities->ordinateIsVINT(type))
      {
      float v = ordinates[0];
      for(long ipick = 0; ipick < npicks; ipick++)
          {
          v = ordinates[ipick];
          if(abscissae[ipick] >= abscissa) break;
          }
      xarray->deleteArrayPointer(abscissae);
      varray->deleteArrayPointer(ordinates);
      return v;
      }
  int npicks2 = (int)npicks;
/*
  return terp1 (&abscissa, abscissae, &npicks2, ordinates);
*/
  float result = terp1 (&abscissa, abscissae, &npicks2, ordinates);
  xarray->deleteArrayPointer(abscissae);
  varray->deleteArrayPointer(ordinates);
  return result;
}


float VfFunction::getInterpolatedTime (float depth)  const
{
  long npicks = numPicks();
  if(npicks == 0) return 0.0;
  if(_depth->numNilValues() > 0) return 0.0;
  if(_time ->numNilValues() > 0) return 0.0;
  float *abscissae = _depth->createArrayPointer();
  float *ordinates = _time ->createArrayPointer();
  int npicks2 = (int)npicks;
/*
  return terp1 (&depth, abscissae, &npicks2, ordinates);
*/
  float result = terp1 (&depth, abscissae, &npicks2, ordinates);
  _depth->deleteArrayPointer(abscissae);
  _time ->deleteArrayPointer(ordinates);
  return result;
}


float VfFunction::getInterpolatedDepth (float time)  const
{
  return getInterpolatedVelocity(time, VTDP);
}



//------------- get interpolated velocity function ----------------//
//------------- get interpolated velocity function ----------------//
//------------- get interpolated velocity function ----------------//

     // public.
     // returns ordinates interpolated to the specified abscissae.
     // abscissae[npicks] are input and ordinates[npicks] are returned.
     // if abscissae[] and ordinates[] are in fact the same array,
     //   the abscissae will be replaced by the ordinates.
     // fills ordinates[] with zeroes if this type has a type error.
     // type must not have abscissa = layer thickness.
     // if vint_grade is FALSE, does special interval velocity
     //   interpolation.
     // values in this velocity function are iii, nnn, xxx[nnn], vvv[nnn].

void VfFunction::getInterpolatedVelocityFunction
                   (long npicks, const float *abscissae, float *ordinates,
                    int type, int vint_grade)
{
  assert(npicks > 1);
  assert(abscissae && ordinates);
  if(_ORDINATES)
      {
      assert(npicks == _NPICKS);
      memcpy(ordinates, _ORDINATES, (int)npicks);
      return;
      }
  if(type == -1) type = _type;
  assert(!VfUtilities::abscissaIsThickness(type));
  if(!_utilities->ordinateIsVINT(type)) vint_grade = TRUE;
  float *xxx = abscissaArray(type)->createArrayPointer();
  float *vvv = ordinateArray(type)->createArrayPointer();
  int error = typeError(type);
  long nnn = numPicks();
  long iii = 0;
  float slope = 0.0;
  for(long ipick = 0; ipick < npicks; ipick++)
       {
       float abscissa = abscissae[ipick];
       float ordinate;
     label:
       if(error)
           {
           ordinate = 0.0;
           }
       else if(abscissa < xxx[iii])
           {
           if(iii == 0 || vint_grade == FALSE)
               {
               ordinate = vvv[iii];
               }
           else
               {
               float frac = abscissa - xxx[iii-1];
               ordinate = vvv[iii-1] + slope * frac;
               }
           }
       else if(iii == nnn - 1)
           {
           ordinate = vvv[iii];
           }
       else
           {
           iii++;
           float num = vvv[iii] - vvv[iii-1];
           float den = xxx[iii] - xxx[iii-1];
           slope = num / den;
           goto label;
           }
       ordinates[ipick] = ordinate;
       }
  abscissaArray(type)->deleteArrayPointer(xxx);
  ordinateArray(type)->deleteArrayPointer(vvv);
  if(_NPICKS > 1)
      {
      assert(_ORDINATES == NULL);
      _ORDINATES = new float [npicks];
      memcpy(_ORDINATES, ordinates, (int)npicks);
      }
}



//------------------------- vel to float --------------------------//
//------------------------- vel to float --------------------------//
//------------------------- vel to float --------------------------//

     // public.
     // puts evenly-sampled velocity picks into ordinates[npicks].
     // tmin (minimum time or depth) is mapped to ordinates[0].
     // tmax (maximum time or depth) is mapped to ordinates[npicks-1].
     // fills ordinates[] with zeroes if this type has a type error.
     // type must not have abscissa = layer thickness.
     // if vint_grade is FALSE, does special interval velocity
     //   interpolation.

void VfFunction::velToFloat(float tmin, float tmax,
              long npicks, float *ordinates, int type, int vint_grade)
{
  assert(tmax > tmin);
  assert(npicks > 1);
  assert(ordinates);
  if(_ORDINATES)
      {
      assert(npicks == _NPICKS);
      memcpy(ordinates, _ORDINATES, (int)npicks);
      return;
      }
  float dt = (tmax - tmin)/(npicks - 1);
  for(long ipick = 0; ipick < npicks; ipick++)
      {
      ordinates[ipick] = tmin + ipick * dt;  // really abscissae at this stage.
      }
  getInterpolatedVelocityFunction
                     (npicks, ordinates, ordinates, type, vint_grade);
}



//----------------- remember and forget interpolated ordinates -------------//
//----------------- remember and forget interpolated ordinates -------------//
//----------------- remember and forget interpolated ordinates -------------//

        // public.

void VfFunction::rememberInterpolatedOrdinates(long npicks)
{
  assert(_NPICKS == -1);
  assert(npicks > 1);
  _NPICKS = npicks;
}


void VfFunction::deleteInterpolatedOrdinates()
{
  assert(_NPICKS > 1);
  if(_ORDINATES) delete [] _ORDINATES;
  _ORDINATES = NULL;
}


void VfFunction::forgetInterpolatedOrdinates()
{
  assert(_NPICKS > 1);
  if(_ORDINATES) delete [] _ORDINATES;
  _ORDINATES = NULL;
  _NPICKS = -1;
}



//----------------------- set general values --------------------------//
//----------------------- set general values --------------------------//
//----------------------- set general values --------------------------//

        // public.

void VfFunction::setXloc(float value)
{
  BEFORE
  _xloc = value;
  MIGHT
  AFTER
}


void VfFunction::setYloc(float value)
{
  BEFORE
  _yloc = value;
  MIGHT
  AFTER
}


void VfFunction::setDefaultType(int value)
{
  assert(value >= FIRSTTYPE && value <= LASTTYPE);
  _type = value;
}


void VfFunction::setSelectFlag(int value)
{
  assert(value == SELECT_MAYBE  || value == SELECT_YES ||
         value == SELECT_NO     || value == SELECT_TOP ||
         value == SELECT_BOTTOM || value == '*');
  _select = value;
}


void VfFunction::setActivePick(long ipick)
{
  _active->setActiveIndex(ipick);
}



#define SET_GENERAL(setVfid, VFID_LENGTH, _vfid)     \
void VfFunction::setVfid(const char *value)          \
{                                                    \
  BEFORE                                             \
  strncpy(_vfid, value, VFID_LENGTH);                \
  _vfid[VFID_LENGTH] = '\0';                         \
  AFTER                                              \
}

SET_GENERAL (setVfid   ,    VFID_LENGTH, _vfid   )
SET_GENERAL (setProject, PROJECT_LENGTH, _project)
SET_GENERAL (setLine   ,    LINE_LENGTH, _line   )
SET_GENERAL (setRdate  ,   RDATE_LENGTH, _rdate  )
SET_GENERAL (setPdate  ,   PDATE_LENGTH, _pdate  )
SET_GENERAL (setUserid ,  USERID_LENGTH, _userid )
SET_GENERAL (setComment, COMMENT_LENGTH, _comment)


void VfFunction::setElevation(float elevation)
{
  char buffer[80];
  sprintf(buffer, "E %f", elevation);
  setComment(buffer);
}


void VfFunction::setWaterDepth(float water_depth)
{
  char buffer[80];
  sprintf(buffer, "W %f", water_depth);
  setComment(buffer);
}



//------------------- set individual pick values -----------------------//
//------------------- set individual pick values -----------------------//
//------------------- set individual pick values -----------------------//

        // public.

#define SET_PICK(setDepth, _depth)                             \
void VfFunction::setDepth(long ipick, float value, int type)   \
{                                                              \
  BEFORE                                                       \
  _depth ->setValue(ipick, value);                             \
  _active->setValue(ipick);                                    \
  privateUpdatePicks(type);                                    \
  AFTER                                                        \
}

SET_PICK (setDepth    , _depth   )
SET_PICK (setTime     , _time    )
SET_PICK (setVrms     , _vrms    )
SET_PICK (setVav      , _vav     )
SET_PICK (setVint     , _vint    )
SET_PICK (setVnmo     , _vnmo    )
SET_PICK (setThickness, _thick   )



void VfFunction::setAbscissa(long ipick, float abscissa, int type)
{
  BEFORE
  abscissaArray(type)->setValue(ipick, abscissa);
  _active            ->setValue(ipick);
  privateUpdatePicks(type);
  AFTER
}



void VfFunction::setOrdinate(long ipick, float ordinate, int type)
{
  BEFORE
  ordinateArray(type)->setValue(ipick, ordinate);
  _active            ->setValue(ipick);
  privateUpdatePicks(type);
  AFTER
}



void VfFunction::replacePick(long ipick, float abscissa,
                                         float ordinate, int type)
{
  BEFORE
  abscissaArray(type)->setValue(ipick, abscissa);
  ordinateArray(type)->setValue(ipick, ordinate);
  _active            ->setValue(ipick);
  privateUpdatePicks(type);
  AFTER
}



//------------------ set arrays of pick values ----------------------//
//------------------ set arrays of pick values ----------------------//
//------------------ set arrays of pick values ----------------------//

        // public.

#define SET_PICKS(setDepthArray, _depth)                        \
void VfFunction::setDepthArray(const float *values, int type)   \
{                                                               \
  BEFORE                                                        \
  _depth->setAllValues(values);                                 \
  privateUpdatePicks(type);                                     \
  AFTER                                                         \
}


SET_PICKS (setDepthArray    , _depth   )
SET_PICKS (setTimeArray     , _time    )
SET_PICKS (setVrmsArray     , _vrms    )
SET_PICKS (setVavArray      , _vav     )
SET_PICKS (setVintArray     , _vint    )
SET_PICKS (setVnmoArray     , _vnmo    )
SET_PICKS (setOffsetArray   , _offset  )
SET_PICKS (setThicknessArray, _thick   )



void VfFunction::setAbscissaArray(const float *abscissae, int type)
{
  BEFORE
  abscissaArray(type)->setAllValues(abscissae);
  privateUpdatePicks(type);
  AFTER
}



void VfFunction::setOrdinateArray(const float *ordinates, int type)
{
  BEFORE
  ordinateArray(type)->setAllValues(ordinates);
  privateUpdatePicks(type);
  AFTER
}



void VfFunction::replaceAllPicks(const float *abscissae,
                                 const float *ordinates, int type)
{
  BEFORE
  abscissaArray(type)->setAllValues(abscissae);
  ordinateArray(type)->setAllValues(ordinates);
  privateUpdatePicks(type);
  AFTER
}



//---------------- insert or remove one pick -------------------------//
//---------------- insert or remove one pick -------------------------//
//---------------- insert or remove one pick -------------------------//

          // public.

void VfFunction::appendNilPick(int type)
{
  insertNilPick(numPicks(), type);
}


void VfFunction::insertNilPick (long ipick, int type)
{
  BEFORE
  _active  ->insertElement   (ipick);
  _depth   ->insertNilElement(ipick);
  _time    ->insertNilElement(ipick);
  _vnmo    ->insertNilElement(ipick);
  _vrms    ->insertNilElement(ipick);
  _vav     ->insertNilElement(ipick);
  _vint    ->insertNilElement(ipick);
  _thick   ->insertNilElement(ipick);
  _offset  ->insertNilElement(ipick);
  _selkeep ->insertElement   (ipick);
  privateUpdatePicks(type);
  AFTER
}


void VfFunction::insertPickFromBuffer (long ipick, int type)
{
  BEFORE
  _active  ->insertElement          (ipick);
  _depth   ->insertElementFromBuffer(ipick);
  _time    ->insertElementFromBuffer(ipick);
  _vnmo    ->insertElementFromBuffer(ipick);
  _vrms    ->insertElementFromBuffer(ipick);
  _vav     ->insertElementFromBuffer(ipick);
  _vint    ->insertElementFromBuffer(ipick);
  _thick   ->insertElementFromBuffer(ipick);
  _offset  ->insertElementFromBuffer(ipick);
  _selkeep ->insertElement          (ipick);
  privateUpdatePicks(type);
  AFTER
}


void VfFunction::removePick(long ipick, int type)
{
  BEFORE
  _active  ->removeElement(ipick);
  _depth   ->removeElement(ipick);
  _time    ->removeElement(ipick);
  _vnmo    ->removeElement(ipick);
  _vrms    ->removeElement(ipick);
  _vav     ->removeElement(ipick);
  _vint    ->removeElement(ipick);
  _thick   ->removeElement(ipick);
  _offset  ->removeElement(ipick);
  _selkeep ->removeElement(ipick);
  privateUpdatePicks(type);
  AFTER
}


void VfFunction::removePickToBuffer(long ipick, int type)
{
  BEFORE
  _active  ->removeElement        (ipick);
  _depth   ->removeElementToBuffer(ipick);
  _time    ->removeElementToBuffer(ipick);
  _vnmo    ->removeElementToBuffer(ipick);
  _vrms    ->removeElementToBuffer(ipick);
  _vav     ->removeElementToBuffer(ipick);
  _vint    ->removeElementToBuffer(ipick);
  _thick   ->removeElementToBuffer(ipick);
  _offset  ->removeElementToBuffer(ipick);
  _selkeep ->removeElement        (ipick);
  privateUpdatePicks(type);
  AFTER
}


void VfFunction::appendPick(float abscissa, float ordinate, int type)
{
  insertPick(numPicks(), abscissa, ordinate, type);
}


void VfFunction::insertPick (long ipick, float abscissa, float ordinate,
                                                                 int type)
{
  BEFORE
  _active  ->insertElement   (ipick);
  _depth   ->insertNilElement(ipick);
  _time    ->insertNilElement(ipick);
  _vnmo    ->insertNilElement(ipick);
  _vrms    ->insertNilElement(ipick);
  _vav     ->insertNilElement(ipick);
  _vint    ->insertNilElement(ipick);
  _thick   ->insertNilElement(ipick);
  _offset  ->insertNilElement(ipick);
  _selkeep ->insertElement   (ipick);
  abscissaArray(type)->setValue(ipick, abscissa);
  ordinateArray(type)->setValue(ipick, ordinate);
  _active            ->setValue(ipick);
  privateUpdatePicks(type);
  AFTER
}



//------------------- invoke or cancel ray tracing ----------------//
//------------------- invoke or cancel ray tracing ----------------//
//------------------- invoke or cancel ray tracing ----------------//

      // public.

void VfFunction::invokeRayTracing(int type)
{
  if(_raytrace == RAYTRACE_NO)
      {
      BEFORE
      _raytrace = RAYTRACE_FORWARD;  // or RAYTRACE_INVERSE (irrelevant).
      privateUpdatePicks(type);
      AFTER
      }
}



void VfFunction::cancelRayTracing(int type)
{
  if(_raytrace != RAYTRACE_NO)
      {
      BEFORE
      _raytrace = RAYTRACE_NO;
      privateUpdatePicks(type);
      AFTER
      }
}



//---------------------- private update picks ----------------------------//
//---------------------- private update picks ----------------------------//
//---------------------- private update picks ----------------------------//

       // private.
       // updates velocity function picks for all velocity types.
       // does ray tracing if _raytrace is not RAYTRACE_NO.
       // updates according to offset/mute parameters in VfUpdate.
       // stores information about traced rays in VfUpdate.
       // stores error message in VfUpdate.

void VfFunction::privateUpdatePicks(int type)
{
  if(_several) return;
  if(type == -1) type = _type;
  assert(type >= FIRSTTYPE && type <= LASTTYPE);

  int invoke;
  if(_raytrace == RAYTRACE_NO) invoke = 0;
  else                         invoke = 1;

  long npicks = numPicks();
  float depth [MAXPICKS];
  float time  [MAXPICKS];
  float vnmo  [MAXPICKS];
  float vrms  [MAXPICKS];
  float vav   [MAXPICKS];
  float vint  [MAXPICKS];
  float thick [MAXPICKS];
  float offset[MAXPICKS];

  _depth   ->getAllValues(depth);
  _time    ->getAllValues(time );
  _vnmo    ->getAllValues(vnmo );
  _vrms    ->getAllValues(vrms );
  _vav     ->getAllValues(vav  );
  _vint    ->getAllValues(vint );
  _thick   ->getAllValues(thick);

// Possible idea as to how to save memory by saving only the independent arrays:
//
// If the dependent arrays above are shrunken, this means their returned values
// will be wrong (but not needed).  Therefore, before calling the updatePicks
// function below, it will be necessary to fix the dependent arrays by first
// calling updatePicks using _type.  Then, if type != _type, call it again
// as follows.

  int ierr = _utilities->update()->updatePicks
                                        (invoke,     //    input
                                         type  ,     //    input
                                        &npicks,     // input/output
                                         depth ,     // input/output
                                         time  ,     // input/output
                                         vnmo  ,     // input/output
                                         vrms  ,     // input/output
                                         vav   ,     // input/output
                                         vint  ,     // input/output
                                         thick ,     // input/output
                                         offset);    //    output

// Continuation of idea as to how to save memory by saving only the independent arrays:
//
// Now, the dependent arrays can be called below using resetNumElementsAndClear
// to let them be shrunken arrays.
//
// If we do these things, we need to make sure that updatePicks is called without
// shrinking the arrays afterwards when being asked to return a dependent array.

  _active  ->resetNumElements        (npicks);
  _depth   ->resetNumElements(depth , npicks);
  _time    ->resetNumElements(time  , npicks);
  _vnmo    ->resetNumElements(vnmo  , npicks);
  _vrms    ->resetNumElements(vrms  , npicks);
  _vav     ->resetNumElements(vav   , npicks);
  _vint    ->resetNumElements(vint  , npicks);
  _thick   ->resetNumElements(thick , npicks);
  _offset  ->resetNumElements(offset, npicks);
  _selkeep ->resetNumElements        (npicks);

  if     (ierr ==  0) _error = ERROR_NONE;
  else if(ierr == -1) _error = ERROR_ZERO;
  else                _error = ERROR_NILS;

  if     (invoke == 0)  _raytrace = RAYTRACE_NO;
  else if(type == VTNM) _raytrace = RAYTRACE_INVERSE;
  else                  _raytrace = RAYTRACE_FORWARD;
}



//------------------- pass thru to SelectionsKeeper ------------------------//
//------------------- pass thru to SelectionsKeeper ------------------------//
//------------------- pass thru to SelectionsKeeper ------------------------//


long VfFunction::numSelectedPicks ()           const
{
  return _selkeep->numSelected();
}


int  VfFunction::pickIsSelected   (long ipick)  const
{
  return _selkeep->isSelected(ipick);
}


int  VfFunction::getPickSelectFlag    (long ipick)  const
{
  return _selkeep->getSelectFlag(ipick);
}


const char *VfFunction::getPickSelectString  (long ipick)  const
{
  return _selkeep->getSelectString(ipick);
}


void VfFunction::beforeSettingSeveralPickSelectFlags ()
{
  _selkeep->beforeSettingSeveralSelectFlags();
}


void VfFunction::setOneOfSeveralPickSelectFlags (long ipick, int select)
{
  _selkeep->setOneOfSeveralSelectFlags(ipick, select);
}


void VfFunction::afterSettingSeveralPickSelectFlags ()
{
  _selkeep->afterSettingSeveralSelectFlags();
}


void VfFunction::clearPickSelectFlags    ()
{
  _selkeep->clearSelectFlags();
}


void VfFunction::incrementPickSelectFlag (long ipick)
{
  _selkeep->incrementSelectFlag(ipick);
}


void VfFunction::setPickSelectFlag       (long ipick, int select)
{
  _selkeep->setSelectFlag(ipick, select);
}


void VfFunction::toggleAllPickSelections   ()
{
  _selkeep->toggleAllSelections();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

