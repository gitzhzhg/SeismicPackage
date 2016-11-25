
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
//--------------------- field_geometry.cc ---------------------//
//--------------------- field_geometry.cc ---------------------//
//--------------------- field_geometry.cc ---------------------//

//         implementation file for the FieldGeometry class
//                 not derived from any class
//                      subdirectory geom


   // The FieldGeometry class uses special nil values
   // (long, float, and double) which are defined in named_constants:
   //                      int    INIL 
   //                      float  FNIL 
   //                      double DNIL 
/*
   // The FieldGeometry class uses special nil and error values
   // (long, float, and double) which are defined in named_constants:
   //            int    INIL       int    IERR
   //            float  FNIL       float  FERR
   //            double DNIL       double DERR
*/
   // The SLp... classes and the windowbox utilities also use
   // the above functions to get the same nil and error values.
   // If the default values in the above functions are to be
   // changed, they should be changed by the main application
   // before calling any constructors of this class, any SLp...
   // classes, or any SLDatabox classes.  And they should not be
   // changed afterwards.


#include "geom/field_geometry.hh"
#include "geom/fg_informer.hh"
#include "geom/fg_user_abort.hh"
#include "geom/fg_connect.hh"
#include "geom/fg_constants.hh"
#include "geom/seis_survey.hh"
#include "geom/pp_cards.hh"
#include "geom/rp_cards.hh"
#include "geom/zt_cards.hh"
#include "geom/fg_groups.hh"
#include "geom/fg_traces.hh"
#include "geom/midpoints.hh"
#include "geom/fg_trace_values.hh"
#include "geom/fg_headers.hh"
#include "oprim/grid_transform.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


#define DZERO    0.0



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


FieldGeometry::FieldGeometry()
           :
                 _data_needs_saving   (FALSE),
                 _frozen              (FALSE),
                 _out_of_date         (FALSE),
                 _major               (0),
                 _data_changing       (FALSE),
                 _lock                (LOCK_DEL),
		 _updatingMidpointGathers(0),	/* ehs */

                 _informer            (NULL),  // reset below
                 _ua                  (NULL),  // reset below
                 _connect             (NULL),  // reset below
                 _transform           (NULL),  // reset below
                 _testing             (NULL),  // reset below
                 _survey              (NULL),  // reset below
                 _pp_cards            (NULL),  // reset below
                 _rp_cards            (NULL),  // reset below
                 _zt_cards            (NULL),  // reset below
                 _groups              (NULL),  // reset below
                 _traces              (NULL),  // reset below
                 _midpoints           (NULL),  // reset below
                 _tv                  (NULL),  // reset below
                 _headers             (NULL)   // reset below
{
  _informer  = new FgInformer    (this);
  _ua        = new FgUserAbort   (_informer);
  _connect   = new FgConnect     (this);
  _transform = new GridTransform ();
  _testing   = new GridTransform ();
  _survey    = new SeisSurvey    (_informer, _connect);
  _pp_cards  = new PpCards       (_informer, _connect);
  _rp_cards  = new RpCards       (_informer, _connect);
  _zt_cards  = new ZtCards       (_informer, _connect);
  _groups    = new FgGroups      ();
  _traces    = new FgTraces      (_informer);
  _midpoints = new Midpoints     (_informer, _ua, _transform, _traces);
  _tv        = new FgTraceValues (_informer, _ua,
                                    _survey, _rp_cards, _pp_cards,
                                    _zt_cards, _groups, _traces, _midpoints);
  _headers   = new FgHeaders     (_tv, _transform);
  _connect->addSeisSurvey(_survey);
  _connect->addPpCards   (_pp_cards);
  _connect->addRpCards   (_rp_cards);
  _connect->addZtCards   (_zt_cards);
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

FieldGeometry::~FieldGeometry()
{
  _informer->dataGoingAway();  // must be first item in destructor.
  delete _headers;
  delete _tv;
  delete _midpoints;
  delete _traces;
  delete _groups;
  delete _zt_cards;
  delete _rp_cards;
  delete _pp_cards;
  delete _survey;
  delete _testing;
  delete _transform;
  delete _connect;
  delete _ua;
  delete _informer;            // must be last item deleted.
}



//---------------------- register abort fun ------------------------//
//---------------------- register abort fun ------------------------//
//---------------------- register abort fun ------------------------//


void FieldGeometry::registerAbortFun(AbortFun *abort_fun, void *abort_data)
{
  _ua->registerAbortFun(abort_fun, abort_data);
}


void FieldGeometry::startAbortOption()
{
  _ua->startAbortOption();
}



void FieldGeometry::stopAbortOption()
{
  _ua->stopAbortOption();
}



int FieldGeometry::aborted()
{
  return _ua->aborted();
}



//---------------- add or remove FgInform object ----------------//
//---------------- add or remove FgInform object ----------------//
//---------------- add or remove FgInform object ----------------//

      // called from FgInform object constructor/destructor


void FieldGeometry::addInformObject(class FgInform *inform)
       { _informer->addInformObject                (inform); }


void FieldGeometry::removeInformObject(class FgInform *inform)
       { _informer->removeInformObject                (inform); }




//---------------------- macros ---------------------------------//
//---------------------- macros ---------------------------------//
//---------------------- macros ---------------------------------//

   // suffix _NO     means nothing is affected except active/selected items.
   // suffix _DEL    means that something is being deleted.
   // suffix _DEPEND means that dependent values might be affected.
   // suffix _ZT1    means that                 live fold    is affected.
   // suffix _ZT2    means that                 live fold    is affected.
   // suffix _ZT3    means that                 live fold    is affected.
   // suffix _ZT4    means that                 live fold    is affected.
   // suffix _GRID   means that                 CMP gathers are affected.
   // suffix _COORDS means that                 CMP gathers are affected.
   // suffix _RM     means that        receiver/CMP gathers are affected.
   // suffix _SRM    means that source/receiver/CMP gathers are affected.

#define LOCKED_ALL     isLocked (LOCK_ALL)
#define LOCKED_DEL     isLocked (LOCK_DEL)
#define LOCKED_ZT1     isLocked (LOCK_S_R_CMP)
#define LOCKED_ZT2     isLocked (LOCK_S_R_CMP)
#define LOCKED_ZT3     isLocked (LOCK_S_R_CMP)
#define LOCKED_ZT4     isLocked (LOCK_S_R_CMP)
#define LOCKED_GRID    isLocked (LOCK_S_R_CMP)
#define LOCKED_COORDS  isLocked (LOCK_S_R_CMP)
#define LOCKED_RM      isLocked (LOCK_S_R)
#define LOCKED_SRM     isLocked (LOCK_S)

enum { CHNG_NO    , CHNG_DEPEND,
       CHNG_ZT1   , CHNG_ZT2   , CHNG_ZT3 , CHNG_ZT4,
       CHNG_GRID  , CHNG_COORDS, CHNG_RM  , CHNG_SRM,
       CHNG_TRED };	/* ehs */

#define BEFORE_NO     notifyDataWillChange (FALSE);
#define BEFORE_DEPEND notifyDataWillChange (TRUE);
#define BEFORE_ZT1    notifyDataWillChange (TRUE);
#define BEFORE_ZT2    notifyDataWillChange (TRUE);
#define BEFORE_ZT3    notifyDataWillChange (TRUE);
#define BEFORE_ZT4    notifyDataWillChange (TRUE);
#define BEFORE_GRID   notifyDataWillChange (TRUE);
#define BEFORE_COORDS notifyDataWillChange (TRUE);
#define BEFORE_RM     notifyDataWillChange (TRUE);
#define BEFORE_SRM    notifyDataWillChange (TRUE);

#define AFTER_NO      notifyDataHasChanged (CHNG_NO);
#define AFTER_DEPEND  notifyDataHasChanged (CHNG_DEPEND);
#define AFTER_ZT1     notifyDataHasChanged (CHNG_ZT1);
#define AFTER_ZT2     notifyDataHasChanged (CHNG_ZT2);
#define AFTER_ZT3     notifyDataHasChanged (CHNG_ZT3);
#define AFTER_ZT4     notifyDataHasChanged (CHNG_ZT4);
#define AFTER_GRID    notifyDataHasChanged (CHNG_GRID);
#define AFTER_COORDS  notifyDataHasChanged (CHNG_COORDS);
#define AFTER_RM      notifyDataHasChanged (CHNG_RM);
#define AFTER_SRM     notifyDataHasChanged (CHNG_SRM);



//----------------------- is locked ----------------------------//
//----------------------- is locked ----------------------------//
//----------------------- is locked ----------------------------//

   // private.
   // to be called before calling notifyDataWillChange if
   //   any changes to the data are requested.
   // should NOT be called if no changes are being made to the data.
   // should NOT be called if you are simply setting an "active" item
   //   or selecting/unselecting items.

   // set lowest_lock to the lowest value of the locking constant which
   //   will prohibit the requested change.

   // returns error = TRUE  if the requested change is locked.
   // returns error = FALSE if the requested change is allowed.

int FieldGeometry::isLocked(int lowest_lock)
{
  if(_lock < lowest_lock) return FALSE;
  _informer->showMessage("attempt to change data which is locked");
  _informer->ringBell();
  return TRUE;
}



//-------------- notify data will change or has changed ------------//
//-------------- notify data will change or has changed ------------//
//-------------- notify data will change or has changed ------------//

  // private.
  // to be called before and after data changes.
  // prohibits nested calls to make changes.
  // nested calls could occur if data users attempt
  //   to make changes from a virtual function which
  //   receives a message about changes.
  // in notifyDataWillChange:
  //   set chng to FALSE if data-needs-saving flag should NOT be turned on.
  //   set chng to TRUE  if data-needs-saving flag SHOULD be turned on.
  // in notifyDataHasChanged:
  //   set chng == CHNG_NO     if dependent values    will NOT go out-of-date.
  //   set chng >= CHNG_DEPEND if dependent values       might go out-of-date.
  //   set chng >= CHNG_ZT1    if live fold (not S,R,CMP) will go out of date.
  //   set chng >= CHNG_GRID   if CMPs (not sources/recs) will go out of date.
  //   set chng >= CHNG_RM     if recs/CMPs (not sources) will go out of date.
  //   set chng >= CHNG_SRM    if sources/recs/CMPs       will go out of date.


void FieldGeometry::notifyDataWillChange(int chng)
{
  assert(!_data_changing);
  _data_changing = TRUE;
  _informer->preMultipleOperations();
  if(!_frozen) _informer->preSlowOperations();
  if(chng)
      {
      _tv     ->startFromScratch();
      _headers->startHeadersFromScratch();
      if(!_data_needs_saving && (chng != CHNG_TRED))	/* ehs */
          {
          _data_needs_saving = TRUE;
          _informer->dataNeedsSavingFlagTurnedOn();
          }
      }
}


void FieldGeometry::notifyDataHasChanged(int chng)
{
  assert(_data_changing);
  if(chng >= CHNG_DEPEND &&  !_out_of_date && _frozen)
                            { _out_of_date = TRUE;
                              _tv      ->dependentValuesOutOfDate();
                              _informer->dependentValuesOutOfDate(); }
  switch(chng)
      {
      case CHNG_NO    :                                 break;
      case CHNG_DEPEND:                                 break;
      case CHNG_ZT1   : _tv->zt1CodesChanging       (); break;
      case CHNG_ZT2   : _tv->zt2CodesChanging       (); break;
      case CHNG_ZT3   : _tv->zt3CodesChanging       (); break;
      case CHNG_ZT4   : _tv->zt4CodesChanging       (); break;
      case CHNG_GRID  : _tv->transformChanging      (); break;
      case CHNG_COORDS: _tv->coordsChanging         (); break;
      case CHNG_RM    : _tv->receiverGathersChanging(); break;
      case CHNG_SRM   : _tv->sourceGathersChanging  (); break;
      case CHNG_TRED  : _tv->tredCodesChanging      (); break; /* ehs */
      default         : assert(FALSE);
      }
  if(!_frozen) _informer->postSlowOperations();
  _informer->postMultipleOperations();
  _data_changing = FALSE;
}



//----------------------- set data lock ------------------------//
//----------------------- set data lock ------------------------//
//----------------------- set data lock ------------------------//

       // public.

void FieldGeometry::setDataLock(int lock)
{
  switch(lock)
      {
      case LOCK_NONE   : break;
      case LOCK_DEL    : break;
      case LOCK_S      : break;
      case LOCK_S_R    : break;
      case LOCK_S_R_CMP: break;
      case LOCK_ALL    : break;
      default          : assert(FALSE);
      }
  _informer->preChangeDataLock();
  _lock = lock;
  _informer->postChangeDataLock();
}



//----------------------- maximize data lock ------------------------//
//----------------------- maximize data lock ------------------------//
//----------------------- maximize data lock ------------------------//

       // public.

void FieldGeometry::maximizeDataLock()
{
  _informer->preChangeDataLock();
  if(_lock < LOCK_S_R_CMP && !midpointGathersOutOfDate()) _lock = LOCK_S_R_CMP;
  if(_lock < LOCK_S_R     && !receiverGathersOutOfDate()) _lock = LOCK_S_R;
  if(_lock < LOCK_S       && !sourceGathersOutOfDate  ()) _lock = LOCK_S;
  _informer->postChangeDataLock();
}



//------------------ allow deleting and modifying ------------//
//------------------ allow deleting and modifying ------------//
//------------------ allow deleting and modifying ------------//

      // public.

int FieldGeometry::allowDeletingData     () const
                               { return (_lock < LOCK_DEL); }

int FieldGeometry::allowModifyingLdCards () const
                               { return (_lock < LOCK_S  ); }

int FieldGeometry::allowModifyingPpCards () const
                               { return (_lock < LOCK_S  ); }

int FieldGeometry::allowModifyingRpCards () const
                               { return (_lock < LOCK_S_R); }

int FieldGeometry::allowModifyingZt1Cards() const
                               { return (_lock < LOCK_S  ); }

int FieldGeometry::allowModifyingZt2Cards() const
                               { return (_lock < LOCK_S_R); }

int FieldGeometry::allowModifyingZt3Cards() const
                               { return (_lock < LOCK_S  ); }

int FieldGeometry::allowModifyingZt4Cards() const
                               { return (_lock < LOCK_S  ); }

int FieldGeometry::allowModifyingGridTransform() const
                               { return (_lock < LOCK_S_R_CMP); }



//------------------ turn off data needs saving flag ----------------//
//------------------ turn off data needs saving flag ----------------//
//------------------ turn off data needs saving flag ----------------//

        // public
        // to be called after saving the field geometry data,
        // perhaps to a file.

void FieldGeometry::turnOffDataNeedsSavingFlag()
{
  if(_data_needs_saving)
      {
      _data_needs_saving = FALSE;
      _informer->dataNeedsSavingFlagTurnedOff();
      }
}



//----------------- pre and post major changes ------------------//
//----------------- pre and post major changes ------------------//
//----------------- pre and post major changes ------------------//


void FieldGeometry::preMajorChanges()
{
  _major++;
  if(_major == 1)
      {
      _informer->preMultipleOperations();
      _informer->preSlowOperations();
      freezeDependentUpdates();
      }
}



void FieldGeometry::postMajorChanges()
{
  if(_major == 1)
      {
      resumeDependentUpdates();
      _informer->postSlowOperations();
      _informer->postMultipleOperations();
      }
  if(_major > 0) _major--;
}






//--------------- interface to FgInform data users --------------//
//--------------- interface to FgInform data users --------------//
//--------------- interface to FgInform data users --------------//


void FieldGeometry::preMultipleOperations()
       { _informer->preMultipleOperations(); }

void FieldGeometry::postMultipleOperations()
       { _informer->postMultipleOperations(); }

void FieldGeometry::preSlowOperations()
       { _informer->preSlowOperations(); }

void FieldGeometry::postSlowOperations()
       { _informer->postSlowOperations(); }

void FieldGeometry::ringBell()
       { _informer->ringBell(); }

void FieldGeometry::showMessage(char *msg)
       { _informer->showMessage(msg); }

void FieldGeometry::sendMessage(char *msg, long i1, long i2, long i3,
                                                    long i4, long i5)
       { _informer->sendMessage(msg, i1, i2, i3, i4, i5); }

void FieldGeometry::returningToEventLoop()
       { _informer->returningToEventLoop(); }



//---------------------- allow setting value ---------------------//
//---------------------- allow setting value ---------------------//
//---------------------- allow setting value ---------------------//

           // public
           // returns TRUE if setting value is allowed.
           // returns FALSE if setting value is not allowed.

int FieldGeometry::allowSettingXgrid()  const
          { return allowSettingXloc (); }


int FieldGeometry::allowSettingYgrid()  const
          { return allowSettingXloc (); }


int FieldGeometry::allowSettingValue(int ident)  const
{
  if(ident < FIRST_FIELD_FLAG_VARIABLE ||
     ident > LAST_FIELD_FLAG_VARIABLE) return FALSE;
  int allow = TRUE;
  switch(ident)
     {
     case FG_DIST : allow = allowSettingIncrDistance(); break;
     case FG_XLOC : allow = allowSettingXloc();         break;
     case FG_XGRID: allow = allowSettingXgrid();        break;
     case FG_YGRID: allow = allowSettingYgrid();        break;
     case FG_CUM  : allow = FALSE;                      break;
     case FG_AZIM : allow = FALSE;                      break;
     default:       allow = TRUE;                       break;
     }
  return allow;
}





//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//
//--------- pass-thru functions to active grid transform ---------//



//--------------------- get values --------------------------//
//--------------------- get values --------------------------//
//--------------------- get values --------------------------//


double FieldGeometry::getXorigin()  const
 { return _transform->getXorigin(); }

double FieldGeometry::getYorigin()  const
 { return _transform->getYorigin(); }

double FieldGeometry::getRotationAngle()  const
 { return _transform->getRotationAngle(); }

double FieldGeometry::getXgridWidth()  const
 { return _transform->getXgridWidth(); }

double FieldGeometry::getYgridWidth()  const
 { return _transform->getYgridWidth(); }

int    FieldGeometry::isRightHanded()  const
 { return _transform->isRightHanded(); }

int    FieldGeometry::isLeftHanded()  const
 { return _transform->isLeftHanded(); }



double FieldGeometry::getCosineAngle()  const
 { return _transform->getCosineAngle(); }

double FieldGeometry::getSineAngle()  const
 { return _transform->getSineAngle(); }

double FieldGeometry::getDx11()  const
 { return _transform->getDx11(); }

double FieldGeometry::getDx12()  const
 { return _transform->getDx12(); }

double FieldGeometry::getDx21()  const
 { return _transform->getDx21(); }

double FieldGeometry::getDx22()  const
 { return _transform->getDx22(); }

double FieldGeometry::getDn11()  const
 { return _transform->getDn11(); }

double FieldGeometry::getDn12()  const
 { return _transform->getDn12(); }

double FieldGeometry::getDn21()  const
 { return _transform->getDn21(); }

double FieldGeometry::getDn22()  const
 { return _transform->getDn22(); }

double FieldGeometry::getDeterminant()  const
 { return _transform->getDeterminant(); }



//---------------- get transformed coordinates -------------------//
//---------------- get transformed coordinates -------------------//
//---------------- get transformed coordinates -------------------//


double FieldGeometry::getXlocCoord(double xgrid, double ygrid)  const
 { return _transform->getXlocCoord(       xgrid,        ygrid); }

double FieldGeometry::getYlocCoord(double xgrid, double ygrid)  const
 { return _transform->getYlocCoord(       xgrid,        ygrid); }

double FieldGeometry::getXgridCoord(double xloc, double yloc)  const
 { return _transform->getXgridCoord(       xloc,        yloc); }

double FieldGeometry::getYgridCoord(double xloc, double yloc)  const
 { return _transform->getYgridCoord(       xloc,        yloc); }



//------------------------ set values ---------------------------//
//------------------------ set values ---------------------------//
//------------------------ set values ---------------------------//


void FieldGeometry::setGridTransformValues()
{
  if(LOCKED_GRID) return;
  BEFORE_GRID
  _informer->preSlowOperations();
  _informer->preNewGridTransform();
  _transform->setGridTransformValues(_testing);
  _informer->postNewGridTransform();
  _informer->postSlowOperations();
  AFTER_GRID
}


// this also sets the testing transform:

void FieldGeometry::incrementGridCoords(double xstep, double ystep)
{
  if(xstep == DNIL || ystep == DNIL) return;
  if(xstep ==  0.0 && ystep ==  0.0) return;
  long ixstep = NearestInteger(xstep);
  long iystep = NearestInteger(ystep);
  if(xstep != ixstep || ystep != iystep)
      {
      if(LOCKED_GRID) return;
      BEFORE_GRID
      _informer->preSlowOperations();
      _informer->preNewGridTransform();
      _informer->preNewTestingGridTransform();
      _transform->incrementGridCoords( xstep,  ystep);
      _testing  ->incrementGridCoords( xstep,  ystep);
      _midpoints->incrementGridCoords(ixstep, iystep);
      _informer->postNewTestingGridTransform();
      _informer->postNewGridTransform();
      _informer->postSlowOperations();
      AFTER_GRID
      return;
      }
  if(LOCKED_ALL) return;
  if(_frozen) return;
  BEFORE_DEPEND
  _informer->preSlowOperations();
  _informer->preNewGridTransform();
  _informer->preNewTestingGridTransform();
  _transform->incrementGridCoords( xstep,  ystep);
  _testing  ->incrementGridCoords( xstep,  ystep);
  _midpoints->incrementGridCoords(ixstep, iystep);
  _informer->postNewTestingGridTransform();
  _informer->postNewGridTransform();
  _informer->postSlowOperations();
  AFTER_DEPEND
}



//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//
//--------- pass-thru functions to testing grid transform --------------//



//--------------------- get values --------------------------//
//--------------------- get values --------------------------//
//--------------------- get values --------------------------//


double FieldGeometry::getTestingXorigin()  const
          { return _testing->getXorigin(); }

double FieldGeometry::getTestingYorigin()  const
          { return _testing->getYorigin(); }

double FieldGeometry::getTestingRotationAngle()  const
          { return _testing->getRotationAngle(); }

double FieldGeometry::getTestingXgridWidth()  const
          { return _testing->getXgridWidth(); }

double FieldGeometry::getTestingYgridWidth()  const
          { return _testing->getYgridWidth(); }

int    FieldGeometry::isTestingRightHanded()  const
          { return _testing->isRightHanded(); }

int    FieldGeometry::isTestingLeftHanded()  const
          { return _testing->isLeftHanded(); }



double FieldGeometry::getTestingCosineAngle()  const
          { return _testing->getCosineAngle(); }

double FieldGeometry::getTestingSineAngle()  const
          { return _testing->getSineAngle(); }

double FieldGeometry::getTestingDx11()  const
          { return _testing->getDx11(); }

double FieldGeometry::getTestingDx12()  const
          { return _testing->getDx12(); }

double FieldGeometry::getTestingDx21()  const
          { return _testing->getDx21(); }

double FieldGeometry::getTestingDx22()  const
          { return _testing->getDx22(); }

double FieldGeometry::getTestingDn11()  const
          { return _testing->getDn11(); }

double FieldGeometry::getTestingDn12()  const
          { return _testing->getDn12(); }

double FieldGeometry::getTestingDn21()  const
          { return _testing->getDn21(); }

double FieldGeometry::getTestingDn22()  const
          { return _testing->getDn22(); }

double FieldGeometry::getTestingDeterminant()  const
          { return _testing->getDeterminant(); }

void   FieldGeometry::getTestingGridTransformValues
                                  (GridTransform *transform)  const
          { _testing->getGridTransformValues(transform); }



//---------------- get transformed coordinates -------------------//
//---------------- get transformed coordinates -------------------//
//---------------- get transformed coordinates -------------------//


double FieldGeometry::getTestingXlocCoord(double xgrid, double ygrid)  const
          { return _testing->getXlocCoord(       xgrid,        ygrid); }

double FieldGeometry::getTestingYlocCoord(double xgrid, double ygrid)  const
          { return _testing->getYlocCoord(       xgrid,        ygrid); }

double FieldGeometry::getTestingXgridCoord(double xloc, double yloc)  const
          { return _testing->getXgridCoord(       xloc,        yloc); }

double FieldGeometry::getTestingYgridCoord(double xloc, double yloc)  const
          { return _testing->getYgridCoord(       xloc,        yloc); }



//------------------------ set values ---------------------------//
//------------------------ set values ---------------------------//
//------------------------ set values ---------------------------//

/*
#define BEFORE_TESTING                       \
  _informer->preSlowOperations();            \
  _informer->preNewTestingGridTransform();

#define AFTER_TESTING                         \
  _informer->postSlowOperations();            \
  _informer->postNewTestingGridTransform();
*/

#define BEFORE_TESTING                       \
  _informer->preNewTestingGridTransform();

#define AFTER_TESTING                         \
  _informer->postNewTestingGridTransform();



void FieldGeometry::resetTestingGridTransformValues()
{
  BEFORE_TESTING
  _testing ->setGridTransformValues(_transform);
  AFTER_TESTING
}



//------------------------ set values ---------------------------//
//------------------------ set values ---------------------------//
//------------------------ set values ---------------------------//

#define TRAN_SET(setTestingXorigin, setXorigin)        \
void FieldGeometry::setTestingXorigin(double value)    \
{                                                      \
  BEFORE_TESTING                                       \
  _testing->setXorigin(value);                         \
  AFTER_TESTING                                        \
}


TRAN_SET (setTestingXorigin      , setXorigin)
TRAN_SET (setTestingYorigin      , setYorigin)
TRAN_SET (setTestingRotationAngle, setRotationAngle)
TRAN_SET (setTestingXgridWidth   , setXgridWidth)
TRAN_SET (setTestingYgridWidth   , setYgridWidth)
TRAN_SET (setTestingDx11         , setDx11)
TRAN_SET (setTestingDx21         , setDx21)
TRAN_SET (setTestingDx12         , setDx12)
TRAN_SET (setTestingDx22         , setDx22)
TRAN_SET (setTestingDn11         , setDn11)
TRAN_SET (setTestingDn21         , setDn21)
TRAN_SET (setTestingDn12         , setDn12)
TRAN_SET (setTestingDn22         , setDn22)


void FieldGeometry::setTestingRightHanded   ()
{
  BEFORE_TESTING
  _testing->setRightHanded();
  AFTER_TESTING
}


void FieldGeometry::setTestingLeftHanded    ()
{
  BEFORE_TESTING
  _testing->setLeftHanded();
  AFTER_TESTING
}



void FieldGeometry::setTestingRightHandedTransform
                         (double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth)
{
  BEFORE_TESTING
  _testing->setRightHandedTransform(xorigin, yorigin, angle, xwidth, ywidth);
  AFTER_TESTING
}


void FieldGeometry::setTestingLeftHandedTransform
                         (double xorigin, double yorigin,
                          double angle, double xwidth, double ywidth)
{
  BEFORE_TESTING
  _testing->setLeftHandedTransform(xorigin, yorigin, angle, xwidth, ywidth);
  AFTER_TESTING
}



void FieldGeometry::setTestingForwardRotationMatrix
                (double dx11, double dx12, double dx21, double dx22)
{
  BEFORE_TESTING
  _testing->setForwardRotationMatrix(dx11, dx12, dx21, dx22);
  AFTER_TESTING
}


void FieldGeometry::setTestingReverseRotationMatrix
                (double dn11, double dn12, double dn21, double dn22)
{
  BEFORE_TESTING
  _testing->setReverseRotationMatrix(dn11, dn12, dn21, dn22);
  AFTER_TESTING
}


void FieldGeometry::setTestingGridTransformValues
                                  (const GridTransform *transform)
{
  BEFORE_TESTING
  _testing->setGridTransformValues(transform);
  AFTER_TESTING
}



//------------ useful functions to define coordinate system ---------//
//------------ useful functions to define coordinate system ---------//
//------------ useful functions to define coordinate system ---------//


void FieldGeometry::defineTestingOrigin(double xgrid, double ygrid,
                                 double xloc , double yloc )
{
  BEFORE_TESTING
  _testing->defineOrigin(xgrid, ygrid, xloc, yloc);
  AFTER_TESTING
}


void FieldGeometry::defineTestingRotationAngle(double xloc1, double yloc1,
                                        double xloc2, double yloc2)
{
  BEFORE_TESTING
  _testing->defineRotationAngle(xloc1, yloc1, xloc2, yloc2);
  AFTER_TESTING
}



void FieldGeometry::defineTestingOriginAndAngle(double xgrid, double ygrid,
                                         double xloc1, double yloc1,
                                         double xloc2, double yloc2)
{
  BEFORE_TESTING
  _testing->defineOriginAndAngle(xgrid, ygrid, xloc1, yloc1, xloc2, yloc2);
  AFTER_TESTING
}


void FieldGeometry::refineTestingBinCenter(double xloc, double yloc)
{
  BEFORE_TESTING
  _testing->refineBinCenter(xloc, yloc);
  AFTER_TESTING
}


void FieldGeometry::refineTestingRotationAngle(double xloc, double yloc)
{
  BEFORE_TESTING
  _testing->refineRotationAngle(xloc, yloc);
  AFTER_TESTING
}






//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//
//-------------- pass-thru functions to seis survey -----------------//

                     // ixl = index of desired line.


//----------------------- get values from survey --------------------//
//----------------------- get values from survey --------------------//
//----------------------- get values from survey --------------------//


#define SURVEY_GET(int2, getChaining)       \
int2  FieldGeometry::getChaining()  const   \
{                                           \
  return _survey->getChaining();            \
}


SURVEY_GET (int   , getChaining          )
SURVEY_GET (int   , allowSettingIncrDistance)
SURVEY_GET (int   , allowSettingXloc     )
SURVEY_GET (int   , allowChangeChaining  )
SURVEY_GET (int   , allowReverseLineDirections)
SURVEY_GET (int   , linesAreDuplicatedOrNotSorted)
SURVEY_GET (double, minimumXlocInSurvey)
SURVEY_GET (double, maximumXlocInSurvey)
SURVEY_GET (double, minimumYlocInSurvey)
SURVEY_GET (double, maximumYlocInSurvey)


double FieldGeometry::minimumXgridInSurvey()  const
{
  double xmin = DZERO;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      double x = minimumXgridOnLine(ixl);
      if(ixl == 0 || x < xmin) xmin = x;
      }
  return xmin;
}


double FieldGeometry::maximumXgridInSurvey()  const
{
  double xmax = DZERO;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      double x = maximumXgridOnLine(ixl);
      if(ixl == 0 || x > xmax) xmax = x;
      }
  return xmax;
}


double FieldGeometry::minimumYgridInSurvey()  const
{
  double ymin = DZERO;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      double y = minimumYgridOnLine(ixl);
      if(ixl == 0 || y < ymin) ymin = y;
      }
  return ymin;
}


double FieldGeometry::maximumYgridInSurvey()  const
{
  double ymax = DZERO;
  long n = numLines();
  for(long ixl = 0; ixl < n; ixl++)
      {
      double y = maximumYgridOnLine(ixl);
      if(ixl == 0 || y > ymax) ymax = y;
      }
  return ymax;
}



SURVEY_GET (long  , numLines             )
SURVEY_GET (long  , numSelectedLines     )
SURVEY_GET (long  , totNumSources        )
SURVEY_GET (long  , totNumReceivers      )
SURVEY_GET (long  , totNumFlags          )

SURVEY_GET (int   , deadSourceCodesAreSet)
SURVEY_GET (int   , deadReceiverCodesAreSet)
SURVEY_GET (long  , totNumDeadSources)
SURVEY_GET (long  , totNumDeadReceivers)
SURVEY_GET (long  , totNumReversedSources)
SURVEY_GET (long  , totNumReversedReceivers)
SURVEY_GET (long  , totNumMissingSources)
SURVEY_GET (long  , totNumMissingReceivers)
SURVEY_GET (long  , totNumLiveSources)
SURVEY_GET (long  , totNumLiveReceivers)

SURVEY_GET (long  , getFirstLineNumber   )
SURVEY_GET (long  , getLastLineNumber    )
SURVEY_GET (long  , getSmallestLineNumber)
SURVEY_GET (long  , getLargestLineNumber )
SURVEY_GET (long  , getMinLineIncrement  )
SURVEY_GET (long  , getMaxLineIncrement  )



//----------------------- set values in survey --------------------//
//----------------------- set values in survey --------------------//
//----------------------- set values in survey --------------------//


void   FieldGeometry::setChaining (int chaining)
{
  if(LOCKED_COORDS) return;
  BEFORE_COORDS
  _survey->setChaining(chaining);
  AFTER_COORDS
}



void   FieldGeometry::sortByLineNumber    ()
{
  if(LOCKED_SRM) return;
  BEFORE_SRM
  _survey->sortByLineNumber();
  AFTER_SRM
}


void   FieldGeometry::freezeDependentUpdates       ()
{
  if(_frozen) return;
////////// interchanged the following two lines 4/18/96:
  _informer->freezingDependentUpdates();
  _frozen = TRUE;
////////// interchanged the above two lines 4/18/96.
  _survey  ->freezeDependentUpdates();
  _rp_cards->freezeDependentUpdates();
  _pp_cards->freezeDependentUpdates();
}


void   FieldGeometry::resumeDependentUpdates       ()
{
  if(!_frozen) return;
  int chng1 = FALSE;
  int chng2 = CHNG_NO;
  if(_out_of_date) chng1 = TRUE;
  if(_out_of_date) chng2 = CHNG_SRM;
  notifyDataWillChange(chng1);   // does not do preSlowOperations.
  _informer->preSlowOperations();
  _informer->preResumeDependentUpdates();
  _survey  ->resumeDependentUpdates();
  _rp_cards->resumeDependentUpdates();
  _pp_cards->resumeDependentUpdates();
  _out_of_date = FALSE;
  _frozen      = FALSE;
  _informer->postResumeDependentUpdates();
  _tv      ->dependentUpdatesResumed();
  notifyDataHasChanged(chng2);   // does postSlowOperations.
}



//----------------- search among seismic lines ------------------//
//----------------- search among seismic lines ------------------//
//----------------- search among seismic lines ------------------//

           // these return an index (ixl), or -1 if not found.

long  FieldGeometry::findNearestLineNumber (long line_number, int dir) const
{
  return _survey->findNearestLineNumber(line_number, dir);
}


long  FieldGeometry::findMatchingLineNumber(long line_number)          const
{
  return _survey->findMatchingLineNumber(line_number);
}


long  FieldGeometry::findNearestLine  (double xloc, double yloc)  const
{
  return _survey->findNearestLine(xloc, yloc);
}


long  FieldGeometry::findNearestLineWithSource
                                      (double xloc, double yloc)  const
{
  return _survey->findNearestLineWithSource(xloc, yloc);
}


long  FieldGeometry::findNearestLineWithReceiver
                                      (double xloc, double yloc)  const
{
  return _survey->findNearestLineWithReceiver(xloc, yloc);
}





//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//
//------------- pass-thru functions to individual lines ---------------//

                // ixl = index of desired line.
                // ixf = index of desired flag on desired line.


//----------------- get seismic line values ----------------------//
//----------------- get seismic line values ----------------------//
//----------------- get seismic line values ----------------------//


#define DEF(long2, getLineNumber)                     \
long2 FieldGeometry::getLineNumber(long ixl)  const   \
{                                                     \
  return _survey->getLineNumber(ixl);                 \
}



DEF(long  , getLineNumber              )
DEF(int   , lineIsSelected             )
DEF(char  , getLineSelectValue         )
DEF(long  , numSourcesOnLine           )
DEF(long  , numReceiversOnLine         )
DEF(long  , numFlagsOnLine             )
DEF(long  , numSelectedFlagsOnLine     )
DEF(long  , firstCumulativeGroundPosition  )
DEF(long  , firstMatchableGroundPosition   )
DEF(float , getFirstShotpointOnLine    )
DEF(float , getLastShotpointOnLine     )
DEF(float , getSmallestShotpointOnLine )
DEF(float , getLargestShotpointOnLine  )
DEF(float , getMinShotpointIncrOnLine  )
DEF(float , getMaxShotpointIncrOnLine  )
DEF(int   , shotpointsAreDuplicatedOrNotSorted  )
DEF(double, minimumXlocOnLine  )
DEF(double, maximumXlocOnLine  )
DEF(double, minimumYlocOnLine  )
DEF(double, maximumYlocOnLine  )


long FieldGeometry::firstGroundPosition(long ixl)  const
{
  float fixdist = _tv->getFixdist();
  if(fixdist <= 0.0)
      {
      return _survey->firstCumulativeGroundPosition(ixl);
      }
  return _survey->firstMatchableGroundPosition(ixl);
}


double FieldGeometry::minimumXgridOnLine(long ixl)  const
{
  double xmin = DZERO;
  long n = numFlagsOnLine(ixl);
  for(long ixf = 0; ixf < n; ixf++)
      {
      double x = getXgrid(ixl, ixf);
      if(ixf == 0 || x < xmin) xmin = x;
      }
  return xmin;
}


double FieldGeometry::maximumXgridOnLine(long ixl)  const
{
  double xmax = DZERO;
  long n = numFlagsOnLine(ixl);
  for(long ixf = 0; ixf < n; ixf++)
      {
      double x = getXgrid(ixl, ixf);
      if(ixf == 0 || x > xmax) xmax = x;
      }
  return xmax;
}


double FieldGeometry::minimumYgridOnLine(long ixl)  const
{
  double ymin = DZERO;
  long n = numFlagsOnLine(ixl);
  for(long ixf = 0; ixf < n; ixf++)
      {
      double y = getYgrid(ixl, ixf);
      if(ixf == 0 || y < ymin) ymin = y;
      }
  return ymin;
}


double FieldGeometry::maximumYgridOnLine(long ixl)  const
{
  double ymax = DZERO;
  long n = numFlagsOnLine(ixl);
  for(long ixf = 0; ixf < n; ixf++)
      {
      double y = getYgrid(ixl, ixf);
      if(ixf == 0 || y > ymax) ymax = y;
      }
  return ymax;
}


double FieldGeometry::distanceToLine
                         (long ixl, double xloc, double yloc)  const
{
  return _survey->distanceToLine(ixl, xloc, yloc);
}


double FieldGeometry::distanceSquaredToLine
                         (long ixl, double xloc, double yloc)  const
{
  return _survey->distanceSquaredToLine(ixl, xloc, yloc);
}



//----------------- set seismic line values ----------------------//
//----------------- set seismic line values ----------------------//
//----------------- set seismic line values ----------------------//


void   FieldGeometry::setLineNumber     (long ixl, long line_number)
{
  if(LOCKED_SRM) return;
  BEFORE_SRM
  _survey->setLineNumber(ixl, line_number);
  AFTER_SRM
}



void   FieldGeometry::reverseLineDirection  (long ixl)
{
  if(LOCKED_SRM) return;
  BEFORE_SRM
  _survey->reverseLineDirection(ixl);
  AFTER_SRM
}



void FieldGeometry::setLineSelectValue(long ixl, char value)
{
  BEFORE_NO
  _survey->setLineSelectValue(ixl, value);
  AFTER_NO      // was AFTER (see note above)
}


void FieldGeometry::incrementLineSelectValue(long ixl)
{
  BEFORE_NO
  _survey->incrementLineSelectValue(ixl);
  AFTER_NO      // was AFTER (see note above)
}


void FieldGeometry::clearLineSelections()
{
  BEFORE_NO
  _survey->clearLineSelections();
  AFTER_NO      // was AFTER (see note above)
}




//---------------- search along seismic line -------------------//
//---------------- search along seismic line -------------------//
//---------------- search along seismic line -------------------//

           // these return an index (ixf), or -1 if not found.


long FieldGeometry::findNearestShotpointOnLine
                            (long ixl, float shotpoint, int dir)  const
{
  return _survey->findNearestShotpointOnLine(ixl, shotpoint, dir);
}


long FieldGeometry::findMatchingShotpointOnLine
                            (long ixl, float shotpoint)  const
{
  return _survey->findMatchingShotpointOnLine(ixl, shotpoint);
}


long FieldGeometry::findNearestFlagOnLine
                                 (long ixl, double xloc, double yloc) const
{
  return _survey->findNearestFlagOnLine(ixl, xloc, yloc);
}


long FieldGeometry::findNearestSourceOnLine
                                   (long ixl, double xloc, double yloc) const
{
  return _survey->findNearestSourceOnLine(ixl, xloc, yloc);
}


long FieldGeometry::findNearestReceiverOnLine
                                   (long ixl, double xloc, double yloc) const
{
  return _survey->findNearestReceiverOnLine(ixl, xloc, yloc);
}


//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//
//----------------- pass-thru to sources and receivers --------------//

   // ixl  = index of desired line.
   // ixf  = index of desired flag on desired line.
   // ixs2 = index of desired source at desired flag on desired line.
   // ixr2 = index of desired receiver at desired flag on desired line.

//----------------- get source-receiver values -----------------//
//----------------- get source-receiver values -----------------//
//----------------- get source-receiver values -----------------//


long FieldGeometry::numSourcesAtFlag    (long ixl, long ixf) const
{
  return _survey->numSourcesAtFlag(ixl, ixf);
}

long FieldGeometry::numReceiversAtFlag  (long ixl, long ixf) const
{
  return _survey->numReceiversAtFlag(ixl, ixf);
}

int  FieldGeometry::flagHasSource       (long ixl, long ixf) const
{
  return _survey->flagHasSource(ixl, ixf);
}

int  FieldGeometry::flagHasReceiver     (long ixl, long ixf) const
{
  return _survey->flagHasReceiver(ixl, ixf);
}


long FieldGeometry::sourceGroupNumber    (long ixl, long ixf, long ixs2) const
{
  return _survey->sourceGroupNumber(ixl, ixf, ixs2);
}

long FieldGeometry::receiverTraceNumber  (long ixl, long ixf, long ixr2) const
{
  return _survey->receiverTraceNumber(ixl, ixf, ixr2);
}




//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//
//----------- pass-thru functions to individual flags ----------------//

       // the functions regarding xgrid and ygrid are indirect
       // pass-thru functions which use transformations.

     // ixl  = index of desired line.
     // ixf  = index of desired flag on desired line.



//----------------------- get flag values ------------------------//
//----------------------- get flag values ------------------------//
//----------------------- get flag values ------------------------//


class FieldFlag *FieldGeometry::getFlagPointer(long ixl, long ixf)  const
{
  return _survey->getFlagPointer(ixl, ixf);
}


long FieldGeometry::getLineIndex(class FieldFlag *field_flag)  const
{
  return _survey->getLineIndex(field_flag);
}


long FieldGeometry::getFlagIndex(class FieldFlag *field_flag)  const
{
  return _survey->getFlagIndex(field_flag);
}


int    FieldGeometry::flagIsSelected   (long ixl, long ixf)  const
{
  return _survey->flagIsSelected(ixl, ixf);
}


double FieldGeometry::distanceToFlag
               (long ixl, long ixf, double xloc, double yloc) const
{
  return _survey->distanceToFlag(ixl, ixf, xloc, yloc);
}


double FieldGeometry::distanceSquaredToFlag
               (long ixl, long ixf, double xloc, double yloc) const
{
  return _survey->distanceSquaredToFlag(ixl, ixf, xloc, yloc);
}


int    FieldGeometry::flagValueIsDependent
                           (long ixl, long ixf, int ident)  const
{
  if(ident == FG_XGRID || ident == FG_YGRID)
      {
      int xdep = _survey->flagValueIsDependent(ixl, ixf, FG_XLOC);
      int ydep = _survey->flagValueIsDependent(ixl, ixf, FG_YLOC);
      return (xdep || ydep);
      }
  return _survey->flagValueIsDependent(ixl, ixf, ident);
}


double FieldGeometry::getFlagValue  (long ixl, long ixf, int ident)  const
{
  if(ident == FG_XGRID) return getXgrid(ixl, ixf);
  if(ident == FG_YGRID) return getYgrid(ixl, ixf);
  return _survey->getFlagValue(ixl, ixf, ident);
}




#define GETV(float, getShotpoint)                               \
float  FieldGeometry::getShotpoint(long ixl, long ixf)  const   \
{                                                               \
  return _survey->getShotpoint(ixl, ixf);                       \
}


GETV(float , getShotpoint       )
GETV(double, getIncrDistance    )
GETV(double, getXloc            )
GETV(double, getYloc            )
GETV(float , getElevation       )
GETV(float , getHoleDepth       )
GETV(float , getUpholeTime      )
GETV(float , getReceiverStatic  )
GETV(float , getSourceStatic    )
GETV(float , getReceiverXskid   )
GETV(float , getReceiverYskid   )
GETV(float , getReceiverEskid   )
GETV(char  , getFlagSelectValue )
GETV(double, getCumDistance     )
GETV(double, getAzimuth         )
GETV(int   , getDeadSourceCode  )
GETV(int   , getDeadReceiverCode)
GETV(int   , sourceMaybeDead    )
GETV(int   , receiverMaybeDead  )
GETV(long  , getCumulativeGroundPosition)
GETV(long  , getMatchableGroundPosition)


long FieldGeometry::getGroundPosition(long ixl, long ixf)  const
{
  float fixdist = _tv->getFixdist();
  if(fixdist <= 0.0)
      {
      return _survey->getCumulativeGroundPosition(ixl, ixf);
      }
  return _survey->getMatchableGroundPosition(ixl, ixf);
}


double FieldGeometry::getXgrid(long ixl, long ixf)  const
{
  double xloc = _survey->getXloc(ixl, ixf);
  double yloc = _survey->getYloc(ixl, ixf);
  return _transform->getXgridCoord(xloc, yloc);
}


double FieldGeometry::getYgrid(long ixl, long ixf)  const
{
  double xloc = _survey->getXloc(ixl, ixf);
  double yloc = _survey->getYloc(ixl, ixf);
  return _transform->getYgridCoord(xloc, yloc);
}



//------------------ find ground position ------------------//
//------------------ find ground position ------------------//
//------------------ find ground position ------------------//

   // these return ixf, or -1 if not found.
   // the first function also returns ixl, or -1 if not found.
   // there may be up to one matching matchable_gp on each line.
   // there will be only one matching cumulative_gp in survey.
   // the last function assumes gp == cumulative_gp when fixdist <= 0.
   // the last function assumes gp == matchable_gp  when fixdist >  0.


void FieldGeometry::findCumulativeGroundPosition(long cumulative_gp,
                                           long *ixl, long *ixf)  const
{
  _survey->findCumulativeGroundPosition(cumulative_gp, ixl, ixf);
}


long FieldGeometry::findCumulativeGroundPosition
                               (long ixl, long cumulative_gp)  const
{
  return _survey->findCumulativeGroundPosition(ixl, cumulative_gp);
}


long FieldGeometry::findMatchableGroundPosition
                               (long ixl, long matchable_gp)  const
{
  return _survey->findMatchableGroundPosition(ixl, matchable_gp);
}


long FieldGeometry::findGroundPosition(long ixl, long gp)  const
{
  float fixdist = _tv->getFixdist();
  if(fixdist <= 0.0)
      {
      return _survey->findCumulativeGroundPosition(ixl, gp);
      }
  return _survey->findMatchableGroundPosition(ixl, gp);
}



//------------------- get skidded coords ------------------------//
//------------------- get skidded coords ------------------------//
//------------------- get skidded coords ------------------------//

   // skidded coords          = flag coords + argument skids.
   // skidded coords plus     = flag coords + argument skids
   //                                       + receiver skids on LD card.

   // skidded  source  coords = flag coords +   source skids on PP card.
   // skidded receiver coords = flag coords + receiver skids on LD card.
   // skidded  trace   coords = flag coords + receiver skids on PP card +
   //                                          channel skids on RP card.

   // source   gathers must have been created when using ixs2.
   // receiver gathers must have been created when using ixr2.


void FieldGeometry::getSkiddedCoords(long ixl, long ixf,
                           float inline_skid, float crossline_skid,
                           double *x, double *y)  const
{
  _survey->getSkiddedCoords
                       (ixl, ixf, inline_skid, crossline_skid, x, y);
}


void FieldGeometry::getSkiddedCoordsPlus(long ixl, long ixf,
                           float inline_skid, float crossline_skid,
                           double *x, double *y)  const
{
  _survey->getSkiddedCoordsPlus
                       (ixl, ixf, inline_skid, crossline_skid, x, y);
}



void FieldGeometry::getSourceSkids(long ixpp, long group,
                      float *inline_skid, float *crossline_skid)  const
{
  _pp_cards->getSourceSkids(ixpp, group, inline_skid, crossline_skid);
}


void FieldGeometry::getSourceSkids(long group,
                      float *inline_skid, float *crossline_skid)  const
{
  long ixpp = _pp_cards->findPpCardWithDesiredGroup(group);
  if(ixpp == -1)
      {
      *inline_skid    = 0.0;
      *crossline_skid = 0.0;
      return;
      }
  _pp_cards->getSourceSkids(ixpp, group, inline_skid, crossline_skid);
}


void FieldGeometry::getSourceSkids(long ixl, long ixf, long ixs2,
                      float *inline_skid, float *crossline_skid)  const
{
  long group = _survey->sourceGroupNumber(ixl, ixf, ixs2);
  getSourceSkids(group, inline_skid, crossline_skid);
}


int FieldGeometry::receiverIsSkidded(long ixl, long ixf)  const
{
  return _survey->receiverIsSkidded(ixl, ixf);
}


int FieldGeometry::sourceIsSkidded(long ixl, long ixf, long ixs2)  const
{
/*
  long group = _survey->sourceGroupNumber(ixl, ixf, ixs2);
  long ixpp  = _pp_cards->findPpCardWithDesiredGroup(group);
  if(ixpp == -1) return FALSE;
  float inline_skid, crossline_skid;
  _pp_cards->getSourceSkids(ixpp, group, &inline_skid, &crossline_skid);
*/
  float inline_skid, crossline_skid;
  getSourceSkids(ixl, ixf, ixs2, &inline_skid, &crossline_skid);
  return (inline_skid != 0.0 || crossline_skid != 0.0);
}


// this will not work if the trace has been skidded to another flag,
// and that flag has no receiver skid, and there is no pattern skid:

int FieldGeometry::traceIsSkidded
                       (long /*ixl*/, long /*ixf*/, long /*ixr2*/)  const
{
     assert(FALSE);
      //// do not let people call this until I get it fixed...
/*
  if(_survey->getReceiverXskid(ixl, ixf) != 0.0) return TRUE;
  if(_survey->getReceiverYskid(ixl, ixf) != 0.0) return TRUE;
  // if the trace has not been skidded to another flag, the above are correct.
  // if the trace has been so skidded, then obviously TRUE is correct.
  long trace = _survey->receiverTraceNumber(ixl, ixf, ixr2);
  long ixpp  = _pp_cards->findPpCardWithDesiredTrace(trace);
  if(ixpp == -1) return FALSE;   // should never be FALSE.
  long group, channel;
  _pp_cards->getGroupAndChannel(ixpp, trace, &group, &channel);
  if(group == 0 || channel == 0) return FALSE;   // should never be FALSE.
  long pattern = _pp_cards->getPatternNumber(ixpp);
  long ixrp1   = _rp_cards->findReceiverPattern(pattern);
  if(ixrp1 == -1) return FALSE;   // should never be FALSE.
  long ixrp    = _rp_cards->getRpCardWithDesiredChannel(ixrp1, channel);
  if(ixrp == -1) return FALSE;   // should never be FALSE.
  if(_rp_cards->getRpXskid(ixrp) != 0.0) return TRUE;
  if(_rp_cards->getRpYskid(ixrp) != 0.0) return TRUE;
      // still have to get ixf, just in case it differs from the
      // input value (since trace receiver might have been skidded
      // to another flag), and then re-do the first two lines.
*/
  return FALSE;
}



void FieldGeometry::getSkiddedReceiverCoords(long ixl, long ixf,
                           double *x, double *y)  const
{
  _survey->getSkiddedReceiverCoords(ixl, ixf, x, y);
}



void FieldGeometry::getSkiddedSourceCoords(long group,
                           double *x, double *y)  const
{
  *x = DNIL;
  *y = DNIL;
  long ixpp  = _pp_cards->findPpCardWithDesiredGroup(group);
    if(ixpp == -1) return;
  float inline_skid, crossline_skid;
  _pp_cards->getSourceSkids(ixpp, group, &inline_skid, &crossline_skid);

  long  source_line = _pp_cards->getSourceLine       (ixpp);
  float source_shot = _pp_cards->getSourceShotpoint  (ixpp);

  long ixl = _survey->findMatchingLineNumber(source_line);
    if(ixl == -1) return;
  long ixf_first = _survey->findMatchingShotpointOnLine(ixl, source_shot);
    if(ixf_first == -1) return;

  long ixg       = group - _pp_cards->getFirstGroupNumber(ixpp);
  long flag_step = ixg * _pp_cards->getSourceMove(ixpp);
  long nflags    = _survey->numFlagsOnLine(ixl);
  long ixf       = ixf_first + flag_step;
    if(ixf < 0 || ixf >= nflags) return;
  _survey->getSkiddedCoords
                     (ixl, ixf, inline_skid, crossline_skid, x, y);
}



// the commented stuff
// will not work if the source has been skidded to another flag:

void FieldGeometry::getSkiddedSourceCoords
                          (long ixl, long ixf, long ixs2,
                           double *x, double *y)  const
{
  *x = DNIL;
  *y = DNIL;
  long group = _survey->sourceGroupNumber(ixl, ixf, ixs2);

  getSkiddedSourceCoords(group, x, y);
/*
  long ixpp  = _pp_cards->findPpCardWithDesiredGroup(group);
  if(ixpp == -1) return;
  float inline_skid, crossline_skid;
  _pp_cards->getSourceSkids(ixpp, group, &inline_skid, &crossline_skid);
  _survey->getSkiddedCoords
                     (ixl, ixf, inline_skid, crossline_skid, x, y);
*/
}



void FieldGeometry::getSkiddedTraceCoords
                          (long group, long channel,
                           double *x, double *y)  const
{
  _tv->calculateTraceValues(group, channel, FALSE);
  *x = _tv->getReceiverXloc();
  *y = _tv->getReceiverYloc();
}

/*********
//////// this is not yet correct:
void FieldGeometry::getSkiddedTraceCoords
                          (long group, long channel,
                           double *x, double *y)  const
{
  *x = DNIL;
  *y = DNIL;
  long ixpp  = _pp_cards->findPpCardWithDesiredGroup(group);
    if(ixpp == -1) return;
  long pattern = _pp_cards->getPatternNumber(ixpp);
  long ixrp1   = _rp_cards->findReceiverPattern(pattern);
    if(ixrp1 == -1) return;
  long ixrp    = _rp_cards->getRpCardWithDesiredChannel(ixrp1, channel);
    if(ixrp == -1) return;
  float inline_skid    = _rp_cards->getRpXskid(ixrp);
  float crossline_skid = _rp_cards->getRpYskid(ixrp);

  long  rec_line = _pp_cards->getReceiverLine      (ixpp);
  float rec_shot = _pp_cards->getReceiverShotpoint (ixpp);

  long  ixl  = _survey->findMatchingLineNumber(rec_line);
     if(ixl == -1) return;
  long ixf_first = _survey->findMatchingShotpointOnLine(ixl, rec_shot);
    if(ixf_first == -1) return;

  long ixg       = group - _pp_cards->getFirstGroupNumber(ixpp);
  long flag_step = ixg * _pp_cards->getReceiverMove(ixpp);
  long nflags    = _survey->numFlagsOnLine(ixl);
  long ixf       = ixf_first + flag_step;
    if(ixf < 0 || ixf >= nflags) return;

/ *
    // now ixl and ixf refer to the actual location of the first receiver
    //    (channel) in the receiver pattern.
    // have to adjust ixf and ixl here...
  long xinc, yinc, incrx, incry;
  _rp_cards->getRpIncrements(ixrp, channel, &xinc, &yinc, &incrx, &incry);

* /

  _survey->getSkiddedCoordsPlus
                     (ixl, ixf, inline_skid, crossline_skid, x, y);
}
**********/



// the commented stuff
// will not work if the trace has been skidded to another flag,
// and that flag has no receiver skid, and there is no pattern skid:

void FieldGeometry::getSkiddedTraceCoords
                          (long ixl, long ixf, long ixr2,
                           double *x, double *y)  const
{
  *x = DNIL;
  *y = DNIL;
  long trace = _survey->receiverTraceNumber(ixl, ixf, ixr2);
  long ixpp  = _pp_cards->findPpCardWithDesiredTrace(trace);
  if(ixpp == -1) return;
  long group, channel;
  _pp_cards->getGroupAndChannel(ixpp, trace, &group, &channel);
  if(group == 0 || channel == 0) return;

  getSkiddedTraceCoords(group, channel, x, y);
/*
  long pattern = _pp_cards->getPatternNumber(ixpp);
  long ixrp1   = _rp_cards->findReceiverPattern(pattern);
  if(ixrp1 == -1) return;
  long ixrp    = _rp_cards->getRpCardWithDesiredChannel(ixrp1, channel);
  if(ixrp == -1) return;
  float inline_skid    = _rp_cards->getRpXskid(ixrp);
  float crossline_skid = _rp_cards->getRpYskid(ixrp);
  _survey->getSkiddedCoordsPlus
                       (ixl, ixf, inline_skid, crossline_skid, x, y);
*/
}



//----------------------- set flag values ------------------------//
//----------------------- set flag values ------------------------//
//----------------------- set flag values ------------------------//


#define FG_MAYBE_BEFORE(ident)                                       \
  switch(ident)                                                      \
     {                                                               \
     case FG_SHOT : if(LOCKED_SRM   ) return; BEFORE_SRM     break;  \
     case FG_DIST : if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case FG_XLOC : if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case FG_YLOC : if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case FG_ELEV : if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case FG_XGRID: if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case FG_YGRID: if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case FG_HD   : if(LOCKED_ALL   ) return; BEFORE_DEPEND  break;  \
     case FG_TUH  : if(LOCKED_ALL   ) return; BEFORE_DEPEND  break;  \
     case FG_RSTAT: if(LOCKED_ALL   ) return; BEFORE_DEPEND  break;  \
     case FG_SSTAT: if(LOCKED_ALL   ) return; BEFORE_DEPEND  break;  \
     case FG_XSKID: if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case FG_YSKID: if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case FG_ESKID: if(LOCKED_ALL   ) return; BEFORE_DEPEND  break;  \
     case FG_SEL  :                           BEFORE_NO      break;  \
     default: assert(FALSE);                                         \
     }


#define FG_MAYBE_AFTER(ident)             \
  switch(ident)                           \
     {                                    \
     case FG_SHOT : AFTER_SRM     break;  \
     case FG_DIST : AFTER_COORDS  break;  \
     case FG_XLOC : AFTER_COORDS  break;  \
     case FG_YLOC : AFTER_COORDS  break;  \
     case FG_ELEV : AFTER_COORDS  break;  \
     case FG_XGRID: AFTER_COORDS  break;  \
     case FG_YGRID: AFTER_COORDS  break;  \
     case FG_HD   : AFTER_DEPEND  break;  \
     case FG_TUH  : AFTER_DEPEND  break;  \
     case FG_RSTAT: AFTER_DEPEND  break;  \
     case FG_SSTAT: AFTER_DEPEND  break;  \
     case FG_XSKID: AFTER_COORDS  break;  \
     case FG_YSKID: AFTER_COORDS  break;  \
     case FG_ESKID: AFTER_DEPEND  break;  \
     case FG_SEL  : AFTER_NO      break;  \
     default: assert(FALSE);              \
     }


   // note: AFTER_NO used above and below for FG_SEL to keep from
   // sending out-of-date flag even though updates might be frozen.
   // this keeps people from thinking all kinds of stuff might be
   // out of date when only the selections are.  the data user
   // must assume that the selections must be re-drawn from scratch
   // when he gets the postResumeUpdates call, even if he has never
   // received the out-of-date call.

   // it might be a good idea to send these new messages:
   //   dependentValuesRestored
   //   selectionsOutOfDate        (uses new flag _sel_out_of_date)
   //   selectionsRestored
   //   activeItemsOutOfDate       (uses new flag _active_out_of_date)
   //   activeItemsRestored
   // in addition to these already sent:
   //   freezingDependentUpdates
   //   dependentValuesOutOfDate   (uses flag _out_of_date)
   //   preResumeDependentUpdates
   //   postResumeDependentUpdates


void FieldGeometry::setFlagValue(long ixl, long ixf, int ident, double value)
{
  assert(allowSettingValue(ident));
  if     (ident == FG_XGRID) { setXgrid(ixl, ixf, value); return; }
  else if(ident == FG_YGRID) { setYgrid(ixl, ixf, value); return; }
  FG_MAYBE_BEFORE(ident)
  _survey->setFlagValue(ixl, ixf, ident, value);
  FG_MAYBE_AFTER(ident)
}


void FieldGeometry::setDependentFlagValue
                                (long ixl, long ixf, int ident, double value)
{
  if(!allowSettingValue(ident)) return;
  if(ident == FG_XGRID) return;
  if(ident == FG_YGRID) return;
  FG_MAYBE_BEFORE(ident)
  _survey->setDependentFlagValue(ixl, ixf, ident, value);
  FG_MAYBE_AFTER(ident)
}


/***********
#define SETV(setShotpoint, float, ident)                           \
void FieldGeometry::setShotpoint(long ixl, long ixf, float value)  \
{                                                                  \
  assert(allowSettingValue(ident));                                \
  FG_MAYBE_BEFORE(ident)                                           \
  _survey->setShotpoint(ixl, ixf, value);                          \
  FG_MAYBE_AFTER(ident)                                            \
}


SETV(setShotpoint       , float , FG_SHOT )
SETV(setIncrDistance    , double, FG_DIST )
SETV(setXloc            , double, FG_XLOC )
SETV(setYloc            , double, FG_YLOC )
SETV(setElevation       , float , FG_ELEV )
SETV(setHoleDepth       , float , FG_HD   )
SETV(setUpholeTime      , float , FG_TUH  )
SETV(setReceiverStatic  , float , FG_RSTAT)
SETV(setSourceStatic    , float , FG_SSTAT)
SETV(setReceiverXskid   , float , FG_XSKID)
SETV(setReceiverYskid   , float , FG_YSKID)
SETV(setReceiverEskid   , float , FG_ESKID)
**********/


#define SETV(ident, LOCKED_SRM, BEFORE_SRM, AFTER_SRM, setXxxx, long2)  \
void FieldGeometry::setXxxx(long ixl, long ixf, long2 value)            \
{                                                                       \
  assert(allowSettingValue(ident));                                     \
  if(LOCKED_SRM) return;                                                \
  BEFORE_SRM                                                            \
  _survey->setXxxx(ixl, ixf, value);                                    \
  AFTER_SRM                                                             \
}


SETV(FG_SHOT ,LOCKED_SRM   ,BEFORE_SRM   ,AFTER_SRM   ,setShotpoint     ,float )
SETV(FG_DIST ,LOCKED_COORDS,BEFORE_COORDS,AFTER_COORDS,setIncrDistance  ,double)
SETV(FG_XLOC ,LOCKED_COORDS,BEFORE_COORDS,AFTER_COORDS,setXloc          ,double)
SETV(FG_YLOC ,LOCKED_COORDS,BEFORE_COORDS,AFTER_COORDS,setYloc          ,double)
SETV(FG_ELEV ,LOCKED_COORDS,BEFORE_COORDS,AFTER_COORDS,setElevation     ,float )
SETV(FG_HD   ,LOCKED_ALL   ,BEFORE_DEPEND,AFTER_DEPEND,setHoleDepth     ,float )
SETV(FG_TUH  ,LOCKED_ALL   ,BEFORE_DEPEND,AFTER_DEPEND,setUpholeTime    ,float )
SETV(FG_RSTAT,LOCKED_ALL   ,BEFORE_DEPEND,AFTER_DEPEND,setReceiverStatic,float )
SETV(FG_SSTAT,LOCKED_ALL   ,BEFORE_DEPEND,AFTER_DEPEND,setSourceStatic  ,float )
SETV(FG_XSKID,LOCKED_COORDS,BEFORE_COORDS,AFTER_COORDS,setReceiverXskid ,float )
SETV(FG_YSKID,LOCKED_COORDS,BEFORE_COORDS,AFTER_COORDS,setReceiverYskid ,float )
SETV(FG_ESKID,LOCKED_ALL   ,BEFORE_DEPEND,AFTER_DEPEND,setReceiverEskid ,float )







void FieldGeometry::setFlagSelectValue(long ixl, long ixf, char value)
{
  assert(allowSettingValue(FG_SEL));
  BEFORE_NO
  _survey->setFlagSelectValue(ixl, ixf, value);
  AFTER_NO      // was AFTER (see note above)
}


void FieldGeometry::incrementFlagSelectValue(long ixl, long ixf)
{
  assert(allowSettingValue(FG_SEL));
  BEFORE_NO
  _survey->incrementFlagSelectValue(ixl, ixf);
  AFTER_NO      // was AFTER (see note above)
}


void FieldGeometry::clearFlagSelections(long ixl)
{
  assert(allowSettingValue(FG_SEL));
  BEFORE_NO
  _survey->clearFlagSelections(ixl);
  AFTER_NO      // was AFTER (see note above)
}



void FieldGeometry::setLocation(long ixl, long ixf,
                                      double xloc, double yloc)
{
  _informer->preMultipleOperations();
  setXloc(ixl, ixf, xloc);
  setYloc(ixl, ixf, yloc);
  _informer->postMultipleOperations();
}



void FieldGeometry::setGridLocation(long ixl, long ixf,
                                      double xgrid, double ygrid)
{
  double xloc = getXlocCoord(xgrid, ygrid);
  double yloc = getYlocCoord(xgrid, ygrid);
  setLocation(ixl, ixf, xloc, yloc);
}



void FieldGeometry::setXgrid(long ixl, long ixf, double xgrid)
{
  assert(allowSettingXgrid());
  double ygrid = getYgrid(ixl, ixf);
  setGridLocation(ixl, ixf, xgrid, ygrid);
}


void FieldGeometry::setYgrid(long ixl, long ixf, double ygrid)
{
  assert(allowSettingYgrid());
  double xgrid = getXgrid(ixl, ixf);
  setGridLocation(ixl, ixf, xgrid, ygrid);
}




//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//
//---------- pass-thru functions to PP card array ---------------//

         // ixpp = index of desired PP card.



//--------------------- get pp values -----------------------------//
//--------------------- get pp values -----------------------------//
//--------------------- get pp values -----------------------------//


long FieldGeometry::numPpCards()  const
{ return _pp_cards->numPpCards(); }

long FieldGeometry::numGroups()  const
{ return _pp_cards->numGroups(); }

long FieldGeometry::numTraces()  const
{ return _pp_cards->numTraces(); }

long FieldGeometry::groupNumber(long trace)  const
{ return _pp_cards->groupNumber     (trace); }

long FieldGeometry::channelNumber(long trace)  const
{ return _pp_cards->channelNumber     (trace); }



//--------------------- search among PP cards --------------------//
//--------------------- search among PP cards --------------------//
//--------------------- search among PP cards --------------------//


long FieldGeometry::findPpCardWithDesiredGroup(long group) const
{
  if(!_tv->sourceGathersOutOfDate()) return _groups->getIxpp(group);
  return _pp_cards->findPpCardWithDesiredGroup(group);
}


long FieldGeometry::findPpCardWithDesiredTrace(long trace) const
{
  return _pp_cards->findPpCardWithDesiredTrace(trace);
}


long FieldGeometry::findNumChannelsInGroup(long group) const
{
  if(!_tv->sourceGathersOutOfDate())
      {
      long ixpp = _groups->getIxpp(group);
      if(ixpp != -1) return _pp_cards->getNumChannelsOnCard(ixpp);
      }
  return _pp_cards->findNumChannelsInGroup(group);
}


long FieldGeometry::findTraceNumber(long group, long channel) const
{
  return _pp_cards->findTraceNumber(group, channel);
}


long FieldGeometry::numUnplacedTraces(long group) const
{
  if(!_tv->receiverGathersOutOfDate())
      {
      return _groups->numUnplacedTraces(group);
      }
  return INIL;
}




//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//
//----------- pass-thru functions to individual PP cards -------------//

     // ixpp = index of desired PP card.


//-------------------- get pp card values --------------------------//
//-------------------- get pp card values --------------------------//
//-------------------- get pp card values --------------------------//

int  FieldGeometry::ppValueIsDependent(long ixpp, int ident)  const
{ return _pp_cards->ppValueIsDependent     (ixpp,     ident); }

/*
float FieldGeometry::getPpValue(long ixpp, int ident)  const
*/
double FieldGeometry::getPpValue(long ixpp, int ident)  const
 { return _pp_cards->getPpValue     (ixpp,     ident); }


#define GETVPP(float, getSourceShotpoint)                    \
float  FieldGeometry::getSourceShotpoint(long ixpp)  const   \
{                                                            \
  return _pp_cards->getSourceShotpoint(ixpp);                \
}


GETVPP(long , getFirstFileNumber  )
GETVPP(long , getThruFileNumber   )
GETVPP(float, getSourceShotpoint  )
GETVPP(float, getReceiverShotpoint)
GETVPP(long , getSourceLine       )
GETVPP(long , getReceiverLine     )
GETVPP(long , getPatternNumber    )
GETVPP(float, getSourceXskid      )
GETVPP(float, getSourceYskid      )
GETVPP(long , getSkidHold         )
GETVPP(float, getNewElevation     )
GETVPP(float, getNewHoleDepth     )
GETVPP(float, getNewUpholeTime    )
GETVPP(long , getSourceMove       )
GETVPP(long , getReceiverMove     )
GETVPP(long , getNumGroupsOnCard  )
GETVPP(long , getNumTracesOnCard  )
GETVPP(long , getNumChannelsOnCard)
GETVPP(long , getFirstGroupNumber )
GETVPP(long , getFirstTraceNumber )
GETVPP(long , getThruGroupNumber  )
GETVPP(long , getThruTraceNumber  )



//-------------------- set pp card values --------------------------//
//-------------------- set pp card values --------------------------//
//-------------------- set pp card values --------------------------//

#define PP_MAYBE_BEFORE(ident)                                         \
  switch(ident)                                                        \
     {                                                                 \
     case PP_FILE   : if(LOCKED_ALL   ) return; BEFORE_DEPEND  break;  \
     case PP_SSHOT  : if(LOCKED_SRM   ) return; BEFORE_SRM     break;  \
     case PP_SLINE  : if(LOCKED_SRM   ) return; BEFORE_SRM     break;  \
     case PP_RSHOT  : if(LOCKED_RM    ) return; BEFORE_RM      break;  \
     case PP_RLINE  : if(LOCKED_RM    ) return; BEFORE_RM      break;  \
     case PP_PAT    : if(LOCKED_RM    ) return; BEFORE_RM      break;  \
     case PP_XSKID  : if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case PP_YSKID  : if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case PP_HOLD   : if(LOCKED_COORDS) return; BEFORE_COORDS  break;  \
     case PP_ELEV   : if(LOCKED_ALL   ) return; BEFORE_DEPEND  break;  \
     case PP_HD     : if(LOCKED_ALL   ) return; BEFORE_DEPEND  break;  \
     case PP_TUH    : if(LOCKED_ALL   ) return; BEFORE_DEPEND  break;  \
     case PP_SMOVE  : if(LOCKED_SRM   ) return; BEFORE_SRM     break;  \
     case PP_RMOVE  : if(LOCKED_RM    ) return; BEFORE_RM      break;  \
     case PP_NGROUPS: if(LOCKED_SRM   ) return; BEFORE_SRM     break;  \
     case PP_NTRACES: if(LOCKED_SRM   ) return; BEFORE_RM      break;  \
     case PP_THRU_GR: if(LOCKED_SRM   ) return; BEFORE_SRM     break;  \
     case PP_THRU_TR: if(LOCKED_SRM   ) return; BEFORE_RM      break;  \
     default: assert(FALSE);                                           \
     }

#define PP_MAYBE_AFTER(ident)               \
  switch(ident)                             \
     {                                      \
     case PP_FILE   : AFTER_DEPEND  break;  \
     case PP_SSHOT  : AFTER_SRM     break;  \
     case PP_SLINE  : AFTER_SRM     break;  \
     case PP_RSHOT  : AFTER_RM      break;  \
     case PP_RLINE  : AFTER_RM      break;  \
     case PP_PAT    : AFTER_RM      break;  \
     case PP_XSKID  : AFTER_COORDS  break;  \
     case PP_YSKID  : AFTER_COORDS  break;  \
     case PP_HOLD   : AFTER_COORDS  break;  \
     case PP_ELEV   : AFTER_DEPEND  break;  \
     case PP_HD     : AFTER_DEPEND  break;  \
     case PP_TUH    : AFTER_DEPEND  break;  \
     case PP_SMOVE  : AFTER_SRM     break;  \
     case PP_RMOVE  : AFTER_RM      break;  \
     case PP_NGROUPS: AFTER_SRM     break;  \
     case PP_NTRACES: AFTER_RM      break;  \
     case PP_THRU_GR: AFTER_SRM     break;  \
     case PP_THRU_TR: AFTER_RM      break;  \
     default: assert(FALSE);                \
     }


/*
void FieldGeometry::setPpValue         (long ixpp, int ident, float value)
*/
void FieldGeometry::setPpValue         (long ixpp, int ident, double value)
{
  PP_MAYBE_BEFORE(ident)
  _pp_cards->setPpValue(ixpp, ident, value);
  PP_MAYBE_AFTER(ident)
}


/*
void FieldGeometry::setDependentPpValue(long ixpp, int ident, float value)
*/
void FieldGeometry::setDependentPpValue(long ixpp, int ident, double value)
{
  PP_MAYBE_BEFORE(ident)
  _pp_cards->setDependentPpValue(ixpp, ident, value);
  PP_MAYBE_AFTER(ident)
}



#define SETVPP(setSourceShotpoint, float, ident)                \
void FieldGeometry::setSourceShotpoint(long ixpp, float value)  \
{                                                               \
  PP_MAYBE_BEFORE(ident)                                        \
  _pp_cards->setSourceShotpoint(ixpp, value);                   \
  PP_MAYBE_AFTER(ident)                                         \
}


SETVPP(setFirstFileNumber  , long , PP_FILE   )
SETVPP(setSourceShotpoint  , float, PP_SSHOT  )
SETVPP(setSourceLine       , long , PP_SLINE  )
SETVPP(setReceiverShotpoint, float, PP_RSHOT  )
SETVPP(setReceiverLine     , long , PP_RLINE  )
SETVPP(setPatternNumber    , long , PP_PAT    )
SETVPP(setSourceXskid      , float, PP_XSKID  )
SETVPP(setSourceYskid      , float, PP_YSKID  )
SETVPP(setSkidHold         , long , PP_HOLD   )
SETVPP(setNewElevation     , float, PP_ELEV   )
SETVPP(setNewHoleDepth     , float, PP_HD     )
SETVPP(setNewUpholeTime    , float, PP_TUH    )
SETVPP(setSourceMove       , long , PP_SMOVE  )
SETVPP(setReceiverMove     , long , PP_RMOVE  )
SETVPP(setNumGroupsOnCard  , long , PP_NGROUPS)




//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//
//----------- pass-thru functions to RP card array -------------//

        // ixrp = index of desired RP card.

//---------------------- get rp values --------------------------//
//---------------------- get rp values --------------------------//
//---------------------- get rp values --------------------------//

long FieldGeometry::numRpCards()  const
{ return _rp_cards->numRpCards(); }

long FieldGeometry::numChannelsInPattern(long pattern)  const
{ return _rp_cards->numChannelsInPattern     (pattern); }

int  FieldGeometry::receiverPatternsAreSorted()  const
{ return _rp_cards->receiverPatternsAreSorted(); }



//------------------ search among RP cards -------------------//
//------------------ search among RP cards -------------------//
//------------------ search among RP cards -------------------//

    // these return an index (ixrp), or -1 if not found.

long FieldGeometry::findReceiverPattern (long pattern)  const
{ return _rp_cards->findReceiverPattern      (pattern); }


long FieldGeometry::findEndOfReceiverPattern (long pattern)  const
{ return _rp_cards->findEndOfReceiverPattern      (pattern); }


long FieldGeometry::findRpCardWithDesiredChannel
                                     (long pattern, long channel)  const
{ return _rp_cards->findRpCardWithDesiredChannel(pattern, channel); }


long FieldGeometry::getEndOfReceiverPattern (long ixrp)  const
{ return _rp_cards->getEndOfReceiverPattern      (ixrp); }


long FieldGeometry::getRpCardWithDesiredChannel
                                     (long ixrp, long channel)  const
{ return _rp_cards->getRpCardWithDesiredChannel(ixrp, channel); }


void FieldGeometry::sortReceiverPatterns()
{
  if(LOCKED_RM) return;
  BEFORE_RM
         _rp_cards->sortReceiverPatterns();
  AFTER_RM
}






//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//
//----------- pass-thru functions to individual RP cards -------------//

     // ixrp = index of desired RP card.


//---------------------- get rp card values --------------------------//
//---------------------- get rp card values --------------------------//
//---------------------- get rp card values --------------------------//

#define GETVRP(long2, getXxxx)                    \
long2  FieldGeometry::getXxxx(long ixrp)  const   \
{                                                 \
  return _rp_cards->getXxxx(ixrp);                \
}


GETVRP(long , getRpPatternNumber)
GETVRP(long , getRpNumChannels  )
GETVRP(long , getRpCumChannels  )
GETVRP(int  , getRpFlag         )
GETVRP(float, getRpShotpoint    )
GETVRP(long , getRpLineNumber   )
GETVRP(long , getRpNumX         )
GETVRP(long , getRpXinc         )
GETVRP(long , getRpNumY         )
GETVRP(long , getRpYinc         )
GETVRP(float, getRpXskid        )
GETVRP(float, getRpYskid        )
GETVRP(float, getRpEskid        )


void FieldGeometry::getRpIncrements(long ixrp, long channel,
                                    long *xinc,  long *yinc,
                                    long *incrx, long *incry)  const
{
  _rp_cards->getRpIncrements(ixrp, channel, xinc, yinc, incrx, incry);
}



//---------------------- set rp card values --------------------------//
//---------------------- set rp card values --------------------------//
//---------------------- set rp card values --------------------------//

#define RP_MAYBE_BEFORE(ident)                                        \
  switch(ident)                                                       \
     {                                                                \
     case RP_PAT    : if(LOCKED_RM    ) return; BEFORE_RM     break;  \
     case RP_FLAG   : if(LOCKED_RM    ) return; BEFORE_RM     break;  \
     case RP_SHOT   : if(LOCKED_RM    ) return; BEFORE_RM     break;  \
     case RP_LINE   : if(LOCKED_RM    ) return; BEFORE_RM     break;  \
     case RP_NX     : if(LOCKED_RM    ) return; BEFORE_RM     break;  \
     case RP_XINC   : if(LOCKED_RM    ) return; BEFORE_RM     break;  \
     case RP_NY     : if(LOCKED_RM    ) return; BEFORE_RM     break;  \
     case RP_YINC   : if(LOCKED_RM    ) return; BEFORE_RM     break;  \
     case RP_XSKID  : if(LOCKED_COORDS) return; BEFORE_COORDS break;  \
     case RP_YSKID  : if(LOCKED_COORDS) return; BEFORE_COORDS break;  \
     case RP_ESKID  : if(LOCKED_ALL   ) return; BEFORE_DEPEND break;  \
     default: assert(FALSE);                                          \
     }

#define RP_MAYBE_AFTER(ident)              \
  switch(ident)                            \
     {                                     \
     case RP_PAT    : AFTER_RM     break;  \
     case RP_FLAG   : AFTER_RM     break;  \
     case RP_SHOT   : AFTER_RM     break;  \
     case RP_LINE   : AFTER_RM     break;  \
     case RP_NX     : AFTER_RM     break;  \
     case RP_XINC   : AFTER_RM     break;  \
     case RP_NY     : AFTER_RM     break;  \
     case RP_YINC   : AFTER_RM     break;  \
     case RP_XSKID  : AFTER_COORDS break;  \
     case RP_YSKID  : AFTER_COORDS break;  \
     case RP_ESKID  : AFTER_DEPEND break;  \
     default: assert(FALSE);               \
     }


#define SETVRP(setXxxx, long2, ident)                \
void FieldGeometry::setXxxx(long ixrp, long2 value)  \
{                                                    \
  RP_MAYBE_BEFORE(ident)                             \
  _rp_cards->setXxxx(ixrp, value);                   \
  RP_MAYBE_AFTER(ident)                              \
}


SETVRP (setRpPatternNumber, long , RP_PAT  )
SETVRP (setRpFlag         , int  , RP_FLAG )
SETVRP (setRpShotpoint    , float, RP_SHOT )
SETVRP (setRpLineNumber   , long , RP_LINE )
SETVRP (setRpNumX         , long , RP_NX   )
SETVRP (setRpXinc         , long , RP_XINC )
SETVRP (setRpNumY         , long , RP_NY   )
SETVRP (setRpYinc         , long , RP_YINC )
SETVRP (setRpXskid        , float, RP_XSKID)
SETVRP (setRpYskid        , float, RP_YSKID)
SETVRP (setRpEskid        , float, RP_ESKID)




//---------------------- get active item -----------------------//
//---------------------- get active item -----------------------//
//---------------------- get active item -----------------------//


long FieldGeometry::getActiveLineIndex()  const
  { return _survey->getActiveLineIndex(); }

long FieldGeometry::getActiveFlagIndexOnLine(long ixl)  const
{   return _survey->getActiveFlagIndexOnLine(ixl); }

long FieldGeometry::getActiveRpCardIndex()  const
{ return _rp_cards->getActiveRpCardIndex(); }

long FieldGeometry::getActivePpCardIndex()  const
{ return _pp_cards->getActivePpCardIndex(); }

long FieldGeometry::getActiveZt1CardIndex()  const
{ return _zt_cards->getActiveZt1CardIndex(); }

long FieldGeometry::getActiveZt2CardIndex()  const
{ return _zt_cards->getActiveZt2CardIndex(); }

long FieldGeometry::getActiveZt3CardIndex()  const
{ return _zt_cards->getActiveZt3CardIndex(); }

long FieldGeometry::getActiveZt4CardIndex()  const
{ return _zt_cards->getActiveZt4CardIndex(); }

long  FieldGeometry::getActiveCmpIndex()  const
{ return _midpoints->getActiveCmpIndex(); }

long FieldGeometry::getActiveTraceNumber()  const
{ return _pp_cards->getActiveTraceNumber(); }

long FieldGeometry::getActiveGroupNumber()  const
{ return _pp_cards->getActiveGroupNumber(); }

long FieldGeometry::getActiveLineNumber()  const
  { return _survey->getActiveLineNumber(); }

float FieldGeometry::getActiveShotpointOnLine(long ixl)  const
{    return _survey->getActiveShotpointOnLine(ixl); }



//------------------ set active item ---------------------------------//
//------------------ set active item ---------------------------------//
//------------------ set active item ---------------------------------//


#define   ACTIVE(_cards, setActiveCardIndex)           \
void FieldGeometry::setActiveCardIndex(long index)     \
{                                                      \
  BEFORE_NO                                            \
            _cards->setActiveCardIndex(index);         \
  AFTER_NO                                             \
}



ACTIVE (_survey   , setActiveLineIndex)
ACTIVE (_survey   , setActiveLineNumber)
ACTIVE (_rp_cards , setActiveRpCardIndex)
ACTIVE (_pp_cards , setActivePpCardIndex)
ACTIVE (_zt_cards , setActiveZt1CardIndex)
ACTIVE (_zt_cards , setActiveZt2CardIndex)
ACTIVE (_zt_cards , setActiveZt3CardIndex)
ACTIVE (_zt_cards , setActiveZt4CardIndex)
ACTIVE (_pp_cards , setActiveTraceNumber)
ACTIVE (_pp_cards , setActiveGroupNumber)
ACTIVE (_midpoints, setActiveCmpIndex)


void FieldGeometry::setActiveFlagIndexOnLine(long ixl, long index)
{
  BEFORE_NO
           _survey->setActiveFlagIndexOnLine(ixl, index);
  AFTER_NO
}


void FieldGeometry::setActiveShotpointOnLine(long ixl, float shotpoint)
{
  BEFORE_NO
           _survey->setActiveShotpointOnLine(ixl, shotpoint);
  AFTER_NO
}



//----------------- set combinations of active items -----------------//
//----------------- set combinations of active items -----------------//
//----------------- set combinations of active items -----------------//

  // sets active line and flag:

void FieldGeometry::setActiveIndices(long ixl, long ixf)
{
  if(ixl == -1 || ixf == -1) return;
  _informer->preMultipleOperations();
  setActiveLineIndex      (ixl);
  setActiveFlagIndexOnLine(ixl, ixf);
  _informer->postMultipleOperations();
}



  // sets active line and flag:
  // also sets active RP and PP indices, and group number, if
  //   source gathers are created and ixs2 is valid:

void FieldGeometry::setActiveSourceIndices
                                 (long ixl, long ixf, long ixs2)
{
  _informer->preMultipleOperations();
  setActiveIndices(ixl, ixf);
  long ns = numSourcesAtFlag(ixl, ixf);
  if(ixs2 < 0 || ixs2 >= ns)
      {
      _informer->postMultipleOperations();
      return;
      }
  long group = sourceGroupNumber(ixl, ixf, ixs2);
  setActiveGroupNumber(group);
  long ixpp = findPpCardWithDesiredGroup(group);
  if(ixpp  == -1)
      {
      _informer->postMultipleOperations();
      return;
      }
  setActivePpCardIndex(ixpp);
  long pattern = getPatternNumber(ixpp);
  long ixrp = findReceiverPattern(pattern);
  if(ixrp  == -1)
      {
      _informer->postMultipleOperations();
      return;
      }
  setActiveRpCardIndex(ixrp);
  _informer->postMultipleOperations();
}



#define  TEMPORARY_MAYBE_RESET_IXRP                                  \
  if(ixrp == -1 && ixpp != -1 && group >= 1.0 && group != DNIL)      \
      {                                                              \
      long num_groups  = _pp_cards->getNumGroupsOnCard (ixpp);       \
      long first_group = _pp_cards->getFirstGroupNumber(ixpp);       \
      long num_traces  = _pp_cards->getNumTracesOnCard (ixpp);       \
      long first_trace = _pp_cards->getFirstTraceNumber(ixpp);       \
      if(num_groups >= 1 && first_group >= 1 &&                      \
         num_traces >= 1 && first_trace >= 1)                        \
          {                                                          \
          long igp        = NearestInteger(group) - first_group;     \
          long nchan      = num_traces / num_groups;                 \
          long channel    = 1 + trace - first_trace - igp * nchan;   \
          long ixrp_first = _tv->getFirstRpCardIndex();              \
          if(ixrp_first != -1 && channel >= 1 && channel <= nchan)   \
              {                                                      \
              ixrp = _rp_cards->getRpCardWithDesiredChannel          \
                                              (ixrp_first, channel); \
              }                                                      \
          }                                                          \
      }



  // sets active line and flag:
  // also sets active RP and PP and CMP indices, and trace and group
  //   numbers, if receiver gathers are created and ixr2 is valid:

void FieldGeometry::setActiveReceiverIndices
                                 (long ixl, long ixf, long ixr2)
{
  _informer->preMultipleOperations();
  setActiveIndices(ixl, ixf);
  long nr = numReceiversAtFlag(ixl, ixf);
  if(ixr2 < 0 || ixr2 >= nr)
      {
      _informer->postMultipleOperations();
      return;
      }
  long trace = receiverTraceNumber(ixl, ixf, ixr2);
  setActiveTraceNumber(trace);
  calculateHeaderWords(trace, FALSE);
  long   ixrp  = getHeaderRpCardIndex();
  long   ixpp  = getHeaderPpCardIndex();
  long   ixcmp = getHeaderCmpIndex();
  double group = getHeaderWordValue(9);
  TEMPORARY_MAYBE_RESET_IXRP
  if(ixrp  != -1) setActiveRpCardIndex(ixrp);
  if(ixpp  != -1) setActivePpCardIndex(ixpp);
  if(ixcmp != -1) setActiveCmpIndex   (ixcmp);
  if(group != DNIL) setActiveGroupNumber(NearestInteger(group));
  _informer->postMultipleOperations();
}



  // sets active source line, source flag, RPcard, PPcard, CMP,
  //   trace number, and group number:

void FieldGeometry::setActiveSourceIndices(long trace)
{
  _informer->preMultipleOperations();
  setActiveTraceNumber(trace);
  calculateHeaderWords(trace, FALSE);
  long   ixl   = getHeaderSourceLineIndex();
  long   ixf   = getHeaderSourceFlagIndex();
  long   ixrp  = getHeaderRpCardIndex();
  long   ixpp  = getHeaderPpCardIndex();
  long   ixcmp = getHeaderCmpIndex();
  double group = getHeaderWordValue(9);
  setActiveIndices(ixl, ixf);
  TEMPORARY_MAYBE_RESET_IXRP
  if(ixrp  !=   -1) setActiveRpCardIndex(ixrp);
  if(ixpp  !=   -1) setActivePpCardIndex(ixpp);
  if(ixcmp !=   -1) setActiveCmpIndex   (ixcmp);
  if(group != DNIL) setActiveGroupNumber(NearestInteger(group));
  _informer->postMultipleOperations();
}



  // sets active receiver line, receiver flag, RPcard, PPcard, CMP,
  //   trace number, and group number:

void FieldGeometry::setActiveReceiverIndices(long trace)
{
  _informer->preMultipleOperations();
  setActiveTraceNumber(trace);
  calculateHeaderWords(trace, FALSE);
  long   ixl   = getHeaderReceiverLineIndex();
  long   ixf   = getHeaderReceiverFlagIndex();
  long   ixrp  = getHeaderRpCardIndex();
  long   ixpp  = getHeaderPpCardIndex();
  long   ixcmp = getHeaderCmpIndex();
  double group = getHeaderWordValue(9);
  setActiveIndices(ixl, ixf);
  TEMPORARY_MAYBE_RESET_IXRP
  if(ixrp  !=   -1) setActiveRpCardIndex(ixrp);
  if(ixpp  !=   -1) setActivePpCardIndex(ixpp);
  if(ixcmp !=   -1) setActiveCmpIndex   (ixcmp);
  if(group != DNIL) setActiveGroupNumber(NearestInteger(group));
  _informer->postMultipleOperations();
}



  // sets active source line, source flag, RPcard, PPcard,
  //    and group number:

void FieldGeometry::setActiveGroupNumberPlus(long group)
{
  long ngroups = numGroups();
  if(group < 1 || group > ngroups) return;
  _informer->preMultipleOperations();
  setActiveGroupNumber(group);
  long ixpp = findPpCardWithDesiredGroup(group);
  if(ixpp == -1) { _informer->postMultipleOperations(); return; }
  setActivePpCardIndex(ixpp);
  long pattern = getPatternNumber(ixpp);
  long ixrp = findReceiverPattern(pattern);
  if(ixrp == -1) { _informer->postMultipleOperations(); return; }
  setActiveRpCardIndex(ixrp);
     //// also find shotpoint and line number, then set them active.
     //// also find shotpoint and line number, then set them active.
     //// also find shotpoint and line number, then set them active.
     //// also find shotpoint and line number, then set them active.
  _informer->postMultipleOperations();
}



//--------------- convenience functions -------------------------//
//--------------- convenience functions -------------------------//
//--------------- convenience functions -------------------------//


float FieldGeometry::defaultSourceDatumStatic(long ixl, long ixf)  const
{
  float ref = _tv->getRef();
  float ve  = _tv->getVe ();
  return _survey->defaultSourceDatumStatic(ixl, ixf, ref, ve);
}


float FieldGeometry::defaultReceiverDatumStatic(long ixl, long ixf)  const
{
  float ref = _tv->getRef();
  float ve  = _tv->getVe ();
  return _survey->defaultReceiverDatumStatic(ixl, ixf, ref, ve);
}



long FieldGeometry::numSourcesAtActiveFlag()  const
{
  if(sourceGathersOutOfDate()) return 0;
  long ixl = getActiveLineIndex();
  if(ixl == -1) return 0;
  long ixf = getActiveFlagIndexOnLine(ixl);
  if(ixf == -1) return 0;
  return numSourcesAtFlag(ixl, ixf);
}


long FieldGeometry::numReceiversAtActiveFlag()  const
{
  if(receiverGathersOutOfDate()) return 0;
  long ixl = getActiveLineIndex();
  if(ixl == -1) return 0;
  long ixf = getActiveFlagIndexOnLine(ixl);
  if(ixf == -1) return 0;
  return numReceiversAtFlag(ixl, ixf);
}


long FieldGeometry::sourceGroupNumberAtActiveFlag(long ixs2)  const
{
  if(sourceGathersOutOfDate()) return 0;
  long ixl = getActiveLineIndex();
  if(ixl == -1) return 0;
  long ixf = getActiveFlagIndexOnLine(ixl);
  if(ixf == -1) return 0;
  long n = numSourcesAtFlag(ixl, ixf);
  if(ixs2 < 0 || ixs2 >= n) return 0;
  return sourceGroupNumber(ixl, ixf, ixs2);
}


long FieldGeometry::receiverTraceNumberAtActiveFlag(long ixr2)  const
{
  if(receiverGathersOutOfDate()) return 0;
  long ixl = getActiveLineIndex();
  if(ixl == -1) return 0;
  long ixf = getActiveFlagIndexOnLine(ixl);
  if(ixf == -1) return 0;
  long n = numReceiversAtFlag(ixl, ixf);
  if(ixr2 < 0 || ixr2 >= n) return 0;
  return receiverTraceNumber(ixl, ixf, ixr2);
}



float FieldGeometry::getSourceElevation(long group)  const
{
  long channel = 1;
  _tv->calculateTraceValues(group, channel, FALSE);
  return _tv->getSourceElevation();
}



float FieldGeometry::getSourceHoleDepth(long group)  const
{
////////// the commented-out code in principle is efficient.
////////// except for the fact that getSourceLineIndex and getSourceFlagIndex
////////// call calculateTraceValues.
/*****
  long ixpp = findPpCardWithDesiredGroup(group);
  if(ixpp == -1) return FNIL;
  long ixg = group - _pp_cards->getFirstGroupNumber(ixpp);
  float hd = FNIL;
  if(ixg == 0) hd = _pp_cards->getNewHoleDepth(ixpp);
  if(hd != FNIL) return hd;
  long ixl = getSourceLineIndex(group);
  long ixf = getSourceFlagIndex(group);
  if(ixl == -1 || ixf == -1) return FNIL;
  hd = _survey->getHoleDepth(ixl, ixf);
  return hd;
*****/
  long channel = 1;
  _tv->calculateTraceValues(group, channel, FALSE);
  return _tv->getSourceHoleDepth();
}



float FieldGeometry::getSourceUpholeTime(long group)  const
{
  long channel = 1;
  _tv->calculateTraceValues(group, channel, FALSE);
  return _tv->getSourceUpholeTime();
}



float FieldGeometry::getSourceDatumStatic(long group)  const
{
  long channel = 1;
  _tv->calculateTraceValues(group, channel, FALSE);
  return _tv->getTotalSourceStatic();
}



//------------------ allocate and free space for cards ---------------//
//------------------ allocate and free space for cards ---------------//
//------------------ allocate and free space for cards ---------------//


void FieldGeometry::allocateSpaceForLine  (long ixl, long nadd)
         { _survey->allocateSpaceForLine  (     ixl,      nadd); }

void FieldGeometry::allocateSpaceForLines    (long nadd)
         { _survey->allocateSpaceForLines         (nadd); }

void FieldGeometry::allocateSpaceForRpCards  (long nadd)
       { _rp_cards->allocateSpaceForRpCards  (     nadd); }

void FieldGeometry::allocateSpaceForPpCards  (long nadd)
       { _pp_cards->allocateSpaceForPpCards  (     nadd); }

void FieldGeometry::allocateSpaceForZt1Cards (long nadd)
       { _zt_cards->allocateSpaceForZt1Cards (     nadd); }

void FieldGeometry::allocateSpaceForZt2Cards (long nadd)
       { _zt_cards->allocateSpaceForZt2Cards (     nadd); }

void FieldGeometry::allocateSpaceForZt3Cards (long nadd)
       { _zt_cards->allocateSpaceForZt3Cards (     nadd); }

void FieldGeometry::allocateSpaceForZt4Cards (long nadd)
       { _zt_cards->allocateSpaceForZt4Cards (     nadd); }



void FieldGeometry::freeSpaceForLine (long ixl)
{          _survey->freeSpaceForLine      (ixl); }

void FieldGeometry::freeSpaceForLines()
{          _survey->freeSpaceForLines(); }

void FieldGeometry::freeSpaceForRpCards()
{        _rp_cards->freeSpaceForRpCards(); }

void FieldGeometry::freeSpaceForPpCards()
{        _pp_cards->freeSpaceForPpCards(); }

void FieldGeometry::freeSpaceForZt1Cards()
{        _zt_cards->freeSpaceForZt1Cards(); }

void FieldGeometry::freeSpaceForZt2Cards()
{        _zt_cards->freeSpaceForZt2Cards(); }

void FieldGeometry::freeSpaceForZt3Cards()
{        _zt_cards->freeSpaceForZt3Cards(); }

void FieldGeometry::freeSpaceForZt4Cards()
{        _zt_cards->freeSpaceForZt4Cards(); }



//---------------------- append card -------------------------//
//---------------------- append card -------------------------//
//---------------------- append card -------------------------//


#define APPEND(LOCKED, BEFORE, AFTER, _cards, appendNewCard)  \
long FieldGeometry::appendNewCard()                \
{                                                  \
  if(LOCKED) return -1;                            \
  BEFORE                                           \
  long index = _cards->appendNewCard();            \
  AFTER                                            \
  return index;                                    \
}


APPEND (LOCKED_SRM, BEFORE_SRM, AFTER_SRM, _survey  , appendNewLine)
APPEND (LOCKED_RM , BEFORE_RM , AFTER_RM , _rp_cards, appendNewRpCard)
APPEND (LOCKED_SRM, BEFORE_SRM, AFTER_SRM, _pp_cards, appendNewPpCard)
APPEND (LOCKED_ZT1, BEFORE_ZT1, AFTER_ZT1, _zt_cards, appendNewZt1Card)
APPEND (LOCKED_ZT2, BEFORE_ZT2, AFTER_ZT2, _zt_cards, appendNewZt2Card)
APPEND (LOCKED_ZT3, BEFORE_ZT3, AFTER_ZT3, _zt_cards, appendNewZt3Card)
APPEND (LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, _zt_cards, appendNewZt4Card)


long   FieldGeometry::appendNewFlagToLine  (long ixl)
{
  if(LOCKED_SRM) return -1;
  BEFORE_SRM
  long ixf = _survey->appendNewFlagToLine(ixl);
  AFTER_SRM
  return ixf;
}



//--------------------- place card ------------------------------//
//--------------------- place card ------------------------------//
//--------------------- place card ------------------------------//


long FieldGeometry::placeNewLine(long line_number)
{
  if(LOCKED_SRM) return -1;
  BEFORE_SRM
  long ixl = _survey->placeNewLine(line_number);
  AFTER_SRM
  return ixl;
}


long FieldGeometry::placeNewRpCard(long pattern)
{
  if(LOCKED_RM) return -1;
  BEFORE_RM
  long ixrp = _rp_cards->placeNewRpCard(pattern);
  AFTER_RM
  return ixrp;
}



//---------------- insert or delete card ---------------------//
//---------------- insert or delete card ---------------------//
//---------------- insert or delete card ---------------------//


#define INSERT(LOCKED, BEFORE, AFTER, _cards, insertNewCard)  \
long FieldGeometry::insertNewCard(long index)    \
{                                                \
  if(LOCKED) return -1;                          \
  BEFORE                                         \
  index = _cards->insertNewCard(index);          \
  AFTER                                          \
  return index;                                  \
}


INSERT(LOCKED_SRM, BEFORE_SRM, AFTER_SRM, _survey  , insertNewLine)
INSERT(LOCKED_RM , BEFORE_RM , AFTER_RM , _rp_cards, insertNewRpCard)
INSERT(LOCKED_SRM, BEFORE_SRM, AFTER_SRM, _pp_cards, insertNewPpCard)
INSERT(LOCKED_ZT1, BEFORE_ZT1, AFTER_ZT1, _zt_cards, insertNewZt1Card)
INSERT(LOCKED_ZT2, BEFORE_ZT2, AFTER_ZT2, _zt_cards, insertNewZt2Card)
INSERT(LOCKED_ZT3, BEFORE_ZT3, AFTER_ZT3, _zt_cards, insertNewZt3Card)
INSERT(LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, _zt_cards, insertNewZt4Card)

INSERT(LOCKED_SRM, BEFORE_SRM, AFTER_SRM, _survey  , insertNewLineFromBuffer)
INSERT(LOCKED_RM , BEFORE_RM , AFTER_RM , _rp_cards, insertNewRpCardFromBuffer)
INSERT(LOCKED_SRM, BEFORE_SRM, AFTER_SRM, _pp_cards, insertNewPpCardFromBuffer)
INSERT(LOCKED_ZT1, BEFORE_ZT1, AFTER_ZT1, _zt_cards, insertNewZt1CardFromBuffer)
INSERT(LOCKED_ZT2, BEFORE_ZT2, AFTER_ZT2, _zt_cards, insertNewZt2CardFromBuffer)
INSERT(LOCKED_ZT3, BEFORE_ZT3, AFTER_ZT3, _zt_cards, insertNewZt3CardFromBuffer)
INSERT(LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, _zt_cards, insertNewZt4CardFromBuffer)

INSERT(LOCKED_DEL, BEFORE_SRM, AFTER_SRM, _survey  , deleteLine)
INSERT(LOCKED_DEL, BEFORE_RM , AFTER_RM , _rp_cards, deleteRpCard)
INSERT(LOCKED_DEL, BEFORE_SRM, AFTER_SRM, _pp_cards, deletePpCard)
INSERT(LOCKED_DEL, BEFORE_ZT1, AFTER_ZT1, _zt_cards, deleteZt1Card)
INSERT(LOCKED_DEL, BEFORE_ZT2, AFTER_ZT2, _zt_cards, deleteZt2Card)
INSERT(LOCKED_DEL, BEFORE_ZT3, AFTER_ZT3, _zt_cards, deleteZt3Card)
INSERT(LOCKED_DEL, BEFORE_ZT4, AFTER_ZT4, _zt_cards, deleteZt4Card)

INSERT(LOCKED_DEL, BEFORE_SRM, AFTER_SRM, _survey  , deleteLineToBuffer)
INSERT(LOCKED_DEL, BEFORE_RM , AFTER_RM , _rp_cards, deleteRpCardToBuffer)
INSERT(LOCKED_DEL, BEFORE_SRM, AFTER_SRM, _pp_cards, deletePpCardToBuffer)
INSERT(LOCKED_DEL, BEFORE_ZT1, AFTER_ZT1, _zt_cards, deleteZt1CardToBuffer)
INSERT(LOCKED_DEL, BEFORE_ZT2, AFTER_ZT2, _zt_cards, deleteZt2CardToBuffer)
INSERT(LOCKED_DEL, BEFORE_ZT3, AFTER_ZT3, _zt_cards, deleteZt3CardToBuffer)
INSERT(LOCKED_DEL, BEFORE_ZT4, AFTER_ZT4, _zt_cards, deleteZt4CardToBuffer)


long   FieldGeometry::insertNewFlagOnLine (long ixl, long ixf)
{
  if(LOCKED_SRM) return -1;
  BEFORE_SRM
  ixf = _survey->insertNewFlagOnLine(ixl, ixf);
  AFTER_SRM
  return ixf;
}


long   FieldGeometry::placeNewFlagOnLine (long ixl, float shotpoint)
{
  if(LOCKED_SRM) return -1;
  BEFORE_SRM
  long ixf = _survey->placeNewFlagOnLine(ixl, shotpoint);
  AFTER_SRM
  return ixf;
}


long   FieldGeometry::insertNewFlagOnLineFromBuffer (long ixl, long ixf)
{
  if(LOCKED_SRM) return -1;
  BEFORE_SRM
  ixf = _survey->insertNewFlagOnLineFromBuffer(ixl, ixf);
  AFTER_SRM
  return ixf;
}


long   FieldGeometry::deleteFlagFromLine     (long ixl, long ixf)
{
  if(LOCKED_DEL) return -1;
  BEFORE_SRM
  ixf = _survey->deleteFlagFromLine(ixl, ixf);
  AFTER_SRM
  return ixf;
}


long   FieldGeometry::deleteFlagFromLineToBuffer     (long ixl, long ixf)
{
  if(LOCKED_DEL) return -1;
  BEFORE_SRM
  ixf = _survey->deleteFlagFromLineToBuffer(ixl, ixf);
  AFTER_SRM
  return ixf;
}



//----------------------- delete all cards ---------------------//
//----------------------- delete all cards ---------------------//
//----------------------- delete all cards ---------------------//


#define DELETE(LOCKED, BEFORE, AFTER, _cards, deleteAllCards)   \
void FieldGeometry::deleteAllCards()   \
{                                      \
  if(LOCKED) return;                   \
  BEFORE                               \
  _cards->deleteAllCards();            \
  AFTER                                \
}


DELETE (LOCKED_DEL, BEFORE_SRM, AFTER_SRM, _survey  , deleteAllLines)
DELETE (LOCKED_DEL, BEFORE_RM , AFTER_RM , _rp_cards, deleteAllRpCards)
DELETE (LOCKED_DEL, BEFORE_SRM, AFTER_SRM, _pp_cards, deleteAllPpCards)
DELETE (LOCKED_DEL, BEFORE_ZT1, AFTER_ZT1, _zt_cards, deleteAllZt1Cards)
DELETE (LOCKED_DEL, BEFORE_ZT2, AFTER_ZT2, _zt_cards, deleteAllZt2Cards)
DELETE (LOCKED_DEL, BEFORE_ZT3, AFTER_ZT3, _zt_cards, deleteAllZt3Cards)
DELETE (LOCKED_DEL, BEFORE_ZT4, AFTER_ZT4, _zt_cards, deleteAllZt4Cards)


void   FieldGeometry::deleteAllFlagsFromLine (long ixl)
{
  if(LOCKED_DEL) return;
  BEFORE_SRM
  _survey->deleteAllFlagsFromLine(ixl);
  AFTER_SRM
}



//-------------- get num ZT cards ----------------------------//
//-------------- get num ZT cards ----------------------------//
//-------------- get num ZT cards ----------------------------//


long FieldGeometry::numZt1Cards()  const
{ return _zt_cards->numZt1Cards(); }

long FieldGeometry::numZt2Cards()  const
{ return _zt_cards->numZt2Cards(); }

long FieldGeometry::numZt3Cards()  const
{ return _zt_cards->numZt3Cards(); }

long FieldGeometry::numZt4Cards()  const
{ return _zt_cards->numZt4Cards(); }



//---------------------- get ZT card value ------------------------//
//---------------------- get ZT card value ------------------------//
//---------------------- get ZT card value ------------------------//


#define ZT_GET(long2, getXxxx)                      \
long2  FieldGeometry::getXxxx(long index)  const    \
{                                                   \
  return _zt_cards->getXxxx(index);                 \
}


ZT_GET (int  , getZt1Code               )
ZT_GET (float, getZt1FromSourceShotpoint)
ZT_GET (float, getZt1ToSourceShotpoint  )
ZT_GET (long , getZt1SourceLineNumber   )

ZT_GET (int  , getZt2Code                 )
ZT_GET (float, getZt2FromReceiverShotpoint)
ZT_GET (float, getZt2ToReceiverShotpoint  )
ZT_GET (long , getZt2ReceiverLineNumber   )

ZT_GET (int  , getZt3Code           )
ZT_GET (long , getZt3FromGroupNumber)
ZT_GET (long , getZt3ToGroupNumber  )
ZT_GET (long , getZt3FromTraceNumber)
ZT_GET (long , getZt3ToTraceNumber  )
ZT_GET (int  , groupPartlyDead      )

ZT_GET (int  , getZt4Code                 )
ZT_GET (float, getZt4FromSourceShotpoint  )
ZT_GET (float, getZt4ToSourceShotpoint    )
ZT_GET (long , getZt4SourceLineNumber     )
ZT_GET (float, getZt4FromReceiverShotpoint)
ZT_GET (float, getZt4ToReceiverShotpoint  )
ZT_GET (long , getZt4ReceiverLineNumber   )



//---------------------- set ZT card value ---------------------------//
//---------------------- set ZT card value ---------------------------//
//---------------------- set ZT card value ---------------------------//


#define ZT_SET(LOCKED_ZT1, BEFORE_ZT1, AFTER_ZT1, setXxxx, long2)  \
void FieldGeometry::setXxxx(long index, long2 value)   \
{                                                      \
  if(LOCKED_ZT1) return;                               \
  BEFORE_ZT1                                           \
  _zt_cards->setXxxx(index, value);                    \
  AFTER_ZT1                                            \
}


ZT_SET (LOCKED_ZT1, BEFORE_ZT1, AFTER_ZT1, setZt1Code                 , int  )
ZT_SET (LOCKED_ZT1, BEFORE_ZT1, AFTER_ZT1, setZt1FromSourceShotpoint  , float)
ZT_SET (LOCKED_ZT1, BEFORE_ZT1, AFTER_ZT1, setZt1ToSourceShotpoint    , float)
ZT_SET (LOCKED_ZT1, BEFORE_ZT1, AFTER_ZT1, setZt1SourceLineNumber     , long )

ZT_SET (LOCKED_ZT2, BEFORE_ZT2, AFTER_ZT2, setZt2Code                 , int  )
ZT_SET (LOCKED_ZT2, BEFORE_ZT2, AFTER_ZT2, setZt2FromReceiverShotpoint, float)
ZT_SET (LOCKED_ZT2, BEFORE_ZT2, AFTER_ZT2, setZt2ToReceiverShotpoint  , float)
ZT_SET (LOCKED_ZT2, BEFORE_ZT2, AFTER_ZT2, setZt2ReceiverLineNumber   , long )

ZT_SET (LOCKED_ZT3, BEFORE_ZT3, AFTER_ZT3, setZt3Code                 , int  )
ZT_SET (LOCKED_ZT3, BEFORE_ZT3, AFTER_ZT3, setZt3FromGroupNumber      , long )
ZT_SET (LOCKED_ZT3, BEFORE_ZT3, AFTER_ZT3, setZt3ToGroupNumber        , long )
ZT_SET (LOCKED_ZT3, BEFORE_ZT3, AFTER_ZT3, setZt3FromTraceNumber      , long )
ZT_SET (LOCKED_ZT3, BEFORE_ZT3, AFTER_ZT3, setZt3ToTraceNumber        , long )

ZT_SET (LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, setZt4Code                 , int  )
ZT_SET (LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, setZt4FromSourceShotpoint  , float)
ZT_SET (LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, setZt4ToSourceShotpoint    , float)
ZT_SET (LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, setZt4SourceLineNumber     , long )
ZT_SET (LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, setZt4FromReceiverShotpoint, float)
ZT_SET (LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, setZt4ToReceiverShotpoint  , float)
ZT_SET (LOCKED_ZT4, BEFORE_ZT4, AFTER_ZT4, setZt4ReceiverLineNumber   , long )






//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//
//------------------ pass-thru to trace gathers --------------------//


//-------------------- set values ----------------------------------//
//-------------------- set values ----------------------------------//
//-------------------- set values ----------------------------------//


void FieldGeometry::updateSourceGathers()
{
  resumeDependentUpdates();          // in case updates are frozen.
  assert(!_data_changing);
  _data_changing = TRUE;
  _informer->preSlowOperations();
  _informer->preMultipleOperations();
  _tv      ->updateSourceGathers();
  maximizeDataLock();
  _informer->postSlowOperations();
  _informer->postMultipleOperations();
  _headers->startHeadersFromScratch();
  _data_changing = FALSE;
}


void FieldGeometry::updateReceiverGathers()
{
  resumeDependentUpdates();          // in case updates are frozen.
  assert(!_data_changing);
  _data_changing = TRUE;
  _informer->preSlowOperations();
  _informer->preMultipleOperations();
  _tv      ->updateReceiverGathers();
  maximizeDataLock();
  _informer->postSlowOperations();
  _informer->postMultipleOperations();
  _headers->startHeadersFromScratch();
  _data_changing = FALSE;
}


long FieldGeometry::updateMidpointGathers()
{
  _updatingMidpointGathers = 1;	/* ehs */

  resumeDependentUpdates();          // in case updates are frozen.
  assert(!_data_changing);
  _data_changing = TRUE;
  _informer->preSlowOperations();
  _informer->preMultipleOperations();
  long error = _tv->updateMidpointGathers();
  maximizeDataLock();
  _informer->postSlowOperations();
  _informer->postMultipleOperations();
  _headers->startHeadersFromScratch();
  _data_changing = FALSE;

  _updatingMidpointGathers = 0;	/* ehs */

  return error;
}



void FieldGeometry::updateLiveFold()
{
  resumeDependentUpdates();          // in case updates are frozen.
  assert(!_data_changing);
  _data_changing = TRUE;
  _informer->preSlowOperations();
  _informer->preMultipleOperations();
  _tv      ->updateLiveFold();
  maximizeDataLock();
  _informer->postMultipleOperations();
  _informer->postSlowOperations();
  _headers->startHeadersFromScratch();
  _data_changing = FALSE;
}



//-------------------- get values ----------------------------------//
//-------------------- get values ----------------------------------//
//-------------------- get values ----------------------------------//

long  FieldGeometry::numCmpGathers()  const
{ return _midpoints->numCmpGathers(); }

long  FieldGeometry::numXbins()  const
{ return _midpoints->numXbins(); }

long  FieldGeometry::numYbins()  const
{ return _midpoints->numYbins(); }

long  FieldGeometry::numSelectedCmpGathers()  const
{ return _midpoints->numSelectedCmpGathers(); }

long  FieldGeometry::foldOfStack(long ixcmp)  const
{ return _midpoints->foldOfStack(ixcmp); }

long  FieldGeometry::liveFoldOfStack(long ixcmp)  const
{ return _midpoints->liveFoldOfStack(ixcmp); }

long  FieldGeometry::headerWord3(long ixcmp)  const
{ return _midpoints->headerWord3(ixcmp); }

char  FieldGeometry::getCmpSelectValue(long ixcmp)  const
{ return _midpoints->getCmpSelectValue(ixcmp); }

int   FieldGeometry::cmpIsSelected(long ixcmp)  const
{ return _midpoints->cmpIsSelected(ixcmp); }

int   FieldGeometry::cmpFattestBinette(long ixcmp)  const
{ return _midpoints->cmpFattestBinette(ixcmp); }


int    FieldGeometry::firstBinHasNilCoords()  const
 { return _midpoints->firstBinHasNilCoords(); }

long   FieldGeometry::getMinimumFold()  const
 { return _midpoints->getMinimumFold(); }

long   FieldGeometry::getMaximumFold()  const
 { return _midpoints->getMaximumFold(); }

double FieldGeometry::getMinimumXlocBinCenter()  const
 { return _midpoints->getMinimumXlocBinCenter(); }

double FieldGeometry::getMinimumYlocBinCenter()  const
 { return _midpoints->getMinimumYlocBinCenter(); }

double FieldGeometry::getMaximumXlocBinCenter()  const
 { return _midpoints->getMaximumXlocBinCenter(); }

double FieldGeometry::getMaximumYlocBinCenter()  const
 { return _midpoints->getMaximumYlocBinCenter(); }

float  FieldGeometry::getMinimumOffset()  const
 { return    _traces->getMinimumOffset(); }

float  FieldGeometry::getMaximumOffset()  const
 { return    _traces->getMaximumOffset(); }

double FieldGeometry::getMinimumXgridBinCenter()  const
 { return _midpoints->getMinimumXgridBinCenter(); }

double FieldGeometry::getMinimumYgridBinCenter()  const
 { return _midpoints->getMinimumYgridBinCenter(); }

double FieldGeometry::getMaximumXgridBinCenter()  const
 { return _midpoints->getMaximumXgridBinCenter(); }

double FieldGeometry::getMaximumYgridBinCenter()  const
 { return _midpoints->getMaximumYgridBinCenter(); }



long  FieldGeometry::originalTraceIndex(long ixcmp, long ixfold)  const
{ return _midpoints->originalTraceIndex(ixcmp, ixfold); }

long  FieldGeometry::originalTraceIndex(long ixsorted)  const
{ return _midpoints->originalTraceIndex(ixsorted); }



long  FieldGeometry::findNearestCmp(double xloc, double yloc)  const
{ return _midpoints->findNearestCmp(xloc, yloc); }

long  FieldGeometry::getMatchingCmp(double xloc, double yloc)  const
{ return _midpoints->getMatchingCmp(xloc, yloc); }

long  FieldGeometry::findNearestCmpUsingGrid(double xgrid, double ygrid)  const
{ return _midpoints->findNearestCmpUsingGrid(xgrid, ygrid); }

long  FieldGeometry::getMatchingCmpUsingGrid(double xgrid, double ygrid)  const
{ return _midpoints->getMatchingCmpUsingGrid(xgrid, ygrid); }



void FieldGeometry::getCmpLocBinCenter
                             (long ixcmp, double *xloc, double *yloc) const
      { _midpoints->getCmpLocBinCenter(ixcmp, xloc, yloc); }

void FieldGeometry::getCmpGridBinCenter
                             (long ixcmp, double *xgrid, double *ygrid) const
      { _midpoints->getCmpGridBinCenter(ixcmp, xgrid, ygrid); }



void FieldGeometry::getCmpTraceLoc
                (long ixcmp, long ixfold, double *xloc , double *yloc ) const
      { _midpoints->getCmpTraceLoc(ixcmp, ixfold, xloc, yloc); }

void FieldGeometry::getCmpTraceGrid
                (long ixcmp, long ixfold, double *xgrid, double *ygrid) const
      { _midpoints->getCmpTraceGrid(ixcmp, ixfold, xgrid, ygrid); }

float FieldGeometry::getCmpTraceOffset(long ixcmp, long ixfold) const
{ return _midpoints->getCmpTraceOffset(ixcmp, ixfold); }

int   FieldGeometry::getCmpTraceBinette(long ixcmp, long ixfold) const
{ return _midpoints->getCmpTraceBinette(ixcmp, ixfold); }





void FieldGeometry::setCmpSelectValue(long ixcmp, char value)
{
  BEFORE_NO
  _midpoints->setCmpSelectValue(ixcmp, value);
  AFTER_NO
}


void FieldGeometry::incrementCmpSelectValue(long ixcmp)
{
  BEFORE_NO
  _midpoints->incrementCmpSelectValue(ixcmp);
  AFTER_NO
}


void FieldGeometry::clearCmpSelections()
{
  BEFORE_NO
  _midpoints->clearCmpSelections();
  AFTER_NO
}



long FieldGeometry::getSourceLineIndex(long group)  const
      { return _tv->getSourceLineIndex(group); }

long FieldGeometry::getSourceFlagIndex(long group)  const
      { return _tv->getSourceFlagIndex(group); }

long FieldGeometry::getReceiverLineIndex(long group, long channel)  const
      { return _tv->getReceiverLineIndex(group, channel); }

long FieldGeometry::getReceiverFlagIndex(long group, long channel)  const
      { return _tv->getReceiverFlagIndex(group, channel); }



int  FieldGeometry::sourceGathersOutOfDate()  const
      { return _tv->sourceGathersOutOfDate(); }

int  FieldGeometry::receiverGathersOutOfDate()  const
      { return _tv->receiverGathersOutOfDate(); }

int   FieldGeometry::midpointGathersOutOfDate()  const
{ return _midpoints->midpointGathersOutOfDate(); }

int   FieldGeometry::liveFoldOutOfDate()  const
{ return _midpoints->liveFoldOutOfDate(); }



int  FieldGeometry::sourceGathersIncomplete()  const
      { return _tv->sourceGathersIncomplete(); }

int  FieldGeometry::receiverGathersIncomplete()  const
      { return _tv->receiverGathersIncomplete(); }

int  FieldGeometry::midpointGathersIncomplete()  const
      { return _tv->midpointGathersIncomplete(); }



long FieldGeometry::numUnplacedSources()  const
      { return _tv->numUnplacedSources(); }

long FieldGeometry::numUnplacedTraces()  const
      { return _tv->numUnplacedTraces(); }




//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//
//------------------ pass thru to FgHeaders --------------------//


//------------------ set and get FGD variables --------------------//
//------------------ set and get FGD variables --------------------//
//------------------ set and get FGD variables --------------------//

          // public.

void   FieldGeometry::setVe        (float value)
               { _tv->setVe              (value); }

void   FieldGeometry::setRef       (float value)
               { _tv->setRef             (value); }

void   FieldGeometry::setFixdist   (float value)
               { if(LOCKED_COORDS) return;
                 BEFORE_COORDS
                 _tv->setFixdist         (value);
                 AFTER_COORDS }

void   FieldGeometry::setNdpt      (long  value)
               { _tv->setNdpt            (value); }


float  FieldGeometry::getVe        ()             const
        { return _tv->getVe(); }

float  FieldGeometry::getRef       ()             const
        { return _tv->getRef(); }

float  FieldGeometry::getFixdist   ()             const
        { return _tv->getFixdist(); }

long   FieldGeometry::getNdpt      ()             const
        { return _tv->getNdpt(); }



//------------- calculate header words for one trace ------------------//
//------------- calculate header words for one trace ------------------//
//------------- calculate header words for one trace ------------------//

          // public.
          // returns error flag (TRUE if error, FALSE if no error).

int    FieldGeometry::calculateHeaderWords (long itrace, int more)
   { return _headers->calculateHeaderWords      (itrace,     more); }



//------------- get information calculated above ----------------//
//------------- get information calculated above ----------------//
//------------- get information calculated above ----------------//

      // public.
      // all but the last must follow call to calculateHeaderWords.

int    FieldGeometry::getHeaderErrorFlag()            const
   { return _headers->getHeaderErrorFlag(); }

long   FieldGeometry::getHeaderTraceNumber()            const
   { return _headers->getHeaderTraceNumber(); }

long   FieldGeometry::getHeaderSourceLineIndex()        const
   { return _headers->getHeaderSourceLineIndex(); }

long   FieldGeometry::getHeaderSourceFlagIndex()        const
   { return _headers->getHeaderSourceFlagIndex(); }

long   FieldGeometry::getHeaderReceiverLineIndex()      const
   { return _headers->getHeaderReceiverLineIndex(); }

long   FieldGeometry::getHeaderReceiverFlagIndex()      const
   { return _headers->getHeaderReceiverFlagIndex(); }

long   FieldGeometry::getHeaderRpCardIndex()      const
              { return _tv->getRpCardIndex(); }

long   FieldGeometry::getHeaderPpCardIndex()      const
              { return _tv->getPpCardIndex(); }

double FieldGeometry::getHeaderWordValue  (int ihead)  const
   { return _headers->getHeaderWordValue      (ihead); }

const char *FieldGeometry::getHeaderWordDescription (int ihead)  const
        { return _headers->getHeaderWordDescription     (ihead); }

long  FieldGeometry::getHeaderCmpIndex()  const
{
  long ncmp = _midpoints->numCmpGathers();
  if(ncmp == 0) return -1;
  double xgrid = _headers->getHeaderWordValue(7);
  double ygrid = _headers->getHeaderWordValue(8);
  if(xgrid == DNIL || ygrid == DNIL)
      {
      return 0;  // first CMP contains unplaced traces if there are any.
      }
  xgrid = (double)NearestInteger(xgrid);
  ygrid = (double)NearestInteger(ygrid);
  long ixcmp = _midpoints->findNearestCmpUsingGrid(xgrid, ygrid);
  if(ixcmp == -1) return 0;
  double xgrid2, ygrid2;
  _midpoints->getCmpGridBinCenter(ixcmp, &xgrid2, &ygrid2);
  if(AbsoluteValue(xgrid2 - xgrid) > 0.1) return 0;
  if(AbsoluteValue(ygrid2 - ygrid) > 0.1) return 0;
  return ixcmp;
}


/*
 * This enables a pre/postUpdateReceiverGathers to check and
 * see if the gather calculation is stopping with receivers
 * or continuing to midpoint.  Used by Fg2DGpc.
 */
int FieldGeometry::updatingMidpointGathers()	/* ehs */
{
	return _updatingMidpointGathers;
}

int FieldGeometry::getDeadCode(int itrace)	/* ehs */
{
	return _tv->getDeadCode(itrace);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//

