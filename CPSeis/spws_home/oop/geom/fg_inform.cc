
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
//---------------------- fg_inform.cc ------------------------//
//---------------------- fg_inform.cc ------------------------//
//---------------------- fg_inform.cc ------------------------//

//         implementation file for the FgInform class
//                 derived from the FgInform class
//                        subdirectory geom


#include "geom/fg_inform.hh"
#include "geom/fg_constants.hh"
#include "geom/field_geometry.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>

#define EXIT  if(!_printit) return;


//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


FgInform::FgInform(FieldGeometry *fg, int printit)
       :
             _fg       (fg),
             _printit  (printit),
             _disabled (FALSE)
{
  assert(_fg);
  _fg->addInformObject(this);
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


FgInform::~FgInform()
{
  if(_fg) _fg->removeInformObject(this);
}



//--------------------- data now gone -------------------------//
//--------------------- data now gone -------------------------//
//--------------------- data now gone -------------------------//

   // called by FgInformer when FieldGeometry is no longer viable.

void FgInform::dataNowGone()
{
  _fg = NULL;
}



//------------------ fg ident name (static) ----------------------//
//------------------ fg ident name (static) ----------------------//
//------------------ fg ident name (static) ----------------------//

static char *fg_ident_name(int ident)
{
  static char name[10];
  sprintf(name, "%2d ", ident);
  switch(ident)
      {
      case FG_SHOT  : strcat(name, "SHOT  "); break;
      case FG_DIST  : strcat(name, "DIST  "); break;
      case FG_XLOC  : strcat(name, "XLOC  "); break;
      case FG_YLOC  : strcat(name, "YLOC  "); break;
      case FG_ELEV  : strcat(name, "ELEV  "); break;
      case FG_XGRID : strcat(name, "XGRID "); break;
      case FG_YGRID : strcat(name, "YGRID "); break;
      case FG_HD    : strcat(name, "HD    "); break;
      case FG_TUH   : strcat(name, "TUH   "); break;
      case FG_RSTAT : strcat(name, "RSTAT "); break;
      case FG_SSTAT : strcat(name, "SSTAT "); break;
      case FG_XSKID : strcat(name, "XSKID "); break;
      case FG_YSKID : strcat(name, "YSKID "); break;
      case FG_ESKID : strcat(name, "ESKID "); break;
      case FG_SEL   : strcat(name, "SEL   "); break;
      case FG_CUM   : strcat(name, "CUM   "); break;
      case FG_AZIM  : strcat(name, "AZIM  "); break;
      case FG_COORDS: strcat(name, "COORDS"); break;
      default:        strcat(name, "??????"); break;
      }
  return name;
}



//------------------ rp ident name (static) ----------------------//
//------------------ rp ident name (static) ----------------------//
//------------------ rp ident name (static) ----------------------//

static char *rp_ident_name(int ident)
{
  static char name[10];
  sprintf(name, "%2d ", ident);
  switch(ident)
      {
      case RP_PAT    : strcat(name, "PAT    "); break;
      case RP_NCHAN  : strcat(name, "NCHAN  "); break;
      case RP_CUMCHAN: strcat(name, "CUMCHAN"); break;
      case RP_FLAG   : strcat(name, "FLAG   "); break;
      case RP_SHOT   : strcat(name, "SHOT   "); break;
      case RP_LINE   : strcat(name, "LINE   "); break;
      case RP_NX     : strcat(name, "NX     "); break;
      case RP_XINC   : strcat(name, "XINC   "); break;
      case RP_NY     : strcat(name, "NY     "); break;
      case RP_YINC   : strcat(name, "YINC   "); break;
      case RP_XSKID  : strcat(name, "XSKID  "); break;
      case RP_YSKID  : strcat(name, "YSKID  "); break;
      case RP_ESKID  : strcat(name, "ESKID  "); break;
      default:         strcat(name, "???????"); break;
      }
  return name;
}



//------------------ pp ident name (static) ----------------------//
//------------------ pp ident name (static) ----------------------//
//------------------ pp ident name (static) ----------------------//

static char *pp_ident_name(int ident)
{
  static char name[10];
  sprintf(name, "%2d ", ident);
  switch(ident)
      {
      case PP_FILE   : strcat(name, "FILE   "); break;
      case PP_SSHOT  : strcat(name, "SSHOT  "); break;
      case PP_SLINE  : strcat(name, "SLINE  "); break;
      case PP_RSHOT  : strcat(name, "RSHOT  "); break;
      case PP_RLINE  : strcat(name, "RLINE  "); break;
      case PP_PAT    : strcat(name, "PAT    "); break;
      case PP_XSKID  : strcat(name, "XSKID  "); break;
      case PP_YSKID  : strcat(name, "YSKID  "); break;
      case PP_HOLD   : strcat(name, "HOLD   "); break;
      case PP_ELEV   : strcat(name, "ELEV   "); break;
      case PP_HD     : strcat(name, "HD     "); break;
      case PP_TUH    : strcat(name, "TUH    "); break;
      case PP_SMOVE  : strcat(name, "SMOVE  "); break;
      case PP_RMOVE  : strcat(name, "RMOVE  "); break;
      case PP_NGROUPS: strcat(name, "NGROUPS"); break;
      case PP_NTRACES: strcat(name, "NTRACES"); break;
      case PP_THRU_GR: strcat(name, "THRU_GR"); break;
      case PP_THRU_TR: strcat(name, "THRU_TR"); break;
      default:         strcat(name, "???????"); break;
      }
  return name;
}



//------------------ zt1 ident name (static) ----------------------//
//------------------ zt1 ident name (static) ----------------------//
//------------------ zt1 ident name (static) ----------------------//

static char *zt1_ident_name(int ident)
{
  static char name[10];
  sprintf(name, "%2d ", ident);
  switch(ident)
      {
      case ZT1_CODE : strcat(name, "CODE "); break;
      case ZT1_SHOT1: strcat(name, "SHOT1"); break;
      case ZT1_SHOT2: strcat(name, "SHOT2"); break;
      case ZT1_LINE : strcat(name, "LINE "); break;
      default:        strcat(name, "?????"); break;
      }
  return name;
}



//------------------ zt2 ident name (static) ----------------------//
//------------------ zt2 ident name (static) ----------------------//
//------------------ zt2 ident name (static) ----------------------//

static char *zt2_ident_name(int ident)
{
  static char name[10];
  sprintf(name, "%2d ", ident);
  switch(ident)
      {
      case ZT2_CODE : strcat(name, "CODE "); break;
      case ZT2_SHOT1: strcat(name, "SHOT1"); break;
      case ZT2_SHOT2: strcat(name, "SHOT2"); break;
      case ZT2_LINE : strcat(name, "LINE "); break;
      default:        strcat(name, "?????"); break;
      }
  return name;
}



//------------------ zt3 ident name (static) ----------------------//
//------------------ zt3 ident name (static) ----------------------//
//------------------ zt3 ident name (static) ----------------------//

static char *zt3_ident_name(int ident)
{
  static char name[10];
  sprintf(name, "%2d ", ident);
  switch(ident)
      {
      case ZT3_CODE  : strcat(name, "CODE  "); break;
      case ZT3_GROUP1: strcat(name, "GROUP1"); break;
      case ZT3_GROUP2: strcat(name, "GROUP2"); break;
      case ZT3_TRACE1: strcat(name, "TRACE1"); break;
      case ZT3_TRACE2: strcat(name, "TRACE2"); break;
      default:         strcat(name, "??????"); break;
      }
  return name;
}



//------------------ zt4 ident name (static) ----------------------//
//------------------ zt4 ident name (static) ----------------------//
//------------------ zt4 ident name (static) ----------------------//

static char *zt4_ident_name(int ident)
{
  static char name[10];
  sprintf(name, "%2d ", ident);
  switch(ident)
      {
      case ZT4_CODE  : strcat(name, "CODE  "); break;
      case ZT4_SSHOT1: strcat(name, "SSHOT1"); break;
      case ZT4_SSHOT2: strcat(name, "SSHOT2"); break;
      case ZT4_SLINE : strcat(name, "SLINE "); break;
      case ZT4_RSHOT1: strcat(name, "RSHOT1"); break;
      case ZT4_RSHOT2: strcat(name, "RSHOT2"); break;
      case ZT4_RLINE : strcat(name, "RLINE "); break;
      default:         strcat(name, "??????"); break;
      }
  return name;
}



//------------- miscellaneous notifications --------------------------//
//------------- miscellaneous notifications --------------------------//
//------------- miscellaneous notifications --------------------------//


void FgInform::dataGoingAway(FieldGeometry*)
{
  EXIT
  cout << "QUIT     dataGoingAway" << endl;
}


void FgInform::startingChanges(FieldGeometry*)
{
  EXIT
  cout << " " << endl << "STARTING startingChanges" << endl;
}


void FgInform::finishedChanges(FieldGeometry*)
{
  EXIT
  cout << "FINISHED finishedChanges" << endl << " " << endl;
}


void FgInform::ringBell(FieldGeometry*)
{
  EXIT
  cout << "BELL     ringBell" << endl;
}


void FgInform::showMessage(FieldGeometry*, char *msg)
{
  EXIT
  cout << "SHOW     showMessage  " << msg << endl;
}


void FgInform::sendMessage(FieldGeometry*, char *msg,
                   long i1, long i2, long i3, long i4, long i5)
{
  EXIT
  cout << "SEND     sendMessage  " << msg << " "
       << i1 << " " << i2 << " " << i3 << " " << i4 << " " << i5
       << endl;
}


void FgInform::returningToEventLoop(FieldGeometry*)
{
  EXIT
  cout << "RETURN   returningToEventLoop" << endl;
}



//------------------ always inform specific change -------------------//
//------------------ always inform specific change -------------------//
//------------------ always inform specific change -------------------//


void FgInform::preSlowOperations(FieldGeometry*)
{
  EXIT
  cout << "PRE   preSlowOperations" << endl;
}


void FgInform::postSlowOperations(FieldGeometry*)
{
  EXIT
  cout << "POST postSlowOperations" << endl;
}



void FgInform::dataNeedsSavingFlagTurnedOn(FieldGeometry*)
{
  EXIT
  cout << "ON       dataNeedsSavingFlagTurnedOn" << endl;
}

void FgInform::dataNeedsSavingFlagTurnedOff(FieldGeometry*)
{
  EXIT
  cout << "OFF      dataNeedsSavingFlagTurnedOff" << endl;
}



void FgInform::freezingDependentUpdates(FieldGeometry*)
{
  EXIT
  cout << "FREEZE   freezingDependentUpdates" << endl;
}

void FgInform::dependentValuesOutOfDate(FieldGeometry*)
{
  EXIT
  cout << "BAD      dependentValuesOutOfDate" << endl;
}

void FgInform::preResumeDependentUpdates(FieldGeometry*)
{
  EXIT
  cout << "PRE   preResumeDependentUpdates" << endl;
}

void FgInform::postResumeDependentUpdates(FieldGeometry*)
{
  EXIT
  cout << "POST postResumeDependentUpdates" << endl;
}



void FgInform::preChangeDataLock(FieldGeometry*)
{
  EXIT
  cout << "PRE   preChangeDataLock" << endl;
}

void FgInform::postChangeDataLock(FieldGeometry*)
{
  EXIT
  cout << "POST postChangeDataLock" << endl;
}



void FgInform::preNewGridTransform(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewGridTransform" << endl;
}

void FgInform::postNewGridTransform(FieldGeometry*)
{
  EXIT
  cout << "POST postNewGridTransform" << endl;
}



void FgInform::preNewTestingGridTransform(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewTestingGridTransform" << endl;
}

void FgInform::postNewTestingGridTransform(FieldGeometry*)
{
  EXIT
  cout << "POST postNewTestingGridTransform" << endl;
}



void FgInform::sourceGathersOutOfDate(FieldGeometry*)
{
  EXIT
  cout << "BAD      sourceGathersOutOfDate" << endl;
}

void FgInform::preUpdateSourceGathers(FieldGeometry*)
{
  EXIT
  cout << "PRE   preUpdateSourceGathers" << endl;
}

void FgInform::postUpdateSourceGathers(FieldGeometry*)
{
  EXIT
  cout << "POST postUpdateSourceGathers" << endl;
}



void FgInform::receiverGathersOutOfDate(FieldGeometry*)
{
  EXIT
  cout << "BAD      receiverGathersOutOfDate" << endl;
}

void FgInform::preUpdateReceiverGathers(FieldGeometry*)
{
  EXIT
  cout << "PRE   preUpdateReceiverGathers" << endl;
}

void FgInform::postUpdateReceiverGathers(FieldGeometry*)
{
  EXIT
  cout << "POST postUpdateReceiverGathers" << endl;
}



void FgInform::midpointGathersOutOfDate(FieldGeometry*)
{
  EXIT
  cout << "BAD      midpointGathersOutOfDate" << endl;
}

void FgInform::preUpdateMidpointGathers(FieldGeometry*)
{
  EXIT
  cout << "PRE   preUpdateMidpointGathers" << endl;
}

void FgInform::postUpdateMidpointGathers(FieldGeometry*)
{
  EXIT
  cout << "POST postUpdateMidpointGathers" << endl;
}



void FgInform::liveFoldOutOfDate(FieldGeometry*)
{
  EXIT
  cout << "BAD      liveFoldOutOfDate" << endl;
}

void FgInform::preUpdateLiveFold(FieldGeometry*)
{
  EXIT
  cout << "PRE   preUpdateLiveFold" << endl;
}

void FgInform::postUpdateLiveFold(FieldGeometry*)
{
  EXIT
  cout << "POST postUpdateLiveFold" << endl;
}



//------------- sometimes inform specific change ---------------//
//------------- sometimes inform specific change ---------------//
//------------- sometimes inform specific change ---------------//


void FgInform::preNewChaining(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewChaining" << endl;
}

void FgInform::postNewChaining(FieldGeometry*)
{
  EXIT
  cout << "POST postNewChaining" << endl;
}



void FgInform::preSortByLineNumber(FieldGeometry*)
{
  EXIT
  cout << "PRE   preSortByLineNumber" << endl;
}

void FgInform::postSortByLineNumber(FieldGeometry*)
{
  EXIT
  cout << "POST postSortByLineNumber" << endl;
}



void FgInform::preReverseLineDirection(FieldGeometry*, long ixl)
{
  EXIT
  cout << "PRE   preReverseLineDirection" <<
             "  ixl=" << ixl << endl;
}

void FgInform::postReverseLineDirection(FieldGeometry*, long ixl)
{
  EXIT
  cout << "POST postReverseLineDirection" <<
             "  ixl=" << ixl << endl;
}



void FgInform::preNewActiveLine(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActiveLine" << endl;
}

void FgInform::postNewActiveLine(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActiveLine" << endl;
}



void FgInform::preNewActiveFlag(FieldGeometry*, long ixl)
{
  EXIT
  cout << "PRE   preNewActiveFlag" <<
             "  ixl=" << ixl << endl;
}

void FgInform::postNewActiveFlag(FieldGeometry*, long ixl)
{
  EXIT
  cout << "POST postNewActiveFlag" <<
             "  ixl=" << ixl << endl;
}



void FgInform::preNewActiveRpCard(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActiveRpCard" << endl;
}

void FgInform::postNewActiveRpCard(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActiveRpCard" << endl;
}



void FgInform::preNewActivePpCard(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActivePpCard" << endl;
}

void FgInform::postNewActivePpCard(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActivePpCard" << endl;
}



void FgInform::preNewActiveZt1Card(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActiveZt1Card" << endl;
}

void FgInform::postNewActiveZt1Card(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActiveZt1Card" << endl;
}



void FgInform::preNewActiveZt2Card(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActiveZt2Card" << endl;
}

void FgInform::postNewActiveZt2Card(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActiveZt2Card" << endl;
}



void FgInform::preNewActiveZt3Card(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActiveZt3Card" << endl;
}

void FgInform::postNewActiveZt3Card(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActiveZt3Card" << endl;
}



void FgInform::preNewActiveZt4Card(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActiveZt4Card" << endl;
}

void FgInform::postNewActiveZt4Card(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActiveZt4Card" << endl;
}



void FgInform::preNewActiveCmp(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActiveCmp" << endl;
}

void FgInform::postNewActiveCmp(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActiveCmp" << endl;
}



void FgInform::preNewActiveTrace(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActiveTrace" << endl;
}

void FgInform::postNewActiveTrace(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActiveTrace" << endl;
}



void FgInform::preNewActiveGroup(FieldGeometry*)
{
  EXIT
  cout << "PRE   preNewActiveGroup" << endl;
}

void FgInform::postNewActiveGroup(FieldGeometry*)
{
  EXIT
  cout << "POST postNewActiveGroup" << endl;
}



void FgInform::preCmpSelectionsChanged(FieldGeometry*)
{
  EXIT
  cout << "PRE   preCmpSelectionsChanged" << endl;
}

void FgInform::postCmpSelectionsChanged(FieldGeometry*)
{
  EXIT
  cout << "POST postCmpSelectionsChanged" << endl;
}



void FgInform::preLineSelectionsChanged(FieldGeometry*,
                                long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preLineSelectionsChanged" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postLineSelectionsChanged(FieldGeometry*,
                                long index, long nrem, long nins)
{
  EXIT
  cout << "POST postLineSelectionsChanged" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}



void FgInform::preFlagValuesChanged(FieldGeometry*,
            long ixl, int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preFlagValuesChanged" <<
             "  ixl=" << ixl <<
             "  id=" << fg_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postFlagValuesChanged(FieldGeometry*,
            long ixl, int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "POST postFlagValuesChanged" <<
             "  ixl=" << ixl <<
             "  id=" << fg_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}



void FgInform::preRemoveInsertFlags(FieldGeometry*,
           long ixl, long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preRemoveInsertFlags" <<
             "  ixl=" << ixl <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postRemoveInsertFlags(FieldGeometry*,
           long ixl, long index, long nrem, long nins)
{
  EXIT
  cout << "POST postRemoveInsertFlags" <<
             "  ixl=" << ixl <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}



void FgInform::preLineNumbersChanged(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preLineNumbersChanged" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postLineNumbersChanged(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "POST postLineNumbersChanged" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}



void FgInform::preRemoveInsertLines(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preRemoveInsertLines" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postRemoveInsertLines(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "POST postRemoveInsertLines" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}


void FgInform::preRemoveInsertPpCards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preRemoveInsertPpCards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postRemoveInsertPpCards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "POST postRemoveInsertPpCards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}


void FgInform::prePpValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   prePpValuesChanged" <<
             "  id=" << pp_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postPpValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "POST postPpValuesChanged" <<
             "  id=" << pp_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}


void FgInform::preRemoveInsertRpCards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preRemoveInsertRpCards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postRemoveInsertRpCards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "POST postRemoveInsertRpCards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}


void FgInform::preRpValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preRpValuesChanged" <<
             "  id=" << rp_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postRpValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "POST postRpValuesChanged" <<
             "  id=" << rp_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}



void FgInform::preRemoveInsertZt1Cards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preRemoveInsertZt1Cards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postRemoveInsertZt1Cards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "POST postRemoveInsertZt1Cards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}


void FgInform::preZt1ValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preZt1ValuesChanged" <<
             "  id=" << zt1_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postZt1ValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "POST postZt1ValuesChanged" <<
             "  id=" << zt1_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}




void FgInform::preRemoveInsertZt2Cards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preRemoveInsertZt2Cards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postRemoveInsertZt2Cards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "POST postRemoveInsertZt2Cards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}


void FgInform::preZt2ValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preZt2ValuesChanged" <<
             "  id=" << zt2_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postZt2ValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "POST postZt2ValuesChanged" <<
             "  id=" << zt2_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}




void FgInform::preRemoveInsertZt3Cards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preRemoveInsertZt3Cards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postRemoveInsertZt3Cards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "POST postRemoveInsertZt3Cards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}


void FgInform::preZt3ValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preZt3ValuesChanged" <<
             "  id=" << zt3_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postZt3ValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "POST postZt3ValuesChanged" <<
             "  id=" << zt3_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}




void FgInform::preRemoveInsertZt4Cards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preRemoveInsertZt4Cards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postRemoveInsertZt4Cards(FieldGeometry*,
           long index, long nrem, long nins)
{
  EXIT
  cout << "POST postRemoveInsertZt4Cards" <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}


void FgInform::preZt4ValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "PRE   preZt4ValuesChanged" <<
             "  id=" << zt4_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}

void FgInform::postZt4ValuesChanged(FieldGeometry*,
           int ident, long index, long nrem, long nins)
{
  EXIT
  cout << "POST postZt4ValuesChanged" <<
             "  id=" << zt4_ident_name(ident) <<
             "  i=" << index <<
             "  rem=" << nrem <<
             "  ins=" << nins <<
                endl;
}




void FgInform::preTredValuesChanged(FieldGeometry*)	/* ehs */
{
  EXIT
  cout << "PRE   preTredValuesChanged" <<
                endl;
}

void FgInform::postTredValuesChanged(FieldGeometry*)	/* ehs */
{
  EXIT
  cout << "POST postTredValuesChanged" <<
                endl;
}


void FgInform::preSortReceiverPatterns(FieldGeometry*)
{
  EXIT
  cout << "PRE   preSortReceiverPatterns" << endl;
}

void FgInform::postSortReceiverPatterns(FieldGeometry*)
{
  EXIT
  cout << "POST postSortReceiverPatterns" << endl;
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
