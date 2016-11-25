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

//---------------------- refraction_data.cc -----------------------//
//---------------------- refraction_data.cc -----------------------//
//---------------------- refraction_data.cc -----------------------//

//         implementation file for the RefractionData class
//              derived from the BaseData class
//                     subdirectory data


#include "oprim/refraction_data.hh"
#include "cprim.h"
#include "inquire.h"
#include "named_constants.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>


//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


RefractionData::RefractionData(
                   const char *filename1, const char *filename2,
                   int   required1, int   required2,
                   RefrMsgFun *msgfun, void *msgdata,
                   long typegp, const char *tracefile,
                   RefrTfOpen *tf_open, RefrTfRead *tf_read,
                   RefrTfRewind *tf_rewind, RefrTfClose *tf_close,
                   int tf_pickhead)
           : BaseData(),
                 _ss           (NULL),
                 _error        (TRUE),
                 _status1      (FILE_ERROR),
                 _status2      (FILE_ERROR),
                 _status3      (FILE_ERROR),
                 _id_constant  (0),
                 _needs_saving (FALSE)
{
  strcpy(_msg1    , "error");
  strcpy(_msg2    , "error");
  strcpy(_msg3    , "error");
  strcpy(_errmsg  , "error");
  strcpy(_filename, "error");

  _status3 = inquire((char*)filename1, (char*)filename2,
                     required1, required2,
                     _msg1, _msg2, _msg3, &_status1, &_status2);

  if(_status3 == FILE_ERROR)
       {
       strcpy(_errmsg, _msg3);
       }
  else
       {
       _ss = scrs_open((char*)filename1, (char*)filename2,
                   (MsgFun*)msgfun,
                   msgdata,
                   typegp, _status3, _errmsg, (char*)tracefile,
                   (TFOpen      *)tf_open,
                   (TFReadHeader*)tf_read,
                   (TFRewind    *)tf_rewind,
                   (TFClose     *)tf_close, tf_pickhead);
       }

  if(_ss)
       {
       _error = FALSE;
       if(_status3 == FILE_READ_ONLY) strcpy(_filename, filename1);
       else                           strcpy(_filename, filename2);
       _id_constant = lastShotRecord() + 1000;
       }
}



//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

RefractionData::~RefractionData()
{
  if(_ss)
       {
       int error = scrs_close(_ss);
       if(error) free(_ss);
       }
}



//-------------- private id helpers ----------------------//
//-------------- private id helpers ----------------------//
//-------------- private id helpers ----------------------//

enum { ID_SHOTS = -99, ID_ELEV, ID_DEPTH, ID_UPTIME };

enum { ID_XPE = 1, ID_YPE, ID_XEP, ID_YEP, ID_XOP, ID_YOP,
       ID_XYP, ID_XYE, ID_XYO };


long RefractionData::createId(long igrp, int id_which)
{
  if     (igrp <  0                ) igrp = 0;
  else if(igrp <  firstShotRecord()) igrp = 0;
  else if(igrp >  lastShotRecord ()) igrp = 0;
  else if(igrp >= _id_constant     ) igrp = 0;
  long id = id_which * _id_constant + igrp;
  return id;
}


int RefractionData::getWhichFromId(long id)
{
  int id_which = (int)(id / _id_constant);
  return id_which;
}


long RefractionData::getGroupFromId(long id)
{
  long id_which = id / _id_constant;
  long igrp = id - (id_which * _id_constant);
  return igrp;
}



//------------------------- get ids ---------------------------//
//------------------------- get ids ---------------------------//
//------------------------- get ids ---------------------------//

long RefractionData::idXrecPicksElev(long igrp)
{ return createId(igrp, ID_XPE); }

long RefractionData::idYrecPicksElev(long igrp)
{ return createId(igrp, ID_YPE); }

long RefractionData::idXrecElevPicks(long igrp)
{ return createId(igrp, ID_XEP); }

long RefractionData::idYrecElevPicks(long igrp)
{ return createId(igrp, ID_YEP); }

long RefractionData::idXrecOffsetPicks(long igrp)
{ return createId(igrp, ID_XOP); }

long RefractionData::idYrecOffsetPicks(long igrp)
{ return createId(igrp, ID_YOP); }

long RefractionData::idXrecYrecPicks(long igrp)
{ return createId(igrp, ID_XYP); }

long RefractionData::idXrecYrecElev(long igrp)
{ return createId(igrp, ID_XYE); }

long RefractionData::idXrecYrecOffset(long igrp)
{ return createId(igrp, ID_XYO); }

long RefractionData::idShots () { return ID_SHOTS ; }
long RefractionData::idElev  () { return ID_ELEV  ; }
long RefractionData::idDepth () { return ID_DEPTH ; }
long RefractionData::idUptime() { return ID_UPTIME; }



//-------------------- derived from base data ---------------//
//-------------------- derived from base data ---------------//
//-------------------- derived from base data ---------------//


int RefractionData::getNumPts(long id)
{
  if(id >= ID_SHOTS && id <= ID_ELEV) return numberOfShotRecords();
  return numberOfChannels();
}


float RefractionData::getX(int i, long id)
{
  int error;
  if(id >= ID_SHOTS && id <= ID_UPTIME)
     {
     error = readShotRecord(i+1);
     if(error) return 0.0;
     return (float)getSourceXgp();
     }
  int id_which = getWhichFromId(id);
  long igrp    = getGroupFromId(id);
  if(igrp > 0)
     {
     error = readShotRecord(igrp);
     if(error) return 0.0;
     }
  switch(id_which)
     {
     case ID_XPE: return (float)getReceiverXgp(i);
     case ID_YPE: return (float)getReceiverYgp(i);
     case ID_XEP: return (float)getReceiverXgp(i);
     case ID_YEP: return (float)getReceiverYgp(i);
     case ID_XOP: return (float)getReceiverXgp(i);
     case ID_YOP: return (float)getReceiverYgp(i);
     case ID_XYP: return (float)getReceiverXgp(i);
     case ID_XYE: return (float)getReceiverXgp(i);
     case ID_XYO: return (float)getReceiverXgp(i);
     default:     break;
     }
  return 0.0;
}


float RefractionData::getY(int i, long id)
{
  int error;
  if(id >= ID_SHOTS && id <= ID_UPTIME)
     {
     error = readShotRecord(i+1);
     if(error) return 0.0;
     return (float)getSourceYgp();
     }
  int id_which = getWhichFromId(id);
  long igrp    = getGroupFromId(id);
  if(igrp > 0)
     {
     error = readShotRecord(igrp);
     if(error) return 0.0;
     }
  switch(id_which)
     {
     case ID_XPE: return (float)getPickTime(i);
     case ID_YPE: return (float)getPickTime(i);
     case ID_XEP: return (float)getReceiverElevation(i);
     case ID_YEP: return (float)getReceiverElevation(i);
     case ID_XOP: return (float)getOffset(i);
     case ID_YOP: return (float)getOffset(i);
     case ID_XYP: return (float)getReceiverYgp(i);
     case ID_XYE: return (float)getReceiverYgp(i);
     case ID_XYO: return (float)getReceiverYgp(i);
     default:     break;
     }
  return 0.0;
}


float RefractionData::getZ(int i, long id)
{
  int error;
  if(id >= ID_SHOTS && id <= ID_UPTIME)
     {
     error = readShotRecord(i+1);
     if(error) return 0.0;
     switch(id)
        {
        case ID_SHOTS  :  return (float)getShotRecordNumber();
        case ID_ELEV   :  return (float)getSourceElevation ();
        case ID_DEPTH  :  return (float)getSourceDepth     ();
        case ID_UPTIME :  return (float)getSourceUpholeTime();
        default:          return 0.0;
        }
     }
  int id_which = getWhichFromId(id);
  long igrp    = getGroupFromId(id);
  if(igrp > 0)
     {
     error = readShotRecord(igrp);
     if(error) return 0.0;
     }
  switch(id_which)
     {
     case ID_XPE: return (float)getReceiverElevation(i);
     case ID_YPE: return (float)getReceiverElevation(i);
     case ID_XEP: return (float)getPickTime(i);
     case ID_YEP: return (float)getPickTime(i);
     case ID_XOP: return (float)getPickTime(i);
     case ID_YOP: return (float)getPickTime(i);
     case ID_XYP: return (float)getPickTime(i);
     case ID_XYE: return (float)getReceiverElevation(i);
     case ID_XYO: return (float)getOffset(i);
     default:     break;
     }
  return 0.0;
}



//--------------- read or write one record --------------------//
//--------------- read or write one record --------------------//
//--------------- read or write one record --------------------//


int RefractionData::readHeaderRecord()
{
  assert(_ss);
  return scrs_head_get(_ss);
}


int RefractionData::writeHeaderRecord()
{
  assert(_ss);
  return scrs_head_put(_ss);
}


int RefractionData::readNextShotRecord()
{
  assert(_ss);
  _needs_saving = FALSE;
  return scrs_shot_get(_ss);
}


int RefractionData::writeNextShotRecord()
{
  assert(_ss);
  _needs_saving = FALSE;
  return scrs_shot_put(_ss);
}


int RefractionData::readShotRecord(long igroup)
{
  assert(_ss);
  long current = getShotRecordNumber();
  if(igroup == current) return 0;
  if(_needs_saving)
       {
       int error = writeShotRecord(current);
       if(error) return error;
       }
  _needs_saving = FALSE;
  return scrs_shot_getd(_ss, igroup);
}


int RefractionData::writeShotRecord(long igroup)
{
  assert(_ss);
  _needs_saving = FALSE;
  return scrs_shot_putd(_ss, igroup);
}


int RefractionData::readTailRecord()
{
  assert(_ss);
  return scrs_tail_get(_ss);
}


int RefractionData::writeTailRecord()
{
  assert(_ss);
  return scrs_tail_put(_ss);
}



//----------------- get general information ---------------------//
//----------------- get general information ---------------------//
//----------------- get general information ---------------------//


long RefractionData::numberOfShotRecords() const
{
  assert(_ss);
  return scrs_get_ngrp(_ss);
}

long RefractionData::numberOfChannels() const
{
  assert(_ss);
  return scrs_get_nch(_ss);
}

long RefractionData::firstShotRecord() const
{
  assert(_ss);
  return scrs_get_first_profile(_ss);
}


long RefractionData::lastShotRecord() const
{
  assert(_ss);
  return scrs_get_last_profile(_ss);
}


long RefractionData::latestModifiedShotRecord() const
{
  assert(_ss);
  return scrs_get_latest(_ss);
}


long RefractionData::nearestShotRecord(long igroup, long adjustment) const
{
  assert(_ss);
  return scrs_get_nearest_profile(_ss, igroup, adjustment);
}


//--------------- get and set header information ----------------//
//--------------- get and set header information ----------------//
//--------------- get and set header information ----------------//


int RefractionData::getHeaderValues(long *ngrp, long *incgp, float *rinc,
                                    long *nch, float *trsh, char *izc)
{
  assert(_ss);
  return scrs_head_grab(_ss, ngrp, incgp, rinc, nch, trsh, izc);
}



int RefractionData::setHeaderValues(long ngrp, long incgp, float rinc,
                                    long nch, float trsh, char *izc)
{
  assert(_ss);
  return scrs_head_fill(_ss, ngrp, incgp, rinc, nch, trsh, izc);
}



int RefractionData::getLimits(long *itmax, long *mxxgp, long *mxygp,
                                           long *mnxgp, long *mnygp, long *mxo)
{
  assert(_ss);
  return scrs_head_grab3(_ss, itmax, mxxgp, mxygp, mnxgp, mnygp, mxo);
}



int RefractionData::setLimits(long itmax, long mxxgp, long mxygp,
                                          long mnxgp, long mnygp, long mxo)
{
  assert(_ss);
  return scrs_head_fill3(_ss, itmax, mxxgp, mxygp, mnxgp, mnygp, mxo);
}



int RefractionData::getPickWindows(long *nxpw, long *nypw, long *npwoff,
                                   long *ipkwhx, long *ipkwhy,
                                   float *xpw, float *ypw, float *pkwn)
{
  assert(_ss);
  return scrs_head_grab2(_ss, nxpw, nypw, npwoff, ipkwhx, ipkwhy,
                                                     xpw, ypw, pkwn);
}



int RefractionData::setPickWindows(long nxpw, long nypw, long npwoff,
                                   long ipkwhx, long ipkwhy,
                                   float *xpw, float *ypw, float *pkwn)
{
  assert(_ss);
  return scrs_head_fill2(_ss, nxpw, nypw, npwoff, ipkwhx, ipkwhy,
                                                     xpw, ypw, pkwn);
}



//------------------ get shot record scalars -----------------//
//------------------ get shot record scalars -----------------//
//------------------ get shot record scalars -----------------//


long  RefractionData::getShotRecordNumber() const
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  return NearestInteger(shot[8]);
}

long  RefractionData::getSourceXgp() const
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  return NearestInteger(shot[0]);
}

long  RefractionData::getSourceYgp() const
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  return NearestInteger(shot[7]);
}

long  RefractionData::getSourceElevation() const
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  return NearestInteger(shot[3]);
}

long  RefractionData::getSourceDepth() const
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  return NearestInteger(shot[1]);
}

float RefractionData::getSourceUpholeTime() const
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  return shot[2];
}



//------------------ set shot record scalars -----------------//
//------------------ set shot record scalars -----------------//
//------------------ set shot record scalars -----------------//


void  RefractionData::setShotRecordNumber(long igroup)
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  shot[8] = igroup;
  scrs_shot_fill(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  _needs_saving = TRUE;
}

void  RefractionData::setSourceXgp(long source_xgp)
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  shot[0] = source_xgp;
  scrs_shot_fill(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  _needs_saving = TRUE;
}

void  RefractionData::setSourceYgp(long source_ygp)
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  shot[7] = source_ygp;
  scrs_shot_fill(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  _needs_saving = TRUE;
}

void  RefractionData::setSourceElevation(long source_elev)
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  shot[3] = source_elev;
  scrs_shot_fill(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  _needs_saving = TRUE;
}

void  RefractionData::setSourceDepth(long source_depth)
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  shot[1] = source_depth;
  scrs_shot_fill(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  _needs_saving = TRUE;
}

void  RefractionData::setSourceUpholeTime(float source_uptime)
{
  assert(_ss);
  float shot[10];
  scrs_shot_grab(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  shot[2] = source_uptime;
  scrs_shot_fill(_ss, shot, NULL, NULL, NULL, NULL, NULL);
  _needs_saving = TRUE;
}


//---------- get single value from shot record array -----------//
//---------- get single value from shot record array -----------//
//---------- get single value from shot record array -----------//

    // this stuff is enormously and ridiculously inefficient,
    // and should be changed by keeping a copy of the entire
    // shot record in this class.

float  RefractionData::getPickTime(long ich)     const
{
  long nch = numberOfChannels();
  ich = ConstrainValue(ich, 1, nch);
  float *picks = new float[nch];
  getPickTimes(picks);
  float pick = picks[ich - 1];
  delete [] picks;
  return pick;
}


long  RefractionData::getReceiverXgp(long ich)     const
{
  long nch = numberOfChannels();
  ich = ConstrainValue(ich, 1, nch);
  long *rec_xgps = new long[nch];
  getReceiverXgps(rec_xgps);
  long rec_xgp = rec_xgps[ich - 1];
  delete [] rec_xgps;
  return rec_xgp;
}


long  RefractionData::getReceiverYgp(long ich)     const
{
  long nch = numberOfChannels();
  ich = ConstrainValue(ich, 1, nch);
  long *rec_ygps = new long[nch];
  getReceiverYgps(rec_ygps);
  long rec_ygp = rec_ygps[ich - 1];
  delete [] rec_ygps;
  return rec_ygp;
}


long  RefractionData::getReceiverElevation(long ich)     const
{
  long nch = numberOfChannels();
  ich = ConstrainValue(ich, 1, nch);
  long *rec_elevs = new long[nch];
  getReceiverElevations(rec_elevs);
  long rec_elev = rec_elevs[ich - 1];
  delete [] rec_elevs;
  return rec_elev;
}


long  RefractionData::getOffset(long ich)     const
{
  long nch = numberOfChannels();
  ich = ConstrainValue(ich, 1, nch);
  long *offsets = new long[nch];
  getOffsets(offsets);
  long offset = offsets[ich - 1];
  delete [] offsets;
  return offset;
}

//------------------ get shot record arrays -----------------//
//------------------ get shot record arrays -----------------//
//------------------ get shot record arrays -----------------//

    // this stuff is inefficient, and should be changed by
    // keeping a copy of the entire shot record in this class.

void  RefractionData::getPickTimes(float *picks)     const
{
  assert(_ss);
  scrs_shot_grab(_ss, NULL, picks, NULL, NULL, NULL, NULL);
}

void  RefractionData::getReceiverXgps(long *rec_xgps)  const
{
  assert(_ss);
  scrs_shot_grab(_ss, NULL, NULL, rec_xgps, NULL, NULL, NULL);
}

void  RefractionData::getReceiverYgps(long *rec_ygps)  const
{
  assert(_ss);
  scrs_shot_grab(_ss, NULL, NULL, NULL, rec_ygps, NULL, NULL);
}

void  RefractionData::getReceiverElevations(long *rec_elevs) const
{
  assert(_ss);
  scrs_shot_grab(_ss, NULL, NULL, NULL, NULL, NULL, rec_elevs);
}

void  RefractionData::getOffsets(long *offsets)   const
{
  assert(_ss);
  scrs_shot_grab(_ss, NULL, NULL, NULL, NULL, offsets, NULL);
}



//------------------ set shot record arrays -----------------//
//------------------ set shot record arrays -----------------//
//------------------ set shot record arrays -----------------//

    // some of this stuff is inefficient, and should be changed by
    // keeping a copy of the entire shot record in this class.

void  RefractionData::setPickTimes(float *picks)
{
  assert(_ss);
  scrs_shot_fill(_ss, NULL, picks, NULL, NULL, NULL, NULL);
  _needs_saving = TRUE;
}

void  RefractionData::setReceiverXgps(long *rec_xgps)
{
  assert(_ss);
  scrs_shot_fill(_ss, NULL, NULL, rec_xgps, NULL, NULL, NULL);
  _needs_saving = TRUE;
}

void  RefractionData::setReceiverYgps(long *rec_ygps)
{
  assert(_ss);
  scrs_shot_fill(_ss, NULL, NULL, NULL, rec_ygps, NULL, NULL);
  _needs_saving = TRUE;
}

void  RefractionData::setReceiverElevations(long *rec_elevs)
{
  assert(_ss);
  scrs_shot_fill(_ss, NULL, NULL, NULL, NULL, NULL, rec_elevs);
  _needs_saving = TRUE;
}

void  RefractionData::setOffsets(long *offsets)
{
  assert(_ss);
  scrs_shot_fill(_ss, NULL, NULL, NULL, NULL, offsets, NULL);
  _needs_saving = TRUE;
}


int RefractionData::setPicksByTraceHeaders(float *head, long nwords,
                                           float *picks)
{
  assert(_ss);
  _needs_saving = TRUE;
  return scrs_shot_fillh_spws(_ss, head, nwords, picks);
}


int RefractionData::setPickByTraceHeader(float *head, long nwords,
                                         float pick, long ich)
{
  assert(_ss);
  _needs_saving = TRUE;
  return scrs_shot_fill1_spws(_ss, head, nwords, pick, ich);
}



//----------------- read or write picks for CBYT ------------------//
//----------------- read or write picks for CBYT ------------------//
//----------------- read or write picks for CBYT ------------------//


int RefractionData::readPicks(const float head[], long nwords,
                                             long ntraces, float picks[])
{
  assert(_ss);
  _needs_saving = FALSE;
  return scrs_get_picks_spws(_ss, (float*)head, nwords, ntraces, picks);
}



int RefractionData::writePicks(const float head[], long nwords,
                                             long ntraces, float picks[])
{
  assert(_ss);
  _needs_saving = FALSE;
  return scrs_put_picks_spws(_ss, (float*)head, nwords, ntraces, picks);
}



int RefractionData::readPrevPicks(const float head[], long nwords,
                      long ntraces, float picks[], long align)
{
  assert(_ss);
  _needs_saving = FALSE;
  return scrs_get_prev_picks_spws(_ss, (float*)head, nwords, ntraces, picks,
                                                    align);
}



int RefractionData::readNextPicks(const float head[], long nwords,
                      long ntraces, float picks[], long align)
{
  assert(_ss);
  _needs_saving = FALSE;
  return scrs_get_next_picks_spws(_ss, (float*)head, nwords, ntraces, picks,
                                                    align);
}



int RefractionData::readSelPicks (const float head[], long nwords,
                      long ntraces, float picks[], long align, long grp)
{
  assert(_ss);
  _needs_saving = FALSE;
  return scrs_get_sel_picks_spws (_ss, (float*)head, nwords, ntraces, picks,
                                                    align, grp);
}



//---------------- static methods to check files -----------------//
//---------------- static methods to check files -----------------//
//---------------- static methods to check files -----------------//


long RefractionData::checkValidity(char *filename, char *info,
                                            long *ngrp, long *nch)
{
  return scrs_check_validity(filename, info, ngrp, nch);
}



void RefractionData::checkValidities(char *filename1, char *filename2,
                                     long *valid1, long *valid2,
                                     char *info1, char *info2,
                                     long *same_datasets)
{
  scrs_check_validities(filename1, filename2,
                        valid1, valid2, info1, info2,
                        same_datasets);
}


void RefractionData::checkValiditiesAlternative(void * /*data*/,
                                     char *filename1, char *filename2,
                                     long *valid1, long *valid2,
                                     char *info1, char *info2,
                                     long *same_datasets)
{
  scrs_check_validities(filename1, filename2,
                        valid1, valid2, info1, info2,
                        same_datasets);
}



long RefractionData::inquire(char *filename1, char *filename2,
                             int required1, int required2,
                             char *msg1, char *msg2, char *msg3,
                             long *status1, long *status2)
{
  return scrs_inquire(filename1, filename2,
                      required1, required2,
                      msg1, msg2, msg3,
                      status1, status2);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
