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

//----------------------- tp_statfile_pair.cc -----------------------//
//----------------------- tp_statfile_pair.cc -----------------------//
//----------------------- tp_statfile_pair.cc -----------------------//

//       implementation file for the TpStatfilePair class
//               derived from the TpPairBase class
//                       subdirectory pick

              // general CPS static file interface

#include "cprim.h"
#include "named_constants.h"
#include "pick/tp_statfile_pair.hh"
#include "pick/tp_popup_base.hh"
#include "stat/static_kernal.hh"
#include "stat/statio_wrapper.hh"
#include "oprim/history_cards.hh"
#include "sp/seis_plot.hh"
#include "sl/shell_watch.hh"
#include "sl/slp_file.hh"
#include "plot/pick_watch.hh"
#include <iostream.h>
#include <string.h>
#include <assert.h>
#include "inquire.h"


//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpStatfilePair::TpStatfilePair(SLDelay *slparent,
                          TpPopupBase *pop, SeisPlot* /*dummy*/,
                          const char * const filetype,
                          const char * const extension,
                          const Boolean      required1,
                          const Boolean      required2,
                          const char * const program,
                          const char * const default_stattype)
       : TpPairBase(slparent, "statfile_pair",
                         filetype, extension, required1, required2),
                  _kernal            (NULL),
                  _pop               (pop),

                  _align             (XMATCH),
                  _xy_switch         (FALSE),
                  _xsel              (0.0),
                  _ysel              (0.0),
                  _xysel             (0.0),

                  _allow_header_input(TRUE)
{
  assert(_pop);
  _kernal = new StaticKernal(program);
  _kernal->setStattype(default_stattype);
  adjustSelectedXbin(_xsel, FALSE);
  adjustSelectedYbin(_ysel, FALSE);
  strcpy(_encoding, "ascii");
}


    // NOTE: The variables _xsel, _ysel, _xysel are
    //       reset in adjustSelectedXbin and adjustSelectedYbin.


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpStatfilePair::~TpStatfilePair()
{
  delete _kernal;
}



//--------------------- adjust selected bins ----------------------//
//--------------------- adjust selected bins ----------------------//
//--------------------- adjust selected bins ----------------------//

  // If directional is TRUE, adjustment is made to the nearest bin
  //        at a location in the direction of the specified bin.
  // If directional is FALSE, adjustment is made to the nearest bin,
  //        regardless of the direction from the specified bin.
  // Returns TRUE if a change was made, and FALSE otherwise.

Boolean TpStatfilePair::adjustSelectedXbin(float xsel, Boolean directional)
{
  long adjustment = 0;
  if(directional && xsel > _xsel) adjustment =  1;
  if(directional && xsel < _xsel) adjustment = -1;
  float keep = _xsel;
  _xsel = _kernal->getNearestXbinCenter(xsel, adjustment);
  if(_xy_switch) _xysel = _xsel;
  return (_xsel != keep);
}


Boolean TpStatfilePair::adjustSelectedYbin(float ysel, Boolean directional)
{
  long adjustment = 0;
  if(directional && ysel > _ysel) adjustment =  1;
  if(directional && ysel < _ysel) adjustment = -1;
  float keep = _ysel;
  _ysel = _kernal->getNearestYbinCenter(ysel, adjustment);
  if(!_xy_switch) _xysel = _ysel;
  return (_ysel != keep);
}




//------------ get first and last displayed ybin -----------//
//------------ get first and last displayed ybin -----------//
//------------ get first and last displayed ybin -----------//


float TpStatfilePair::getFirstDisplayedYbin()  const
{
  SeisPlot *sp = _pop->getSeisPlot();         // new 9/2/97.
  if(!sp->isPlotDisplayed()) return 0.0;      // new 2/18/97.
  long n =  sp->memoryTraces();
  long nhy = _kernal->getNhy();
  if(nhy > 0 && n > 0)
       {
       const float *head =  sp->firstMemoryHeaderData();
       if(head) return head[nhy - 1];
       }
  return 0.0;
}



float TpStatfilePair::getLastDisplayedYbin()  const
{
  SeisPlot *sp = _pop->getSeisPlot();         // new 9/2/97.
  if(!sp->isPlotDisplayed()) return 0.0;      // new 2/18/97.
  long n =  sp->memoryTraces();
  long nhy = _kernal->getNhy();
  if(nhy > 0 && n > 0)
       {
       const float *head =  sp->firstMemoryHeaderData();
       long       nwords =  sp->numHeaders();
       return head[nhy - 1 + (n - 1) * nwords];
       }
  return 0.0;
}



//-------------------- search for special history card -------------------//
//-------------------- search for special history card -------------------//
//-------------------- search for special history card -------------------//

// returns history card number (or -1 if not found).


static int search_for_special_history_card(const StaticKernal *kernal)
{
  long ncards = kernal->history()->numHistoryCards();
  for(int icard = 0; icard < ncards; icard++)
      {
      const char *card = kernal->history()->getHistoryCard(icard);
      if(memcmp(card, "###fvar2 latest:", 16) == 0) return icard;
      }
  return -1;
}



//------------ get first and last latest ybin updated -----------//
//------------ get first and last latest ybin updated -----------//
//------------ get first and last latest ybin updated -----------//


float TpStatfilePair::getFirstLatestYbinUpdated()  const
{
  float late1, late2;
  int icard = search_for_special_history_card(_kernal);
  if(icard == -1) return 0.0;
  const char *card = _kernal->history()->getHistoryCard(icard);
  int e = sscanf(card, "###fvar2 latest: %g %g", &late1, &late2);
  if (e == 2) return late1;
  return 0.0;
}


float TpStatfilePair::getLastLatestYbinUpdated()  const
{
  float late1, late2;
  int icard = search_for_special_history_card(_kernal);
  if(icard == -1) return 0.0;
  const char *card = _kernal->history()->getHistoryCard(icard);
  int e = sscanf(card, "###fvar2 latest: %g %g", &late1, &late2);
  if (e == 2) return late2;
  return 0.0;
}



//------------------------ do validate ------------------------//
//------------------------ do validate ------------------------//
//------------------------ do validate ------------------------//

            // overrides SLFilePairPlus
            // called from private SLFilePairPlus::validateTrap
            //       which is called from SLFilePair
            //       which is called from make_filepair

void TpStatfilePair::doValidate (const char *filename1,
                           const char *filename2,
                           long *valid1, long *valid2,
                           char *info1, char *info2,
                           long *same_datasets)
{
  // cout << "am in TpStatfilePair::doValidate" << endl;
  int nhx=0, nhy=0, nx=0, ny=0, error;
  float x1=0.0, y1=0.0, xinc=0.0, yinc=0.0;

  StatioWrapper *statio = new StatioWrapper(0);
  error = statio->validateFile(filename1, info1);
  if(!error)
      {
      *valid1 = INQUIRE_VALID_YES;
      nhx  = statio->getScalarInteger("nhx" );
      nhy  = statio->getScalarInteger("nhy" );
      nx   = statio->getScalarInteger("nx"  );
      ny   = statio->getScalarInteger("ny"  );
      x1   = statio->getScalarFloat  ("x1"  );
      y1   = statio->getScalarFloat  ("y1"  );
      xinc = statio->getScalarFloat  ("xinc");
      yinc = statio->getScalarFloat  ("yinc");
      _kernal->setNhx (nhx);
      _kernal->setNhy (nhy);
      _kernal->setNx  (nx);
      _kernal->setNy  (ny);
      _kernal->setX1  (x1);
      _kernal->setY1  (y1);
      _kernal->setXinc(xinc);
      _kernal->setYinc(yinc);
      _allow_header_input = FALSE;
      }
  else
      {
      *valid1 = INQUIRE_VALID_NO;
      _allow_header_input = SLpFile::isAnEmptyFilename (filename1);
      }

  error = statio->validateFile(filename2, info2);
  if(!error)
      {
      *valid2 = INQUIRE_VALID_YES;
      nhx  = statio->getScalarInteger("nhx" );
      nhy  = statio->getScalarInteger("nhy" );
      nx   = statio->getScalarInteger("nx"  );
      ny   = statio->getScalarInteger("ny"  );
      x1   = statio->getScalarFloat  ("x1"  );
      y1   = statio->getScalarFloat  ("y1"  );
      xinc = statio->getScalarFloat  ("xinc");
      yinc = statio->getScalarFloat  ("yinc");
      }
  else
      {
      *valid2 = INQUIRE_VALID_NO;
      }

  delete statio;

 *same_datasets = (  *valid1 == INQUIRE_VALID_YES  &&
                     *valid2 == INQUIRE_VALID_YES  &&
                        nhx  == _kernal->getNhx () &&
                        nhy  == _kernal->getNhy () &&
                        nx   == _kernal->getNx  () &&
                        ny   == _kernal->getNy  () &&
                        x1   == _kernal->getX1  () &&
                        y1   == _kernal->getY1  () &&
                        xinc == _kernal->getXinc() &&
                        yinc == _kernal->getYinc()   );

  adjustSelectedXbin(_xsel, FALSE);
  adjustSelectedYbin(_ysel, FALSE);
  _pop->update();
}



//------------- helper for working message ---------------------------//
//------------- helper for working message ---------------------------//
//------------- helper for working message ---------------------------//

static void working(MsgFun *msgfun, void *msgdata,
                    long i, long n, char *phrase1, char *phrase2)
{
  char msg[200];

  if(msgfun && i == 10*(i/10))
       {
       sprintf(msg, "%s record %d of %d %s", phrase1, i+1, n, phrase2);
       msgfun(msgdata, msg);
       }
}



//--------- fill in data structure from tracefile info ---------------//
//--------- fill in data structure from tracefile info ---------------//
//--------- fill in data structure from tracefile info ---------------//

#define MAXW      65
#define ERROR(n)                                                       \
    {                                                                  \
    if(msg) sprintf(msg,                                               \
           "error %d trying to\ncreate static file\nfrom traces", n);  \
    if(glbl) tracefile_close(glbl);                                    \
    return (n);                                                        \
    }


static int create_from_tracefile(StaticKernal *kernal, MsgFun *msgfun,
             void *msgdata, char *msg, char *tracefile,
             TFOpen   *tracefile_open  , TFReadHeader *tracefile_read_header,
             TFRewind *tracefile_rewind, TFClose      *tracefile_close)
{
  long ntraces, nwords, e, j;
  float head[MAXW], xend=0.0, yend=0.0, headx=0.0, heady=0.0;
  float x1=0.0, y1=0.0;
  int nx=0, ny=0;
  void *glbl = NULL;
  int open_file = 1;
  long nhx = kernal->getNhx();
  long nhy = kernal->getNhy();

  if(!tracefile || !tracefile_open || !tracefile_read_header ||
     !tracefile_rewind || !tracefile_close)                       ERROR(401)
  glbl = tracefile_open(tracefile, &nwords, &ntraces);  if(!glbl) ERROR(402)
  if(ntraces <= 0)                                                ERROR(403)
  if(nhx > nwords)                                                ERROR(451)
  if(nhy > nwords)                                                ERROR(452)

  int i = 0;
  while(i < ntraces && nx == 0)
       {
       e = tracefile_read_header(glbl, head, open_file);
                                                      if(e == -1) ERROR(405)
              // e == -1 means a bad error has occurred.
              // e == -2 means the first trace does not have hdr1 = 1.
              // e == nwords means all is ok.
       open_file = 0;
                    headx = head[nhx - 1];
       if(nhy >= 1) heady = head[nhy - 1];
       if(i == 0)
            {
            x1 = xend = headx;
            y1 = yend = heady;
            }
       else if(heady != y1)
            {
            nx = i;
            }
       i++;
       }

  if(nx == 0) nx = ntraces;
  if(nx <= 2)                                                  ERROR(406)
  ny = ntraces / nx;                    if(ntraces != nx * ny) ERROR(407)
  e = tracefile_rewind(glbl);                            if(e) ERROR(412)
  for(i = 0; i < ny; i++)
       {
       working(msgfun, msgdata, i, ny, "creating", "on output file");
       for(j = 0; j < nx; j++)
            {
            e = tracefile_read_header(glbl, head, open_file);
                                                   if(e == -1) ERROR(413)
                         headx = head[nhx - 1];
            if(nhy >= 1) heady = head[nhy - 1];
            x1     = MinimumValue(x1    , headx);
            y1     = MinimumValue(y1    , heady);
            xend   = MaximumValue(xend  , headx);
            yend   = MaximumValue(yend  , heady);
            }
       }
  kernal->setX1  (x1);
  kernal->setY1  (y1);
  kernal->setXend(xend);
  kernal->setYend(yend);
  tracefile_close(glbl);
  if(msg) strcpy(msg, " ");
  return 0;
}



//-------------- prepare data for cbyt --------------------------------//
//-------------- prepare data for cbyt --------------------------------//
//-------------- prepare data for cbyt --------------------------------//


static int prepare(StaticKernal *kernal, const char *encoding,
                   const char *filename1, const char *filename2,
                   MsgFun *msgfun, void *msgdata, long status,
                   char *msg, char *tracefile,
       TFOpen   *tracefile_open  , TFReadHeader *tracefile_read_header,
       TFRewind *tracefile_rewind, TFClose      *tracefile_close)
{
  long e;

  if(status == FILE_CREATE)
       {
       if(msgfun) msgfun(msgdata, "creating static file...");
       if(kernal->getNx() == 1)
            {
            e = create_from_tracefile(kernal,
                 msgfun, msgdata, msg, tracefile,
                 tracefile_open, tracefile_read_header,
                 tracefile_rewind, tracefile_close);    if(e) goto error;
            }
       e = kernal->saveFile(filename2, msg, encoding);  if(e) goto error;
       }
  else if(status == FILE_COPY)
       {
       if(msgfun) msgfun(msgdata, "copying static file...");
       e = kernal->readFile(filename1, msg);            if(e) goto error;
       e = kernal->saveFile(filename2, msg, encoding);  if(e) goto error;
       }
  else if(status == FILE_READ_ONLY)
       {
       if(msgfun) msgfun(msgdata, "reading static file...");
       e = kernal->readFile(filename1, msg);            if(e) goto error;
       }
  else if(status == FILE_UPDATE)
       {
       if(msgfun) msgfun(msgdata, "reading static file...");
       e = kernal->readFile(filename1, msg);            if(e) goto error;
       }
  else
       {
       e = 777;  if(msg) strcpy(msg, "illegal status");       goto error;
       }
  if(msgfun) msgfun(msgdata, " ");
  return 0;

  error:
     if(msgfun) msgfun(msgdata, " ");
     kernal->clear();
     return e;
}



//-------------------- do open --------------------------------//
//-------------------- do open --------------------------------//
//-------------------- do open --------------------------------//

            // overrides SLFilePairPlus
            // called from public SLFilePairPlus::openFiles()

int TpStatfilePair::doOpen (long status,
                              const char *filename1,
                              const char *filename2,
                              Boolean /*required1*/, Boolean /*required2*/,
                              FppWorkingMessage *working_message_trap,
                              void              *working_message_data,
                              char *msg)
{
  // cout << "am in TpStatfilePair::doOpen" << endl;

  SeisPlot *sp = _pop->getSeisPlot();                // new 9/2/97.
  int error = prepare(_kernal, _encoding, filename1, filename2,
                      (MsgFun*)working_message_trap, working_message_data,
                      status, msg, sp->filename(),
                      (TFOpen      *)tracefile_open,
                      (TFReadHeader*)tracefile_read_header,
                      (TFRewind    *)tracefile_rewind,
                      (TFClose     *)tracefile_close);
  if(error)
       {
       _allow_header_input = (status == FILE_CREATE);
       }
  else
       {
       _allow_header_input = FALSE;
       }
  adjustSelectedXbin(_xsel, FALSE);
  adjustSelectedYbin(_ysel, FALSE);
  _pop->update();
  return error;
}



//---------------------- do close ----------------------------//
//---------------------- do close ----------------------------//
//---------------------- do close ----------------------------//

            // overrides SLFilePairPlus
            // called from public SLFilePairPlus::closeFiles()

void TpStatfilePair::doClose()
{
  // cout << "am in TpStatfilePair::doClose" << endl;
  _kernal->clear();
}


//----------------- get or put picks for cbyt -------------------------//
//----------------- get or put picks for cbyt -------------------------//
//----------------- get or put picks for cbyt -------------------------//

#define CURR  1    // get  current  picks
#define PREV  2    // get  previous picks
#define NEXT  3    // get  next     picks
#define SEL   4    // get  selected picks
#define SAVE  5    // save current  picks


static void move_picks(StaticKernal *kernal, const float head[], long nwords,
                       long ntraces, float picks[],
                       long att, int option, float ysel, long xy_switch)
{
  for(int i = 0; i < ntraces; i++)
       {
       long nhx = kernal->getNhx();
       long nhy = kernal->getNhy();
       float xbin = head[nwords * i + nhx - 1];
       float ybin = 0.0;
       if(nhy >= 0) ybin = head[nwords * i + nhy - 1];
       float xbin2;
       float ybin2;
       if(xy_switch)
            {
            if     (option == PREV) xbin2 = xbin - kernal->getXinc();
            else if(option == NEXT) xbin2 = xbin + kernal->getXinc();
            else if(option == SEL ) xbin2 = ysel;
            else                    xbin2 = xbin;
            if     (att == XMATCH)   ybin2 = ybin;
            else                     ybin2 = ybin - xbin + xbin2;
            }
       else
            {
            if     (option == PREV) ybin2 = ybin - kernal->getYinc();
            else if(option == NEXT) ybin2 = ybin + kernal->getYinc();
            else if(option == SEL ) ybin2 = ysel;
            else                    ybin2 = ybin;
            if     (att == XMATCH)   xbin2 = xbin;
            else                     xbin2 = xbin - ybin + ybin2;
            }
       long xindex = kernal->getUnconstrainedIx(xbin2);
       long yindex = kernal->getUnconstrainedIy(ybin2);
       if(nhy == 0)
            {
            if(option == CURR || option == SAVE) yindex =  0;
            else                                 yindex = -1;
            }
       long nx = kernal->getNx();
       long ny = kernal->getNy();
       if(xindex >= 0 && xindex < nx &&
          yindex >= 0 && yindex < ny)
            {
            if(option == SAVE)
                 {
                 float value;
                 if(picks[i] == FNIL) value =          picks[i];
                 else                 value = 1000.0 * picks[i];
                 kernal->setValue(xindex, yindex, value);
                 }
            else
                 {
                 float value = kernal->getValue(xindex, yindex);
                 if(value == FNIL) picks[i] =         value;
                 else              picks[i] = 0.001 * value;
                 }
            }
       else if(option != SAVE)
            {
            picks[i] = MISSING;
            }
       }
}



//-------------------- do read current picks -------------------//
//-------------------- do read current picks -------------------//
//-------------------- do read current picks -------------------//

void TpStatfilePair::doReadCurrentPicks(float *picks,
                         const float *head, long nwords, long n)
{
  move_picks(_kernal, head, nwords, n, picks, XMATCH, CURR, 0.0, FALSE);
}



//-------------------- do read previous picks -------------------//
//-------------------- do read previous picks -------------------//
//-------------------- do read previous picks -------------------//

void TpStatfilePair::doReadPreviousPicks(float *picks,
                         const float *head, long nwords, long n)
{
  move_picks(_kernal, head, nwords, n, picks, _align, PREV, 0.0, _xy_switch);
}



//-------------------- do read next picks -------------------//
//-------------------- do read next picks -------------------//
//-------------------- do read next picks -------------------//

void TpStatfilePair::doReadNextPicks(float *picks,
                         const float *head, long nwords, long n)
{
  move_picks(_kernal, head, nwords, n, picks, _align, NEXT, 0.0, _xy_switch);
}



//-------------------- do read selected picks -------------------//
//-------------------- do read selected picks -------------------//
//-------------------- do read selected picks -------------------//

void TpStatfilePair::doReadSelectedPicks(float *picks,
                         const float *head, long nwords, long n)
{
  move_picks(_kernal, head, nwords, n, picks, _align, SEL, _xysel, _xy_switch);
}



//-------------------- do save current picks -------------------//
//-------------------- do save current picks -------------------//
//-------------------- do save current picks -------------------//

     // also sets picks to zero if action is not one of these:
     //      AUTOMATIC  MANUAL  ZERO  SNAP.
     // (this means that action == SHIFT_SNAP, which is a constant
     // which is not defined in this file.)

void TpStatfilePair::doSaveCurrentPicks(float *picks,
                const float *head, long nwords, long n, long action)
{
  if(action != AUTOMATIC && action != MANUAL &&
     action != ZERO      && action != SNAP)
        {
        float missing = MISSING;
        for(int i = 0; i < n; i++)
            {
            if(picks[i] != missing) picks[i] = 0.0;
            }
        }
  move_picks(_kernal, head, nwords, n, picks, XMATCH, SAVE, 0.0, FALSE);
}



//-------------------- do update file --------------------------//
//-------------------- do update file --------------------------//
//-------------------- do update file --------------------------//


void TpStatfilePair::doUpdateFile()
{
  char msg[333];
  ShellWatch watch1;
  PickWatch  watch2;
  _pop->showMessage("saving picks\nto file...");

  float fvar  = getFirstDisplayedYbin();
  float fvar2 = getLastDisplayedYbin();
  int icard = search_for_special_history_card(_kernal);
  if(icard == -1)
      {
      char card[81];
      sprintf(card, "###fvar2 latest: %g %g   latest ybins updated",
                                                   fvar, fvar2);
      _kernal->history()->addHistoryCard(card);
      }
  else
      {
      char *card = (char*)_kernal->history()->getHistoryCard(icard);
      sprintf(card, "###fvar2 latest: %g %g   latest ybins updated",
                                                  fvar, fvar2);
      }
  _kernal->saveFile(workingFilename(), msg, _encoding);
}



//---------------------- get variables -----------------------//
//---------------------- get variables -----------------------//
//---------------------- get variables -----------------------//

 
const char *TpStatfilePair::getStattype() const
{
  return _kernal->getStattype();
}
 

long  TpStatfilePair::getNhx  ()  const  { return _kernal->getNhx (); }
long  TpStatfilePair::getNhy  ()  const  { return _kernal->getNhy (); }
long  TpStatfilePair::getNhx2 ()  const  { return _kernal->getNhx2(); }
long  TpStatfilePair::getNhy2 ()  const  { return _kernal->getNhy2(); }
float TpStatfilePair::getX1   ()  const  { return _kernal->getX1  (); }
float TpStatfilePair::getY1   ()  const  { return _kernal->getY1  (); }
float TpStatfilePair::getXinc ()  const  { return _kernal->getXinc(); }
float TpStatfilePair::getYinc ()  const  { return _kernal->getYinc(); }
long  TpStatfilePair::getNx   ()  const  { return _kernal->getNx  (); }
long  TpStatfilePair::getNy   ()  const  { return _kernal->getNy  (); }
float TpStatfilePair::getXend ()  const  { return _kernal->getXend(); }
float TpStatfilePair::getYend ()  const  { return _kernal->getYend(); }



//-------------------- set variables -----------------------//
//-------------------- set variables -----------------------//
//-------------------- set variables -----------------------//


void TpStatfilePair::setOverlayAlignment(long align)
{
  if(align == _align) return;
  _align = align;
  _pop->updatePrevNextSelVectors();
  char msg[100];
  strcpy(msg, "overlay alignment changed to\n");
  switch(_align)
     {
     case XMATCH : if(_xy_switch) strcat(msg, "matching X bin");
                   else           strcat(msg, "matching Y bin");
                   break;
     case XYDIFF : if(_xy_switch) strcat(msg, "matching X-Y bin");
                   else           strcat(msg, "matching Y-X bin");
                   break;
     default: assert(FALSE);
     }
  _pop->showMessage(msg);
}



void TpStatfilePair::setSwitch(long xy_switch)
{
  if(xy_switch == _xy_switch) return;
  _xy_switch = xy_switch;
  adjustSelectedXbin(_xsel, FALSE);
  adjustSelectedYbin(_ysel, FALSE);
  _pop->updatePrevNextSelVectors();
  char msg[100];
  strcpy(msg, "overlay switch changed to\n");
  if(_xy_switch) strcat(msg, "overlay previous, next, and selected X bins");
  else           strcat(msg, "overlay previous, next, and selected Y bins");
  _pop->showMessage(msg);
}



void TpStatfilePair::setStattype(const char *stattype)
{
  _kernal->setStattype(stattype);
}



void TpStatfilePair::setNhx(long nhx)
{
  if(!_allow_header_input) return;
  _kernal->setNhx(nhx);
}



void TpStatfilePair::setNhy(long nhy)
{
  if(!_allow_header_input) return;
  _kernal->setNhy(nhy);
}



void TpStatfilePair::setNhx2(long nhx2)
{
  if(!_allow_header_input) return;
  _kernal->setNhx2(nhx2);
}



void TpStatfilePair::setNhy2(long nhy2)
{
  if(!_allow_header_input) return;
  _kernal->setNhy2(nhy2);
}



void TpStatfilePair::setX1(float x1)
{
  if(!_allow_header_input) return;
  _kernal->setX1(x1);
}



void TpStatfilePair::setY1(float y1)
{
  if(!_allow_header_input) return;
  _kernal->setY1(y1);
}



void TpStatfilePair::setXinc(float xinc)
{
  if(!_allow_header_input) return;
  _kernal->setXinc(xinc);
}



void TpStatfilePair::setYinc(float yinc)
{
  if(!_allow_header_input) return;
  _kernal->setYinc(yinc);
}



void TpStatfilePair::setNx(long nx)
{
  if(!_allow_header_input) return;
  _kernal->setNx(nx);
}



void TpStatfilePair::setNy(long ny)
{
  if(!_allow_header_input) return;
  _kernal->setNy(ny);
}



void TpStatfilePair::setXend(float xend)
{
  if(!_allow_header_input) return;
  _kernal->setXend(xend);
}



void TpStatfilePair::setYend(float yend)
{
  if(!_allow_header_input) return;
  _kernal->setYend(yend);
}



void TpStatfilePair::setXsel(float xsel)
{
  Boolean changed = adjustSelectedXbin(xsel, TRUE);
  if(changed)
       {
       _pop->showMessage("new selected X bin\nchosen");
       if(_xy_switch) _pop->updateSelVector();
       }
}



void TpStatfilePair::setYsel(float ysel)
{
  Boolean changed = adjustSelectedYbin(ysel, TRUE);
  if(changed)
       {
       if(strings_equal("MUTE", (char*)_kernal->getStattype()))
            _pop->showMessage("new selected gather\nchosen");
       else
            _pop->showMessage("new selected Y bin\nchosen");
       if(!_xy_switch) _pop->updateSelVector();
       }
}



void TpStatfilePair::setEncoding(const char *encoding)
{
  strcpy(_encoding, encoding);
}


//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
