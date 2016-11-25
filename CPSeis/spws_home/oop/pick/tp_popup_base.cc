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

//---------------------- tp_popup_base.cc -----------------------//
//---------------------- tp_popup_base.cc -----------------------//
//---------------------- tp_popup_base.cc -----------------------//

//           implementation file for the TpPopupBase class
//                 derived from the StpPopupBase class
//                       subdirectory pick


//    This is a base class for popups which control the picking of
//    seismic traces displayed by SeisPlot, for cases in which each
//    trace has one pick.  The primary purpose of this class is for
//    use with CBYT picking modes such as refraction statics picking,
//    general trace picking, fish picking, sisc picking, and maybe
//    mute picking.

//    Up to five types of picks are displayed using up to six vectors:
//    The minimum number of vectors is TP_CURR          (one).
//    The maximum number of vectors is TP_MAXNUMVECTORS (currently six).
//    The actual  number of vectors is _numvectors      (set by constructor).

//    All but the first vector are considered to be overlays.
//    The first  vector (index 0) displays current  picks.
//    The second vector (index 1) displays original picks.
//    The third  vector (index 2) displays previous picks or integrated picks.
//    The fourth vector (index 3) displays next     picks.
//    The fifth  vector (index 4) displays selected picks.
//    The sixth  vector (index 5) displays a reference file of picks.

//    For each class derived from this class, you must also derive
//    a class from TpPairBase.  In that latter class, you must override
//    several virtual functions to be used to validate data files, open
//    and close data files, and move picks to or from the data files.

//    Classes derived from this class are to complete the popup by
//    adding an object derived from TpPairBase to the SLSmartForm
//    object _work1, and by adding any required SLDelay children
//    to the SLSmartForm objects _work2 and _work3.  These should
//    be done in the constructor.

//    Several helper classes are used by this class.  This class,
//    and the associated helper classes, have names beginning with
//    the letters Tp... (for "trace picking").  The helper classes
//    do not have to be overridden.

//    There are several virtual functions which may optionally be
//    overridden in a derived class.


#include "pick/tp_popup_base.hh"
#include "pick/tp_reference_base.hh"
#include "pick/hurst.hh"
#include "sp/seis_plot.hh"
#include "pick/tp_pair_base.hh"
#include "pick/tp_vectors.hh"
#include "pick/tp_message_gui.hh"
#include "pick/tp_working_gui.hh"
#include "pick/seis_shift.hh"
#include "sl/shell_watch.hh"
#include "plot/pick_watch.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_text.hh"
#include "tfdefs.h"
#include "image_amplitude_recovery.hh"
#include "cprim.h"
#include "named_constants.h"
#include <stream.h>
#include <iostream.h>
#include <assert.h>


#define WANT_UPDATE_BUTTON   TRUE
#define WANT_KEYHELP_BUTTON  FALSE

#define SHIFT_ZERO 999 // must be different from MANUAL, AUTOMATIC, ZERO, SNAP.


//--------------- reiteration of constants -----------------------//
//--------------- reiteration of constants -----------------------//
//--------------- reiteration of constants -----------------------//


//  Defined constants in tp_resources.hh:
//
//              -1       0        1        2        3        4        5
//    _snap = TP_NONE, TP_CURR, TP_ORIG, TP_PREV, TP_NEXT, TP_SEL, TP_REF
//                             (but not to exceed _numvectors-1)
//
//                                                6
//    maximum allowed number of vectors = TP_MAXNUMVECTORS.
//
//                                   0        1        2        3        4
//    ivector (index of vector) = TP_CURR, TP_ORIG, TP_PREV, TP_NEXT, TP_SEL
//                                   5
//                                 TP_REF.
//                             (but not to exceed _numvectors-1)


//  Defined constants in cprim.h (used by derive_picks):
//
//    _pickmode =  PEAK, TROUGH, POSITIVE, NEGATIVE.
//
//    _automode =  FOLLOW_LINE, FOLLOW_SLOPE, FOLLOW_CURVE,
//                     FIRST_BREAK, FIRST_BREAK_NO_SNAP, FIRST_BREAK_CORR
//                     HURST_BREAK, HURST_BREAK_NO_SNAP, HURST_CORR,
//                     COMBO, COMBO_CORR, PICK_CORR, CORRELATE.
//
//    action =  MANUAL, AUTOMATIC, ZERO, SNAP.
//                  plus SHIFT_ZERO defined above.



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


TpPopupBase::TpPopupBase(SLDelay *slparent, char *name, HelpCtx hctx,
                            SeisPlot          *sp,
                            const long         numvectors,
                            const float        zero_pick,
                            const float        missing_pick,
                            const Boolean      allow_snap,
                            const Boolean      allow_auto,
                            const char * const picking_mode,
                            const char * const help_token,
                            const char * const help_fallback)
            : StpPopupBase(slparent, name, hctx,
                           sp, WANT_UPDATE_BUTTON, WANT_KEYHELP_BUTTON,
                           picking_mode, help_token, help_fallback),
                   _pair           (NULL),
                   _vectors        (NULL),
                   _message        (NULL),
                   _working        (NULL),
                   _seis_shift     (NULL),
                   _work1          (NULL),
                   _work2          (NULL),
                   _work3          (NULL),
                   _work4          (NULL),

                   _numvectors     (numvectors),
                   _zero_pick      (zero_pick),
                   _missing_pick   (missing_pick),
                   _allow_snap     (allow_snap),
                   _allow_auto     (allow_auto),

                   _auto_direction (0),
                   _update_overlays_after_picking (TRUE),

                   _show_overlays  (TRUE),
                   _snap           (TP_NONE),
                   _pickmode       (PEAK),
                   _automode       (FOLLOW_LINE),
                   _threshhold     (0.10),
                   _sigDiff        (3.00),
                   _orf            (1.00),
                   _shortMin       (0.00),
                   _shortMax       (0.00),
                    _longMin       (0.00),
                    _longMax       (0.00),
                   _poto           (0.00),
                   _outlier        (2.00),
                   _autosnap       (FALSE),
                   _autobreak      (FALSE),
                   _auto_mute      (0),
                   _mute_mode      (0),
                   _velocity       (5000.0F),
                   _external_function(NULL),
                   _external_object(NULL)
{
  assert(_numvectors >= TP_CURR && _numvectors <= TP_MAXNUMVECTORS);
  for(int ivector = TP_CURR; ivector < TP_MAXNUMVECTORS; ivector++)
       {
       _show[ivector] = (ivector == TP_CURR || ivector == TP_PREV);
       }
  TpResources::startup(this);
  setPickingColorName(TpResources::getColorRubber());
  setPickingCursor   (TpResources::getCursor());
  setPickingLineWidth(TpResources::getLineWidth());

  SLSmartForm *work = workArea();

  _message = new TpMessageGui(work, "message");
  _working = new TpWorkingGui(work, "working");
  _work1   = new SLSmartForm (work, "work1");
  _work2   = new SLSmartForm (work, "work2");
  _work3   = new SLSmartForm (work, "work3");
  _work4   = new SLSmartForm (work, "work4");

  work->attach(_work1  ,  work, work,  work   , NULL);
  work->attach(_message,  work, work, _work1  , NULL);
  work->attach(_working,  work, work, _message, NULL);
  work->attach(_work2  ,  work, NULL, _working, _work4, 10,  0,  5, 20);
  work->attach(_work3  ,_work2, work, _working, _work4, 20, 10,  5, 20);
  work->attach(_work4  ,  work, work,  NULL   ,   work, 20, 10,  5,  0);

  showBlank();

  set_hurst_func(&hurst_func);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//

TpPopupBase::~TpPopupBase()
{
  stopPicking();
}



//---------------------- set seis shift ------------------------//
//---------------------- set seis shift ------------------------//
//---------------------- set seis shift ------------------------//

void TpPopupBase::setSeisShift(SeisShift *seis_shift)
{
  _seis_shift = seis_shift; 
}



//----------------------- get file pair plus --------------------//
//----------------------- get file pair plus --------------------//
//----------------------- get file pair plus --------------------//

                 // overriding virtual function

SLFilePairPlus *TpPopupBase::getFilePairPlus()
{
  return _pair;
}



//---------------------- verify picking action --------------------//
//---------------------- verify picking action --------------------//
//---------------------- verify picking action --------------------//

                 // overriding virtual function

Boolean TpPopupBase::verifyPickingAction(int button,
                                PickBase::Modifier modifier,
                                PickBase::Action action)
{
  long action2 = getPickingAction(button, modifier);
  if(action2 == -999) return FALSE;
  char post[20];
  switch(action)
     {
     case PickBase::press  : strcpy(post, "started"    ); break;
     case PickBase::motion : strcpy(post, "in progress"); break;
     case PickBase::release: strcpy(post, "completed"  ); break;
     default               : assert(FALSE);
     }
  if(action2 == SHIFT_ZERO) action2 = ZERO;
  _message->showActionMessage(action2, _pickmode, _automode,
                                                   _snap, post);
  return TRUE;
}



//------------------ picking action completed ---------------//
//------------------ picking action completed ---------------//
//------------------ picking action completed ---------------//

                 // overriding virtual function

void TpPopupBase::pickingActionCompleted(SeisPlot *sp,
                             int button,
                             PickBase::Modifier modifier,
                             long direction,
                             long first_trace, long last_trace,
                             float first_time, float last_time)
{
  long action2 = getPickingAction(button, modifier);
  if(action2 == -999) return;
  modifyCurrentPicks(sp, action2, direction,
           first_trace, last_trace, first_time, last_time);
  if(_update_overlays_after_picking)
      {
      updateCurrVectorNow(sp, FALSE);
      updatePrevVectorNow(sp);
      updateNextVectorNow(sp);
      updateSelVectorNow (sp);
      }
}

// note:  modifyCurrentPicks updates the current vector over the
//  picked range only (by passing the picks to the data file, and
//  then getting them back from the data file).  updateCurrVectorNow
//  updates the current vector over the full range of the traces
//  in the display.  Ditto the other updates.


//--------------------- get picking action ------------------------//
//--------------------- get picking action ------------------------//
//--------------------- get picking action ------------------------//

        // If the picking action is to be aborted, this function
        // returns -999, and might show an error message.

long TpPopupBase::getPickingAction(int button, 
                                    PickBase::Modifier modifier)
{
  if(!_vectors->getVectorVisibility(TP_CURR))
     {
     showError
        ("picking action attempted\n without displaying current picks");
     return -999;
     }
  long action = -999;
  if     (button == 1 && modifier == PickBase::none)
                                        action = AUTOMATIC;
  else if(button == 1 && modifier == PickBase::shft)
                                        action = MANUAL;
  else if(button == 1 && modifier == PickBase::cntl && _allow_snap)
                                        action = SNAP;
  else if(button == 2 && modifier == PickBase::none)
                                        action = ZERO;
  else if(button == 2 && modifier == PickBase::shft)
                                        action = SHIFT_ZERO;
  else if(button == 3) return -999;
  if(action == AUTOMATIC && !_allow_auto) action = MANUAL;
  if(action == AUTOMATIC && _mute_mode && !_auto_mute) action = MANUAL;
  if(action == -999)
     {
     showError("unknown picking action\n attempted");
     return -999;
     }
  if(action == SNAP)
     {
     if(_snap == TP_NONE)
        {
        showError
           ("SNAP picking action attempted\n with nothing to snap to");
        return -999;
        }
     if(!_vectors->getVectorVisibility(_snap))
        {
        showError
  ("SNAP picking action attempted\n without displaying picks to snap to");
        return -999;
        }
     }
  return action;
}



//----------------------- prepare to snap --------------------------//
//----------------------- prepare to snap --------------------------//
//----------------------- prepare to snap --------------------------//

           // private - called from modifyCurrentPicks

   // replace current picks by picks to snap to.
   // returns error = TRUE if the snap should not be performed.
   // error occurs if the current picks are not visible.
   // error occurs if the the picks to snap to are not visible.

   // index  = the index in the vector data which corresponds
   //               to the first index in the picks array.
   // npicks = the number of elements in the picks array.

Boolean TpPopupBase::prepareToSnap(float *picks, long index, long npicks)
{
  if(_snap < TP_CURR || _snap >= _numvectors) return TRUE; // invalid _snap.
  if(!_vectors->getVectorVisibility(TP_CURR)) return TRUE; // picks not visible.
  if(!_vectors->getVectorVisibility(_snap  )) return TRUE; // picks not visible.
  long i;
  if(_snap != TP_ORIG)
     {
     long big = npicks + 999;
     for(i = 0; i < npicks; i++)
       {
       if(picks[i] != _missing_pick)
         {
         float pick = _vectors->getSinglePick(_snap, index + i);
         if(pick != _missing_pick && pick != _zero_pick) i = big;
         }
       }
     if(i < big) return TRUE;  // all picks to snap to are zero or missing.
     }
  for(i = 0; i < npicks; i++)
     {
     if(picks[i] != _missing_pick)
       {
       float pick = _vectors->getSinglePick(_snap, index + i);
       if(pick == _missing_pick) pick = _zero_pick;
       picks[i] = pick;
       }
     }
  return FALSE;
}



//------------------ modify current picks ---------------------//
//------------------ modify current picks ---------------------//
//------------------ modify current picks ---------------------//

    // called from pickingActionCompleted after user releases button.
    // called from updateVectors for automatic action when scanning.

void TpPopupBase::modifyCurrentPicks(SeisPlot *sp,
                      long action, long direction,
                      long first_trace, long last_trace,
                      float first_time, float last_time)
{
unsigned char *bytes = NULL;
long i, hindex;


  if(   action != MANUAL       &&    action != AUTOMATIC &&
        action != ZERO         &&    action != SNAP &&
        action != SHIFT_ZERO) return;
  if(_pickmode != PEAK         && _pickmode != TROUGH &&
     _pickmode != POSITIVE     && _pickmode != NEGATIVE) return;
  if(_automode != FOLLOW_LINE  && _automode != FOLLOW_SLOPE &&
     _automode != FOLLOW_CURVE &&
     _automode != FIRST_BREAK  && _automode != FIRST_BREAK_NO_SNAP &&
     _automode != FIRST_BREAK_CORR &&
     _automode != HURST_BREAK  && _automode != HURST_BREAK_NO_SNAP &&
     _automode != HURST_CORR   &&
     _automode != COMBO        && _automode != COMBO_CORR &&
     _automode != PICK_CORR    && _automode != CORRELATE) return;

  if(!sp || !sp->imageIsDisplayed()) return;

  float               tmincur = sp->plottedTmin();
  float               tmaxcur = sp->plottedTmax();
  long                  nsamp = sp->samplesPerTrace();
  float                  tmin = sp->memTmin();
  float                    dt = sp->sampleRate();
  long                      n = sp->displayedTraces(sp->currentFrame());
  const float           *head = sp->firstMemoryHeaderData();
  const float       *all_head = head;
  long                 nwords = sp->numHeaders();



  if(n == 0) return;
  assert(n == _vectors->getNumPicks());
  if(last_trace == 0) last_trace = n;
  first_trace = ConstrainValue(first_trace, 1, n);
  last_trace  = ConstrainValue( last_trace, 1, n);
  if(last_trace < first_trace) return;


  long index  = first_trace - 1;
  long npicks = last_trace - first_trace + 1;
  float *picks = new float[npicks];
  _vectors->getSomePicks(TP_CURR, picks, index, npicks);
  if(action == SNAP)
       {
       Boolean error = prepareToSnap(picks, index, npicks);
       if(error) 
         { 
         delete [] picks; 
         return;
         }
       }
  //Had to change this method in order to support float data. The trace
  //array it did use was always of byte type and is passed to too many
  //methods to rewrite as float so I am doing the easy but slow way
  //of converting the floats to bytes as necessary. M.L.Sherrill 12/00
  const unsigned char *traces;
  const unsigned char *all_traces;
  if(sp->isByteData())
    {
    traces = sp->firstMemoryByteTraceData();
    all_traces = traces;
    }
  else
    {
    bytes = (unsigned char *)malloc((sp->samplesPerTrace() *
                             sp->displayedTraces(sp->currentFrame()))    *
                                                   sizeof(unsigned char));
    if(bytes == NULL)
      {
      printf("Not enough memory in TpPopupBase::modifyCurrentPicks\n");
      return;
      }
    float *tr = (float *)sp->firstMemoryFloatTraceData();

    //Used to convert float data to bytes using the lav of the file but
    //we had a case where the user was using an external amplitude to make
    //first breaks show up and the lav of the file was so huge that these
    //first breaks were converted to 0 going to byte precision and of
    //course the picking routines would skip those points. Tom and I
    //decided it is ok to scale the data as the user has the display
    //scaled and pass those bytes to the picking. M.L.Sherrill 09/01

    //float lav = sp->maxDataAmp();
    //if(lav == 0.0)//segy
    //   lav = sp->getAmplitudeRecovery()->getScaleAmp(sp->currentFrame());

    float lav = sp->getAmplitudeRecovery()->getScaleAmp(sp->currentFrame());
    int   nsamp = (int)sp->samplesPerTrace();

    for(int i = 0; i < sp->displayedTraces(sp->currentFrame()); i++)
      float_to_byt_(&tr[i * nsamp], &nsamp,  &bytes[i * nsamp], &nsamp, &lav);

    traces = bytes;
    all_traces = traces;
    }
  
  head   += index * nwords;
  traces += index * nsamp;
  

 
  long action2 = action;
  if(action == SHIFT_ZERO) action2 = ZERO;

  float *all_picks = new float[n];
  _vectors->getSomePicks(TP_CURR, all_picks, 0, n);

  derive_picks_spws(action2, _pickmode, _automode,
       direction, _threshhold, _sigDiff, 1, npicks,
       npicks, first_time, last_time,
       tmincur, tmaxcur, picks, _missing_pick, _zero_pick,
       (unsigned char*)traces, nsamp, tmin, dt, NULL, head, nwords,
       (unsigned char*)all_traces, n, all_picks, all_head, index,
       _orf, _shortMin, _shortMax, _longMin, _longMax, _poto, _outlier);

  if (action == AUTOMATIC
     && (_automode == CORRELATE || (_automode == PICK_CORR && npicks == 1)))
  {
    /*
     * ehs 19dec96
     */
    shiftPicksFromDisplayToFile (all_picks, 0, n);

    //If we are doing automatic picking of mutes we need to loop thru
    //the picks and save them one by one. If we did not do this we would
    //miss all of the points in between a first and last pick since
    //mute saving only saves the first and last pick for normal mute picking.
    //Note that we will be getting some pretty large mute files in the
    //auto mute mode. M.L.Sherrill 01-2002
    if(_auto_mute)
      {
      for(i = 0; i < n; i++)
        {
        hindex = i * nwords;
        _pair->doSaveCurrentPicks(&all_picks[i], &all_head[hindex], nwords, n,
                                  action);
        }
      }
    else
      {
      _pair->doSaveCurrentPicks   (all_picks, all_head, nwords, n, action);
      }
    _pair->doReadCurrentPicks   (all_picks, all_head, nwords, n);
    shiftPicksFromFileToDisplay (all_picks, 0, n);
    _vectors->replaceSomePicks(TP_CURR, all_picks, 0, n);
  }
  else
  {
    shiftPicksFromDisplayToFile (picks, index, npicks);

    //If we are doing automatic picking of mutes we need to loop thru
    //the picks and save them one by one. If we did not do this we would
    //miss all of the points in between a first and last pick since
    //mute saving only saves the first and last pick for normal mute picking.
    //Note that we will be getting some pretty large mute files in the
    //auto mute mode. M.L.Sherrill 01-2002
    if(_auto_mute)
      {
      for(i = 0; i < npicks; i++)
        {
        hindex = i * nwords;
        _pair->doSaveCurrentPicks(&picks[i], &head[hindex], nwords, 1, action);
        }
      }
    else
      {
      _pair->doSaveCurrentPicks   (picks, head, nwords, npicks, action);
      }
    _pair->doReadCurrentPicks   (picks, head, nwords, npicks); // see note
    shiftPicksFromFileToDisplay (picks, index, npicks);        // see note
    _vectors->replaceSomePicks(TP_CURR, picks, index, npicks); // see note
  }
  delete [] picks;
  delete [] all_picks;
  if(bytes != NULL) free (bytes);
}

// note:  The three lines marked above are not necessary if
//   modifyCurrentPicks is followed by updateCurrVectorNow.
//   However, inclusion of this code here makes it possible
//   to skip the call to updateCurrVectorNow to improve
//   performance if the display contains thousands of traces.
//   However, heed this warning:  If picks within the range
//   from first_trace to last_trace cause a change in the
//   picks outside that range, the change will not show up
//   unless updateCurrVectorNow is called.



//--------------------- create vectors -----------------------//
//--------------------- create vectors -----------------------//
//--------------------- create vectors -----------------------//

                 // overriding virtual function

SeisVectLinkedList *TpPopupBase::createVectors(SeisPlot *sp)
{
  _vectors = new TpVectors(sp, (int)_numvectors, _zero_pick, _missing_pick);
  for(int ivector = TP_CURR; ivector < _numvectors; ivector++)
       {
       if(ivector == TP_CURR || _show_overlays)
            _vectors->setVectorVisibility(ivector, _show[ivector]);
       }

  if(!_pair->fileIsReadOnly())
       {
       showMessage("READ-ONLY MODE\n all picking actions are disabled");
       }
  else
       {
       showMessage("UPDATE MODE\n picking is enabled");
       }
  return _vectors;
}



//--------------------- delete vectors -----------------------//
//--------------------- delete vectors -----------------------//
//--------------------- delete vectors -----------------------//

                 // overriding virtual function

void TpPopupBase::deleteVectors()
{
  delete _vectors;
  _vectors = NULL;
  showBlank();
}



//---------------------- update vectors ----------------------//
//---------------------- update vectors ----------------------//
//---------------------- update vectors ----------------------//

                    // overriding virtual function

     // possible value of why:  _UPDATE_REQUESTED
     // possible value of why:  _STARTING
     // possible value of why:  _NEW_DISPLAY
     // possible value of why:  _LEFT
     // possible value of why:  _RIGHT

void TpPopupBase::updateVectors(SeisPlot *sp, Why why)
{
  updateCurrVectorNow(sp, TRUE);
  updatePrevVectorNow(sp);
  updateNextVectorNow(sp);
  updateSelVectorNow (sp);
  long action = getAutomaticAction(why);
  if(action == SNAP || action == AUTOMATIC)
      {
      modifyCurrentPicks(sp, action, FORWARD, 0, 0, 0.0, 0.0);
      //// updateCurrVectorNow(sp, FALSE);  // not needed here
      updateFile(sp, why);         // added 2/25/97
      }
//updatePrevVectorNow(sp);   // moved to above spot 4/24/96
//updateNextVectorNow(sp);   // moved to above spot 4/24/96
//updateSelVectorNow (sp);   // moved to above spot 4/24/96
  if(action == SNAP || action == AUTOMATIC)
      {
      _message->showActionMessage(action, _pickmode, _automode, _snap,
             "performed while scanning");
      }
  else if(why == _UPDATE_REQUESTED)
      {
      showMessage("update\nperformed");
      }
  else if(action != -999)
      {
      showMessage("new\ndisplay");
      }
}



//---------------- turn off auto action ---------------------//
//---------------- turn off auto action ---------------------//
//---------------- turn off auto action ---------------------//


long TpPopupBase::turnOffAutoAction()
{
  _auto_direction = 0;
  if(_autosnap)
       {
       showError("automatic snapping action\n turned off");
       _autosnap = FALSE;
       return -999;
       }
  if(_autobreak)
       {
       showError
         ("automatic first break picking while scanning\n turned off");
       _autobreak = FALSE;
       return -999;
       }
  return -888;
}



//------------------- get automatic action ---------------------//
//------------------- get automatic action ---------------------//
//------------------- get automatic action ---------------------//

     // returns SNAP if snapping action should occur.
     // returns AUTOMATIC if automatic picking action should occur.
     // returns -999 if automatic action has just now been turned off.
     // returns -888 otherwise.

     // possible value of why:  _UPDATE_REQUESTED
     // possible value of why:  _STARTING
     // possible value of why:  _NEW_DISPLAY
     // possible value of why:  _LEFT
     // possible value of why:  _RIGHT

long TpPopupBase::getAutomaticAction(Why why)
{
  if(why == _UPDATE_REQUESTED) return -888;
  if(why == _STARTING) _auto_direction = 0;
  assert(_pair);
  if(_pair->fileIsReadOnly())
       {
       return turnOffAutoAction();
       }
  if(_autosnap && _autobreak)
       {
       assert(FALSE);
       }
  if(!_autosnap && !_autobreak)
       {
       return turnOffAutoAction();
       }
  if(!_vectors->getVectorVisibility(TP_CURR))
       {
       return turnOffAutoAction();
       }
  if(why != _STARTING && why != _LEFT && why != _RIGHT)
       {
       return turnOffAutoAction();
       }
  if(_auto_direction > 0 && why != _RIGHT)
       {
       return turnOffAutoAction();
       }
  if(_auto_direction < 0 && why != _LEFT)
       {
       return turnOffAutoAction();
       }
  if(_autosnap && _snap == TP_NONE)
       {
       return turnOffAutoAction();
       }
  if(_autosnap && _snap != TP_NONE && !_vectors->getVectorVisibility(_snap))
       {
       return turnOffAutoAction();
       }
  if(_autobreak && _automode != FIRST_BREAK &&
                   _automode != FIRST_BREAK_NO_SNAP &&
                   _automode != FIRST_BREAK_CORR &&
                   _automode != HURST_BREAK &&
                   _automode != HURST_BREAK_NO_SNAP &&
                   _automode != HURST_CORR &&
                   _automode != COMBO &&
                   _automode != COMBO_CORR)
       {
       return turnOffAutoAction();
       }
  if(why == _LEFT ) _auto_direction = -1;
  if(why == _RIGHT) _auto_direction =  1;
  if(_autosnap ) return SNAP;
  if(_autobreak) return AUTOMATIC;
  return -888;
}



//---------------------- update curr vector now ------------------//
//---------------------- update curr vector now ------------------//
//---------------------- update curr vector now ------------------//


void TpPopupBase::updateCurrVectorNow(SeisPlot *sp, Boolean orig_also)
{
  showWorking("reading current picks...");
  assert(sp && _pair && _pair->fileIsLoaded() && _vectors);
  long            n = sp->displayedTraces(sp->currentFrame());
  const float *head = sp->firstMemoryHeaderData();
  long       nwords = sp->numHeaders();
  float *picks = NULL;

  if(n > 0)
      {
      picks = new float[n];
      _pair->doReadCurrentPicks  (picks, head, nwords, n);
      shiftPicksFromFileToDisplay(picks, 0, n);
      }
  _vectors->replaceAllPicks(TP_CURR, picks, n);

  if(_numvectors > TP_ORIG  && orig_also)
      {
      _vectors->replaceAllPicks(TP_ORIG, picks, n);
      }

  if(_ref && _ref->fileIsLoaded())
    {
    if(n > 0)
      {
      _ref->doReadCurrentPicks  (picks, head, nwords, n);
      shiftPicksFromFileToDisplay(picks, 0, n);
      }
    _vectors->replaceAllPicks(TP_REF, picks, n);
    }

  if(n > 0) delete [] picks;
  showWorking(" ");
}



//---------------------- update prev vector now ------------------//
//---------------------- update prev vector now ------------------//
//---------------------- update prev vector now ------------------//


void TpPopupBase::updatePrevVectorNow(SeisPlot *sp)
{
  if(_numvectors <= TP_PREV) return;
  showWorking("reading picks for previous overlay...");
  assert(sp && _pair && _pair->fileIsLoaded() && _vectors);
  long            n = sp->displayedTraces(sp->currentFrame());
  const float *head = sp->firstMemoryHeaderData();
  long       nwords = sp->numHeaders();
  assert(n == _vectors->getNumPicks());
  float *picks = NULL;

  if(n > 0)
      {
      picks = new float[n];
      _pair->doReadPreviousPicks (picks, head, nwords, n);
      shiftPicksFromFileToDisplay(picks, 0, n);
      }
  _vectors->replaceAllPicks(TP_PREV, picks, n);

  if(_ref && _ref->fileIsLoaded())
    {
    if(n > 0)
      {
      _ref->doReadPreviousPicks  (picks, head, nwords, n);
      shiftPicksFromFileToDisplay(picks, 0, n);
      }
    _vectors->replaceAllPicks(TP_REF, picks, n);
    }


  if(n > 0) delete [] picks;
  showWorking(" ");
}




//---------------------- update next vector now ------------------//
//---------------------- update next vector now ------------------//
//---------------------- update next vector now ------------------//


void TpPopupBase::updateNextVectorNow(SeisPlot *sp)
{
  if(_numvectors <= TP_NEXT) return;
  showWorking("reading picks for next overlay...");
  assert(sp && _pair && _pair->fileIsLoaded() && _vectors);
  long            n = sp->displayedTraces(sp->currentFrame());
  const float *head = sp->firstMemoryHeaderData();
  long       nwords = sp->numHeaders();
  assert(n == _vectors->getNumPicks());
  float *picks = NULL;

  if(n > 0)
      {
      picks = new float[n];
      _pair->doReadNextPicks (picks, head, nwords, n);
      shiftPicksFromFileToDisplay(picks, 0, n);
      }
  _vectors->replaceAllPicks(TP_NEXT, picks, n);

  if(_ref && _ref->fileIsLoaded())
    {
    if(n > 0)
      {
      _ref->doReadNextPicks  (picks, head, nwords, n);
      shiftPicksFromFileToDisplay(picks, 0, n);
      }
    _vectors->replaceAllPicks(TP_REF, picks, n);
    } 

  if(n > 0) delete [] picks;
  showWorking(" ");
}




//---------------------- update sel vector now ------------------//
//---------------------- update sel vector now ------------------//
//---------------------- update sel vector now ------------------//


void TpPopupBase::updateSelVectorNow(SeisPlot *sp)
{
  if(_numvectors <= TP_SEL) return;
  showWorking("reading picks for selected overlay...");
  assert(sp && _pair && _pair->fileIsLoaded() && _vectors);
  long            n = sp->displayedTraces(sp->currentFrame());
  const float *head = sp->firstMemoryHeaderData();
  long       nwords = sp->numHeaders();
  assert(n == _vectors->getNumPicks());
  float *picks = NULL;
  if(n > 0)
      {
      picks = new float[n];
      _pair->doReadSelectedPicks (picks, head, nwords, n);
      shiftPicksFromFileToDisplay(picks, 0, n);
      }
  _vectors->replaceAllPicks(TP_SEL, picks, n);
  if(n > 0) delete [] picks;
  showWorking(" ");
}



//----------------- update overlays -------------------------//
//----------------- update overlays -------------------------//
//----------------- update overlays -------------------------//


void TpPopupBase::updateAllVectors()   // public
{
  if(!pickingInProgress()) return;
  holdAllVectors();
  updateCurrVectorNow(getSeisPlot(), FALSE);
  updatePrevVectorNow(getSeisPlot());
  updateNextVectorNow(getSeisPlot());
  updateSelVectorNow (getSeisPlot());
  flushAllVectors();
}


void TpPopupBase::updatePrevNextSelVectors()   // public
{
  if(!pickingInProgress()) return;
  holdAllVectors();
  updatePrevVectorNow(getSeisPlot());
  updateNextVectorNow(getSeisPlot());
  updateSelVectorNow (getSeisPlot());
  flushAllVectors();
}


void TpPopupBase::updateSelVector()           // public
{
  if(!pickingInProgress()) return;
  updateSelVectorNow (getSeisPlot());
}



//----------------------- update file -----------------------//
//----------------------- update file -----------------------//
//----------------------- update file -----------------------//

                  // overriding virtual function

     // possible value of why:  _UPDATE_REQUESTED
     // possible value of why:  _STOPPING
     // possible value of why:  _END_DISPLAY
     // possible value of why:  _NEW_DISPLAY_COMING

void TpPopupBase::updateFile(SeisPlot *, Why /*why*/) 
{
  showWorking("saving picks to file...");
  _pair->doUpdateFile();
  showMessage("picks saved\nto file");
  showWorking(" ");
}



//---------------------- shift picks ---------------------------//
//---------------------- shift picks ---------------------------//
//---------------------- shift picks ---------------------------//

   // index  = the index in the data within SeisShift (this would
   //            be the same as in SeisPlot memory traces) which
   //            corresponds to the first index in the picks array.
   // npicks = the number of elements in the picks array.

         // direction must be plus or minus one.
         // getShift takes trace number (not index) as an argument.

void TpPopupBase::shiftPicksHelper(float *picks,
                          long index, long npicks, int direction) const
{
  if(!_seis_shift || !_seis_shift->dataShifted()) return;
  int factor;
  if     (_seis_shift->forwardApplied()) factor =  direction;
  else if(_seis_shift->reverseApplied()) factor = -direction;
  else return;
  for(int i = 0; i < npicks; i++)
       {
       if(picks[i] != _zero_pick && picks[i] != _missing_pick)
            {
            picks[i] += factor * _seis_shift->getShift(index + i + 1);
            if(picks[i] == _zero_pick   ) picks[i] += factor * 0.001;
            if(picks[i] == _missing_pick) picks[i] += factor * 0.001;
            if(picks[i] == _zero_pick   ) picks[i] += factor * 0.001;
            }
       }
}



void TpPopupBase::shiftPicksFromFileToDisplay(float *picks,
                                     long index, long npicks) const
{
  shiftPicksHelper(picks, index, npicks, 1);
}



void TpPopupBase::shiftPicksFromDisplayToFile(float *picks,
                                     long index, long npicks) const
{
  shiftPicksHelper(picks, index, npicks, -1);
}



Boolean TpPopupBase::haveSeisShift()  const
{
  return (_seis_shift != NULL);
}



Boolean TpPopupBase::dataIsShifted()  const
{
  return (_seis_shift && _seis_shift->dataShifted());
}



//------------- set special zero value coordinate --------------//
//------------- set special zero value coordinate --------------//
//------------- set special zero value coordinate --------------//

void TpPopupBase::setSpecialZeroValueCoordinate(float coord)
{
  if(_vectors) _vectors->setSpecialZeroValueAndCoord(_zero_pick, coord);
}



//----------------- flatten and unflatten -----------------------//
//----------------- flatten and unflatten -----------------------//
//----------------- flatten and unflatten -----------------------//

void TpPopupBase::flattenToVelocity(float velocity)
{
  if(!_seis_shift) return;
  SeisPlot *sp = getSeisPlot();
  long n = sp->displayedTraces(sp->currentFrame());
  if(n == 0) return;
  ShellWatch watch1;
  PickWatch  watch2;
  float save_time     = _seis_shift->getFlattenTime();
  float save_velocity = _seis_shift->getVelocity();
  int   save_header   = _seis_shift->getHeader();
  float tmincur       = sp->plottedTmin();
  _seis_shift->setFlattenTime(tmincur + 0.2);
  if(velocity != 0.0F) 
    _seis_shift->changeVelocity(velocity);
  else//this method being called by an external class
    _seis_shift->changeVelocity(_velocity);
  _seis_shift->changeHeader  (6);
  _seis_shift->linearShift(FALSE);
  _seis_shift->setFlattenTime(save_time);
  _seis_shift->changeVelocity(save_velocity);
  _seis_shift->changeHeader  (save_header);
  char msg[80];
  sprintf(msg, "traces\nflattened to linear velocity %.0f", velocity);
  showMessage(msg);
  if(_external_function != NULL) _external_function(FLATTEN_VELOCITY, 
                                                    _external_object);

}



void TpPopupBase::flattenPicks()
{
  if(!_seis_shift || !pickingInProgress()) return;
  SeisPlot *sp = getSeisPlot();
  long n = sp->displayedTraces(sp->currentFrame());
  if(n == 0) return;
  assert(n == _vectors->getNumPicks());
  ShellWatch watch1;
  PickWatch  watch2;
  float *picks = new float[n];
  _vectors->getAllPicks(TP_CURR, picks);
  shiftPicksFromDisplayToFile(picks, 0, n);
  for(int i = 0; i < n; i++)
       {
       if(picks[i] == _zero_pick || picks[i] == _missing_pick)
             picks[i] = 0.0;
       }
  float tmincur = sp->plottedTmin();
  float save_time = _seis_shift->getFlattenTime();
  _seis_shift->setFlattenTime(tmincur + 0.2);
  _seis_shift->nonlinearShift(FALSE, picks);
  delete [] picks;
  _seis_shift->setFlattenTime(save_time);
  showMessage("traces\nflattened to pick times");
  if(_external_function != NULL) _external_function(FLATTEN_PICKS,
                                                    _external_object);
}




void TpPopupBase::unflatten()
{
  if(!_seis_shift) return;
  ShellWatch watch1;
  PickWatch  watch2;
  _seis_shift->removeShift();
  showError("traces\nunflattened");
  if(_external_function != NULL) _external_function(UNFLATTEN, 
                                                    _external_object);
}


//-------------- access to GUI variables -----------------------//
//-------------- access to GUI variables -----------------------//
//-------------- access to GUI variables -----------------------//

void TpPopupBase::showBlank  ()                { _message->showBlank  ()   ; }
void TpPopupBase::showMessage(const char *msg) { _message->showMessage(msg); }
void TpPopupBase::showError  (const char *msg) { _message->showError  (msg); }
void TpPopupBase::showWorking(const char *msg)
                                      { _working->setCvar((char*)msg); }


void TpPopupBase::updateOverlaysAfterPicking
              (Boolean update_overlays_after_picking)
{
  if(update_overlays_after_picking == _update_overlays_after_picking) return;
  _update_overlays_after_picking = update_overlays_after_picking;
  if(_update_overlays_after_picking && dataIsChanged())
      {
      updatePrevNextSelVectors();
      }
}



void TpPopupBase::showOverlays()
{
  _show_overlays = TRUE;
  showMessage("overlays\nturned on");
  if(!_vectors) return;
  for(int ivector = (int)_numvectors - 1; ivector >= TP_ORIG; ivector--)
       {
       if(_show[ivector]) _vectors->setVectorVisibility(ivector, TRUE);
       }
  if(_show[TP_CURR]) _vectors->redrawVector(TP_CURR);
}


void TpPopupBase::hideOverlays()
{
  _show_overlays = FALSE;
  showError("overlays\nturned off");
  if(!_vectors) return;
  holdAllVectors();
  for(int ivector = TP_ORIG; ivector < _numvectors; ivector++)
       {
       _vectors->setVectorVisibility(ivector, FALSE);
       }
  flushAllVectors();
}


void TpPopupBase::showVector(int ivector)
{
  if(ivector < TP_CURR || ivector >= _numvectors) return;
  if(_show[ivector]) return;
  _show[ivector] = TRUE;
  char msg[100];
  switch(ivector)
     {
     case TP_CURR: strcpy(msg, "display of current picks"   ); break;
     case TP_ORIG: strcpy(msg, "overlay of original picks"  ); break;
     case TP_PREV: strcpy(msg, "overlay of previous profile"); break;
     case TP_NEXT: strcpy(msg, "overlay of next profile"    ); break;
     case TP_SEL : strcpy(msg, "overlay of selected profile"); break;
     case TP_REF : strcpy(msg, "overlay of reference file"  ); break;
     default: assert(FALSE);
     }
  strcat(msg, "\nturned on");
  showMessage(msg);
  if(!_vectors) return;
  if(_show_overlays || ivector == TP_CURR)
       _vectors->setVectorVisibility(ivector, TRUE);
}


void TpPopupBase::hideVector(int ivector)
{
  if(ivector < TP_CURR || ivector >= _numvectors) return;
  if(!_show[ivector]) return;
  _show[ivector] = FALSE;
  char msg[100];
  switch(ivector)
     {
     case TP_CURR: strcpy(msg, "display of current picks"   ); break;
     case TP_ORIG: strcpy(msg, "overlay of original picks"  ); break;
     case TP_PREV: strcpy(msg, "overlay of previous profile"); break;
     case TP_NEXT: strcpy(msg, "overlay of next profile"    ); break;
     case TP_SEL : strcpy(msg, "overlay of selected profile"); break;
     case TP_REF : strcpy(msg, "overlay of reference file"  ); break;
     default: assert(FALSE);
     }
  strcat(msg, "\nturned off");
  showError(msg);
  if(!_vectors) return;
  _vectors->setVectorVisibility(ivector, FALSE);
}



void TpPopupBase::setShowOverlays(Boolean show_overlays)
{
  if(show_overlays) showOverlays();
  else              hideOverlays();
}


void TpPopupBase::setShowVector(int ivector, Boolean show_vector)
{
  if(show_vector) showVector(ivector);
  else            hideVector(ivector);
}



void TpPopupBase::setSnapChoice(int snap)
{
  if(snap < TP_NONE || snap >= _numvectors) return;
  if(snap == _snap) return;
  _snap = snap;
  if(_snap == TP_NONE && _autosnap)
     {
     showError
        ("snap choice (and automatic snap while scanning)\nturned off");
     _autosnap = FALSE;
     }
  else if(_snap == TP_NONE)
     {
     showError("snap choice\nturned off");
     }
  else
     {
     char msg[100];
     strcpy(msg, "snap choice changed to\n");
     switch(_snap)
        {
        case TP_CURR: strcat(msg, "current picks"   ); break;
        case TP_ORIG: strcat(msg, "original picks"  ); break;
        case TP_PREV: strcat(msg, "previous profile"); break;
        case TP_NEXT: strcat(msg, "next profile"    ); break;
        case TP_SEL : strcat(msg, "selected profile"); break;
        default: assert(FALSE);
        }
     showMessage(msg);
     }
}


void TpPopupBase::setPickMode(long pickmode)
{
  if(pickmode == _pickmode) return;
  _pickmode = pickmode;
  char msg[100];
  strcpy(msg, "automatic picking hot spot changed to\n");
  switch(_pickmode)
     {
     case PEAK    : strcat(msg, "peak"                  ); break;
     case TROUGH  : strcat(msg, "trough"                ); break;
     case POSITIVE: strcat(msg, "positive zero crossing"); break;
     case NEGATIVE: strcat(msg, "negative zero crossing"); break;
     default: assert(FALSE);
     }
  showMessage(msg);
}


void TpPopupBase::setAutoMode(long automode)
{
  if(automode == _automode) return;
  _automode = automode;
  char msg[200];
  strcpy(msg, "automatic picking method changed to\n");
  switch(_automode)
     {
     case FOLLOW_LINE        : strcat(msg, "follow line"            ); break;
     case FOLLOW_SLOPE       : strcat(msg, "follow slope"           ); break;
     case FOLLOW_CURVE       : strcat(msg, "follow curve"           ); break;
     case FIRST_BREAK        : strcat(msg, "first break"            ); break;
     case FIRST_BREAK_NO_SNAP: strcat(msg, "first break no snap"    ); break;
     case FIRST_BREAK_CORR   : strcat(msg, "first break with corr." ); break;
     case HURST_BREAK        : strcat(msg, "Hurst 1st break"        ); break;
     case HURST_BREAK_NO_SNAP: strcat(msg, "Hurst 1st break no snap"); break;
     case HURST_CORR         : strcat(msg, "Hurst with correlation" ); break;
     case COMBO              : strcat(msg, "combo 1st break"        ); break;
     case COMBO_CORR         : strcat(msg, "combo with correlation" ); break;
     case PICK_CORR          : strcat(msg, "pick with correlation"  ); break;
     case CORRELATE          : strcat(msg, "correlate from non-zero"); break;
     default: assert(FALSE);
     }
  if(_automode != FIRST_BREAK && _automode != FIRST_BREAK_NO_SNAP &&
     _automode != FIRST_BREAK_CORR &&
     _automode != HURST_BREAK && _automode != HURST_BREAK_NO_SNAP &&
     _automode != HURST_CORR  && _automode != COMBO &&
     _automode != COMBO_CORR  &&
     _autobreak)
        {
        _autobreak = FALSE;
        strcat(msg, " (and automatic action while scanning turned off)");
        }
  showMessage(msg);
}


void TpPopupBase::setThreshhold(float threshhold)
{
  _threshhold = ConstrainValue(threshhold, 0.00001, 0.999);;
  showMessage("first break threshhold\nchanged");
}


void TpPopupBase::setSigDiff(float sigDiff)
{
  _sigDiff = ConstrainValue(sigDiff, 0.001, 9.999);
  showMessage("Hurst 1st break significant difference\nchanged");
}


void TpPopupBase::setORF(float orf)
{
  _orf = ConstrainValue(orf, 0.000, 9.999);
  showMessage("offset ratio factor\nchanged");
}


void TpPopupBase::setShortMin(float shortMin)
{
  _shortMin = ConstrainValue(shortMin, 0.000, 9.999);
  showMessage("short offset pick window min\nchanged");
}


void TpPopupBase::setShortMax(float shortMax)
{
  _shortMax = ConstrainValue(shortMax, 0.000, 9.999);
  showMessage("short offset pick window max\nchanged");
}


void TpPopupBase::setLongMin(float longMin)
{
  _longMin = ConstrainValue(longMin, 0.000, 9.999);
  showMessage("long offset pick window min\nchanged");
}


void TpPopupBase::setLongMax(float longMax)
{
  _longMax = ConstrainValue(longMax, 0.000, 9.999);
  showMessage("long offset pick window max\nchanged");
}


void TpPopupBase::setPoto(float poto)
{
  _poto = ConstrainValue(poto, 0.0, 100.0);
  showMessage("percent offset threshold only\nchanged");
}


void TpPopupBase::setOutlier(float outlier)
{
  _outlier = ConstrainValue(outlier, 0.1, 10.0);
  showMessage("outlier for pick vs. offset\nchanged");
}


void TpPopupBase::setAutoSnap(Boolean autosnap)
{
  if(autosnap == _autosnap) return;
  _autosnap = autosnap;
  char msg[100];
  strcpy(msg, "automatic snap while scanning\n");
  if(_autosnap && _autobreak)
     {
     _autobreak = FALSE;
     strcat(msg, "turned on (and automatic first break picking turned off)");
     showMessage(msg);
     }
  else if(_autosnap)
     {
     strcat(msg, "turned on");
     showMessage(msg);
     }
  else
     {
     strcat(msg, "turned off");
     showError(msg);
     }

  if(_external_function)
    {
    if(_autosnap) 
      _external_function(SCAN_ACTION_SNAP, _external_object);
    else if(_autobreak)
      _external_function(SCAN_ACTION_PICKS, _external_object);
    else
      _external_function(SCAN_ACTION_OFF, _external_object);
    }
  
}


void TpPopupBase::setAutoBreak(Boolean autobreak)
{
  if(autobreak == _autobreak) return;
  _autobreak = autobreak;
  char msg[100];
  strcpy(msg, "automatic first break picking while scanning\n");
  if(_autobreak && _autosnap)
     {
     _autosnap = FALSE;
     strcat(msg, "turned on (and automatic snap turned off)");
     showMessage(msg);
     }
  else if(_autobreak)
     {
     strcat(msg, "turned on");
     showMessage(msg);
     }
  else
     {
     strcat(msg, "turned off");
     showError(msg);
     }

  if(_external_function)
    {
    if(_autobreak) 
      _external_function(SCAN_ACTION_PICKS,_external_object );
    else if(_autosnap)
      _external_function(SCAN_ACTION_SNAP, _external_object);
    else
      _external_function(SCAN_ACTION_OFF, _external_object);
    }
}



Boolean TpPopupBase::showingVector(int ivector) const
{
  if(ivector >= TP_CURR && ivector < _numvectors) return _show[ivector];
  return FALSE;
}

//Let any external class know what the sensitivity of the scan options
//should be.
void TpPopupBase::broadcastScanSensitivityState()
{
int first_break_state = 0;
int snap_state = 0;
int sensitive_state = SCAN_SENSITIVITY_ALL_OFF;

  if(!_external_function) return;


  if(getAutoMode() == FIRST_BREAK ||
     getAutoMode() == FIRST_BREAK_NO_SNAP ||
     getAutoMode() == FIRST_BREAK_CORR ||
     getAutoMode() == HURST_BREAK ||
     getAutoMode() == HURST_BREAK_NO_SNAP ||
     getAutoMode() == HURST_CORR ||
     getAutoMode() == COMBO ||
     getAutoMode() == COMBO_CORR)
       first_break_state = 1;

  if(getSnapChoice() != TP_NONE)
    snap_state = 1;

  if(!first_break_state && !snap_state)
    sensitive_state = SCAN_SENSITIVITY_ALL_OFF;
  else if(first_break_state && !snap_state)
    sensitive_state = SCAN_SENSITIVITY_FIRST_BREAK;
  else if(!first_break_state && snap_state)
    sensitive_state = SCAN_SENSITIVITY_SNAP;
  else
    sensitive_state = SCAN_SENSITIVITY_ALL_ON;
  
  _external_function(sensitive_state,_external_object );
}
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
