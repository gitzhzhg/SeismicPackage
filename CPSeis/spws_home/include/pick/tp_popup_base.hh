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

//------------------------ tp_popup_base.hh ---------------------//
//------------------------ tp_popup_base.hh ---------------------//
//------------------------ tp_popup_base.hh ---------------------//

//               header file for the TpPopupBase class
//                derived from the StpPopupBase class
//                         subdirectory pick


#ifndef _TP_POPUP_BASE_HH_
#define _TP_POPUP_BASE_HH_

#include "pick/stp_popup_base.hh"
#include "pick/tp_resources.hh"
#include <X11/Intrinsic.h>


typedef void(*PickingExternalFunction)(int which, void *data);


class TpPopupBase : public StpPopupBase
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

protected:     //External class function to notify
  PickingExternalFunction _external_function;
  void                    *_external_object;

protected:     // objects

  class TpPairBase      *_pair;
  class TpVectors       *_vectors;
  class TpMessageGui    *_message;
  class TpWorkingGui    *_working;
  class SeisShift       *_seis_shift;
  class SLSmartForm     *_work1;
  class SLSmartForm     *_work2;
  class SLSmartForm     *_work3;
  class SLSmartForm     *_work4;

protected:     // constants

  const long         _numvectors;
  const float        _zero_pick;
  const float        _missing_pick;
  const Boolean      _allow_snap;   // whether to allow snapping.
  const Boolean      _allow_auto;   // whether to allow automatic picking.
  

private:        // control variables

  int     _auto_direction;     // scanning direction for automatic action.
  Boolean _update_overlays_after_picking;
  

protected:        // GUI variables

  Boolean _show[TP_MAXNUMVECTORS]; // whether to show each vector.
  Boolean _show_overlays; // whether to show overlays.
  int     _snap;          // which vector to snap to.
  long    _pickmode;      // value for picking mode.
  long    _automode;      // value for auto mode.
  float   _threshhold;    // value for threshhold.
  float   _sigDiff;       // value for Hurst significant difference.
  float   _orf;           // offset ratio factor for offset aided correlation
  float   _shortMin;      // minimum pick time at shortest offset
  float   _shortMax;      // maximum pick time at shortest offset
  float    _longMin;      // minimum pick time at longest  offset
  float    _longMax;      // maximum pick time at longest  offset
  float   _poto;          // percent offset threshold only for combo picking
  float   _outlier;       // outlier for lstsq_adjacent for correlation picking
  Boolean _autosnap;      // whether to allow auto snapping while scanning.
  Boolean _autobreak;     // whether to allow auto breaking while scanning.
  float   _velocity;      // velocity used for flatten, supplied by a gui


//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//

protected:          // constructor and destructor

  TpPopupBase(SLDelay *slparent, char *name, HelpCtx hctx,
                        SeisPlot          *sp,
                        const long         numvectors,
                        const float        zero_pick,
                        const float        missing_pick,
                        const Boolean      allow_snap,
                        const Boolean      allow_auto,
                        const char * const picking_mode,
                        const char * const help_token,
                        const char * const help_fallback);
  
  virtual ~TpPopupBase();

protected:     // overriding virtual functions

  virtual void    updateFile    (SeisPlot *sp, Why why);
  virtual void    updateVectors (SeisPlot *sp, Why why);

  virtual class SeisVectLinkedList *createVectors (SeisPlot *sp);
  virtual void                      deleteVectors ();

  virtual class SLFilePairPlus *getFilePairPlus ();

  virtual Boolean verifyPickingAction (int button,
                                PickBase::Modifier modifier,
                                PickBase::Action action);

  virtual void pickingActionCompleted (SeisPlot *sp,
                                int button,
                                PickBase::Modifier modifier,
                                long direction,
                                long first_trace, long last_trace,
                                float first_time, float last_time);


public:  // Enums to notify or receive info from/to an external class
  enum{ FLATTEN_VELOCITY, FLATTEN_PICKS, UNFLATTEN, SCAN_ACTION_PICKS,
        SCAN_ACTION_SNAP, SCAN_ACTION_OFF, SCAN_SENSITIVITY_ALL_OFF,
        SCAN_SENSITIVITY_FIRST_BREAK, SCAN_SENSITIVITY_SNAP,
        SCAN_SENSITIVITY_ALL_ON};

  virtual void setPickingExternalFunction(PickingExternalFunction func, 
                                          void *data)
                                          {_external_function = func;
                                           _external_object = data;} 
  
  virtual void broadcastScanSensitivityState();

  long  _auto_mute;    // allows mute automatic picking.
  long  _mute_mode;    // doing mute picking

public:     // take some action

  void  updateAllVectors         ();
  void  updatePrevNextSelVectors ();
  void  updateSelVector          ();

  void  flattenToVelocity (float velocity = 0.0F);
  void  flattenPicks      ();
  void  unflatten         ();

  void  showBlank   ();                  // uses label widget
  void  showMessage (const char *msg);   // uses label widget
  void  showError   (const char *msg);   // uses label widget
  void  showWorking (const char *msg);   // uses text widget

public:     // set variables

  void  setSeisShift     (SeisShift *seis_shift);

  void  setSpecialZeroValueCoordinate (float coord);

  void  updateOverlaysAfterPicking (Boolean update_overlays_after_picking);

  void  setShowVector    (int ivector, Boolean show_vector);
  void  setShowOverlays  (Boolean show_overlays);
  void  setSnapChoice    (int     snap);
  void  setPickMode      (long    pickmode);
  void  setAutoMode      (long    automode);
  void  setThreshhold    (float   threshhold);
  void  setSigDiff       (float   sigDiff);
  void  setORF           (float   orf);
  void  setShortMin      (float   shortMin);
  void  setShortMax      (float   shortMax);
  void   setLongMin      (float    longMin);
  void   setLongMax      (float    longMax);
  void  setPoto          (float   poto);
  void  setOutlier       (float   outlier);
  void  setAutoSnap      (Boolean autosnap);
  void  setAutoBreak     (Boolean autobreak);
  void  setVelocity      (float velocity){_velocity = velocity;}

public:     // get variables

  Boolean haveSeisShift    ()            const;
  Boolean dataIsShifted    ()            const;

  Boolean showingVector    (int ivector) const;
  Boolean showingOverlays  ()            const  { return _show_overlays; }

  Boolean getUpdateOverlaysAfterPicking () const
                  { return _update_overlays_after_picking; }

  int     getSnapChoice    ()            const  { return _snap; }
  long    getPickMode      ()            const  { return _pickmode; }
  long    getAutoMode      ()            const  { return _automode; }
  float   getThreshhold    ()            const  { return _threshhold; }
  float   getSigDiff       ()            const  { return _sigDiff; }
  float   getORF           ()            const  { return _orf; }
  float   getShortMin      ()            const  { return _shortMin; }
  float   getShortMax      ()            const  { return _shortMax; }
  float    getLongMin      ()            const  { return  _longMin; }
  float    getLongMax      ()            const  { return  _longMax; }
  float    getPoto         ()            const  { return  _poto; }
  float    getOutlier      ()            const  { return  _outlier; }
  Boolean getAutoSnap      ()            const  { return _autosnap; }
  Boolean getAutoBreak     ()            const  { return _autobreak; }
  virtual void setBreakState(Boolean) {};
  virtual void setBreakSensitivity(Boolean) {};
  virtual void setSnapState(Boolean) {};
  virtual void setSnapSensitivity(Boolean) {};


private:

  long getPickingAction(int button, PickBase::Modifier modifier);

  void  modifyCurrentPicks (SeisPlot *sp,
                            long action, long direction,
                            long first_trace, long last_trace,
                            float first_time, float last_time);

  void    shiftPicksFromFileToDisplay (float *picks,
                                       long index, long npicks) const;
  void    shiftPicksFromDisplayToFile (float *picks,
                                       long index, long npicks) const;

  void    updateCurrVectorNow (SeisPlot *sp, Boolean orig_also);
  void    updatePrevVectorNow (SeisPlot *sp);
  void    updateNextVectorNow (SeisPlot *sp);
  void    updateSelVectorNow  (SeisPlot *sp);

  long    turnOffAutoAction   ();
  long    getAutomaticAction  (Why why);
  void    shiftPicksHelper    (float *picks, long index, long npicks,
                                         int direction) const;
 
  Boolean prepareToSnap(float *picks, long index, long npicks);

  void  showVector       (int ivector);
  void  hideVector       (int ivector);
  void  showOverlays     ();
  void  hideOverlays     ();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
