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
#ifndef _VA_SEMBLANCE_PICKS_HH
#define _VA_SEMBLANCE_PICKS_HH

// $Id: va_semblance_picks.hh,v 1.3 2004/07/16 13:08:52 wjdone Exp $
// $Name:  $

#include "vaplots/va_picks.hh"
#include "vaplots/va_horizons.hh"

#define VA_SEMBLANCE_PICKS "VaSemblancePicks"

class VaSemblancePickData : public VaPickData
{
public:
	
  VaSemblancePickData(class VfManager *manager,
                      class VaPicks *picks);
  virtual ~VaSemblancePickData();

  /*
   * From BaseData
   */
  virtual int   getNumPts        (       long id = defaultId);
  virtual float getX             (int i, long id = defaultId);
  virtual float getY             (int i, long id = defaultId);
  virtual int   getAltMarkerColor(int i, long id = defaultId);

  void hideActivePick();
  void setOverlayType(int set);
  void redrawActivePicks();
  

private:
            

  int _hiding_active_pick, _mod_count, _multi_hold;
  int _overlay_type;
  int _doing_total_changes;

  /*
   * From VfInform
   */
  virtual void beforeChanges();
  virtual void  afterChanges();
  virtual void  preTotalChanges(VfDataset *dataset);
  virtual void postTotalChanges(VfDataset *dataset);
  virtual void  preNewActiveVelocityFunction(VfDataset *dataset);
  virtual void postNewActiveVelocityFunction(VfDataset *dataset);
  virtual void  preModifyPicks(VfDataset *dataset,
                               long ifun, int type, long ipick, long nrem);
  virtual void postModifyPicks(VfDataset *dataset,
                               long ifun, int type, long ipick, long nrem, long nins);
  virtual void  preNewActiveDataset();
  virtual void postNewActiveDataset();
  virtual void  preNewActivePicks(VfDataset *dataset, long ifun,
                                  long nchng);
  virtual void postNewActivePicks(VfDataset *dataset, long ifun,
                                  long nchng);
  virtual void  preNewReferenceDataset();
  virtual void postNewReferenceDataset();
  virtual void  preNewReferenceVelocityFunction(
                                                VfDataset *dataset);
  virtual void postNewReferenceVelocityFunction(
                                                VfDataset *dataset);
  virtual void  preNewNeighbors(VfDataset *dataset);
  virtual void postNewNeighbors(VfDataset *dataset);

  void get_ds_and_ifun(long id, VfDataset **ds, long *ifun);
  int  xlateNumPts(        int  num, long id          );
  int  xlateXindex(int  i,           long id          );
  int  xlateYindex(int  i,           long id          );
  void xlateRange (int *i, int *num, long id          );
  void checkRange (int *i, int *num, long id, int type);
};

class VaSemblancePicker : public VaPicker
{
public:
	
  VaSemblancePicker(class PlotBase *plot,
                    class VfManager *manager,
                    class VaPicks *picks,
                    class VectorLinkedList *vectors);
  virtual ~VaSemblancePicker();

protected:

  Boolean _snapOverrideOnCntl;

private:


  virtual void noModButtonOnePress  (int x , int y);
  virtual void noModButtonOneMotion (int x1, int x2,
                                     int y1, int y2);
  virtual void noModButtonOneRelease(int x1, int x2,
                                     int y1, int y2);

  virtual void noModButtonTwoPress  (int x , int y);
  virtual void noModButtonTwoMotion (int x1, int x2,
                                     int y1, int y2);
  virtual void noModButtonTwoRelease(int x1, int x2,
                                     int y1, int y2);
  
  virtual void shiftButtonTwoPress  (int x , int y);
  virtual void shiftButtonTwoMotion (int x1, int x2,
                                     int y1, int y2);
  virtual void shiftButtonTwoRelease(int x1, int x2,
                                     int y1, int y2);

  virtual void cntlButtonOnePress  (int x , int y);
  virtual void cntlButtonOneMotion (int x1, int x2,
                                    int y1, int y2);
  virtual void cntlButtonOneRelease(int x1, int x2,
                                    int y1, int y2);

  void dpyInvVel(int mode, int index, float v, float t);

  /*! \name Snap Mode for Semblance Picking
      Private interface for using the semblance picking snap mode.
   */
  //@{
  //! Perform the snap operation, changing xWC and yWC.
  void snapPickToLocalMax(float v_snap_min, float v_snap_max,
                          float t_snap_min, float t_snap_max,
                          float &xWC, float &yWC);
  //@}
};

class VaSemblancePicks : public VaPicks
{
public:
	
  VaSemblancePicks(class VfManager *manager,
                   class VfHorizons *horizons, class VaPlot *plot,
                   SeisPlot *sp, class VaVectColors *colors);
  virtual ~VaSemblancePicks();

  class Vector *getOverlay(int i);

  virtual void registerEtaPlot(class VaEtaPlot *eta){_eta = eta;}
 
  class VaEtaPlot *eta(){return _eta;}

  class VaEtaOverlayData *getEtaOverlays(){return _eta_data;}

  void showEtaOverlays();
  void hideEtaOverlays();

  /*! \name Snap Mode for Semblance Picking
      Interface for using the semblance picking snap mode.
   */
  //@{
  //! Used by gui class to activate snap mode in semblance picking.
  void activateSnapMode(float vel_halfwin, float time_halfwin,
                        float semb_t_min, float semb_t_max,
                        float semb_v_min, float semb_v_max);
  //! Used by gui class to deactivate snap mode in semblance picking.
  void deactivateSnapMode();
  //! Get maximum time set in semblance gui.
  float   getSembGuiMaxTime() const { return _semb_t_max; };
  //! Get minimum time set in semblance gui.
  float   getSembGuiMinTime() const { return _semb_t_min; };
  //! Get maximum velocity set in semblance gui.
  float   getSembGuiMaxVel() const { return _semb_v_max; };
  //! Get minimum velocity set in semblance gui.
  float   getSembGuiMinVel() const { return _semb_v_min; };
  //! Get time half window size for search.
  float getTimeHalfWindow() const { return _time_half_win; }
  //! Get velocity half window size for search.
  float getVelocityHalfWindow() const { return _velocity_half_win; }
  //! Find out if snap mode is active.
  Boolean isSnapModeActive() const { return _do_snap_mode; }
  //! Snap a pick to max semblance value within search window, changing
  //! the values of xWC and yWC.
  void snapPickToLocalMax(float v_snap_min, float v_snap_max,
                          float t_snap_min, float t_snap_max,
                          float &xWC, float &yWC);
  //@}

private:

  class Vector *_sem_vect;
  class Vector *_eta_vect;
  class Vector *_overlay[VaVectColors::SEL];
  int _show_overlay_markers;

  class VaHorizons              *_va_horizons               ;
  class VaSemblanceHorizonsData *_va_semblance_horizons_data;
  class VaEtaOverlayData        *_eta_data;

  float     _velocity_half_win;
  float     _time_half_win;
  float     _semb_t_min, _semb_t_max;
  float     _semb_v_min, _semb_v_max;
  Boolean   _do_snap_mode;

  /*
   * From VaPicks
   */
  virtual void init(SeisPlot *sp);
  virtual void insStrt(int index, float x, float y);
  virtual void insDrag(int index, float x, float y);
  virtual void insDone(int index, float x, float y);
  virtual void repStrt(int index, float x, float y);
  virtual void repDrag(int index, float x, float y);
  virtual void repDone(int index, float x, float y);
  virtual void setShowFunc  (int which, int set);
  virtual void setEnableShowFuncs      (int set);
  virtual void setShowActiveFunc       (int set);
  virtual void setShowOverlayMarkers   (int set);
  virtual void setOverlayType          (int set);
  virtual void setShowOverlayActivePick(int set);
  virtual char *getClassName();

  virtual VaPicker *newSeparateWindow(SeisPlot *sp);

  class VaEtaPlot *_eta;
};

class VaSemblanceHorizonsData : public VaHorizonsData
{
public:
	
  VaSemblanceHorizonsData(class VfManager *vf_manager,
                          VaVectColors *colors);
  virtual ~VaSemblanceHorizonsData();

  /*
   * From BaseData
   */
  virtual int getNumPts    (       long id);
  virtual float getX       (int i, long id);
  virtual float getY       (int i, long id);
  virtual int getMarkerType(int i, long id);
  virtual int getLineStyle (       long id);

  /*
   * From VaHorizonsData
   */
  virtual int usesMarkers();

protected:


private:

  int _num_picks_in_bin, _line_style;
  float *_pick_time;
};

#endif /* _VA_SEMBLANCE_PICKS_HH */
