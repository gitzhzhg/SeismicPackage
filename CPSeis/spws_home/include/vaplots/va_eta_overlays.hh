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
#ifndef _VA_ETA_OVERLAYS_HH
#define _VA_ETA_OVERLAYS_HH

#include "vaplots/va_picks.hh"



class VaEtaOverlayData : public VaPickData
{
public:
	
  VaEtaOverlayData(class VfManager *eta_manager, 
                   class VaPicks *picks,
                   class VfManager *main_manager,
                   class VaVectColors *colors,
                   class SeisPlot *sp,
                   class VaPlot   *semblance_plot);
  virtual ~VaEtaOverlayData();

  //Temporary kludge
  virtual void changeActiveVelocityFunction();


  /*
   * From BaseData
   */
  virtual int   getNumPts        (       long id = defaultId);
  virtual float getX             (int i, long id = defaultId);
  virtual float getY             (int i, long id = defaultId);
  virtual int   getAltMarkerColor(int i, long id = defaultId);

  int isChanging();
  int ignoreOther();

private:
  class VfManager *_main_manager;
  class VfManager *_eta_manager;
  class VaVectColors *_colors;
  class SeisPlot     *_sp;
  class VaPlot       *_semblance_plot;
  int _changing, _hold_update;
  int *_before, *_after, *_changed;
  int _alloc4change;
  int _doing_post, _doing_total_changes, _doing_rem_ins_func, _rem_ins_func;
  int _using_mod;

  /*
   * From VfInform
   */
  virtual void beforeChanges();
  virtual void afterChanges();
  virtual void preTotalChanges(VfDataset *dataset);
  virtual void postTotalChanges(VfDataset *dataset);
  virtual void preNewActiveVelocityFunction(VfDataset *dataset);
  virtual void postNewActiveVelocityFunction(VfDataset *dataset);
  virtual void preRemoveInsertVelocityFunctions(
                         VfDataset *dataset, long ifun, long nrem, long nins);
  virtual void postRemoveInsertVelocityFunctions(
                         VfDataset *dataset, long ifun, long nrem, long nins);
  virtual void preNewActiveDataset();
  virtual void postNewActiveDataset();

};

#endif /* _VA_ETA_OVERLS_HH */
