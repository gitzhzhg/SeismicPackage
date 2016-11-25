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
#ifndef _VA_GRID_PICKS_HH
#define _VA_GRID_PICKS_HH

#include "vaplots/va_picks.hh"
#include "vaplots/va_horizons.hh"

#define VA_GRID_PICKS "VaGridPicks"

class VaGridPickData : public VaPickData
{
	public:
	
		VaGridPickData(class VfManager *manager, class VaPicks *picks,
			class VaVectColors *colors);
		virtual ~VaGridPickData();

		/*
		 * From BaseData
		 */
		virtual int   getNumPts        (       long id = defaultId);
		virtual float getX             (int i, long id = defaultId);
		virtual float getY             (int i, long id = defaultId);
		virtual int   getAltMarkerColor(int i, long id = defaultId);

		int isChanging();
		int ignoreOther();
		void  preCheckMarkersChanged(              );
		void postCheckMarkersChanged(int doing_post,
			int rem_ins_or_coordchange = 0, int func_index = 0,
			int func_nrem              = 0, int func_nins  = 0,
			int using_mod              = 0);

	private:

		class VaVectColors *_colors;
		int _changing;
		int *_before, *_after, *_changed;
		int _alloc4change;
		int _doing_post, _doing_total_changes, _doing_rem_ins_func;
		int _using_mod;
                int _coord_change_without_remove_or_insert;

		/*
		 * From VfInform
		 */
		virtual void beforeChanges();
		virtual void  afterChanges();
		virtual void  preTotalChanges(VfDataset *dataset);
		virtual void postTotalChanges(VfDataset *dataset);
		virtual void  preNewActiveVelocityFunction(VfDataset *dataset);
		virtual void postNewActiveVelocityFunction(VfDataset *dataset);
		virtual void  preRemoveInsertVelocityFunctions(
			VfDataset *dataset, long ifun, long nrem, long nins);
		virtual void postRemoveInsertVelocityFunctions(
			VfDataset *dataset, long ifun, long nrem, long nins);
		virtual void  preChangeCoords(VfDataset *dataset, long ifun,
			long nchng);
		virtual void postChangeCoords(VfDataset *dataset, long ifun,
			long nchng);
		virtual void  preNewActiveDataset();
		virtual void postNewActiveDataset();

		void check_alloc4change(int save_before = 0);
};

class VaGridPicker : public VaPicker
{
	public:
	
		VaGridPicker(class PlotBase *plot,
			class VfManager *manager,
			class VaPicks *picks,
			class VectorLinkedList *vectors);
		virtual ~VaGridPicker();

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

		virtual void shiftButtonOnePress  (int x , int y);
		virtual void shiftButtonOneMotion (int x1, int x2,
						   int y1, int y2);
		virtual void shiftButtonOneRelease(int x1, int x2,
						   int y1, int y2);

		virtual void shiftButtonTwoPress  (int x , int y);
		virtual void shiftButtonTwoMotion (int x1, int x2,
						   int y1, int y2);
		virtual void shiftButtonTwoRelease(int x1, int x2,
						   int y1, int y2);

		virtual void cntlButtonOnePress   (int x , int y);
		virtual void cntlButtonOneMotion  (int x1, int x2,
						   int y1, int y2);
		virtual void cntlButtonOneRelease (int x1, int x2,
						   int y1, int y2);
};

class VaGridPicks : public VaPicks
{
	public:
	
		VaGridPicks(class VfManager *manager,
			class VfHorizons *horizons, class VaPlot *plot,
			SeisPlot *sp, class VaVectColors *colors);
		virtual ~VaGridPicks();
		int isChanging();
		int isSelected(int ifun, int doing_pre);
		int getSelectType     ();
		int getEnableShowFuncs();
		int getShowActiveFunc ();

		void inlineStrt   (float x , float y );
		void inlineDrag   (float x2          );
		void inlineDone   (float x2          );
		void crosslineStrt(float x , float y );
		void crosslineDrag(          float y2);
		void crosslineDone(          float y2);
		void timesliceStrt(float x , float y );
		void timesliceDrag(float x2, float y2);
		void timesliceDone(float x2, float y2);

	private:

		class VaIsoPicks *_vaIsoPicks      ;
		class VaGvsPicks *_vaGvsPicks      ;
		class VectData   *_extra_rbn_data  ;
		class Vector     *_extra_rbn_vector;
		int _changing;
		int _select_type, _enable_show_funcs, _show_active_func;

		class VaHorizons         *_va_horizons          ;
                class VaGridHorizonsData *_va_grid_horizons_data;

		/*
		 * From VaPicks
		 */
		virtual void init(SeisPlot *sp);
		virtual void setShowFunc(int which, int set);
		virtual void setSelectType     (int set);
		virtual void setEnableShowFuncs(int set);
		virtual void setShowActiveFunc (int set);
		virtual char *getClassName();
		virtual void otherPreDataChange  (VaPicks *other_class);
		virtual void otherPrePlot        (VaPicks *other_class);
		virtual void otherNewPlot        (VaPicks *other_class);
		virtual void otherNoPlotDisplayed(VaPicks *other_class);
		virtual void otherPreMovie       (VaPicks *other_class);
		virtual void otherPostMovie      (VaPicks *other_class);

		class VaIsoPicks *getVaIsoPicks();
		class VaGvsPicks *getVaGvsPicks();
		void selectFuncs(float x1, float x2, float y1, float y2);

		virtual VaPicker *newSeparateWindow(SeisPlot *sp);
};

class VaGridHorizonsData : public VaHorizonsData
{
	public:
	
		VaGridHorizonsData(class VfManager *vf_manager,
			VaVectColors *colors);
		virtual ~VaGridHorizonsData();

		/*
		 * From BaseData
		 */
		virtual int getNumPts    (       long id);
		virtual float getX       (int i, long id);
		virtual float getY       (int i, long id);
		virtual int getMarkerType(int i, long id);
		virtual int getLineStyle (       long id);
		virtual int getPenLift   (int i, long id);

	protected:


	private:

		SortedHorizon::HorizonStruct *getHorizonStuct(int i, long id);
};

#endif /* _VA_GRID_PICKS_HH */
