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
#ifndef _VA_CROSSPLOT_PICKS_HH
#define _VA_CROSSPLOT_PICKS_HH

#include "vaplots/va_picks.hh"
#include "vaplots/va_vect_picks.hh"
#include "oprim/base_data.hh"

#define VA_CROSSPLOT_PICKS "VaCrossplotPicks"

class VaCrossFuncData : public BaseData
{
	public:
	
		VaCrossFuncData(class VaCrossplotPickData *data, int ifunc);
		virtual ~VaCrossFuncData();

		void setFunc(int ifunc);
		int  getFunc(         );

		void redrawActivePick();
		void redrawAllPicks  ();
		void redrawPicks     (int *indices, int numIndices);

		/*
		 * From BaseData
		 */
		virtual int   getNumPts        (       long id = defaultId);
		virtual float getX             (int i, long id = defaultId);
		virtual float getY             (int i, long id = defaultId);
		virtual int   getAltMarkerColor(int i, long id = defaultId);

		static void xlateRange (int  *i, int *num, int overlay_type);
		static int  xlateNumPts(int num,           int overlay_type);
		static int  xlateXindex(int   i,           int overlay_type);
		static int  xlateYindex(int   i,           int overlay_type);
		void pointsRemoved (int ipick, int nrem, int type);
		void pointsInserted(int ipick, int nins, int type);

	private:

		class VaCrossplotPickData *_data ;
		      int                  _ifunc;

		void checkRange (int  *i, int *num, int overlay_type, int type);
};

class VaCrossplotPickData : public VaVectPickData
{
	friend class VaCrossFuncData;

	public:
	
		VaCrossplotPickData(class VfManager *manager,
			VaPicks *picks, SeisPlot *sp,
			class VectorLinkedList *vectors,
			class VaVectColors *colors);
		virtual ~VaCrossplotPickData();

		void hideActivePick();
		int hidingActivePick();
		int updateLineColors();
		void on ();
		void off();
		void setMarkers    (int old_set, int new_set);
		void setActivePicks(int old_set, int new_set);
		int  activeFuncVisible();
		int getIndex(class Vector *vector);

	private:

		int _hiding_active_pick;
		int *_old_selections;

		/*
		 * From VfInform
		 */
		virtual void beforeChanges();
		virtual void  afterChanges();
		virtual void  preTotalChanges(VfDataset *dataset);
		virtual void postTotalChanges(VfDataset *dataset);
		virtual void  preModifyPicks(VfDataset *dataset,
			long ifun, int type, long ipick, long nrem);
		virtual void postModifyPicks(VfDataset *dataset,
			long ifun, int type, long ipick, long nrem, long nins);
		virtual void  preNewActiveDataset();
		virtual void postNewActiveDataset();
		virtual void  preNewActivePicks(VfDataset *dataset,
			long ifun, long nchng);
		virtual void postNewActivePicks(VfDataset *dataset,
			long ifun, long nchng);
		virtual void  preRemoveInsertVelocityFunctions(
			VfDataset *dataset, long ifun, long nrem, long nins);
		virtual void postRemoveInsertVelocityFunctions(
			VfDataset *dataset, long ifun, long nrem, long nins);
		virtual void  preChangeSelections(VfDataset *dataset,
			long ifun, long nchng);
		virtual void postChangeSelections(VfDataset *dataset,
			long ifun, long nchng);
};

class VaCrossplotPicker : public VaPicker
{
	public:
	
		VaCrossplotPicker(class PlotBase *plot,
			class VfManager *manager,
			VaPicks *picks,
			class VectorLinkedList *vectors);
		virtual ~VaCrossplotPicker();

	private:

		int _picked_fun, _picked_pck;

		virtual void noModButtonOnePress  (int x , int y);
		virtual void noModButtonOneMotion (int x1, int x2,
						   int y1, int y2);
		virtual void noModButtonOneRelease(int x1, int x2,
						   int y1, int y2);

		virtual void shiftButtonOnePress  (int x , int y);
		virtual void shiftButtonOneMotion (int x1, int x2,
						   int y1, int y2);
		virtual void shiftButtonOneRelease(int x1, int x2,
						   int y1, int y2);
};

class VaCrossplotPicks : public VaPicks
{
	public:
	
		VaCrossplotPicks(class VfManager *manager,
			class VfHorizons *horizons, class VaPlot *plot,
			SeisPlot *sp, class VaVectColors *colors);
		virtual ~VaCrossplotPicks();

		int getIndex(class Vector *vector);
		int getPick (int pick            );
		int getOverlayType          ();
		int getShowOverlayMarkers   ();
		int getShowOverlayActivePick();

	private:

		int _select_type, _overlay_type, _show_overlay_markers,
			_show_overlay_active_pick;
		int _rbn_convert_init, _rbn_index, _rbn_add_prev,
			_rbn_add_next, _rbn_replacing;
		float *_xs, *_ys;

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
		virtual void setShowFunc  (int which, int   set);
		virtual void setSelectType           (int   set);
		virtual void setEnableShowFuncs      (int   set);
		virtual void setShowActiveFunc       (int   set);
		virtual void setShowOverlayMarkers   (int   set);
		virtual void setOverlayType          (int   set);
		virtual void setShowOverlayActiveFunc(int   set);
		virtual void setShowOverlayActivePick(int   set);
		virtual char *getClassName();

		virtual VaPicker *newSeparateWindow(SeisPlot *sp);

		int  getActiveFuncNumPts();
		void initConvertFromVTNM(int index, float y, int doing_replace);
		void     convertFromVTNM(float *xs, float *ys, int *num);
		void doneConvertFromVTNM();
};

#endif /* _VA_CROSSPLOT_PICKS_HH */
