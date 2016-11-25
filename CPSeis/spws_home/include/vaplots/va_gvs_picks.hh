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
#ifndef _VA_GVS_PICKS_HH
#define _VA_GVS_PICKS_HH

#include "vaplots/va_picks.hh"
#include "vaplots/va_vect_picks.hh"
#include "vaplots/va_horizons.hh"
#include "oprim/base_data.hh"

#define VA_GVS_PICKS "VaGvsPicks"

class VaGvsFuncData : public BaseData
{
	public:
	
		VaGvsFuncData(class VaGvsPickData *data, int ifun);
		virtual ~VaGvsFuncData();

		void setFunc(int ifun);
		void updatePicks(int *indices, int numIndices);
		void hideActivePick();
		int  closestToVel(float vel);

		/*
		 * From BaseData
		 */
		virtual int   getNumPts        (       long id = defaultId);
		virtual float getX             (int i, long id = defaultId);
		virtual float getY             (int i, long id = defaultId);
		virtual int   getMarkerType    (int i, long id = defaultId);
		virtual int   getAltMarkerColor(int i, long id = defaultId);

		void pointsRemoved (int index, int num);
		void pointsInserted(int index, int num);
		int  drawLine();

	private:

		class VaGvsPickData *_data;
		      int            _ifun;
		      int            _remove_cntr, _need_mod_done;
		      int            _hiding_active_pick;
};

class VaGvsPickData : public VaVectPickData
{
	friend class VaGvsFuncData;

	public:
	
		VaGvsPickData(class VfManager *manager, class VaPicks *picks,
			SeisPlot *sp, class VectorLinkedList *vectors,
			class VaVectColors *colors);
		virtual ~VaGvsPickData();

		int isDisplayed(int ifun);
		int traceNumber(int ifun);
		void insertOrSelectFunc(int index, float xtrace);
		int updateLineColors(int check_picks);
		void updateLineDrawing();
		void hideActivePick();
		void  traceToXorY(float trace, int *is_x, float *value);
		float XorYtoTrace(float x, float y, float nil);
                //SeisInform
                void newPlot(SeisPlot *sp);

	private:
                int _error_in_headers_shown;
                char _filename[512];
		float _old_act_pick_vel;

		int getRange(int *x_fast,
			int *fast, float *fast_first, float *fast_last,
			int *slow, float *slow_val); 

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
		virtual void  preNewActivePicks(VfDataset *dataset, long ifun,
			long nchng);
		virtual void postNewActivePicks(VfDataset *dataset, long ifun,
			long nchng);
		virtual void  preChangeBinTolerances();
		virtual void postChangeBinTolerances();
};

class VaGvsPicker : public VaPicker
{
	public:
	
		VaGvsPicker(class PlotBase *plot,
			class VfManager *manager,
			class VaPicks *picks,
			class VectorLinkedList *vectors);
		virtual ~VaGvsPicker();

	private:

		int _start_frame, _max_frame, _trace_number;

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
};

class VaGvsPicks : public VaPicks
{
	public:
	
		VaGvsPicks(class VfManager *manager, class VfHorizons *horizons,
			class VaPlot *plot, SeisPlot *sp,
			class VaVectColors *colors);
		virtual ~VaGvsPicks();

		int getIndex(class Vector *vector);
		int traceNumber(int ifun);
		float getVelFromPick(float x, float y,
			int start_frame, int max_frame, int trace_number);
		void insertOrSelectFunc(int index, float xtrace);
		int withinDisplay(long ifun, int use_getPlottedLineType);
		void  traceToXorY(float trace, int *is_x, float *value);
		float XorYtoTrace(float x, float y, float nil);

	private:

		class VaHorizons        *_va_horizons         ;
		class VaGvsHorizonsData *_va_gvs_horizons_data;

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
		virtual void setShowFunc(int which, int set);
		virtual void setSelectType     (int set);
		virtual void setEnableShowFuncs(int set);
		virtual void setShowActiveFunc (int set);
		virtual char *getClassName();

		void strtRbn(float v, float t);
		void dragRbn(float v, float t);
		void doneRbn(float v, float t);

		virtual VaPicker *newSeparateWindow(SeisPlot *sp);
};

class VaGvsHorizonsData : public VaHorizonsData
{
	public:
	
		VaGvsHorizonsData(class VfManager *vf_manager,
			VaVectColors *colors,
			class VaGvsPlot  *va_gvs_plot ,
			class VaGvsPicks *va_gvs_picks);
		virtual ~VaGvsHorizonsData();

		/*
		 * From BaseData
		 */
		virtual int getNumPts    (       long id);
		virtual float getX       (int i, long id);
		virtual float getY       (int i, long id);
		virtual int getMarkerType(int i, long id);
		virtual int getLineStyle (       long id);

	protected:


	private:

		class VaGvsPlot  *_va_gvs_plot ;
		class VaGvsPicks *_va_gvs_picks;
		int               _is_inline   ;
};

#endif /* _VA_GVS_PICKS_HH */
