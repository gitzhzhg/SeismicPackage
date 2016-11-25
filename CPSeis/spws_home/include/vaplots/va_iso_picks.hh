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
#ifndef _VA_ISO_PICKS_HH
#define _VA_ISO_PICKS_HH

#include "vaplots/va_picks.hh"
#include "vaplots/va_horizons.hh"

#define VA_ISO_PICKS "VaIsoPicks"

class VaIsoPickData : public VaPickData
{
	public:
	
		VaIsoPickData(class VfManager *manager, class VaPicks *picks,
			SeisPlot *sp, class VaVectColors *colors);
		virtual ~VaIsoPickData();

		/*
		 * From BaseData
		 */
		virtual int   getNumPts        (       long id = defaultId);
		virtual float getX             (int i, long id = defaultId);
		virtual float getY             (int i, long id = defaultId);
		virtual int   getMarkerType    (int i, long id = defaultId);
		virtual int   getAltMarkerColor(int i, long id = defaultId);
		virtual int   getLineStyle     (       long id = defaultId);

		int isDisplayed(int line_type, class VfDataset *ds, long ifun);
		void hideActivePick();
		void  preChangeShowActiveFunc();
		void postChangeShowActiveFunc();

	private:

		      SeisPlot     *_sp    ;
		class VaVectColors *_colors;
		      int           _need_mod_done     ;
		      int           _hiding_active_pick;
		/*
		 * _numPts is set in getNumPts, used in getY and getMarkerType.
		 */
		      int           _numPts;

		/*
		 * From VfInform
		 */
		virtual void beforeChanges();
		virtual void  afterChanges();
		virtual void  preTotalChanges(VfDataset *dataset);
		virtual void postTotalChanges(VfDataset *dataset);
		virtual void  preNewActiveVelocityFunction(VfDataset *dataset);
		virtual void postNewActiveVelocityFunction(VfDataset *dataset);
		virtual void  preNewActivePicks(VfDataset *dataset, long ifun,
			long nchng);
		virtual void postNewActivePicks(VfDataset *dataset, long ifun,
			long nchng);
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
		virtual void  preChangeBinTolerances();
		virtual void postChangeBinTolerances();
};

class VaIsoPicker : public VaPicker
{
	public:
	
		VaIsoPicker(class PlotBase *plot,
			class VfManager *manager,
			class VaPicks *picks,
			class VectorLinkedList *vectors);
		virtual ~VaIsoPicker();

	private:

		float _t, _v;

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

		int  initPick(int x, int y);
		long closestFunc(int x_dc, int y_dc, float *x_wc, float *y_wc);
};

class VaIsoPicks : public VaPicks
{
	public:
	
		VaIsoPicks(class VfManager *manager, class VfHorizons *horizons,
			class VaPlot *plot, SeisPlot *sp,
			class VaVectColors *colors);
		virtual ~VaIsoPicks();
		void extraRbnStrt(float x1, float y1);
		void extraRbnDrag(float x2, float y2);
		void extraRbnDone(                  );
		int withinDisplay(long ifun, int use_getPlottedLineType);
		int getShowActiveFunc();

	private:

		class VectData *_extra_rbn_data  ;
		class Vector   *_extra_rbn_vector;
		int             _doNormalRbn     ;
		int             _show_active_func;

		class VaHorizons        *_va_horizons         ;
		class VaIsoHorizonsData *_va_iso_horizons_data;

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
		virtual void setIsoTSOverlay  (int set);
		virtual void setShowActiveFunc(int set);
		virtual char *getClassName();

		void strtRbn(float t);
		void dragRbn(float t);
		void doneRbn(float t);

		virtual VaPicker *newSeparateWindow(SeisPlot *sp);

		/*
		 * VaPicks
		 */
		virtual int getVelocityType();
};

class VaIsoHorizonsData : public VaHorizonsData
{
	public:
	
		VaIsoHorizonsData(class VfManager *vf_manager,
			VaVectColors *colors, class VaIsoPlot *va_iso_plot);
		virtual ~VaIsoHorizonsData();

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

		class VaIsoPlot *_va_iso_plot;
		SortedHorizon::HorizonStruct *getHorizonStuct(int i, long id);
};

#endif /* _VA_ISO_PICKS_HH */
