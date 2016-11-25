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
#ifndef _VA_CMP_PICKS_HH
#define _VA_CMP_PICKS_HH

#include "vaplots/va_picks.hh"
#include "vaplots/va_vect_picks.hh"
#include "oprim/base_data.hh"

#define VA_CMP_PICKS "VaCmpPicks"

class VaHyperData : public BaseData
{
	public:
	
		VaHyperData(class VaCmpPickData *data, int ipick,
			int is_doppler_mute = 0);
		virtual ~VaHyperData();

		void setPick(int ipick);

		/*
		 * From BaseData
		 */
		virtual int getNumPts(long id);
		virtual float getX(int i, long id);
		virtual float getY(int i, long id);

	private:

		class VaCmpPickData *_data           ;
		      int            _ipick          ;
		/*
		 * _trace_offset is set in getNumPts and used
		 * in getY to make sure
		 * you get the right offset from getHeaderFromTrace
		 * if there is a movie loaded
		 */
		      int            _trace_offset   ;
		      int            _is_doppler_mute;
};

class VaCmpPickData : public VaVectPickData
{
	friend class VaHyperData;

	public:
	
		VaCmpPickData(class VfManager *manager, class VaPicks *picks,
			SeisPlot *sp, class VectorLinkedList *vectors,
			class VectorLinkedList *constant_vectors,
			class VaVectColors *colors);
		virtual ~VaCmpPickData();
		void hideActivePick();

	private:

		class VectorLinkedList *_constant_vectors;
		class Vector           *_dop_vect        ;
		      VaHyperData      *_dop_data        ;

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
		virtual void  preNewActivePicks(VfDataset *dataset,
			long ifun, long nchng);
		virtual void postNewActivePicks(VfDataset *dataset,
			long ifun, long nchng);

		int displayDopplerMute();
};

class VaCmpPicker : public VaPicker
{
	public:
	
		VaCmpPicker(class PlotBase *plot,
			class VfManager *manager,
			class VaPicks *picks,
			class VectorLinkedList *vectors);
		virtual ~VaCmpPicker();

	private:

		int _started;
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
};

class VaCmpPicks : public VaPicks
{
	public:
	
		VaCmpPicks(class VfManager *manager, class VfHorizons *horizons,
			class VaPlot *plot, SeisPlot *sp,
			class VaVectColors *colors);
		virtual ~VaCmpPicks();

		int getIndex(class Vector *vector);
		int   getShowDopplerMute();
		float getDopplerMuteParameter();

	private:

		float *_offset, *_x, *_y;
		float *_x_in_range, *_y_in_range;
		int _num_in_range;
		int _numTracesAllocated, _numTracesUsed;
		int _show_doppler_mute;
		float _doppler_mute_param;

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
		virtual void setShowActiveFunc(int set);
		virtual void setShowHyper     (int set);
		virtual void setShowDopplerMute     (int   set);
		virtual void setDopplerMuteParameter(float set);
		virtual char *getClassName();

		void strtRbn(float v, float t);
		void dragRbn(float v, float t);
		void doneRbn(float v, float t);

		void checkAllocation();

		virtual VaPicker *newSeparateWindow(SeisPlot *sp);

		int checkHyperRange();
};

#endif /* _VA_CMP_PICKS_HH */
