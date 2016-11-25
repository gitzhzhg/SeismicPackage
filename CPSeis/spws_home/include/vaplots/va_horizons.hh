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
#ifndef _VA_HORIZONS_HH
#define _VA_HORIZONS_HH

#include "oprim/base_data.hh"
#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include "vf/vf_inform.hh"

class SortedHorizon
{
	public:
	
		typedef struct { float x, y, t; } HorizonStruct;
		enum SortBy { INLINE, CROSSLINE, TIME };

		SortedHorizon(class VfManager *vf_manager,
			class VfHorizons *vf_horizons, long ihorizon);
		virtual ~SortedHorizon();

		void loadData ();
		void sortLines();

		SortedHorizon::HorizonStruct **getSorted  (SortBy sort_by);
		SortedHorizon::HorizonStruct  *getUnsorted(              );
		int getNumPicks();
		int getInlineIndex   (int ybin,               int *index);
		int getCrosslineIndex(int xbin,               int *index);
		int getTimeIndex     (float tmin, float tmax, int *index);
		int getNumPicksInBin (float xloc, float yloc,
			float **pick_time, int *interpolated);
		int gotLineNumbers();
		int onSameLine(int i1, int i2);

	protected:


	private:

		class VfHorizons *_vf_horizons;
		long              _ihorizon   ;
		HorizonStruct  *_hs                 ;
		HorizonStruct **_sorted_hs[TIME + 1];
		int _num_picks;
		int _min_x_bin, _max_x_bin, _min_y_bin, _max_y_bin;
		int *_x_bin_index, *_y_bin_index;
		int _sorted_lines;
		int _got_line_numbers;

		static class VfManager *_vf_manager;

		static int    inlineComparFunc(
			const void *element1, const void *element2);
		static int crosslineComparFunc(
			const void *element1, const void *element2);
		static int      timeComparFunc(
			const void *element1, const void *element2);
		static int time_bs(float value, HorizonStruct **hs, int num,
			int inc);
		static int insert_loc_bs(SortBy sort_by, float value,
			HorizonStruct **hs, int num);
		static float interpOrExtrap(SortBy sort_by, float loc,
			HorizonStruct **hs, int insert_index, int num);
		static float doInterpOrExtrap(float x,
			float x_closest     , float y_closest     ,
			float x_next_closest, float y_next_closest);
		static float getValue(SortBy sort_by, HorizonStruct *hs);

		void setGotLineNumbers();
};

class VaHorizonsData : public BaseData
{
	public:
	
		VaHorizonsData(class VfManager *vf_manager,
			class VaVectColors *colors);
		virtual ~VaHorizonsData();

		void setVaHorizonsManager(
			class VaHorizonsManager *va_horizons_manager);

		virtual int usesMarkers();
		void setShowHorizonMarkers(int set);
		int  getShowHorizonMarkers(       );

	protected:

		class VfManager          *_vf_manager         ;
		class VaVectColors       *_colors             ;
		class VaHorizonsManager  *_va_horizons_manager;

		int _starting_index;	/* set in getNumPts */
		int _show_horizon_markers;

	private:

};

class VaHorizons
{
	public:
	
		VaHorizons(class VfManager *vf_manager,
			class VfHorizons *vf_horizons,
			VaHorizonsData *va_horizons_data,
			class VaVectColors *colors,
			class SeisVectLinkedList *hor_vectors,
			class SeisVectLinkedList *top_vectors,
			int hiding = 0,
			int line_width_factor = 1, int pen_lift = 0);
		virtual ~VaHorizons();

		/*
		 * Called from VfInform of same name in VaHorizonsManager.
		 */
		void  preNewActiveHorizon    ();
		void postNewActiveHorizon    ();
		void  preRemoveInsertHorizons(long index, long nrem, long nins);
		void postRemoveInsertHorizons(long index, long nrem, long nins);

		/*
		 * Called from VfInform in VaHorizonsManager.
		 */
		void setColor     (long ihorizon);
		void makeVisible  (long ihorizon);
		void makeInvisible(long ihorizon);

		void putTopVectorsOnTop();

		void hide  ();
		void reveal();

		/*
		 * Called from VaVectColors
		 */
		static void broadcastSetShowHorizonMarkers(int set);
		static void broadcastSetShowActiveHorizon (int set);

		void setShowHorizonMarkers(int set);
		void setShowActiveHorizon (int set);
		
	protected:


	private:

		class VfHorizons         *_vf_horizons     ;
		      VaHorizonsData     *_va_horizons_data;
		class VaVectColors       *_colors          ;
		class SeisVectLinkedList *_hor_vectors     ;
		class SeisVectLinkedList *_top_vectors     ;

		static int                _num_va_horizons    ;
		static VaHorizonsManager *_va_horizons_manager;

		int _hiding, _line_width_factor, _pen_lift;

		void     addHorizon(long ihorizon);
		void  preRemHorizon(long ihorizon);
		void postRemHorizon(long ihorizon);
};

class VaHorizonsElement : public Element
{
	friend class VaHorizonsLinkedList;

	private:

		VaHorizons *_va_horizons;

		VaHorizonsElement(VaHorizons *va_horizons);
		virtual ~VaHorizonsElement();
		virtual int operator ==(void * const _va_horizons) const;
		virtual void print() const;
};

class VaHorizonsLinkedList : public BaseLinkedList
{
	public:

		VaHorizonsLinkedList();
		virtual ~VaHorizonsLinkedList();

		void add(VaHorizons *va_horizons);
		void remove(VaHorizons *va_horizons);

		VaHorizons *find(VaHorizons *va_horizons,
			void **p = (void **) 0);

		VaHorizons *top    (void **p = (void **) 0);
		VaHorizons *bottom (void **p = (void **) 0);
		VaHorizons *next   (void **p = (void **) 0);
		VaHorizons *prev   (void **p = (void **) 0);
		VaHorizons *current(void **p = (void **) 0);
};

class VaHorizonsManager : public VfInform
{
	public:
	
		VaHorizonsManager(class VfManager  *vf_manager ,
				  class VfHorizons *vf_horizons);
		virtual ~VaHorizonsManager();
		void addVaHorizons(VaHorizons *va_horizons);
		SortedHorizon *getSortedHorizon(long ihorizon);
		void setShowHorizonMarkers(int set);
		void setShowActiveHorizon (int set);

	protected:


	private:

		class VfHorizons     *_vf_horizons        ;
		SortedHorizon       **_sorted_horizons    ;
		int                   _num_sorted_horizons;
		VaHorizonsLinkedList *_va_horizons_list   ;
		int                  *_selected_before    ;
		int                   _num_selected_before;
		int                   _watch_was_ok       ;
		class PickWatch      *_watch              ;

		void addVfHorizon(long ihorizon);
		void remVfHorizon(long ihorizon);

		/*
		 * From VfInform
		 */
		virtual void  preNewActiveHorizon    ();
		virtual void postNewActiveHorizon    ();
		virtual void  preRemoveInsertHorizons(long index, long nrem,
			long nins);
		virtual void postRemoveInsertHorizons(long index, long nrem,
			long nins);
		virtual void  preNewSelectedHorizons ();
		virtual void postNewSelectedHorizons ();
		virtual void  preNewHorizonColor     (long ihorizon);
		virtual void postNewHorizonColor     (long ihorizon);
		virtual void  preChangeBinTolerances ();
		virtual void postChangeBinTolerances ();
		virtual void  preNewHorizonTransform ();
		virtual void postNewHorizonTransform ();

		void  preDataChange();
		void postDataChange();

		void watchOn ();
		void watchOff();
};

#endif /* _VA_HORIZONS_HH */
