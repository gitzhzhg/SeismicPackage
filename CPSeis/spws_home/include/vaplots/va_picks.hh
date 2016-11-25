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
#ifndef _VA_PICKS_HH
#define _VA_PICKS_HH

#include "vaplots/va_vect_colors.hh"
#include "oprim/base_data.hh"
#include "vf/vf_inform.hh"
#include "vf/vf_constants.hh"
#include "plot/pick_base.hh"
#include "sp/seis_inform.hh"

#define _NUM_VA_DISPLAYS 7

class VaPickData : public BaseData, public VfInform
{
	public:
	
		VaPickData(class VfManager *manager, class VaPicks *picks);
		virtual ~VaPickData();

		int vaHoldVectors     ();
		int vaFlushVectors    ();
		int vaIsHoldingVectors();

		class VaPicks *getPicks();

	protected:

		class VaPicks *_picks  ;

		/*
		 * I adopt a strategy of doing remove/insert picks and
		 * functions in the pre/post informs, but holding off
		 * the recoloring due to changes in active picks and
		 * functions until the afterChanges inform.  This strategy
		 * allows me to make sure I am not recoloring something
		 * that is no longer there.  It makes no assumption about
		 * the order of informs.  These variables are needed to
		 * store information in the pre/post informs that is
		 * needed in afterChanges for recoloring.
		 */
		int _change_act_pick, _old_act_pick;
		int _change_act_func, _old_act_func;
		int _rem_ins_pick, _pick_index, _pick_nrem, _pick_nins;
		int _rem_ins_func, _func_index, _func_nrem, _func_nins;

		int adjustIndex     (int old_index, int rem_ins_index,
			int nrem, int nins, int extra = 1);
		int newIndexEffected(int new_index, int rem_ins_index,
			int nrem, int nins, int extra = 1);

	private:

		static int _hold_count;
};

class VaPicker : public PickBase
{
	public:

		VaPicker(class PlotBase *plot, char *mode,
			const char * const helpToken,
			const char * const helpFallback,
			class VfManager *manager,
			class VaPicks *picks,
			class VectorLinkedList *vectors);
		virtual ~VaPicker();

	protected:

		class VfManager        *_manager;
		class VaPicks          *_picks  ;
		class VectorLinkedList *_vectors;

		int _insert ;	/* 1 = insert, 0 = replace */
		int _index  ;
		int _use_t_min, _use_t_max;
		float   _t_min,     _t_max;

		void bracketTime(float time,
			int calc_time_tol = 1, int vel_type = VTNM);
		int canEdit();

	private:

		virtual void buttonAny(int x1, int x2, int y1, int y2,
			int button, Action action, Modifier modifier);
};

class VaSepWinElement : public Element
{
	friend class VaSepWinLinkedList;

	private:

		SeisPlot *_plot;
		VaPicker *_pick;

		VaSepWinElement(SeisPlot *plot, VaPicker *pick);
		virtual ~VaSepWinElement();
		virtual int operator ==(void * const _plot) const;
		virtual void print() const;
};

class VaSepWinLinkedList : public BaseLinkedList
{
	public:

		VaSepWinLinkedList();
		virtual ~VaSepWinLinkedList();

		void add(SeisPlot *plot, VaPicker *pick);
		void remove(SeisPlot *plot);

		SeisPlot *find(SeisPlot *plot, void **p = (void **) NULL);

		SeisPlot *top    (void **p = (void **) NULL);
		SeisPlot *bottom (void **p = (void **) NULL);
		SeisPlot *next   (void **p = (void **) NULL);
		SeisPlot *prev   (void **p = (void **) NULL);
		SeisPlot *current(void **p = (void **) NULL);
		VaPicker *picker (void **p = (void **) NULL);
};

class VaPicks : public SeisInform
{
	public:
	
		VaPicks(class VfManager *manager, class VfHorizons *horizons,
			class VaPlot *plot, SeisPlot *sp, VaVectColors *colors,
			int num_const_vect_ll = 0);
		virtual ~VaPicks();

		/*
		 * SeisInform
		 */
		virtual void expose(SeisPlot *sp, int x, int y,
			int width, int height);
		virtual void preDataChange  (SeisPlot *sp);
		virtual void prePlot        (SeisPlot *sp);
		virtual void newPlot        (SeisPlot *sp);
		virtual void noPlotDisplayed(SeisPlot *sp);
		virtual void  preMovie(SeisPlot *sp, SeisPlot::MovieDir dir);
		virtual void postMovie(SeisPlot *sp, SeisPlot::MovieDir dir);
		virtual void postZoomSeparateWindow(SeisPlot *sp,
			SeisPlot *zoomsp);
		virtual void destroyed(SeisPlot *sp);

		/*
		 * Mechanism for VaPickers to effect rubberbanding on
		 * all VaPicks.
		 * The VaPicker derived class calls one of the below 6
		 * functions on its VaPicks class.  These functions
		 * in turn call the virtual function of the same name without
		 * the broadcast prefix for each VaPicks class listed
		 * in the static array _va_picks.  These virtual functions are
		 * defined in the derived VaPicks calls to do the
		 * rubberbanding.
		 */
		void broadcastInsStrt(int index, float x, float y);
		void broadcastInsDrag(int index, float x, float y);
		void broadcastInsDone(int index, float x, float y);
		void broadcastRepStrt(int index, float x, float y);
		void broadcastRepDrag(int index, float x, float y);
		void broadcastRepDone(int index, float x, float y);

		static void broadcastUpdateVectColors();
		static void broadcastSetShowFunc(int which, int set);
		static void broadcastSetSelectType     (int set);
		static void broadcastSetIsoTSOverlay   (int set);
		static void broadcastSetEnableShowFuncs(int set);
		static void broadcastSetCrossHairSize(int length, int width);
		static void broadcastSetCrossHairVisibility(int set);
		static void broadcastSetShowActiveFunc(int set);
		static void broadcastSetShowHyper(int set);
		static void broadcastSetShowOverlayMarkers(int set);
		static void broadcastSetOverlayType(int set);
		static void broadcastSetShowDopplerMute(int set);
		static void broadcastSetDopplerMuteParameter(float set);
		static void broadcastSetShowOverlayActiveFunc(int set);
		static void broadcastSetShowOverlayActivePick(int set);

		class VaPlot       *getPlot  ();
		      VaVectColors *getColors();
		int getShowFunc(int which);

		void setWatch(int set);
		int  getWatch(       );

		void makeEditableVisible  ();
		void makeEditableInvisible();

	protected:

		static class CoupledCrossHairs _cross_hairs;

		class VfManager           *_manager         ;
		class VfHorizons          *_horizons        ;
		class VaPlot              *_plot            ;
		      VaVectColors        *_colors          ;
		class SeisVectLinkedList  *_editable_vectors;
		class SeisVectLinkedList **_constant_vectors;
		class SeisVectLinkedList  *     _rbn_vectors;
		class VectData            *_rbn_data        ;
		class Vector              *_rbn_vector      ;
		      VaPickData          *_data            ;
		      VaPicker            *_picker          ;
		class CrossHairTranslator *_xh_trans        ;

		int _show_func[VaVectColors::NUM_SHOW_FUNC];

		VaPicks *getOtherClass(char *other_class_name);

	private:

		static VaPicks *_va_picks[_NUM_VA_DISPLAYS];
		static int _num_va_picks;
		int _is_inited;
		int _num_const_vect_ll;
		VaSepWinLinkedList *_sep_wins;
		int _watch_on, _watch_was_ok;

		virtual void init(SeisPlot *sp);

		/*
		 * For rubberbanding with picking
		 */
		virtual void insStrt(int index, float x, float y);
		virtual void insDrag(int index, float x, float y);
		virtual void insDone(int index, float x, float y);
		virtual void repStrt(int index, float x, float y);
		virtual void repDrag(int index, float x, float y);
		virtual void repDone(int index, float x, float y);

		virtual void updateVectColors();
		virtual void setShowFunc  (int which, int   set);
		virtual void setSelectType           (int   set);
		virtual void setIsoTSOverlay         (int   set);
		virtual void setEnableShowFuncs      (int   set);
		virtual void setShowActiveFunc       (int   set);
		virtual void setShowHyper            (int   set);
		virtual void setShowOverlayMarkers   (int   set);
		virtual void setOverlayType          (int   set);
		virtual void setShowDopplerMute      (int   set);
		virtual void setDopplerMuteParameter (float set);
		virtual void setShowOverlayActiveFunc(int   set);
		virtual void setShowOverlayActivePick(int   set);

		virtual char *getClassName() = 0;

		/*
		 * Called on all other VaPicks from SeisInforms.
		 * The word other reminds that the plot/movie we
		 * are informed about is from a VaPlot other than
		 * the one with the VaPicks that is receiving the message.
		 */
		virtual void otherPreDataChange  (VaPicks *other_class);
		virtual void otherPrePlot        (VaPicks *other_class);
		virtual void otherNewPlot        (VaPicks *other_class);
		virtual void otherNoPlotDisplayed(VaPicks *other_class);
		virtual void otherPreMovie       (VaPicks *other_class);
		virtual void otherPostMovie      (VaPicks *other_class);

		virtual VaPicker *newSeparateWindow(SeisPlot *sp) = 0;

		virtual int getVelocityType();
};

#endif /* _VA_PICKS_HH */
