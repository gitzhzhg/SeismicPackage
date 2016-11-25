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
#ifndef _LL_VECTOR_H
#define _LL_VECTOR_H

#include "oprim/ll_base.hh"
#include "vect/vector.hh"

class VectorLinkedList : public BaseLinkedList
{
	public:

		VectorLinkedList(float roomForMarkerFact = 1.2F);
		~VectorLinkedList();
		Vector *add(const char *name,
			class BaseData *data,
			long id = BaseData::defaultId,
			const char *color = "white",
			unsigned int width = 1,
			Bool rbn = False,
			Vector::VectorStyle style = Vector::SolidLine,
			Vector::VectorMarker marker = Vector::NoMarker,
			unsigned int markerSize = 5,
			unsigned int markerLineWidth = 1,
			const char *label = (char *) NULL,
			const char *font = "fixed",
			Bool add_at_top = False);
		Vector *add(const char *name,
			class BaseData *data,
			const char *color = "white",
			unsigned int width = 1,
			Bool rbn = False,
			Vector::VectorStyle style = Vector::SolidLine,
			Vector::VectorMarker marker = Vector::NoMarker,
			unsigned int markerSize = 5,
			unsigned int markerLineWidth = 1,
			const char *label = (char *) NULL,
			const char *font = "fixed",
			long id = BaseData::defaultId,
			Bool add_at_top = False);
		Vector *add(class BaseData *data,
			long id = BaseData::defaultId,
			const char *color = "white",
			unsigned int width = 1,
			Bool rbn = False,
			Vector::VectorStyle style = Vector::SolidLine,
			Vector::VectorMarker marker = Vector::NoMarker,
			unsigned int markerSize = 5,
			unsigned int markerLineWidth = 1,
			const char *label = (char *) NULL,
			const char *font = "fixed",
			Bool add_at_top = False);
		Vector *add(class BaseData *data,
			const char *color = "white",
			unsigned int width = 1,
			Bool rbn = False,
			Vector::VectorStyle style = Vector::SolidLine,
			Vector::VectorMarker marker = Vector::NoMarker,
			unsigned int markerSize = 5,
			unsigned int markerLineWidth = 1,
			const char *label = (char *) NULL,
			const char *font = "fixed",
			long id = BaseData::defaultId,
			Bool add_at_top = False);
		Vector *addLabel(class BaseData *data,
			const char *label,
			long id = BaseData::defaultId,
			const char *color = "white",
			const char *font = "fixed");
		Vector *addLabel(class BaseData *data,
			const char *label,
			const char *color = "white",
			const char *font = "fixed",
			long id = BaseData::defaultId);
		void addPlot(class PlotBase *plot, Bool dummy = False);
		void remove(Vector *vector)
			{ BaseLinkedList::remove((void *) vector); }
		void remove(char *name);
		void removePlot(class PlotBase *plot, Bool erase = True);
		Vector *find(Vector *vector);
		Vector *find(char *name);
		int find(char *name, Vector ***vectors);
		Vector *top    (void **p = (void **) NULL);
		class VectorElement *topElement(void **p = (void **) NULL)
			{
				return ((class VectorElement *)
					BaseLinkedList::top(p));
			}
		Vector *bottom (void **p = (void **) NULL);
		Vector *next   (void **p = (void **) NULL);
		class VectorElement *nextElement(void **p = (void **) NULL)
			{
				return ((class VectorElement *)
					BaseLinkedList::next(p));
			}
		Vector *prev   (void **p = (void **) NULL);
		Vector *current(void **p = (void **) NULL);
		void repair(class PlotBase *plot, int x, int y,
			int width, int height);
		void repairVisible(class PlotBase *plot);
		void deferredRepairAll(class PlotBase *plot);
		Vector *closest(float x, float y,
			class PlotBase *plot = (class PlotBase *) NULL,
			float *distPtr = (float *) NULL);
		Vector *closest(int x, int y, class PlotBase *plot,
			float *distPtr = (float *) NULL)
		{ return closest((float) x, (float) y, plot, distPtr); }
		Vector *closest(float x, float y, int *index,
			class PlotBase *plot = (class PlotBase *) NULL);
		Vector *closest(int x, int y, int *index, class PlotBase *plot)
		{ return closest((float) x, (float) y, index, plot); }
		Vector *closest(float x, float y, int *index1, int *index2,
			class PlotBase *plot = (class PlotBase *) NULL);
		Vector *closest(int x, int y, int *index1, int *index2,
			class PlotBase *plot)
		{ return closest((float) x, (float) y, index1, index2, plot); }
		Vector *closestVertex(float x, float y, int *vertexPtr,
			class PlotBase *plot = (class PlotBase *) NULL,
			float *distPtr = (float *) NULL);
		Vector *closestVertex(int   x, int   y, int *vertexPtr,
			class PlotBase *plot, float *distPtr = (float *) NULL)
		{ return closestVertex(
			(float) x, (float) y, vertexPtr, plot, distPtr); }
		/*
		 * PlotBase is not optional in closestLabel because label
		 * size only has meaning on a display.
		 */
		Vector *closestLabel(float x, float y, class PlotBase *plot);
		Vector *closestLabel(int x, int y, class PlotBase *plot)
		{ return closestLabel((float) x, (float) y, plot); }
		class PlotBase **getPlots();
		int getNumPlots();
		void makeVisible();
		void makeInvisible();
		void makeInvisibleAndStore();
		void restoreVisibility();
		void redisplay();
		void visableAreaChange(class PlotBase *plot, int x, int y,
			int width, int height);
		Bool okToPlot(class PlotBase *plot);
		void setAutoMarkers(Bool autoMarkers);
		Bool getAutoMarkers();
		int roomForMarkers(class PlotBase *plot);
		Bool isAdded(class PlotBase *plot);
		void setDoDefer(int set);
		int  getDoDefer(       );
		static void holdNewPlots(int hold);

		typedef struct {
			VectorLinkedList *list;
			class PlotBase   *plot;
		} CMStruct;

	private:

		class PlotBaseLinkedList *_plots;
		      Vector            **_toBeRestored;
		      Bool                _autoMarkers;
		      float               _roomForMarkerFact;
		      int                 _do_defer;

		static int _hold_new_plots, _num_new_plots_held,
			_num_new_plots_allocated;
		static CMStruct **_new_plots_held;

		static void clientMessageFunc(void *cmStruct);
		static void   doNewPlot(CMStruct *new_plot);
		static void holdNewPlot(CMStruct *new_plot);
		void repairAll(class PlotBase *plot);
};

#endif
