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
#include <string.h>
#include "vect/ll_vector.hh"
#include "vect/vector_element.hh"
#include "vect/ll_plot_base.hh"
#include "plot/plot_base.hh"
#include "plot/pick_watch.hh"
#include "sl/shell_watch.hh"
#include "oprim/base_data.hh"
#include "oprim/static_utils.hh"
#include "sl/sl_client_message.hh"

#include <stdlib.h>
#include <assert.h>

/*
 * Static variables
 */
int VectorLinkedList::_hold_new_plots          = 0;
int VectorLinkedList::_num_new_plots_held      = 0;
int VectorLinkedList::_num_new_plots_allocated = 0;
VectorLinkedList::CMStruct **VectorLinkedList::_new_plots_held;

VectorLinkedList::VectorLinkedList(float roomForMarkerFact)
	: _toBeRestored((Vector **) NULL), _autoMarkers(False),
	  _roomForMarkerFact(roomForMarkerFact), _do_defer(1)
{
	_plots = new PlotBaseLinkedList;
}

VectorLinkedList::~VectorLinkedList()
{
	for (Vector *vPtr = top(); vPtr; vPtr = next())
	{
		/* Rbns are transient and should be already removed. */
		assert(!vPtr->_rbn);

		/*
		 * removePlot will make invisible.
		 * We do not want to makeInvisible in ~Vector because
		 * _plots (_plotList in Vector) will have already been
		 * deleted.
		 */
		vPtr->_visible = False;
	}

	PlotBase *pPtr;
	PlotBase *nextPPtr = (PlotBase *) NULL;
	void *p;
	for (pPtr = _plots->top(&p); pPtr; pPtr = nextPPtr)
	{
		nextPPtr = _plots->next(&p);
		removePlot(pPtr);
	}

	delete _plots;
}

Vector *VectorLinkedList::add(	const char *name,
				BaseData *data,
				long id,
				const char *color,
				unsigned int width,
				Bool rbn,
				Vector::VectorStyle style,
				Vector::VectorMarker marker,
				unsigned int markerSize,
				unsigned int markerLineWidth,
				const char *label,
				const char *font,
				Bool add_at_top)
{
	VectorElement *theElement = new VectorElement(_plots, name, data, this,
		id, color, width, rbn, style, marker, markerSize,
		markerLineWidth, label, font);

	if (add_at_top)
	{
		void *p;
		top(&p);
		BaseLinkedList::addBeforeCurrent((Element *) theElement, &p);
	}
	else
	{
		BaseLinkedList::add((Element *) theElement);
	}

	return theElement->_vector;
}

Vector *VectorLinkedList::add(	const char *name,
				BaseData *data,
				const char *color,
				unsigned int width,
				Bool rbn,
				Vector::VectorStyle style,
				Vector::VectorMarker marker,
				unsigned int markerSize,
				unsigned int markerLineWidth,
				const char *label,
				const char *font,
				long id,
				Bool add_at_top)
{
	return VectorLinkedList::add(name, data, id, color, width, rbn,
		style, marker, markerSize, markerLineWidth, label, font,
		add_at_top);
}

Vector *VectorLinkedList::add(	BaseData *data,
				long id,
				const char *color,
				unsigned int width,
				Bool rbn,
				Vector::VectorStyle style,
				Vector::VectorMarker marker,
				unsigned int markerSize,
				unsigned int markerLineWidth,
				const char *label,
				const char *font,
				Bool add_at_top)
{
	return VectorLinkedList::add((char *) NULL, data, id, color, width, rbn,
		style, marker, markerSize, markerLineWidth, label, font,
		add_at_top);
}

Vector *VectorLinkedList::add(	BaseData *data,
				const char *color,
				unsigned int width,
				Bool rbn,
				Vector::VectorStyle style,
				Vector::VectorMarker marker,
				unsigned int markerSize,
				unsigned int markerLineWidth,
				const char *label,
				const char *font,
				long id,
				Bool add_at_top)
{
	return VectorLinkedList::add((char *) NULL, data, id, color, width, rbn,
		style, marker, markerSize, markerLineWidth, label, font,
		add_at_top);
}

Vector *VectorLinkedList::addLabel(BaseData *data, const char *label,
	long id, const char *color, const char *font)
{
	assert(data->getNumPts(id) == 1);

	return VectorLinkedList::add(label, data, id, color, 1, False,
		Vector::NoLine, Vector::NoMarker, 5, 1, label, font);
}

Vector *VectorLinkedList::addLabel(BaseData *data, const char *label,
	const char *color, const char *font, long id)
{
	assert(data->getNumPts(id) == 1);

	return VectorLinkedList::add(label, data, id, color, 1, False,
		Vector::NoLine, Vector::NoMarker, 5, 1, label, font);
}

void VectorLinkedList::addPlot(PlotBase *plot, Bool /*dummy*/)
{	// note demotion of calling arg SeisPlot * to its base class PlotBase *
	/*
	 * dummy used to be newPlot.
	 * Since all addPlots use a client message, there
	 * is no reason to wait for 1st exposure on new plots.
	 * New plot and old plots are handled just the same.
	 */
	PickWatch  pickWatch ;
	ShellWatch shellWatch;

	_plots->add(plot);  // adds this plot to PlotBaseLinkedList via
						// a PlotBaseElement (see file vect/ll_plot_base.hh)
	Vector::_allPlots.addUser(plot);  // _allPlots is a static
									  // PlotBaseLinkedList, defined in
									  // vector.hh (list of all plots)

	/*
	 * Visible area for origin for offset vectors.
	 */
	int x, y, width, height;
	 plot ->getVisibleArea(&x, &y, &width, &height);
	_plots->setVisibleOrigin(plot, x, y);

	for (Vector *ptr = top(); ptr; ptr = next())
		if (ptr->isRubberBand())
			ptr->addRbnInfo(plot);
		else
			ptr->addPlotInfo(plot);

	Widget w = plot->getWidget();
	assert(w);
	Widget parent = get_shell_child(get_toplevel_shell(w));

	/*
	 * Only send client message if there can be a window to
	 * receive it.  If we do not send here, there will be
	 * a deferredRepairAll in the future, so do not worry.
	 */
	if (XtIsRealized(parent))
	{
		/*
		 * Hold repairs until after client message.
		 * Client message function will draw it all.
		 */
		_plots->holdRepair(plot);

		/*
		 * Use client message to draw vectors, so
		 * we do not have to worry if this is a new plot
		 * and exposes are coming.
		 */
		CMStruct *cmStruct = new CMStruct;
		cmStruct->list     = this;
		cmStruct->plot     = plot;

		SLClientMessage **cmPtrPtr = _plots->getCMPtrPtr(plot);

		assert(!(*cmPtrPtr));

		*cmPtrPtr = new SLClientMessage(w, "spws_junk",
			clientMessageFunc, (void *) cmStruct, cmPtrPtr, 2);
	}
}

void VectorLinkedList::remove(char *name)
{
	for (VectorElement *ptr = topElement(); ptr; ptr = nextElement())
		if (!strcmp(ptr->_vector->getName(), name))
		{
			removeByPtr((Element *) ptr);
			return;
		}

	assert(False);
}

void VectorLinkedList::removePlot(PlotBase *plot, Bool erase)
{
	 PickWatch *pickWatch ;
	ShellWatch *shellWatch;
		
	if (erase)
	{
		 pickWatch = new  PickWatch();
		shellWatch = new ShellWatch();
	}

	SLClientMessage **cmPtrPtr = _plots->getCMPtrPtr(plot);
	if (*cmPtrPtr)
	{
		(*cmPtrPtr)->forgetIt();
		 *cmPtrPtr = (SLClientMessage *) NULL;

	}

	_plots->remove(plot);

	if (erase && count())
	{
		int x, y, width, height;
		plot->getExposedArea(&x, &y, &width, &height);
		Vector::repairPlot(plot, x, y, width, height);
	}

	Vector::_allPlots.removeUser(plot);

	for (Vector *ptr = top(); ptr; ptr = next())
		if (ptr->isRubberBand())
			ptr->removeRbnInfo(plot);
		else
			ptr->removePlotInfo(plot);

	if (erase)
	{
		delete  pickWatch;
		delete shellWatch;
	}
}

Vector *VectorLinkedList::find(Vector *vector)
{
	VectorElement *ptr =
		(VectorElement *) BaseLinkedList::find((void *) vector);

	return (ptr ? ptr->_vector : (Vector *) NULL);
}

Vector *VectorLinkedList::find(char *name)
{
	for (VectorElement *ptr = topElement(); ptr; ptr = nextElement())
		if (!strcmp(ptr->_vector->getName(), name))
			return(ptr->_vector);

	return((Vector *) NULL);
}

int VectorLinkedList::find(char *name, Vector ***vectors)
{
	int retval = 0;
	/*
	 * array = NULL is never used.  It is set here to silence
	 * a compiler warning that array may have been used before it is set
	 * since array is set in an if block.
	 */
	Vector **array = (Vector **) NULL;

	for (VectorElement *ptr = topElement(); ptr; ptr = nextElement())
		if (!strcmp(ptr->_vector->getName(), name))
		{
			if (retval)
				array = (Vector **) realloc(array, (size_t)
					(retval + 1) * sizeof(Vector *));
			else
				array = (Vector **) malloc(sizeof(Vector *));

			array[retval++] = ptr->_vector;
		}

	// Use new to make sure application program's delete will cause
	// no problems.
	*vectors = new Vector *[retval + 1];
	memcpy(*vectors, array, (size_t) retval * sizeof(Vector *));
	(*vectors)[retval] = (Vector *) NULL;
	free(array);

	return retval;
}

Vector *VectorLinkedList::top(void **p)
{
	VectorElement *ptr = (VectorElement *) BaseLinkedList::top(p);

	return (ptr ? ptr->_vector : (Vector *) NULL);
}

Vector *VectorLinkedList::bottom(void **p)
{
	VectorElement *ptr = (VectorElement *) BaseLinkedList::bottom(p);

	return (ptr ? ptr->_vector : (Vector *) NULL);
}

Vector *VectorLinkedList::next(void **p)
{
	VectorElement *ptr = (VectorElement *) BaseLinkedList::next(p);

	return (ptr ? ptr->_vector : (Vector *) NULL);
}

Vector *VectorLinkedList::prev(void **p)
{
	VectorElement *ptr = (VectorElement *) BaseLinkedList::prev(p);

	return (ptr ? ptr->_vector : (Vector *) NULL);
}

Vector *VectorLinkedList::current(void **p)
{
	VectorElement *ptr = (VectorElement *) BaseLinkedList::current(p);

	return (ptr ? ptr->_vector : (Vector *) NULL);
}

void VectorLinkedList::repair(PlotBase *plot, int x, int y,
	int width, int height)
{
    PickWatch *pickWatch ;
   ShellWatch *shellWatch;

   if (PlotBase::watchOK())
   {
       pickWatch = new  PickWatch();
      shellWatch = new ShellWatch();
   }
   else
   {
       pickWatch = ( PickWatch *) NULL;
      shellWatch = (ShellWatch *) NULL;
   }

   if (_plots->find(plot) && !_plots->repairHeld(plot)
     && plot->isCurrentInWindow())
   {
      if (Vector::_holding)
      {
         Vector::_allPlots.updateRange(plot,
            x, x + width - 1, y, y + height - 1);
      }
      else
      {
         void *p;

         if (getAutoMarkers())
         {
            p = (void *) NULL;
            int oldRoomForMarkers = _plots->getRoomForMarkers(plot, &p);
            int newRoomForMarkers =            roomForMarkers(plot    );

            if (oldRoomForMarkers != newRoomForMarkers)
            {
               _plots->setRoomForMarkers(plot, newRoomForMarkers, &p);

               /*
                * Expand repair to redraw all markers.
                */
               int xExp, yExp, widthExp, heightExp;
               plot->getExposedArea(&xExp, &yExp, &widthExp, &heightExp);

               if (x > xExp || y > yExp || width < widthExp
                || height < heightExp)
               {
                  /*
                   * Call yourself recursively with expanded area.
                   */
                  Vector::repairPlot(plot, xExp, yExp, widthExp, heightExp);
   
                  /*
                   * Clean up.
                   */
                  if (pickWatch)
                  {
                     assert(shellWatch);
               
                     delete  pickWatch;
                     delete shellWatch;
                  }
                  else
                  {
                     assert(!shellWatch);
                  }
   
                  return;   /* Do not finish this time thru. */
               }
            }
         }

         int xVis, yVis, widthVis, heightVis;
         plot->getVisibleArea(&xVis, &yVis, &widthVis, &heightVis);
         /*
          * The repair request can be bigger than
          * the visible area if backing store is working.
          */
         Bool wholeArea = xVis >= x && yVis >= y &&
		widthVis <= x - xVis + width && heightVis <= y - yVis + height;

         int xVisOld, yVisOld;
         _plots->getVisibleOrigin(plot, &xVisOld, &yVisOld);

         Bool xOrgChanged = xVis != xVisOld;
         Bool yOrgChanged = yVis != yVisOld;
         Bool  orgChanged = xOrgChanged || yOrgChanged;
         if (orgChanged)
            _plots->setVisibleOrigin(plot, xVis, yVis);

         /*
          * If visible origin is unchanged, go ahead and repair it.
          */
         if (!orgChanged)
         {
            /*
             * This is the important few lines of code in
             * this function.  All the other junk is
             * only to handle offset vectors.
             */
            for (Vector *ptr = top(&p); ptr; ptr = next(&p))
            {
               /*
                * Start plotInfo entry from scratch if for whole area.
                */
               if (wholeArea)
               {
                  ptr->setDrawn(plot, False);
               }

               ptr->repair(plot, x, y, width, height);
            }
         }
         /*
          * If visible origin has changed, but repair request includes
          * whole visible window, go ahead and repair it.  Must reset
          * drawn variable on vectors offset in the same direction
          * as the origin shift.
          */
         else if (wholeArea)
         {
            Bool xIsOffset, yIsOffset;

            for (Vector *ptr = top(&p); ptr; ptr = next(&p))
            {
               // Not only is calling getBoolXYOffsets on an invisible
               // vector a waste of time, it can also crash because same
               // data specified offset's get[XY]OffsetType might crash with
               // invisible vector.
               //
               if (ptr->isVisible())
               {
                  ptr->getBoolXYOffsets(&xIsOffset, &yIsOffset);
   
                  /*
                   * If origin moved in offset direction,
                   * reset drawn variable since vector
                   * will be totally redraw.
                   */
                  if ((xOrgChanged && xIsOffset)
                     || (yOrgChanged && yIsOffset))
                  {
                     ptr->setDrawn(plot, False);
                  }
   
                  ptr->repair(plot, x, y, width, height);
               }
            }
         }
         /*
          * If not, check if repair area must be expanded to cleanup
          * after offset vectors.
          */
         else
         {
            int xMin = x;
            int yMin = y;
            int xMax = x + width  - 1;
            int yMax = y + height - 1;

            /*
             * See how big an area offset vectors have been draw to.
             * Only check visible vectors.
             */
            for (Vector *ptr = top(&p); ptr; ptr = next(&p))
               if (ptr->isVisible())
                  ptr->offsetCheck(plot, xOrgChanged, yOrgChanged,
                     &xMin, &yMin, &xMax, &yMax);

            /*
             * Clip to exposed area.
             */
            int xExp, yExp, widthExp, heightExp;
            plot->getExposedArea(&xExp, &yExp, &widthExp, &heightExp);

            int xRep = (xExp > xMin) ? xExp : xMin;
            int yRep = (yExp > yMin) ? yExp : yMin;
            int widthRep   = (xExp + widthExp  - 1 < xMax)
               ? widthExp  + xExp - xRep : xMax - xRep + 1;
            int heightRep  = (yExp + heightExp - 1 < yMax)
               ? heightExp + yExp - yRep : yMax - yRep + 1;

            /*
             * If offset vectors require no additional area,
             * go ahead and repair.  Resetting of drawn variable
             * if required was handled by offsetCheck.
             */
            if (xRep == x && yRep == y
               && widthRep == width && heightRep == height)
            {
               for (Vector *ptr = top(&p); ptr; ptr = next(&p))
                  ptr->repair(plot, x, y, width, height);
            }
            /*
             * If not, enlarge repair area.
             * This will recursively come back here.
             * widthRep and heightRep get <= 0 if expose event is
             * behind exposed area as can happen with rapid scrolling.
             */
            else if ((widthRep > 0) && (heightRep > 0))
            {
               Vector::repairPlot(plot, xRep, yRep, widthRep, heightRep);
            }
         }
      }
   }

   if (pickWatch)
   {
      assert(shellWatch);

      delete  pickWatch;
      delete shellWatch;
   }
   else
   {
      assert(!shellWatch);
   }
}

void VectorLinkedList::repairVisible(PlotBase *plot)
{
	int x, y, width, height;
	plot->getExposedArea(&x, &y, &width, &height);

	repair(plot, x, y, width, height);
}

void VectorLinkedList::deferredRepairAll(PlotBase *plot)
{
	if (_do_defer)
	{
		/*
		 * Hold repairs until after client message.
		 * Client message function will draw it all.
		 */
		_plots->holdRepair(plot);

		/*
		 * Use client message to draw vectors, so
		 * we do not have to worry if this is a new plot
		 * and exposes are coming.
		 */
		CMStruct *cmStruct = new CMStruct;
		cmStruct->list     = this;
		cmStruct->plot     = plot;

		SLClientMessage **cmPtrPtr = _plots->getCMPtrPtr(plot);

		if (*cmPtrPtr)
			(*cmPtrPtr)->forgetIt();

		if (_hold_new_plots)
		{
			holdNewPlot(cmStruct);
			*cmPtrPtr = (SLClientMessage *) 0;
		}
		else
		{
			Widget w = plot->getWidget();
			assert(w);
			Widget parent = get_shell_child(get_toplevel_shell(w));
			assert(XtIsRealized(parent));

			*cmPtrPtr = new SLClientMessage(w, "spws_junk",
				clientMessageFunc, (void *) cmStruct, cmPtrPtr,
				2);
		}
	}
	else
	{
		repairAll(plot);
	}
}

void VectorLinkedList::clientMessageFunc(void *cmStruct)
{
	if (_hold_new_plots)
		holdNewPlot((CMStruct *) cmStruct);
	else
		  doNewPlot((CMStruct *) cmStruct);
}

void VectorLinkedList::holdNewPlots(int hold)
{
	if (!hold && (_num_new_plots_held > 0))
	{
		for (int i = 0; i < _num_new_plots_held; i++)
			doNewPlot(_new_plots_held[i]);

		_num_new_plots_held = 0;
	}

	_hold_new_plots = hold;
}

void VectorLinkedList::doNewPlot(CMStruct *new_plot)
{
	VectorLinkedList *list = new_plot->list;
	PlotBase         *plot = new_plot->plot;

	list->repairAll(plot);

	delete new_plot;
}

#define _NEW_PLOTS_ALLOC_INC 10

void VectorLinkedList::holdNewPlot(CMStruct *new_plot)
{
	for (int i = 0; i < _num_new_plots_held; i++)
		if ((_new_plots_held[i]->list == new_plot->list)
		 && (_new_plots_held[i]->plot == new_plot->plot))
		{
			delete new_plot;
			return;
		}

	if (_num_new_plots_held == _num_new_plots_allocated)
	{
		_num_new_plots_allocated += _NEW_PLOTS_ALLOC_INC;

		if (_num_new_plots_allocated == _NEW_PLOTS_ALLOC_INC)
			assert(_new_plots_held = (CMStruct **)  malloc(
				(size_t) _num_new_plots_allocated
				* sizeof(CMStruct *)));
		else
			assert(_new_plots_held = (CMStruct **) realloc(
				(void *) _new_plots_held,
				(size_t) _num_new_plots_allocated
				* sizeof(CMStruct *)));
	}

	_new_plots_held[_num_new_plots_held++] = new_plot;
}

void VectorLinkedList::repairAll(PlotBase *plot)
{
	_plots->releaseRepair(plot);

	int x, y, width, height;
	plot->getExposedArea(&x, &y, &width, &height);
	repair(plot, x, y, width, height);
}

Vector *VectorLinkedList::closest(float x, float y, PlotBase *plot,
	float *distPtr)
{
	Vector *retval = (Vector *) NULL;
	float distance;
	float closestDistance = 0.0;	/* Silence compiler warning. */

	if (plot)
	{
		Vector *ptr;
		void *p;
		Bool first;
		for (ptr = top(&p), first = True; ptr; ptr = next(&p))
		{
			if (!ptr->isRubberBand() && ptr->isVisible()
				&& ptr->getData()->getNumPts(ptr->getId()))
			{
				if (first)
				{
					closestDistance =
						ptr->maxDistance(x, y, plot);

					first = False;
				}
				else
				{
					distance =
						ptr->maxDistance(x, y, plot);

					if (distance < closestDistance)
						closestDistance = distance;
				}
			}
		}

		for (ptr = top(&p); ptr; ptr = next(&p))
		{
			if (!ptr->isRubberBand() && ptr->isVisible() &&
				ptr->getData()->getNumPts(ptr->getId()) &&
				ptr->minDistance(x, y, plot) <= closestDistance)
			{
				if (retval)
				{
					distance = ptr->howClose(x, y, plot);

					if (distance < closestDistance)
					{
						closestDistance = distance;
						retval = ptr;
					}
				}
				else
				{
					closestDistance = ptr->howClose(x, y,
						plot);
					retval = ptr;
				}
			}
		}
	}
	else
	{
		Vector *ptr;
		void *p;
		for (ptr = top(&p); ptr; ptr = next(&p))
		{
			if (!ptr->isRubberBand() && ptr->isVisible()
				&& ptr->getData()->getNumPts(ptr->getId()))
			{
				if (retval)
				{
					distance = ptr->howClose(x, y, plot);

					if (distance < closestDistance)
					{
						closestDistance = distance;
						retval = ptr;
					}
				}
				else
				{
					closestDistance = ptr->howClose(x, y,
						plot);
					retval = ptr;
				}
			}
		}
	}

	if (retval && distPtr)
		*distPtr = closestDistance;

	return retval;
}

Vector *VectorLinkedList::closest(float x, float y, int *index, PlotBase *plot)
{
	Vector *retval = closest(x, y, plot);

	if (retval)
		*index = retval->closestIndex(x, y, plot);

	return retval;
}

Vector *VectorLinkedList::closest(float x, float y, int *index1, int *index2,
	PlotBase *plot)
{
	Vector *retval = closest(x, y, index1, plot);

	if (retval)
		retval->closestIndices(x, y, index1, index2, plot);

	return retval;
}

Vector *VectorLinkedList::closestVertex(float x, float y, int *vertexPtr,
	PlotBase *plot, float *distPtr)
{
	Vector *retval = (Vector *) NULL;
	int vertex, closestVertex;
	float distance;
	/*
	 * closestDistance = 0.0 is never used.  It is set here to silence
	 * a compiler warning that closestDistance may have been used before
	 * it is set since closestDistance is set in an if block.
	 */
	float closestDistance = 0.0;

	for (Vector *ptr = top();
		ptr && (0.0 < closestDistance || (Vector *) NULL == retval);
		ptr = next())
	{
		if (!ptr->isRubberBand() && ptr->isVisible()
			&& ptr->getData()->getNumPts(ptr->getId()))
		{
			if (retval)
			{
				vertex = ptr->closestVertex(x, y, plot,
					&distance);

				if (distance < closestDistance)
				{
					closestVertex   = vertex  ;
					closestDistance = distance;
					retval          = ptr     ;
				}
			}
			else
			{
				closestVertex = ptr->closestVertex(x, y,
					plot, &closestDistance);
				retval = ptr;
			}
		}
	}

	if (retval)
	{
		*vertexPtr = closestVertex;

		if (distPtr)
			*distPtr = closestDistance;
	}

	return retval;
}

Vector *VectorLinkedList::closestLabel(float x, float y, PlotBase *plot)
{
	Vector *retval = (Vector *) NULL;
	float distanceToRect, distanceToPt;
	/*
	 * closestToRect = 0.0 and closestToPt = 0.0 are never used.
	 * They are set here to silence a compiler warning that they
	 * may have been used before they are set since they are set
	 * in an if block.
	 */
	float closestToRect = 0.0;
	float closestToPt   = 0.0;

	for (Vector *ptr = top(); ptr; ptr = next())
		if (ptr->isLabel())
		{
			if (retval)
			{
				ptr->howCloseToLabel(x, y, plot,
					&distanceToRect, &distanceToPt);

				if ((distanceToRect < closestToRect) ||
					(distanceToRect == closestToRect &&
					distanceToPt < closestToPt))
				{
					closestToRect = distanceToRect;
					closestToPt   = distanceToPt  ;
					retval = ptr;
				}
			}
			else
			{
				ptr->howCloseToLabel(x, y, plot,
					&closestToRect, &closestToPt);

				retval = ptr;
			}
		}

	return retval;
}

PlotBase **VectorLinkedList::getPlots()
{
	PlotBase **retval = new PlotBase *[_plots->count() + 1];
	PlotBase **retPtr = retval;

	for (PlotBase *ptr = _plots->top(); ptr; ptr = _plots->next())
		*retPtr++ = ptr;

	*retPtr = (PlotBase *) NULL;

	return retval;
}

int VectorLinkedList::getNumPlots()
{
	return _plots->count();
}

void VectorLinkedList::makeVisible()
{
	Vector *ptr;
	void *p;
	for (ptr = top(&p); ptr != (Vector *) NULL; ptr = next(&p))
	{
		assert(!ptr->_rbn);
		if (!ptr->isVisible())
			ptr->makeVisible();
	}
}

void VectorLinkedList::makeInvisible()
{
	Bool holding = SU::isHoldingVectors();

	if (!holding)
		SU::holdVectors();

	Vector *ptr;
	void *p;
	for (ptr = top(&p); ptr != (Vector *) NULL; ptr = next(&p))
	{
		assert(!ptr->_rbn);
		if (ptr->isVisible())
			ptr->makeInvisible();
	}

	if (!holding)
		SU::flushVectors();
}

void VectorLinkedList::makeInvisibleAndStore()
{
	assert(_toBeRestored == NULL);

	Bool holding = SU::isHoldingVectors();

	if (!holding)
		SU::holdVectors();

	Vector **toBeRestoredPtr;
	assert(toBeRestoredPtr = _toBeRestored = new Vector *[count() + 1]);

	Vector *ptr;
	void *p;
	for (ptr = top(&p); ptr != (Vector *) NULL; ptr = next(&p))
	{
		assert(!ptr->_rbn);
		if (ptr->isVisible())
		{
			ptr->makeInvisible();
			*toBeRestoredPtr++ = ptr;
		}
	}

	*toBeRestoredPtr = NULL;	/* NULL terminate */

	if (!holding)
		SU::flushVectors();
}

void VectorLinkedList::restoreVisibility()
{
	assert(_toBeRestored != NULL);

	for (Vector **ptr = _toBeRestored; *ptr != (Vector *) NULL; ptr++)
	{
		if (!(*ptr)->isVisible())
			(*ptr)->makeVisible();
	}

	delete [] _toBeRestored;
	_toBeRestored = NULL;
}

void VectorLinkedList::redisplay()
{
	PlotBase *ptr;
	void *p;
	int x, y, width, height;

	for (ptr = _plots->top(&p); ptr; ptr = _plots->next(&p))
	{
		ptr->getExposedArea(    &x, &y, &width, &height);
		Vector::repairPlot (ptr, x,  y,  width,  height);
	}
}

void VectorLinkedList::visableAreaChange(PlotBase *plot, int x, int y,
	int /*width*/, int /*height*/)
{
	/*
	 * Handle offset vectors if you have backing store.
	 * If not, let expose handle it.
	 * This code is inefficient if the x-server supports backing store
	 * and the client requests backing store, but backing store is not
	 * used (lack of server memory).  This inefficiency is tolerated
	 * since this should rarely be the case.
	 */

	if (plot->hasBackingStore())
	{
		int xOld, yOld;
		_plots->getVisibleOrigin(plot, &xOld, &yOld);

		Bool xOrgChanged = x != xOld;
		Bool yOrgChanged = y != yOld;

		/*
		 * Its possible that the origin didn't change, but
		 * we got here because the window changed size.
		 * In that case, offset vectors need no special updating.
		 */
		if (xOrgChanged || yOrgChanged)
		{
			Bool holding = SU::isHoldingVectors();

			if (!holding)
				SU::holdVectors();

			Vector **offsets = new Vector *[count()];
			int numOffsets = 0;

			Bool xIsOffset, yIsOffset;
			Vector *ptr;
			void *p;
			for (ptr = top(&p); ptr; ptr = next(&p))
			{
				if (ptr->isVisible())
				{
					ptr->getBoolXYOffsets(&xIsOffset,
						&yIsOffset);

					if ((xOrgChanged && xIsOffset) ||
						    (yOrgChanged && yIsOffset))
					{
						ptr->undrawVisible(plot);
						offsets[numOffsets++] = ptr;
					}
				}
			}

			if (!holding)
				SU::flushVectors();

			_plots->setVisibleOrigin(plot, x, y);

			for (int i = 0; i < numOffsets; i++)
				offsets[i]->drawVisible(plot);

			delete [] offsets;
		}
	}
}

Bool VectorLinkedList::okToPlot(PlotBase *plot)
{
	return !_plots->repairHeld(plot);
}

void VectorLinkedList::setAutoMarkers(Bool autoMarkers)
{
   if (_autoMarkers != autoMarkers)
   {
      _autoMarkers = autoMarkers;

      PlotBase *pptr;
      void     *pp  ;
      Vector *vptr;
      void   *vp  ;
      int x, y, width, height;
      Vector::VectorMarker marker;
      unsigned int markerSize, markerLineWidth;
      Bool doit;

      /*
       * going off to on
       */
      if (_autoMarkers)
      {
         int room;

         for (pptr = _plots->top(&pp); pptr; pptr = _plots->next(&pp))
         {
            room = roomForMarkers(pptr);
            _plots->setRoomForMarkers(pptr, room, &pp);

            if (!room)
            {
               for (doit = False, vptr = top(&vp);
                  !doit && vptr;
                  vptr = next(&vp))
               {
                  vptr->getMarker(&marker, &markerSize, &markerLineWidth);

                  if (vptr->isVisible() && marker != Vector::NoMarker)
                     doit = True;
               }

               if (doit)
               {
                  pptr->getExposedArea(     &x, &y, &width, &height);
                  Vector::repairPlot  (pptr, x,  y,  width,  height);
               }
            }
         }
      }
      /*
       * going on to off
       */
      else
      {
         for (pptr = _plots->top(&pp); pptr; pptr = _plots->next(&pp))
            if (!_plots->getRoomForMarkers(pptr, &pp))
            {
               for (doit = False, vptr = top(&vp);
                   !doit && vptr;
                   vptr = next(&vp))
               {
                  vptr->getMarker(&marker, &markerSize, &markerLineWidth);

                  if (vptr->isVisible() && marker != Vector::NoMarker)
                  {
                     if (SU::isHoldingVectors())
                        doit = True;
                     else
                        vptr->drawVisible(pptr);
                  }
               }

               if (doit)
               {
                  pptr->getExposedArea(     &x, &y, &width, &height);
                  Vector::repairPlot  (pptr, x,  y,  width,  height);
               }
            }
      }
   }
}

Bool VectorLinkedList::getAutoMarkers()
{
	return _autoMarkers;
}

int VectorLinkedList::roomForMarkers(PlotBase *plot)
{
	int retval;

	int width, height;
	plot->getSize(&width, &height);

	float xMin = plot->xWC(0);
	float yMin = plot->yWC(0);
	float xMax = plot->xWC(width  - 1);
	float yMax = plot->yWC(height - 1);

	float temp;

	if (xMin > xMax)
	{
		temp = xMin;
		xMin = xMax;
		xMax = temp;
	}

	if (yMin > yMax)
	{
		temp = yMin;
		yMin = yMax;
		yMax = temp;
	}

	Vector *ptr;
	void *p;
	Vector::VectorMarker marker;
	unsigned int markerSize, markerLineWidth;
	BaseData *data;
	int numPts, pts, i;
	float x, y, xMn, xMx, yMn, yMx, pixelDist;
	double dX, dY;

	for (retval = 1, ptr = top(&p); retval && ptr; ptr = next(&p))
	{
		ptr->getMarker(&marker, &markerSize, &markerLineWidth);

		if (ptr->isVisible() && (marker != Vector::NoMarker))
		{
			data = ptr->getData();
			numPts = data->getNumPts();

			for (xMn = xMax, xMx = xMin, yMn = yMax, yMx = yMin,
				pts = i = 0; i < numPts; i++)
			{
				x = data->getX(i);
				y = data->getY(i);

				if (x >= xMin && x <= xMax
				 && y >= yMin && y <= yMax
				 && (marker != Vector::DataSpecifiedMarker
				 || data->getMarkerType(i) != Vector::NoMarker))
				{
					pts++;
					if (x < xMn) xMn = x;
					if (x > xMx) xMx = x;
					if (y < yMn) yMn = y;
					if (y > yMx) yMx = y;
				}
			}

			if (pts > 1)
			{
				dX = (double)
					(plot->xPixel(xMx) - plot->xPixel(xMn));
				dY = (double)
					(plot->yPixel(yMx) - plot->yPixel(yMn));

				pixelDist = (float) sqrt(dX * dX + dY * dY);

				if (pts > (int) (pixelDist
							/ (float) markerSize
							/ _roomForMarkerFact
							+ 0.5F))
				{
					retval = 0;
				}
			}
		}
	}

	return retval;
}

Bool VectorLinkedList::isAdded(PlotBase *plot)
{
	return (_plots->find(plot)) ? True : False;
}

void VectorLinkedList::setDoDefer(int set)
{
	_do_defer = set;
}

int  VectorLinkedList::getDoDefer()
{
	return _do_defer;
}
