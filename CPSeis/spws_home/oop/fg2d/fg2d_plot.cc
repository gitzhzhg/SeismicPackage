#include "fg2d/fg2d_plot.hh"
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
#include "fg2d/fg2d_data.hh"
#include "vect/ll_seis_vect.hh"

#include <assert.h>

Fg2DPlot::Fg2DPlot(class FieldGeometry *fg, class FgSeisPlotList *spList)
	: FgInform(fg), _spList(spList)
{
	/* just initializers */
}

Fg2DPlot::~Fg2DPlot()
{
	/* do nothing */
}

int Fg2DPlot::setActive(int x, int y, class PlotBase *plot)
{
	int retval;

	int index;
	Vector *vector = _vectors->closestVertex(x, y, &index, plot);

	if (vector)
	{
		setActiveByIndex(index);	/* Do not care which vector. */
		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int Fg2DPlot::setSelected(int x1, int x2, int y1, int y2, class PlotBase *plot,
	char c, int threshold)
{
   int retval;

   VectorAreaPick *vap1, *vap2;

   switch (_vectors->count())
   {
      case 0:   /* No vectors, do nothing. */
         retval = 0;
         break;
      case 1:   /* One vector, easy. */
         vap1 = _vectors->top()->getIndicesInArea(x1, y1, x2, y2, plot);

         if (vap1)
         {
            setSelectedByIndex(vap1->index, vap1->numIndices, c, threshold);
            freeVectorAreaPick(vap1);
         }

         retval = 1;
         break;
      case 2:   /* Two vectors, a little tricky. */
         vap1 = _vectors->top   ()->getIndicesInArea(x1, y1, x2, y2, plot);
         vap2 = _vectors->bottom()->getIndicesInArea(x1, y1, x2, y2, plot);

         switch (2 * (vap2 != (VectorAreaPick *) NULL)
               +     (vap1 != (VectorAreaPick *) NULL))
         {
            case 0:   /* No points. */
               /* do nothing */
               break;
            case 1:   /* Only points in vap1, top vect. */
               setSelectedByIndex(vap1->index, vap1->numIndices, c, threshold);
               freeVectorAreaPick(vap1);
               break;
            case 2:   /* Only points in vap2, bottom vect. */
               setSelectedByIndex(vap2->index, vap2->numIndices, c, threshold);
               freeVectorAreaPick(vap2);
               break;
            case 3:   /* Points in both vects. */
            /*
             * Braces so I can define variables
             * only used in this case.
             */
            {
               int *index = new int[vap1->numIndices + vap2->numIndices];
               int numIndices = 0;

               int    numPts =  _vectors->top   ()->getData()->getNumPts() ;
               assert(numPts == _vectors->bottom()->getData()->getNumPts());

               for (int i = 0, ivap1 = 0, ivap2 = 0, doit = 0; i < numPts; i++)
               {
                  if ((ivap1 < vap1->numIndices) && (vap1->index[ivap1] == i))
                  {
                     doit = 1;
                     ivap1++;
                  }

                  if ((ivap2 < vap2->numIndices) && (vap2->index[ivap2] == i))
                  {
                     doit = 1;
                     ivap2++;
                  }

                  if (doit)
                  {
                     index[numIndices++] = i;
                     doit = 0;
                  }
               }

               assert(numIndices > 0);
               setSelectedByIndex(index, numIndices, c, threshold);
               freeVectorAreaPick(vap1);
               freeVectorAreaPick(vap2);
               delete [] index;
            }
               break;
            default:
               assert(False);
         }

         retval = 1;
         break;
      default:
         assert(False);
   }

   return retval;
}

Boolean Fg2DPlot::outputByXlat(SeisPlot *sp, int x, int y,
	long *line_index, long *flag_index, long *shot_index,
	FgMapToFlag::SkidType *skid_type)
{
	Boolean retval;

	int index;
	float dist;
	Vector *vector = _vectors->closestVertex(x, y, &index, sp, &dist);

	if (vector)
	{
		/* 6.0 mm is what Trey uses in FgLocOut. */
		if (dist < 6.0)
		{
			/* Do not care which vector. */
			outputByXlatByIndex(index,
				line_index, flag_index, shot_index, skid_type);

			retval = True;
		}
		else
		{
			retval = False;
		}
	}
	else
	{
		retval = False;
	}

	return retval;
}

void Fg2DPlot::startingChanges(FieldGeometry * /*fg*/)
{
	/*
	 * If there is no data plotted, the linked list will be empty.
	 * No harm, no foul.
	 */
	Vector *ptr;
	void *p;
	for (ptr = _vectors->top(&p); ptr; ptr = _vectors->next(&p))
	{
		((Fg2DData *) ptr->getData())->storeMarkerColorsIndices();
	}

	_ignoreFinishedChanges = 0;
}

void Fg2DPlot::finishedChanges(FieldGeometry * /*fg*/)
{
	/*
	 * If there is no data plotted, the linked list will be empty.
	 * If the data is changed, the Fg2DData will not do anything
	 * with updateMarkerColorsIndices.
	 */
	if (!_ignoreFinishedChanges)
	{
		Vector *ptr;
		void *p;
		Fg2DData *data;
		for (ptr = _vectors->top(&p); ptr; ptr = _vectors->next(&p))
		{
			data = (Fg2DData *) ptr->getData();
			data->updateMarkerColorsIndices();
			data->markerColorFlush         ();
		}
	}
}

void Fg2DPlot::freezingDependentUpdates(FieldGeometry * /*fg*/)
{
	/*
	 * Make updateMarkerColorsIndices and markerColorFlush wait
	 * until finishedChanges after postResumeDependentUpdates.
	 */
	_ignoreFinishedChanges = 1;
}

void Fg2DPlot::dependentValuesOutOfDate(FieldGeometry * /*fg*/)
{
	if (isPlotted())
		removePlot();
}

void Fg2DPlot::preResumeDependentUpdates(FieldGeometry * /*fg*/)
{
	/* do nothing */
}

void Fg2DPlot::postResumeDependentUpdates(FieldGeometry * /*fg*/)
{
	if (isPlotted())
	{
		/*
		 * Changed active line or active cmp while frozen?
		 */
		if (changePlot())
		{
			removePlot();

			if (okToPlot())
				displayPlot();
		}
	}
	else
	{
		if (okToPlot())
			displayPlot();
	}
}
