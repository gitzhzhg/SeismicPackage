#include <string.h>
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
#include "fgxp/fgxp_2d_plot.hh"
#include "fgxp/ll_fgxp_vect.hh"
#include "fgxp/ll_fgxp_data.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_client_message.hh"
#include "sl/sl_shell_container.hh"

#include <string.h>

FgXp2DPlot::FgXp2DPlot(FieldGeometry *fg, FgXpPlotLinkedList *fgXpPlotList,
	SelectMode selectMode, DisplayMode displayMode, int numIndices,
	CardType xCardType, int xDataType, long xIndex,
	CardType yCardType, int yDataType, long yIndex,
	unsigned int width, unsigned int markerSize,
	unsigned int markerLineWidth,
	Vector::VectorMarker srcMarker , Vector::VectorMarker rcvMarker    ,
	Vector::VectorMarker bothMarker, Vector::VectorMarker neitherMarker,
	Vector::VectorMarker noMarker  ,
	const char *label, const char *font)
	: FgXpPlot(fg, fgXpPlotList, selectMode, displayMode,
	  xCardType, xDataType, yCardType, yDataType, LdCardType, FG_NONE,
	  width, markerSize, markerLineWidth,
	  srcMarker, rcvMarker, bothMarker, neitherMarker, noMarker,
	  label, font)
{
	long i;

	switch (_selectMode)
	{
		case OneLine:
			numIndices = 1;
			/* no break */
		case RangeOfLines:
			for (i = 0; i < (long) numIndices; i++)
				_vectors->add(_data->add(xIndex+i, yIndex+i));
			break;
		case AllLines:
			for (i = 0; i < fg->numLines(); i++)
				_vectors->add(_data->add(i));
			break;
		case ActiveLine:
			_vectors->add(_data->add(_fg->getActiveLineIndex()));
			break;
		case SelectedLines:
			for (i = 0; i < fg->numLines(); i++)
				if (fg->lineIsSelected(i))
					_vectors->add(_data->add(i));
			break;
		default:
			assert(False);
	}
}

FgXp2DPlot::~FgXp2DPlot()
{
	if (!_delayedDeletion)
	{
		delete _vectors;
		delete _data   ;
	}
}

void FgXp2DPlot::delayedDelete()
{
	assert(!_delayedDeletion);

	delete _vectors;
	delete _data   ;

	_fgXpPlotList->remove(this);

	Widget w = SLShellContainer::anyContainer()->W();
	assert(w);
	new SLClientMessage(w, "spws_junk", clientMessageFunc, (void *) this);

	_delayedDeletion = True;
}

void FgXp2DPlot::postResumeDependentUpdates(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	Vector   *vectPtr;
	Vector   *nextVectPtr = (Vector *) NULL;
	FgXpData *dataPtr;
	void *p;
	long activeLine, i;

	if (_outOfDate)
	{
		switch (_selectMode)
		{
			case OneLine:
			case RangeOfLines:
				for (vectPtr = _vectors->top(&p);
					vectPtr; vectPtr = nextVectPtr)
				{
					dataPtr = (FgXpData *)
						vectPtr->getData();
					nextVectPtr = _vectors->next(&p);
					_vectors->remove(vectPtr);

					if (!dataPtr->restoreLineNumber())
						_data->remove(dataPtr);
				}

				for (dataPtr = _data->top(&p);
					dataPtr; dataPtr = _data->next(&p))
				{
					_vectors->add(dataPtr, True);
				}

				if (_vectors->count() == 0)
				{
					assert(_data->count() == 0);
					delayedDelete();
				}

				break;
			case AllLines:
				for (vectPtr = _vectors->top(&p);
					vectPtr; vectPtr = nextVectPtr)
				{
					dataPtr = (FgXpData *)
						vectPtr->getData();
					nextVectPtr = _vectors->next(&p);
					_vectors->remove(vectPtr);
					_data   ->remove(dataPtr);
				}

				for (i = 0; i < fg->numLines(); i++)
					_vectors->add(_data->add(i), True);

				break;
			case ActiveLine:
				vectPtr = _vectors->top();
				dataPtr = (FgXpData *) vectPtr->getData();
				_vectors->remove(vectPtr);
				_data   ->remove(dataPtr);

				assert( _vectors->count() == 0 
					&& _data->count() == 0);

				activeLine = _fg->getActiveLineIndex();

				if (-1 == activeLine)
					delayedDelete();
				else
					_vectors->add(_data->add(activeLine),
						True);

				break;
			case SelectedLines:
				for (vectPtr = _vectors->top(&p);
					vectPtr; vectPtr = nextVectPtr)
				{
					dataPtr = (FgXpData *)
						vectPtr->getData();
					nextVectPtr = _vectors->next(&p);
					_vectors->remove(vectPtr);
					_data   ->remove(dataPtr);
				}

				for (i = 0; i < fg->numLines(); i++)
					if (fg->lineIsSelected(i))
						_vectors->add(_data->add(i),
							True);

				break;
			default:
				assert(False);
		}
	}
	else
	{
		Bool lineColorChanged;

		switch (_selectMode)
		{
			case OneLine:
			case RangeOfLines:
				for (dataPtr = _data->top(&p);
					dataPtr; dataPtr = _data->next(&p))
				{
					assert(dataPtr->restoreLineNumber());
				}
				/* no break */
			case AllLines:
				for (vectPtr = _vectors->top(&p);
					vectPtr; vectPtr = _vectors->next(&p))
				{
					lineColorChanged = _vectors->setColor
						(vectPtr, True);

					if (!lineColorChanged &&
					  _vectors->getDisplayMode() != Lines)
					{
						vectPtr->redraw();
					}
				}
				break;
			case ActiveLine:
				vectPtr = _vectors->top();
				dataPtr = (FgXpData *) vectPtr->getData();
				_vectors->remove(vectPtr);
				_data   ->remove(dataPtr);

				assert( _vectors->count() == 0 
					&& _data->count() == 0);

				activeLine = _fg->getActiveLineIndex();
				assert(-1 != activeLine);
				_vectors->add(_data->add(activeLine));

				break;
			case SelectedLines:
				for (vectPtr = _vectors->top(&p);
					vectPtr; vectPtr = nextVectPtr)
				{
					dataPtr = (FgXpData *)
						vectPtr->getData();
					nextVectPtr = _vectors->next(&p);
					_vectors->remove(vectPtr);
					_data   ->remove(dataPtr);
				}

				for (i = 0; i < fg->numLines(); i++)
					if (fg->lineIsSelected(i))
						_vectors->add(_data->add(i),
							True);

				break;
			default:
				assert(False);
		}
	}
}

char *FgXp2DPlot::getPlotLabel()
{
	char *retval = new char[strlen(dataTypeName(_data->getXDataType())) +
				strlen(dataTypeName(_data->getYDataType())) +
				11];

	strcpy(retval, "X:  ");
	strcat(retval, dataTypeName(_data->getXDataType()));
	strcat(retval, ", Y:  ");
	strcat(retval, dataTypeName(_data->getYDataType()));

	return retval;
}

BaseData *FgXp2DPlot::addData(long index)
{
	return _data->add(index);
}

void FgXp2DPlot::removeData(BaseData *data)
{
	_data->remove((FgXpData *) data);
}
