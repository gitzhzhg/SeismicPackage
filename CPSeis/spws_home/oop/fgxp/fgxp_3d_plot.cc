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
#include "fgxp/fgxp_3d_plot.hh"
#include "fgxp/ll_fgxp_vect.hh"
#include "fgxp/ll_fgxp_data.hh"
#include "fgxp/ll_fgxp_plot.hh"
#include "vect/ll_trans_3d_to_2d.hh"
#include "vect/trans_3d_to_2d.hh"
#include "geom/field_geometry.hh"
#include "sl/sl_client_message.hh"
#include "sl/sl_shell_container.hh"

#include <string.h>
#include <assert.h>

FgXp3DPlot::FgXp3DPlot(FieldGeometry *fg, FgXpPlotLinkedList *fgXpPlotList,
	float degreesZ, float degreesY, float xOffset, float xFactor,
	float yOffset , float yFactor , float zOffset, float zFactor,
	SelectMode selectMode, DisplayMode displayMode, int numIndices,
	CardType xCardType, int xDataType, long xIndex,
	CardType yCardType, int yDataType, long yIndex,
	CardType zCardType, int zDataType, long zIndex,
	float xExp, float yExp, float zExp,
	unsigned int width, unsigned int markerSize,
	unsigned int markerLineWidth,
	Vector::VectorMarker srcMarker , Vector::VectorMarker rcvMarker    ,
	Vector::VectorMarker bothMarker, Vector::VectorMarker neitherMarker,
	Vector::VectorMarker noMarker  ,
	const char *label, const char *font)
	: FgXpPlot(fg, fgXpPlotList, selectMode, displayMode,
	  xCardType, xDataType, yCardType, yDataType, zCardType, zDataType,
	  width, markerSize, markerLineWidth,
	  srcMarker, rcvMarker, bothMarker, neitherMarker, noMarker,
	  label, font)
{
	_transData = new Trans3Dto2DLinkedList(degreesZ, degreesY,
		xOffset, xFactor, yOffset, yFactor, zOffset, zFactor,
		xExp, yExp, zExp);

	long i;

	switch (_selectMode)
	{
		case OneLine:
			numIndices = 1;
			/* no break */
		case RangeOfLines:
			for (i = 0; i < (long) numIndices; i++)
				_vectors->add(_transData->add(_data->add
					(xIndex + i, yIndex + i, zIndex + i)));
			break;
		case AllLines:
			for (i = 0; i < fg->numLines(); i++)
				_vectors->add(_transData->add(_data->add(i)));
			break;
		case ActiveLine:
			_vectors->add(_transData->add(_data->add
				(_fg->getActiveLineIndex())));
			break;
		case SelectedLines:
			for (i = 0; i < fg->numLines(); i++)
				if (fg->lineIsSelected(i))
					_vectors->add(_transData->add
						(_data->add(i)));
			break;
		default:
			assert(False);
	}
}

FgXp3DPlot::~FgXp3DPlot()
{
	if (!_delayedDeletion)
	{
		delete _vectors  ;
		delete _transData;
		delete _data     ;
	}
}

void FgXp3DPlot::setAngles(float degreesZ, float degreesX)
{
	if (!_delayedDeletion)
		_transData->setAngles(degreesZ, degreesX);
}

Trans3Dto2DLinkedList *FgXp3DPlot::getTransLL()
{
	if (_delayedDeletion)
		return (Trans3Dto2DLinkedList *) NULL;
	else
		return _transData;
}

void FgXp3DPlot::delayedDelete()
{
	assert(!_delayedDeletion);

	delete _vectors  ;
	delete _transData;
	delete _data     ;

	_fgXpPlotList->remove(this);

	Widget w = SLShellContainer::anyContainer()->W();
	assert(w);
	new SLClientMessage(w, "spws_junk", clientMessageFunc, (void *) this);

	_delayedDeletion = True;
}

void FgXp3DPlot::postResumeDependentUpdates(FieldGeometry *fg)
{
	assert(fg == _fg);

	if (_delayedDeletion)
		return;

	Vector      *vectPtr;
	Vector      *nextVectPtr = (Vector *) NULL;
	Trans3Dto2D *tranPtr;
	FgXpData    *dataPtr;
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
					tranPtr = (Trans3Dto2D *)
						vectPtr->getData();
					dataPtr = (FgXpData *)
						tranPtr->actualData();
					nextVectPtr = _vectors->next(&p);
					_vectors->remove(vectPtr);

					if (!dataPtr->restoreLineNumber())
					{
						_transData->remove(tranPtr);
						_data     ->remove(dataPtr);
					}
				}

				for (dataPtr = _data->top(&p);
					dataPtr; dataPtr = _data->next(&p))
				{
					_vectors->add(tranPtr, True);
				}

				if (_vectors->count() == 0)
				{
					assert(_transData->count() == 0
						&& _data ->count() == 0);
					delayedDelete();
				}

				break;
			case AllLines:
				for (vectPtr = _vectors->top(&p);
					vectPtr; vectPtr = nextVectPtr)
				{
					tranPtr = (Trans3Dto2D *)
						vectPtr->getData();
					dataPtr = (FgXpData *)
						tranPtr->actualData();
					nextVectPtr = _vectors->next(&p);
					_vectors  ->remove(vectPtr);
					_transData->remove(tranPtr);
					_data     ->remove(dataPtr);
				}

				for (i = 0; i < fg->numLines(); i++)
					_vectors->add(_transData->add
						(_data->add(i)), True);

				break;
			case ActiveLine:
				vectPtr = _vectors->top();
				tranPtr = (Trans3Dto2D *) vectPtr->getData();
				dataPtr = (FgXpData *) tranPtr->actualData();
				_vectors  ->remove(vectPtr);
				_transData->remove(tranPtr);
				_data     ->remove(dataPtr);

				assert(    _vectors  ->count() == 0 
					&& _transData->count() == 0
					&& _data     ->count() == 0);

				activeLine = _fg->getActiveLineIndex();

				if (-1 == activeLine)
					delayedDelete();
				else
					_vectors->add(_transData->add
						(_data->add(activeLine)), True);

				break;
			case SelectedLines:
				for (vectPtr = _vectors->top(&p);
					vectPtr; vectPtr = nextVectPtr)
				{
					tranPtr = (Trans3Dto2D *)
						vectPtr->getData();
					dataPtr = (FgXpData *)
						tranPtr->actualData();
					nextVectPtr = _vectors->next(&p);
					_vectors  ->remove(vectPtr);
					_transData->remove(tranPtr);
					_data     ->remove(dataPtr);
				}

				for (i = 0; i < fg->numLines(); i++)
					if (fg->lineIsSelected(i))
						_vectors->add(_transData->add
							(_data->add(i)), True);

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
				tranPtr = (Trans3Dto2D *) vectPtr->getData();
				dataPtr = (FgXpData *) tranPtr->actualData();
				_vectors  ->remove(vectPtr);
				_transData->remove(tranPtr);
				_data     ->remove(dataPtr);

				assert(    _vectors  ->count() == 0 
					&& _transData->count() == 0
					&& _data     ->count() == 0);

				activeLine = _fg->getActiveLineIndex();

				assert(-1 != activeLine);
				_vectors->add(_transData->add
					(_data->add(activeLine)));

				break;
			case SelectedLines:
				for (vectPtr = _vectors->top(&p);
					vectPtr; vectPtr = nextVectPtr)
				{
					tranPtr = (Trans3Dto2D *)
						vectPtr->getData();
					dataPtr = (FgXpData *)
						tranPtr->actualData();
					nextVectPtr = _vectors->next(&p);
					_vectors  ->remove(vectPtr);
					_transData->remove(tranPtr);
					_data     ->remove(dataPtr);
				}

				for (i = 0; i < fg->numLines(); i++)
					if (fg->lineIsSelected(i))
						_vectors->add(_transData->add
							(_data->add(i)), True);

				break;
			default:
				assert(False);
		}
	}
}

char *FgXp3DPlot::getPlotLabel()
{
	char *retval = new char[strlen(dataTypeName(_data->getXDataType())) +
				strlen(dataTypeName(_data->getYDataType())) +
				strlen(dataTypeName(_data->getZDataType())) +
				17];

	strcpy(retval, "X:  ");
	strcat(retval, dataTypeName(_data->getXDataType()));
	strcat(retval, ", Y:  ");
	strcat(retval, dataTypeName(_data->getYDataType()));
	strcat(retval, ", Z:  ");
	strcat(retval, dataTypeName(_data->getZDataType()));

	return retval;
}

BaseData *FgXp3DPlot::addData(long index)
{
	return _transData->add(_data->add(index));
}

void FgXp3DPlot::removeData(BaseData *data)
{
	_data     ->remove((FgXpData    *) data->actualData());
	_transData->remove((Trans3Dto2D *) data              );
}
