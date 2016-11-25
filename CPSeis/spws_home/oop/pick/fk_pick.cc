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
#include "pick/fk_pick.hh"
#include "pick/fk_pop.hh"
#include "pick/fk_pair.hh"
#include "pick/ll_fk_data.hh"
#include "pick/fk_fan_data.hh"
#include "pick/fk_poly_data.hh"
#include "pick/fk_restrictions.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vect_data.hh"
#include "sl/sl_error_pop.hh"

#include <string.h>
#include <assert.h>

#define FALLBACK1 "*FK_PICK1:  BTN#1: add FAN, shft BTN#1: edit FAN, cntl BTN#1: delete FAN\\nBTN#2: add POLY, shft BTN#2: select POLY for edit, cntl BTN#2: delete POLY"

#define FALLBACK2 "*FK_PICK2:  BTN#1: add next slope, shft BTN#1: delete last slope, cntl BTN#1: abort\\nBTN#2: none"

#define FALLBACK3 "*FK_PICK3:  BTN#1: finish POLY\\nBTN#2: add next point, shft BTN#2: delete last point, cntl BTN#2: abort"

#define FALLBACK4 "*FK_PICK4:  BTN#1: end POLY edit\\nBTN#2: replace point, shft BTN#2: insert point, cntl BTN#2: delete point"

#define FALLBACK5 "*FK_PICK5:  BTN#1: add next slope, shft BTN#1: delete last slope, cntl BTN#1: abort\\nBTN#2: make symmetric fan"

#define REALLY_CLOSE    1.0F
#define FAIRLY_CLOSE    5.0F
#define FIXED_INFO_SIZE 132

FkPick::FkPick(FkPop *fkPop)
	: PickBase(fkPop->getSeisPlot(), "FK Picking", "FK_PICK1", FALLBACK1),
	  _fkPop(fkPop), _pickMode(Normal)
{
	_vectors = new SeisVectLinkedList();

	_vectors->addPlot(_fkPop->getSeisPlot());
}

FkPick::~FkPick()
{
	switch (_pickMode)
	{
		case InsertFan:
		case InsertFanSym:
		case InsertPoly:
			_vectors->remove(_insVect);
			delete _insData;
			break;
		case Normal:
		case EditFan:
		case EditPoly:
			/* do nothing */
			break;
		default:
			assert(False);
	}

	delete _vectors;
}

void FkPick::noModButtonOnePress(int x, int y)
{
	float xFan[2], yFan[2];

	switch (_pickMode)
	{
		case InsertFan:
		case InsertFanSym:
			if (_insVect->howClose(x, y, getPlot()) <= REALLY_CLOSE)
			{
				int i1, i2;
				_insVect->closestIndices(x, y, &i1, &i2,
					getPlot());
				_insData->remove(((i1 < i2) ? i1 : i2), 2);
			}
			getFanValues(x, y, &xFan[0], &yFan[0]);
			xFan[1] = yFan[1] = 0.0F;
			_rbnData = new VectData(2, xFan, yFan);
			_rbnVect = _vectors->add(_rbnData, "red", 2, True);
			 break;
		case Normal:
			if (canInsertFan())
			{
				getFanValues(x, y, &xFan[0], &yFan[0]);
				xFan[1] = yFan[1] = 0.0F;
				_rbnData = new VectData(2, xFan, yFan);
				_rbnVect = _vectors->add(_rbnData, "red", 2,
					True);
			}
			else
			{
				doBeep();
				ignoreActions();
			}
			break;
		case InsertPoly:
			_vectors->remove(_insVect);

			float h1, h2;
			if (_fkPop->getDisplayedHeaders(&h1, &h2))
			{
				new SLErrorPop(_fkPop, "FK Picking Error",
                          "Display has differing values for selected headers");
			}
			else
			{
				int numPts = _insData->getNumPts();
				float *w = new float[numPts];
				float *f = new float[numPts];

				for (int i = 0; i < numPts; i++)
				{
					w[i] = _insData->getX(i);
					f[i] = _insData->getY(i);
				}

				FkPolyData *data = new FkPolyData(
					_fkPop->getData(), h1, h2,
					numPts, w, f, _fkPop->getCutPass());
				_fkPop->getData()->addSorted(data);

				delete [] w;
				delete [] f;

				Vector *v = _fkPop->getVectors()->add(
					data, _fkPop->nextColor(),2);
				v->makeVisible();

				char fixedInfo[FIXED_INFO_SIZE];
				if (_fkPop->getData()->writeFile(
					_fkPop->getFilePair()->
						workingFilename(),
					fixedInfo))
				{
					new SLErrorPop(_fkPop,
						"FK Picking Error", fixedInfo);
				}
			}

			delete _insData;
			_pickMode = Normal;
			changeHelpToken("FK_PICK1", FALLBACK1);
			ignoreActions();
			break;
		case EditFan:
			assert(False);
			break;
		case EditPoly:
			_edtVect->setColor(_vectColor);
			delete [] _vectColor;
			_pickMode = Normal;
			changeHelpToken("FK_PICK1", FALLBACK1);
			ignoreActions();
			break;
		default:
			assert(False);
	}
}

void FkPick::shiftButtonOnePress(int x, int y)
{
	float xFan[2], yFan[2];
	Vector *closest;
	float howClose;

	switch (_pickMode)
	{
		case Normal:
			closest = _fkPop->getVectors()->closest(
				x, y, getPlot(), &howClose);

			if (closest)
				_edtData = (FkData *) closest->getData();

			if (closest && howClose <= FAIRLY_CLOSE
				&& _edtData->getFkType() == Fan)
			{
				int i1, i2;
				closest->closestIndices(x, y, &i1, &i2,
					getPlot());

				_edtIndex = (i1 % 2) ? i2 : i1;

				getFanValues(x, y, &xFan[0], &yFan[0]);
				xFan[1] = yFan[1] = 0.0F;
				_rbnData = new VectData(2, xFan, yFan);
				_rbnVect = _vectors->add(_rbnData, "red", 2,
					True);

				_pickMode = EditFan;
			}
			else
			{
				doBeep();
				ignoreActions();
			}

			break;
		case InsertFan:
		case InsertFanSym:
			if (_insData->getNumPts() > 2)
			{
				_insData->remove(_insData->getNumPts() - 2, 2);

				if (_pickMode == InsertFanSym)
				{
					_pickMode = InsertFan;
					changeHelpToken("FK_PICK2", FALLBACK2);
				}
				else if (_insData->getNumPts() == 4)
				{
					float wn[2];
					wn[0] = _insData->getX(0);
					wn[1] = _insData->getX(2);

					if (wn[0] * wn[1] > 0.0F)
					{
						_pickMode = InsertFanSym;
						changeHelpToken("FK_PICK5",
							FALLBACK5);
					}
				}
				
			}
			else
			{
				_vectors->remove(_insVect);
				delete _insData;
				_pickMode = Normal;
				changeHelpToken("FK_PICK1", FALLBACK1);
			}
			ignoreActions();
			break;
		case EditFan:
			assert(False);
			break;
		case InsertPoly:
		case EditPoly:
			doBeep();
			ignoreActions();
			break;
		default:
			assert(False);
	}
}

void FkPick::cntlButtonOnePress(int x, int y)
{
	Vector *closest;
	float howClose;
	FkData *data;

	switch (_pickMode)
	{
		case Normal:
			closest = _fkPop->getVectors()->closest(
				x, y, getPlot(), &howClose);

			if (closest)
				data = (FkData *) closest->getData();

			if (closest && howClose <= FAIRLY_CLOSE
				&& data->getFkType() == Fan)
			{
				_fkPop->getVectors()->remove(closest);
				_fkPop->getData   ()->remove(data   );
				delete data;

				char fixedInfo[FIXED_INFO_SIZE];
				if (_fkPop->getData()->writeFile(
					_fkPop->getFilePair()->
						workingFilename(),
					fixedInfo))
				{
					new SLErrorPop(_fkPop,
						"FK Picking Error", fixedInfo);
				}
			}
			else
			{
				doBeep();
			}
			ignoreActions();
			break;
		case InsertFan:
		case InsertFanSym:
			_vectors->remove(_insVect);
			delete _insData;
			_pickMode = Normal;
			changeHelpToken("FK_PICK1", FALLBACK1);
			ignoreActions();
			break;
		case EditFan:
			assert(False);
			break;
		case InsertPoly:
		case EditPoly:
			doBeep();
			ignoreActions();
			break;
		default:
			assert(False);
	}
}

void FkPick::noModButtonOneMotion(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float xFan, yFan;

	switch (_pickMode)
	{
		case Normal:
		case InsertFan:
		case InsertFanSym:
			getFanValues(x2, y2, &xFan, &yFan);
			_rbnData->replace(0, 1, &xFan, &yFan);
			break;
		case InsertPoly:
		case EditFan:
		case EditPoly:
			assert(False);
			break;
		default:
			assert(False);
	}
}

void FkPick::shiftButtonOneMotion(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float xFan, yFan;

	switch (_pickMode)
	{
		case EditFan:
			getFanValues(x2, y2, &xFan, &yFan);
			_rbnData->replace(0, 1, &xFan, &yFan);
			break;
		case Normal:
		case InsertFan:
		case InsertFanSym:
		case InsertPoly:
		case EditPoly:
			assert(False);
			break;
		default:
			assert(False);
	}
}

void FkPick::cntlButtonOneMotion(int /*x1*/, int /*x2*/, int /*y1*/, int /*y2*/)
{
	switch (_pickMode)
	{
		case Normal:
		case InsertFan:
		case InsertFanSym:
		case InsertPoly:
		case EditFan:
		case EditPoly:
			assert(False);
			break;
		default:
			assert(False);
	}
}

void FkPick::noModButtonOneRelease(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float xFan[2], yFan[2];

	switch (_pickMode)
	{
		case Normal:
			_vectors->remove(_rbnVect);
			delete _rbnData;
			getFanValues(x2, y2, &xFan[0], &yFan[0]);
			xFan[1] = yFan[1] = 0.0F;
			_insData =  new VectData(2, xFan, yFan);
			_insVect = _vectors->add(_insData, "red", 2);
			_insVect->makeVisible();
			_pickMode = InsertFan;
			changeHelpToken("FK_PICK2", FALLBACK2);
			break;
		case InsertFan:
		case InsertFanSym:
			_vectors->remove(_rbnVect);
			delete _rbnData;
			getFanValues(x2, y2, &xFan[0], &yFan[0]);
			xFan[1] = yFan[1] = 0.0F;
			if (_insData->getNumPts() == 6)
			{
				_vectors->remove(_insVect);

				float h1, h2;
				if (_fkPop->getDisplayedHeaders(&h1, &h2))
				{
					new SLErrorPop(_fkPop,
						"FK Picking Error",
                          "Display has differing values for selected headers");
				}
				else
				{
					float wn[4], freq[4];
					for (int i = 0; i < 3; i++)
					{
						wn  [i] = _insData->getX(2 * i);
						freq[i] = _insData->getY(2 * i);
					}
					wn  [3] = xFan[0];
					freq[3] = yFan[0];

					FkFanData *data = new FkFanData(
						_fkPop->getData(), h1, h2,
						wn, freq, _fkPop->getCutPass());
					_fkPop->getData()->addSorted(data);

					Vector *v = _fkPop->getVectors()->add(
						data, _fkPop->nextColor(),2);
					v->makeVisible();

					char fixedInfo[FIXED_INFO_SIZE];
					if (_fkPop->getData()->writeFile(
						_fkPop->getFilePair()->
							workingFilename(),
						fixedInfo))
					{
						new SLErrorPop(_fkPop,
							"FK Picking Error",
							fixedInfo);
					}
				}

				delete _insData;
				_pickMode = Normal;
				changeHelpToken("FK_PICK1", FALLBACK1);
			}
			else if (_insData->getNumPts() == 2)
			{
				_insData->insert(_insData->getNumPts(), 2,
						xFan, yFan);

				float wn[2];
				wn[0] = _insData->getX(0);
				wn[1] = _insData->getX(2);

				if (wn[0] * wn[1] > 0.0F
					&& _pickMode == InsertFan)
				{
					_pickMode = InsertFanSym;
					changeHelpToken("FK_PICK5", FALLBACK5);
				}
				else if (wn[0] * wn[1] <= 0.0F
					&& _pickMode == InsertFanSym)
				{
					_pickMode = InsertFan;
					changeHelpToken("FK_PICK2", FALLBACK2);
				}

			}
			else
			{
				_insData->insert(_insData->getNumPts(), 2,
						xFan, yFan);

				if (_pickMode == InsertFanSym)
				{
					_pickMode = InsertFan;
					changeHelpToken("FK_PICK2", FALLBACK2);
				}
			}

			break;
		case InsertPoly:
		case EditFan:
		case EditPoly:
			assert(False);
			break;
		default:
			assert(False);
	}
}

void FkPick::shiftButtonOneRelease(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float xFan, yFan;
	char fixedInfo[FIXED_INFO_SIZE];

	switch (_pickMode)
	{
		case EditFan:
			_vectors->remove(_rbnVect);
			delete _rbnData;
			getFanValues(x2, y2, &xFan, &yFan);
			((FkFanData *) _edtData)->replace(_edtIndex,
				xFan, yFan);

			if (_fkPop->getData()->writeFile(
				_fkPop->getFilePair()-> workingFilename(),
				fixedInfo))
			{
				new SLErrorPop(_fkPop, "FK Picking Error",
					fixedInfo);
			}

			_pickMode = Normal;
			break;
		case Normal:
		case InsertFan:
		case InsertFanSym:
		case InsertPoly:
		case EditPoly:
			assert(False);
			break;
		default:
			assert(False);
	}
}

void FkPick::cntlButtonOneRelease(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	switch (_pickMode)
	{
		case Normal:
		case InsertFan:
		case InsertFanSym:
		case InsertPoly:
		case EditFan:
		case EditPoly:
			assert(False);
			break;
		default:
			assert(False);
	}
}

void FkPick::noModButtonTwoPress(int x, int y)
{
	float xRbn[3], yRbn[3];
	float howClose;

	switch (_pickMode)
	{
		case Normal:
			if (canInsertPoly())
			{
				/* wait for release */
			}
			else
			{
				doBeep();
				ignoreActions();
			}
			break;
		case InsertFan:
			doBeep();
			ignoreActions();
			break;
		case InsertFanSym:
			_vectors->remove(_insVect);

			float h1, h2;
			if (_fkPop->getDisplayedHeaders(&h1, &h2))
			{
				new SLErrorPop(_fkPop, "FK Picking Error",
                          "Display has differing values for selected headers");
			}
			else
			{
				float wn[4], freq[4];
				for (int i = 0; i < 2; i++)
				{
					wn  [i    ] = _insData->getX(2 * i);
					wn  [i + 2] = -wn  [i];
					freq[i    ] = _insData->getY(2 * i);
					freq[i + 2] =  freq[i];
				}

				FkFanData *data = new FkFanData(
					_fkPop->getData(), h1, h2,
					wn, freq, _fkPop->getCutPass());
				_fkPop->getData()->addSorted(data);

				Vector *v = _fkPop->getVectors()->add(
					data, _fkPop->nextColor(),2);
				v->makeVisible();

				char fixedInfo[FIXED_INFO_SIZE];
				if (_fkPop->getData()->writeFile(
					_fkPop->getFilePair()->
						workingFilename(),
						fixedInfo))
				{
					new SLErrorPop(_fkPop,
						"FK Picking Error",
						fixedInfo);
				}
			}

			delete _insData;
			_pickMode = Normal;
			changeHelpToken("FK_PICK1", FALLBACK1);
			ignoreActions();
			break;
		case InsertPoly:
			xRbn[0] = _insData->getX(_insData->getNumPts() - 1);
			yRbn[0] = _insData->getY(_insData->getNumPts() - 1);
			xRbn[1] = getPlot()->xWC(x);
			yRbn[1] = getPlot()->yWC(y);
			_rbnData = new VectData(2, xRbn, yRbn);
			_rbnVect = _vectors->add(_rbnData, "red", 2, True);
			break;
		case EditFan:
			assert(False);
			break;
		case EditPoly:
			_edtIndex = _edtVect->closestVertex(x, y, getPlot(),
				&howClose);
			if (howClose <= FAIRLY_CLOSE)
			{
				xRbn[1] = getPlot()->xWC(x);
				yRbn[1] = getPlot()->yWC(y);

				if (_edtIndex == 0
				 || _edtIndex == _edtData->getNumPts() - 1)
				{
					xRbn[0] = _edtData->getX(1);
					yRbn[0] = _edtData->getY(1);
					xRbn[2] = _edtData->getX(
						_edtData->getNumPts() - 2);
					yRbn[2] = _edtData->getY(
						_edtData->getNumPts() - 2);
				}
				else
				{
					xRbn[0] = _edtData->getX(_edtIndex - 1);
					yRbn[0] = _edtData->getY(_edtIndex - 1);
					xRbn[2] = _edtData->getX(_edtIndex + 1);
					yRbn[2] = _edtData->getY(_edtIndex + 1);
				}

				_rbnData = new VectData(3, xRbn, yRbn);
				_rbnVect = _vectors->add(_rbnData, "red", 2,
					True);
			}
			else
			{
				doBeep();
				ignoreActions();
			}
			break;
		default:
			assert(False);
	}
}

void FkPick::shiftButtonTwoPress(int x, int y)
{
	float xRbn[3], yRbn[3];
	float howClose;

	switch (_pickMode)
	{
		case Normal:
			_edtVect = _fkPop->getVectors()->closest(
				x, y, getPlot(), &howClose);

			if (_edtVect)
				_edtData = (FkData *) _edtVect->getData();

			if (_edtVect && howClose <= FAIRLY_CLOSE
				&& _edtData->getFkType() == Polygon)
			{
				_vectColor = new
					char[strlen(_edtVect->getColor()) + 1];
				strcpy(_vectColor,  _edtVect->getColor());
				_edtVect->setColor("red");
				_pickMode = EditPoly;
				changeHelpToken("FK_PICK4", FALLBACK4);
			}
			else
			{
				doBeep();
			}

			ignoreActions();
			break;
		case InsertFan:
		case InsertFanSym:
			doBeep();
			ignoreActions();
			break;
		case InsertPoly:
			if (_insData->getNumPts() > 1)
			{
				_insData->remove(_insData->getNumPts() - 1, 1);
			}
			else
			{
				_vectors->remove(_insVect);
				delete _insData;
				_pickMode = Normal;
				changeHelpToken("FK_PICK1", FALLBACK1);
			}
			ignoreActions();
			break;
		case EditFan:
			assert(False);
			break;
		case EditPoly:
			if (canInsertPolyPt()
			 && _edtVect == _fkPop->getVectors()->closest(
				x, y, getPlot(), &howClose)
			 && howClose <= FAIRLY_CLOSE)
			{
				int i1, i2;
				_edtVect->closestIndices(x, y, &i1, &i2,
					getPlot());
				_edtIndex = (i1 > i2) ? i1 : i2;

				xRbn[0] = _edtData->getX(i1);
				yRbn[0] = _edtData->getY(i1);
				xRbn[1] = getPlot()->xWC(x);
				yRbn[1] = getPlot()->yWC(y);
				xRbn[2] = _edtData->getX(i2);
				yRbn[2] = _edtData->getY(i2);

				_rbnData = new VectData(3, xRbn, yRbn);
				_rbnVect = _vectors->add(_rbnData, "red", 2,
					True);
			}
			else
			{
				doBeep();
				ignoreActions();
			}
			break;
		default:
			assert(False);
	}
}

void FkPick::cntlButtonTwoPress(int x, int y)
{
	Vector *closest;
	float howClose;
	FkData *data;

	switch (_pickMode)
	{
		case Normal:
			closest = _fkPop->getVectors()->closest(
				x, y, getPlot(), &howClose);

			if (closest)
				data = (FkData *) closest->getData();

			if (closest && howClose <= FAIRLY_CLOSE
				&& data->getFkType() == Polygon)
			{
				_fkPop->getVectors()->remove(closest);
				_fkPop->getData   ()->remove(data   );
				delete data;

				char fixedInfo[FIXED_INFO_SIZE];
				if (_fkPop->getData()->writeFile(
					_fkPop->getFilePair()->
						workingFilename(),
					fixedInfo))
				{
					new SLErrorPop(_fkPop,
						"FK Picking Error", fixedInfo);
				}
			}
			else
			{
				doBeep();
			}
			ignoreActions();
			break;
		case InsertFan:
		case InsertFanSym:
			doBeep();
			ignoreActions();
			break;
		case InsertPoly:
			_vectors->remove(_insVect);
			delete _insData;
			_pickMode = Normal;
			changeHelpToken("FK_PICK1", FALLBACK1);
			ignoreActions();
			break;
		case EditFan:
			assert(False);
			break;
		case EditPoly:
			_edtIndex = _edtVect->closestVertex(x, y, getPlot(),
				&howClose);
			if (howClose <= FAIRLY_CLOSE)
			{
				((FkPolyData *) _edtData)->remove(_edtIndex);

				if (0 == _edtData->getNumPts())
				{
					_fkPop->getVectors()->remove(_edtVect);
					_fkPop->getData   ()->remove(_edtData);
					delete _edtData;

					char fixedInfo[FIXED_INFO_SIZE];
					if (_fkPop->getData()->writeFile(
						_fkPop->getFilePair()->
							workingFilename(),
						fixedInfo))
					{
						new SLErrorPop(_fkPop,
							"FK Picking Error",
							fixedInfo);
					}
					_pickMode = Normal;
					changeHelpToken("FK_PICK1", FALLBACK1);
				}
			}
			else
			{
				doBeep();
			}
			ignoreActions();
			break;
		default:
			assert(False);
	}
}

void FkPick::noModButtonTwoMotion(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float xRbn, yRbn;

	switch (_pickMode)
	{
		case Normal:
			/* wait for release */
			break;
		case InsertPoly:
		case EditPoly:
			xRbn = getPlot()->xWC(x2);
			yRbn = getPlot()->yWC(y2);
			_rbnData->replace(1, 1, &xRbn, &yRbn);
			break;
		case InsertFan:
		case InsertFanSym:
		case EditFan:
			assert(False);
			break;
		default:
			assert(False);
	}
}

void FkPick::shiftButtonTwoMotion(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float xRbn, yRbn;

	switch (_pickMode)
	{
		case Normal:
		case InsertFan:
		case InsertFanSym:
		case InsertPoly:
		case EditFan:
			assert(False);
			break;
		case EditPoly:
			xRbn = getPlot()->xWC(x2);
			yRbn = getPlot()->yWC(y2);
			_rbnData->replace(1, 1, &xRbn, &yRbn);
			break;
		default:
			assert(False);
	}
}

void FkPick::cntlButtonTwoMotion(int /*x1*/, int /*x2*/, int /*y1*/, int /*y2*/)
{
	switch (_pickMode)
	{
		case Normal:
		case InsertFan:
		case InsertFanSym:
		case InsertPoly:
		case EditFan:
		case EditPoly:
			assert(False);
			break;
		default:
			assert(False);
	}
}

void FkPick::noModButtonTwoRelease(int /*x1*/, int x2, int /*y1*/, int y2)
{
	float wn, freq;
	int ix, closed;
	float d;
	char fixedInfo[FIXED_INFO_SIZE];

	switch (_pickMode)
	{
		case Normal:
			wn   = getPlot()->xWC(x2);
			freq = getPlot()->yWC(y2);
			_insData =  new VectData(1, &wn, &freq);
			_insVect = _vectors->add(_insData, "red", 2);
			_insVect->makeVisible();
			_pickMode = InsertPoly;
			changeHelpToken("FK_PICK3", FALLBACK3);
			break;
		case InsertFan:
		case InsertFanSym:
		case EditFan:
			assert(False);
			break;
		case InsertPoly:
			_vectors->remove(_rbnVect);
			delete _rbnData;

			ix = _insVect->closestVertex(x2, y2, getPlot(), &d);
			closed = (ix == 0) && (d <= REALLY_CLOSE);
			if (closed || insertedPolyDone())
			{
				_vectors->remove(_insVect);

				if (!closed)
				{
					wn   = getPlot()->xWC(x2);
					freq = getPlot()->yWC(y2);
					_insData->insert(_insData->getNumPts(),
						1, &wn, &freq);
				}

				float h1, h2;
				if (_fkPop->getDisplayedHeaders(&h1, &h2))
				{
					new SLErrorPop(_fkPop,
						"FK Picking Error",
                          "Display has differing values for selected headers");
				}
				else
				{
					int numPts = _insData->getNumPts();
					float *w = new float[numPts];
					float *f = new float[numPts];

					for (int i = 0; i < numPts; i++)
					{
						w[i] = _insData->getX(i);
						f[i] = _insData->getY(i);
					}

					FkPolyData *data = new FkPolyData(
						_fkPop->getData(), h1, h2,
						numPts, w, f,
						_fkPop->getCutPass());
					_fkPop->getData()->addSorted(data);

					delete [] w;
					delete [] f;

					Vector *v = _fkPop->getVectors()->add(
						data, _fkPop->nextColor(),2);
					v->makeVisible();

					char fixedInfo[FIXED_INFO_SIZE];
					if (_fkPop->getData()->writeFile(
						_fkPop->getFilePair()->
							workingFilename(),
						fixedInfo))
					{
						new SLErrorPop(_fkPop,
							"FK Picking Error",
							fixedInfo);
					}
				}

				delete _insData;
				_pickMode = Normal;
				changeHelpToken("FK_PICK1", FALLBACK1);
			}
			else
			{
				wn   = getPlot()->xWC(x2);
				freq = getPlot()->yWC(y2);
				_insData->insert(_insData->getNumPts(), 1,
					&wn, &freq);
			}
			break;
		case EditPoly:
			_vectors->remove(_rbnVect);
			delete _rbnData;

			((FkPolyData *) _edtData)->replace(_edtIndex,
				getPlot()->xWC(x2), getPlot()->yWC(y2));

			if (_fkPop->getData()->writeFile(
				_fkPop->getFilePair()->workingFilename(),
				fixedInfo))
			{
				new SLErrorPop(_fkPop, "FK Picking Error",
					fixedInfo);
			}

			break;
		default:
			assert(False);
	}
}

void FkPick::shiftButtonTwoRelease(int /*x1*/, int x2, int /*y1*/, int y2)
{
	char fixedInfo[FIXED_INFO_SIZE];

	switch (_pickMode)
	{
		case Normal:
		case InsertFan:
		case InsertFanSym:
		case InsertPoly:
		case EditFan:
			assert(False);
			break;
		case EditPoly:
			_vectors->remove(_rbnVect);
			delete _rbnData;

			((FkPolyData *) _edtData)->insert(_edtIndex,
				getPlot()->xWC(x2), getPlot()->yWC(y2));

			if (_fkPop->getData()->writeFile(
				_fkPop->getFilePair()->workingFilename(),
				fixedInfo))
			{
				new SLErrorPop(_fkPop, "FK Picking Error",
					fixedInfo);
			}

			break;
		default:
			assert(False);
	}
}

void FkPick::cntlButtonTwoRelease(int /*x1*/, int /*x2*/,
	int /*y1*/, int /*y2*/)
{
	switch (_pickMode)
	{
		case Normal:
		case InsertFan:
		case InsertFanSym:
		case InsertPoly:
		case EditFan:
		case EditPoly:
			assert(False);
			break;
		default:
			assert(False);
	}
}

void FkPick::getFanValues(int xDc, int yDc, float *xFan, float *yFan)
{
	float wn   = getPlot()->xWC(xDc);
	float freq = getPlot()->yWC(yDc);
	float slope;

	if (freq != 0.0F)
		slope = wn / freq;
	else if (wn == 0.0F)
		slope = 0.0F;
	else if (wn >  0.0F)
		slope = (float) LONG_MAX;
	else
		slope = (float) LONG_MIN;

	int minWN, maxWN, nyquistWN;
	float minFreq, maxFreq, freqPerSec;
	_fkPop->getData()->getParams(&minWN, &maxWN, &nyquistWN,
		&minFreq, &maxFreq, &freqPerSec);

	if      (slope <  0.0F)
	{
		*xFan = ((float) minWN / slope > maxFreq) ?
			slope * maxFreq : (float) minWN;
		*yFan = ((float) minWN / slope > maxFreq) ?
			maxFreq : (float) minWN / slope;
	}
	else if (slope == 0.0F)
	{
		*xFan = 0.0F;
		*yFan = maxFreq;
	}
	else
	{
		*xFan = ((float) maxWN / slope > maxFreq) ?
			slope * maxFreq : (float) maxWN;
		*yFan = ((float) maxWN / slope > maxFreq) ?
			maxFreq : (float) maxWN / slope;
	}
}

int FkPick::canInsertFan()
{
#ifdef NUM_FANS
	int retval;

	float hw1, hw2;
	if ((0 == _fkPop->getDisplayedHeaders(&hw1, &hw2)))
	{
		void *p;
		if (_fkPop->getData()->top(hw1, hw2, &p))
			retval = 0;
		else
			retval = 1;
	}
	else
	{
		retval = 1;
	}

	return retval;
#else
	return 1;
#endif
}

int FkPick::canInsertPoly()
{
#ifdef NUM_POLYS
	int retval;

	float hw1, hw2;
	if ((0 == _fkPop->getDisplayedHeaders(&hw1, &hw2)))
	{
		FkData *ptr;
		void *p;
		int numPolys;
		for (ptr = _fkPop->getData()->top (hw1, hw2, &p),
		     retval = 1, numPolys = 0;
		     ptr && retval;
		     ptr = _fkPop->getData()->next(hw1, hw2, &p))
		{
			if (ptr->getFkType() == Fan)
			{
				retval = 0;
			}
			else
			{
				assert(ptr->getFkType() == Polygon);
				if (++numPolys == NUM_POLYS)
					retval = 0;
			}
		}
	}
	else
	{
		retval = 1;
	}

	return retval;
#else
	return 1;
#endif
}

int FkPick::canInsertPolyPt()
{
#ifdef NUM_POLY_PTS
	return (_edtData->getNumPts() < NUM_POLY_PTS);
#else
	return 1;
#endif
}

int FkPick::insertedPolyDone()
{
#ifdef NUM_POLY_PTS
	return NUM_POLY_PTS - 2 == _insData->getNumPts();
#else
	return 0;
#endif
}
