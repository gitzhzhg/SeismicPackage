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
#include "vect/v3d_cage.hh"
#include "vect/ll_v3d_data.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "vect/trans_3d_to_2d.hh"
#include "oprim/static_utils.hh"

#include <assert.h>

static char *labelFont = "-*-*-*-*-*-*-*-160-*-*-*-*-iso8859-*";

#define CAGE_MIN -0.5
#define CAGE_MAX  0.5
#define CAGE_RNG  (CAGE_MAX - CAGE_MIN)
#define CAGE_MID  ((CAGE_MAX + CAGE_MIN) / 2.0)
#define CAGE_AXE  (CAGE_MIN + (CAGE_MAX - CAGE_MIN) / 4.0)
#define CAGE_OFF  (CAGE_RNG / 20.0)
#define ADJUST(x) ((CAGE_MIN == (x)) ? (x) - CAGE_OFF \
				    : (CAGE_MAX == (x)) ? (x) + CAGE_OFF \
							: (x))
#define NUM_HANDLES 4

V3dCage::V3dCage(V3dDataLinkedList *mainData, int numXAnnos, int numYAnnos,
	int numZAnnos) : _mainData(mainData)
{
	float degreesZ, degreesY;
	_mainData->getAngles(&degreesZ, &degreesY);

	float xOffset, xFactor, yOffset, yFactor, zOffset, zFactor;
	_mainData->getScale(&xOffset, &xFactor, &yOffset, &yFactor,
		&zOffset, &zFactor);

	float xExp, yExp, zExp;
	_mainData->getExpansion(&xExp, &yExp, &zExp);

	constructorHelper(degreesZ, degreesY, xOffset, xFactor,
		yOffset, yFactor, zOffset, zFactor, xExp, yExp, zExp,
		numXAnnos, numYAnnos, numZAnnos);
}

V3dCage::V3dCage(float degreesZ, float degreesY, float xOffset , float xFactor ,
		 float yOffset , float yFactor , float zOffset , float zFactor ,
		 float xExp    , float yExp    , float zExp    ,
		 int numXAnnos , int numYAnnos , int numZAnnos )
	: _mainData((V3dDataLinkedList *) NULL)
{
	constructorHelper(degreesZ, degreesY, xOffset, xFactor,
		yOffset, yFactor, zOffset, zFactor, xExp, yExp, zExp,
		numXAnnos, numYAnnos, numZAnnos);
}

void V3dCage::constructorHelper(float degreesZ, float degreesY,
				float xOffset , float xFactor ,
				float yOffset , float yFactor ,
				float zOffset , float zFactor ,
				float xExp    , float yExp    , float zExp,
		 		int numXAnnos , int numYAnnos , int numZAnnos)
{
	assert(numXAnnos >= 0 && numYAnnos >= 0 && numZAnnos >= 0);

	_degreesZ  = degreesZ ;
	_degreesY  = degreesY ;
	_xOffset   = xOffset  ;
	_xFactor   = xFactor  ;
	_yOffset   = yOffset  ;
	_yFactor   = yFactor  ;
	_zOffset   = zOffset  ;
	_zFactor   = zFactor  ;
	_xExp      = xExp     ;
	_yExp      = yExp     ;
	_zExp      = zExp     ;
	_numXAnnos = numXAnnos;
	_numYAnnos = numYAnnos;
	_numZAnnos = numZAnnos;

	_vectors         = new SeisVectLinkedList();
	_rotationHandles = new SeisVectLinkedList();

 	_cageData = new V3dDataLinkedList(_degreesZ, _degreesY);

	showCage();

	float *x, *y, *z;
	float step;
	int limit, i;

	limit  = (_numXAnnos >= 2) ? _numXAnnos : 2;
	x      = new float   [limit     ];
	y      = new float   [limit     ];
	z      = new float   [limit     ];
	_xAnno = new Vector *[_numXAnnos];

	step = (_numXAnnos > 1) ? CAGE_RNG / (float) (_numXAnnos - 1)
				: CAGE_RNG;

	for (i = 0; i < limit; i++)
	{
		x[i] = CAGE_MIN + (float) i * step;
		y[i] = CAGE_MAX;
		z[i] = CAGE_MIN;
	}

	showRange(x, y, z, "blue",
		CAGE_MIN / _xFactor / _xExp + _xOffset,
		CAGE_MAX / _xFactor / _xExp + _xOffset,
		_xAnno, _numXAnnos);

	delete [] x;
	delete [] y;
	delete [] z;

	limit  = (_numYAnnos >= 2) ? _numYAnnos : 2;
	x      = new float   [limit     ];
	y      = new float   [limit     ];
	z      = new float   [limit     ];
	_yAnno = new Vector *[_numYAnnos];

	step = (_numYAnnos > 1) ? CAGE_RNG / (float) (_numYAnnos - 1)
				: CAGE_RNG;

	for (i = 0; i < limit; i++)
	{
		x[i] = CAGE_MIN;
		y[i] = CAGE_MIN + (float) i * step;
		z[i] = CAGE_MAX;
	}

	showRange(x, y, z, "magenta",
		CAGE_MIN / _yFactor / _yExp + _yOffset,
		CAGE_MAX / _yFactor / _yExp + _yOffset,
		_yAnno, _numYAnnos);

	delete [] x;
	delete [] y;
	delete [] z;

	limit  = (_numZAnnos >= 2) ? _numZAnnos : 2;
	x      = new float   [limit     ];
	y      = new float   [limit     ];
	z      = new float   [limit     ];
	_zAnno = new Vector *[_numZAnnos];

	step = (_numZAnnos > 1) ? CAGE_RNG / (float) (_numZAnnos - 1)
				: CAGE_RNG;

	for (i = 0; i < limit; i++)
	{
		x[i] = CAGE_MAX;
		y[i] = CAGE_MIN;
		z[i] = CAGE_MIN + (float) i * step;
	}

	showRange(x, y, z, "green",
		CAGE_MIN / _zFactor / _zExp + _zOffset,
		CAGE_MAX / _zFactor / _zExp + _zOffset,
		_zAnno, _numZAnnos);

	delete [] x;
	delete [] y;
	delete [] z;

	showDirection("blue", "magenta", "green");

	showRotationHandles();
}

V3dCage::~V3dCage()
{
	delete _vectors        ;
	delete _rotationHandles;
	delete _cageData       ;
}

void V3dCage::addPlot(class SeisPlot *plot)
{
	_vectors        ->addPlot(plot);
	_rotationHandles->addPlot(plot);
}

void V3dCage::updateAngles()
{
	assert(_mainData);

	float degreesZ, degreesY;
	_mainData->getAngles(&degreesZ, &degreesY);

	updateAngles(degreesZ, degreesY);
}

void V3dCage::updateAngles(float degreesZ, float degreesY)
{
	_degreesZ = degreesZ;
	_degreesY = degreesY;

	_cageData->setAngles(_degreesZ, _degreesY);
}

void V3dCage::updateLabels()
{
	assert(_mainData);

	float xOffset, xFactor, yOffset, yFactor, zOffset, zFactor;
	_mainData->getScale(&xOffset, &xFactor, &yOffset, &yFactor,
		&zOffset, &zFactor);

	float xExp, yExp, zExp;
	_mainData->getExpansion(&xExp, &yExp, &zExp);

	updateLabels(xOffset, xFactor, yOffset, yFactor, zOffset, zFactor,
		xExp, yExp, zExp);
}

void V3dCage::updateLabels(float xOffset, float xFactor,
			   float yOffset, float yFactor,
			   float zOffset, float zFactor,
			   float xExp   , float yExp   , float zExp)
{
	_xOffset = xOffset;
	_xFactor = xFactor;
	_yOffset = yOffset;
	_yFactor = yFactor;
	_zOffset = zOffset;
	_zFactor = zFactor;
	_xExp    = xExp   ;
	_yExp    = yExp   ;
	_zExp    = zExp   ;

	drawLabels(CAGE_MIN / _xFactor / _xExp + _xOffset, 
		   CAGE_MAX / _xFactor / _xExp + _xOffset,
		   CAGE_MIN / _yFactor / _yExp + _yOffset,
		   CAGE_MAX / _yFactor / _yExp + _yOffset,
		   CAGE_MIN / _zFactor / _zExp + _zOffset,
		   CAGE_MAX / _zFactor / _zExp + _zOffset);
}

void V3dCage::updateLabels(float xMin, float xMax, float yMin, float yMax,
	float zMin, float zMax)
{
	_xOffset = (xMax + xMin) / 2.0;
	_xFactor = 1.0 / (xMax - xMin);
	_yOffset = (yMax + yMin) / 2.0;
	_yFactor = 1.0 / (yMax - yMin);
	_zOffset = (zMax + zMin) / 2.0;
	_zFactor = 1.0 / (zMax - zMin);
	_xExp    = 1.0;
	_yExp    = 1.0;
	_zExp    = 1.0;

	drawLabels(xMin, xMax, yMin, yMax, zMin, zMax);
}

int V3dCage::getClosestRotationHandle(int xDc, int yDc, PlotBase *plot,
	float *x3D, float *y3D, float *z3D, float *x2D, float *y2D, float *z2D,
	float tolerance)
{
	int retval;
	float distPtr;

	Vector *vector = _rotationHandles->closest(xDc, yDc, plot, &distPtr);
	assert(vector);

	if (distPtr <= tolerance)
	{
		/*
		 * A little downcasting now and then,
		 * is done even by the best of men.
		 */
		*x3D = ((Trans3Dto2D *) vector->getData())->getData()->getX(0);
		*y3D = ((Trans3Dto2D *) vector->getData())->getData()->getY(0);
		*z3D = ((Trans3Dto2D *) vector->getData())->getData()->getZ(0);

		*x2D = vector->getData()->getX(0);
		*y2D = vector->getData()->getY(0);
		*z2D = vector->getData()->getZ(0);

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

void V3dCage::drawLabels(float xMin, float xMax, float yMin, float yMax,
	float zMin, float zMax)
{
	char buff[80];	/* sloopy memory allocation. */
	float step;
	int i;

	step = (_numXAnnos > 1) ? (xMax - xMin) / (float) (_numXAnnos - 1)
				: 0.0;

	for (i = 0; i < _numXAnnos; i++)
	{
		sprintf(buff, "%g", xMin + step * (float) i);
		_xAnno[i]->setLabel(buff, labelFont);
	}

	step = (_numYAnnos > 1) ? (yMax - yMin) / (float) (_numYAnnos - 1)
				: 0.0;

	for (i = 0; i < _numYAnnos; i++)
	{
		sprintf(buff, "%g", yMin + step * (float) i);
		_yAnno[i]->setLabel(buff, labelFont);
	}

	step = (_numZAnnos > 1) ? (zMax - zMin) / (float) (_numZAnnos - 1)
				: 0.0;

	for (i = 0; i < _numZAnnos; i++)
	{
		sprintf(buff, "%g", zMin + step * (float) i);
		_zAnno[i]->setLabel(buff, labelFont);
	}
}

void V3dCage::showCage()
{
	static float cageX[] = {
		CAGE_MAX, CAGE_MAX, CAGE_MAX, CAGE_AXE,
		CAGE_MIN, CAGE_MIN, CAGE_MAX, CAGE_MAX,
			CAGE_MIN, CAGE_MIN, CAGE_MIN,
	};
	int pts1 = 4;
	int pts2 = (int) (sizeof(cageX) / sizeof(float)) - pts1;

	static float cageY[] = {
		CAGE_MAX, CAGE_MAX, CAGE_MIN, CAGE_MIN,
		CAGE_MIN, CAGE_MIN, CAGE_MIN, CAGE_MAX,
			CAGE_MAX, CAGE_MAX, CAGE_AXE,
	};
	assert((int) (sizeof(cageY) / sizeof(float)) == pts1 + pts2);

	static float cageZ[] = {
		CAGE_MAX, CAGE_MIN, CAGE_MIN, CAGE_MIN,
		CAGE_AXE, CAGE_MAX, CAGE_MAX, CAGE_MAX,
			CAGE_MAX, CAGE_MIN, CAGE_MIN,
	};
	assert((int) (sizeof(cageZ) / sizeof(float)) == pts1 + pts2);

	(_vectors->add(
		_cageData->add(pts1, cageX       , cageY       , cageZ       ),
		"black", 1))->makeVisible();

	(_vectors->add(
		_cageData->add(pts2, cageX + pts1, cageY + pts1, cageZ + pts1),
		"black", 1))->makeVisible();

	       float extraX[5];
	static float extraY[5] =
		{ CAGE_MIN, CAGE_MAX, CAGE_MAX, CAGE_MIN, CAGE_MIN };
	static float extraZ[5] =
		{ CAGE_MIN, CAGE_MIN, CAGE_MAX, CAGE_MAX, CAGE_MIN };

	float step;
	int i;

	if (_numXAnnos > 2)
	{
		step = CAGE_RNG / (float) (_numXAnnos - 1);

		for (i = 1; i < _numXAnnos - 1; i++)
		{
			extraX[0] = extraX[1] = extraX[2] = extraX[3]
				  = extraX[4] = CAGE_MIN + step * (float) i;

			(_vectors->add(
				_cageData->add(5, extraX, extraY, extraZ),
				"black", 1))->makeVisible();
		}
	}

	if (_numYAnnos > 2)
	{
		step = CAGE_RNG / (float) (_numYAnnos - 1);

		for (i = 1; i < _numYAnnos - 1; i++)
		{
			extraX[0] = extraX[1] = extraX[2] = extraX[3]
				  = extraX[4] = CAGE_MIN + step * (float) i;

			(_vectors->add(
				_cageData->add(5, extraY, extraX, extraZ),
				"black", 1))->makeVisible();
		}
	}

	if (_numZAnnos > 2)
	{
		step = CAGE_RNG / (float) (_numZAnnos - 1);

		for (i = 1; i < _numZAnnos - 1; i++)
		{
			extraX[0] = extraX[1] = extraX[2] = extraX[3]
				  = extraX[4] = CAGE_MIN + step * (float) i;

			(_vectors->add(
				_cageData->add(5, extraY, extraZ, extraX),
				"black", 1))->makeVisible();
		}
	}
}

void V3dCage::showRange(float *x, float *y, float *z, const char *color,
	float min, float max, Vector **labels, int numLabels)
{
	(_vectors->add(_cageData->add(((numLabels >= 2) ? numLabels : 2),
		x, y, z), color, 1)) ->makeVisible();

	char buff[80];	/* sloopy memory allocation. */
	float step = (numLabels > 1) ? (max - min) / (float) (numLabels - 1)
				     : 0.0;

	/*
	 * Note, array values passed to showRange are changed.
	 */
	for (int i = 0; i < numLabels; i++)
	{
		x[i] = ADJUST(x[i]);
		y[i] = ADJUST(y[i]);
		z[i] = ADJUST(z[i]);

		sprintf(buff, "%g", min + step * (float) i);
		labels[i] = _vectors->addLabel(_cageData->add(
			1, &x[i], &y[i], &z[i]), buff, color, labelFont);
		labels[i]->setLabelPlacement(CenterLeft);
		labels[i]->makeVisible();
	}
}

void V3dCage::showDirection(const char *xColor, const char *yColor,
	const char *zColor)
{
	float x[2], y[2], z[2];
	x[0] = CAGE_MIN;
	y[0] = CAGE_MIN;
	z[0] = CAGE_MIN;

	x[1] = CAGE_AXE;
	y[1] = CAGE_MIN;
	z[1] = CAGE_MIN;
	(_vectors->add(_cageData->add(2, x, y, z), xColor, 1))->makeVisible();

	x[1] += CAGE_OFF;
	(_vectors->addLabel(_cageData->add(1, x + 1, y + 1, z + 1),
		"X", xColor, labelFont))->makeVisible();

	x[1] = CAGE_MIN;
	y[1] = CAGE_AXE;
	z[1] = CAGE_MIN;
	(_vectors->add(_cageData->add(2, x, y, z), yColor, 1))->makeVisible();

	y[1] += CAGE_OFF;
	(_vectors->addLabel(_cageData->add(1, x + 1, y + 1, z + 1),
		"Y", yColor, labelFont))->makeVisible();

	x[1] = CAGE_MIN;
	y[1] = CAGE_MIN;
	z[1] = CAGE_AXE;
	(_vectors->add(_cageData->add(2, x, y, z), zColor, 1))->makeVisible();

	z[1] += CAGE_OFF;
	(_vectors->addLabel(_cageData->add(1, x + 1, y + 1, z + 1),
		"Z", zColor, labelFont))->makeVisible();
}

void V3dCage::showRotationHandles()
{
	static float x[NUM_HANDLES] ={ CAGE_MAX, CAGE_MIN, CAGE_MIN, CAGE_MAX };
	static float y[NUM_HANDLES] ={ CAGE_MAX, CAGE_MAX, CAGE_MIN, CAGE_MIN };
	static float z[NUM_HANDLES] ={ CAGE_MID, CAGE_MID, CAGE_MID, CAGE_MID };

	Vector *vector;

	for (int i = 0; i < NUM_HANDLES; i++)
	{
		vector = _rotationHandles->addLabel(
			_cageData->add(1, x + i, y + i, z + i),
			"R", "red", labelFont);
		vector->setLabelPlacement(DeadCenter);
		vector->makeVisible();
	}
}

void V3dCage::redisplay()
{
	_vectors->redisplay();
}

void V3dCage::setVisibility(int set)
{
	if (set)
	{
		_vectors        ->makeVisible();
		_rotationHandles->makeVisible();
	}
	else
	{
		Bool holding = SU::isHoldingVectors();

		if (!holding)
			SU::holdVectors();

		_vectors        ->makeInvisible();
		_rotationHandles->makeInvisible();

		if (!holding)
			SU::flushVectors();
	}
}
