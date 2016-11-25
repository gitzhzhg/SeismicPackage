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
#include "vect/rotate_3d.hh"
#include "vect/ll_v3d_data.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/vector.hh"
#include "vect/trans_3d_to_2d.hh"

#include <math.h>
#include <assert.h>

#define NEAR_ZERO(x) (assert(-1.0e-05 < (x) && 1.0e-05 > (x)))

#define CAGE_MIN -0.5
#define CAGE_MAX  0.5
#define CAGE_PARTS  4
#define CAGE_PTS_PER_PART 4

static float xCage[CAGE_PARTS][CAGE_PTS_PER_PART] = {
	{ CAGE_MIN, CAGE_MAX, CAGE_MAX, CAGE_MIN },
	{ CAGE_MAX, CAGE_MAX, CAGE_MAX, CAGE_MAX },
	{ CAGE_MAX, CAGE_MIN, CAGE_MIN, CAGE_MAX },
	{ CAGE_MIN, CAGE_MIN, CAGE_MIN, CAGE_MIN } };

static float yCage[CAGE_PARTS][CAGE_PTS_PER_PART] = {
	{ CAGE_MIN, CAGE_MIN, CAGE_MAX, CAGE_MAX },
	{ CAGE_MIN, CAGE_MIN, CAGE_MAX, CAGE_MAX },
	{ CAGE_MIN, CAGE_MIN, CAGE_MAX, CAGE_MAX },
	{ CAGE_MIN, CAGE_MIN, CAGE_MAX, CAGE_MAX } };

static float zCage[CAGE_PARTS][CAGE_PTS_PER_PART] = {
	{ CAGE_MIN, CAGE_MIN, CAGE_MIN, CAGE_MIN },
	{ CAGE_MIN, CAGE_MAX, CAGE_MAX, CAGE_MIN },
	{ CAGE_MAX, CAGE_MAX, CAGE_MAX, CAGE_MAX },
	{ CAGE_MAX, CAGE_MIN, CAGE_MIN, CAGE_MAX } };

float Rotate3D::_rads2Degs = 0.0;

Rotate3D::Rotate3D(float x3D, float y3D, float z3D,
	float /*x2D*/, float /*y2D*/, float z2D, float degreesZ, float degreesY)
{
	if (0.0 == _rads2Degs)
		_rads2Degs = 45.0 / (float) atan((double) 1.0);

	_from[0] = x3D;
	_from[1] = y3D;
	_from[2] = z3D;

	_distSquared = x3D * x3D + y3D * y3D + z3D * z3D;

	_z2DNegative = (z2D < 0.0);

	_vectors = new SeisVectLinkedList();
 	_data    = new V3dDataLinkedList(degreesZ, degreesY);

	for (int i = 0; i < CAGE_PARTS; i++)
		_vectors->add(_data->add(CAGE_PTS_PER_PART,
			xCage[i], yCage[i], zCage[i]), "red", 2, True);
}

Rotate3D::~Rotate3D()
{
	/*
	 * Remove rbn vectors individually since VectorLinkedList
	 * destructor expects no rbns.
	 */
	Vector *ptr;
	Vector *nextPtr = (Vector *) NULL;	/* Avoid compiler warning. */
	void *p;
	for (ptr = _vectors->top(&p); ptr; ptr = nextPtr)
	{
		nextPtr = _vectors->next(&p);
		_vectors->remove(ptr);
	}

	delete _vectors;

	delete _data   ;
}

void Rotate3D::addPlot(class SeisPlot *plot)
{
	_vectors->addPlot(plot);
}

void Rotate3D::update(float x2D, float y2D)
{
	float z2DSquared = _distSquared - x2D * x2D - y2D *y2D;
	float to[3];
	float degreesZ[2], degreesY[2];
	float oldDegreesZ, oldDegreesY;

	if (z2DSquared >= 0.0)
	{
		/*
		 * 2D x is 3D y, 2D y is 3D z, 2D z is 3D x.
		 */
		to[1] = x2D;
		to[2] = y2D;
		to[0] = (float) sqrt((double) z2DSquared);

		if (_z2DNegative)
			to[0] = -to[0];
	}
	else
	{
		float factor = (float) sqrt((double)
			(_distSquared / (_distSquared - z2DSquared)));
		to[1] = factor * x2D;
		to[2] = factor * y2D;
		to[0] = 0.0;
	}

	calcAngles(_from, to, degreesZ, degreesY);

	_data->getAngles(&oldDegreesZ, &oldDegreesY);

	float deltaZ1 = degreesZ[0] - oldDegreesZ;
	float deltaY1 = degreesY[0] - oldDegreesY;
	float deltaZ2 = degreesZ[1] - oldDegreesZ;
	float deltaY2 = degreesY[1] - oldDegreesY;

	if ((deltaZ1 * deltaZ1 + deltaY1 * deltaY1) < 
		(deltaZ2 * deltaZ2 + deltaY2 * deltaY2))
	{
		_data->setAngles(degreesZ[0], degreesY[0], 1);
	}
	else
	{
		_data->setAngles(degreesZ[1], degreesY[1], 1);
	}
}

void Rotate3D::getAngles(float *degreesZ, float *degreesY)
{
	_data->getAngles(degreesZ, degreesY);
}

float Rotate3D::mag(float *a)
{
	float retval = (float)
		sqrt((double) (a[0] * a[0] + a[1] * a[1] + a[2] * a[2]));

	assert(0.0 <= retval);

	return retval;
}

float Rotate3D::dp(float *a, float *b)
{
	return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

void Rotate3D::xp(float *a, float *b, float *c)
{
	c[0] = a[1] * b[2] - a[2] * b[1];
	c[1] = a[2] * b[0] - a[0] * b[2];
	c[2] = a[0] * b[1] - a[1] * b[0];
}

void Rotate3D::planeToCone(float * /*from*/, float *to, float step[][3])
{
	/*
	 * y and z are easy.
	 */
	step[0][1] = step[1][1] = to[1];
	step[0][2] = step[1][2] =   0.0;

	/*
	 * Calc equation of y cone.
	 */
	assert(0.0 != to[1]);

	float yCone = (to[0] * to[0] + to[2] * to[2]) / (to[1] * to[1]);

	/*
	 * Calc x.
	 */
	float yCheck = yCone * step[0][1] * step[0][1];

	assert(0.0 <= yCheck);

	step[0][0] = (float) sqrt((double) yCheck);
	step[1][0] = -step[0][0];
}

void Rotate3D::planeToPlane(float *from, float *to, float step[][3])
{
	/*
	 * y and z are easy.
	 */
	step[0][1] = step[1][1] = 0.0;
	step[0][2] = step[1][2] = 0.0;

	step[0][0] = (float) sqrt
		((double) (from[0] * from[0] + from[1] * from[1]));

	NEAR_ZERO(step[0][0]
		- (float) sqrt((double) (to[0] * to[0] + to[2] * to[2])));

	step[1][0] = -step[0][0];
}

void Rotate3D::planeToLine(float * /*from*/, float *to, float step[][3])
{
	/*
	 * Everything is easy.
	 */
	step[0][0] = step[1][0] =   0.0;
	step[0][1] = step[1][1] = to[1];
	step[0][2] = step[1][2] =   0.0;
}

void Rotate3D::anglesFromStep(float *from, float *step, float *to,
	float *z, float *y)
{
	/*
	 * temp is necessary to restore 3D vectors after they are projected
	 * to 2D.
	 */
	float temp [2];
	float theCos, cross[3];

	/*
	 * For angle about z-axis, work in the xy plane.
	 */
	if ((0.0 == from[0] && 0.0 == from[1])
		|| (0.0 == step[0] && 0.0 == step[1]))
	{
		assert((0.0 == from[0] && 0.0 == from[1])
			&& (0.0 == step[0] && 0.0 == step[1]));

		*z = 0.0;
	}
	else
	{
		temp[0] = from[2];
		temp[1] = step[2];
		from[2] = step[2] = 0.0;

		theCos = dp(step, from) / mag(step) / mag(from);
		assert(1.00001 >= theCos && -1.00001 <= theCos);
		theCos = (theCos < -1.0) ? -1.0 : theCos;
		theCos = (theCos >  1.0) ?  1.0 : theCos;

		*z = _rads2Degs * (float) acos((double) theCos);

		xp(step, from, cross);
		NEAR_ZERO(cross[0]);
		NEAR_ZERO(cross[1]);

		if (0.0 >= cross[2])
			*z = -*z;

		from[2] = temp[0];
		step[2] = temp[1];
	}

	/*
	 * For angle about y-axis, work in the xz plane.
	 */
	if ((0.0 == step[0] && 0.0 == step[2])
		|| (0.0 == to[0] && 0.0 == to[2]))
	{
		assert((0.0 == step[0] && 0.0 == step[2])
			&& (0.0 == to[0] && 0.0 == to[2]));

		*y = 0.0;
	}
	else
	{
		temp[0] = to  [1];
		temp[1] = step[1];
		to  [1] = step[1] = 0.0;

		theCos = dp(to, step) / mag(to) / mag(step);
		assert(1.00001 >= theCos && -1.00001 <= theCos);
		theCos = (theCos < -1.0) ? -1.0 : theCos;
		theCos = (theCos >  1.0) ?  1.0 : theCos;

		*y = _rads2Degs * (float) acos((double) theCos);

		xp(to, step, cross);
		NEAR_ZERO(cross[0]);
		NEAR_ZERO(cross[2]);

		if (0.0 >= cross[1])
			*y = -*y;

		to  [1] = temp[0];
		step[1] = temp[1];
	}
}

void Rotate3D::calcAngles(float *from, float *to, float *z, float *y)
{
	float step[2][3];

	NEAR_ZERO(from[0] * from[0] + from[1] * from[1] + from[2] * from[2] -
		  to  [0] * to  [0] - to  [1] * to  [1] - to  [2] * to  [2]);

	switch (2 * (0.0 == to[0] && 0.0 == to[2]) + (0.0 == to[1]))
	{
		case 0:
			planeToCone(from, to, step);
			break;
		case 1:
			planeToPlane(from, to, step);
			break;
		case 2:
			planeToLine(from, to, step);
			break;
		case 3:
		default:
			assert(0);
	}

	anglesFromStep(from, step[0], to, z    , y    );
	anglesFromStep(from, step[1], to, z + 1, y + 1);
}
