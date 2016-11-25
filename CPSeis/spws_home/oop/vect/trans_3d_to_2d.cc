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
#include "vect/trans_3d_to_2d.hh"
#include "vect/ijk.hh"
#include "oprim/ll_base_data.hh"

#include <stdio.h>
#include <math.h>

/* static variable */
float Trans3Dto2D::_degreesToRadiansFactor = 0.0;

Trans3Dto2D::Trans3Dto2D(BaseData *data3D, float degreesZ, float degreesY,
	float xOffset, float xFactor, float yOffset, float yFactor,
	float zOffset, float zFactor, float xExp, float yExp, float zExp)
	: BaseData(), DataUser(), _degreesZ(degreesZ), _degreesY(degreesY),
	_xOffset(xOffset), _xFactor(xFactor), _yOffset(yOffset),
	_yFactor(yFactor), _zOffset(zOffset), _zFactor(zFactor),
	_xExp(xExp), _yExp(yExp), _zExp(zExp),
	_newAngle(1)
{
	addData(data3D);

	/* Initialize static variable, atan(1) = 45 degrees */
	if (_degreesToRadiansFactor == 0.0)
		_degreesToRadiansFactor = (float) atan(1.0) / 45.0;

	setRotationFactors();
}

void Trans3Dto2D::setAngles(float degreesZ, float degreesY, int doMod, long id)
{
	BaseData *data;
	int numPts;

	if (doMod)
	{
		data = getData();
		numPts = data->getNumPts();
		BaseData::modIndicesBefore(0, numPts, id);
	}

	_degreesZ = degreesZ;
	_degreesY = degreesY;
	setRotationFactors();
	_newAngle = 1;

	if (doMod)
	{
		BaseData::modIndicesAfter(0, numPts, id);
		BaseData::modDone(id);
	}
}

void Trans3Dto2D::setScale(float xOffset, float xFactor, float yOffset,
	float yFactor, float zOffset, float zFactor, int doMod, long id)
{
	BaseData *data;
	int numPts;

	if (doMod)
	{
		data = getData();
		numPts = data->getNumPts();
		BaseData::modIndicesBefore(0, numPts, id);
	}

	_xOffset = xOffset;
	_xFactor = xFactor;
	_yOffset = yOffset;
	_yFactor = yFactor;
	_zOffset = zOffset;
	_zFactor = zFactor;

	if (doMod)
	{
		BaseData::modIndicesAfter(0, numPts, id);
		BaseData::modDone(id);
	}
}

void Trans3Dto2D::setExpansion(float xExp, float yExp, float zExp, int doMod,
	long id)
{
	BaseData *data;
	int numPts;

	if (doMod)
	{
		data = getData();
		numPts = data->getNumPts();
		BaseData::modIndicesBefore(0, numPts, id);
	}

	_xExp = xExp;
	_yExp = yExp;
	_zExp = zExp;

	if (doMod)
	{
		BaseData::modIndicesAfter(0, numPts, id);
		BaseData::modDone(id);
	}
}

BaseData *Trans3Dto2D::getData()
{
	void *p;
	BaseData *retval = _baseDatas->top(&p);

	/* One and only one */
	assert((retval != NULL) && (_baseDatas->next(&p) == NULL));

	return retval;
}

int Trans3Dto2D::x2Dto3D(float x2D, float y2D, float x3D,
	float *y3D, float *z3D)
{
	int retval;

	if (not90(_degreesZ) && not90(_degreesY))
	{
		x3D = _xExp * _xFactor * (x3D - _xOffset);

		/* Calc. y3D in 2 steps because z3D needs result of 1st step. */
		*y3D = (x2D - _x1 * x3D             ) / _x2;
		*z3D = (y2D - _y1 * x3D - _y2 * *y3D) / _y3
			/ (_zExp * _zFactor) + _zOffset;
		*y3D = *y3D / (_yExp * _yFactor) + _yOffset;

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int Trans3Dto2D::y2Dto3D(float x2D, float y2D, float y3D,
	float *x3D, float *z3D)
{
	int retval;

	if (not0(_degreesZ) && not90(_degreesY))
	{
		y3D = _yExp * _yFactor * (y3D - _yOffset);

		/* Calc. x3D in 2 steps because z3D needs result of 1st step. */
		*x3D = (x2D - _x2 *  y3D             ) / _x1;
		*z3D = (y2D - _y1 * *x3D - _y2 *  y3D) / _y3
			/ (_zExp * _zFactor) + _zOffset;
		*x3D = *x3D / (_xExp * _xFactor) + _xOffset;

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int Trans3Dto2D::z2Dto3D(float x2D, float y2D, float z3D,
	float *x3D, float *y3D)
{
	int retval;

	if (not90(_degreesZ) && not0(_degreesY))
	{
		z3D = _zExp * _zFactor * (z3D - _zOffset);

		float denom = _x1 * _y2 - _x2 * _y1;
		*x3D = (x2D * _y2 - _x2 * (y2D - _y3 * z3D)) / denom
			/ (_xExp * _xFactor) + _xOffset;
		*y3D = (_x1 * (y2D - _y3 * z3D) - x2D * _y1) / denom
			/ (_yExp * _yFactor) + _yOffset;

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int Trans3Dto2D::xy2Dto3D(float x2D, float y2D, float x3D, float y3D,
	float *z3D)
{
	int retval;

	if (not90(_degreesY))
	{
		x3D = _xExp * _xFactor * (x3D - _xOffset);
		y3D = _yExp * _yFactor * (y3D - _yOffset);

		Ijk vt1_3D = Ijk(0.0, 0.0, 1.0);	/* z-axis */
		Ijk pt1_3D = Ijk(x3D, y3D, 0.0);

		Ijk vt2_2D = Ijk(1.0, 0.0, 0.0);	/* Straight at you */
		Ijk vt2_3D = rotate2SpaceTo3Space(&vt2_2D);

		Ijk pt2_2D = Ijk(0.0, x2D, y2D);	/* x & y in 2D are */
		Ijk pt2_3D = rotate2SpaceTo3Space(&pt2_2D); /* y & z in 3D */

		Ijk closest = closestOnLineOne(&vt1_3D, &pt1_3D,
			&vt2_3D, &pt2_3D);

		float a, b, c;
		closest.get(&a, &b, &c);
		*z3D = c / (_zExp * _zFactor) + _zOffset;

		float zeroCheck;
		zeroCheck = (a - x3D) / x3D;
		// assert(zeroCheck <  1.0e-6 && zeroCheck > -1.0e-6);
		if (zeroCheck >=  1.0e-6 || zeroCheck <= -1.0e-6)
			printf("zeroCheck:  %f\n", zeroCheck);
		zeroCheck = (b - y3D) / y3D;
		// assert(zeroCheck <  1.0e-6 && zeroCheck > -1.0e-6);
		if (zeroCheck >=  1.0e-6 || zeroCheck <= -1.0e-6)
			printf("zeroCheck:  %f\n", zeroCheck);

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int Trans3Dto2D::xz2Dto3D(float x2D, float y2D, float x3D, float z3D,
	float *y3D)
{
	int retval;

	if (not90(_degreesZ) || not0(_degreesY))
	{
		x3D = _xExp * _xFactor * (x3D - _xOffset);
		z3D = _zExp * _zFactor * (z3D - _zOffset);

		Ijk vt1_3D = Ijk(0.0, 1.0, 0.0);	/* y-axis */
		Ijk pt1_3D = Ijk(x3D, 0.0, z3D);

		Ijk vt2_2D = Ijk(1.0, 0.0, 0.0);	/* Straight at you */
		Ijk vt2_3D = rotate2SpaceTo3Space(&vt2_2D);

		Ijk pt2_2D = Ijk(0.0, x2D, y2D);	/* x & y in 2D are */
		Ijk pt2_3D = rotate2SpaceTo3Space(&pt2_2D); /* y & z in 3D */

		Ijk closest = closestOnLineOne(&vt1_3D, &pt1_3D,
			&vt2_3D, &pt2_3D);

		float a, b, c;
		closest.get(&a, &b, &c);
		*y3D = b / (_yExp * _yFactor) + _yOffset;

		float zeroCheck;
		zeroCheck = (a - x3D) / x3D;
		// assert(zeroCheck <  1.0e-6 && zeroCheck > -1.0e-6);
		if (zeroCheck >=  1.0e-6 || zeroCheck <= -1.0e-6)
			printf("zeroCheck:  %f\n", zeroCheck);
		zeroCheck = (c - z3D) / z3D;
		// assert(zeroCheck <  1.0e-6 && zeroCheck > -1.0e-6);
		if (zeroCheck >=  1.0e-6 || zeroCheck <= -1.0e-6)
			printf("zeroCheck:  %f\n", zeroCheck);

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int Trans3Dto2D::yz2Dto3D(float x2D, float y2D, float y3D, float z3D,
	float *x3D)
{
	int retval;

	if (not0(_degreesZ) || not0(_degreesY))
	{
		y3D = _yExp * _yFactor * (y3D - _yOffset);
		z3D = _zExp * _zFactor * (z3D - _zOffset);

		Ijk vt1_3D = Ijk(1.0, 0.0, 0.0);	/* x-axis */
		Ijk pt1_3D = Ijk(0.0, y3D, z3D);

		Ijk vt2_2D = Ijk(1.0, 0.0, 0.0);	/* Straight at you */
		Ijk vt2_3D = rotate2SpaceTo3Space(&vt2_2D);

		Ijk pt2_2D = Ijk(0.0, x2D, y2D);	/* x & y in 2D are */
		Ijk pt2_3D = rotate2SpaceTo3Space(&pt2_2D); /* y & z in 3D */

		Ijk closest = closestOnLineOne(&vt1_3D, &pt1_3D,
			&vt2_3D, &pt2_3D);

		float a, b, c;
		closest.get(&a, &b, &c);
		*x3D = a / (_xExp * _xFactor) + _xOffset;

		float zeroCheck;
		zeroCheck = (b - y3D) / y3D;
		// assert(zeroCheck <  1.0e-6 && zeroCheck > -1.0e-6);
		if (zeroCheck >=  1.0e-6 || zeroCheck <= -1.0e-6)
			printf("zeroCheck:  %f\n", zeroCheck);
		zeroCheck = (c - z3D) / z3D;
		// assert(zeroCheck <  1.0e-6 && zeroCheck > -1.0e-6);
		if (zeroCheck >=  1.0e-6 || zeroCheck <= -1.0e-6)
			printf("zeroCheck:  %f\n", zeroCheck);

		retval = 1;
	}
	else
	{
		retval = 0;
	}

	return retval;
}

int Trans3Dto2D::getNumPts(long id)
{
	return getData()->getNumPts(id);
}

/*
 * 2D x is 3D y.
 */
float Trans3Dto2D::getX(int i, long id)
{
	class BaseData *data3D = getData();

	/*
	 * Store values for getX, getY, and getZ to make sure
	 * are called in proper order.  Some BaseData classes expect
	 * getX to be called first, etc.  If you just put getX, etc.
	 * directly into equation, compiler is free to make calls in
	 * any order it likes.
	 */
	float x = data3D->getX(i, id);
	float y = data3D->getY(i, id);

	return _x1 * _xExp * _xFactor * (x - _xOffset)
	     + _x2 * _yExp * _yFactor * (y - _yOffset);
}

/*
 * 2D y is 3D z.
 */
float Trans3Dto2D::getY(int i, long id)
{
	class BaseData *data3D = getData();
	float x = data3D->getX(i, id);
	float y = data3D->getY(i, id);
	float z = data3D->getZ(i, id);

	return _y1 * _xExp * _xFactor * (x - _xOffset)
	     + _y2 * _yExp * _yFactor * (y - _yOffset)
	     + _y3 * _zExp * _zFactor * (z - _zOffset);
}

/*
 * 2D z is 3D x.
 */
float Trans3Dto2D::getZ(int i, long id)
{
	class BaseData *data3D = getData();
	float x = data3D->getX(i, id);
	float y = data3D->getY(i, id);
	float z = data3D->getZ(i, id);

	return _z1 * _xExp * _xFactor * (x - _xOffset)
	     + _z2 * _yExp * _yFactor * (y - _yOffset)
	     + _z3 * _zExp * _zFactor * (z - _zOffset);
}

int Trans3Dto2D::getMarkerType(int i, long id)
{
	return getData()->getMarkerType(i, id);
}

int Trans3Dto2D::getAltMarkerColor(int i, long id)
{
	return getData()->getAltMarkerColor(i, id);
}

BaseData *Trans3Dto2D::actualData()
{
	return getData()->actualData();
}

void Trans3Dto2D::modIndicesBefore(class BaseData * /*baseData*/,
	int startIndex, int numIndices, long id)
{
	BaseData::modIndicesBefore(startIndex, numIndices, id);
}

void Trans3Dto2D::modIndicesAfter (class BaseData * /*baseData*/,
	int startIndex, int numIndices, long id)
{
	BaseData::modIndicesAfter(startIndex, numIndices, id);
}

void Trans3Dto2D::modDone(class BaseData * /*baseData*/, long id)
{
	BaseData::modDone(id);
}

void Trans3Dto2D::modAttributes(class BaseData * /*baseData*/,
	int startIndex, int numIndices, int ignoreHold, long id)
{
	BaseData::modAttributes(startIndex, numIndices, ignoreHold, id);
}

void Trans3Dto2D::modAttributesByIndices(class BaseData * /*baseData*/,
	int *indices, int numIndices, int ignoreHold, long id)
{
	BaseData::modAttributesByIndices(indices, numIndices, ignoreHold, id);
}

void Trans3Dto2D::modAttributesNeedingRepair(class BaseData * /*baseData*/,
	int startIndex, int numIndices, long id)
{
	BaseData::modAttributesNeedingRepair(startIndex, numIndices, id);
}

void Trans3Dto2D::setRotationFactors()
{
	/*
	 * The rotation matrix is from "Mathematics of Classical
	 * and Quantum Physics", Byron & Fuller, Vol. 1,  p. 11.
	 * The screen x-axis is the 3D y-axis and
	 * the screen y-axis is the 3D z-axis.
	 */
	double radiansZ = (double) degreesToRadians(_degreesZ);
	_sinZ = (float) sin(radiansZ);
	_cosZ = (float) cos(radiansZ);

	double radiansY = (double) degreesToRadians(_degreesY);
	_sinY = (float) sin(radiansY);
	_cosY = (float) cos(radiansY);

	_x1 = -_sinZ;
	_x2 =  _cosZ;
	/* x3 is zero */

	_y1 = _cosZ * _sinY;
	_y2 = _sinZ * _sinY;
	_y3 = _cosY;

	_z1 =  _cosZ * _cosY;
	_z2 =  _sinZ * _cosY;
	_z3 = -_sinY;
}

float plusMinus180(float degrees)
{
	for (; degrees >  180.0; degrees -= 360.0);
	for (; degrees < -180.0; degrees += 360.0);

	return degrees;
}

float Trans3Dto2D::degreesToRadians(float degrees)
{
	return _degreesToRadiansFactor * plusMinus180(degrees);
}

int Trans3Dto2D::not90(float degrees)
{
	degrees = plusMinus180(degrees);

	return (degrees != 90.0 && degrees != -90.0);
}

int Trans3Dto2D::not0(float degrees)
{
	degrees = plusMinus180(degrees);

	return (degrees != 0.0 && degrees != 180.0 && degrees != -180.0);
}

Ijk Trans3Dto2D::rotate2SpaceTo3Space(Ijk *vector)
{
	/*
	 * Rotate opposite direction as when going 3D to 2D.
	 */
	float sinZ = -_sinZ;
	float cosZ =  _cosZ;
	float sinY = -_sinY;
	float cosY =  _cosY;

	/*
	 * Rotate in opposite order, Y then Z, for this transformation
	 * matrix.
	 */
	static float a11, a12, a13;
	static float a21, a22, a23;
	static float a31,      a33;	/* a32 is zero */

	if (_newAngle)	/* Only calc. this trans. matrix if you need it. */
	{
		a11 =  cosZ * cosY;
		a12 =  sinZ;
		a13 = -cosZ * sinY;
		a21 = -sinZ * cosY;
		a22 =  cosZ;
		a23 =  sinZ * sinY;
		a31 =  sinY;
		a33 =  cosY;

		_newAngle = 0;
	}

	float a, b, c;
	vector->get(&a, &b, &c);

	return Ijk(	a11 * a + a12 * b + a13 * c,
			a21 * a + a22 * b + a23 * c,
			a31 * a +           a33 * c);
}

Ijk Trans3Dto2D::closestOnLineOne(Ijk *vect1, Ijk *pt1, Ijk *vect2, Ijk *pt2)
{
	/*
	 * Algorithm by C. C. Burch.
	 * Pass in pointers for speed, no copy constructors run.
	 */

	Ijk deltaB = *pt2 - *pt1;
	float deltaBa1 = deltaB % *vect1;
	float deltaBa2 = deltaB % *vect2;
	float a1a1 = *vect1 % *vect1;
	float a1a2 = *vect1 % *vect2;
	float a2a2 = *vect2 % *vect2;

	float t1 = (deltaBa1 * -a2a2 + deltaBa2 * a1a2)
		/ (a1a1 * -a2a2 + a1a2 * a1a2);

	return Ijk(*pt1 + *vect1 * t1);
}
