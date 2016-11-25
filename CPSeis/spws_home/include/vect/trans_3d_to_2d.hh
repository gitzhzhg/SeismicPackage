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
#ifndef _TRANS_3D_TO_2D_HH
#define _TRANS_3D_TO_2D_HH

#include "oprim/base_data.hh"
#include "oprim/data_user.hh"

class Trans3Dto2D : public BaseData, public DataUser
{
	public:

		Trans3Dto2D(BaseData *data3D, float degreesZ, float degreesY,
			float xOffset = 0.0, float xFactor = 1.0,
			float yOffset = 0.0, float yFactor = 1.0,
			float zOffset = 0.0, float zFactor = 1.0,
			float xExp = 1.0, float yExp = 1.0, float zExp = 1.0);
		virtual ~Trans3Dto2D()
			{ /* do nothing */ }
		void setAngles(float  degreesZ, float  degreesY,
			int doMod = 1, long id = defaultId);
		void getAngles(float *degreesZ, float *degreesY)
			{ *degreesZ = _degreesZ;  *degreesY = _degreesY; }
		friend float plusMinus180(float degrees);
		void setScale(float xOffset, float xFactor, float yOffset,
			float yFactor, float zOffset, float zFactor,
			int doMod = 1, long id = defaultId);
		void getScale(float *xOffset, float *xFactor, float *yOffset,
			float *yFactor, float *zOffset, float *zFactor)
		{
			*xOffset = _xOffset;  *xFactor = _xFactor;
			*yOffset = _yOffset;  *yFactor = _yFactor;
			*zOffset = _zOffset;  *zFactor = _zFactor;
		}
		void setExpansion(float xExp, float yExp, float zExp,
			int doMod = 1, long id = defaultId);
		void getExpansion(float *xExp, float *yExp, float *zExp)
			{ *xExp = _xExp;  *yExp = _yExp;  *zExp = _zExp; }
		BaseData *getData();
		int x2Dto3D(float x2D, float y2D, float x3D,
			float *y3D, float *z3D);
		int y2Dto3D(float x2D, float y2D, float y3D,
			float *x3D, float *z3D);
		int z2Dto3D(float x2D, float y2D, float z3D,
			float *x3D, float *y3D);
		int xy2Dto3D(float x2D, float y2D, float x3D, float y3D,
			float *z3D);
		int xz2Dto3D(float x2D, float y2D, float x3D, float z3D,
			float *y3D);
		int yz2Dto3D(float x2D, float y2D, float y3D, float z3D,
			float *x3D);
		/*
		 * BaseData virtual functions.
		 */
		virtual int getNumPts(long id = defaultId);
		virtual float getX(int i, long id = defaultId);
		virtual float getY(int i, long id = defaultId);
		virtual float getZ(int i, long id = defaultId);
		virtual int getMarkerType    (int i, long id = defaultId);
		virtual int getAltMarkerColor(int i, long id = defaultId);
		virtual BaseData *actualData();
		/*
		 * DataUser virtual functions.
		 */
		virtual void modIndicesBefore(BaseData *baseData,
			int startIndex, int numIndices,
			long id = BaseData::defaultId);
		virtual void modIndicesAfter (BaseData *baseData,
			int startIndex, int numIndices,
			long id = BaseData::defaultId);
		virtual void modDone(BaseData *baseData,
			long id = BaseData::defaultId);
		virtual void modAttributes(BaseData *baseData,
			int startIndex, int numIndices, int ignoreHold = 0,
			long id = BaseData::defaultId);
		virtual void modAttributesByIndices(BaseData *baseData,
			int *indices, int numIndices, int ignoreHold = 0,
			long id = BaseData::defaultId);
		virtual void modAttributesNeedingRepair(BaseData *baseData,
			int startIndex, int numIndices,
			long id = BaseData::defaultId);

	private:

		float _degreesZ, _degreesY;
		int _newAngle;		/* flag 0 or 1 */
		float _sinZ, _cosZ, _sinY, _cosY;
		float _x1, _x2;
		float _y1, _y2, _y3;
		float _z1, _z2, _z3;
		float _xOffset, _xFactor, _yOffset, _yFactor,
			_zOffset, _zFactor;
		float _xExp, _yExp, _zExp;
		static float _degreesToRadiansFactor;

		void setRotationFactors();
		float degreesToRadians(float degrees);
		int not90(float degrees);
		int not0 (float degrees);
		class Ijk rotate2SpaceTo3Space(class Ijk *vector);
		class Ijk closestOnLineOne(class Ijk *vect1, class Ijk *pt1,
			class Ijk *vect2, class Ijk *pt2);

		Trans3Dto2D()
			{ /* private, no access to default constructor */ }
		Trans3Dto2D(Trans3Dto2D &)
			{ /* private, no access to copy constructor */ }
		Trans3Dto2D& operator=(Trans3Dto2D &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _TRANS_3D_TO_2D_HH */
