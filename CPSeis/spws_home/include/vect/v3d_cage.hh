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
#ifndef _V3D_CAGE_HH
#define _V3D_CAGE_HH

class V3dCage
{
	public:

		V3dCage(class V3dDataLinkedList *mainData, int numXAnnos = 2,
			int numYAnnos = 2, int numZAnnos = 2);
		V3dCage(float degreesZ, float degreesY,
			float xOffset , float xFactor ,
			float yOffset , float yFactor ,
			float zOffset , float zFactor ,
			float xExp    , float yExp    , float zExp,
			int numXAnnos = 2, int numYAnnos = 2,
			int numZAnnos = 2);
		~V3dCage();
		void addPlot(class SeisPlot *plot);
		void updateAngles();
		void updateAngles(float degreesZ, float degreesY);
		void updateLabels();
		void updateLabels(float xOffset, float xFactor,
				  float yOffset, float yFactor,
				  float zOffset, float zFactor,
				  float xExp   , float yExp   , float zExp);
		void updateLabels(float xMin, float xMax,
				  float yMin, float yMax,
				  float zMin, float zMax);
		void getAngles(float *degreesZ, float *degreesY)
			{ *degreesZ = _degreesZ;  *degreesY = _degreesY; }
		void getScale (float *xOffset, float *xFactor, float *yOffset,
			float *yFactor, float *zOffset, float *zFactor)
		{
			*xOffset = _xOffset;  *xFactor = _xFactor;
			*yOffset = _yOffset;  *yFactor = _yFactor;
			*zOffset = _zOffset;  *zFactor = _zFactor;
		}
		void getExpansion(float *xExp, float *yExp, float *zExp)
			{ *xExp = _xExp;  *yExp = _yExp;  *zExp = _zExp; }
		int getClosestRotationHandle(int xDc, int yDc,
			class PlotBase *plot,
			float *x3D, float *y3D, float *z3D,
			float *x2D, float *y2D, float *z2D,
			float tolerance = 5.0);
		void drawLabels(float xMin, float xMax,
				float yMin, float yMax,
				float zMin, float zMax);
		void redisplay();
		void setVisibility(int set);

	private:

		class SeisVectLinkedList *_vectors, *_rotationHandles;
		class V3dDataLinkedList  *_mainData, *_cageData;
		int _numXAnnos, _numYAnnos, _numZAnnos;
		class Vector **_xAnno;
		class Vector **_yAnno;
		class Vector **_zAnno;
		float _degreesZ, _degreesY;
		float _xOffset, _xFactor, _yOffset, _yFactor,
			_zOffset, _zFactor;
		float _xExp, _yExp, _zExp;

		void constructorHelper(float degreesZ, float degreesY,
				       float xOffset , float xFactor ,
				       float yOffset , float yFactor ,
				       float zOffset , float zFactor ,
				       float xExp    , float yExp    ,
				       float zExp    ,
				       int numXAnnos , int numYAnnos ,
				       int numZAnnos );
		void showCage();
		void showRange(float *x, float *y, float *z, const char *color,
			float min, float max,
			Vector **labels, int numLabels);
		void showDirection(const char *xColor, const char *yColor,
			const char *zColor);
		void showRotationHandles();

		V3dCage()
			{ /* private, no access to default constructor */ }
		V3dCage(V3dCage &)
			{ /* private, no access to copy constructor */ }
		V3dCage& operator=(V3dCage &p)
			{ /* private, no access to = */ return p; }

};

#endif /* _V3D_CAGE_HH */
