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
#ifndef _ROTATE_3D_HH
#define _ROTATE_3D_HH

class Rotate3D
{
	public:

		Rotate3D(float x3D, float y3D, float z3D,
			float x2D, float y2D, float z2D,
			float degreesZ, float degreesY);
		~Rotate3D();
		void addPlot(class SeisPlot *plot);
		void update(float x2D, float y2D);
		void getAngles(float *degreesZ, float *degreesY);

	private:

		class SeisVectLinkedList *_vectors;
		class V3dDataLinkedList  *_data;

		static float _rads2Degs;
		float _from[3];
		float _distSquared;
		int _z2DNegative;

		/*
		 * These static funcs do the angle calculations.
		 */
		static float mag(float *a);
		static float dp(float *a, float *b);
		static void xp(float *a, float *b, float *c);
		static void planeToCone (float *from, float *to,
			float step[][3]);
		static void planeToPlane(float *from, float *to,
			float step[][3]);
		static void planeToLine (float *from, float *to,
			float step[][3]);
		static void anglesFromStep(float *from, float *step, float *to,
			float *z, float *y);
		static void calcAngles(float *from, float *to,
			float *z, float *y);
};

#endif	/* _ROTATE_3D_HH */
