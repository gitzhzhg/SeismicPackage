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
#ifndef _CSV_XH_TRANS_HH
#define _CSV_XH_TRANS_HH

#include "vect/cross_hairs.hh"

#define _NUM_GENERIC 3		/* x, y, z or t */

class CsvCrossHairTranslator : public CrossHairTranslator
{
	public:
	
		CsvCrossHairTranslator(class CubePosition *pos,
			class CubeDisplay *cd);
		virtual ~CsvCrossHairTranslator();

		virtual const float *nilGeneric();
		virtual       float  getNil    ();

	protected:

		class CubePosition *_pos;
		class CubeDisplay  *_cd ;

		static       float _generic[_NUM_GENERIC];
		static const float _nil;

	private:

};

class InlineCrossHairTranslator : public CsvCrossHairTranslator
{
	public:
	
		InlineCrossHairTranslator(class CubePosition *pos,
			class CubeDisplay *cd);
		virtual ~InlineCrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y);
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y);
		virtual int getColor(const float *generic);

	private:

};

class CrosslineCrossHairTranslator : public CsvCrossHairTranslator
{
	public:
	
		CrosslineCrossHairTranslator(class CubePosition *pos,
			class CubeDisplay *cd);
		virtual ~CrosslineCrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y);
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y);
		virtual int getColor(const float *generic);

	private:

};

class TimesliceCrossHairTranslator : public CsvCrossHairTranslator
{
	public:
	
		TimesliceCrossHairTranslator(class CubePosition *pos,
			class CubeDisplay *cd);
		virtual ~TimesliceCrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y);
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y);
		virtual int getColor(const float *generic);

	private:

};

class WireFrameCrossHairTranslator : public CsvCrossHairTranslator
{
	public:
	
		WireFrameCrossHairTranslator(class CubePosition *pos,
			class CubeDisplay *cd);
		virtual ~WireFrameCrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y);
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y);
		virtual int getColor(const float *generic);

		int getAxisValues(float *xl, float *il, float *ts);

	private:

		float _axis[_NUM_GENERIC];

};

class WireFrameCrossHair : public CrossHair
{
	public:
	
		WireFrameCrossHair(class CubeWireFrame *wf,
			WireFrameCrossHairTranslator *trans);
		virtual ~WireFrameCrossHair();

		/*
		 * from CrossHair
		 */
		virtual void move(float x, float y, int color);
		virtual void setVisibility(int visible);
		virtual int  getVisibility(           );
		virtual void expose(int x, int y, int width, int height,
			float x_move, float y_move, int color_move);
		virtual void noPlot();
		virtual void backingStoreChange();
		virtual void getParams(      char ***colors, int *num_colors,
			int *length, int *height);
		virtual void setParams(const char   *color ,
			int  length, int  height);
		virtual void setParams(const char   *color , int  num_color ,
			int  length, int  height);

	protected:


	private:

		class CubeWireFrame                *_wf   ;
		      WireFrameCrossHairTranslator *_trans;

};

#endif /* _CSV_XH_TRANS_HH */
