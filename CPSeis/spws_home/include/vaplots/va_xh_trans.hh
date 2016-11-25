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
#ifndef _XH_TRANS_HH
#define _XH_TRANS_HH

#include "vect/cross_hairs.hh"

#define _NUM_GENERIC 5		/* x, y, z or t, vel, cmp_trace */

class VaCrossHairTranslator : public CrossHairTranslator
{
	public:
	
		VaCrossHairTranslator(class VaPicks *picks);
		virtual ~VaCrossHairTranslator();

		virtual const float *nilGeneric();
		virtual       float  getNil    ();

	protected:

		static       float _generic[_NUM_GENERIC];
		static const float _nil;

		class VaPicks *_picks;

	private:

};

class CmpCrossHairTranslator : public VaCrossHairTranslator
{
	public:
	
		CmpCrossHairTranslator(class VaPicks *picks);
		virtual ~CmpCrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y);
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y);

	private:

};

class GvsCrossHairTranslator : public VaCrossHairTranslator
{
	public:
	
		GvsCrossHairTranslator(class VaPicks *picks);
		virtual ~GvsCrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y);
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y);

	private:

};

class IsoCrossHairTranslator : public VaCrossHairTranslator
{
	public:
	
		IsoCrossHairTranslator(class VaPicks *picks);
		virtual ~IsoCrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y);
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y);

	private:

};

class SemblanceCrossHairTranslator : public VaCrossHairTranslator
{
	public:
	
		SemblanceCrossHairTranslator(class VaPicks *picks);
		virtual ~SemblanceCrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y);
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y);

	private:

};

class GridCrossHairTranslator : public VaCrossHairTranslator
{
	public:
	
		GridCrossHairTranslator(class VaPicks *picks);
		virtual ~GridCrossHairTranslator();

		virtual const float *  toGenericCoords(float  x, float  y);
		virtual void         fromGenericCoords(const float *generic,
						       float *x, float *y);

	private:

};

#endif /* _XH_TRANS_HH */
