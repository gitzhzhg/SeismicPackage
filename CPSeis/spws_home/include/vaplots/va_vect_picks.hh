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
#ifndef _VA_VECT_PICKS_HH
#define _VA_VECT_PICKS_HH

#include "vaplots/va_picks.hh"

class VaVectPickData : public VaPickData
{
	public:

		int getIndex(class Vector *vector);

	protected:
	
		      SeisPlot         *_sp     ;
		class VectorLinkedList *_vectors;
		class VaVectColors     *_colors ;
		class Vector          **_vect   ;
		class BaseData        **_data   ;
		int _numVectAllocated, _numVectUsed;

		VaVectPickData(class VfManager *manager, class VaPicks *picks,
			SeisPlot *sp, class VectorLinkedList *vectors,
			class VaVectColors *colors);
		virtual ~VaVectPickData();

		void checkAllocation(int new_numVectUsed);
		const char *getLineColor(int ifunc,
			const char * activeFuncColor,
			const char *defaultFuncColor,
			int show_active_line = 1);

	private:
};

#endif /* _VA_VECT_PICKS_HH */
