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
#ifndef _HARD_TAG_HH
#define _HARD_TAG_HH

#include "vect/label_placement.hh"

class HardTag
{
	public:

		HardTag(class TagLinkedList *tags, class PlotBase *plot,
			class HardCopyPlot *hard);
		~HardTag()
			{ /* do nothing */ }

	private:

		class PlotBase     *_plot;
		class HardCopyPlot *_hard;

		void plotTag(float x, float y, float width, float height,
			const char *label, const char *font, const char *color,
			VectorLabelPlacement labelPlacement);

		HardTag()
			{ /* private, no access to default constructor */ }
		HardTag(HardTag &)
			{ /* private, no access to copy constructor */ }
		HardTag& operator=(HardTag &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _HARD_TAG_HH */
