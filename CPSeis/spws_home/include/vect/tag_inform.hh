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
#ifndef _TAG_INFORM_HH
#define _TAG_INFORM_HH

#include "sp/seis_inform.hh"
#include "vect/ll_tag.hh"
#include "vect/hard_tag.hh"

#include <assert.h>

class TagInform : public SeisInform
{
	public:

		TagInform(SeisPlot *sp, TagLinkedList *tags)
			: SeisInform(sp), _tags(tags)
			{ /* just initializers */ }
		~TagInform()
			{ /* do nothing */ }
		void newPlot(SeisPlot * /*sp*/)
			{ _tags->checkPositions(); }
		void destroyed(SeisPlot * /*sp*/)
			{ assert(False); }
		void postZoom (SeisPlot * /*sp*/, SeisPlot::ZoomDir /*dir*/)
			{ _tags->checkPositions(); }
		void writeToHardCopy(SeisPlot *sp, HardCopyPlot *hard,
			float /* scale */)
			/* junk is only to suppress compiler warning. */
			{ HardTag junk(_tags, sp, hard); }

	private:

		TagLinkedList *_tags;
};

#endif	/* _TAG_INFORM_HH */
