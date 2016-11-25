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
#ifndef LL_BASE_H
#define LL_BASE_H

class BaseLinkedList
{
	private:

		class Element *_top;
		class Element *_bottom;
		class Element *_current;
		int _count;

		void recursiveDestroy(class Element *);

	protected:

		BaseLinkedList();
		~BaseLinkedList();
		void add(class Element *);
		void addBeforeCurrent(class Element *, void ** = (void **) 0);
		void  addAfterCurrent(class Element *, void ** = (void **) 0);
		void remove(void *);
		void removeByPtr(class Element *);
		class Element *find(void *, void ** = (void **) 0);
		class Element *moveTo(void *, void ** = (void **) 0);
		void printList();
		class Element *top    (void **ptr = (void **) 0)
		{
			return ((!ptr) ?  _current = _top
				: (class Element *) (*ptr = (void *) _top    ));
		}
		class Element *bottom (void **ptr = (void **) 0)
		{
			return ((!ptr) ?  _current = _bottom
				: (class Element *) (*ptr = (void *) _bottom ));
		}
		class Element *next   (void **ptr = (void **) 0);
		class Element *prev   (void **ptr = (void **) 0);
		class Element *current(void **ptr = (void **) 0)
		{
			return ((!ptr) ?  _current
				: (class Element *) (*ptr));
		}

	public:

		int count()
			{ return(_count); }
};

#endif
