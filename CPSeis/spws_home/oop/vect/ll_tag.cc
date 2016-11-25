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
#include "vect/ll_tag.hh"
#include "vect/tag.hh"
#include "vect/tag_inform.hh"
#include "sl/sl_client_message.hh"

TagLinkedList::TagLinkedList(SeisPlot *sp, VectorLabelPlacement labelPlacement,
	const char *font, const char *color, Bool makeInvisibleWhenSelected,
	void (*overPrintFunc)(class Tag *, class Tag *, int), int offset)
	: _plot(sp), _labelPlacement(labelPlacement),
	_font((char *) NULL), _color((char *) NULL),
	_makeInvisibleWhenSelected(makeInvisibleWhenSelected),
	_overPrintFunc(overPrintFunc),  _offset(offset),
	_fontStruct((XFontStruct *) NULL), _holding(False)
{
	assert(font && color);

	smartStrcpy(&_font , font );
	smartStrcpy(&_color, color);

	_inform = new TagInform(sp, this);
}

TagLinkedList::~TagLinkedList()
{
	/*
	 * nextPtr initialized only to silence bogus compiler used
	 * before set warning.
	 * Goes thru linked list from bottom to top so Tag destructors
	 * do not have any visibilities to check.
	 */
	Tag *ptr, *nextPtr = (Tag *) NULL;
	void *p;
	for (ptr = bottom(&p); ptr; ptr = nextPtr)
	{
		/*
		 * Get nextPtr before delete because Tag destructor
		 * removes it from linked list.
		 */
		nextPtr = prev(&p);

		delete ptr;
	}

	if (_font)
		delete [] _font;

	if (_color)
		delete [] _color;

	delete _inform;

	if (_fontStruct)
		freeFont(_fontStruct);
}

void TagLinkedList::checkPositions()
{
	Tag *ptr;
	void *p;
	for (ptr = top(&p); ptr; ptr = next(&p))
	{
		ptr->checkPosition(p);
	}
}

void TagLinkedList::flush()
{
	_holding = False;

	void *p;
	Tag *topTag = top(&p);
	topTag->checkVisibilityDownward(p);
}

void TagLinkedList::add(Tag *tag)
{
	TagElement *theElement = new TagElement(tag);

	BaseLinkedList::add((Element *) theElement);
}

void TagLinkedList::addAtTop(Tag *tag)
{
	TagElement *theElement = new TagElement(tag);

	void *p;
	top(&p);
	addBeforeCurrent((Element *) theElement, &p);
}

XFontStruct *TagLinkedList::getFontStruct()
{
	if (!_fontStruct)
	{
		_fontStruct = XLoadQueryFont(_plot->getDisplay(), _font);

		if(!_fontStruct)
			assert(_fontStruct = XLoadQueryFont(_plot->getDisplay(),
				"fixed"));
	}

	return _fontStruct;
}

/*
 * Just like smartStrcpy in Vector.
 */
void TagLinkedList::smartStrcpy(char **dst, const char *src)
{
	if (*dst != src)
	{
		if (*dst)
			delete [] *dst;

		if (src)
		{
			*dst = new char[strlen(src) + 1];
			strcpy(*dst, src);
		}
		else
		{
			*dst = (char *) NULL;
		}
	}
}

void TagLinkedList::freeFont(XFontStruct *fontStruct)
{
	CMStruct *cmStruct   = new CMStruct;
	cmStruct->display    = _plot->getDisplay();
	cmStruct->fontStruct = fontStruct;

	Widget w = _plot->getWidget();
	assert(w);

	new SLClientMessage(w, "spws_junk", clientMessageFunc,
		(void *) cmStruct);
}

void TagLinkedList::clientMessageFunc(void *cmStruct)
{
	XFreeFont(((CMStruct *) cmStruct)->display,
		  ((CMStruct *) cmStruct)->fontStruct);

	delete (CMStruct *) cmStruct;
}

void TagLinkedList::addVectToTag(Tag *tag, class Vector *vect)
{
	TagElement *e = (TagElement *) BaseLinkedList::find((void *) tag);
	assert(e);
	e->_vect = vect;
}

class Vector *TagLinkedList::getVectFromTag(Tag *t)
{
	TagElement *e = (TagElement *) BaseLinkedList::find((void *) t);
	assert(e);
	return e->_vect;
}

Tag *TagLinkedList::getTagFromVect(class Vector *v)
{
	TagElement *e;
	void *p;
	for (	e = (TagElement *) BaseLinkedList::top (&p);
		e;
		e = (TagElement *) BaseLinkedList::next(&p))
	{
		if (e->_vect == v)
			return e->_tag;
	}

	return (Tag *) 0;
}
