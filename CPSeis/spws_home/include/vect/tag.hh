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
#ifndef _TAG_HH
#define _TAG_HH

#include "vect/label_placement.hh"

#include <Xm/Xm.h>

class Tag
{
	public:

		Tag(class TagLinkedList *tags, const char *label,
			float x, float y,
			VectorLabelPlacement labelPlacement = Normal,
			const char *font  = (const char *) NULL,
			const char *color = (const char *) NULL);
		~Tag();
		Bool checkPosition(void *p);
		void checkVisibilityDownward(void *p);
		void getPosition(float *x, float *y) const;
		Bool setPosition(float  x, float  y, void *p = (void *) NULL);
		void getArea(int *x, int *y, int *width, int *height) const;
		void getLabel(const char **label, const char **font) const;
		Bool setLabel(const char *label,
			const char *font = (const char *) NULL,
			void *p = (void *) NULL);
		const char *getColor() const;
		void setColor(const char *color);
		VectorLabelPlacement getLabelPlacement() const;
		Bool setLabelPlacement(VectorLabelPlacement labelPlacement,
			void *p = (void *) NULL);
		Bool makeVisible(void *p = (void *) NULL);
		void makeInvisible();
		Bool isVisible() const
			{ return _visible; }
		void getSizeWC(float *width, float *height);
		void mustBeVisible();

	private:

		class TagLinkedList *_tags ;
		Widget _w;
		float _xWc, _yWc;
		int _xDc, _yDc, _width, _height;
		char *_label;
		char *_font;
		char *_color;
		VectorLabelPlacement _labelPlacement;
		Bool _visible, _going;
		XFontStruct *_fontStruct;

		void makeWidget();
		XFontStruct *getFontStruct();
		Pixel getPixel() const;
		void calcSize(XFontStruct *fontStruct,
			int *marginTop, int *marginBottom);
		void calcSize(XmString string, XmFontList fontList);
		void calcPosition();
		Bool checkVisibility(void *p);
		Bool noOverLap(void *p) const;
		Bool noOverLap(Tag *otherTag) const;
		static void staticEventHandler(Widget w, XtPointer,
			XEvent *event, Boolean *);
		void eventHandler(Widget w, XEvent *event);
		static void staticDestroyCallback(Widget, XtPointer client,
			XtPointer);
		void destroyCallback();

		Tag()
			{ /* Private, no access to default constructor */ }
		Tag(Tag &)
			{ /* Private, no access to copy constructor */ }
		Tag& operator=(Tag &p)
			{ /* Private, no access to operator = */ return p; }
};

#endif	/* _TAG */
