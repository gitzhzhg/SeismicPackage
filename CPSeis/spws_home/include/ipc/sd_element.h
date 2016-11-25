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
#ifndef SD_ELEMENT_H
#define SD_ELEMENT_H

#ifndef ELEMENT_H
#include "oprim/element.hh"
#define ELEMENT_H
#endif

#ifndef HANDLES_ERRORS_H
#include "oprim/handles_errors.hh"
#define HANDLES_ERRORS_H
#endif

#ifndef SD_MASTER_H
#include "ipc/sd_master.hh"
#define SD_MASTER_H
#endif

#ifndef XM_H
#include <Xm/Xm.h>
#define XM_H
#endif

#ifndef ASSERT_H
#include <assert.h>
#define ASSERT_H
#endif

#ifndef STRING_H
#include <string.h>
#define STRING_H
#endif

#ifndef IOSTREAM_H
#include <iostream.h>
#define IOSTREAM_H
#endif

class SlaveDisplayLinkedList;

class SlaveDisplayElement : public Element, public HandlesErrors
{
	friend class SlaveDisplayLinkedList;

	private:

		char *_node;
		Bool _privateColormap;
		SlaveDisplayMaster *_slaveDisplay;

		SlaveDisplayElement()	{ assert(False); }	// dummy
		SlaveDisplayElement(char *, SlaveDisplayLinkedList *,
			Bool = False);
		~SlaveDisplayElement();
		int operator ==(void * const value) const
			{ return(!strcmp(_node, (char *) value)); }
		void print() const
			{ cout << " " << _node; }

		void errorNotify(int);

		// Used in sneaky trick to translate this ptr from
		// HandlesErrors to this ptr for SlaveDisplayElement.
		// Multiple inheritance leads to different this ptrs.
		class SlaveDisplayElement *getThis()
			{ return(this); }

	public:
};

// SlaveDisplayLinkedList needs sd_element.hh in ll_sd.hh
// SlaveDisplayElement doesn't need ll_sd.hh until sd_element.cc
#ifndef LL_SD_H
#include "ipc/ll_sd.hh"
#define LL_SD_H
#endif

#endif
