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
#ifndef SLLISTPICKER_H
#define SLLISTPICKER_H
 
#include "sl/sl_form.hh"
#include "wproc.h"
#include <Xm/Xm.h>

class SLListPicker : public SLForm
{
	public:
		enum
		{
			title,
			topSep,
			list,
			bottomSep,
			input,
			_NUM_WIDGETS
		};
	private:

		Widget _title;
		Widget _topSep;
		Widget _list;
		Widget _bottomSep;
		Widget _input;

		
		static char *widgetName[_NUM_WIDGETS];
		static char *_emptyString;

		int _itemCount;
		char *_capItemsBlock;
		char **_capItems;

		Bool _unmadeResources;
		XmString *_unmadeItems;
		int _unmadeItemCount;
		XmString *_unmadeXmSelItem;
		int _unmadeSelItemCount;
		char *_unmadeSelItem;

		void (*_singleClickFunc)(char *, void *);
		void (*_doubleClickFunc)(char *, void *);
		void (*_textUniqueFunc )(char *, void *);
		void (*_textReturnFunc )(char *, void *);
		void *_singleClickData;
		void *_doubleClickData;
		void *_textUniqueData ;
		void *_textReturnData ;
		void (*_extraStaticFunc)(void);

		Bool _uniqueTextEntry;

		void sortList();
		void checkInitialSelection();
		void checkUnmadeSelection();
		void strcpyToUpper(char *, const char *);
		void sortTwoLists(char **, XmString *, int);

		static void textCB(Widget, XtPointer, XtPointer);
		void modifyVerify(XmTextVerifyCallbackStruct *);
		void activate(Widget);

		static void listCB(Widget, XtPointer, XtPointer);
		void browseSelection(XmListCallbackStruct *);

		void gotSingleClick(      );
		void gotDoubleClick(      );
		void gotTextUnique (      );
		void gotTextReturn (char *);

		char *editString(char *, char *, int, int, int);
		int listMatchItem(char *);
		void getUnmadeResourcesFromRDB();
		void storeUnmadeResources(char **, int, char *);
		void freeUnmadeResources();

		SLListPicker() :
			SLForm((Widget) NULL, (char *) NULL)
			{ /* private, no access to default constructor */ }
		SLListPicker(SLListPicker &) :
			SLForm((Widget) NULL, (char *) NULL)
			{ /* private, no access to copy constructor */ }

	protected:

		virtual void singleClick(char *selection)
			{ if (selection) delete [] selection; }
		virtual void doubleClick(char *selection)
			{ if (selection) delete [] selection; }
		virtual void textUnique (char *selection)
			{ if (selection) delete [] selection; }
		virtual void textReturn (char *selection)
			{ if (selection) delete [] selection; }

	public:

		enum {
			SINGLE_CLICK,
			DOUBLE_CLICK,
			TEXT_UNIQUE ,
			TEXT_RETURN
		};


		SLListPicker(Widget   , char *, HelpCtx = NULL, Boolean = False,
			Boolean = True);
		SLListPicker(SLDelay *, char *, HelpCtx = NULL, Boolean = False,
			Boolean = True);
		~SLListPicker();

		Widget make(Widget);

		void setList(char **, int, char *);
		void setSelection(char *);
		char *getSelection();
		void setSingleClick(void (*)(char *, void *), void * = NULL);
		void setDoubleClick(void (*)(char *, void *), void * = NULL);
		void setTextUnique (void (*)(char *, void *), void * = NULL);
		void setTextReturn (void (*)(char *, void *), void * = NULL);
		void setExtra      (void (*)(void));
};

#endif
