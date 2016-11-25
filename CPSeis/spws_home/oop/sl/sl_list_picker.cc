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
#include "sl/sl_list_picker.hh"
#include "sl/psuedo_widget.hh"
#include "wproc.h"
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#ifndef myMIN
#define myMIN(x, y)	(((x) < (y)) ? (x) : (y))
#endif

#ifndef myMAX
#define myMAX(x, y)	(((x) > (y)) ? (x) : (y))
#endif

// statics
char *SLListPicker::widgetName[SLListPicker::_NUM_WIDGETS] =
	{
		"SLLP_title",
		"SLLP_topSep",
		"SLLP_list",
		"SLLP_bottomSep",
		"SLLP_input"
	};

char *SLListPicker::_emptyString = "";

SLListPicker::SLListPicker(Widget p, char *name, HelpCtx hctx,
	Boolean doframe, Boolean make_now) :
	SLForm(p, name, hctx, doframe, False, False)
{
	_itemCount       = 0;
	_unmadeResources = False;
	_singleClickFunc = (void (*)(char *, void *)) NULL;
	_doubleClickFunc = (void (*)(char *, void *)) NULL;
	_textUniqueFunc  = (void (*)(char *, void *)) NULL;
	_textReturnFunc  = (void (*)(char *, void *)) NULL;
	_extraStaticFunc = (void (*)(void          )) NULL;

	if (make_now)
		make(p);
}

SLListPicker::SLListPicker(SLDelay *contain, char *name, HelpCtx hctx,
	Boolean doframe, Boolean make_if_can) :
	SLForm(contain, name, hctx, doframe, False, False)
{
	_itemCount       = 0;
	_unmadeResources = False;
	_singleClickFunc = (void (*)(char *, void *)) NULL;
	_doubleClickFunc = (void (*)(char *, void *)) NULL;
	_textUniqueFunc  = (void (*)(char *, void *)) NULL;
	_textReturnFunc  = (void (*)(char *, void *)) NULL;
	_extraStaticFunc = (void (*)(void          )) NULL;

	if (contain->made() && make_if_can)
		make(contain->topWidget());
}

SLListPicker::~SLListPicker()
{
	if (_itemCount)
	{
		delete [] _capItemsBlock;
		delete [] _capItems     ;
	}

	if (_unmadeResources)
		freeUnmadeResources();
}

Widget SLListPicker::make(Widget p)
{
	Cardinal n;
	Arg args[10];

	if (!made())
	{
		SLForm::make(p);

		_title = XtVaCreateManagedWidget(widgetName[title],
			xmLabelWidgetClass, topWidget(),
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNleftAttachment  , XmATTACH_FORM  ,
			XmNrightAttachment , XmATTACH_FORM  ,
			NULL);

		_topSep = XtVaCreateManagedWidget(widgetName[topSep],
			xmSeparatorWidgetClass, topWidget(),
			XmNtopAttachment   , XmATTACH_WIDGET,
			XmNtopWidget       , _title         ,
			XmNleftAttachment  , XmATTACH_FORM  ,
			XmNrightAttachment , XmATTACH_FORM  ,
			NULL);

		n = 0;
		XtSetArg(args[n], XmNleftAttachment  , XmATTACH_FORM); n++;
		XtSetArg(args[n], XmNrightAttachment , XmATTACH_FORM); n++;
		XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;

		if (_unmadeResources)
		{
			if (_unmadeSelItemCount)
			{
				XtSetArg(args[n], XmNvalue,
					_unmadeSelItem); n++;
			}
			else
			{
				assert(!_unmadeSelItem); // Should be NULL
				XtSetArg(args[n], XmNvalue, ""); n++;
			}
		}

		// Delay manage until after checkInitialSelection.
		_input = XtCreateWidget(widgetName[input],
			xmTextWidgetClass, topWidget(), args, n);

		XtAddCallback(_input, XmNmodifyVerifyCallback, textCB,
			(XtPointer) this);
		XtAddCallback(_input, XmNvalueChangedCallback, textCB,
			(XtPointer) this);
		XtAddCallback(_input, XmNactivateCallback    , textCB,
			(XtPointer) this);

		_bottomSep = XtVaCreateManagedWidget(widgetName[bottomSep],
			xmSeparatorWidgetClass, topWidget(),
			XmNleftAttachment  , XmATTACH_FORM  ,
			XmNrightAttachment , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_WIDGET,
			XmNbottomWidget    , _input         ,
			NULL);

		n = 0;
		XtSetArg(args[n], XmNtopAttachment   , XmATTACH_WIDGET); n++;
		XtSetArg(args[n], XmNtopWidget       , _topSep        ); n++;
		XtSetArg(args[n], XmNleftAttachment  , XmATTACH_FORM  ); n++;
		XtSetArg(args[n], XmNrightAttachment , XmATTACH_FORM  ); n++;
		XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
		XtSetArg(args[n], XmNbottomWidget    , _bottomSep     ); n++;

		if (_unmadeResources)
		{
			XtSetArg(args[n], XmNitems            ,
				_unmadeItems       ); n++;
			XtSetArg(args[n], XmNitemCount        ,
				_unmadeItemCount   ); n++;
			XtSetArg(args[n], XmNselectedItems    ,
				_unmadeXmSelItem   ); n++;
			XtSetArg(args[n], XmNselectedItemCount,
				_unmadeSelItemCount); n++;
		}

		_list = XmCreateScrolledList(topWidget(), widgetName[list],
			args, n);

		XtAddCallback(_list, XmNbrowseSelectionCallback, listCB,
			(XtPointer) this);
		XtAddCallback(_list, XmNdefaultActionCallback  , listCB,
			(XtPointer) this);

		sortList();
		checkInitialSelection();

		XtManageChild(_input);
		XtManageChild(_list);

		manage();
	}

	return topWidget();
}

void SLListPicker::setList(char **items, int itemCount, char *selected)
{
	assert(itemCount >= 0);

	if (made())
	{
		XmString *XmItems;
		int i;

		if (itemCount)
			XmItems = new XmString[itemCount];
		else
			XmItems = (XmString *) NULL;

		for (i = 0; i < itemCount; i++)
			XmItems[i] = XmStringCreateSimple(items[i]);

		// XmNselectedItems not always set to NULL on sun with 
		// XmNselectedItemCount = 0.  Use XmListDeselectAllItems
		// instead.  Also, though no problems observed yet, use
		// XmListDeleteAllItems if XmNitemCount = 0 just to be sure.
		// Also, XmListDeselectAllItems does not work on sun if
		// XmNitemCount = 0.  Therefore XmListDeselectAllItems is
		// done before setting list, in case XmNitemCount is
		// getting set to 0.
		// Maybe someday there will be a debugged sun list widget.

		XmListDeselectAllItems(_list);

		if (itemCount)
			XtVaSetValues(_list,
				XmNitemCount, itemCount,
				XmNitems    , XmItems  ,
				NULL);
		else
			XmListDeleteAllItems(_list);

		for (i = 0; i < itemCount; i++)
			XmStringFree(XmItems[i]);

		if (itemCount)
			delete [] XmItems;

		if (selected)
			XmTextSetString(_input, selected    );
		else
			XmTextSetString(_input, _emptyString);

		sortList();
		checkInitialSelection();
	}
	else
	{
		storeUnmadeResources(items, itemCount, selected);
	}
}

void SLListPicker::setSelection(char *selection)
{
	if (made())
	{
		XmListDeselectAllItems(_list);

		if (selection)
			XmTextSetString(_input, selection);
		else
			XmTextSetString(_input, _emptyString);

		checkInitialSelection();
	}
	else
	{
		if (!_unmadeResources)
			getUnmadeResourcesFromRDB();

		if (_unmadeSelItemCount)
		{
			XmStringFree(*_unmadeXmSelItem);
			XtFree((char *) _unmadeXmSelItem);
			delete [] _unmadeSelItem;
			_unmadeSelItemCount = 0;
			_unmadeXmSelItem = (XmString *) NULL;
		}

		if (selection)
		{
			_unmadeSelItem = new char[strlen(selection) + 1];
			strcpy(_unmadeSelItem, selection);
		}
		else
		{
			_unmadeSelItem = (char *) NULL;
		}

		checkUnmadeSelection();
	}
}

char *SLListPicker::getSelection()
{
	char *retval;

	if (made())
	{
		XmString *selectedItems;
		int selectedItemCount;

		XtVaGetValues(_list,
			XmNselectedItems    , &selectedItems    ,
			XmNselectedItemCount, &selectedItemCount,
			NULL);

		assert(selectedItemCount == 0 || selectedItemCount == 1);

		if (selectedItemCount)
		{
			char *string;
			assert(XmStringGetLtoR(*selectedItems,
				XmSTRING_DEFAULT_CHARSET, &string));

			// Copy to newed char [] so user of method
			// can use delete instead of XtFree.
			retval = new char[strlen(string) + 1];
			strcpy(retval, string);
			XtFree(string);
		}
		else
		{
			retval = (char *) NULL;
		}
	}
	else
	{
		if (!_unmadeResources)
			getUnmadeResourcesFromRDB();

		if (_unmadeSelItemCount)
		{
			// Copy to newed char [] so user of method can delete.
			retval = new char[strlen(_unmadeSelItem) + 1];
			strcpy(retval, _unmadeSelItem);
		}
		else
		{
			retval = (char *) NULL;
		}
	}

	return retval;
}

void SLListPicker::setSingleClick(void (*func)(char *, void *), void *data)
{
	_singleClickFunc = func;
	_singleClickData = data;
}

void SLListPicker::setDoubleClick(void (*func)(char *, void *), void *data)
{
	_doubleClickFunc = func;
	_doubleClickData = data;
}

void SLListPicker::setTextUnique (void (*func)(char *, void *), void *data)
{
	_textUniqueFunc  = func;
	_textUniqueData  = data;
}

void SLListPicker::setTextReturn (void (*func)(char *, void *), void *data)
{
	_textReturnFunc  = func;
	_textReturnData  = data;
}

void SLListPicker::setExtra(void (*func)(void))
{
	_extraStaticFunc = func;
}

void SLListPicker::sortList()
{
	if (_itemCount)
	{
		delete [] _capItemsBlock;
		delete [] _capItems     ;
	}

	XmString *items;
	unsigned char selectionPolicy;

	XtVaGetValues(_list,
		XmNitemCount      , &_itemCount     ,
		XmNitems          , &items          ,
		XmNselectionPolicy, &selectionPolicy,
		NULL);

	assert(selectionPolicy == XmBROWSE_SELECT);

	if (_itemCount)
	{
		char    **locItems   = new char *  [_itemCount];
		XmString *locXmItems = new XmString[_itemCount];

		size_t size;
		int i;
		for (i = 0, size = 0; i < _itemCount; i++)
		{
			assert(XmStringGetLtoR(items[i],
				XmSTRING_DEFAULT_CHARSET, locItems + i));
			assert(*locItems[i]);	// not empty string

			locXmItems[i] = XmStringCopy(items[i]);

			size += strlen(locItems[i]) + 1;
		}

		_capItemsBlock = new char   [size      ];
		_capItems      = new char * [_itemCount];

		char *ptr;
		for (i = 0, ptr = _capItemsBlock; i < _itemCount; i++)
		{
			strcpyToUpper(ptr, locItems[i]);
			_capItems[i] = ptr;
			ptr += strlen(ptr) + 1;
		}

		sortTwoLists(_capItems, locXmItems, _itemCount);

		XmListReplaceItemsPos(_list, locXmItems, _itemCount, 1);

		for (i = 0; i < _itemCount; i++)
		{
			XtFree      (locItems  [i]);
			XmStringFree(locXmItems[i]);
		}

		delete [] locItems;
		delete [] locXmItems;
	}
	else
	{
		assert(!items);
	}
}

void SLListPicker::checkInitialSelection()
{
/*
 * Get list widget selection, if any.
 */
	XmString *selectedItems;
	int selectedItemCount, visibleItems, topItem;

	XtVaGetValues(_list,
		XmNselectedItems    , &selectedItems    ,
		XmNselectedItemCount, &selectedItemCount,
		XmNvisibleItemCount , &visibleItems     ,
		XmNtopItemPosition  , &topItem          ,
		NULL);

	assert(selectedItemCount == 0 || selectedItemCount == 1);

	char *listSelect;

	if (selectedItemCount)
	{
		assert(XmStringGetLtoR(*selectedItems, XmSTRING_DEFAULT_CHARSET,
			&listSelect));
		assert(*listSelect);		// not empty string
	}
	else
	{
		assert(!selectedItems);		// should be NULL
		listSelect = _emptyString;
	}
/*
 * Get text widget selection, if any.
 */
	char *textSelect = XmTextGetString(_input);

	if (!textSelect)
		textSelect = _emptyString;
/*
 * Determine source of selection.
 */
	XmString select;

	if (*listSelect && *textSelect)		// both selections set
	{
		select = *selectedItems;
		assert(!strcmp(listSelect, textSelect));
	}
	else if (*listSelect)			// list selection set
	{
		select = *selectedItems;
		XmTextSetString(_input, listSelect);
	}
	else if (*textSelect)			// text selection set
	{
		select = XmStringCreateSimple(textSelect);
	}
	else					// no selections set
	{
		/* do nothing */
	}
/*
 * Validate selection.
 * Need to validate with XmListGetMatchPos even if select XmString
 * came from list widget.
 */
	if (*listSelect || *textSelect)		// any selection set
	{
		int *positions;
		int count;

		if (XmListGetMatchPos(_list, select, &positions, &count))
		{
			assert(count == 1);

			XmListSetPos(_list, myMIN(_itemCount - visibleItems + 1,
				*positions));

			if (!(*listSelect))
			{
				XmListSelectPos(_list, *positions, False);
				XmStringFree(select);
			}

			XtFree((char *) positions);
		}
		else
		{
			/*
			 * If you are here, you got the text value out
			 * of the resource file.  I am going to ignore it.
			 */
			assert(listSelect == _emptyString);

			XmTextSetString(_input, _emptyString);
		}
	}
/*
 * Clean-up.
 */
	if (listSelect != _emptyString)
		XtFree(listSelect);

	if (textSelect != _emptyString)
		XtFree(textSelect);
}

void SLListPicker::checkUnmadeSelection()
{
	assert(_unmadeItemCount >= 0);
	if (!_unmadeItemCount)
		assert(!_unmadeItems);		// should be NULL
	assert(_unmadeSelItemCount == 0 || _unmadeSelItemCount == 1);

	char *listSelect;

	if (_unmadeSelItemCount)
	{
		assert(XmStringGetLtoR(*_unmadeXmSelItem,
			XmSTRING_DEFAULT_CHARSET, &listSelect));
		assert(*listSelect);		// not empty string
	}
	else
	{
		assert(!_unmadeXmSelItem);	// should be NULL
		listSelect = _emptyString;
	}

	if (!_unmadeSelItem)
	{
		_unmadeSelItem = _emptyString;
	}
	else
	{
		// Only NULL input to signify no selection.  Otherwise
		// not sure if I should free.
		assert(strcmp(_unmadeSelItem, _emptyString));
	}
/*
 * Determine source of selection.
 */
	if (*listSelect && *_unmadeSelItem)	// both selections set
	{
		assert(!strcmp(listSelect, _unmadeSelItem));
	}
	else if (*listSelect)			// list selection set
	{
		_unmadeSelItem = new char[strlen(listSelect) + 1];
		strcpy(_unmadeSelItem, listSelect);
	}
	else if (*_unmadeSelItem)		// text selection set
	{
		 _unmadeXmSelItem = (XmString *) XtMalloc(sizeof(XmString));
		*_unmadeXmSelItem = XmStringCreateSimple(_unmadeSelItem);
		_unmadeSelItemCount = 1;
	}
	else					// no selections set
	{
		/* do nothing */
	}
/*
 * Validate selection.
 */
	if (*listSelect || *_unmadeSelItem)	// any selection set
	{
		char *string;
                int   i;
                int   matches;

		for (i = 0, matches = 0; i < _unmadeItemCount; i++)
		{
			assert(XmStringGetLtoR(_unmadeItems[i],
				XmSTRING_DEFAULT_CHARSET, &string));

			if (!strcmp(string, _unmadeSelItem))
				matches++;

			XtFree(string);
		}

		assert(matches == 1);		// Must be 1 match
	}
/*
 * Clean-up.
 */
	if (listSelect != _emptyString)
		XtFree(listSelect);

	if (_unmadeSelItem == _emptyString)
		_unmadeSelItem = (char *) NULL;
}

void SLListPicker::strcpyToUpper(char *to, const char *from)
{
	for (; *from;)
	{
#ifndef VMS
		*to++ = (char) toupper((int) *from++);
#else
		// toupper is screwed-up on VMS
		if (*from >= 'a' && *from <= 'z')
			*to++ = *from++ - ('a' - 'A');
		else
			*to++ = *from++;
#endif
	}

	*to = '\0';
}

void SLListPicker::sortTwoLists(char **key, XmString *other, int num)
{
	int gap, i, j;
	char *tempKey;
	XmString tempOther;

	for (gap = num / 2; gap > 0; gap /= 2)
		for (i = gap; i < num; i++)
			for (j = i - gap; j >= 0; j -= gap)
			{
				if (strcmp(key[j], key[j + gap]) <= 0)
					break;

				tempKey = key[j];
				key[j] = key[j + gap];
				key[j + gap] = tempKey;

				tempOther = other[j];
				other[j] = other[j + gap];
				other[j + gap] = tempOther;
			}
}

void SLListPicker::textCB(Widget w, XtPointer client, XtPointer call)
{
	SLListPicker *obj = (SLListPicker *) client;
	XEvent *event = ((XmAnyCallbackStruct *) call)->event;
	assert(w == obj->_input);

	switch (((XmAnyCallbackStruct *) call)->reason)
	{
		case XmCR_MODIFYING_TEXT_VALUE:
			if (event)
				obj->modifyVerify(
					(XmTextVerifyCallbackStruct *) call);
			break;
		case XmCR_VALUE_CHANGED:
			if (event && obj->_uniqueTextEntry)
				obj->gotTextUnique();
			break;
		case XmCR_ACTIVATE:
			obj->activate(w);
			break;
		default:
			assert(False);
	}
}

void SLListPicker::modifyVerify(XmTextVerifyCallbackStruct *CBStruct)
{
	char *oldText = XmTextGetString(_input);
	if (!oldText)
		oldText = _emptyString;

	char *newText = editString(oldText, CBStruct->text->ptr,
		(int) CBStruct->startPos, (int) CBStruct->endPos,
		CBStruct->text->length);

	int item = listMatchItem(newText);

	int visibleItems, topItem;
	XtVaGetValues(_list,
		XmNvisibleItemCount, &visibleItems,
		XmNtopItemPosition , &topItem     ,
		NULL);

	if (item > 0)		// unique match
	{
		_uniqueTextEntry = True;

		int *positions, count;

		if (XmListGetSelectedPos(_list, &positions, &count))
		{
			if (*positions != item)	// no flashing
				XmListSelectPos(_list, item, False);

			XtFree((char *) positions);
		}
		else
		{
			XmListSelectPos(_list, item, False);
		}

		if (item < topItem || item > topItem + visibleItems - 1)
			XmListSetPos(_list,
				myMIN(_itemCount - visibleItems + 1, item));
	}
	else if (item == 0)	// no match
	{
		CBStruct->doit = False;
	}
	else			// item < 0, non-unique match
	{
		_uniqueTextEntry = False;

		XmListDeselectAllItems(_list);

		if (strlen(newText))
			XmListSetPos(_list,
				myMIN(_itemCount - visibleItems + 1, -item));
	}

	if (oldText != _emptyString)
		XtFree(oldText);

	delete [] newText ;
}

void SLListPicker::activate(Widget w)
{
	char *selection = getSelection();

	if (selection)
		gotTextReturn(selection);
	else
		XBell(XtDisplay(w), 100);
}

void SLListPicker::listCB(Widget w, XtPointer client, XtPointer call)
{
	SLListPicker *obj = (SLListPicker *) client;
	assert(w == obj->_list);

	switch (((XmAnyCallbackStruct *) call)->reason)
	{
		case XmCR_BROWSE_SELECT:
			obj->browseSelection((XmListCallbackStruct *) call);
			break;
		case XmCR_DEFAULT_ACTION:
			obj->gotDoubleClick();
			break;
		default:
			assert(False);
	}
}

void SLListPicker::browseSelection(XmListCallbackStruct *CBStruct)
{
	char *string;

	assert(XmStringGetLtoR(CBStruct->item, XmSTRING_DEFAULT_CHARSET,
		&string));
	XmTextSetString(_input, string);

	XtFree(string);

	gotSingleClick();
}

void SLListPicker::gotSingleClick()
{
	// 2 getSelections so selection is newed 2X, once for each function.

	if (_singleClickFunc)
		(*_singleClickFunc)(getSelection(), _singleClickData);

	singleClick(getSelection());

	callNotifyComplex(SINGLE_CLICK);

	if (_extraStaticFunc)
		(*_extraStaticFunc)();
}

void SLListPicker::gotDoubleClick()
{
	// 2 getSelections so selection is newed 2X, once for each function.

	if (_doubleClickFunc)
		(*_doubleClickFunc)(getSelection(), _doubleClickData);

	doubleClick(getSelection());

	callNotifyComplex(DOUBLE_CLICK);

	if (_extraStaticFunc)
		(*_extraStaticFunc)();
}

void SLListPicker::gotTextUnique()
{
	// 2 getSelections so selection is newed 2X, once for each function.

	if (_textUniqueFunc)
		(*_textUniqueFunc)(getSelection(), _textUniqueData);

	textUnique(getSelection());

	callNotifyComplex(TEXT_UNIQUE);

	if (_extraStaticFunc)
		(*_extraStaticFunc)();
}

void SLListPicker::gotTextReturn(char *selection)
{
	// 2 getSelections so selection is newed 2X, once for each function.

	if (_textReturnFunc)
		(*_textReturnFunc)(getSelection(), _textReturnData);

	textReturn(selection);		// getSelection was in activate.

	callNotifyComplex(TEXT_RETURN);

	if (_extraStaticFunc)
		(*_extraStaticFunc)();
}

char *SLListPicker::editString(char *inString, char *insertString,
	int editStart, int editEnd, int editLength)
{
	char *outString = new char[strlen(inString) + (size_t) editLength + 1];
	char *outPtr    = outString;

	int i;
	char *inPtr;

	for (i = 0, inPtr = inString; i < editStart; i++)
	{
#ifndef VMS
		*outPtr++ = (char ) toupper((int) *inPtr++       );
#else
		// toupper is screwed-up on VMS
		if (*inPtr >= 'a' && *inPtr <= 'z')
			*outPtr++ = *inPtr++ - ('a' - 'A');
		else
			*outPtr++ = *inPtr++;
#endif
	}

	for (i = 0; i < editLength; i++)
	{
#ifndef VMS
		*outPtr++ = (char ) toupper((int) *insertString++);
#else
		// toupper is screwed-up on VMS
		if (*insertString >= 'a' && *insertString <= 'z')
			*outPtr++ = *insertString++ - ('a' - 'A');
		else
			*outPtr++ = *insertString++;
#endif
	}

	for (inPtr = inString + editEnd; *inPtr != '\0';)
	{
#ifndef VMS
		*outPtr++ = (char ) toupper((int) *inPtr++       );
#else
		// toupper is screwed-up on VMS
		if (*inPtr >= 'a' && *inPtr <= 'z')
			*outPtr++ = *inPtr++ - ('a' - 'A');
		else
			*outPtr++ = *inPtr++;
#endif
	}

	*outPtr = '\0';

	return(outString);
}

int SLListPicker::listMatchItem(char *item)
{
	int retval = 0;
	size_t length = strlen(item);

	int i;
	char **theList;
	for (i = 1, theList = _capItems; i <= _itemCount; i++, theList++)
		if (!strncmp(item, *theList, length))
			if (!strcmp(item, *theList))
			{
				return(i);
			}
			else
			{
				if (!retval)
					retval = i;
				else if (retval > 0)
					retval *= -1;
			}

	return(retval);
}

void SLListPicker::getUnmadeResourcesFromRDB()
{
	assert(!_unmadeResources);

	_unmadeItems        = pW()->childItemsDef        (widgetName[list ]);
	_unmadeItemCount    = pW()->childListCountDef    (widgetName[list ]);
	_unmadeXmSelItem    = pW()->childSelectedItemDef (widgetName[list ]);
	_unmadeSelItemCount = pW()->childSelectedCountDef(widgetName[list ]);

	char *string        = pW()->childTextDef         (widgetName[input]);
	if (string)
	{
		_unmadeSelItem = new char[strlen(string) + 1];
		strcpy(_unmadeSelItem, string);
	}
	else
	{
		_unmadeSelItem = (char *) NULL;
	}

	checkUnmadeSelection();

	_unmadeResources = True;
}

void SLListPicker::storeUnmadeResources(char **items, int itemCount,
	char *selected)
{
	assert(itemCount >= 0);

	if (_unmadeResources)
		freeUnmadeResources();

	_unmadeItemCount = itemCount;
	if (_unmadeItemCount)
		_unmadeItems = (XmString *)
			XtMalloc(sizeof(XmString) * (size_t) _unmadeItemCount);
	else
		_unmadeItems = (XmString *) NULL;

	for (int i = 0; i < _unmadeItemCount; i++)
		_unmadeItems[i] = XmStringCreateSimple(items[i]);

	if (selected)
	{
		_unmadeSelItem = new char[strlen(selected) + 1];
		strcpy(_unmadeSelItem, selected);
	}
	else
	{
		_unmadeSelItem = (char *) NULL;
	}

	_unmadeXmSelItem = (XmString *) NULL;	// checkUnmadeSelection will
	_unmadeSelItemCount = 0;		// set from _unmadeSelItem.

	checkUnmadeSelection();

	_unmadeResources = True;
}

void SLListPicker::freeUnmadeResources()
{
	assert(_unmadeResources);

	for (int i = 0; i < _unmadeItemCount; i++)
		XmStringFree(_unmadeItems[i]);

	if (_unmadeItemCount)
		XtFree((char *) _unmadeItems);
	else
		assert(!_unmadeItems);

	switch (_unmadeSelItemCount)
	{
		case 0:
			assert(!_unmadeXmSelItem);
			break;
		case 1:
			XmStringFree(*_unmadeXmSelItem);
			XtFree((char *) _unmadeXmSelItem);
			delete [] _unmadeSelItem;
			break;
		default:
			assert(False);
	}

	_unmadeResources = False;
}
