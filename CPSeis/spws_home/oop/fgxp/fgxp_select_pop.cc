#include "fgxp/fgxp_select_pop.hh"
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
#include "geom/field_geometry.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/slp_text.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"

#include <Xm/Xm.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <Xm/Label.h>

#include <string.h>
#include <assert.h>

static SLRadio indexRads[] = {
	{ "single_index"  , OneLine       },
	{ "range_indices" , RangeOfLines  },
	{ "all_indices"   , AllLines      },
	{ "xp_active_line", ActiveLine    },	/* already is an active_line */
	{ "selected_lines", SelectedLines },
};

static SLTog toggles[] = {
	{ "constant_line", (long *) NULL, CONSTANT_LINE },
	{ "replace"      , (long *) NULL, REPLACE       },
};

static String  defres[]= {
	"*junk.labelString:",
	"*index_mode*T0.labelString:   Selection Mode",
	"*single_index.labelString:    Single line",
	"*range_indices.labelString:   Range",
	"*all_indices.labelString:     All lines",
	"*xp_active_line.labelString:  Active line",
	"*selected_lines.labelString:  Selected lines",
	"*constant_line.labelString:   Constant line",
	"*constant_line.set:           True",
	"*replace.labelString:         Replace when adding",
	"*replace.set:                 True",
	NULL
};

FgXpSelectPop::FgXpSelectPop(SLDelay *contain, char *name, HelpCtx hctx,
	FieldGeometry *fg)
	: SLFPopSep(contain, name, FP_DOALL, hctx, False, False),
	FgInform(fg), _removedLines(False), _insertedLines(False),
	_newActiveLine(False), _justThawed(False)
{
	/* just initializers */
}

Widget FgXpSelectPop::make(Widget p)
{
	if (!made())
	{
		Widget parent = p ? p : wParent();

		ShellStatMsg bld_info(parent, "Building Select...");

		setDefaultResources(parent, _name, defres);

		SLFPopSep::make(p);

		defaultButtonOK(False);

		/*
		 * First bottom up.
		 */
		_message = new SLpText(topWidget(), "message");
		_message->showLabelAppearance();
		_message->setCvar("Message:");

		XtVaSetValues(_message->W(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNbottomAttachment, XmATTACH_WIDGET  ,
			XmNbottomWidget    , bottomSeparator(),
			NULL);

		Widget midSep = XtVaCreateManagedWidget("message_sep",
			xmSeparatorWidgetClass, topWidget(),
			XmNleftAttachment  , XmATTACH_FORM  ,
			XmNrightAttachment , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_WIDGET,
			XmNbottomWidget    , _message->W()  ,
			NULL);

		/*
		 * Then top down.
		 */
		_topSep = XtVaCreateManagedWidget("top_sep",
			xmSeparatorWidgetClass, topWidget(),
			XmNleftAttachment  , XmATTACH_FORM,
			XmNrightAttachment , XmATTACH_FORM,
			NULL);

		_indexForm = XtVaCreateManagedWidget("index_form",
			xmFormWidgetClass, topWidget(),
			XmNleftAttachment , XmATTACH_FORM  ,
			XmNtopAttachment  , XmATTACH_WIDGET,
			XmNtopWidget      , _topSep        ,
			NULL);

		_indexMode = new SLRadioBox(_indexForm, "index_mode",
			getHelpCtx(), indexRads, XtNumber(indexRads),
			NULL, True, True);

		_indexMode->SetRadio((long) AllLines);

		XtSetSensitive(_indexMode->GetRadioWidget((long) OneLine),
			False);

		XtSetSensitive(_indexMode->GetRadioWidget((long) RangeOfLines),
			False);

		XtSetSensitive(_indexMode->GetRadioWidget((long) ActiveLine),
			False);

		_indexMode->setComplexNotify(this);

		XtVaSetValues(_indexMode->W(),
			XmNrightAttachment,  XmATTACH_POSITION,
			XmNrightPosition   , 50               ,
			XmNtopAttachment   , XmATTACH_FORM    ,
			XmNbottomAttachment, XmATTACH_FORM    ,
			NULL);

		_toggles = new SLTogBox(_indexForm, "fgxp_select_pop_toggles",
			getHelpCtx(), toggles, XtNumber(toggles), True);

		_toggles->setComplexNotify(this);

		XtVaSetValues(_toggles->W(),
			XmNleftAttachment , XmATTACH_WIDGET ,
			XmNleftWidget      , _indexMode->W(),
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_FORM  ,
			NULL);

		XtVaCreateManagedWidget("junk",
			xmLabelWidgetClass, _indexForm,
			XmNleftAttachment  , XmATTACH_FORM  ,
			XmNrightAttachment , XmATTACH_WIDGET,
			XmNrightWidget     , _indexMode->W(),
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_FORM  ,
			NULL);

		XtVaCreateManagedWidget("junk",
			xmLabelWidgetClass, _indexForm,
			XmNleftAttachment  , XmATTACH_WIDGET,
			XmNleftWidget      , _toggles->W()  ,
			XmNrightAttachment , XmATTACH_FORM  ,
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_FORM  ,
			NULL);

		XtVaCreateManagedWidget("junk",
			xmLabelWidgetClass, topWidget(),
			XmNleftAttachment  , XmATTACH_WIDGET         ,
			XmNleftWidget      , _indexForm              ,
			XmNrightAttachment , XmATTACH_FORM           ,
			XmNtopAttachment   , XmATTACH_WIDGET         ,
			XmNtopWidget       , _topSep                 ,
			XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNbottomWidget    , _indexForm              ,
			NULL);

		/*
		 * Bottom up meets top down.
		 */
		XtVaCreateManagedWidget("junk",
			xmLabelWidgetClass, topWidget(),
			XmNleftAttachment  , XmATTACH_FORM  ,
			XmNrightAttachment , XmATTACH_FORM  ,
			XmNtopAttachment   , XmATTACH_WIDGET,
			XmNtopWidget       , _indexForm     ,
			XmNbottomAttachment, XmATTACH_WIDGET,
			XmNbottomWidget    , midSep         ,
			NULL);
	}

	return topWidget();
}

Boolean FgXpSelectPop::notify(SLPrim *obj)
{
	Boolean retval;

	char *errorMessage = (char *) NULL;

	switch ((int) obj->id())
	{
		case FP_OK:
		case FP_APPLY:
		case FP_CANCEL:
		case FP_HELP:
		case FP_REMOVE:
			retval = SLFormPop::notify(obj);
			break;
		case X_INDEX1:
			retval = True;
			errorMessage = textInput(X_INDEX1);
			if (errorMessage)
				resetText(X_INDEX1);
			break;
		case Y_INDEX1:
			retval = True;
			errorMessage = textInput(Y_INDEX1);
			if (errorMessage)
				resetText(Y_INDEX1);
			break;
		case Z_INDEX1:
			retval = True;
			errorMessage = textInput(Z_INDEX1);
			if (errorMessage)
				resetText(Z_INDEX1);
			break;
		case X_INDEX2:
			retval = True;
			errorMessage = textInput(X_INDEX2);
			if (errorMessage)
				resetText(X_INDEX2);
			break;
		case Y_INDEX2:
		case Z_INDEX2:
			assert(False);	/* Should never be sensitive. */
			break;
		default:
			assert(False);
	}

	if (errorMessage)
	{
		if (XtIsManaged(W()))
		{
			_message->setCvar(errorMessage);
			_fg->ringBell();
		}
	}
	else
	{
		if (strcmp(_message->cvar(), "Message:"))
			_message->setCvar("Message:");
	}

	return retval;
}

void FgXpSelectPop::makeAttachments(Widget widget)
{
	XtVaSetValues(_topSep,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget    , widget         ,
		NULL);

	XtVaSetValues(_indexForm,
		XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNrightWidget    , widget                  ,
		NULL);
}

void FgXpSelectPop::preRemoveInsertLines(FieldGeometry *fg, long /*index*/,
	long /*nrem*/, long /*nins*/)
{
	assert(fg == _fg);

	/* do nothing */
}

void FgXpSelectPop::postRemoveInsertLines(FieldGeometry *fg, long /*index*/,
	long nrem, long nins)
{
	assert(fg == _fg);

	if (0 < nrem)
		_removedLines  = True;

	if (0 < nins)
		_insertedLines = True;
}

void FgXpSelectPop::preNewActiveLine(FieldGeometry *fg)
{
	assert(fg == _fg);

	/* do nothing */
}

void FgXpSelectPop::postNewActiveLine(FieldGeometry *fg)
{
	assert(fg == _fg);

	_newActiveLine = True;
}

void FgXpSelectPop::startingChanges(FieldGeometry *fg)
{
	assert(fg == _fg);

	/* do nothing */
}

void FgXpSelectPop::finishedChanges(FieldGeometry *fg)
{
	assert(fg == _fg);

	char *errorMessage = (char *) NULL;

	if (_removedLines || _insertedLines || _justThawed)
	{
		if (made())
			errorMessage = checkIndices();

		_removedLines  = False;
		_insertedLines = False;
		_justThawed    = False;
	}

	if (_newActiveLine)
	{
		if (made() && !errorMessage)
			errorMessage = newActiveLine();

		_newActiveLine = False;
	}

	if (errorMessage)
	{
		if (XtIsManaged(W()))
		{
			_message->setCvar(errorMessage);
			_fg->ringBell();
		}
	}
	else if (made())
	{
		if (strcmp(_message->cvar(), "Message:"))
			_message->setCvar("Message:");
	}
}

void FgXpSelectPop::freeze()
{
	if (made())
	{
		XtSetSensitive(_rc       , False);
		XtSetSensitive(_indexForm, False);
		XtSetSensitive(getWidget(FP_OK   ), False);
		XtSetSensitive(getWidget(FP_APPLY), False);
	}
}

void FgXpSelectPop::thaw()
{
	if (made())
	{
		XtSetSensitive(_rc       , True);
		XtSetSensitive(_indexForm, True);
		XtSetSensitive(getWidget(FP_OK   ), True);
		XtSetSensitive(getWidget(FP_APPLY), True);

		_justThawed = True;
	}
}

Boolean FgXpSelectPop::notifyComplex(SLDelay *obj, int ident)
{
	if (obj == _indexMode)
	{
		setSelectMode((SelectMode) ident);
	}
	else if (obj == _toggles)
	{
		if (ident == CONSTANT_LINE)
			setSelectMode((SelectMode) _indexMode->WhichSelected());
	}

	return True;
}
