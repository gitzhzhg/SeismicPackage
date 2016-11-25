#include "fgxp/fgxp_2d_select_pop.hh"
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
#include "geom/fg_constants.hh"
#include "fgxp/fgxp_2d_pop.hh"
#include "sl/slp_text.hh"
#include "sl/sl_option_menu.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_tog_box.hh"
#include "named_constants.h"

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>

#include <assert.h>

static SLPush opts[] = {
	{ "Shot Point"   , (long) FG_SHOT  },
	{ "Inc Distance" , (long) FG_DIST  },
	{ "Cum Distance" , (long) FG_CUM   },
	{ "X loc"        , (long) FG_XLOC  },
	{ "Y loc"        , (long) FG_YLOC  },
	{ "Azimuth"      , (long) FG_AZIM  },
	{ "Elevation"    , (long) FG_ELEV  },
	{ "X grid"       , (long) FG_XGRID },
	{ "Y grid"       , (long) FG_YGRID },
	{ "Hole depth"   , (long) FG_HD    },
	{ "Uphole time"  , (long) FG_TUH   },
	{ "Rcv static"   , (long) FG_RSTAT },
	{ "Src static"   , (long) FG_SSTAT },
	{ "Rcv X skid"   , (long) FG_XSKID },
	{ "Rcv Y skid"   , (long) FG_YSKID },
	{ "Rcv elev skid", (long) FG_ESKID },
};

static String  defres[]= {
	"_popup.title:  2D Crossplot Select",
	"*junk.labelString:",
	"*x_label.labelString:  X",
	"*y_label.labelString:  Y",
	"*type_label.labelString:  Data Type",
	"*index_label1.labelString:  Start Line",
	"*index_label2.labelString:  End Line",
	NULL
};

FgXp2DSelectPop::FgXp2DSelectPop(SLDelay *contain, char *name, HelpCtx hctx,
	FieldGeometry *fg, FgXp2DPop *fgXp2DPop)
	: FgXpSelectPop(contain, name, hctx, fg), _fgXp2DPop(fgXp2DPop)
{
	/* just initializers */
}

Widget FgXp2DSelectPop::make(Widget p)
{
	if (!made())
	{
		Widget parent = p ? p : wParent();

		setDefaultResources(parent, _name, defres);

		FgXpSelectPop::make(p);

		/*
		 * Beginning of RowColumn
		 */
		_rc = XtVaCreateManagedWidget("rc",
			xmRowColumnWidgetClass, topWidget(),
			XmNleftAttachment, XmATTACH_FORM     ,
			XmNtopAttachment , XmATTACH_FORM     ,
			XmNpacking       , XmPACK_COLUMN     ,
			XmNnumColumns    , 3                 ,
			XmNentryAlignment, XmALIGNMENT_CENTER,
			NULL);

		/*
		 * Column 1
		 */
		XtVaCreateManagedWidget("junk",
			xmLabelWidgetClass, _rc,
			NULL);

		XtVaCreateManagedWidget("type_label",
			xmLabelWidgetClass, _rc,
			NULL);

		_indexLabel1 = XtVaCreateManagedWidget("index_label1",
			xmLabelWidgetClass, _rc,
			XmNmappedWhenManaged, False,
			NULL);

		_indexLabel2 = XtVaCreateManagedWidget("index_label2",
			xmLabelWidgetClass, _rc,
			XmNmappedWhenManaged, False,
			NULL);

		/*
		 * Column 2
		 */
		XtVaCreateManagedWidget("x_label",
			xmLabelWidgetClass, _rc,
			NULL);

		_xDataType = new SLOptionMenu(_rc, "x_index_opt",
			getHelpCtx(), opts, XtNumber(opts), (long *) NULL);
		_xDataType->setButton(FG_XLOC);

		_xIndex1 = new SLpText(_rc, "index_text", (long) X_INDEX1,
			SLpText::_LONG);
		_xIndex1->setNotify(this);
		_xIndex1->setIvar(INIL);
		XtVaSetValues(_xIndex1->W(),  /* default is AllLines */
			XmNmappedWhenManaged, False,
			NULL);

		_xIndex2 = new SLpText(_rc, "index_text", (long) X_INDEX2,
			SLpText::_LONG);
		_xIndex2->setNotify(this);
		_xIndex2->setIvar(INIL);
		XtVaSetValues(_xIndex2->W(),  /* default is AllLines */
			XmNmappedWhenManaged, False,
			NULL);

		/*
		 * Column 3
		 */
		Widget yLabel = XtVaCreateManagedWidget("y_label",
			xmLabelWidgetClass, _rc,
			NULL);

		_yDataType = new SLOptionMenu(_rc, "y_index_opt",
			getHelpCtx(), opts, XtNumber(opts), (long *) NULL);
		_yDataType->setButton(FG_YLOC);

		_yIndex1 = new SLpText(_rc, "index_text", (long) Y_INDEX1,
			SLpText::_LONG);
		_yIndex1->setNotify(this);
		_yIndex1->setIvar(INIL);
		XtVaSetValues(_yIndex1->W(),  /* default is AllLines */
			XmNmappedWhenManaged, False,
			NULL);

		_yIndex2 = new SLpText(_rc, "index_text", (long) Y_INDEX2,
			SLpText::_LONG);
		_yIndex2->setNotify(this);
		_yIndex2->setIvar(INIL);
		XtVaSetValues(_yIndex2->W(),  /* default is AllLines */
			XmNmappedWhenManaged, False,
			XmNsensitive        , False,
			NULL);
		/*
		 * End of RowColumn
		 */

		XtVaCreateManagedWidget("junk",
			xmLabelWidgetClass, topWidget(),
			XmNleftAttachment  , XmATTACH_WIDGET         ,
			XmNleftWidget      , _rc                     ,
			XmNrightAttachment , XmATTACH_FORM           ,
			XmNtopAttachment   , XmATTACH_FORM           ,
			XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNbottomWidget    , _rc                     ,
			NULL);

		/*
		 * Make attachments with widgets in base class.
		 */
		makeAttachments(_rc);

		assert(checkIndices() == (char *) NULL);
	}

	return topWidget();
}

void FgXp2DSelectPop::DoAction()
{
	SelectMode selectMode = (SelectMode) _indexMode->WhichSelected();
	long xIndex, yIndex;
	int numIndices;

	if (_toggles->IsSelected(REPLACE))
		_fgXp2DPop->clearPlots();

	switch (selectMode)
	{
		case OneLine:
			xIndex = _fg->findMatchingLineNumber(_xIndex1->ivar());
			yIndex = _fg->findMatchingLineNumber(_yIndex1->ivar());
			assert(-1 != xIndex && -1 != yIndex);
			numIndices = 1;
			break;
		case RangeOfLines:
			xIndex = _fg->findMatchingLineNumber(_xIndex1->ivar());
			yIndex = _fg->findMatchingLineNumber(_yIndex1->ivar());
			assert(-1 != xIndex && -1 != yIndex);
			numIndices = (int) (_fg->findMatchingLineNumber(
				_xIndex2->ivar()) - xIndex + 1);
			assert(numIndices == (int) (_fg->findMatchingLineNumber(
				_yIndex2->ivar()) - yIndex + 1));
			break;
		case AllLines:
		case ActiveLine:
		case SelectedLines:
			/* just avoid compiler warnings. */
			xIndex = yIndex = -1;
			numIndices = -1;
			break;
		default:
			assert(False);
	}

	_fgXp2DPop->addPlot(	selectMode,
				numIndices,
				LdCardType,
				_xDataType->whichSelected(),
				xIndex,
				LdCardType,
				_yDataType->whichSelected(),
				yIndex);

	_textInput = 0;
}

char *FgXp2DSelectPop::checkIndices()
{
	char *retval = (char *) NULL;

	static char *mess1 =
		"Message:  removed line that was input, all inputs reset";
	static char *mess2 =
		"Message:  y range no longer valid, all inputs reset";

	Boolean singleSense = XtIsSensitive(
		_indexMode->GetRadioWidget((long) OneLine));

	Boolean rangeSense  = XtIsSensitive(
		_indexMode->GetRadioWidget((long) RangeOfLines));

	assert(singleSense == rangeSense);

	Bool lines = (Bool) ((int) _fg->numLines() != 0);

	if (!singleSense && lines)	/* First lines added? */
	{
		long firstLine = _fg->getFirstLineNumber();

		_xIndex1->setIvar(firstLine);
		_xIndex2->setIvar(firstLine);
		_yIndex1->setIvar(firstLine);
		_yIndex2->setIvar(firstLine);

		XtSetSensitive(_indexMode->GetRadioWidget((long) OneLine),
			True);

		XtSetSensitive(_indexMode->GetRadioWidget((long) RangeOfLines),
			True);

		XtSetSensitive(_indexMode->GetRadioWidget((long) ActiveLine),
			True);
	}
	else if (singleSense && !lines)	/* Last lines deleted? */
	{
		_indexMode->SetRadio((long) AllLines);

		_xIndex1->setIvar(INIL);
		_xIndex2->setIvar(INIL);
		_yIndex1->setIvar(INIL);
		_yIndex2->setIvar(INIL);

		XtSetSensitive(_indexMode->GetRadioWidget((long) OneLine),
			False);

		XtSetSensitive(_indexMode->GetRadioWidget((long) RangeOfLines),
			False);

		XtSetSensitive(_indexMode->GetRadioWidget((long) ActiveLine),
			False);
	}
	else if (lines)
	{
		long x1 = _fg->findMatchingLineNumber(_xIndex1->ivar());
		long x2 = _fg->findMatchingLineNumber(_xIndex2->ivar());
		long y1 = _fg->findMatchingLineNumber(_yIndex1->ivar());
		long y2 = _fg->findMatchingLineNumber(_yIndex2->ivar());

		if ( -1 == x1 || -1 == x2 || -1 == y1 || -1 == y2)
		{
			retval = mess1;
		}
		else if (x2 - x1 != y2 - y1)
		{
			long y2 = x2 - x1 + y1;

			if (y2 > _fg->numLines() - 1)
				retval = mess2;
			else
				_yIndex2->setIvar(_fg->getLineNumber(y2));
		}

		if (retval)	/* Reset input if error. */
		{
			long firstLine = _fg->getFirstLineNumber();
	
			_xIndex1->setIvar(firstLine);
			_xIndex2->setIvar(firstLine);
			_yIndex1->setIvar(firstLine);
			_yIndex2->setIvar(firstLine);

			_textInput = 0;
		}
	}

	return retval;
}

char *FgXp2DSelectPop::newActiveLine()
{
	char *retval = (char *) NULL;

	static char *mess1 =
		"Message:  max range must be greater than or equal to min";
	static char *mess2 = "Message:  not enough lines for y range";

	long activeLine = _fg->getActiveLineNumber();
	long x1, x2, y1, y2;

	Boolean constantLine = _toggles->IsSelected(CONSTANT_LINE);

	switch (_indexMode->WhichSelected())
	{
		case OneLine:
			switch (_textInput)
			{
				case 0:
					_xIndex1->setIvar(activeLine);
					_xIndex2->setIvar(activeLine);
					_yIndex1->setIvar(activeLine);
					_yIndex2->setIvar(activeLine);

					if (!constantLine)
						_textInput++;
						
					break;
				case 1:
					_yIndex1->setIvar(activeLine);
					_yIndex2->setIvar(activeLine);
					_textInput = 0;
					break;
				default:
					assert(False);
			}
			break;
		case RangeOfLines:
			switch (_textInput)
			{
				case 0:
					_xIndex1->setIvar(activeLine);
					_xIndex2->setIvar(activeLine);
					_yIndex1->setIvar(activeLine);
					_yIndex2->setIvar(activeLine);
					_textInput++;
					break;
				case 1:
					x1 = _fg->findMatchingLineNumber(
						_xIndex1->ivar());
					x2 = _fg->findMatchingLineNumber(
						activeLine);
					assert(-1 != x1 && -1 != x2);
					if (x2 < x1)
					{
						retval = mess1;
					}
					else
					{
						_xIndex2->setIvar(activeLine);
						_yIndex2->setIvar(activeLine);

						if (constantLine)
							_textInput = 0;
						else
							_textInput++;
					}
					break;
				case 2:
					x1 = _fg->findMatchingLineNumber(
						_xIndex1->ivar());
					x2 = _fg->findMatchingLineNumber(
						_xIndex2->ivar());
					y1 = _fg->findMatchingLineNumber(
						activeLine);
					assert(-1 != x1 && -1 != x2
						&& -1 != y1);
					y2 = x2 - x1 + y1;
					if (y2 > _fg->numLines() - 1)
					{
						retval = mess2;
					}
					else
					{
						_yIndex1->setIvar(activeLine);
						_yIndex2->setIvar(
							_fg->getLineNumber(y2));
						_textInput = 0;
					}
					break;
				default:
					assert(False);
			}
			break;
		case AllLines:
		case ActiveLine:
		case SelectedLines:
			/* do nothing */
			break;
		default:
			assert(False);
	}

	return retval;
}

char *FgXp2DSelectPop::textInput(WhichText whichText)
{
	char *retval = (char *) NULL;

	static char *mess1 = "Message:  no such line";
	static char *mess2 =
		"Message:  max range must be greater than or equal to min";
	static char *mess3 = "Message:  invalid y range";

	long x1 = _fg->findMatchingLineNumber(_xIndex1->ivar());
	long x2 = _fg->findMatchingLineNumber(_xIndex2->ivar());
	long y1 = _fg->findMatchingLineNumber(_yIndex1->ivar());
	long y2 = _fg->findMatchingLineNumber(_yIndex2->ivar());

	SelectMode selectMode = (SelectMode) _indexMode->WhichSelected();
	assert(selectMode == OneLine || selectMode == RangeOfLines);

	assert(whichText == X_INDEX1 || whichText == Y_INDEX1
		|| whichText == X_INDEX2);

	Bool y1Changed = False;
	Bool x2Changed = False;
	Bool y2Changed = False;

	if (_toggles->IsSelected(CONSTANT_LINE))
	{
		switch (whichText)
		{
			case X_INDEX1:
				y1 = x1;
				y1Changed = True;
				break;
			case X_INDEX2:
				y2 = x2;
				y2Changed = True;
				break;
			default:
				assert(False);
		}
	}

	if ( -1 == x1 || -1 == x2 || -1 == y1 || -1 == y2)
	{
		retval = mess1;
	}
	else
	{
		/*
		 * Below if elses are more verbose than necessary,
		 * but this is to keep it straight in my mind
		 * what is going on.
		 */

		if (x2 < x1)
		{
			assert(whichText != Y_INDEX1);

			if (selectMode == OneLine)
			{
				if (whichText == X_INDEX1)
				{
					x2 = x1;
					x2Changed = True;
				}
				else	/* X_INDEX2 */
				{
					assert(False);
				}
			}
			else	/* RangeOfLines */
			{
				if (whichText == X_INDEX1)
				{
					x2 = x1;
					x2Changed = True;
				}
				else	/* X_INDEX2 */
				{
					retval = mess2;
				}
			}
		}

		if (!retval && x2 - x1 != y2 - y1)
		{
			y2 = x2 - x1 + y1;

			if (y2 > _fg->numLines() - 1)
			{
				if (selectMode == OneLine)
				{
					if (whichText == X_INDEX1)
					{
						x2 = x1;
						x2Changed = True;
						y2 = y1;
						y2Changed = True;
					}
					else if (whichText == Y_INDEX1)
					{
						x2 = x1;
						x2Changed = True;
						y2 = y1;
						y2Changed = True;
					}
					else	/* X_INDEX2 */
					{
						assert(False);
					}
				}
				else	/* RangeOfLines */
				{
					if (whichText == X_INDEX1)
					{
						x2 = x1;
						x2Changed = True;
						y2 = y1;
						y2Changed = True;
					}
					else if (whichText == Y_INDEX1)
					{
						retval = mess3;
					}
					else	/* X_INDEX2 */
					{
						y1 = x1;
						y1Changed = True;
						y2 = x2;
						y2Changed = True;
					}
				}
			}
			else
			{
				y2Changed = True;
			}
		}

		if (!retval)
		{
			if (y1Changed)
				_yIndex1->setIvar(_fg->getLineNumber(y1));

			if (x2Changed)
				_xIndex2->setIvar(_fg->getLineNumber(x2));

			if (y2Changed)
				_yIndex2->setIvar(_fg->getLineNumber(y2));
		}
	}

	_textInput = 0;

	return retval;
}

void FgXp2DSelectPop::resetText(WhichText whichText)
{
	switch (whichText)
	{
		case X_INDEX1:
			_xIndex1->setIvar(_xIndex1->oldIvar());
			break;
		case Y_INDEX1:
			_yIndex1->setIvar(_yIndex1->oldIvar());
			break;
		case X_INDEX2:
			_xIndex2->setIvar(_xIndex2->oldIvar());
			break;
		default:
			assert(False);
	}
}

void FgXp2DSelectPop::setSelectMode(SelectMode selectMode)
{
	Boolean constantLine = _toggles->IsSelected(CONSTANT_LINE);

	switch (selectMode)
	{
		case OneLine:
			XtMapWidget(_indexLabel1);
			XtMapWidget(_xIndex1->W());

			if (constantLine)
				XtUnmapWidget(_yIndex1->W());
			else
				XtMapWidget  (_yIndex1->W());

			XtUnmapWidget(_indexLabel2);
			XtUnmapWidget(_xIndex2->W());
			XtUnmapWidget(_yIndex2->W());

			_textInput = 0;
			break;
		case RangeOfLines:
			XtMapWidget(_indexLabel1);
			XtMapWidget(_xIndex1->W());
			XtMapWidget(_indexLabel2);
			XtMapWidget(_xIndex2->W());

			if (constantLine)
			{
				XtUnmapWidget(_yIndex1->W());
				XtUnmapWidget(_yIndex2->W());
			}
			else
			{
				XtMapWidget  (_yIndex1->W());
				XtMapWidget  (_yIndex2->W());
			}

			_textInput = 0;
			break;
		case AllLines:
		case ActiveLine:
		case SelectedLines:
			XtUnmapWidget(_indexLabel1);
			XtUnmapWidget(_xIndex1->W());
			XtUnmapWidget(_yIndex1->W());
			XtUnmapWidget(_indexLabel2);
			XtUnmapWidget(_xIndex2->W());
			XtUnmapWidget(_yIndex2->W());
			break;
		default:
			assert(False);
	}

	if (constantLine)
	{
		_yIndex1->setIvar(_xIndex1->ivar());
		_yIndex2->setIvar(_xIndex2->ivar());
	}
}

void FgXp2DSelectPop::freezingDependentUpdates(FieldGeometry *fg)
{
	assert(fg == _fg);

	_fgXp2DPop->freeze();
	            freeze();
}

void FgXp2DSelectPop::postResumeDependentUpdates(FieldGeometry *fg)
{
	assert(fg == _fg);

	_fgXp2DPop->thaw();
	            thaw();
}
