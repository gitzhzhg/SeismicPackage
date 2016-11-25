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
/*
 * Modeled after Mike Sherrill's SeisGridPop.
 */

#include "fgxp/fgxp_3d_grid_pop.hh"
#include "fgxp/fgxp_3d_pop.hh"
#include "sl/sl_text_box.hh"
#include "sl/shell_stat_msg.hh"
#include "sp/seis_plot.hh"

#include <Xm/Label.h>

enum { SIZE, XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX };

static SLText texts[]  = {
	{"size", NULL, NULL, SLType_float, SIZE},
	{"xmin", NULL, NULL, SLType_float, XMIN},
	{"xmax", NULL, NULL, SLType_float, XMAX},
	{"ymin", NULL, NULL, SLType_float, YMIN},
	{"ymax", NULL, NULL, SLType_float, YMAX},
	{"zmin", NULL, NULL, SLType_float, ZMIN},
	{"zmax", NULL, NULL, SLType_float, ZMAX},
};

static String  defres[]= {
	"_popup.title:                   3D Grid Menu",
	"*grid_3d_label.fontList:        -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
	"*grid_3d_label.labelString:     3D Grid Coordinates",
	"*grid_3d_label.topOffset:       10",
	"*grid_3d_label.leftOffset:      50",
	"*sizeL.labelString:             Size/Inches:",
	"*xminL.labelString:             Xmin:",
	"*xmaxL.labelString:             Xmax:",
	"*yminL.labelString:             Ymin:",
	"*zmaxL.labelString:             Zmax:",
	"*zminL.labelString:             Zmin:",
	"*ymaxL.labelString:             Ymax:",
	"*junk.labelString:",
	"*junk.rightOffset:              50",
	NULL
};

FgXp3DGridPop::FgXp3DGridPop(SLDelay *contain, char *name, HelpCtx hctx,
	SeisPlot *sp, FgXp3DPop *fgXp3DPop)
	: SLFPopSep(contain, name, FP_DOALL, hctx, True, False),
	  _sp(sp), _fgXp3DPop(fgXp3DPop)
{
	/* just initializers */
}

FgXp3DGridPop::~FgXp3DGridPop()
{
	if (made())
		delete _textBox;
}

Widget FgXp3DGridPop::make(Widget p)
{
	if (!made())
	{
		Widget parent = p ? p : wParent();

		ShellStatMsg bld_info(parent, "Building 3D grid pop...");

		setDefaultResources(parent, _name, defres);

		SLFPopSep::make(p);

		Widget grid_3d_label = XtVaCreateManagedWidget("grid_3d_label",
			xmLabelWidgetClass, topWidget(),
			XmNtopAttachment ,  XmATTACH_FORM,
			XmNleftAttachment,  XmATTACH_FORM,
			NULL);

		texts[0].target = (void *) &_size;
		texts[1].target = (void *) &_xMin;
		texts[2].target = (void *) &_xMax;
		texts[3].target = (void *) &_yMin;
		texts[4].target = (void *) &_yMax;
		texts[5].target = (void *) &_zMin;
		texts[6].target = (void *) &_zMax;

		_textBox = new SLTextBox(this, "text_box", getHelpCtx(),
			texts, XtNumber(texts), True, 1, True);

		XtVaSetValues(_textBox->W(),
			XmNtopAttachment  , XmATTACH_WIDGET         ,
			XmNtopWidget      , grid_3d_label           ,
			XmNleftAttachment , XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget     , grid_3d_label           ,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget    , grid_3d_label           ,
			NULL);

		XtVaCreateManagedWidget("junk", xmLabelWidgetClass, topWidget(),
			XmNtopAttachment   , XmATTACH_OPPOSITE_WIDGET,
			XmNtopWidget       , grid_3d_label           ,
			XmNleftAttachment  , XmATTACH_WIDGET         ,
			XmNleftWidget      , grid_3d_label           ,
			XmNrightAttachment , XmATTACH_FORM           ,
			XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNbottomWidget    , _textBox->W()           ,
			NULL);

		XtVaCreateManagedWidget("junk", xmLabelWidgetClass, topWidget(),
			XmNtopAttachment   , XmATTACH_WIDGET         ,
			XmNtopWidget       , _textBox->W()           ,
			XmNleftAttachment  , XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget      , _textBox->W()           ,
			XmNrightAttachment , XmATTACH_FORM           ,
			XmNbottomAttachment, XmATTACH_WIDGET         ,
			XmNbottomWidget    , bottomSeparator()       ,
			NULL);
	}

	return topWidget();
}

void FgXp3DGridPop::managing()
{
	_textBox->SetValue(SIZE, (float) _sp->gridWidth());

	_fgXp3DPop->getScale(&_xMin, &_xMax, &_yMin, &_yMax, &_zMin, &_zMax);

	_textBox->SetValue(XMIN, _xMin);
	_textBox->SetValue(XMAX, _xMax);
	_textBox->SetValue(YMIN, _yMin);
	_textBox->SetValue(YMAX, _yMax);
	_textBox->SetValue(ZMIN, _zMin);
	_textBox->SetValue(ZMAX, _zMax);
}

void FgXp3DGridPop::DoAction()
{
	if (_size < 0.0)
	{
		_size = -_size;
		_textBox->SetValue(SIZE, _size);
	}
	else if (_size == 0.0)
	{
		_size = 1.0;
		_textBox->SetValue(SIZE, _size);
	}

	_sp->setGridWidth(_size);
	_sp->setGridHeight(_size);

	if (_xMin == _xMax)
	{
		_xMax += 1.0;
		_textBox->SetValue(XMAX, _xMax);
	}

	if (_yMin == _yMax)
	{
		_yMax += 1.0;
		_textBox->SetValue(YMAX, _yMax);
	}

	if (_zMin == _zMax)
	{
		_zMax += 1.0;
		_textBox->SetValue(ZMAX, _zMax);
	}

	/*
	 * False for doDraw since _sp->plot() will do it.
	 */
	_fgXp3DPop->setScale(_xMin, _xMax, _yMin, _yMax, _zMin, _zMax, False);

	_sp->plot();
}
