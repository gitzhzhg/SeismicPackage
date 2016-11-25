#include "fggui/fgte_pop.hh"
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
#include "geom/fgte_data.hh"
#include "fggui/fgte_pair.hh"
#include "fggui/fgte_table.hh"
#include "geom/field_geometry.hh"
#include "geom/fg_trace_values.hh"
#include "fggui/fg_status_gui.hh"
#include "fggui/fg_message_gui.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_error_pop.hh"
#include "sl/slp_label.hh"
#include "inquire.h"

static String  defres[]= {
	"*fgte_message.topOffset:        5",
	"*fgte_message.leftOffset:      20",
	"*fgte_message.rightOffset:     20",
	"*fgte_status.leftOffset:       20",
	"*fgte_status.rightOffset:      20",
	"*fgte_pair_Frame.topOffset:    20",
	"*fgte_pair_Frame.leftOffset:   20",
	"*fgte_pair_Frame.rightOffset:  20",
	"*fgte_pair*Fileshell*dirMask:  *.tred",
	"*fgte_label.topOffset:         20",
	"*fgte_label.leftOffset:        20",
	"*fgte_label.rightOffset:       20",
	"*fgte_table.topOffset:         20",
	NULL
};

enum {
	DCP = 11,
	START   ,
	CLEAR
};

FgTePop::FgTePop(SLDelay *contain, char *name, HelpCtx hctx, FieldGeometry *fg,
	SLDelay *dcp, class ContainerList *all_pops)
	: SLFPopSep(contain, name, FP_DOHELP | FP_DOREMOVE, hctx, False, False),
	  _fg(fg), _dcp(dcp), _all_pops(all_pops)
{
	addExtraButton("Data Control...", DCP  );
	addExtraButton("Start"          , START);
	addExtraButton("Clear"          , CLEAR);
}

FgTePop::~FgTePop()
{
	if (made())
	{
		delete _data   ;
		delete _status ;
		delete _message;
		delete _pair   ;
		delete _table  ;
	}
}

Widget FgTePop::make(Widget p)
{
	if (!made())
	{
		Widget parent = p ? p : wParent();
		
		ShellStatMsg bld_info(parent, "Building Trace Edit...");
		
		setDefaultResources(parent, _name, defres);

		/*
		 * All the news are done before the SLFPopSep::make
		 * because otherwise the FgTePair will be made in
		 * its constructor and will call the base class
		 * doValidate.
		 */
		_data = new FgTeData(_fg);
		_fg->getFgTraceValues()->setTred(_data);

		_message = new FgMessageGui(this, "fgte_message", _fg);
		_status  = new FgStatusGui (this, "fgte_status" , _fg,
			_all_pops);

		_pair  = new FgTePair (this, "fgte_pair" , getHelpCtx(), _data,
			this);
		_label = new SLpLabel (this, "fgte_label", 0,
			_data->getStatusMessage());
		_label->setupLabelFun(staticLabelUpdate, _data);
		_table = new FgTeTable(this, "fgte_table",               _data,
			this);

		SLFPopSep::make(p);

		XtVaSetValues(_message->W(),
			XmNtopAttachment   , XmATTACH_FORM    ,
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			NULL);

		XtVaSetValues(_status->W(),
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _message->W()    ,
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			NULL);

		XtVaSetValues(_pair->W(),
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _status->W()     ,
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			NULL);

		XtVaSetValues(_label->W(),
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _pair->W()       ,
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			NULL);

		XtVaSetValues(_table->W(),
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , _label->W()      ,
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNbottomAttachment, XmATTACH_WIDGET  ,
			XmNbottomWidget    , bottomSeparator(),
			NULL);

		XtSetSensitive(getWidget(START), False);
		XtSetSensitive(getWidget(CLEAR), False);
	}
		
	return topWidget();
}

void FgTePop::extraButton(int ident)
{
	switch (ident)
	{
		case DCP  :
			_dcp->makeAndManage();
			break;
		case START:
			if (FILE_ERROR != _pair->openFile())
			{
				XtSetSensitive(getWidget(START), False);
				XtSetSensitive(getWidget(CLEAR), True );

				if (!_pair->fileIsReadOnly())
					_table->setEditable(True);
			}
			break;
		case CLEAR:
			_pair->closeFile();
			_table->setEditable(False);

			/*
			 * Assumes that file that was valid when
			 * started will still be there.  If that
			 * file got rm or mv, error will be detected
			 * in open.
			 */
			XtSetSensitive(getWidget(START), True );
			XtSetSensitive(getWidget(CLEAR), False);
			break;
		default:
			assert(False);
	}
}

void FgTePop::setValidity(int flag)
{
	if (flag)
		XtSetSensitive(getWidget(START), True );
	else
		XtSetSensitive(getWidget(START), False);
}

void FgTePop::updateFile()
{
	char msg[300];	/* copied size from SLFilePairPlus::openFile */

	if (tredfile_put(_pair->workingFilename(), _data->getTredFile(), msg))
	{
		new SLErrorPop(this, "Tred file write error", msg);
	}
}

char *FgTePop::staticLabelUpdate(void *data)
{
	return ((FgTeData *) data)->getStatusMessage();
}
