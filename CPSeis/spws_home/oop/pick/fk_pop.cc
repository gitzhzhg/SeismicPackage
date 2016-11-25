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
#include "pick/fk_pop.hh"
#include "pick/fk_pair.hh"
#include "pick/ll_fk_data.hh"
#include "pick/fk_fan_data.hh"
#include "pick/fk_poly_data.hh"
#include "pick/fk_pick.hh"
#include "pick/fk_restrictions.hh"
#include "vect/ll_seis_vect.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_error_pop.hh"
#include "sl/slp_text.hh"
#include "sl/sl_option_menu.hh"
#include "oprim/static_utils.hh"
#include "inquire.h"
#include "named_constants.h"

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>

#include <assert.h>

FkSPTransform::FkSPTransform(FkDataLinkedList *data)
	: _data(data)
{
	/* just initializer */
}

float FkSPTransform::convDataToXOffset(SeisPlot *sp, float data)
{
	return data - sp->getHeaderFromTrace(1, 6) + 1.0F;
}

float FkSPTransform::convXOffsetToData(SeisPlot *sp, float data)
{
	float retval;
	static float last = 0.0F;

	if (data >= 1.0F && data <= (float) sp->memoryTraces())
	{
		float x1 = (float) floor((double) data);
		float x2 = (float) ceil ((double) data);

		if (x1 == x2)
		{
			retval = sp->getHeaderFromTrace((int) x1, 6);
		}
		else
		{
			assert(x2 - x1 == 1.0F);
			float h1 = sp->getHeaderFromTrace((int) x1, 6);
			float h2 = sp->getHeaderFromTrace((int) x2, 6);
			assert(h2 - h1 == 1.0F);
/*			retval = h1 + (h2 - h1) / (x2 - x1) * (data - x1); */
			retval = h1 +                          data - x1 ;
		}

		last = retval;
	}
	else
	{
		retval = last;
	}

	return retval;
}

float FkSPTransform::convDataToYOffset(SeisPlot * /*sp*/, float data)
{
	int minWN, maxWN, nyquistWN;
	float minFreq, maxFreq, freqPerSec;
	_data->getParams(&minWN, &maxWN, &nyquistWN,
		&minFreq, &maxFreq, &freqPerSec);

	return data / freqPerSec;
}

float FkSPTransform::convYOffsetToData(SeisPlot * /*sp*/, float data)
{
	int minWN, maxWN, nyquistWN;
	float minFreq, maxFreq, freqPerSec;
	_data->getParams(&minWN, &maxWN, &nyquistWN,
		&minFreq, &maxFreq, &freqPerSec);

	return data * freqPerSec;
}

#define FIXED_INFO_SIZE 132

static String  defres[] = {
	"_popup.title:                     FK Picking",
	"_popup.iconName:                  FK",
	"*junk.labelString:",
	"*fk_pair_Frame.leftOffset:        10",
	"*fk_pair_Frame.rightOffset:       10",
	"*fk_pair_Frame.topOffset:         10",
	"*fk_header_form.leftOffset:       10",
	"*fk_header_form.rightOffset:      10",
	"*fk_header_form.topOffset:        10",
	"*fk_header_label.labelString:     header words......",
	"*junk.topOffset:                  10",
	NULL
};

char *FkPop::_colors[NUM_COLORS] =
	{ "green", "blue", "cyan", "magenta", "yellow" };

enum { END_PICKING = 11, COPY_PREV, CLEAR, REMOVE };

static SLPush opts[] = {
	{ "Cut" , (long) Cut  },
	{ "Pass", (long) Pass },
};

FkPop::FkPop(SLDelay *contain, char *name, HelpCtx hctx, SeisPlot *sp)
	: SLFPopSep(contain, name, FP_DOOK | FP_DOAPPLY | FP_DOHELP,
	  hctx, True, False),
	  _sp(sp), _pick((FkPick *) NULL), _firstDisplay(1), _scannedDisplay(1),
	  /* _hw1 & _hw2 inited just to avoid compiler warnings. */
	  _hw1(0), _hw2(0)
{
	_data = new FkDataLinkedList();

	_vectors = new SeisVectLinkedList();
	_vectors->addPlot(_sp);

	addSeisPlot(_sp);

	addExtraButton("End Picking", END_PICKING);
#ifndef SINGLE_GND_POS
	addExtraButton("Prev Picks" , COPY_PREV  );
#endif
	addExtraButton("Clear Picks", CLEAR      );
	addExtraButton("Remove"     , REMOVE     );
}

FkPop::~FkPop() 
{
	if (made())
	{
		delete _fkPair;
		delete _whw1  ;
		delete _whw2  ;
	}

	delete _vectors;
	delete _data   ;
}

Widget FkPop::make(Widget p) 
{
	if (!made())
	{
		Widget parent = p ? p : wParent();

		ShellStatMsg bld_info(parent, "Building FK Picking Popup...");

		setDefaultResources(parent, _name, defres);

		/*
		 * FkPair constructed before SLFPopSep::make is called
		 * so virtual FkPair::doValidate can be called when
		 * FkPair is made.  If SLFPopSep::make is called 1st,
		 * doValidate will be called in the FkPair constructor and
		 * SLFilePairPlus::doValidate will execute.
		 */
		_fkPair = new FkPair(this, "fk_pair", getHelpCtx(), _data);

		SLFPopSep::make(p);

		XtVaSetValues(_fkPair->W(),
			XmNleftAttachment , XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment  , XmATTACH_FORM,
			NULL);

		Widget form = XtVaCreateManagedWidget("fk_header_form",
			xmFormWidgetClass, topWidget(),
			XmNleftAttachment , XmATTACH_FORM  ,
			XmNrightAttachment, XmATTACH_FORM  ,
			XmNtopAttachment  , XmATTACH_WIDGET,
			XmNtopWidget      , _fkPair->W()   ,
			NULL);

		Widget label = XtVaCreateManagedWidget("fk_header_label",
			xmLabelWidgetClass, form,
			XmNleftAttachment  , XmATTACH_FORM,
			XmNtopAttachment   , XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			NULL);

		_whw1 = new SLpText(form, "fk_header_text", 0,
			SLpText::_LONG, 2);
#ifdef SINGLE_GND_POS
		_whw1->setSense(0);
		_whw1->setIvar((long) 0);
#else
		_whw1->setIvar(INIL);
#endif
		XtVaSetValues(_whw1->W(),
			XmNleftAttachment  , XmATTACH_WIDGET,
			XmNleftWidget      , label          ,
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_FORM  ,
			NULL);

		_whw2 = new SLpText(form, "fk_header_text", 0,
			SLpText::_LONG, 2);
#ifdef SINGLE_GND_POS
		_whw2->setSense(0);
		_whw2->setIvar((long) 0);
#else
		_whw2->setIvar(INIL);
#endif
		XtVaSetValues(_whw2->W(),
			XmNleftAttachment  , XmATTACH_WIDGET,
			XmNleftWidget      , _whw1->W()     ,
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_FORM  ,
			NULL);

		_cutPass = new SLOptionMenu(form, "cut_pass", getHelpCtx(),
			opts, XtNumber(opts));
		_cutPass->setButton((long) Cut);
		XtVaSetValues(_cutPass->W(),
			XmNleftAttachment  , XmATTACH_WIDGET,
			XmNleftWidget      , _whw2->W()     ,
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_FORM  ,
			NULL);
		_cutPass->setComplexNotify(this);

		XtVaCreateManagedWidget("junk",
			xmLabelWidgetClass, form,
			XmNleftAttachment  , XmATTACH_WIDGET,
			XmNleftWidget      , _cutPass->W()  ,
			XmNrightAttachment , XmATTACH_FORM  ,
			XmNtopAttachment   , XmATTACH_FORM  ,
			XmNbottomAttachment, XmATTACH_FORM  ,
			NULL);

		XtVaCreateManagedWidget("junk",
			xmLabelWidgetClass, topWidget(),
			XmNleftAttachment  , XmATTACH_FORM    ,
			XmNrightAttachment , XmATTACH_FORM    ,
			XmNtopAttachment   , XmATTACH_WIDGET  ,
			XmNtopWidget       , form             ,
			XmNbottomAttachment, XmATTACH_WIDGET  ,
			XmNbottomWidget    , bottomSeparator(),
			NULL);

		XtSetSensitive(getWidget(END_PICKING), False);
#ifndef SINGLE_GND_POS
		XtSetSensitive(getWidget(COPY_PREV  ), False);
#endif
		XtSetSensitive(getWidget(CLEAR      ), False);
	}

	return topWidget();
}

void FkPop::newPlot(SeisPlot *sp)
{
	assert(sp == _sp);

	updateDisplay();
}

void FkPop::preScan(SeisPlot *sp, SeisPlot::ScanDir /*dir*/)
{
	assert(sp == _sp);

#ifndef SINGLE_GND_POS
	float hw1, hw2;
	if (_pick && (0 == getDisplayedHeaders(&hw1, &hw2)))
	{
		_oldHw1 = hw1;
		_oldHw2 = hw2;

		if (_scannedDisplay)
		{
			XtSetSensitive(getWidget(COPY_PREV), True);
			_scannedDisplay = 0;
		}
	}
#endif
}

void FkPop::postScan(SeisPlot *sp, SeisPlot::ScanDir /*dir*/)
{
	assert(sp == _sp);

	updateDisplay();
}

void FkPop::updateDisplay()
{
	if (_data->initialized())
	{
		float hw1, hw2;

		if (0 == getDisplayedHeaders(&hw1, &hw2))
		{
			if (_firstDisplay || hw1 != _hw1 || hw2 != _hw2)
			{
				_firstDisplay = 0;
				_hw1 = hw1;
				_hw2 = hw2;

				SU::holdVectors();

				Vector *vp, *nextVp = (Vector *) NULL;
				void *p;
				for (vp = _vectors->top(&p); vp; vp = nextVp)
				{
					nextVp = _vectors->next(&p);
					_vectors->remove(vp);
				}

				_colorIndex = 0;

				FkData *dp;
				for (dp = _data->top(hw1, hw2, &p);
					dp;
					dp = _data->next(hw1, hw2, &p))
				{
					vp = _vectors->add(dp, nextColor(), 2);

					vp->makeVisible(True);
				}

				SU::flushVectors();

				CutPass cutPass = _data->getCutPass(hw1, hw2);
				switch (cutPass)
				{
					case Unknown:
						/* do nothing */
						break;
					case Cut :
					case Pass:
						_cutPass->setButton(
							(long) cutPass);
						break;
					case Error:
					default:
						assert(0);
				}
			}
		}
		else
		{
			new SLErrorPop(this, "FK Picking Error",
			  "Display has differing values for selected headers");
		}
	}
}

int FkPop::getDisplayedHeaders(float *hw1, float *hw2)
{
	int retval;

	int whichHeader1, whichHeader2;
	_data->getWhichHeaders(&whichHeader1, &whichHeader2);

	if (whichHeader1 || whichHeader2)
	{
		int i;
		for (retval = 0, i = 2,
			*hw1 = (whichHeader1) ?
				_sp->getHeaderFromTrace(1, whichHeader1) : 0.0F,
			*hw2 = (whichHeader2) ?
				_sp->getHeaderFromTrace(1, whichHeader2) : 0.0F;
			i < (int) _sp->memoryTraces();
			i++)
		{
			if ((whichHeader1
			   && *hw1 != _sp->getHeaderFromTrace(i, whichHeader1))
			 || (whichHeader2
			   && *hw2 != _sp->getHeaderFromTrace(i, whichHeader2)))
			{
				retval = -1;
				break;
			}
		}
	}
	else
	{
		*hw1 = 0.0F;
		*hw2 = 0.0F;
		retval = 0;
	}

	return retval;
}

void FkPop::DoAction()
{
	int minWN1, maxWN1, nyquistWN1;
	float minFreq1, maxFreq1, freqPerSec1;
	int minWN2, maxWN2, nyquistWN2;
	float minFreq2, maxFreq2, freqPerSec2;
	int whichHeader1, whichHeader2;
	char *info;

	switch (_fkPair->openFile())
	{
		case FILE_READ_ONLY:
		case FILE_UPDATE:
		case FILE_COPY:
			_data->getParams(&minWN1, &maxWN1, &nyquistWN1,
				&minFreq1, &maxFreq1, &freqPerSec1);

			if (FkDataLinkedList::getGlobalParams(_sp->filename(),
				&minWN2, &maxWN2, &nyquistWN2,
				&minFreq2, &maxFreq2, &freqPerSec2,
				&info))
			{
				_data->clear();
				_fkPair->closeFile();
				new SLErrorPop(this, "FK Picking Error", info);
			}
			else if (minWN1      != minWN2
			      || maxWN1      != maxWN2
			      || nyquistWN1  != nyquistWN2
			      || minFreq1    != minFreq2
			      || maxFreq1    != maxFreq2
			      || freqPerSec1 != freqPerSec2)
			{
				_data->clear();
				_fkPair->closeFile();
				new SLErrorPop(this, "FK Picking Error",
			"Pick file and global file parameters disagree");
			}
			else
			{
				_data->getWhichHeaders(&whichHeader1,
					&whichHeader2);
				_whw1->setIvar((long) whichHeader1);
				_whw2->setIvar((long) whichHeader2);

				start();
			}
			break;
		case FILE_CREATE:
			if (FkDataLinkedList::getGlobalParams(_sp->filename(),
				&minWN2, &maxWN2, &nyquistWN2,
				&minFreq2, &maxFreq2, &freqPerSec2,
				&info))
			{
				_data->clear();
				_fkPair->closeFile();
				new SLErrorPop(this, "FK Picking Error", info);
			}
			else
			{
				_data->setParams(minWN2, maxWN2, nyquistWN2,
					minFreq2, maxFreq2, freqPerSec2);

				whichHeader1 = (int) _whw1->ivar();
				if (whichHeader1 < 1 )
				{
					whichHeader1 = 0;
					_whw1->setIvar((long) whichHeader1);
				}

				whichHeader2 = (int) _whw2->ivar();
				if (whichHeader2 < 1 )
				{
					whichHeader2 = 0;
					_whw2->setIvar((long) whichHeader2);
				}

				_data->setWhichHeaders(whichHeader1,
					whichHeader2);

				char fixedInfo[FIXED_INFO_SIZE];
				if (_data->writeFile(
					_fkPair->workingFilename(), fixedInfo))
				{
					_data->clear();
					_fkPair->closeFile();
					new SLErrorPop(this,
						"FK Picking Error", fixedInfo);
				}

				start();
			}
			break;
		case FILE_ERROR:
			break;
		default:
			assert(0);
	}
}

void FkPop::extraButton(int ident)
{
	switch(ident)
	{
		case END_PICKING:
			stop();
			break;
#ifndef SINGLE_GND_POS
		case COPY_PREV:
			copyPrev();
			break;
#endif
		case CLEAR:
			clear();
			break;
		case REMOVE:
			unmanage();
			if (!_pick)
				activityNotify(PopDownNoActivity);
			break;
		default:
			assert(0);
	}
}

void FkPop::start()
{
	_firstDisplay = _scannedDisplay = 1;

	_oldTrans = _sp->transform();
	_trans = new FkSPTransform(_data);
	_sp->setTransform(_trans);

	updateDisplay();

#ifndef SINGLE_GND_POS
	_whw1->setSense(0);
	_whw2->setSense(0);
#endif

	XtSetSensitive(getWidget(FP_OK      ), False);
	XtSetSensitive(getWidget(FP_APPLY   ), False);
	XtSetSensitive(getWidget(END_PICKING), True );
	XtSetSensitive(getWidget(CLEAR      ), True );

	_pick = new FkPick(this);

	activityNotify(StartingActivity);
}

void FkPop::stop()
{
	SU::holdVectors();

	Vector *vp, *nextVp = (Vector *) NULL;
	void *p;
	for (vp = _vectors->top(&p); vp; vp = nextVp)
	{
		nextVp = _vectors->next(&p);
		_vectors->remove(vp);
	}

	SU::flushVectors();

	_data->clear();

	_sp->setTransform(_oldTrans);
	delete _trans;

	_fkPair->closeFile();

#ifndef SINGLE_GND_POS
	_whw1->setSense(1);
	_whw2->setSense(1);
#endif

	XtSetSensitive(getWidget(FP_OK      ), True );
	XtSetSensitive(getWidget(FP_APPLY   ), True );
	XtSetSensitive(getWidget(END_PICKING), False);
#ifndef SINGLE_GND_POS
	XtSetSensitive(getWidget(COPY_PREV  ), False);
#endif
	XtSetSensitive(getWidget(CLEAR      ), False);

	delete _pick;
	_pick = (FkPick *) NULL;

	unmanage();

	activityNotify(EndingActivity);
}

void FkPop::copyPrev()
{
	assert(!_scannedDisplay);

	float hw1, hw2;
	if (0 == getDisplayedHeaders(&hw1, &hw2))
	{
		if (_oldHw1 != hw1 || _oldHw2 != hw2)
		{
			CutPass cutPass1 = _data->getCutPass(_oldHw1, _oldHw2);
			assert(cutPass1 != Error);
			CutPass cutPass2 = _data->getCutPass(    hw1,     hw2);
			assert(cutPass2 != Error);

			if (cutPass1 != Unknown
			 && cutPass2 != Unknown
			 && cutPass1 != cutPass2)
			{
				_data->setCutPass(hw1, hw2, cutPass1);
			}

			FkData *ptr, *data;
			void *p;
			for (ptr = _data->top (_oldHw1, _oldHw2, &p);
			     ptr;
			     ptr = _data->next(_oldHw1, _oldHw2, &p))
			{
				switch (ptr->getFkType())
				{
					case Fan:
						data = new FkFanData (_data,
							hw1, hw2,
							(FkFanData  *) ptr);
						break;
					case Polygon:
						data = new FkPolyData(_data,
							hw1, hw2,
							(FkPolyData *) ptr);
						break;
					default:
						assert(0);
				}

				_data->addSorted(data);
				Vector *v = _vectors->add(data, nextColor(),2);
				v->makeVisible();
			}

			if (cutPass1 != Unknown)
				_cutPass->setButton((long) cutPass1);

			char fixedInfo[FIXED_INFO_SIZE];
			if (_data->writeFile(_fkPair->workingFilename(),
				fixedInfo))
			{
				new SLErrorPop(this, "FK Picking Error",
					fixedInfo);
			}
		}
	}
	else
	{
		new SLErrorPop(this, "FK Picking Error",
			"Display has differing values for selected headers");
	}
}

void FkPop::clear()
{
	float  hw1,  hw2;
	float vhw1, vhw2;
	if (0 == getDisplayedHeaders(&hw1, &hw2))
	{
		SU::holdVectors();

		Vector *vp, *nextVp = (Vector *) NULL;
		FkData *dp;
		void *p;
		for (vp = _vectors->top(&p); vp; vp = nextVp)
		{
			nextVp = _vectors->next(&p);

			dp = (FkData *) vp->getData();
			dp->getHeaderValues(&vhw1, &vhw2);
			assert(hw1 == vhw1 && hw2 == vhw2);

			_vectors->remove(vp);
			_data->remove(dp);
			delete dp;
		}

		SU::flushVectors();

		char fixedInfo[FIXED_INFO_SIZE];
		if (_data->writeFile(_fkPair->workingFilename(), fixedInfo))
			new SLErrorPop(this, "FK Picking Error", fixedInfo);
	}
}

char *FkPop::nextColor()
{
	char *retval = _colors[_colorIndex++];

	if (_colorIndex == NUM_COLORS)
		_colorIndex = 0;

	return retval;
}

FkPair *FkPop::getFilePair()
{
	return _fkPair;
}

SeisPlot *FkPop::getSeisPlot()
{
	return _sp;
}

FkDataLinkedList *FkPop::getData()
{
	return _data;
}

SeisVectLinkedList *FkPop::getVectors()
{
	return _vectors;
}

Boolean FkPop::notifyComplex(SLDelay *obj, int ident)
{
   if (obj == _cutPass)
   {
      float hw1, hw2;
      char fixedInfo[FIXED_INFO_SIZE];

      switch (ident)
      {
         case Cut:
         case Pass:
            if (_data->initialized() && (0 == getDisplayedHeaders(&hw1, &hw2)))
            {
               CutPass cutPass = _data->getCutPass(hw1, hw2);

               if (cutPass != Unknown && cutPass != (CutPass) ident)
               {
                  _data->setCutPass(hw1, hw2, (CutPass) ident);

                  if (_data->writeFile( _fkPair->workingFilename(), fixedInfo))
                     new SLErrorPop(this, "FK Picking Error", fixedInfo);
               }
            }
            break;
         case Unknown:
         case Error:
         default:
            assert(0);
      }
   }

   return True;
}

CutPass FkPop::getCutPass()
{
	return (CutPass) _cutPass->whichSelected();
}

void FkPop::stopActivity()
{
	stop();
}

void FkPop::managing()
{
	activityNotify(PopUpNoActivity);
}
