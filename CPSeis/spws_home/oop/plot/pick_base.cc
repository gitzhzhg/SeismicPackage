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
#include "plot/pick_base.hh"
#include "plot/ll_pick_base.hh"
#include "plot/plot_base.hh"
#include "sl/shell_mode.hh"
#include "sl/shell_mouse_help.hh"

#include <Xm/Xm.h>
#include <assert.h>

/*
 * Without these typedefs, Gnu c++ gags on the 3rd member function array.
 */
/*
 * See comment in pick_base.hh dated 10Nov94
 * typedef void (PickBase::*memberFuncWith2Args)(int, int          );
 * typedef void (PickBase::*memberFuncWith4Args)(int, int, int, int);
 */

Bool PickBase::_first = True;
PickBaseLinkedList *PickBase::_pickBases;

PickBase::PickBase(PlotBase *plot, char *mode, const char * const helpToken,
	const char * const helpFallback, unsigned cursor,
	SecondPress secondPress, Bool stickyModifier,
	void (*extraReleaseStaticFunc)(void))
	: _plot(plot), _secondPress(secondPress),
	_stickyModifier(stickyModifier),
	_extraReleaseStaticFunc(extraReleaseStaticFunc),
	_ignore(False), _frozen(False)
{
	/*
	 * I depend on this assertion being true so button can be used as
	 * index into arrays.
	 */
	assert((Button1 == 1) && (Button2 == 2) && (Button3 == 3));

	Widget widget = _plot->getWidget();
	if (widget)
	{
		if (_first)
		{
			/*
			 * Could have _pickBases as a static PickBaseLinkedList
			 * instead of a static pointer, but that would require
			 * ll_pick_base.hh to be included in pick_base.hh.
			 */
			_pickBases = new PickBaseLinkedList(widget);

			_first = False;
		}

		/* Get currently active before we add this one. */
		PickBase *active = _pickBases->bottom(_plot);

		/*
		 * Add before setting translations so if 1st for this PlotBase,
		 * old translations can be correctly stored.
		 */
		_pickBases->add(this);

		/*
		 * Used for 1st button pressed if _secondPress = beep or abort.
		 * Always 0 if _secondPress = allow.
		 */
		_buttonPressed = 0;

		/*
		 * Save _cursorDisplay so we can XFreeCursor even if
		 * PlotBase widget is gone.
		 */
		_cursorDisplay = _plot->getDisplay();
		_cursor = XCreateFontCursor(_cursorDisplay, cursor);
		XDefineCursor(_plot->getDisplay(), _plot->getDrawable(),
			_cursor);

		_shellMode      = new ShellMode     (widget, mode);
		_shellMouseHelp = new ShellMouseHelp(widget, helpToken,
			helpFallback);

		/* Linked list is in new state. */
		if (active != NULL)
		{
			active->inactiveNotify();

			if (!_extraReleaseStaticFunc
			 && active->_extraReleaseStaticFunc)
			{
				_extraReleaseStaticFunc =
					active->_extraReleaseStaticFunc;
			}
		}

		for (int i = 0; i < _numButtons; i++)
			_pressed[i] = False;

		_widgetInitially = True;
	}
	else
	{
		_widgetInitially = False;
	}
}

PickBase::~PickBase()
{
	if (_widgetInitially)
	{
		void *p;
		PickBase *active = _pickBases->bottom(_plot, &p);

		/*
		 * Is this PickBase currently active?
		 */
		if (active == this)
		{
			/*
			 * Is there any other PickBase in waiting?
			 */
			PickBase *revertTo = _pickBases->prev(_plot, &p);
			if (revertTo != NULL)
			{
				Widget widget = _plot->getWidget();

				if (widget)
					XDefineCursor(_plot->getDisplay(),
						_plot->getDrawable(),
						revertTo->_cursor);

				/* Linked list is still in old state. */
				revertTo->activeNotify();
			}
			/*
			 * If no PickBase waiting, clean up.
			 * Deleting PickBaseElement will restore
			 * initial translations.
			 */
			else if (_plot->getDrawable())
			{
				XUndefineCursor(_plot->getDisplay(),
					_plot->getDrawable());
			}
		}
		/*
		 * If this PickBase is not currently active,
		 * just go away quietly.
		 */
		else
		{
			assert(_pickBases->find(this, &p) != NULL);
			PickBase *nextNewer = _pickBases->next(_plot, &p);
			nextNewer->_shellMode     ->takeRecovery(
				_shellMode     );
			nextNewer->_shellMouseHelp->takeRecovery(
				_shellMouseHelp);
		}

		XFreeCursor(_cursorDisplay, _cursor);

		delete _shellMode     ;
		delete _shellMouseHelp;

		/*
		 * Leave in linked list until end so can be found above.
		 */
		_pickBases->remove(this);
	}
}

void PickBase::setHelpFallback(const char * const fallback)
{
	_shellMouseHelp->setFallback(fallback);
}

void PickBase::changeHelpToken(const char * const token,
	const char * const fallback)
{
	_shellMouseHelp->changeToken(token, fallback);
}

Bool PickBase::isActive()
{
	if (_first)
		return False;
	else
		return (Bool) (this == _pickBases->bottom(getPlot()));
}

int getPickPlots(PlotBase ***plotBases)
{
	if (PickBase::_first)
		return 0;
	else
		return PickBase::_pickBases->getPlots(plotBases);
}

void deleteAllPickBases(PlotBase *plot)
{
	if (!PickBase::_first)
	{
		PickBase *ptr;
		/* Silence bogus compiler warning. */
		PickBase *nextPtr = (PickBase *) NULL;
		void *p;
		for (ptr = PickBase::_pickBases->top(plot, &p);
			ptr;
			ptr = nextPtr)
		{
			/*
			 * Get nextPtr before delete since delete
			 * removes from linked list.
			 */
			nextPtr = PickBase::_pickBases->next(plot, &p);
			delete ptr;
		}
	}
}

void PickBase::pressAction(int x, int y, unsigned btn, unsigned state)
{
/*
 * See comment in pick_base.hh dated 10Nov94
 *	static memberFuncWith2Args pressFunc[_numButtons][_numModifiers] =
 *	{
 *		{&cntlButtonOnePress  , &shiftButtonOnePress  ,
 *			&noModButtonOnePress  },
 *		{&cntlButtonTwoPress  , &shiftButtonTwoPress  ,
 *			&noModButtonTwoPress  },
 *		{&cntlButtonThreePress, &shiftButtonThreePress,
 *			&noModButtonThreePress},
 *	};
 */
        if (btn > 3) btn = 2; // only accept press of wheel (4,5)

	if (_frozen)
	{
		_ignore = True;
		doBeep();
	}

	if (!_ignore)
	{
		Modifier modifier;

		if (getModifier(state, &modifier))
		{
			int bi = (int) btn - 1;
			const unsigned buttonDown = Button1Mask | Button2Mask
				| Button3Mask;

			if ((state & buttonDown) == 0)
			{
				if (_secondPress != allow)
	  				_buttonPressed = btn;

				_pressed[bi] = True;

				_x1[bi] = x;
				_y1[bi] = y;

				if (_stickyModifier)
					_modifier[bi] = modifier;

				/* (this->*pressFunc[bi][modifier])(x, y); */
				           pressFunc(bi, modifier,  x, y);
			}
			else	/* Button already down */
			{
				switch (_secondPress)
				{
					case beep:
						doBeep();
						break;
					case allow:
						_pressed[bi] = True;
						_x1[bi] = x;
						_y1[bi] = y;
						if (_stickyModifier)
							_modifier[bi]=modifier;
					//	(this->*pressFunc[bi][modifier])
					//		(x, y);
						pressFunc(bi, modifier, x, y);
						break;
					case abortPick:
						doAbort();
						break;
					default:
						assert(False);
				}
			}
		}
		else	/* Invalid modifier */
		{
			doBeep();
		}
	}
}

void PickBase::motionAction(int x, int y, unsigned state)
{
/*
 * See comment in pick_base.hh dated 10Nov94
 * static memberFuncWith4Args motionFunc[_numButtons][_numModifiers] =
 * {
 *   {&cntlButtonOneMotion  , &shiftButtonOneMotion  , &noModButtonOneMotion  },
 *   {&cntlButtonTwoMotion  , &shiftButtonTwoMotion  , &noModButtonTwoMotion  },
 *   {&cntlButtonThreeMotion, &shiftButtonThreeMotion, &noModButtonThreeMotion},
 * };
 */

  if (!_ignore)
  {
    Modifier modifier;

    if (_stickyModifier || getModifier(state, &modifier))
    {
      static unsigned buttonMask[_numButtons] =
        { Button1Mask, Button2Mask, Button3Mask };

      if (_buttonPressed != 0)
      {
        int bi = _buttonPressed - 1;
        assert(_pressed[bi]);
        if ((state & buttonMask[bi]) == 0)
        {
          doBeep();
        }
        else
        {
          if (_stickyModifier)
         /* (this->*motionFunc[bi][_modifier[bi]]) (_x1[bi], x, _y1[bi], y); */
                    motionFunc(bi, _modifier[bi] ,  _x1[bi], x, _y1[bi], y);
          else
         /* (this->*motionFunc[bi][ modifier    ]) (_x1[bi], x, _y1[bi], y); */
                    motionFunc(bi,  modifier     ,  _x1[bi], x, _y1[bi], y);
        }
      }
      else if (_secondPress == allow)
      {
        for (int i = 0; i < _numButtons; i++)
          if ((state & buttonMask[i]) != 0 && _pressed[i])
          {
            if (_stickyModifier)
           /* (this->*motionFunc [i] [_modifier[i]]) (_x1[i], x, _y1[i], y); */
                      motionFunc (i,  _modifier[i] ,  _x1[i], x, _y1[i], y);
            else
           /* (this->*motionFunc[i] [ modifier   ] ) (_x1[i], x, _y1[i], y); */
                      motionFunc(i,   modifier     ,  _x1[i], x, _y1[i], y);
          }
      }
    }
    else  /* Invalid modifier */
    {
      doBeep();
    }
  }
}

void PickBase::releaseAction(int x, int y, unsigned btn, unsigned state)
{
/*
 * See comment in pick_base.hh dated 10Nov94
 *	static memberFuncWith4Args releaseFunc[_numButtons][_numModifiers] =
 *	{
 *		{&cntlButtonOneRelease  , &shiftButtonOneRelease  ,
 *			&noModButtonOneRelease  },
 *		{&cntlButtonTwoRelease  , &shiftButtonTwoRelease  ,
 *			&noModButtonTwoRelease  },
 *		{&cntlButtonThreeRelease, &shiftButtonThreeRelease,
 *			&noModButtonThreeRelease},
 *	};
 */
	int bi = btn - 1;

	/*
	 * Make copy in case in case releaseFunc deletes this.
	 */
	void (*extraReleaseStaticFunc)(void) = _extraReleaseStaticFunc;

	if (_ignore)
	{
		static unsigned otherButtonsMask[_numButtons] =
		{
			Button2Mask | Button3Mask,
			Button1Mask | Button3Mask,
			Button1Mask | Button2Mask,
		};

		/* Last button released? */
		if ((state & otherButtonsMask[bi]) == 0)
		{
			_buttonPressed = 0;
			_ignore = False;
		}

		_pressed[bi] = False;
	}
	else
	{
		Modifier modifier;

		if (_stickyModifier || getModifier(state, &modifier))
		{
			if ((btn == _buttonPressed)
				|| (_secondPress == allow && _pressed[bi]))
			{
				/*
				 * Don't bother to check for allow.
				 * Set _buttonPressed before releaseFunc,
				 * in case releaseFunc deletes this.
				 */
				_buttonPressed = 0;
				_pressed[bi] = False;

				if (_stickyModifier)
				     /* (this->*releaseFunc[bi][_modifier[bi]])
						(_x1[bi], x, _y1[bi], y); */
					        releaseFunc(bi, _modifier[bi],
						 _x1[bi], x, _y1[bi], y);
				else
				     /* (this->*releaseFunc[bi][ modifier    ])
						(_x1[bi], x, _y1[bi], y); */
					        releaseFunc(bi,  modifier    ,
						 _x1[bi], x, _y1[bi], y);
			}
		}
		else	/* Invalid modifier */
		{
		  doBeep();
		}
	}

	if (extraReleaseStaticFunc)
		(*extraReleaseStaticFunc)();
}

Bool PickBase::getModifier(unsigned state, Modifier *modifier)
{
	Bool retval;
	const unsigned illegalModifiers = Mod1Mask | Mod2Mask | Mod3Mask
		| Mod4Mask | Mod5Mask;
	const unsigned shiftModifier    = ShiftMask | LockMask;

	Bool illegal    = (state & illegalModifiers) != 0;
	Bool controlled = (state & ControlMask     ) != 0;
	Bool shifted    = (state & shiftModifier   ) != 0;

	if (illegal)
	{
		retval = False;
	}
	else if (controlled && shifted)
	{
		retval = False;
	}
	else if (controlled)
	{
		retval    = True;
		*modifier = cntl;
	}
	else if (shifted)
	{
		retval    = True;
		*modifier = shft;
	}
	else
	{
		retval    = True;
		*modifier = none;
	}

	return retval;
}

void PickBase::doBeep()
{
	if (getPlot()->getDisplay())
		XBell(getPlot()->getDisplay(), 20);
}

void PickBase::pressFunc(int button, Modifier modifier, int x, int y)
{
    Bool wasOK = PlotBase::watchOK();

    if (wasOK)
        PlotBase::setWatchOK(False);

    switch (button)
    {
        case 0:
            switch (modifier)
            {
                case 0 :  cntlButtonOnePress(x, y); break;
                case 1 : shiftButtonOnePress(x, y); break;
                case 2 : noModButtonOnePress(x, y); break;
                default: assert(False);
            }
            break;
        case 1:
            switch (modifier)
            {
                case 0 :  cntlButtonTwoPress(x, y); break;
                case 1 : shiftButtonTwoPress(x, y); break;
                case 2 : noModButtonTwoPress(x, y); break;
                default: assert(False);
            }
            break;
        case 2:
            switch (modifier)
            {
                case 0 :  cntlButtonThreePress(x, y); break;
                case 1 : shiftButtonThreePress(x, y); break;
                case 2 : noModButtonThreePress(x, y); break;
                default: assert(False);
            }
            break;
        default:
            assert(False);
    }

    if (wasOK)
        PlotBase::setWatchOK(True);
}

void PickBase::motionFunc(int button, Modifier modifier,
    int x1, int x2, int y1, int y2)
{
    Bool wasOK = PlotBase::watchOK();

    if (wasOK)
        PlotBase::setWatchOK(False);

    switch (button)
    {
        case 0:
            switch (modifier)
            {
                case 0 :  cntlButtonOneMotion(x1, x2, y1, y2); break;
                case 1 : shiftButtonOneMotion(x1, x2, y1, y2); break;
                case 2 : noModButtonOneMotion(x1, x2, y1, y2); break;
                default: assert(False);
            }
            break;
        case 1:
            switch (modifier)
            {
                case 0 :  cntlButtonTwoMotion(x1, x2, y1, y2); break;
                case 1 : shiftButtonTwoMotion(x1, x2, y1, y2); break;
                case 2 : noModButtonTwoMotion(x1, x2, y1, y2); break;
                default: assert(False);
            }
            break;
        case 2:
            switch (modifier)
            {
                case 0 :  cntlButtonThreeMotion(x1, x2, y1, y2); break;
                case 1 : shiftButtonThreeMotion(x1, x2, y1, y2); break;
                case 2 : noModButtonThreeMotion(x1, x2, y1, y2); break;
                default: assert(False);
            }
            break;
        default:
            assert(False);
    }

    if (wasOK)
        PlotBase::setWatchOK(True);
}

void PickBase::releaseFunc(int button, Modifier modifier,
    int x1, int x2, int y1, int y2)
{
    switch (button)
    {
        case 0:
            switch (modifier)
            {
                case 0 :  cntlButtonOneRelease(x1, x2, y1, y2); break;
                case 1 : shiftButtonOneRelease(x1, x2, y1, y2); break;
                case 2 : noModButtonOneRelease(x1, x2, y1, y2); break;
                default: assert(False);
            }
            break;
        case 1:
            switch (modifier)
            {
                case 0 :  cntlButtonTwoRelease(x1, x2, y1, y2); break;
                case 1 : shiftButtonTwoRelease(x1, x2, y1, y2); break;
                case 2 : noModButtonTwoRelease(x1, x2, y1, y2); break;
                default: assert(False);
            }
            break;
        case 2:
            switch (modifier)
            {
                case 0 :  cntlButtonThreeRelease(x1, x2, y1, y2); break;
                case 1 : shiftButtonThreeRelease(x1, x2, y1, y2); break;
                case 2 : noModButtonThreeRelease(x1, x2, y1, y2); break;
                default: assert(False);
            }
            break;
        default:
            assert(False);
    }
}

void PickBase::changePlotBase(PlotBase *from, PlotBase *to)
{
	if (!_first)
		_pickBases->changePlotBase(from, to);
}
