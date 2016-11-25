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
#ifndef _PICK_BASE_HH
#define _PICK_BASE_HH

#include <Xm/Xm.h>
#include <X11/cursorfont.h>
#include <assert.h>

int getPickPlots(class PlotBase ***plotBases);

class PickBase
{
	friend class PickBaseLinkedList;

	public:

		enum SecondPress
		{
			beep     ,
			allow    ,
			abortPick /* avoid conflict with system call abort */
		};

		enum Modifier
		{
			cntl,
			shft,
			none,
			_numModifiers
		};

		enum Action
		{
			press  ,
			motion ,
			release
		};

		PickBase(class PlotBase *plot, char *mode = "picking",
			const char * const helpToken = NULL,
			const char * const helpFallback = NULL,
			unsigned cursor = XC_tcross,
			SecondPress secondPress = beep,
			Bool stickyModifier = True,
			void (*extraReleaseStaticFunc)(void) =
				(void (*)(void)) NULL);
		virtual ~PickBase();
		void setHelpFallback(const char * const fallback);
		void changeHelpToken(const char * const token,
			const char * const fallback = NULL);
		class PlotBase *getPlot()
			{ return _plot; }
		Bool isActive();
		friend int getPickPlots(class PlotBase ***plotBases);
		friend void deleteAllPickBases(class PlotBase *plot);
		void freeze()
			{ _frozen = True; }
		void thaw  ()
			{ _frozen = False; }
		static void changePlotBase(class PlotBase *from,
			class PlotBase *to);

	protected:

		/*
		 * 27 lowest level virtual functions.
		 */
		virtual void cntlButtonOnePress     (int x, int y)
			{ buttonOnePress(x, y, cntl); }
		virtual void shiftButtonOnePress    (int x, int y)
			{ buttonOnePress(x, y, shft); }
		virtual void noModButtonOnePress    (int x, int y)
			{ buttonOnePress(x, y, none); }
		virtual void cntlButtonOneMotion    (int x1, int x2,
			int y1, int y2)
			{ buttonOneMotion(x1, x2, y1, y2, cntl); }
		virtual void shiftButtonOneMotion   (int x1, int x2,
			int y1, int y2)
			{ buttonOneMotion(x1, x2, y1, y2, shft); }
		virtual void noModButtonOneMotion   (int x1, int x2,
			int y1, int y2)
			{ buttonOneMotion(x1, x2, y1, y2, none); }
		virtual void cntlButtonOneRelease   (int x1, int x2,
			int y1, int y2)
			{ buttonOneRelease(x1, x2, y1, y2, cntl); }
		virtual void shiftButtonOneRelease  (int x1, int x2,
			int y1, int y2)
			{ buttonOneRelease(x1, x2, y1, y2, shft); }
		virtual void noModButtonOneRelease  (int x1, int x2,
			int y1, int y2)
			{ buttonOneRelease(x1, x2, y1, y2, none); }
		virtual void cntlButtonTwoPress     (int x, int y)
			{ buttonTwoPress(x, y, cntl); }
		virtual void shiftButtonTwoPress    (int x, int y)
			{ buttonTwoPress(x, y, shft); }
		virtual void noModButtonTwoPress    (int x, int y)
			{ buttonTwoPress(x, y, none); }
		virtual void cntlButtonTwoMotion    (int x1, int x2,
			int y1, int y2)
			{ buttonTwoMotion(x1, x2, y1, y2, cntl); }
		virtual void shiftButtonTwoMotion   (int x1, int x2,
			int y1, int y2)
			{ buttonTwoMotion(x1, x2, y1, y2, shft); }
		virtual void noModButtonTwoMotion   (int x1, int x2,
			int y1, int y2)
			{ buttonTwoMotion(x1, x2, y1, y2, none); }
		virtual void cntlButtonTwoRelease   (int x1, int x2,
			int y1, int y2)
			{ buttonTwoRelease(x1, x2, y1, y2, cntl); }
		virtual void shiftButtonTwoRelease  (int x1, int x2,
			int y1, int y2)
			{ buttonTwoRelease(x1, x2, y1, y2, shft); }
		virtual void noModButtonTwoRelease  (int x1, int x2,
			int y1, int y2)
			{ buttonTwoRelease(x1, x2, y1, y2, none); }
		virtual void cntlButtonThreePress   (int x, int y)
			{ buttonThreePress(x, y, cntl); }
		virtual void shiftButtonThreePress  (int x, int y)
			{ buttonThreePress(x, y, shft); }
		virtual void noModButtonThreePress  (int x, int y)
			{ buttonThreePress(x, y, none); }
		virtual void cntlButtonThreeMotion  (int x1, int x2,
			int y1, int y2)
			{ buttonThreeMotion(x1, x2, y1, y2, cntl); }
		virtual void shiftButtonThreeMotion (int x1, int x2,
			int y1, int y2)
			{ buttonThreeMotion(x1, x2, y1, y2, shft); }
		virtual void noModButtonThreeMotion (int x1, int x2,
			int y1, int y2)
			{ buttonThreeMotion(x1, x2, y1, y2, none); }
		virtual void cntlButtonThreeRelease (int x1, int x2,
			int y1, int y2)
			{ buttonThreeRelease(x1, x2, y1, y2, cntl); }
		virtual void shiftButtonThreeRelease(int x1, int x2,
			int y1, int y2)
			{ buttonThreeRelease(x1, x2, y1, y2, shft); }
		virtual void noModButtonThreeRelease(int x1, int x2,
			int y1, int y2)
			{ buttonThreeRelease(x1, x2, y1, y2, none); }
		/*
		 * 9 button/action virtual void functions.
		 */
		virtual void buttonOnePress    (int x, int y, Modifier modifier)
			{ buttonPress(x, y, 1, modifier); }
		virtual void buttonOneMotion   (int x1, int x2, int y1, int y2,
			Modifier modifier)
			{ buttonMotion(x1, x2, y1, y2, 1, modifier); }
		virtual void buttonOneRelease  (int x1, int x2, int y1, int y2,
			Modifier modifier)
			{ buttonRelease(x1, x2, y1, y2, 1, modifier); }
		virtual void buttonTwoPress    (int x, int y, Modifier modifier)
			{ buttonPress(x, y, 2, modifier); }
		virtual void buttonTwoMotion   (int x1, int x2, int y1, int y2,
			Modifier modifier)
			{ buttonMotion(x1, x2, y1, y2, 2, modifier); }
		virtual void buttonTwoRelease  (int x1, int x2, int y1, int y2,
			Modifier modifier)
			{ buttonRelease(x1, x2, y1, y2, 2, modifier); }
		virtual void buttonThreePress  (int x, int y, Modifier modifier)
			{ buttonPress(x, y, 3, modifier); }
		virtual void buttonThreeMotion (int x1, int x2, int y1, int y2,
			Modifier modifier)
			{ buttonMotion(x1, x2, y1, y2, 3, modifier); }
		virtual void buttonThreeRelease(int x1, int x2, int y1, int y2,
			Modifier modifier)
			{ buttonRelease(x1, x2, y1, y2, 3, modifier); }
		/*
		 * 3 action virtual void functions.
		 * When sub-classing, actions take precedence over buttons.
		 */
		virtual void buttonPress(int x, int y,
			int button, Modifier modifier)
		{
			switch (button)
			{
				case 1:
					buttonOne  (x, x, y, y,
						press, modifier);
					break;
				case 2:
					buttonTwo  (x, x, y, y,
						press, modifier);
					break;
				case 3:
					buttonThree(x, x, y, y,
						press, modifier);
					break;
				default:
					assert(False);
			}
		}
		virtual void buttonMotion(int x1, int x2, int y1, int y2,
			int button, Modifier modifier)
		{
			switch (button)
			{
				case 1:
					buttonOne  (x1, x2, y1, y2,
						motion, modifier);
					break;
				case 2:
					buttonTwo  (x1, x2, y1, y2,
						motion, modifier);
					break;
				case 3:
					buttonThree(x1, x2, y1, y2,
						motion, modifier);
					break;
				default:
					assert(False);
			}
		}
		virtual void buttonRelease(int x1, int x2, int y1, int y2,
			int button, Modifier modifier)
		{
			switch (button)
			{
				case 1:
					buttonOne  (x1, x2, y1, y2,
						release, modifier);
					break;
				case 2:
					buttonTwo  (x1, x2, y1, y2,
						release, modifier);
					break;
				case 3:
					buttonThree(x1, x2, y1, y2,
						release, modifier);
					break;
				default:
					assert(False);
			}
		}
		/*
		 * 3 button virtual void functions.
		 * When sub-classing, actions take precedence over buttons.
		 */
		virtual void buttonOne  (int x1, int x2, int y1, int y2,
			Action action, Modifier modifier)
			{ buttonAny(x1, x2, y1, y2, 1, action, modifier); }
		virtual void buttonTwo  (int x1, int x2, int y1, int y2,
			Action action, Modifier modifier)
			{ buttonAny(x1, x2, y1, y2, 2, action, modifier); }
		virtual void buttonThree(int x1, int x2, int y1, int y2,
			Action action, Modifier modifier)
			{ buttonAny(x1, x2, y1, y2, 3, action, modifier); }
		/*
		 * 1 top level, catch-all virtual function.
		 */
		virtual void buttonAny(int /*x1*/, int /*x2*/,
			int /*y1*/, int /*y2*/, int /*button*/,
			Action /*action*/, Modifier /*modifier*/)
			{ /* no nothing */ }
		/*
		 * Abort virtual function.
		 */
		virtual void doAbort()
			{ assert(False); }

		void doBeep();
		void ignoreActions()
			{ _ignore = True; }

	private:

		class PlotBase *_plot;
		static class PickBaseLinkedList *_pickBases;
		static Bool _first;
		SecondPress _secondPress;
		Bool _stickyModifier;
		int _buttonPressed;
		enum { _numButtons = 3 };
		Bool _pressed[_numButtons];
		Modifier _modifier[_numButtons];
		int _x1[_numButtons], _y1[_numButtons];
		Bool _ignore, _frozen, _widgetInitially;
		Cursor _cursor;
		Display *_cursorDisplay;
		class ShellMode *_shellMode;
		class ShellMouseHelp *_shellMouseHelp;
		void (*_extraReleaseStaticFunc)(void);

		void pressAction  (int x, int y, unsigned btn, unsigned state);
		void motionAction (int x, int y,               unsigned state);
		void releaseAction(int x, int y, unsigned btn, unsigned state);
		Bool getModifier(unsigned state, Modifier *modifier);

		PickBase()
			{ /* private, no access to default constructor */ }
		PickBase(const PickBase &)
			{ /* private, no access to copy constructor */ }
		PickBase& operator=(PickBase &p)
			{ /* private, no access to = */ return p; }

		/*
		 * When activeNotify is called, previously active PickBase
		 * is still at bottom of linked list.  When inactiveNotify
		 * is called new active PickBase is already at bottom of
		 * linked list.
		 */
		virtual void activeNotify()
			{ /* do nothing */ }
		virtual void inactiveNotify()
			{ /* do nothing */ }

		/*
		 * These 3 functions are switch statements which call the
		 * appropriate member function for the button and modifier
		 * given in the argument list.  Originally I called the
		 * appropriate member function using a 2 dimensional array
		 * of member function pointers, one dimension for the
		 * button and one for the modifiers.  The switch
		 * statements are necessary because using a pointer
		 * to a virtual member function always calls the
		 * base class virtual function in g++245.  In g++260
		 * the derived class virtual function is called, as
		 * I expected.  Since I am not sure what the standard
		 * is, and even if I was sure apparently should not take
		 * for granted that the standard is followed, I switched
		 * to switch statements (pun intended).  Use of the arrays
		 * of member function pointers has only been commented out,
		 * so if at some later date all compilers work as I expected,
		 * we can switch back to member function pointers.
		 * ehs  10Nov94
		 */
		void pressFunc  (int button, Modifier modifier, int x, int y);
		void motionFunc (int button, Modifier modifier,
			int x1, int x2, int y1, int y2);
		void releaseFunc(int button, Modifier modifier,
			int x1, int x2, int y1, int y2);
};

#endif /* _PICK_BASE_HH */
