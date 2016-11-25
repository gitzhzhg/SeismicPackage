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
#ifndef _FGXP_CONSTANTS_HH
#define _FGXP_CONSTANTS_HH

/*
 * You now have a choice of one.
 */
enum CardType
{
	LdCardType
};

/*
 * All 5 SelectModes are for LdCardType.
 */
enum SelectMode
{
	OneLine,
	RangeOfLines,
	AllLines,
	ActiveLine,	/* Line plotted changes with active line. */
	SelectedLines	/* Lines plotted changes with selected lines. */
};

enum DisplayMode
{
	Lines,
	Flags,
	LinesAndFlags,
	LinesAndAutoFlags
};

enum FlagMode
{
	ShowAll,
	ShowSrcsOnly,
	ShowRcvsOnly,
	HideComputed,
	HideUnassigned	/* Not srcs or rcvs */
};

#endif /* _FGXP_CONSTANTS_HH */
