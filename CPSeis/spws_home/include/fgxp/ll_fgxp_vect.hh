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
#ifndef _LL_FGXP_VECT_H
#define _LL_FGXP_VECT_H

#include "vect/ll_seis_vect.hh"
#include "fgxp/fgxp_constants.hh"

class FgXpVectLinkedList : public SeisVectLinkedList
{
	public:

		FgXpVectLinkedList(DisplayMode displayMode,
                        const char *normalLineColor    = "red"    ,
                        const char *sourceLineColor    = "cyan"   ,
                        const char *receiverLineColor  = "magenta",
                        const char *srcAndRcvLineColor = "purple" ,
                        const char *selectedLineColor  = "green"  ,
                        const char *activeLineColor    = "blue"   ,
                        const char *normalFlagColor    = "red"    ,
                        const char *dependentFlagColor = "yellow" ,
                        const char *selectedFlagColor  = "green"  ,
                        const char *activeFlagColor    = "blue"   ,
			int normalFlagColorIndex    = 1,
			int dependentFlagColorIndex = 2,
                        int selectedFlagColorIndex  = 3,
			int activeFlagColorIndex    = 4,
			int activeDependent    = 1, int selectedDependent = 1,
			int srcAndRcvDependent = 1, int receiverDependent = 1,
			int sourceDependent    = 1,
			unsigned int width = 2,
			unsigned int markerSize = 9,
			unsigned int markerLineWidth = 1,
			const char *label = (char *) NULL,
			const char *font = "fixed");
		~FgXpVectLinkedList();
		Vector *add(class BaseData *data, Bool waitIfHolding = False);
		Bool setColor(Vector *vector, Bool ignoreHold);
		void makeNormal(Vector *vector);
		void makeActive(Vector *vector);
		DisplayMode getDisplayMode();
		void setDisplayMode(DisplayMode displayMode);
		void getLineColors(const char **normalLineColor,
			const char **sourceLineColor   ,
			const char **receiverLineColor ,
			const char **srcAndRcvLineColor,
			const char **selectedLineColor ,
			const char **activeLineColor   );
		void setLineColors(const char *normalLineColor,
			const char *sourceLineColor   ,
			const char *receiverLineColor ,
			const char *srcAndRcvLineColor,
			const char *selectedLineColor ,
			const char *activeLineColor   );
		void getFlagColors(const char **normalFlagColor,
			const char **dependentFlagColor,
			const char **selectedFlagColor ,
			const char **activeFlagColor   );
		void setFlagColors(const char *normalFlagColor,
			const char *dependentFlagColor,
			const char *selectedFlagColor ,
			const char *activeFlagColor   );
		void setPrecedence(int activeDependent,
			int selectedDepend    ,
			int srcAndRcvDependent,
			int receiverDependent ,
			int sourceDependent   );
		void getPrecedence(int *activeDependent,
			int *selectedDepend    ,
			int *srcAndRcvDependent,
			int *receiverDependent ,
			int *sourceDependent   );

	private:

		DisplayMode _displayMode;
		char *_normalLineColor, *_sourceLineColor, *_receiverLineColor,
			*_srcAndRcvLineColor, *_selectedLineColor,
			*_activeLineColor;
		char *_normalFlagColor, *_dependentFlagColor,
			*_selectedFlagColor, *_activeFlagColor;
		int _normalFlagColorIndex, _dependentFlagColorIndex,
			_selectedFlagColorIndex, _activeFlagColorIndex;
		int _activeDependent, _selectedDependent, _sourceDependent,
			_srcAndRcvDependent, _receiverDependent;
		unsigned int _width, _markerSize, _markerLineWidth;
		char *_label, *_font;

		const char *getColor(class FgXpData *fgXpData);
};

#endif
