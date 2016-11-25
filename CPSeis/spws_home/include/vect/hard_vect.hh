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
#ifndef _HARD_VECT_HH
#define _HARD_VECT_HH

#include "vect/vector.hh"
#include "hardcopy/hardcopy_plot.hh"

class HardVect
{
	public:

		HardVect(class VectorLinkedList *vectors, class PlotBase *plot,
			class HardCopyPlot *hard, float scale = 1.0);
		~HardVect()
			{ /* do nothing */ }

	private:

		class PlotBase     *_plot;
		class HardCopyPlot *_hard;
		static int _markers[];
		static int _numMarkers;
		float _scale;

		void plotVector(class Vector *vector);
		void plotPolyFill(class BaseData *data, long id,
			const char *polyFillColor,
			Vector::VectorOffset xIsOffset,
			Vector::VectorOffset yIsOffset);
		void plotLine(class BaseData *data, long id,
			int numColors, const char * const *colors,
			Vector::VectorStyle style, unsigned int width,
			Vector::VectorOffset xIsOffset,
			Vector::VectorOffset yIsOffset,
			Bool alt_line_colors, int pen_lift);
		void plotMultiColorLine(class BaseData *data, long id,
			int numColors, const char * const *colors,
			HCpoint *points, int start_index, int end_index,
			int tot_num_pts);
		void plotArrows(class BaseData *data, long id,
			const char *color, unsigned arrowLength,
			float percentArrowDist,
			Vector::VectorOffset xIsOffset,
			Vector::VectorOffset yIsOffset);
		float getArrowAngle(float deltaXInches, float deltaYInches);
		void plotMarkers(class BaseData *data, long id,
			int numColors, const char * const *colors,
			Vector::VectorMarker marker, unsigned int markerSize,
			Vector::VectorOffset xIsOffset,
			Vector::VectorOffset yIsOffset,
			Bool alt_marker_colors);
		void oneMarkerOneColor(HCpoint *points, int numPts,
			const char *color,
			Vector::VectorMarker marker);
		void oneMarkerMultiColors(HCpoint *points, int numPts,
			const char * const *colors, int numColors,
			Vector::VectorMarker marker,
			class BaseData *data, long id);
		void multiMarkersOneColor(HCpoint *points, int numPts,
			const char *color,
			class BaseData *data, long id);
		void multiMarkersMultiColors(HCpoint *points, int numPts,
			const char * const *colors, int numColors,
			class BaseData *data, long id);
		void plotLabels(class BaseData *data, long id,
			const char *color, const char *label, const char *font,
			VectorLabelPlacement labelPlacement,
			Vector::VectorOffset xIsOffset,
			Vector::VectorOffset yIsOffset);
		int makeHCpointStruct(class BaseData *data, long id,
			Vector::VectorOffset xIsOffset,
			Vector::VectorOffset yIsOffset,
			HCpoint **points);
		Bool offset(class BaseData *data, long id, int index,
			Vector::VectorOffset isOffset,
			/* HPOC compiler chokes with class BaseData. */
			int (BaseData::*getOffsetType)(int, long));
		int setColor(const char *color);

		HardVect()
			{ /* private, no access to default constructor */ }
		HardVect(HardVect &)
			{ /* private, no access to copy constructor */ }
		HardVect& operator=(HardVect &p)
			{ /* private, no access to = */ return p; }
};

#endif /* _HARD_VECT_HH */
