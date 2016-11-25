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
#ifndef _VECTOR_HH
#define _VECTOR_HH

#include "oprim/data_user.hh"
#include "vect/label_placement.hh"
#include "vect/ll_plot_base.hh"

#include <Xm/Xm.h>
#include <assert.h>
#include <math.h>

typedef struct _VectorAreaPick {
	long vectorId[6];
	int  numIndices;
	int *index;
} VectorAreaPick;

void freeVectorAreaPick(VectorAreaPick *vap);

class Vector : public DataUser
{
	// Construction and destruction of Vectors through VectorElement
	// so all Vectors are in a VectorLinkedList.
	friend class VectorElement;
	friend class VectorLinkedList;
	friend class FgXpVectLinkedList;	/* Uses static smartStrcpy. */
	friend class FgXpPlotLinkedList;	/* Uses static smartStrcpy. */
        friend class SU;

	public:

		enum VectorStyle
		{
			SolidLine         ,
			DashedLine        ,
			NoLine            ,
			DataSpecifiedStyle
		};

		enum VectorMarker
		{
			CrossMarker         ,
			XMarker             ,
			NMarker             ,
			HorizontalLineMarker,
			VerticalLineMarker  ,
			CircleMarker        ,
			FilledCircleMarker  ,
			SquareMarker        ,
			FilledSquareMarker  ,
			DiamondMarker       ,
			FilledDiamondMarker ,
			TriangleMarker      ,
			FilledTriangleMarker,
			XInSquareMarker     ,
			DataSpecifiedMarker ,
			NoMarker            
		};

		enum VectorOffset
		{
			IsNotOffset        ,
			IsOffset           ,
			DataSpecifiedOffset
		};

		const char *getColor()
			{ return (const char *) _color[0]; }
		void getAllColors(int *numColors, const char * const ** colors)
			{
				*numColors = _numColors;
				*colors = (const char * const *) _color;
			}
		unsigned int getWidth()
			{ return ( _width ? _width : 1 ); }
		class BaseData *getData();
		VectorStyle getStyle()
			{ return _style; }
		void getMarker(VectorMarker *marker, unsigned int *markerSize,
			unsigned int *markerLineWidth)
		{
			*marker          = _marker         ;
			*markerSize      = _markerSize     ;
			*markerLineWidth = _markerLineWidth;
		}
		const char *getLabel(const char **font = (const char **) NULL)
		{
			if (font) *font = _font;
			return (const char *) _label;
		}
		VectorLabelPlacement getLabelPlacement()
			{ return (VectorLabelPlacement) _labelPlacement; }
		const char *getName()
			{ return (const char *) _name; }
		void setData(class BaseData * data);
		Bool setColor(const char *color, Bool ignoreHold = False);
		void setWidth(unsigned int width);
		void setStyle(VectorStyle style);
		void setMarker(VectorMarker marker, unsigned int markerSize,
			unsigned int markerLineWidth);
		void setLabel(const char *label, const char *font = "fixed");
		void setLabelPlacement(VectorLabelPlacement labelPlacement);
		void setName(const char *name)
			{ smartStrcpy(&_name, name); }
		void makeVisible(Bool waitIfHolding = False);
		void makeInvisible();
		void repair(class PlotBase *plot, int x, int y,
			int width, int height);
		float howClose(float x, float y,
			class PlotBase *plot = (class PlotBase *) NULL);
		float howClose(int x, int y, class PlotBase *plot);
		void howCloseToLabel(float x, float y, class PlotBase *plot,
			float *rect, float *pt);
		void howCloseToLabel(int   x, int   y, class PlotBase *plot,
			float *rect, float *pt);
		float minDistance(float x, float y, class PlotBase *plot);
		float maxDistance(float x, float y, class PlotBase *plot);
		Bool isLabel();
		/*
		 * closestIndex (closestIndices) and closestVertex are
	 	 * subtly different.  closestIndex 1st finds the closest
		 * point along the vector polyline to the input x, y
		 * and then the closest index to that vector point.
		 * closestVertex immediately finds the closest index
		 * with no reguard to the closest point along the vector
		 * polyline.  They will often give the same result, but
		 * not always.
		 */
		int closestIndex(float x, float y,
			class PlotBase *plot = (class PlotBase *) NULL);
		int closestIndex(int x, int y, class PlotBase *plot);
		void closestIndices(float x, float y, int *index1, int *index2,
			class PlotBase *plot = (class PlotBase *) NULL);
		void closestIndices(int x, int y, int *index1, int *index2,
			class PlotBase *plot);
		int closestVertex(float x, float y,
			class PlotBase *plot = (class PlotBase *) NULL,
			float *distPtr = (float *) NULL);
		int closestVertex(int   x, int   y,
			class PlotBase *plot, float *distPtr = (float *) NULL);
		Bool isVisible()
			{ return _visible; }
		Bool isRubberBand()
			{ return _rbn; }
		virtual void modIndicesBefore(class BaseData *baseData,
			int startIndex, int numIndices,
			long id = BaseData::defaultId);
		virtual void modIndicesAfter (class BaseData *baseData,
			int startIndex, int numIndices,
			long id = BaseData::defaultId);
		virtual void modDone(class BaseData *baseData,
			long id = BaseData::defaultId);
		virtual void modAttributes(class BaseData *baseData,
			int startIndex, int numIndices, int ignoreHold = 0,
			long id = BaseData::defaultId);
		virtual void modAttributesByIndices(class BaseData *baseData,
			int *indices, int numIndices, int ignoreHold = 0,
			long id = BaseData::defaultId);
		virtual void modAttributesNeedingRepair(
			class BaseData *baseData, int startIndex,
			int numIndices, long id = BaseData::defaultId);
		virtual void modAddedPnts(class BaseData *baseData,
			int startIndex, int numIndices, int ignoreHold = 0,
			long id = BaseData::defaultId);
		void allowAltMarkerColors(Bool allow);
		Bool altMarkerColorsAllowed()
			{ return _allowAltMarkerColors; }
		void allowAltLineColors(Bool allow);
		Bool altLineColorsAllowed()
			{ return _allowAltLineColors; }
		void setAltMarkerColor(int colorIndex, const char *color);
		void setId(long id)
			{ _id = id; }
		long getId()
			{ return _id; }
		void setXYOffsets(VectorOffset  xIsOffset,
			VectorOffset  yIsOffset);
		void getXYOffsets(VectorOffset *xIsOffset,
			VectorOffset *yIsOffset)
			{ *xIsOffset = _xIsOffset; *yIsOffset = _yIsOffset; }
		void setAutoPenLift(int auto_pen_lift);
		int  getAutoPenLift(                 );
		void setDataPenLift(int data_pen_lift);
		int  getDataPenLift(                 );
		friend void holdVectors ();
		friend void flushVectors();
		friend Bool isHoldingVectors();
		void redraw();	/* to put vector on top */
		VectorAreaPick *getIndicesInArea(int x1, int y1, int x2, int y2,
			class PlotBase *plot);
		void polyFillOn (const char *color);
		void polyFillOff();
		Bool isPolyFill()
			{ return _polyFill; }
		const char *getPolyFillColor()
			{ return (const char *) _polyFillColor; }
		void arrowsOn(unsigned arrowLength = 10, int arrowWidth = -1,
			const char *arrowColor = (const char *) NULL, 
			float percentArrowDist = 0.40,
			unsigned minArrowDist = 25, float arrowDegrees = 150.0);
		void arrowsOff();
		Bool isArrows()
			{ return _arrows; }
		void getArrowInfo(unsigned *arrowLength, int *arrowWidth,
			const char **arrowColor, float *percentArrowDist,
			unsigned *minArrowDist, float *arrowDegrees)
		{
			*arrowLength      = _arrowLength     ;
			*arrowWidth       = _arrowWidth      ;
			*arrowColor       = _arrowColor      ;
			*percentArrowDist = _percentArrowDist;
			*minArrowDist     = _minArrowDist    ;
			*arrowDegrees     = _arrowDegrees    ;
		}
		static void setUseXClip(Bool set);
		static unsigned long getColorPixel(Display *display,
			Colormap colormap, char *color);

	private:

		long _id;
		char *_name;
		char **_color;
		int _numColors;
		unsigned int _width;
		Bool _rbn;
		VectorStyle _style;
		VectorMarker _marker;
		VectorLabelPlacement _labelPlacement;
		unsigned int _markerSize;
		short _markerHalfSize;
		unsigned int _markerLineWidth;
		char *_label;
		char *_font;
		Bool _modPending;

		      PlotBaseLinkedList *_plotList;
		class PlotInfoLinkedList *_plotInfo;
		class RbnInfoLinkedList  *_rbnInfo ;
		class VectorLinkedList   *_vectList;

		int _markerX, _markerY;	// To put marker center at data pt.
		Bool _visible;
		static int _lineStyle[NoLine];
		static int _capStyle [NoLine];
		short _xClipMinS, _yClipMinS, _xClipMaxS, _yClipMaxS;
		float _xClipMinF, _yClipMinF, _xClipMaxF, _yClipMaxF;
		/*
		 * When each vector coord. (x or y) was either all world
		 * coords. or all offsets, min/maxs for modIndices could be
		 * kept in world coords.  But since a vector can now be
		 * mixed with world coords. and offsets, min/maxs for
		 * modIndices must be kept individually for each PlotBase
		 * in _plotList.   ehs 28dec94
		 * float _xModMin, _yModMin, _xModMax, _yModMax;
		 */
		class Markers *_markers;
		Bool _allowAltMarkerColors, _allowAltLineColors;
		VectorOffset _xIsOffset, _yIsOffset;

		static Bool _holding;
		static PlotBaseLinkedList _allPlots;
		static Bool _mustRepair;
		static int  _nonModDoneRepairsWhileHolding;

		static int *_drawMarkersIndices      ;
		static int  _drawMarkersIndicesLength;

		Bool  _polyFill;
		char *_polyFillColor;

		Bool     _arrows;
		float    _percentArrowDist, _arrowDegrees, _arrowRadians;
		unsigned _minArrowDist, _arrowLength;
		int      _arrowWidth;
		char    *_arrowColor;

		static Bool _useXClip;

		int _auto_pen_lift;/* if 1, lift pen between straight sements */
		int _data_pen_lift;

		static class VectorColorPixel *_color_pixel;
		
		Vector()
			{ assert(False); }	// No default constructor.
		Vector(	PlotBaseLinkedList *plotList,
			const char *name,
			class BaseData *data,
			class VectorLinkedList *vectList,
			long id = BaseData::defaultId,
			const char *color = "white",
			unsigned int width = 1,
			Bool rbn = False,
			VectorStyle style = SolidLine,
			VectorMarker marker = NoMarker,
			unsigned int markerSize = 5,
			unsigned int markerLineWidth = 1,
			const char *label = (char *) NULL,
			const char *font = "fixed");
		~Vector();
		Vector(Vector &)
			{ assert(False); }	// No copy constructor.
		void draw(class PlotBase *plot, char *color, int x, int y,
			int width, int height);
		void drawLines(class PlotBase *plot, int x, int y,
			int width, int height, Bool doCheck,
			int index, int numPts);
		void drawMarkers(class PlotBase *plot, int x, int y,
			int width, int height, Bool doCheck,
			int startIndex, int endIndex);
		void drawMarkers(class PlotBase *plot, int x, int y,
			int width, int height, Bool doCheck,
			int *indices, int numIndices);
		void drawLabels(class PlotBase *plot, int x, int y,
			int width, int height, Bool doCheck);
		void drawVisible(class PlotBase *plot);
		void undrawVisible(class PlotBase *plot);
		void getUndrawArea(int xInfoMin, int yInfoMin, int xInfoMax,
			int yInfoMax, int labelLeftOffset,
			int labelRightOffset, int labelUpOffset,
			int labelDownOffset,
			int *xMin, int *yMin, int *xMax, int *yMax);
		void setDrawn(class PlotBase *plot, Bool drawnValue);
		void getPtAndBoxInMM(float x, float y, class PlotBase *plot,
			float *xMM , float *yMM ,
			float *x1MM, float *y1MM, float *x2MM, float *y2MM); 
		static float minDistanceInBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float minDistanceLeftOfBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float minDistanceLeftOfAndAboveBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float minDistanceAboveBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float minDistanceRightOfAndAboveBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float minDistanceRightOfBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float minDistanceRightOfAndBelowBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float minDistanceBelowBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float minDistanceLeftOfAndBelowBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float minDistanceScrewUp
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceInBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceLeftOfBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceLeftOfAndAboveBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceAboveBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceRightOfAndAboveBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceRightOfBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceRightOfAndBelowBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceBelowBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceLeftOfAndBelowBox
		  (float x, float y, float x1, float y1, float x2, float y2);
		static float maxDistanceScrewUp
		  (float x, float y, float x1, float y1, float x2, float y2);
		float howClose(float x, float y, int *segNum,
			class PlotBase *plot = (class PlotBase *) NULL);
		float howClose(float x, float y,
			float x1, float y1, float x2, float y2);
		void howCloseToLabel(float x, float y, float xp, float yp,
			float x1, float y1, float x2, float y2,
			float *rect, float *pt);
		void getXY(float *x, float *y, int index, class BaseData *data,
			class PlotBase *plot = (class PlotBase *) NULL);
		void pixelToMM(float xPixel, float yPixel,
			float *xMM, float *yMM, class PlotBase *plot);
		void modDone(class PlotBase *plot, int xMin, int xMax,
			int yMin, int yMax);
		void redrawMarkers(int startIndex, int endIndex,
			Bool doCheck = False);
		void redrawMarkers(int *indices, int numIndices,
			Bool doCheck = False);
		void redrawLines(int index, int numPts);
		void getBoolXYOffsets(Bool *xIsOffset, Bool *yIsOffset);
		void offsetCheck(class PlotBase *plot,
			Bool xOrgChanged, Bool yOrgChanged,
			int *xMin, int *yMin, int *xMax, int *yMax);
		void addPlotInfo(class PlotBase *plot);
		void removePlotInfo(class PlotBase *plot);
		void addRbnInfo(class PlotBase *plot);
		void removeRbnInfo(class PlotBase *plot);
		static void smartStrcpy(char **dst, const char *src);
		Bool clip(short *x1, short *y1, short *x2, short *y2);
		short roundOff(float n);
		void setPoint(XPoint *point, short x, short y, Bool *drawn,
			short *xMin, short *yMin, short *xMax, short *yMax,
			Bool doCheck);
		void checkPoint(short x, short y, Bool *drawn,
			short *xMin, short *yMin, short *xMax, short *yMax);
		float pointToPoint(float x1, float y1, float x2, float y2) 
		{
			double x1D = (double) x1;
			double y1D = (double) y1;
			double x2D = (double) x2;
			double y2D = (double) y2;

			return (float) sqrt(pow(x1D - x2D, 2.0)
				+ pow(y1D - y2D, 2.0));
		}
		void drawRbn  ();
		void undrawRbn();
		void drawRbn  (class PlotBase *plot);
		void undrawRbn(class PlotBase *plot);
		void syncRbn  (class PlotBase *plot);
		void getExtra(int *extraX, int *extraY);
		void initLabel(Display *display, XFontStruct **fontStruct,
			Bool *loadFont, Bool *checkLabel,
			int *labelLeftOffset, int *labelRightOffset,
			int *labelUpOffset, int *labelDownOffset,
			short *labelDrawX, short *labelDrawY);
		void getPixels(int index, class BaseData *data,
			class PlotBase *plot, short *x, short *y);
		void getPixels(float xIn, float yIn,
			Bool xIsOffset, Bool yIsOffset, class PlotBase *plot,
			short *xOut, short *yOut);
		static void repairPlot(class PlotBase *plot,
			int x, int y, int width, int height,
			Bool fromModDone = False);
		Bool colinear       (short x1, short y1, short x2, short y2,
			short x3, short y3);
		Bool changeDirection(short x1, short y1, short x2, short y2,
			short x3, short y3);
		void setToDrawInRepair(class PlotBase *plot);
		Bool checkIfDrawInRepair(class PlotBase *plot, int x, int y,
			int width, int height);
		void resetPlotInfoRange(class PlotBase *plot, Bool *drawn,
			short *xMin, short *yMin, short *xMax, short *yMax);
		Bool boxesOverlap(int x1, int y1, int x2, int y2,
			class PlotBase *plot);
		void checkdrawMarkersIndicesArray(int endIndex);
		void drawPolyFill(class PlotBase *plot, int x, int y,
			int width, int height, Bool doCheck);
		void clipForPolyFill(float *x, float *y,
			int numIn, int *numOut);
		static int clipAtLine(int numIn, float *checkIn, float *otherIn,
			float minMax, int (*function)(float val1, float val2),
			float *checkOut, float *otherOut);
		static int clipPoints(float *check1, float *other1,
			float *check2, float *other2, float minMax,
			int (*function)(float val1, float val2));
		static int clipSmaller(float val1, float val2);
		static int clipGreater(float val1, float val2);
		void drawArrows(class PlotBase *plot, int x, int y,
			int width, int height);
		void drawArrow(int x, int y, double angle,
			Display *display, Drawable drawable, GC gc);
		static Bool mapped(class PlotBase *plot);
		VectorStyle getS();
		VectorStyle getS(VectorStyle style);
};

class Sortable
{
	public:

		Sortable();
		virtual ~Sortable();

		virtual int compar(Sortable *sortable) = 0;
		virtual void complete() = 0;
		virtual char *name() = 0;

	protected:

		int _completed;

	private:

};

class SortedList
{
	public:

		SortedList();
		virtual ~SortedList();

		Sortable *findOrInsert(Sortable *key);

	protected:


	private:

		int        _num_sortables;
		Sortable **_sortables    ;
};

class DisplayAndColormap : public Sortable
{
	public:

		DisplayAndColormap(Display *display, Colormap colormap);
		virtual ~DisplayAndColormap();

		virtual int compar(Sortable *sortable);
		virtual void complete();
		virtual char *name();

		Display  *getDisplay ();
		Colormap  getColormap();
		unsigned long getPixel(char *color_name);

	protected:


	private:

		Display    *_display ;
		Colormap    _colormap;
		SortedList *_colors  ;
};

class ColorAndPixel : public Sortable
{
	public:

		ColorAndPixel(char *color_name,
			DisplayAndColormap *display_and_colormap);
		virtual ~ColorAndPixel();

		virtual int compar(Sortable *sortable);
		virtual void complete();
		virtual char *name();

		unsigned long getPixel();

	protected:


	private:

		char               *_color_name          ;
		DisplayAndColormap *_display_and_colormap;
		unsigned long       _pixel               ;
};

class VectorColorPixel
{
	public:

		VectorColorPixel();
		virtual ~VectorColorPixel();

		unsigned long getColorPixel(Display *display,
			Colormap colormap, char *color);

	protected:


	private:

		SortedList *_displays_and_colormaps;
};

#endif
