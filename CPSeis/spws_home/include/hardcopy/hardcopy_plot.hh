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
#ifndef HARDCOPYPLOT_H
#define HARDCOPYPLOT_H

#include "hardcopy/hardcopy_color.hh"

typedef struct HCpointStruct { 
                          float x,y;
                      } HCpoint;

class PaperPlot;
class SeisPlot;
class SPTransform;


class HardCopyPlot { 


  private:
      static HardCopyColor *_color;

      unsigned char   _dim_set;            // boolean value
      unsigned char   _range_set;          // boolean value
      float           _width;
      float           _height;
      float           _x0, _x1;
      float           _y0, _y1;
      float           _trace_x0;
      float           _trace_x1;
      float           _xinch;
      float           _yinch;
      float           _minx;      // percent value
      float           _maxx;      // percent value
      float           _miny;      // percent value
      float           _maxy;      // percent value
      float           _x_delta;
      PaperPlot      *_pp;
      float           _top_anno_height;
      float           _bottom_anno_height;
      float           _left_anno_width;
      float           _right_anno_width;
      int             _plot_number;
      float           _xmap;
      int             _headers_decrement;
      SeisPlot       *_sp;
      SPTransform    *_transform;
      float computePlotOffset();
      void  convertToLogarithmicX(HCpoint *pts, int  numpts);

  public:
      enum CoordSystem {PLOT =1, INCHES =2, YplotXinches=3, XplotYinches=4 };
      enum LineType {SolidLine=1, DashLine=2, DotLine=3, DashDotLine=4};
      enum FillType {SolidFill=1, HollowFill=0, PatternFill=2, HatchFill=3};
      enum FontType {
                        Mono_Sans_Serif        =1,
                        Cartographic_Roman     =2,
                        Complex_Script         =3,
                        Simplex_Script         =4,
                        Complex_Italic         =5,
                        Triplex_Italic         =6,
                        Complex_Greek          =7,
                        Complex_Greek2         =8,
                        Triplex_Roman          =9,
                        Complex_Roman          =10,
                        Gothic_English         =11,
                        Gothic_German          =12,
                        Gothic_Italian         =13,
                        Duplex_Roman           =14,
                        Simplex_Roman          =15,
                        Complex_Cyrillic       =16,
                        Simplex_Roman2         =17,
                        Simplex_Roman3         =18,
                        Simplex_Roman4         =19,
                        Helvetica              =20,
                        Duplex_Roman2          =21,
                        Helvetica2             =22,
                        Helvetica_Oblique      =23,
                        Helvetica_Bold         =24,
                        Helvetica_Bold_Oblique =25,
                        Times_Roman            =26,
                        Times_Italic           =27,
                        Times_Bold             =28,
                        Times_Bold_Italic      =29 };

       enum HardCopyLabelPlacement
       {
               Normal     ,
               LowerLeft  ,
               LowerCenter,
               LowerRight ,
               CenterLeft ,
               DeadCenter ,
               CenterRight,
               TopLeft    ,
               TopCenter  ,
               TopRight
       };

      HardCopyPlot(PaperPlot *pp, SeisPlot *sp= (SeisPlot*)0);
      ~HardCopyPlot();

      
      void transformXY(float  *x, float  *y);

      float getX0();
      float getX1();
      float getY0();
      float getY1();
 
      float getTraceX0();
      float getTraceX1();

      float plotWidth();              // width of whole plot
      float plotHeight();             // height of whole plot
      float drawingAreaWidth();       // width of actual graph
      float drawingAreaHeight();      // height of actual graph

      void setLeftBorderWidth(const float w);
      void setRightBorderWidth(const float w);
      void setTopBorderHeight(const float h);
      void setBottomBorderHeight(const float h);

      float leftBorderWidth();
      float rightBorderWidth();
      float topBorderHeight();
      float bottomBorderHeight();

      static float defaultLeftBorderWidth();
      static float defaultRightBorderWidth();
      static float defaultTopBorderHeight();
      static float defaultBottomBorderHeight();

      void setWidthHeight(float xinch,
                          float yinch,
                          float width, 
                          float height);
/***/ void setRange( float x0, float y0, float x1, float y1);
      unsigned char goodFile();          // returns boolean value
      void setTransformation(CoordSystem  coord_sys);
      int getTransformation(CoordSystem  coord_sys);
      void setXMap(float xmap) {_xmap= xmap;}
      float xMap()             { return _xmap;}

// Color Stuff
/***/ static int defineColorIndex( float percent_red,
                                   float percent_green, 
                                   float percent_blue);
      static int  blackColor();
      static void initColor();
      static void resetColor();

// Line Stuff
/***/ void setLineType(LineType line_type);     
/***/ void setLineWidth(float width);
/***/ void setLineColor(int color_index);
/***/ void drawLine( float x1, 
                     float y1, 
                     float x2, 
                     float y2, 
                     CoordSystem  coord_sys =PLOT);
/***/ void drawLines( HCpoint     *pts, 
                      int          numpts, 
                      CoordSystem  coord_sys =PLOT);

// Polygon Stuff
/***/ void setFillType(FillType fill_type);
/***/ void setFillColor(int color_index);
/***/ void drawFilledPolygon( HCpoint     *pts, 
                              int          numpts, 
                              CoordSystem  coord_sys =PLOT);
       

// Text Stuff
/***/ void setTextColor(int color_index);
/***/ void setTextHeight(float height, CoordSystem coord_sys =INCHES);
/***/ void writeText(float        x, 
                     float        y, 
                     char        *text,
                     CoordSystem  coord_sys =PLOT);
      void writeConstrainedText(float        x, 
                                float        y, 
                                float        width,
                                float        height, 
                                char        *text,
                                CoordSystem  coord_sys =PLOT);
/***/ void setFont( enum FontType font);
/***/ void setLabelPlacement( HardCopyLabelPlacement place);


// Marker Stuff
/***/ void setMarkerColor(int color_index);
/***/ void setMarkerSize(float size, CoordSystem  coord_sys =INCHES);
/***/ void setMarkerAngle(float angle);
/***/ void drawMarker(float        x, 
                      float        y, 
                      int          type,
                      CoordSystem  coord_sys =PLOT);


// Raster Stuff
      void drawRasterImage(float        x1,
                           float        y1,
                           float        x2,
                           float        y2,
                           int          x_array_length,
                           int          y_array_length,
                           char        *array,
                           CoordSystem  coord_sys =INCHES);


};
#endif
