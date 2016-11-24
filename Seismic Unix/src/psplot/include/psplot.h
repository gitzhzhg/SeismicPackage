/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* include file for C function interface to PostScript */

#ifndef PAR_H
#include "par.h"
#endif

#ifndef PSPLOT_H
#define PSPLOT_H


/* DEFINES */

/* PostScript limits */
#define PATHLIMIT 1500
#define ARRAYLIMIT 65535
#define STRINGLIMIT 65535

/* LegendBox defines */
#define VERTLEFT 0
#define VERTRIGHT 1
#define HORIBOTTOM 2

/* axes drawing */
#define NONE 0
#define DOT 1
#define DASH 2
#define SOLID 3
#define NORMAL 0
#define SEISMIC 1

/* mark types */
#define NMARKS 9
#define MPLUS 0
#define MASTERISK 1
#define MCROSS 2
#define MTRIANGLE 3
#define MSQUARE 4
#define MCIRCLE 5
#define MFILLEDTRIANGLE 6
#define MFILLEDSQUARE 7
#define MFILLEDCIRCLE 8


/* FUNCTION PROTOTYPES */

/* basic PostScript functions */
void beginps (void);
void endps (void);
void begineps (void);
void endeps (void);
void newpage (const char *label, int ordinal);
void boundingbox (int llx, int lly, int urx, int ury);
void showpage (void);
void gsave (void);
void grestore (void);
void newpath (void);
void closepath (void);
void clip(void);
void translate (float tx, float ty);
void scale (float sx, float sy);
void rotate (float angle);
void concat (float m[]);
void setgray (float gray);
void setrgbcolor (float red, float green, float blue);
void setcymkcolor (float cyan, float magenta, float yellow, float black);
void setcolor (const char *name);
void setlinewidth (float width);
void setlinejoin (int code);
void setdash (float dash[], int ndash, float offset);
void moveto (float x, float y);
void rmoveto (float x, float y);
void lineto (float x, float y);
void rlineto (float x, float y);
void arc (float x, float y, float r, float ang1, float ang2);
void stroke (void);
void fill (void);
void show (const char *str);
void justshow (float just, const char *str);
void image (int w, int h, int bps, float m[], unsigned char *samples);
void rgbimage (int w, int h, int bpc, float m[], unsigned char *samples);
void setfont (const char *fontname, float fontsize);
void fontbbox (const char *fontname, float fontsize, float bbox[]);
float fontheight (const char *fontname, float fontsize);
float fontwidth (const char *fontname, float fontsize);
float fontcapheight (const char *fontname, float fontsize);
float fontxheight (const char *fontname, float fontsize);
float fontdescender (const char *fontname, float fontsize);
float fontascender (const char *fontname, float fontsize);
void polyline (const float *x, const float *y, int n);
void markto (float x, float y, int index, float size);
void rectclip (float x, float y, float width, float height);
void rectfill (float x, float y, float width, float height);
void rectstroke (float x, float y, float width, float height);

/* high-level PostScript functions */
void psAxesBox(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
	float x2Beg, float x2End, float p2Beg, float p2End,
	float d2Num, float f2Num, int n2Tic, int grid2, char *label2,
	char *labelFont, float labelSize,
	char *title, char *titleFont, float titleSize,
	char *titleColor, char *axesColor, char *gridColor,
	float ticwidth, float axeswidth, float gridwidth,
	int style);
void psAxesBox3(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
	float x2Beg, float x2End, float p2Beg, float p2End,
	float d2Num, float f2Num, int n2Tic, int grid2, char *label2,
	char *labelFont, float labelSize,
	char *title, char *titleFont, float titleSize,
	int style, char *title2);
void psAxesBBox(
	float x, float y, float width, float height,
	char *labelFont, float labelSize,
	char *titleFont, float titleSize,
	int style, int bbox[]);
void psAxesBBox3(
	float x, float y, float width, float height,
	char *labelFont, float labelSize,
	char *titleFont, float titleSize,
	int style, int bbox[]);
void psContour (
	float c, int nx, float x[], int ny, float y[], float z[],
	float lcs, char *lcf, char *lcc, float *w, int nplaces);
void psWiggle (
	int n, float z[], float zmin, float zmax, float zbase,
	float yzmin, float yzmax, float xfirst, float xlast, int fill,
	const char *tracecolor);
void psCubeAxesBox(
	float x, float y, float size1, float size2, float size3, float angle,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
	float x2Beg, float x2End, float p2Beg, float p2End,
	float d2Num, float f2Num, int n2Tic, int grid2, char *label2,
	float x3Beg, float x3End, float p3Beg, float p3End,
	float d3Num, float f3Num, int n3Tic, int grid3, char *label3,
	char *labelFont, float labelSize,
	char *title, char *titleFont, float titleSize,
	char *titleColor, char *axesColor, char *gridColor);
void psDrawCurve(
	float x, float y, float width, float height,
	float x1Beg, float x1End, float p1Beg, float p1End, 
	float x2Beg, float x2End, float p2Beg, float p2End,
	float *x1curve, float *x2curve, int ncurve,
	char *curveColor, float curvewidth, int curvedash, int style);
void psLegendBox(float x, float y, float width, float height,
 	float x1Beg, float x1End, float p1Beg, float p1End, 
 	float d1Num, float f1Num, int n1Tic, int grid1, char *label1,
 	char *labelFont, float labelSize,
 	char *axesColor, char *gridColor,
 	int style);
void psLegendBBox(float x, float y, float width, float height,
 	char *labelFont, float labelSize,
 	int style, int bbox[]);
 
#endif /* PSPLOT_H */
