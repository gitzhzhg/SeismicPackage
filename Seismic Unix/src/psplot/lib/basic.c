/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
BASIC - Basic C function interface to PostScript

beginps		write PostScript prolog (including %%Pages comment)
endps		write PostScript trailer (including %%Pages comment)
begineps	write encapsulated PostScript prolog (no %%Pages comment)
endeps		write encapsulated PostScript trailer (no %%Pages comment)
boundingbox	set BoundingBox to llx lly urx ury
newpage		print "%%%%Page: label ordinal" to stdout
showpage	print "showpage" to stdout
gsave		print "GS" to stdout
grestore	print "GR" to stdout
newpath		print "NP" to stdout
closepath	print "CP" to stdout
clip		print "clip" to stdout
translate	print "tx ty TR" to stdout, tx,ty = translation in x,y
scale		print "sx sy SC" to stdout, sx,sy = scaling in x,y
rotate		print "angle RO" to stdout, angle = rotation angle
concat		print "m[0] m[1] m[2] m[3] m[4] m[5] CAT" to stdout
setgray		print "gray setgray" to stdout, gray is 0-255 gray level
setrgbcolor	print "red green blue setrgbcolor" to stdout
			red,green,blue = 0-255 red,green,blue levels
setcolor	set color by name based on definition in color structure
setlinewidth	print "width SLW" to stdout, width = desired line width
setlinejoin	print "code setlinejoin"
setdash		print "[ dash ] offset setdash" to stdout
			dash = array defining dash, offset = dash offset
moveto		print "x y M" to stdout,   move to x,y
rmoveto		print "x y RM" to stdout,  move to x,y
lineto		print "x y L" to stdout, draw a line to x,y
rlineto		print "x y RL" to stdout, draw a line to x,y
arc		print "x y r ang1 ang2 arc" to stdout, draw an arc
			x,y = vertex  r = radius from ang1 to ang2
stroke		print "S" to stdout
fill		print "F" to stdout
show 		print "str SH" to stdout, show a string
justshow	justify and show a string
image		write a sampled gray-scale image
rgbimage	write sampled color (rgb) image
setfont		execute findfont, scalefont, and setfont for specified font
			 and size
fontbbox	determine font bounding box for specified font and size
fontheight	return maximum height for specified font and size
fontwidth	return maximum width for specified font and size
fontcapheight	return maximum capheight for specified font and size
fontxheight	return maximum xheight for specified font and size
fontdescender	return maximum descender for specified font and size
polyline	draw a segmented line
markto		draw a mark at specified location
rectclip	set a rectangular clipping path
rectfill	draw a filled rectangle
strokerect	stroke a rectangle

******************************************************************************
Function Prototypes:
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
void cymkimage (int w, int h, int bpc, float m[], unsigned char *samples);
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

******************************************************************************
justshow:
Input:
just		justification factor
str		string

******************************************************************************
image:
Input:
w		width of image (in samples)
h		height of image (in samples)
bps		number of bits per sample
m		array[6] containing image matrix
samples		array[w*h] of sample values

******************************************************************************
rgbimage:
Input:
w		width of image (in samples)
h		height of image (in samples)
bpc		number of bits per component
m		array[6] containing image matrix
samples		array[3*w*h] of sample values

******************************************************************************
cymkimage:
Input:
w		width of image (in samples)
h		height of image (in samples)
bpc		number of bits per component
m		array[6] containing image matrix
samples		array[4*w*h] of sample values


******************************************************************************
polyline:
Input:
x		array[n] of x-coordinates
y		array[n] of y-coordinates
n		number of points

******************************************************************************
markto:
Input:
x		x-coordinate of mark
y		y-coordinate of mark
index		type of mark to draw
size		size of mark

*****************************************************************************
rectclip:
Input:
x		x-coordinate of clipping path origin
y		y-coordinate of clipping path origin
width		width of clipping path
height		height of clipping path

******************************************************************************
rectfill:
Input:
x		x-coordinate of rectangle origin
y		y-coordinate of rectangle origin
width		width of rectangle
height		height of rectangle

******************************************************************************
strokerect:
Input:
x		x-coordinate of rectangle origin
y		y-coordinate of rectangle origin
width		width of rectangle
height		height of rectangle

******************************************************************************
Notes:
The majority of these routines are self explanatory. They are just
C wrappers that echo PostScript graphics commands.

justshow:
The justification factor positions the string relative to the current point.
"just" may assume any value, but the common uses are:
	-1.0	right-justify the string
	-0.5	center the string on the current point
	 0.0	left-justify the string (like using "show")

image:
Level 1 PostScript implementations support 1, 2, 4, and 8 bits per
sample.  Level 2 adds support for 12 bits per sample.
Samples are hex-encoded, and output lines are limited to 78 characters.

rgbimage:
In general, Level 1 PostScript implementations do not support rgbimage.
Level 2 supports 1, 2, 4, 8, and 12 bits per color component.  The
samples array should contain three color components (in R,G,B... order)
for each sample value.  Samples are hex-encoded, and output lines are
limited to 78 characters.

polyline:
The path is stroked every 200 points.
*****************************************************************************
References:

*****************************************************************************
Author:  Dave Hale, Colorado School of Mines, 1989
with modifications by Craig Artley, Colorado School of Mines, 1991, and
additions by Dave Hale, Advance Geophysical, 1992.
*****************************************************************************/
/**************** end self doc ********************************/

/*
AUTHOR:  Dave Hale, Colorado School of Mines, 06/27/89
MODIFIED:  Craig Artley, Colorado School of Mines, 08/30/91
		Change beginps/endps functions to allow BoundingBox at top.
MODIFIED:  Dave Hale, Advance Geophysical, 10/17/92
		Added function rgbimage().
MODIFIED: Robert Krueger of Terrasys, Germany, 31 July 1998, added X 
		Window style colormap decoding. 
*/

#include <stdio.h>
#include "psplot.h"

/* Conforming PostScript defaults */
static struct {
	int llx,lly,urx,ury;
} BBox = {0,0,612,792};
static int BBoxSet = 0;
static int BBoxOut = 0;
static int nPages = 0;

/* Font metrics (take from the .afm files in the font library) */
#define NFONTS 38
#define FONTSCALE 0.001
typedef struct {
	char *name;
	int fontbbox[4];
	int capheight;
	int xheight;
	int descender;
	int ascender;
} FontMetric;
static FontMetric FontMetrics[NFONTS] = {
{"AvantGarde-Book",{-113,-222,1148,955},740,547,-192,740},
{"AvantGarde-BookOblique",{-113,-222,1279,955},740,547,-192,740},
{"AvantGarde-Demi",{-123, -251, 1222,1021},740,555,-185,740},
{"AvantGarde-DemiOblique",{-123,-251,1256,1021},740,555,-185,740},
{"Bookman-Demi",{-194,-250,1346,934},681,502,-212,725},
{"Bookman-DemiItalic",{-231,-250,1333,941},681,515,-213,732},
{"Bookman-Light",{-188,-251,1266,908},681,484,-228,717},
{"Bookman-LightItalic",{-228,-250,1269,883}, 681,494,-212,717},
{"Courier",{-28, -250, 628, 805}, 562, 426, -157, 629},
{"Courier-Bold",{-113, -250, 749, 801}, 562, 439, -142, 626},
{"Courier-BoldOblique",{-56, -250, 868, 801}, 562, 439, -142, 626},
{"Courier-Oblique",{-28, -250, 742, 805}, 562, 426, -157, 629},
{"Helvetica", {-166, -225, 1000, 931}, 718, 523, -207, 718},
{"Helvetica-Bold",{-170, -228, 1003, 962}, 718, 532, -207, 718},
{"Helvetica-BoldOblique",{-174, -228, 1114, 962}, 718, 532, -207, 718},
{"Helvetica-Oblique",{-170, -225, 1116, 931}, 718, 523, -207, 718},
{"Helvetica-Narrow",{-136,-225,820,931},718,523,-207,718},
{"Helvetica-Narrow-Bold",{-139,-228,822,962},718,532,-207,718},
{"Helvetica-Narrow-BoldOblique",{-143,-228,913,962},718,532,-207,718},
{"Helvetica-Narrow-Oblique",{-139,-225,915,931},718,523,-207,228},
{"NewCentrySchlbk-Bold",{-165,-250,1000,988},722,475,-205,737},
{"NewCenturySchlbk-BoldItalic",{-205,-250,1147,991},722,477,-204,737},
{"NewCenturySchlbk-Roman",{-195,-250,1000,965},722,464,-205,737},
{"Palatino-Bold",{-152, -266, 1000, 924}, 681, 471, -258, 720},
{"Palatino-BoldItalic",{-170,-271,1073,926}, 681,469,-271,726},
{"Palatino-Italics",{-170, -276, 1010, 918}, 692, 482, -276, 733},
{"Palatino-Roman",{-166, -283, 1021, 927}, 692, 469, -281, 726},
{"SanSerif-Bold",{-165,-250,1000,988},722,475,-205,737},
{"SanSerif-BoldItalic",{-205,-250,1147,991},722,477,-204,737},
{"SanSerif-Roman",{-195,-250,1000,965},722,464,-205,737},
{"Symbol",{-180, -293, 1090, 1010}, 729, 525, -219, 729},
{"Times-Bold",{-168, -218, 1000, 935}, 676, 461, -205, 676},
{"Times-BoldItalic",{-200, -218, 996, 921}, 669, 462, -205, 699},
{"Times-Roman",{-168, -218, 1000, 898}, 662, 450, -217, 683},
{"Times-Italic",{-169, -217, 1010, 883}, 653, 441, -205, 683},
{"ZapfChancery-MediumItalic",{-181, -314, 1065, 831}, 708, 438, -314, 714},
{"Unknown",{-174, -220, 1001, 944}, 729, 525, -219, 729},
};

/* Colors from box of 64 crayons, with some additions */
#define NCOLOR 68
static struct {
	char *name;
	float rgb[3];
	float cymk[4];
} colors[NCOLOR+1] = {
	{"greenyellow",{0.85,0,0.31},{0.15,0,0.69,0}},
	{"yellow",{1,1,0},{0,0,1.0,0}},
	{"goldenrod",{1,0.9,0.16},{0,0.10,0.84,0}},
	{"dandelion",{1,0.71,0.16},{0,0.29,0.84,0}},
	{"apricot",{1,0.68,0.48},{0,0.32,0.52,0}},
	{"peach",{1,0.50,0.30},{0,0.50,0.70,0}},
	{"melon",{1,0.54,0.50},{0,0.46,0.50,0}},
	{"yelloworange",{1,0.58,0},{0,0.42,1.,0}},
	{"orange",{1,0.39,0.13},{0,0.61,0.87,0}},
	{"burntorange",{1,0.49,0},{0,0.51,1.,0}},
	{"bittersweet",{0.76,0.01,0},{0,0.75,1.,0.24}},
	{"redorange",{1,0.23,0.13},{0,0.77,0.87,0}},
	{"mahogany",{0.65,0,0},{0,0.85,0.87,0.35}},
	{"maroon",{0.68,0,0},{0,0.87,0.68,0.32}},
	{"brickred",{1,0,0},{0,0.89,0.94,0.28}},
	{"red",{1,0,0},{0,1.,1.,0}},
	{"orangered",{1,0,0.50},{0,1.,0.50,0}},
	{"rubinered",{1,0,0.87},{0,1.,0.13,0}},
	{"wildstrawberry",{1,0.04,0.61},{0,0.96,0.39,0}},
	{"salmon",{1,0.47,0.62},{0,0.53,0.38,0}},
	{"carnationpink",{1,0.37,1},{0,0.63,0,0}},
	{"magenta",{1,0,1},{0,1.,0,0}},
	{"violetred",{1,0.19,1},{0,0.81,0,0}},
	{"rhodamine",{1,0.18,1},{0,0.82,0,0}},
	{"mulberry",{0.64,0.08,0.98},{0.34,0.90,0,0.02}},
	{"redviolet",{0.59,0,0.64},{0.07,0.90,0,0.34}},
	{"fuchsia",{0.45,0.01,0.92},{0.47,0.91,0,0.08}},
	{"lavender",{1,0.52,1},{0,0.48,0,0}},
	{"thistle",{0.88,0.41,1},{0.12,0.59,0,0}},
	{"orchid",{0.68,0.36,1},{0.32,0.64,0,0}},
	{"darkorchid",{0.60,0.20,0.80},{0.40,0.80,0.20,0}},
	{"purple",{0.55,0.14,1},{0.45,0.86,0,0}},
	{"plum",{0.50,0,1},{0.50,1.,0,0}},
	{"violet",{0.21,0.12,1},{0.79,0.88,0,0}},
	{"royalpurple",{0.25,0.10,1},{0.75,0.90,0,0}},
	{"blueviolet",{0.10,0.05,0.96},{0.86,0.91,0,0.04}},
	{"periwinkle",{0.43,0.45,1},{0.57,0.55,0,0}},
	{"cadetblue",{0.38,0.43,0.77},{0.62,0.57,0.23,0}},
	{"cornflowerblue",{0.35,0.87,1},{0.65,0.13,0,0}},
	{"midnightblue",{0,0.44,0.57},{0.98,0.13,0,0.43}},
	{"naveblue",{0.06,0.46,1},{0.94,0.54,0,0}},
	{"royalblue",{0,0.50,1},{1.,0.50,0,0}},
	{"blue",{0,0,1},{1.,1.,0,0}},
	{"cerulean",{0.06,0.89,1},{0.94,0.11,0,0}},
	{"cyan",{0,1,1},{1.,0,0,0}},
	{"processblue",{0.04,1,1},{0.96,0,0,0}},
	{"skyblue",{0.38,1,0.88},{0.62,0,0.12,0}},
	{"turquoise",{0.15,1,0.80},{0.85,0,0.20,0}},
	{"tealblue",{0.12,0.98,0.64},{0.86,0,0.34,0.02}},
	{"aquamarine",{0.18,1,0.70},{0.82,0,0.30,0}},
	{"bluegreen",{0.15,1,0.67},{0.85,0,0.33,0}},
	{"emerald",{0,1,0.50},{1.,0,0.50,0}},
	{"junglegreen",{0.01,1,0.48},{0.99,0,0.52,0}},
	{"seagreen",{0.31,1,0.50},{0.69,0,0.50,0}},
	{"green",{0,1,0},{1.,0,1.,0}},
	{"forestgreen",{0,0.88,0},{0.91,0,0.88,0.12}},
	{"pinegreen",{0,0.75,0.16},{0.92,0,0.59,0.25}},
	{"limegreen",{0.50,1,0},{0.50,0,1.,0}},
	{"yellowgreen",{0.56,1,0.26},{0.44,0,0.74,0}},
	{"springgreen",{0.74,1,0.24},{0.26,0,0.76,0}},
	{"olivegreen",{0,0.60,0},{0.64,0,0.95,0.40}},
	{"rawsienna",{0.55,0,0},{0,0.72,1.,0.45}},
	{"sepia",{0.30,0,0},{0,0.83,1.,0.70}},
	{"brown",{0.40,0,0},{0,0.81,1.,0.60}},
	{"tan",{0.86,0.58,0.44},{0.14,0.42,0.56,0}},
	{"white",{1,1,1},{0,0,0,0}},
	{"black",{0,0,0},{0,0,0,1.}},
	{"gray",{0.5,0.5,0.5},{0,0,0,0.50}},
	{"",{0,0,0},{0,0,0}} /* spare, for arbitrary RGB and CMYK definition */
};

/* PostScript definitions to make output files smaller */
static char *Prologue = "\
/M {moveto} def\n\
/RM {rmoveto} def\n\
/L {lineto} def\n\
/RL {rlineto} def\n\
/S {stroke} def\n\
/F {fill} def\n\
/GS {gsave} def\n\
/GR {grestore} def\n\
/SH {show} def\n\
/SW {stringwidth} def\n\
/NP {newpath} def\n\
/CP {closepath} def\n\
/SC {scale} def\n\
/RO {rotate} def\n\
/TR {translate} def\n\
/CAT {concat} def\n\
/CLW {currentlinewidth} def\n\
/SLW {setlinewidth} def\n\
";

/* private functions (for internal use only) */
static int fontindex(const char *fontname)
/*****************************************************************************
Return index of named font in FontMetrics table
*****************************************************************************/
{
	int i;

	/* look for font name in FontMetrics table */
	for (i=0; i<NFONTS; i++)
		if (0==strcmp(fontname,FontMetrics[i].name))
			return i;

	/* if font name not found, then return index of unknown font */
	return i;
}

static int colorindex(const char *colorname)
/*****************************************************************************
Return index of named color in colors table
*****************************************************************************/
{
	int i;
	char name[256];
    	char *fmt;
	unsigned int  r,g,b;
	float f=0.0;
	
	/* convert color name to lower case */
	strncpy(name,colorname,255);
	for (i=0; i<256 && name[i]!='\0'; ++i)
		name[i] = tolower(name[i]);

	/* look for color name in colors table */
	for (i=0; i<NCOLOR; ++i)
		if (0==strcmp(name,colors[i].name))
			return i;

	/* try to decode RGB color definition with X11 syntax
	   rkr-31jul98 */
	
	if (name[0] == '#') {
	    fmt = NULL;
	    switch (strlen(name+1)) {
	    case 12:	/* Format: RRRRGGGGBBBB */
		fmt = "%4x%4x%4x";
	    	f = 65535;
		break;
	    case 9:	/* Format: RRRGGGBBB */
		fmt = "%3x%3x%3x";
		f = 4095;
		break;
	    case 6:	/* Format: RRGGBB */
		fmt = "%2x%2x%2x";
		f = 255;
		break;
	    case 3:	/* Format: RGB */
		fmt = "%1x%1x%1x";
		f = 15;
		break;
	    }
	    /* get RGB values and store them in free table slot */
	    if (fmt && sscanf (name+1, fmt, &r, &g, &b) == 3) {
    		colors[NCOLOR].rgb[0] = r / f;
		colors[NCOLOR].rgb[1] = g / f;
		colors[NCOLOR].rgb[2] = b / f;
		return (NCOLOR);
	    }
    	}
	
	/* if color name not found, then return index of last color */
	return NCOLOR-1;
}

/* public functions */
void beginps(void)
/*****************************************************************************
Write PostScript prolog (including %%Pages comment)
*****************************************************************************/
{
	fprintf(stdout,"%%!PS-Adobe-2.0 EPSF-1.2\n");
	fprintf(stdout,"%%%%DocumentFonts:\n");
	if (BBoxSet) {
		fprintf(stdout,"%%%%BoundingBox: %d %d %d %d\n",
			BBox.llx,BBox.lly,BBox.urx,BBox.ury);
		BBoxOut = 1;
	} else {
		fprintf(stdout,"%%%%BoundingBox: (atend)\n");
	}
	fprintf(stdout,"%%%%Pages: (atend)\n");
	fprintf(stdout,"%%%%EndComments\n");
	fprintf(stdout,"%s\n",Prologue);
	fprintf(stdout,"%%%%EndProlog\n");
	fprintf(stdout,"GS\n");
}

void endps(void)
/*****************************************************************************
Write PostScript trailer (including %%Pages comment)
*****************************************************************************/
{
	fprintf(stdout,"GR\n");
	fprintf(stdout,"%%%%Trailer\n");
	if (!BBoxOut) {
		fprintf(stdout,"%%%%BoundingBox: %d %d %d %d\n",
			BBox.llx,BBox.lly,BBox.urx,BBox.ury);
		BBoxOut = 1;
	}
	fprintf(stdout,"%%%%Pages: %d\n",nPages);
}

void begineps(void)
/*****************************************************************************
Write encapsulated PostScript prolog (no %%Pages comment)
*****************************************************************************/
{
	fprintf(stdout,"%%!PS-Adobe-2.0 EPSF-1.2\n");
	fprintf(stdout,"%%%%DocumentFonts:\n");
	if (BBoxSet) {
		fprintf(stdout,"%%%%BoundingBox: %d %d %d %d\n",
			BBox.llx,BBox.lly,BBox.urx,BBox.ury);
		BBoxOut = 1;
	} else {
		fprintf(stdout,"%%%%BoundingBox: (atend)\n");
	}
	fprintf(stdout,"%%%%EndComments\n");
	fprintf(stdout,"%s\n",Prologue);
	fprintf(stdout,"%%%%EndProlog\n");
	fprintf(stdout,"GS\n");
}

void endeps(void)
/*****************************************************************************
Write encapsulated PostScript trailer (no %%Pages comment)
*****************************************************************************/
{
	fprintf(stdout,"GR\n");
	fprintf(stdout,"%%%%Trailer\n");
	if (!BBoxOut) {
		fprintf(stdout,"%%%%BoundingBox: %d %d %d %d\n",
			BBox.llx,BBox.lly,BBox.urx,BBox.ury);
		BBoxOut = 1;
	}
}

void boundingbox(int llx, int lly, int urx, int ury)
/*****************************************************************************
Set BoundingBox to llx lly urx ury
*****************************************************************************/
{
	BBox.llx = llx;
	BBox.lly = lly;
	BBox.urx = urx;
	BBox.ury = ury;
	BBoxSet = 1;
}

void newpage(const char *label, int ordinal)
{
	fprintf(stdout,"%%%%Page: %s %d\n",label,ordinal);
	nPages++;
}

void showpage(void)
{
	fprintf(stdout,"showpage\n"); 
}

void gsave(void)
{
	fprintf(stdout,"GS\n");
}

void grestore(void)
{
	fprintf(stdout,"GR\n");
}

void newpath(void)
{
	fprintf(stdout,"NP\n");
}

void closepath(void)
{
	fprintf(stdout,"CP\n");
}

void clip(void)
{
	fprintf(stdout,"clip\n");
}

void translate(float tx, float ty)
{
	fprintf(stdout,"%.7g %.7g TR\n",tx,ty);
}

void scale(float sx, float sy)
{
	fprintf(stdout,"%.4g %.4g SC\n",sx,sy);
}

void rotate(float angle)
{
	fprintf(stdout,"%.4g RO\n",angle);
}

void concat(float m[])
{
	fprintf(stdout,"[%.4g %.4g %.4g %.4g %.4g %.4g] CAT\n",
		m[0],m[1],m[2],m[3],m[4],m[5]);
}

void setgray(float gray)
{
	fprintf(stdout,"%.4g setgray\n",gray);
}

void setcymkcolor(float cyan, float magenta, float yellow, float black)
{
	fprintf(stdout,"%.4g %.4g %.4g %.4g setcymkcolor\n",cyan,magenta,yellow,black);
}

void setrgbcolor(float red, float green, float blue)
{
	fprintf(stdout,"%.4g %.4g %.4g setrgbcolor\n",red,green,blue);
}

void setcolor(const char *name)
{
	int i=colorindex(name);
	setrgbcolor(colors[i].rgb[0],colors[i].rgb[1],colors[i].rgb[2]);
}

void setlinewidth(float width)
{
	fprintf(stdout,"%.4g SLW\n",width);
}

void setlinejoin(int code)
{
	fprintf(stdout,"%d setlinejoin\n",code);
}

void setdash(float dash[], int ndash, float offset)
{
	int i;
	fprintf(stdout,"[ ");
	for (i=0; i<ndash; i++)
		fprintf(stdout,"%.4g ",dash[i]);
	fprintf(stdout,"] %.4g setdash\n",offset);
}

void moveto(float x, float y)
{
	fprintf(stdout,"%.4g %.4g M\n",x,y); 
}

void rmoveto(float x, float y)
{
	fprintf(stdout,"%.4g %.4g RM\n",x,y); 
}

void lineto(float x, float y)
{
	fprintf(stdout,"%.4g %.4g L\n",x,y); 
}

void rlineto(float x, float y)
{
	fprintf(stdout,"%.4g %.4g RL\n",x,y); 
}

void arc(float x, float y, float r, float ang1, float ang2)
{
	fprintf(stdout,"%.4g %.4g %.4g %.4g %.4g arc\n",x,y,r,ang1,ang2);
}

void stroke(void)
{
	fprintf(stdout,"S\n"); 
}

void fill(void)
{
	fprintf(stdout,"F\n");
}

void show(const char *str)
{
	fprintf(stdout,"(%s) SH\n",str);
}

void justshow(float just, const char *str)
/*****************************************************************************
Justify and show a string
******************************************************************************
Input:
just		justification factor
str		string
******************************************************************************
Notes:
The justification factor positions the string relative to the current point.
"just" may assume any value, but the common uses are:
	-1.0	right-justify the string
	-0.5	center the string on the current point
	 0.0	left-justify the string (like using "show")
*****************************************************************************/
{
	fprintf(stdout,"(%s) SW exch %.4g mul\n",str,just);
	fprintf(stdout,"exch %.4g mul RM (%s) SH\n",just,str);
}

void image(int w, int h, int bps, float m[], unsigned char *samples)
/*****************************************************************************
Write sampled gray-scale image
******************************************************************************
Input:
w		width of image (in samples)
h		height of image (in samples)
bps		number of bits per sample
m		array[6] containing image matrix
samples		array[w*h] of sample values
******************************************************************************
Notes:
Level 1 PostScript implementations support 1, 2, 4, and 8 bits per
sample.  Level 2 adds support for 12 bits per sample.
Samples are hex-encoded, and output lines are limited to 78 characters.
*****************************************************************************/
{
	int nline=39,i,rowbytes,nbytes;
	char line[80],*linei,*hexi;
	static int hexbuilt=0;
	static char hex[513];

	/* build table of hex codes if not already built */
	if (!hexbuilt) {
		for (i=0; i<256; i++)
			sprintf(&hex[2*i],"%02x",(unsigned int) i);
		hexbuilt = 1;
	}

	/* determine number of bytes per row and total number of bytes */
	rowbytes = 1+(w*bps-1)/8;
	nbytes = rowbytes*h;

	/* set up the image */
	fprintf(stdout,"/picstr %d string def\n",rowbytes);
	fprintf(stdout,"%d %d %d [%.4g %.4g %.4g %.4g %.4g %.4g]\n",
		w,h,bps,m[0],m[1],m[2],m[3],m[4],m[5]);
	fprintf(stdout,"{currentfile picstr readhexstring pop} image\n");

	/* encode and write the image in lines of 78 hex characters */
	while(nbytes) {
		if (nbytes<nline) nline = nbytes;
		for (i=0,linei=line; i<nline; i++) {
			hexi = hex+2*(*samples++);
			*linei++ = *hexi++;
			*linei++ = *hexi;
		}
		*linei++ = '\n';
		*linei = '\0';
		fputs(line,stdout);
		nbytes -= nline;
	}
}

void rgbimage(int w, int h, int bpc, float m[], unsigned char *samples)
/*****************************************************************************
Write sampled color (rgb) image
******************************************************************************
Input:
w		width of image (in samples)
h		height of image (in samples)
bpc		number of bits per component
m		array[6] containing image matrix
samples		array[3*w*h] of sample values
******************************************************************************
Notes:
In general, Level 1 PostScript implementations do not support rgbimage.
Level 2 supports 1, 2, 4, 8, and 12 bits per color component.  The
samples array should contain three color components (in R,G,B... order)
for each sample value.  Samples are hex-encoded, and output lines are
limited to 78 characters.
*****************************************************************************/
{
	int ncomp=3,nline=39,i,rowbytes,nbytes;
	char line[80],*linei,*hexi;
	static int hexbuilt=0;
	static char hex[513];

	/* build table of hex codes if not already built */
	if (!hexbuilt) {
		for (i=0; i<256; i++)
			sprintf(&hex[2*i],"%02x",(unsigned int) i);
		hexbuilt = 1;
	}

	/* determine number of bytes per row and total number of bytes */
	rowbytes = 1+(w*bpc*ncomp-1)/8;
	nbytes = rowbytes*h;

	/* set up the image */
	fprintf(stdout,"/picstr %d string def\n",rowbytes);
	fprintf(stdout,"%d %d %d [%.4g %.4g %.4g %.4g %.4g %.4g]\n",
		w,h,bpc,m[0],m[1],m[2],m[3],m[4],m[5]);
	fprintf(stdout,"{currentfile picstr readhexstring pop}\n");
	fprintf(stdout,"false %d colorimage\n",ncomp);

	/* encode and write the image in lines of 78 hex characters */
	while(nbytes) {
		if (nbytes<nline) nline = nbytes;
		for (i=0,linei=line; i<nline; i++) {
			hexi = hex+2*(*samples++);
			*linei++ = *hexi++;
			*linei++ = *hexi;
		}
		*linei++ = '\n';
		*linei = '\0';
		fputs(line,stdout);
		nbytes -= nline;
	}
}

void setfont(const char *fontname, float fontsize)
/*****************************************************************************
Execute findfont, scalefont, and setfont for specified font and size
*****************************************************************************/
{
	fprintf(stdout,"/%s findfont %.4g scalefont setfont\n",fontname,fontsize);
}

void fontbbox(const char *fontname, float fontsize, float bbox[])
/*****************************************************************************
Determine font bounding box for specified font and size
*****************************************************************************/
{
	int i = fontindex(fontname);
	int *b = FontMetrics[i].fontbbox;
	bbox[0] = b[0]*FONTSCALE*fontsize;
	bbox[1] = b[1]*FONTSCALE*fontsize;
	bbox[2] = b[2]*FONTSCALE*fontsize;
	bbox[3] = b[3]*FONTSCALE*fontsize;
}

float fontheight(const char *fontname, float fontsize)
/*****************************************************************************
Return maximum height for specified font and size
*****************************************************************************/
{
	int i;
	float h;
	i = fontindex(fontname);
	h = FontMetrics[i].fontbbox[3] - FontMetrics[i].fontbbox[1];
	h *= FONTSCALE*fontsize;
	return h;
}

float fontwidth(const char *fontname, float fontsize)
/*****************************************************************************
Return maximum width for specified font and size
*****************************************************************************/
{
	int i;
	float w;
	i = fontindex(fontname);
	w = FontMetrics[i].fontbbox[2] - FontMetrics[i].fontbbox[0];
	w *= FONTSCALE*fontsize;
	return w;
}

float fontcapheight(const char *fontname, float fontsize)
/*****************************************************************************
Return maximum capheight for specified font and size
*****************************************************************************/
{
	int i = fontindex(fontname);
	return FontMetrics[i].capheight*FONTSCALE*fontsize;
}

float fontxheight(const char *fontname, float fontsize)
/*****************************************************************************
Return maximum xheight for specified font and size
*****************************************************************************/
{
	int i = fontindex(fontname);
	return FontMetrics[i].xheight*FONTSCALE*fontsize;
}

float fontdescender(const char *fontname, float fontsize)
/*****************************************************************************
Return maximum descender for specified font and size
*****************************************************************************/
{
	int i = fontindex(fontname);
	return FontMetrics[i].descender*FONTSCALE*fontsize;
}

float fontascender(const char *fontname, float fontsize)
/*****************************************************************************
Return maximum ascender for specified font and size
*****************************************************************************/
{
	int i = fontindex(fontname);
	return FontMetrics[i].ascender*FONTSCALE*fontsize;
}

void polyline(const float *x, const float *y, int n)
/*****************************************************************************
Draw a segmented line
******************************************************************************
Input:
x		array[n] of x-coordinates
y		array[n] of y-coordinates
n		number of points
******************************************************************************
Notes:
The path is stroked every 200 points.
*****************************************************************************/
{
	int i;

	gsave();
	newpath();
	moveto(x[0],y[0]);
	for (i=1; i<n; i++) {
		lineto(x[i],y[i]);
		if (i%200==0) {
			stroke();
			moveto(x[i],y[i]);
		}
	}
	stroke();
	grestore();
}

void markto(float x, float y, int index, float size)
/*****************************************************************************
Draw a mark at specified location
******************************************************************************
Input:
x		x-coordinate of mark
y		y-coordinate of mark
index		type of mark to draw
size		size of mark
*****************************************************************************/
{
	gsave();
	translate(x,y);
	scale(size,size);
	fprintf(stdout,"CLW %0.4g div SLW\n",size);
	newpath();
	switch (index%NMARKS) {
	case MPLUS: /* plus */
		moveto(-0.5,0.0);
		rlineto(1.0,0.0);
		moveto(0.0,-0.5);
		rlineto(0.0,1.0);
		stroke();
		break;
	case MASTERISK: /* asterisk */
		moveto(-0.5,0.0);
		rlineto(1.0,0.0);
		moveto(-0.25,-0.433);
		rlineto(0.5,0.866);
		moveto(-0.25,0.433);
		rlineto(0.5,-0.866);
		stroke();
		break;
	case MCROSS: /* X */
		moveto(-0.5,-0.5);
		rlineto(1.0,1.0);
		moveto(-0.5,0.5);
		rlineto(1.0,-1.0);
		stroke();
		break;
	case MTRIANGLE: /* triangle */
		moveto(-0.5,-0.25);
		rlineto(1.0,0.0);
		rlineto(-0.5,0.809);
		closepath();
		stroke();
		break;
	case MSQUARE: /* square */
		moveto(-0.5,-0.5);
		rlineto(1.0,0.0);
		rlineto(0.0,1.0);
		rlineto(-1.0,0.0);
		closepath();
		stroke();
		break;
	case MCIRCLE: /* circle */
		arc(0.0,0.0,0.5,0.0,360.0);
		stroke();
		break;
	case MFILLEDTRIANGLE: /* filled triangle */
		moveto(-0.5,-0.25);
		rlineto(1.0,0.0);
		rlineto(-0.5,0.809);
		closepath();
		fill();
		break;
	case MFILLEDSQUARE: /* filled square */
		moveto(-0.5,-0.5);
		rlineto(1.0,0.0);
		rlineto(0.0,1.0);
		rlineto(-1.0,0.0);
		closepath();
		fill();
		break;
	case MFILLEDCIRCLE: /* filled circle */
		arc(0.0,0.0,0.5,0.0,360.0);
		fill();
		break;
	}
	grestore();
}

void rectclip(float x, float y, float width, float height)
/*****************************************************************************
Set a rectangular clipping path
******************************************************************************
Input:
x		x-coordinate of clipping path origin
y		y-coordinate of clipping path origin
width		width of clipping path
height		height of clipping path
*****************************************************************************/
{
	newpath();
	moveto(x,y);
	rlineto(width,0.0);
	rlineto(0.0,height);
	rlineto(-width,0.0);
	closepath();
	clip();
	newpath();
}

void rectfill(float x, float y, float width, float height)
/*****************************************************************************
Draw a filled rectangle
******************************************************************************
Input:
x		x-coordinate of rectangle origin
y		y-coordinate of rectangle origin
width		width of rectangle
height		height of rectangle
*****************************************************************************/
{
	gsave();
	newpath();
	moveto(x,y);
	rlineto(width,0.0);
	rlineto(0.0,height);
	rlineto(-width,0.0);
	closepath();
	fill();
	grestore();
}

void rectstroke(float x, float y, float width, float height)
/*****************************************************************************
Stroke a rectangle
******************************************************************************
Input:
x		x-coordinate of rectangle origin
y		y-coordinate of rectangle origin
width		width of rectangle
height		height of rectangle
*****************************************************************************/
{
	gsave();
	newpath();
	moveto(x,y);
	rlineto(width,0.0);
	rlineto(0.0,height);
	rlineto(-width,0.0);
	closepath();
	stroke();
	grestore();
}
