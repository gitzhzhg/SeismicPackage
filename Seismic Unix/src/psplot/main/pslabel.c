/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PSLABEL: $Revision: 1.7 $ ; $Date: 2011/11/17 00:10:53 $	*/

#include "par.h"
#include "psplot.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
" 									",
" PSLABEL - output PostScript file consisting of a single TEXT string	",
"          on a specified background. (Use with psmerge to label plots.)",
" 									",
" pslabel t= [t=] [optional parameters] > epsfile			",
" 									",
"Required Parameters (can have multiple specifications to mix fonts):	",
"  t=                 text string to write to output			",
"									",
"Optional Parameters:							",
"  f=Times-Bold       font for text string				",
"                      (multiple specifications for each t)		",
"  size=30            size of characters in points (72 points/inch)	",
"  tcolor=black       color of text string				",
"  bcolor=white       color of background box				",
"  nsub=0             number of characters to subtract when		",
"                     computing size of background box (not all		",
"                     characters are the same size so the		",
"                     background box may be too big at times.)		",
"									",
" Example:								",
" pslabel t=\"(a) \" f=Times-Bold t=\"h\" f=Symbol t=\"=0.04\" nsub=3 > epsfile",
"									",
" This example yields the PostScript equivalent of the string		",
"  (written here in LaTeX notation) $ (a)\\; \\eta=0.04 $		",
"									",
" Notes:								",
" This program produces a (color if desired) PostScript text string that",
" can be positioned and pasted on a PostScript plot using   psmerge 	",
"     see selfdoc of   psmerge for further information.			",
"									",
" Possible fonts:   Helvetica, Helvetica-Oblique, Helvetica-Bold,	",
"  Helvetica-BoldOblique,Times-Roman,Times-Italic,Times-Bold,		",
"  Times-BoldItalic,Courier,Courier-Bold,Courier-Oblique,		",
"  Courier-BoldOblique,Symbol						",
"									",
" Possible colors:  greenyellow,yellow,goldenrod,dandelion,apricot,	",
"  peach,melon,yelloworange,orange,burntorange,bittersweet,redorange,	",
"  mahogany,maroon,brickred,red,orangered,rubinered,wildstrawberry,	",
"  salmon,carnationpink,magenta,violetred,rhodamine,mulberry,redviolet,	",
"  fuchsia,lavender,thistle,orchid,darkorchid,purple,plum,violet,royalpurple,",
"  blueviolet,periwinkle,cadetblue,cornflowerblue,midnightblue,naveblue,",
"  royalblue,blue,cerulean,cyan,processblue,skyblue,turquoise,tealblue,	",
"  aquamarine,bluegreen,emerald,junglegreen,seagreen,green,forestgreen,	",
"  pinegreen,limegreen,yellowgreen,springgreen,olivegreen,rawsienna,sepia,",
"  brown,tan,white,black,gray						",
"									",
" All color specifications may also be made in X Window style Hex format",
" example:   tcolor=#255						",
"									",
" Legal font names are:							",
" AvantGarde-Book AvantGarde-BookOblique AvantGarde-Demi AvantGarde-DemiOblique"
" Bookman-Demi Bookman-DemiItalic Bookman-Light Bookman-LightItalic ",
" Courier Courier-Bold Courier-BoldOblique Courier-Oblique ",
" Helvetica Helvetica-Bold Helvetica-BoldOblique Helvetica-Oblique ",
" Helvetica-Narrow Helvetica-Narrow-Bold Helvetica-Narrow-BoldOblique ",
" Helvetica-Narrow-Oblique NewCentrySchlbk-Bold"
" NewCenturySchlbk-BoldItalic NewCenturySchlbk-Roman Palatino-Bold  ",
" Palatino-BoldItalic Palatino-Italics Palatino-Roman ",
" SanSerif-Bold SanSerif-BoldItalic SanSerif-Roman ",
" Symbol Times-Bold Times-BoldItalic ",
" Times-Roman Times-Italic ZapfChancery-MediumItalic ",
"									",
NULL};

/*
 *
 * AUTHOR:  John E. Anderson, Visiting Scientist from Mobil, 1994
 */
/************************** end self doc ********************************/

int main (int argc, char **argv)
{
	char **text,**font,*textcolor,*boxcolor;
	float size,labelCD=0.0,labelCA,labelCW=0.0,labelCH,bigx,bigy,eps,eps2;
	float *x;
	int n,j,nsub;
	size_t nchar;

	/* Hook up getpars */
	initargs(argc,argv);
	requestdoc(0);

	/* Get parameters */
	if(!getparint("nsub",&nsub))nsub=0;
	if(!getparfloat("size",&size))size=30;
	if(!getparstring("tcolor",&textcolor))textcolor="black";
	if(!getparstring("bcolor",&boxcolor))boxcolor="white";

        checkpars();

	eps=0.25*size;
	eps2=0.1*size;
	n=countparname("t");
	if(n==0)
 	err("must enter at least one PSTEXT text stream as parameter t");

	if(n!=countparname("f")) 
		warn("suggest specify same number of values for t and f");
	text =(char **)malloc( (n+1)*sizeof(char *) );
	font =(char **)malloc( (n+1)*sizeof(char *) );
	x = (float *)malloc( (n+1)*sizeof(float) );	
	for(bigx=eps,bigy=0.,j=0;j<n;j++){
		x[j]=bigx;
		if(!getnparstring(j+1,"t",&text[j]))text[j]="hello";
		if(!getnparstring(j+1,"f",&font[j]))font[j]="Times-Bold";
		labelCH = fontheight(font[j],size);
		labelCW = fontwidth(font[j],size);
		labelCA = fontascender(font[j],size);
		labelCD = MIN(labelCD,fontdescender(font[j],size));
		nchar = strlen(text[j]);
		bigx+=0.5*(((double) nchar)*labelCW);
		bigy=MAX(bigy,labelCH+eps+0.0*labelCA);
	}
	bigx+=eps;
	bigx-=0.5*nsub*labelCW;

	/* open output eps file */
	boundingbox(-eps2,-eps2,bigx+eps2,bigy+eps2);
	begineps();
	gsave();
	rectclip(0.,0.,bigx,bigy);

	/* fill background box with background color */
	newpath();
	moveto(0.,0.);
	lineto(bigx,0.);
	lineto(bigx,bigy);
	lineto(0.,bigy);
	closepath();
	setcolor(boxcolor);
	fill();
	
	/* write out text strings */
	setcolor(textcolor);
	moveto(eps,eps-labelCD);
	for(j=0;j<n;j++) {
		setfont(font[j],size);
		show(text[j]);
	}

	/* close output stream */
	grestore();
	showpage();
	endeps();

	return EXIT_SUCCESS;
}
