/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* XRECTS: $Revision: 1.11 $ ; $Date: 2011/11/30 21:15:49 $	*/


#include "par.h"
#include "Xtcwp/Xtcwp.h"
#include "Xtcwp/Axes.h"
#include <X11/Shell.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" XRECTS - plot rectangles on a two-dimensional grid			",
" 									",
" xrects x1min= x1max= x2min= x2max= [optional parameters] <rectangles 	",
" 									",
" Required Parameters:							",
" x1min                  minimum x1 coordinate				",
" x1max                  maximum x1 coordinate				",
" x2min                  minimum x2 coordinate				",
" x2max                  maximum x2 coordinate				",
" 									",
" Optional Parameters:							",
" color=red              color used for rectangules			",
" 									",
" Optional resource parameters (defaults taken from resource database):	",
" width=                 width in pixels of window			",
" height=                height in pixels of window			",
" nTic1=                 number of tics per numbered tic on axis 1	",
" grid1=                 grid lines on axis 1 - none, dot, dash, or solid",
" label1=                label on axis 1				",
" nTic2=                 number of tics per numbered tic on axis 2	",
" grid2=                 grid lines on axis 2 - none, dot, dash, or solid",
" label2=                label on axis 2				",
" labelFont=             font name for axes labels			",
" title=                 title of plot					",
" titleFont=             font name for title				",
" titleColor=            color for title				",
" axesColor=             color for axes					",
" gridColor=             color for grid lines				",
" style=                 normal (axis 1 horizontal, axis 2 vertical) or	",
"                        seismic (axis 1 vertical, axis 2 horizontal)	",
" 									",
NULL};
/**************** end self doc ********************************/


/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 06/21/91
 */


/* client data structures for callbacks */
typedef struct ClientDataStruct {
	float x1min;
	float x1max;
	float x2min;
	float x2max;
	char *color;
	struct MRectListStruct *mrectlist;
	FGC fgc;
	int exposed;
	Widget axes;
} ClientData;

/* monitor rectangle */
typedef struct MRectStruct {
	float x1a,x2a,x1b,x2b;
	struct MRectStruct *next;
} MRect;
typedef struct MRectListStruct {
	struct MRectStruct *head;
	struct MRectStruct *tail;
} MRectList;

/* work procedures */
Boolean readRect (ClientData *cd);

/* callback functions */
void resizeCB (Widget w, 
	ClientData *clientdata,
	XtcwpAxesCallbackStruct *calldata);
void exposeCB (Widget w, 
	ClientData *clientdata,
	XtcwpAxesCallbackStruct *calldata);
void inputCB (Widget w, 
	ClientData *clientdata,
	XtcwpAxesCallbackStruct *calldata);

/* functions defined and used internally */

int
main (int argc, char **argv)
{
	int nTic1,nTic2,width,height;
	float x1min,x1max,x2min,x2max;
	char *label1="",*label2="",*title="",
		*labelFont="",*titleFont="",
		*axesColor="",*gridColor="",*titleColor="",
		*style="normal",*grid1="none",*grid2="none",
		*color;
	ClientData cd;
	XrmValue from,to;
	Widget toplevel,axes;
	XtAppContext ac;
	Arg args[100];
	int nargs;

	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters */
	if (!getparfloat("x1min",&x1min)) err("must specify x1min!\n");
	if (!getparfloat("x1max",&x1max)) err("must specify x1max!\n");
	if (!getparfloat("x2min",&x2min)) err("must specify x2min!\n");
	if (!getparfloat("x2max",&x2max)) err("must specify x2max!\n");
	if (!getparstring("color",&color)) color = "red";

	/* initialize toolkit and set toplevel parameters */
 	toplevel = XtAppInitialize(&ac,"XRects_XAPP_DEF",NULL,0,&argc,argv,NULL,NULL,0);
	nargs = 0;
	if (getparint("width",&width))
		{XtSetArg(args[nargs],XtNwidth,width); nargs++;}
	if (getparint("height",&height))
		{XtSetArg(args[nargs],XtNheight,height); nargs++;}
	XtSetValues(toplevel,args,nargs);

	/* create axes and set axes parameters */
	axes = XtCreateManagedWidget("axes",xtcwpAxesWidgetClass, 
		toplevel,NULL,0);
	nargs = 0;
	if (getparstring("grid1",&grid1)) {
		from.addr = (char *)grid1;
		XtConvertAndStore(axes,XtRString,&from,XtcwpRAxesGrid,&to);
		if (to.addr) XtSetArg(args[nargs],XtNgrid1,*((int*)to.addr));
		nargs++;
	}
	if (getparstring("grid2",&grid2)) {
		from.addr = (char *)grid2;
		XtConvertAndStore(axes,XtRString,&from,XtcwpRAxesGrid,&to);
		if (to.addr) XtSetArg(args[nargs],XtNgrid2,*((int*)to.addr));
		nargs++;
	}
	if (getparint("nTic1",&nTic1))
		{XtSetArg(args[nargs],XtNnTic1,nTic1); nargs++;}
	if (getparint("nTic2",&nTic2))
		{XtSetArg(args[nargs],XtNnTic2,nTic2); nargs++;}
	if (getparstring("label1",&label1))
		{XtSetArg(args[nargs],XtNlabel1,label1); nargs++;}
	if (getparstring("label2",&label2))
		{XtSetArg(args[nargs],XtNlabel2,label2); nargs++;}
	if (getparstring("title",&title))
		{XtSetArg(args[nargs],XtNtitle,title); nargs++;}
	if (getparstring("style",&style)) {
		from.size = (unsigned int) strlen(style);  from.addr = (char *)style;
		XtConvertAndStore(axes,XtRString,&from,XtcwpRAxesStyle,&to);
		if (to.addr) XtSetArg(args[nargs],XtNstyle,*((int*)to.addr));
		nargs++;
	}
	if (getparstring("axesColor",&axesColor)) {
		from.addr = (char *)axesColor;
		XtConvertAndStore(axes,XtRString,&from,XtRPixel,&to);
		if (to.addr) XtSetArg(args[nargs],XtNaxesColor,
			*((unsigned long*)to.addr));
		nargs++;
	}
	if (getparstring("gridColor",&gridColor)) {
		from.addr = (char *)gridColor;
		XtConvertAndStore(axes,XtRString,&from,XtRPixel,&to);
		if (to.addr) XtSetArg(args[nargs],XtNgridColor,
			*((unsigned long*)to.addr));
		nargs++;
	}
	if (getparstring("titleColor",&titleColor)) {
		from.addr = (char *)titleColor;
		XtConvertAndStore(axes,XtRString,&from,XtRPixel,&to);
		if (to.addr) XtSetArg(args[nargs],XtNtitleColor,
			*((unsigned long*)to.addr));
		nargs++;
	}
	if (getparstring("labelFont",&labelFont)) {
		from.addr = (char *)labelFont;
		XtConvertAndStore(axes,XtRString,&from,XtRFont,&to);
		if (to.addr) XtSetArg(args[nargs],XtNlabelFont,
			*((Font*)to.addr));
		nargs++;
	}
	if (getparstring("titleFont",&titleFont)) {
		from.addr = (char *)titleFont;
		XtConvertAndStore(axes,XtRString,&from,XtRFont,&to);
		if (to.addr) XtSetArg(args[nargs],XtNtitleFont,
			*((Font*)to.addr));
		nargs++;
	}
        checkpars();

	XtSetValues(axes,args,nargs);
	XtcwpSetAxesValues(axes,x1min,x1max,x2min,x2max);

	/* initialize client data */
	cd.x1min = x1min;
	cd.x1max = x1max;
	cd.x2min = x2min;
	cd.x2max = x2max;
	cd.color = color;
	cd.mrectlist = malloc(sizeof(MRectList));
	cd.mrectlist->head = cd.mrectlist->tail = NULL;
	cd.exposed = 0;
	cd.axes = axes;
	
	/* add work procedure */
	XtAppAddWorkProc(ac,(XtWorkProc) readRect,&cd);

	/* add callbacks to axes widget */
	XtAddCallback(axes,XtNresizeCallback,(XtCallbackProc) resizeCB,&cd);
	XtAddCallback(axes,XtNexposeCallback,(XtCallbackProc) exposeCB,&cd);
	XtAddCallback(axes,XtNinputCallback,(XtCallbackProc) inputCB,&cd);

	/* realize everything */
	XtRealizeWidget(toplevel);
	
	/* go */
	XtAppMainLoop(ac);

	return EXIT_SUCCESS;
}

Boolean readRect (ClientData *cd)
/*****************************************************************************
An XtWorkProc that reads a rectangle when there is nothing else to do.
If a new rectangle exists (assumed true on the first call), forces
an expose event by clearing one pixel (kludge!!!).
If the current frame has been exposed, attempts to read a new 
frame of floats.
*****************************************************************************/
{
	float x1a,x1b,x2a,x2b,temp[4];
	static int newrect=1;
	MRect *mrect;
	MRectList *mrectlist=cd->mrectlist;
	
	/* if a new rectangle exists */
	if (newrect) {

		/* clear one pixel to force expose event (kludge!!!) */
		XClearArea(XtDisplay(cd->axes),XtWindow(cd->axes),
			0,0,1,1,True);
	}

	/* if current rectangle exposed, try to read a new rectangle */
	if (cd->exposed) {

		/* if new rectangle can be read */
		/*
		if (fscanf(stdin,"%f %f %f %f",&x1a,&x2a,&x1b,&x2b)!=EOF) {
		*/
		if (fread(temp,sizeof(float),4,stdin)==4) {
			x1a = temp[0];
			x2a = temp[1];
			x1b = temp[2];
			x2b = temp[3];
			
			/* note that a new rectangle was read */
			newrect = 1;

			/* add rectangle to list */
			mrect = malloc(sizeof(*mrect));
			mrect->x1a = x1a;
			mrect->x2a = x2a;
			mrect->x1b = x1b;
			mrect->x2b = x2b;
			mrect->next = NULL;
			if (mrectlist->head==NULL)
				mrectlist->head = mrect;
			if (mrectlist->tail!=NULL) 
				mrectlist->tail->next = mrect;
			mrectlist->tail = mrect;
	
		/* else, remove this work procedure */
		} else {
			return True;
		}

		/* note that the new rectangle has not been exposed */
		cd->exposed = 0;
	
	/* else, do not read new rectangle until previous rectangle exposed */
	} else {
		newrect = 0;
	}
	
	/* ensure that we will be called again */
	return False;
}

void exposeCB (Widget w, 
	ClientData *cd,
	XtcwpAxesCallbackStruct *ca)
/*****************************************************************************
Expose event callback - draws rectangles inside axes box
*****************************************************************************/
{
	float x1min=cd->x1min,x1max=cd->x1max,x2min=cd->x2min,x2max=cd->x2max;
	char *color=cd->color;
	MRectList *mrectlist=cd->mrectlist;
	FGC fgc=cd->fgc;
	Position x=ca->x,y=ca->y;
	Dimension width=ca->width,height=ca->height;
	float fx,fy,fwidth,fheight;
	Display *dpy=XtDisplay(w);
	Window win=XtWindow(w);
	GC gc;
	XWindowAttributes wa;
	XColor scolor,ecolor;
	Pixel black=BlackPixelOfScreen(XtScreen(w));
	MRect *mrect;
	static int firstexpose=1;

	/* if first expose, create FGC */
	if (firstexpose) {
		gc = XCreateGC(dpy,win,0L,NULL);
		if (ca->style==XtcwpNORMAL)
			fgc = FXCreateFGC(gc,x,y,width,height,
				x1min,x2max,x1max-x1min,x2min-x2max);
		else
			fgc = FXCreateFGC(gc,x,y,width,height,
				x2min,x1min,x2max-x2min,x1max-x1min);
		FSetClipRectangle(fgc,x1min,x2min,x1max,x2max);
		XGetWindowAttributes(dpy,win,&wa);
		if (XAllocNamedColor(dpy,wa.colormap,color,&scolor,&ecolor))
			XSetForeground(dpy,gc,ecolor.pixel);
		else
			XSetForeground(dpy,gc,black);
		cd->fgc = fgc;
		firstexpose = 0;
	}

	/* update coordinate mapping (in case window was resized) */
	if (ca->style==XtcwpNORMAL)
		FSetMap(fgc,x,y,width,height,
			x1min,x2max,x1max-x1min,x2min-x2max);
	else
		FSetMap(fgc,x,y,width,height,
			x2min,x1min,x2max-x2min,x1max-x1min);

	/* loop over rectangles in list */
	for (mrect=mrectlist->head; mrect!=NULL; mrect=mrect->next) {
		if (ca->style==XtcwpNORMAL) {
			fx = MIN(mrect->x1a,mrect->x1b);
			fy = MAX(mrect->x2a,mrect->x2b);
			fwidth = ABS(mrect->x1b-mrect->x1a);
			fheight = -ABS(mrect->x2b-mrect->x2a);
			FXFillRectangle(dpy,win,fgc,fx,fy,fwidth,fheight);
		} else {
			fx = MIN(mrect->x2a,mrect->x2b);
			fy = MAX(mrect->x1a,mrect->x1b);
			fwidth = ABS(mrect->x2b-mrect->x2a);
			fheight = ABS(mrect->x1b-mrect->x1a);
			FXFillRectangle(dpy,win,fgc,fx,fy,fwidth,fheight);
		}
	}
	
	/* update client data */
	cd->exposed = 1;
}

void inputCB (Widget w, 
	ClientData *cd,
	XtcwpAxesCallbackStruct *ca)
/*****************************************************************************
Input event callback - currently does nothing.
*****************************************************************************/
{
	if(cd-cd) inputCB(w,cd,ca); /* dummy */
}

void resizeCB (Widget w, 
	ClientData *cd,
	XtcwpAxesCallbackStruct *ca)
/*****************************************************************************
Resize event callback - clears window to force an expose event.
*****************************************************************************/
{
	/* clear window and force an expose event */
	if (cd->exposed) XClearArea(XtDisplay(w),XtWindow(w),(int)(cd-cd),(int)(ca-ca),0,0,True);
}
