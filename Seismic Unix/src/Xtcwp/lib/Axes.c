/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/******************************************************************************
AXES - the Axes Widget

XtcwpPointInAxesRectangle	returns TRUE if point is inside axes
				rectangle, otherwise FALSE
XtcwpSetAxesValues	set axes values
XtcwpSetAxesPads	set axes pads

******************************************************************************
Function Prototype:
Boolean XtcwpPointInAxesRectangle (Widget w, Position x, Position y);
void XtcwpSetAxesValues (Widget w, float x1beg, float x1end, float x2beg,
				float x2end);

void XtcwpSetAxesPads (Widget w, float p1beg, float p1end, float p2beg,
				float p2end);
******************************************************************************
XtcwpPointInAxesRectangle:
Input:
w		axes widget
x		x coordinate of point
y		y coordinate of point

******************************************************************************
XtcwpSetAxesValues:
Input:
w		axes widget
x1beg		axis value at beginning of axis 1
x1end		axis value at end of axis 1
x2beg		axis value at beginning of axis 2
x2end		axis value at end of axis 2

******************************************************************************
XtcwpSetAxesPads:
Input:
w		axes widget
p1beg		axis pad at beginning of axis 1
p1end		axis pad at end of axis 1
p2beg		axis pad at beginning of axis 2
p2end		axis pad at end of axis 2

******************************************************************************
Notes:
XtcwpPointInAxesRectangle:
This function is useful for determining whether or not input events
occured with the pointer inside the axes rectangle.  I.e., the input
callback function will typically call this function.

XtcwpSetAxesPads:
Pad values must be specified in the same units as the corresponding 
axes values.  These pads are useful when the contents of the axes box
require more space than implied by the axes values.  For example, the
first and last seismic wiggle traces plotted inside an axes box
will typically extend beyond the axes values corresponding to the
first and last traces.  However, all tics will lie within the limits
specified in the axes values (x1beg, x1end, x2beg, and x2end).

*******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 08/28/90
*******************************************************************************
Modified:  Craig Artley, Colorado School of Mines, 06/03/93, Rotate label for
	   vertical axis (Courtesy Dave Hale, Advance Geophysical).
******************************************************************************/
/**************** end self doc ********************************/



#include "Xtcwp/Xtcwp.h"
#include "Xtcwp/AxesP.h"

/* resources */
static XtResource resources[] = {
	{XtNgrid1,XtcwpCAxesGrid,XtcwpRAxesGrid,sizeof(int),
		XtOffset(XtcwpAxesWidget,axes.grid1), 
		XtRString,"none"},
	{XtNgrid2,XtcwpCAxesGrid,XtcwpRAxesGrid,sizeof(int),
		XtOffset(XtcwpAxesWidget,axes.grid2), 
		XtRString,"none"},
	{XtNnTic1,XtCParameter,XtRInt,sizeof(int),
		XtOffset(XtcwpAxesWidget,axes.n1tic), 
		XtRString,"1"},
	{XtNnTic2,XtCParameter,XtRInt,sizeof(int),
		XtOffset(XtcwpAxesWidget,axes.n2tic), 
		XtRString,"1"},
	{XtNlabel1,XtCString,XtRString,sizeof(String),
		XtOffset(XtcwpAxesWidget,axes.label1), 
		XtRString,""},
	{XtNlabel2,XtCString,XtRString,sizeof(String),
		XtOffset(XtcwpAxesWidget,axes.label2), 
		XtRString,""},
	{XtNtitle,XtCString,XtRString,sizeof(String),
		XtOffset(XtcwpAxesWidget,axes.title), 
		XtRString,""},
	{XtNstyle,XtcwpCAxesStyle,XtcwpRAxesStyle,sizeof(int),
		XtOffset(XtcwpAxesWidget,axes.style), 
		XtRString,"seismic"},
	{XtNaxesColor,XtCColor,XtRPixel,sizeof(Pixel),
		XtOffset(XtcwpAxesWidget,axes.axescolor), 
		XtRString,"black"},
	{XtNgridColor,XtCColor,XtRPixel,sizeof(Pixel),
		XtOffset(XtcwpAxesWidget,axes.gridcolor), 
		XtRString,"black"},
	{XtNtitleColor,XtCColor,XtRPixel,sizeof(Pixel),
		XtOffset(XtcwpAxesWidget,axes.titlecolor), 
		XtRString,"black"},
	{XtNlabelFont,XtCFont,XtRFont,sizeof(Font),
		XtOffset(XtcwpAxesWidget,axes.labelfont), 
		XtRString,"fixed"},
	{XtNtitleFont,XtCFont,XtRFont,sizeof(Font),
		XtOffset(XtcwpAxesWidget,axes.titlefont), 
		XtRString,"fixed"},
	{XtNresizeCallback,XtCCallback,XtRCallback,sizeof(char *),
		XtOffset(XtcwpAxesWidget,axes.resize), 
		XtRCallback,NULL},
	{XtNexposeCallback,XtCCallback,XtRCallback,sizeof(char *),
		XtOffset(XtcwpAxesWidget,axes.expose), 
		XtRCallback,NULL},
	{XtNinputCallback,XtCCallback,XtRCallback,sizeof(char *),
		XtOffset(XtcwpAxesWidget,axes.input), 
		XtRCallback,NULL},
};

/* functions defined and used internally */
static void ClassInitialize (void);
static void Initialize (XtcwpAxesWidget request, XtcwpAxesWidget new_widget);
static void Destroy (XtcwpAxesWidget w);
static void Resize (XtcwpAxesWidget w);
static void Redisplay (XtcwpAxesWidget w, XEvent *event, Region region);
static Boolean SetValues (XtcwpAxesWidget current, 
	XtcwpAxesWidget request, 
	XtcwpAxesWidget new_widget);
static void fillCallbackStruct (XtcwpAxesWidget w,
	int reason, XEvent *event, Region region, XtcwpAxesCallbackStruct *cb);
static void inputAxes (XtcwpAxesWidget w, XEvent *event, 
	char *args[], int nargs);
static void XtcwpStringToAxesGrid (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal);
static void XtcwpStringToAxesStyle (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal);

/* translations */
static char defaultTranslations[] = 
	"<Btn1Down>: input()\n"
	"<Btn1Up>: input()\n"
	"<Btn1Motion>: input()\n";

/* action procedures */
static XtActionsRec actionsList[] = {
	{"input",(XtActionProc)inputAxes},
};

/* class record */
XtcwpAxesClassRec  XtcwpaxesClassRec = {
	/* CoreClassPart */
	{
	(WidgetClass) &widgetClassRec,  /* superclass            */
	(String) "XtcwpAxes",           /* class_name            */
	(Cardinal) sizeof(XtcwpAxesRec),  /* widget_size           */
	(XtProc) ClassInitialize,       /* class_initialize      */
	(XtWidgetClassProc) NULL,       /* class_part_initialize */
	(XtEnum) FALSE,                 /* class_inited          */
	(XtInitProc) Initialize,        /* initialize            */
	(XtArgsProc) NULL,              /* initialize_hook       */
	(XtRealizeProc) XtInheritRealize,  /* realize               */
	(XtActionList) actionsList,        /* actions               */
	(Cardinal) XtNumber(actionsList),  /* num_actions           */
	(XtResourceList) resources,        /* resources             */
	(Cardinal) XtNumber(resources),  /* num_resources         */
	(XrmClass) NULLQUARK,           /* xrm_class             */
	(Boolean) TRUE,                 /* compress_motion       */
	(XtEnum) TRUE,                  /* compress_exposure     */
	(Boolean) TRUE,                 /* compress_enterleave   */
	(Boolean) TRUE,                 /* visible_interest      */
	(XtWidgetProc) Destroy,         /* destroy               */
	(XtWidgetProc) Resize,          /* resize                */
	(XtExposeProc) Redisplay,       /* expose                */
	(XtSetValuesFunc) SetValues,     /* set_values            */
	(XtArgsFunc) NULL,              /* set_values_hook       */
	(XtAlmostProc) XtInheritSetValuesAlmost,   /* set_values_almost     */
	(XtArgsProc) NULL,              /* get_values_hook       */
	(XtAcceptFocusProc) NULL,       /* accept_focus          */
	(XtVersionType) XtVersion,      /* version               */
	(XtPointer) NULL,               /* callback private      */
	(String) defaultTranslations,   /* tm_table              */
	(XtGeometryHandler) NULL,       /* query_geometry        */
	(XtStringProc) NULL,            /* display_accelerator   */
	(XtPointer) NULL,               /* extension             */
	},
	/* Axes class fields */
	{
	0,                              /* ignore                */
	}
};
WidgetClass xtcwpAxesWidgetClass = (WidgetClass) &XtcwpaxesClassRec;

/* class functions */
static void ClassInitialize (void)
{
	/* add type converters */
	XtAddConverter(XtRString,XtcwpRAxesGrid, (XtConverter) XtcwpStringToAxesGrid,NULL,0);
	XtAddConverter(XtRString,XtcwpRAxesStyle,(XtConverter) XtcwpStringToAxesStyle,NULL,0);
}
static void Initialize (XtcwpAxesWidget request, XtcwpAxesWidget new_widget)
{
	/* initialize axes values and pads */
	new_widget->axes.x1beg = 0.0;
	new_widget->axes.x1end = 1.0;
	new_widget->axes.x2beg = 0.0;
	new_widget->axes.x2end = 1.0;
	new_widget->axes.p1beg = 0.0;
	new_widget->axes.p1end = 0.0;
	new_widget->axes.p2beg = 0.0;
	new_widget->axes.p2end = 0.0;
	
	/* ensure window size is not zero */	
	if (request->core.width==0) new_widget->core.width = 200;
	if (request->core.height==0) new_widget->core.height = 200;

	/* set parameters that depend on window size */
	Resize(new_widget);
}
static void Destroy (XtcwpAxesWidget w)
{
	XtRemoveAllCallbacks((Widget) w,XtNresizeCallback);
	XtRemoveAllCallbacks((Widget) w,XtNexposeCallback);
	XtRemoveAllCallbacks((Widget) w,XtNinputCallback);
}
static void Resize (XtcwpAxesWidget w)
{
	XtcwpAxesCallbackStruct cb;
	XFontStruct *fa,*ft;
	XCharStruct cs;
	int labelch,labelcw,titlech,bl,bt,br,bb,ticsize,dummy;
	
	/* get fonts and determine character dimensions */
	fa = XQueryFont(XtDisplay(w),w->axes.labelfont);
	XTextExtents(fa,"2",1,&dummy,&dummy,&dummy,&cs);
	labelch = cs.ascent+cs.descent;
	labelcw = cs.width;
	ft = XQueryFont(XtDisplay(w),w->axes.titlefont);
	titlech = ft->max_bounds.ascent+ft->max_bounds.descent;

	/* determine axes rectangle position and dimensions */
	ticsize = labelcw;
	bl = labelch+7*labelcw;
	br = w->core.width-5*labelcw;
	while (br<bl) {
		br += labelcw;
		bl -= labelcw;
	}
	if (bl<0) bl = 0;
	if (br>((int) w->core.width)) br = w->core.width; 
	if (w->axes.style==XtcwpNORMAL) {
		bt = labelch+labelch/2+titlech;
		bb = w->core.height-3*ticsize/2-2*labelch;
	} else {
		bt = 3*ticsize/2+2*labelch;
		bb = w->core.height-labelch-labelch/2-titlech;
	}
	while (bb<bt) {
		bb += labelch;
		bt -= labelch;
	}
	if (bt<0) bt = 0;
	if (bb>((int) w->core.height)) bb = w->core.height;	
	w->axes.x = bl;
	w->axes.y = bt;
	w->axes.width = br-bl;
	w->axes.height = bb-bt;
		
	/* Free font info */
	XFreeFontInfo(NULL,fa,1);
	XFreeFontInfo(NULL,ft,1);
		
	/* call callbacks */
	fillCallbackStruct(w,XtcwpCR_RESIZE,NULL,NULL,&cb);
	XtCallCallbacks ((Widget) w,XtNresizeCallback,&cb);
} 
static void Redisplay (XtcwpAxesWidget w, XEvent *event, Region region)
{
	Display *dpy=XtDisplay(w);
	Window win=XtWindow(w);
	int x=w->axes.x;
	int y=w->axes.y;
	int width=w->axes.width;
	int height=w->axes.height;
	float x1beg=w->axes.x1beg;
	float x1end=w->axes.x1end;
	float x2beg=w->axes.x2beg;
	float x2end=w->axes.x2end;
	float p1beg=w->axes.p1beg;
	float p1end=w->axes.p1end;
	float p2beg=w->axes.p2beg;
	float p2end=w->axes.p2end;
	int n1tic=w->axes.n1tic;
	int n2tic=w->axes.n2tic;
	int grid1=w->axes.grid1;
	int grid2=w->axes.grid2;
	char *label1=w->axes.label1;
	char *label2=w->axes.label2;
	char *title=w->axes.title;
	Font labelfont=w->axes.labelfont;
	Font titlefont=w->axes.titlefont;
	Pixel axescolor=w->axes.axescolor;
	Pixel gridcolor=w->axes.gridcolor;
	Pixel titlecolor=w->axes.titlecolor;
	int style=w->axes.style;
	XGCValues values;
	GC gca,gcg,gct;
	XtcwpAxesCallbackStruct cb;
	XFontStruct *fa,*ft;
	XCharStruct cs;
	int labelca,labelcd,labelch,labelcw,titleca,titlech,
		ntic,xa,ya,tw,ticsize,ticb,numb,labelb,grided,grid,
		n1num,n2num,dummy;
	size_t lstr;
	float dnum,fnum,dtic,amin,amax,base,scale,anum,atic,azero;
	float d1num=0.0,f1num=0.0,d2num=0.0,f2num=0.0;
	char str[256],dash[2],*label;
	
	/* if not visible, then simply return */
	if (!w->core.visible) return;
		
	/* call callbacks before drawing axes (so grid will be on top) */
	fillCallbackStruct(w,XtcwpCR_EXPOSE,event,region,&cb);
	XtCallCallbacks ((Widget) w,XtNexposeCallback,&cb);
	
	/* create GCs */
	gca = XCreateGC(dpy,win,0,&values);
	gcg = XCreateGC(dpy,win,0,&values);
	gct = XCreateGC(dpy,win,0,&values);
	
	/* set colors */
	XSetForeground(dpy,gca,axescolor);
	XSetForeground(dpy,gcg,gridcolor);
	XSetForeground(dpy,gct,titlecolor);

	/* set fonts and determine character dimensions */
	XSetFont(dpy,gca,labelfont);
	XSetFont(dpy,gct,titlefont);
	fa = XQueryFont(dpy,labelfont);
	XTextExtents(fa,"2",1,&dummy,&dummy,&dummy,&cs);
	labelca = cs.ascent;
	labelcd = cs.descent;
	labelch = labelca+labelcd;
	labelcw = cs.width;
	ft = XQueryFont(dpy,titlefont);
	titleca = ft->max_bounds.ascent;
	titlech = ft->max_bounds.ascent+ft->max_bounds.descent;

	/* determine tic size */
	ticsize = labelcw;

	/* determine numbered tic intervals */
	n1num = (style==XtcwpNORMAL ? width : height)/(8*labelcw);
	scaxis(x1beg,x1end,&n1num,&d1num,&f1num);
	n2num = (style==XtcwpNORMAL ? height : width)/(8*labelcw);
	scaxis(x2beg,x2end,&n2num,&d2num,&f2num);

	/* draw horizontal axis */
	if (style==XtcwpNORMAL) {
		amin = (x1beg<x1end)?x1beg:x1end;
		amax = (x1beg>x1end)?x1beg:x1end;
		dnum = d1num;  fnum = f1num;  ntic = n1tic;
		scale = width/(x1end+p1end-x1beg-p1beg);
		base = x-scale*(x1beg+p1beg);
		ya = y+height;
		ticb = ticsize;
		numb = ticb+labelca+ticsize/4;
		labelb = numb+labelch+ticsize/4;
		grid = grid1;
		label = label1;
	} else {
		amin = (x2beg<x2end)?x2beg:x2end;
		amax = (x2beg>x2end)?x2beg:x2end;
		dnum = d2num;  fnum = f2num;  ntic = n2tic;
		scale = width/(x2end+p2end-x2beg-p2beg);
		base = x-scale*(x2beg+p2beg);
		ya = y;
		ticb = -ticsize;
		numb = ticb-labelcd-ticsize/4;
		labelb = numb-labelch-ticsize/4;
		grid = grid2;
		label = label2;
	}
	if (grid==XtcwpSOLID) {
		XSetLineAttributes(dpy,gcg,1L,LineSolid,CapButt,JoinMiter);
		grided = True;
	} else if (grid==XtcwpDASH) {
		grided = True;
		XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
		dash[0] = 8;  dash[1] = 4;
		XSetDashes(dpy,gcg,0,dash,2);
	} else if (grid==XtcwpDOT) {
		grided = True;
		XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
		dash[0] = 1;  dash[1] = 4;
		XSetDashes(dpy,gcg,0,dash,2);
	} else
		grided = False;
	azero = 0.0001*(amax-amin);
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if (anum<amin) continue;
		xa = base+scale*anum;
		if (grided) XDrawLine(dpy,win,gcg,xa,y,xa,y+height);
		XDrawLine(dpy,win,gca,xa,ya,xa,ya+ticb);
		if (anum>-azero && anum<azero)
			sprintf(str,"%1.5g",0.0);
		else
			sprintf(str,"%1.5g",anum);
		lstr = strlen(str);
		tw = XTextWidth(fa,str,(int)lstr);
		XDrawString(dpy,win,gca,xa-tw/2,ya+numb,str,(int)lstr);
	}
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		xa = base+scale*atic;
		XDrawLine(dpy,win,gca,xa,ya,xa,ya+ticb/2);
	}
	lstr = strlen(label);
	tw = XTextWidth(fa,label,(int)lstr);
	XDrawString(dpy,win,gca,x+width/2-tw/2,ya+labelb,label,(int)lstr);

	/* draw vertical axis */
	if (style==XtcwpNORMAL) {
		amin = (x2beg<x2end)?x2beg:x2end;
		amax = (x2beg>x2end)?x2beg:x2end;
		dnum = d2num;  fnum = f2num;  ntic = n2tic;
		scale = -height/(x2end+p2end-x2beg-p2beg);
		base = y+height-scale*(x2beg+p2beg);
		grid = grid2;
		label = label2;
	} else {
		amin = (x1beg<x1end)?x1beg:x1end;
		amax = (x1beg>x1end)?x1beg:x1end;
		dnum = d1num;  fnum = f1num;  ntic = n1tic;
		scale = height/(x1end+p1end-x1beg-p1beg);
		base = y-scale*(x1beg+p1beg);
		grid = grid1;
		label = label1;
	}
	xa = x;
	ticb = -ticsize;
	numb = ticb-ticsize/4;
	if (grid==XtcwpSOLID) {
		XSetLineAttributes(dpy,gcg,1L,LineSolid,CapButt,JoinMiter);
		grided = True;
	} else if (grid==XtcwpDASH) {
		grided = True;
		XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
		dash[0] = 8;  dash[1] = 4;
		XSetDashes(dpy,gcg,0,dash,2);
	} else if (grid==XtcwpDOT) {
		grided = True;
		XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
		dash[0] = 1;  dash[1] = 4;
		XSetDashes(dpy,gcg,0,dash,2);
	} else
		grided = False;
	azero = 0.0001*(amax-amin);
	for (anum=fnum; anum<=amax; anum+=dnum) {
		if (anum<amin) continue;
		ya = base+scale*anum;
		if (grided) XDrawLine(dpy,win,gcg,x,ya,x+width,ya);
		XDrawLine(dpy,win,gca,xa,ya,xa+ticb,ya);
		if (anum>-azero && anum<azero)
			sprintf(str,"%1.5g",0.0);
		else
			sprintf(str,"%1.5g",anum);
		lstr = strlen(str);
		tw = XTextWidth(fa,str,(int)lstr);
		XDrawString(dpy,win,gca,xa+numb-tw,ya+labelca/4,str,(int)lstr);
	}
	dtic = dnum/ntic;
	for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
		if (atic<amin) continue;
		ya = base+scale*atic;
		XDrawLine(dpy,win,gca,xa,ya,xa+ticb/2,ya);
	}
	lstr = strlen(label);
	tw = XTextWidth(fa,label,(int)lstr);
	xa = x+ticb-5*labelcw-ticsize/2;
	if (xa<labelch+1) xa = labelch+1;
	XtcwpDrawString90(dpy,win,gca,xa,y+height/2+tw/2,label,(int)lstr);

	/* draw title */
	lstr = strlen(title);
	tw = XTextWidth(ft,title,(int)lstr);
	if (style==XtcwpNORMAL) {
		XClearArea(dpy,win,0,y+labelca/4-labelch-labelch-titleca,
			w->core.width,titlech,False);
		XDrawString(dpy,win,gct,
			x+width/2-tw/2,
			y+labelca/4-labelch-labelch,title,(int)lstr);
	} else {
		XClearArea(dpy,win,0,y+height+labelca/4+labelch,
			w->core.width,titlech,False);
		XDrawString(dpy,win,gct,
			x+width/2-tw/2,
			y+height+labelca/4+labelch+titleca,title,(int)lstr);
	}

	/* draw axes box */
	XDrawRectangle(dpy,win,gca,x,y,width,height);
	
	/* free GCs and font info */
	XFreeGC(dpy,gca);
	XFreeGC(dpy,gcg);
	XFreeGC(dpy,gct);
	XFreeFontInfo(NULL,fa,1);
	XFreeFontInfo(NULL,ft,1);
}
static Boolean SetValues (XtcwpAxesWidget current, 
	XtcwpAxesWidget request, 
	XtcwpAxesWidget new_widget)
{
	Boolean redraw = FALSE;
	if(redraw) {
		XtcwpAxesWidget junk; /* keep compiler happy */
		junk = current; junk = request; junk = new_widget;
		junk = junk;
		}
	
	return redraw; 
} 

/* utilities */
static void fillCallbackStruct (XtcwpAxesWidget w,
	int reason, XEvent *event, Region region, XtcwpAxesCallbackStruct *cb)
{
	cb->reason = reason;
	cb->event = event;
	cb->region = region;
	cb->x = w->axes.x;
	cb->y = w->axes.y;
	cb->width = w->axes.width;
	cb->height = w->axes.height;
	cb->x1beg = w->axes.x1beg;
	cb->x1end = w->axes.x1end;
	cb->x2beg = w->axes.x2beg;
	cb->x2end = w->axes.x2end;
	cb->p1beg = w->axes.p1beg;
	cb->p1end = w->axes.p1end;
	cb->p2beg = w->axes.p2beg;
	cb->p2end = w->axes.p2end;
	cb->style = w->axes.style;
}
	
/* action procedures */
static void inputAxes (XtcwpAxesWidget w, XEvent *event, 
	char *args[], int nargs)
{
	XtcwpAxesCallbackStruct cb;
	args += 0*(nargs); /* keep compiler happy */
		
	/* call callback */
	fillCallbackStruct(w,XtcwpCR_INPUT,event,NULL,&cb);
	XtCallCallbacks ((Widget) w,XtNinputCallback,&cb);
} 

/* public functions */
Boolean XtcwpPointInAxesRectangle (Widget w, Position x, Position y)
{
	XtcwpAxesWidget aw=(XtcwpAxesWidget)w;
	Position xa=aw->axes.x,ya=aw->axes.y;
	Dimension wa=aw->axes.width,ha=aw->axes.height;
	
	return ((((int) x)>=((int) xa)) && (((int) x)<=((int) (xa+wa))) &&
		(((int) y)>= ((int) ya)) && (((int) y)<=((int) (ya+ha))));
	/* return (x>=xa && x<=xa+wa && y>=ya && y<=ya+ha); */
}
void XtcwpSetAxesValues (Widget w,
	float x1beg, float x1end, float x2beg, float x2end)
{
	XtcwpAxesWidget aw=(XtcwpAxesWidget)w;
	aw->axes.x1beg = x1beg;
	aw->axes.x1end = x1end;
	aw->axes.x2beg = x2beg;
	aw->axes.x2end = x2end;
}
void XtcwpSetAxesPads (Widget w,
	float p1beg, float p1end, float p2beg, float p2end)
{
	XtcwpAxesWidget aw=(XtcwpAxesWidget)w;
	aw->axes.p1beg = p1beg;
	aw->axes.p1end = p1end;
	aw->axes.p2beg = p2beg;
	aw->axes.p2end = p2end;
}

/* resource type converters */
static void XtcwpStringToAxesGrid (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal)
{
	static int result;
	char *string=fromVal->addr;
	
	args += 0*(*nargs); /* keep compiler happy */
	/* convert axes grid string in fromVal to int in toVal */
	if (strcmp(string,"none")==0)
		result = XtcwpNONE;
	else if (strcmp(string,"dot")==0)
		result = XtcwpDOT;
	else if (strcmp(string,"dash")==0)
		result = XtcwpDASH;
	else if (strcmp(string,"solid")==0)
		result = XtcwpSOLID;
	else {
		result = XtcwpNONE;
		XtWarning("Invalid AxesGrid specification!");
	}
	toVal->size = sizeof(int);
	toVal->addr = (char *)&result;
}
static void XtcwpStringToAxesStyle (XrmValue *args, int *nargs, 
	XrmValue *fromVal, XrmValue *toVal)
{
	static int result;
	char *string=fromVal->addr;
	args += 0*(*nargs); /* keep compiler happy */
	
	/* convert axes style string in fromVal to int in toVal */
	if (strcmp(string,"normal")==0)
		result = XtcwpNORMAL;
	else if (strcmp(string,"seismic")==0)
		result = XtcwpSEISMIC;
	else {
		result = XtcwpNORMAL;
		XtWarning("Invalid AxesStyle specification!");
	}
	toVal->size = sizeof(int);
	toVal->addr = (char *)&result;
}

