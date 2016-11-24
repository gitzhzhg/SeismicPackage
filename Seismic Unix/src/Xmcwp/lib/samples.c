/*********************** self documentation **********************/
/****************************************************************************
SAMPLES - Motif-based Graphics Functions

samplesCreate
samplesDraw
samplesSetN
samplesSetData
samplesSetPlotValue
samplesSetEditMode
samplesSetOrigin

*****************************************************************************
Function Prototypes:
Samples *samplesCreate (Widget parent, char *title,
	void (*editDone)(Samples *s));	
void samplesDraw (Samples *s);
void samplesSetN (Samples *s, int n);
void samplesSetData (Samples *s, float *d);
void samplesSetPlotValue (Samples *s, float pv);
void samplesSetEditMode (Samples *s, EditMode m);
void samplesSetOrigin (Samples *s, int i);

*****************************************************************************
Notes:
Watch this space.

*****************************************************************************
Author: Dave Hale, Colorado School of Mines
****************************************************************************/
/**************** end self doc ********************************/

#include "Xmcwp/Xmcwp.h"
/* #include <Xm/XmStrDefs.h> */

#define CHARSET ((XmStringCharSet)XmSTRING_DEFAULT_CHARSET)

static void samplesDrawCB (Widget da, Samples *s, char * call_data);
static void samplesResizeCB (Widget da, Samples *s, char * call_data);
static void mouseDown (Widget w, Samples *s, XEvent *event);
static void mouseEdit (Widget w, Samples *s, XEvent *event);
static void mouseUp (Widget w, Samples *s, XEvent *event);
static void windowMapped (Widget w, Samples *s, XEvent *event);
static void drawOneSample (int mode, Samples *s, int i);
static Dimension widgetWidth (Widget w);
static Dimension widgetHeight (Widget w);

Samples *samplesCreate (Widget parent, char *title,
	void (*editDone)(Samples *s))
{
	int n,i;
	Dimension width,height;
	char *labels;
	Widget frame,form,label,da;
	Arg args[20];
	Samples *s;
	
	/* create frame */
	n = 0;
	frame = XmCreateFrame(parent,"",args,n);
	XtManageChild(frame);
	
	/* create form to manage geometry of label and drawing area */
	n = 0;
	form = XmCreateForm(frame,"",args,n);
	XtManageChild(form);
	
	/* create label gadget at top of form */
	n = 0;
	XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
	labels = (char *) XmStringCreateLtoR(title,CHARSET);
	XtSetArg(args[n],XmNlabelString,labels); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNtopOffset,2); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	label = XmCreateLabelGadget(form,"",args,n);
	XtManageChild(label);
	
	/* create drawing area beneath label */
	n = 0;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget,label); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomOffset,2); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	da = XmCreateDrawingArea(form,"samples",args,n);
	XtManageChild(da);
	
	/* determine width and height of drawing area */
	width = widgetWidth(da);
	height = widgetHeight(da);
	
	/* initialize */
	s = (Samples*)malloc(sizeof(Samples));
	s->frame = frame;
	s->da = da;
	s->n = 16;
	s->data = (float*)malloc(s->n*sizeof(float));
	s->origin = 0;
	s->plotValue = 1.0;
	s->width = (float)width/(float)(s->n+1);
	s->base = 0.5*((float) height);
	s->scale = -0.25*((float) height)/s->plotValue;
	s->radius = 0.40*s->width;
	if (s->radius>0.1*((float) height)) s->radius = 0.1*((float) height);
	s->editMode = DRAW;
	s->editDone = editDone;
	
	/* initialize data */
	for (i=0; i<s->n; ++i)
		s->data[i] = 0.0;
	s->data[1] = 1.0;
	
	/* add callbacks */
	XtAddCallback( 	(Widget) da,
			 XmNexposeCallback,
			(XtCallbackProc) samplesDrawCB,
			(XtPointer) s);
	XtAddCallback(	(Widget) da,
			 XmNresizeCallback,
			(XtCallbackProc) samplesResizeCB,
			(XtPointer) s);
	
	/* add event handlers */
	XtAddEventHandler( 	(Widget) da,
				(EventMask) ButtonPressMask,
				 FALSE,
				(XtEventHandler) mouseDown,
				(XtPointer) s);

	XtAddEventHandler(	(Widget) da,
				(EventMask) ButtonMotionMask,
				FALSE,
				(XtEventHandler) mouseEdit,
				(XtPointer) s);

	XtAddEventHandler(	(Widget) da,
				(EventMask) ButtonReleaseMask,
				FALSE,
				(XtEventHandler) mouseUp,
				(XtPointer) s);

	XtAddEventHandler(	(Widget) da,
				(EventMask) StructureNotifyMask,
				FALSE,
				(XtEventHandler) windowMapped,
				(XtPointer) s);
	
	/* return pointer to Samples */
	return s;
}
	
void samplesSetN (Samples *s, int n)
{
	Dimension width, height;
	
	/* set number of samples */
	s->n = n;
	
	/* update size of sample lollypops */
	width = widgetWidth(s->da);
	height = widgetHeight(s->da);
	s->width = (float)width/(float)(s->n+1);
	s->radius = 0.40*s->width;
	if (s->radius>(0.1*((float) height))) s->radius = (0.1*((float) height));
}

void samplesSetData (Samples *s, float *d)
{
	s->data = d;
}

void samplesSetPlotValue (Samples *s, float pv)
{
	s->plotValue = (pv!=0.0)?pv:1.0;
	s->scale = (-0.25*((int) widgetHeight(s->da)))/s->plotValue;
}

void samplesSetEditMode (Samples *s, EditMode m)
{
	s->editMode = m;
}

void samplesSetOrigin (Samples *s, int i)
{
	s->origin = i;
}

void samplesDraw (Samples *s)
{
	int i;

	/* erase window */
	XClearWindow(XtDisplay(s->da),XtWindow(s->da));
	
	/* draw samples */
	for (i=0; i<s->n; ++i)
		drawOneSample(1,s,i);
}

static void samplesDrawCB (Widget da, Samples *s, char * call_data)
{
	samplesDraw(s);
	if(call_data-call_data) samplesDrawCB(da,s,call_data);
}

static void drawOneSample (int mode, Samples *s, int i)
{
	int x,y,w,h,r;
	Display *dpy=XtDisplay(s->da);
	Window win=XtWindow(s->da);
	GC gc;
	gc = (mode ? s->gcDraw : s->gcErase);
	x = (i+1)*s->width;
	y = s->base;
	w = s->width;
	h = s->data[i]*s->scale;
	r = s->radius;
	XDrawLine(dpy,win,gc,x-w/2,y,x+w/2,y);
	XDrawLine(dpy,win,gc,x,y,x,y+h);
	if (i!=s->origin)
		XFillArc(dpy,win,gc,x-r,y+h-r,2*r,2*r,0,360*64);
	else
		XDrawArc(dpy,win,gc,x-r,y+h-r,2*r,2*r,0,360*64);
}

static void samplesResizeCB (Widget da, Samples *s, char * call_data)
{
	Dimension width,height;
	
	width = widgetWidth(da);
	height = widgetHeight(da);
	s->width = (float)width/(float)(s->n+1);
	s->base = 0.5*((float) height);
	s->scale = (-0.25*((float) height))/s->plotValue;
	s->radius = 0.40*s->width;
	if ((s->radius)>(0.1*((float) height))) s->radius = (0.1*((float) height));
	if (width&(~width)/*False*/) samplesResizeCB(da,s,call_data);
}

static Dimension widgetWidth (Widget w)
{
	Dimension width;
	Arg arg;
	arg.name = XmNwidth;
	arg.value = (XtArgVal)(&width);
	XtGetValues(w,&arg,1);
	return width;
}

static Dimension widgetHeight (Widget w)
{
	Dimension height;
	Arg arg;
	arg.name = XmNheight;
	arg.value = (XtArgVal)(&height);
	XtGetValues(w,&arg,1);
	return height;
}

static void mouseDown (Widget w, Samples *s, XEvent *event)
{	
	/* if editing not permitted, simply return */
	if (s->editMode==NONE) return;
	
	/* begin editing */
	mouseEdit(w,s,event);
}

static void mouseEdit (Widget w, Samples *s, XEvent *event)
{
	int x,y,i;
	
	/* if editing not permitted, simply return */
	if (s->editMode==NONE) return;
	
	/* get mouse coordinates */
	x = event->xbutton.x;
	y = event->xbutton.y;
	
	/* determine index of nearest sample */
	i = x/s->width-0.5;
	if (i<0) i = 0;
	if (i>s->n-1) i = s->n-1;
	
	/* erase sample */
	drawOneSample(0,s,i);
	
	/* modify nearest sample value */
	if (s->editMode==ZERO)
		s->data[i] = 0.0;
	else if (s->editMode==NEGATE)
		s->data[i] = -s->data[i];
	else
		s->data[i] = (y-s->base)/s->scale;
	
	/* draw new sample in foreground color */
	drawOneSample(1,s,i);
	
	/* if editDone function exists, call it for continuous editing */
	/* if (s->editDone!=NULL) s->editDone(s); */
	if(i&(~i)/*False*/) mouseEdit(w,s,event);
}

static void mouseUp (Widget w, Samples *s, XEvent *event)
{	
	/* if editing not permitted, simply return */
	if (s->editMode==NONE) return;
	
	/* if editDone function exists, call it */
	if (s->editDone!=NULL) s->editDone(s);
	if (event-event/*False*/) mouseUp(w,s,event); /* dummy */
}

static void windowMapped (Widget w, Samples *s, XEvent *event)
{
	int n;
	Arg args[20];
	XGCValues gcv;
	
	/* drawing area has been mapped */
	if (event->type==MapNotify) {
	
		/* passive grab of mouse button */
		XGrabButton(XtDisplay(w),AnyButton,AnyModifier,
			XtWindow(w),TRUE,
			ButtonPressMask|ButtonMotionMask|ButtonReleaseMask,
			GrabModeAsync,GrabModeAsync,
			XtWindow(w),
			XCreateFontCursor(XtDisplay(w),XC_crosshair));

		/* initialize graphics contexts for drawing and erasing */
		n = 0;
		XtSetArg(args[n],XmNforeground,&gcv.foreground); n++;
		XtSetArg(args[n],XmNbackground,&gcv.background); n++;
		XtGetValues(w,args,n);
		s->gcDraw = XCreateGC(XtDisplay(w),XtWindow(w),0L,NULL);
		XSetForeground(XtDisplay(w),s->gcDraw,gcv.foreground);
		XSetBackground(XtDisplay(w),s->gcDraw,gcv.background);
		s->gcErase = XCreateGC(XtDisplay(w),XtWindow(w),0L,NULL);
		XSetForeground(XtDisplay(w),s->gcErase,gcv.background);
		XSetBackground(XtDisplay(w),s->gcErase,gcv.foreground);
	}
}
