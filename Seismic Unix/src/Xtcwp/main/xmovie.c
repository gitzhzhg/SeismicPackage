/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.		       */

/* XMOVIE: $Revision: 1.26 $ ; $Date: 2011/11/30 21:34:48 $	*/

#include "par.h"
#include "Xtcwp/Xtcwp.h"
#include "Xtcwp/Axes.h"
#include <X11/keysym.h>
#include <X11/Shell.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" XMOVIE - image one or more frames of a uniformly sampled function f(x1,x2)",
" 									",
" xmovie n1= n2= [optional parameters] <fileoffloats			",
" 									",
" X Functionality:							",
" Button 1	Zoom with rubberband box				",
" Button 2 	reverse the direction of the movie.			",
" Button 3 	stop and start the movie.				",
" q or Q key	Quit 							",
" s or S key	stop display and switch to Step mode		    ",
" b or B key	set frame direction to Backward			 ",
" f or F key	set frame direction to Forward			  ",
" n or N key	same as 'f'					     ",
" c or C key	set display mode to Continuous mode		     ",
" 									",
" Required Parameters:							",
" n1=		    number of samples in 1st (fast) dimension	",
" n2=		    number of samples in 2nd (slow) dimension	",
" 									",
" Optional Parameters:							",
" d1=1.0		 sampling interval in 1st dimension		",
" f1=0.0		 first sample in 1st dimension			",
" d2=1.0		 sampling interval in 2nd dimension		",
" f2=0.0		 first sample in 2nd dimension			",
" perc=100.0	     percentile used to determine clip		",
" clip=(perc percentile) clip used to determine bclip and wclip		",
" bperc=perc	     percentile for determining black clip value	",
" wperc=100.0-perc       percentile for determining white clip value	",
" bclip=clip	     data values outside of [bclip,wclip] are clipped",
" wclip=-clip	    data values outside of [bclip,wclip] are clipped",
" x1beg=x1min	    value at which axis 1 begins			",
" x1end=x1max	    value at which axis 1 ends			",
" x2beg=x2min	    value at which axis 2 begins			",
" x2end=x2max	    value at which axis 2 ends			",
" fframe=1	       value corresponding to first frame		",
" dframe=1	       frame sampling interval			",
" loop=0		 =1 to loop over frames after last frame is input",
"			=2 to run movie back and forth		 ",
" interp=1	       =0 for a non-interpolated, blocky image	",
" verbose=1	      =1 for info printed on stderr (0 for no info)	",
" idm=0		  =1 to set initial display mode to stepmode",
" 									",
" Optional resource parameters (defaults taken from resource database):	",
" windowtitle=      	 title on window and icon			",
" width=		 width in pixels of window			",
" height=		height in pixels of window			",
" nTic1=		 number of tics per numbered tic on axis 1	",
" grid1=		 grid lines on axis 1 - none, dot, dash, or solid",
" label1=		label on axis 1				",
" nTic2=		 number of tics per numbered tic on axis 2	",
" grid2=		 grid lines on axis 2 - none, dot, dash, or solid",
" label2=		label on axis 2				",
" labelFont=	     font name for axes labels			",
" title=		 title of plot					",
" titleFont=	     font name for title				",
" titleColor=	    color for title				",
" axesColor=	     color for axes					",
" gridColor=	     color for grid lines				",
" style=		 normal (axis 1 horizontal, axis 2 vertical) or	",
"			seismic (axis 1 vertical, axis 2 horizontal)	",
" sleep=		 delay between frames in seconds (integer)	",
" 									",
" Color options:							",
" cmap=gray     gray, hue, saturation, or default colormaps may be specified",
" bhue=0	hue mapped to bclip (hue and saturation maps)		",
" whue=240      hue mapped to wclip (hue and saturation maps)		",
" sat=1	 saturation (hue map only)				",
" bright=1      brightness (hue and saturation maps)			",
" white=(bclip+wclip)/2  data value mapped to white (saturation map only)",
"									",
" Notes:								",
" Colors are specified using the HSV color wheel model:			",
"   Hue:  0=360=red, 60=yellow, 120=green, 180=cyan, 240=blue, 300=magenta",
"   Saturation:  0=white, 1=pure color					",
"   Value (brightness):  0=black, 1=maximum intensity			",
" For the saturation mapping (cmap=sat), data values between white and bclip",
"   are mapped to bhue, with saturation varying from white to the pure color.",
"   Values between wclip and white are similarly mapped to whue.	",
" For the hue mapping (cmap=hue), data values between wclip and bclip are",
"   mapped to hues between whue and bhue.  Intermediate hues are found by",
"   moving counterclockwise around the circle from bhue to whue.  To reverse",
"   the polarity of the image, exchange bhue and whue.  Equivalently,	",
"   exchange bclip and wclip (setting perc=0 is an easy way to do this).",
"   Hues in excess of 360 degrees can be specified in order to reach the",
"   opposite side of the color circle, or to wrap around the circle more",
"   than once.								",
"									",
" The title string may contain a C printf format string containing a	",
"   conversion character for the frame number.  The frame number is	",
"   computed from dframe and fframe.  E.g., try setting title=\"Frame %g\".",
" 									",
NULL};
/**************** end self doc ********************************/

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 06/01/91
 * MODIFIED:  Craig Artley, Colorado School of Mines, 06/03/93
 *	      Optionally place a frame number in the plot title,
 *	      add facility to pause movie using Button3 (courtesy
 *	      of Dave Hale, Advance Geophysical).
 * MODIFIED:  Craig Artley, Colorado School of Mines, 11/23/93
 *	      Made hues used by hue map user-specifiable, added
 *	    user-specifiable saturation map.
 * MODIFIED:  Jack Cohen, Colorado School of Mines, 07/19/95
 *	    Added quit on q or Q keypress.
 * MODIFIED:  Craig Artley, Fairfield Industries, 08/31/98
 *	    Added Button 2 to reverse movie, and loop=2 switch to
 *	    automatically bounce movie off ends of frame loop.
 *	    Added interp=0 option for non-interpolated (blocky) images.
 * MODIFIED:  Sang-yong Suh, KIGAM, 06/20/2002
 *	    Added keypress functions to enable step mode display.
 */

#define DM_CONT	0
#define DM_STEP 1
static int displayMode;

/* data structure for one frame in film loop */
typedef struct FrameStruct {
	unsigned char *abytes;		/* bytes in one frame of loop */
	int nxa;			/* bytes per scanline in frame */
	int nya;			/* number of scanlines in frame */
	struct FrameStruct *next;	/* pointer to next frame in loop */
	struct FrameStruct *prev;	/* pointer to previous frame in loop */
} Frame;

/* client data structures for callbacks */
typedef struct ClientDataStruct {
	int n1;			/* number of float samples in 1st dimension */
	float d1;		/* sampling interval in 1st dimension */
	float f1;		/* first sample in 1st dimension */
	int n2;			/* number of float samples in 2nd dimension */
	float d2;		/* sampling interval in 2nd dimension */
	float f2;		/* first sample in 2nd dimension */
	float *floats;		/* one entire frame f(x1,x2) */
	float fmin;		/* float value corresponding to bmin */
	float fmax;		/* float value corresponding to bmax */
	unsigned char bmin;	/* minimum byte value in pixels */
	unsigned char bmax;	/* maximum byte value in pixels */
	float x1bega;		/* axis 1 beginning value in entire frame */
	float x1enda;		/* axis 1 ending value in entire frame */
	float x2bega;		/* axis 2 beginning value in entire frame */
	float x2enda;		/* axis 2 ending value in entire frame */
	int nxa;		/* bytes per scanline in entire frame */
	int nya;		/* number of scanlines in entire frame */
	unsigned char *abytes;	/* bytes in entire frame */
	int nxb;		/* bytes per scanline in zoomed frame */
	int nyb;		/* number of scanlines in zoomed frame */
	int ixb;		/* index of first byte in zoomed frame */
	int iyb;		/* index of first scanline in zoomed frame */
	unsigned char *bbytes;	/* bytes in zoomed frame */
	XImage *image;		/* pixels interpolated from bbytes */
	int exposed;		/* non-zero if frame has been exposed */
	int noframes;		/* non-zero until first frame processed */
	int looping;		/* non-zero if looping over frames */
	Frame *frame;		/* current frame in film loop */
	Widget axes;		/* axes widget inside which pixels are drawn */
	char *format;		/* title format string */
	char title[256];	/* title string */
	float fframe;		/* first frame sample */
	float dframe;		/* frame sample interval */
	int iframe;		/* index of current frame */
	int interp;		/* ==1 for smooth interpolated image */
	int forward;		/* direction of movie */
	int sleep;	      /* microsecond sleep to slow movie */
	XtWorkProcId wpid;	/* work proc id */
	XtAppContext ac;	/* application context */
} ClientData;

/* Bill Wingle's typedefs */
typedef struct {
    int  x1, y1, x2, y2;
    int  (*func) ();
    GC   gc;
    } GBUFFER;

typedef struct {
    int	  start_x, start_y, last_x, last_y;
    GC	   xorgc;
    GC	   gc;
    int	  (*current_func)();
    int	  foreground, background;
    int	  id;
    GBUFFER      buffer[10];
    int	  next_pos;
    } graphics_data;

graphics_data key_data;

/* work procedures */
Boolean readFrame (ClientData *cd);

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
static void adjustAxesValues (int n1, float d1, float f1,
	int n2, float d2, float f2,
	float *x1beg, float *x1end, float *x2beg, float *x2end);
static unsigned char *makeABytes (int style,
	float x1beg, float x1end, float x2beg, float x2end,
	float fmin, float fmax, unsigned char bmin, unsigned char bmax,
	int n1, float d1, float f1, int n2, float d2, float f2, float *floats,
	int *nxa, int *nya);
static unsigned char *makeBBytes (int nxa, int nya, unsigned char *abytes,
	int nxb, int nyb, int ixb, int iyb);
static XImage *makeImage (Display *dpy, int width, int height,
	int nx, int ny, int interp, unsigned char *bytes);
void key_pressed (Widget w, ClientData *data, XKeyEvent *event);

void intn2b (int nxin, float dxin, float fxin,
	     int nyin, float dyin, float fyin, unsigned char *zin,
	     int nxout, float dxout, float fxout,
	     int nyout, float dyout, float fyout, unsigned char *zout);

int
main (int argc, char **argv)
{
	int n1,n2,nz,iz,verbose,looping,nTic1,nTic2,width,height,interp;
	float d1,f1,d2,f2,*z,
		clip,bclip,wclip,white,wfrac,
		perc,bperc,wperc,*temp,
		bhue,whue,sat,bright,
		x1beg,x2beg,x1end,x2end,
		x1min,x1max,x2min,x2max,
		fframe,dframe;
	char *label1="",*label2="",*format="",*windowtitle="",
		*labelFont="",*titleFont="",
		*axesColor="",*gridColor="",*titleColor="",
		*style="normal",*grid1="none",*grid2="none",
		*cmap;
	ClientData cd;
	XrmValue from,to;
	Widget toplevel,axes;
	XtAppContext ac;
	Display *dpy;
	Window win;
	Arg args[98];
	int nargs;
	int scr;
	unsigned int depth;

	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters describing colormaps */
	cmap = "gray";  getparstring("cmap",&cmap);
	if (STREQ("saturation",cmap)) cmap = "sat";
	bhue = 0;  getparfloat("bhue",&bhue);  bhue /= 360.0;
	whue = 240;  getparfloat("whue",&whue); whue /= 360.0;
	sat = 1.0; getparfloat("sat",&sat);
	if (sat<0.0 || sat>1.0) err("sat must be in range [0,1]!\n");
	bright = 1.0; getparfloat("bright",&bright);
	if (bright<0.0 || bright>1.0) err("bright must be in range [0,1]!\n");

	/* get parameters describing 1st dimension sampling */
	if (!getparint("n1",&n1))
		err("Must specify number of samples in 1st dimension!\n");
	if (!getparfloat("d1",&d1)) d1 = 1.0;
	if (!getparfloat("f1",&f1)) f1 = 0.0;
	x1min = (d1>0.0)?f1:f1+(n1-1)*d1;
	x1max = (d1<0.0)?f1:f1+(n1-1)*d1;
	if (!getparfloat("x1beg",&x1beg)) x1beg = x1min;
	if (!getparfloat("x1end",&x1end)) x1end = x1max;

	/* get parameters describing 2nd dimension sampling */
	if (!getparint("n2",&n2))
		err("Must specify number of samples in 2nd dimension!\n");
	if (!getparfloat("d2",&d2)) d2 = 1.0;
	if (!getparfloat("f2",&f2)) f2 = 0.0;
	x2min = (d2>0.0)?f2:f2+(n2-1)*d2;
	x2max = (d2<0.0)?f2:f2+(n2-1)*d2;
	if (!getparfloat("x2beg",&x2beg)) x2beg = x2min;
	if (!getparfloat("x2end",&x2end)) x2end = x2max;

	/* read first frame of float data */
	nz = n1*n2;
	z = ealloc1float(nz);
	if (fread(z,sizeof(float),nz,stdin)!=nz)
		err("error reading input file");

	/* if necessary, determine clips from percentiles */
	if (getparfloat("clip",&clip)) {
		bclip = clip;
		wclip = -clip;
	}
	if ((!getparfloat("bclip",&bclip) || !getparfloat("wclip",&wclip)) &&
		!getparfloat("clip",&clip)) {
		perc = 100.0;  getparfloat("perc",&perc);
		temp = ealloc1float(nz);
		for (iz=0; iz<nz; iz++)
			temp[iz] = z[iz];
		if (!getparfloat("bclip",&bclip)) {
			bperc = perc;	getparfloat("bperc",&bperc);
			iz = (nz*bperc/100.0);
			if (iz<0) iz = 0;
			if (iz>nz-1) iz = nz-1;
			qkfind(iz,nz,temp);
			bclip = temp[iz];
		}
		if (!getparfloat("wclip",&wclip)) {
			wperc = 100.0-perc;  getparfloat("wperc",&wperc);
			iz = (nz*wperc/100.0);
			if (iz<0) iz = 0;
			if (iz>nz-1) iz = nz-1;
			qkfind(iz,nz,temp);
			wclip = temp[iz];
		}
		free1float(temp);
	}
	if (!getparfloat("white",&white)) white = (bclip+wclip)/2.0;
	if (!getparint("verbose",&verbose)) verbose = 1;
	if (!getparint("sleep",&cd.sleep)) cd.sleep=0 ;
	if (!getparint("loop",&looping)) looping = 0;
	if (verbose) {
		if(STREQ(cmap,"sat") || STREQ(cmap,"customsat")) {
			warn("bclip=%g wclip=%g white=%g",bclip,wclip,white);
		} else {
			warn("bclip=%g wclip=%g",bclip,wclip);
		}
	}
	wfrac = (bclip!=wclip) ? (bclip-white)/(bclip-wclip) : 1.0;

	/* initialize toolkit and set toplevel parameters */
 	toplevel = XtAppInitialize(&ac,"XMovie_XAPP_DEF",NULL,0,&argc,argv,NULL,NULL,0);
	dpy = XtDisplay(toplevel);
	nargs = 0;
	if (getparstring("windowtitle",&windowtitle))
		{XtSetArg(args[nargs],XtNtitle,windowtitle); nargs++;}
	if (getparstring("windowtitle",&windowtitle))
		{XtSetArg(args[nargs],XtNiconName,windowtitle); nargs++;}
	if (getparint("width",&width))
		{XtSetArg(args[nargs],XtNwidth,width); nargs++;}
	if (getparint("height",&height))
		{XtSetArg(args[nargs],XtNheight,height); nargs++;}
	XtSetArg(args[nargs],XtNinput,TRUE);nargs++; 
	XtSetValues(toplevel,args,nargs);

	/* if necessary, make private colormap */
	win = XRootWindowOfScreen(XtScreen(toplevel));
	nargs = 0;

	scr=DefaultScreen(dpy);
	depth=(unsigned int)DefaultDepth(dpy,scr);

	if (depth<=8) {
	  if (STREQ(cmap,"gray")) {
	    Colormap cm=XtcwpCreateGrayColormap(dpy,win);
	    XtSetArg(args[nargs],XtNcolormap,cm); nargs++;
	  } else if (STREQ(cmap,"hue")) {
	    Colormap cm=XtcwpCreateHueColormap(dpy,win,
					       bhue,whue,sat,bright);
	    XtSetArg(args[nargs],XtNcolormap,cm); nargs++;
	  } else if (STREQ(cmap,"sat")) {
	    Colormap cm=XtcwpCreateSatColormap(dpy,win,
					       bhue,whue,wfrac,bright);

	    XtSetArg(args[nargs],XtNcolormap,cm); nargs++;
	  }
	  XtSetValues(toplevel,args,nargs);
	}

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
	if (getparstring("title",&format))
		{XtSetArg(args[nargs],XtNtitle,format); nargs++;}
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
	XtSetValues(axes,args,nargs);
	x1beg = x1min; getparfloat("x1beg",&x1beg);
	x1end = x1max; getparfloat("x1end",&x1end);
	x2beg = x2min; getparfloat("x2beg",&x2beg);
	x2end = x2max; getparfloat("x2end",&x2end);
	adjustAxesValues(n1,d1,f1,n2,d2,f2,&x1beg,&x1end,&x2beg,&x2end);
	XtcwpSetAxesValues(axes,x1beg,x1end,x2beg,x2end);

	/* frame sampling */
	if (!getparfloat("fframe",&fframe)) fframe = 1.0;
	if (!getparfloat("dframe",&dframe)) dframe = 1.0;

	/* interpolation */
	if (!getparint("interp",&interp)) interp = 1;

	/* initial display mode: cont or step */
	if (!getparint("idm", &displayMode)) displayMode = DM_CONT;

	checkpars();

	/* initialize client data */
	cd.n1 = n1;  cd.d1 = d1;  cd.f1 = f1;
	cd.n2 = n2;  cd.d2 = d2;  cd.f2 = f2;
	cd.floats = z;
	cd.fmin = bclip;
	cd.fmax = wclip;
	cd.bmin = (unsigned char) (XtcwpGetFirstPixel(dpy));
	cd.bmax = (unsigned char) (XtcwpGetLastPixel(dpy));
	if(cd.bmax==0)cd.bmax=255;
	warn("bmin=%d bmax=%d",cd.bmin,cd.bmax);
	cd.x1bega = x1beg;
	cd.x1enda = x1end;
	cd.x2bega = x2beg;
	cd.x2enda = x2end;
	cd.abytes = NULL;
	cd.bbytes = NULL;
	cd.image = NULL;
	cd.exposed = 0;
	cd.noframes = 1;
	cd.axes = axes;
	cd.looping = looping;
	cd.frame = NULL;
	cd.format = format;
	cd.iframe = 0;
	cd.fframe = fframe;
	cd.dframe = dframe;
	cd.interp = interp;
	cd.forward = 1;
	cd.ac = ac;

	/* adjust axes title if formatted */
	if (strchr(cd.format,'%') && !strstr(cd.format,"%%")) {
		sprintf(cd.title,cd.format,cd.fframe+cd.iframe*cd.dframe);
		XtVaSetValues(cd.axes,XtNtitle,cd.title,NULL);
	}

	/* add work procedure */
	cd.wpid = XtAppAddWorkProc(ac,(XtWorkProc) readFrame,&cd);

	/* add callbacks to axes widget */
	XtAddCallback(axes,XtNresizeCallback,(XtCallbackProc) resizeCB,&cd);
	XtAddCallback(axes,XtNexposeCallback,(XtCallbackProc) exposeCB,&cd);
	XtAddCallback(axes,XtNinputCallback,(XtCallbackProc) inputCB,&cd);

	/* add Button2 translation for reversing the movie */
	XtOverrideTranslations(axes,
		XtParseTranslationTable("<Btn2Up>: input()"));

	/* add Button3 translation for pausing the movie */
	XtOverrideTranslations(axes,
		XtParseTranslationTable("<Btn3Up>: input()"));

	/* set up keypress */
	XtAddEventHandler(axes, KeyPress, FALSE,
			  (XtEventHandler) key_pressed, &cd);

	/* realize everything */
	XtRealizeWidget(toplevel);

	/* go */
	XtAppMainLoop(ac);

	return EXIT_SUCCESS;
}

#include <unistd.h>
Boolean readFrame (ClientData *cd)
/*****************************************************************************
An XtWorkProc that reads a frame of floats when there is nothing else to do.
If a new frame of floats exists (assumed true on the first call),
 (1) converts floats to abytes, according to the clip parameters and
     the initial window parameters.
 (2) sets bbytes and image to NULL, so that they will be remade next 
     expose event.
 (3) forces an expose event by clearing one pixel (kludge!!!).
Finally, if the current frame has been exposed, attempts to read a new 
frame of floats.
*****************************************************************************/
{
	int n1=cd->n1,n2=cd->n2,nxa=cd->nxa,nya=cd->nya,looping=cd->looping;
	float d1=cd->d1,f1=cd->f1,d2=cd->d2,f2=cd->f2,
		x1bega=cd->x1bega,x1enda=cd->x1enda,
		x2bega=cd->x2bega,x2enda=cd->x2enda,
		fmin=cd->fmin,fmax=cd->fmax,
		*floats=cd->floats;
	unsigned char bmin=cd->bmin,bmax=cd->bmax,
		*abytes=cd->abytes;
	Frame *frame=cd->frame,*frame1;
	int nargs,style;
	Arg args[10];
	static int newframe=1,nframe=0;

	/* if a new frame exists */
	if (newframe) {

		/* count frames */
		nframe++;

		/* DEBUG
		fprintf(stderr,"frame %d\n",nframe);
		*/

		/* determine axes style */
		nargs = 0;
		XtSetArg(args[nargs],XtNstyle,&style); nargs++;
		XtGetValues(cd->axes,args,nargs);

		/* if not looping, free old bytes */
		if (!looping && abytes!=NULL) free1(abytes);
		
		/* convert frame of floats to bytes */
		abytes = makeABytes(style,x1bega,x1enda,x2bega,x2enda,
			fmin,fmax,bmin,bmax,
			n1,d1,f1,n2,d2,f2,floats,
			&nxa,&nya);
		
		/* if looping, save bytes in list */
		if (looping) {
			frame1 = (Frame*)malloc(sizeof(Frame));
			frame1->abytes = abytes;
			frame1->nxa = nxa;
			frame1->nya = nya;
			if (frame==NULL) {
				frame1->prev = frame1->next = frame1;
			} else {
				frame1->prev = frame;
				frame1->next = frame->next;
				frame->next->prev = frame1;
				frame->next = frame1;
			}
			frame = frame1;
		}
		
		/* free bbytes and destroy image */
		if (cd->bbytes!=NULL) free1(cd->bbytes);
		if (cd->image!=NULL) XDestroyImage(cd->image);

		/* set client data */
		if (cd->noframes) {
			cd->nxb = nxa;
			cd->nyb = nya;
			cd->ixb = 0;
			cd->iyb = 0;
			cd->noframes = 0;
		}
		cd->iframe = nframe-1;
		cd->nxa = nxa;
		cd->nya = nya;
		cd->abytes = abytes;
		cd->bbytes = NULL;
		cd->image = NULL;
		cd->frame = frame;
		cd->exposed = 0;
		
		/* Sleep to slow down frames */
		if( cd->sleep ){ /* sleep in seconds */
		   usleep( 100000*(cd->sleep) );
		}

		/* clear one pixel to force expose event (kludge!!!) */
		XClearArea(XtDisplay(cd->axes),XtWindow(cd->axes),
			0,0,1,1,True);
	}
	
	/* if we've exposed previous frame, try to read another frame */
	if (cd->exposed) {
		newframe = (fread(floats,sizeof(float),n1*n2,stdin)==n1*n2);

		/* if no more new frames can be read */
		if (!newframe) {

			/* if not looping, quit calling ourself */
			if (!looping) return True;
		
			/* free bbytes and destroy image */
			if (cd->bbytes!=NULL) free1(cd->bbytes);
			if (cd->image!=NULL) XDestroyImage(cd->image);

			/* reverse direction at ends of movie */
			if (looping==2) {
			    if (((cd->forward) && (cd->iframe==nframe-1)) ||
				((!cd->forward) && (cd->iframe==0))) {
				cd->forward = !cd->forward;
			    }
			}

			/* looping, so set abytes to next frame */
			if (cd->forward)
			    frame = frame->next;
			else
			    frame = frame->prev;
			abytes = frame->abytes;
			nxa = frame->nxa;
			nya = frame->nya;

			/* set client data */
			if (cd->forward) {
			    ++cd->iframe;
			    if (cd->iframe==nframe) cd->iframe = 0;
			} else {
			    --cd->iframe;
			    if (cd->iframe==-1) cd->iframe = nframe-1;
			}
			cd->nxa = nxa;
			cd->nya = nya;
			cd->abytes = abytes;
			cd->bbytes = NULL;
			cd->image = NULL;
			cd->frame = frame;
			cd->exposed = 0;
		
			if( cd->sleep ){
			   usleep( 100000*cd->sleep );
			}

			/* clear one pixel to force expose event (kludge!!!) */
			XClearArea(XtDisplay(cd->axes),XtWindow(cd->axes),
				0,0,1,1,True);
			if( displayMode == DM_STEP )
				return True;
		}
	
	/* else, do not read new frame until previous frame exposed */
	} else {
		newframe = 0;
	}
	
	/* ensure that we will be called again */
	/* return False; */
	return ( displayMode == DM_STEP && cd->exposed );
}

void exposeCB (Widget w, 
	ClientData *cd,
	XtcwpAxesCallbackStruct *ca)
/*****************************************************************************
Expose event callback - draws image inside axes box.
If abytes is NULL, simple returns.
If bbytes is NULL, makes bbytes from abytes.
If image is NULL, makes image from bbytes before drawing.
*****************************************************************************/
{
	int nxa=cd->nxa,nya=cd->nya,nxb=cd->nxb,nyb=cd->nyb,
		ixb=cd->ixb,iyb=cd->iyb;
	unsigned char *abytes=cd->abytes,*bbytes=cd->bbytes;
	XImage *image=cd->image;
	Position x=ca->x,y=ca->y;
	Dimension width=ca->width,height=ca->height;
	Display *dpy=XtDisplay(w);
	Window win=XtWindow(w);
	GC gc;

	/* if abytes do not exist, return */
	if (abytes==NULL) return;
	
	/* if necessary, make bbytes */
	if (bbytes==NULL) bbytes = makeBBytes(nxa,nya,abytes,nxb,nyb,ixb,iyb);
		
	/* if necessary, make image */
	if (image==NULL) image = makeImage(dpy,width,height,nxb,nyb,
					   cd->interp,bbytes);

	/* create GC */
	gc = XCreateGC(dpy,win,0L,NULL);
	
	/* draw image */
	XPutImage(dpy,win,gc,image,0,0,x,y,image->width,image->height);
	
	/* free GC */
	XFreeGC(dpy,gc);
	
	/* update client data */
	cd->bbytes = bbytes;
	cd->image = image;
	cd->exposed = 1;

	/* update axes */
	if (strchr(cd->format,'%') && !strstr(cd->format,"%%")) {
		sprintf(cd->title,cd->format,cd->fframe+cd->iframe*cd->dframe);
		XtVaSetValues(cd->axes,XtNtitle,cd->title,NULL);
	}
}

void inputCB (Widget w, 
	ClientData *cd,
	XtcwpAxesCallbackStruct *ca)
/*****************************************************************************
Input event callback - currently handles rubber zoom box and pause on
button 3 only.  Updates dimensions and start indices of bbytes, based
on user-dragged zoom box, and sets both bbytes and image to NULL, so
that expose callback will make new bbytes and image.
*****************************************************************************/
{
	int nxa=cd->nxa,nya=cd->nya,
		nxb=cd->nxb,nyb=cd->nyb,ixb=cd->ixb,iyb=cd->iyb;
	unsigned char *bbytes=cd->bbytes;
	float x1bega=cd->x1bega,x1enda=cd->x1enda,
		x2bega=cd->x2bega,x2enda=cd->x2enda;
	int x=ca->x,y=ca->y,width=ca->width,height=ca->height;
	float x1beg=ca->x1beg,x1end=ca->x1end,x2beg=ca->x2beg,x2end=ca->x2end;
	int style=ca->style;
	XEvent *event=ca->event;
	int xb,yb,wb,hb;
	int nxbn,nybn,ixbn,iybn;
	float x1begn,x1endn,x2begn,x2endn;
	static int stopflag=0;

	/* check for button 2 */
	if (event->type==ButtonRelease && event->xbutton.button==Button2) {
		cd->forward = !cd->forward;
		return;
	}

	/* check for button 3 */
	if (event->type==ButtonRelease && event->xbutton.button==Button3) {
		stopflag = !stopflag;
		if (stopflag) {
			XtRemoveWorkProc(cd->wpid);
		} else {
			cd->wpid = XtAppAddWorkProc(cd->ac,
					(XtWorkProc) readFrame,cd);
		}
		return;
	}

	/* track pointer and get rubber box */
	XtcwpRubberBox(XtDisplay(w),XtWindow(w),*event,&xb,&yb,&wb,&hb);

	/* if zoom box has tiny width or height */
	if (wb<3 || hb<3) {
	
		/* restore number of samples inside box */
		nxbn = nxa;
		nybn = nya;
		
		/* restore indices of first samples inside box */
		ixbn = 0;
		iybn = 0;
		
		/* restore axes limits */
		x1begn = x1bega;
		x1endn = x1enda;
		x2begn = x2bega;
		x2endn = x2enda;
	
	/* else if valid zoom box */
	} else {
	
		/* clip zoom box to lie within axes rectangle */
		if (xb<x) {
			wb -= x-xb;
			xb = x;
		}
		if (yb<y) {
			hb -= y-yb;
			yb = y;
		}
		if (xb+wb>x+width) wb = x-xb+width;
		if (yb+hb>y+height) hb = y-yb+height;
		
		/* determine number of samples inside box (at least 2 by 2) */
		nxbn = 1+NINT((float)wb/width*(nxb-1));
		if (nxbn<2) nxbn = 2;
		nybn = 1+NINT((float)hb/height*(nyb-1));
		if (nybn<2) nybn = 2;
		
		/* determine indices of first samples inside box */
		ixbn = ixb+NINT((float)(xb-x)/width*(nxb-1));
		if (ixbn+nxbn>ixb+nxb) ixbn = ixb+nxb-nxbn;
		iybn = iyb+NINT((float)(yb-y)/height*(nyb-1));
		if (iybn+nybn>iyb+nyb) iybn = iyb+nyb-nybn;
		
		/* determine axes limits */
		if (style==XtcwpNORMAL) {
			x1begn = x1beg+(ixbn-ixb)*(x1end-x1beg)/(nxb-1);
			x1endn = x1beg+(ixbn+nxbn-ixb-1)*(x1end-x1beg)/(nxb-1);
			x2begn = x2end+(iybn+nybn-iyb-1)*(x2beg-x2end)/(nyb-1);
			x2endn = x2end+(iybn-iyb)*(x2beg-x2end)/(nyb-1);
		} else {
			x1endn = x1beg+(iybn+nybn-iyb-1)*(x1end-x1beg)/(nyb-1);
			x1begn = x1beg+(iybn-iyb)*(x1end-x1beg)/(nyb-1);
			x2begn = x2beg+(ixbn-ixb)*(x2end-x2beg)/(nxb-1);
			x2endn = x2beg+(ixbn+nxbn-ixb-1)*(x2end-x2beg)/(nxb-1);
		}
	}
	
	/* set axes limits */
	XtcwpSetAxesValues(w,x1begn,x1endn,x2begn,x2endn);
	
	/* set client data */
	cd->nxb = nxbn;
	cd->nyb = nybn;
	cd->ixb = ixbn;
	cd->iyb = iybn;

	/* if bytes inside box exist, destroy and set pointer to NULL */
	if (bbytes!=NULL) free1(bbytes);
	cd->bbytes = NULL;
	
	/* if image exists, destroy and set pointer to NULL */
	if (cd->image!=NULL) {
		XDestroyImage(cd->image);
		cd->image = NULL;
	}
	
	/* clear window and force an expose event */
	XClearArea(XtDisplay(w),XtWindow(w),0,0,0,0,True);
}

void resizeCB (Widget w, 
	ClientData *cd,
	XtcwpAxesCallbackStruct *ca)
/*****************************************************************************
Resize event callback - destroys image, so that expose callback
will remake it.
*****************************************************************************/
{
	/* if an image exists, destroy it and set pointer to NULL */
	if (cd->image!=NULL) {
		XDestroyImage(cd->image);
		cd->image = NULL;
	}
	
	if (cd->image != NULL) /* dummy */
		resizeCB(w,cd,ca);
	
	/* clear window and force an expose event */
	/*
	if (cd->exposed) XClearArea(XtDisplay(w),XtWindow(w),0,0,0,0,True);
	*/
}

static void adjustAxesValues (int n1, float d1, float f1,
	int n2, float d2, float f2,
	float *x1beg, float *x1end, float *x2beg, float *x2end)
/*****************************************************************************
Adjust axes values to nearest samples; ensure at least 2 by 2 samples.
*****************************************************************************/
{
	int ix1beg,ix1end,ix2beg,ix2end;
	
	ix1beg = NINT((*x1beg-f1)/d1);
	ix1end = NINT((*x1end-f1)/d1);
	ix1beg = MAX(0,MIN(n1-1,ix1beg));
	ix1end = MAX(0,MIN(n1-1,ix1end));
	if (ix1beg==ix1end) {
		if (*x1beg<*x1end) {
			if (ix1beg>0)
				ix1beg--;
			else
				ix1end++;
		} else {
			if (ix1beg<n1-1)
				ix1beg++;
			else
				ix1end--;
		}
	}
	*x1beg = f1+ix1beg*d1;
	*x1end = f1+ix1end*d1;
	ix2beg = NINT((*x2beg-f2)/d2);
	ix2end = NINT((*x2end-f2)/d2);
	ix2beg = MAX(0,MIN(n2-1,ix2beg));
	ix2end = MAX(0,MIN(n2-1,ix2end));
	if (ix2beg==ix2end) {
		if (*x2beg<*x2end) {
			if (ix2beg>0)
				ix2beg--;
			else
				ix2end++;
		} else {
			if (ix2beg<n2-1)
				ix2beg++;
			else
				ix2end--;
		}
	}	
	*x2beg = f2+ix2beg*d2;
	*x2end = f2+ix2end*d2;
}

static unsigned char *makeABytes (int style,
	float x1beg, float x1end, float x2beg, float x2end,
	float fmin, float fmax, unsigned char bmin, unsigned char bmax,
	int n1, float d1, float f1, int n2, float d2, float f2, float *floats,
	int *nxa, int *nya)
/*****************************************************************************
Makes a frame of bytes from a frame of floats.  This function
is responsible for making an initial (non-zoomed) frame of
bytes from a frame of floats.  Floats outside the range
[fmin,fmax] are clipped in converting to bytes in the
corresponding range [bmin,bmax].  The output bytes are ordered
as nxa samples left-to-right, nya scanlines top-to-bottom, where
nxa and nya are computed according to the window specified
by x1beg, x1end, x2beg, and x2end.  Only floats inside this
window are converted.
*****************************************************************************/
{
	int i1,i2,ix,iy,i1beg,i1end,i1step,i2beg,i2end,i2step,nx,ny;
	unsigned char *b,*bp;
	float fbmin,fbmax,fscale=0.0,foffset,fi,*f=floats;
	
	/* determine coefficients to convert from floats to bytes */
	fbmin = bmin;
	fbmax = bmax;
	if(fbmax==0.0) fbmax=255.0;
	/*	if( fmax < fmin){float tmp=fmax; fmax=fmin; fmin=tmp;}*/
	/* warn("fbmin=%f fbmax=%f fscale=%f",fbmin,fbmax,fscale); */
	if(fbmax==0)fbmax=255;
	fscale = (fmax!=fmin) ? (fbmax-fbmin)/(fmax-fmin) : 1.0e10;

	foffset = fbmin-fmin*fscale;
	/* warn("fbmin=%f fbmax=%f fscale=%f",fbmin,fbmax,fscale);	 */
	/* adjust axes limits to nearest samples */
	adjustAxesValues(n1,d1,f1,n2,d2,f2,&x1beg,&x1end,&x2beg,&x2end);
	
	/* determine sample index limits and increments */
	i1beg = NINT((x1beg-f1)/d1);
	i1end = NINT((x1end-f1)/d1);
	i1step = (i1end>i1beg) ? 1 : -1;
	i2beg = NINT((x2beg-f2)/d2);
	i2end = NINT((x2end-f2)/d2);
	i2step = (i2end>i2beg) ? 1 : -1;
	
	/* convert floats to bytes */
	if (style==XtcwpNORMAL) {
		nx = 1+ABS(i1end-i1beg);
		ny = 1+ABS(i2end-i2beg);
		b = ealloc1(nx*ny,sizeof(unsigned char));
		for (iy=0,i2=i2beg; iy<ny; ++iy,i2+=i2step) {
			bp = b+nx*ny-(iy+1)*nx;
			for (ix=0,i1=i1beg; ix<nx; ++ix,i1+=i1step) {
				fi = foffset+f[i1+i2*n1]*fscale;
				if (fi<fbmin) fi = fbmin;
				if (fi>fbmax) fi = fbmax;
				*bp++ = (unsigned char)fi;
			}
		}
	} else {
		nx = 1+ABS(i2end-i2beg);
		ny = 1+ABS(i1end-i1beg);
		bp = b = ealloc1(nx*ny,sizeof(unsigned char));
		for (iy=0,i1=i1beg; iy<ny; ++iy,i1+=i1step) {
			for (ix=0,i2=i2beg; ix<nx; ++ix,i2+=i2step) {
				fi = foffset+f[i1+i2*n1]*fscale;
				if (fi<fbmin) fi = fbmin;
				if (fi>fbmax) fi = fbmax;
				*bp++ = (unsigned char)fi;
			}
		}
	}
	
	/* set output parameters and return */
	*nxa = nx;
	*nya = ny;
	return b;
}

static unsigned char *makeBBytes (int nxa, int nya, unsigned char *abytes,
	int nxb, int nyb, int ixb, int iyb)
/*****************************************************************************
Makes bytes (unsigned char) in zoom box, with bytes ordered
as nxb samples left-to-right, nyb scanlines top-to-bottom.
*****************************************************************************/
{
	int ix,iy;
	unsigned char *b,*bp,*ap;
	
	bp = b = ealloc1(nxb*nyb+0*nya,sizeof(unsigned char));
	for (iy=0; iy<nyb; ++iy)
		for (ix=0,ap=abytes+(iyb+iy)*nxa+ixb; ix<nxb; ++ix)
			*bp++ = *ap++;
	return b;
}

static XImage *makeImage (Display *dpy,  int width, int height,
	int nx, int ny, int interp, unsigned char *bytes)
/*****************************************************************************
Makes image from bytes ordered as left-to-right, top-to-bottom scanlines.
*****************************************************************************/
{
	int widthpad;
	unsigned char *pixels;
	unsigned char *data;
	Colormap wcmap;
	XColor color;
	int i,j,k;
	int scr;
	int depth;
	unsigned long truecolor_pixel[256];
	XImage *xim;
	int byte_perpixel;
	int line, iline,jline;
	int ih,half;
	Screen *scr1;
	

# define RGB_BLACK      {0x00, 0x00, 0x00}
# define RGB_WHITE      {0xff, 0xff, 0xff}
# define RGB_GRAY       {0x80, 0x80, 0x80}


	float c_rgb [3][3]  = 
	{ RGB_BLACK,    RGB_GRAY,   RGB_WHITE  };

half=128;
byte_perpixel=4;
	  scr1=XDefaultScreenOfDisplay(dpy);

	xim=(XImage *) NULL;

	scr=DefaultScreen(dpy);
	wcmap=DefaultColormapOfScreen(scr1);

	/* Build the 1st ramp					   */
	for (ih = 0; ih < 128; ++ih) {
		color.red   = c_rgb[0][0] +  
			(c_rgb[1][0] - c_rgb[0][0]) * ((float) ih)/((float) half);
		color.green = c_rgb[0][1] +
			(c_rgb[1][1] - c_rgb[0][1]) * ((float) ih)/((float) half);
		color.blue  = c_rgb[0][2] +
			(c_rgb[1][2] - c_rgb[0][2]) * ((float) ih)/((float) half);
 
		color.red   *= 257.0;
		color.green *= 257.0;
		color.blue  *= 257.0;
	
		color.flags = DoRed|DoGreen|DoBlue;
XAllocColor(dpy,wcmap,&color);
	  truecolor_pixel[ih]=(unsigned long )color.pixel;
		
	}
		
	/* Build the 2nd ramp					   */
	for (ih=128; ih<256; ++ih) {
		color.red   = c_rgb[1][0] +
			(c_rgb[2][0] - c_rgb[1][0]) * ((float) (ih-half))/((float) half);
		color.green = c_rgb[1][1] +
			(c_rgb[2][1] - c_rgb[1][1]) * ((float) (ih-half))/((float) half);
		color.blue  = c_rgb[1][2] +
			(c_rgb[2][2] - c_rgb[1][2]) * ((float) (ih-half))/((float) half);
		
		color.red   *= 257.0;
		color.green *= 257.0;
		color.blue  *= 257.0;
		color.flags = DoRed|DoGreen|DoBlue;			
		XAllocColor(dpy,wcmap,&color);
		truecolor_pixel[ih]=(unsigned long )color.pixel;
			    
	}	   

	depth=(unsigned int)DefaultDepth(dpy,scr);
	if(depth<=8) byte_perpixel=1;
	else if(depth<=16) byte_perpixel=2;
	

	/* determine scanline padding and allocate memory for pixels */
	widthpad = (1+(width-1)/(BitmapPad(dpy)/8))*BitmapPad(dpy)/8;
	pixels = ealloc1(widthpad*height,sizeof(unsigned char));

	/* bilinearly interpolate bytes to pixels */
	if (interp)
	    intl2b(nx,1.0,0.0,ny,1.0,0.0,bytes,
		   width,(float)(nx-1)/(float)(width-1),0.0,
		   height,(float)(ny-1)/(float)(height-1),0.0,pixels);
	else
	    intn2b(nx,1.0,0.0,ny,1.0,0.0,bytes,
		   width,(float)(nx-1)/(float)(width-1),0.0,
		   height,(float)(ny-1)/(float)(height-1),0.0,pixels);
	


data = ealloc1(widthpad*height,byte_perpixel);

	xim=XCreateImage(     (Display *) dpy,
			   (Visual *) DefaultVisual(dpy,scr),
			   (unsigned int) DefaultDepth(dpy,scr),
			   (int) ZPixmap,
			   (int) 0,
			   (char *) data,
			   (unsigned int) widthpad,
			   (unsigned int) height,
			   (int) BitmapPad(dpy),
			   (int) widthpad*byte_perpixel);


	byte_perpixel=xim->bits_per_pixel/8;
/*
	fprintf(stderr,"\nbyte_perpixel = %d, depth= %d\n", byte_perpixel,depth);
*/

	/* translate bytes to pixels, padding scanlines as necessary */
	for (line=0; line<height; line++) {
		iline = line*width;
		jline = line*widthpad;
		for (i=iline,j=jline,k=0; k<width; ++i,++j,++k)
		{       if(byte_perpixel==1)
			((unsigned char *)data)[j] =(unsigned char)pixels[i];
			if(byte_perpixel==2)
			  {
			    int edn=xim->byte_order;
			    if(edn==LSBFirst){
			      ((unsigned char *)data)[j*2+0] =(unsigned char)(truecolor_pixel[pixels[i]]);
			      ((unsigned char *)data)[j*2+1] =(unsigned char)(truecolor_pixel[pixels[i]]>>8);
			    }else{
			      ((unsigned char *)data)[j*2+0] =(unsigned char)(truecolor_pixel[pixels[i]]>>24); 
			      ((unsigned char *)data)[j*2+1] =(unsigned char)(truecolor_pixel[pixels[i]]>>16);
			      }

			    /*((unsigned short *)data)[j] =(unsigned short)(truecolor_pixel[pixels[i]]);*/
			    /*((unsigned short *)data)[j] =(unsigned short)(truecolor_pixel[pixels[i]]);*/
			  }	
			if(byte_perpixel==3){
			int edn=xim->byte_order;
			if(edn==LSBFirst){
			((unsigned char *)data)[j*3+0] =(unsigned char)(truecolor_pixel[pixels[i]]);
			((unsigned char *)data)[j*3+1] =(unsigned char)(truecolor_pixel[pixels[i]]>>8);
			((unsigned char *)data)[j*3+2] =(unsigned char)(truecolor_pixel[pixels[i]]>>16);
			}else{
			((unsigned char *)data)[j*3+0] =(unsigned char)(truecolor_pixel[pixels[i]]>>24);   
			((unsigned char *)data)[j*3+1] =(unsigned char)(truecolor_pixel[pixels[i]]>>16); 
			((unsigned char *)data)[j*3+2] =(unsigned char)(truecolor_pixel[pixels[i]]>>8);
			}

			}	
			if(byte_perpixel==4){
			  int edn=xim->byte_order;
			  if(edn==LSBFirst){
			    ((unsigned char *)data)[j*4+0] =(unsigned char)(truecolor_pixel[pixels[i]]);
			    ((unsigned char *)data)[j*4+1] =(unsigned char)(truecolor_pixel[pixels[i]]>>8);
			    ((unsigned char *)data)[j*4+2] =(unsigned char)(truecolor_pixel[pixels[i]]>>16);
			    ((unsigned char *)data)[j*4+3] =(unsigned char)(truecolor_pixel[pixels[i]]>>24);
			  }else{
			    ((unsigned char *)data)[j*4+0] =(unsigned char)(truecolor_pixel[pixels[i]]>>24);
			    ((unsigned char *)data)[j*4+1] =(unsigned char)(truecolor_pixel[pixels[i]]>>16);   
			    ((unsigned char *)data)[j*4+2] =(unsigned char)(truecolor_pixel[pixels[i]]>>8); 
			    ((unsigned char *)data)[j*4+3] =(unsigned char)(truecolor_pixel[pixels[i]]);
			  }
			/*((unsigned long *)data)[j] =(unsigned long)truecolor_pixel[pixels[i]];*/
			}

		}
		for (j=jline+width,k=width; k<widthpad; ++j,++k)
		{

		       if(byte_perpixel==1)
			((unsigned char *)data)[j] =((unsigned char *)data)[jline+width-1];
			if(byte_perpixel==2)
			  {
			 ((unsigned char *)data)[j*2+0] =((unsigned char *)data)[(jline+width-1)*2+0];
			((unsigned char *)data)[j*2+1] =((unsigned char *)data)[(jline+width-1)*2+1];
			/*((unsigned short *)data)[j] =((unsigned short *)data)[jline+width-1];*/
			  }
			if(byte_perpixel==3){
			((unsigned char *)data)[j*3+0] =((unsigned char *)data)[(jline+width-1)*3+0];
			((unsigned char *)data)[j*3+1] =((unsigned char *)data)[(jline+width-1)*3+1];
			((unsigned char *)data)[j*3+2] =((unsigned char *)data)[(jline+width-1)*3+2];
			}
			if(byte_perpixel==4)
			{
		       ((unsigned char *)data)[j*4+0] =((unsigned char *)data)[(jline+width-1)*4+0];
			((unsigned char *)data)[j*4+1] =((unsigned char *)data)[(jline+width-1)*4+1];
			((unsigned char *)data)[j*4+2] =((unsigned char *)data)[(jline+width-1)*4+2];
			((unsigned char *)data)[j*4+3] =((unsigned char *)data)[(jline+width-1)*4+3];    
			/*((unsigned long *)data)[j] =((unsigned long *)data)[jline+width-1];*/
			}

		}
	}
	
	/* create and return image structure */
	free1(pixels);
	return xim;



}

/*****************************************************************************
Bill Wingle's routine to handle key presses.
*****************************************************************************/
void key_pressed (Widget w, ClientData *cd, XKeyEvent *event)
{
	char	   buffer[2];
	int	    bufsize = 2;
	KeySym	 key;
	XComposeStatus compose;

	XLookupString (event, buffer, bufsize, &key, &compose);

	if (key==XK_q || key==XK_Q)  {
		exit(0);
	}
	else if (key==XK_s || key==XK_S) {
		XtRemoveWorkProc(cd->wpid);
		displayMode = DM_STEP;
		return;
	}
	if (key==XK_c || key==XK_C)
		displayMode = DM_CONT;
	else if (key==XK_b || key==XK_B || key==XK_Left)
		cd->forward = 0;
	else if (key==XK_f || key==XK_F || key==XK_n || key==XK_N || key==XK_Right)
		cd->forward = 1;
	else
		return;
	cd->wpid = XtAppAddWorkProc(cd->ac, (XtWorkProc) readFrame,cd);
}

void intn2b (int nxin, float dxin, float fxin,
	     int nyin, float dyin, float fyin, unsigned char *zin,
	     int nxout, float dxout, float fxout,
	     int nyout, float dyout, float fyout, unsigned char *zout)
/*****************************************************************************
nearest neighbor interpolation of a 2-D array of bytes
******************************************************************************
Input:
nxin		number of x samples input (fast dimension of zin)
dxin		x sampling interval input
fxin		first x sample input
nyin		number of y samples input (slow dimension of zin)
dyin		y sampling interval input
fyin		first y sample input
zin		array[nyin][nxin] of input samples (see notes)
nxout		number of x samples output (fast dimension of zout)
dxout		x sampling interval output
fxout		first x sample output
nyout		number of y samples output (slow dimension of zout)
dyout		y sampling interval output
fyout		first y sample output

Output:
zout		array[nyout][nxout] of output samples (see notes)
******************************************************************************
Notes:
The arrays zin and zout must passed as pointers to the first element of
a two-dimensional contiguous array of unsigned char values.

Constant extrapolation of zin is used to compute zout for
output x and y outside the range of input x and y.
******************************************************************************
Author:  Craig Artley, Fairfield Industries, 08/31/98
*****************************************************************************/
{
    float xout, yout, xi, yi;
    int ixout, iyout, ix, iyin, *ixin;

    ixin = ealloc1int(nxout);

    for (ixout=0, xout=fxout; ixout<nxout; ++ixout, xout+=dxout) {
	xi = (xout-fxin)/dxin;
	ix = NINT(xi);
	ix = MAX(0,ix);
	ix = MIN(ix,nxin-1);
	ixin[ixout] = ix;
    }

    for (iyout=0, yout=fyout; iyout<nyout; ++iyout, yout+=dyout) {
	yi = (yout-fyin)/dyin;
	iyin = NINT(yi);
	iyin = MAX(0,iyin);
	iyin = MIN(iyin,nyin-1);

	for (ixout=0; ixout<nxout; ++ixout) {
	    *zout++ = zin[iyin*nxin+ixin[ixout]];
	}
    }

    free1int(ixin);
}
