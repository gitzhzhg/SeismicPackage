/* SXPLOT: $Test Release: 1.1 $ ; $Date: 2009/11/10 16:08:16 $   */

#include "par.h"
#include "Triangles/tri.h"
#include "Triangles/sloth.h"
#include "Xtcwp/Xtcwp.h"
#include "Xtcwp/Axes.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                                       ",
" SXPLOT - X Window plot a triangulated sloth function s(x1,x2)		",
"                                                                       ",
" sxplot <modelfile [optional parameters]				",
"                                                                       ",
" Optional Parameters:							",
" edgecolor=cyan         color to draw fixed edges			",
" tricolor=yellow        color to draw non-fixed edges of triangles	",
"	 =none		non-fixed edges of triangles are not shown      ",
" bclip=minimum sloth    sloth value corresponding to black		",
" wclip=maximum sloth    sloth value corresponding to white		",
" x1beg=x1min            value at which x1 axis begins			",
" x1end=x1max            value at which x1 axis ends			",
" x2beg=x2min            value at which x2 axis begins			",
" x2end=x2max            value at which x2 axis ends			",
" cmap=gray              gray, hue, or default colormaps may be specified",
"									",
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
"                       seismic (axis 1 vertical, axis 2 horizontal)	",
"									",
NULL};

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 05/17/91
 */
/**************** end self doc ***********************************/


/* client data structures for callbacks */
typedef struct ExposeCDStruct {
	Model *model;
	char *edgecolor;
	char *tricolor;
	float bclip;
	float wclip;
} ExposeCD;

/* callback functions */
void resizeCB (Widget w, 
	char * clientdata,
	XtcwpAxesCallbackStruct *calldata);
void exposeCB (Widget w, 
	ExposeCD *clientdata,
	XtcwpAxesCallbackStruct *calldata);
void inputCB (Widget w, 
	char * clientdata,
	XtcwpAxesCallbackStruct *calldata);

/* functions defined and used internally */
static void minmax (Model *m, float *smin, float *smax);
static void drawimage (Display *dpy, Drawable dbl, Region region,
	FGC fgc, Model *m, float fmin, float fmax, int style);

int main (int argc, char **argv)
{
	int nTic1,nTic2,width,height;
	float smin,smax,bclip,wclip,x1beg,x1end,x2beg,x2end;
	char *edgecolor="cyan",*tricolor="yellow",*cmap="gray",
		*label1="",*label2="",*title="",
		*labelFont="",*titleFont="",
		*axesColor="",*gridColor="",*titleColor="",
		*style="seismic",*grid1="none",*grid2="none";
	Model *model;
	XrmValue from,to;
	ExposeCD exposeCD;
	Widget toplevel,axes;
	Display *dpy;
	Window win;
	Arg args[100];
	int nargs;
	float bhue=0,whue=240,sat=1,bright=1;

	/* initialize getpar */
	initargs(argc,argv);
        requestdoc(0);

	
	/* read model */
	model = readModel(stdin);
	
	/* determine minimum and maximum s(x,y) */
	minmax(model,&smin,&smax);
	
	/* get optional parameters */
	getparstring("edgecolor",&edgecolor);
	getparstring("tricolor",&tricolor);
	bclip = smin; getparfloat("bclip",&bclip);
	wclip = smax; getparfloat("wclip",&wclip);
        getparstring("cmap",&cmap);   

	/* initialize toolkit intrinsics and set toplevel parameters */
	toplevel = XtInitialize(argv[0],"Sxplot",NULL,0,&argc,argv);
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
	XtSetValues(axes,args,nargs);
	x1beg = model->xmin; getparfloat("x1beg",&x1beg);
	x1end = model->xmax; getparfloat("x1end",&x1end);
	x2beg = model->ymin; getparfloat("x2beg",&x2beg);
	x2end = model->ymax; getparfloat("x2end",&x2end);
	XtcwpSetAxesValues(axes,x1beg,x1end,x2beg,x2end);

	/* add callbacks to axes widget */
	XtAddCallback(axes,XtNresizeCallback,(XtCallbackProc) resizeCB,NULL);
	exposeCD.model = model;
	exposeCD.edgecolor = edgecolor;
	exposeCD.tricolor = tricolor;
	exposeCD.bclip = bclip;
	exposeCD.wclip = wclip;
	XtAddCallback(axes,XtNexposeCallback,(XtCallbackProc) exposeCB,&exposeCD);
	XtAddCallback(axes,XtNinputCallback,(XtCallbackProc) inputCB,NULL);

	/* realize widgets */
	XtRealizeWidget(toplevel);

	/* if necessary, create private colormap */
	dpy = XtDisplay(toplevel);
	win = XtWindow(toplevel);

	if (STREQ(cmap,"gray")) {
	  XSetWindowColormap(dpy,win,XtcwpCreateGrayColormap(dpy,win));
	} else if (STREQ(cmap,"hue")) {
	  XSetWindowColormap(dpy,win,XtcwpCreateHueColormap(dpy,win,
														bhue,whue,sat,bright)); /* see Note below */
	}
	
	/* go */
	XtMainLoop();

	return EXIT_SUCCESS;
}

void resizeCB (Widget w, 
	char * clientdata,
	XtcwpAxesCallbackStruct *ca)
{
	/* printf("resize callback\n"); */
	if(ca-ca) resizeCB(w,clientdata,ca);
}

void exposeCB (Widget w, 
	ExposeCD *cd,
	XtcwpAxesCallbackStruct *ca)
{
	Model *model=cd->model;
	char *edgecolor=cd->edgecolor;
	char *tricolor=cd->tricolor;
	float bclip=cd->bclip;
	float wclip=cd->wclip;
	Region region=ca->region;
	Position x=ca->x,y=ca->y;
	Dimension width=ca->width,height=ca->height;
	float x1beg=ca->x1beg,x1end=ca->x1end,
		x2beg=ca->x2beg,x2end=ca->x2end;
	int style=ca->style;
	Display *dpy=NULL;
	Window win;
	Region drawregion,tempregion;
	XWindowAttributes wa;
	Colormap cmap;
	XColor scolor,ecolor;
	XRectangle rect;
	long black=0;
	long white=0;
	GC gcedge,gctri;
	FGC fgcedge,fgctri,fgc;
	float x1,y1,x2,y2,x3,y3;
	Face *f;
	
	/* JG */
	dpy=XtDisplay(w);
	win=XtWindow(w);
	black=(long) BlackPixelOfScreen(XtScreen(w));
	white=(long) WhitePixelOfScreen(XtScreen(w));
	/* .. JG */

	/* determine current colormap */
	XGetWindowAttributes(dpy,win,&wa);
	cmap = wa.colormap;

	/* create GCs */
	gcedge = XCreateGC(dpy,win,0L,NULL);
	gctri = XCreateGC(dpy,win,0L,NULL);


	/* set line colors */
	if (XAllocNamedColor(dpy,cmap,edgecolor,&scolor,&ecolor))
		XSetForeground(dpy,gcedge,ecolor.pixel);
	else
		XSetForeground(dpy,gcedge,black);
	if (XAllocNamedColor(dpy,cmap,tricolor,&scolor,&ecolor))
		XSetForeground(dpy,gctri,ecolor.pixel);
	else
		XSetForeground(dpy,gctri,white);
	
	/* clip to intersection of axes rectangle and expose region */
	rect.x = x;  rect.y = y;  rect.width = width;  rect.height = height;
	tempregion = XCreateRegion();
	XUnionRectWithRegion(&rect,tempregion,tempregion);
	drawregion = XCreateRegion();
	XIntersectRegion(region,tempregion,drawregion);
	XSetRegion(dpy,gcedge,drawregion);
	XSetRegion(dpy,gctri,drawregion);
	
	/* create FGCs with clipping */
	if (style==XtcwpNORMAL) {
		fgcedge = FXCreateFGC(gcedge,x,y,width,height,
			x1beg,x2end,x1end-x1beg,x2beg-x2end);
		FSetClipRectangle(fgcedge,x1beg,x2end,x1end,x2beg);
		fgctri = FXCreateFGC(gctri,x,y,width,height,
			x1beg,x2end,x1end-x1beg,x2beg-x2end);
		FSetClipRectangle(fgctri,x1beg,x2end,x1end,x2beg);
	} else {
		fgcedge = FXCreateFGC(gcedge,x,y,width,height,
			x2beg,x1beg,x2end-x2beg,x1end-x1beg);
		FSetClipRectangle(fgcedge,x2beg,x1beg,x2end,x1end);
		fgctri = FXCreateFGC(gctri,x,y,width,height,
			x2beg,x1beg,x2end-x2beg,x1end-x1beg);
		FSetClipRectangle(fgctri,x2beg,x1beg,x2end,x1end);
	}
	
	/* draw image */
	drawimage(dpy,win,drawregion,fgctri,model,bclip,wclip,style);
	
	/* loop over triangles */
	f = model->f;
	do {
		/* get float coordinates of vertices */
		if (style==XtcwpNORMAL) {
			x1 = f->eu->vu->v->x;
			y1 = f->eu->vu->v->y;
			x2 = f->eu->euCW->vu->v->x;
			y2 = f->eu->euCW->vu->v->y;
			x3 = f->eu->euCCW->vu->v->x;
			y3 = f->eu->euCCW->vu->v->y;
		} else {
			x1 = f->eu->vu->v->y;
			y1 = f->eu->vu->v->x;
			x2 = f->eu->euCW->vu->v->y;
			y2 = f->eu->euCW->vu->v->x;
			x3 = f->eu->euCCW->vu->v->y;
			y3 = f->eu->euCCW->vu->v->x;
		}
		
		/* draw edges of triangle */
		fgc = f->eu->e->fixed ? fgcedge : fgctri;
		if (((fgc==fgcedge && !STREQ(edgecolor,"none")) ||
		     (fgc==fgctri && !STREQ(tricolor,"none"))))
			FXDrawLine(dpy,win,fgc,x1,y1,x2,y2);
		fgc = f->eu->euCW->e->fixed ? fgcedge : fgctri;
		if (((fgc==fgcedge && !STREQ(edgecolor,"none")) ||
		     (fgc==fgctri && !STREQ(tricolor,"none"))))
			FXDrawLine(dpy,win,fgc,x2,y2,x3,y3);
		fgc = f->eu->euCCW->e->fixed ? fgcedge : fgctri;
		if (((fgc==fgcedge && !STREQ(edgecolor,"none")) ||
		     (fgc==fgctri && !STREQ(tricolor,"none"))))
			FXDrawLine(dpy,win,fgc,x3,y3,x1,y1);
		f = f->fNext;
	} while (f!=model->f);

	/* free everything */
	FXFreeFGC(fgcedge);
	FXFreeFGC(fgctri);
	XFreeGC(dpy,gcedge);
	XFreeGC(dpy,gctri);
	XDestroyRegion(drawregion);
	XDestroyRegion(tempregion);
}

void inputCB (Widget w, 
	char * clientdata,
	XtcwpAxesCallbackStruct *ca)
{
	int x=ca->x,y=ca->y,width=ca->width,height=ca->height;
	float x1beg=ca->x1beg,x1end=ca->x1end,x2beg=ca->x2beg,x2end=ca->x2end;
	int style=ca->style;
	XEvent *event=ca->event;
	int xb,yb,wb,hb;
	float x1begn,x1endn,x2begn,x2endn;
	static int firstinput=1;
	static float x1begs,x1ends,x2begs,x2ends;

	/* if first input, save initial axes limits */
	if (firstinput) {
		x1begs = x1beg; 
		x1ends = x1end; 
		x2begs = x2beg;
		x2ends = x2end;
		firstinput = 0;
		clientdata += 0; /* dummy */
	}

	if (event->type==ButtonPress && event->xbutton.button==Button3)
	  { 
		/* JG: requires extra fields in Axes.c's defaultTranslations[] to make this work. Harmless if not */
		exit(0);
	  }
	/* track pointer and get rubber box */
	XtcwpRubberBox(XtDisplay(w),XtWindow(w),*event,&xb,&yb,&wb,&hb);

	/* if new box has zero width or height */
	if (wb==0 || hb==0) {

		/* restore initial limits */
		XtcwpSetAxesValues(w,x1begs,x1ends,x2begs,x2ends);
	
	/* else if non-zero zoom box */
	} else {
	
		/* clip box */
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
	
		/* determine axes limits */
		if (style==XtcwpNORMAL) {
			x1begn = x1beg+(xb-x)*(x1end-x1beg)/width;
			x1endn = x1beg+(xb+wb-x)*(x1end-x1beg)/width;
			x2begn = x2end+(yb+hb-y)*(x2beg-x2end)/height;
			x2endn = x2end+(yb-y)*(x2beg-x2end)/height;
		} else {
			x1endn = x1beg+(yb+hb-y)*(x1end-x1beg)/height;
			x1begn = x1beg+(yb-y)*(x1end-x1beg)/height;
			x2begn = x2beg+(xb-x)*(x2end-x2beg)/width;
			x2endn = x2beg+(xb+wb-x)*(x2end-x2beg)/width;
		}
	
		/* set axes limits */
		XtcwpSetAxesValues(w,x1begn,x1endn,x2begn,x2endn);
	}
	
	/* force an expose event */
	XClearArea(XtDisplay(w),XtWindow(w),0,0,0,0,True);
}

/* determine minimum and maximum s(x,z) in model */
static void minmax (Model *m, float *smin, float *smax)
{
	float s00,dsdx,dsdz,x,z,sxz;
	Face *f;
	FaceAttributes *fa;
	EdgeUse *eu;
	Vertex *v;
	
	/* initialize min and max values */
	*smin = FLT_MAX;
	*smax = -FLT_MAX;
	
	/* loop over faces */
	f = m->f;
	do {
		/* if face attributes exist */
		if ((fa=f->fa)!=NULL) {

			/* get face attributes */
			s00 = fa->s00;
			dsdx = fa->dsdx;
			dsdz = fa->dsdz;
			
			/* loop over vertexes */
			eu = f->eu;
			do {
				v = eu->vu->v;
				x = v->y;  z = v->x;
				sxz = s00+x*dsdx+z*dsdz;
				if (sxz<*smin) *smin = sxz;
				if (sxz>*smax) *smax = sxz;
				eu = eu->euCW;
			} while (eu!=f->eu);
		}
		
		/* next face */
		f = f->fNext;
		
	} while (f!=m->f);
}

static void drawimage (Display *dpy, Drawable dbl, Region region,
					   FGC fgc, Model *m, float fmin, float fmax, int style)
{
  int scr=-1;
  int x,y,width,height;
  int i,j,k,line,iline,jline,widthpad;
  unsigned long pmin,pmax,p;

  float fx,fy,s,base,scale;
  Tri *t=NULL;
  TriAttributes *ta;
  XRectangle rect;
  XImage *image=NULL;
  int bitmap_pad=0;
  int nbpr=0;


	 

#if 0 /* OLD VERSION . See JG fix below */ 
  unsigned char *data=NULL;

  scr=DefaultScreen(dpy);

  /* Kludge to fix problem with XCreateImage introduced in */
  /* Xorg 7.0 update for security */
 
  if (BitmapPad(dpy)>16) {
	bitmap_pad = 16;
  } else if (BitmapPad(dpy) < 16) {
	bitmap_pad = 8;
  }


  /* determine smallest box enclosing region */
  XClipBox(region,&rect);
  x = rect.x;  y = rect.y;  width = rect.width;  height = rect.height;
  if (width==0 || height==0) return;

  /* allocate memory for image data */
  widthpad = (1+(width-1)/bitmap_pad)*bitmap_pad;
  nbpr = widthpad-1;

  data = alloc1(widthpad*height,sizeof(unsigned char));
  if (data==NULL) err("width,widthpad,height = %d %d %d",
					  width,widthpad,height);

  warn("nbpr = %d  widthpad = %d height = %d bitmap_pad = %d ",
	   nbpr,widthpad,height,bitmap_pad);
  /* determine min and max pixels from standard colormap */
  pmin = XtcwpGetFirstPixel(dpy);  
  pmax = XtcwpGetLastPixel(dpy);

  /* determine base and scale factor */
  scale = (fmax!=fmin) ? ((float) (pmax-pmin))/(fmax-fmin) : 0.0;
  base = ((float) pmin)-fmin*scale;
	
  /* loop over scan lines */
  for (line=0; line<height; line++) {
	iline = line*width;
	jline = line*widthpad;
		
	/* loop over pixels in scan line */
	for (i=iline,j=jline,k=0; k<width; ++i,++j,++k) {
		
	  /* determine float x and y coordinates */
	  if (style==XtcwpNORMAL) {
		fx = MapX(fgc,x+k);
		fy = MapY(fgc,y+line);
	  } else {
		fx = MapY(fgc,y+line);
		fy = MapX(fgc,x+k);
	  }
			
	  /* determine sloth */
	  t = insideTriInModel(m,t,fx,fy);
	  ta = (TriAttributes*)t->fa;
	  s = ta->s00+fy*ta->dsdx+fx*ta->dsdz;
			
	  /* convert to pixel and put in image */
	  p = (unsigned long) (base+s*scale);
	  if (p<pmin) p = pmin;
	  if (p>pmax) p = pmax;
	  data[j] = (unsigned char) p;
	}
	for (j=jline+width,k=width; k<widthpad; ++j,++k)
	  data[j] = data[jline+width-1];
  }
  
	
  /* create, put, and destroy image */
  image = XCreateImage(	(Display *) dpy,
						(Visual *) DefaultVisual(dpy,scr),
						(unsigned int) DefaultDepth(dpy,scr),
						(int)ZPixmap,
						(int) 0,
						(char*)data,
						(unsigned int) widthpad,
						(unsigned int) height,
						(int) bitmap_pad, 
						(int) nbpr);

#else
  char *data=NULL;
  char noCmap;

  scr=DefaultScreen(dpy);

  /* JG: get bitmap_pad from X */ 
  bitmap_pad = BitmapPad(dpy);
	
  /* determine smallest box enclosing region */
  XClipBox(region,&rect);
  x = rect.x;  y = rect.y;  width = rect.width;  height = rect.height;
  if (width==0 || height==0) return;

  /* allocate memory for image data */
  widthpad = (1+(width-1)/bitmap_pad)*bitmap_pad;
  nbpr = widthpad-1;

  /* create image & determine alloc size from X */
  image = XCreateImage(	(Display *) dpy,
						(Visual *) DefaultVisual(dpy,scr),
						(unsigned int) DefaultDepth(dpy,scr),
						(int)ZPixmap,
						(int) 0,
						NULL ,
						(unsigned int) widthpad,
						(unsigned int) height,
						(int) bitmap_pad, 
						0);
  /* JG XCreateImage(....,0)  gets X to compute the size it needs. Then we alloc. */
  image->data = (char *)calloc(image->bytes_per_line, image->height);
  if (image->data==NULL) err("width,widthpad,height = %d %d %d",
					  width,widthpad,height);

  /* warn("nbpr = %d  widthpad = %d height = %d bitmap_pad = %d ", nbpr,widthpad,height,bitmap_pad); */
  /* determine min and max pixels from standard colormap */
  pmin = XtcwpGetFirstPixel(dpy);  
  pmax = XtcwpGetLastPixel(dpy);
  /* JGHACK ... When colormap fails, we get pmax=pmin=0 */
  noCmap = (pmax==0 && pmin==0) ? 1:0;
  if (noCmap) 
	{
	  pmax = 255; 
	  warn("No colormap found....");
	}
  /* ...JGHACK */

  /* determine base and scale factor */
  scale = (fmax!=fmin) ? ((float) (pmax-pmin))/(fmax-fmin) : 0.0;
  base = ((float) pmin)-fmin*scale;
	

  data = (char *)image->data ;
  /* loop over scan lines */
  for (line=0; line<height; line++) {
	iline = line*width;
	jline = line*widthpad;
		
	/* loop over pixels in scan line */
	for (i=iline,j=jline,k=0; k<width; ++i,++j,++k) {
		
	  /* determine float x and y coordinates */
	  if (style==XtcwpNORMAL) {
		fx = MapX(fgc,x+k);
		fy = MapY(fgc,y+line);
	  } else {
		fx = MapY(fgc,y+line);
		fy = MapX(fgc,x+k);
	  }
			
	  /* determine sloth */
	  t = insideTriInModel(m,t,fx,fy);
	  ta = (TriAttributes*)t->fa;
	  s = ta->s00+fy*ta->dsdx+fx*ta->dsdz;
			
	  /* convert to pixel and put in image */
	  p = (unsigned long) (base+s*scale);
	  if (p<pmin) p = pmin;
	  if (p>pmax) p = pmax;

	  if (noCmap) 
		{
		  /* JG. Can't get colormap. Might as well write RGB pixels as grayscale */
		  XPutPixel(image,k,line, p | (p<<8)| (p<<16));
		}
	  else 
		{
		  /* original */
		  /* data[j] = (unsigned char) p; */
		  XPutPixel(image,k,line,p);
		}
	}
	/* original. Not sure this is needed JG */
	/*
	  for (j=jline+width,k=width; k<widthpad; ++j,++k) data[j] = data[jline+width-1]; 
	*/
  }
	

#endif

  XPutImage(dpy,dbl,fgc->gc,image,0,0,x,y,image->width,image->height);

  /* free(data); */
  XDestroyImage(image);
}
