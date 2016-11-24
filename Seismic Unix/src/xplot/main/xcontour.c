/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 2003.*/
/* All rights reserved.                       */

/* XCONTOUR: $Revision: 1.10 $ ; $Date: 2011/11/21 17:03:51 $	*/

#include "xplot.h"
#include <X11/Xatom.h>
#include <X11/keysym.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" XCONTOUR - X CONTOUR plot of f(x1,x2) via vector plot call		",
" 									",
" xcontour n1= [optional parameters] <binaryfile [>psplotfile]		",
" 									",
" X Functionality:							",
" Button 1	Zoom with rubberband box				",
" Button 2	Show mouse (x1,x2) coordinates while pressed		",
" q or Q key	Quit 							",
" s key		Save current mouse (x1,x2) location to file		",
" p or P key	Plot current window with pswigb	(only from disk files)	",
" 									",
" Required Parameters:							",
" n1                     number of samples in 1st (fast) dimension	",
" 									",
" Optional Parameters:							",
" d1=1.0                 sampling interval in 1st dimension		",
" f1=0.0                 first sample in 1st dimension			",
" x1=f1,f1+d1,...        array of sampled values in 1nd dimension	",
" n2=all                 number of samples in 2nd (slow) dimension	",
" d2=1.0                 sampling interval in 2nd dimension		",
" f2=0.0                 first sample in 2nd dimension			",
" x2=f2,f2+d2,...        array of sampled values in 2nd dimension	",
" mpicks=/dev/tty        file to save mouse picks in			",
" verbose=1              =1 for info printed on stderr (0 for no info)	",
" nc=5                   number of contour values                       ",
" dc=(zmax-zmin)/nc      contour interval                               ",
" fc=min+dc              first contour                                  ",
" c=fc,fc+dc,...         array of contour values                        ",
" cwidth=1.0,...         array of contour line widths                   ",
" ccolor=none,...        array of contour colors; none means use cgray  ",
" cdash=0.0,...          array of dash spacings (0.0 for solid)         ",
" labelcf=1              first labeled contour (1,2,3,...)              ",
" labelcper=1            label every labelcper-th contour               ",
" nlabelc=nc             number of labeled contours (0 no contour label)",
" nplaces=6              number of decimal places in contour labeling	",
" xbox=50                x in pixels of upper left corner of window	",
" ybox=50                y in pixels of upper left corner of window	",
" wbox=550               width in pixels of window			",
" hbox=700               height in pixels of window			",
" x1beg=x1min            value at which axis 1 begins			",
" x1end=x1max            value at which axis 1 ends			",
" d1num=0.0              numbered tic interval on axis 1 (0.0 for automatic)",
" f1num=x1min            first numbered tic on axis 1 (used if d1num not 0.0)",
" n1tic=1                number of tics per numbered tic on axis 1	",
" grid1=none             grid lines on axis 1 - none, dot, dash, or solid",
" x2beg=x2min            value at which axis 2 begins			",
" x2end=x2max            value at which axis 2 ends			",
" d2num=0.0              numbered tic interval on axis 2 (0.0 for automatic)",
" f2num=x2min            first numbered tic on axis 2 (used if d2num not 0.0)",
" n2tic=1                number of tics per numbered tic on axis 2	",
" grid2=none             grid lines on axis 2 - none, dot, dash, or solid",
" label2=                label on axis 2				",
" labelfont=Erg14        font name for axes labels			",
" title=                 title of plot					",
" titlefont=Rom22        font name for title				",
" windowtitle=xwigb      title on window				",
" labelcolor=blue        color for axes labels				",
" titlecolor=red         color for title				",
" gridcolor=blue         color for grid lines				",
" labelccolor=black      color of contour labels                        ",   
" labelcfont=fixed       font name for contour labels                   ",
" style=seismic		 normal (axis 1 horizontal, axis 2 vertical) or	",
"			 seismic (axis 1 vertical, axis 2 horizontal)	",
"                                                                       ",
"									",
" Notes:								",
" For some reason the contour might slight differ from ones generated   ",
" by pscontour (propably due to the pixel nature of the plot            ",
" coordinates)                                                          ",
"                                                                       ",
" The line width of unlabeled contours is designed as a quarter of that	",
" of labeled contours. 							",
" 									",
NULL};
/*
 * AUTHOR: Morten Wendell Pedersen, Aarhus University 
 *
 * All the coding is based on snippets taken from xwigb, ximage and pscontour
 * All I have done is put the parts together and put in some bugs ;-)
 *
 * So credits should go to the authors of these packages... 
 *
 * Caveats and Notes:
 * The code has been developed under Linux 1.3.20/Xfree 3.1.2E (X11 6.1)
 * with gcc-2.7.0 But hopefully it should work on other platforms as well
 *
 * Since all the contours are drawn by Vector plot call's everytime the
 * Window is exposed, the exposing can be darn slow 
 * OOPS This should be history... Now I keep my window content with backing
 * store so I won't have to redraw my window unless I really have to...
 *
 * Portability Question: I guess I should check if the display supports
 * backingstore and redraw if it doesn't (see DoesBackingStore(3) )
 * I have to be able to use CWBackingStore==Always (other values can be
 * NonUseful and WhenMapped
 *
 * Since I put the contour labels everytime I draw one contour level the area 
 * that contains the label will be crossed by the the next contour lines...
 * (this bug also seems to be present in pscontour)
 * To fix this I have to redraw all the labels after been through all
 * the contour calls
 * Right now I can't see a way to fix this without actually to through
 * the entire label positioning again....Overkill I would say
 *
 * 
 * The relative short length of the contour segments will propably mask the
 * cdash settings
 * which means it is disposable (but I know how to draw dashed lines :)
 * A way of fixing this could be to get all connected point and then use
 * XDrawlines or XDrawSegments... just an idea...No idea if it'll work. 
 *
 * I think there is a bug in xContour since my plot coordinates increase
 * North and west ward instead of south and eastward
 *
 *   I need to check the Self Doc so if the right parameters are described
 *   (I have been through it a couple of times but....)
 *
 *   All functions need a heavy cleanup for unused variables
 *   I suppose there is a couple of memory leaks due to missing free'ing of
 * numerous pointers (especially fonts,GC's & colors could be a problem...
 *
 *   I have to browse through the internal pscontour call... basically what
 * I have done is just putting pscontour instead of pswigb... Instead of
 * repositioning the input file  pointer (which doesnt work with pipes) one
 * should consider the use of temporary file
 *   or write your zoombox to pscontour (...how one does that?)
 *
 *  Wish List:
 *   The use of cgray's unused until now... I guess I'll need to allocate
 * a gray Colormap  -> meaning that the code not will run at other display
 * than 8 bit Pseudocolor :( (with the use of present version of the colormap
 * library (code in $CWPROOT/src/xplot/lib ) )
 *
 *  The format of contour label should be open for the user.. 
 *  
 *  It could be nice if one could choose to have a pixmap (like ximage )
 * underlying  the contours... this should be defined either by the input
 * data  or by a seperate file
 *  eg useful for viewing traveltime contours on top a plot of the velocity
 * field
 */
/************************ end self doc ********************************/


/* functions defined and used internally */
static void zoomBox (int x, int y, int w, int h, 
	int xb, int yb, int wb, int hb,
	int nx, int ix, float x1, float x2,
	int ny, int iy, float y1, float y2,
	int *nxb, int *ixb, float *x1b, float *x2b,
	int *nyb, int *iyb, float *y1b, float *y2b);
static void newContour (Display *dpy, Window win,GC gci,
	int xbase, int ybase, int n1, float x1[], int n2, float x2[], float *z,
	int nc, float c[],float cwidth[], char *ccolor[],float cdash[],
	int labelcf,int labelcper,int nlabelc,char *labelcfont,
	char *labelccolor, int nplaces);

void xMouseLoc(Display *dpy, Window win, XEvent event, int style, Bool show,
	int x, int y, int width, int height,
	float x1begb, float x1endb, float x2begb, float x2endb,
	float p2beg, float p2end);
void xMousePrint(XEvent event, int style, FILE *mpicksfp,
		 int x, int y, int width, int height,
	float x1begb, float x1endb, float x2begb, float x2endb,
	float p2beg, float p2end);

#define NCMAX 200

int
main (int argc,char **argv)
{
	int n1,n2,nc,n1tic,n2tic,nplaces,
		i1,i2,iz,ic,npar,grid1,grid2,style,
	        i1beg,i2beg,i1end,i2end,
	        i1min,i2min,i1max,i2max,i1step,i2step,i1c,i2c,
		verbose,
		xbox,ybox,wbox,hbox,
		xb,yb,wb,hb,
		x,y,width,height,
		i,j,nx,ny,nxb,nyb,ixb,iyb,n1c,n2c,
		imageOutOfDate,winwidth=-1,winheight=-1,
		showloc=0;
	unsigned long nfloats;
	float labelsize,titlesize,cwidth[NCMAX],cgray[NCMAX],cdash[NCMAX],
		d1,f1,d2,f2,dc,fc,*z,zmin,zmax,*x1,*x2,c[NCMAX],
	        *ix,*iy,
		x1beg,x1end,x2beg,x2end,
		x1min,x1max,x2min,x2max,
		d1num,f1num,d2num,f2num,
		x1begb,x1endb,x2begb,x2endb,p2beg=0.0,p2end=0.0;

	float *cz,*czp,*czb,*czbp;
	float labelcsize;
	int labelcf, nlabelc, labelcper;
	char *label1="",*label2="",*title="",*windowtitle="xcontour",
		*labelfont="Erg14",*titlefont="Rom22",
	        *labelcfont="fixed",*labelccolor="black",
		*styles="seismic",*grid1s="none",*grid2s="none",
		*labelcolor="blue",*titlecolor="red",
		*gridcolor="blue",*scolor="none",*ccolor[NCMAX],
	        keybuf[256],*mpicks;
	FILE *infp=stdin, *mpicksfp;
	Display *dpy;
	Window win;
	XEvent event;
	KeySym keysym;
	XComposeStatus keystat;
	GC gci;
        XSetWindowAttributes set_window_attributes;
	int scr;
	unsigned long black,white;

	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters describing 1st dimension sampling */
	if ((n1=countparval("x1"))==0 && !getparint("n1",&n1))
		err("Must specify n1!\n");
	x1 = ealloc1float(n1);
	if (!getparfloat("x1",x1)) {
		d1 = 1.0;  getparfloat("d1",&d1);
		f1 = d1;  getparfloat("f1",&f1);
		for (i1=0; i1<n1; i1++)
			x1[i1] = f1+i1*d1;
	}
	for (i1=1,x1min=x1max=x1[0]; i1<n1; i1++) {
		x1min = MIN(x1min,x1[i1]);
		x1max = MAX(x1max,x1[i1]);
	}

	/* get parameters describing 2nd dimension sampling */
	if ((n2=countparval("x2"))==0 && !getparint("n2",&n2)) {
			if (efseeko(infp,(off_t) 0,SEEK_END)!=0)
				err("must specify n2 if in a pipe!\n");
			nfloats =(int) (eftello(infp)/((off_t) sizeof(float)));
			efseeko(infp,(off_t) 0,SEEK_SET);
			n2 = (int) (nfloats/n1);
	}
	x2 = ealloc1float(n2);
	if (!getparfloat("x2",x2)) {
		d2 = 1.0;  getparfloat("d2",&d2);
		f2 = d2;  getparfloat("f2",&f2);
		for (i2=0; i2<n2; i2++)
			x2[i2] = f2+i2*d2;
	}
	for (i2=1,x2min=x2max=x2[0]; i2<n2; i2++) {
		x2min = MIN(x2min,x2[i2]);
		x2max = MAX(x2max,x2[i2]);
	}
	/* set up file to save mouse picks */
	if (!getparstring("mpicks", &mpicks)) mpicks = "/dev/tty";
		mpicksfp = efopen(mpicks, "w");

	/* read binary data to be contoured */
	z = ealloc1float(n1*n2);
	if (fread(z,sizeof(float),n1*n2,infp)!=n1*n2)
		err("error reading input file!\n");

	
	/* determine data min and max */
	/* Is this a bug... Shouldn't we only examine
	   the maximum plot area (defined by x1beg x1end...)*/
	for (i2=0,zmin=zmax=z[0]; i2<n2; i2++) {
		for (i1=0,iz=i2*n1; i1<n1; i1++,iz++) {
			zmin = MIN(zmin,z[iz]);
			zmax = MAX(zmax,z[iz]);
		}
	}

	/* get contouring parameters */
	if ((nc=getparfloat("c",c))==0) {
		nc = 5;  getparint("nc",&nc);
		dc = (zmax-zmin)/(nc);  getparfloat("dc",&dc);
		fc = zmin+dc;  getparfloat("fc",&fc);
		for (ic=0; ic<nc; ic++)
			c[ic] = fc+ic*dc;
	}
	for (ic=0; ic<nc; ic++) {
		cwidth[ic] = 1.0;
		cgray[ic] = 0.0;
		cdash[ic] = 0.0;
		ccolor[ic] = scolor;
	}
	if ((npar=getparfloat("cwidth",cwidth))!=0)
		for (ic=npar; ic<nc; ic++)
			cwidth[ic] = cwidth[npar-1];
	if ((npar=getparfloat("cgray",cgray))!=0)
		for (ic=npar; ic<nc; ic++)
			cgray[ic] = cgray[npar-1];
	if ((npar=getparfloat("cdash",cdash))!=0)
		for (ic=npar; ic<nc; ic++)
			cdash[ic] = cdash[npar-1];
	if (getparstring("ccolor",&scolor)) {
		int i,j;  char *s;
		for (i=0,s=strtok(scolor,","); s!=NULL; ++i,s=strtok(NULL,","))
			ccolor[i] = s;
		for (j=i-1; i<nc; ++i)
			ccolor[i] = ccolor[j];
	}
 	labelcf = 1; getparint("labelcf",&labelcf);
	labelcper = 1; getparint("labelcper",&labelcper);
 	nlabelc = nc; getparint("nlabelc",&nlabelc);
  	labelcsize = 6; getparfloat("labelcsize",&labelcsize);
	getparstring("labelcfont",&labelcfont);
	getparstring("labelccolor",&labelccolor);
	verbose = 1;  getparint("verbose",&verbose);
	if (!getparint("nplaces",&nplaces))	nplaces=6;
	

	/* get axes parameters */
	xbox = 50; getparint("xbox",&xbox);
	ybox = 50; getparint("ybox",&ybox);
	wbox = 550; getparint("wbox",&wbox);
	hbox = 700; getparint("hbox",&hbox);
	x1beg = x1min; getparfloat("x1beg",&x1beg);
	x1end = x1max; getparfloat("x1end",&x1end);
	d1num = 0.0; getparfloat("d1num",&d1num);
	f1num = x1min; getparfloat("f1num",&f1num);
	n1tic = 1; getparint("n1tic",&n1tic);
	getparstring("grid1",&grid1s);
	if (STREQ("dot",grid1s)) grid1 = DOT;
	else if (STREQ("dash",grid1s)) grid1 = DASH;
	else if (STREQ("solid",grid1s)) grid1 = SOLID;
	else grid1 = NONE;
	getparstring("label1",&label1);
	x2beg = x2min; getparfloat("x2beg",&x2beg);
	x2end = x2max; getparfloat("x2end",&x2end);
	d2num = 0.0; getparfloat("d2num",&d2num);
	f2num = 0.0; getparfloat("f2num",&f2num);
	n2tic = 1; getparint("n2tic",&n2tic);
	getparstring("grid2",&grid2s);
	if (STREQ("dot",grid2s)) grid2 = DOT;
	else if (STREQ("dash",grid2s)) grid2 = DASH;
	else if (STREQ("solid",grid2s)) grid2 = SOLID;
	else grid2 = NONE;
	getparstring("label2",&label2);
	getparstring("labelfont",&labelfont);
	labelsize = 18.0; getparfloat("labelsize",&labelsize);
	getparstring("title",&title);
	getparstring("titlefont",&titlefont);
	titlesize = 24.0; getparfloat("titlesize",&titlesize);
	getparstring("style",&styles);
	if (STREQ("normal",styles)) style = NORMAL;
	  else style = SEISMIC;
	/* else err("Sorry, only style=seismic is currently available!");*/
	getparstring("titlecolor",&titlecolor);
	getparstring("labelcolor",&labelcolor);
	getparstring("gridcolor",&gridcolor);
	getparstring("windowtitle",&windowtitle);

	/* adjust x1beg and x1end to fall on sampled values */
	i1max=0;
	i1min=n1;
	for (i1=0,n1c=0; i1<n1; i1++)
	  if (x1[i1]>=MIN(x1beg,x1end) && x1[i1]<=MAX(x1beg,x1end)) {
	    i1min=MIN(i1,i1min);
	    i1max=MAX(i1,i1max);
	    n1c++;
	  }
	if(x1beg<=x1end) {
	  i1beg=i1min;
	  i1end=i1max;
	    }else{
	  i1beg=i1max;
	  i1end=i1min;
	    }
	x1beg=x1[i1beg];
	x1end=x1[i1end];

	/* adjust x2beg and x2end to fall on sampled values */
	i2max=0;
	i2min=n2;
	for (i2=0,n2c=0; i2<n2; i2++)
	  if (x2[i2]>=MIN(x2beg,x2end) && x2[i2]<=MAX(x2beg,x2end)) {
	    i2min=MIN(i2,i2min);
	    i2max=MAX(i2,i2max);
	    n2c++;
	  }
	if(x2beg<=x2end) {
	  i2beg=i2min;
	  i2end=i2max;
	    }else{
	  i2beg=i2max;
	  i2end=i2min;
	    }
	x2beg=x2[i2beg];
	x2end=x2[i2end];

	cz = ealloc1float(n1c*n2c);
	/* map the data after style parameter */
	i1step = (i1end>i1beg)?1:-1;
	i2step = (i2end>i2beg)?1:-1;
	if (style==NORMAL) {
	  for (i2c=0,i2=i2beg; i2c<n2c; i2c++,i2+=i2step) {
	    czp = cz+n1c*n2c-(i2c+1)*n1c;
	    for (i1c=0,i1=i1beg; i1c<n1c; i1c++,i1+=i1step) 
				*czp++ = z[i1+i2*n1];
	  }
	} else {
	  czp = cz;
	  for (i1c=0,i1=i1beg; i1c<n1c; i1c++,i1+=i1step) {
	    for (i2c=0,i2=i2beg; i2c<n2c; i2c++,i2+=i2step) 
	      *czp++ = z[i1+i2*n1];
	  }
	}
	free1float(z);

	/* initialize zoom box parameters */
	nxb = nx = (style==NORMAL ? n1c : n2c);
	nyb = ny = (style==NORMAL ? n2c : n1c);
	ixb = iyb = 0;
	czb = cz;
	x1begb = x1beg;  x1endb = x1end;
	x2begb = x2beg;  x2endb = x2end;
	/* Generate a set of suitable plot coordinates 
	   (the actual scaling to X-windows coordinates will be done later*/
	ix=ealloc1float(nx);
	iy=ealloc1float(ny);
	i1step = (i1end>i1beg)?1:-1;
	i2step = (i2end>i2beg)?1:-1;
	if(style==NORMAL){
	    for(i1=0,i2=i1beg;i1<nx;i1++,i2+=i1step)
	      ix[i1]=x1[i2];
	    for(i1=0,i2=i2end;i1<ny;i1++,i2-=i2step)
	      iy[i1]=x2[i2];
	}else{
	    for(i1=0,i2=i2beg;i1<nx;i1++,i2+=i2step)
	      ix[i1]=x2[i2];
	    for(i1=0,i2=i1beg;i1<ny;i1++,i2+=i1step)
	      iy[i1]=x1[i2];
	}


	/* connect to X server */
	if ((dpy=XOpenDisplay(NULL))==NULL)
		err("Cannot connect to display %s!\n",XDisplayName(NULL));
	scr = DefaultScreen(dpy);
	black = BlackPixel(dpy,scr);
	white = WhitePixel(dpy,scr);
	


	/* create window */
	win = xNewWindow(dpy,xbox,ybox,wbox,hbox,(int)black,(int)white,windowtitle);
		
	/* make GC for image */
	gci = XCreateGC(dpy,win,0,NULL);

	/* make sure foreground/background are black/white */
	XSetForeground(dpy,gci,black);
	XSetBackground(dpy,gci,white);


	/* set normal event mask */
	XSelectInput(dpy,win,
		StructureNotifyMask |
		ExposureMask |
		KeyPressMask |
		PointerMotionMask |
		ButtonPressMask |
		ButtonReleaseMask |
		Button1MotionMask |
		Button2MotionMask);
	
	/* map window */
	XMapWindow(dpy,win);
	
	/* clear the window */
	XClearWindow(dpy,win);

	/* Set Backing store so we don't need to update the plot unless we
	   need it... Check if it's work in IRIX*/
	set_window_attributes.backing_store = Always;
	XChangeWindowAttributes (dpy,win, CWBackingStore, &set_window_attributes);


	/* determine good size for axes box */
	xSizeAxesBox(dpy,win,
		labelfont,titlefont,style,
		&x,&y,&width,&height);
	
	/* note that image is out of date */
	imageOutOfDate = 1;

	/* main event loop */
	while((imageOutOfDate)|(~imageOutOfDate)/*True*/) {
		XNextEvent(dpy,&event);

		/* if window was resized */
		if (event.type==ConfigureNotify &&
			(event.xconfigure.width!=winwidth ||
			 event.xconfigure.height!=winheight)) {
			winwidth = event.xconfigure.width;
			winheight = event.xconfigure.height;
							
			/* determine good size for axes box */
			xSizeAxesBox(dpy,win,
				labelfont,titlefont,style,
				&x,&y,&width,&height);
			
			/* clear the window */
/*			XClearWindow(dpy,win);*/
			
			/* note that image is out of date */
			imageOutOfDate = 1;
		/* else if window exposed */
		} else if (event.type==Expose) {

		        /* clear all expose events from queue */
		        while (XCheckTypedEvent(dpy,Expose,&event));
		        if(imageOutOfDate){
			  float *ixX,*iyX,yscale,xscale;
			  /* I only clear the window when I need to*/
			  /* For a static image I just let backingstore 
			     handle it*/
			  XClearWindow(dpy,win);

			  ixX=ealloc1float(nxb);
			  iyX=ealloc1float(nyb);
			  /* map x1 and x2 units to bitmap units */
			  xscale = (width-1)/(ix[ixb+nxb-1]-ix[ixb]);
			  yscale = (height-1)/(iy[iyb+nyb-1]-iy[iyb]);
			  
			  for (i1=0; i1<nxb; i1++) 
			    ixX[i1]=(ix[ixb+i1]-ix[ixb])*xscale;
			  for (i2=0; i2<nyb; i2++) 
			    iyX[i2]=(iy[iyb+i2]-iy[iyb])*yscale;
			
			  newContour(dpy, win,gci,
				     x,y,nxb,&ixX[0],nyb,
				     &iyX[0],czb,
				     nc,c,cwidth,ccolor,cdash,labelcf,labelcper,nlabelc,labelcfont,labelccolor,nplaces);
			free1float(ixX);
			free1float(iyX);
			}
			
			/* draw axes on top of image */
			xDrawAxesBox(dpy,win,
				x,y,width,height,
				x1begb,x1endb,0.0,0.0,
				d1num,f1num,n1tic,grid1,label1,
				x2begb,x2endb,0.0,0.0,
				d2num,f2num,n2tic,grid2,label2,
				labelfont,title,titlefont,
				labelcolor,titlecolor,gridcolor,
				style);

		/* else if key down */
		} else if (event.type==KeyPress) {

			XLookupString(&(event.xkey),keybuf,0,&keysym,&keystat);
			if (keysym==XK_s) {
				xMousePrint(event,style, mpicksfp,
					    x,y,width,height,
					    x1begb,x1endb,x2begb,x2endb,
					    p2beg, p2end);
			} else if (keysym==XK_q || keysym==XK_Q) {
			/* This is the exit from the event loop */
				break;
			} else if (keysym==XK_p || keysym==XK_P) {
			/* invoke pscontour with appropriate data */
				char cmdline[1024], cmdtemp[256];
				float cmdfloat;
				int iargc;
				int nbpi;

				efseeko(infp,(off_t) 0,SEEK_SET);
				strcpy(cmdline,"pscontour ");
				for (iargc = 1; iargc < argc; iargc++ ) {
					strcat(cmdline," ");
					strcat(cmdline,argv[iargc]);
					}
				/* override incompatible args */
				sprintf(cmdtemp," axescolor=%s",labelcolor);
				strcat(cmdline,cmdtemp);
				nbpi = 300; getparint("nbpi", &nbpi);
                                checkpars();
				sprintf(cmdtemp," nbpi=%d",nbpi);
				strcat(cmdline,cmdtemp);
				cmdfloat = DisplayWidthMM(dpy,scr)/25.4;
				cmdfloat /= DisplayWidth(dpy,scr);
				sprintf(cmdtemp," wbox=%g", cmdfloat*width);
				strcat(cmdline,cmdtemp);
				sprintf(cmdtemp," xbox=%g", 0.5+cmdfloat*xbox);
				strcat(cmdline,cmdtemp);
				cmdfloat = DisplayHeightMM(dpy,scr)/25.4;
				cmdfloat /= DisplayHeight(dpy,scr);
				sprintf(cmdtemp," hbox=%g", cmdfloat*height);
				strcat(cmdline,cmdtemp);
				sprintf(cmdtemp," ybox=%g", 0.5+cmdfloat*ybox);
				strcat(cmdline,cmdtemp);
				sprintf(cmdtemp," x1beg=%g", x1begb);
				strcat(cmdline,cmdtemp);
				sprintf(cmdtemp," x1end=%g", x1endb);
				strcat(cmdline,cmdtemp);
				sprintf(cmdtemp," x2beg=%g", x2begb);
				strcat(cmdline,cmdtemp);
				sprintf(cmdtemp," x2end=%g", x2endb);
				strcat(cmdline,cmdtemp);
				strcat(cmdline," title=\"");
				strcat(cmdline,title); strcat(cmdline,"\"");
				strcat(cmdline," label1=\"");
				strcat(cmdline,label1); strcat(cmdline,"\"");
				strcat(cmdline," label2=\"");
				strcat(cmdline,label2); strcat(cmdline,"\"");
				fprintf(stderr,"%s\n",cmdline);
				system(cmdline);
			} else {
				continue;
			}

		/* else if button down (1 == zoom, 2 == mouse tracking */
		} else if (event.type==ButtonPress) {
			/* if 1st button: zoom */
			if (event.xbutton.button==Button1) {

				/* track pointer and get new box */
				xRubberBox(dpy,win,event,&xb,&yb,&wb,&hb);
			
				/* if new box has tiny width or height */
				if (wb<4 || hb<4) {
				
					/* reset box to initial values */
					x1begb = x1beg;
					x1endb = x1end;
					x2begb = x2beg;
					x2endb = x2end;
					nxb = nx;
					nyb = ny;
					ixb = iyb = 0;
					if (czb!=cz) free1(czb);
					czb = cz;

				/* else, if new box has non-zero width */
				} else {
			
					/* calculate new box parameters */
					if (style==NORMAL) {
					    zoomBox(x,y,width,height,
						    xb,yb,wb,hb,
						    nxb,ixb,x1begb,x1endb,
						    nyb,iyb,x2endb,x2begb,
						    &nxb,&ixb,&x1begb,&x1endb,
						    &nyb,&iyb,&x2endb,&x2begb);
					} else {
					    zoomBox(x,y,width,height,
						    xb,yb,wb,hb,
						    nxb,ixb,x2begb,x2endb,
						    nyb,iyb,x1begb,x1endb,
						    &nxb,&ixb,&x2begb,&x2endb,
						    &nyb,&iyb,&x1begb,&x1endb);
					}

					/* make new bytes in zoombox */
					if (czb!=cz) free1(czb);
					czb = ealloc1float(nxb*nyb);
					for (i=0,czbp=czb; i<nyb; i++) {
					    czp = cz+(iyb+i)*nx+ixb;
					    for (j=0; j<nxb; j++)
						    *czbp++ = *czp++; 
					}

				}

				/* clear area and force an expose event */
				XClearArea(dpy,win,0,0,0,0,True);
				/* note that image is out of date */
				imageOutOfDate = 1;
			
			/* else if 2nd button down: display mouse coords */
			} else if (event.xbutton.button==Button2) {

				showloc = 1;
				xMouseLoc(dpy,win,event,style,showloc,
					  x,y,width,height,x1begb,x1endb,
					  x2begb,x2endb,p2beg,p2end);

			} else {
				continue;
			}

		/* else if pointer has moved */
		} else if (event.type==MotionNotify) {
			
			/* if button2 down, show mouse location */
			if (showloc)
				xMouseLoc(dpy,win,event,style,True,
					x,y,width,height,x1begb,x1endb,
					x2begb,x2endb,p2beg,p2end);

		/* else if button2 released, stop tracking */
		} else if (event.type==ButtonRelease &&
			   event.xbutton.button==Button2) {
			showloc = 0;
		}

	} /* end of event loop */

	/* close connection to X server */
	XCloseDisplay(dpy);

	return EXIT_SUCCESS;
}
			
/* update parameters associated with zoom box */
static void zoomBox (int x, int y, int w, int h, 
	int xb, int yb, int wb, int hb,
	int nx, int ix, float x1, float x2,
	int ny, int iy, float y1, float y2,
	int *nxb, int *ixb, float *x1b, float *x2b,
	int *nyb, int *iyb, float *y1b, float *y2b)
{
	/* if width and/or height of box are zero, just copy values */
	if (wb==0 || hb==0) {
		*nxb = nx; *ixb = ix; *x1b = x1; *x2b = x2;
		*nyb = ny; *iyb = iy; *y1b = y1; *y2b = y2;
		return;		
	} 
	
	/* clip box */
	if (xb<x) {
		wb -= x-xb;
		xb = x;
	}
	if (yb<y) {
		hb -= y-yb;
		yb = y;
	}
	if (xb+wb>x+w) wb = x-xb+w;
	if (yb+hb>y+h) hb = y-yb+h;
	
	/* determine number of samples in rubber box (at least 2) */
	*nxb = MAX(nx*wb/w,2);
	*nyb = MAX(ny*hb/h,2);
	
	/* determine indices of first samples in box */
	*ixb = ix+(xb-x)*(nx-1)/w;
	*ixb = MIN(*ixb,ix+nx-*nxb);
	*iyb = iy+(yb-y)*(ny-1)/h;
	*iyb = MIN(*iyb,iy+ny-*nyb);
	
	
	/* determine box limits to nearest samples */
	*x1b = x1+(*ixb-ix)*(x2-x1)/(nx-1);
	*x2b = x1+(*ixb+*nxb-1-ix)*(x2-x1)/(nx-1);
	*y1b = y1+(*iyb-iy)*(y2-y1)/(ny-1);
	*y2b = y1+(*iyb+*nyb-1-iy)*(y2-y1)/(ny-1);
}

/* Plots a new set of contours with the right apperance */
static void newContour (Display *dpy, Window win,GC gci,
	int xbase, int ybase, int nx, float x[], int ny, float y[], float *z,
	int nc, float c[],float cwidth[], char *ccolor[],float cdash[],int labelcf,int labelcper,int nlabelc,char *labelcfont,char *labelccolor, int nplaces)
{
        float *w;
	int ic,linewidth=0;
	XColor scolor,ecolor;
	char dash[2];
	char lcflag=0;
	XGCValues *values=NULL;
	GC gcc;
	GC gcl;
	XWindowAttributes wa;
	Colormap cmap;
	XFontStruct *fs;
	int scr=DefaultScreen(dpy);
	int i;
	/* zero w array for contour labeling*/
	w = ealloc1float(nx*ny);
	for(i=0; i<nx*ny; i++) {
	  w[i] = 0.;
	}
	

	/* shift the axes due to xbase and ybase */
	for(i=0;i<nx;i++)
	  x[i]=x[i]+xbase;
	for(i=0;i<ny;i++)
	  y[i]=y[i]+ybase;
	  /* determine window's current colormap */
	  XGetWindowAttributes(dpy,win,&wa);
	  cmap = wa.colormap;

	/* Generate at local GC used for the contour lines */ 
	gcc=gci;
	gcc=XCreateGC(dpy,win,0,values);
	/* Generate at local GC used for the contour labels */ 
	gcl=XCreateGC(dpy,win,0,NULL);

	/*Set the color of the contour labels */
	if(strcmp(labelccolor,"none")){
	  if (!XAllocNamedColor(dpy,cmap,labelccolor,&scolor,&ecolor))
	    XSetForeground(dpy,gcl,BlackPixel(dpy,scr));
	  else
	    XSetForeground(dpy,gcl,ecolor.pixel);
	} else {
	  XSetForeground(dpy,gcl,BlackPixel(dpy,scr));
	}
	  XSetBackground(dpy,gcl,WhitePixel(dpy,scr));
	/* Set the labeling font */
	fs = XLoadQueryFont(dpy,labelcfont);
	if (fs==NULL) fs = XLoadQueryFont(dpy,"fixed");

	XSetFont(dpy,gcl,fs->fid);

	/*loop over contours*/
	for (ic=0;ic<nc;ic++){
	  /*Set the linestyle */
	  if (nlabelc>0) {
	    if((ic-labelcf+1)%labelcper==0 && ic>=labelcf-1  
			   && ic<labelcf-1+labelcper*nlabelc) {
			  linewidth=cwidth[ic];
			  lcflag=1;
			}
			else { 
			  linewidth=0.25*cwidth[ic];
			  lcflag=0;	
			}
	  }
			
	/*	(note that small line segments will mask the dash settings) */
	  if(cdash[ic]!=0.0){
	    XSetLineAttributes(dpy,gcc,linewidth,LineOnOffDash,CapButt,JoinMiter);
	    dash[0] = cdash[ic];  dash[1] = cdash[ic];
	    XSetDashes(dpy,gcc,0,dash,2);
	  } else {
	    XSetLineAttributes(dpy,gcc,linewidth,LineSolid,CapButt,JoinMiter);
	  }
	  
	  /*Set the color of the contour lines */
	  if(strcmp(ccolor[ic],"none")){
	    if (!XAllocNamedColor(dpy,cmap,ccolor[ic],&scolor,&ecolor))
	      XSetForeground(dpy,gcc,BlackPixel(dpy,scr));
	    else
	      XSetForeground(dpy,gcc,ecolor.pixel);
	  }else{
	    XSetForeground(dpy,gcc,BlackPixel(dpy,scr));
	  }

	  /* Let xContour draw the contours */
	  xContour(dpy,win,gcc,gcl,&c[ic],nx,x,ny,y,z,lcflag,labelcfont,labelccolor,w,nplaces);
	  }
	free1float(w);
}

void xMouseLoc(Display *dpy, Window win, XEvent event, int style, Bool show,
	int x, int y, int width, int height,
	float x1begb, float x1endb, float x2begb, float x2endb,
	float p2beg, float p2end)
{
	static XFontStruct *fs=NULL;
	static XCharStruct overall;
	static GC gc;
	int dummy,xoffset=5,yoffset=5;
	float x1,x2;
	char string[256];

	/* if first time, get font attributes and make gc */
	if (fs==NULL) {
		fs = XLoadQueryFont(dpy,"fixed");
		gc = XCreateGC(dpy,win,0,NULL);

		/* make sure foreground/background are black/white */
		XSetForeground(dpy,gc,BlackPixel(dpy,DefaultScreen(dpy)));
		XSetBackground(dpy,gc,WhitePixel(dpy,DefaultScreen(dpy)));

		XSetFont(dpy,gc,fs->fid);
		overall.width = 1;
		overall.ascent = 1;
		overall.descent = 1;
	}

	/* erase previous string */
	XClearArea(dpy,win,xoffset,yoffset,
		overall.width,overall.ascent+overall.descent,False);

	/* if not showing, then return */
	if (!show) return;

	/* convert mouse location to (x1,x2) coordinates */
	if (style==NORMAL) {
		x1 = x1begb+(x1endb-x1begb)*(event.xmotion.x-x)/width;
		x2 = p2end+x2endb+(p2beg+x2begb-x2endb-p2end)*
			(event.xmotion.y-y)/height;
	} else {
		x1 = x1begb+(x1endb-x1begb)*(event.xmotion.y-y)/height;
		x2 = p2beg+x2begb+(p2end+x2endb-x2begb-p2beg)*
			(event.xmotion.x-x)/width;
	}

	/* draw string indicating mouse location */
	sprintf(string,"(%0.3g,%0.3g)",x1,x2);
	XTextExtents(fs,string,(int) strlen(string),&dummy,&dummy,&dummy,&overall);
	XDrawString(dpy,win,gc,xoffset,yoffset+overall.ascent,
		string,(int) strlen(string));
}

void xMousePrint(XEvent event, int style, FILE *mpicksfp,
		 int x, int y, int width, int height,
		 float x1begb, float x1endb, float x2begb, float x2endb,
		 float p2beg, float p2end)
{
	float x1,x2;

	/* convert mouse location to (x1,x2) coordinates */
	if (style==NORMAL) {
		x1 = x1begb+(x1endb-x1begb)*(event.xmotion.x-x)/width;
		x2 = p2end+x2endb+(p2beg+x2begb-x2endb-p2end)*
			(event.xmotion.y-y)/height;
	} else {
		x1 = x1begb+(x1endb-x1begb)*(event.xmotion.y-y)/height;
		x2 = p2beg+x2begb+(p2end+x2endb-x2begb-p2beg)*
			(event.xmotion.x-x)/width;
	}

	/* write string indicating mouse location */
	fprintf(mpicksfp, "%0.4g  %0.4g\n", x1, x2);
}




