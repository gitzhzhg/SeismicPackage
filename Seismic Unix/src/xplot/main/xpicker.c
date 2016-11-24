/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* XPICKER: $Revision: 1.26 $ ; $Date: 2011/11/21 17:03:51 $  */

#include "par.h"
#include "xplot.h"
#include <X11/Xatom.h>
#include <X11/keysym.h>
#define EGSTERN
#include "xwindow.h"
#include "garnish.h"
#include "picking.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" XPICKER - X wiggle-trace plot of f(x1,x2) via Bitmap with PICKing	",
"									",
" xpicker n1= [optional parameters] <binaryfile				",
"									",
" X Menu functionality:							",
"    Pick Filename Window	default is pick_file			",
"    Load		load an existing Pick Filename			",
"    Save		save to Pick Filename				",
"    View only/Pick	default is View, click to enable Picking	",
"    Add/Delete		default is Add, click to delete picks		",
"    Cross off/on	default is Cross off, click to enable Crosshairs",
"									",
" In View mode:								",
"    a or page up keys          enhance clipping by 10%                 ",
"    c or page down keys        reduce clipping by 10%                  ",
"    up,down,left,right keys    move zoom window by half width/height   ",
"    i or +(keypad)             zoom in by factor 2                     ",
"    o or -(keypad)             zoom out by factor 2                    ",
"    l 				lock the zoom while moving the coursor	",
"    u 				unlock the zoom 			",
"									",
" Notes:								",
"	Menu selections and toggles (\"clicks\") are made with button 1	",
"	Pick selections are made with button 3				",
"	Edit a pick selection by dragging it with button 3 down	or	",
"	by making a new pick on that trace				",
"	Reaching the window limits while moving within changes the zoom	",
"	factor in this direction. The use of zoom locking(l) disables it",
"									",
" Other X Mouse functionality:						",
" Mouse Button 1	Zoom with rubberbox				",
" Mouse Button 2	Show mouse (x1,x2) coordinates while pressed	",
"									",
" The following keys are active in View Only mode:			",
"									",
" Required Parameters:							",
" n1=		number of samples in 1st (fast) dimension		",
"									",
" Optional Parameters:							",
" mpicks=pick_file	name of output (input) pick file		",
" d1=1.0		sampling interval in 1st dimension		",
" f1=d1		  first sample in 1st dimension				",
" n2=all		 number of samples in 2nd (slow) dimension	",
" d2=1.0		 sampling interval in 2nd dimension		",
" f2=d2		  first sample in 2nd dimension				",
" x2=f2,f2+d2,...	array of sampled values in 2nd dimension	",
" bias=0.0	       data value corresponding to location along axis 2",
" perc=100.0	     percentile for determining clip			",
" clip=(perc percentile) data values < bias+clip and > bias-clip are clipped",
" xcur=1.0	       wiggle excursion in traces corresponding to clip	",
" wt=1		   =0 for no wiggle-trace; =1 for wiggle-trace		",
" va=1		   =0 for no variable-area; =1 for variable-area fill	",
"                        =2 for variable area, solid/grey fill          ",
"                        SHADING: 2<=va<=5  va=2 light grey, va=5 black ",
" verbose=1	      =1 for info printed on stderr (0 for no info)	",
" xbox=50		x in pixels of upper left corner of window	",
" ybox=50		y in pixels of upper left corner of window	",
" wbox=550	      	width in pixels of window			",
" hbox=700		height in pixels of window			",
" x1beg=x1min		value at which axis 1 begins			",
" x1end=x1max		value at which axis 1 ends			",
" d1num=0.0		 numbered tic interval on axis 1 (0.0 for automatic)",
" f1num=x1min		first numbered tic on axis 1 (used if d1num not 0.0)",
" n1tic=1		number of tics per numbered tic on axis 1	",
" grid1=none		grid lines on axis 1 - none, dot, dash, or solid",
" label1=		label on axis 1					",
" x2beg=x2min		value at which axis 2 begins			",
" x2end=x2max		value at which axis 2 ends			",
" d2num=0.0		 numbered tic interval on axis 2 (0.0 for automatic)",
" f2num=x2min		first numbered tic on axis 2 (used if d2num not 0.0)",
" n2tic=1		number of tics per numbered tic on axis 2	",
" grid2=none		grid lines on axis 2 - none, dot, dash, or solid ",
" label2=		label on axis 2					",
" labelfont=Erg14	font name for axes labels			",
" title=		title of plot					",
" titlefont=Rom22	font name for title				",
" labelcolor=blue	color for axes labels				",
" titlecolor=red	color for title					",
" gridcolor=blue	color for grid lines				",
" style=seismic		normal (axis 1 horizontal, axis 2 vertical) or	",
" 		    	seismic (axis 1 vertical, axis 2 horizontal)	",
" endian=		=0 little endian, =1 big endian			",
" interp=0		no sinc interpolation				",
"			=1 perform sinc interpolation			",
" x2file=		file of \"acceptable\" x2 values		",
" x1x2=1		save picks in the order (x1,x2) 		",
" 			=0 save picks in the order (x2,x1) 		",
"									",
" Notes:								",
" Xpicker will try to detect the endian value of the X-display and will	",
" set it to the right value. If it gets obviously wrong information the ",
" endian value will be set to the endian value of the machine that is	",
" given at compile time as the value of CWPENDIAN defined in cwp.h	",
" and set via the compile time flag ENDIANFLAG in Makefile.config.	",
"								       ",
" The only time that you might want to change the value of the endian   ",
" variable is if you are viewing traces on a machine with a different   ",
" byte order than the machine you are creating the traces on AND if for ",
" some reason the automaic detection of the display byte order fails.   ",
" Set endian to that of the machine you	are viewing the traces on.	",
" 									",
" The interp flag is useful for making better quality wiggle trace for	",
" making plots from screen dumps. However, this flag assumes that the	",
" data are purely oscillatory. This option may not be appropriate for all",
" data sets.								",
" 									",
" If the x2file=  option is set, then the values from the specified file",
" will define the set of \"acceptable\" values of x2 for xpicker to	",
" output. The format is a single column of ASCII values. The number of  ",
" specified values is arbitrary.					",
" 									",
" Such a file can be built from an SU data set via:			",
"     sugethw < sudata key=offset output=geom > x2example 		",
" 									",
" If the value of x2file= is not set, then				",
" xpicker will use the values specified via: x2=.,.,.,. or those that are", 
" computed from the values of f2=  and d2= as being the \"acceptible\" ",
" values.								",
" 									",
" See the selfdoc of  suxpicker  for information on using key fields from",
" the SU trace headers directly. 					",
" 									",
NULL};

/*
 * AUTHOR:  Dave Hale, Colorado School of Mines, 08/09/90
 * with picking by Wenying Cai of University of Utah.
 * Endian stuff by Morten Pedersen and John Stockwell of CWP.
 * Interp stuff by Tony Kocurko of Memorial University of Newfoundland
 * Modified to include acceptable values by Bill Lutter of the
 *     Department of Geology, University of Wisconsin 10/96
 * MODIFIED:  P. Michaels, Boise State Univeristy  29 December 2000
 *            Added solid/grey color scheme for peaks/troughs
 * 
 * G.Klein, IFG Kiel University, 2003-09-29, added cursor scrolling and
 *            interactive change of zoom and clipping.
 *
 * NOTES:
 * Interactive picker improved to allow x-axis of picks to be
 * coordinated with "key=header" parameter set in driver routine
 * suxpicker. Multiple picks per trace are now allowed.
 *
 *  Input:
 *  The command line of suxpicker is unchanged.  The parameter"key=header"
 *  set in  suxpicker controls a) trace x-axis  displayed via xpicker and
 *  b) the header values in the first column of a pick file either read in
 *     or written out from xpicker c) header values expected in optional file
 *      or written out from xpicker c) header values expected in optional file
 *     x2file= which reads into xpicker allowable trace x-axis values.
 *
 *   a) example command line:  suxpicker key=offset < shot10.plotpik
 *
 *   b) pick file format:
 *	x-axis_value_1 time_1
 *	x-axis_value_2 time_2  
 *	x-axis_value_3 time_3
 *	etc.
 *	x-axis_value_n time_n
 *
 *	pick file example:
 *         1000.000000 0.500000
 *         2000.000000 1.000000
 *         3000.000000 1.500000
 *         4000.000000 2.000000
 *         5000.000000 2.500000
 *
 *  c)  format of optional file x2file=:
 *    	   header_value_1
 * 	   header_value_2
 *	   etc.
 *	   header_val_m
 *
 *       If file "x2file=" exists in directory from which suxpicker is
 *      invoked, then these trace header x-axis values are the only allowable
 *      x-axis pick values used in the pick "add" or "delete" menu operation.
 *      Header values do not need to be sorted or 1 to 1 with input traces.
 *      Further, pick file x-axis values can be read into xpicker via load
 *      operation without having to match key_pickx1_val x-axis values and
 *      can also be rewritten out an output pickfile.  As indicated, only
 *      the "add" and "delete" pick operations are influenced by existence
 *       of this file.
 *
 *      Offset header values for "x2file=" can be generated by the
 *      command line:
 *
 *      sugethw < su_segyfile key=offset output=geom >  x2examplefile=
 *
 *      Output: Only change is in format of pick_file (format described above).
 *      If x2file= file exists then x-axis value of added picks
 *      will be forced to nearest allowable trace x-axis value (input values
 *      of x2file= file). If x2file= is not set, then the values of x2 
 *      that are either assigned uniformly to the traces via f2 and d2,
 *      or by the vector of values of x2=.,.,.,.    will be the "acceptable"
 *      values.
 *
 *    Strategy:
 *   a) malloc() and realloc() used to dynamically allocate memory
 *	  for array of x-axis value read in from optional file
 *	  x2file=.  This is done in function read_keyval().
 *
 *	  b) The pick file dimensions are set in main program via malloc()
 *	  and then initialized (*apick)[i].picked = FALSE) in function
 *	  init_picks().  The pick file is declared as pick_t **apick, in
 *	  order to use realloc() as needed in functions load_picks where the
 *	  pick file is read in and edit_picks where picks are added.  The
 *	  call to realloc() and further initializing is performed in
 *	  function realloc_picks().
 *
 *	  c) If x2file= file exists the mouse derived x-axis value
 *	  for a pick to be added is checked against allowable x-axis values
 *	  to find the closest match via function add_pick called from
 *	  edit_picks.  If the pick is to be deleted, first a search is done
 *	  to find the closest x-axis value, then the existing pick values
 *	  are searched to find the closest radial value (x**2 + t**2) via
 *	  function del_pick() invoked from edit_picks.
 *
 *	  d) Code modifications are limited to above mentioned functions,
 *	  except for additional parameters passed to functions edit_picks,
 *	  load_picks, save_picks, and check_buttons.
 */ 

/**************** end self doc ********************************/

/* Prototypes of functions defined and used internally */
static void zoomBox (int x, int y, int w, int h, 
	int xb, int yb, int wb, int hb,
	float x1, float x2,
	float y1, float y2,
	float *x1b, float *x2b,
	float *y1b, float *y2b);
static XImage *newBitmap (Display *dpy, int width, int height,
	int n1, float d1, float f1, int n2, float *x2, float *z,
	float x1beg, float x1end, float x2beg, float x2end,
	float xcur, float clip, int wt, int va,
	float *p2begp, float *p2endp, int endian, int interp);
void xMouseLoc(Display *dpy, Window win, XEvent event, int style, Bool show,
	int x, int y, int width, int height,
	float x1begb, float x1endb, float x2begb, float x2endb,
	float p2beg, float p2end);
void xMousePrint(XEvent event, int style,
	FILE *mpicksfp, int x, int y, int width, int height,
	float x1begb, float x1endb, float x2begb, float x2endb);
static void draw_cross(Display *dpy, Window win, GC gc, int x, int y,
			int w, int h, int mx, int my, int cross_mode,
			int draw_f);
void init_stuff(int winwidth, int num_wiggles,
		TextSet **filename_input, char *pick_fname,
		int *control_mode,int *edit_mode, int *cross_mode);
void save_picks(pick_t **apick, int num_wiggles, char *fnam,
	       int *pickdimax, int *pickdimend, int verbose, int x1x2);
void load_picks(pick_t **apick, int num_wiggles, char *fname,
	       int *pickdimax, int *pickdimend, int verbose, int x1x2);

/** added by Bill Lutter - Oct 96     */
void realloc_picks(pick_t **apick, int more,
	int *pickdimax, int *pickdimend, int verbose);
float *read_keyval(char *x2file,int *ioff, int verbose);
void init_picks(pick_t **apick, int *pickdimax, int *pickdimend);
void add_pick(pick_t **apick, int *pickdimax, int *pickdimend,
	     float *off, int ioff,
	     float fy, int *pick_num, int *ihead,int verbose);
void del_pick(pick_t **apick, int *pickdimax, int *pickdimend,
	     float *off, int ioff,
	     float fy, int *pick_num, int *ihead, int verbose); 

void edit_pick(Display *dpy, Window win, GC gc, XEvent event,
	       int style, int x, int y,
	       int width, int height,
	       pick_t **apick, int num_wiggles,
	       float x1begb, float x1endb,
	       float x2begb, float x2endb,
	       float p2beg, float p2end,
	       int control_mode,
	       int edit_mode,
	       int cross_mode,
	       int *pickdimax,
	       int *pickdimend,
	       int ioff, float *off, int verbose);
/** 			*/

void draw_pick(Display *dpy, Window win, GC gc, pick_t *pick, int i,
	       int xmargin, int ymargin,
	       int width, int height,
	       float x1begb, float x1endb,
	       float x2begb, float x2endb,
	       float p2beg, float p2end);
void draw_command_bar(int winwidth, TextSet *filename_input,
		      char *pick_fname, int control_mode, int edit_mode,
		      int cross_mode);
void draw_seg(Display *dpy, Window win, GC gc, int x, int y);

void check_buttons(Display *dpy, Window win, GC gc,XEvent event,pick_t **apick,
		   int num_wiggles,
		   int x, int y,
		   int winwidth, int height,
		   float x1begb, float x1endb,
		   float x2begb, float x2endb,
		   float p2beg, float p2end,
		   TextSet *filename_input,
		   char *pick_fname,
		   int *control_mode,
		   int *edit_mode,
		   int *cross_mode,
		   int *pickdimax,
		   int *pickdimend,
		   int verbose,
		   int x1x2);

int
main (int argc, char **argv)
{
	/* begin picking related variables */
	int num_wiggles;	/* number of total wiggles in plot */
	int control_mode;	/* specifies pick mode or regular mode */
	int edit_mode;		/* specifies pick add mode or pick delete */
	int cross_mode;		/* draw crosshair if TRUE */
	char pick_fname[50];	/* pick file name */	

	TextSet *filename_input;
	/* end picking related variables */
  
	int n1,n2,n1tic,n2tic,wt,va,
		i2,grid1,grid2,style=0,
		nz,iz,verbose=0,
		xbox,ybox,wbox,hbox,
		xb,yb,wb,hb,
		x,y,width,height,
		i,
		imageOutOfDate,winwidth=-1,winheight=-1,
		showloc=0,endian,interp;
	unsigned long nfloats;
	float labelsize,titlesize,perc,clip,xcur,bias,
		d1,f1,d2,f2,*z,*temp,*x2,
		x1beg,x1end,x2beg,x2end,
		x1min,x1max,x2min,x2max,
		d1num,f1num,d2num,f2num,
		x1begb,x1endb,x2begb,x2endb,p2beg=0,p2end=0;
	char *label1="",*label2="",*title="",
		*labelfont="Erg14",*titlefont="Rom22",
		*styles="seismic",*grid1s="none",*grid2s="none",
		*labelcolor="blue",*titlecolor="red",
		*gridcolor="blue",keybuf[256],*mpicks;
	FILE *infp=stdin, *mpicksfp;
	Display *dpy;
	Window win;
	XEvent event;
	KeySym keysym;
	XComposeStatus keystat;
	XImage *image=NULL;
	GC gci,rgc;
	int scr;

	unsigned long mask;
	unsigned long black,white;

	/** Added by Bill Lutter 10/96    */
	float *off;     /* pointer for 1-d float array */
	int ioff;	/* number of header value entries in off */
	int pickdimax=10; /* current dimension of pick array apick */
	int pickdimend=0; /* number of pick entries in apick  */
	pick_t **apick,*cpick; 

	/* added by John Stockwell */
	char *x2file="";	/* x2file name	*/
	
	/* added by John Stockwell */
	int x1x2;		/* flag to specfy (x1,x2) output */ 

	/* added by Gerald Klein */
	int lock=0;		/* lock/unlock zoom while scrolling */
	float mve;		/* distance for scrolling */
	char  *msg="";		/* message on screen */
	
	/* initialize getpar */
	initargs(argc,argv);
	requestdoc(1);


	/* get parameters describing 1st dimension sampling */
	if (!getparint("n1",&n1))
		err("Must specify number of samples in 1st dimension!\n");
	d1 = 1.0;  getparfloat("d1",&d1);
	f1 = d1;  getparfloat("f1",&f1);
	x1min = (d1>0.0)?f1:f1+(n1-1)*d1;
	x1max = (d1<0.0)?f1:f1+(n1-1)*d1;

	/* get parameters describing 2nd dimension sampling */
	if ((n2=countparval("x2"))==0 && !getparint("n2",&n2)) {
			if (efseeko(infp,(off_t) 0,SEEK_END)!=0)
				err("must specify n2 if in a pipe!\n");
			nfloats = (int)(eftello(infp)/((off_t) sizeof(float)));
			efseeko(infp,(off_t) 0,SEEK_SET);
			n2 = (int) nfloats/n1;
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

	getparstring("x2file",&x2file);
	
	if (*x2file!='\0') { 	
		/**  added by Bill Lutter 10/96   */
		/* Read in header values (allowable pick.x2 values and also */
		/* allowable values (for these traces) in pick file  */
		off = read_keyval(x2file,&ioff,verbose);
		if (verbose) warn("x-axis value 0:   %f\n",off[0]);
		if (verbose) warn("x-axis value ioff:  %d %f\n",ioff,off[ioff]);
	} else { /* use n2 and x2 for the "ioff" and "off" values */
		ioff = n2;
		off = ealloc1float(ioff);
		memcpy( (void *) off, (const void *) x2, n2*FSIZE);
	}
	if (!getparint("x1x2",&x1x2))		x1x2 = 1;
		
	/* dynamically allocate memory for picks  - Bill Lutter    */
	cpick = (pick_t *) malloc(pickdimax*sizeof(struct pick_tag ));
	apick = &cpick;
	init_picks(apick,&pickdimax,&pickdimend);

	/* set up file to save mouse picks */
	if (!getparstring("mpicks", &mpicks)) mpicks = "pick_file";
		mpicksfp = efopen(mpicks, "w");
	/* read binary data to be plotted */
	nz = n1*n2;
	z = ealloc1float(nz);
	if (fread(z,sizeof(float),nz,infp)!=nz)
		err("error reading input file");

	/* if necessary, subtract bias */
	if (getparfloat("bias",&bias) && bias!=0.0)
		for (iz=0; iz<nz; iz++)
			z[iz] -= bias;
	
	/* if necessary, determine clip from percentile */
	if (!getparfloat("clip",&clip)) {
		perc = 100.0;  getparfloat("perc",&perc);
		temp = ealloc1float(nz);
		for (iz=0; iz<nz; iz++)
			temp[iz] = fabs(z[iz]);
		iz = (nz*perc/100.0);
		if (iz<0) iz = 0;
		if (iz>nz-1) iz = nz-1;
		qkfind(iz,nz,temp);
		clip = temp[iz];
		free1float(temp);
	}
	verbose = 1;  getparint("verbose",&verbose);
	if (verbose) warn("clip=%g",clip);

	/* get wiggle-trace-variable-area parameters */
	wt = 1;  getparint("wt",&wt);
	va = 1;  getparint("va",&va);

       /* set wt=va for va with solid/grey coloring  */
        if (va>=2) 
        {  wt=va; va=1; } 

	xcur = 1.0;  getparfloat("xcur",&xcur);

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
	if (STREQ("seismic",styles)) style = SEISMIC;
	else err("Sorry, only style=seismic is currently available!");
	getparstring("titlecolor",&titlecolor);
	getparstring("labelcolor",&labelcolor);
	getparstring("gridcolor",&gridcolor);
	
	/* initialize zoom box parameters */
	x1begb = x1beg;  x1endb = x1end;
	x2begb = x2beg;  x2endb = x2end;

	/* connect to X server */
	if ((dpy=XOpenDisplay(NULL))==NULL)
		err("Cannot connect to display %s!\n",XDisplayName(NULL));
	scr = DefaultScreen(dpy);
	black = BlackPixel(dpy,scr);
	white = WhitePixel(dpy,scr);

	/* set endian for display */
	if (!getparint("endian",&endian)){
		if (BitmapBitOrder(dpy)==LSBFirst)
			endian=0;
		else if (BitmapBitOrder(dpy)==MSBFirst)
			endian=1;
		else 
			endian=CWPENDIAN;
	}

	/* set interpolation flag for display */
	if (!getparint("interp",&interp)) 	interp = 0;
        checkpars();	
	/* create window */
	win = xNewWindow(dpy,xbox,ybox,
			 wbox+COMMAND_WIDTH,hbox,(int) black,(int) white,"xpicker");
		
	/* make GC for image */
	gci = XCreateGC(dpy,win,0,NULL);

	/* copy this stuff for garnish's stupid global variables */
	display=dpy;
	window=win;
	gc=XCreateGC(dpy,win,0,NULL);
	
	/* GC for cross hairs */
	rgc = XCreateGC(dpy,win,0,NULL);
	XSetFunction(dpy,rgc,GXxor);
	XSetForeground(dpy,rgc,0xffffff);
	XSetBackground(dpy,rgc,0);
	mask = BlackPixel(dpy,scr) ^ WhitePixel(dpy,scr);
	XSetPlaneMask(dpy,rgc,mask);

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
		Button2MotionMask |
		Button3MotionMask);
	
	/* map window */
	XMapWindow(dpy,win);
	
	/* clear the window */
	XClearWindow(dpy,win);
					
	/* determine good size for axes box */
	xSizeAxesBox(dpy,win,
		labelfont,titlefont,style,
		&x,&y,&width,&height);
	width-=COMMAND_WIDTH;  /*had to make room for command buttons on left*/
	
	/* note that image is out of date */
	imageOutOfDate = 1;
	
	strncpy(pick_fname,mpicks,sizeof(pick_fname));
	num_wiggles=n2;

	init_stuff(wbox,num_wiggles,&filename_input,pick_fname,
		   &control_mode,&edit_mode,&cross_mode); 

	/* main event loop */
	while(num_wiggles|(~num_wiggles)/*True*/) {
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
			/*had to make room for command buttons on left*/
			width-=COMMAND_WIDTH;
			
			/* clear the window */
			XClearWindow(dpy,win);
			
			init_stuff(winwidth,num_wiggles,
				   &filename_input,pick_fname,
				   &control_mode,&edit_mode,&cross_mode);
			
			/* note that image is out of date */
			imageOutOfDate = 1;

		/* else if window exposed */
		} else if (event.type==Expose) {
			
			/* clear all expose events from queue */
			while (XCheckTypedEvent(dpy,Expose,&event));
			
			draw_command_bar(winwidth,filename_input,pick_fname,
					 control_mode,edit_mode, cross_mode);
			/* if necessary, make new image */
			if (imageOutOfDate) {
				if (image!=NULL) {
					/* free1(image->data); */
					XDestroyImage(image);
				}
				image = newBitmap(dpy,width,height,
					n1,d1,f1,n2,x2,z,
					x1begb,x1endb,x2begb,x2endb,
					xcur,clip,wt,va,
					&p2beg,&p2end,endian,interp);
				imageOutOfDate = 0;
			}
	
			/* draw image (before axes so grid lines visible) */
			XPutImage(dpy,win,gci,image,0,0,x,y,
				image->width,image->height);
			
			/* draw axes on top of image */
			xDrawAxesBox(dpy,win,
				x,y,width,height,
				x1begb,x1endb,0.0,0.0,
				d1num,f1num,n1tic,grid1,label1,
				x2begb,x2endb,p2beg,p2end,
				d2num,f2num,n2tic,grid2,label2,
				labelfont,title,titlefont,
				labelcolor,titlecolor,gridcolor,
				style);

			if (control_mode==PICK_MODE) {
			  /*display pick points if in pick mode*/
			  for(i=0;i<pickdimax;i++)
			    if((*apick)[i].picked)
			      draw_pick(dpy,win,gc,
					*apick,i,
					x,y,
					width,height,
					x1begb,x1endb,
					x2begb,x2endb,
					p2beg,p2end);
			}

		/* else if key down */
		} else if (event.type==KeyPress) {

			XLookupString( (XKeyEvent*) &event,
					(char*) keybuf,
					(int) 1,
					(KeySym*) &keysym,
					(XComposeStatus*) &keystat);

			if(control_mode==REGULAR_MODE) {
			  if (keysym==XK_s) {
			    xMousePrint(event,style,
					mpicksfp, x,y,width,height,
					x1begb,x1endb,x2begb,x2endb);
			  } else if (keysym==XK_l ) {
				/* set lock */		  
			     lock = 1 ;
			  if (verbose) warn("zoom lock set  %d\n",lock);

 			} else if (keysym==XK_u ) {
				/* unset lock */		  
			     lock = 0 ;
			  if (verbose) warn("zoom lock released %d\n",lock);

 			} else if (keysym==XK_Left ) {
 			  /* move zoom box to left by half window width */
			  mve = (x2endb - x2begb)/2. ;
			  x2begb = x2begb - mve ;
			  x2endb = x2endb - mve ;
			  msg="move "; 
			  /* check for bounds of full window */
			  if (x2begb < x2beg) {
			    if ( lock ) { x2begb = x2begb + mve ;
			                  x2endb = x2endb + mve ;
					  msg="limit ";
					  mve=0;
			    } else { x2begb = x2beg ;}
			  }
			  
			  if (verbose) fprintf(stderr,"%s %g\n",msg,mve);

			  /* clear area and force an expose event */
			  XClearArea(dpy,win,0,0,0,0,True);
			  /* note that image is out of date */
			  imageOutOfDate = 1;
								
			} else if (keysym==XK_Right ) {
			  /* move zoom box to right by half window width*/
			  mve = (x2endb - x2begb)/2. ;
			  x2begb = x2begb + mve ;
			  x2endb = x2endb + mve ;
			  msg="move "; 
			  /* check for bounds of full window */
			  if (x2endb > x2end) {
			    if ( lock ) { x2begb = x2begb - mve ;
			                  x2endb = x2endb - mve ;
					  msg="limit ";
					  mve=0;
			    } else { x2endb = x2end ;}
			  }
			  if (verbose) fprintf(stderr,"%s %g\n",msg,mve);

			  /* clear area and force an expose event */
			  XClearArea(dpy,win,0,0,0,0,True);
			  /* note that image is out of date */
			  imageOutOfDate = 1;
								
			} else if (keysym==XK_Down ) {
			  /* move zoom box down by half window height */
			  mve = (x1endb - x1begb)/2. ;
			  x1begb = x1begb + mve ;
			  x1endb = x1endb + mve ;
			  msg="move "; 
			  /* check for bounds of full window */
			  if (x1endb > x1end) {
			    if ( lock ) {  x1begb = x1begb - mve ;
			                   x1endb = x1endb - mve ;
					  msg="limit ";
					  mve=0;
			    } else { x1endb = x1end ;}
			  }
			  if (verbose) fprintf(stderr,"%s %g\n",msg,mve);

			  /* clear area and force an expose event */
			  XClearArea(dpy,win,0,0,0,0,True);
			  /* note that image is out of date */
			  imageOutOfDate = 1;
								
			} else if (keysym==XK_Up ) {
			  /* move zoom box down by half window height */
			  mve = (x1endb - x1begb)/2. ;
			  x1begb = x1begb - mve ;
			  x1endb = x1endb - mve ;
			  msg="move "; 
			  /* check for bounds of full window */
			  if (x1begb < x1beg) {
			    if ( lock ) { x1begb = x1begb + mve ;
			                  x1endb = x1endb + mve ;
					  msg="limit ";
					  mve=0;
			    } else { x1begb = x1beg ;}
			  }
			  if (verbose) fprintf(stderr,"%s %g\n",msg,mve);
				
			/* clear area and force an expose event */
			XClearArea(dpy,win,0,0,0,0,True);
			
			/* note that image is out of date */
			imageOutOfDate = 1;
								
			} else if (keysym==XK_o || keysym==XK_KP_Subtract ) {
			  /* zoom out .... vertical*/
			  mve = (x1endb - x1begb)/2. ;
			  x1begb = x1begb - mve ;
			  x1endb = x1endb + mve ;
			  /* check for bounds of full window */
			  if (x1begb < x1beg) x1begb = x1beg ;
			  if (x1endb > x1end) x1endb = x1end ;
			  /*   .... and horizontal */
			  mve = (x2endb - x2begb)/2. ;
			  x2begb = x2begb - mve ;
			  x2endb = x2endb + mve ;
			  /* check bounds of original image */
			  if (x2begb < x2beg) x2begb = x2beg ;
			  if (x2endb > x2end) x2endb = x2end ;
			 	
			  /* clear area and force an expose event */
		   	  XClearArea(dpy,win,0,0,0,0,True);
			 
			  /* note that image is out of date */
			  imageOutOfDate = 1;
								
			} else if (keysym==XK_i || keysym==XK_KP_Add ) {
			  /* zoom in .... vertical*/
			  mve = (x1endb - x1begb)/4. ;
			  x1begb = x1begb + mve ;
			  x1endb = x1endb - mve ;
			  /*   .... and horizontal */
			  mve = (x2endb - x2begb)/4. ;
			  x2begb = x2begb + mve ;
			  x2endb = x2endb - mve ;

			  /* clear area and force an expose event */
			  XClearArea(dpy,win,0,0,0,0,True);
			 
			  /* note that image is out of date */
			  imageOutOfDate = 1;
								
			} else if (keysym==XK_c || keysym==XK_Page_Down) {
		  		
				/* Change clip for image */
 		       		clip += clip/10. ;
				if (verbose) warn("clip=%g\n",clip);
 				/* note that image is out of date */
				 imageOutOfDate = 1;				
				 
			} else if (keysym==XK_a || keysym==XK_Page_Up) {

				/* Change clip for image */
			        clip -= clip/10. ;
				if (verbose) warn("clip=%g\n",clip);
				/* note that image is out of date */
				imageOutOfDate = 1;				

			} else if (keysym==XK_q || keysym==XK_Q) {
			    /* This is the exit from the event loop */
			    break;
			  } else {
			    continue;
			  }
			} else {	   /* ==== must be pick mode ===== */
			  if(keysym==XK_Delete || keysym==XK_BackSpace)
			    DeleteTextSetChar(0);
			  else if (keysym==XK_Left ) {
 			  /* move zoom box to left by half window width */
			  mve = (x2endb - x2begb)/2. ;
			  x2begb = x2begb - mve ;
			  x2endb = x2endb - mve ;
			  msg="move "; 
			  /* check for bounds of full window */
			  if (x2begb < x2beg) {
			    if ( lock ) { x2begb = x2begb + mve ;
			                  x2endb = x2endb + mve ;
					  msg="limit ";
					  mve=0;
			    } else { x2begb = x2beg ;}
			  }
			  
			  if (verbose) fprintf(stderr,"%s %g\n",msg,mve);

			  /* clear area and force an expose event */
			  XClearArea(dpy,win,0,0,0,0,True);
			  /* note that image is out of date */
			  imageOutOfDate = 1;
								
			} else if (keysym==XK_Right ) {
			  /* move zoom box to right by half window width*/
			  mve = (x2endb - x2begb)/2. ;
			  x2begb = x2begb + mve ;
			  x2endb = x2endb + mve ;
			  msg="move "; 
			  /* check for bounds of full window */
			  if (x2endb > x2end) {
			    if ( lock ) { x2begb = x2begb - mve ;
			                  x2endb = x2endb - mve ;
					  msg="limit ";
					  mve=0;
			    } else { x2endb = x2end ;}
			  }
			  if (verbose) fprintf(stderr,"%s %g\n",msg,mve);

			  /* clear area and force an expose event */
			  XClearArea(dpy,win,0,0,0,0,True);
			  /* note that image is out of date */
			  imageOutOfDate = 1;
								
			} else if (keysym==XK_Down ) {
			  /* move zoom box down by half window height */
			  mve = (x1endb - x1begb)/2. ;
			  x1begb = x1begb + mve ;
			  x1endb = x1endb + mve ;
			  msg="move "; 
			  /* check for bounds of full window */
			  if (x1endb > x1end) {
			    if ( lock ) {  x1begb = x1begb - mve ;
			                   x1endb = x1endb - mve ;
					  msg="limit ";
					  mve=0;
			    } else { x1endb = x1end ;}
			  }
			  if (verbose) fprintf(stderr,"%s %g\n",msg,mve);

			  /* clear area and force an expose event */
			  XClearArea(dpy,win,0,0,0,0,True);
			  /* note that image is out of date */
			  imageOutOfDate = 1;
								
			} else if (keysym==XK_Up ) {
			  /* move zoom box down by half window height */
			  mve = (x1endb - x1begb)/2. ;
			  x1begb = x1begb - mve ;
			  x1endb = x1endb - mve ;
			  msg="move "; 
			  /* check for bounds of full window */
			  if (x1begb < x1beg) {
			    if ( lock ) { x1begb = x1begb + mve ;
			                  x1endb = x1endb + mve ;
					  msg="limit ";
					  mve=0;
			    } else { x1begb = x1beg ;}
			  }
			  if (verbose) fprintf(stderr,"%s %g\n",msg,mve);
				
			/* clear area and force an expose event */
			XClearArea(dpy,win,0,0,0,0,True);
			
			/* note that image is out of date */
			imageOutOfDate = 1;
								
			} else if (keysym == XK_Shift_L || 
				   keysym == XK_Shift_R || 
				   keysym == XK_Control_L ||
				   keysym == XK_Control_R ||
				   keysym == XK_Return)
			    continue;
			  else {
			    AddTextSetChar(0,keybuf[0]);
			  }
			}
		/* else if button down (1 == zoom, 2 == mouse tracking */
		} else if (event.type==ButtonPress) {
		  if(event.xbutton.x>x+width) {
		    check_buttons(dpy,win,rgc,event,
				  apick,num_wiggles,
				  x,y,
				  winwidth,height,
				  x1begb,x1endb,x2begb,x2endb,
				  p2beg,p2end,
				  filename_input,
				  pick_fname,
				  &control_mode,
				  &edit_mode,
				  &cross_mode,
				  &pickdimax,
				  &pickdimend,
			          verbose,
				  x1x2);
		  }
			/* if 1st button: zoom */
		  else if (event.xbutton.button==Button1) {
		      /* track pointer and get new box */
		    draw_cross(dpy,win,blue_r_gc,x,y,width,height,
			       event.xbutton.x,event.xbutton.y,cross_mode,
			       ERASE_FLAG);		    
				xRubberBox(dpy,win,event,&xb,&yb,&wb,&hb);
			
				/* if new box has tiny width or height */
				if (wb<4 || hb<4) {
				
					/* reset box to initial values */
					x1begb = x1beg;
					x1endb = x1end;
					x2begb = x2beg;
					x2endb = x2end;
			
				/* else, if new box has non-zero width */
				/* if new box has zero width or height */
				} else {
			
					/* calculate new box parameters */
					zoomBox(x,y,width,height,
						xb,yb,wb,hb,
						x2begb,x2endb,
						x1begb,x1endb,
						&x2begb,&x2endb,
						&x1begb,&x1endb);
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

			} else {       /*3rd button*/
				edit_pick(dpy,win,gc,event,
				    style,x,y,
				    width,height,
				    apick,num_wiggles,
				    x1begb,x1endb,
				    x2begb,x2endb,
				    p2beg,p2end,
				    control_mode,
				    edit_mode,
				    cross_mode,
				    &pickdimax,
				    &pickdimend,
				    ioff,
				    off,verbose);
			}

		/* else if pointer has moved */
		} else if (event.type==MotionNotify) {
		  draw_cross(dpy,win,blue_r_gc,x,y,width,height,
			     event.xbutton.x,event.xbutton.y,cross_mode,
			     DRAW_FLAG);
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

/*draw a crosshair at coordinates*/
static void draw_cross(Display *dpy, Window win, GC gc, int x, int y,
		int w, int h, int mx, int my, int cross_mode, int draw_f)
{
	static int oldx, oldy, first_time=1;

	if(!cross_mode)
		return;

	if((mx < x) || (mx > x+w) || (my < y) || (my > y+h)) {
		if(!first_time) {
			first_time = TRUE;
			XDrawLine(dpy,win,gc,x,oldy,x+w,oldy);
			XDrawLine(dpy,win,gc,oldx,y,oldx,y+h);
    		}
		return;
	}

	if(first_time) {
		first_time=0;
		oldx = mx;
		oldy = my;
		XDrawLine(dpy,win,gc,x,my,x+w,my);
		XDrawLine(dpy,win,gc,mx,y,mx,y+h);
	} else {
		XDrawLine(dpy,win,gc,x,oldy,x+w,oldy);
		XDrawLine(dpy,win,gc,oldx,y,oldx,y+h);
		oldx = mx;
		oldy = my;

		if(draw_f==DRAW_FLAG) {
			XDrawLine(dpy,win,gc,x,my,x+w,my);
			XDrawLine(dpy,win,gc,mx,y,mx,y+h);
		} else {
			first_time=1;
		}
  	}
}
	  
/* update parameters associated with zoom box */
static void zoomBox (int x, int y, int w, int h, 
	int xb, int yb, int wb, int hb,
	float x1, float x2,
	float y1, float y2,
	float *x1b, float *x2b,
	float *y1b, float *y2b)
{
	/* if width and/or height of box are zero, just copy values */
	if (wb==0 || hb==0) {
		*x1b = x1; *x2b = x2;
		*y1b = y1; *y2b = y2;
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
	
	/* determine box limits */
	*x1b = x1+(xb-x)*(x2-x1)/w;
	*x2b = x1+(xb+wb-x)*(x2-x1)/w;
	*y1b = y1+(yb-y)*(y2-y1)/h;
	*y2b = y1+(yb+hb-y)*(y2-y1)/h;
}

/* return pointer to new image bitmap of rasterized wiggles */
static XImage *newBitmap (Display *dpy, int width, int height,
	int n1, float d1, float f1, int n2, float *x2, float *z,
	float x1beg, float x1end, float x2beg, float x2end,
	float xcur, float clip, int wt, int va,
	float *p2begp, float *p2endp, int endian, int interp)
{
	int widthpad,nbpr,i1beg,i1end,if1r,n1r,b1fz,b1lz,i2,i,n2in;
	float x2min,x2max,p2beg,p2end,bscale,boffset,bxcur,bx2;
	unsigned char *bits;
	int scr=DefaultScreen(dpy);
	int bitmap_pad=0;

        /* Kludge to fix problem with XCreateImage introduced in */
        /* Xorg 7.0 update for security */
        if (BitmapPad(dpy)>16) {
                bitmap_pad = 16;
        } else if (BitmapPad(dpy) < 16) {
                bitmap_pad = 8;
        }
	
	/* return pointer to image */
	/* determine bitmap dimensions and allocate space for bitmap */
	widthpad = (1+(width-1)/(bitmap_pad))*bitmap_pad;
	nbpr = widthpad-1;
	bits = ealloc1(nbpr*height,sizeof(unsigned char));
	for (i=0; i<nbpr*height; ++i) bits[i] = 0;

	/* determine number of traces that fall within axis 2 bounds */
	x2min = MIN(x2beg,x2end);
	x2max = MAX(x2beg,x2end);
	for (i2=0,n2in=0; i2<n2; i2++)
		if (x2[i2]>=x2min && x2[i2]<=x2max) n2in++;

	/* determine pads for wiggle excursion along axis 2 */
	xcur = fabs(xcur);
	if (n2in>1) xcur *= (x2max-x2min)/(n2in-1);
	p2beg = (x2end>=x2beg)?-xcur:xcur;
	p2end = (x2end>=x2beg)?xcur:-xcur;

	/* determine scale and offset to map x2 units to bitmap units */
	bscale = (width-1)/(x2end+p2end-x2beg-p2beg);
	boffset = -(x2beg+p2beg)*bscale;
	bxcur = xcur*bscale;

	/* adjust x1beg and x1end to fall on sampled values */
	i1beg = NINT((x1beg-f1)/d1);
	i1beg = MAX(0,MIN(n1-1,i1beg));
	x1beg = f1+i1beg*d1;
	i1end = NINT((x1end-f1)/d1);
	i1end = MAX(0,MIN(n1-1,i1end));
	x1end = f1+i1end*d1;

	/* determine first sample and number of samples to rasterize */
	if1r = MIN(i1beg,i1end);
	n1r = MAX(i1beg,i1end)-if1r+1;

	/* determine bits corresponding to first and last samples */
	b1fz = (x1end>x1beg)?0:height-1;
	b1lz = (x1end>x1beg)?height-1:0;

	/* rasterize traces */
	for (i2=0; i2<n2; i2++,z+=n1) {

		/* skip traces not in bounds */
		if (x2[i2]<x2min || x2[i2]>x2max) continue;

		/* determine bitmap coordinate of trace */
		bx2 = boffset+x2[i2]*bscale;

		/* rasterize one trace */
		if (interp==0) { /* don't use interpolation */
			rfwtva(n1r,&z[if1r],-clip,clip,va?0:clip,
				(int)(bx2-bxcur),(int)(bx2+bxcur),b1fz,b1lz,
				wt,nbpr,bits,endian);
		} else { /* Use sinc interpolation */
			rfwtvaint(n1r,&z[if1r],-clip,clip,va?0:clip,
				(int)(bx2-bxcur),(int)(bx2+bxcur),b1fz,b1lz,
				wt,nbpr,bits,endian);
		}
	}
	
	/* return axis 2 pads */
	*p2begp = p2beg;  *p2endp = p2end;

	return XCreateImage((Display*) dpy,
			    (Visual*) DefaultVisual(dpy,scr),
			    (unsigned int) 1,
			    (int) XYBitmap,
			    (int) 0,
			    (char*) bits,
			    (unsigned int) widthpad,
			    (unsigned int) height,
			    (int) bitmap_pad,
			    (int) nbpr);
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
	sprintf(string,"(%0.6g,%0.6g)",x1,x2);
	XTextExtents(fs,string,(int) strlen(string),&dummy,&dummy,&dummy,&overall);
	XDrawString(dpy,win,gc,xoffset,yoffset+overall.ascent,
		string,(int) strlen(string));
}

void xMousePrint(XEvent event, int style,
	FILE *mpicksfp, int x, int y, int width, int height,
	float x1begb, float x1endb, float x2begb, float x2endb)
{
	float x1,x2;

	/* convert mouse location to (x1,x2) coordinates */
	if (style==NORMAL) {
		x1 = x1begb+(x1endb-x1begb)*(event.xmotion.x-x)/width;
		x2 = x2endb+(x2begb-x2endb)*(event.xmotion.y-y)/height;
	} else {
		x1 = x1begb+(x1endb-x1begb)*(event.xmotion.y-y)/height;
		x2 = x2begb+(x2endb-x2begb)*(event.xmotion.x-x)/width;
	}

	/* write string indicating mouse location */
	fprintf(mpicksfp, "%0.6g  %0.6g\n", x1, x2);
}

void init_stuff(int winwidth,int num_wiggles,
		TextSet **filename_input, char *pick_fname,
		int *control_mode,int *edit_mode, int *cross_mode)
{
	static int first_time=TRUE;

	if(first_time && /*True*/(pick_fname == (pick_fname+0*num_wiggles)) ) {
		first_time=FALSE;
		screen=DefaultScreen(display);
		visual=DefaultVisual(display,screen);
		foreground=BlackPixel(display,screen);
		background=WhitePixel(display,screen);
		colormap=DefaultColormap(display,screen);
    
 		grey_color.flags=COLOR_FLAGS;
		grey_color.red = grey_color.green = grey_color.blue = BUTTON_BRIGHTNESS;
		XAllocColor(display,colormap,&grey_color);
		grey_pixel = grey_color.pixel;

		black_color.flags=COLOR_FLAGS;
		black_color.red = black_color.green = black_color.blue = 0;
		XAllocColor(display,colormap,&black_color);
		black_pixel = black_color.pixel;

		red_color.flags=COLOR_FLAGS;
		red_color.red=65000; red_color.green=red_color.blue=0;
		XAllocColor(display,colormap,&red_color);
		red_pixel = red_color.pixel;

		blue_color.flags=COLOR_FLAGS;
		blue_color.blue=65000; blue_color.green=blue_color.red=0;
		XAllocColor(display,colormap,&blue_color);
		blue_pixel = blue_color.pixel;

		blue_r_gc = XCreateGC(display,window,0,NULL);
		XSetFunction(display,blue_r_gc,GXxor);
		XSetForeground(display,blue_r_gc,blue_pixel);

		red_r_gc = XCreateGC(display,window,0,NULL);
		XSetFunction(display,red_r_gc,GXxor);
		XSetForeground(display,red_r_gc,red_pixel);

		font_struct=XLoadQueryFont(display,FONT_NAME);

		if(!font_struct)
			err("Cannot allocate font '%s'.",FONT_NAME);

		char_width=font_struct->max_bounds.width;
		char_height=font_struct->ascent+font_struct->descent;
		font=XLoadFont(display, FONT_NAME);
		XSetFont(display,gc,font);

		*control_mode = REGULAR_MODE;    
		*edit_mode = ADD_MODE;
		*cross_mode = 0;

		*filename_input = (TextSet *)CreateTextSet(window,
			       winwidth-COMMAND_WIDTH-25,BUTTON_HEIGHT*1,
			       1,0,12,font,char_width,char_height+5,
			       black_pixel,grey_pixel);

		SetTextSetLine(*filename_input,0);
		SetCurrentTextSet(*filename_input,UP);


	} else {
		if(*filename_input) {
			free(*filename_input);
		}

		*filename_input = (TextSet *)CreateTextSet(window,
			       winwidth-COMMAND_WIDTH-25,BUTTON_HEIGHT*1,
			       1,0,12,font,char_width,char_height+5,
			       black_pixel,grey_pixel);
		SetTextSetLine(*filename_input,0);
	}
    
	/* make sure fg,bg are what xpicker expects */
	/* garnish may have mauled them */
	XSetForeground(display,gc,BlackPixel(display,screen));
	XSetBackground(display,gc,WhitePixel(display,screen));
}
 
void save_picks(pick_t **apick, int num_wiggles, char *fname,
	     int *pickdimax, int *pickdimend, int verbose, int x1x2)
{
	FILE *fp;
	int i=num_wiggles/*dummy*/;
	int num_picks=0;

	fp=fopen(fname,"w+");
	if(fp == NULL)
		err("Could not open pick output file '%s'",fname);

	

	if (verbose) warn("save: end %d max %d ",*pickdimend,*pickdimax);
 	for(i=0;i< *pickdimax;i++) {
		if((*apick)[i].picked)  {
		  num_picks++;
	if (verbose) warn("save: ar%d pick%d %f %f %d ", i,num_picks,
			(*apick)[i].x2, (*apick)[i].time,(*apick)[i].picked);
		}
	}

	/* fprintf(fp,"%d\n",num_picks); */
	/* don't print number of picks */

	for(i=0;i<*pickdimax;i++) {
		if((*apick)[i].picked) {
			if (x1x2==0) {
			fprintf(fp,"%f %f\n",(*apick)[i].x2,(*apick)[i].time);
			} else {
			fprintf(fp,"%f %f\n",(*apick)[i].time,(*apick)[i].x2);
			}
    		}
	}

	fclose(fp);
	warn("Pick output successful");
}

void load_picks(pick_t **apick, int num_wiggles, char *fname,
      int *pickdimax, int *pickdimend, int verbose, int x1x2)
{
	FILE *fp;
	int i,num_picks = num_wiggles/*dummy*/;
	float xval,time;

	if((fp=fopen(fname,"r+")) == NULL)
		err("Could not open pick input file '%s'",fname);

	 /* fscanf(fp,"%d\n",&num_picks); */
/* dynamically reallocate memory for picks if necessary  - Bill Lutter    */
     realloc_picks(apick, num_picks, pickdimax, pickdimend,verbose);


	for(i=0;i<num_picks;i++) {
		++*pickdimend;
		if (x1x2==0) {
			fscanf(fp,"%f %f\n",&xval,&time);
		} else {
			fscanf(fp,"%f %f\n",&time,&xval);
		}
		(*apick)[*pickdimend-1].picked=TRUE;
		(*apick)[*pickdimend-1].x2=xval;
		(*apick)[*pickdimend-1].time=time;
       printf("load: %d %f %f %d \n",*pickdimend,(*apick)[*pickdimend-1].x2,
	   (*apick)[*pickdimend-1].time,(*apick)[*pickdimend-1].picked);
	}
	printf("load: end %d max %d  \n",*pickdimend,*pickdimax);

	fclose(fp);
	warn("Pick input successful");
}

void edit_pick(Display *dpy, Window win, GC gc, XEvent event,
	       int style, int x, int y,
	       int width, int height,
	       pick_t **apick, int num_wiggles,
	       float x1begb, float x1endb,
	       float x2begb, float x2endb,
	       float p2beg, float p2end,
	       int control_mode,
	       int edit_mode,
	       int cross_mode,
	       int *pickdimax,
	       int *pickdimend,
	       int ioff, float *off, int verbose)
{
  
		int done=FALSE;
		float fy;
		int oldy=num_wiggles/*dummy*/;
		int pick_num;
		int pick_x;

		float scale,base;
/*    Added by bill Lutter	   */
		int ihead = -1;
		
		if((control_mode==PICK_MODE) &&
			(event.xbutton.x > x) &&
			(event.xbutton.x < x+width) &&
			(event.xbutton.y > y) &&
			(event.xbutton.y < y+height)) {

			if(edit_mode==DELETE_MODE) {
				pick_num = (int)(p2beg+x2begb+(p2end+x2endb-x2begb-p2beg)*
			       (event.xbutton.x-x)/width+0.5);
				fy=x1begb+(x1endb-x1begb)*(event.xbutton.y-y)/height;

/* existence of x2file= file with x-axis header values is
   indicated by positive values of "ioff".  If file does not exist
   then ioff is negative.  Then use mouse derived x-axis value for pick.  */
				del_pick(apick, pickdimax, pickdimend, off, ioff, 
				   fy, &pick_num, &ihead,verbose);

		       if ( ihead >= 0 && ihead < *pickdimend )  {
				if((*apick)[ihead].picked) {
					draw_pick(dpy,win,red_r_gc,*apick,ihead,
						x,y, width,height, x1begb,
						x1endb, x2begb,x2endb,
						p2beg,p2end);
					(*apick)[ihead].picked=FALSE;
      				}
		 	}

			} else {
				fy=x1begb+(x1endb-x1begb)*(event.xbutton.y-y)/height;
				pick_num = (int)(p2beg+x2begb+(p2end+x2endb-x2begb-p2beg)*
				(event.xbutton.x-x)/width+0.5);
				add_pick(apick, pickdimax, pickdimend, off, ioff, 
				   fy, &pick_num, &ihead,verbose);
 
/*			       if((*apick)[pick_num-1].picked) {
					draw_pick(dpy,win,red_r_gc,*apick,pick_num,
							x,y, width,height,
		 					x1begb,x1endb,
							x2begb,x2endb,
							p2beg,p2end);
				}   */
      
			scale = width/(x2endb+p2end-x2begb-p2beg);
			base = x-scale*(x2begb+p2beg);
			pick_x = base+scale*pick_num-0.5;

			oldy=event.xbutton.y;
			draw_seg(dpy,win,gc,pick_x,oldy);

			xMouseLoc(dpy,win,event,style,True,
				x,y,width,height,x1begb,x1endb,
				x2begb,x2endb,p2beg,p2end);

			while(!done) {
				XSync(display,0);
				XNextEvent(display, &event);
				if(event.type == MotionNotify) {
	  				draw_cross(dpy,win,red_r_gc,x,y,
							width,height,
		     					event.xmotion.x,
							event.xmotion.y,
							cross_mode,DRAW_FLAG);
	  				fy=x1begb+(x1endb-x1begb)*(event.xmotion.y-y)/height;
	  				draw_seg(dpy,win,gc,pick_x,oldy);
	  				oldy=event.xmotion.y;
	  				draw_seg(dpy,win,gc,pick_x,oldy);
	  				xMouseLoc(dpy,win,event,style,True,
		    				x,y,width,height,x1begb,x1endb,
		    				x2begb,x2endb,p2beg,p2end);
				}
				if(event.type == ButtonRelease) {
					done=TRUE;
				}
      			}
		}
	}
}

void draw_pick(Display *dpy, Window win, GC gc, pick_t *pick, int i,
	       int xmargin, int ymargin,
	       int width, int height,
	       float x1begb, float x1endb,
	       float x2begb, float x2endb,
	       float p2beg, float p2end)
{
	int x,y;
	float scale,base;

	scale = width/(x2endb+p2end-x2begb-p2beg);
	base = xmargin-scale*(x2begb+p2beg);
/*	x = base+scale*i-0.5;     */
  	x = base+scale*(pick[i].x2)-0.5;       
	/* y=ymargin+x1begb+(pick[i].time-x1begb)/(x1endb-x1begb)*height; */
	y=ymargin+(pick[i].time-x1begb)/(x1endb-x1begb)*height; /* xxxc */
	printf("DRAW: %d %f %f \n",i,pick[i].x2,pick[i].time);
	draw_seg(dpy,win,gc,x,y);
}

void check_buttons(Display *dpy, Window win, GC gc,XEvent event,pick_t **apick, 
		   int num_wiggles,
		   int x, int y,
		   int winwidth, int height,
		   float x1begb, float x1endb,
		   float x2begb, float x2endb,
		   float p2beg, float p2end,
		   TextSet *filename_input,
		   char *pick_fname,
		   int *control_mode,
		   int *edit_mode,
		   int *cross_mode,
		   int *pickdimax,
		   int *pickdimend,
	           int verbose,
		   int x1x2)
{
	int mx=event.xbutton.x;
	int my=event.xbutton.y;
	if(gc != gc)
	height += 0;  x+= 0;  y += 0; /* keep compiler happy */
	x1begb += 0.0; x1endb += 0.0; /* keep compiler happy */
	x2begb += 0.0; x2endb += 0.0; /* keep compiler happy */
	p2beg  += 0.0; p2end  += 0.0; /* keep compiler happy */

	if(mx>winwidth-COMMAND_WIDTH-10 &&
	      mx<winwidth-COMMAND_WIDTH+BUTTON_WIDTH-10) {
		if(my>BUTTON_HEIGHT*1-(char_height+10) && my<BUTTON_HEIGHT*1) {
			AddTextSetString(filename_input,"");
			filename_input->char_pos=0;
			RefreshTextSet(filename_input);
    		}
		if(my>BUTTON_HEIGHT*2 && my<BUTTON_HEIGHT*3) {    /*LOAD*/
			NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*2,
				BUTTON_WIDTH,BUTTON_HEIGHT,
				DOWN,grey_color,black_color,
				"Load",char_width,char_height,RELATIVE);
				GetTextLineString(filename_input,0,pick_fname);
			   if (verbose) warn("before load: 1: %lu 2: %lu",
					(unsigned long) *apick, (unsigned long) *apick+1);
				load_picks(apick,num_wiggles,pick_fname,
				 pickdimax, pickdimend,verbose,x1x2);
			   if (verbose) warn("after load: 1: %lu 2: %lu",
					(unsigned long) *apick, (unsigned long) *apick+1);

      			/* force expose event */
			XClearArea(dpy,win,0,0,0,0,True);	      
			NewButton(window,winwidth-COMMAND_WIDTH-10,
				BUTTON_HEIGHT*2,
				BUTTON_WIDTH,BUTTON_HEIGHT,
				UP,grey_color,black_color,
				"Load",char_width,char_height,RELATIVE);
    		}
		if(my>BUTTON_HEIGHT*4 && my<BUTTON_HEIGHT*5) {   /*save*/
			NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*4,
				BUTTON_WIDTH,BUTTON_HEIGHT,
				DOWN,grey_color,black_color,
				"Save",char_width,char_height,RELATIVE);
				GetTextLineString(filename_input,0,pick_fname);
			   if (verbose) warn("before save: 1: %lu 2: %lu",
				(unsigned long) *apick, (unsigned long) *apick+1);
      				save_picks(apick,num_wiggles,pick_fname,
				 pickdimax,pickdimend,verbose,x1x2);
			   if (verbose) warn("after save: 1: %lu 2: %lu",
				(unsigned long) *apick,(unsigned long) *apick+1);
			NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*4,
				BUTTON_WIDTH,BUTTON_HEIGHT,
				UP,grey_color,black_color,
				"Save",char_width,char_height,RELATIVE);
		}
    
		if(my>BUTTON_HEIGHT*6 && my<BUTTON_HEIGHT*7) {   /*control mode*/
				*control_mode=!(*control_mode);
      			if(*control_mode==PICK_MODE) {
				SetCurrentTextSet(filename_input,DOWN);
			} else {
				SetCurrentTextSet(filename_input,UP);
      			}
			/* force expose event */
			XClearArea(dpy,win,0,0,0,0,True);	
			NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*6,
				BUTTON_WIDTH,BUTTON_HEIGHT,
				UP,grey_color,black_color,
				(*control_mode==PICK_MODE) ? "Pick" : "View only",
				char_width,char_height,RELATIVE);
    		}

		if(my>BUTTON_HEIGHT*8 && my<BUTTON_HEIGHT*9) {  /*edit mode*/
			*edit_mode = !(*edit_mode);
			NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*8,
				BUTTON_WIDTH,BUTTON_HEIGHT,
				UP,grey_color,black_color,
				(*edit_mode==ADD_MODE) ? "Add" : "Delete",
				char_width,char_height,RELATIVE);
    		}

		if(my>BUTTON_HEIGHT*10 && my<BUTTON_HEIGHT*11) {  /*crosshair toggle*/
			*cross_mode = !(*cross_mode);
			NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*10,
				BUTTON_WIDTH,BUTTON_HEIGHT,
				UP,grey_color,black_color,
				(*cross_mode) ? "Cross On" : "Cross Off",
				char_width,char_height,RELATIVE);
		}
	}
}

void draw_command_bar(int winwidth, TextSet *filename_input,
		      char *pick_fname, int control_mode, int edit_mode,
		      int cross_mode)
{

	/* load button */
	NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*2,
			BUTTON_WIDTH,BUTTON_HEIGHT,
			UP,grey_color,black_color,
			"Load",char_width,char_height,RELATIVE);

	/* save button */
	NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*4,
			BUTTON_WIDTH,BUTTON_HEIGHT,
			UP,grey_color,black_color,
			"Save",char_width,char_height,RELATIVE);

	/* command-mode button */
	NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*6,
			BUTTON_WIDTH,BUTTON_HEIGHT,
			UP,grey_color,black_color,
			(control_mode==PICK_MODE) ? "Pick" : "View Only",
			char_width,char_height,RELATIVE);
  
	/* edit-mode button */
	NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*8,
			BUTTON_WIDTH,BUTTON_HEIGHT,
			UP,grey_color,black_color,
			(edit_mode==ADD_MODE) ? "Add" : "Delete",
			char_width,char_height,RELATIVE);

	/* cross-hair toggle */
	NewButton(window,winwidth-COMMAND_WIDTH-10,BUTTON_HEIGHT*10,
			BUTTON_WIDTH,BUTTON_HEIGHT,
			UP,grey_color,black_color,
			(cross_mode) ? "Cross On" : "Cross Off",
			char_width,char_height,RELATIVE);

	AddTextSetString(filename_input,pick_fname);
	if(control_mode==PICK_MODE) 
		SetCurrentTextSet(filename_input,DOWN);
	else
		SetCurrentTextSet(filename_input,UP);
		RefreshTextSet(filename_input);

	/* make sure fg,bg are what xpicker expects */
	/* garnish may have mauled them */
	XSetForeground(display,gc,BlackPixel(display,screen));
	XSetBackground(display,gc,WhitePixel(display,screen));
}

void draw_seg(Display *dpy, Window win, GC gc, int x, int y)
{
	XDrawLine(dpy,win,gc,x-10,y+5,x,y);
	XDrawLine(dpy,win,gc,x-10,y-5,x,y);
}

float *read_keyval(char *x2file,int *ioff,int verbose)
{
	char line[80];
	float off_tmp;
	float *off;     /* pointer for 1-d float array */
	int keyvalnum;
	FILE *infpoff;

/* Read in header values (allowable pick.x2 values and also
   allowable values (for these traces) in pick file  */
	  keyvalnum = 10;
	  infpoff = fopen(x2file,"r");
	  off = (float *) malloc(keyvalnum*sizeof(float ));
	  if (infpoff == 0 ) {
	    *ioff = -99;
	  } else {
	    *ioff = -1; 
	    while ( fgets(line,81,infpoff) != 0 && line[0] != '\n' )
	    {
	       ++*ioff;
	       if ( *ioff >= (keyvalnum - 2 ) ) {
		  keyvalnum = 2 * keyvalnum;
		  off = (float *) realloc(off,keyvalnum*sizeof(float ));
	       }
	       sscanf(line,"%f",&off_tmp);
	       *(off + *ioff) = off_tmp;
		if (verbose) warn("x-axis value  %d %f",*ioff,off[*ioff]);
	    }
	  }
	  return(off);
}
void realloc_picks(pick_t **apick, int more, int *pickdimax, int *pickdimend,
			int verbose)
{
	    int i;   /* loop counter */
/* dynamically reallocate memory for picks  - Bill Lutter    */
	    if ( (more + *pickdimend) >=  *pickdimax - 2 )
	    {
	      *pickdimax =  2*( *pickdimax + more );
			   if (verbose) warn("before realloc: 1: %lu 2: %lu",
				(unsigned long) *apick, (unsigned long) *apick+1);
	     *apick = (pick_t *) realloc(*apick,*pickdimax*sizeof(struct pick_tag ));
			   if (verbose) warn("after realloc: 1: %lu 2: %lu\n",
				(unsigned long) *apick, (unsigned long) *apick+1);

		if(*apick==NULL)  err("Could not allocate picks!");

		for(i= *pickdimend;i< *pickdimax;i++) {
			(*apick)[i].picked=FALSE;
    		}
   	}


}
void init_picks(pick_t **apick, int *pickdimax, int *pickdimend)
{
int i = pickdimend[0]/*dummy*/;
/* dynamically allocate memory for picks  - Bill Lutter    */
		if(*apick==NULL) err("Could not allocate picks!");

		for(i=0;i<*pickdimax;i++) {
			(*apick)[i].picked=FALSE;
    		}

}
void add_pick(pick_t **apick, int *pickdimax, int *pickdimend,
	     float *off, int ioff, 
	     float fy, int *pick_num, int *ihead, int verbose)
{
	   int i, more=1;
	   float dxval, dxmin=100000.;
/* dynamically reallocate memory for picks  - Bill Lutter    */
	    realloc_picks(apick, more, pickdimax, pickdimend, verbose);
  
/* existence of x2file= file with x-axis header values is
   indicated by positive values of "ioff".  If file does not exist
   then ioff is negative.  Then use mouse derived x-axis value for pick.  */
			      if (ioff >= 0 ) {
				for (i=0;i <= ioff; i++) {
				   dxval = abs(off[i] - *pick_num);
				   if ( dxval <= dxmin) {
				      *ihead = i;
				      dxmin = dxval;   
				   }
				}   
				*pick_num = off[*ihead];
			      }   
			       (*apick)[*pickdimend].x2 = *pick_num;
			       (*apick)[*pickdimend].time = fy;
			       (*apick)[*pickdimend].picked=TRUE;
		   printf("ADD (x,t): (%d, %f) pick(i,x,t): (%d, %f, %f)\n",
		   *pick_num, fy,*pickdimend,(*apick)[*pickdimend].x2,(*apick)[*pickdimend].time);
			       ++*pickdimend;

}
void del_pick(pick_t **apick, int *pickdimax, int *pickdimend,
	     float *off, int ioff, 
	     float fy, int *pick_num, int *ihead, int verbose)
{
int i = pickdimax[0]/*dummy*/;
float dxmin=100000., dxval,dtval;
			      if (ioff >= 0 ) {
				for (i=0;i <= ioff; i++) {
				   dxval = abs(off[i] - *pick_num);
				   if ( dxval <= dxmin) {
				      *ihead = i;
				      dxmin = dxval;   
				   }
				}   
				if (*ihead >=0 )
				   *pick_num = off[*ihead];
			      }
/* search for pick to delete */   
			      dxmin = 10000000.;
			      *ihead = -1;
			      for (i=0;i < *pickdimend; i++) {
				dxval = (*apick)[i].x2 - *pick_num;
				dxval = dxval * dxval;
				dtval = fy - (*apick)[i].time; 
				dtval = dtval * dtval;
				dxval = dxval + dtval;
				 if ( dxval <= dxmin) {
				    *ihead = i;
				    dxmin = dxval;
				 } 
			       }
		   if (verbose) warn("DEL (x,t): (%d, %f) pick(i,x,t): (%d, %f, %f)",
		    *pick_num, fy,*ihead,(*apick)[*ihead].x2,(*apick)[*ihead].time);
}

