/*********************** self documentation **********************/
/*
 * FFTLAB - Motif-X based graphical 1D Fourier Transform
 *
 * Usage:  fftlab
 *
 * Caveat: you must have the Motif Developer's package to install this code
 */
/**************** end self doc ********************************/

/*
 * Credits: Dave Hale
 */

#include "Xmcwp/Xmcwp.h"

typedef struct {
	Samples *r,*i;
} CSamples;

/* GLOBAL VARIABLES USED INTERNALLY */
static Samples *fReal;		/* real part of f */
static Samples *fImag;		/* imaginary part of f */
static Samples *gReal;		/* real part of g */
static Samples *gImag;		/* imaginary part of g */
static CSamples *fComplex;	/* complex samples f */
static CSamples *gComplex;	/* Fourier transform of f */
static int nSamples;		/* number of samples */
static int originCentered;	/* non-zero if origin at nSamples/2 */

/* FUNCTIONS USED INTERNALLY */
static void CreateApplication (Widget parent);
static void editingCB (int selected, void *callData);
static void lengthCB (int selected, void *callData);
static void originChangeCB (Widget w, char * client_data, char * call_data);
static void zeroAllCB (Widget w, char * client_data, char * call_data);
static void quitCB (Widget w, char * client_data, char * call_data);
static void editDone (Samples *s);
static void newData (Samples *s);
static void zeroData (Samples *s);
static void transform (CSamples *this_fft, CSamples *that_fft);
static float determinePlotValue (CSamples *cs);
static void shiftOrigin (Samples *s);
static void fftcc(int sign, int n, complex cc[]);

/* main program */
int
main (int argc, char **argv)
{
	Widget app;
	
	/* initialize toolkit */
	app = XtInitialize(argv[0],"Fftlab",NULL,0,&argc,argv);
	
	/* create and realize the application */
	CreateApplication(app);
	XtRealizeWidget(app);
	
	/* handle events */
	XtMainLoop();

	return(EXIT_SUCCESS); /* not reached */
}

/* create all application widgets */
static void CreateApplication (Widget parent)
{
	int n;
	XmString labels;
	Widget win,frame,framee,framel,formo,formw,form,label,pushb,toggleb;
	Arg args[20];
	int nEditingButtons=4;
	char *editingButtons[] = {"Draw","Negate","Zero","None"};
	int nLengthButtons=3;
	static int lengths[]={16,32,64};
	char *lengthButtons[]={"16","32","64"};
	
	/* create main window */
	n = 0;
	win = XmCreateMainWindow(parent,"",args,n);
	XtManageChild(win);
	
	/* create form widget to hold options panel and complex samples */
	n = 0;
	formw = XmCreateForm(win,"",args,n);
	XtManageChild(formw);
	
	/* create options panel */
	n = 0;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNtopOffset,2); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomOffset,2); n++;
	formo = XmCreateForm(formw,"",args,n);
	XtManageChild(formo);
	framee = XtcwpCreateStringRadioButtons(formo,"Editing",
		nEditingButtons,editingButtons,0,
		editingCB,NULL);
	XtManageChild(framee);
	framel = XtcwpCreateStringRadioButtons(formo,"Length",
		nLengthButtons,lengthButtons,0,
		lengthCB,lengths);
	XtManageChild(framel);
	n = 0;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget,framee); n++;
	XtSetArg(args[n],XmNtopOffset,10); n++;
	XtSetValues(framel,args,n);
	n = 0;
	XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
	labels = XmStringCreateLtoR("Origin\nCentered",CHARSET);
	XtSetArg(args[n],XmNlabelString,labels); n++;
	XtSetArg(args[n],XmNalignment,XmALIGNMENT_BEGINNING); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget,framel); n++;
	XtSetArg(args[n],XmNtopOffset,10); n++;
	toggleb = XmCreateToggleButtonGadget(formo,"",args,n);
	XtManageChild(toggleb);
	XtAddCallback(	(Widget) toggleb,
			XmNvalueChangedCallback,
			(XtCallbackProc) originChangeCB,
			(XtPointer) NULL);
	n = 0;
	XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
	labels = XmStringCreateLtoR("Zero All",CHARSET);
	XtSetArg(args[n],XmNlabelString,labels); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget,toggleb); n++;
	XtSetArg(args[n],XmNtopOffset,10); n++;
	pushb = XmCreatePushButtonGadget(formo,"",args,n);
	XtManageChild(pushb);
	XtAddCallback(	(Widget) pushb,
			XmNactivateCallback,
			(XtCallbackProc) zeroAllCB,
			(XtPointer) NULL);
	n = 0;
	XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
	labels = XmStringCreateLtoR("Quit",CHARSET);
	XtSetArg(args[n],XmNlabelString,labels); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomOffset,2); n++;
	pushb = XmCreatePushButtonGadget(formo,"",args,n);
	XtManageChild(pushb);
	XtAddCallback(	(Widget) pushb,
			XmNactivateCallback,
			(XtCallbackProc) quitCB,
			(XtPointer) NULL);

	/* allocate space for complex f and g */
	fComplex = (CSamples*)malloc(sizeof(CSamples));
	gComplex = (CSamples*)malloc(sizeof(CSamples));
	
	/* create complex f */
	n = 0;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget,formo); n++;
	XtSetArg(args[n],XmNleftOffset,5); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNtopOffset,2); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,50); n++;
	frame = XmCreateFrame(formw,"",args,n);
	XtManageChild(frame);
	n = 0;
	form = XmCreateForm(frame,"",args,n);
	XtManageChild(form);
	n = 0;
	XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
	labels = XmStringCreateLtoR("f(x)",CHARSET);
	XtSetArg(args[n],XmNlabelString,labels); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNtopOffset,2); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	label = XmCreateLabelGadget(form,"label",args,n);
	XtManageChild(label);
	fComplex->r = fReal = samplesCreate(form,"Real",editDone);
	fComplex->i = fImag = samplesCreate(form,"Imaginary",editDone);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget,label); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,50); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomOffset,2); n++;
	XtSetValues(fReal->frame,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget,label); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,50); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomOffset,2); n++;
	XtSetValues(fImag->frame,args,n);
	
	/* create complex g */
	n = 0;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget,formo); n++;
	XtSetArg(args[n],XmNleftOffset,5); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomOffset,2); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,50); n++;
	frame = XmCreateFrame(formw,"",args,n);
	XtManageChild(frame);
	n = 0;
	form = XmCreateForm(frame,"",args,n);
	XtManageChild(form);
	n = 0;
	XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
	labels = XmStringCreateLtoR("F(k)",CHARSET);
	XtSetArg(args[n],XmNlabelString,labels); n++;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNtopOffset,2); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	label = XmCreateLabelGadget(form,"",args,n);
	XtManageChild(label);
	gComplex->r = gReal = samplesCreate(form,"Real",editDone);
	gComplex->i = gImag = samplesCreate(form,"Imaginary",editDone);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget,label); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftOffset,2); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,50); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomOffset,2); n++;
	XtSetValues(gReal->frame,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNtopWidget,label); n++;
	XtSetArg(args[n],XmNleftAttachment,XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,50); n++;
	XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightOffset,2); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomOffset,2); n++;
	XtSetValues(gImag->frame,args,n);
	
	/* initialize number of samples */
	nSamples = 16;
	samplesSetN(fReal,nSamples);
	samplesSetN(fImag,nSamples);
	samplesSetN(gReal,nSamples);
	samplesSetN(gImag,nSamples);
	
	/* initialize origin */
	originCentered = 0;
	samplesSetOrigin(fReal,0);
	samplesSetOrigin(fImag,0);
	samplesSetOrigin(gReal,0);
	samplesSetOrigin(gImag,0);
	
	/* initialize data */
	newData(fReal);  newData(fImag);
	newData(gReal);  newData(gImag);
	transform(fComplex,gComplex);
}

static void editingCB (int mode, void *callData)
{
	/* set edit mode */
	if(mode&(~mode)/* False */) editingCB(mode,callData);
	samplesSetEditMode(fReal,(EditMode) mode);
	samplesSetEditMode(fImag,(EditMode) mode);
	samplesSetEditMode(gReal,(EditMode) mode);
	samplesSetEditMode(gImag,(EditMode) mode);
}

static void lengthCB (int i, void *lengths)
{
	int n=((int*)(lengths))[i];
	int origin = (originCentered ? n/2 : 0);
	
	/* change length */
	nSamples = n;
	samplesSetN(fReal,n);  newData(fReal);  samplesSetOrigin(fReal,origin);
	samplesSetN(fImag,n);  newData(fImag);  samplesSetOrigin(fImag,origin);
	samplesSetN(gReal,n);  newData(gReal);  samplesSetOrigin(gReal,origin);
	samplesSetN(gImag,n);  newData(gImag);  samplesSetOrigin(gImag,origin);
	samplesDraw(fReal);
	samplesDraw(fImag);
	samplesDraw(gReal);
	samplesDraw(gImag);
}

static void originChangeCB (Widget w, char * client_data, char * call_data)
{
	int origin = (originCentered ? 0 : nSamples/2);
	if(origin&(~origin)/*False*/) originChangeCB(w,client_data,call_data);
	originCentered = !originCentered;
	shiftOrigin(fReal);
	samplesSetOrigin(fReal,origin);
	samplesDraw(fReal);
	shiftOrigin(fImag);
	samplesSetOrigin(fImag,origin);
	samplesDraw(fImag);
	shiftOrigin(gReal);
	samplesSetOrigin(gReal,origin);
	samplesDraw(gReal);
	shiftOrigin(gImag);
	samplesSetOrigin(gImag,origin);
	samplesDraw(gImag);
}

static void zeroAllCB (Widget w, char * client_data, char * call_data)
{
	zeroData(fReal);  samplesDraw(fReal);
	zeroData(fImag);  samplesDraw(fImag);
	zeroData(gReal);  samplesDraw(gReal);
	zeroData(gImag);  samplesDraw(gImag);
	if(client_data-client_data) zeroAllCB(w,client_data,call_data);
}

static void quitCB (Widget w, char * client_data, char * call_data)
{
	XtCloseDisplay(XtDisplay(w));
	exit(0+(int)(client_data-client_data)+(int)(call_data-call_data));
}

static void editDone (Samples *s)
{
	float pv;
	CSamples *this_fft,*that_fft;
	
	/* determine this_fft and that_fft */
	this_fft = (fReal==s || fImag==s) ? fComplex : gComplex;
	that_fft = (fReal==s || fImag==s) ? gComplex : fComplex;
	
	/* transform this_fft to that_fft and draw that_fft */
	transform(this_fft,that_fft);
	pv = determinePlotValue(that_fft);
	samplesSetPlotValue(that_fft->r,pv);
	samplesSetPlotValue(that_fft->i,pv);
	samplesDraw(that_fft->r);
	samplesDraw(that_fft->i);
}

static void newData (Samples *s)
{
	if (s->data!=NULL) free(s->data);
	s->data = (float*)malloc(s->n*sizeof(float));
	zeroData(s);
}

static void zeroData (Samples *s)
{
	int i;
	for (i=0; i<s->n; ++i)
		s->data[i] = 0.0;
}

static void transform (CSamples *this_fft, CSamples *that_fft)
{
	int i,sign;
	float *thisr,*thisi,*thatr,*thati;
	complex *c;
		
	/* allocate space for complex to complex FFT */
	c = (complex*)malloc(nSamples*sizeof(complex));
	
	/* merge real and imaginary parts from this_fft */
	thisr = this_fft->r->data;
	thisi = this_fft->i->data;
	for (i=0; i<nSamples; i++) {
		c[i].r = thisr[i];
		c[i].i = thisi[i];
	}
	
	/* determine sign of transform */
	sign = (this_fft==fComplex)?1:-1;
	
	/* if origin centered, then change sign of every other sample */
	if (originCentered) {
		for (i=1; i<nSamples; i+=2) {
			c[i].r = -c[i].r;
			c[i].i = -c[i].i;
		}
	}
	
	/* do complex to complex FFT */
	fftcc(sign,nSamples,c);
	
	/* if origin centered, then change sign of every other sample */
	if (originCentered) {
		for (i=1; i<nSamples; i+=2) {
			c[i].r = -c[i].r;
			c[i].i = -c[i].i;
		}
	}
	
	/* unmerge real and imaginary parts from that_fft */ 
	thatr = that_fft->r->data;
	thati = that_fft->i->data;
	for (i=0; i<nSamples; i++) {
		thatr[i] = c[i].r;
		thati[i] = c[i].i;
	}
	
	/* free workspace */
	free(c);
}

static float determinePlotValue (CSamples *cs)
{
	int i;
	float *s,si,pv=0.0;
	
	s = cs->r->data;
	for (i=0; i<nSamples; i++) {
		si = s[i];
		if (-si>pv)
			pv = -si;
		else if (si>pv)
			pv = si;
	}
	s = cs->i->data;
	for (i=0; i<nSamples; i++) {
		si = s[i];
		if (-si>pv)
			pv = -si;
		else if (si>pv)
			pv = si;
	}
	return pv;
}

static void shiftOrigin (Samples *s)
{
	int i,shift=nSamples/2;
	float *d,*t;
	
	t = (float*)malloc(nSamples*sizeof(float));
	d = s->data;
	for (i=0; i<nSamples; i++)
		t[i] = d[i];
	for (i=0; i<shift; i++)
		d[i] = t[i+shift];
	for (i=shift; i<nSamples; i++)
		d[i] = t[i-shift];
	free(t);
}

static void fftcc(int sign, int n, complex cc[])
{
	int mmax,m,j,istep,i;
	float *c=(float*)cc;
	double scale,wr,wi,wpr,wpi,wtemp,theta,tempr,tempi;

	scale = sqrt(1.0/n);
	n <<= 1;

	/* bit reverse and scaling */
	for (i=0,j=0; i<n; i+=2) {
		if (j>=i) {
			tempr = c[j]*scale;
			tempi = c[j+1]*scale;
			c[j] = c[i]*scale;
			c[j+1] = c[i+1]*scale;
			c[i] = tempr;
			c[i+1] = tempi;
		}
		m = n>>1;
		while (m>=2 && j>=m) {
			j -= m;
			m >>= 1;
		}
		j += m;
	}

	/* butterflies via Danielson-Lanczos method */
	mmax = 2;
	while (n>mmax) {
		istep = 2*mmax;
		theta = 8.0*atan(1.0)/(sign*mmax);
		wtemp = sin(0.5*theta);
		wpr = -2.0*wtemp*wtemp;
		wpi = sin(theta);
		wr = 1.0;
		wi = 0.0;
		for (m=0; m<mmax; m+=2) {
			for (i=m; i<n; i+=istep) {
				j = i+mmax;
				tempr = wr*c[j]-wi*c[j+1];
				tempi = wr*c[j+1]+wi*c[j];
				c[j] = c[i]-tempr;
				c[j+1] = c[i+1]-tempi;
				c[i] += tempr;
				c[i+1] += tempi;
			}
			wr = (wtemp=wr)*wpr-wi*wpi+wr;
			wi = wi*wpr+wtemp*wpi+wi;
		}
		mmax = istep;
	}
}
