/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/* ELAMODEL: $Test Release: 1.1 $ ; $Date: 2011/11/21 17:00:43 $	*/

#include "par.h"
#include "tri.h"
#include "elastic.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" ELAMODEL - make piecewise homogeneous anisotropic model    		",
"									",
" elamodel >modelfile fill= [optional parameters]   			",
" 									",
" Input Parameters: 							",
" xmin=0.0               minimum horizontal coordinate (x) 		",
" xmax=1.0               maximum horizontal coordinate (x) 		",
" zmin=0.0               minimum vertical coordinate (z) 		",
" zmax=1.0               maximum vertical coordinate (z) 		",
" xedge=                 x coordinates of an edge 			",
" zedge=                 z coordinates of an edge 			",
" kedge=                 array of indices used to identify edges 	",
" fill=    iso      	 x,z,v_p,v_s,rho   				",
"          tiso      	 x,z,v_p,v_s,epsilon,delta,gamma,phi,rho	",
"          ani           x,z,a1111,a3333,a1133,a1313,a1113,a3313        ",
"                            a1212,a2323,a1223,rho                      ",
" maxangle=5.0           maximum angle (deg) between adjacent edge segments ",
"   									",
" Notes: 								",
" More than set of xedge and zedge parameters may be 		        ",
" specified, but the numbers of these parameters must be equal. 	",
" 									",
" Within each set, vertices will be connected by fixed edges. 		",
" 									",
" Edge indices in the k array are used to identify edges 		",
" specified by the x and z parameters.  The first k index 		",
" corresponds to the first set of x and z parameters, the 		",
" second k index corresponds to the second set, and so on. 		",
" 									",
" After all vertices have been inserted into the model,	the fill        ",
" parameters is used to fill closed regions bounded by fixed edges.     ",
" Three input modes are available:                                      ",
" Isotropic blocks:     x,z,v_p,v_s,rho                                 ",
" Transversely iso:     x,z,v_p,v_s,epsilon,delta,gamma,phi,rho         ",
" General 2D aniso:     x,z,a1111,a3333,a1133,a1313,a1113,a3313         ",
"                       a1212,a2323,a1223,rho                           ",
" Hereby:  ",
" x,z			   coordinates of one point in a bounded region ",
" v_p,v_s		   P, S-wave velocity along symmetry axis       ",
" epsilon, delta, gammma   Thomsen's parameters              ",
" rho 			   density 			     ",
" phi			   angle of symmetry axes with vertical ",
" aijkl			   density normalized stiffness coefficients ",
"  ",
" Each block can be defined by different input modes. The number of     ",
" input parameters defines the input type. Incorrect number of input    ",
" parameters result in an Error-message					",
" 									",
NULL};

/*
 *
 * AUTHOR:  Dave Hale, Colorado School of Mines, 02/12/91
 * modified : Andreas Rueger, Colorado School of Mines, 01/18/94
 * built anisotropic models
 *
 */
/**************** end self doc ***********************************/

/* prototypes for functions defined and used internally */ 

static void smoothTwoSegments (float maxangle, int i,
	int nd, float ud[], float xd[][4], float yd[][4],
	int *n, float *u[], float *x[], float *y[]);

static void smoothLineSegments (float maxangle,	int nin, float *xin,
	float *yin, int *nout, float **xout, float **yout, float **txout,
	float **tyout, float **cout);

static Model *makemod (float xmin, float zmin, float xmax, float zmax,
	int ne, int *ke, int *ns, float **xs, float **zs,
	float **txs, float **tzs, float **cs);

static Vertex *newVertex (Model *m, Vertex *vlast,
	float x, float z);

static void setEdgeAttributes (Vertex *v1, Vertex *v2, int k);

static void setEdgeUseAttributes (Vertex *v1, Vertex *v2,float tx1,
	 float tz1, float c1, float tx2, float tz2, float c2);

static void fillcoeff (Model *m, int nr, float **sra);

static void setcoeff (Tri *t, float a1111, float a3333, float a1133, float
 	a1313, float a1113, float a3313, float a1212, float a2323,
	float a1223, float rho, int mindex);

static void checkfill (Model *m, int nr, float **sra);

int rottensors (float *aijkl, float angle);

/* the main program */
int main (int argc, char **argv)
{
	int nxe,nze,nke,ne,nr,*nxz,*ns,ie,ir,*ke,num,i;
	float xmin,zmin,xmax,zmax,maxangle,temp,
		**xe,**ze,**xs,**zs,**txs,**tzs,**cs,*sr,**sra;
	float angle;
	Model *m;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/***************** get optional parameters **********************/

	if (!getparfloat("xmin",&xmin)) xmin = 0.0;
	if (!getparfloat("xmax",&xmax)) xmax = 1.0;
	if (!getparfloat("zmin",&zmin)) zmin = 0.0;
	if (!getparfloat("zmax",&zmax)) zmax = 1.0;
	if (!getparfloat("maxangle",&maxangle)) maxangle = 5.0;
	maxangle *= PI/180.0;

	/***************** input interfaces ******************************/

	nxe = countparname("xedge");
	nze = countparname("zedge");

	if (nxe!=nze)
	 err("\n ERROR number of xedge must equal number of zedge\n");

	ne = nxe;
	if (ne>0) {	
		xe = (float**)malloc(ne*sizeof(float*));
		ze = (float**)malloc(ne*sizeof(float*));
		nxz = alloc1int(ne);
	} else {
		xe = (float**)malloc(sizeof(float*));
		ze = (float**)malloc(sizeof(float*));
		nxz = alloc1int(1);
	}	

	for (ie=0; ie<ne; ++ie) {
		nxe = countnparval(ie+1,"xedge");
		nze = countnparval(ie+1,"zedge");
		if (nxe!=nze) 
		 err("\n ERROR number of xedge must equal number of zedge\n");

		nxz[ie] = nxe;
		xe[ie] = alloc1float(nxz[ie]);
		ze[ie] = alloc1float(nxz[ie]);
		getnparfloat(ie+1,"xedge",xe[ie]);
		getnparfloat(ie+1,"zedge",ze[ie]);
	}


	nke = countparval("kedge");
	ke = alloc1int(ne);
	if (nke>ne) err("\n ERROR more <kedge> values than edges specified\n");
	if (nke<ne) err("\n ERROR less <kedge> values than edges specified\n");

	getparint("kedge",ke);

        checkpars();


	/*************** read seismic parameters *****************************/
	nr = countparname("fill");
	if(nr ==0)
		 err("\n ERROR required parameters <fill> missing \n");

	sr = alloc1float(12);
	sra = alloc2float(13,nr);

	for (ir=0; ir<nr; ++ir) {

		num=getnparfloat(ir+1,"fill",sr);
		if(num != 5 && num != 9 && num != 12)
		err("\nERROR wrong number of input parameters in <fill> #%i\n",
		 	ir+1);

                /****** isotropic input *******/
		if(num == 5){
			sra[ir][0] = sr[0];
		 	sra[ir][1] = sr[1];
			sra[ir][2] = sra[ir][3] = sr[2]*sr[2];
			sra[ir][5] = sr[3]*sr[3];
 			sra[ir][4] = sra[ir][2] - 2*sra[ir][5];
			sra[ir][6] = sra[ir][7] = 0;
			sra[ir][8] = sra[ir][9] = sra[ir][5];
			sra[ir][10] = 0;
			sra[ir][11] = sr[4];
			sra[ir][12] = 0;

		/*** transversly isotropic ***/
		} else if(num == 9){	
			sra[ir][0] = sr[0];
			sra[ir][1] = sr[1];
			sra[ir][3] = sr[2]*sr[2];
			sra[ir][5] = sr[3]*sr[3];
			sra[ir][2] = sra[ir][3]+2*sr[4]*sra[ir][3];

			temp=2*sr[5]*sra[ir][3]*(sra[ir][3]-sra[ir][5])
			+(sra[ir][3]-sra[ir][5])*(sra[ir][3]-sra[ir][5]);

			if(temp <0) err("ERROR: a1133 not reel \n");

			sra[ir][4] = sqrt(temp)-sra[ir][5];

			sra[ir][6] = sra[ir][7] = sra[ir][10] =0;

			sra[ir][8] = 2*sra[ir][5]*sr[6]  + sra[ir][5];

			sra[ir][9] = sra[ir][5];

			sra[ir][11] = sr[8];
			sra[ir][12] = 1;


      			angle=sr[7]*PI/180.0;

    			if(angle != 0){

 			  /******** do the rotation ***********/
			  if(rottensors(sra[ir], angle) !=1)
				err(" Error \n rotation failed \n");

			}

		} else if (num==12){
			/****** anisotropic ************/

			for(i=0;i<12;i++)
				sra[ir][i] = sr[i];

			sra[ir][12]=2;

		} else {
		  err("\n ERROR wrong input parameters in <fill> #%i\n",ir+1);
		}
	}

	/*************** smooth interfaces *********************************/
	if (ne>0) {
		ns = alloc1int(ne);
 		xs = (float**)malloc(ne*sizeof(float*));
		zs = (float**)malloc(ne*sizeof(float*));
		txs = (float**)malloc(ne*sizeof(float*));
		tzs = (float**)malloc(ne*sizeof(float*));
		cs = (float**)malloc(ne*sizeof(float*));
		for (ie=0; ie<ne; ++ie)
			smoothLineSegments(maxangle,nxz[ie],xe[ie],ze[ie],
			&ns[ie],&xs[ie],&zs[ie],&txs[ie],&tzs[ie],&cs[ie]);
	} else {
		ns = alloc1int(1);
 		xs = (float**)malloc(sizeof(float*));
		zs = (float**)malloc(sizeof(float*));
		txs = (float**)malloc(sizeof(float*));
		tzs = (float**)malloc(sizeof(float*));
		cs = (float**)malloc(sizeof(float*));
	}

	/*************** create the  model *********************************/
	m = makemod(xmin,zmin,xmax,zmax,ne,ke,ns,xs,zs,txs,tzs,cs);

	/********* fill regions with elastic stiffness coefficient *********/
	fillcoeff(m,nr,sra);

	/*************** check if all regions are filled *******************/
	checkfill(m,nr,sra);

	/********************* write model *********************************/
	writeModel(m,stdout);

        return 1;
}

/* build model by connecting vertices of smoothed edges */
static Model *makemod (float xmin, float zmin, float xmax, float zmax,
	int ne, int *ke, int *ns, float **xs, float **zs, 
	float **txs, float **tzs, float **cs)
{
	int ie,is,islast;
	float txs1,tzs1,cs1,txs2,tzs2,cs2;
	Vertex *vlast,*v;
	Model *m;
	
	/* initialize model */
	m = makeModel(zmin,xmin,zmax,xmax);
	m->eps = 1.0e-5*sqrt(pow(xmax-xmin,2.0)+pow(zmax-zmin,2.0));
	m->sfa = sizeof(FaceAttributes);
	m->seua = sizeof(EdgeUseAttributes);
	m->sea = sizeof(EdgeAttributes);

	/* loop over smoothed edges */
	for (ie=0; ie<ne; ++ie) {
		
		/* add first vertex to model */
		v = newVertex(m,NULL,xs[ie][0],zs[ie][0]);
		vlast = v;
		islast = 0;
		
		/* loop over vertices in smoothed edge */
		for (is=1; is<ns[ie]; ++is) {
			
			/* add vertex to model */
			v = newVertex(m,vlast,
				xs[ie][is],zs[ie][is]);
			
			/* if new vertex is too close to last vertex */
			if (v==vlast) continue;
			
			/* connect vertex to last vertex with fixed edge */
			fixEdgeBetweenVertices(vlast,v);
			
			/* set edge attributes */
			setEdgeAttributes(vlast,v,ke[ie]);
			
			/* set edge-use attributes */
			txs1 = txs[ie][islast];
			tzs1 = tzs[ie][islast];
			cs1 = cs[ie][islast];
			txs2 = -txs[ie][is];
			tzs2 = -tzs[ie][is];
			cs2 = -cs[ie][is];
			setEdgeUseAttributes(vlast,v,
				txs1,tzs1,cs1,txs2,tzs2,cs2);
			
			/* remember last vertex */
			vlast = v;
			islast = is;
		}
	}
	
	
	/* return model */
	return m;
}




static void smoothLineSegments (float maxangle,	int nin, float *xin, float *yin,
	int *nout, float **xout, float **yout, float **txout, float **tyout,
	float **cout)
/* smooth line segments and compute tangent vectors and curvatures */
{
	int n,i;
	float dx,dy,ddx,ddy,ds,
		*uin,*u,*x,*y,*tx,*ty,*c,
		(*xind)[4],(*yind)[4];
	
	/* if only one (x,z) input, do not smooth */
	if (nin==1) {
		*nout = nin;
		*xout = xin;
		*yout = yin;
		*txout = NULL;
		*tyout = NULL;
		*cout = NULL;
		return;
	}
	
	/* initially, copy input segments to output segments */
	n = nin;
	uin = alloc1float(nin);
	u = alloc1float(n);
	x = alloc1float(n);
	y = alloc1float(n);
	for (i=0; i<n; ++i) {
		x[i] = xin[i];
		y[i] = yin[i];
	}
	
	/* spline parameterization is chord length */
	uin[0] = u[0] = 0.0;
	for (i=1; i<n; ++i)
		uin[i] = u[i] = u[i-1]+
			sqrt(pow(x[i]-x[i-1],2.0)+pow(y[i]-y[i-1],2.0));
	
	/* compute cubic interpolation coefficients */
	xind = (float(*)[4])alloc1float(4*nin);
	yind = (float(*)[4])alloc1float(4*nin);
	csplin(nin,uin,xin,xind);
	csplin(nin,uin,yin,yind);
	
	/* loop over interior vertices */
	for (i=1; i<n-1; ++i)
		smoothTwoSegments(maxangle,i,nin,uin,xind,yind,&n,&u,&x,&y);
		
	/* tangent vectors, and curvatures */
	tx = alloc1float(n);
	ty = alloc1float(n);
	c = alloc1float(n);
	for (i=0; i<n; ++i) {
		intcub(1,nin,uin,xind,1,&u[i],&dx);
		intcub(1,nin,uin,yind,1,&u[i],&dy);
		intcub(2,nin,uin,xind,1,&u[i],&ddx);
		intcub(2,nin,uin,yind,1,&u[i],&ddy);
		ds = sqrt(dx*dx+dy*dy);
		tx[i] = dx/ds;
		ty[i] = dy/ds;
		c[i] = (dx*ddy-dy*ddx)/(ds*ds*ds);
	}
		
	/* free workspace */
	free1float(uin);
	free1float(u);
	free1float((float*)xind);
	free1float((float*)yind);
	
	/* set output parameters before returning */
	*nout = n;  *xout = x;  *yout = y;  ;
	*txout = tx;  *tyout = ty;  *cout = c;
}

static void smoothTwoSegments (float maxangle, int i,
	int nd, float ud[], float xd[][4], float yd[][4],
	int *n, float *u[], float *x[], float *y[])
/* used by smoothLineSegments to smooth just two adjacent line segments */
{
	int no,inew,j;
	float *uo,*xo,*yo,xm,ym,xp,yp,dot,ams,aps,cosa,angle;
	
	/* input/output arrays describing line segments */
	no = *n;  uo = *u;  xo = *x;  yo = *y;
	
	/* if at endpoint, simply return */
	if (i==0 || i==no-1) return;
	
	/* line segments joined at vertex i */
	xm = xo[i]-xo[i-1];  ym = yo[i]-yo[i-1];
	xp = xo[i+1]-xo[i];  yp = yo[i+1]-yo[i];
	
	/* angle between segments */
	dot = xm*xp+ym*yp;
	ams = xm*xm+ym*ym;
	aps = xp*xp+yp*yp;
	cosa = dot/sqrt(ams*aps);
	cosa = MAX(-1.0,MIN(1.0,cosa));
	angle = acos(cosa);
	
	/* if angle is small enough, simply return */
	if (angle<=maxangle) return;
	
	/* make room for new vertex */
	no++;
	uo = realloc1float(uo,no);
	xo = realloc1float(xo,no);
	yo = realloc1float(yo,no);
	
	/* divide longest segment */
	inew = (ams>aps?i:i+1);
	for (j=no-1; j>inew; --j) {
		uo[j] = uo[j-1];
		xo[j] = xo[j-1];
		yo[j] = yo[j-1];
	}
	uo[inew] = 0.5*(uo[inew-1]+uo[inew+1]);
	intcub(0,nd,ud,xd,1,&uo[inew],&xo[inew]);
	intcub(0,nd,ud,yd,1,&uo[inew],&yo[inew]);
	
	/* smooth line segments affected by new vertex */
	smoothTwoSegments(maxangle,inew,nd,ud,xd,yd,&no,&uo,&xo,&yo);
	smoothTwoSegments(maxangle,inew-1,nd,ud,xd,yd,&no,&uo,&xo,&yo);
	smoothTwoSegments(maxangle,inew+1,nd,ud,xd,yd,&no,&uo,&xo,&yo);
	
	/* set output parameters before returning */
	*n = no;  *u = uo;  *x = xo;  *y = yo;
}
static Vertex *newVertex (Model *m, Vertex *vlast,
	float x, float z)
/* make a new vertex */
{
	Vertex *v;
	
	v = addVertexToModel(m,z,x);
	if (v==NULL) v = nearestVertexInModel(m,vlast,z,x);
	return v;
}

static void setEdgeAttributes (Vertex *v1, Vertex *v2, int k)
/* set edge attributes for fixed edge between two connected vertices */
{
	VertexUse *vu;
	EdgeUse *eu;
	EdgeAttributes *ea;
	
	/* determine edge use from v1 to v2 */
	vu = v1->vu;
	do {
		eu = vu->eu;
		if (eu->euMate->vu->v==v2) break;
		vu = vu->vuNext;
	} while (vu!=v1->vu);
	
	/* if v1 and v2 not connected, just return */
	if (eu->euMate->vu->v!=v2) return;
	
	/* set edge attributes for edge between v1 and v2 */
	eu->e->ea = ea = (EdgeAttributes*)malloc(sizeof(EdgeAttributes));
	ea->k = k;
}

static void setEdgeUseAttributes (Vertex *v1, Vertex *v2,
	float tx1, float tz1, float c1, float tx2, float tz2, float c2)
/* set edge-use attributes for fixed edge between two connected vertices */
{
	VertexUse *vu;
	EdgeUse *eu;
	EdgeUseAttributes *eua;
	
	/* determine edge use from v1 to v2 */
	vu = v1->vu;
	do {
		eu = vu->eu;
		if (eu->euMate->vu->v==v2) break;
		vu = vu->vuNext;
	} while (vu!=v1->vu);
	
	/* if v1 and v2 not connected, just return */
	if (eu->euMate->vu->v!=v2) return;
	
	/* set edge-use attributes for edge-use from v1 to v2 */
	eu->eua = eua = (EdgeUseAttributes*)malloc(sizeof(EdgeUseAttributes));
	eua->tx = tx1;
	eua->tz = tz1;
	eua->c = c1;
	
	/* set edge-use attributes for edge-use from v2 to v1 */
	eu = eu->euMate;
	eu->eua = eua = (EdgeUseAttributes*)malloc(sizeof(EdgeUseAttributes));
	eua->tx = tx2;
	eua->tz = tz2;
	eua->c = c2;
}

static void fillcoeff (Model *m, int nr, float **sr)
/* fill regions bounded by fixed edges with sloths */
{
	int ir,mindex;
	float x,z,a1111,a3333,a1133,a1313,a1113,a3313;
	float a1212,a2323,a1223,rho;
	

	Tri *t;
	
	/* loop over regions for which sloth function is specified */
	for (ir=0; ir<nr; ++ir) {
		
		/* determine parameters of sloth function */
		x = sr[ir][0];  z = sr[ir][1];
		a1111 = sr[ir][2];  a3333 = sr[ir][3];
		a1133 = sr[ir][4];  a1313 = sr[ir][5]; 
		a1113 = sr[ir][6];  a3313 = sr[ir][7];
		a1212 = sr[ir][8];  a2323 = sr[ir][9]; 
		a1223 = sr[ir][10]; rho = sr[ir][11];
		mindex = (int) sr[ir][12];

		

		/* determine triangle containing point (x,z) */
		t = insideTriInModel(m,NULL,z,x);
		
		/* flood triangles in region */
		setcoeff(t,a1111,a3333,a1133,a1313,a1113,a3313,a1212,a2323,
			a1223,rho,mindex);

	}
}

static void setcoeff (Tri *t, float a1111, float a3333, float a1133, float
 	a1313, float a1113, float a3313, float a1212, float a2323,
	float a1223, float rho, int mindex)
/* recursively set elastic coefficients in triangles */
{
	EdgeUse *eu;
	FaceAttributes *fa;
	
	/* if sloth already set, then return */
	if ((fa=t->fa)!=NULL)
		if (fa->a1111==a1111 && fa->a3333==a3333 && fa->a1133==a1133
		&& fa->a1313==a1313 && fa->a3313==a3313 && fa->a1113==a1113
		&& fa->a1212==a1212 && fa->a2323==a2323 && fa->a1223==a1223
		&& fa->rho==rho)
			return;
	
	/* if necessary, allocate space for attributes */
	if (fa==NULL)
		t->fa = fa = (FaceAttributes*)malloc(sizeof(FaceAttributes));
	
	/* set attributes */
	fa->a1111 = a1111;
	fa->a3333 = a3333;
	fa->a1133 = a1133;
	fa->a1313 = a1313;
	fa->a1113 = a1113;
	fa->a3313 = a3313;
	fa->a1212 = a1212;
	fa->a2323 = a2323;
	fa->a1223 = a1223;
	fa->rho   = rho;
	fa->mindex = mindex;

	/* for each edge not fixed, set attributes in opposite triangle */
	eu = t->eu;
	do {
		if (!eu->e->fixed)
		  	setcoeff(eu->euMate->f,a1111,a3333,a1133,a1313,a1113,
				a3313,a1212,a2323,a1223,rho,mindex);
		eu = eu->euCW;
	} while (eu!=t->eu);
}

static void checkfill (Model *m, int nr, float **sra)
/* check if all regions are filled */
{
	int ir, icheck;
	float fac;
	Face *f;
	FaceAttributes *fa;
	fa = (FaceAttributes*)malloc(sizeof(FaceAttributes)); 
 
        /* loop over faces */
   
	f = m->f;
	do {
		if(f->fa != NULL) {
			fa = f->fa;
		} else {
		  err("\n ERROR: You didn't fill all regions \n");
		}

		fac = fa->a1111;

		icheck = 0 ;
		for (ir=0; ir<nr; ++ir) {
			if(fac == sra[ir][2]){
				icheck = 1;
				break;
			}
		}
		if(!icheck)
		  err("\n ERROR: You didn't fill all regions \n");

		f = f->fNext;

	} while (f != m->f);

}

int rottensors (float *aijkl, float angle)
/*****************************************************************************
Rotate 2-D elastic stiffness tensor
******************************************************************************
Input:
aijkl		input/output stiffness elements

Author:  Andreas Rueger, Colorado School of Mines, 03/14/94
******************************************************************************/
{
		float si,co,ss,cc,sc,a,c,f,l,m,n;
		float o,p,q;
		si=sin(angle); co=cos(angle);
		ss=si*si; cc=co*co;
		sc=si*co;
		a=aijkl[2];
		c=aijkl[3];
		f=aijkl[4];
		l=aijkl[5];
		m=aijkl[6];
		n=aijkl[7];
		o=aijkl[8];
		p=aijkl[9];
		q=aijkl[10];

		aijkl[2]=a*cc*cc+c*ss*ss+2*f*cc*ss+4*l*cc*ss-4*m*cc*sc-
			4*n*ss*sc;
		aijkl[3]=a*ss*ss+c*cc*cc+2*f*cc*ss+4*l*cc*ss+4*m*ss*sc+
			4*n*cc*sc;
		aijkl[4]=a*ss*cc+c*ss*cc+f*(ss*ss+cc*cc)-4*l*ss*cc-2*m*
			(ss*sc-cc*sc)-2*n*(cc*sc- ss*sc);
		aijkl[5]=a*ss*cc+c*ss*cc-2*f*ss*cc+l*(cc*cc+ss*ss-2*ss*cc)
			-2*m*(ss*sc-cc*sc)-2*n*(cc*sc- ss*sc);
		aijkl[6]=a*cc*sc-c*ss*sc+f*(ss*sc-cc*sc)+2*l*(ss*sc-cc*sc)+
			m*(cc*cc -3*cc*ss) +(3*cc*ss - ss*ss )*n;;
		aijkl[7]=a*ss*sc-c*cc*sc+f*(cc*sc-ss*sc)+2*l*(cc*sc-ss*sc)+
			 m*( 3*cc*ss - ss*ss ) + n*( cc*cc - 3*ss*cc);
		aijkl[8]=cc*o-2*sc*q+ss*p;
		aijkl[9]=ss*o+2*sc*q+cc*p;
		aijkl[10]=sc*o+(cc-ss)*q-sc*p;
		
		return 1;

}
