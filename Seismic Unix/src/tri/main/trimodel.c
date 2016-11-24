/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* TRIMODEL: $Revision: 1.7 $ ; $Date: 2011/11/21 16:56:25 $	*/

#include "par.h"
#include "tri.h"
#include "sloth.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" TRIMODEL - make a triangulated sloth (1/velocity^2) model                  		",
"									",
" trimodel >modelfile [optional parameters] 				",
" 									",
" Optional Parameters:							",
" xmin=0.0               minimum horizontal coordinate (x)		",
" xmax=1.0               maximum horizontal coordinate (x)		",
" zmin=0.0               minimum vertical coordinate (z)		",
" zmax=1.0               maximum vertical coordinate (z)		",
" xedge=                 x coordinates of an edge			",
" zedge=                 z coordinates of an edge			",
" sedge=                 sloth along an edge				",
" kedge=                 array of indices used to identify edges	",
" normray               0:do not generate parameters 1: does it   	",
" normface              specify which interface to shoot rays   	",
" nrays                 number of locations to shoot rays      	        ",
" sfill=                 x, z, x0, z0, s00, dsdx, dsdz to fill a region	",
" densfill=              x, z, dens to fill a region 			",
" qfill=                 x, z, Q-factor to fill a region 		",
" maxangle=5.0           maximum angle (deg) between adjacent edge segments",
"   									",
" Notes: 								",
" More than set of xedge, zedge, and sedge parameters may be 		",
" specified, but the numbers of these parameters must be equal. 	",
" 									",
" Within each set, vertices will be connected by fixed edges. 		",
" 									",
" Edge indices in the k array are used to identify edges 		",
" specified by the x and z parameters.  The first k index 		",
" corresponds to the first set of x and z parameters, the 		",
" second k index corresponds to the second set, and so on. 		",
" 									",
" After all vertices and their corresponding sloth values have 		",
" been inserted into the model, the optional sfill parameters 		",
" are used to fill closed regions bounded by fixed edges. 		",
" Let (x,z) denote any point inside a closed region.  Sloth inside 	",
" this region is determined by s(x,z) = s00+(x-x0)*dsdx+(z-z0)*dsdz.  	",
" The (x,z) component of the sfill parameter is used to identify a 	",
" closed region. 							",
" 									",
NULL};

/*
 *
 * AUTHOR:  Dave Hale, Colorado School of Mines, 02/12/91
 * MODIFIED: Andreas Rueger, Colorado School of Mines, 01/18/93
 *    Fill regions with attenuation Q-factors and density values.
 * MODIFIED: Craig Artley, Colorado School of Mines, 03/27/94
 *    Corrected bug in computing s00 in makeSlothForTri() function.
 * MODIFIED: Boyi Ou, Colorado School of Mines, 4/14/95
 *     Make code to generate interface parameters for shooting rays 
 *     from specified interface
 *
 * NOTE:
 * When you use normface to specify interface, the number of interface might
 * not be the number of interface in the picture, for example, you build a one
 * interface model, this interface is very long, so in the shell, you use three
 * part of xedge,zedge,sedge to make this interface, so when you use normface to
 * specify interface, this interface is just part of whole interface. If you
 * want see the normal rays from entire interface, you need to maek normal ray
 * picture few times, and merge them together.
 */ 

/**************** end self doc ***********************************/

/* prototypes for functions defined and used internally */
static Model *makemod (float xmin, float zmin, float xmax, float zmax,
	int ne, int *ke, int *ns, float **xs, float **zs, float **ss,
	float **txs, float **tzs, float **cs);
static void smoothTwoSegments (float maxangle, int i,
	int nd, float ud[], float xd[][4], float yd[][4],
	int *n, float *u[], float *x[], float *y[]);
static void smoothLineSegments (float maxangle, 
	int nin, float *xin, float *yin, float *sin,
	int *nout, float **xout, float **yout, float **sout,
	float **txout, float **tyout, float **cout,int ie);
static Vertex *newVertex (Model *m, Vertex *vlast,
	float x, float z, float s);
static void setEdgeAttributes (Vertex *v1, Vertex *v2, int k);
static void setEdgeUseAttributes (Vertex *v1, Vertex *v2,
	float tx1, float tz1, float c1, float tx2, float tz2, float c2);
static void makeSlothForTri (Tri *t);
static void fillsloth (Model *m, int nr, float **sr);
static void setsloth (Tri *t, float s00, float dsdx, float dsdz);
static void filldens (Model *m, int nd, float **dptr);
static void fillq (Model *m, int nq, float **qptr);
static void setdens (Tri *t, float dens);
static void setq (Tri *t, float qfac);

int normface,normray,nrays;
FILE *xfp,*zfp,*afp,*xzfp;


/* the main program */
int main (int argc, char **argv)
{
	int nxe,nze,nse,nke,ne,nr,*nxz,*ns,ie,ir,*ke=NULL;
        int id,iq,nq,nd;
	float xmin,zmin,xmax,zmax,maxangle,
		**xe,**ze,**se,**xs,**zs,**ss,**txs,**tzs,**cs,**sr=NULL;
        float **qptr=NULL,**dptr=NULL;
	Model *m;
	FILE *outfp=stdout;

	/* initialize pointers to NULL */
	nxz = ns = NULL;
	xe = ze = se = xs = zs = ss = txs = tzs = cs = NULL;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	/* get optional parameters */
	if (!getparfloat("xmin",&xmin)) xmin = 0.0;
	if (!getparfloat("xmax",&xmax)) xmax = 1.0;
	if (!getparfloat("zmin",&zmin)) zmin = 0.0;
	if (!getparfloat("zmax",&zmax)) zmax = 1.0;
	if (!getparfloat("maxangle",&maxangle)) maxangle = 1.0;
	if (!getparint("normray",&normray)) normray = 0;
        if (!getparint("normface",&normface)) normface = 0;
        if (!getparint("nrays",&nrays)) nrays = 0;
	maxangle *= PI/180.0;
	nxe = countparname("xedge");
	nze = countparname("zedge");
	nse = countparname("sedge");
	if (nxe!=nze) err("number of xedge must equal number of zedge");
	if (nxe!=nse) err("number of xedge must equal number of sedge");
	ne = nxe;
	if (ne>0) {
		xe = (float**)ealloc1(ne,sizeof(float*));
		ze = (float**)ealloc1(ne,sizeof(float*));
		se = (float**)ealloc1(ne,sizeof(float*));
		nxz = ealloc1int(ne);
		ke = ealloc1int(ne);
	}
	for (ie=0; ie<ne; ++ie) {
		nxe = countnparval(ie+1,"xedge");
		nze = countnparval(ie+1,"zedge");
		nse = countnparval(ie+1,"sedge");
		if (nxe!=nze) 
			err("number of xedge must equal number of zedge");
		if (nxe!=nse) 
			err("number of xedge must equal number of sedge");
		nxz[ie] = nxe;
		xe[ie] = ealloc1float(nxz[ie]);
		ze[ie] = ealloc1float(nxz[ie]);
		se[ie] = ealloc1float(nxz[ie]);
		getnparfloat(ie+1,"xedge",xe[ie]);
		getnparfloat(ie+1,"zedge",ze[ie]);
		getnparfloat(ie+1,"sedge",se[ie]);
	}
	nke = countparval("kedge");
	if (nke>ne) err("more kedge values than edges specified");
	getparint("kedge",ke);

	for (ie=nke; ie<ne; ++ie)
		ke[ie] = 0;

	if(normray==1){
         xfp=fopen("XInterface","w");
         zfp=fopen("ZInterface","w");
         xzfp=fopen("XZInterface","w");
         afp=fopen("AInterface","w");
        }


	if ((nr=countparname("sfill"))) sr = ealloc2float(7,nr);
	for (ir=0; ir<nr; ++ir)
		if (getnparfloat(ir+1,"sfill",sr[ir])!=7)
			err("7 values must be specified in sfill parameter");

	if ((nd=countparname("densfill"))) dptr = ealloc2float(3,nd);
	for (id=0; id<nd; ++id)
		if (getnparfloat(id+1,"densfill",dptr[id])!=3)
			err("3 values must be specified in densfill "
				"parameter");

	if ((nq=countparname("qfill"))) qptr = ealloc2float(3,nq);
	for (iq=0; iq<nq; ++iq)
		if (getnparfloat(iq+1,"qfill",qptr[iq])!=3)
			err("3 values must be specified in qfill parameter");

	/* smooth edges */
	if (ne>0) {
		ns = ealloc1int(ne);
 		xs = (float**)ealloc1(ne,sizeof(float*));
		zs = (float**)ealloc1(ne,sizeof(float*));
		ss = (float**)ealloc1(ne,sizeof(float*));
		txs = (float**)ealloc1(ne,sizeof(float*));
		tzs = (float**)ealloc1(ne,sizeof(float*));
		cs = (float**)ealloc1(ne,sizeof(float*));
		for (ie=0; ie<ne; ++ie)
			smoothLineSegments(maxangle,
				nxz[ie],xe[ie],ze[ie],se[ie],
				&ns[ie],&xs[ie],&zs[ie],&ss[ie],
				&txs[ie],&tzs[ie],&cs[ie],ie);
	}

	/* make model */
	m = makemod(xmin,zmin,xmax,zmax,ne,ke,ns,xs,zs,ss,txs,tzs,cs);

	/* fill regions with sloth */
	fillsloth (m,nr,sr);

        /* fill regions with dens if densfill is specified */
        if (nd!=0) filldens(m,nd,dptr);

        /* fill regions with dens if qfill is specified */
        if (nq!=0) fillq(m,nq,qptr);

	/* write model */
	writeModel(m,outfp);

	return EXIT_SUCCESS;
}

/* build model by connecting vertices of smoothed edges */
static Model *makemod (float xmin, float zmin, float xmax, float zmax,
	int ne, int *ke, int *ns, float **xs, float **zs, float **ss,
	float **txs, float **tzs, float **cs)
{
	int ie,is,islast;
	float txs1,tzs1,cs1,txs2,tzs2,cs2;
	Vertex *vlast,*v;
	Face *f;
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
		v = newVertex(m,NULL,xs[ie][0],zs[ie][0],ss[ie][0]);
		vlast = v;
		islast = 0;

		/* loop over vertices in smoothed edge */
		for (is=1; is<ns[ie]; ++is) {

			/* add vertex to model */
			v = newVertex(m,vlast,
				xs[ie][is],zs[ie][is],ss[ie][is]);

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

	/* set sloth function in triangles */
	f = m->f;
	do {
		makeSlothForTri(f);
		f = f->fNext;
	} while (f!=m->f);

	/* return model */
	return m;
}

static void smoothLineSegments (float maxangle, 
	int nin, float *xin, float *yin, float *sin,
	int *nout, float **xout, float **yout, float **sout,
	float **txout, float **tyout, float **cout,int ie)
/* smooth line segments and compute tangent vectors and curvatures */
{
	int n,i;
	float xs,ys,dx,dy,ddx,ddy,ds,dchord,maxchord,
		*uin,*u,*x,*y,*s,*tx,*ty,*c,*agou=NULL,*xou=NULL,
		*you=NULL,*uou=NULL,(*xind)[4],(*yind)[4],(*sind)[4];

	/* if only one (x,z) input, do not smooth */
	if (nin==1) {
		*nout = nin;
		*xout = xin;
		*yout = yin;
		*sout = sin;
		*txout = NULL;
		*tyout = NULL;
		*cout = NULL;
		return;
	}

	/* initially, copy input segments to output segments */
	n = nin;
	uin = ealloc1float(nin);
	u = ealloc1float(n);
	x = ealloc1float(n);
	y = ealloc1float(n);
	for (i=0; i<n; ++i) {
		x[i] = xin[i];
		y[i] = yin[i];
	}

	/* allocate space to my chord length */
        if(normray==1 && ie==normface){
          uou = alloc1float(nrays);
          xou = alloc1float(nrays);
          you = alloc1float(nrays);
          agou = alloc1float(nrays);
        }


	/* spline parameterization is chord length */
	uin[0] = u[0] = 0.0;
	for (i=1; i<n; ++i)
		uin[i] = u[i] = u[i-1]+
			sqrt(pow(x[i]-x[i-1],2.0)+pow(y[i]-y[i-1],2.0));

	if(ie==normface && normray==1){

          maxchord=u[n-1];

          uou[0]=0; dchord=maxchord/nrays;

          for(i=1;i<nrays;++i)
            uou[i]=uou[i-1]+dchord;

        }


	
	/* compute cubic interpolation coefficients */
	xind = (float(*)[4])ealloc1float(4*nin);
	yind = (float(*)[4])ealloc1float(4*nin);
	sind = (float(*)[4])ealloc1float(4*nin);
	csplin(nin,uin,xin,xind);
	csplin(nin,uin,yin,yind);
	csplin(nin,uin,sin,sind);

	/* loop over interior vertices */
	for (i=1; i<n-1; ++i)
		smoothTwoSegments(maxangle,i,nin,uin,xind,yind,&n,&u,&x,&y);

	/* compute sloth, tangent vectors, and curvatures */
	s = ealloc1float(n);
	tx = ealloc1float(n);
	ty = ealloc1float(n);
	c = ealloc1float(n);
	for (i=0; i<n; ++i) {
		intcub(0,nin,uin,sind,1,&u[i],&s[i]);
		intcub(1,nin,uin,xind,1,&u[i],&dx);
		intcub(1,nin,uin,yind,1,&u[i],&dy);
		intcub(2,nin,uin,xind,1,&u[i],&ddx);
		intcub(2,nin,uin,yind,1,&u[i],&ddy);
		ds = sqrt(dx*dx+dy*dy);
		tx[i] = dx/ds;
		ty[i] = dy/ds;
		c[i] = (dx*ddy-dy*ddx)/(ds*ds*ds);
	}

	if(ie==normface && normray==1) {

        for (i=0; i<nrays; ++i) {
                intcub(1,nin,uin,xind,1,&uou[i],&dx);
                intcub(1,nin,uin,yind,1,&uou[i],&dy);
                intcub(0,nin,uin,xind,1,&uou[i],&xs);
                intcub(0,nin,uin,yind,1,&uou[i],&ys);
                ds = sqrt(dx*dx+dy*dy);
                xou[i]=xs;
                you[i]=ys;
                agou[i]=90.-atan2(-dx,dy)*180/PI;
                fprintf(xzfp,"%f %f\n",xs,ys);
        }

        /*output interface (x,z,angle)*/
        fwrite(xou,sizeof(float),nrays,xfp);
        fwrite(you,sizeof(float),nrays,zfp);
        fwrite(agou,sizeof(float),nrays,afp);

        } /* end of if */


	/* free workspace */
	free1float(uin);
	free1float(u);
	free1float((float*)xind);
	free1float((float*)yind);
	free1float((float*)sind);

	/* set output parameters before returning */
	*nout = n;  *xout = x;  *yout = y;  *sout = s;
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
	uo = erealloc1float(uo,no);
	xo = erealloc1float(xo,no);
	yo = erealloc1float(yo,no);

	/* divide longest segment */
	inew = (ams>aps)?i:i+1;
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
	float x, float z, float s)
/* make a new vertex */
{
	Vertex *v;
	VertexAttributes *va;

	v = addVertexToModel(m,z,x);
	if (v==NULL) v = nearestVertexInModel(m,vlast,z,x);
	if (v->va==NULL)
		v->va = va = (VertexAttributes*)
			ealloc1(1,sizeof(VertexAttributes));
	else
		va = v->va;
	va->s = s;
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
	eu->e->ea = ea = (EdgeAttributes*) ealloc1(1,sizeof(EdgeAttributes));
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
	eu->eua = eua = (EdgeUseAttributes*)
		ealloc1(1,sizeof(EdgeUseAttributes));
	eua->tx = tx1;
	eua->tz = tz1;
	eua->c = c1;

	/* set edge-use attributes for edge-use from v2 to v1 */
	eu = eu->euMate;
	eu->eua = eua = (EdgeUseAttributes*)
		ealloc1(1,sizeof(EdgeUseAttributes));
	eua->tx = tx2;
	eua->tz = tz2;
	eua->c = c2;
}

static void makeSlothForTri (Tri *t)
/* make sloth function for triangle */
{
	FaceAttributes *fa;
	VertexAttributes *va;
	double x1,z1,x2,z2,x3,z3,s1,s2,s3,
		x2mx1,z2mz1,s2ms1,x3mx1,z3mz1,s3ms1,
		a,b,det,s00,dsdx,dsdz;
	
	x1 = t->eu->vu->v->y;
	z1 = t->eu->vu->v->x;
	va = t->eu->vu->v->va;
	s1 = va->s;
	x2 = t->eu->euCW->vu->v->y;
	z2 = t->eu->euCW->vu->v->x;
	va = t->eu->euCW->vu->v->va;
	s2 = va->s;
	x3 = t->eu->euCCW->vu->v->y;
	z3 = t->eu->euCCW->vu->v->x;
	va = t->eu->euCCW->vu->v->va;
	s3 = va->s;
	x2mx1 = x2-x1;  z2mz1 = z2-z1;  s2ms1 = s2-s1;
	x3mx1 = x3-x1;  z3mz1 = z3-z1;  s3ms1 = s3-s1;
	det = z3mz1*x2mx1-z2mz1*x3mx1;
	a = z3mz1*s2ms1-z2mz1*s3ms1;
	b = s3ms1*x2mx1-s2ms1*x3mx1;
	dsdx = a/det;
	dsdz = b/det;
	s00 = s2-dsdx*x2-dsdz*z2;
	t->fa = fa = (FaceAttributes*) ealloc1(1,sizeof(FaceAttributes));
	fa->s00 = s00;  fa->dsdx = dsdx;  fa->dsdz = dsdz;
        fa->dens = FLT_MAX;  fa->qfac = FLT_MAX;
}

static void fillsloth (Model *m, int nr, float **sr)
/* fill regions bounded by fixed edges with sloths */
{
	int ir;
	float x,z,x0,z0,s00,dsdx,dsdz;
	Tri *t;

	/* loop over regions for which sloth function is specified */
	for (ir=0; ir<nr; ++ir) {

		/* determine parameters of sloth function */
		x = sr[ir][0];  z = sr[ir][1];
		x0 = sr[ir][2];  z0 = sr[ir][3];
		s00 = sr[ir][4];  dsdx = sr[ir][5];  dsdz = sr[ir][6];

		/* adjust v0 for x0 and z0 */
		s00 -= x0*dsdx+z0*dsdz;

		/* determine triangle containing point (x,z) */
		t = insideTriInModel(m,NULL,z,x);

		/* flood triangles in region */
		setsloth(t,s00,dsdx,dsdz);
	}
}

static void setsloth (Tri *t, float s00, float dsdx, float dsdz)
/* recursively set sloth functions in triangles */
{
	EdgeUse *eu;
	FaceAttributes *fa;

	/* if sloth already set, then return */
	if ((fa=t->fa)!=NULL)
		if (fa->s00==s00 && fa->dsdx==dsdx && fa->dsdz==dsdz)
			return;

	/* if necessary, allocate space for attributes */
	if (fa==NULL)
		t->fa = fa = (FaceAttributes*)
			ealloc1(1,sizeof(FaceAttributes));

	/* set attributes */
	fa->s00 = s00;
	fa->dsdx = dsdx;
	fa->dsdz = dsdz;

	/* for each edge not fixed, set attributes in opposite triangle */
	eu = t->eu;
	do {
		if (!eu->e->fixed) setsloth(eu->euMate->f,s00,dsdx,dsdz);
		eu = eu->euCW;
	} while (eu!=t->eu);
}

static void filldens (Model *m, int nd, float **dptr)
/* fill regions bounded by fixed edges with density */
{
	int id, filled;
	float x,z,dens;
	Tri *t;
	Face *f;
	FaceAttributes *fa;

	/* loop over regions for which a density is specified */
	for (id=0; id<nd; ++id) {

		/* determine parameters of density function */
		x = dptr[id][0];  z = dptr[id][1];
		dens = dptr[id][2];  

		/* determine triangle containing point (x,z) */
		t = insideTriInModel(m,NULL,z,x);

		/* flood triangles in region */
		setdens(t,dens);
	}

	/* loop over faces to check that density is specified everywhere */
	f = m->f;
	do {
		fa = f->fa;
		dens = fa->dens;

		for (id=0,filled=0; id<nd; ++id) {
			if (dens==dptr[id][2]) {
				filled = 1;
				break;
			}
		}
		if(!filled)
			err("WARNING!!\n"
				"CHECK THAT ALL REGIONS ARE "
				"FILLED WITH A DENSITY VALUE\n");

		/* next face */
		f = f->fNext;

	} while (f!=m->f);
}

static void setdens (Tri *t, float dens)
/* recursively set density values in triangles */
{
	EdgeUse *eu;
	FaceAttributes *fa;

	/* if dens already set, then return */
	if ((fa=t->fa)!=NULL)
		if (fa->dens==dens)
			return;

	/* if necessary, allocate space for attributes */
	if (fa==NULL)
		t->fa = fa = (FaceAttributes*)
			ealloc1(1,sizeof(FaceAttributes));

	/* set attribute */
	fa->dens = dens;

	/* for each edge not fixed, set attributes in opposite triangle */
	eu = t->eu;
	do {
		if (!eu->e->fixed) setdens(eu->euMate->f,dens);
		eu = eu->euCW;
	} while (eu!=t->eu);
}

static void fillq (Model *m, int nq, float **qptr)
/* fill regions bounded by fixed edges with a Q-factor */
{
	int iq, filled;
	float x,z,qfac;
	Tri *t;
	Face *f;
	FaceAttributes *fa;

	/* loop over regions for which Q-factor is specified */
	for (iq=0; iq<nq; ++iq) {

		/* determine parameters of Q-function */
		x = qptr[iq][0];  z = qptr[iq][1];
		qfac = qptr[iq][2];  

		/* determine triangle containing point (x,z) */
		t = insideTriInModel(m,NULL,z,x);

		/* flood triangles in region */
		setq(t,qfac);
	}

	/* loop over faces to check that a Q-factor is specified everywhere */
	f = m->f;
	do {
		fa = f->fa;
		qfac = fa->qfac;

		for (iq=0,filled=0; iq<nq; ++iq) {
			if (qfac==qptr[iq][2]) {
				filled = 1;
				break;
			}
		}
		if(!filled)
			err("WARNING!!\n"
				"CHECK THAT ALL REGIONS ARE "
				"FILLED WITH A Q-FACTOR\n");

		/* next face */
		f = f->fNext;

	} while (f!=m->f);

}

static void setq (Tri *t, float qfac)
/* recursively set a Q-factor in triangles */
{
	EdgeUse *eu;
	FaceAttributes *fa;

	/* if Q-factor already set, then return */
	if ((fa=t->fa)!=NULL)
		if (fa->qfac==qfac)
			return;

	/* if necessary, allocate space for attributes */
	if (fa==NULL)
		t->fa = fa = (FaceAttributes*)
			ealloc1(1,sizeof(FaceAttributes));

	/* set attribute */
	fa->qfac = qfac;

	/* for each edge not fixed, set attributes in opposite triangle */
	eu = t->eu;
	do {
		if (!eu->e->fixed) setq(eu->euMate->f,qfac);
		eu = eu->euCW;
	} while (eu!=t->eu);
}
