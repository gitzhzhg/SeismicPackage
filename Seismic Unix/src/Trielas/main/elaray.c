/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

/* ELARAY: $Test Release: 1.2 $ ; $Date: 2011/11/21 17:00:43 $	*/


#include "par.h"
#include "tri.h"
#include "elastic.h"

#define diprint(expr) printf(#expr " = %i\n",expr)
typedef struct RSStruct {
	float sigma,x,z,px,pz,t;
	float q1,p1,q2,p2;
	float vgx,vgz; 
	float g11,g13,g33;    
	int kmah,nref,mode;
	EdgeUse *eu;
        float atten;
        float ampli,ampliphase;
	Face *f;
	struct RSStruct *rsNext;
	float rho;
} RayStep;
/*********************** self documentation **********************/
char *sdoc[] = {
" ELARAY - ray tracing for elastic anisotropic models",
"                                              				",
" elaray <modelfile >rayends [optional parameters]			",
"									",
" Optional Parameters:							",
" xs=(max-min)/2 x coordinate of source (default is halfway across model)",
" zs=min         z coordinate of source (default is at top of model)	",
" nangle=101     number of takeoff angles				",
" fangle=-45     first takeoff angle (in degrees)			",
" langle=45      last takeoff angle (in degrees)			",
" nxz=101        number of (x,z) in optional rayfile (see notes below)	",
" mode=0         shoot P-rays						",
"	      =1 shoot SV-rays						",
"	      =2 shoot SH-rays						",
" prim        =1 only reflected rays are plotted 		",     
"             =0 only direct hits are displayed  			",
" refseq=1,0,0   index of reflector followed by sequence of:		",
"		 transmission(0)					",
"		 reflection (1)						",
"		 transmission with mode conversion (2)			",					
"		 reflection with mode conversion (3)			",					
"                ray stops(-1).						",
" krecord        if specified, only rays incident at interface with index",
"                krecord are displayed and stored			",
" f0=1	         force impact strenght					",
" fdip=0         force dip with respect to vertical			",
" fazi=0         force azimuth with respect to positive x-axis 		",
" reftrans=0	 =1 include reflec/transm coeff(currently only for P)	",
" rayfile        file of ray x,z coordinates of ray-edge intersections	",
" wavefile       file of ray x,z coordinates uniformly sampled in time	",
" nt=		 number of (x,z) in optional wavefile (see notes below)	",
" tw=		 traveltime associated with wavefront (alternative to nt)",	
" infofile       ASCII-file to store useful information 		",
" outparfile     contains parameters for the plotting software. 	",
"                default is <outpar> 					",
" NOTES:								",
" The rayends file contains ray parameters for the locations at which	",
" the rays terminate.  							",
"									",
" The rayfile is useful for making plots of ray paths.			",
" nxz should be larger than twice the number of triangles intersected	",
" by the rays.								",
"									",
" The wavefile is useful for making plots of wavefronts.		",
" The time sampling interval in the wavefile is tmax/(nt-1),		",
" where tmax is the maximum time for all rays. Alternatively, ",
" one wavefront at time tw is plotted.	",
"									",
" The infofile is useful for collecting information along the		",
" individual rays. 							",
" The outparfile stores information used for the plotting software	",
"									",
NULL};

/*
 * AUTHORS:  Andreas Rueger, Colorado School of Mines, 01/02/95
  The program is based on :
 	        gbray.c, AUTHOR: Andreas Rueger, 08/12/93
 	       	sdray.c, AUTHOR Dave Hale, CSM, 02/26/91
 */
/**************** end self doc ***********************************/


#define TAPER 0.9



/* prototypes for functions defined and used internally */
void shootRaysAni (Model *m, float xs, float zs, int nangle, int reftrans,
	float dangle, float fangle, RefCheck *rc, int kk, RayStep *rsp[], 
	RayEnd re[], int *nrefseq,FILE *ifp, int mode, float fdip, float f0,
	float fazi);
void writeRays (Model *m,FILE *rayfp, int nxz, int nray, RayStep *rs[], RayEnd  
        re[], int krecord, int prim, FILE *ifp, FILE *outparfp);
void writeWaves (FILE *wavefp, int nt, int nray, RayStep *rs[]);
void writeWaves2 (FILE *wavefp, float tw, int nray, RayStep *rs[]);
static RayStep* RayInAnTri (RayStep *rs);
static RayStep* RayAcrossEdge (RayStep *rs,FILE *ifp, int conv,FILE *junkfp,
	int reftrans);
static RayStep* reflectRay (RayStep *rs,FILE *ifp, int conv,FILE *junkfp,
	int reftrans);
int testIfSourceOnEdge(Face *tris, float *zs, float *xs);
static void gvelreal (float a1111, float a3333, float a1133, float a1313,
	float a1113, float a3313, float px, float pz, float *vgx,  float *vgz,
	float *g11n, float *g13n, float *g33n);
void polar(float px, float pz, float g11, float g13,
	float g33, float *polx, float *polz, int mode );
int findnewmode(int mode , int* newmode, int conv, int mindex);
int findqPqSV(float s, float c, float pl, float a1111, float a3333,
	float a1133,float a1313, float a1113, float a3313, int mode, float
	*pxnew, float *pznew, float *vgx, float *vgz, float *g11, float *g13,  
	float *g33, float rt, FILE *ifp);


/* the main program */
int main (int argc, char **argv)
{
	Model *m;
	int i,ii,nangle,nxz,nt,kk,nseq,krecord,prim;
	int **hitseqa,mode,reftrans,*nrefseq;
	float xs,zs,fangle,langle,dangle,f0,fdip,fazi;
	/* float t0,t1,t2,tw; */
        float tw;
	
	char *rayfile,*wavefile,*infofile,*outparfile;
	FILE *rayfp=NULL,*wavefp=NULL,*ifp=NULL;
        FILE *outparfp=NULL;

	RayStep **rsp;
	RayEnd *re;	
        RefCheck *rc;

        /* t0=cpusec(); */

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* read model */
	m = readModel(stdin);

	/* check raymode */
	if (!getparint("mode",&mode)) mode = 0;
	
	if (mode != 0 && mode != 1 && mode != 2)
		err("ERROR Wrong raymode defined \n");


	/* get optional parameters */
	if (!getparfloat("xs",&xs)) xs = 0.5*(m->ymin+m->ymax);
	if (!getparfloat("zs",&zs)) zs = m->xmin;
	if (!getparint("nangle",&nangle)) nangle = 101;
	if (!getparint("reftrans",&reftrans)) reftrans = 0;
	if (!getparfloat("fangle",&fangle)) fangle = -45.0;
	if (!getparfloat("langle",&langle)) langle = 45.0;
	if (!getparfloat("f0",&f0)) f0 = 1.0;
	if (!getparfloat("fdip",&fdip)) fdip = 0;
	if (!getparfloat("fazi",&fazi)) fazi = 0;
	if (getparstring("rayfile",&rayfile)) rayfp = efopen(rayfile,"w");
	if (getparstring("infofile",&infofile))ifp = efopen(infofile,"w");
	if (!getparint("nxz",&nxz)) nxz = 101;
	if (getparstring("wavefile",&wavefile)) wavefp = efopen(wavefile,"w");
	if (!getparint("nt",&nt)) nt = INT_MAX;
	if (!getparfloat("tw",&tw)) tw = FLT_MAX;
	if (!getparint("krecord",&krecord)) krecord = INT_MAX;
	if (!getparint("prim",&prim)) prim = INT_MAX;
	if (getparstring("outparfile",&outparfile)) {
                 outparfp = efopen(outparfile,"w");
        } else {
                 outparfp = efopen("outpar","w");
        }

        checkpars();

	/* how many interfaces are of interest */
        kk=countparname("refseq");

        /* errorchecks */
        if(prim == 1 && kk == 0)
           	 err("\n first define target reflector \n ");

        /* allocate space for raychecks */
	rc=(RefCheck*)emalloc(kk*sizeof(RefCheck));

	/* Warning */
	if(reftrans != 0){
	  warn("\n Refl/Transm Coeff. need more work and testing !!!!\n");
	}
	
        /* allocate space for hitseq */
	if(kk>0){
	  hitseqa=(int**)emalloc(kk*sizeof(int*));
	  nrefseq=alloc1int(kk);
	} else {
	  hitseqa=(int**)emalloc(sizeof(int*));
	  nrefseq=alloc1int(1);
  	}

        if (ifp!=NULL) {
	 fprintf(ifp,"\n THIS FILE CONTAINS RAY TRACING INFORMATION \n"); 		
	 fprintf(ifp,"\n *********************************************\n");
 	 fprintf(ifp," defined reflection/transmission sequences \n");
	 fprintf(ifp," *********************************************\n");
        }

	/* determine interface modes */
        for(i=0;i<kk;i++){

	  /* how many parameters are defined for <refseq> ? */
	  nseq=countnparval(i+1,"refseq");
	  nrefseq[i]=nseq-1;

	  hitseqa[i]=alloc1int(nseq);

	  /* get the address and strip off the interface index */
	  getnparint(i+1,"refseq",hitseqa[i]);
	  rc[i].k=hitseqa[i][0];
	  rc[i].nhits=0;

	  /* now get the defined sequence of trans/ref/stops */
          rc[i].hitseq=hitseqa[i]+1;


          if (ifp!=NULL){
		fprintf(ifp,"\n interface %i :",rc[i].k);
		for(ii=0;ii<nseq-1;ii++){
		  fprintf(ifp,"%i ",rc[i].hitseq[ii]);
		}
          }
        }
        if (ifp!=NULL)
	 fprintf(ifp,
		"\n interfaces without defined refseq are transmitting \n");

	
	/* convert angles to radians and determine angle increment */
	fangle *= PI/180.0;
	langle *= PI/180.0;
	fdip   *= PI/180.0;
	fazi   *= PI/180.0;

	if(nangle==1){
	   dangle=0;
	} else {
	   dangle = (langle-fangle)/(nangle-1);
	}

	/* allocate space for ray step lists and ray ends */
	rsp = (RayStep**)alloc1(nangle,sizeof(RayStep*));
	re = (RayEnd*)alloc1(nangle,sizeof(RayEnd));

	/* shoot rays */
	shootRaysAni(m,xs,zs,nangle,reftrans,dangle,fangle,rc,kk,rsp,
			re,nrefseq,ifp,mode,fdip,f0,fazi);
	
        /* t0 = cpusec() - t0;*/

	/* if requested, write rays to rayfile */
        if (rayfp!=NULL) writeRays(m,rayfp,nxz,nangle,rsp,re,krecord,
		prim,ifp,outparfp);

	/* if requested, write waves to wavefile */
	if (wavefp!=NULL) {
		if(nt==INT_MAX && tw!=FLT_MAX)
			writeWaves2(wavefp,tw,nangle,rsp);
		else if(nt!=INT_MAX && tw==FLT_MAX)
			writeWaves(wavefp,nt,nangle,rsp);
		else 
			fprintf(stderr,"\n define either nt or tw \n");
	}
			

	/* t2 = cpusec()- t0;*/

	
	/* write ray ends */
	if (efwrite(re,sizeof(RayEnd),nangle,stdout)!=nangle)
		err("Error writing ray ends to stdout!\n");

	/* t1 = cpusec()- t2 +t0; */

	/* fprintf(stderr,"\n ray tracing cpu time= %g sec \n",t1);
	fprintf(stderr,"\n plotting cpu time= %g sec \n\n",t2); */


	return 1;
}

void shootRaysAni (Model *m, float xs, float zs, int nangle, int reftrans, 
	float dangle, float fangle, RefCheck *rc, int kk, RayStep *rsp[],  
	RayEnd re[], int *nrefseq, FILE *ifp, int mode, float fdip,
        float f0, float fazi)
/*****************************************************************************
Shoot rays in anisotropic models
******************************************************************************
Input:
m		triangulated model
xs		horizontal coordinate of source
zs		vertical coordinate (depth) of source
nangle		number of ray takeoff angles
dangle		increment in ray takeoff angle
fangle		first ray takeoff angle
rc      	pointer to reflector structure
nrefseq		pointer to refseq-size
kk	        number of reflectors 
mode		ray mode at takeoff
fdip		force direction with vertical
fazi		force azimuth with positive x-axis
f0		magnitude of impact source
reftrans	=1 refl/transm coeff included

Output:
rsp		array[nangle] of pointers to linked list of RaySteps
re		array[nangle] of RayEnds
******************************************************************************
Notes:
Rays are traced from triangle to triangle.
Whenever a ray crosses a triangle edge, two RaySteps are added
to the linked list of RaySteps, one for each side of the edge.
The RaySteps are useful if ray parameters along the entire raypath
are required.  The RayEnds are useful if parameters at the ray
endpoint are required.
******************************************************************************
AUTHORS:  Andreas Rueger, Colorado School of Mines, 02/02/94
  The program is based on :
 	(gbray.c)shootRays.c, AUTHOR: Andreas Rueger, 08/12/93
 	(sdray.c)shootRays.c, AUTHOR Dave Hale, CSM, 02/26/91
******************************************************************************/
{
	int iangle,refl,stop,i,temp,nameref,free;
	int refmod,transmod,*ireset,mindex;
	float a1111,a3333,a1133,a1313,a1113,a3313;
	float a1212,a1223,a2323,polx,polz,rhob;
	float s,ss,c,cc,sc,px,pz,sa,soa,angle;
	float gamm11,gamm33,gamm13,sqr,vp2,vp,ovp;
	float g11,g33,g13,vgx,vgz,fx,fz,fy;
	FILE *junkfp;

	Tri *tris;
	FaceAttributes *fa;
	EdgeAttributes *ea;
	RayStep *rs,*rslast;
	
	ireset = alloc1int(kk);

	/* junkfp=efopen("trans.dat","w");*/
        junkfp=0;
	


	/* initializations */
	px=pz=0.0;
	nameref=0;

	/* determine triangle in model containing source location */
	tris = insideTriInModel(m,NULL,zs,xs);
	temp = 1;
	i = 0;

	while(temp == 1){

		temp = testIfSourceOnEdge(tris,&zs,&xs);
		if(temp){
		    tris = insideTriInModel(m,NULL,zs,xs);
		    ++i;
		    if(i==5)
			err("\n ERROR in testIfSourceOnEdge\n");
		}
	}	

        
	/* determine stiffness at source location */
	fa = tris->fa;

	/* get stiffness at source point  */

	a1111 = fa->a1111;
	a3333 = fa->a3333;
	a1133 = fa->a1133;
	a1313 = fa->a1313;
	a1113 = fa->a1113;
	a3313 = fa->a3313;
	a1212 = fa->a1212;
	a1223 = fa->a1223;
	a2323 = fa->a2323;
	rhob  = fa->rho;
	mindex  = fa->mindex;

	/* error check */
	if(a3333 < 0) 
		err("\n ERROR: a3333 cannot be negative !! \n");
	if(a1313 < 0) 
		err("\n ERROR: a1313 cannot be negative !! \n");
	if(a1212 < 0) 
		err("\n ERROR: a1212 cannot be negative !! \n");
	if(a2323 < 0) 
		err("\n ERROR: a1313 cannot be negative !! \n");

	/* isotropic case P-rays*/
	if(mindex == 0 && mode == 0) mode=0;

	/* isotropic case SV-rays*/
	else if(mindex == 0 && mode == 1) mode=1;

	/* isotropic case SH-rays*/
	else if(mindex == 0 && mode == 2) mode=2;

	/* anisotropic case qP-rays*/
	else if(mindex != 0 && mode == 0) mode=3;

	/* anisotropic case qSV-rays*/
	else if(mindex != 0 && mode == 1) mode=4;

	/* anisotropic case qSH-rays*/
	else if(mindex != 0 && mode == 2) mode=5;


	/* force components */
	fx=f0*sin(fdip)*cos(fazi);
	fz=f0*cos(fdip);
	fy=f0*sin(fdip)*sin(fazi);

	if (ifp!=NULL){
	  fprintf(ifp,"\n ***** Force components *****\n");
	  fprintf(ifp,"\n fx=%g \t fy=%g \t fz=%g \n",fx,fy,fz);
	}

	/* loop over takeoff angles */
	for (iangle=0,angle=fangle; iangle<nangle; ++iangle,angle+=dangle) {

		/************** initialize ray tracing *************/

		c = cos(angle);
		s = sin(angle);

		/* isotropic case P-ray*/
		if(mode == 0 ){
			sa = sqrt(a3333);
			soa=1/sa;

			px  = s*soa;
			pz  = c*soa;
			vgx = px*a3333;
			vgz = pz*a3333;
			g11 = vgx*px;
			g33 = vgz*pz;
			g13 = vgx*pz;

		/* isotropic case SV-ray*/
		} else if(mode == 1){
			sa = sqrt(a1313);
			soa=1/sa;

			px  = s*soa;
			pz  = c*soa;
			vgx = px*a1313;
			vgz = pz*a1313;
			g11 = vgz*pz;
			g33 = vgx*px;
			g13 = -vgx*pz;

		/* isotropic case SH-ray */
		} else if(mode == 2){
			sa = sqrt(a1313);
			soa=1/sa;

			px  = s*soa;
			pz  = c*soa;
			vgx = px*a1313;
			vgz = pz*a1313;
			g11 = 0;
			g33 = 0;
			g13 = 0;


		/* anisotropic case qP/qSV-ray*/
		} else if(mode == 3 || mode == 4){
			cc = c*c;
			ss = s*s;
			sc = s*c;

			/*computing phase velocity for angle */
			gamm11 = a1111*ss+ a1313*cc +2*a1113*sc;
			gamm33 = a3333*cc + a1313*ss+2*a3313*sc;
			gamm13 = (a1133+a1313)*sc+ a1113*ss+ a3313*cc;
			sqr    = sqrt((gamm11+gamm33)*(gamm11+gamm33)-
				4*(gamm11*gamm33-gamm13*gamm13));
			vp2    = gamm11+gamm33;
			vp2    =((mode == 3)? vp2+sqr : vp2-sqr);

			/* error check */
			if(vp2 < 0) 
			   err("\n ERROR: ray initialization \n");

			vp     = sqrt(vp2*.5);
			ovp    = 1/vp;
			px     = s*ovp;
			pz     = c*ovp;

			gvelreal (a1111,a3333,a1133,a1313,a1113,a3313,px,
				pz,&vgx,&vgz,&g11,&g13,&g33);

		/* anisotropic case SH-ray*/
		} else if(mode == 5){
			cc = c*c;
			ss = s*s;
			sc = s*c;

			/*computing phase velocity for angle */
			sqr    = a1212*ss+ 2*a1223*sc + a2323*cc;

			/* error check */
			if(sqr < 0) 
				err("\n ERROR: ray initialization \n");

			vp     = sqrt(sqr);
			ovp    = 1/vp;
			px     = s*ovp;
			pz     = c*ovp;
				
			vgx = a1212*px + a1223*pz;
			vgz = a1223*px + a2323*pz;
			
			g11=g13=g33=0;

		} else 
			err("\n ERROR: wrong ray mode in initialization \n");


		/* get polariation with respect to ray direction */
		if( mode != 2 && mode !=5)
			polar(px,pz,g11,g13,g33,&polx,&polz,mode);



		/* initialize linked list of ray steps */
		rs = rsp[iangle] = (RayStep*)emalloc(sizeof(RayStep));
		rs->sigma = 0.0;
		rs->x = xs;
		rs->z = zs;
		rs->px = px;
		rs->pz = pz;
		rs->q1 = 1.0;
		rs->p1 = 0.0;
		rs->q2 = 0.0;
		rs->p2 = 1.0;
		rs->t = 0.0;
		rs->kmah = 0;
		rs->nref = 0;
		rs->eu = NULL;
		rs->f = tris;
                rs->ampliphase = 0;
                rs->vgx = vgx;
                rs->vgz = vgz;
                rs->g11 = g11;
                rs->g13 = g13;
                rs->g33 = g33;
                rs->rho = rhob;

		rs->rsNext = NULL;
		rs->mode = mode;
		
		/******* initialize ray amplitude **********/
		if(mode==2 || mode==5){
			rs->ampli = fy;
		} else {
                	rs->ampli = fx*polx + fz*polz;
		}

                /* initialize iresets for new ray */
                for(i=0;i<kk;i++)
		       ireset[i] = 0;


		if (ifp!=NULL){
			fprintf(ifp,"\n"
				"****************************************\n"
				"RAY WITH TAKEOFF ANGLE: %g (degrees)\n"
				"****************************************\n",
				angle*180.0/PI);
			fprintf(ifp,"\n initial values : \n");
			fprintf(ifp,"\n"
				"px=%g \t pz=%g \t vgx=%g \t vgz=%g \n",
				px,pz,vgx,vgz);
			fprintf(ifp,"g11=%g \t g33=%g \t g13=%g \t ampli=%g\n",
				g11,g33,g13,rs->ampli);
			fprintf(ifp,"xs=%g \t zs=%g \t mode=%i \t rho=%g\n ",
				xs,zs,mode,rhob);
		}
			
		/* trace ray */
		do {
			/* trace ray in triangle */
			rs = RayInAnTri(rs);
			
			/* remember last ray step */
			rslast = rs;
			
			/* edge attributes */
			ea = rs->eu->e->ea;
                       

			/* null edges are transmitting */
			refl = stop = temp = refmod = transmod = free= 0;

			if (ea!=NULL)
			{

			        /* check if edge is reflecting or stopping */
				for (i=0,temp=ea->k;i<kk; ++i) {

					if (temp==rc[i].k) {

						if (rc[i].hitseq[0] == 1)
							refl = 1;
						else if (rc[i].hitseq[0] == -1)
							stop = 1;
						else if (rc[i].hitseq[0] == 2)
							transmod = 1;
						else if (rc[i].hitseq[0] == 3)
							refmod = 1;						
						else if (rc[i].hitseq[0] == 4)
							free = 1;						
						else if (rc[i].hitseq[0] != 0)
						warn("wrong mode in refseq\n");

						++(rc[i].nhits);

						if(rc[i].nhits<nrefseq[i]){
							++(rc[i].hitseq);
							++(ireset[i]);
						}

						break;
				        }
				}
			}

			if (ifp!=NULL){
				fprintf(ifp,"^^^\n"
					"Interaction with interface %d at "
					"(x=%g,z=%g).\n",
					temp,rslast->x,rslast->z);

                         	if(refl){
				 fprintf(ifp," Ray is reflected ");
				} else if(refmod){
				 fprintf(ifp," Ray is reflected and "
				 "converted.\n");
			 	} else if(transmod){
				 fprintf(ifp," Ray is transmitted and " 	
				 "converted.\n");
				} else if(stop){
				 fprintf(ifp,"Hit stopping edge.  "
					     "Ray stopped.\n");
				} else if(free){
				 fprintf(ifp,"Free surface.  "
					     "Ray is reflected.\n");
				} else {
				 fprintf(ifp," Ray is transmitted ");
			        }
			}



			/* if ray at stopping edge, stop */
			if (stop) {
			 break;

			/* else if ray at reflecting edge, reflect */
			} else if (ea!=NULL && (refl)) {
				rs = reflectRay(rs,ifp,0,junkfp,reftrans);
				nameref=temp;

			/* else reflecting edge, mode conv */
			} else if (ea!=NULL && (refmod)) {
				rs = reflectRay(rs,ifp,1,junkfp,reftrans);
				nameref=temp;

			/* else transm edge, mode conv */
			} else if (ea!=NULL && (transmod)) {
				rs = RayAcrossEdge(rs,ifp,1,junkfp,reftrans);

			/* else free surface reflection */
			} else if (ea!=NULL && (free)) {
				rs = reflectRay(rs,ifp,2,junkfp,reftrans);

			/* else trace ray across edge */
			} else {
				rs = RayAcrossEdge(rs,ifp,0,junkfp,reftrans);
			}
			
		} while(rs!=NULL);
		

                /* reinitialize RayChecks for new ray */
                for(i=0;i<kk;i++){
		       rc[i].hitseq=rc[i].hitseq -ireset[i];
		       rc[i].nhits=0;

                }

		/* save ray end parameters 
		fa = rslast->f->fa;*/

		re[iangle].sigma = rslast->sigma;
		re[iangle].x = rslast->x;
		re[iangle].z = rslast->z;
		re[iangle].px = rslast->px;
		re[iangle].pz = rslast->pz;
		re[iangle].t = rslast->t;
		re[iangle].q1 = rslast->q1;
		re[iangle].p1 = rslast->p1;
		re[iangle].q2 = rslast->q2;
		re[iangle].p2 = rslast->p2;
		re[iangle].kmah = rslast->kmah;
		re[iangle].nref = rslast->nref;
		re[iangle].vgx = rslast->vgx;
		re[iangle].vgz = rslast->vgz;
		re[iangle].ab = angle;
                re[iangle].kend = temp;
                re[iangle].ampli = rslast->ampli;
                re[iangle].ampliphase = rslast->ampliphase;
                re[iangle].dangle = dangle;
                re[iangle].mode = rslast->mode;
                re[iangle].g11 = rslast->g11;
                re[iangle].g33 = rslast->g33;
                re[iangle].g13 = rslast->g13;
                re[iangle].num = iangle;
                re[iangle].nameref = nameref;
                re[iangle].rhob = rhob;
                re[iangle].rhoe = rslast->rho;

		
		/* optional output of rayendinformation */
                if (ifp!=NULL){
		 fprintf(ifp,"\n---------------------------"
				"------------------------ \n");
		 fprintf(ifp,"\n The following information is "
				"stored at the rayend \n");
		 fprintf(ifp,"\n Ray stops at "
			     "(%g,%g) at interface %i\n", 	
			     re[iangle].x,re[iangle].z,re[iangle].kend);
	         fprintf(ifp," Slowness components px= "
			    "%g pz=%g\n",re[iangle].px,re[iangle].pz);
		 fprintf(ifp," Traveltime = "
			     "%g  \t Sigma=%g\n",re[iangle].t,re[iangle].sigma);
		 fprintf(ifp," kmah index=%i \t Number of reflections=%i\n", 	
			     re[iangle].kmah,re[iangle].nref);
		 fprintf(ifp," g11=%g \t \t g33=%g \t g13=%g\n",
			     re[iangle].g11, re[iangle].g33,re[iangle].g13);
		 fprintf(ifp," Refl/Transm Amplitude =%g Phase=%g\n",
			    re[iangle].ampli,re[iangle].ampliphase); 
		 fprintf(ifp," ray number =%i reflected from=%i\n",
			    re[iangle].num,re[iangle].nameref); 
		 fprintf(ifp," density at begin =%g dens at end=%g\n",
			    re[iangle].rhob,re[iangle].rhoe); 
		 fprintf(ifp,"\n ------------------------" 	
			      "--------------------------- \n");
               } 
	}
}

    



static RayStep* RayInAnTri (RayStep *rs)
/*****************************************************************************
Trace rays in anisotropic triangles
******************************************************************************
Input:
* rs		pointer to last ray step
Output:
*rs		new ray step pointer
******************************************************************************
Notes:
Rays are traced from triangle to triangle. Intersection is found with
next triangle edge. Kinematic and dynamic parameters are updated.
******************************************************************************
Credits:  Andreas Rueger, Colorado School of Mines, 02/06/94

  The program is based on :
 	(gbray.c)traceRayInTri.c, AUTHOR: Andreas Rueger, 08/12/93
 	(sdray.c)traceRayInTri.c, AUTHOR: Dave Hale, CSM, 02/26/91
******************************************************************************/

{
	int kmah,nref,mode;
	float x,z,px,pz,t,q1,p1,q2,p2,ampli,
	      xa,za,xb,zb,dx,dz,b,c,
	      sigma,dt,dt1,small,frac,dsigma,
              ampliphase,rho;
	float vgx,vgz,g11,g33,g13;
	EdgeUse *eu,*eut,*eusmall;
	Face *f;
	
	/* get input parameters */
	sigma =	rs->sigma;
	x = rs->x;
	z = rs->z;
	px = rs->px;
	pz = rs->pz;
	t = rs->t;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	kmah = rs->kmah;
	nref = rs->nref;
	eu = rs->eu;
	f = rs->f;
	ampli = rs->ampli;
        ampliphase = rs->ampliphase;
	mode = rs->mode;
	vgx = rs->vgx;
	vgz = rs->vgz;
	g11 = rs->g11;
	g33 = rs->g33;
	g13 = rs->g13;
	rho = rs->rho;

	/* determine edge intersection with smallest positive dt */
	eusmall = NULL;
	small = FLT_MAX;
	eut = f->eu;
	do {
		/* edge endpoints */
		xa = eut->vu->v->y;  za = eut->vu->v->x;
		xb = eut->euCW->vu->v->y;  zb = eut->euCW->vu->v->x;
		
		/* coefficients b and c of equation to be solved for dt */
		dx = xb-xa;  dz = zb-za;
		b = dx*vgz - dz*vgx;
		c = (eut==eu ? 0.0 : dx*(z-za)-dz*(x-xa)); 
		
		if (b!=0.0)
			dt1 = -c/b;
		else
			dt1 = FLT_MAX;

		
		/* remember edge with smallest positive dt */
		if (0.0 <dt1 && dt1<small) {
			small = dt1;
			eusmall = eut;
		}
		
		/* next edge use */
		eut = eut->euCW;
		
	} while (eut!=f->eu);
	
	/* ray exits at edge with smallest positive dt */
	dt = small;
	eu = eusmall;
		
	/* change this if you include 2.5D spreading */
	dsigma=0;

	/* update ray parameters */
	sigma += dsigma;
	t +=dt;
	x += dt*vgx;
	z += dt*vgz;
	
	/* buggy place. Need to find a save fix. 
	   origionally, 0.0001 0.9999 */

	/* don't let ray exit too close to a vertex */
	xa = eu->vu->v->y;  dx = eu->euCW->vu->v->y-xa;
	za = eu->vu->v->x;  dz = eu->euCW->vu->v->x-za;
	frac = (ABS(dx)>ABS(dz)?(x-xa)/dx:(z-za)/dz);
	if (frac<0.001) {
		x = xa+0.001*dx;
		z = za+0.001*dz;
	} else if (frac>0.999) {
		x = xa+0.999*dx;
		z = za+0.999*dz;
	}

	/* return new raystep */
	rs->rsNext = (RayStep*)emalloc(sizeof(RayStep));
	rs = rs->rsNext;
	rs->sigma = sigma;
	rs->x = x;
	rs->z = z;
	rs->px = px;
	rs->pz = pz;
	rs->t = t;
	rs->q1 = q1;
	rs->p1 = p1;
	rs->q2 = q2;
	rs->p2 = p2;
	rs->kmah = kmah;
	rs->nref = nref;
	rs->eu = eu;
	rs->f = f;
	rs->rsNext = NULL;
        rs->vgx = vgx;
        rs->vgz = vgz;
        rs->ampli = ampli;
        rs->ampliphase = ampliphase;
	rs->mode = mode;
        rs->g11 = g11;
        rs->g13 = g13;
        rs->g33 = g33;
        rs->rho = rho;

	return rs;
}


static RayStep* RayAcrossEdge (RayStep *rs,FILE *ifp, int conv,FILE *junkfp,
	int reftrans)
/*****************************************************************************
rays across edges in anisotropic models
******************************************************************************
Input:
* rs		pointer to last ray step
conv		mode conversion=1
reftrans	=1 include transmission coeff

Output:
*rs		new ray step pointer
******************************************************************************
Notes:
Rays are traced across triangle edges. Kinematic and dynamic
boundary conditions are applied.If successful, return pointer 
to new RayStep.
If unsuccessful, return NULL.
Failure to trace across an edge is because:
(1) the ray was incident with angle greater than the critical angle, or
(2) the ray is incident at a boundary edge 
(3) grazing incidence
******************************************************************************
Credits:  Andreas Rueger, Colorado School of Mines, 02/06/94

  The program is based on :
 	(gbray.c)traceRayAcrossEdge.c, AUTHOR: Andreas Rueger, 08/12/93
 	(sdray.c)traceRayAcrossEdge.c, AUTHOR: Dave Hale, CSM, 02/26/91
******************************************************************************/
{
	int kmah,nref,modei,temp,mindexi,mindext,modet;
	float a1111i,a3333i,a1133i,a1313i,a1113i,a3313i;
	float a1111t,a3333t,a1133t,a1313t,a1113t,a3313t;
	float a1212i,a1223i,a2323i,a1212t,a1223t,a2323t;
	float rhoi,rhot,scale,gx,gz,frac,dx,dz,vgx,vgz;
	float vgxr,vgzr,pxr,pzr,pxi,pzi,coeff,phase;
	float sigma,x,z,px,pz,t,q1,p1,q2,p2,pl,rt,vgxi,vgzi;
        float ampli,ampliphase,g11,g13,g33,g11i,g13i,g33i,plold;
	EdgeUse *eu,*eum;
	EdgeUseAttributes *eua,*euma;
	Face *f;
	FaceAttributes *fa;

	/* get input parameters */
	sigma = rs->sigma;
	x = rs->x;
	z = rs->z;
	pxi = px = rs->px;
	pzi = pz = rs->pz;
	t = rs->t;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	kmah = rs->kmah;
	nref = rs->nref;
	eu = rs->eu;
	f = rs->f;
        ampli = rs->ampli;
	ampliphase = rs->ampliphase;
	vgxi = vgx = rs->vgx;
	vgzi = vgz = rs->vgz;
	modet = modei = rs->mode;
	g11 = g11i = rs->g11;
	g13 = g13i = rs->g13;
	g33 = g33i = rs->g33;

	/* check for boundary */
	if (eu->euMate->f==NULL) return NULL;
	
	/* determine stiffness on this side of edge */
	fa = f->fa;

	a1111i = fa->a1111;
	a3333i = fa->a3333;
	a1133i = fa->a1133;
	a1313i = fa->a1313;
	a1113i = fa->a1113;
	a3313i = fa->a3313;
        a1212i = fa->a1212;            
        a2323i = fa->a2323;             
        a1223i = fa->a1223;
        rhoi   = fa->rho;
	mindexi= fa->mindex;

	/* determine stiffness on other side of edge */
	eum = eu->euMate;
	f = eum->f;
	fa = f->fa;
	a1111t = fa->a1111;
	a3333t = fa->a3333;
	a1133t = fa->a1133;
	a1313t = fa->a1313;
	a1113t = fa->a1113;
	a3313t = fa->a3313;
        a1212t = fa->a1212;            
        a2323t = fa->a2323;             
        a1223t = fa->a1223;
        rhot   = fa->rho;
	mindext= fa->mindex;

	/* check if we need to apply boundary conditions */
	if(a1111i != a1111t || a3333i != a3333t ||
	   a1133i != a1133t || a1313i != a1313t ||
	   a1113i != a1113t || a3313i != a3313t ||
	   a1212i != a1212t || a1223i != a1223t ||
	   a2323i != a2323t || rhoi != rhot || conv)
        {
	    /* edge vector */
	    dx = eum->vu->v->y-eu->vu->v->y;
	    dz = eum->vu->v->x-eu->vu->v->x;

	    /* fractional distance along edge */
	    frac = (ABS(dx)>ABS(dz)?
		(x-eu->vu->v->y)/dx:
		(z-eu->vu->v->x)/dz);

	    /* linearly interpolate unit vector g tangent to edge */
	    eua = eu->eua;
	    euma = eum->eua;
	    if (eua!=NULL && euma!=NULL) {
		gx = frac*euma->tx-(1.0-frac)*eua->tx;
		gz = frac*euma->tz-(1.0-frac)*eua->tz;
	    } else {
		gx = -dx;
		gz = -dz;
	    }
	    scale = 1.0/sqrt(gx*gx+gz*gz);
	    gx *= scale;
	    gz *= scale;
	
	    /* gx = cos(horizontal/interface). The angle is
	       measured with respect to a right handed coordinate
	       system on the interface. gz = sin accordingly.
	       pl is measured relative to the same coordinate
	       system. Note that the sign differs for incidence
	       rays from both sides  */
	
	    /* tangent slowness component  */
	    plold = pl = gx*px+pz*gz;

	    /* optional output incidence values */
            if (ifp!=NULL){
		 fprintf(ifp,"\n incidence values :\n");
		 fprintf(ifp," mode=%i \t vgx=%g vgz=%g \n",
			     modei,vgxi,vgzi);
		 fprintf(ifp," g11=%g \t g13=%g g33=%g \n",
			     g11,g13,g33);
		 fprintf(ifp," ampli=%g \t ampliphase=%g \n",
			     ampli,ampliphase);
		 fprintf(ifp," tangential slowness component=%g \n",
			     pl);
	    }

	    /* find the ray mode in the new triangle */
 	    if(findnewmode(modei,&modet,conv,mindext) != 1)
		err("\n Error in findnewmode/transmission .\n");

	    /* isotropic case P-wave */
 	    if(modet == 0){

	      if(findPiso(a3333t,pl,gx,gz,&px,&pz,&vgx,&vgz,&g11,&g13,&g33,0)
			  !=1)
		return NULL;

	    /* isotropic case SV/SH-wave */
	    } else if(modet == 1 || modet == 2){

	      if(findSiso(a1313t,pl,gx,gz,&px,&pz,&vgx,&vgz,&g11,&g13,&g33,0)
			!=1)
			return NULL;
	      else if( modet == 2) {
		g11=g13=g33=0;
	      }

	    /* anisotropic case qP/qSV */
	    } else if(modet == 3 || modet == 4){
		rt=1;

		if(findqPqSV(gz,gx,pl,a1111t,a3333t,a1133t,a1313t,a1113t,
			a3313t,modet,&px,&pz,&vgx,&vgz,&g11,&g13,&g33,rt,ifp)
			!=1)
			return NULL;

	    /* anisotropic case SH */
	    } else if(modet == 5){

		if(findqSH(gz,gx,pl,a1212t,a1223t,a2323t,&px,&pz,&vgx,
		&vgz,0) != 1)
			return NULL;
		else {
			g11=g13=g33=0;
		}


	    } else
		err(" ERROR. Wrong mode in RayAcrossEdge");

	    /* This is the final check */
	    pl = gz*pz + px*gx;
	    if((pl-plold)*(pl-plold) > 0.000000001)
		err(" ERROR; Snell's law not satisfied in transmission \n");

	    /* optional output transmitted values */
            if (ifp!=NULL){
		fprintf(ifp,"\n transmitted values :\n");
		fprintf(ifp," mode=%i \t vgx=%g vgz=%g \n",
			     modet,vgx,vgz);
		fprintf(ifp," g11=%g \t g13=%g g33=%g \n",
			     g11,g13,g33);
		fprintf(ifp," tangential slowness component=%g \n",
			     pl);
	    }

	    /* check if we need to compute transmission coeff */
            if(reftrans !=0 )
            {
		
	    /* initialize */
	    coeff = 1;
	    phase = 0;

	    /* real/complex isotropic ref/transm coefficient */
	    if((modei==0 || modei==1 || modei==2) &&
	       (modet==0 || modet==1 || modet==2) ){

		temp = rt_iso_real(sqrt(a1111i),sqrt(a1111t),sqrt(a1313i),
		sqrt(a1313t),rhoi,rhot,pl,modei,modet,0,&coeff);

		/* complex coeff; needs to be tested */
		if(temp==0) {
			if(rt_iso_cmplx(sqrt(a1111i),sqrt(a1313t),
				sqrt(a1111t), sqrt(a1313t),rhoi,rhot,pl,
				modei,modet,0,&coeff,&phase) ==-1){

				warn("problems in rt_iso_cmplx/trans");
				return NULL;
			}

		/* problems with rt_iso_real/transm */
		}else if (temp==-1){

			warn(" Problems in rt_iso_real/transm");
			return NULL;
		}

	    /* real/complex coeff for SH-ani propagation */
	    } else if(modei==5 && modet==5){

		/* get the reflected wave root */
		if(findqSH(gz,gx,plold,a1212i,a1223i,a2323i,&pxr,&pzr,&vgxr,
		&vgzr,1) != 1){

			warn(" No real SH-reflection ");
			return NULL;

		}

                if(rt_SHa_real(a1212i*rhoi,a2323i*rhoi,a1223i*rhoi,
			a1212t*rhot,a2323t*rhot,a1223t*rhot,pxi,pzi,
			px,pz,pxr,pzr,0,&coeff,gz,gx) !=1 ){

				warn("problems in SHa_real");
				return NULL;
			}


	    /* real/complex anisotropic coeff */
	    } else {

		if(rt_ani_real(gz,gx,a1111i,a3333i,a1133i,a1313i,a1113i, 	
			a3313i,rhoi,a1111t,a3333t,a1133t,a1313t,a1113t,a3313t,
		        rhot,pxi,pzi,g11i,g13i,g33i,px,pz,g11,g13,g33, 	
	                modei,modet,0,&coeff,ifp) !=1 ){

				warn("problems in rt_ani_real");
				return NULL;
		}

	   
            }

	    ampli *= coeff;
	    ampliphase += phase;

	    /* optional output transmitted values */
            if (ifp!=NULL){
		 fprintf(ifp," ampli=%g \t ampliphase=%g \n",
			     ampli,ampliphase);
	    }

	    /* fprintf(junkfp,"%f %f \n", 
		   (atan(pxi/pzi)+asin(gz))*180/PI,coeff); */

	    if(ifp!=NULL)
		fprintf(ifp," transmission coeff=%g \n",coeff);


	    } /* close if reftrans */

	} /* close if boundary conditions */


	/* return new raystep */
	rs->rsNext = (RayStep*)emalloc(sizeof(RayStep));
	rs = rs->rsNext;
	rs->sigma = sigma;
	rs->x = x;
	rs->z = z;
	rs->px = px;
	rs->pz = pz;
	rs->t = t;
	rs->q1 = q1;
	rs->p1 = p1;
	rs->q2 = q2;
	rs->p2 = p2;
	rs->kmah = kmah;
	rs->nref = nref;
	rs->eu = eum;
	rs->f = f;
	rs->vgx = vgx;
	rs->vgz = vgz;
	rs->rsNext = NULL; 
        rs->ampli = ampli;
        rs->ampliphase = ampliphase;
	rs->mode = modet;
        rs->g11 = g11;
        rs->g13 = g13;
        rs->g33 = g33;
        rs->rho = rhot;


	return rs;
}

static RayStep* reflectRay (RayStep *rs,FILE *ifp, int conv,FILE *junkfp,
	int reftrans)
/* Reflect ray from edge and return pointer to new RayStep. */
{
	int kmah,nref,modei,modet,temp,mindexi,mindext,rort;
	float x,z,px,pz,t,q1,p1,q2,p2,rt,
	      scale,gx,gz,frac,dx,dz,plold;
	float g11,g13,g33,g11i,g13i,g33i;
	float rhoi,rhot,coeff,phase;
	float pxi,pzi,vgxi,vgzi,vgxt,vgzt,pxt,pzt ;
        float ampli,ampliphase,sigma,pl,vgx,vgz;
	float a1111i,a3333i,a1133i,a1313i,a1113i,a3313i;
	float a1111t,a3333t,a1133t,a1313t,a1113t,a3313t;
	float a1212i,a1223i,a2323i,a1212t,a1223t,a2323t;

	EdgeUse *eu,*eum;
	EdgeUseAttributes *eua,*euma;
	Face *f,*fn;
	FaceAttributes *fa;

	/* get input parameters */
	sigma = rs->sigma;
	x = rs->x;
	z = rs->z;
	pxi = px = rs->px;
	pzi = pz = rs->pz;
	t = rs->t;
	q1 = rs->q1;
	p1 = rs->p1;
	q2 = rs->q2;
	p2 = rs->p2;
	kmah = rs->kmah;
	nref = rs->nref;
	eu = rs->eu;
	f = rs->f;
        ampli = rs->ampli;
        ampliphase = rs->ampliphase;
	vgxi= vgx = rs->vgx;
	vgzi = vgz = rs->vgz;
	modei = rs->mode;
	g11 = g11i = rs->g11;
	g13 = g13i = rs->g13;
	g33 = g33i = rs->g33;


	/* reflection or free surface */
	/* check for boundary */
	if (eu->euMate->f==NULL) 
		rort=2;
	else 
		rort=1;

	/* determine stiffness on this side of edge */
	fa = f->fa;

	a1111t=a1111i= fa->a1111;
	a3333t=a3333i= fa->a3333;
	a1133t=a1133i= fa->a1133;
	a1313t=a1313i= fa->a1313;
	a1113t=a1113i= fa->a1113;
	a3313t=a3313i= fa->a3313;
        a1212t=a1212i = fa->a1212;            
        a2323t=a2323i = fa->a2323;             
        a1223t=a1223i = fa->a1223;
        rhot = rhoi   = fa->rho;
	mindext = mindexi= fa->mindex;


	if(rort != 2){
		/* determine stiffness on other side of edge */
		eum = eu->euMate;
		fn = eum->f;
		fa = fn->fa;
		a1111t= fa->a1111;
		a3333t= fa->a3333;
		a1133t= fa->a1133;
		a1313t= fa->a1313;
		a1113t= fa->a1113;
		a3313t= fa->a3313;
      		a1212t = fa->a1212;            
      		a2323t = fa->a2323;             
      		a1223t = fa->a1223;
     	 	rhot   = fa->rho;
		mindext= fa->mindex;

		dx = eum->vu->v->y-eu->vu->v->y;
		dz = eum->vu->v->x-eu->vu->v->x;

		/* fractional distance along edge */
		frac = (ABS(dx)>ABS(dz)?
			(x-eu->vu->v->y)/dx:
			(z-eu->vu->v->x)/dz);
	
		/* linearly interpolate unit vector g tangent to edge */
		eua = eu->eua;
		euma = eum->eua;
		gx = frac*euma->tx-(1.0-frac)*eua->tx;
		gz = frac*euma->tz-(1.0-frac)*eua->tz;
		scale = 1.0/sqrt(gx*gx+gz*gz);
		gx *= scale;
		gz *= scale;
	
		/* gx = cos(horizontal/interface). The angle is
		measured with respect to a right handed coordinate
		system on the interface. gz = sin accordingly.
		pl is measured relative to the same coordinate
		system. Note that the sign differs for incidence
		rays from both sides  */
	
		/* tangent slowness component  */
		pl = plold = gx*px+pz*gz;

		/* optional output incidence values */
       		if (ifp!=NULL){
		 fprintf(ifp,"\n incidence values :\n");
		 fprintf(ifp," mode=%i \t vgx=%g vgz=%g \n",
			     modei,vgxi,vgzi);
		 fprintf(ifp," g11=%g \t g13=%g g33=%g \n",
			     g11,g13,g33);
		 fprintf(ifp," ampli=%g \t ampliphase=%g \n",
			     ampli,ampliphase);
		 fprintf(ifp," tangential slowness component=%g \n",
			     pl);
		}
	} else {
		gx = 1.0;
		gz=0.0;
		pl = plold = px;
	}	

	/* find the ray mode in the new triangle */
 	if(findnewmode(modei,&modet,conv,mindexi) != 1)
		err("\n Error in findnewmode/transmission .\n");

	/* isotropic case P-wave */
 	if(modet == 0){

	if(findPiso(a3333i,pl,gx,gz,&px,&pz,&vgx,&vgz,&g11,&g13,&g33,1)
			  !=1)
		return NULL;

	/* isotropic case SV/SH-wave */
	} else if(modet == 1 || modet == 2){

		if(findSiso(a1313i,pl,gx,gz,&px,&pz,&vgx,&vgz,&g11,&g13,&g33,1)
			!=1)
			return NULL;
		else if( modet == 2) {
			g11=g13=g33=0;
		}

	/* anisotropic case qP/qSV */
	} else if(modet == 3 || modet == 4){
		rt = -1;

		if(findqPqSV(gz,gx,pl,a1111i,a3333i,a1133i,a1313i,a1113i,
			a3313i,modet,&px,&pz,&vgx,&vgz,&g11,&g13,&g33,rt,ifp)
			!=1)
			return NULL;

	/* anisotropic case SH */
	} else if(modet == 5){

		if(findqSH(gz,gx,pl,a1212i,a1223i,a2323i,&px,&pz,&vgx,
		&vgz,1) != 1)
			return NULL;
		else {
			g11=g13=g33=0;
		}


	} else
		err(" ERROR. Wrong mode in ReflectRay");

	/* This is the final check */
	pl = gz*pz + px*gx;
	if((pl-plold)*(pl-plold) > 0.000000001)
		err(" ERROR; Snell's law not satisfied in reflection \n");

	/* reflection coefficients */
	if(reftrans != 0){

	    /* initialize */
	    coeff = 1;
	    phase = 0;

	    /* real/complex isotropic ref/transm coefficient */
	    if((modei==0 || modei==1 || modei==2) &&
	       (modet==0 || modet==1 || modet==2) ){

		temp = rt_iso_real(sqrt(a1111i),sqrt(a1111t),sqrt(a1313i),
		sqrt(a1313t),rhoi,rhot,pl,modei,modet,rort,&coeff);

		/* real isotropic coeff */
		if(temp==1)
			ampli*=coeff;

		/* complex iso coeff NOT FINISHED YET*/
		else if(temp==0) {

			if(rt_iso_cmplx(sqrt(a1111i),sqrt(a1313t),
				sqrt(a1111t), sqrt(a1313t),rhoi,rhot,pl,
				modei,modet,rort,&coeff,&phase) ==-1){

				       warn("problems in rt_iso_cmplx/trans");
				       return NULL;
			}

		/* problems with rt_iso_real/transm */
		} else if (temp==-1){
			warn(" Problems in rt_iso_real/transm");
			return NULL;
		}
	
	    /* real/complex coeff for SH-ani propagation */
	    } else if(modei==5 && modet==5 && rort !=2 ){

		/* get the transmitted wave root */
		if(findqSH(gz,gx,plold,a1212t,a1223t,a2323t,&pxt,&pzt,&vgxt,
		&vgzt,0) != 1){
			warn(" No real SH-transmission ");
			return NULL;
		}

		/* get SH reflection coeff */
                if(rt_SHa_real(a1212i*rhoi,a2323i*rhoi,a1223i*rhoi,
			a1212t*rhot,a2323t*rhot,a1223t*rhot,pxi,pzi,
			pxt,pzt,px,pz,rort,&coeff,gz,gx)!= 1){
				warn(" problems in rt_SHa_real ");
				return NULL;
		}

	    /* get free surface SH reflection coeff */
	    } else if(modei==5 && modet==5 && rort == 2 ){
                if(rt_SHa_real(a1212i*rhoi,a2323i*rhoi,a1223i*rhoi,
			0,0,0,pxi,pzi,
			0,0,px,pz,rort,&coeff,gz,gx)!= 1){
				warn(" problems in rt_SHa_real ");
				return NULL;
		}

	    /* real/complex anisotropic coeff */
	    } else {

		temp=rt_ani_real(gz,gx,a1111i,a3333i,a1133i,a1313i,a1113i, 	
			a3313i,rhoi,a1111t,a3333t,a1133t,a1313t,a1113t,a3313t,
		        rhot,pxi,pzi,g11i,g13i,g33i,px,pz,g11,g13,g33, 	
	                modei,modet,1,&coeff,ifp);
		if(temp!=1) 
			return NULL;

	    }

	    ampli *= coeff;
	    ampliphase += phase;

	    /* fprintf(junkfp,"%f %f\n", 
		   (atan(pxi/pzi)+asin(gz))*180/PI,coeff); */

	}

	/* optional output reflected values */
        if (ifp!=NULL){
		 fprintf(ifp,"\n reflected values :\n");
		 fprintf(ifp," mode=%i \t vgx=%g vgz=%g \n",
			     modet,vgx,vgz);
		 fprintf(ifp," g11=%g \t g13=%g g33=%g \n",
			     g11,g13,g33);
		 fprintf(ifp," ampli=%g \t ampliphase=%g \n",
			     ampli,ampliphase);
		 fprintf(ifp," tangential slowness component=%g \n",pl);
		 fprintf(ifp," reflection coeff=%g\n",coeff);

	}


	/* return new raystep */
	rs->rsNext = (RayStep*)emalloc(sizeof(RayStep));
	rs = rs->rsNext;
	rs->sigma = sigma;
	rs->x = x;
	rs->z = z;
	rs->px = px;
	rs->pz = pz;
	rs->t = t;
	rs->q1 = q1;
	rs->p1 = p1;
	rs->q2 = q2;
	rs->p2 = p2;
	rs->kmah = kmah;
	rs->nref = nref+1;
	rs->eu = eu;
	rs->f = f;
	rs->rsNext = NULL;
        rs->ampli = ampli;
        rs->ampliphase = ampliphase;
        rs->vgx = vgx;
        rs->vgz = vgz;
	rs->mode =modet;
	rs->g11 =g11;
	rs->g33 =g33;
	rs->g13 =g13;
        rs->rho = rhoi;

	return rs;
}


int testIfSourceOnEdge(Face *tris, float *z, float *x)
/*****************************************************************************
 check if source is on an edge or on a vertex. If so, move the source
 not the most elegant program, feel free to change it
******************************************************************************
Input:
*xs		source coordinate
*zs		source coordinate
*tris		face containing source coordinate
******************************************************************************
Output:
0		source is not on edge/vertex
1		source is on edge/vertex
This is a test version
******************************************************************************
Author:   Andreas Rueger, Colorado School of Mines, 02/01/94
******************************************************************************/
{
    float x1,x2,x3,z1,z2,z3,eps,zmax,xs,zs;
    float xmax;
    float m12,m13,m23,b12,b13,b23;
    EdgeUse *eu;

    eu=tris->eu;
    eps=tris->m->eps;
    zmax=tris->m->xmax;
    xmax=tris->m->ymax;
    xs=*x;
    zs=*z;
    m12=m13=m23=0.0;


    /* get vertices */
    x1=eu->vu->v->y;
    z1=eu->vu->v->x;
    x2=eu->euCW->vu->v->y;
    z2=eu->euCW->vu->v->x;
    x3=eu->euCCW->vu->v->y;
    z3=eu->euCCW->vu->v->x;

    /* source is sitting on vertex */
    if( (xs-x1)*(xs-x1) + (zs-z1)*(zs-z1) < eps*eps ||
	(xs-x2)*(xs-x2) + (zs-z2)*(zs-z2) < eps*eps ||
	(xs-x3)*(xs-x3) + (zs-z3)*(zs-z3) < eps*eps ){
             *z = (zs < zmax-eps ? zs + 2*eps : zs - 2*eps);
             *x = (xs < xmax-eps ? xs + 2*eps : xs - 2*eps);
	     return 1;
    /* check the most typical cases */
    } else if((x3-x2)*(x3-x2) < eps*eps && (x3-xs)*(x3-xs) < eps*eps){
             *x = (xs < xmax-eps ? xs + eps : xs - eps);
	     return 1;
    } else if((x1-x2)*(x1-x2) < eps*eps && (x1-xs)*(x1-xs) < eps*eps){
             *x = (xs < xmax-eps ? xs + eps : xs - eps);
	     return 1;
    } else if((x1-x3)*(x1-x3) < eps*eps && (x1-xs)*(x1-xs) < eps*eps){
             *x = (xs < xmax-eps ? xs + eps : xs - eps);
	     return 1;

    } else if((z1-z2)*(z1-z2) < eps*eps && (z1-zs)*(z1-zs) < eps*eps){
             *z = (zs < zmax-eps ? zs + eps : zs - eps);
	     return 1;

    } else if((z1-z3)*(z1-z3) < eps*eps && (z1-zs)*(z1-zs) < eps*eps){
             *z = (zs < zmax-eps ? zs + eps : zs - eps);
	     return 1;

    } else if((z2-z3)*(z2-z3) < eps*eps && (z2-zs)*(z2-zs) < eps*eps){
             *z = (zs < zmax-eps ? zs + eps : zs - eps);
	     return 1;


    /* now the hard test */
    } else if((x1-x2)*(x1-x2) < eps*eps){ 
	     m12=FLT_MAX;
    } else if((x1-x3)*(x1-x3) < eps*eps){ 
	     m13=FLT_MAX;
    } else if((x2-x3)*(x2-x3) < eps*eps){ 
	     m23=FLT_MAX;
    } 

    if(m13!=FLT_MAX) m13=(z1-z3)/(x1-x3);
    if(m23!=FLT_MAX) m23=(z2-z3)/(x2-x3);
    if(m12!=FLT_MAX) m12=(z1-z2)/(x1-x2);
    b12=z1-m12*x1;
    b13=z1-m13*x1;
    b23=z2-m23*x2;


    /* source on edge? */
    if((zs-m12*xs-b12)*(zs-m12*xs-b12)<eps*eps ||
       (zs-m13*xs-b13)*(zs-m13*xs-b13)<eps*eps || 
       (zs-m23*xs-b23)*(zs-m23*xs-b23)<eps*eps){
             *z = (zs < zmax-eps ? zs + eps : zs - eps);
             return 1;

    /* source is not on edge */
    } else {
	return 0;
    }

}


void writeWaves (FILE *wavefp, int nt, int nray, RayStep *rs[])
/* for each ray, write nt z(t),x(t) pairs uniformly sampled in time t */
{
	int iray,it;
	float tmax,dt,t1,t2,x1,z1,x,z,xstart,zstart;
	float vgx,vgz,ti,ddt;
	RayStep *rsi,*rs1,*rs2;
	
	/* determine maximum time for all rays */
	for (iray=0,tmax=0.0; iray<nray; ++iray)
		for (rsi=rs[iray]; rsi!=NULL; rsi=rsi->rsNext)
			tmax = MAX(tmax,rsi->t);
	
	/* determine time sampling interval */
	dt = tmax/(nt>1?nt-1:1);

	fprintf(stderr,"wavefront time increment = %g\n",dt);

	/* loop over rays */
	for (iray=0; iray<nray; ++iray) {
	
		/* initialize */
		rs1 = rs[iray];
		rs2 = rs1->rsNext;
		t1 = rs1->t;
		t2 = rs2->t;
		x1 = rs1->x;
		z1 = rs1->z;
		vgx = rs1->vgx;
		vgz = rs1->vgz;
	
		/* remember start x,z */
		xstart = x1;
		zstart = z1;
		
		/* loop over times */
		for (it=0,ti=0.0; it<nt; ++it,ti+=dt) {
			
			/* if necessary, go to next ray step */
			if (t2<ti) {

				do {
					rs1 = rs2;
					rs2 = rs1->rsNext;
					if (rs2==NULL) break;
				} while (rs2->t<ti);

				if (rs2==NULL) break;
				t1 = rs1->t;
				t2 = rs2->t;
				x1 = rs1->x;
				z1 = rs1->z;
				vgx = rs1->vgx;
				vgz = rs1->vgz;
			}
			
			ddt = ti-t1;
	
			/* compute x and z */
			x = x1+ddt*vgx;
			z = z1+ddt*vgz;
		
			/* write x and z */
			fwrite(&z,sizeof(float),1,wavefp);
			fwrite(&x,sizeof(float),1,wavefp);
		}
		
		/* finish writing x and z */
		while (it<nt) {
			fwrite(&z,sizeof(float),1,wavefp);
			fwrite(&x,sizeof(float),1,wavefp);
			++it;
		}
	}
}

void writeWaves2 (FILE *wavefp, float tw, int nray, RayStep *rs[])
/* for each ray, write z(tw),x(tw) pairs uniformly sampled in time t */
{
	int iray;
	float tmax,t1,t2,x1,z1,x,z,xstart,zstart;
	float vgx,vgz,ddt;
	RayStep *rsi,*rs1,*rs2;

	fprintf(stderr,"\n wavefront plotted for tw=%g\n",tw);

	/* determine maximum time for all rays */
	for (iray=0,tmax=0.0; iray<nray; ++iray)
		for (rsi=rs[iray]; rsi!=NULL; rsi=rsi->rsNext)
			tmax = MAX(tmax,rsi->t);
	
	/* loop over rays */
	for (iray=0; iray<nray; ++iray) {
	
		/* initialize */
		rs1 = rs[iray];
		rs2 = rs1->rsNext;
		t1 = rs1->t;
		t2 = rs2->t;
		x1 = rs1->x;
		z1 = rs1->z;
		vgx = rs1->vgx;
		vgz = rs1->vgz;
	
		/* remember start x,z */
		xstart = x1;
		zstart = z1;
		
		/* if necessary, go to next ray step */
		if (t2<tw) {

				do {
					rs1 = rs2;
					rs2 = rs1->rsNext;
					if (rs2==NULL) break;
				} while (rs2->t<tw);
				if (rs2==NULL){
					/* write xstart and zstart */
					fwrite(&zstart,sizeof(float),1,wavefp);
					fwrite(&xstart,sizeof(float),1,wavefp);
					 continue;
				}
				t1 = rs1->t;
				t2 = rs2->t;
				x1 = rs1->x;
				z1 = rs1->z;
				vgx = rs1->vgx;
				vgz = rs1->vgz;
		}
			
			ddt = tw-t1;
	
			/* compute x and z */
			x = x1+ddt*vgx;
			z = z1+ddt*vgz;
			/* write x and z */
			fwrite(&z,sizeof(float),1,wavefp);
			fwrite(&x,sizeof(float),1,wavefp);
	}
		
}
	
void writeRays (Model *m,FILE *rayfp, int nxz, int nray, RayStep *rs[],RayEnd re[],int krecord,int prim,FILE *ifp,FILE *outparfp)

/* for each ray, write x,z pairs */
{
	int iray,nxzw,icount;
	float x,z;
	RayStep *rsi;

	/* initialize counting */
        icount=0;

	/* loop over rays */
	for (iray=0; iray<nray; ++iray) {

	   if(( krecord == INT_MAX) || (krecord ==re[iray].kend &&
		 ( prim == INT_MAX || prim == re[iray].nref))){ 
                
 		/* count # of rays */
                icount +=1;
			
		/* loop through ray steps */
		for (rsi=rs[iray],nxzw=0; rsi!=NULL; rsi=rsi->rsNext,++nxzw){
			if (nxzw>=nxz) break;
			x = rsi->x;
			z = rsi->z;
			fwrite(&z,sizeof(float),1,rayfp);
			fwrite(&x,sizeof(float),1,rayfp);
		}

 		/* if necessary, repeat last (x,z) */
		while (nxzw<nxz) {
			fwrite(&z,sizeof(float),1,rayfp);
			fwrite(&x,sizeof(float),1,rayfp);
			++nxzw;
		}

	    } else {
                /* manipulate rayend information */
                re[iray].kend=-1;

            } /* closes if statement */

       } /* end loop over rays */

      if (ifp!=NULL)
   	fprintf(ifp,"\n Number of rays written into output : %i \n",icount);

      fprintf(outparfp,"\n %i\n",icount);

}

static void gvelreal (float a1111, float a3333, float a1133, float a1313,
	float a1113, float a3313, float px, float pz, float *vgx,  float *vgz,
	float *g11n, float *g13n, float *g33n)
/*****************************************************************************
compute group velocity and polarization
******************************************************************************
Input:
aijkl		stiffness elements
px,pz		slowness elements

Output:
*vgx, *vgz	group velocities
*g11,*g13,*g33  polarizations
******************************************************************************/
{
	float gamm11,gamm13,gamm33;
	float px2,pz2,pxz,den,g11,g13,g33;

	px2   = px*px;
	pz2   = pz*pz;
	pxz   = px*pz;


	/*anisotropy parameters*/
	gamm11 = a1111*px2+ a1313*pz2 +2*a1113*pxz;
	gamm33 = a3333*pz2 + a1313*px2+2*a3313*pxz;
	gamm13 = (a1133+a1313)*pxz+ a1113*px2+ a3313*pz2;
	den     = 1/(gamm11+gamm33-2);
	g11     = (gamm33-1)*den;
	g33     = (gamm11-1)*den;
	g13     = -gamm13*den;

	/* computing ray (group) velocities */
	*vgx =  (a1111*px*g11+(a1133+a1313)*pz*g13+a3313*pz*g33+
		a1113*(pz*g11+2*px*g13)+a1313*g33*px);
	*vgz =  (a3333*pz*g33+(a1133+a1313)*px*g13+a1113*px*g11+
		+a3313*(px*g33+2*pz*g13)+a1313*g11*pz);

	/* kill round-off */
	if( g11 < -10*FLT_EPSILON || g33 < -10*FLT_EPSILON) 
		err("\n ERROR: g11 or g33 <0 in initialization \n");

	else if( g11 < 0.0 )
		g11 = 0;

	else if( g33 < 0.0 )
		g33 = 0;


	*g11n=g11;
	*g13n=g13;
	*g33n=g33;


}


void polar(float px, float pz, float g11, float g13,
	float g33, float *polx, float *polz, int mode )
/*****************************************************************************
compute polarization oriented along slowness. This convention is identical to that used in Aki&Richards, pages 148ff.                     
*******************************************************************************
Input:
px,pz		slowness components
g11,g13,g33 	polarizations squared
mode=0,1,3,4	ray-mode(qP.qSV)

Output:
polx,polz	polarization components

Author:  Andreas Rueger, Colorado School of Mines, 03/14/94
******************************************************************************/
{
	float polarx,polarz;

	if(g11 < FLT_EPSILON){
		polarx=0;
		polarz=sqrt(g33);
	} else if(g33 < FLT_EPSILON) {
		polarz=0;
		polarx=sqrt(g11);
	} else {
		polarx=sqrt(g11)*SGN(g13);
		polarz=sqrt(g33);
        }
	
	if((mode==0 || mode==3) && px*polarx+pz*polarz <0 ){
		polarx=-polarx;
		polarz=-polarz;
	} else if((mode==4 || mode==1) && px*polarx <0){
		polarx=-polarx;
		polarz=-polarz;
	}


	*polx=polarx;
	*polz=polarz;
}

int findnewmode (int mode , int* newmode, int conv, int mindex)
/*****************************************************************************
find the next ray mode
******************************************************************************
Input:
mode		incidence mode
conv		mode conversion=1
mindex		medium index

Output:
*newmode	pointer to new mode
******************************************************************************
Notes:
routine returns 1 if new mode found
******************************************************************************
Credits:  Andreas Rueger, Colorado School of Mines, 02/06/94
******************************************************************************/
{

	/* P isotropic */
	if( (mode == 0 && mindex == 0 && conv == 0) ||
	    (mode == 3 && mindex == 0 && conv == 0) ||
	    (mode == 4 && mindex == 0 && conv == 1) ||
	    (mode == 1 && mindex == 0 && conv == 1))
		*newmode = 0;

	/* SV isotropic */
	else if( (mode == 1 && mindex == 0 && conv == 0) ||
	    (mode == 4 && mindex == 0 && conv == 0) ||
	    (mode == 0 && mindex == 0 && conv == 1) ||
	    (mode == 3 && mindex == 0 && conv == 1))
		*newmode = 1;

	/* SH isotropic */
	else if( (mode == 2 && mindex == 0 && conv == 0) ||
	    (mode == 2 && mindex == 0 && conv == 1) ||
	    (mode == 5 && mindex == 0 && conv == 0) ||
	    (mode == 5 && mindex == 0 && conv == 1))
		*newmode = 2;

	/* qP anisotropic */
	else if( (mode == 0 && mindex > 0 && conv == 0) ||
	    (mode == 1 && mindex > 0 && conv == 1) ||
	    (mode == 3 && mindex > 0 && conv == 0) ||
	    (mode == 4 && mindex > 0 && conv == 1))
		*newmode = 3;

	/* qSV anisotropic */
	else if( (mode == 0 && mindex > 0 && conv == 1) ||
	    (mode == 1 && mindex > 0 && conv == 0) ||
	    (mode == 3 && mindex > 0 && conv == 1) ||
	    (mode == 4 && mindex > 0 && conv == 0))
		*newmode = 4;

	/* qSH anisotropic */
	else if( (mode == 2 && mindex > 0 && conv == 1) ||
	    (mode == 2 && mindex > 0 && conv == 0) ||
	    (mode == 5 && mindex > 0 && conv == 1) ||
	    (mode == 5 && mindex > 0 && conv == 0))
		*newmode = 5;
	else 
		return -1;

	return 1;
}
int findqPqSV(float s, float c, float pl, float a1111, float a3333,
	float a1133,float a1313, float a1113, float a3313, int mode, float
	*pxnew, float *pznew, float *vgx, float *vgz, float *g11, float *g13,  
	float *g33, float rt, FILE *ifp)
/*****************************************************************************
Continue slowness across interface 
******************************************************************************
Input:
s,c		slope of interface measured with respect to horizontal
pl		tangential component of slowness
aijkl		density-normalized stiffness elements
mode		Ray mode
rt		=-1 transmission
		=1 reflection
******************************************************************************
Output:
*pxnew		address pointing to new px
*pznew 		address pointing to new pz
-1		no root found
1		root found
*vgx,*vgz  	group velocity pointers
*g11,*g33,*g13  polarizations

******************************************************************************
Author:  Andreas Rueger, Colorado School of Mines, 02/01/94
******************************************************************************/
{
	int check1,check2;
	float vgm,vgxd,vgzd,pxnewd,pznewd,plold;
 
	check1 = check2 = 0;
	plold = pl;

	if(mode == 3){

		check1=solveForSlowqP(s,c,pl,a1111,a3333,a1133,a1313,a1113,
		a3313,pxnew,pznew,rt);
		
		
	}else if (mode ==4)

		check1=solveForSlowqSV(s,c,pl,a1111,a3333,a1133,a1313,a1113,
		a3313,pxnew,pznew,rt);

	else 
		err(" wrong mode in findqP \n ");


	if (check1 == 1){
		gvelreal (a1111,a3333,a1133,a1313,a1113,a3313,*pxnew,
				*pznew,vgx,vgz,g11,g13,g33);
	
		vgxd=*vgx;
		vgzd=*vgz;

		vgm=-s*vgxd+c*vgzd;

		if(rt == 1 && vgm > 0 ){
			check2 = 1;
		} else if( rt == -1 && vgm <0 ){
			check2 = 1;
		} else
			check2 = 0;

		if (ifp!=NULL && check2 == 1)
			fprintf(ifp,"\n first try in rootfinder "
				"was successfull \n");
	}
	if (check1 != 1 || check2 != 1){

		if (ifp!=NULL)
		  fprintf(ifp,"\n need second try in rootfinder \n");

		if(mode == 3)
		  check1=solveForSlowqP(s,c,pl,a1111,a3333,a1133,a1313,a1113,
		  a3313,pxnew,pznew,-1*rt);
		
		
		else if (mode ==4)
		  check1=solveForSlowqSV(s,c,pl,a1111,a3333,a1133,a1313,a1113,
		  a3313,pxnew,pznew,-1*rt);

	

		
		
		if(check1 != 1){
		  
	                fprintf(ifp," rootfinder not sucessful !!! \n");
			return 0;
		}
		
	        else {
		
			gvelreal (a1111,a3333,a1133,a1313,a1113,a3313,*pxnew,
				*pznew,vgx,vgz,g11,g13,g33);
	
			vgxd=*vgx;
			vgzd=*vgz;

			vgm=-s*vgxd+c*vgzd;

			if(rt == 1 && vgm < 0){
				return 0;
			} else if(rt == -1 && vgm >0){
				return 0;
			} else {
				check2 = 1;
				if (ifp!=NULL)
				fprintf(ifp,"\n second try "
				"in rootfinder was successfull \n");
			}
		}
	}

	pxnewd=*pxnew;
	pznewd=*pznew;

	pl = c*pxnewd+pznewd*s;
	if((pl-plold)*(pl-plold)<0.000000001 && check2 == 1 && check1 == 1)
		return 1;
	else
		return 0;		

}

