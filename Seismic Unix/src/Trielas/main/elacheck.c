/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "par.h"
#include "tri.h"
#include "elastic.h"

#define diprint(expr) printf(#expr " = %i\n",expr)
#define dfprint(expr) printf(#expr " = %f\n",expr)

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" ELACHECK - get elastic coefficients of model  	   		",
"									",
" elacheck file= (required modelfile)					",
"                                                                       ",
" ____ interactive program _____________                                ",
" 									",
NULL};

/*
 *
 * AUTHOR:: Andreas Rueger, Colorado School of Mines, 01/20/94
 * get stiffness information for anisotropic model (interactive)
 *
 */
/**************** end self doc ***********************************/

/* the main program */
int main (int argc, char **argv)
{
	int want,mindex=0;
	float x,z,a1111,a3333,a1133,a1313,a1113,a3313,v_p,v_s;
	float a1212,a2323,a1223,rho;
	Model *m;
	Face *t;
	FaceAttributes *fa;
	char *mfile;
	FILE *mfp;

	want=1;

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);

	if (!getparstring("file",&mfile))
		err("ERROR: No modelfile defined");
        mfp = efopen(mfile,"r");

        checkpars();

	a1111=a3333=a1133=a1313=a1113=a3313=v_p=v_s=0;
	a1212=a1223=a2323=rho=0;

	/* read model */
	m = readModel(mfp);

 	fprintf (stderr," **********************************************\n");
 	fprintf (stderr," *********check stiffness coefficients*********\n");
 	fprintf (stderr," **********************************************\n\n");

	do {
		want = 1;
 		fprintf (stderr," Enter x-coordinate \n");
 		scanf  ("%f", &x);

 		fprintf (stderr," Enter z-coordinate \n");
 		scanf  ("%f", &z);

		/* determine triangle containing point (x,z) */

		if(x > m->ymax || x < m->ymin || z > m->xmax || z < m->xmin){ 
			fprintf (stderr," Coordinate outside of model \n\n");
			continue;
		}

		t = insideTriInModel(m,NULL,z,x);

		if ((fa=t->fa)!=NULL){
	 		a1111 = fa->a1111;
	 		a3333 = fa->a3333;
	 		a1133 = fa->a1133;
	 		a1313 = fa->a1313;
	 		a1113 = fa->a1113;
	 		a3313 = fa->a3313;
			v_p = sqrt(a3333);
			v_s = sqrt(a1313);

			a1212 = fa->a1212;
			a1223 = fa->a1223;
			a2323 = fa->a2323;
			rho   = fa->rho;
			mindex  = fa->mindex;


		}
		fprintf(stderr,"\n ----Coordinates x=%g \t z=%g ----\n\n",x,z); 
  		/* fprintf(stderr,"\t\t mindex=%i \n ",mindex); */

		fprintf(stderr,"\t\t a1111=%g \n",a1111);   
		fprintf(stderr,"\t\t a3333=%g \n",a3333);   
		fprintf(stderr,"\t\t a1133=%g \n",a1133);   
		fprintf(stderr,"\t\t a1313=%g \n",a1313);   
		fprintf(stderr,"\t\t a1113=%g \n",a1113);   
		fprintf(stderr,"\t\t a3313=%g \n",a3313);  
		fprintf(stderr,"\t\t a1212=%g \n",a1212);   
		fprintf(stderr,"\t\t a1223=%g \n",a1223);   
		fprintf(stderr,"\t\t a2323=%g \n",a2323);  
 
 		fprintf(stderr,"\t\t rho=%g \n",rho);   

		fprintf(stderr,"\t\t   v_p=%g \n",v_p);   
		fprintf(stderr,"\t\t   v_s=%g \n\n",v_s);

		fprintf (stderr," More checks type: 1 if yes, 0 if no \n");
 		scanf  ("%i", &want);
	} while (want!=0);

	return 1;

}
