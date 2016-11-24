/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* TRANSP3D: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"
#include "VND.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" TRANSP3D - TRANSPose an n1 by n2 by n3 element matrix			",
" 									",
" transp3d <infile >outfile n1= n2= [optional parameters]		",
" 									",
" Required Parameters:							",
" n1		number of elements in 1st (fast) dimension of matrix	",
" n2		number of elements in 2nd (middle) dimension of matrix	",
" 									",
" Optional Parameters:							",
" n3=all	number of elements in 3rd (slow) dimension of matrix	",
" perm=231		desired output axis ordering		   	",
" nbpe=sizeof(float)	number of bytes per matrix element		",
" scratchdir=/tmp	directory for scratch files			",
" scratchstem=foo	stem prefix for scratch file names		",
" verbose=0		=1 for diagnostic information			",
" 									",
NULL};
/**************** end self doc ********************************/

/*
 * AUTHOR:  Stewart A. Levin, Landmark Graphics, 01/20/10
 */

int
main (int argc, char **argv)
{
	int n1,n2,n3,nbpe,perm,i1,i2,i3,verbose;
	char *v=NULL;
	char *tempstem=NULL, **tempdirs=NULL;
	int ntempdirs;
	char tmpstem[] = "foo";
	char tmpdir[] = "/tmp";
	FILE *infp=stdin,*outfp=stdout;
	VND *handle;
	long N[3] ;
	long key[3] ;

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(1);

	/* get parameters */
	if (!getparint("n1",&n1)) err("must specify n1!");
	if (!getparint("n2",&n2)) err("must specify n2!");
	ntempdirs = countparval("scratchdir");
	if(ntempdirs > 0) {
		tempdirs = (char **) ealloc1(ntempdirs, sizeof(char *));
		getparstringarray("scratchdir",tempdirs);
	} else  {
		ntempdirs = 1;
		tempdirs = (char **) ealloc1(ntempdirs, sizeof(char *));
		strcpy(tempdirs[0],tmpdir);
	}
	if (!getparstring("scratchstem",&tempstem))
		tempstem = &tmpstem[0];
	if (!getparint("nbpe",&nbpe)) nbpe = sizeof(float);
	if (!getparint("perm",&perm)) perm = 231; /* trace to time slice */
	if (!getparint("n3",&n3)) {
		if (efseeko(infp,(off_t) 0,SEEK_END)==-1)
			err("input file size unknown; specify n3\n");
		n3 = (int) (eftello(infp)/(((off_t)n1)*((off_t)n2)*((off_t)nbpe)));
		efseeko(infp, (off_t) 0,SEEK_SET);
	}
	verbose = 0;  getparint("verbose",&verbose);
	if (verbose) warn("n1=%d  n2=%d n3=%d nbpe=%d perm=%d\n",n1,n2,n3,nbpe,perm);
    
        checkpars();

	/* allocate space for a single vector in any dimension */
	v = ealloc1((n1+n2+n3),nbpe);

	/* allocate big matrix state */
	N[0] = n1; N[1] = n2; N[2] = n3;
	handle = VNDop(2,200000000,3,N,1,nbpe,tempstem,ntempdirs,tempdirs,1);

	/* put vectors along 1st dimension to big matrix */
	if (verbose) warn("Reading input file");
	for (i3=0; i3<n3; i3++) {
	for (i2=0; i2<n2; i2++) {
		if (fread(v,1,nbpe*n1,infp)!=nbpe*n1)
			err("Error reading input file:  i2=%d\n",i2);
		key[0] = 0; key[1] = i2; key[2] = i3;
		VNDrw('w',0,handle,0,key,0,v,0,1,n1,21,NULL);
	}
	}

	/* get vectors along 2nd dimension from big matrix */
	if (verbose) warn("Writing output file");
	switch(perm) {

	case 123:
		for (i3=0; i3<n3; i3++) {
		for (i2=0; i2<n2; i2++) {
			key[0] = 0; key[1] = i2; key[2] = i3;
			VNDrw('r',0,handle,0,key,0,v,0,1,n1,123,NULL);
		if (fwrite(v,1,nbpe*n1,outfp)!=nbpe*n1)
			err("Error writing output file:  i2=%d i3=%d",i2,i3);
		}
		}
		break;

	case 132:
		for (i2=0; i2<n2; i2++) {
		for (i3=0; i3<n3; i3++) {
			key[0] = 0; key[1] = i2; key[2] = i3;
			VNDrw('r',0,handle,0,key,0,v,0,1,n1,132,NULL);
		if (fwrite(v,1,nbpe*n1,outfp)!=nbpe*n1)
			err("Error writing output file:  i2=%d i3=%d",i2,i3);
		}
		}
		break;

	case 213:
		for (i3=0; i3<n3; i3++) {
		for (i1=0; i1<n1; i1++) {
			key[0] = i1; key[1] = 0; key[2] = i3;
			VNDrw('r',0,handle,1,key,0,v,0,1,n2,213,NULL);
		if (fwrite(v,1,nbpe*n2,outfp)!=nbpe*n2)
			err("Error writing output file:  i1=%d i3=%d",i1,i3);
		}
		}
		break;

	case 231:
		for (i1=0; i1<n1; i1++) {
		for (i3=0; i3<n3; i3++) {
			key[0] = i1; key[1] = 0; key[2] = i3;
			VNDrw('r',0,handle,1,key,0,v,0,1,n2,231,NULL);
		if (fwrite(v,1,nbpe*n2,outfp)!=nbpe*n2)
			err("Error writing output file:  i1=%d i3=%d",i1,i3);
		}
		}
		break;

	case 312:
		for (i2=0; i2<n2; i2++) {
		for (i1=0; i1<n1; i1++) {
			key[0] = i1; key[1] = i2; key[2] = 0;
			VNDrw('r',0,handle,2,key,0,v,0,1,n3,312,NULL);
		if (fwrite(v,1,nbpe*n3,outfp)!=nbpe*n3)
			err("Error writing output file:  i1=%d i2=%d",i1,i2);
		}
		}
		break;

	case 321:
		for (i1=0; i1<n1; i1++) {
		for (i2=0; i2<n2; i2++) {
			key[0] = i1; key[1] = i2; key[2] = 0;
			VNDrw('r',0,handle,2,key,0,v,0,1,n3,321,NULL);
		if (fwrite(v,1,nbpe*n3,outfp)!=nbpe*n3)
			err("Error writing output file:  i1=%d i2=%d",i1,i2);
		}
		}
		break;

	default:
		err("Unrecognized transpose permutation %d",perm);
	}

	/* free big matrix state */
	VNDcl(handle,1);
	if (verbose) warn("Transpose done!");

	return(CWP_Exit());
}
