/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* REGRID3: $Revision: 1.4 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
"									",
" REGRID3 - REwrite a [ni3][ni2][ni1] GRID to a [no3][no2][no1] 3-D grid",
" 									",
" regrid3 < oldgrid > newgrid [parameters]				",
" 									",
" Optional parameters:							",
" ni1=1   fastest (3rd) dimension in input grid                         ",
" ni2=1   second fastest (2nd) dimension in input grid			",
" ni3=1   slowest (1st) dimension in input grid                       	",
" no1=1   fastest (3rd) dimension in output grid                        ",
" no2=1   second fastest (2nd) dimension in output grid                 ",                
" no3=1   slowest (1st) dimension in output grid                        ",
" Optional Parameters:                                                  ",
" verbose=0	=1 print some useful information			",
"									",
" Notes:								",
" REGRID3 can be used to span a 1-D grid to a 2-D grid,  or a 2-D grid  ",
" to a 3-D grid; or to change grid parameters within the dimensions.	",
" Together with MUL and UNIF3, most 3-D velocity model can be 		",
" constructed.								",NULL};

/*
 * Credits:
 *  	CWP: Zhaobo Meng, 1996, Colorado School of Mines
 */
/**************** end self doc *******************************************/

int
main(int argc, char **argv)
{

	float ***gridi;              /*input 3d grid*/
	float ***grido;    	     /*output 3d grid*/ 
	int ni1,ni2,ni3;             /*dimensions if input grid*/
	int no1,no2,no3;   	     /*dimensions if output grid*/

	int ix1,ix2,ix3;
	int ix1plus,ix2plus,ix3plus; /*indices+1*/
	float alpha1,alpha2,alpha3;  /*weights for interpolations*/
	int io1,io2,io3;	     /*indices for output file*/
	float xi1,xi2,xi3;
	int verbose;

	/* hook up getpar */
	initargs(argc,argv);
	requestdoc(1);

	/**************************************************
	Input 3D grid size: ni1 by ni2 by ni3;
	output 3D grid size: no1 by no2 by no3.
	The default size of input is 1 by 1 by 1;
	and the default size of output is also 1 by 1 by 1
	**************************************************/	
        if (!getparint("ni1",&ni1))    ni1=1;
        if (!getparint("ni2",&ni2))    ni2=1;
	if (!getparint("ni3",&ni3))    ni3=1;	

        if (!getparint("no1",&no1))    no1=1;
        if (!getparint("no2",&no2))    no2=1;
        if (!getparint("no3",&no3))    no3=1;   

	if (!getparint("verbose",&verbose)) verbose=0;
        checkpars();

	if (verbose) {
		warn("ni1,ni2,ni3=%d %d %d",ni1,ni2,ni3);
                warn("no3,no2,no1=%d %d %d",no1,no2,no3);
	}		

	if (ni1<=0 || ni2<=0 || ni3<=0 || no1<0 || no2<=0 || no3<=0) 
		err("ni1,ni2,ni3 and no1,no2,no3 must be all positive");

	gridi=alloc3float(ni1,ni2,ni3);
        grido=alloc3float(no1,no2,no3);

	if (fread(gridi[0][0],sizeof(float),ni3*ni2*ni1,stdin)!=ni3*ni2*ni1)
		err("Can not read in gridi");

	/*linear interpolation*/
	for (io1=0;io1<no1;io1++) {
		xi1=(float)io1*(float)ni1/(float)no1;
		ix1=MIN(ni1-1,(int)xi1);
		alpha1=xi1-ix1;
		ix1plus=MIN(ni1-1,ix1+1);
		for (io2=0;io2<no2;io2++) {
		        xi2=(float)io2*(float)ni2/(float)no2;
	                ix2=MIN(ni2-1,(int)xi2);
			alpha2=xi2-ix2;
			ix2plus=MIN(ni2-1,ix2+1);
			for (io3=0;io3<no3;io3++) {
	                        xi3=(float)io3*(float)ni3/(float)no3;
	                        ix3=MIN(ni3-1,(int)xi3);
				alpha3=xi3-ix3;
				ix3plus=MIN(ni3-1,ix3+1);
				grido[io3][io2][io1]=
			alpha1*(alpha2*
					(alpha3*gridi[ix3plus][ix2plus][ix1plus]+
					(1-alpha3)*gridi[ix3plus][ix2plus][ix1])
				+
				(1-alpha2)*
					(alpha3*gridi[ix3plus][ix2][ix1plus]+
					(1-alpha3)*gridi[ix3plus][ix2][ix1])
				)+
			(1-alpha1)*
				(alpha2*
					(alpha3*gridi[ix3][ix2plus][ix1plus]+
					(1-alpha3)*gridi[ix3][ix2plus][ix1])
				+
                                (1-alpha2)*
					(alpha3*gridi[ix3][ix2][ix1plus]+
					(1-alpha3)*gridi[ix3][ix2][ix1])	
				);
			}
		}
	}

	/*write the output grid to stdout*/
	fwrite(grido[0][0],sizeof(float),no3*no2*no1,stdout);

	return(CWP_Exit());
}
