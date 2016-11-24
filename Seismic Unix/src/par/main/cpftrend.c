/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* CPFTREND: $Revision: 1.4 $ ; $Date: 2012/03/28 21:43:33 $	*/

#include "par.h"

/*********************** self documentation **********************/
char* sdoc[] = {
"									",
"   CPFTREND - generate picks of the Cumulate Probability Function 	",
"									",
" Required parameters:							",
"  ix=      - column containing X variable				",
"  iy=      - column containing Y variable				",
"  min_x=   - minimum X bin						",
"  max_x=   - maximum X bin						",
"  min_y=   - minimum Y bin 						",
"  max_y=   - maximum Y bin						",
"									",
" Optional parameters:							",
"  nx=100    - number of X bins 					",
"  ny=100    - number of Y bins 					",
"  logx=0   - =1 use logarithmic scale for X axis			",
"  logy=0   - =1 use logarithmic scale for Y axis			",
"  ir=       - column containing reject variable 			",
"  rmin=     - reject values below rmin 				",
"  rmax=     - reject values above rmax 				",
"              NOTE: only one, rmin or rmax, may be used		",
" NOTES:								",
"  cpftrend makes picks on the 2D cumulate representing the		",
"  probability density function of the input data.			",
"									",
"   Commandline options allow selecting any of several normalizations	",
"   to apply to the distributions.					",
"									",
NULL};

/********************************************************************
 *  cpftrend(1) makes picks on the 2D cumulate representing the
 *  probability density function of the input data.
 *
 *   Commandline options allow selecting any of several normalizations 
 *   to apply to the distributions.
 *
 * Credits:  Reginald H. Beardsley                      rhb@acm.org
 *     Copyright 2006 Exploration Software Consultants Inc.
**********************************************************************/
/**************** end self doc ********************************/

int
main(int argc ,char **argv)
{

	char buf[1024];
	char *ptr;

	int i;
	int j;
	int k;

	int total;

	int t1;
	int t2;
	int t3;
	int t4;
	int t5;

	float r;
	float v[128];

	float x;
	float dx;
	float min_x;
	float max_x;

	float rmin = 0.0;
	float rmax = 0.0;


	float y;
	float dy;
	float min_y;
	float max_y;

	int ix;
	int iy;
	int ir = -1;

	int nx = 100;
	int ny = 100;

	float** bin;
	int* xcnt;

	float sigma;

	/* intermediates for calculating standard deviation */

	float* a;
	float* b;

	float sum;
	float m;

	int logx = 0;
	int logy = 0;

/*--------------------------------------------------------------------*\
	Get the commandline arguents in SU style 
\*--------------------------------------------------------------------*/

	initargs( argc, argv );
	requestdoc( 1 );

	if( !getparint( "ix" ,&ix ) ){
	   err( "Missing X column index, ix=" );
	}

	if( !getparint( "iy" ,&iy ) ){
	   err( "Missing Y column index, ix=" );
	}

	getparint( "ir" ,&ir );

	if(   ir > 0 
	   && !getparfloat( "rmax" ,&rmax ) 
	   && !getparfloat( "rmin" ,&rmin ) ){
	   err( "Failed to specify rejection threshold" );
	} 

	if( !getparfloat( "min_x" ,&min_x ) ){
	   err( "min_x= not specified" );
	}

	if( !getparfloat( "max_x" ,&max_x ) ){
	   err( "max_x= not specified" );
	}

	if( !getparfloat( "min_y" ,&min_y ) ){
	   err( "min_y= not specified" );
	}

	if( !getparfloat( "max_y" ,&max_y ) ){
	   err( "max_y= not specified" );
	}

	getparint( "nx" ,&nx );
	getparint( "ny" ,&ny );

	getparint( "logx" ,&logx );
	getparint( "logy" ,&logy );
        checkpars();


	if( logx ){

	   if( min_x < 0 || max_x < 0 ){
	      err( "Cannot use logscale for X" );

	   }else{
	      min_x = log(min_x);
	      max_x = log(max_x);
	   }

	}

	if( logy ){

	   if( min_y < 0 || max_y < 0 ){
	      err( "Cannot use logscale for Y" );

	   }else{
	      min_y = log(min_y);
	      max_y = log(max_y);

	   }
	}

	dx = (max_x - min_x) / nx;
	dy = (max_y - min_y) / ny;

/*--------------------------------------------------------------------*\
	Allocate and clear the space for the work arrays.
\*--------------------------------------------------------------------*/

	bin  = (float**) ealloc2( ny ,nx ,sizeof(float) );
	xcnt = (int*)    ealloc1( nx ,sizeof(int) );

	a = (float*)  ealloc1( nx ,sizeof(float) );
	b = (float*)  ealloc1( nx ,sizeof(float) );

	for( i=0; i<nx; i++ ){
	   memset( bin[i] ,0 ,ny*sizeof(float) );
	}
	memset( xcnt ,0 ,nx*sizeof(int) );
	memset( a    ,0 ,nx*sizeof(float) );
	memset( b    ,0 ,nx*sizeof(float) );

	total = 0;

/*--------------------------------------------------------------------*\
	Read the input data as space delimited ASCII text
\*--------------------------------------------------------------------*/

	while (!feof(stdin) && fgets( buf ,sizeof(buf) ,stdin ) ){

	  /* tokenize and read input */

	   ptr = buf;
	   k = 0;

	   while( (ptr=strtok( ptr ," \t\n" )) ){
	      sscanf( ptr ,"%f" ,&v[k] );
	      k++;
	      ptr = 0;
	    }

	    x = v[ix-1];
	    y = v[iy-1];

	    if( logx && x > 0 ){
	       x = log(x);
	    }

	    if( logy && y > 0 ){
	       y = log(y);
	    }

	    i = (x - min_x ) /dx;
	    j = (y - min_y ) /dy;

	    if( ir > 0 ){

	       /* 3rd value threshold options */

	       r = v[ir-1];

	       if( rmin != 0.0 
	          && r > rmin 
	          && i > 0 && i < nx 
	          && j > 0 && j < ny ){

	          /* lower threshold */

	          bin[i][j]++;
	          xcnt[i]++;
	          a[i] += y*y;
	          b[i] += y;
	          total++;

	       }else if( rmax != 0.0 
	          && r < rmax
	          && i > 0 && i < nx 
	          && j > 0 && j < ny ){

	          /* upper threshold */

	          bin[i][j]++;
	          xcnt[i]++;
	          a[i] += y*y;
	          b[i] += y;
	          total++;

	      }

	   }else{

	       /* no threshold value */

	       if( i > 0 && i < nx 
	        && j > 0 && j < ny ){

	          bin[i][j]++;
	          xcnt[i]++;
	          a[i] += y*y;
	          b[i] += y;
	          total++;

	       }

	   }

	}

	for( i=0; i<nx; i++ ){

	   if( xcnt[i] > 0.0001*total ){

	      sum = 0.0;

	      if( logx ){
	         printf( "%f " ,exp(min_x + i*dx) );

	      }else{
	         printf( "%f " ,min_x + i*dx );

	      }

	      printf( "%d " ,xcnt[i]       );

	      t1 = 0;
	      t2 = 0;
	      t3 = 0;
	      t4 = 0;
	      t5 = 0;

	      for( j=0; j<ny; j++ ){

	         sum += bin[i][j];

	         m = (100.0*sum ) / xcnt[i];
	            
	         if( !t1 && m >= 2.28 ){
	            t1 = 1;

	            if( logy ){
	               printf( "%f " ,exp(min_y + j*dy) );

	            }else{
	               printf( "%f " ,min_y + j*dy );

	            }

	         }else if( !t2 && m >= 15.87 ){
	            t2 = 1;

	            if( logy ){
	               printf( "%f " ,exp(min_y + j*dy) );

	            }else{
	               printf( "%f " ,min_y + j*dy );

	            }

	         }else if( !t3 && m >= 50.00 ){
	            t3 = 1;

	            if( logy ){
	               printf( "%f " ,exp(min_y + j*dy) );

	            }else{
	               printf( "%f " ,min_y + j*dy );

	            }

	         }else if( !t4 && m >= 84.13 ){
	            t4 = 1;

	            if( logy ){
	               printf( "%f " ,exp(min_y + j*dy) );

	            }else{
	               printf( "%f " ,min_y + j*dy );

	            }

	         }else if( !t5 && m >= 97.72 ){
	            t5 = 1;

	            if( logy ){
	               printf( "%f " ,exp(min_y + j*dy) );

	            }else{
	               printf( "%f " ,min_y + j*dy );

	            }

	         }

	      }

	      sigma = sqrt( (xcnt[i]*a[i] - b[i]*b[i])/(xcnt[i]*xcnt[i]-1) );

	      if( logy && sigma > 0 ){
	         printf( "%f " ,log(sigma) );

	      }else{
	         printf( "%f " ,sigma );

	      }

	      printf( "\n" );

	   }


	}

	return EXIT_SUCCESS;
}

