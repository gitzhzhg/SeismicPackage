/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* PDFHISTOGRAM: $Revision: 1.3 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" PDFHISTOGRAM - generate a HISTOGRAM of the Probability Density function",
" 									",
"  pdfhistogram < stdin > sdtout [Required params] (Optional params)	",
" 									",
" Required parameters: 							",
" ix=		column containing X variable				",
" iy=		column containing Y variable 				",
" min_x=	minimum X bin						",
" max_x=	maximum X bin						",
" min_y=	minimum Y bin						",
" max_y=	maximum Y bin						",
" logx=0	=1 use logarithmic scale for X axis			",
" logy=0	=1 use logarithmic scale for Y axis			",
" norm=	selected normalization type 					",
"		sqrt	- bin / sqrt( xnct*ycnt) 			",
"		avg_cnt	- 0.5* bin / (xcnt + ycnt) 			",
"		avg_sum	- (bin / xcnt + bin / ycnt ) / 2 		",
"		xcnt	- bin / xcnt 					",
"		ycnt	- bin / ycnt 					",
"		log	- log(bin) 					",
"		total	- bin / total 					",
" Optional parameters: 							",
"  nx=100	- number of X bins					",
"  ny=100	- number of Y bins 					",
"  ir=	- column containing reject variable 				",
"  rmin=	- reject values below rmin 				",
"  rmax=	- reject values above rmax 				",
"		NOTE: only one, rmin or rmax, may be used 		",
" Notes:								",
" PDFHISTOGRAM creates a 2D histogram representing the probability density",
" function of the input data. The output is in the form of a binary array",
" that can then be plotted via ximage.					",
" Commandline options allow selecting any of several normalizations	",
" to apply to the distributions.					",
NULL};

/*
 * Credits:
 *  Reginald H. Beardsley	rhb@acm.org
 *	Copyright 2006 Exploration Software Consultants Inc.
 */
/**************** end self doc ********************************/

int 
main( int argc ,char **argv )
{

	char buf[1024];
	char *ptr=NULL;

	cwp_String norm_str;

	int i;
	int j;
	int k;

	int total;

	float zero = 0.0;
	float clip = 0.0;

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

	int logx = 0;
	int logy = 0;

	int ix;
	int iy;
	int ir = -1;

	int nx = 100;
	int ny = 100;

	float **bin=NULL;
	int *xcnt=NULL;
	int *ycnt=NULL;

	enum { 
			SQRT 
			,AVG_CNT
			,AVG_SUM
  		,XCNT
			,YCNT
			,LOG
			,TOTAL
		}norm;

	norm = TOTAL;

/*--------------------------------------------------------------------*\
	Get the commandline arguments in SU style 
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

	if(	ir > 0 
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

	getparint( "logx" ,&logx );
	getparint( "logy" ,&logy );

	getparint( "nx" ,&nx );
	getparint( "ny" ,&ny );

	if( getparstring( "norm" ,&norm_str ) ){

		if( !strcmp( norm_str ,"sqrt" ) ){
	 		norm = SQRT;

		} else  if( !strcmp( norm_str ,"avg_cnt" ) ){
	 		norm = AVG_CNT;

		} else  if( !strcmp( norm_str ,"avg_sum" ) ){
	 		norm = AVG_SUM;

		} else  if( !strcmp( norm_str ,"xcnt" ) ){
	 		norm = XCNT;

		} else  if( !strcmp( norm_str ,"ycnt" ) ){
	 		norm = YCNT;

		} else  if( !strcmp( norm_str ,"log" ) ){
	 		norm = LOG;

		} else  if( !strcmp( norm_str ,"total" ) ){
	 		norm = TOTAL;

		} else {
			 err( "Unknown normalization" );

		}
	}

        checkpars();
	if( logx ){

	if( min_x < 0 || max_x < 0 ){
		 err( "Cannot use logscale for X" );

		} else {
			 min_x = log(min_x);
			 max_x = log(max_x);

		}
	}
	

	if( logy ){

		if( min_y < 0 || max_y < 0 ){
	 		err( "Cannot use logscale for Y" );

		} else {
			 min_y = log(min_y);
	 		max_y = log(max_y);

		}
	}
	
	
	dx = (max_x - min_x) / nx;
	dy = (max_y - min_y) / ny;

/*--------------------------------------------------------------------*\
	Allocate and clear the space for the work arrays.
\*--------------------------------------------------------------------*/

	bin  = (float**) ealloc2( nx ,ny ,sizeof(float) );
	xcnt = (int*)	ealloc1( nx ,sizeof(int) );
	ycnt = (int*)	ealloc1( ny ,sizeof(int) );

	for( i=0; i<ny; i++ ){
		memset( bin[i] ,0 ,ny*sizeof(float) );
	}
	memset( xcnt ,0 ,nx*sizeof(int) );
	memset( ycnt ,0 ,ny*sizeof(int) );

	total=0;

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

	i = (x - min_x ) / dx;
	j = (y - min_y ) / dy;

	if( ir > 0 ){

	  /* 3rd value threshold options */

	  r = v[ir-1];

	  if( rmin != 0.0 
		&& r > rmin 
		&& i > 0 && i < nx 
		&& j > 0 && j < ny ){

		/* lower threshold */

		bin[j][i]++;
		xcnt[i]++;
		ycnt[j]++;
		total++;

	  } else  if( rmax != 0.0 
		&& r < rmax
		&& i > 0 && i < nx 
		&& j > 0 && j < ny ){

		/* upper threshold */

		bin[j][i]++;
		xcnt[i]++;
		ycnt[j]++;
		total++;

	 }

	} else {

	  /* no threshold value */

	  if( i > 0 && i < nx 
		&& j > 0 && j < ny ){

		bin[j][i]++;
		xcnt[i]++;
		ycnt[j]++;
		total++;

	  }

	}
	
	}

/*--------------------------------------------------------------------*\
	Apply normalization factor to histogram values.
\*--------------------------------------------------------------------*/

	for( i=0; i<nx; i++ ){

		for( j=0; j<ny; j++ ){

	 		if( bin[j][i] > 0 ){

			if( norm == SQRT && xcnt[i] > 1e-4*total 
			&& ycnt[j] > 1e-4*total ){

			bin[j][i] = bin[j][i] / sqrt( (float)xcnt[i] * (float)ycnt[j] );

	} else  if( norm == AVG_CNT && xcnt[i] > 1e-4*total 
	&& ycnt[j] > 1e-4*total ){

		bin[j][i] = bin[j][i] / (float)xcnt[i] + bin[j][i] / (float)ycnt[j];
		bin[j][i] *= 0.5;

	} else  if( norm == AVG_SUM && xcnt[i] > 1e-4*total 
			&& ycnt[j] > 1e-4*total){

		bin[j][i] = 2.0* bin[j][i] / ((float)xcnt[i] + (float)ycnt[j]);

	} else  if( norm == XCNT && xcnt[i] > 1e-4*total ){
		bin[j][i] = bin[j][i] / (float)xcnt[i];

	} else  if( norm == YCNT && ycnt[i] > 1e-4*total ){
		bin[j][i] = bin[j][i] / (float)ycnt[i];

	} else  if( norm == LOG ){
		bin[j][i] = log(bin[j][i]);

	} else  if( norm == TOTAL ){
		bin[j][i] = bin[j][i] / total;

	} else {
		bin[j][i] = 0.0;

	}

	if( bin[j][i] != bin[j][i] ){

		fprintf( stderr ,"Nan at " );

		if( logx ){
		  fprintf( stderr ,"%f " ,exp(min_x + i*dx) );

		} else {
		  fprintf( stderr ,"%f " ,min_x + i*dx );

		}

		if( logy ){
		  fprintf( stderr ,"%f " ,exp(min_y + j*dy) );

		} else {
		  fprintf( stderr ,"%f " ,min_y + j*dy );

		}

		fprintf( stderr ,"\n" );

	}

	/* save clip value for plotting */

	if( bin[j][i] > clip ){
		clip = bin[j][i];
	}

	 } else {
	bin[j][i] = 0.0;

	 }
	}
	}

	clip *= 0.9;

	fwrite( &nx	,sizeof(int)	,1 ,stdout );
	fwrite( &min_x ,sizeof(float) ,1 ,stdout );
	fwrite( &dx	,sizeof(float) ,1 ,stdout );

	fwrite( &ny	,sizeof(int)	,1 ,stdout );
	fwrite( &min_y ,sizeof(float) ,1 ,stdout );
	fwrite( &dy	,sizeof(float) ,1 ,stdout );

	fwrite( &zero  ,sizeof(float) ,1 ,stdout );
	fwrite( &clip  ,sizeof(float) ,1 ,stdout );

	for( j=0; j<ny; j++ ){
	fwrite( bin[j] ,nx*sizeof(float) ,1 ,stdout );
	}

	return EXIT_SUCCESS;
}
