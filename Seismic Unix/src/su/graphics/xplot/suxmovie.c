/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUXMOVIE:  Revision: 1.22 ; Date: 2003/08/20 18:26:36  */

#include "su.h"
#include "segy.h"
#include <signal.h>

/*********************** self documentation *****************************/
char *sdoc[] = {
" 									",
" SUXMOVIE - X MOVIE plot of a 2D or 3D segy data set 			",
" 									",
" suxmovie <stdin [optional parameters]		 			",
" 						 	        	",
" Optional parameters: 							",
" 							        	",
" n1=tr.ns         	    	number of samples per trace  		",
" ntr=tr.ntr     	    	number of traces in the data set	",
" n2=tr.shortpad or tr.ntr	number of traces in inline direction 	",
" n3=ntr/n2     	    	number of traces in crossline direction	",
" 							        	",
" d1=tr.d1 or tr.dt/10^6    sampling interval in the fast dimension	",
"   =.004 for seismic 		(if not set)				",
"   =1.0 for nonseismic		(if not set)				",
" 							        	",
" d2=tr.d2		    sampling interval in the slow dimension	",
"   =1.0 			(if not set)				",
" 							        	",
" d3=1.0		    sampling interval in the slowest dimension	",
" 							        	",
" f1=tr.f1 or 0.0  	    first sample in the z dimension		",
" f2=tr.f2 or 1.0           first sample in the x dimension		",
" f3=1.0 		    						",
" 							        	",
" mode=0          0= x,z slice movie through y dimension (in line)      ",
"                 1= y,z slice movie through x dimension (cross line)   ",
"                 2= x,y slice movie through z dimension (time slice)   ",
" 							        	",
" verbose=0              =1 to print some useful information		",
"									",
" tmpdir=	 	if non-empty, use the value as a directory path	",
"		 	prefix for storing temporary files; else if the	",
"	         	the CWP_TMPDIR environment variable is set use	",
"	         	its value for the path; else use tmpfile()	",
" 									",
" Notes:",
" For seismic data, the \"fast dimension\" is either time or		",
" depth and the \"slow dimension\" is usually trace number.	        ",
" The 3D data set is expected to have n3 sets of n2 traces representing ",
" the horizontal coverage of n2*d2 in x  and n3*d3 in y direction.      ",
" 							        	",
" The data is read to memory with and piped to xmovie with the         	",
" respective sampling parameters.			        	",
" See the xmovie selfdoc for the remaining parameters and X functions.	",
" 							        	",
NULL};
/**************** end self doc *******************************************/

/* Credits:
 *
 *	CWP: Dave (xmovie), Jack (su tee shirt)
 *      IFM-GEOMAR: Gerald, rewrite for time slices (mode=2)
 *                    crosslines (mode=1) and inlines (mode=0)
 *
 * Notes:
 *	When n2,n3 isn't getparred, we need to count the traces
 *	for xmovie.  In this case:
 *	we are using tmpfile because on many machines it is
 *	implemented as a memory area instead of a disk file.
 *	Although we compute ntr, we don't allocate a 2-d array
 *	and content ourselves with copying trace by trace from
 *	the data "file" to the pipe into the plotting program.
 *	Although we could use tr.data, we allocate a trace buffer
 *	for code clarity.
 */
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files	*/
char tracefile[BUFSIZ];	/* filename for trace storage file	*/
FILE *tracefp;		/* fp for trace storage file		*/

segy tr;

int
main(int argc, char **argv)
{
  char plotcmd[BUFSIZ];	/* build xmovie command for popen 	*/

  float ***trbuf=0;	/* read trace buffer			*/

  FILE *plotfp;		/* fp for plot data			*/
 
  int ix,iy;
  int itr;		/* trace counter */
  int n1;		/* number of samples on trace		*/
  int n2;		/* number of traces per inline (x)	*/
  int n3;		/* number of traces per crossline (y)	*/
  int ntr;		/* number of traces			*/

  int verbose;		/* verbose flag				*/
  short mode;           /* movie mode                           */

  float d1;		/* time/depth sample rate 		*/
  float d2;		/* trace/dx sample rate (inline)	*/
  float d3;		/* trace/dy sample rate (crossline)	*/
  float f1;		/* tmin/zmin				*/
  float f2;		/* tracemin/xmin			*/
  float f3;		/* tracemin/ymin	 		*/
  cwp_Bool seismic;	/* is this seismic data?		*/
  cwp_Bool got_n2 = cwp_false;/* was n2 getparred?		*/
  cwp_Bool got_n3 = cwp_false;/* was n3 getparred?		*/
  
  char *cwproot;	/* value of CWPROOT environment variable*/
  char *bindir;		/* directory path for tmp files		*/
  
  char *tmpdir;		/* directory path for tmp files		*/
  cwp_Bool istmpdir=cwp_false;/* true for user given path	*/
  
  /* Initialize */
  initargs(argc, argv);
  requestdoc(1);
  
  /* Get info from first trace */
  if (!gettr(&tr)) err("can't get first trace");
  seismic = ISSEISMIC(tr.trid);

  if (!getparint("verbose", &verbose))	verbose=0;
  if (!getparshort("mode",&mode))          mode=0;

  if (!getparfloat("d1", &d1)) {
    if       (tr.dt)  d1 = ((double) tr.dt)/1000.0;
    else if  (tr.d1)  d1 = tr.d1;
    else {
      if (seismic) {
	d1 = 0.004;
	warn("tr.dt not set, assuming dt=0.004");
      } else { /* non-seismic data */
	d1 = 1.0;
	warn("tr.dt not set, assuming d1=1.0");
      }
    }
  }
  

  /* get number of traces in file */
  if (!getparint("ntr", &ntr)) ntr = tr.ntr;
  
  /* Get n1 */
  if (!getparint("n1", &n1)) n1 = tr.ns;
  /* Get n2 */
  if (getparint("n2", &n2)) got_n2 = cwp_true;  
  /* Get n3 */
  if (getparint("n3", &n3)) got_n3 = cwp_true;

  if (!got_n2) { /* is ntr and n2 set ? */
   if ( tr.shortpad ) { 
     n2 = tr.shortpad; 
     got_n2 = cwp_true;
     warn("n2 unset, used tr.shortpad : n2=%i ",n2);
   } else 
     if ( tr.ntr ) {
     n2 = tr.ntr; 
     got_n2 = cwp_true;
     warn("n2 unset, used tr.ntr : n2=%i ",n2);     
   }
  }
  if (!got_n3) { /* is ntr and n2 set ? */
   if ( ntr != n2 && ntr > 0 ) {/* ntr multiple of n2 ?*/
    if ( ntr%n2 == 0 ) {
     n3 = ntr / n2 ;
     got_n3 = cwp_true;
     warn("n3 unset, but deduced from ntr=%i and n2=%i : n3=%i ",
    	ntr,n2,n3);
    }
   } else { /* n2 = ntr */
     n3 = 1.0 ;
   }
  }
  
  if ( ntr != (n2*n3) ) { 
    warn("ntr=%i inconsistent ! set ntr=(n2=%i)*(n3=%i), trace count required.",
    	ntr,n2,n3);
    ntr= n2*n3; got_n3= cwp_false ;
  }

  if (!getparfloat("d1", &d1)) d1 = (tr.d1) ? tr.d1 : 1.0;
  if (!getparfloat("d2", &d2)) d2 = (tr.d2) ? tr.d2 : 1.0;
  if (!getparfloat("d3", &d3)) d3 = 1.0;
  
  if (!getparfloat("f1", &f1)) f1 = (tr.f1) ? tr.f1 : 0.0;
  if (!getparfloat("f2", &f2)) f2 = (tr.f2) ? tr.f2 : 1.0;
  if (!getparfloat("f3", &f3)) f3 = 1.0;  

  /* Look for user-supplied tmpdir */
  if (!getparstring("tmpdir",&tmpdir) &&
      !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
  if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
    err("you can't write in %s (or it doesn't exist)", tmpdir);
  
  /* See if CWPBIN environment variable is not set */
  if (!(bindir = getenv("CWPBIN"))) { /* construct bindir from CWPROOT */
    
    bindir = (char *) emalloc(BUFSIZ);

    /* Get value of CWPROOT environment variable */
    if (!(cwproot = getenv("CWPROOT"))) cwproot ="" ;
    if (STREQ(cwproot, "")) {
      warn("CWPROOT environment variable is not set! ");
      err("Set CWPROOT in shell environment as per instructions in CWP/SU Installation README files");
    }
    /* then bindir = $CWPROOT/bin */
    sprintf(bindir, "%s/bin", cwproot);
  }
  strcat(bindir,"/");   /* put / at the end of bindir */

  /* Allocate trace buffer */
  if (got_n3)   trbuf = ealloc3float(n1,n2,n3);
	
  /* count and store traces  */
	
  if (!got_n3) { /* count traces and store in tmpfile */
    warn("must have n3, counting traces ... "); 

    /* Create temporary "file" to hold data */
    if (STREQ(tmpdir,"")) {
      if (verbose) warn("using tmpfile() call");
      tracefp = etmpfile();
    } else { /* user-supplied tmpdir */
      char directory[BUFSIZ];
      strcpy(directory, tmpdir);
      strcpy(tracefile, temporary_filename(directory));
      if (verbose)
	warn("putting temporary files in %s", directory);
      /* Handle user interrupts */
      signal(SIGINT, (void (*) (int)) closefiles);
      signal(SIGTERM, (void (*) (int)) closefiles);
      tracefp = efopen(tracefile, "w+");
      istmpdir=cwp_true;		
    }
  }

  /* Loop over input traces & put them into the data file */
  itr = 0;
  iy  = 0;
  ix  = -1;
  do {
    ++itr;		
    if (++ix>=n2) {		
      ix=0;iy++; 
      if (verbose>1) 
	fprintf(stderr,"trace %i,ix=%i,iy=%i\n",itr,ix,iy);
    } ;


    if (got_n3) /* write to memory */
      memcpy((void *) trbuf[iy][ix],(const void *)tr.data,n1*FSIZE);
    else      /* write to tempfile */
      efwrite(tr.data, FSIZE, n1, tracefp);
    
  } while (gettr(&tr));

  if (itr != ntr) { 
   warn("ntr=%i and trace count inconsistent! resetting ntr=%i",
    	ntr,itr);
	ntr=itr;
  }
  
  if (verbose) 
    fprintf(stderr,"End at trace %i,ix=%i,iy=%i\n",ntr,ix+1,iy+1);
  
  /* set n3 if not getparred */
  if (!got_n3) { 
    register int ix,iy;
    float *rbuf;

    if (!got_n2) {
       n2 = ntr;
    }
    n3 = ntr / n2 ; 
    if (verbose) warn("counted %i traces, set n3=%i",ntr,n3);

    if (verbose)
      fprintf(stderr,"allocating memory and reading tempfile to memory ... ");

    rewind(tracefp);
    trbuf = ealloc3float(n1,n2,n3); 
    rbuf  = ealloc1float(n1);

    for (iy=0;iy<n3;++iy)
      for (ix=0;ix<n2;++ix) {
	efread (rbuf, FSIZE, n1, tracefp);
	memcpy((void *) trbuf[iy][ix],(const void *)rbuf,n1*FSIZE);
      }
    free(rbuf);
    if (verbose)
      fprintf(stderr,"\t done !\n");
  }
	
  /* use transposed data array for output to create time/depth slices */
  if (verbose) {
    float range,max;
    warn("Cube dimensions:");
    range=(n2-1)*d2;    max=f2+range;
    fprintf(stderr," x range: %g + (%i * %g) = %g\n",f2,n2,d2,max);
    range=(n3-1)*d3;    max=f3+range;
    fprintf(stderr," y range: %g + (%i * %g) = %g\n",f3,n3,d3,max);
    range=(n1-1)*d1;    max=f1+range;
    fprintf(stderr," z range: %g + (%i * %g) = %g\n",f1,n1,d1,max);
  }
		

  /* Set up xmovie command line */
  switch ( mode ) {
  case 0 :
    sprintf(plotcmd, "%sxmovie n1=%d n2=%d d1=%f d2=%f f1=%f f2=%f", bindir,
	    n1, n2, d1, d2, f1, f2);
    break;
  case 1 :
    sprintf(plotcmd, "%sxmovie n1=%d n2=%d d1=%f d2=%f f1=%f f2=%f", bindir,
	    n1, n3, d1, d3, f1, f3);
    break;
  case 2 :
    sprintf(plotcmd, "%sxmovie n1=%d n2=%d d1=%f d2=%f f1=%f f2=%f", bindir,
	    n2, n3, d2, d3, f2, f3);
    break;
  default :
    err("Mode %i unkown",mode); 
  }
	
  for (--argc, ++argv; argc; --argc, ++argv) {
    
    /* skip those already set */
    if (strncmp(*argv, "d1=", 3) &&
	strncmp(*argv, "d2=", 3) &&
	strncmp(*argv, "d3=", 3) &&
	strncmp(*argv, "n1=", 3) &&
	strncmp(*argv, "n2=", 3) &&
	strncmp(*argv, "n3=", 3) &&
	strncmp(*argv, "ntr=", 3) &&
	strncmp(*argv, "f1=", 3) &&
	strncmp(*argv, "f3=", 3) &&
	strncmp(*argv, "f2=", 3)) {
      
      /* put a space between args */
      strcat(plotcmd, " ");
      
      /* user quotes are stripped */
      strcat(plotcmd, "\"");
      strcat(plotcmd, *argv); /* add the arg */
      
      /* user quotes are stripped */
      strcat(plotcmd, "\"");
    }
  }

  /* Open pipe to xmovie and send the traces */
  if (verbose) {
    warn("open pipe for plot command ... ");
    fprintf(stderr,"call %s \n",plotcmd);
  }
  plotfp = epopen(plotcmd, "w");
	
  /* send out stored traces one by one */
  
  { /*register int itr;*/
    register int ix,iy,iz;
    float *wrbuf;

    switch ( mode ) {    
    case 0 :   /*  vertical (x) slices   */
      for   (iy  = 0; iy  < n3;  ++iy ) 
	for   (ix  = 0; ix  < n2;  ++ix ) 
	  efwrite(trbuf[iy][ix], FSIZE, n1, plotfp);
      break;
    case 1 :   /*  vertical (y) slices   */
      for   (ix  = 0; ix  < n2;  ++ix ) 
	for   (iy  = 0; iy  < n3;  ++iy ) 
	  efwrite(trbuf[iy][ix], FSIZE, n1, plotfp);
      break;
    case 2 :   /*  horizontal slices */
      wrbuf=ealloc1float(n2);
      for   (iz = 0; iz < n1; ++iz) {	
	for   (iy  = 0; iy  < n3;  ++iy ) {
	  memset((void *)wrbuf,(int)'\0',n1*FSIZE);
	  for   (ix  = 0; ix  < n2;  ++ix ) 
	    wrbuf[ix]= trbuf[iy][ix][iz] ;
	  efwrite(wrbuf, FSIZE, n2, plotfp);
	}
	
	/* efwrite(trbuf[iy][ix], FSIZE, ntr, plotfp);
	 *
	 * efwrite(wrbuf, FSIZE, n1, plotfp); */
      }
      free(wrbuf);    	
      break;
    default :
      err("Mode %i unkown",mode);
      break;
    }
  }
  free(trbuf);
  /* Clean up */
  epclose(plotfp);
  
  if (!got_n3) {
    efclose(tracefp);
    if (istmpdir) eremove(tracefile);
  }
  
  return EXIT_SUCCESS;
}

static void closefiles(void)
{
  efclose(tracefp);
  eremove(tracefile);
  exit(EXIT_FAILURE);
}
