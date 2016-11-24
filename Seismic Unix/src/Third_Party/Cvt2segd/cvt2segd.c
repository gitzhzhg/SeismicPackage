/* do our best to avoid 2Gig filesize limits */
#ifndef _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE
#endif
#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif

#ifdef __GNUC__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#include <byteswap.h>
#endif
#endif

#ifdef _AIX
#ifndef _ALL_SOURCE
#define _ALL_SOURCE
#endif
#ifndef _LARGE_FILES
#define _LARGE_FILES
#endif
#endif

#ifndef _FILE_OFFSET_BITS
#define _FILE_OFFSET_BITS 64
#endif

#include <stdio.h>
#include <stdlib.h>
#if !defined(_WIN32)
#include <unistd.h>
#endif
#include <math.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <fcntl.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <rpc/types.h>
#include <netinet/in.h>
/*
 * The <SeisData.h> file has some COLORREF variables defined for setting
 * the colors to support first break picking.  A COLORREF is simply
 * a 32 bit unsigned integer.  Remove the following statement if you
 * are running on an NT machine.  If you are running on a UNIX machine
 * leave it as is.
 */
#if !defined(_WIN32)
typedef unsigned long int COLORREF;  /* COLORREF is defined by windows */
typedef unsigned long int DWORD;     /* DWORD is defined by windows    */
typedef long int BOOL;               /* BOOL is defined by windows     */
#endif

#include "SeisData.h"

static int big_endian;
/*
 * Define a number to identify a data object.  This only needed to make sure
 * that the file we are reading is really a valid data object file.
 */
static DWORD SEISDATAOBJECTID = 0x78F7B6CA;

/* self-documentation */
static void syntax(const char *program_name) {
   fprintf(stderr,"Syntax: %s [-o out_segd_filename] infile1 [ infile2 ... ]\n",
           program_name); 
   fprintf(stderr,"\n        Default output is stdout if -o not supplied\n");
   fprintf(stderr,"        infile name(s) that don't match \"FFID_*%%*.SEIS\" are ignored\n");
   fprintf(stderr,"\n        Copyright 2002, Landmark Graphics Corp., All Rights Reserved.\n");
   exit(EXIT_FAILURE);
}

/* sanity check input SeisNet filename */
static BOOL check_filename(const char* inname) {

  const char *ptr = inname;
  const char ffid[] = "FFID_";
  const char seis[] = ".SEIS";
  int i,n;

  /* make sure filename begins with FFID_ */
  n = strlen(ffid);
  for(i=0; i<n; ++i)
     if(toupper(ptr[i]) != ffid[i]) return ((BOOL) (!1)); 

  /* make sure that there is a % sign after initial FFID_ */
  ptr += n;

  while((*ptr) != '\000') {
    if((*ptr) == '\045') break;
    ++ptr;
  }
  
  if((*ptr) == '\000') return ((BOOL) (!1));

  /* make sure filename ends in ".SEIS" */
  n = strlen(seis);
  ptr += (strlen(ptr) - n);
  
  if(ptr < inname) return ((BOOL) (!1));

  for(i=0; i<n; ++i)
     if(toupper(ptr[i]) != seis[i]) return ((BOOL) (!1)); 

  /* we passed our sanity checks */
  return ((BOOL) (!(!1)));
}

/*
 * byte swapping routines from ProMAX maxutil/misc 
 */
static void u_swapends4(void *array, int nvals)       
{                                              
 unsigned int *ivals = (unsigned int *) array; 
 int i;                                        
 for(i=0; i<nvals; ++i)                        
 {                                             
#if defined(__i386)
   ivals[0] = bswap_32(ivals[0]);              
#else                                          
   unsigned int tmp1, tmp2;                    
   tmp1 = (ivals[0]<<16)+(ivals[0]>>16);       
   tmp2 = tmp1&0xff00ff00;                     
   ivals[0] = ((tmp1-tmp2)<<8) + (tmp2>>8);    
#endif                                         
   ++ivals;                                    
 }                                             
}                                              
static void u_swapends2(void *array, int nvals)
{
#if defined(__i386)
 int i;
 unsigned short *ivals = (unsigned short *) array;
 for(i=0; i<nvals; ++i)
 {
   ivals[0] = bswap_16(ivals[0]);
   ++ivals;
 }
#else
 swab(array, array, nvals*2);
#endif
}
static void writeTODcount(FILE*out_segd, const char *segd_outname, size_t nbytes) {
    ssize_t nwrite;
    size_t nb;
    nb = htonl(nbytes);

    errno = 0;
    nwrite = fwrite((void *) &nb, sizeof(nb), 1, out_segd);
    if(nwrite < 1) {
      perror(segd_outname);
      fprintf(stderr,"%s write of SEGD TOD byte count\n",
	 (nwrite < 0) ? "Bad" : "Short");
      fprintf(stderr,"Exiting.\n"); exit(EXIT_FAILURE);
    }
}

static void writeTODdata(FILE *out_segd, const char *segd_outname, void *buf, size_t nbytes) {
    ssize_t nwrite;

    errno = 0;
    nwrite = fwrite((void *) buf, sizeof(char), nbytes, out_segd);
    if(nwrite < nbytes) {
      perror(segd_outname);
      fprintf(stderr,"%s write of SEGD record\n",
	 (nwrite < 0) ? "Bad" : "Short");
      fprintf(stderr,"Exiting.\n"); exit(EXIT_FAILURE);
    }

}

static void writeTODrec(FILE *out_segd, const char *segd_outname, void *buf, size_t nbytes) {
    ssize_t nwrite;

    writeTODcount(out_segd, segd_outname, nbytes);
    writeTODdata(out_segd, segd_outname, buf, nbytes);
    writeTODcount(out_segd, segd_outname, nbytes);
}
/*
 * Start of the main program.
 */
int main (int argc, char **argv)
{
    /* Declare some local variables */
    ssize_t nread, nwrite;
    int ntraces;
    size_t nb;
    int i,j;
    off_t nbytes;
    SEISFILEHEADER sh;      /* See the <SeisData.h> file */
    SEISDATAPARMS sd;	    /* See the <SeisData.h> file */
    SEISTRACELOCATION *pl;  /* See the <SeisData.h> file */
    TRACEDATA *td;	    /* See the <SeisData.h> file */
    SEISTRACEHEADER *thdr;  /* See the <SeisData.h> file */
    unsigned char *segd;    /* SEG-D file headers */
    float *trace;           /* little endian IEEE float */
    unsigned char *seghead; /* SEG-D trace header */
    FILE *in_seisnet;       /* input SeisNet file handle */
    FILE *out_segd;         /* output SEG-D file handle */
    const char *segd_outname;     /* output SEG-D filename or "stdout" */
    char **inname_ptr;      /* where SeisNet filenames start in cmdline */
    int ifile, nfiles;

    /* check local endianness to see if we need to byte swap from
     * the PC little-endian format of SeisData file.
     */
    big_endian = (1 == htonl(1));
    if(big_endian) u_swapends4((void *) &SEISDATAOBJECTID, 1);

    /* process command line flag(s) */
    inname_ptr = argv+1;
    if(argc < 2) syntax(argv[0]); /* need at least one input filename */

    if(0 == strncmp(argv[1],"-o",strlen("-o"))) { /* explicit output SEG-D filename */
       char *ptr = argv[1]+strlen("-o");
       inname_ptr++;
       if(*ptr == '\0' && argc > 2) {
	    ptr = argv[2]; /* next arg filename */
	    inname_ptr++;
       }
       if(*ptr == '\0') syntax(argv[0]); /* couldn't get a filename */
       segd_outname = ptr;
       out_segd =  fopen(ptr,"wb");
       if(out_segd == ((FILE *) NULL)) {
	 perror(ptr);
	 fprintf(stderr,"Failed to open output file \"%s\"\n", ptr);
	 exit(EXIT_FAILURE);
       }
    } else {
       out_segd = stdout;
       segd_outname = "stdout";
    }
    
    /* remaining command line arguments are input filenames */

    nfiles = argc - (inname_ptr - argv);

    for(ifile = 0; ifile < nfiles; ++ifile) {

	/*
	 * Cycle through all the data file. Note the format of the filename. 
	 *
	 * FFID_ ------> Identifies the file as a shot record
	 * 101 --------> The FFID of the shot record
	 * %843066991 -> Number of seconds since Jan 1, 1970 (a time stamp)
	 *
	 * .SEIS ------> Filename extension for SeisNet Seismic Data files
	 *
	 * Encoding this information in the filename makes sorting much easier
	 * for real time applications
	 */

        if(!check_filename(inname_ptr[ifile])) {
           fprintf(stderr,"*** skipping input file \"%s\"\n",inname_ptr[ifile]);
           continue;
        }

        /* open next input file */

	in_seisnet = fopen(inname_ptr[ifile], "rb");
        if(in_seisnet == ((FILE *) NULL)) {
           perror(inname_ptr[ifile]);
           fprintf(stderr,"Unable to open input filename \"%s\"\n", 
                  inname_ptr[ifile]);
           fprintf(stderr,"Exiting %s\n", argv[0]);
           exit(EXIT_FAILURE);
        }

	/*
	 * Determine the size of the data object.
	 */

	if( 0 != fseeko(in_seisnet, (off_t) 0, SEEK_END)) {
           perror(inname_ptr[ifile]);
           fprintf(stderr,"Unable to seek to end of input file.\n");
           fprintf(stderr,"Will try to continue.\n");
        }
	nbytes = ftello(in_seisnet);
        if(nbytes < 0) {
           perror(inname_ptr[ifile]);
           fprintf(stderr,"Unable to get input filesize.\n");
           fprintf(stderr,"Will try to continue.\n");
        }
	if( 0 != fseeko(in_seisnet, (off_t) 0, SEEK_SET)) {
           perror(inname_ptr[ifile]);
           fprintf(stderr,"Unable to rewind input file.\n");
           fprintf(stderr,"Will try to continue.\n");
        }

           
	/*
	 * Read the SEISFILEHEADER structure.
         */
        errno = 0;
	nread = fread((void *)&sh, sizeof(char), sizeof(SEISFILEHEADER), in_seisnet);
        if(nread < sizeof(SEISFILEHEADER)) {
          perror(inname_ptr[ifile]);
          fprintf(stderr,"%s read of SEISFILEHEADER from \"%s\"\n",
                 (nread < 0) ? "Bad" : "Short", inname_ptr[ifile]);
          fprintf(stderr,"Exiting.\n");
          exit(EXIT_FAILURE);
        }

	/*
	 * Make sure that we have opened a valid data object.
         */
	if(sh.ID != SEISDATAOBJECTID) {
	    fprintf(stderr,
                "File \"%s\" is apparently not a valid SeisNet object.\n",
		inname_ptr[ifile]);
	    fprintf(stderr,"Skipping.\n");
            (void) fclose(in_seisnet);
	    continue;
	}

	/*
	 * Read the SEISDATAPARMS structure
         */
        errno = 0;
	nread = fread((void *)&sd, sizeof(char), sizeof(SEISDATAPARMS), in_seisnet);
        if(nread < sizeof(SEISDATAPARMS)) {
          perror(inname_ptr[ifile]);
          fprintf(stderr,"%s read of SEISDATAPARMS from \"%s\"\n",
                 (nread < 0) ? "Bad" : "Short", inname_ptr[ifile]);
          fprintf(stderr,"Exiting.\n");
          exit(EXIT_FAILURE);
        }

	/*
	 * Read the orginal SEG-D record header
         */
        if(big_endian) u_swapends4((void *) &sh.nseghead, 1);
	if(sh.nseghead > 4) {
            errno = 0;
	    seghead = (unsigned char *)malloc(sh.nseghead);
            if(seghead == ((unsigned char *) NULL)) {
              perror("malloc");
              fprintf(stderr,"Unable to malloc %ul bytes. Exiting.\n",
                   (unsigned long) sh.nseghead);
              exit(EXIT_FAILURE);
            }
            errno = 0;
	    nread = fread((void *)seghead, sizeof(char), (size_t) sh.nseghead, in_seisnet);
            if(nread < (size_t) sh.nseghead) {
              perror(inname_ptr[ifile]);
              fprintf(stderr,"%s read of SEGD file header from \"%s\"\n",
                 (nread < 0) ? "Bad" : "Short", inname_ptr[ifile]);
              fprintf(stderr,"Exiting.\n");
              exit(EXIT_FAILURE);
            }
        } else {
            fprintf(stderr,"SeisNet file \"%s\" doesn't contain a SEG-D file header ... skipping\n", inname_ptr[ifile]);
            (void) fclose(in_seisnet);
            continue;
        }

        /* change data format to IEEE float (8058) in SEG-D file header */
        seghead[2] = 8*16; seghead[3] = 5*16 + 8;

	/*
	 * Read in the location of the trace data
         */
        if(big_endian) u_swapends4((void *) &sh.ntraces, 1);
	ntraces = sh.ntraces;
	fprintf(stderr,"Total number of traces (data + aux) in dataset is: %d\n", ntraces);
	nb = ntraces * sizeof(SEISTRACELOCATION);
        errno = 0;
	pl = (SEISTRACELOCATION *)malloc(nb);
        if(pl == ((SEISTRACELOCATION *) NULL)) {
           perror("malloc");
	  fprintf(stderr,"Unable to malloc %ul bytes. Exiting.\n",
	       (unsigned long) nb);
          exit(EXIT_FAILURE);
        }

        errno = 0;
	nread = fread((void *)pl, sizeof(char), (size_t) nb, in_seisnet);
	if(nread < nb) {
	  perror(inname_ptr[ifile]);
	  fprintf(stderr,"%s read of SEISTRACELOCATION from \"%s\"\n",
	     (nread < 0) ? "Bad" : "Short", inname_ptr[ifile]);
	  fprintf(stderr,"Exiting.\n");
	  exit(EXIT_FAILURE);
	}

	/*
	 * Read the trace location information.  Also, assign pointers for the SeisNet
	 * trace headers, the orginal SEG-D trace headers, and the seismic traces.
         */

        errno = 0;
	td = (TRACEDATA *)malloc(ntraces * sizeof(TRACEDATA));
        if(td == ((TRACEDATA *) NULL)) {
           perror("malloc");
	  fprintf(stderr,"Unable to malloc %ul bytes. Exiting.\n",
	       (unsigned long) (ntraces*sizeof(TRACEDATA)));
          exit(EXIT_FAILURE);
        }

	if(big_endian) u_swapends4((void *) &sd.m_nsegtrchead, 1);

	/* Read the trace headers and seismic data */
	for(i=0; i<ntraces; i++) {
	    /* Seek to correct location in the file. */
            if(big_endian) u_swapends4((void *) &((pl+i)->trcoffset), 1);
	    if(0 != fseeko(in_seisnet, (off_t) ((pl+i)->trcoffset), SEEK_SET)) {
	      perror(inname_ptr[ifile]);
	      perror("Exiting on seek failure.\n");
	      exit(EXIT_FAILURE);
	    }

	    /* Allocate memory to hold the SeisNet trace header. */
            errno = 0;
	    (td+i)->thdr = (SEISTRACEHEADER *)malloc(sizeof(SEISTRACEHEADER));
	    if( ((td+i)->thdr)  == ((SEISTRACEHEADER *) NULL)) {
	      perror("malloc");
	      fprintf(stderr,"Unable to malloc %ul bytes. Exiting.\n",
		   (unsigned long) (sizeof(SEISTRACEHEADER)));
              exit(EXIT_FAILURE);
	    }

	    /* Assign a local pointer to the trace header for less typing below. */
	    thdr = (td+i)->thdr;

	    /* Read the SeisNet trace header. */
            errno = 0;
	    nread = fread((void *)thdr, sizeof(char), sizeof(SEISTRACEHEADER), in_seisnet);
	    if(nread < sizeof(SEISTRACEHEADER)) {
	      perror(inname_ptr[ifile]);
	      fprintf(stderr,"%s read of SEISTRACEHEADER from \"%s\"\n",
		 (nread < 0) ? "Bad" : "Short", inname_ptr[ifile]);
	      fprintf(stderr,"Exiting.\n"); exit(EXIT_FAILURE);
	    }

	    /* Allocate memory to hold the orginal SEG-D trace header. */
	    (td+i)->segd = (unsigned char *)malloc(sd.m_nsegtrchead);
	    if( ((td+i)->segd)  == ((unsigned char *) NULL)) {
	      perror("malloc");
	      fprintf(stderr,"Unable to malloc %ul bytes. Exiting.\n",
		   (unsigned long) (sd.m_nsegtrchead));
              exit(EXIT_FAILURE);
	    }

	    /* Read the orginal SEG-D trace header. */
            errno = 0;
	    nread = fread((void *)(td+i)->segd, sizeof(char), (size_t) sd.m_nsegtrchead, in_seisnet);
	    if(nread < (size_t) sd.m_nsegtrchead) {
	      perror(inname_ptr[ifile]);
	      fprintf(stderr,"%s read of SEGD trace header from \"%s\"\n",
		 (nread < 0) ? "Bad" : "Short", inname_ptr[ifile]);
	      fprintf(stderr,"Exiting.\n"); exit(EXIT_FAILURE);
	    }

	    /* Allocate memory to hold the seismic trace. */
            if(big_endian) u_swapends4((void *) &(thdr->m_nsamp), 1);
	    (td+i)->trace = (float *)malloc(sizeof(float) * thdr->m_nsamp);
	    if( ((td+i)->trace)  == ((float *) NULL)) {
	      perror("malloc");
	      fprintf(stderr,"Unable to malloc %ul bytes. Exiting.\n",
		   (unsigned long) (thdr->m_nsamp*sizeof(float)));
              exit(EXIT_FAILURE);
	    }

	    /* Read the seismic trace. */
            errno = 0;
	    nread = fread((void *)(td+i)->trace, sizeof(char) , sizeof(float) * thdr->m_nsamp, in_seisnet);
	    if(nread < (size_t) (sizeof(float) * thdr->m_nsamp)) {
	      perror(inname_ptr[ifile]);
	      fprintf(stderr,"%s read of SeisNet trace data from \"%s\"\n",
		 (nread < 0) ? "Bad" : "Short", inname_ptr[ifile]);
	      fprintf(stderr,"Exiting.\n"); exit(EXIT_FAILURE);
	    }

            /* make IEEE floats big-endian for output */
            u_swapends4((void *) (td+i)->trace, thdr->m_nsamp);
	}
	
	/* Close the file so that another process can get to it. */
	(void) fclose(in_seisnet);

	/* Write out SEG-D file header in ProMAX TOD format */
        writeTODrec(out_segd, segd_outname, seghead, (size_t) sh.nseghead);

        /* write out SEG-D traces in ProMAX TOD format */
	for(i=0; i<ntraces; i++) {
	    /* Assign a local pointer to the SeisNet trace header. */
	    thdr = (td+i)->thdr;

	    /* Assign a local pointer to the orginal SEG-D trace header. */
	    segd = (td+i)->segd;
	    /* Assign a local pointer to point to the trace data.  Remember that
	     * the C language is zero-based.  That is, the first trace has an index
	     * of zero.
	     */
	    trace = (td+i)->trace;

            /* write TOD prefix */
            writeTODcount(out_segd, segd_outname,
		sd.m_nsegtrchead + thdr->m_nsamp*sizeof(float));
	    writeTODdata(out_segd, segd_outname, segd, (size_t) sd.m_nsegtrchead);
	    writeTODdata(out_segd, segd_outname, trace, (size_t) thdr->m_nsamp*sizeof(float));
            writeTODcount(out_segd, segd_outname,
		sd.m_nsegtrchead + thdr->m_nsamp*sizeof(float));
            
        }

        /* write TOD file mark */
        writeTODcount(out_segd, segd_outname, (size_t) 0);
	
	/*  Free all the memory that was allocated above */
	for(i=0; i<ntraces; i++)
		{
		free((void *) ((td+i)->thdr));
		free((void *) ((td+i)->segd));
		free((void *) ((td+i)->trace));
		}
	free((void *) seghead);
	free((void *) pl);
	free((void *) td);
    } /* end of input file loop */

    /* Exit the program. */
    return EXIT_SUCCESS;
}
