/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
/*
C      tracefile.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C             written in c -- designed to be called from c
C
C     Utility Name:  tracefile    (tracefile reading routines)
C          Written:  93/08/01  by:  Scott Michell
C     Last revised:  00/12/04  by:  Scott Michell
C
C  Purpose:       These are simple routines for reading trace files.
C                 Currently, only trace headers are read.
C
C  Related Documentation:  Richard Day's trace file I/O routines are
C                          more general, and can deal with various
C                          types of files.
C-----------------------------------------------------------------------
C                            THIS UTILITY     
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/cprim   (shared)
C  library:                cprim.a            (shared)
C  header file:            cprim.h            (shared)
C  source file:            tracefile.c
C
C  static functions:       err
C  documented functions:   tracefile_open     tracefile_read_header
C                          tracefile_rewind   tracefile_close

C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES 
C         (this utility does not reference X, Xt, and Motif)
C              (standard C references not listed here)
C
C  libraries:     cprim.a
C  header files:  cprim.h
C  functions:     glotch   botch   slosh
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  5. 00/12/04  Sherrill   Renamed everything in this file that used to
C                          be called "byte" to "trace" and added the
C                          ability to use other file types besides byte types
C  4. 95/02/14  Sherrill   Check for not null pointer before frees in
C                          tracefile_close for portability.
C  3. 93/08/20  Michell	   Allow user to pass a file name to 
C			    tracefile_open with or without extension.
C			    Add a scratch string to structure so 
C			    header string is not altered.  Add trval
C			    to structure for later addition of reading
C			    trace values.  
C                           Add function tracefile_read_hdr_tr_spws.
C  2. 93/08/02  Stoeckley  Combine into one file, put header info into
C                           cprim.h, rename routines, change to return
C                           pointer to structure, and several other
C                           modifications.
C  1. 93/08/01  Michell    Initial version (made from PC C++ code).
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
C
C  For pointers, the flag (i,o,b) refers to the contents pointed to
C  by the pointer, not to the value of the pointer itself.  The pointer
C  value is required upon INPUT in all cases.
C-----------------------------------------------------------------------
C  To open a trace file and return some global information:
C                                   i         o         o
C          glbl = tracefile_open (filename, *nwords, *ntraces)
C
C  char       *filename = name of tracefile (with or without .byt or .glbl).
C  long         *nwords = number of words in each trace header.
C  long        *ntraces = number of traces in the tracefile.
C  TracefileStruct *glbl = pointer to opaque structure allocated by
C                          this routine.
C
C  The structure is allocated if the tracefile is successfully opened.
C  Returns NULL if the open is not successful.
C-----------------------------------------------------------------------
C  To read the next trace header in the tracefile:
C                                          i     o
C           nhead = trace_file_read_header_spws(glbl, fhead)
C	    nhead = tracefile_read_hdr_tr_spws(glbl, fhead)
C
C	trace_file_read_header_spws reads only the header while
C	tracefile_read_hdr_tr_spws reads the header and the trace
C		values.  They are almost identical code !
C
C  TracefileStruct *glbl = pointer to opaque structure.
C  float        fhead[] = trace header array.
C  long           nhead = number of words put into fhead[].
C
C  The trace header array must be allocated before calling this routine.
C  Returns nhead = -1 if the number of header words read is not the
C    same as the number returned by tracefile_open.
C  Returns nhead = -2 if the first header word is not equal to the
C    current trace number.
C-----------------------------------------------------------------------
C  To rewind the tracefile:
C                                    i
C           error = tracefile_rewind(glbl)
C
C  TracefileStruct *glbl = pointer to opaque structure.
C  long           error = zero if no error and non-zero if error.
C-----------------------------------------------------------------------
C  To close the tracefile:
C                                   i
C           error = tracefile_close(glbl)
C
C  TracefileStruct *glbl = pointer to opaque structure.
C  long           error = zero if no error and non-zero if error.
C
C  The structure is de-allocated if the tracefile is successfully closed.
C-----------------------------------------------------------------------
C                             EXAMPLE CODE
C The following code fragment will open a trace file, read one set of
C headers and a set of trace values.  It then prints out some header
C values and some diagnostics.

#include <stdio.h>
#include <string.h>
#include "/u/pospsva/spws/include/cprim.h"

main()
{
	TracefileStruct *glbl;
	long nwords,ntr,nhead;
	float fhead[65];

	glbl=tracefile_open("p430svat.glbl",&nwords,&ntr);
	nhead=tracefile_read_hdr_tr_spws(glbl,fhead);
	tracefile_close(glbl);
	printf("nhead = %ld\tnwords= %ld\tntr= %ld\n",nhead,nwords,ntr);
	printf("headers 1 7 37 %f  %f  %f\n",fhead[0],fhead[6],fhead[36]);
}
	
	
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "tfdefs.h"
#include "tfio.h"
#include "cprim.h"



#ifdef MSDOS
#define GLBL ".glb"
#define BYT  ".byt"
#else
#define GLBL ".glbl"
#define BYT  ".byt"
#endif

/*
#define DEBUG
*/

#define MAX_ARRAY_SIZE 1000


/*---------------------- opaque structure ------------------------------*/

struct _TracefileStruct
{
	    int        ntrc, /* number of traces */
                       last_header_returned, /*last header request filled*/
   	               nhead,/*number of headers in trace */
                       read_counter,
                       max_traces_to_read;
            float      *trace, /*array to hold trace data*/
                       *headers, /*array to hold headers*/
                       *scalers; /*array to hold trace scalers */       
	    char       name[512];  /* base file name (w/o extension) */
            TF_Global  g; /*For calls to Richard's io*/
            IO_request Cl; /*For calls to Richard's io*/
} ;



/*------------------------ static functions ----------------------------*/

static void err(char *mssg)                  /* error function */
{
    fprintf(stderr,"%s\n",mssg);
}



/*-------------------------- tracefile_open -----------------------------*/

TracefileStruct *tracefile_open(char *name, long *nhead, long *ntrc)
{
char msg[80];
char *tmpname;
TracefileStruct *glbl;
int chk,len,i,j,count;

    glbl = (TracefileStruct*)malloc(sizeof(TracefileStruct));
    if(glbl == NULL) 
      {
      err("TracefileStruct malloc in tracefile_open failed");
      return NULL;
      }

    *nhead = glbl->nhead = -1;      /* number of heads = -1 if fails */
    strcpy(glbl->name,name); 

    chk = get_global_data_(name, &glbl->g, &chk);

    /* lets get the globals from the glbl file */
    if( chk != 0)
      {
      free(glbl);
      err("Failed to read file in tracefile_open");
      return NULL;
      }

    if(glbl->g.nbydp < 4 && strstr(glbl->g.ftyp,"BYTE")!=0) 
       glbl->nhead = 64;/*Old byte file*/
    else
       glbl->nhead  = glbl->g.nhdwd;


    glbl->max_traces_to_read = glbl->g.ntrfil < MAX_ARRAY_SIZE ? glbl->g.ntrfil
                                                               : MAX_ARRAY_SIZE;


    glbl->trace = (float *)malloc(glbl->max_traces_to_read * sizeof(float));
    if(glbl->trace == NULL)
      {
      free(glbl);
      err("Not enough memory in tracefile_open");
      return NULL;
      }

    glbl->scalers = (float *)malloc((glbl->max_traces_to_read + 1)
                    * sizeof(float));
    if(glbl->scalers == NULL)
      {
      free(glbl->trace);
      free(glbl);
      err("Not enough memory in tracefile_open");
      return NULL;
      }

    glbl->headers = (float *)malloc((glbl->max_traces_to_read * glbl->g.nhdwd)
                                    * sizeof(double));
    if(glbl->headers == NULL)
      {
      free(glbl->trace);
      free(glbl->scalers);
      free(glbl);
      err("Not enough memory in tracefile_open");
      return NULL;
      }

    glbl->ntrc   = glbl->g.ntrfil;
    *nhead       = glbl->nhead;
    *ntrc        = glbl->ntrc;

    if(glbl->g.nbydp < 4 && strstr(glbl->g.ftyp,"BYTE")!=0)
      {
      tf_glbl_getn_( &chk,  glbl->name, &glbl->g);
      tf_close_(&glbl->g.lun,&chk,msg);
      }

    tracefile_rewind(glbl);

    return(glbl);
} /* end tracefile_open - get header info from global file */








/*---------------------- tracefile_read_header --------------------------*/
long tracefile_read_header(TracefileStruct *glbl, float fhead[], 
                           int open_file)
{
float trace[2];
float scalers[2];
int chk=0, nhdrs;
int normalize = 0;
long header_index, i;
static int lun;

   if(glbl->Cl.ntot > 1 && 
      glbl->last_header_returned >= glbl->Cl.iskp ||
      glbl->last_header_returned == 0                               )
     {
     if(glbl->g.nbydp < 4 && strstr(glbl->g.ftyp,"BYTE")!=0) 
       read_byt_(glbl->name, &chk, &glbl->Cl, &normalize,
                  &nhdrs, (char *) &glbl->headers[0], 
                  (unsigned char *) &glbl->trace[0], 
                  &glbl->scalers[0]); 
     else
       chk = read_data_(glbl->name, &chk, &glbl->Cl, 
                       (char *) &glbl->headers[0], 
                       (char *) &glbl->trace[0], &glbl->scalers[0],
                       &lun, open_file);

     if(chk != 0)
     return(-1);

     glbl->read_counter = 0;

     /*Set up for next read*/
     glbl->Cl.iskp += glbl->max_traces_to_read;
     if(glbl->Cl.ntot + glbl->Cl.iskp > glbl->g.ntrfil)
        glbl->Cl.ntot = glbl->g.ntrfil - glbl->Cl.iskp;
     }

   

   header_index = glbl->read_counter * glbl->nhead;
   for(i = 0; i < glbl->nhead; i++)
     fhead[i] = glbl->headers[header_index + i];  

   

   ++glbl->last_header_returned;
   ++glbl->read_counter;

   return(glbl->nhead);
}



/*-------------------------- tracefile_rewind ---------------------------*/

long tracefile_rewind(TracefileStruct *glbl)
{

    /*Set up the control structure for subsequent reads*/
    glbl->Cl.iskp   = 0;  /*Initial No. of traces to skip.          */
    glbl->Cl.ndo    = 1;   /*No. of traces to read per group.        */
    glbl->Cl.nskp   = 0;  /*No. of traces to skip between groups.*/
    glbl->Cl.ntot   = glbl->max_traces_to_read;
    glbl->Cl.nsamp  = 1; /*No. of samples to return                */
    glbl->Cl.samp1  = 0; /*1st sample to return                    */
    glbl->Cl.sdec   = 1;  /*Sample decimation factor                */
    glbl->Cl.trnsps = 0;/*Transpose flag. 0=no transpose          */
    glbl->Cl.cnvrt  = 0; /*Conversion flag. 0-no conversion, 1-ch to real */
    glbl->Cl.axis   = 0;  /*If !=0 Indicates a cube axis. 1,2,3    */
    glbl->Cl.index  = 0; /* slice index(from 0) when axis is set.  */

    glbl->last_header_returned = 0;
    glbl->read_counter         = 0;

    return (0);
}




/*-------------------------- tracefile_close ----------------------------*/

long tracefile_close(TracefileStruct *glbl)
{
int chk;
char msg[80];

  if(glbl)
    {
    if(glbl->g.nbydp < 4 && strstr(glbl->g.ftyp,"BYTE")!=0)
       tf_glbl_getn_( &chk,  glbl->name, &glbl->g); 
    tf_close_(&glbl->g.lun,&chk,msg);
    free(glbl->trace);
    free(glbl->scalers);
    free(glbl->headers);
    free(glbl);
    }

  return 0;
	
}
