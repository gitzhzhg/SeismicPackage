/* readp190 p190= [ ellipsoididx=23 semimajoraxis= flattening= lon0= ] */
/* Decodes marine P1/90 shot-receiver file and populates database */
/* with projected coordinates for source and receiver. */
/* creates/extends sup190.gdbm in current directory */
/* so that multiple P1/90 files can be merged */

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include <gdbm.h>
#include "par.h"

typedef
struct {
  double longitude;
  double latitude;
} coords;

/*********************** self documentation ******************************/
char *sdoc[] = {
"                                                                       ",
" READP190 - Read a P1/90 geometry file, converting to gdbm database    ",
"                                                                       ",
" readp190 p190=                                                        ",
"                                                                       ",
" Required parameters:                                                  ",
"    p190            Input P1/90 file to decode, convert and store      ",
"                                                                       ",
" Optional parameters:                                                  ",
"    verbose=0       =1  chatty progress                                ",
"                                                                       ",
NULL};

int main(int argc, char **argv)
{
  size_t nentries = 0;
  GDBM_FILE coordDB;
  datum xyKey, xyVal;
#define SHOTLONG 0
#define SHOTLAT 1
#define RECLONG 2
#define RECLAT 3
#define SWDEP 4
#define GELEV 5
  double datumvals[6]; /* shot long, lat; receiver long, lat; swdep, gelev */
  FILE *fp;
  char ctmp[BUFSIZ];
  char cbuf[BUFSIZ];
  char *p190file;
  int hcode;
  int shotpt;
  int recgrp;
  int datumpair[2]; /* shotpt, group no. */ /* may need streamer ID later */
  int verbose = 0;

  xyKey.dptr = (char *) (&(datumpair[0]));
  xyKey.dsize = (int) sizeof(datumpair);
  xyVal.dptr = (char *) (&(datumvals[0]));
  xyVal.dsize = (int) sizeof(datumvals);

  /* Initialize */
  initargs(argc, argv);
  requestdoc(1);

  if(!getparstring("p190",&p190file)) {
    fprintf(stderr,"Missing p190= command line parameter\n");
    return(EXIT_FAILURE);
  }
  (void) getparint("verbose",&verbose);

  fp = fopen(p190file,"r");
  if(fp == ((FILE *) NULL)) {
     perror(argv[0]);
     fprintf(stderr,"Unable to open P1/90 file \"%s\" for input.  Exiting.\n",
             p190file);
     return (EXIT_FAILURE);
  }
  coordDB = gdbm_open("./sup190.gdbm",1024,GDBM_WRCREAT, 0664, NULL);
  if(coordDB == NULL) {
     fprintf(stderr,"%s: %s\n",argv[0],gdbm_strerror(gdbm_errno));
     return(EXIT_FAILURE);
  }

  nentries = 0;
  while(((char *) NULL) != fgets(cbuf, sizeof(cbuf), fp)) {
     switch(cbuf[0]) {
     case 'H':
        ctmp[4] = '\0';
        strncpy(ctmp,cbuf+1,4); /* get H code */
        hcode = atoi(ctmp);
        switch(hcode) {
        case 1800:
           strncpy(ctmp,cbuf+32,4);
           if(verbose)fputs(cbuf,stderr);
           break;
        case 1900:
           if(verbose)fputs(cbuf,stderr);
           break;
        default:
           break;
        }
        break;
     case 'S':
        ctmp[6]='\0';
        strncpy(ctmp,cbuf+19,6);
        shotpt = atoi(ctmp);
	datumpair[0] = shotpt;
	ctmp[9] = '\0';
        strncpy(ctmp,cbuf+46,9);
        datumvals[SHOTLONG] = atof(ctmp);
        ctmp[9] = '\0';
        strncpy(ctmp,cbuf+55,9);
        datumvals[SHOTLAT] = atof(ctmp);
        ctmp[6] = '\0';
        strncpy(ctmp,cbuf+64,6);
        datumvals[SWDEP] = atof(ctmp);
        break;
     case 'R':
	ctmp[4] = '\0';
	strncpy(ctmp,cbuf+1,4);
	recgrp = atoi(ctmp);
	datumpair[1] = recgrp;
        strncpy(ctmp,cbuf+23,4);
        datumvals[GELEV] = -atof(ctmp);
	ctmp[9] = '\0';
	strncpy(ctmp,cbuf+5,9);
	datumvals[RECLONG] = atof(ctmp);
	strncpy(ctmp,cbuf+14,9);
	datumvals[RECLAT] =  atof(ctmp);
	/* write record to dbm file */
	(void) gdbm_store(coordDB,xyKey,xyVal,GDBM_REPLACE);
        ++nentries;
	ctmp[4] = '\0';
	strncpy(ctmp,cbuf+27,4);
	if(strncmp(ctmp,"    ",4) == 0) break;
	recgrp = atoi(ctmp);
	datumpair[1] = recgrp;
        strncpy(ctmp,cbuf+49,4);
        datumvals[GELEV] = -atof(ctmp);
	ctmp[9] = '\0';
	strncpy(ctmp,cbuf+31,9);
	datumvals[RECLONG] = atof(ctmp);
	strncpy(ctmp,cbuf+40,9);
	datumvals[RECLAT] = atof(ctmp);
	/* write record to dbm file */
	(void) gdbm_store(coordDB,xyKey,xyVal,GDBM_INSERT);
        ++nentries;
	ctmp[4] = '\0';
	strncpy(ctmp,cbuf+53,4);
	if(strncmp(ctmp,"    ",4) == 0) break;
	recgrp = atoi(ctmp);
	datumpair[1] = recgrp;
        strncpy(ctmp,cbuf+75,4);
        datumvals[GELEV] = -atof(ctmp);
	ctmp[9] = '\0';
	strncpy(ctmp,cbuf+57,9);
	datumvals[RECLONG] = atof(ctmp);
	strncpy(ctmp,cbuf+66,9);
	datumvals[RECLAT] = atof(ctmp);
	/* write record to dbm file */
	(void) gdbm_store(coordDB,xyKey,xyVal,GDBM_INSERT);
        ++nentries;
        break;
     default:
        break;
     }
     if(verbose && (nentries > 0) && (nentries % 10000 == 0)) fprintf(stderr,
       "%lu entries loaded into sup190.gdbm database.\n", nentries);
  }

  gdbm_close(coordDB);
  if(verbose) fprintf(stderr,"Finished loading %lu entries into sup190.gdbm database.\n", (unsigned long) nentries);

   return (gdbm_errno == GDBM_NO_ERROR ? EXIT_SUCCESS : EXIT_FAILURE);
}
