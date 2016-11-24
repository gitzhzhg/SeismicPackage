/* Copyright (c) Colorado School of Mines, 1999. */
/* All rights reserved.                       */


/*--------------------------------------------------------------------*\
   SUWRITE: write SU output stream

   code by J.E. Anderson, Mobil/CWP (June, 1993) modified by J.E.
   Anderson, April, 1995, updated trace header words and internal
   documentation

\*--------------------------------------------------------------------*/

#include     "cwpsegy.h"
#include     "cpromax.h"
#include     <pwd.h>
#include     <stdio.h>
#include     <string.h>

#if defined( __sparc) || defined( __linux__ )

#else
   extern FILE    *popen(char *cmnd, char *type);
#endif

void init_suwrite_(
    int *lensav
   ,int *itooltype
);

void exec_suwrite_(
    float* trace
   ,int*   ihead
   ,float* rhead
);


static char* suwrite_stringPrep(char *s); 

static void suwrite_putsu(
    FILE*    fp
   ,cwpsegy* tp
   ,long*    itr
   ,int*     nsegy
);

static FILE* suwrite_open(
    int   ichoose
   ,char* cmnd
   ,char* datafile
);

static void suwrite_hmap_2su(
    cwpsegy* tr
   ,int*     h
);

static void suwrite_hdrinit(
    int* fmt
   ,int* ind
);

static void suwrite_geth(
    int*   h
   ,int*   fmt
   ,int*   ind
   ,int*   ihead
   ,float* rhead
);

static cwpbhed bin_hdr;
static char txt_hdr[3200];

BEGINPARMS
   FILE*    fp;
   int      ichoose;
   char*    cmnd;
   char*    datafile;
   int      input_numsmp;
   int      input_samprat;
   long     first;
   cwpsegy* tr;
   long     itr;
   int      nsegy;
   int*     fmt;
   int*     ind;
   int*     h;
ENDPARMS(parms)

/*--------------------------------------------------------------------*\
                   Initialization phase for SUWRITE 
\*--------------------------------------------------------------------*/

void init_suwrite_(
    int* lensav 
   ,int* itooltype
   ){

   char temp[PATH_MAX];

   exParGetInt("CHOOSE1", &parms->ichoose);
   exParGetString("CMND", &parms->cmnd);
   exParGetString("DATAFILE", &parms->datafile );
   suwrite_stringPrep(temp);

   parms->input_numsmp = globalRuntime->numsmp;
   parms->input_samprat = globalRuntime->samprat;
   parms->tr = (cwpsegy *) malloc(sizeof(cwpsegy));
   parms->fmt = (int *) malloc(120 * sizeof(int));
   parms->ind = (int *) malloc(120 * sizeof(int));
   parms->h = (int *) malloc(120 * sizeof(int));
   parms->first = 0;
   parms->itr = 0;
   suwrite_hdrinit(parms->fmt, parms->ind);
   *lensav = NPARMS(parms);
   *itooltype = ISIMPLE;
   return;
}

/*--------------------------------------------------------------------*\
                      Execution phase for SUWRITE 
\*--------------------------------------------------------------------*/

void exec_suwrite_(float *trace, int *ihead, float *rhead)
{
   int             j;
   cwpsegy        *tr;

   if (globalRuntime->cleanup) {
      if (parms->ichoose == 0) {
         pclose(parms->fp);
      } else {
         fclose(parms->fp);
      }
      (void) free(parms->h);
      (void) free(parms->ind);
      (void) free(parms->fmt);
      (void) free(parms->tr);
      return;
   }
   tr = parms->tr;

   if (parms->first == 0){
      parms->fp = suwrite_open(parms->ichoose, parms->cmnd,
                parms->datafile );

      memset( txt_hdr ,0x20 ,sizeof( txt_hdr ) );
      strncpy( txt_hdr ,"C 1 CLIENT",10); 

      fwrite( txt_hdr ,1 ,sizeof(txt_hdr) ,parms->fp );
      parms->first++;
      suwrite_geth(parms->h, parms->fmt, parms->ind, ihead, rhead);
      suwrite_hmap_2su(tr, parms->h);

      bin_hdr.format = 1;
      bin_hdr.hdt    = 1000 * parms->input_samprat;
      bin_hdr.hns    =  parms->input_numsmp;

      fprintf( stderr, "bin_hdr: %d %d\n" ,bin_hdr.hdt ,bin_hdr.hns);

      fwrite( &bin_hdr ,1 ,sizeof(bin_hdr) ,parms->fp );
      parms->first--;

   }
   parms->first++;
   suwrite_geth(parms->h, parms->fmt, parms->ind, ihead, rhead);
   suwrite_hmap_2su(tr, parms->h);
   tr->tracl = parms->first;
   tr->ns = parms->input_numsmp;
   tr->dt = 1000 * parms->input_samprat;
   tr->d2 = 1;
   for (j = 0; j < parms->input_numsmp; j++)
      tr->data[j] = trace[j];
   suwrite_putsu(parms->fp, tr, &parms->itr, &parms->nsegy);
   return;
}
/*--------------------------------------------------------------------*\
   return a FILE pointer to output stream whether for command connected
   by a pipe or a file

   int ichoose 0  write to pipe
               1  write to file
\*--------------------------------------------------------------------*/

#define BUFSIZ 1024

FILE* suwrite_open(
    int ichoose
   ,char *cmnd        /* command reading from pipe */
   ,char *datafile    /* output data file          */
   ){

   
   FILE* fp;
   char buf[BUFSIZ];

   if (ichoose == 0) {

      fprintf(stderr, "\nSUWRITE Pipe command:\n %s\n", cmnd);
      fp = popen(cmnd, "w");
      if (fp == NULL){
         exErrFatal("suwrite_open: Could not open output pipe to %s\n"
                   , cmnd);
      }else{
         while (fgets(buf, BUFSIZ, fp) != NULL) {
            (void) printf("%s", buf);
         }
      }

   }else if (ichoose == 1) {

      fp = fopen64(datafile, "w");
      if (fp == NULL){
         exErrFatal("suwrite_open: Could not open output file %s\n"
                   , datafile);
      }

   }else{

      /* obsolete option encountered ? */
      return( 0 );
   }
   return (fp);

}

/*--------------------------------------------------------------------*\
   Initialize trace header map from ProMAX to SU in terms of format
   and indicies
\*--------------------------------------------------------------------*/

void suwrite_hdrinit(
    int* fmt
   ,int* ind
   ){
   int j;

   fprintf( stderr, "\n\n       Output Header Mapping\n\n" );

   j = 0;
   while ((sumap[j].suoffs > -1) && (sumap[j].suoffs < 240)) {
      if (hdrExists(sumap[j].pkey)) {
         fmt[j] = hdrFormat(sumap[j].pkey);
         ind[j] = hdrIndex(sumap[j].pkey);
         fprintf( stderr ," ProMAX: %8-s " ,sumap[j].pkey   );
         fprintf( stderr ," index %3d -->"    ,j            );
         fprintf( stderr ," SU: %-8s "     ,sumap[j].sukey  );
         fprintf( stderr ," offset %4d "   ,sumap[j].suoffs );
         fprintf( stderr ,"\n" );
      } else {
         fmt[j] = HDRUNDEFINED;
         ind[j] = -1;
      }

      j++;
   }
   return;
}
/*--------------------------------------------------------------------*\
            Extract ProMAX trace header values into h array 
\*--------------------------------------------------------------------*/
void suwrite_geth(
    int*   h
   ,int*   fmt
   ,int*   ind
   ,int*   ihead
   ,float* rhead
   ){

   int j;
   j = 0;
   while ((sumap[j].suoffs > -1) && (sumap[j].suoffs < 240)) {
      if (fmt[j] == HDRINT) {
         h[j] = ihead[ind[j]];
      } else if (fmt[j] == HDRFLOAT) {
         h[j] = rhead[ind[j]];
      } else {
         h[j] = 0;
      }
      j++;
   }
   return;
}

/*--------------------------------------------------------------------*\
            Move header values into SU header from h array 
\*--------------------------------------------------------------------*/
void suwrite_hmap_2su(
    cwpsegy*  tr
   ,int* h
   ){

   tr->tracl    = h[0];
   tr->tracr    = h[1];
   tr->fldr     = h[2];
   tr->tracf    = h[3];
   tr->ep       = h[4];
   tr->cdp      = h[5];
   tr->cdpt     = h[6];
   tr->trid     = h[7];
   tr->nvs      = h[8];
   tr->nhs      = h[9];
   tr->duse     = h[10];
   tr->offset   = h[11];
   tr->gelev    = h[12];
   tr->selev    = h[13];
   tr->sdepth   = h[14];
   tr->gdel     = h[15];
   tr->sdel     = h[16];
   tr->swdep    = h[17];
   tr->gwdep    = h[18];

   tr->scalel   = 0;
   tr->scalco   = 0;

   tr->sx       = h[21];
   tr->sy       = h[22];
   tr->gx       = h[23];
   tr->gy       = h[24];
   tr->counit   = h[25];
   tr->wevel    = h[26];
   tr->swevel   = h[27];
   tr->sut      = h[28];
   tr->gut      = h[29];
   tr->sstat    = h[30];
   tr->gstat    = h[31];
   tr->tstat    = h[32];
   tr->laga     = h[33];
   tr->lagb     = h[34];
   tr->delrt    = h[35];

   tr->muts     = 0;

   tr->mute     = h[37];

   tr->ns       = globalRuntime->numsmp;
   tr->dt       = 1000 * globalRuntime->samprat;

   tr->gain     = h[40];
   tr->igc      = h[41];
   tr->igi      = h[42];
   tr->corr     = h[43];
   tr->sfs      = h[44];
   tr->sfe      = h[45];
   tr->slen     = h[46];
   tr->styp     = h[47];
   tr->stas     = h[48];
   tr->stae     = h[49];
   tr->tatyp    = h[50];
   tr->afilf    = h[51];
   tr->afils    = h[52];
   tr->nofilf   = h[53];
   tr->nofils   = h[54];
   tr->lcf      = h[55];
   tr->hcf      = h[56];
   tr->lcs      = h[57];
   tr->hcs      = h[58];
   tr->year     = h[59];
   tr->day      = h[60];
   tr->hour     = h[61];
   tr->minute   = h[62];
   tr->sec      = h[63];
   tr->timbas   = h[64];
   tr->trwf     = h[65];
   tr->grnors   = h[66];
   tr->grnofr   = h[67];
   tr->grnlof   = h[68];
   tr->gaps     = h[69];
   tr->otrav    = h[70];

   tr->d1       = globalRuntime->samprat;
   tr->f1       = 0;
   tr->d2       = 1;
   tr->f2       = 0;

   tr->ungpow   = h[75];
   tr->unscale  = h[76];
   tr->mark     = h[77];
   tr->mutb     = h[78];
   tr->dz       = h[79];
   tr->fz       = h[80];
   tr->n2       = h[81];
   tr->ens_end  = h[82];
   tr->ntr      = h[83];
   tr->cdp_x    = h[84];
   tr->cdp_y    = h[85];
   tr->aoffset  = h[86];
   tr->amp_norm = h[87];

   return;
}

/*--------------------------------------------------------------------*\
   suwrite_putsu - put a segy trace on a file by descriptor (based on
   fputtr but made reentrant)
\*--------------------------------------------------------------------*/

void suwrite_putsu(
    FILE*    fp
   ,cwpsegy* tp
   ,long*    itr
   ,int*     nsegy
   ){

   if (*itr == 0){
      *nsegy = HDRBYTES + sizeof(float) * tp->ns;
   }
   (void) fwrite(tp, 1, *nsegy, fp);
   *itr++;
   return;
}
/*--------------------------------------------------------------------*\
   suwrite_stringPrep() strips blanks spaces from filenames entered
   by the user in the ProMAX text edit widget.  It operates on the
   string in place.
\*--------------------------------------------------------------------*/

char* suwrite_stringPrep(char *s) {

   char* a=s;

   while( *s ){

      if( isspace(*s) ){
         s++;

      }else{
         *a++=*s++;

      }

   }

   *a='\0';

}

