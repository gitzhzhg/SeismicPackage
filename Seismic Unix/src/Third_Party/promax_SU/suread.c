
/*====================================================================*\

   SUREAD: read SU input stream from pipe or file and pass to ProMAX
   executive. Handles both CWP/SU format without line headers and
   Unocal/SU format with a 3200 byte - 400 byte line header in the
   machine native form transparently.

   Original code by J.E. Anderson, Mobil/CWP (June 1993 & April 1995)

   Major overhaul to add support for files greater than 2 GB and to
   allow for 3200 byte text & 400 byte line header at beginning of
   file. Eliminated some minor memory leaks and strengthened
   testing of return codes for errors. Lots of gratutious
   reformating and commenting to make it easier for me to
   understand.

   Reginald H. Beardsley                            rhb@acm.org

\*====================================================================*/

#include     <cpromax.h>
#include     <pwd.h>

#include     "cwpsegy.h"

/*--------------------------------------------------------------------*\
                      Public function prototypes 
\*--------------------------------------------------------------------*/

#if defined( __sparc) || defined(__linux__ )

   /* popen() defined in header file */

#else
   extern FILE* popen(
       char* cmnd
      ,char* type
   );

#endif

void init_suread_(
    int* lensav
   ,int* itooltype
);

void exec_suread_(
    float* trace
   ,int*   ihead
   ,float* rhead
);

/*--------------------------------------------------------------------*\
                      Private function prototypes 
\*--------------------------------------------------------------------*/

static FILE *suread_open(
    int ichoose
   ,char* cmnd
   ,char* file
); 

static void suread_inith(
    int *ind
);

static void suread_puth(cwpsegy *tr
   ,int*   ihead
   ,float* rhead
   ,int*   ind
);

static char* suread_stringPrep(
    char *s
);

static int islinehdr(
    unsigned char* buf
);

#ifndef ABS
#define ABS(x) ((x) < 0 ? -(x) : (x))
#endif

/*--------------------------------------------------------------------*\
  The COMMMON block definition has been sorted longest first to avoid
  generating alignment errors on 64 bit machines on the assumption
  that the block is allocated in order of definition.  This may not
  be a valid assumption in some cases. <rhb@acm.org>
\*--------------------------------------------------------------------*/

BEGINPARMS
   long     ens_value;
   long     iens;
   long     numsmp;
   long     tr_in_ens;
   char*    cmnd;
   char*    datafile;
   cwpsegy* tr;
   int*     ind;
   FILE*    fp;
   float    samprat;
   int      ichoose;
   int      idtyp;
   int      ipkey;
   int      ipsort;
   int      iskey;
   int      maxdtr;
   int      nbytes;
   int      nread;
ENDPARMS(parms)

/*--------------------------------------------------------------------*\
   Initialization phase for SUREAD. Get input arguments and cleanup
   file path strings to remove embedded blanks and trailing
   newlines.
\*--------------------------------------------------------------------*/

void init_suread_( int* lensav ,int* itooltype ){

   exParGetInt("CHOOSE1",&parms->ichoose);
   exParGetString("CMND",&parms->cmnd);

   exParGetString("DATAFILE", &parms->datafile );
   suread_stringPrep(parms->datafile);

   exParGetInt("SORT",&parms->ipsort);
   exParGetInt("TYPE",&parms->idtyp);
   exParGetInt("NPENS",&parms->maxdtr);


/*--------------------------------------------------------------------*\
   Allocate save buffers and open the input stream
\*--------------------------------------------------------------------*/

   if(  !(parms->tr=(cwpsegy*)malloc(sizeof(cwpsegy)) ) 
          || !(parms->ind=(int*)malloc(120*sizeof(int))    ) ){
           exErrFatal( "suread: malloc failed!\n" );

   }

   if( !(parms->fp=suread_open(parms->ichoose
                              ,parms->cmnd
                              ,parms->datafile)) ){
      exErrFatal( "suread: unable to open input stream!" );
   }

/*--------------------------------------------------------------------*\
   Check for the presence of a SEGY line header of 3600 bytes and skip
   over it if found. Then read the first trace header to determine the
   number of samples in a trace and read the trace data for the
   first trace.
\*--------------------------------------------------------------------*/

   
   parms->nread=fread(parms->tr,1,3600,parms->fp);
   if( parms->nread != 3600  ){
      exErrFatal("suread: Could not read beginning of file!\n" );
   }

   if( !islinehdr( (void*)parms->tr ) ){
      fseeko64( parms->fp ,0 ,SEEK_SET );
   }

   parms->nread=fread(parms->tr,1,240,parms->fp);
   if( parms->nread != 240  ){
      exErrFatal("suread: Could not get first trace header.");
   }

   parms->nbytes=parms->tr->ns*sizeof(float);
   parms->nread=fread(&parms->tr->data[0] ,1 ,parms->nbytes ,parms->fp);
   if( parms->nread == 0 ){
      exErrFatal("suread: Could not get data for first trace.");

   }else if(parms->nread!=parms->nbytes){
      exErrFatal("suread: Read wrong number of bytes on first trace");

   }

   parms->nbytes+=240;
   parms->nread=parms->nbytes;

   fprintf( stderr ,"Successfully read first trace.\n"         );
   fprintf( stderr ,"Sample rate = %d usec.\n"  ,parms->tr->dt );
   fprintf( stderr ,"Number of samples = %d.\n" ,parms->tr->ns );

   parms->samprat=0.001*parms->tr->dt;
   parms->numsmp=parms->tr->ns;

/*--------------------------------------------------------------------*\
    Initialize ensemble, common block and trace header information 
\*--------------------------------------------------------------------*/

   parms->iens=1;
   parms->tr_in_ens=1;
   suread_inith(parms->ind);
   parms->iskey=1+hdrIndex("SEQNO");

   switch(parms->ipsort){
      case ICDP:
         parms->ipkey=1+hdrIndex("CDP");
         parms->ens_value=parms->tr->cdp;
         fprintf( stderr ,"suread: Input ensembles based on cdp\n");
         break;

      case ISIN:
         parms->ipkey=1+hdrIndex("SIN");
         parms->ens_value=parms->tr->fldr;
         fprintf( stderr ,"suread: Input ensembles based on fldr\n");
         break;

      case IRECSLOC:
         parms->ipkey=1+hdrIndex("REC_SLOC");
         parms->ens_value=parms->tr->wevel;
         fprintf( stderr ,"suread: Input ensembles based on wevel\n");
         break;

      case IOFFSET:
         parms->ipkey=1+hdrIndex("OFFSET");
         parms->ens_value=parms->tr->offset;
         fprintf( stderr ,"suread: Input ensembles based on offset\n");
         break;

      case ICHAN:
         parms->ipkey=1+hdrIndex("CHAN");
         parms->ens_value=parms->tr->tracf;
         fprintf( stderr ,"suread: Input ensembles based on tracf\n");
         break;

      case IUNKNOWN:
         parms->ipkey=1+hdrIndex("TRACENO");
         parms->ens_value=parms->tr->tracl;
         fprintf( stderr ,"suread: Input ensembles based on tracl\n");
         break;

   }

   globalRuntime->numsmp      = parms->numsmp;
   globalRuntime->samprat     = parms->samprat;
   globalRuntime->igeom_match = FALSE;
   globalRuntime->itrno_valid = FALSE;
   globalRuntime->idomain     = ITX;
   globalRuntime->maxdtr      = parms->maxdtr;
   globalRuntime->ipsort      = parms->ipsort;
   globalRuntime->idtyp       = parms->idtyp;
   globalRuntime->iskey       = parms->iskey;
   globalRuntime->ipkey       = parms->ipkey;

/*--------------------------------------------------------------------*\
    Return the length of the saved common block and type of module. 
\*--------------------------------------------------------------------*/

   *lensav = NPARMS(parms);
   *itooltype = INPUT;
   return;
}

/*--------------------------------------------------------------------*\
                      execution phase for SUREAD 
\*--------------------------------------------------------------------*/

void exec_suread_(
    float* trace
   ,int* ihead
   ,float* rhead
   ){

   int trbytes;
   long ens;

/*--------------------------------------------------------------------*\
   Check to see if this is the last pass or the cleanup pass and
   close files, etc.
\*--------------------------------------------------------------------*/

   if (globalRuntime->cleanup) {
      (void)free(parms->ind);
      return;

   }

   if( feof( parms->fp ) ){

      if(parms->ichoose==0) {
         pclose(parms->fp);

      }else{
         fclose(parms->fp);

      }

      exPipeMode();
      return;

   }

/*--------------------------------------------------------------------*\
   Fix up the output trace header and output trace from information
   in save buffer
\*--------------------------------------------------------------------*/

   suread_puth(parms->tr,ihead,rhead,parms->ind);

#if 0

   ihead[STDHDR(isource)]  = parms->iens;
   ihead[STDHDR(iseqno)]   = parms->tr_in_ens;

   rhead[STDHDR(itlive_s)] = 0.0;
   rhead[STDHDR(itlive_e)] = (globalRuntime->numsmp-1)
                             *globalRuntime->samprat;

   rhead[STDHDR(itfull_s)] = rhead[STDHDR(itlive_s)];
   rhead[STDHDR(itfull_e)] = rhead[STDHDR(itlive_e)];

#endif

   trbytes = globalRuntime->numsmp*sizeof(float);

   memmove( trace ,parms->tr->data ,trbytes );

/*--------------------------------------------------------------------*\
   Read one trace ahead for next pass. Always need to know ahead of
   time about changes to a new ensemble or if an end of file will
   be encountered to set end of ensemble and end of data trace
   header flags for current trace so have to always check values
   for one trace ahead of where current trace is being output. 
\*--------------------------------------------------------------------*/

readnext:

   parms->nread = fread(parms->tr,1,parms->nbytes,parms->fp);

   if( feof( parms->fp ) ){

      /* EOF encountered */

      fprintf(stderr ,"suread: Hit EOF on input stream.\n");
      fprintf(stderr ,"Read %ld ensembles.\n",parms->iens);
      fprintf(stderr
             ,"Key for last ensemble equals %ld\n" ,parms->ens_value);

      parms->tr_in_ens=1;
      ihead[STDHDR(iend_ens)]=LASTTR;
      ihead[STDHDR(ieoj)]=LASTTR;

      if(parms->ichoose==0) {
         pclose(parms->fp);

      }else{
         fclose(parms->fp);

      }

      exPipeMode();
      return;

   }

   switch(parms->ipsort){

      case ICDP: 
         ens = parms->tr->cdp;
         break;

      case ISIN:
         ens = parms->tr->fldr;
         break;

      case IRECSLOC:
         ens = parms->tr->wevel;
         break;

      case IOFFSET:
         ens = parms->tr->offset;
         break;

      case ICHAN:
         ens = parms->tr->tracf;
         break;

      case IUNKNOWN:
         ens = parms->tr->tracl;
         break;

   }

/*--------------------------------------------------------------------*\
   Encountered new ensemble. Current trace is end of ensemble
\*--------------------------------------------------------------------*/

   if(parms->ens_value!=ens) {

      if(parms->tr_in_ens>globalRuntime->maxdtr ){
         fprintf(stderr, "Too many traces for ensemble %ld. "
                 ,parms->iens);
         fprintf(stderr, "Current count = %d.\n" ,parms->tr_in_ens);
      }
      parms->ens_value=ens;
      parms->tr_in_ens=1;
      parms->iens++;
      ihead[STDHDR(iend_ens)]=LASTTR;

/*--------------------------------------------------------------------*\
      Next trace part of same ensemble.
\*--------------------------------------------------------------------*/

   }else{
      parms->tr_in_ens++;
      ihead[STDHDR(iend_ens)]=NLAST;

   }

   ihead[STDHDR(ieoj)]=NLAST;

   if(parms->tr_in_ens>globalRuntime->maxdtr){
      goto readnext;
   }

   exFlushMode();    
   return;
}

/*--------------------------------------------------------------------*\
   return a FILE pointer to input stream whether from Unix command,
   file, or command file
\*--------------------------------------------------------------------*/

FILE* suread_open(
    int ichoose
   ,char *cmnd      /* module or script writing to stdout */ 
   ,char *datafile  /* SU format input data file          */
   ){

   FILE* fp;

   if( ichoose == 0 ){

      /* read input pipe */

      fprintf(stderr,"\nsuread input pipe: \n%s\n",cmnd);
      if( !(fp=popen(cmnd,"r") ) ){
         exErrFatal("Couldn't open input");
     
      }
      return (fp);

   }else if( ichoose == 1 ){

      /*  read input data file */

      fprintf(stderr,"suread input data file: %s\n",datafile);
      fp=fopen64(datafile,"r");
      if(fp==NULL) exErrFatal("Couldn't open input");
      return (fp);

   }else{

      /* encountered obsolete option ? */

      exErrFatal( "Invalid CHOOSE option in suread.\n" );

   }
}

/*--------------------------------------------------------------------*\
   initialize ProMAX trace header values and return indecies for
   later access. ind[] returns as with ProMAX trace header indecies
\*--------------------------------------------------------------------*/

void suread_inith(int *ind) {
   int j;

   defStdHdr();

   fprintf( stderr, "\n\n       Input Header Mapping\n\n" );

   j=0;
   while( (sumap[j].suoffs>-1) && (sumap[j].suoffs<240) ){

      if( (sumap[j].pkey[0]=='S') && (sumap[j].pkey[1]=='U') ){
         ind[j]=hdrAdd( sumap[j].pkey ,sumap[j].desc
                       ,1 ,(int)sumap[j].ptype);
      }else{
         hdrAddStd(sumap[j].pkey);
         ind[j]=hdrIndex(sumap[j].pkey);

      }
      fprintf( stderr ," SU: %-8s "      ,sumap[j].sukey  );
      fprintf( stderr ," offset %4d -->" ,sumap[j].suoffs );
      fprintf( stderr ," ProMAX: %8-s "  ,sumap[j].pkey   );
      fprintf( stderr ," index %3d "     ,j );
      fprintf( stderr ,"\n" );

      j++;  
   }

   fflush( stderr );

   hdrAddStd("TFULL_E");
   hdrAddStd("TLIVE_E");
}

/*--------------------------------------------------------------------*\
   move SU header values into ProMAX trace header values
\*--------------------------------------------------------------------*/

void suread_puth(
    cwpsegy* tr     /* input SU trace header structure     */
   ,int*     ihead  /* output ProMAX integer trace headers */
   ,float*   rhead  /* output ProMAX real trace headers    */
   ,int*     ind    /* input ProMAX  trace header indices  */
   ){

   char *tp = (char *)tr;
   int j;
   long iv;
   float rv;

   initStdHdr(ihead,rhead);

   j=0;
   while( (sumap[j].suoffs>-1) && (sumap[j].suoffs<240) ) {

      if(sumap[j].sutype==1) {

        iv = *( (long *) (tp + sumap[j].suoffs) );

        if(sumap[j].ptype==1){
           ihead[ind[j]]=iv;

        }else{
           rhead[ind[j]]=iv;

        }

      }else if( sumap[j].sutype==0 ){

        iv = *( (short *) (tp + sumap[j].suoffs) );
    
        if( sumap[j].ptype==1 ){
           ihead[ind[j]]=iv;

        }else{
           rhead[ind[j]]=iv;

        }

       }else if( sumap[j].sutype==3 ) {

          iv = *( (unsigned short *) (tp + sumap[j].suoffs) );

          if(sumap[j].ptype==1){
             ihead[ind[j]]=iv;

          }else{
             rhead[ind[j]]=iv;
          
          }

       }else if(sumap[j].sutype==2) {
       
          rv = *( (float *) (tp + sumap[j].suoffs) );

          if(sumap[j].ptype==1){
             ihead[ind[j]]=rv;

          }else{
             rhead[ind[j]]=rv;

          }
       }

     j++;

     }

    /*------------------------------------------------*/
    /*  take care of elevation and coordinate scalers */
    /*------------------------------------------------*/

     if(tr->scalel<0) {
        rhead[STDHDR(irec_elev)]/=ABS(tr->scalel);
        rhead[STDHDR(isou_elev)]/=ABS(tr->scalel);
     }
     if(tr->scalel>1) {
        rhead[STDHDR(irec_elev)]*=tr->scalel;
        rhead[STDHDR(isou_elev)]*=tr->scalel;
     }
     if(tr->scalco<0) {
        rhead[STDHDR(irec_x)]/=ABS(tr->scalco);
        rhead[STDHDR(irec_y)]/=ABS(tr->scalco);
        rhead[STDHDR(isou_x)]/=ABS(tr->scalco);
        rhead[STDHDR(isou_y)]/=ABS(tr->scalco);
     }
     if(tr->scalco>1) {
        rhead[STDHDR(irec_x)]*=tr->scalco;
        rhead[STDHDR(irec_y)]*=tr->scalco;
        rhead[STDHDR(isou_x)]*=tr->scalco;
        rhead[STDHDR(isou_y)]*=tr->scalco;
     }

     rhead[STDHDR(iaoffset)]=sqrt(     
               ( ( rhead[STDHDR(irec_x)]- rhead[STDHDR(isou_x)] )
                *( rhead[STDHDR(irec_x)]- rhead[STDHDR(isou_x)] ) )
             + ( ( rhead[STDHDR(irec_y)]- rhead[STDHDR(isou_y)] )
                *( rhead[STDHDR(irec_y)]- rhead[STDHDR(isou_y)] ) ) );
     return;
}

/*--------------------------------------------------------------------*\
   suread_stringPrep() strips blanks spaces from filenames entered
   by the user in the ProMAX text edit widget.  It operates on the
   string in place.
\*--------------------------------------------------------------------*/

char *suread_stringPrep(char *s) {

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

/*--------------------------------------------------------------------*\
   islinehdr() checks to see if there is a SEGY style line header 
   using either ASCII or EBCDIC character sets.  
\*--------------------------------------------------------------------*/

static  unsigned char ascii_table[256] = {
    1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 
};
  
static  unsigned char ebcdic_table[256] = {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
    1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
    1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,
    0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
    0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
    0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
    0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
    0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
    0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
    0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0 
};
  
int islinehdr( unsigned char* buf ){

  int ascii  = 0;
  int ebcdic = 0;
  int i;

  for (i=0; i<3200; i++){

     ascii += ascii_table[buf[i]];
     ebcdic += ebcdic_table[buf[i]];
  }

  if( ascii > 3100 ){
     return(1);

  }else if( ebcdic > 3100 ){
     exErrFatal( "suread: found EBCDIC header! %d\n" ,ebcdic );

  }else{
     return(0);

  }
}

