/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUKEYCOUNT: $Revision: 1.3 $ ; $Date: 2011/11/16 22:10:29 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"                                                             ",
" SUKEYCOUNT - sukeycount writes a count of a selected key    ",
"                                                             ",
"   sukeycount key=keyword < infile [> outfile]                  ",
"                                                             ",
" Required parameters:                                        ",
" key=keyword      One key word.                                 ",
"                                                             ",
" Optional parameters:                                        ",
" verbose=0  quiet                                            ",
"        =1  chatty                                           ",
"                                                             ",
" Writes the key and the count to the terminal or a text      ",
"   file when a change of key occurs. This does not provide   ",
"   a unique key count (see SUCOUNTKEY for that).             ",
" Note that for key values  1 2 3 4 2 5                       ",
"   value 2 is counted once per occurrence since this program ",
"   only recognizes a change of key, not total occurrence.    ",
"                                                             ",
" Examples:                                                   ",
"    sukeycount < stdin key=fldr                              ",
"    sukeycount < stdin key=fldr > out.txt                    ",
"                                                             ",
NULL};

/* Credits:
 *
 *   MTU: David Forel, Jan 2005
 */
/**************** end self doc ***********************************/

segy tr ;

int
main(int argc, char **argv)
{
   cwp_String key[SU_NKEYS] ;  /* array of keywords */
   int nkeys ;            /* number of keywords to retrieve */
   int iarg ;             /* arguments in argv loop */
   int countkey = 0 ;     /* counter of keywords in argc loop */
   int verbose = 0 ;      /* verbose ? */
   cwp_String type1=NULL;     /* key string */
   float sort, sortold ;  /* for comparing new/old key values */
   int isort, isortold ;  /* for comparing new/old key values */
   int gatherkount ;      /* counter of traces within key */
   int itotal = 0 ;       /* counter of total traces */

   /* Initialize */
   initargs(argc, argv) ;
   requestdoc(1) ;

   sortold  = -99999. ;
   isortold  = -99999 ;

   /* Get key values */
   if (!getparint("verbose",&verbose)) verbose=0 ;
   if ((nkeys=countparval("key"))!=0) {
      getparstringarray("key",key) ;
   } else {
      /* support old fashioned method for inputting key fields */
      /* as single arguments:  sukeycount key1 */
      if (argc==1) err("must set one key value!") ;

      for (iarg = 1; iarg < argc; ++iarg)
      {
         cwp_String keyword ;  /* keyword */

         keyword = argv[iarg] ;

         if (verbose) warn("argv=%s",argv[iarg]);
         /* get array of types and indexes to be set */
         if ((strncmp(keyword,"output=",7)!=0)) {
            key[countkey] = keyword ;
            ++countkey ;
         }
         if (countkey==0) err("must set one key value!") ;
         if (countkey>1) err("must set only one key value!") ;
      }
      nkeys=countkey;
   }
   checkpars();
   if (nkeys>1) err("must set only one key value!") ;

   printf("\n") ;

   /* Loop over traces */
   gatherkount = 0 ;
   while (gettr(&tr)) {
      /* Do not loop over keys because only one is used */
      Value vsort ;
      gethdval(&tr, key[0], &vsort) ;
      type1 = hdtype(key[0]) ;

      if (*type1 == 'f')  /* float header */ {
         sort  = vtof(type1,vsort) ;

         /* Don't write just because first trace is new */
         if ( itotal == 0 ) sortold = sort ;

         if ( sort != sortold ) {
            printf(" %8s = %f", key[0], sortold) ;
            printf("      has  %d  trace(s)", gatherkount) ;
            printf("\n") ;
            sortold = sort ;
            gatherkount = 0 ;
         }
         ++gatherkount ;
         ++itotal ;
      } else  /* non-float header */ {
         isort = vtoi(type1,vsort) ;

         /* Don't write just because first trace is new */
         if ( itotal == 0 ) isortold = isort ;

         if ( isort != isortold )
         {
            printf(" %8s = %d", key[0], isortold) ;
            printf("      has  %d  trace(s)", gatherkount) ;
            printf("\n") ;
            isortold = isort ;
            gatherkount = 0 ;
         }
         ++gatherkount ;
         ++itotal ;
      }
   }

   /* Write after last trace is read */
   if (*type1 == 'f')
   {
      printf(" %8s = %f", key[0], sortold) ;
      printf("      has  %d  trace(s)", gatherkount) ;
      printf("\n") ;
   }
   else
   {
      printf(" %8s = %d", key[0], isortold) ;
      printf("      has  %d  trace(s)", gatherkount) ;
      printf("\n") ;
   }
   printf("\n        %d trace(s) read\n\n", itotal) ;

   return(CWP_Exit()) ;
}

