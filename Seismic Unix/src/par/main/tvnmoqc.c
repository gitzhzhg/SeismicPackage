/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* David Forel    May 2005 */

#include "par.h"
#include <string.h>

/*********************** self documentation **************************/
char *sdoc[] = {
"                                                                    ",
" TVNMOQC - Check tnmo-vnmo pairs; create t-v column files           ",
"                                                                    ",
" tvnmoqc [parameters] cdp=... tnmo=... vnmo=...                     ",
"                                                                    ",
"   Example:                                                         ",
" tvnmoqc mode=1 \\                                                  ",
" cdp=15,35 \\                                                       ",
" tnmo=0.0091,0.2501,0.5001,0.7501,0.9941 \\                         ",
" vnmo=1497.0,2000.0,2500.0,3000.0,3500.0 \\                         ",
" tnmo=0.0082,0.2402,0.4902,0.7402,0.9842 \\                         ",
" vnmo=1495.0,1900.0,2400.0,2900.0,3400.0                            ",
"                                                                    ",
" Required Parameter:                                                ",
"   prefix=        Prefix of output t-v file(s)                      ",
"                  Required only for mode=2                          ",
"                                                                    ",
" Optional Parameter:                                                ",
"   mode=1         1=qc: check that tnmo values increase             ",
"                  2=qc and output t-v files                         ",
"                                                                    ",
" mode=1                                                             ",
"   TVNMOQC checks that there is a tnmo and vnmo series for each CDP ",
"     and it checks that each tnmo series increases in time.         ",
"                                                                    ",
" mode=2                                                             ",
"   TVNMOQC does mode=1 checking, plus ...                           ",
"                                                                    ",
"   TVNMOQC converts par (MKPARFILE) values written as:              ",
"                                                                    ",
"          cdp=15,35,...,95 \\                                       ",
"          tnmo=t151,t152,...,t15n \\                                ",
"          vnmo=v151,v152,...,v15n \\                                ",
"          tnmo=t351,t352,...,t35n \\                                ",
"          vnmo=v351,v352,...,v35n \\                                ",
"          tnmo=... \\                                               ",
"          vnmo=... \\                                               ",
"          tnmo=t951,t952,...,t95n \\                                ",
"          vnmo=v951,v952,...,v95n \\                                ",
"                                                                    ",
"   to column format. The format of each output file is:             ",
"                                                                    ",
"          t1 v1                                                     ",
"          t2 v2                                                     ",
"           ...                                                      ",
"          tn vn                                                     ",
"                                                                    ",
"   One file is output for each input pair of tnmo-vnmo series.      ",
"                                                                    ",
"   A CDP VALUE MUST BE SUPPLIED FOR EACH TNMO-VNMO ROW PAIR.        ",
"                                                                    ",
"   Prefix of each output file is the user-supplied value of         ",
"     parameter PREFIX.                                              ",
"   Suffix of each output file is the cdp value.                     ",
"   For the example above, output files names are:                   ",
"     PREFIX.15  PREFIX.35  ...  PREFIX.95                           ",
"                                                                    ",
NULL};

/* Credits:
 *      MTU: David Forel (adapted from SUNMO)
 */
/**************** end self doc ***************************************/

void mktvfile(char outfile[], int ntnmo, float *tnmo, float *vnmo);
int main(int argc, char **argv)
{
   int k;              /* index used in loop */
   int mode;           /* mode=1: qc; mode=2: qc + make cdp-t-v file */
   int icdp;           /* index into cdp array */
   int ncdp;           /* number of cdps specified */
   int *cdp;           /* array[ncdp] of cdps */
   int nvnmo;          /* number of vnmos specified */
   float *vnmo;        /* array[nvnmo] of vnmos */
   int ntnmo;          /* number of tnmos specified */
   float *tnmo;        /* array[ntnmo] of tnmos */
   cwp_String prefix;  /* prefix of output files */
   char dot[] = ".";   /* for output file name */
   char outfile[80];   /* output file name */

   /* Hook up getpar */
   initargs(argc, argv);
   requestdoc(1);

   /* Get parameters */
   if(!getparint("mode",&mode))mode=1;
   if(mode==2)
      if (!getparstring("prefix", &prefix))
         err("When mode=2, you must supply a prefix name.");

   /* Are there cdp values and vnmo-tnmo sets for each cdp? */
   ncdp = countparval("cdp");
   warn("This file has %i CDPs.",ncdp);
   if (ncdp>0) {
      if (countparname("vnmo")!=ncdp)
         err("A vnmo set must be specified for each cdp");
      if (countparname("tnmo")!=ncdp)
         err("A tnmo set must be specified for each cdp");
   } else {
      err("A cdp value must be supplied for each tnmo-vnmo set");
   }

   /* Get cdp values */
   cdp = ealloc1int(ncdp);
   if (!getparint("cdp",cdp))
      err("A cdp value must be supplied for each tnmo-vnmo set");


   /* Get tnmo-vnmo values */
   for (icdp=0; icdp<ncdp; ++icdp)
   {
      nvnmo = countnparval(icdp+1,"vnmo");
      ntnmo = countnparval(icdp+1,"tnmo");
      if (nvnmo!=ntnmo)
         err("number of vnmo and tnmo values must be equal");
      if (nvnmo==0) err("Each cdp must have at least one velocity");
      if (ntnmo==0) err("Each cdp must have at least one time");

      vnmo = ealloc1float(nvnmo);
      tnmo = ealloc1float(nvnmo);

      if (!getnparfloat(icdp+1,"vnmo",vnmo))
         err("Each cdp must have at least one velocity");
      if (!getnparfloat(icdp+1,"tnmo",tnmo))
         err("Each cdp must have at least one time");
      for (k=1; k<ntnmo; ++k)
         if (tnmo[k]<=tnmo[k-1]) {
            warn("tnmo values must increase for use in NMO");
            /* A tnmo series that does not increase is not an
               error in velocity QC (but IS an error in SUNMO) */
            warn("For cdp=%i, check times %g and %g.",
                  cdp[icdp],tnmo[k-1],tnmo[k]);
         }

      /* write cdp-tnmo-vnmo values to file "prefix" "dot" "cdp" */
      if (mode == 2) {
         sprintf(outfile,"%s%s%i",prefix,dot,cdp[icdp]);
         mktvfile(outfile,ntnmo,tnmo,vnmo);
      }


      free1float(vnmo);
      free1float(tnmo);
   }

   free1int(cdp);
   warn("End of cdp-tnmo-vnmo check.");

   return(CWP_Exit());
}


void mktvfile(char outfile[], int ntnmo, float *tnmo, float *vnmo)
{
   int i;       /* index used in loop */
   FILE *fptr;  /* file pointer for output */

   fptr = efopen(outfile, "w");
   for (i=0; i<ntnmo; ++i) {
      fprintf(fptr,"%g   %g\n",tnmo[i],vnmo[i]);
   }
   efclose(fptr);
}

