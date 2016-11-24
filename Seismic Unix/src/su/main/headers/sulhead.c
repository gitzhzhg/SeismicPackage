/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SULHEAD: $Revision: 1.6 $ ; $Date: 2015/08/11 21:31:50 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation *****************************/
char *sdoc[] = {
"SULHEAD - Load information from an ascii column file into HEADERS based",
"	   on the value of the user specified header field		",
"  sulhead < inflie > outfile cf=Column_file key=..  [ optional parameters]",
"									",
" Required parameters:							",
" cf=Name of column file						",
" key=key1,key2,...Number of column entires				",
" Optional parameters:							",
" mc=1		Column number to use to match rows to traces		",
"									",
"Notes:									",
" Caveat: This is not simple trace header setting, but conditional	",
" setting.								",
"									",
" This utility reads the column file and loads the values into the	",
" specified header locations. Each column represents one set of header  ",
" words, one of them (#mc) is used to match the rows to the traces	",
" using header tr.key[mc].						",
"									",
" Example:								",
" key=cdp,ep,sx   mc=1	cf=file						",
" file contains:							",
"	1  2  3								",
"	2  3  4								",
"									",
" if tr.cdp = 1 then tr.ep and tr.sx will be set to 2 and 3		",
" if tr.cdp = 2 then tr.ep and tr.sx will be set to 3 and 4		",
" if tr.cdp=other than tr.trid=3					",
"									",
" Caveat: the user has to make it sure that number of entires in key=	",
"	 is equal the number of columns stored in the file.		",
"									",
" For simple mass setting of header words, see selfdoc of:  sushw	",
"									",
NULL};

/*
 * Credits: Balasz Nemeth, Potash Corporation, Saskatoon Saskatchewan
 * Given to CWP in 2008 
 *
 */
/**************** end self doc ********************************/

/* Segy data constants */
segy tr;				/* SEGY trace */

/* function prototype of subroutine used internally */
Value *setval_f_i( cwp_String type, int a);

int main( int argc, char *argv[] )
{

	cwp_String key[SU_NKEYS];	/* header word descriptions .. */
	int 	   kind[SU_NKEYS];	/* indexes.. */
	cwp_String ktype[SU_NKEYS];	/* and types */
	int nc;			/* number of columns */
	int rc;			/* number of rows */
	int mc;			/* master column index */
	int **tab=NULL;		/* data table */
	cwp_String tmtype;	/* master key type of trace */
	int tmkind;		/* master key index */
	Value tmval;		/* master key value */
	cwp_String cf;		/* column file naem */
	FILE *fp=NULL;		/* ..its file pointer */
	
	int verbose=0;		/* verbose flag =1 chatty */
	
	
	initargs(argc, argv);
   	requestdoc(1);
	
	
	/* read  command line data */
	/* name of the file */
	MUSTGETPARSTRING("cf",  &cf);
		
	/* Get "key" values number of columns must equal nc */
	if ((nc=countparval("key"))!=0) {
		getparstringarray("key",key);
	} else {
		key[0]="cdp";
	}
	
	/* Get parameters */
	if (!getparint("mc",&mc)) mc = 1;
	if (!getparint("verbose",&verbose)) verbose = 0;
	
        checkpars();

	if(mc<1 && mc>nc) 
		err("Master column index has to be in the range [1..nc].\n");
	
	/* compute types and indexes corresponding to the keys */
	{ int ikey;
		for (ikey=0; ikey<nc; ++ikey) {
			ktype[ikey]=hdtype(key[ikey]);
			kind[ikey]=getindex(key[ikey]);
		}
	}
	tmkind = kind[mc-1];
	tmtype = ktype[mc-1];

	/* open the file */
	fp = fopen(cf,"r");
	if(!fp) {
		err("No file %s was found!",cf);
	}
	
	/* count the rows */
	rc=0;
	while(!feof(fp)) {
		/* count the new lines hex 0a */
		if (fgetc(fp) == 0x0a ) rc++;
	}
	if(verbose) {
		warn("Number of rows in file: %d",rc);
		warn("Number of columns in file: %d",nc);
	}
	rewind(fp);
	
	/* allocate data for the table */
	tab = ealloc2int(nc,rc);
	
	/* all the header values are integer types */
	{ int ir,ic;
	  float tmp;
		for(ir=0;ir<rc;ir++) 
			for(ic=0;ic<nc;ic++) {
				fscanf(fp,"%f",&tmp);
				tab[ir][ic]=(int)(tmp+0.5);
			}
	}
	
	/* close the file */
	fclose(fp);	
	
	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	
	do{
		gethval(&tr, tmkind, &tmval);
		
		{ int ir,ic,flag=0;
			for(ir=0;ir<rc;ir++) {
				if(vtoi(tmtype,tmval)==tab[ir][mc-1]) {
					for(ic=0;ic<nc;ic++)
						puthval(&tr,kind[ic],
							setval_f_i(ktype[ic],tab[ir][ic]));
					flag=1;
				}
			}
			if(flag==0) {
				/* mark it as dummy */
				tr.trid = TDUMMY;
				if (verbose) warn("No data for trace with: ");
				if (verbose) warn(" %s ",key[mc-1]);
				if (verbose) warn("=");
				if (verbose) fprintfval(stderr,tmtype,tmval);
				if (verbose) warn("\n");
			}
		}
		
		puttr(&tr);
	} while (gettr(&tr));
	
	free2int(tab);
	return EXIT_SUCCESS;
}

Value *setval_f_i( cwp_String type, int a)
/* set value form integer */
{
	 static  Value val;
	switch (*type) {
	case 's':
		err("can't set char header word");
	break;
	case 'h':
		val.h = (short)a;
	break;
	case 'u':
		val.u = (unsigned short)a;
	break;
	case 'l':
		val.l = (long)a;
	break;
	case 'v':
		val.v = (unsigned long)a;
	break;

	case 'i':
		val.i = a;
	break;
	case 'p':
		val.p = (unsigned int)a ;
	break;
	case 'f':
		val.f = (float)a;
	break;
	case 'd':
		val.d = (double)a;
	default:
		err("unknown type %s", type);
	break;
	}
	return(&val);
}
