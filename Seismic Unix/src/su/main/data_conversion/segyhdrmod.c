/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* : $Revision: 1.5 $ ; $Date: 2011/11/16 17:43:20 $	*/

#include "par.h"
#include "su.h"

/*********************** self documentation ***************************/

char *sdoc[] = {
" SEGYHDRMOD - replace the text header on a SEGY file		",
"								",
"   segyhdrmod text=file data=file				",
"								",
"   Required parameters:					",
"								",
"   text=      name of file containing new 3200 byte text header",
"   data=      name of file containing SEGY data set		",
"								",
" Notes:							",
" This program simply does a replacement of the content of the first",
" 3200 bytes of the SEGY file with the contents of the file specified",
" by the text= parameter. If the text header in the SEGY standard",
" ebcdic format, the user will need to supply an ebcdic format file",
" as the text=  as input file. A text file may be converted from",
" ascii to ebcdic via:						",
"   dd if=ascii_filename of=ebcdic_filename conv=ebcdic ibs=3200 count=1",
" or from ebcdic to ascii via:					",
"   dd if=ebcdic_filename of=ascii_filename ibs=3200 conv=ascii count=1",
" 								",
NULL};


/*====================================================================*\

   sgyhdrmod - replace the text header on a SEGY data file in place

   This program only reads and writes 3200 bytes

   Reginald H. Beardsley                            rhb@acm.org

\*====================================================================*/
/************************** end self doc ******************************/
      

int main(int argc ,char *argv[] ){

   FILE *txtfp=NULL;   /* file pointer for new text */
   FILE *datfp=NULL;   /* file pointer for data file */

   int n;

   char *text=NULL;
   char *data=NULL;

   char buf[3200];

   /*------------*/
   /* Initialize */
   /*------------*/

   initargs(argc, argv);
   requestdoc(2); /* two file args required */

   if( !getparstring( "text" ,&text ) ){
      err( "missing text header filename" );
   }
 
   if( !getparstring( "data" ,&data ) ){
      err( "missing data filename" );
   }
   checkpars();
 
   /*------------------*/
   /* Open input files */
   /*------------------*/

   if( !(txtfp = fopen(text, "rb")) ){
      err( "unable to open %s" ,text );
   }

   if( !(datfp = fopen(data, "rb+")) ){
      err( "unable to open %s" ,data );
   }

   /*---------------------------*/
   /* rewrite text header block */
   /*---------------------------*/

   if( (n=fread( buf ,1 ,sizeof(buf) ,txtfp )) != sizeof(buf) ){
      err( "unable to read new text header" );
   }

   if( (n=fwrite( buf ,1 ,sizeof(buf) ,datfp )) != sizeof(buf) ){
      err( "write of new text header failed!!!!!" );
   }

   return(CWP_Exit());
}
