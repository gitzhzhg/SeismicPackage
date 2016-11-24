/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "su.h"

/* mkxdrhdrsub - makes xdr-based subroutine for encoding/decoding segy structure
 *  from the output of mkoffs.sh
 *
 * Credits:
 *	Mobil: Stewart A. Levin  7/3/95
 *
 * $Author: john $
*/


#include "segy.h"
#include "hdr.h"

segy tr;

int main()
{
	int SU_NKEYS = DIM(hdr);
	int ikey, nkeys;

	/* Print header.h on stdout */
	printf("/*\n * xdrhdrsub.c - subroutine for segy header R/W\n");
	printf(" * THIS FILE IS GENERATED AUTOMATICALLY - \n");
	printf(" * see the Makefile in the ../include directory\n */\n\n");
	printf("#ifdef SUXDR\n");
	printf("#include \"su_xdr.h\"\n");
	printf("int xdrhdrsub(XDR *segy_xdr, segy *trace)\n");
	printf("{\n");
	printf(" int i1, i2;\n");
	printf(" int status;\n");
	printf(" cwp_Bool outgoing;\n");
	printf(" unsigned int u1, u2, utemp;\n");

	printf(
	"#define HHTOU(H1,H2,U) u1=H1;u1&=65535;u1<<=16;\\\n");
	printf(
	"                       u2=H2;u2&=65535;U=u1|u2;\n");
	printf(
	"#define UTOHH(U,H1,H2) \\\n");
	printf(
	"        u2=(U);i2=u2&65535;H2=(i2>32767)?(i2-65536):i2;\\\n");
	printf(
	"        u1=(U)>>16;i1=u1&65535;H1=(i1>32767)?(i1-65536):i1;\n\n");

	printf(
	"#define HUTOU(H1,U2,U) u1=H1;u1&=65535;u1<<=16;\\\n");
	printf(
	"                       u2=U2;u2&=65535;U=u1|u2;\n");
	printf(
	"#define UTOHU(U,H1,U2) \\\n");
	printf(
	"        u2=(U);U2=u2&65535;\\\n");
	printf(
	"        u1=(U)>>16;i1=u1&65535;H1=(i1>32767)?(i1-65536):i1;\n\n");

	printf(
	"#define UHTOU(U1,H2,U) u1=U1;u1&=65535;u1<<=16;\\\n");
	printf(
	"                       u2=H2;u2&=65535;U=u1|u2;\n");
	printf(
	"#define UTOUH(U,U1,H2) \\\n");
	printf(
	"        u2=(U);i2=u2&65535;H2=(i2>32767)?(i2-65536):i2;\\\n");
	printf(
	"        u1=(U)>>16;U1=u1&65535;\n\n");

	printf(
	"#define UUTOU(U1,U2,U) u1=U1;u1&=65535;u1<<=16;\\\n");
	printf(
	"                       u2=U2;u2&=65535;U=u1|u2;\n");
	printf(
	"#define UTOUU(U,U1,U2) \\\n");
	printf(
	"        u2=(U);U2=u2&65535;\\\n");
	printf(
	"        u1=(U)>>16;U1=u1&65535;\n\n");

	printf("\n status=TRUE;\n");
	printf(" outgoing = (segy_xdr->x_op == XDR_ENCODE)?cwp_true:cwp_false;\n");
	for(ikey=0; ikey < SU_NKEYS; ikey++) {
	   if('i' == hdr[ikey].type[0]) /* integer field */
	   printf(" if(FALSE == xdr_int(segy_xdr,(&trace->%s))) return(FALSE);\n",
		hdr[ikey].key);
	   if('p' == hdr[ikey].type[0]) /* unsigned integer field */
	   printf(" if(FALSE == xdr_u_int(segy_xdr,(&trace->%s))) return(FALSE);\n",
		hdr[ikey].key);
	   if('l' == hdr[ikey].type[0]) /* long integer field */
	   printf(" if(FALSE == xdr_long(segy_xdr,&(trace->%s))) return(FALSE);\n",
		hdr[ikey].key);
	   if('f' == hdr[ikey].type[0]) /* float field */
	   printf(" if(FALSE == xdr_float(segy_xdr,&(trace->%s))) return(FALSE);\n",
		hdr[ikey].key);
	   /* ideally we would handle the shorts with xdr_short, etc. but we */
	   /* want to preserve the external 2 byte formatting */
	   if(('h' == hdr[ikey].type[0]) || ('u' == hdr[ikey].type[0])) {
		printf(" if(outgoing) {\n");
		printf("    %c%cTOU(trace->%s,trace->%s,utemp)\n",
		      toupper(hdr[ikey].type[0]),
		      toupper(hdr[ikey+1].type[0]),
		      hdr[ikey].key,hdr[ikey+1].key);
		printf("    if(FALSE == xdr_u_int(segy_xdr,&utemp))status= FALSE;\n");
		printf(" } else {\n");
		printf("    if(FALSE == xdr_u_int(segy_xdr,&utemp))status= FALSE;\n");
		printf("    UTO%c%c(utemp,trace->%s,trace->%s)\n",
		      toupper(hdr[ikey].type[0]),
		      toupper(hdr[ikey+1].type[0]),
		      hdr[ikey].key,hdr[ikey+1].key);
		printf("    }\n");
		ikey++;
		}
	    }
	    /* handle remaining uassigned segy header bytes */
	    printf(" if(outgoing) {\n");
	    nkeys = DIM(((segy *) NULL)->unass);
	    for(ikey=0; ikey<nkeys; ikey += 2) {
		printf("    HHTOU(trace->unass[%d],trace->unass[%d],utemp)\n",
		      ikey, ikey+1);
		printf("    if(FALSE == xdr_u_int(segy_xdr,&utemp))status= FALSE;\n");
		}
		printf(" } else {\n");
	    for(ikey=0; ikey<nkeys; ikey += 2) {
		printf("    if(FALSE == xdr_u_int(segy_xdr,&utemp))status= FALSE;\n");
		printf("    UTOHH(utemp,trace->unass[%d],trace->unass[%d])\n",
		      ikey,ikey+1);
		}
		printf("    }\n");
	     
	    printf("\n return(status);\n");
	    printf("}\n");

	    printf("#else\n");
	    printf("void xdrhdrsub(){return;}\n");
	    printf("#endif\n");

	return EXIT_SUCCESS;
}
