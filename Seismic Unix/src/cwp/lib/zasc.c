/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/************************************************************************
ZASC - routine to translate ncharacters from ebcdic to ascii

	zasc - convert n characters from ebcdic to ascii format

*************************************************************************
Input:
nchar		number of characters to be translated
ainput		pointer to input characters

Output:
aoutput		pointer to output characters

*************************************************************************
Function Prototype:
int zasc(char *ainput, char *aoutput, integer nchar);
*************************************************************************
Notes:
 translated by f2c.  Horribly inefficient, but little used
 
*************************************************************************
Author: Stew Levin of Mobil, 1997
************************************************************************/
/**************** end self doc ********************************/

typedef int integer;

/*<       SUBROUTINE ZASC(INPUT,OUTPUT,NCHAR) >*/
#ifdef __STDC__
/* Subroutine */ int zasc(char *ainput, char *aoutput, integer nchar)
#else
/* Subroutine */ int zasc(ainput, aoutput, nchar)
char *ainput; char *aoutput; integer nchar;
#endif
{
    /* Initialized data */

    static integer t1[96] = { 32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
	    48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,
	    70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,
	    92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
	    110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,
	    126,127 };
    static integer t2[96] = { 64,79,127,123,91,108,80,125,77,93,92,78,107,96,
	    75,97,240,241,242,243,244,245,246,247,248,249,122,94,76,126,110,
	    111,124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,
	    214,215,216,217,226,227,228,229,230,231,232,233,74,224,90,95,108,
	    64,129,130,131,132,133,134,135,136,137,145,146,147,148,149,150,
	    151,152,153,162,163,164,165,166,167,168,169,192,106,208,161,64 };

    /* System generated locals */
    integer i__1;

    /* Local variables */
    char this_[1];
    integer i, k, kk;
    char *input, *output;

/*# 2 "zasc.f"*/
/*<       INTEGER NCHAR >*/
/*# 3 "zasc.f"*/
/*<       CHARACTER*1 INPUT(NCHAR),OUTPUT(NCHAR) >*/
/*# 4 "zasc.f"*/
/*<       CHARACTER*1 THIS >*/
/*# 5 "zasc.f"*/
/*<       INTEGER T1(96),T2(96) >*/
/*# 6 "zasc.f"*/
/*<    >*/
    /* Parameter adjustments */
    output = aoutput-1;
    input = ainput-1;

    /* Function Body */
/* *','"','#','$','%','&',1H','(',')','*','+',',','-','.','/', */
/*    1 ' ',' */
/*    2 '0','1','2','3','4','5','6','7','8','9',':',';','<','=','>','?', 
*/
/*    3 '@','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O', 
*/
/*    4 'P','Q','R','S','T','U','V','W','X','Y','Z','[','\',']','^','_', 
*/
/*    5 ' ','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o', 
*/
/*    6 'p','q','r','s','t','u','v','w','x','y','z','{','|','}','~',' '/ 
*/
/*# 20 "zasc.f"*/
/*<    >*/

/*     CONVERT NCHAR CHARACTERS FROM EBCDIC TO ASCII */

/*# 30 "zasc.f"*/
/*<       DO 100 I=1,NCHAR >*/
    i__1 = nchar;
    for (i = 1; i <= i__1; ++i) {
/*# 31 "zasc.f"*/
/*<       KK=ICHAR(INPUT(I)) >*/
	kk = input[i];

/*# 33 "zasc.f"*/
/*<       DO 300 K=1,96 >*/
	for (k = 1; k <= 96; ++k) {
/*# 34 "zasc.f"*/
/*<       IF(KK.NE.T2(K)) GO TO 300 >*/
	    if (kk != t2[k - 1]) {
		goto L300;
	    }
/*# 35 "zasc.f"*/
/*<       THIS=CHAR(T1(K)) >*/
	    *this_ = t1[k - 1];
/*# 36 "zasc.f"*/
/*<       GO TO 200 >*/
	    goto L200;
/*# 37 "zasc.f"*/
/*<   300 CONTINUE >*/
L300:
	    ;
	}
/*# 38 "zasc.f"*/
/*<       THIS=' ' >*/
	*this_ = ' ';

/*# 40 "zasc.f"*/
/*<   200 OUTPUT(I)=THIS >*/
L200:
	output[i] = *this_;

/*# 42 "zasc.f"*/
/*<   100 CONTINUE >*/
/* L100: */
    }

/*# 44 "zasc.f"*/
/*<       RETURN >*/
    return 0;
/*# 45 "zasc.f"*/
/*<       END >*/
} /* zasc_ */

