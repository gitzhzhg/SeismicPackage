/****
!<CPS_v1 type=AUXILIARY_FILE"/>
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : text
! Category   : stand-alone
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2001-04-16   by: Donna K. Vunderink
! Maturity   : beta
! Purpose    : CGM library.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2001-04-16  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>
****/

char text_ident[100] = 
"$Id: text.c,v 1.2 2008/02/15 22:05:34 mengewm Exp $";


#include "cgm_common.h"

static char font_name[MAX_FONTS][31];

void cgmTextInit()
{
  int i, length;

  for (i=0;i<MAX_FONTS;i++) {
     length = strlen(font_name[i]);
     if (length%2 != 0) length = length + 1;
      cgmPutString(font_name[i],length);
  }
  cgmPutData(1,13);

/* Initialize all text defaults in the common block */
  textfont= 1;
  textprec = 1;
  textih = 0;
  textiv = 0;
  textheight = 0.125;
  textxp = 1.0;
  textcolor = -1;
  textux = 0.0;
  textuy = 1.0;
  textpath = 0;
}

void cgmFonts()
{
  int i;

  for (i=0;i<MAX_FONTS;i++) {
    strcpy(font_name[i],"HELVETICA");
  }
}

void cgmPipFonts()
{
  strcpy(font_name[0],"PIP/Mono_Sans_Serif");
  strcpy(font_name[1],"Hershey/Cartographic_Roman");
  strcpy(font_name[2],"Hershey/Complex_Script");
  strcpy(font_name[3],"Hershey/Simplex_Script");
  strcpy(font_name[4],"Hershey/Complex_Italic");
  strcpy(font_name[5],"Hershey/Triplex_Italic");
  strcpy(font_name[6],"Hershey/Complex_Greek");
  strcpy(font_name[7],"Hershey/Complex_Greek");
  strcpy(font_name[8],"Hershey/Triplex_Roman");
  strcpy(font_name[9],"Hershey/Complex_Roman");
  strcpy(font_name[10],"Hershey/Gothic_English");
  strcpy(font_name[11],"Hershey/Gothic_German");
  strcpy(font_name[12],"Hershey/Gothic_Italian");
  strcpy(font_name[13],"Hershey/Duplex_Roman");
  strcpy(font_name[14],"Hershey/Simplex_Roman");
  strcpy(font_name[15],"Hershey/Complex_Cyrillic");
  strcpy(font_name[16],"Hershey/Simplex_Roman");
  strcpy(font_name[17],"Hershey/Simplex_Roman");
  strcpy(font_name[18],"Hershey/Simplex_Roman");
  strcpy(font_name[19],"Helvetica");
  strcpy(font_name[20],"Hershey/Duplex_Roman");
  strcpy(font_name[21],"Helvetica");
  strcpy(font_name[22],"Helvetica_Oblique");
  strcpy(font_name[23],"Helvetica_Bold");
  strcpy(font_name[24],"Helvetica_Bold_Oblique");
  strcpy(font_name[25],"Times_Roman");
  strcpy(font_name[26],"Times_Italic");
  strcpy(font_name[27],"Times_Bold");
  strcpy(font_name[28],"Times_Bold_Italic");
}


void cgmGqtxt(int *nfont,int *nprec,float *h,float *xp,float *sp,float *xup,
               float *yup,int *ih,int *iv,int *icol,int *ipath)
{
  int ierr;

  cgmGqtxfp(&ierr,nfont,nprec);
  cgmGqchxp(&ierr,xp);
  cgmGqchsp(&ierr,sp);
  cgmGqchh(&ierr,h);
  cgmGqchup(&ierr,xup,yup);
  cgmGqtxal(&ierr,ih,iv);
  cgmGqtxci(&ierr,icol);
  cgmGqtxp(&ierr,ipath);
}

void cgmGptxt(int nfont,int nprec,float h,float xp,float sp,float xup,
               float yup,int ih,int iv,int icol,int ipath)
{
  cgmGstxfp(nfont,nprec);
  cgmGschxp(xp);
  cgmGschsp(sp);
  cgmGschh(h);
  cgmGschup(xup,yup);
  cgmGstxal(ih,iv);
  cgmGstxci(icol);
  cgmGstxp(ipath);
}

/* --- extract text path */
void cgmGqtxp(int *ierr, int *ipath)
{
  *ipath = textpath;
  *ierr = 0;
}

/* --- extract text font and precision parameters */
void cgmGqtxfp(int *ierr, int *nfont, int *nprec)
{
  *nfont = textfont;
  *nprec = textprec;
  *ierr = 0;
}

/* --- extract text expansion parameters */
void cgmGqchxp(int *ierr, float *xp)
{
  *xp = textxp;
  *ierr = 0;
}

/* --- extract text space parameters */
void cgmGqchsp(int *ierr, float *sp)
{
  *sp = textsp;
  *ierr = 0;
}

/* --- extract text heigth in y wc */
void cgmGqchh(int *ierr, float *h)
{
  float tmp;

  cgmGsdcwc(textheight,textheight,&tmp,h);
  *ierr = 0;
}

/* --- extract text heigth in x wc */
void cgmGqchhX(int *ierr, float *h)
{
  float tmp;

  cgmGsdcwc(textheight,textheight,h,&tmp);
  *ierr = 0;
}

/* --- extract text width in x wc */
void cgmGqchwX(int *ierr, float *h)
{
  float tmp=0.0;

  cgmGsdcwc(textheight*textxp,tmp,h,&tmp);
  *ierr = 0;
}

/* --- extract text width in y wc */
void cgmGqchwY(int *ierr, float *h)
{
  float tmp=0.0;

  cgmGsdcwc(tmp,textheight*textxp,&tmp,h);
  *ierr = 0;
}

/* --- extract character spacing in y wc */
void cgmGqchspY(int *ierr, float *sp)
{
  float tmp;

  tmp = textsp * textheight;
  cgmGsdcwc(tmp,tmp,&tmp,sp);
  *ierr = 0;
}

/* --- extract character spacing in x wc */
void cgmGqchspX(int *ierr, float *sp)
{
  float tmp;

  tmp = textsp * textheight;
  cgmGsdcwc(tmp,tmp,sp,&tmp);
  *ierr = 0;
}

/* --- extract text up direction parameters */
void cgmGqchup(int *ierr, float *xup, float *yup)
{
  *xup = textux;
  *yup = textuy;
  *ierr = 0;
}

/* --- extract text alignment parameters */
void cgmGqtxal(int *ierr, int *ih, int *iv)
{
  *ih = textih;
  *iv = textiv;
  *ierr = 0;
}

/* Define the text font and precision */
void cgmGstxfp(int iff, int ip)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gstxfp: %d %d \n",iff,ip);
  textfont = iff;
  textprec = ip;
  if (iff > MAX_FONTS) {
     fprintf(err_unit," Font if = %d ip = %d \n",iff,ip);
     fprintf(err_unit,"     Not found -- default used");
     textfont = 1;
  }
  cgmPutInt(textfont);
  cgmPutData(5,10);
  cgmPutInt(textprec);
  cgmPutData(5,11);
}

/* Set the character height corresponding to the distance h
** in the y-world (vertical) coordinate dimension
*/
void cgmGschh(float h)
{
  float tmp, tmp1, tmp2;

  if (cgmdebug) fprintf(err_unit,"cgm_gschh: %f \n",h);
  if (h <= 0.0) {
     fprintf(err_unit,"Bad GSCHH Parameter -- %f \n",h);
     return;
  }
  cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
  cgmGswcdc(h+tmp1,h+tmp2,&tmp,&textheight);
  tmp = ABS(textheight*scalefactor);
  if (vdc_type == 0 && tmp < 1.0) tmp = 1.0;
  cgmPutVdc(tmp);
  cgmPutData(5,15);
}

/* Set the character height corresponding to the distance h
** in the x-world (horizonal) coordinate dimension
*/
void cgmGschhX(float h)
{
  float tmp, tmp1, tmp2;

  if (cgmdebug) fprintf(err_unit,"cgm_gschh_x: %f \n",h);
  if (h <= 0.0) {
     fprintf(err_unit,"Bad GSCHH_X Parameter -- %f \n",h);
     return;
  }
  cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
  cgmGswcdc(h+tmp1,h+tmp2,&textheight,&tmp);
  tmp = ABS(textheight*scalefactor);
  if (vdc_type == 0 && tmp < 1.0) tmp = 1.0;
  cgmPutVdc(tmp);
  cgmPutData(5,15);
}

/* Character expansion factor */
void cgmGschxp(float xp)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gschxp: %f \n",xp);
  if (xp < 0.0) {
     fprintf(err_unit,"Bad GSCHXP Parameter -- %f \n",xp);
     return;
  }
  textxp = xp;
  cgmPutReal(textxp);
  cgmPutData(5,12);
}

/* Calculate the character expansion factor from
** the character width corresponding to the distance w
** in the y-world (vertical) coordinate dimension
*/
void cgmGschwY(float w)
{
  float tmp, tmp1, tmp2, wid;

  if (cgmdebug) fprintf(err_unit,"cgm_gschw_y: %f \n",w);
  if (w < 0.0) {
     fprintf(err_unit,"Bad GSCHW_V Parameter -- %f \n",w);
     return;
  }
  cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
  cgmGswcdc(w+tmp1,w+tmp2,&tmp,&wid);
  textxp = wid / textheight;
  cgmPutReal(textxp);
  cgmPutData(5,12);
}

/* Calculate the character expansion factor from
** the character width corresponding to the distance w
** in the x-world (horizonal) coordinate dimension
*/
void cgmGschwX(float w)
{
  float tmp, tmp1, tmp2, wid;

  if (cgmdebug) fprintf(err_unit,"cgm_gschw_x: %f \n",w);
  if (w < 0.0) {
     fprintf(err_unit,"Bad GSCHW_H Parameter -- %f \n",w);
     return;
  }
  cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
  cgmGswcdc(w+tmp1,w+tmp2,&wid,&tmp);
  textxp = wid / textheight;
  cgmPutReal(textxp);
  cgmPutData(5,12);
}

/* Character space expansion factor */
void cgmGschsp(float sp)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gschsp: %f \n",sp);
  textsp = sp;
  cgmPutReal(textsp);
  cgmPutData(5,13);
}

/* Calculate the character space expansion factor from
** the character space corresponding to the distance sp
** in the y-world (vertical) coordinate dimension
*/
void cgmGschspY(float sp)
{
  float tmp, tmp1, tmp2, hgt;

  if (cgmdebug) fprintf(err_unit,"cgm_gschsp_y: %f \n",sp);
  cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
  cgmGswcdc(sp+tmp1,sp+tmp2,&tmp,&hgt);
  textsp = hgt / textheight;
  cgmPutReal(textsp);
  cgmPutData(5,13);
}

/* Calculate the character space expansion factor from
** the character space corresponding to the distance sp
** in the x-world (horizonal) coordinate dimension
*/
void cgmGschspX(float sp)
{
  float tmp, tmp1, tmp2, hgt;

  if (cgmdebug) fprintf(err_unit,"cgm_gschsp_x: %f \n",sp);
  cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
  cgmGswcdc(sp+tmp1,sp+tmp2,&hgt,&tmp);
  textsp = hgt / textheight;
  cgmPutReal(textsp);
  cgmPutData(5,13);
}

/* Output the character orientation */
void cgmGschup(float ux, float uy)
{
  float r;

  if (cgmdebug) fprintf(err_unit,"cgm_gschup: %f %f \n",ux,uy);
  if (ux == 0.0 && uy == 0.0) {
     fprintf(err_unit,"Bad GSCHUP Parameter -- %f %f \n",ux,uy);
     return;
  }
  textux = ux*cos(w_ang_c)-uy*sin(w_ang_c);
  textuy = ux*sin(w_ang_c)+uy*cos(w_ang_c);
  r = sqrt(textux*textux+textuy*textuy);
  textux = 10000.0*textux/r;
  textuy = 10000.0*textuy/r;
  cgmPutVdc(textux);
  cgmPutVdc(textuy);
  cgmPutVdc(textuy);        /* This is the base vector (-90 to up vector) */
  cgmPutVdc(-1.0*textux);
  cgmPutData(5,16);
}

/* Text Path */
void cgmGstxp(int dir)
{
  if (cgmdebug) fprintf(err_unit,"gstxp: %d \n",dir);
  if (dir < 0 || dir > 3) {
     fprintf(err_unit,"Bad GSTXP Parameter -- %d \n",dir);
     return;
  }
  textpath = dir;
  cgmPutEnum(textpath);
  cgmPutData(5,17);
}

/* Text horizontal and vertical alignment */
void cgmGstxal(int ih, int iv)
{
  if (cgmdebug) fprintf(err_unit,"cgm_gstxal: %d %d \n",ih,iv);
  if (ih < 0 || ih > 3 || iv < 0 || iv > 5) {
     fprintf(err_unit,"Bad GSTXAL Parameter -- %d %d \n",ih,iv);
     return;
  }
  textih = ih;
  textiv= iv;
  cgmPutEnum(textih);
  cgmPutEnum(textiv);
  cgmPutReal(0.0);
  cgmPutReal(0.0);
  cgmPutData(5,18);
}

/* Routine to display text */
void cgmGtx(float x, float y, char *a)
{
  int i;
  char string[256];

  if (cgmdebug) fprintf(err_unit,"cgm_gtx: %f %f \n",x,y);
  i = 0;
  while (a[i] > 31 && a[i] < 127 && i <= 255) {
    string[i] = a[i];
    i = i + 1;
  }
  string[i] = '\0';
  if (i <= 0) return;
  if (cgmdebug) fprintf(err_unit,"%s \n",string);
  if (textcolor == -1)  cgmGstxci(1);
  cgmPutPoint(x,y);
  cgmPutEnum(1);                                 /* Final Text Flag */
  cgmPutString(string,i);
  cgmPutData(4,4);
}

/* Routine to display restricted text */
void cgmGtxr(float dx,float dy, float x,float y, char *a)
{
  int i;
  char string[256];
  float tmp1, tmp2;

  if (cgmdebug) fprintf(err_unit,"cgm_gtxr: %f %f %f %f \n",dx,dy,x,y);
  i = 0;
  while (a[i] > 31 && a[i] < 127 && i <= 255) {
    string[i] = a[i];
    i = i + 1;
  }
  string[i] = '\0';
  if (i <= 0) return;
  if (cgmdebug) fprintf(err_unit,"%s \n",string);
  if (textcolor == -1)  cgmGstxci(1);
  cgmGsdcwc(0.0,0.0,&tmp1,&tmp2);
  cgmGswcdc(dx+tmp1,dy+tmp2,&dx,&dy);
  dx = ABS(dx);
  dy = ABS(dy);
  cgmPutVdc(dx);
  cgmPutVdc(dy);
  cgmPutPoint(x,y);
  cgmPutEnum(1);                                 /* Final Text Flag */
  cgmPutString(string,i);
  cgmPutData(4,5);
}
