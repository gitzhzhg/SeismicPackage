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
! Name       : cgmsubs
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

char cgmsubs_ident[100] = 
"$Id: cgmsubs.c,v 1.1 2008/02/15 20:58:09 mengewm Exp $";


#include <unistd.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <stdio.h>
#include "cgm_common.h"

#define MAX(a,b)   (  (a) > (b) ? (a) :  (b)  )

static void *buf_start;
static FILE *input_file;
static char *file_name_start;
static int eof;
static long int buffer_size;
static union 
  {
  int *word;
  unsigned char *byte;
  void *buf;
  } pointer_struct;

void cgmPutString(char in_string[], int length)
{
  int i;

  if (pointer+length+1 > max_pointer) {
     fprintf(err_unit,"Exceeded workspace buffer\n");
     return;
  }
  pointer = pointer + 1;
  buffer[pointer] = length;
  for (i=0;i<length;i++) {
    pointer = pointer + 1;
    buffer[pointer] = in_string[i];
  }
}

/* Fixed Real Number */
void cgmPutReal(float in_real)
{
  int whole, frac;

  if (in_real < 0.0) {
     whole = in_real;
     if (whole != in_real) {
        whole = in_real - 1;
        frac = ABS((in_real - whole)) * pow(2.0,16);
     }
     else
        frac = 0;
  }
  else {
     whole = in_real;
     frac = (in_real - whole) * pow(2.0,16);
  }
  cgmPutInt(whole);
  cgmPutInt(frac);
}

/* Float Real Number */
void cgmPutFloat(float inreal)
{
  float *rtemp;
  int i;
  unsigned char *btemp;

  rtemp = &inreal;
  btemp = (unsigned char *) rtemp;
  
  cgmPutFloat_c(btemp);
  for (i=0;i<4;i++) {
     pointer = pointer + 1;
     buffer[pointer] = *btemp;
     btemp++;
  }
}

void cgmPutColor(float r, float g, float b)
{
  int ri,gi,bi;

/* scale to 0 to 255 range */
  ri = r * 255.0;
  gi = g * 255.0;
  bi = b * 255.0;

  cgmPutIndex(ri);
  cgmPutIndex(gi);
  cgmPutIndex(bi);
}

void cgmPutVdc(float in_real)
{
  int out_int;
  if (vdc_type == 1)
     cgmPutReal(in_real);
  else {
     out_int = in_real + 0.5;
     cgmPutInt(out_int);
  }
}

void cgmPutInt(int in_int)
{
  int tmp_int, tmp_int2;
  int msb, lsb;

  if (in_int < 0) {
     tmp_int = pow(2.0,16) + in_int;
     tmp_int = tmp_int/256.;
     tmp_int2= 0x00008000;
     msb = cgmOrInt(&tmp_int,&tmp_int2);
     lsb = in_int - msb * 256;
  }
  else {
     msb = in_int/256.;
     lsb = in_int - msb * 256;
  }

  if (pointer+2 > max_pointer) {
     fprintf(err_unit,"Exceeded workspace buffer\n");
     return;
  }
  pointer = pointer + 1;
  buffer[pointer] = msb;
  pointer = pointer + 1;
  buffer[pointer] = lsb;
}

void cgmPutIndex(int in_int)
{
  int out_int;
  int msb;
  int lsb;

  out_int = MIN(in_int,32767);
  msb = in_int/256.;
  lsb = in_int - msb * 256;

  if (pointer+2 > max_pointer) {
     fprintf(err_unit,"Exceeded workspace buffer\n");
     return;
  }
  pointer = pointer + 1;
  buffer[pointer] = msb;
  pointer = pointer + 1;
  buffer[pointer] = lsb;
}

void cgmPutEnum(int in_int)
{
  int out_int;
  int msb;
  int lsb;

  out_int = MIN(in_int,32767);
  msb = in_int/256.;
  lsb = in_int - msb * 256;

  if (pointer+2 > max_pointer) {
     fprintf(err_unit,"Exceeded workspace buffer\n");
     return;
  }
  pointer = pointer + 1;
  buffer[pointer] = msb;
  pointer = pointer + 1;
  buffer[pointer] = lsb;
}

void cgmPutPoint(float in_x, float in_y)
{
  float tmpx,tmpy;

  cgmGswcdc(in_x,in_y,&tmpx,&tmpy);
  cgmPutVdc(tmpx*scalefactor);
  cgmPutVdc(tmpy*scalefactor);
}

void cgmPutByte(int tmpint)
{
  if (pointer+1 > max_pointer) {
     fprintf(err_unit,"Exceeded workspace buffer\n");
     return;
  }
  pointer = pointer + 1;
  buffer[pointer] = tmpint;
}

void cgmPutData(int class, int id)
{
  int i,length;
  char btemp;

  if ((pointer+1)>30 && (pointer+1)!=0) {
     length = 31;
     cgmPutHeader(class,id,length);
     cgmPutIntNow(pointer+1);
  }
  else {
     length = pointer+1;
     cgmPutHeader(class,id,length);
  }
  if (length != 0) {
     for (i=0;i<pointer+1;i++) {
        btemp = buffer[i];
        cgmPutByteNow(btemp);
     }
     if ((pointer+1)%2 != 0) {
        btemp = 0;
        cgmPutByteNow(btemp);
     }
  }
  pointer = -1;
}

void cgmPutHeader(int class, int id,int length)
{
  int headerint;

  headerint = class * pow(2,12);
  headerint = headerint + id * pow(2,5) + length;

  cgmPutIntNow(headerint);
}


/*
 * return = 0:  program found
 * return = 1:  program not found
 * return = 2:  program found, but will not fit in return variable
 * return = 3:  logical spws_bin, not assigned (VMS only)
 */

int cgmFindFile(const char *file, char *fullFileName, int *fullFileNameLen)
{
    int retval = 1;
    char *envPath, *path, *fileWithPath, *ptr;
        char env_var[30];

#ifndef VMS
        strcpy(env_var, "PATH");
#else
        strcpy(env_var, "SPWS_BIN");
#endif /* VMS */
    envPath = getenv(env_var);

    if (envPath)
    {
        assert(path = malloc(strlen(envPath) + 1));
        strcpy(path, envPath);

        assert(fileWithPath = malloc(strlen(path) + strlen(file) + 2));
#ifndef VMS
        for (ptr = strtok(path, ":"); ptr; ptr = strtok(NULL, ":"))
        {
            strcpy(fileWithPath, ptr);
            strcat(fileWithPath, "/");
            strcat(fileWithPath, file);

            if (!access(fileWithPath, R_OK))
            {
                                if (*fullFileNameLen >= strlen(fileWithPath)+1)
                {
                                   strcpy(fullFileName,fileWithPath);
                   retval = 0;
                                }
                                else
                                   retval = 2;
            }
        }
#else
        strcpy(fileWithPath, path);
        strcat(fileWithPath, file);
        strcat(fileWithPath, ".");

        if (!access(fileWithPath, R_OK))
        {
                        if (*fullFileNameLen >= strlen(fileWithPath)+1)
            {
                           strcpy(fullFileName,fileWithPath);
               retval = 0;
                        }
                        else
                           retval = 2;
        }
#endif /* VMS */
        free(path);
        free(fileWithPath);
    }
    else
    {
#ifdef VMS
        retval = 3;
#else
        assert(0);
#endif /* VMS */
    }

    return retval;
}



#define TIME_SIZE 24

void cgmGetDate(imonth,iday,iyear)

int *imonth,*iday,*iyear;
{ 
  time_t current;
  struct tm *now;

  current = time(NULL);
  now = localtime(&current);
  *imonth = now -> tm_mon + 1;
  *iday = now -> tm_mday;
  *iyear = now -> tm_year + 1900;
}



int cgmOrInt(int *ap, int *bp)
{
  return((*ap)|(*bp));
}

int cgmAndInt (int *ap, int *bp)
{
  return((*ap)&(*bp));
}


int cgmFontRead(char *file_in, short *begs, char *c, float *xs, float *ys,
                long *npoints)
{
  FILE *ifp;
  long i,istat;
  short *begs1;
  float *xs1;
  float *ys1;
  unsigned long swaptest = 1;
  unsigned char *ctmp_ptr, ctmp, ctmp2, ctmp3;

  ifp = fopen(file_in,"rb");
  if (ifp == NULL) return 1;

  if (*(char *) &swaptest) {
     if (sizeof(short) == 2 && sizeof(float) == 4) {
        ctmp_ptr = (unsigned char *) malloc(sizeof(float)*MAX(257,*npoints));
        istat = fread(ctmp_ptr,sizeof(char),2*257,ifp);
        for (i=0; i<2*257; i=i+2) {
            ctmp = ctmp_ptr[i];
            ctmp_ptr[i] = ctmp_ptr[i+1];
            ctmp_ptr[i+1] = ctmp;
        }
        begs1 = (short *) ctmp_ptr;
        for (i=0; i<257; i++) {
            begs[i] = begs1[i];
/*          printf("begs[ %d ] = %d\n",i,begs[i]);               */
        }

        istat = fread(c,sizeof(char),*npoints,ifp);

        istat = fread(ctmp_ptr,sizeof(char),4*(*npoints),ifp);
        for (i=0; i<4*(*npoints); i=i+4) {
            ctmp  = ctmp_ptr[i];
            ctmp2 = ctmp_ptr[i+1];
            ctmp3 = ctmp_ptr[i+2];
            ctmp_ptr[i] = ctmp_ptr[i+3];
            ctmp_ptr[i+1] = ctmp3;
            ctmp_ptr[i+2] = ctmp2;
            ctmp_ptr[i+3] = ctmp;
        }
        xs1 = (float *) ctmp_ptr;
        for (i=0; i<(*npoints); i++) {
            xs[i] = xs1[i];
/*          printf("xs[ %d ] =  %f\n",i,xs[i]);                  */
        }

        istat = fread(ctmp_ptr,sizeof(char),4*(*npoints),ifp);
        for (i=0; i<4*(*npoints); i=i+4) {
            ctmp  = ctmp_ptr[i];
            ctmp2 = ctmp_ptr[i+1];
            ctmp3 = ctmp_ptr[i+2];
            ctmp_ptr[i] = ctmp_ptr[i+3];
            ctmp_ptr[i+1] = ctmp3;
            ctmp_ptr[i+2] = ctmp2;
            ctmp_ptr[i+3] = ctmp;
        }
        ys1 = (float *) ctmp_ptr;
        for (i=0; i<(*npoints); i++) {
            ys[i] = ys1[i];
/*          printf("ys[ %d ] =  %f\n",i,ys[i]);                  */
        }
        free(ctmp_ptr);
     }
  }

  else {
     istat = fread(begs,sizeof(short),257,ifp);
     istat = fread(c,sizeof(char),*npoints,ifp);
     istat = fread(xs,sizeof(float),*npoints,ifp);
     istat = fread(ys,sizeof(float),*npoints,ifp);
  }

  fclose(ifp);
  return 0;
} 


long int cgmCloseFileWrite(void)
 {
 int rc;
 cgmPutBlock();
 rc = fclose(input_file);
 if (rc != 0)
   {
   printf("\nError Closing File %d",rc);
   }
 free(buf_start);
 return(0);
 }  

long int cgmOpenFileWrite(file_name_in)
/* Parameter List Data definitions */
  char *file_name_in;
  {
  eof = 0; /* Initialize to not end of file */
  file_name_start = file_name_in; /* Save the buffer starting point */
  buf_start = malloc(4096); /* Allocate the working buffer */

/*  printf("\n File %40s being opened",file_name_start);  */
  input_file = fopen(file_name_start,"w");
/*  printf("\n File Status %d",input_file);               */

  if (input_file == NULL) 
    {
    printf("\n Error Opening File %40s ",file_name_start);
    return(1);
    }
  buffer_size = 0;  
  cgmPutBlock();
  return(0);
  }
  
void cgmPutBlock()
  {
  size_t bytes_write;
  long int i;
  if (buffer_size != 0) 
    {
    if (buffer_size != 4096)
      {
      for (i=buffer_size+1;i==4096;i++)
        {
        pointer_struct.byte = NULL;
        pointer_struct.byte++;
        }
      }
    
    bytes_write = fwrite(buf_start,sizeof(char),4096,input_file);
    }
    
  if (bytes_write != 4096 && buffer_size != 0) 
    {
    printf("\n Error writing file %s ",file_name_start);
    }
  pointer_struct.buf = buf_start;
  buffer_size = 0;
  return;  
  }
         
void cgmPutByteNow(unsigned char byte)
  {
  unsigned char tmpchar;

  if (buffer_size >= 4096)
    {
    cgmPutBlock();
    }   
  tmpchar = byte;
  *pointer_struct.byte = tmpchar;
  pointer_struct.byte++; 
  buffer_size++; 
  return;
  }
      
void cgmPutIntNow(long int intin)
  {
  unsigned char msb;
  unsigned char lsb;
  if (buffer_size >= 4096)
    {
    cgmPutBlock();
    }   
  msb = intin/256;
  lsb = intin - msb*256;
  *pointer_struct.byte = msb;
  pointer_struct.byte++;
  *pointer_struct.byte = lsb;
  pointer_struct.byte++;
  buffer_size += 2;
  return;
  }  
void cgmPutFloat_c(unsigned char inreal[])
 {
  unsigned long swaptest = 1;
  unsigned char ctemp;

  if  (*(char *) &swaptest) {
     ctemp = inreal[0];
     inreal[0] = inreal[3];
     inreal[3] = ctemp;
     ctemp = inreal[1];
     inreal[1] = inreal[2];
     inreal[2] = ctemp;
  }
 }
