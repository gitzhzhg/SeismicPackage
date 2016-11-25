/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
/*------------------------------------------------------------------
 *USER DOC
 *Name   : addext_rep_
 *Purpose: Replaces or adds a file extension to a file path name
 *Author : R. Day
 *Date   : 04/09/91
 *
 *Function Definition:        ( Language = C )
 *  int addext_rep_( char name[], char ext[], int *istat )
 * *istat    output    0 if no errors occur.
 * name      in&out    File name to add extension to.
 * ext       in        Extender to add-replace at end of file name.
 *
 *NOTES:
 * 1. The extension is added as a "." plus the string in ext.
 * 2. This routine should work for VMS or Unix path names.
 *    Searches for last "]" or "/" .
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 *--------  --------    --------------------------------------------
 *END DOC
 *------------------------------------------------------------------*/
  int addext_rep_( char name[], char ext[], int *istat )
{
  char *idir, *iclose, *islash, *idot, *isem, *iext;
  char file_byt[200];
  int lenp;
  *istat=0;
  if(strlen(ext) < 2 )  return *istat;
/*
 * Parse any occurences of leading "." in the extension */
  iext = ext;
  if(ext[0]=='.') iext = ext + 1;
/*
 * Add extension to file name if it is missing */
  iclose=strstr(name,"]");
  islash=strrchr(name,'/');
  idir=NULL;
  if(iclose!=NULL) idir= iclose;
  if(islash!=NULL) idir= islash; 
  if(idir==NULL)
   { idot = strrchr(name,'.');
     isem = strrchr(name,';');
     if(idot != NULL)
       {  lenp = idot-name+1;
          strncpy(file_byt,name,lenp);
          file_byt[lenp]='\0';
          strcat(file_byt,iext); }
     else
       {  strcpy(file_byt,name);
          strcat(file_byt,".");
          strcat(file_byt,iext);}
     if(isem != NULL ) strcat(file_byt,isem);
   }
  else
   { idot = strrchr(idir,'.');
     isem = strrchr(idir,';');
     if(idot != NULL)
       {  lenp = idot-name+1;
          strncpy(file_byt,name,lenp);
          file_byt[lenp]='\0';
          strcat(file_byt,iext); }
     else
       {  strcpy(file_byt,name);
          strcat(file_byt,".");
          strcat(file_byt,iext);}
     if(isem != NULL ) strcat(file_byt,isem);
   }
 strcpy(name, file_byt);
 return *istat;
}


