/*<CPS_v1 type="PRIMITIVE",pretag="!"/>
!----------------------------- cnfg.c -------------------------------
!----------------------------- cnfg.c -------------------------------
!----------------------------- cnfg.c -------------------------------
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
! Name       : CNFG
! Category   : main_prog
! Written    : 2002-06-20   by: Charles C. Burch
! Revised    : 2007-01-25   by: Bill Menger
! Maturity   : beta
! Purpose    : provides access to CPS configuration information
! References : These routines are called from within cnfg.f90
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! These routines provide access to values in the cps configuration file
! such as
!   cps_main_host
!   cps_lock_file
!   cps_mess_lock_file
!   pfio_port_file
!   cpsdata_nodes_file
!   cpswork_nodes_file
!   cpstemp_nodes_file
!   migration_tt_config_file
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!
!  char *cnfg_get_value_c(char *code)
!    Purpose returns a pointer to a charater starting containing the value
!      associated woth the input code.
!      values for case are:
!        cps_main_host_code
!        cps_lock_file_code
!        cps_mess_lock_file_code
!        pfio_port_file_code
!        cpsdata_nodes_file_code
!        cpswork_nodes_file_code
!        cpstemp_nodes_file_code
!        migration_tt_config_file_code
!
!  Note uses 
!    alphalib:cps_config_file_alpha environment variable for config file name
!    betalib :cps_config_file_beta  environment variable for config file name
!    prodlib :cps_config_file       environment variable for config file name
!
!  void cnfg_dump()
!    Purpose: dumps the content of the cps configuration file
!
!  void cnfg_exit()
!    Purpose: cleans up any allocated memory during use of cnfg routines.
!
!  void cnfg_get_tt_info_c(INTEGER *N_NAMES, INTEGER *N_DISKS)
!    Purpose: get number tt sets and max number of disks in a set
!
!  void cnfg_get_tt_names_c(INTEGER *N, char *NAMES, INTEGER *LEN_NAMES)
!    Purpose: get number tt sets and their names from tt_config_file
!
!  void cnfg_get_tt_set_c(char *NAME, INTEGER *N_NAME,
!    char *DISKS, INTEGER *DISK_DIM, INTEGER *DISK_LEN)
!    Purpose: get a given tt set name, find its disks
!
!  void cnfg_exit()
!    Purpose to free any memory allocated by cnfg
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author         Description
!     ----        ------         -----------
!  7. 2007-01-25  Menger         commented out warnings about config files.
!  6. 2005-07-11  Menger         Removed dependency on cgetsys, but still uses
!                                cgetsys.h definitions.
!  5. 2005-05-31  Stoeckley      Changes so will compile with C++.
!  4. 2004-01-21  C C Burch      Minor changes for SGI warnings.
!  3. 2003-03-26  C C Burch      Allow continuation(&) and quote characters.
!                                Allow values up to 256 characters in length.
!                                Added cnfg_exit to free allocated memory.
!                                Added support of cps_config_file_alpha/beta.
!  2. 2002-09-23  Ed Schmauch    If cnfg_get_value_c can't find a code,
!                                return empty string instead of NULL.
!  1. 2002-08-12  Chuck C. Burch Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
! No known portability problems
!-------------------------------------------------------------------------------
!</portability_doc>
*/

/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

char *cnfg_crou_ident =
"$Id: cnfg_crou.c,v 1.7 2007/01/26 14:22:06 Menger beta sps $";

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "cnfg.h"
#include "unix.h"
#include "cgetsys.h"

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

struct cnfg_entry {             /*table of cnfg entries*/
  char *name;
  char *value;
};

struct cnfg_entry *cnfg_entries  = NULL;
int cnfg_n_entries=0;

char *cps_config_file            = NULL;
char *default_cps_config_file    = "/usr/app/vendors/sps/etc/cps_config.dat";

char *cnfg_mig_tt_config_file    = NULL;
char *default_mig_tt_config_file = 
                          "/usr/app/vendors/sps/etc/migration_tt_config.dat";

int cnfg_n_names=0;
int cnfg_n_disks=0;
char *cnfg_none = "";

/* the following line was moved from here to below by Tom Stoeckley
   on 2005-03-22: */
/*
void cnfg_init();
*/

#ifdef __cplusplus
extern "C" {
#endif

/* the following line was moved to here from above by Tom Stoeckley
   on 2005-03-22: */
void cnfg_init();

/***********************************************
* compress a string and make fixed size 
*
* Written June 2002 by Charles C Burch
***********************************************/
int cnfg_compress_fix_length(char *out, char *in, int n){
  int i, istat;
  char c;
 
  if(n<1) return(-1);

  if(in==NULL) {
    memset(out,' ',n);
    return(1);
  }

  i=0;
  istat=1;
  while((c=(*in++))!='\0') {
    if(i==n) {
      istat=-1;
      break;
    }  
    out[i++]=c;
  }
  
  while(i<n) out[i++]=' ';
  return(istat);
} 
   
/********************************************************
* compress a string taking out blanks and make lower case
*
* Written June 2002 by Charles C Burch
********************************************************/
void cnfg_compress_lc(char *out, char *in){
  char c;
 
  while((c=(*in++))!='\0') {
    if(c!=' ') {
      if(c>='A' && c<='Z') c+=('a'-'A');
      (*out++)=c;
    }
  }
  (*out)='\0';
  return;
} 

/**************************************************************
* free any space allocated by cnfg.
*
* Written March 2003 by Charles C Burch
**************************************************************/ 
void cnfg_exit() {
  int i;

  if(cnfg_n_entries==0) return;

  for(i=0; i<cnfg_n_entries; i++) {
    free(cnfg_entries[i].name); 
    free(cnfg_entries[i].value);
  }

  free(cnfg_entries);
  cnfg_entries=NULL;
  cnfg_n_entries=0;
  cps_config_file=NULL;
  return;
}  
  
/**************************************************************
* get a line from a file and trim it, skip comments and blanks
* handle line continuation
*
* Written June 2002 by Charles C Burch
**************************************************************/ 
int cnfg_get_line(char *line_in, int n_line, FILE *fd){
  int n, comment_mode, cont_sw, n_out;
  char *line;

  /*printf("cnfg_get_line:enter\n");fflush(stdout);*/
  line=line_in;
  comment_mode=0;
  cont_sw=0;
  n_out=0;
  while(1) {
    if(fgets(line, n_line, fd)==NULL) break;


    /*strip lf's and trailing blanks(if not continuation mode)*/
    n=strlen(line)-1; 
    while(n>=0) {
      if(line[n]=='\n' || line[n]==' ') {
        line[n--]='\0';
      } else {
        break;
      }
    }

    if(n<0) continue;  /*skip blank lines*/

    if(comment_mode>0) { /*skip comment blocks*/
      if(line[0]=='*' && line[1]=='/') {
        comment_mode=0;  /*end of comment block*/
      }
      continue;
    }

    if(line[0]=='/' && line[1]=='*' && cont_sw==0) {
      comment_mode=1;    /*start of comment block*/
      continue;
    }
    if(line[0]=='#' && cont_sw==0) continue; /*skip comment lines*/

    if(line[n]!='&') {
      n_out+=(n+1);
      break;
    }

    line[n]='\0';
    n_out+=n;
    cont_sw=1;
    line+=n;
    n_line-=n;
        
  }
  
  if(n_out==0) return(-1);
  /* printf("cnfg_get_line:exit n_out=%d, line=%s\n",n_out, line_in);*/
  return(n_out);
}

/**************************************************************
* Return the address of the character value associated with an
* input case (whose values are defined in cnfg.h)
*
* Written June 2002 by Charles C Burch
**************************************************************/ 
char *cnfg_get_value_c(char *code_value) {
  int i;
  char check[80];

  if(cps_config_file==NULL) cnfg_init();
 
  cnfg_compress_lc(check,code_value); 
  for(i=0; i<cnfg_n_entries; i++) {
    if(strcmp(cnfg_entries[i].name, check)==0) 
     return(cnfg_entries[i].value);
  }
  return(cnfg_none);
}  

/**************************************************************
* add data into entries table
*
* Written June 2002 by Charles C Burch
**************************************************************/ 
int cnfg_add_entry(char *name, char *value) {
  int n, isw;
  
  /*printf("cnfg_add_entry: name=%s, value=%s\n",name,value); fflush(stdout);*/
  if((cnfg_entries=(struct cnfg_entry*)
   realloc((char *)cnfg_entries,(cnfg_n_entries+1)*sizeof(struct cnfg_entry)))
   ==NULL) {
    unix_abort_c("Unable to realloc cnfg_entries in cnfg_init-entries skipped");
  }

  n=strlen(name)+1;
  if((cnfg_entries[cnfg_n_entries].name=(char*) malloc(n))==NULL) {
    unix_abort_c("Unable to malloc name in cnfg_add_entry");
  }  
  cnfg_compress_lc(cnfg_entries[cnfg_n_entries].name, name);
  
  n=strlen(value)+1;
  if((cnfg_entries[cnfg_n_entries].value=(char*) malloc(n))==NULL) {
    unix_abort_c("Unable to malloc value in cnfg_add_entry");
  }
  

  isw=1;
  if(n>2) {
    if(value[0]=='"' && value[n-2]=='"') isw=0;
  }

  if(isw==0) {
    strcpy(cnfg_entries[cnfg_n_entries].value,value+1);
    cnfg_entries[cnfg_n_entries].value[n-3]='\0';
  } else {
    strcpy(cnfg_entries[cnfg_n_entries].value,value);
  }

  cnfg_n_entries++;
  /*printf("cnfg_add_entry: exit\n");fflush(stdout);*/
  return(0);
}   
/**************************************************************
* read configuration file if needed
*
* Written June 2002 by Charles C Burch
**************************************************************/ 
void cnfg_init() {
  FILE *fd;
  char line[300], *s;
  int n, lib;
  
  if(cnfg_entries!=NULL) return;

  cps_config_file=NULL;
  /*lib=cgetsys_library();*/
  lib=LNKLIB;                    /* from cgetsys.h */
  /*fprintf(stderr,"LINK LIB=%d\n",lib);*/
  if(lib==CGETSYS_ALPHALIB){ /*try alphalib version*/
    cps_config_file=getenv("cps_config_file_alpha");
    if(cps_config_file==NULL) lib=CGETSYS_BETALIB;
  }

  if(lib==CGETSYS_BETALIB) { /*if betalib or alpha did not work-try beta*/
    cps_config_file=getenv("cps_config_file_beta");
    if(cps_config_file==NULL) lib=CGETSYS_PRODLIB;
  }

  if(cps_config_file==NULL) {/*if prodlib or beta did not work, try prodlib*/
    cps_config_file=getenv("cps_config_file");
  }
 
  if(cps_config_file==NULL) {/*prodlib did not work, try default*/
   /* printf("Warning: Unable to get %s-default value used\n",
     "cps_config_file environment variable"); */
    cps_config_file=default_cps_config_file;
  }
  /*printf("cps_config_file=%s\n",cps_config_file);*/

  if((fd=fopen(cps_config_file,"r"))==NULL) { 
    printf("Warning: Unable to open CPS_config_file-defaults used Attempted to open [%s]\n",cps_config_file);
    return;
  }

  if(cnfg_add_entry("cps_config_file",cps_config_file)<0) return;

  while((n=cnfg_get_line(line,sizeof(line),fd))>=0) {

    /* printf("n=%d, line=%s\n",n,line); */
/*
    if((s=memchr(line,'=',n))!=NULL) { 
*/
/* the above line was changed to the following line by Tom Stoeckley
   on 2005-03-22: */
    if((s=(char*)memchr(line,'=',n))!=NULL) { 
      (*s++)='\0';
      while((*s)==' ') s++;
    } else {
      s=line+n;
    }

    if(line[0]==' ' || (*s)=='\0') {
      printf("Invalid cnfg entry(%s)\n",line);
      continue;
    }

    if(cnfg_get_value_c(line)!=cnfg_none) {
      printf("Warning-duplicate entry(%s) in cps_config_file\n",line);
      continue;
    }

    cnfg_add_entry(line,s);
  }
  fclose(fd);

  cnfg_mig_tt_config_file=cnfg_get_value_c("migration_tt_config_file");
  if(cnfg_mig_tt_config_file==cnfg_none){
    /*printf("Warning migration_tt_config_file not defined-default used\n");*/
    cnfg_mig_tt_config_file=default_mig_tt_config_file;
  }

  return;
}

/******************************************************************
* Return the address of the character value associated with an
* input case (whose values are defined in cnfg.h) Fortran interface
*
* Written June 2002 by Charles C Burch
******************************************************************/ 
void cnfg_get_value_f(char *CODE_VALUE, char *value, INTEGER *N_VALUE) {
  cnfg_compress_fix_length(value, cnfg_get_value_c(CODE_VALUE),*N_VALUE);
  return;
}

/**************************************************************
* Dump contents of reading configuration file
*
* Written June 2002 by Charles C Burch
**************************************************************/ 
void cnfg_dump() { 
  int i;

  cnfg_init();

  for(i=0;i<cnfg_n_entries;i++) {
    printf("%-30s = %s\n",cnfg_entries[i].name,cnfg_entries[i].value);
  }
  return;
}

/**************************************************************
* get number tt sets and max number of disks in a set
*
* Written June 2002 by Charles C Burch
**************************************************************/ 
void cnfg_get_tt_info_c(INTEGER *N_NAMES, INTEGER *N_DISKS) {
  FILE *fd;
  char line[120];
  int i_disks;

  cnfg_init();

  if(cnfg_n_names==0) {
    cnfg_n_names=0;
    cnfg_n_disks=0;
    i_disks=0;

    if((fd=fopen(cnfg_mig_tt_config_file,"r"))==NULL) {  
      sleep(1);
      fd=fopen(cnfg_mig_tt_config_file,"r");
    }
    if(fd!=NULL) {

      while(cnfg_get_line(line,sizeof(line),fd)>=0) {
        if(line[0]!=' ') {
          cnfg_n_names++;
          if(i_disks>cnfg_n_disks) cnfg_n_disks=i_disks;
          i_disks=0;
        } else {
          i_disks++;
        }
      }

      if(i_disks>cnfg_n_disks) cnfg_n_disks=i_disks;
      fclose(fd);
    }
  }
  (*N_NAMES)=cnfg_n_names;
  (*N_DISKS)=cnfg_n_disks;
  return;
}

/**************************************************************
* get number tt sets and their names from tt_config_file
*
* Written June 2002 by Charles C Burch
**************************************************************/ 
void cnfg_get_tt_names_c(INTEGER *N, char *NAMES, INTEGER *LEN_NAMES) {
  FILE *fd;
  char line[120];
  int result, max, len_names;

  cnfg_init();
  max=(*N);
  len_names=(*LEN_NAMES);
  result=0;

  if((fd=fopen(cnfg_mig_tt_config_file,"r"))==NULL) {  
    sleep(1);
    fd=fopen(cnfg_mig_tt_config_file,"r");
  }
  if(fd!=NULL) {

    while(cnfg_get_line(line,sizeof(line),fd)>=0) {
      if(line[0]!=' ') {
        if(result<max)
          cnfg_compress_fix_length(NAMES+result*len_names, line,len_names);
        result++;
      }
    }

    fclose(fd);
  }

  (*N)=result;
  return;
}

/**************************************************************
* get a given tt set name, find its disks
*
* Written June 2002 by Charles C Burch
**************************************************************/ 
void cnfg_get_tt_set_c(char *NAME, INTEGER *N_NAME,
   char *DISKS, INTEGER *DISK_DIM, INTEGER *DISK_LEN) {
  FILE *fd;
  char line[120];
  int n_disks, found, disk_dim, disk_len;

  cnfg_init();

  if((fd=fopen(cnfg_mig_tt_config_file,"r"))==NULL) {  
    sleep(1);
    fd=fopen(cnfg_mig_tt_config_file,"r");
  }

  n_disks=0;
  if(fd!=NULL) {
    disk_dim=(*DISK_DIM);
    disk_len=(*DISK_LEN);
    found=0;

    while(cnfg_get_line(line,sizeof(line),fd)>=0) {

      if(line[0]!=' ') {
        if(found==1) break;
        cnfg_compress_fix_length(line, line,*N_NAME);
        if(strncmp(line,NAME,*N_NAME)==0) found=1;
        continue;
      }
      if(found==1) {
        if((n_disks++)<disk_dim) 
          cnfg_compress_fix_length(DISKS+(n_disks-1)*disk_len, line,disk_len);
      }
    }

    fclose(fd);
  }

  (*DISK_DIM)=n_disks;
  return;
}

#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
