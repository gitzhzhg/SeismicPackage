/*<CPS_v1 type="PRIMITIVE"/>
!------------------------------- pcpsx_crou.c --------------------------------
!------------------------------- pcpsx_crou.c --------------------------------
!------------------------------- pcpsx_crou.c --------------------------------

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
! Name       : PCPSX
! Category   : main_prog
! Written    : 2000-11-29   by: Charles C. Burch
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : provides access to system utilities through c.
! References : These routines are called from within pcpsx.f90
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!subroutines for internal PCPS usage:
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
! Do not call these routines--they are for internal PCPS usage
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author      Description
!     ----        ------      -----------
!  6. 2005-05-31  Stoeckley   Fix to compile with C++.
!  5. 2004-03-15  C C Burch   Add line_break/work_group to buffer routines.
!                             Delete routines moved to unix_crou. 
!  4. 2003-05-28  C C Burch   Use lnklst routines rather than old pfio versions 
!  3. 2002-05-06  C C Burch   Added input data buffer routines
!  2. 2001-05-18  C C Burch   Added use of c2f_interface.h
!  1. 2000-11-29  C C Burch   Initial Version.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!  No known portability problems
!-------------------------------------------------------------------------------
!</portability_doc>

*/

/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

char *pcpsx_crou_ident =
"$Id: pcpsx_crou.c,v 1.6 2005/05/31 13:04:10 Stoeckley prod sps $";

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>

#include "c2f_interface.h"
#include "pcps.h"
#include "pcpsx.h"
#include "lnklst.h"

#ifndef CHARACTER
#define CHARACTER char
#endif

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/************************** LINK LIST VARIABLES *************************/
char* pcpsx_group_space(INTEGER);

/****************** structures and data definitions ***************/

int pcpsx_init_c_sw=0;              /*sw to init once and only once*/
int NULL_ACTION=-4;                 /*must agree with pcpsx.f90 definition*/

int  sizeof_double, sizeof_float, sizeof_integer;

/* ---------------------- list_struct list data  ------------------------ */
struct    list_struct {
  INTEGER instance;     /*parallel group instance          */
  INTEGER trace_group;  /*trace group number               */
  INTEGER ntr;          /*ntr of trace group               */
  INTEGER hdr_len;      /*length of headers                */
  INTEGER trc_len;      /*length of trace data             */
  INTEGER work_group;   /*work group for data, -1 for any  */
  INTEGER line_break;   /*pcps_line break for this data    */
  char    *data;        /*storage of header and trace data */
  struct list_struct *next;
};
struct list_struct *do_par_input_buff[2];   /*roots for input buffer*/

/* ---------------------- trace tracking list  ------------------------ */
struct lnklst_struct *pcpsx_trace_list[2]=LNKLST_INITIALIZER;
FILE *trace_fl=NULL;
#define PCPS_TRACKING_FILE "PCPS_TRACE_TRACKING.dat"

/*********************** functions called externally ***************/

/***************** PUT_TRACE_LIST_C *******************
* Add trace tracking info into pcpsx_trace_list 
*
* Written September 2001 by Charles C Burch
******************************************************/
void pcpsx_put_trace_list_c(DOUBLE *TIME, INTEGER *GROUP, INTEGER *INSTANCE, 
 INTEGER *WORKER, char *ACTION, INTEGER * mode){

  char buff[512], buff1[512];
  long time;

  if(pcpsx_init_c_sw==0) pcpsx_init_c();

  time=(*TIME)*10+.5;
  sprintf(buff,"%ld,%d,%d,%d,%c%c",
   time, *GROUP, *INSTANCE, *WORKER, ACTION[0],'\0');

  if((*mode)==2) {
    if(trace_fl==NULL) {
      if((trace_fl=fopen(PCPS_TRACKING_FILE,"w+"))==NULL)
       printf("Unable to open trace tracking file\n");
    }
  }

  if(trace_fl!=NULL) {
    strcpy(buff1,buff);
    strcat(buff1,"\n");
    fputs(buff1, trace_fl);
    fflush(trace_fl);
  } else {
    lnklst_put_list_entry(pcpsx_trace_list, buff, "");
  }
  return;
}

/******************* GROUP_SPACE **********************
* Return spaces according to group number 
*
* Written February 2002 by Charles C Burch
******************************************************/
char *pcpsx_group_space(INTEGER GROUP){
  static char buff[80];
  int n;

  n=GROUP%10;
  if(n==0) n=10;
  n=5*(n-1);
  if(n>0) memset(buff,' ',n);
  buff[n]='\0';
  return(buff);
}
 
/***************** TRACE_PRINT **********************
* Print contents of a trace tracking entry 
*
* Written February 2002 by Charles C Burch
******************************************************/
void pcpsx_trace_print(char *buff){
  long time, t1;
  INTEGER GROUP,INSTANCE, WORKER;
  char action;

  sscanf(buff,"%ld,%d,%d,%d,%c", &t1, &GROUP, &INSTANCE, &WORKER, &action);
  time=t1/10;
  t1-=(time*10);
  switch((int) action) {
  case 'R':
    printf(
     "%4ld.%1ld %s(%d,%d) received wg %d\n",
     time,t1, pcpsx_group_space(GROUP), GROUP, INSTANCE, WORKER);
    break;

  case 'S':
    printf("%4ld.%1ld %s(%d,%d) sent wg %d\n",
     time,t1, pcpsx_group_space(GROUP), GROUP, INSTANCE, WORKER); 
    break;

  case 'N':
    printf("%4ld.%1ld (NT,%d) sent wg %d\n",
     time,t1, INSTANCE, WORKER); 
    break;
 
  case 'n':
    printf("%4ld.%1ld (NT,%d) received wg %d\n",
     time,t1, INSTANCE, WORKER); 
    break;

  case 'E':
    printf("%4ld.%1ld (NMT,%d) sent wg %d\n",
     time,t1, INSTANCE, WORKER); 
    break;

  case 'e':
    printf("%4ld.%1ld (NMT,%d) received wg %d\n",
     time,t1, INSTANCE, WORKER); 
    break;

  case 'B':
    /*Note 1 and -2 should be obsolete */
    if(WORKER==1) {
      printf("%4ld.%1ld %s(%d,%d) buffered\n",
       time,t1, pcpsx_group_space(GROUP), GROUP, INSTANCE); 
    } else if(WORKER==(-2)) {
      printf("%4ld.%1ld (NT,%d) buffered\n",
       time,t1, INSTANCE); 
    } else if(WORKER==0) {
      printf("%4ld.%1ld (NMT,%d) buffered\n",
       time,t1, INSTANCE); 
    } else {
      printf("%4ld.%1ld Unknown Action:%d, instance:%d buffered\n",
       time,t1, WORKER, INSTANCE); 
    }
    break;

  case 'G':
    if(WORKER<0) {
      printf("%4ld.%1ld (NT,%d) buffered\n",
       time,t1, INSTANCE); 
    } else {
      printf("%4ld.%1ld (NT,%d) buffered for wg %d\n",
       time,t1, INSTANCE, WORKER); 
    }
    break;

  case 's':
    if(WORKER<0) {
      printf("%4ld.%1ld %s(%d,%d) buffered\n",
       time,t1, pcpsx_group_space(GROUP), GROUP, INSTANCE); 
    } else {
      printf("%4ld.%1ld %s(%d,%d) buffered for wg %d\n",
       time,t1, pcpsx_group_space(GROUP), GROUP, INSTANCE, WORKER); 
    }
    break;

  case 'b': 
/**** In this case WORKER contains NTR ****/
    if(WORKER>0) {
      printf(
       "%4ld.%1ld %s(%d,%d), ntr:%d received from BUNCH\n",
       time,t1, pcpsx_group_space(GROUP), GROUP, INSTANCE, WORKER); 
    } else if(WORKER<0){
      printf(
       "%4ld.%1ld %s(%d,%d), ntr:%d sent to BUNCH\n",
       time,t1, pcpsx_group_space(GROUP), GROUP, INSTANCE, -WORKER); 
    } else {
      printf(
       "%4ld.%1ld (NMT,%d) sent to BUNCH\n",
       time,t1, INSTANCE); 
    }
    break;

  default:
    printf(
   "%4ld.%1ld %s(%d,%d) unknown action wg %d\n",
     time,t1, pcpsx_group_space(GROUP), GROUP, INSTANCE, WORKER); 
  } 
}

/***************** DUMP_TRACE_LIST_C *****************
* dump trace tracking info 
*
* Written September 2001 by Charles C Burch
******************************************************/
void pcpsx_dump_trace_list_c(CHARACTER *title, INTEGER *len_title) {

  char buff[512], supplement[512];
  if(pcpsx_init_c_sw==0) pcpsx_init_c();

  memcpy(buff, title, (*len_title));
  buff[(*len_title)]='\0';

  printf("%s\n",buff);
  printf("-Time- 1--- 2--- 3--- 4--- 5--- 6--- 7--- 8--- 9--- 10--\n");    

  lnklst_get_list_entry(pcpsx_trace_list, buff, supplement, -1);
  while(strlen(buff)>0) {
    pcpsx_trace_print(buff);
    lnklst_get_list_entry(pcpsx_trace_list, buff, supplement, -1);
  }

  if(trace_fl!=NULL) {
    fseek(trace_fl,0,SEEK_SET);
    while(fgets(buff,512,trace_fl)!=NULL) {
      pcpsx_trace_print(buff);
    }
    fclose(trace_fl);
    trace_fl=NULL;
    remove(PCPS_TRACKING_FILE);
  }
  return;
}

/***************** DUMP_TRACE_FILE_C *****************
* dump trace tracking info 
*
* Written September 2001 by Charles C Burch
******************************************************/
void pcpsx_dump_trace_file_c(CHARACTER *title, INTEGER *len_title) {

  char buff[512];
  if(pcpsx_init_c_sw==0) pcpsx_init_c();

  memcpy(buff, title, (*len_title));
  buff[(*len_title)]='\0';

  printf("%s\n",buff);
  printf("-Time- 1--- 2--- 3--- 4--- 5--- 6--- 7--- 8--- 9--- 10--\n");    

  if((trace_fl=fopen(PCPS_TRACKING_FILE,"r"))==NULL) {
    perror("Unable to open tracking file");
  } else {
    while(fgets(buff,512,trace_fl)!=NULL) {
      pcpsx_trace_print(buff);
    }
    fclose(trace_fl);
    trace_fl=NULL;
  }
  fflush(stdout);
  return;
}


/************* PUT_BUFFERED_INPUT_DATA ******************
* put buffered input data into linked list
* isw=0 means to insert at beginning of list, else at end
* 
* Written September 2001 by Charles C Burch
********************************************************/
void pcpsx_put_buffer_data_c(INTEGER *instance, INTEGER *trace_group, 
 INTEGER *ntr, DOUBLE *hdr, INTEGER *hdr_len, REAL *trc, INTEGER *trc_len, 
 INTEGER *work_group, INTEGER *isw, INTEGER *line_break){

  struct list_struct *ptr;
  int  n, n_bytes_hdr, n_bytes_trc;
  char *data_ptr;
  char mess[256];
  INTEGER len_mess;
  if(pcpsx_init_c_sw==0) pcpsx_init_c();

/* Get space new new list entry */
  if((ptr=(struct list_struct *)malloc(sizeof(struct list_struct)))==NULL) {
    sprintf(mess,"Unable to malloc list_struct in pcpsx_put_buffer_input_data");
    len_mess = strlen(mess);
    pcps_abort_c(mess,&len_mess);
  }

  ptr->instance   = (*instance);   /*insert scalar data into entry*/
  ptr->trace_group= (*trace_group);
  ptr->hdr_len    = (*hdr_len);
  ptr->trc_len    = (*trc_len);
  ptr->work_group = (*work_group);
  ptr->ntr        = (*ntr);             /*get size of array space*/
  ptr->line_break = (*line_break);

  if((n=ptr->ntr)<0) n=0;
  if(n>0) {
    n_bytes_trc=n*sizeof_float*(*trc_len);  /*allocate array space*/
    n_bytes_hdr=n*sizeof_double*(*hdr_len);
    if( (data_ptr=((char *) malloc(n_bytes_hdr+n_bytes_trc)) )==NULL) {
      sprintf(mess,"Unable to malloc str in pcpsx_put_buffer_data_c");
      len_mess = strlen(mess);
      pcps_abort_c(mess,&len_mess);
    }
    ptr->data=data_ptr;
    memcpy(data_ptr, (char*) hdr, n_bytes_hdr);             /*move hdr data*/
    memcpy(data_ptr+n_bytes_hdr, (char*) trc, n_bytes_trc); /*move trc data*/
  } else {
    ptr->data=NULL;          /*no array data*/
  }
  ptr->next=NULL;            /*pointer to next entry-Null since adding to end*/

/*** See if adding at beginning ***/
  if((*isw)==0 && do_par_input_buff[0]!=NULL) {
    ptr->next=do_par_input_buff[0];
    do_par_input_buff[0]=ptr;
    return;
  }
  
/*** Now add to end of link list ***/
  if(do_par_input_buff[0]==NULL) {
    do_par_input_buff[0]=ptr;             /*first entry*/
  } else {
    do_par_input_buff[1]->next=ptr;      /*have other entries*/
  }
  do_par_input_buff[1]=ptr;              /* points to last entry*/
  return;
}

/************* DUMP_BUFFERED_INPUT_DATA ***************
* print contents of buffered data into specified file
*
* Written September 2001 by Charles C Burch
******************************************************/
void pcpsx_dump_buffer_data_c(char *file,  INTEGER *len_file,
                              char *title, INTEGER *len_title){ 
  struct list_struct* ptr;
  FILE *fptr;
  char c_title[260], c_file[260];

  if(pcpsx_init_c_sw==0) pcpsx_init_c();

  memcpy(c_file, file, (*len_file));
  c_file[(*len_file)]='\0';

  memcpy(c_title, title, (*len_title));
  c_title[(*len_title)]='\0';

  remove(c_file);
  fptr=fopen(c_file,"w");

  if((ptr=do_par_input_buff[0])==NULL) {
    fprintf(fptr,"list_struct(%s) is empty\n",c_title);
    fclose(fptr);
    return;
  }

  fprintf(fptr,"Dump of list_struct(%s)\n",c_title);
  while(ptr!=NULL) {
    fprintf(fptr,
     "inst=%d, trc grp=%d, ntr=%d, hdr_len=%d, trc_len=%d, wrk grp=%d\n",
     ptr->instance, ptr->trace_group, ptr->ntr, ptr->hdr_len, ptr->trc_len,
     ptr->work_group); 
    ptr=ptr->next;
  }
  fclose(fptr);
  fflush(stdout);
  return;
}

/******************** GET_BUFFERED_INPUT_DATA *******************
* get buffered input data from linked list
* On entry,  entry<=0 mean scan from start of list
*               >0 get next entry
* On exit, entry is number of the list element, -1 if end of list              
*
* Written December 2002 by Charles C Burch
*****************************************************************/
void pcpsx_get_buffer_data_c(INTEGER *entry, INTEGER *instance, 
  INTEGER *trace_group, INTEGER *ntr, INTEGER *work_group){
  static int entry_num=0;
  static struct list_struct *get_input_buffer_ptr=NULL;
  
  if((*entry)<=0) {
     get_input_buffer_ptr=do_par_input_buff[0];
     entry_num=1;
  } else {   
    if(get_input_buffer_ptr!=NULL) {
      get_input_buffer_ptr=get_input_buffer_ptr->next;
      entry_num++;
    }
  }
  
  if(get_input_buffer_ptr==NULL) {
    (*entry)=-1;
    return;
  }

  (*instance)    = get_input_buffer_ptr->instance;
  (*trace_group) = get_input_buffer_ptr->trace_group;
  (*ntr)         = get_input_buffer_ptr->ntr;
  (*entry)       = entry_num; 
  (*work_group)  = get_input_buffer_ptr->work_group;
  return;
}
  
/************* EXTRACT_BUFFERED_INPUT *****************
* get "entry" buffered input data from linked list
*
* Written December 2002 by Charles C Burch
********************************************************/
void pcpsx_extract_buffer_data_c(INTEGER *entry, INTEGER *instance, 
  INTEGER *trace_group, INTEGER *ntr, 
  DOUBLE *hdr, INTEGER *hdr_len, REAL *trc, INTEGER *trc_len, 
  INTEGER *work_group, INTEGER *line_break){
  struct list_struct *ptr, *old_ptr;
  char *data_ptr;
  int n;
  
  if(pcpsx_init_c_sw==0) pcpsx_init_c();
 
  ptr=do_par_input_buff[0];    /*find desired entry*/
  old_ptr=NULL;
  n=1;
  while(ptr!=NULL) {
    if(n==(*entry)) break;
    n++;
    old_ptr=ptr;
    ptr=(ptr->next);
  }

  if(ptr==NULL) {
    (*instance)=-1;
    return;
  } 
  
  (*instance)    = ptr->instance;
  (*trace_group) = ptr->trace_group;
  (*ntr)         = ptr->ntr;
  (*hdr_len)     = ptr->hdr_len;
  (*trc_len)     = ptr->trc_len;
  (*work_group)  = ptr->work_group;
  (*line_break)  = ptr->line_break;
  
  if(ptr->ntr>0) {
    data_ptr=ptr->data;                   /*get hdr/trc*/
    n=(ptr->hdr_len)*(ptr->ntr)*sizeof_double;  /*size of hdr data*/
    memcpy((char*)hdr, data_ptr, n);
    data_ptr+=n;
    n=(ptr->trc_len)*(ptr->ntr)*sizeof_float;
    memcpy((char*) trc, data_ptr, n);     /*size of trc data*/
    free(ptr->data);                      /*free hdr/trc data area*/
  }
 /****************** now delete entry ********************/
  if(old_ptr==NULL) {
    do_par_input_buff[0]=ptr->next;                    /*first entry*/
    if(ptr->next==NULL) do_par_input_buff[1]=old_ptr;  /*last entry*/
  } else {
    if(ptr->next==NULL) do_par_input_buff[1]=old_ptr;  /*last entry*/
    old_ptr->next=ptr->next;
  }
  free(ptr);
  return;
}

/************ DELETE_ALL_BUFFERED_INPUT ****************
* delete all entries from linked list specified by root
*
* Written September 2001 by Charles C Burch
*******************************************************/
void pcpsx_delete_all_buffer_data_c() {
  struct list_struct *ptr, *ptr1;

  if(pcpsx_init_c_sw==0) pcpsx_init_c();

  ptr=do_par_input_buff[0];  /* start of linked list */ 

  while(ptr!=NULL) {
    ptr1=(ptr->next);  /*location of next entry */
    free(ptr->data);   /*deallocate data space */
    free(ptr);         /*delete list entry     */
    ptr=ptr1;          /*get ready for next entry*/
  }
  do_par_input_buff[0]=NULL;       /*zap roots*/
  do_par_input_buff[1]=NULL;
  return;
} 

/******************* PCPSX_INIT_C *********************
* General initialization
*
* Written September 2001 by Charles C Burch
******************************************************/
void pcpsx_init_c(){
  DOUBLE  double_var;
  REAL   float_var;
  INTEGER integer_var;

  if(pcpsx_init_c_sw!=0) return;

  pcpsx_init_c_sw=1;
  do_par_input_buff[0]=NULL;
  do_par_input_buff[1]=NULL;

  sizeof_integer=sizeof(integer_var);
  sizeof_double =sizeof(double_var);
  sizeof_float  =sizeof(float_var);

  pcpsx_trace_list[0]=NULL;
  pcpsx_trace_list[1]=NULL;

  do_par_input_buff[0]=NULL;
  do_par_input_buff[1]=NULL;

  return;
}
  
#ifdef __cplusplus
}
#endif

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
  
