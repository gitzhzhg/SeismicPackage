/*<CPS_v1 type="PRIMITIVE"/>
!----------------------------- lnklst_crou.c -------------------------------
!----------------------------- lnklst_crou.c -------------------------------
!----------------------------- lnklst_crou.c -------------------------------

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
! Name       : LNKLST_CROU
! Category   : miscellaneous
! Written    : 2002-09-23   by: Charles C. Burch
! Revised    : 2005-05-31   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Provides an interface into various link list functions
! References : These routines are called by pfio and are for general use
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! This is a set of functions to support first-in-first-out link lists of
!   character variables and their values.
!
! A special list called var_list is supportted by get_var and put_var which
! are for general use for first-in-first-out list of variable and their values.
!
! General purpose function are also included for special-purpose link lists.
! A lnklst linked list can be defined by using 
!    struct lnklst_struct *name[2]=LNKLST_INITIALIZER;
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!  The functions below can be used to perform operations on the linked list
!  called var_list.
!-------------------------------------------------------------------------------
!  void lnklst_get_var(char *var, char *value)
!  Extracts first occurrence of value of var in a specific linked list called
!  var_list and remove this occurrence from var_list
!  This in the get of a first in-first-out queue: see lnklst_put_var
!
!-------------------------------------------------------------------------------
!  void lnklst_put_var(char *var, char *value)
!  Insert var and value into a specific linked list called var_list
!  This in the put of a first in-first-out queue: see lnklst_get_var
!
!-------------------------------------------------------------------------------
!  void lnklst_delete_var_list()
!  Delete all entries from a specific linked list call var_list
!   
!-------------------------------------------------------------------------------
!  The functions below can be used to perform operations on a lnklst linked list
!-------------------------------------------------------------------------------
!  void lnklst_initialize_list(struct lnklst_struct *root[]) 
!    initializes the linked list specified by root
!
!-------------------------------------------------------------------------------
!  void lnklst_put_list_entry(struct lnklst_struct *root[], 
!    char *name, char *str)
!  Inserts a new entry at the end of the linked list specified by root
!  The new entry will have variable name "name" with value "str"
!-------------------------------------------------------------------------------
!  void lnklst_get_list_entry(struct lnklst_struct *root[], 
!     char *name, char *str, int entry) {
!  Get the given entry from linked list specified by root
!   and return pointers to the values into name and str
!  If entry<0 get first entry and delete it from list
!  If entry not found, return null name and str
!
!-------------------------------------------------------------------------------
!  void lnklst_replace_list_entry(struct lnklst_struct *root[], 
!     char *name, char *str, int entry) {
!  Replace the given entry from linked list specified by root
!
!-------------------------------------------------------------------------------
!  void lnklst_scan_list(struct lnklst_struct **ptr, char **name, char **str)
!  This is used to write funtions to process the elements of a linked list.
!  With ptr pointing to an entry in the linked list, this 
!    (1) returns a pointer to the values in the list (pointed to by ptr) 
!        into name and str.
!    (2) advances ptr to point to the next entry in the linked list
!  When there is no more data in the list, ptr, name and str wil be NULL
!
!  Note: normally this routine is used as follows:
!    struct lnklst_struct *root[2]
!    char *name, *str;
!
!    ptr=root[0];
!    lnklst_scan_list(&ptr, &name, &str);
!    while(name!=NULL){
!      do whatever is needed to process name and str
!      if(strcmp(name,name1)==0) return(entry);
!      lnklst_scan_list(&ptr, &name, &str);
!    }
!-------------------------------------------------------------------------------
!  int lnklst_search_list(struct lnklst_struct *root[2], char *name) 
!  This seraches for the first entry in the linked list specified by root
!  with the variable name "name" and returns the entry number.
!  If name is not in the list, -1 is returned.
!
!-------------------------------------------------------------------------------
!  void lnklst_delete_list_entry( struct lnklst_struct *root[], char *name) 
!  Deletes the first entry in the link list specified by root with variable
!  name "name"
!
!-------------------------------------------------------------------------------
!  void lnklst_delete_list(struct lnklst_struct *root[]) 
!  Delete all entries in the linked list specified by root
!
!-------------------------------------------------------------------------------
!  void lnklst_dump_list(struct lnklst_struct** root, char *title) 
!  Dump the contents on the linked list specified by root with a title "title"
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!     Date       Author      Description
!     ----       ------      -----------
!  2. 2005-05-31 Stoeckley   Fix so will compile with C++.
!  1. 2003-02-27 C. C. Burch Initial Version.
!
!  Note: These functions were originally written in March 2001 as part of pfio
!        and extracted as a separate module of code in July 2002.
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!
! no known problems.
!
!-------------------------------------------------------------------------------
!</portability_doc>
*/

/*
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
!--------------------------- start of coding ------------------------------
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "unix.h"
#include "lnklst.h"

char *lnklst_ident =
"$Id: lnklst_crou.c,v 1.2 2005/05/31 13:04:09 Stoeckley prod sps $";

/************************************************************
  For reference lnklst_struct is defined in lnklst.h as

  struct   lnklst_struct {
    char   *name;
    char   *str;
    struct lnklst_struct *next;
  };
*************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

/* the list below called var_list is used by get_var and put_var */
struct lnklst_struct *var_list[2]=LNKLST_INITIALIZER;

/*************** LNKLST_PUT_VAR **********************
* insert var and value into var_list
*
* Written April 2001 by Charles C Burch
***************************************************/
void lnklst_put_var_list(char *var, char *value) {
  lnklst_put_list_entry(var_list, var,value);
  return;
}

/******************* LNKLST_GET_VAR *********************
* Extract value for first occurence of var in var_list
* and delete it
*
* Written April 2001 by Charles C Burch
*******************************************************/
void lnklst_get_var_list(char *var, char *value) {
  struct lnklst_struct *ptr, *old_ptr;

  ptr=var_list[0];
  old_ptr=NULL;
  while(ptr!=NULL) {
    if(strcmp(ptr->name,var)==0) {
    /* have a match */
      strcpy(value,ptr->str);

      if(old_ptr==NULL) {
        var_list[0]=ptr->next;                    /*first entry*/
      } else {
        if(ptr->next==NULL) var_list[1]=old_ptr;  /*last entry*/
        old_ptr->next=ptr->next;
      }

      free(ptr->name);
      free(ptr);
      return;
    }

    old_ptr=ptr;
    ptr=ptr->next;
  }

  strcpy(value,"");                               /*var not found*/
  return;
}
/****************** LNKLST_DELETE_VAR_LIST ******************
* delete entries in var_list
*
* Written July 2002 by Charles C Burch
************************************************************/
void lnklst_delete_var_list(){
  lnklst_delete_list(var_list);
  return;
}

/****************** LNKLST_INITIALIZE_LIST ********************
* Initialize a linked list specified by root
*
* Written March 2001 by Charles C Burch
***************************************************************/
void lnklst_initialize_list(struct lnklst_struct *root[]) {
  root[0]=NULL;
  root[1]=NULL;
  return;
}

/****************** LNKLST_PUT_LIST_ENTRY ********************
* put new entry(name/str) into linked list specified by root)
*
* Written March 2001 by Charles C Burch
*************************************************************/
void lnklst_put_list_entry(struct lnklst_struct *root[], char *name, char *str){
  struct lnklst_struct *ptr;
  int n;

  if((ptr=(struct lnklst_struct *)malloc(sizeof(struct lnklst_struct)))==NULL) {
    unix_abort_c("Unable to malloc new lnklst_struct in lnklst_put_list_entry");
  }

  n=strlen(name);
  if( ((ptr->name)=((char *) malloc(n+strlen(str)+2)) )==NULL) {
    unix_abort_c("Unable to malloc str in lnklst_put_list_entry");
  }

  (ptr->str)=(ptr->name)+n+1;
  strcpy(ptr->name,name);
  strcpy(ptr->str,str);
  ptr->next=NULL;

  if(root[0]==NULL) {
    root[0]=ptr;        /*first entry*/
  } else {
    root[1]->next=ptr;  /*have other entries*/
  }
  root[1]=ptr;
  return;
}

/************* LNKLST_GET_LIST_ENTRY *********************
* get the given entry from linked list specified by root
*   and place results into name/str
* if entry<0 get first entry and delete it from list
* if entry not found, return null name and str
*
* Written March 2001 by Charles C Burch
*******************************************************/
void lnklst_get_list_entry(struct lnklst_struct *root[], char * name, char *str,
 int entry) {
  struct lnklst_struct *ptr;
  int del_sw, i;

  if(entry<0) {
    del_sw=1;
    entry=1;
  } else {
    del_sw=0;
  }

  ptr=root[0];    /*find desired entry*/
  i=1;
  while(ptr!=NULL && i!=entry) {
    ptr=(ptr->next);
    i++;
  }

  if(ptr==NULL) {
    strcpy(name,"");
    strcpy(str,"");
  } else {
    strcpy(name,ptr->name);
    strcpy(str,ptr->str);
    if(del_sw==1) {
      root[0]=(ptr->next);
      free(ptr->name);
      free(ptr);
    }
  }

  return;
}

/************* LNKLST_REPLACE_LIST_ENTRY *********************
* replace the given entry from linked list specified by root
*
* Written August 2002 by Charles C Burch
*******************************************************/
void lnklst_replace_list_entry(struct lnklst_struct *root[], char *name, 
    char *str, int entry) {
  struct lnklst_struct *ptr;
  int  i, len_name, len_str;

  if(entry<0) return;

  ptr=root[0];    /*find desired entry*/
  i=1;
  while(ptr!=NULL && i!=entry) {
    ptr=(ptr->next);
    i++;
  }

  if(ptr==NULL) return;
  
  len_name=strlen(name);
  len_str=strlen(str);
  if(len_name<=strlen(ptr->name) && len_str<=strlen(ptr->str)) {
    strcpy(ptr->name,name);
    strcpy(ptr->str,str);
  } else {
    free(ptr->name);
    if( ((ptr->name)=((char *) malloc(len_name+len_str+2)) )==NULL) {
      unix_abort_c("Unable to malloc str in lnklst_replace_list_entry");
    }

    (ptr->str)=(ptr->name)+len_name+1;
    strcpy(ptr->name,name);
    strcpy(ptr->str,str);
  }

  return;
}

/************** LNKLST_SCAN_LIST *************************
* scan  entry from linked list specified by ptr,
*   place results into name/str, and advance ptr
*
* Written March 2001 by Charles C Burch
*******************************************************/
void lnklst_scan_list(struct lnklst_struct **ptr, char **name, char **str) {

  if((*ptr)==NULL) {
    (*name)=NULL;
    (*str) =NULL;
  } else {
    (*name)=(*ptr)->name;
    (*str) =(*ptr)->str;
    (*ptr) =(*ptr)->next;
  }
  return;
}

/***************** LNKLST_SEARCH_LIST *************************
* searches for (name1) in linked list specified by root
*  returns entry number if found, -1 if not
*
* Written March 2001 by Charles C Burch
************************************************************/
int lnklst_search_list(struct lnklst_struct *root[2], char *name1) {
  struct lnklst_struct *ptr;
  char *name, *str;
  int entry;

  entry=1;
  ptr=root[0];
  lnklst_scan_list(&ptr, &name, &str);
  while(name!=NULL){
    if(strcmp(name,name1)==0) return(entry);
    lnklst_scan_list(&ptr, &name, &str);
    entry++;
  }
  return(-1);
}

/*************** LNKLST_DELETE_LIST_ENTRY ****************
* delete entry(name) from linked list specified by root
*
* Written March 2001 by Charles C Burch
*******************************************************/
void lnklst_delete_list_entry( struct lnklst_struct *root[], char *name) {
  struct lnklst_struct *ptr, *old_ptr;

  ptr=root[0];
  old_ptr=NULL;
  while(ptr!=NULL) {
    if(strcmp(ptr->name,name)==0) {
/* have a match-delete this entry */
      if(old_ptr==NULL) {
        root[0]=ptr->next;                    /*first entry*/
      } else {
        if(ptr->next==NULL) root[1]=old_ptr;  /*last entry*/
        old_ptr->next=ptr->next;
      }
      free(ptr->name);
      free(ptr);
      return;
    }
    old_ptr=ptr;
    ptr=ptr->next;
  }
  return;
}

/*************** LNKLST_DELETE_LIST **********************
* delete all entries from linked list specified by root
*
* Written March 2001 by Charles C Burch
*******************************************************/
void lnklst_delete_list(struct lnklst_struct *root[]) {
  struct lnklst_struct *ptr, *ptr1;

  ptr=root[0];    /*find desired entry*/

  while(ptr!=NULL) {
    ptr1=(ptr->next);
    free(ptr->name);
    free(ptr);
    ptr=ptr1;
  }
  root[0]=NULL;
  root[1]=NULL;
  return;
}

/************** LNKLST_DUMP_LIST *************************
* print contents of linked list specified by root
*
* Written March 2001 by Charles C Burch
*******************************************************/
void lnklst_dump_list(struct lnklst_struct** root, char *title) {
  struct lnklst_struct* ptr;

  if((ptr=(*root))==NULL) {
    printf("lnklst_struct(%s) is empty\n\n",title);
    return;
  }

  printf("Dump of lnklst_struct(%s)\n",title);
  while(ptr!=NULL) {
    printf("%s|%s\n",ptr->name, ptr->str);
    ptr=(struct lnklst_struct*)ptr->next;
  }
  printf("\n");
  return;
}

#ifdef __cplusplus
}
#endif

/****************************** Basic test driver ***************************
#include <stdlib.h>
#include <stdio.h>

#include "lnklst.h"

void form_list(struct lnklst_struct *list[]) {
  lnklst_initialize(list);
  lnklst_put_list_entry(list,"name1", "strg1");
  lnklst_put_list_entry(list,"name2", "strg2");
  lnklst_put_list_entry(list,"name3", "strg3");
  lnklst_put_list_entry(list,"name4", "strg4");
  return;
}

int main() {
  char name[260], str[260];
  int i;

  struct lnklst_struct *list[2]={NULL,NULL};

  lnklst_dump_list(list,"no entries"); 
  lnklst_put_list_entry(list,"name1", "strg1");
  lnklst_dump_list(list,"1 entry");
  lnklst_put_list_entry(list,"name2", "strg2");
  lnklst_dump_list(list,"2 entries");
  lnklst_put_list_entry(list,"name3", "strg3");
  lnklst_dump_list(list,"3 entries");

  for(i=1;i<=4;i++) {
    lnklst_get_list_entry(list,name,str,i);
    printf("i=%d, name=%s, str=%s\n",i,name,str);
  }

  for(i=1;i<=4;i++) {
    lnklst_get_list_entry(list,name,str,-1);
    printf("i=%d, name=%s, str=%s\n",i,name,str);
  }

  form_list(list);
  lnklst_dump_list(list,"formed list");

  printf("search for name1=%d\n",lnklst_search_list(list,"name1"));
  printf("search for name2=%d\n",lnklst_search_list(list,"name2"));
  printf("search for name3=%d\n",lnklst_search_list(list,"name3"));
  printf("search for name4=%d\n",lnklst_search_list(list,"name4"));
  printf("search for name0=%d\n",lnklst_search_list(list,"name0"));

  lnklst_delete_list_entry(list,"name2");
  lnklst_dump_list(list,"post delete 2");
  lnklst_delete_list_entry(list,"name4");
  lnklst_dump_list(list,"post delete 2-4");
  lnklst_delete_list_entry(list,"name1");
  lnklst_dump_list(list,"post delete 1-2-4");
  lnklst_delete_list_entry(list,"name3");
  lnklst_dump_list(list,"post delete 1-2-3-4");

  form_list(list);
  lnklst_delete_list(list);
  lnklst_dump_list(list,"formed list after delete");

  lnklst_put_var_list("var1","value1");
  lnklst_put_var_list("var1","value2");
  lnklst_get_var_list("var1",str);
  printf("var1=%s\n",str);
  lnklst_get_var_list("var1",str);
  printf("var1=%s\n",str);

  lnklst_delete_var_list();

  return(0);
}
****************************************************************************/

/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
