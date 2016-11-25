/*<CPS_v1 type="PRIMITIVE">
!----------------------------- memman_crou.c -------------------------------
!----------------------------- memman_crou.c -------------------------------
!----------------------------- memman_crou.c -------------------------------

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
! Name       : MEMMAN_CROU
! Category   : memory
! Written    : 2002-09-24   by: Charles C. Burch
! Revised    : 2002-09-24   by: Charles C. Burch
! Maturity   : production   2003-06-17
! Purpose    : MEMory MANager functions written in C
! References : These routines are used by memman.f90 and for general use
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
! These routines provide access certain to memory management functions
!
!  They provide a tracking version of the c functions malloc, free
!  and realloc to help detect memory leaks.
!
!  Also provided are routines to allow the Fortran memman all and del functions
!  to use the tracking features.
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
! INTEGER memman_loc_X_c(void *var)                              use:Fortran
!   returns the address of Fortran variable, like %loc(var) in F77 or &var in C
!   X can be char, integer, real, complex, double, logic
!-------------------------------------------------------------------------------
! INTEGER memman_ptrloc_XY_c(void *var)                              use:Fortran
!   returns the address of Fortran pointer variable
!   X can be c(character), i(integer), r(real), z(complex), d(double), l(logic)
!   Y can 1-5 for number of dimensions
!   These routines are used to overloading memman_pointer_loc in memman.f90
!-------------------------------------------------------------------------------
! int memman_malloc(void** v_loc, long v_size, char *v_name)              use:C
!   does a malloc(v_size) with error checking and place result in *vloc.
!   keeps track of previous malloc for *vloc and uses v_name as reference 
!   for deubugging
!   return 0 if no eror and <0 if an error is found
!
! Note: variables allocated with memman_malloc must be freed using memman_free
!       and reallocated with memman_realloc
!-------------------------------------------------------------------------------
! int memman_free(void** v_loc, char* v_name)                   use:C
*   A "free" look alike with error checking, tracking and a variable name
!   return 0 if no eror and <0 if an error is found
! Note: Only variables allocated with memman_malloc should be freed 
!       using memman_free
!-------------------------------------------------------------------------------
! int memman_realloc(void** v_loc, long v_size, char* v_name)  use:C
!   A "realloc" look alike with error checking, tracking and a variable name
!   return 0 if no eror and <0 if an error is found
! Note: variables allocated with memman_malloc must be freed using memman_free
!       and reallocated with memman_realloc
!-------------------------------------------------------------------------------
! INTEGER memman_tracking_del_c(INTEGER *p_loc, INTEGER* v_loc, INTEGER *v_size,
    char *v_name)
! use Fortran and C with memman functions.
!   Delete p_loc from tracking list and determines if  v_loc
!   has been previously allocated and its name and size are consistent
!   Returns 0 if it has been allocated and size/name same as old
!           1 if it has not been allocated
!           2 if it has been allocated, but name or size different
!-------------------------------------------------------------------------------
! INTEGER memman_tracking_all_c(INTEGER *p_loc, INTEGER* v_loc, INTEGER *v_size,
    char *v_name)
! use Fortran and C with memman functions.
!   Insert p_loc into tracking list.
!   Returns 0 if it v_loc has not been previously allocated 
!           1 if it has been previously allocated
!-------------------------------------------------------------------------------
! INTEGER memman_tracking_free_c(INTEGER *p_loc)
! use Fortran and C with memman functions.
!   deletes p_loc from tracking list.
!   Returns 0 if it v_loc had been previously allocated 
!           1 if it has not been previously allocated
!-------------------------------------------------------------------------------
! void memman_dump_tracking_c(char *title)
! lists current content of tracking list
!-------------------------------------------------------------------------------
! void memman_exit_tracking_c(INTEGER* isw)
! lists current content of tracking list and empties it
! if isw==0 do not print message if there are no allocated variables
!-------------------------------------------------------------------------------
! INTEGER memman_get_memory_allocated_c() 
! Return amount memory currently allocated
!-------------------------------------------------------------------------------
!  memman_get_info_c(INTEGER* p_loc, INTEGER *v_loc, INTEGER*size, char*name)
! gets any associated variables with p_loc
!-------------------------------------------------------------------------------
! void memman_change_info_c(INTEGER* p_loc, INTEGER *v_loc,  
    INTEGER *v_size, char *v_name)
! renames associated vname with p_loc, if present, or insert it
!-------------------------------------------------------------------------------
! void memman_set_print_mode_c(INTEGER *sw)
! Purpose: set print control 0: no printing, 1 errors, 2 diagnostics
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY
!     Date        Author      Description
!     ----        ------      -----------
!  1. 2003-06-17  C C Burch   Initial Version.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "c2f_interface.h"
#include "memman.h"
#include "lnklst.h"
#include "unix.h"

char *memman_crou_ident =
"$Id: memman_crou.c,v 1.1 2003/06/16 14:25:20 Burch prod sps $";

long memman_allocated=0;
struct lnklst_struct *memman_al[2]=LNKLST_INITIALIZER; /* allocated list */

unsigned char  bad_data[8]={0xdb,0xad,0xda,0xbd,0xda,0xbd,0xdb,0xad};
int print_sw=1;

/*--------------------------- start of functions ----------------------------*/
/*--------------------------- start of functions ----------------------------*/
/*--------------------------- start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/*************************************************************
* sets print control: 0 no printing, 1 errors, 2 diagnostics
*
* Written July 2002 by Charles C Burch
************************************************************/
  void memman_set_print_mode_c(INTEGER *sw) {
    print_sw=(*sw);
    if(print_sw<0) print_sw=0;
    if(print_sw>2) print_sw=2;
    return;
  }

/*************************************************************
* returns the address of Fortran variable
* These routines are all the same, but have differnet names
* to force F90 to pass the correct argument
*
* Written July 2002 by Charles C Burch
************************************************************/
  INTEGER memman_loc_char_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_loc_integer_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_loc_real_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_loc_double_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_loc_complex_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_loc_logical_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

/*************************************************************
* returns the address of Fortran pointer variable
* These routines are all the same, but have differnet names
* to force F90 to pass the correct argument
*
* Written July 2002 by Charles C Burch
************************************************************/
  INTEGER memman_ptrloc_c1_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_c2_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_c3_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_c4_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_c5_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_i1_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_i2_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_i3_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_i4_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_i5_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_r1_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_r2_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_r3_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_r4_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_r5_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_d1_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_d2_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_d3_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_d4_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_d5_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_z1_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_z2_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_z3_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_z4_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_z5_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_l1_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_l2_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_l3_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_l4_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

  INTEGER memman_ptrloc_l5_c(void *var) {
    if(var==NULL) return((INTEGER) 0);
    return((INTEGER) var);
  }

/************************************************************************
* Utility routine to form name of a memman variable
*
* Written July 2002 by Charles C Burch
************************************************************************/
  void memman_form_v_name(char *a, long p, long v, long size, char *v_name) {
    sprintf(a,"%ld:%ld-%ld-%s",p,v,size,v_name);
    return;
  } 

/************************************************************************
* nullify a c variable
*
* Written July 2002 by Charles C Burch
************************************************************************/
  void memman_null(void **v, char *v_name) {
    INTEGER zero=0, p_loc;
    
    if(v==NULL) unix_abort_c("NULL v argument in memman_null");
    (*v)=NULL;
    p_loc=(INTEGER) v;
    memman_tracking_all_c(&p_loc, &zero, &zero, v_name);
    return;
  }
  
/************************************************************************
* A "malloc" look alike with error checking, tracking and a variable name
*
* Written July 2002 by Charles C Burch
************************************************************************/
  int memman_malloc(void **v, long v_size, char *v_name) {
    void *result;
    INTEGER size, p_loc, v_loc;
    char a_var[160];
  
    if(v==NULL) unix_abort_c("NULL v argument in memman_malloc");
    if((*v)!=NULL) {
      if(print_sw>=2) {
        memman_form_v_name(a_var,(long) v, (long)(*v), v_size, v_name); 
        printf("Warning: memman_malloc(%s) previous allocation being freed\n",
          a_var);
        fflush(stdout);
      }
      memman_free(v, v_name);
    }
    if(v_size<0) v_size=0;
    if(v_size==0) {
      return(memman_free(v,v_name));
    }
    
    size=v_size+16;
    if((result=malloc(size))==NULL) {
      memman_form_v_name(a_var,(long)v,(long)(*v),v_size, v_name);
      printf("Unable to memman_alloc(%s)\n",a_var);
      fflush(stdout);
      unix_abort_c("memman_alloc error");
    }
   
    /** printf("malloc result=%ld\n",(long) result);  **/
    p_loc=(INTEGER) v;
    v_loc=(INTEGER) result;
    memman_tracking_all_c(&p_loc, &v_loc, &size, v_name);
    memcpy((char*)result,          (char*) bad_data, 8);
    memcpy(((char*)result)+size-8, (char*) bad_data, 8);
    (*v)=((char*)result)+8;
    return(0);
  }

/************************************************************************
* A "free" look alike with error checking, tracking and a variable name
*
* Written July 2002 by Charles C Burch
************************************************************************/
  int memman_free(void** v, char* v_name) {
    INTEGER size, v_loc, p_loc;
    int stat;
    char vname[80], *c_ptr, a_var[160], b_var[160];

    if(v==NULL) unix_abort_c("NULL v argument in memman_free");
    if((*v)==NULL) return(0);

    p_loc=(INTEGER) v;
    memman_get_info_c(&p_loc, &v_loc, &size, vname);
    if(size<0) {
      memman_form_v_name(a_var, (long)v, (long)(*v),0,v_name);
      printf(
       "Error: memman_free variable(%s) not allocated with memman_malloc\n",
       a_var);
      fflush(stdout);
      return(-1);
    }

    if(size==0) {
      (*v)=NULL;
      return(0);
    }
  
    c_ptr=(char*) v_loc;
    if((v_loc+8)!=(long)(*v)) {
      memman_form_v_name(a_var, (long)v, (long)(*v),0,v_name);
      memman_form_v_name(b_var, (long)v, v_loc+8, size, vname);
      printf(
      "Error:memman_free variable(%s) conflict with memman data(%s)\n",
        a_var, b_var);
      fflush(stdout);
      unix_abort_c("memman_free error");
    }
    stat=0;
    if(memcmp(c_ptr, (char*)bad_data, 8)!=0) {
      if(print_sw>=1) {
        memman_form_v_name(b_var, (long)v, v_loc, size, vname);
        printf("Error:memman_free data corruption at beginning of (%s)\n",
         b_var);
        fflush(stdout);
      }
      stat=-1;
    }
    if(memcmp(c_ptr+size-8, (char*)bad_data, 8)!=0) {
      if(print_sw>=1) {
        memman_form_v_name(b_var, (long)v, v_loc, size, vname);
        printf("Error:memman_free data corruption at ending of (%s)\n",
         b_var);
        fflush(stdout);
      }
      stat=-1;
    }
    
    /**  printf("free=%ld\n",(long) c_ptr);  **/
    free(c_ptr);
    (*v)=NULL;
    memman_tracking_del_c(&p_loc, &v_loc, &size, vname);
    return(stat);
  }  
  
/**************************************************************************
* A "realloc" look alike with error checking, tracking and a variable name
*
* Written July 2002 by Charles C Burch
**************************************************************************/
  int memman_realloc(void** v, long v_size, char* v_name) {
    void *result;
    INTEGER size, p_loc, v_loc;
    int stat;
    char vname[80], a_var[160], b_var[160], *c_ptr;
 
    if(v==NULL) unix_abort_c("NULL v argument in memman_realloc");
    
    if((*v)==NULL) {
      return(memman_malloc(v, v_size, v_name));
    }
 
    if(v_size<0) v_size=0;
    if(v_size==0) {
      return(memman_free(v,v_name));
    }
    
    p_loc=(INTEGER) v;
    memman_get_info_c(&p_loc, &v_loc, &size, vname);
    if(size<0) {
      memman_form_v_name(a_var, (long)p_loc, (long)*v, 0, v_name);
      printf(
       "Error: memman_realloc variable(%s) not allocated with memman_alloc\n",
       a_var);
      fflush(stdout);
      unix_abort_c("memman_realloc error");
    }

    c_ptr=(char*) v_loc;
    if((v_loc+8)!=(long)(*v)) {
      memman_form_v_name(a_var, (long)v, (long)(*v),v_size,v_name);
      memman_form_v_name(b_var, (long)v, v_loc, size, vname);
      printf(
      "Error:memman_realloc variable(%s) conflict with memman data(%s)\n",
        a_var, b_var);
      fflush(stdout);
      unix_abort_c("memman_realloc error");
    }
    
    stat=0;
    if(memcmp(c_ptr, (char*)bad_data, 8)!=0) {
      if(print_sw>=1) {
        memman_form_v_name(b_var, (long)v, v_loc, size, vname);
        printf("Error:memman_realloc data corruption at beginning of (%s)\n",
         b_var);
        fflush(stdout);
      }
      stat=-1;
    }
    if(memcmp(c_ptr+size-8, (char*)bad_data, 8)!=0) {
      if(print_sw>=1) {
        memman_form_v_name(b_var, (long)v, v_loc, size, vname);
        printf("Error:memman_realloc data corruption at ending of (%s)\n",
         b_var);
        fflush(stdout);
      }
      stat=-1;
    }

    memman_tracking_del_c(&p_loc, &v_loc, &size, vname);
  
    size=v_size+16;
    /** printf("realloc=%ld, s=%ld\n",(long) c_ptr, (long)size);  **/

    if((result=realloc((void*)c_ptr, size))==NULL) {
      memman_form_v_name(b_var, (long)v, v_loc, v_size, v_name);
      sprintf(a_var,"Unable to memman_realloc(%s)\n",b_var);
      unix_abort_c(a_var);
    }

    v_loc=(INTEGER) result;
    memman_tracking_all_c(&p_loc, &v_loc, &size, v_name);

    memcpy((char*)result,          (char*) bad_data, 8);
    memcpy(((char*)result)+size-8, (char*) bad_data, 8);
    (*v)=((char*)result)+8;

    return(stat);
  }

/*******************************************************************
 Delete p_loc from tracking list and determines if  v_loc
* has been previously allocated and its name and size are consistent
*
* Returns 0 if it has been allocated and size/name same as old
*         1 if it has not been allocated
*         2 if it has been allocated, but name or size different
*
* Written July 2002 by Charles C Burch
********************************************************************/
  INTEGER memman_tracking_del_c(INTEGER *p_loc, INTEGER *v_loc, INTEGER *v_size,
      char *v_name) {
    int tl_entry, ierr;
    long tl_size, loc;
    char tl_loc[20], tl_name[260], str[260], a_var[160], b_var[160];

    /** printf("memman_tracking_del:, loc=%ld, name=%s, size=%ld\n",
        (long)(*v_loc), v_name, (long)(*v_size));  **/
   
    while((*v_name)==' ') v_name++;  /*skip beginning blanks*/
    if(v_name[0]=='\0') v_name="-"; 
    
    sprintf(tl_loc,"%ld",(long)(*p_loc));

    if((tl_entry=lnklst_search_list(memman_al, tl_loc))<0) {
      /* v_loc not present */
      memman_form_v_name(str,*p_loc, *v_loc, *v_size,v_name);
      if(print_sw>=2) {
        printf("%s%s%s\n", "Warning: memman loc(",str,
        ") requested to be deallocated, but has not been previously allocated");
        fflush(stdout);
      }
      return(1);
    } 
    
    /* v_loc present */
    lnklst_get_list_entry(memman_al, tl_loc, str,tl_entry);
    sscanf(str,"%ld %ld %s",&loc, &tl_size, tl_name);
    memman_allocated-=tl_size;
    
    sprintf(str,"0 0 %s",v_name);   /*nullify variable*/
    lnklst_replace_list_entry(memman_al, tl_loc, str, tl_entry);

    ierr=0;
    if(tl_size>=0 && (*v_size)>=0) {
      if(tl_size!=(*v_size)) ierr=1;
    }
    
    if(loc>0 && (*v_loc)>0) {
      if(loc!=(*v_loc)) ierr=1;
    }  

    if(!ierr) {
      if(strcmp(v_name,"-")!=0 && strcmp(tl_name,"-")!=0) {
        if(strcmp(v_name,tl_name)!=0) ierr=1;
      }          
    }          

    if(ierr) {        
      memman_form_v_name(a_var,*p_loc, *v_loc, *v_size,v_name);
      memman_form_v_name(b_var,*p_loc, loc, tl_size,tl_name);
      if(print_sw>=2) {
        printf("%s%s%s%s\n", "Warning: memman loc(",a_var,
          ") requested to be deallocated, but was allocated as ", b_var);
        fflush(stdout);
      }
      return(2);
    }

    return(0);
  }  
    
/****************************************************************
* Insert p_loc into tracking list.
*
* Returns 0 if p_loc was previously nullified or being nullified 
*         1 if p_loc was previously allocated
*         2 if p_loc is zero
*         3 if p_loc was not previously allocated 
* 
* Written July 2002 by Charles C Burch
***************************************************************/
  INTEGER memman_tracking_all_c(INTEGER *p_loc, INTEGER *v_loc, INTEGER *v_size,
      char *v_name) {
    int  status, tl_entry;
    long tl_size, vloc;
    char tl_name[260], str[260], tl_loc[20], a_var[160], b_var[160];

    /** printf("memman_tracking_all:, ploc=%ld, vloc=%ld, size=%ld, name=%s\n",
        (long)(*p_loc), (long)(*v_loc), (long)(*v_size), v_name);  **/

    if((*p_loc)==0) return(2);
    while((*v_name)==' ') v_name++;  /*skip beginning blanks*/
    if(v_name[0]=='\0') v_name="-"; 

    sprintf(tl_loc,"%ld",(long)(*p_loc));

    if((tl_entry=lnklst_search_list(memman_al, tl_loc))>=0) {
      /* v_loc present */
      lnklst_get_list_entry(memman_al, tl_loc, str,tl_entry);
      sscanf(str,"%ld %ld %s",&vloc, &tl_size, tl_name);
      memman_allocated-=tl_size;
      
      if((*v_size)>0 && tl_size>0) {
        if(print_sw>=2) {
          memman_form_v_name(a_var,*p_loc, *v_loc, *v_size,v_name);
          memman_form_v_name(b_var,*p_loc, vloc, tl_size,tl_name);
          printf("Warning: memman_alloc(%s) already allocated as(%s)\n", 
           a_var, b_var);
          fflush(stdout);
        }
        status=1;
      } else {
        status=0;
      }        
      sprintf(str,"%ld %ld %s",(long)(*v_loc), (long)(*v_size), v_name);
      lnklst_replace_list_entry(memman_al, tl_loc, str, tl_entry);
    } else {
      if((*v_size)>0) {
        if(print_sw>=2) {
          memman_form_v_name(str, (long)(*p_loc), (long) (*v_loc), 
            (long) (*v_size), v_name);
          printf("%s%s%s\n", 
            "Warning: memman_allocate variable(",str,
            ") not registered as nullified");
          fflush(stdout);
        }
      }        
      status=3;
      sprintf(str,"%ld %ld %s",(long) (*v_loc),(long)(*v_size), v_name);
      lnklst_put_list_entry(memman_al, tl_loc, str);
    }
    
    memman_allocated+=(*v_size);
    return(status);
  }

/*******************************************************************
 Delete p_loc from tracking list 
*
* Written July 2002 by Charles C Burch
********************************************************************/
  void memman_tracking_free_c(INTEGER *p_loc) {
    char tl_loc[20];

    /** printf("memman_tracking_free:, loc=%ld\n", (long)(*v_loc));  **/
   
    sprintf(tl_loc,"%ld",(long)(*p_loc));
    lnklst_delete_list_entry(memman_al, tl_loc);
    return;
  }  
    
/**************************************************************************
* get v_loc/size/name associated with allocated variable at location p_loc
*
* Written July 2002 by Charles C Burch
**************************************************************************/
 void memman_get_info_c(INTEGER *p_loc, INTEGER *v_loc, INTEGER *v_size,
     char *v_name) {
    int tl_entry;
    long tl_size, vloc;
    char tl_loc[20], str[260];

    if((*p_loc)==0) {
      tl_entry=-1;
    } else {  
      sprintf(tl_loc,"%ld",(long)(*p_loc));
      tl_entry=lnklst_search_list(memman_al, tl_loc);
    }
    if(tl_entry<0) {
      /* p_loc not present or p_loc is 0 */
      (*v_size)=-1;
      (*v_loc)=0;
      strcpy(v_name,"");
    } else { 
      /* p_loc present */
      lnklst_get_list_entry(memman_al, tl_loc, str,tl_entry);
      sscanf(str,"%ld %ld %s",&vloc, &tl_size, v_name);
      (*v_loc)=vloc;
      (*v_size)=tl_size;
    }
    return;
  } 
  
/**************************************************************************
* change vname associated with allocated variable at location v_loc
* if v_loc not present, insert it
*
* Written July 2002 by Charles C Burch
**************************************************************************/
  void memman_change_info_c(INTEGER *p_loc, INTEGER *v_loc, INTEGER *v_size,
      char *v_name) {
    int tl_entry;
    long tl_size, vloc;
    char tl_loc[80], tl_name[260], str[260];

    if((*p_loc)==0) return;
    
    sprintf(tl_loc,"%ld",(long)(*p_loc));
    tl_entry=lnklst_search_list(memman_al, tl_loc);
    
    if(tl_entry>=0) {
      /* tl_loc present */
      lnklst_get_list_entry(memman_al, tl_loc, str,tl_entry);
      sscanf(str,"%ld %ld %s",&vloc, &tl_size, tl_name);
      if(tl_size==(*v_size) && vloc==(*v_loc) && strcmp(v_name,tl_name)==0) 
        return; /*same*/
      
      memman_allocated-=tl_size;
      sprintf(str,"%ld %ld %s",(long)(*v_loc),(long)(*v_size),v_name);  
      lnklst_replace_list_entry(memman_al, tl_loc, str,tl_entry);
    } else { 
      sprintf(str,"%ld %ld %s",(long)(*v_loc),(long)(*v_size),v_name);  
        /*insert new entry*/
      lnklst_put_list_entry(memman_al, tl_loc, str);
    }  
    memman_allocated+=(*v_size);
    return;
  } 
  
/*******************************************************************
* Checks to see if info on specified variable is correct
*
* Returns 0 if variable has been allocated and name/size consistent
*         1 if it has not been allocated
*         2 if allocated but name or size different
* 
* Written July 2002 by Charles C Burch
********************************************************************/
  INTEGER memman_check_variable_c(INTEGER *p_loc, INTEGER *v_loc,
      INTEGER *v_size, char *v_name) {
    int  status, tl_entry;
    long tl_size, vloc;
    char tl_name[260], str[260], tl_loc[20], a_var[160], b_var[160];

    /** printf("memman_check_variable:, loc=%ld, name=%s, size=%ld\n",
        (long)(*v_loc), v_name, (long)(*v_size));  **/
    
    if(v_name[0]=='\0') v_name="-"; 
    sprintf(tl_loc,"%ld",(long)(*p_loc));
    memman_form_v_name(a_var, (long)(*p_loc), (long)(*v_loc), (long)(*v_size),
        v_name);

    if((tl_entry=lnklst_search_list(memman_al, tl_loc))>=0) {
      /* p_loc present */
      lnklst_get_list_entry(memman_al, tl_loc, str,tl_entry);
      sscanf(str,"%ld %ld %s", &vloc, &tl_size, tl_name);
      status=0;
      if((*v_size)>=0 && tl_size>=0) {
        if((*v_size)!=tl_size) status=2;
      }        
      if(strcmp(v_name,"-")!=0 && strcmp(tl_name,"-")!=0) {        
        if(strcmp(v_name, tl_name)!=0) status=2;
      }
      if((*v_loc)>0 && vloc>0) {
        if((*v_loc)!=vloc) status=2;
      }
      if(status!=0) {
        if(print_sw>=2) {
          memman_form_v_name(b_var, (long)(*p_loc), vloc, tl_size,tl_name);
          printf("%s%s%s%s%s%s%s\n",
            "Warning: memman_check(",tl_loc,") thought to be allocated as(",
            a_var,") but allocated as (",b_var,")");
          fflush(stdout);
        }
      }
    } else {
      status=1;
      if(print_sw>=2) {
        printf("Warning memman_check(%s) not allocated\n",a_var);
      }
    }
    
    return(status);
  }


/****************************************************************
* Return amount memory currently allocated
* 
* Written July 2002 by Charles C Burch
***************************************************************/
  INTEGER memman_get_memory_allocated_c() {
    return(memman_allocated);
 }  

/****************************************************************
* lists current content of tracking list
* 
* Written July 2002 by Charles C Burch
***************************************************************/
  void memman_dump_tracking_c(char *title) {
    
    lnklst_dump_list(memman_al, title);
    printf("Amount of memory(bytes) currently allocated=%ld\n\n",
      memman_allocated);
    fflush(stdout);
    return;
 }  

/****************************************************************
* lists current contrent of tracking list and empty it
* 
* Written July 2002 by Charles C Burch
***************************************************************/
  void memman_exit_tracking_c(INTEGER* isw) {
    char tl_loc[20], tl_name[260], str[260];
    long tl_size, vloc;

    if(memman_allocated==0) {
      if(*isw) {
        printf("There are currently no allocated memman variables\n");
        fflush(stdout);
      }
      lnklst_delete_list(memman_al);
      return;
    }

    printf("The following are remaining allocated memman variables:\n");
    lnklst_get_list_entry(memman_al, tl_loc, str, -1);
    while(tl_loc[0]!='\0') {
      sscanf(str,"%ld %ld %s",&vloc, &tl_size, tl_name);
      if(*isw || tl_size>0)
        printf(" pointer location=%s, data location=%ld, name=%s, size=%ld\n", 
          tl_loc, vloc, tl_name, tl_size);
      lnklst_get_list_entry(memman_al, tl_loc, str, -1);
    }
    printf("Amount of memory allocated=%ld\n",memman_allocated);
    memman_allocated=0;
    fflush(stdout);
    return;
  }  
    
#ifdef __cplusplus
}
#endif

/*********************************** test driver ****************************
#include <stdlib.h>
#include <stdio.h>
#include "memman.h"

int main() {
  char *p1, *p2;

  p1=(char*) memman_malloc(1,"p1");
  memman_dump_tracking_c("after malloc p1");
  p1[0]=0xaa;
 
  p2=(char*) memman_malloc(1,"p2");
  memman_dump_tracking_c("after malloc p2");
  printf("p2=%ld, purposely creating data corruption\n",(long)p2);
  p2[0]=0;p2[-1]=0;p2[2]=0;

  memman_free(p2,"p2");
  memman_dump_tracking_c("after free p2");

  p1=memman_realloc(p1,1000,"p1");
  memman_dump_tracking_c("after realloc p1");
  printf("p1[0] after realloc(should be aa)=%x\n",0xff & (int)p1[0]);
  memman_free(p1,"p1");
  memman_dump_tracking_c("after free p1");
 
  return(0);
}
****************************************************************************/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
