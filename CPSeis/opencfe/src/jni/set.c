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
/****
!<CPS_v1 type=PRIMITIVE"/>
****/


/*--------------------------------- set.c ------------------------------------*/
/*--------------------------------- set.c ------------------------------------*/
/*--------------------------------- set.c ------------------------------------*/
 

/****


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E 
!
! Name       : set 
! Category   : int_api
! Written    : 1999-08-19   by: Donna K. Vunderink
! Revised    : 2000-02-27   by: Donna K. Vunderink
! Maturity   : raw
! Purpose    : INT API Interface functions.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                            REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2000-02-27  Vunderink    Fixed problem caused by array being NULL.
!  1. 1999-08-19  Vunderink    Initial version.
!
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS 
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES 
!
!
!-------------------------------------------------------------------------------
!</programming_doc>
****/


char XXXX_IDENT[100] = "$Id: set.c,v 1.1 2000/02/27 13:37:39 sps beta sps $";


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include "int_api.h"

#ifdef NEED_UNDERSCORE
#define INT_API_SET_VALUE_STRING           "int_api_set_value_"
#define INT_API_SET_ARRAY_STRING           "int_api_set_array_"
#define INT_API_PUT_POINTERS_STRING        "int_api_put_pointers_"
#else
#ifdef NEED_CAPITALS
#define INT_API_SET_VALUE_STRING           "INT_API_SET_VALUE"
#define INT_API_SET_ARRAY_STRING           "INT_API_SET_ARRAY"
#define INT_API_PUT_POINTERS_STRING        "INT_API_PUT_POINTERS"
#else
#define INT_API_SET_VALUE_STRING           "int_api_set_value"
#define INT_API_SET_ARRAY_STRING           "int_api_set_array"
#define INT_API_PUT_POINTERS_STRING        "int_api_put_pointers"
#endif
#endif

#ifdef __cplusplus
extern "C"
{
#endif


/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/
/*------------------------- start of functions ----------------------------*/



static void *handle;
static void (*int_api_set_value_ptr) (int *processID, const char *action, const char *keyword, const char *value);
static void (*int_api_set_array_ptr) (int *processID, const char *action, const char *keyword, const char *array, int *start, int *end, int *siz);
static void (*int_api_put_pointers_ptr) (void (*pv) (int, const char *, const char *, const char *), void (*pa) (int, const char *, const char *, const char **, int, int));
static const char ctmp[4] = {"   "};

void set_value (int windowID, const char *action, const char *keyword, const char *value)
{
  const char *f_action;
  const char *f_keyword;
  const char *f_value;
  void (*put_value_ptr) ();
  void (*put_array_ptr) ();
 
  
  if (action != NULL)
     f_action = action;
  else
     f_action = ctmp;

  if (keyword != NULL)
     f_keyword = keyword;
  else
     f_keyword = ctmp;

  if (value != NULL)
     f_value = value;
  else
     f_value = ctmp;

  printf("int_debugset_value(%d, \"%s\" , \"%s\" , \"%s\");\n",windowID, f_action,f_keyword,f_value);
  fflush(stdout);

  if (!strcmp(f_action,"BackEndLibPath")) {
    handle = dlopen(f_value, RTLD_LAZY);
    if (!handle) {
      fputs(dlerror(),stderr);
      exit(1);
    }
    else {
      printf("Library %s sucessfully loaded\n",f_value);
      fflush(stdout);
    }
    int_api_set_value_ptr = (void (*) (int *, const char *, const char *, const char *)) dlsym(handle,INT_API_SET_VALUE_STRING);
    if (!int_api_set_value_ptr) {
      fputs(dlerror(),stderr);
      exit(1);
    }
    else {
      printf("Subroutine int_api_set_value_ was found\n");
      fflush(stdout);
    }
    put_value_ptr = put_value;
    put_array_ptr = put_array;
    int_api_put_pointers_ptr = (void (*) (void (*pv) (int, const char *, const char *, const char *), void (*pa) (int, const char *, const char *, const char **, int, int))) dlsym(handle,INT_API_PUT_POINTERS_STRING);
    if (!int_api_put_pointers_ptr) {
      fputs(dlerror(),stderr);
      exit(1);
    }
    else {
      printf("Subroutine int_api_put_pointers_ was found\n");
      fflush(stdout);
    }
    (*int_api_put_pointers_ptr) (put_value_ptr,put_array_ptr);
  }
  else
    (*int_api_set_value_ptr) (&windowID, f_action, f_keyword, f_value);

}



void set_array (int windowID, const char *action, const char *keyword, const char **array, int start, int end)
{

  const char *f_action;
  const char *f_keyword;
  char       *new_array_ptr;
  char       *new_array;
  int        i;
  int        n;
  int        maxstr = 0;

  if (int_api_set_array_ptr == NULL) {
    int_api_set_array_ptr = (void (*) (int *, const char *, const char *, const char *, int *, int *, int *)) dlsym(handle,INT_API_SET_ARRAY_STRING);
    if (!int_api_set_array_ptr) {
      fputs(dlerror(),stderr);
      exit(1);
    }
    else {
      printf("Subroutine int_api_set_array_ was found\n");
      fflush(stdout);
    }
  }

  if (action != NULL)
     f_action = action;
  else
     f_action = ctmp;

  if (keyword != NULL)
     f_keyword = keyword;
  else
     f_keyword = ctmp;

  if (end == -1) {
    end = start;
    start = 1;
  }


  if (array != NULL) {
     for (i=start; i<=end; i++) {
        if (array[i-start] != NULL) {
           n = strlen(array[i-start]);
           if (n > maxstr) maxstr = n;
        }
     }
     maxstr = maxstr + 1;
     n = sizeof(long);
     i = maxstr / n;
     if (i*n < maxstr) i = i + 1;
     maxstr = i * n;
     new_array_ptr = (char *) malloc((end-start+1)*maxstr);
     new_array = new_array_ptr;
     for (i=start; i<=end; i++) {
        if (array[i-start] != NULL) {
           printf("int_debug strcpy(array[%d] , \"%s\");\n",i-start,array[i-start]);
           strcpy(new_array,array[i-start]);
        }
        else {
           printf("int_debug strcpy(array[%d] , NULL);\n",i-start);
           strcpy(new_array,ctmp);
        }
        new_array = new_array + maxstr;
     }
     printf("int_debug set_array(%d , \"%s\" , \"%s\" , array_ptr ,%d,%d);\n",windowID,f_action, f_keyword,start,end);
     fflush(stdout);
     (*int_api_set_array_ptr) (&windowID, f_action, f_keyword, new_array_ptr, &start, &end, &maxstr);
     free(new_array_ptr);
  }
  else {
     i       = start;
     maxstr  = sizeof(long);
     new_array_ptr = (char *) malloc(maxstr);
     new_array = new_array_ptr;
     strcpy(new_array,ctmp);
     printf("set_array: Array is NULL setting to blanks\n");
     printf("int_debug strcpy(array[0] , \"   \");\n");
     printf("int_debug set_array(%d , \"%s\" , \"%s\" , array_ptr ,%d,%d);\n",windowID,f_action, f_keyword,i,i);
     fflush(stdout);
     (*int_api_set_array_ptr) (&windowID, f_action, f_keyword, new_array_ptr, &i, &i, &maxstr);
     free(new_array_ptr);
  }

}


#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
