/*<CPS_v1 type="PROGRAM"/>
!--------------------------------- ezcps.c --------------------------------!!
!--------------------------------- ezcps.c --------------------------------!!
!--------------------------------- ezcps.c --------------------------------!!
!
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
!
!<brief_doc>
!-------------------------------------------------------------------------------
!                       C P S   P R O G R A M
!
! Name       : ezcps
! Category   : stand-alone
! Written    : 2002-06-07   by: Charles C. Burch
! Revised    : 2003-10-07   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Create a skeleton CPS F90 process from a gui_def file.
! Portability: No known limitations.
! Parallel   : No.
!-------------------------------------------------------------------------------
!</brief_doc>
!
!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Using the CPS process_module template and a gui_def file, ezcps creates
! the skeleton fortran code for a CPS process.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! This is executed by entering ezcps [-v] [-t] [-c] input_file [output_file]
!
! The input_file is normally exactly what one uses in the gui_def section of a
! cps file.  In -c mode, it has a different meaning which is described with -c.
!
! ezcps extract the program_name by the name before the last . in the file name.
! The output_file is where the output of ezcps will be written.  If it is not
! specified, it defaults to the program_name.f90
!
! -v (or one can use -verbose) specifies that the skeleton program is to
! contain expanded comments from the CPS process_module template
!
! -t (or one can use -traps) specifies that the skeleton program is to
! contain trap calls on pc_get calls and to include skeleton code for the
! trap subroutines
!
! -c (or one can use -cleanup) specifies to take the input_file, delete
! all !!! comment lines and lines starting with ! --->s, strip off characters
! starting with a --> in every line, and write the output to the output_file.

! The commonly-used ezgui features are supported by ezcps.
! Some of the exotic ezgui elements might not be handled properly, but the
! resulting output should be easily edited to support them.
!-------------------------------------------------------------------------------
!</advice_doc>
!
!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!  6. 2003-10-07  Stoeckley   Fix bug where an & sign might be required at the
!                              beginning of a continuation line.
!  5. 2003-04-01  C C Burch   Change default location of process template.
!                             Removed unneeded fclose.
!  4. 2003-02-07  Stoeckley   Use configuation file to get the template.
!  3. 2002-11-08  C C Burch   Fix arrayset bug.
!  2. 2002-11-07  C C Burch   Changed to use memman where possible.
!                             Added -cleanup option.
!  1. 2002-06-07  C C Burch   Initial Version
!
!-------------------------------------------------------------------------------
!</history_doc>
!
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>
!
!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! None required
!
!-------------------------------------------------------------------------------
!</compile_doc>
!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
!!-------------------------- start of program ------------------------------!!
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>

#include  <time.h>
#include "cnfg.h"

/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/* the following the maximum nesting of include files supportted */
#define N_FDS 10

/* the following defines where the process_module_template resides */
#define TEMPLATE_TEST "c:\\ezcps\\process_module_template"
#define TEMPLATE_PROD "/home/sps/templates/process_module_template"
#define TEMPLATE_BETA "/u/poepsn02e/burchcc/updates/process_module_template"

char  *ezcps_ident=
      "$Id: ezcps.c,v 1.6 2003/10/07 16:52:09 Stoeckley prod sps $";

char template_file[80];          /* location of template file        */
char input_file[80];             /* excps input file name            */
char program_name[80];           /* name of program                  */
char blank_name[80];             /* blanks same size as program name */
char today[12];                  /* today's date                     */
FILE *fdout;                     /* fd for output file               */
char user_name[33];              /* user name                        */
int  verbose=0;                  /*indicates verbose mode 0-no, 1-yes*/
int  traps=0;                    /*indicates traps mode   0-no, 1-yes*/
int  cleanup=0;                  /*indicates cleanup mode 0-no, 1-yes*/
int  len_program_name=0;         /*length of program_name            */

/****************** info on variables within gui *********************/
int max_scalar_name_length=0;    /*length of longest scalar name     */
int max_array_name_length=0;     /*length of longest variable name   */
int max_array0_name_length=0;    /*length of longest nonarrayset name*/
int max_array1_name_length=0;    /*length of longest arrayset name   */
int max_array2_name_length=0;    /*length of longest arrayset el name*/
int max_array3_name_length=0;    /*length of array index name        */
int max_option_name_length=0;    /*length of longest screen name     */
int max_screen_name_length=0;    /*length of longest screen name     */

int max_arrayset_size=0;         /* size of max # arrays in arraysets*/
int n_arrayset_elements=0;       /*number of elements in arrayset    */
int arrayset_elements[100];      /* array of elements in arrayset    */
int len_arrayset_names=0;        /*length longest name in arrayset   */

int nvars=0;                     /* number active variables          */
struct var_struct {
  int  length;                   /* length of variable               */
  int  arrayset_index;           /* arrayset index, 0 if not arrayset*/
  int  include_sw;               /* >0 if came from include file     */
  char name[34];                 /* variable name                    */
  char type;                     /* variable type I, F, C            */
  char class;                    /* V-scalar, A-array, S-screen      */
  char arrayset[34];             /* arrayset name if part of one     */
};
struct var_struct *vars=NULL;    /* array of variable info           */

/*********************************************************************
* extract a substr a[i1:i2] and place into b
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_substr(char *a, int i1,int i2, char *b) {
  while(i1<=i2) {
    (*b++)=a[i1++];
  }
  (*b)='\0';
  return;
}

/*********************************************************************
* find first location of srch in cin, starting at position pos.
* returns strlen(cin if srch not present
*
* Written May 2002 by Charles C Burch
*********************************************************************/
int ezcps_find_chars(char *cin, int pos, char *srch) {
  int i, j, cin_len;

  cin_len=strlen(cin);
  for (i=pos; i<cin_len; i++) {
    for (j=0; srch[j]!='\0'; j++){
      if(cin[i+j]!=srch[j]) break;
    }
    if(srch[j]=='\0') return(i);
  }
  return(cin_len);
}

/*********************************************************************
* find last location of srch in cin, starting at position pos.
* returns strlen(cin if srch not present
*
* Written May 2002 by Charles C Burch
*********************************************************************/
int ezcps_find_last_chars(char *cin, int pos, char *srch) {
  int i, j, last_find, cin_len;

  last_find=-1;
  cin_len=strlen(cin);
  for (i=pos; i<cin_len; i++) {
    for (j=0; srch[j]!='\0'; j++){
      if(cin[i+j]!=srch[j]) break;
    }
    if(srch[j]=='\0') last_find=i;
  }
  if(last_find<0) last_find=cin_len;
  return(last_find);
}

/*********************************************************************
* find first location in cin, starting at position pos, !=srch.
* returns strlen(cin if srch always present
*
* Written May 2002 by Charles C Burch
*********************************************************************/
int ezcps_find_not_chars(char *cin, int pos, char *srch) {
  int i, j, cin_len;

  cin_len=strlen(cin);
  for (i=pos; i<cin_len; i++) {
    for (j=0; srch[j]!='\0'; j++){
      if(cin[i+j]==srch[j]) break;
    }
    if(srch[j]=='\0') return(i);
  }
  return(cin_len);
}

/*********************************************************************
* converts in to upper case and places in out.
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_upper_case(char *in, char *out){
  char c_in;

  while((c_in=(*in++))!='\0') {
    if(c_in>='a' && c_in<='z') c_in-=('a'-'A');
    (*out++)=c_in;
  }
  (*out)='\0';
  return;
}

/*********************************************************************
* converts in to lower case and places in out.
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_lower_case(char *in, char *out){
  char c_in;

  while((c_in=(*in++))!='\0') {
    if(c_in>='A' && c_in<='Z') c_in+=('a'-'A');
    (*out++)=c_in;
  }
  (*out)='\0';
  return;
}

/*********************************************************************
* substitute all occurences of string find in line by string sub
* n_line is the space availabale in line-return -1 if substutions
* caused length of line to exceed n_line, else return 0;
*
* Written May 2002 by Charles C Burch
*********************************************************************/
int ezcps_strsub(char *line, char *find, char *sub, int n_line){
    int i1, n_find, n_sub;

    i1=0;
    n_find=strlen(find);
    n_sub=strlen(sub);
    while(1) {
      i1=ezcps_find_chars(line,i1,find);
      if(line[i1]=='\0') break;
      if((strlen(line)+n_sub-n_find)>=n_line) return(-1);

      memmove(line+i1+n_sub, line+i1+n_find, strlen(line)-i1-n_find+1);
      memmove(line+i1,sub,n_sub);
      i1+=n_sub;
    }
    return(0);
}

/*********************************************************************
* get user name
*
* Written May 2002 by Charles C Burch
*********************************************************************/
int ezcps_get_user_name(char *un){
  struct passwd *user_info;

  if((user_info=getpwuid(geteuid()))==NULL) {
    strcpy(un,"");
    return(-1);
  }

  strcpy(un,user_info->pw_name);
  return(0);
}

/*********************************************************************
* write out line of code
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_write_line(char *line_in) {
  int n, i1, comment_sw;
  char work[81], line_space[1024], *line;

/************
  printf("line=%s\n",line_in);
************/
  line=line_space;
  comment_sw=0;
  strcpy(line, line_in);
  n=strlen(line);
  while(n>0) {
    if(line[n-1]==' ' || line[n-1]=='\n') {
      line[--n]='\0';
    } else {
      break;
    }
  }

  while(n>80) {
    if(line[0]!='!'   || line[1]!='!'   || line[2]!='-' ||
       line[n-3]!='-' || line[n-2]!='!' || line[n-1]!='!') break;
    memmove(line+n-3,line+n-2,3);  /*cut out two dashes*/
    memmove(line+2,line+3,n-3);
    n-=2;
  }

  while(strlen(line)>80) {
    ezcps_substr(line,0,79,work);

    if(work[79]==',' || work[79]=='!') {
      work[79]='&';
      fprintf(fdout,"%s\n",work);
      line+=(80-7);
      memset(line,' ',7);
      continue;
    }

    n=ezcps_find_last_chars(work,0,",");
    if(work[n]==',') n++;

    i1=ezcps_find_last_chars(work,0,"!");
    if(work[i1]=='!') {
      if(i1>n) n=i1;
      comment_sw=1;
    }

    if(n==80 && comment_sw==0)
      n=ezcps_find_last_chars(work,0," ");

    if(n<40) n=80;
    if(n<80) memset(work+n,' ',80-n);
    if(comment_sw==0) strcpy(work+79,"&");

    fprintf(fdout, "%s\n", work);
    line+=(n-7);
    memset(line,' ',7);
    if(comment_sw==0) line[6] = '&';   /* added 2003-10-07 */
    if(comment_sw==1) line[0]='!';
  }
  fprintf(fdout, "%s\n",line);
  return;
}

/******************************************************************************
* starting at indx search vars for next arrayset and fill out arrayset_elements
*
* Written May 2002 by Charles C Burch
******************************************************************************/
int ezcps_get_next_arrayset(int indx) {
  int i;

  n_arrayset_elements=0;
  len_arrayset_names=0;

  if(max_array1_name_length==0) return(nvars);

  while(indx<nvars) {
    if(vars[indx].arrayset_index==1) {
      for(i=0; i<nvars; i++) {
        if(strcmp(vars[i].arrayset,vars[indx].arrayset)==0) {
          n_arrayset_elements++;
          arrayset_elements[vars[i].arrayset_index-1]=i;
          if(strlen(vars[i].name)>len_arrayset_names)
            len_arrayset_names=strlen(vars[i].name);
        }
      }
      return(indx+1);
    }
    indx++;
  }
  return(nvars);
}

/*********************************************************************
* extract a variable name from a substring line[i1:i2]
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_form_var(char *line, int i1, int i2, char *var){
  char c;
  int i3, i4, i5;

/*******************************
  char work[132];
  ezcps_substr(line,i1,i2,work);
  printf("work=%s\n",work);
*******************************/

/**** see if '[  /' or [  ]' present ***/
  if(line[(i3=ezcps_find_chars(line,i1,"["))]!='\0' && i3<=i2) {
    i4=ezcps_find_chars(line,i3,"/");
    i5=ezcps_find_chars(line,i3,"]");
    if(i5<i4) i4=i5;  /*i4=min(i4,i5)*/
    if(i4>(i3+1) && i4<=i2) {
      i1=i3+1;
      i2=i4-1;
    }
  }

/**** extract variable name   ****/
  i5=0;
  while(i1<=i2) {
    if((c=line[i1++])=='\0') break;
    if(c=='[') break;

    if((c>='a' && c<='z') ||
       (c>='A' && c<='Z') ||
       (c>='0' && c<='9') ||
        c=='_') {
      if(c>='a' && c<='z') c-=('a'-'A');
      if(i5>0 || (c>='A' && c<='Z')) {
        (*var++)=c;
        if((++i5)>=32) break;
      }
    }
  }
  (*var)='\0';
  return;
}

/*********************************************************************
* Make a string a specified length padding on end with blanks
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_make_fixed_length(char *a, int n) {
  int len_a;

  len_a=strlen(a);
  if(len_a<n) {
    memset(a+len_a,' ',n-len_a);
    a[n]='\0';
  }
  return;
}

/*********************************************************************
* add new variable info to the variable structure
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_add_var(char *name, char type, char class, int length,
    char *arrayset, int ifd) {
  nvars++;
  if((vars=(struct var_struct*)
          realloc(vars,nvars*sizeof(struct var_struct)))== NULL) {
    printf("Error: Unable to realloc vars\n");
    exit(1);
  }

  strncpy(vars[nvars-1].name,name,33);
  vars[nvars-1].type=type;
  vars[nvars-1].class=class;
  vars[nvars-1].length=length;
  vars[nvars-1].include_sw=ifd;
  strncpy(vars[nvars-1].arrayset,arrayset,33);
/***************
  printf("name=%s, type=%c, length=%d, class=%c, arrayset=%s\n",
    vars[nvars-1].name,vars[nvars-1].type, vars[nvars-1].length,
    vars[nvars-1].class, vars[nvars-1].arrayset);
****************/
  return;
}
/*********************************************************************
* gets the variables in the gui definition.
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_get_variables() {
  char line[133], last_line[133], type, line_uc[133], work[133];
  char var[80], class, arrayset[80];
  FILE *fd, *fds[N_FDS];
  int ifd, i, i1, i2, i3, i4, n1, length, arrayset_sw;

  nvars=0;

  if((fd=fopen(input_file,"r"))==NULL) return;

  ifd=0;
  fds[0]=fd;

  memset(last_line,' ',sizeof(last_line));
  while(1) {
    if(fgets(line,sizeof(line),fd)==NULL) {
      fclose(fd);
      if((--ifd) <0) break;
      fd=fds[ifd];
      continue;
    }

    i1=0;
    if(line[0]=='!') i1=1;
    if((n1=strlen(line))>0) {
      if(line[n1-1]=='\n') line[--n1]='\0';
    }

    ezcps_upper_case(line, line_uc);

/*** look for and handle <INCLUDE cards  ***/
    if(line_uc[i2=ezcps_find_chars(line_uc,0,"<INCLUDE ")]!='\0') {
      if((++ifd)==N_FDS) {
        printf(
         "Number of nested \"<INCLUDE  >\" cards exceeds limit supportted\n");
        printf("  \"%s\" skipped\n", line);
        ifd--;
        continue;
      }
      i1=ezcps_find_not_chars(line,8," ");
      i2=ezcps_find_chars(line,i1,">");
      if((i3=ezcps_find_chars(line,i1," "))<i2) i2=i3;
      if(line[i2]=='\0') {
        printf("Invalid \"<INCLUDE >\" card skipped\n");
        printf("  \"%s\"\n",line);
        ifd--;
        continue;
      }
      ezcps_substr(line,i1,i2-1,work);
      if((fd=fopen(work,"r"))==NULL) {
        printf("Unable to open include file: %s\n", work);
        fd=fds[--ifd];
        continue;
      }
      fds[ifd]=fd;
      continue;
    }

/*** look for and handle <PARMS cards  ***/
    if(line_uc[i2=ezcps_find_chars(line_uc,0,"<PARMS ")]!='\0') {
      i2=ezcps_find_chars(line_uc,i2," ");
      i2=ezcps_find_not_chars(line_uc,i2," ");
      i3=ezcps_find_chars(line_uc,i2,"[");
      i4=ezcps_find_chars(line_uc,i3,"/");
      i=ezcps_find_chars(line_uc,i3,"]");
      if(i<i4) i4=i;

      if(i3>(i2+1) && i4>(i3+1)) {
        ezcps_form_var(line_uc,i2,i3-1,var);
        for(i=0;i<nvars;i++) {
          if(strcmp(var,vars[i].name)==0) {
            ezcps_form_var(line_uc, i3,i4,vars[i].name);
            break;
          }
        }
      }
      continue;
    }

/*** look for <NS cards ***/
    if(line_uc[i2=ezcps_find_chars(line_uc,0,"<NS ")]!='\0') {
      i2=ezcps_find_chars(line_uc,i2," ");
      i2=ezcps_find_not_chars(line_uc,i2," ");
      i3=ezcps_find_chars(line_uc,i2,"/");
      i=ezcps_find_chars(line_uc,i2,"]");
      if(i<i3) i3=i;

      if(i3>(i2+1)) {
        ezcps_form_var(line_uc,i2,i3-1,var);
        ezcps_add_var(var, 'N', 'S', 0,"", ifd);
      }
      continue;
    }

/*** extract variable names from ezgui edit fields ***/
    strcpy(arrayset,"");
    arrayset_sw=0;
    while(i1<n1){
      class='V';
      i1=ezcps_find_not_chars(line,i1," ");
      i2=ezcps_find_chars(line,i1,"  ");
      i3=ezcps_find_chars(line,i1,"`");

      type='-';   /** skip border-use it to skip other elements also **/
      if(i3<i2) {
        i2=ezcps_find_chars(line,i3," ");
        type=line[i3+1];
        if(type>='a' && type<='z') type-=('a'-'A');
        if(type=='X' || type=='S' || type=='D' || type=='P' || type=='O' ||
           type=='R' || type=='A' || type=='Q' || type=='M') {
          type='C';
        } else if(type=='I' || type=='F') {
          ;
        } else if(type=='C' || type=='K') {
          type='O';
        } else {
          type='-';
        }
      }

      if(i3==i1 && type!='-') {
        i3=ezcps_find_chars(line,i1+1,"`");
        if(i3<i2) {
          i2=i3-1;
          if(arrayset_sw==0) arrayset_sw=1;
        } else {
          i2--;
        }
        if(last_line[i1]=='`') {
          type='-';
        } else {
          class='A';    /*array element*/
          i3=i2+1;
        }
      }

      if(type!='-'){
        if(class=='A') {
          /*arrays get their variable name from previous line*/
          ezcps_form_var(last_line,i1,i3-1,var);
          length=i2-i1+1;
          if(arrayset_sw==1) {
            strcpy(arrayset,var);
            strcat(arrayset,"_ARRAYSET");
            arrayset_sw=2;
          }
        } else {
          ezcps_form_var(line,i1,i3-1,var);
          length=i2-i3;
        }

        ezcps_add_var(var, type, class, length, arrayset, ifd);
      }
      i1=i2+1;
      if(line[i1]!='`' && arrayset_sw!=0) {
        arrayset_sw=0;
        strcpy(arrayset,"");
      }
    }
    memset(last_line,' ',sizeof(last_line));   /* save last line */
    memcpy(last_line,line,n1);

  }

/**** get max name lengths ***/
  max_array_name_length=0;
  max_option_name_length=0;
  max_screen_name_length=0;
  max_scalar_name_length=0;
  max_arrayset_size=0;
  max_array0_name_length=0;
  max_array1_name_length=0;
  max_array2_name_length=0;
  arrayset_sw=0;
  strcpy(arrayset,"");
  for(i=0; i<nvars; i++) {
    i1=strlen(vars[i].name);
    vars[i].arrayset_index=0;
    if(vars[i].class=='V') {
      if(i1>max_scalar_name_length) max_scalar_name_length=i1;
      if(vars[i].type=='O') {
        if(i1>max_option_name_length) max_option_name_length=i1;
      }
    } else if(vars[i].class=='A') {
      if(i1>max_array_name_length) max_array_name_length=i1;
      if(strcmp(arrayset,vars[i].arrayset)!=0) {
        strcpy(arrayset,"");
        arrayset_sw=0;
      }
      if(strcmp(vars[i].arrayset,"")!=0) {
        if(arrayset_sw==0) {
          strcpy(arrayset,vars[i].arrayset);
          if(i1>max_array1_name_length ) max_array1_name_length=i1;
        }
        if(i1>max_array2_name_length ) max_array2_name_length=i1;
        arrayset_sw++;
        vars[i].arrayset_index=arrayset_sw;
        if(arrayset_sw>max_arrayset_size) max_arrayset_size=arrayset_sw;
      } else {
        if(i1>max_array0_name_length) max_array0_name_length=i1;
      }
    } else if(vars[i].class=='S') {
      if(i1>max_screen_name_length) max_screen_name_length=i1;
    }
  }

  max_array3_name_length=max_array1_name_length+9;
  if(max_array0_name_length>max_array3_name_length)
    max_array3_name_length=max_array0_name_length;

/*** sort variable names ***/

  i4=nvars-1;
  while (i4>0) {
    i3=i4;
    i4=0;
    for(i=0; i<i3; i++) {

      if(strcmp(vars[i].name,vars[i+1].name)>0) {
        i4=i;
        strcpy(var,vars[i].name);
        strcpy(vars[i].name,vars[i+1].name);
        strcpy(vars[i+1].name,var);

        strcpy(arrayset,vars[i].arrayset);
        strcpy(vars[i].arrayset,vars[i+1].arrayset);
        strcpy(vars[i+1].arrayset,arrayset);

        length=vars[i].length;
        vars[i].length=vars[i+1].length;
        vars[i+1].length=length;

        class=vars[i].class;
        vars[i].class=vars[i+1].class;
        vars[i+1].class=class;

        type=vars[i].type;
        vars[i].type=vars[i+1].type;
        vars[i+1].type=type;

        i1=vars[i].arrayset_index;
        vars[i].arrayset_index=vars[i+1].arrayset_index;
        vars[i+1].arrayset_index=i1;
      }
    }
  }
/**********************
  for (i=0; i<nvars; i++) {
    printf("name=%s, type=%c, length=%d, class=%c, arrayset=%s, as_index=%d\n",
     vars[i].name,vars[i].type, vars[i].length, vars[i].class,
     vars[i].arrayset, vars[i].arrayset_index);
  }
 **********************/
  return;
}

/*********************************************************************
* output gui_def
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_gui_def() {
  FILE *fd;
  char line[132];
  int n;
  int first_line = 1;

  if((fd=fopen(input_file,"r"))!=NULL) {
    while(fgets(line,sizeof(line),fd)!=NULL) {
      n=strlen(line);
      if(line[0]!='!') {
        memmove(line+1, line, ++n);
        line[0]='!';
      }
      if(line[n-1]=='\n') line[--n]='\0';

      if(first_line) {
        if(n<5 || strncmp(line, "!<NS ", 5) != 0)
          fprintf(fdout,"!<NS %s Process/NC=80>\n",program_name);
        first_line=0;
      }
      fprintf(fdout, "%s\n",line);
    }
    fclose(fd);
  }

  return;
}

/*********************************************************************
* output HelpSection
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_HelpSection() {
  int i;
  char line[512];

  for (i=0; i<nvars; i++) {
    if(vars[i].class=='S') continue;
    if(vars[i].include_sw>0) continue;

    sprintf(line, "!<Help KEYWORD=\"%s\">", vars[i].name);
    ezcps_write_line(line);
    ezcps_write_line("!<Tip> -->Insert one-line tip here</Tip>");
    ezcps_write_line("! Default = --> Insert default value");
    ezcps_write_line("! Allowed = --> Insert allowed values");
    sprintf(line, "! --> Insert help contents here for variable %s",
     vars[i].name);
    ezcps_write_line(line);
    ezcps_write_line("!</Help>");
    ezcps_write_line("!");
  }
  return;
}

/*********************************************************************
* output parameter structure info
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_parameter_structure() {
  int i;
  char v_type[80], name_lc[80], work[80], line[256];

  /***********************
  sprintf(line,
   "      type, public                 :: %s_struct",program_name);
  ezcps_write_line(line);
  ezcps_write_line("        private");
  sprintf(line, "        %-26s :: skip_wrapup","logical");
  ezcps_write_line(line);
******************/
  for (i=0; i<nvars; i++) {
    if(vars[i].class!='V' ) continue;

    ezcps_lower_case(vars[i].name, name_lc);
    if(vars[i].type=='I') {
      strcpy(v_type,"integer");
    } else if(vars[i].type=='F') {
      strcpy(v_type,"real");
    } else {
      sprintf(v_type,"character(len=%d)",vars[i].length);
    }

    sprintf(line, "        %-26s :: %s\n",v_type,name_lc);
    ezcps_write_line(line);
  }

  ezcps_write_line(" ");
  for (i=0; i<nvars; i++) {
    if(vars[i].class!='A' ) continue;
    if(vars[i].arrayset_index>1) continue;

    ezcps_lower_case(vars[i].name, work);
    ezcps_make_fixed_length(work, max_array3_name_length);
    sprintf(line,"        %-26s :: n_%s !#elements\n",
     "integer",work);
    ezcps_write_line(line);
  }

  ezcps_write_line(" ");
  for (i=0; i<nvars; i++) {
    if(vars[i].class!='A' ) continue;

    ezcps_lower_case(vars[i].name, name_lc);
    if(vars[i].type=='I') {
      strcpy(v_type,"integer");
    } else if(vars[i].type=='F') {
      strcpy(v_type,"real");
    } else {
      sprintf(v_type,"character(len=%d)",vars[i].length);
    }

    strcat(name_lc, "(:)");
    strcat(v_type,",pointer");
    sprintf(line, "        %-26s :: %s\n",v_type,name_lc);
    ezcps_write_line(line);
  }

  return;
}

/*********************************************************************
* initialize parameters
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_initialize(){
  int i;
  char name_lc[80], line[256];

  ezcps_write_line("! --> Change the default values below as needed");
  ezcps_write_line(" ");

  for (i=0; i<nvars; i++) {
    if(vars[i].class!='V') continue;

    ezcps_lower_case(vars[i].name, name_lc);
    ezcps_make_fixed_length(name_lc,max_scalar_name_length);
    if(vars[i].type=='I') {
      sprintf(line, "      obj%c%s = 0", '%',name_lc);
    } else if(vars[i].type=='F') {
      sprintf(line, "      obj%c%s = 0.0", '%',name_lc);
    } else {
      sprintf(line, "      obj%c%s = ' '", '%',name_lc);
    }
    ezcps_write_line(line);
  }
  if(max_array_name_length>0) {
    ezcps_write_line(" ");
    for (i=0; i<nvars; i++) {
      if(vars[i].class!='A') continue;

      if(vars[i].arrayset_index==0) {
        ezcps_lower_case(vars[i].name, name_lc);
      } else if(vars[i].arrayset_index==1) {
        ezcps_lower_case(vars[i].name, name_lc);
      } else {
        continue;
      }
      ezcps_make_fixed_length(name_lc,max_array3_name_length);
      sprintf(line, "      obj%cn_%s = 0",'%', name_lc);
      ezcps_write_line(line);
    }

    ezcps_write_line(" ");
    for (i=0; i<nvars; i++) {
      if(vars[i].class!='A') continue;

      ezcps_lower_case(vars[i].name, name_lc);
      ezcps_make_fixed_length(name_lc,max_array_name_length);
      if(vars[i].type=='I') {
        sprintf(line, "      if(associated(obj%c%s)) obj%c%s = 0\n",
            '%',name_lc, '%',name_lc);
      } else if(vars[i].type=='F') {
        sprintf(line, "      if(associated(obj%c%s)) obj%c%s = 0.0\n",
            '%',name_lc, '%',name_lc);
      } else {
        sprintf(line, "      if(associated(obj%c%s)) obj%c%s = ' '\n",
            '%',name_lc, '%',name_lc);
      }
      ezcps_write_line(line);
    }
  }
  return;
}

/*********************************************************************
* nullify pointers
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_nullify_pointers(){
  int i;
  char name_lc[80], work[80], line[256];

  for (i=0; i<nvars; i++) {
    if(vars[i].class=='A') {
      ezcps_lower_case(vars[i].name, name_lc);
      strcpy(work, name_lc);
      ezcps_make_fixed_length(name_lc,max_array_name_length);
      sprintf(line, "      call memman_nullify (obj%c%s, \"%s_%s\")\n",
          '%',name_lc, program_name, work);
      ezcps_write_line(line);
    }
  }
  return;
}

/*********************************************************************
* free pointers
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_free_pointers(){
  int i;
  char name_lc[80], line[256];

  for (i=0; i<nvars; i++) {
    if(vars[i].class=='A') {
      ezcps_lower_case(vars[i].name, name_lc);
      sprintf(line,
       "      call memman_free (obj%c%s)\n",
       '%',name_lc);
      ezcps_write_line(line);
    }
  }
  return;
}

/*********************************************************************
* deallocate pointers
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_deallocate_pointers(){
  int i;
  char name_lc[80], work[80], line[256];

  for (i=0; i<nvars; i++) {
    if(vars[i].class=='A') {
      ezcps_lower_case(vars[i].name, name_lc);
      strcpy(work, name_lc);
      ezcps_make_fixed_length(name_lc,max_array_name_length);
      sprintf(line,
       "      if(associated(obj%c%s)) call _memman_deallocate (obj%c%s)\n",
       '%',name_lc, '%', work);
      ezcps_write_line(line);
    }
  }
  return;
}

/*********************************************************************
* form trap name form a variable name
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_form_trap_name(char *var, char *trap) {
  int n, n1;

  n=strlen(var);
  n1=n+1;
  if(n1>31) n-=(n1-31);
  strcpy(trap,program_name);
  strcat(trap,"_");
  strncat(trap,var,n);
  return;
}

/*********************************************************************
* dump_vars
*
* Written October 2002 by Charles C Burch
*********************************************************************/
void ezcps_dump_vars(){
  int i;

  for(i=0; i<nvars; i++) {
    printf("i=%d, name=%s, length=%d, arrayset_index=%d, include_sw=%d\n",
     i,vars[i].name,vars[i].length,vars[i].arrayset_index,vars[i].include_sw);
    printf("  type=%c, class=%c, arrayset=%s\n",
     vars[i].type, vars[i].class, vars[i].arrayset);
  }
  return;
}

/*********************************************************************
* register arraysets
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_register_array_names(){
  int i, j;
  char work[132], name_uc[80], line[1024];
  char arrayset_name[80];

  if(max_array1_name_length>0) {          /*arraysets */
    j=0;
    while(j<nvars) {
      j=ezcps_get_next_arrayset(j);
      if(n_arrayset_elements>0) {
        ezcps_upper_case(vars[arrayset_elements[0]].arrayset, arrayset_name);
        strcpy(name_uc,vars[arrayset_elements[0]].name);
        ezcps_make_fixed_length(name_uc,len_arrayset_names);
        sprintf(line,"      call pc_register_array_names(\'%s\', (/\'%s\'",
          arrayset_name,name_uc);

        for (i=1; i<n_arrayset_elements; i++) {
          strcpy(name_uc,vars[arrayset_elements[i]].name);
          ezcps_make_fixed_length(name_uc,len_arrayset_names);
          sprintf(work,", \'%s\'",name_uc);
          strcat(line,work);
        }
        strcat(line,"/))");
        ezcps_write_line(line);

      }
    }
  }
  return;
}

/*********************************************************************
* read parameters
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_read_parameters(){
  int i, j;
  char name_lc[80], work[132], name_uc[80], line[1024], trap[80], name1_lc[80];
  char arrayset_name[80];

  for (i=0; i<nvars; i++) {
    if(vars[i].class!='V') continue;

    strcpy(work, vars[i].name);
    ezcps_lower_case(work, name_lc);
    ezcps_make_fixed_length(work,max_scalar_name_length);
    if(traps==0) {
      sprintf(line, "      call pc_get(\'%s\', obj%c%s)",
       work, '%', name_lc);
    } else {
      ezcps_form_trap_name(name_lc,trap);
      ezcps_make_fixed_length(name_lc,max_scalar_name_length);
      sprintf(line, "      call pc_get(\'%s\', obj%c%s, %s)",
       work, '%', name_lc, trap);
    }
    ezcps_write_line(line);
  }

  /** printf("max_array0_name_length=%d\n", max_array0_name_length);
      printf("max_array1_name_length=%d\n", max_array1_name_length); **/

  if(max_array0_name_length>0) {
    ezcps_write_line(" ");
    ezcps_write_line("!      *** Arrays ***");

    if(traps==1 && max_array0_name_length>0) {  /*array traps*/
      ezcps_write_line(" ");
      for (i=0; i<nvars; i++) {
        if(vars[i].class!='A' || vars[i].arrayset_index!=0) continue;

        strcpy(name_uc, vars[i].name);
        ezcps_lower_case(name_uc, name_lc);
        ezcps_make_fixed_length(name_uc,max_array0_name_length);
        ezcps_form_trap_name(name_lc,trap);
        sprintf(line, "      call pc_call_array_trap(\'%s\', %s)\n",
         name_uc, trap);
        ezcps_write_line(line);
      }
    }

    for (i=0; i<nvars; i++) {
      if(vars[i].class!='A' || vars[i].arrayset_index!=0) continue;

      strcpy(name_uc, vars[i].name);
      ezcps_lower_case(name_uc, name_lc);
      strcpy(work,name_lc);
      ezcps_make_fixed_length(name_uc,max_array0_name_length);
      ezcps_make_fixed_length(name_lc,max_array0_name_length);

      if(traps==0) {
        sprintf(line, "      call pc_alloc(\'%s\', obj%c%s, obj%cn_%s)\n",
         name_uc, '%', name_lc, '%',work);
      } else {
        ezcps_form_trap_name(work,trap);
        if(strlen(trap)==32) {
          trap[32]='_';
        } else {
          strcat(trap,"_");
        }
        sprintf(line, "      call pc_alloc(\'%s\', obj%c%s, obj%cn_%s, %s)\n",
         name_uc, '%', name_lc, '%', name_lc, trap);
      }
      ezcps_write_line(line);
    }
  }

  if(max_array1_name_length>0) {          /*arraysets */
    j=0;
    while(j<nvars) {
      j=ezcps_get_next_arrayset(j);
      /* printf("arrayset, j=%d\n",j); */

      if(n_arrayset_elements>0) {
        ezcps_upper_case(vars[arrayset_elements[0]].arrayset, arrayset_name);
        ezcps_write_line(" ");
        sprintf(line, "!     ****** %s ******", arrayset_name);
        ezcps_write_line(line);

        strcpy(name_uc,vars[arrayset_elements[0]].name);
        ezcps_lower_case(name_uc, name1_lc);
        ezcps_make_fixed_length(name_uc,len_arrayset_names);

        if(traps!=0) {
          ezcps_write_line(" ");
          ezcps_form_trap_name(name1_lc,trap);
          sprintf(line,"      call pc_call_arrayset_trap(\'%s\', %s)",
            arrayset_name, trap);
          ezcps_write_line(line);
        }
        ezcps_write_line(" ");

        for(i=0;i<n_arrayset_elements;i++) {
          strcpy(name_uc, vars[arrayset_elements[i]].name);
          ezcps_lower_case(name_uc, name_lc);
          ezcps_form_trap_name(name_lc,trap);
          ezcps_make_fixed_length(name_uc,len_arrayset_names);
          ezcps_make_fixed_length(name_lc,len_arrayset_names);
          if(traps==0) {
            sprintf(line,
             "      call pc_alloc(\'%s\', obj%c%s, obj%cn_%s)",
             name_uc, '%', name_lc, '%', name1_lc);
          } else {
            if(strlen(trap)==32) {
              trap[32]='_';
            } else {
              strcat(trap,"_");
            }
            sprintf(line,
             "      call pc_alloc(\'%s\', obj%c%s, obj%cn_%s, %s)",
             name_uc, '%', name_lc,  '%', name1_lc, trap);
          }
          ezcps_write_line(line);
        }
      }
    }
  }

  if(traps==1 && max_screen_name_length>0) {
    ezcps_write_line(" ");
    ezcps_write_line("!      *** screen traps ***");
    ezcps_write_line(" ");

    for (i=0; i<nvars; i++) {
      if(vars[i].class=='S'){
        strcpy(name_uc, vars[i].name);
        ezcps_lower_case(name_uc, name_lc);
        ezcps_make_fixed_length(name_uc,max_screen_name_length);
        ezcps_form_trap_name(name_lc,trap);
        sprintf(line, "      call pc_call_screen_trap(\'%s\', %s)\n",
         name_uc, trap);
        ezcps_write_line(line);
      }
    }
  }


  if(traps!=0) {
    ezcps_write_line(" ");
    ezcps_write_line("!      *** end trap ***");
    ezcps_write_line(" ");
    ezcps_form_trap_name("end",trap);
    sprintf(line,"      call pc_call_end_trap(%s)\n",trap);
    ezcps_write_line(line);
  }
  return;
}

/*********************************************************************
* write put options
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_put_options() {
  int i;
  char name_lc[80], name_uc[80], work[80], line[1024];

  if(max_option_name_length>0) {
    for (i=0; i<nvars; i++) {
      if(vars[i].class!='V' || vars[i].type!='O') continue;

      strcpy(name_uc, vars[i].name);
      ezcps_make_fixed_length(name_uc, max_option_name_length);
      sprintf(line,"      call pc_put_options_field(\'%s\', (/",
       name_uc);
      strcpy(name_lc,"op1");
      ezcps_make_fixed_length(name_lc, vars[i].length);
      sprintf(work, "\'%s\', ",name_lc);
      strcat(line,work);
      strcpy(name_lc,"op2");
      ezcps_make_fixed_length(name_lc, vars[i].length);
      sprintf(work, "\'%s\'/) )",name_lc);
      strcat(line,work);

      ezcps_write_line(line);
    }
  }
  return;
}

/*********************************************************************
* write parameters
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_write_parameters(){
  int i, j;
  char name_lc[80], name_uc[80], work[80], line[1024];

  for (i=0; i<nvars; i++) {
    if(vars[i].class!='V') continue;

    strcpy(name_uc, vars[i].name);
    ezcps_lower_case(name_uc, name_lc);
    ezcps_make_fixed_length(name_uc,max_scalar_name_length);
    sprintf(line, "      call pc_put(\'%s\', obj%c%s)\n",
     name_uc, '%', name_lc);
    ezcps_write_line(line);
  }

  if(max_array_name_length>0) {

    for (i=0; i<nvars; i++) {
      if(vars[i].class!='A') continue;
      if(vars[i].arrayset_index!=0) continue;

      ezcps_write_line(" ");
      strcpy(name_uc,vars[i].name);
      ezcps_lower_case(name_uc, name_lc);
/********************************
      sprintf(line, "      call pc_put(\'N_%s\', obj%cn_%s)",
       name_uc, '%', name_lc);
      ezcps_write_line(line);
*******************************/

      sprintf(line, "      call pc_put(\'%s\', obj%c%s, obj%cn_%s)",
        name_uc, '%', name_lc, '%', name_lc);
      ezcps_write_line(line);
    }
    j=0;
    while(j<nvars) {
      j=ezcps_get_next_arrayset(j);
      if(n_arrayset_elements>0) {
        ezcps_write_line(" ");
        ezcps_lower_case(vars[arrayset_elements[0]].name, work);

        for(i=0;i<n_arrayset_elements; i++) {
          strcpy(name_uc,vars[arrayset_elements[i]].name);
          ezcps_lower_case(name_uc, name_lc);
          ezcps_make_fixed_length(name_uc,len_arrayset_names);
          ezcps_make_fixed_length(name_lc,len_arrayset_names);
          sprintf(line, "      call pc_put(\'%s\', obj%c%s, obj%cn_%s)",
           name_uc, '%', name_lc, '%',work);
          ezcps_write_line(line);
        }
      }
    }
  }
  return;
}

/*********************************************************************
* write code for a trap
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_write_trap(char *trap) {
  char line[256];

  sprintf(line,"      subroutine %s(keyword)",trap);
  ezcps_write_line(line);
  ezcps_write_line("      implicit none");
  ezcps_write_line(
   "      character(len=*), intent(in) :: keyword    !argument");
  ezcps_write_line(" ");
  ezcps_write_line("! --> Insert code to validate data input");
  ezcps_write_line("      return\n");
  sprintf(line,"      end subroutine %s\n",trap);
  ezcps_write_line(line);
  ezcps_write_line(" ");
  return;
}


/*********************************************************************
* write code for a element trap
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_write_element_trap(char *trap) {
  char line[256];

  sprintf(line,"      subroutine %s(keyword, indx, action)",trap);
  ezcps_write_line(line);
  ezcps_write_line("      implicit none");
  ezcps_write_line(
    "      character(len=*), intent(in) :: keyword    !argument");
  ezcps_write_line(
    "      integer,          intent(in) :: indx       !argument");
  ezcps_write_line(
    "      integer,          intent(in) :: action     !argument");
  ezcps_write_line(" ");
  ezcps_write_line("! --> Insert code to validate data input");
  ezcps_write_line("      return");
  sprintf(line,"      end subroutine %s",trap);
  ezcps_write_line(line);
  ezcps_write_line(" ");
  return;
}


/*********************************************************************
* write code for end trap
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_write_end_trap(char *trap) {
  char line[256];

  sprintf(line,"      subroutine %s",trap);
  ezcps_write_line(line);
  ezcps_write_line("      implicit none");
  ezcps_write_line(" ");
  ezcps_write_line("! --> Insert code to validate data input");
  ezcps_write_line("      return");
  sprintf(line,"      end subroutine %s",trap);
  ezcps_write_line(line);
  ezcps_write_line(" ");
  return;
}

/*********************************************************************
* write traps
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_write_traps(){
  int i, j;
  char name_lc[80], trap[80], line[512], arrayset_name[80];

  if(traps==1) {
    for (i=0; i<nvars; i++) {      /*scalar traps*/
      if(vars[i].class=='V') {
        ezcps_lower_case(vars[i].name, name_lc);
        ezcps_form_trap_name(name_lc,trap);
        sprintf(line,"! *** Trap for variable %s ***\n",vars[i].name);
        ezcps_write_line(line);
        ezcps_write_line(" ");
        ezcps_write_trap(trap);
      }
    }

    if(max_array_name_length>0) {   /*array traps*/
      ezcps_write_line(" ");
      for (i=0; i<nvars; i++) {
        if(vars[i].class=='A' && vars[i].arrayset_index==0) {
          ezcps_lower_case(vars[i].name, name_lc);
          ezcps_form_trap_name(name_lc,trap);
          sprintf(line,"! *** Array trap for %s ***\n", name_lc);
          ezcps_write_line(line);
          ezcps_write_line(" ");
          ezcps_write_trap(trap);

          if(strlen(trap)==32) {
            trap[32]='_';
          } else {
            strcat(trap,"_");
          }
          sprintf(line,"! *** Array element trap for %s ***\n",
              vars[i].name);
          ezcps_write_line(line);
          ezcps_write_line(" ");
          ezcps_write_element_trap(trap);
        }
      }
    }

    if(max_array1_name_length>0) {  /*arrayset traps*/
      j=0;
      while(j<nvars) {
        j=ezcps_get_next_arrayset(j);
        if(n_arrayset_elements>0) {
          ezcps_lower_case(vars[arrayset_elements[0]].arrayset, arrayset_name);
          ezcps_lower_case(vars[arrayset_elements[0]].name, name_lc);
          ezcps_form_trap_name(name_lc,trap);
          sprintf(line,"! *** Arrayset trap for %s ***\n", arrayset_name);
          ezcps_write_line(line);
          ezcps_write_line(" ");
          ezcps_write_trap(trap);

          for(i=0;i<n_arrayset_elements;i++) {
            ezcps_lower_case(vars[arrayset_elements[i]].name, name_lc);
            ezcps_form_trap_name(name_lc,trap);
            if(strlen(trap)==32) {
              trap[32]='_';
            } else {
              strcat(trap,"_");
            }
            sprintf(line,"! *** Array element trap for %s ***\n",
              name_lc);
            ezcps_write_line(line);
            ezcps_write_line(" ");
            ezcps_write_element_trap(trap);
          }
        }
      }
    }

    if(max_screen_name_length>0) {    /*screen traps*/
      ezcps_write_line(" ");
      for (i=0; i<nvars; i++) {
        if(vars[i].class=='S') {
          ezcps_lower_case(vars[i].name, name_lc);
          ezcps_form_trap_name(name_lc,trap);
          sprintf(line,"! *** Screen trap for  %s ***\n",vars[i].name);
          ezcps_write_line(line);
          ezcps_write_line(" ");
          ezcps_write_trap(trap);
        }
      }
    }

    ezcps_form_trap_name("end",trap);    /*end trap*/
    ezcps_write_line("! *** End Trap ***");
    ezcps_write_line(" ");
    ezcps_write_end_trap(trap) ;
  }

  return;
}

/*********************************************************************
* skip records in template file until given string is found
*
* Written May 2002 by Charles C Burch
*********************************************************************/
void ezcps_skip_records(FILE *fd, char *next_line) {
  char line[132];

  while(fgets(line,sizeof(line),fd)!=NULL) {
    if(line[ezcps_find_chars(line,0,next_line)]!='\0') break;
  }
  return;
}

/*********************************************************************
* cleanup file--remove !!!/! --> lines and characters starting with --->
*
* Written Nov 2002 by Charles C Burch
*********************************************************************/
int ezcps_cleanup(char *input_file, char *output_file) {
  FILE *fd_in, *fd_out;
  char line[133];
  int i1;

  if(output_file[0]=='\0') strcpy(output_file, input_file);

  printf("Start of ezcps(cleanup mode): input=%s, output=%s\n",
    input_file, output_file);

  if(strcmp(input_file,output_file)==0) {
    printf("input and output are the same, input moved to %s~\n",input_file);
    strcat(input_file,"~");
    remove(input_file);
    if(rename(output_file,input_file)<0) {
      printf("Error: Unable to rename file\n");
      return(-1);
    }
  }

  if((fd_in=fopen(input_file,"r"))==NULL) { /*open input file*/
    printf("Error: input file(%s) does not exist\n",input_file);
    return(-1);
  }

  if((fd_out=fopen(output_file,"w+"))==NULL) {
    printf("Error: Unable to open output file(%s)\n",blank_name);
    fclose(fd_in);
    return(-1);
  }

  while(fgets(line,sizeof(line),fd_in)!=NULL) {
    if((i1=strlen(line))>0) { /*remove trailing /n*/
      if(line[i1-1]=='\n') line[--i1]='\0';
    }

    if(line[0]=='!' && line[1]=='!' && line[2]=='!') {
      continue; /*skip line with ^!!!*/
    }

    if(line[(i1=ezcps_find_chars(line, 0, "-->"))]!='\0') {
      line[i1]='\0'; /*skip chars starting with -->*/
      if(strcmp(line,"! ")==0) continue; /*skip line if ! --->...*/
    }

    fprintf(fd_out,"%s\n",line);
  }

  fclose(fd_in);
  fclose(fd_out);

  return(0);
}

/*********************************************************************
* main program
*
* Written May 2002 by Charles C Burch
*********************************************************************/
int main (int argc, char **argv) {

  time_t      now;
  struct tm   *tm_ptr;
  int i1, i2, n, ifirst;
  char output_file[80], line[256], PROGRAM_NAME[80];
  FILE *fd_template, *fdin;

  if(argc<2) {
    printf("command format is \n");
    printf("  ezcps [-verbose] [-traps] [-cleanup] input_file [output_file]\n");
    exit(1);
  }

/*** get command parameters ***/
  strcpy(input_file,"");
  strcpy(output_file,"");
  verbose=0;
  traps=0;
  cleanup=0;

  i1=1;
  while(i1<argc) {
    strcpy(line,argv[i1++]);
    if(line[0]=='-') {
      if(line[1]=='\0' && i1<argc) strcat(line,argv[i1++]);
      ezcps_upper_case(line, line);
      if(strcmp(line,"-VERBOSE")==0 || strcmp(line,"-V")==0) {
        verbose=1;
      } else if(strcmp(line,"-NOVERBOSE")==0 || strcmp(line,"-NOV")==0) {
        verbose=0;
      } else if(strcmp(line,"-TRAPS")==0 || strcmp(line,"-T")==0) {
        traps=1;
      } else if(strcmp(line,"-NOTRAPS")==0 || strcmp(line,"-NOT")==0) {
        traps=0;
      } else if(strcmp(line,"-CLEANUP")==0 || strcmp(line,"-C")==0) {
        cleanup=1;
      } else if(strcmp(line,"-NOCLEANUP")==0 || strcmp(line,"-NOC")==0) {
        cleanup=0;
      } else {
        printf("Invalid %s argument: %s\n", argv[0], line);
        exit(1);
      }
    } else {
      if(input_file[0]=='\0') {
        strcpy(input_file,line);
      } else if(output_file[0]=='\0') {
        strcpy(output_file,line);
      } else {
        printf("Invalid %s argument: %s\n", argv[0], line);
        exit(1);
      }
    }
  }

  if(cleanup) {
    ezcps_cleanup(input_file, output_file);
    goto done;;
  }

/*** get program name ***/
  n=strlen(input_file);
  for (i2=n-1; i2>=0;i2--) {
    if(input_file[i2]=='.') break;
  }
  if(i2<0) i2=n;
  for (i1=i2-1;i1>=0;i1--) {
    if(input_file[i1]=='/')  break;
  }
  ezcps_substr(input_file,i1+1,i2-1,program_name);
  ezcps_upper_case(program_name,PROGRAM_NAME);
  len_program_name=strlen(program_name);

/*** create default output filename if needed and open it ***/
  if(strcmp(output_file,"")==0) {
    strcpy(output_file, program_name);
    strcat(output_file,".f90");
  }

  if(ezcps_ident[ezcps_find_chars(ezcps_ident,0,"test")]!='\0') {
    strcpy(template_file,TEMPLATE_TEST);

  } else if(ezcps_ident[ezcps_find_chars(ezcps_ident,0,"prod")]!='\0') {
    strcpy(template_file,cnfg_get_value_c("sps_home_dir"));
    if(template_file[0]!='\0') {
      strcat(template_file,"/templates/process_module_template");
    } else {
      strcpy(template_file,TEMPLATE_PROD);
    }

  } else {
    strcpy(template_file,TEMPLATE_BETA);
  }

  printf("Start of ezcps: input=%s, output=%s, program name=%s\n",
    input_file, output_file, program_name);
  printf("process_module_template=%s\n",template_file);

  if((fdin=fopen(input_file,"r"))==NULL) { /*open input file*/
    printf("Error: input file(%s) does not exist\n",input_file);
    goto done;
  } else {
    fclose(fdin);
  }

  if((fdout=fopen(output_file,"w+"))==NULL) {
    printf("Error: Unable to open output file(%s)\n",blank_name);
    goto done;
  }

  if((fd_template=fopen(template_file,"r"))==NULL) { /*open template*/
    printf("Error: Unable to open %s\n",template_file);
    goto done;
  }

  ezcps_get_user_name(user_name); /* get user name */
  if(strlen(user_name)<(sizeof(user_name)-7)) strcat(user_name,"       ");
  user_name[7]='\0';


  n=strlen(program_name);
  memset(blank_name,' ',n);
  blank_name[n]='\0';

  time(&now);                     /*get today's date  */
  tm_ptr=localtime(&now);
  sprintf(today,"%4.4d-%2.2d-%2.2d",
   1900+tm_ptr->tm_year, 1+tm_ptr->tm_mon, tm_ptr->tm_mday);

  ezcps_get_variables();        /*get variable names from inout file*/
  /** ezcps_dump_vars(); **/

  i1=0;
  i1=nvars;  /*bypass arryset printout */
  while(i1<nvars) {
    i1=ezcps_get_next_arrayset(i1);
    if(n_arrayset_elements>0) {
      for (i2=0;i2<n_arrayset_elements; i2++) {
        printf( "%d: %s\n",i2+1,vars[arrayset_elements[i2]].name);
      }
    }
  }

/****** now read template and create skeleton process code *******/
  ifirst=0;
  while(fgets(line,sizeof(line),fd_template)!=NULL) {
    if((i1=strlen(line))>0) { /*remove trailing /n*/
      if(line[i1-1]=='\n') line[--i1]='\0';
    }

    if(ifirst==0) { /*skip until <CPS_v1 found in template*/
      if(line[ezcps_find_chars(line,0, "!<CPS_v1 type=\"PROCESS\"/>")]
            =='\0') continue;
      ifirst=1;
    }

    if(verbose==0) {  /*skip extended comments if not verbose*/
      if(line[0]=='!' && line[1]=='!' && line[2]=='!') continue;
    }

/****************
    check/handle gui_def, HelpSection, obj_struct, pc_gets and pc_puts
****************/
    if(line[ezcps_find_chars(line,0,
     "! --> Insert your GUI layout here")]!='\0') {
      ezcps_skip_records(fd_template,
       "! --> (EZCPS will insert your GUI layout here for you)");
      ezcps_gui_def();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert your context sensitive help here")]!='\0') {
      ezcps_skip_records(fd_template,
       "! --> (EZCPS will insert customized skeleton help for you)");
      ezcps_HelpSection();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert your process parameters here")]!='\0') {
      ezcps_skip_records(fd_template, "! --> (EZCPS will do this for you");
      ezcps_parameter_structure();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert code to nullify process parameter pointers in the OBJ")]
     !='\0') {
      ezcps_skip_records(fd_template, "! --> (EZCPS will do this for you");
      ezcps_nullify_pointers();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert code to deallocate process parameter pointers in the OBJ")]
     !='\0') {
      ezcps_skip_records(fd_template, "! --> (EZCPS will do this for you");
      ezcps_deallocate_pointers();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert code to free process parameter pointers in the OBJ")]
     !='\0') {
      ezcps_skip_records(fd_template, "! --> (EZCPS will do this for you");
      ezcps_free_pointers();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert code to initialize process parameters in the OBJ")]!='\0') {
      ezcps_skip_records(fd_template, "! --> (EZCPS will insert customized");
      ezcps_initialize();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert any calls to PC_REGISTER_ARRAY_NAMES here")]!='\0') {
      ezcps_skip_records(fd_template, "! --> (EZCPS will do this for you");
      ezcps_register_array_names();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert code to read process parameters")]!='\0') {
      ezcps_skip_records(fd_template, "! --> (EZCPS will do this for you");
      ezcps_read_parameters();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert any calls to PC_PUT_OPTIONS_FIELD here")]!='\0') {
      ezcps_skip_records(fd_template,
       "! --> (EZCPS will insert customized skeleton code for you)");
      ezcps_put_options();
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert code here to write process parameters")]!='\0') {
      ezcps_write_parameters();
      ezcps_skip_records(fd_template, "! --> (EZCPS will do this for you");
      continue;
    }

    if(line[ezcps_find_chars(line,0,
     "! --> Insert code for parameter traps here")]!='\0') {
      if(traps!=0) {
        ezcps_skip_records(fd_template,
         "! --> (EZCPS with the -t option will insert skeleton code for you)");
        ezcps_write_traps();
        continue;
      }
    }

    /*Convert Id string*/
    if(line[ezcps_find_chars(line,0, "$Id: process_module_template")]!='\0') {
      sprintf(line,
       "\'%cId: %s,v 1.00 %s 01:01:01 %s beta sps %c\'",
       '$',program_name,today, user_name,'$');
    }

  /************ change program/user name and date **************/
    ezcps_strsub(line,"xxxx",       program_name, sizeof(line));
    ezcps_strsub(line,"XXXX",       PROGRAM_NAME, sizeof(line));
    ezcps_strsub(line,"NNNN   ",    user_name,    sizeof(line));
    ezcps_strsub(line,"NNNN",       user_name,    sizeof(line));
    ezcps_strsub(line,"DDDD-DD-DD", today,        sizeof(line));

    ezcps_write_line(line);
  }

  if(vars!=NULL) {   /** free space for variable names **/
    free(vars);
    vars=NULL;
  }

  fclose(fd_template);
  fclose(fdout);

done:
  printf("End of ezcps\n");
  return(0);
}

#ifdef __cplusplus
}
#endif

/*--------------------------------- end program ------------------------------*/
/*--------------------------------- end program ------------------------------*/
/*--------------------------------- end program ------------------------------*/

