/****
!<CPS_v1 type="PRIMITIVE"/>
****/
/*------------------------------- tpiocnfg.c ------------------------------*/
/*------------------------------- tpiocnfg.c ------------------------------*/
/*------------------------------- tpiocnfg.c ------------------------------*/
 
        /* other files are:  tpiocnfg.h */
 

/****
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
!                        C P S   P R I M I T I V E 
!
! Name       : tpiocnfg.c
! Category   : io
! Written    : 2006-02-06   by: R.S.Day
! Revised    : 2007-05-10   by: Goodger
! Maturity   : beta
! Purpose    : Keep tape configuration information in a single place.
! Portability: Unix only
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
! This file contains configuration information (node names, ports, etc.) for
! tape io.  The configuration information was moved to a single place to ease
! starting cps in new locations like Houston or Bartlesville.  tpiocnfg parses
! a config file for tape io behavior. If no config file is available defaults
! are used that should work.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!
! This file only contains variable definitions, no functions.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY 
!
!     Date        Author       Description
!     ----        ------       -----------
! 12. 2007-05-10  Goodger      Add 3592HD.
! 11. 2005-09-12  Goodger      More changes for 3592.
! 10. 2005-03-24  Goodger      Add device 3592.
!  9. 2004-01-21  R Selzler    Resolve SGI compiler warnings
!  8. 2003-10-29  Goodger      Initialize issp (space flag) to zero at the
!                              beginning of the parsing loop.  The first
!                              drive was getting parsed correctly but the
!                              second one was off by a field, causing a
!                              problem parsing the Alaska drive table.
!  7. 2003-09-29  R.S.Day      Eliminated some spurious  print out
!  6. 2003-08-05  R.S.Day      Changed some config file default locations.
!                              Added server_hosts and robot_nodes to the
!                              config file parameters. Added 
!                              tpiocnfg_get_server_cnt,tpiocnfg_get_server_n,
!                              tpiocnfg_get_nth_entry
!                              Added host field to the drive table structure.
!  5. 2003-06-17  R.S.Day      Added drive_wait configuration parameter
!  4. 2003-06-09  R.S.Day      Corrected char array bug in 
!                              tpiocnfg_get_drive_table
!  3. 2003-05-20  R.S.Day      Internal array increased in tpiocnfg_parse_arr
!  2. 2003-02-27  R.S.Day      Initial version is a conversion of tapecnfg.c.
!                              Added configuration file support. Removed 
!                              old tapecnfg variables.
!  1. 2003-02-06  R.S.Day      Initial version. Conversion of tapecnfg.c
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
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


char TPIOCNFG_IDENT[100] =
"$Id: tpiocnfg.c,v 1.12 2007/05/11 13:59:59 Goodger beta sps $";


/*------------------------- start of data ---------------------------------*/
/*------------------------- start of data ---------------------------------*/
/*------------------------- start of data ---------------------------------*/

/*
 * Port for connection with tape server.
 * Use separate ports for different versions of the server.
 */

/* port for server parent process*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <assert.h>
#include <math.h>
#include <errno.h>
#include "tpiocnfg.h"
#include "cgetsys.h"

void tpiocnfg_initialize(TpioCnfg *cnfg_obj, char *config_file);
   /*
    * sets default values for TpioCnfg structure
    */
int  tpiocnfg_parse_arr(char *sp, char *ptrs[]);
   /*
    * gets n values from string sp "junk=(val1 val2 ...valn)"
    * return n or -1 if ther is an error.
    */

void tpiocnfg_destroy(TpioCnfg *cnfg_obj)
{
 int i;
 int nkeys;
 if(!cnfg_obj) return;

 nkeys = cnfg_obj->nkeys;
 for(i=0;i<nkeys;i++) {
   if(cnfg_obj->keys[i]) {
     free(cnfg_obj->keys[i]);
     cnfg_obj->keys[i]=NULL;
   }
 } 
 for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_3590];i++) {
   if(cnfg_obj->mt_3590_node[i]) {
     free(cnfg_obj->mt_3590_node[i]);
     cnfg_obj->mt_3590_node[i]=NULL;
   }
 }

 for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_3592];i++) {
   if(cnfg_obj->mt_3592_node[i]) {
     free(cnfg_obj->mt_3592_node[i]);
     cnfg_obj->mt_3592_node[i]=NULL;
   }
 }

 for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_3592HD];i++) {
   if(cnfg_obj->mt_3592HD_node[i]) {
     free(cnfg_obj->mt_3592HD_node[i]);
     cnfg_obj->mt_3592HD_node[i]=NULL;
   }
 }

 for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_LTO];i++) {
   if(cnfg_obj->mt_lto_node[i]) {
     free(cnfg_obj->mt_lto_node[i]);
     cnfg_obj->mt_lto_node[i]=NULL;
   }
 }

 for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_DLT];i++) {
   if(cnfg_obj->mt_dlt_node[i]) {
     free(cnfg_obj->mt_dlt_node[i]);
     cnfg_obj->mt_dlt_node[i]=NULL;
   }
 }
 for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_8MM];i++) {
   if(cnfg_obj->mt_8mm_node[i]) {
     free(cnfg_obj->mt_8mm_node[i]);
     cnfg_obj->mt_8mm_node[i]=NULL;
   }
 }
 for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_NR3590];i++) {
   if(cnfg_obj->mt_nr3590_node[i]) {
     free(cnfg_obj->mt_nr3590_node[i]);
     cnfg_obj->mt_nr3590_node[i]=NULL;
   }
 }
 for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_3480];i++) {
   if(cnfg_obj->mt_3480_node[i]) {
     free(cnfg_obj->mt_3480_node[i]);
     cnfg_obj->mt_3480_node[i]=NULL;
   }
 }
 i=0;
 while(cnfg_obj->disabled_lname[i]) {
   if(cnfg_obj->disabled_lname[i]) {
     free(cnfg_obj->disabled_lname[i]);
     cnfg_obj->disabled_lname[i]=NULL;
   }
   i++;
   cnfg_obj->ndisable=0;
 }
 i=0;
 while(cnfg_obj->server_hosts[i]) {
   if(cnfg_obj->server_hosts[i]) {
     free(cnfg_obj->server_hosts[i]);
     cnfg_obj->server_hosts[i]=NULL;
   }
   i++;
 }
 i=0;
 while(cnfg_obj->robot_nodes[i]) {
   if(cnfg_obj->robot_nodes[i]) {
     free(cnfg_obj->robot_nodes[i]);
     cnfg_obj->robot_nodes[i]=NULL;
   }
   i++;
 }
 free(cnfg_obj);
 cnfg_obj = NULL;

}

/*
 * set default values for tape IO parameters
 */
void tpiocnfg_initialize(TpioCnfg *cnfg_obj, char *config_file)
{ int  i,n,nkeys;
 /* INTEGER lib_level; */
  static char *keys[25] = {
   "drive_table_path=",
   "tapelib_path=",
   "tapelib_log=",
   "tapeop_path=",
   "robot_node=",
   "tapecat_node=",
   "tapecat_port=",
   "tapecat_log=",
   "catalog_path=",
   "lto_prog=",
   "lto_file=",
   "lto_devnam=",
   "3590_node=",
   "8mm_node=",
   "dlt_node=",
   "nr3590_node=",
   "lto_node=",
   "3480_node=",
   "disabled_lname=",
   "tpio_mode=",
   "drive_wait=",
   "server_hosts=",
   "robot_nodes=",
   "3592_node=",
   "3592HD_node="
  };

  if(!cnfg_obj) return;

  cnfg_obj->config_file[0]='\0';
  if(config_file) {
    if(strlen(config_file) > 0) strcpy(cnfg_obj->config_file,config_file);
  }

  nkeys = sizeof(keys)/sizeof(keys[0]);
  cnfg_obj->nkeys = nkeys;
  for(i=0;i<nkeys;i++) {
     cnfg_obj->keys[i] = (char *) malloc(strlen(keys[i])+1);
     strcpy(cnfg_obj->keys[i] , keys[i]);
  }
/*
 * set defaults for variables not in a config file
 */
  cnfg_obj->mt_num_types =
   sizeof(cnfg_obj->tape_type_node)/sizeof(cnfg_obj->tape_type_node[0]);


  strcpy(cnfg_obj->tape_type_node[TAPECNFG_3590],"3590_node");
  strcpy(cnfg_obj->tape_type_node[TAPECNFG_3592],"3592_node");
  strcpy(cnfg_obj->tape_type_node[TAPECNFG_3592HD],"3592HD_node");
  strcpy(cnfg_obj->tape_type_node[TAPECNFG_8MM],"8mm_node");
  strcpy(cnfg_obj->tape_type_node[TAPECNFG_DLT],"dlt_node");
  strcpy(cnfg_obj->tape_type_node[TAPECNFG_NR3590],"nr3590_node");
  strcpy(cnfg_obj->tape_type_node[TAPECNFG_LTO],"lto_node");
  strcpy(cnfg_obj->tape_type_node[TAPECNFG_3480],"3480_node");

  cnfg_obj->prod_tpport = 7736;
  cnfg_obj->beta_tpport = 7738;
  cnfg_obj->cust_tpport = 7740;

  tpiocnfg_set_ports(cnfg_obj, "PROD");
/*
  lib_level = cgetsys_library();
  if(lib_level==CGETSYS_BETALIB) {
    tpiocnfg_set_ports(cnfg_obj, "BETA");
  }
  if(lib_level==CGETSYS_ALPHALIB) {
    tpiocnfg_set_ports(cnfg_obj, "CUST");
  }
*/

/*
 * Following sets defaults for variables that ARE in a config file
 */

/*
 * tape catalogue parameters
 */
  cnfg_obj->tapecat_port=1098;
  strcpy(cnfg_obj->tapecat_node,"hoeplm03");
  strcpy(cnfg_obj->catalog_path,"/usr/app/cps_log/tape_cat.txt");
  strcpy(cnfg_obj->tapecat_log ,"/usr/app/cps_log/tapecat.log");

#ifdef sun
 strcpy(cnfg_obj->tapelib_path ,
 "/usr/app/vendors/sps/bin/sparc-sun-solaris/tapelib");
 strcpy(cnfg_obj->tapeop_path,
 "/usr/app/vendors/sps/bin/sparc-sun-solaris/tapeop");
#else
 strcpy(cnfg_obj->tapelib_path ,
 "/usr/app/vendors/sps/bin/ix86-intel-linux/tapelib");
 strcpy(cnfg_obj->tapeop_path,
 "/usr/app/vendors/sps/bin/ix86-intel-linux/tapeop");
#endif
 /*   comment out 
   location of tapelib and tapeop program 
 strcpy(cnfg_obj->tapelib_path , "/home/goodgkp/src/tape/tapelib/tapelib");
 strcpy(cnfg_obj->tapeop_path, "/home/dayrs/TAPEIO/tapeop"); */



 strcpy(cnfg_obj->tapelib_log , "/usr/app/cps_log/tapelib.log");


/*
 * node with the ibm 3494 (the robot)
  strcpy(cnfg_obj->robot_node,"hoepodi74");
 */
  for(i=0;i<4;i++) {
    cnfg_obj->robot_nodes[i]   = 0;
  }
  cnfg_obj->robot_nodes[0]   = (char *) malloc(16);
  strcpy(cnfg_obj->robot_nodes[0],"odi74");

/*
 * Location of device table file - set up by OilData
 * name = tpiocnfg_drive_table_path/hostname/drive_table
 */
  strcpy(cnfg_obj->drive_table_path,"/usr/odi/config/systems/");

/*
 * LTO tape drive parameters
 */
  strcpy(cnfg_obj->lto_devnam,"/dev/sg4");
  strcpy(cnfg_obj->lto_prog,"/usr/local/bin/mtx");
  strcpy(cnfg_obj->lto_file,"mtx_output");

/*
 * Nodes for specific drive types 
 */
  cnfg_obj->mt_3590_node[0]   = (char *) malloc(12);
  cnfg_obj->mt_3592_node[0]   = (char *) malloc(12);
  cnfg_obj->mt_3592HD_node[0]   = (char *) malloc(12);
  cnfg_obj->mt_8mm_node[0]    = (char *) malloc(12);
  cnfg_obj->mt_dlt_node[0]    = (char *) malloc(12);
  cnfg_obj->mt_nr3590_node[0] = (char *) malloc(12);
  cnfg_obj->mt_lto_node[0]    = (char *) malloc(12);
  cnfg_obj->mt_3480_node[0]   = (char *) malloc(12);

  strcpy(cnfg_obj->mt_3590_node[0] , "hoepodi74");
  strcpy(cnfg_obj->mt_3592_node[0] , "hoepodi91");
  strcpy(cnfg_obj->mt_3592HD_node[0] , "hoepodi92");
  strcpy(cnfg_obj->mt_8mm_node[0] ,  "hoepodi63");
  strcpy(cnfg_obj->mt_dlt_node[0] ,  "hoepodi64");
  strcpy(cnfg_obj->mt_nr3590_node[0],"poepsn61");
  strcpy(cnfg_obj->mt_lto_node[0] ,  "poeplt08");
  strcpy(cnfg_obj->mt_3480_node[0] , "hoepodi63");
  for(i=1;i<6;i++) {
    cnfg_obj->mt_3590_node[i]   = 0;
    cnfg_obj->mt_3592_node[i]   = 0; 
    cnfg_obj->mt_3592HD_node[i]   = 0; 
    cnfg_obj->mt_8mm_node[i]    = 0;
    cnfg_obj->mt_dlt_node[i]    = 0;
    cnfg_obj->mt_nr3590_node[i] = 0;
    cnfg_obj->mt_lto_node[i]    = 0;
    cnfg_obj->mt_3480_node[i]    = 0;
  }
  cnfg_obj->mt_num_nodes[TAPECNFG_3590]=1;
  cnfg_obj->mt_num_nodes[TAPECNFG_3592]=1;
  cnfg_obj->mt_num_nodes[TAPECNFG_3592HD]=1;
  cnfg_obj->mt_num_nodes[TAPECNFG_8MM] =1;
  cnfg_obj->mt_num_nodes[TAPECNFG_DLT] =1;
  cnfg_obj->mt_num_nodes[TAPECNFG_NR3590]=1;
  cnfg_obj->mt_num_nodes[TAPECNFG_LTO] =1;
  cnfg_obj->mt_num_nodes[TAPECNFG_3480]=1;


  cnfg_obj->ndisable    = 0;
  n=sizeof(cnfg_obj->disabled_lname)/sizeof(cnfg_obj->disabled_lname[0]);
  for(i=0;i<n;i++ ) {
    cnfg_obj->disabled_lname[i]    = 0;
  }

  cnfg_obj->drive_wait  = 36;

  n=sizeof(cnfg_obj->server_hosts)/sizeof(cnfg_obj->server_hosts[0]);
  for(i=0;i<n;i++) {
    cnfg_obj->server_hosts[i]   = 0;
  }

}

/*
 * allocate TpioCnfg structure
 * Initialize default values of structure
 * Get values from config_file if it is set.
 */
TpioCnfg *tpiocnfg_parse_config(char *config_file)
{ FILE *fp;
  char buf[4096];
  int  nrd;
  char *sp;
  int  i, nkeys;
  int  cnt;
  char line[160];
  TpioCnfg *cnfg_obj;
  char keyval[160];
  
  cnfg_obj = (TpioCnfg *) malloc(sizeof(TpioCnfg));
  if(!cnfg_obj) {
    printf("tpiocnfg_parse_config: error, malloc failed\n");
    return NULL;
  }

  /* set defaults */
  tpiocnfg_initialize(cnfg_obj, config_file);
  if(!config_file) return cnfg_obj;
  if(strlen(config_file) ==0) return cnfg_obj;

 cnt = 0;
 again:
  fp = fopen(config_file,"r");
  cnt++;
  if(fp) { /* check for file */
    buf[0]='\0';
    while(fgets(line,sizeof(line),fp)) {
      if(line[0]=='#') continue;
      if(strlen(line)==0) continue;
      strcat(buf,line);
    }
    nrd = strlen(buf);

  } else {
    sleep(1);
    if(cnt<2) goto again;
    printf("tpiocnfg_parse_config: open error for %s\n",config_file);
    return cnfg_obj;
  }

  fclose(fp);
  if(nrd ==0) {
    return cnfg_obj;
  }

  nkeys = cnfg_obj->nkeys;
  
  for(i=0;i<nkeys;i++) {
    sp = strstr(buf, cnfg_obj->keys[i]);
    if(sp) { /* use 1st occurence */
      
      sscanf(sp+strlen(cnfg_obj->keys[i]),"%s",keyval);
     
      switch (i) {
       case 0:
        strcpy(cnfg_obj->drive_table_path,keyval);
        break;
       case 1:
        strcpy(cnfg_obj->tapelib_path,keyval);
        break;
       case 2:
        strcpy(cnfg_obj->tapelib_log,keyval);
        break;
       case 3:
        strcpy(cnfg_obj->tapeop_path,keyval);
        break;
       case 4:
        /*strcpy(cnfg_obj->robot_node,keyval); */
        printf("tpiocnfg_parse_config: robot_node is obsolete\n");
        break;
       case 5:
        strcpy(cnfg_obj->tapecat_node,keyval);
        break;
       case 6:
        sscanf(keyval,"%d",&cnfg_obj->tapecat_port);
        break;
       case 7:
        strcpy(cnfg_obj->tapecat_log,keyval);
        break;
       case 8:
        strcpy(cnfg_obj->catalog_path,keyval);
        break;
       case 9:
        strcpy(cnfg_obj->lto_prog,keyval);
        break;
       case 10:
        strcpy(cnfg_obj->lto_file,keyval);
        break;
       case 11:
        strcpy(cnfg_obj->lto_devnam,keyval);
        break;

       case 12:
        cnfg_obj->mt_num_nodes[TAPECNFG_3590]=
        tpiocnfg_parse_arr(sp,cnfg_obj->mt_3590_node);
        break;
       case 13:
        cnfg_obj->mt_num_nodes[TAPECNFG_8MM]=
        tpiocnfg_parse_arr(sp,cnfg_obj->mt_8mm_node);
        break;
       case 14:
        cnfg_obj->mt_num_nodes[TAPECNFG_DLT]=
        tpiocnfg_parse_arr(sp,cnfg_obj->mt_dlt_node);
        break;
       case 15:
        cnfg_obj->mt_num_nodes[TAPECNFG_NR3590]=
        tpiocnfg_parse_arr(sp,cnfg_obj->mt_nr3590_node);
        break;
       case 16:
        cnfg_obj->mt_num_nodes[TAPECNFG_LTO]=
        tpiocnfg_parse_arr(sp,cnfg_obj->mt_lto_node);
        break;
       case 17:
        cnfg_obj->mt_num_nodes[TAPECNFG_3480]=
        tpiocnfg_parse_arr(sp,cnfg_obj->mt_3480_node);
        break;
       case 18:
        cnfg_obj->ndisable=
        tpiocnfg_parse_arr(sp,cnfg_obj->disabled_lname);
        break;
       case 19:
        strcpy(cnfg_obj->mode,keyval);
        tpiocnfg_set_ports(cnfg_obj, cnfg_obj->mode);
        break;
       case 20:
        sscanf(keyval,"%d",&cnfg_obj->drive_wait);
        break;
       case 21:
        tpiocnfg_parse_arr(sp,cnfg_obj->server_hosts);
        break;
       case 22:
        tpiocnfg_parse_arr(sp,cnfg_obj->robot_nodes);
        break;
       case 23:
        cnfg_obj->mt_num_nodes[TAPECNFG_3592]=
        tpiocnfg_parse_arr(sp,cnfg_obj->mt_3592_node);
        break;
       case 24:
        cnfg_obj->mt_num_nodes[TAPECNFG_3592HD]=
        tpiocnfg_parse_arr(sp,cnfg_obj->mt_3592HD_node);
        break;
      }
    }
  }

  return cnfg_obj;
}

/*
 * if tokens found replace the contents of ptrs
 */
int tpiocnfg_parse_arr(char *sp, char *ptrs[])
{ char *sp2;
  char *sp3;
  char card[120];
  int nch;
  int i,ntok;
  char *tokptr;
  char arr[12][120];

  i=0;
  while(ptrs[i]) { /* current count */
    i++;
  }


  card[0]='\0';
  sp2 = strstr(sp,"(");
  sp3 = strstr(sp,")");
  if(!sp2) return i;  /* returns ptrs unchanged */
  if(!sp3) return i;  /* returns ptrs unchanged */
  nch = sp3-sp2-1;
  memcpy(card,sp2+1,sp3-sp2-1);
  card[nch]='\0';    /* card = (tok1...tokn) */
  ntok=0;
  arr[ntok][ntok]='\0';
  tokptr = strtok(card, ", \t");
  while(tokptr) {
    arr[ntok][0]='\0';
    strcpy(arr[ntok],tokptr);
    if(ntok < 8
) {
      ntok++;
    } else {
      printf("tpiocnfg_parse_arr: oh ohh ntok >= 8\n");
    }
    tokptr = strtok(NULL, ", \t");
  }

 for(i=0;i<ntok;i++) { /* free then reallocate memory */
   if(ptrs[i]) free(ptrs[i]);
   ptrs[i] = (char *) malloc(strlen(arr[i])+1);
   strcpy(ptrs[i],arr[i]);
 }
 ptrs[ntok]=NULL;
 return ntok;

}

void tpiocnfg_print(TpioCnfg *cnfg_obj)
{ int i;
  if(!cnfg_obj) {
    printf("tpiocnfg_print: NULL object\n");
    return;
  }
  printf("#tpiocnfg_print: config_file =%s\n",cnfg_obj->config_file);
  printf("#tpiocnfg_print: tape mode   =%s\n",cnfg_obj->mode);
  printf("#tpiocnfg_print: drive_table =%s\n",cnfg_obj->drive_table_path);
  printf("#tpiocnfg_print: tapelib_path=%s\n",cnfg_obj->tapelib_path);
  printf("#tpiocnfg_print: tapelib_log =%s\n",cnfg_obj->tapelib_log);
  printf("#tpiocnfg_print: tapeop_path =%s\n",cnfg_obj->tapeop_path);
  printf("#tpiocnfg_print: tapecat_node=%s\n",cnfg_obj->tapecat_node);
  printf("#tpiocnfg_print: tapecat_port=%d\n",cnfg_obj->tapecat_port);
  printf("#tpiocnfg_print: tapecat_log =%s\n",cnfg_obj->tapecat_log);
  printf("#tpiocnfg_print: catalog_path=%s\n",cnfg_obj->catalog_path);
  printf("#tpiocnfg_print:     tpport  =%d\n",cnfg_obj->tpport);
  printf("#tpiocnfg_print: lto_prog    =%s\n",cnfg_obj->lto_prog);
  printf("#tpiocnfg_print: lto_file    =%s\n",cnfg_obj->lto_file);
  printf("#tpiocnfg_print: lto_devnam  =%s\n",cnfg_obj->lto_devnam);
  if(cnfg_obj->mt_3590_node) { i=0;
    while(cnfg_obj->mt_3590_node[i]) {
    printf("#tpiocnfg_print: mt_3590_node=%s\n",cnfg_obj->mt_3590_node[i]);
    i++;
    }
  }

  if(cnfg_obj->mt_3592_node) { i=0;
    while(cnfg_obj->mt_3592_node[i]) {
    printf("#tpiocnfg_print: mt_3592_node=%s\n",cnfg_obj->mt_3592_node[i]);
    i++;
    }
  }

  if(cnfg_obj->mt_3592HD_node) { i=0;
    while(cnfg_obj->mt_3592HD_node[i]) {
    printf("#tpiocnfg_print: mt_3592HD_node=%s\n",cnfg_obj->mt_3592HD_node[i]);
    i++;
    }
  }

  if(cnfg_obj->mt_8mm_node) { i=0;
    while(cnfg_obj->mt_8mm_node[i]) {
    printf("#tpiocnfg_print: mt_8mm_node=%s\n",cnfg_obj->mt_8mm_node[i]);
    i++;
    }
  }
  if(cnfg_obj->mt_dlt_node) { i=0;
    while(cnfg_obj->mt_dlt_node[i]) {
    printf("#tpiocnfg_print: mt_dlt_node=%s\n",cnfg_obj->mt_dlt_node[i]);
    i++;
    }
  }
  if(cnfg_obj->mt_nr3590_node) { i=0;
    while(cnfg_obj->mt_nr3590_node[i]) {
    printf("#tpiocnfg_print: mt_nr3590_node=%s\n",cnfg_obj->mt_nr3590_node[i]);
    i++;
    }
  }
  if(cnfg_obj->mt_lto_node) { i=0;
    while(cnfg_obj->mt_lto_node[i]) {
    printf("#tpiocnfg_print: mt_lto_node=%s\n",cnfg_obj->mt_lto_node[i]);
    i++;
    }
  }
  if(cnfg_obj->mt_3480_node) { i=0;
    while(cnfg_obj->mt_3480_node[i]) {
    printf("#tpiocnfg_print: mt_3480_node=%s\n",cnfg_obj->mt_3480_node[i]);
    i++;
    }
  }
  i=0;
  while(cnfg_obj->disabled_lname[i]) {
    printf("#tpiocnfg_print: disabled=%s\n",cnfg_obj->disabled_lname[i]);
    i++;
  }
  i=0;
  while(cnfg_obj->server_hosts[i]) {
    printf("#tpiocnfg_print: server_hosts=%s\n",cnfg_obj->server_hosts[i]);
    i++;
  }
  printf("#tpiocnfg_print: drive_wait=%d\n",cnfg_obj->drive_wait);
  i=0;
  while(cnfg_obj->robot_nodes[i]) {
    printf("#tpiocnfg_print: robot_nodes=%s\n",cnfg_obj->robot_nodes[i]);
    i++;
  }
 

}

int  tpiocnfg_drive_table_string(TpioCnfg *cnfg_obj)
{int maxsiz = 20;
 DrvTab dtab[20];
 int  ne;
 int  i;
 int  maxlen;
 char card[96];
 char stab[1024];
 ne = tpiocnfg_get_drive_table(cnfg_obj, maxsiz, dtab);
 stab[0]='\0';
 if(ne < 0) return -1;
 maxlen = sizeof(stab);
 for(i=0;i<ne;i++) {
   sprintf(card," lname=%s dvice=%s host=%s \n",
   dtab[i].lname,dtab[i].dvice,dtab[i].host);
   if(strlen(card) + strlen(stab) < maxlen-1) {
     strcat(stab,card);
   } else {
     break;
   }
 }
 return 0;
}

int  tpiocnfg_get_drive_table(TpioCnfg *cnfg_obj, int maxsiz, DrvTab *dtab)
{  FILE   *fp=0;
   char   dtable_path[120];
   int    i,n, issp=0, nfield=0, devno;
   int    status, lsiz=120;
   char   line[120];
   char   lname[8];
   char   pname[32];
   char   dvice[8];
   char   dev[24];
   int    ntargets;
   char   targets[12][16];
   char   target[16];
   int    nd;  /* number of drives on this host */
   char   host[32];
   if(!cnfg_obj) {
    printf("tpiocnfg_get_drive_table: NULL cnfg_obj\n");
    return -1;
   }
   status = gethostname(host,sizeof(host));
   if(status == -1) {
     printf("#tpiocnfg_get_drive_table: gethostname errno=%d\n",errno);
     perror("#tpiocnfg_get_drive_table:");
     return -1;
   }
   strcpy(dtable_path,cnfg_obj->drive_table_path);
   strcat(dtable_path,host);
   strcat(dtable_path,"/drive_table");
   fp = fopen(dtable_path,"r");
   if(!fp) {
     printf("#tpiocnfg_get_drive_table: error opening drive_table\n");
     printf("#tpiocnfg_get_drive_table: error drive_table=%s\n",
     dtable_path);
     return -1;
   }


   ntargets = 0;

   for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_DLT];i++) {
     if(strcmp(host,cnfg_obj->mt_dlt_node[i]) == 0 ) {
       strcpy(targets[ntargets],"MT_ISSTK2");  /* DLT */
       ntargets++;
       break;
     }
   }
   for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_8MM];i++) {
     if(strcmp(host,cnfg_obj->mt_8mm_node[i]) == 0) {
       strcpy(targets[ntargets],"MT_3480");     /* IBM 3480 round tape */
       ntargets++;
       strcpy(targets[ntargets],"MT_ISEXABYTE");/* 8MM */
       ntargets++;
       break;
     }
   }
   for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_3590];i++) {
     if(strcmp(host,cnfg_obj->mt_3590_node[i]) == 0 ) {
       strcpy(targets[ntargets],"MT_3590");   /* robot controlled 3590 */
       ntargets++;
       break;
     }
   }

   for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_3592];i++) {
     if(strcmp(host,cnfg_obj->mt_3592_node[i]) == 0 ) {
       strcpy(targets[ntargets],"MT_3592");   /* robot controlled 3592 */
       ntargets++;
       break;
     }
   }

   for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_3592HD];i++) {
     if(strcmp(host,cnfg_obj->mt_3592HD_node[i]) == 0 ) {
       strcpy(targets[ntargets],"MT_3592HD");   /* robot controlled 3592 High Density */
       ntargets++;
       break;
     }
   }

   for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_NR3590];i++) {
     if(strcmp(host,cnfg_obj->mt_nr3590_node[i]) == 0 ) {
       strcpy(targets[ntargets],"MT_NR3590"); /* non robot 3590 */
       ntargets++;
       break;
     }
   }
   for(i=0;i<cnfg_obj->mt_num_nodes[TAPECNFG_LTO];i++) {
     if(strcmp(host,cnfg_obj->mt_lto_node[i]) == 0) {
       strcpy(targets[ntargets],"MT_LTO");    /* LTO tape */
       ntargets++;
       break;
     }
   }
   if(strstr(host,"odi90") ) {
       strcpy(targets[ntargets],"MT_3590"); /* non robot 3590 */
       ntargets++;
   }

   lsiz = sizeof(line);
   nd = -1;
   while(fgets(line,lsiz,fp)) {

    for(n=0;n<ntargets;n++) {

      if(nd == maxsiz-1) {
        printf("#tpiocnfg_get_drive_table: ndrv=maxsiz reached\n");
        printf("#tpiocnfg_get_drive_table: drive_table parsing stopped\n");
        nd += 1;  /* incrment index by 1 for a count */
        return nd;
      }
      strcpy(target,targets[n]);
      


      if(strstr(line,target)) {
        devno = -1;
        sscanf(line,"%s %s\n",lname,dev);
        strcpy(pname,"/dev/");
        strcat(pname,dev);

        if(strcmp(target,"MT_3590")  ==0) strcpy(dvice,"3590");
        if(strcmp(target,"MT_3592")  ==0) strcpy(dvice,"3592");
        if(strcmp(target,"MT_3592HD")  ==0) strcpy(dvice,"3592HD");
        if(strcmp(target,"MT_3480")  ==0) strcpy(dvice,"3480");
        if(strcmp(target,"MT_NR3590")==0) strcpy(dvice,"NR3590");
        if(strcmp(target,"MT_ISSTK2")==0) strcpy(dvice,"DLT");
        if(strcmp(target,"MT_LTO")   ==0) strcpy(dvice,"LTO");
        if(strcmp(target,"MT_ISEXABYTE")==0) {
          strcpy(dvice,"8MM");
          devno=nd+1;
        }

        nfield=0;
        issp=0;
        for(i=0;i<strlen(line);i++) {
          if(isspace((int) line[i])) {
           if(issp==0) {
            nfield++;
            if(nfield==11) {
             sscanf(line+i,"%d",&devno);
             nd += 1;
            }
           }
           issp=1;
          } else {
           issp=0;
          }
        }

        dtab[nd].devno = devno;
        strcpy(dtab[nd].lname,lname);
        strcpy(dtab[nd].pname,pname);
        strcpy(dtab[nd].dvice,dvice);
        strcpy(dtab[nd].host ,host);
      }
    } /* end of targets loop */
   } /* end of fgets loop */
   nd += 1;  /* increment index by 1 for a count */
   return nd;
}

void tpiocnfg_set_ports(TpioCnfg *cnfg_obj, char *mode)
{  char version[32];
   int i;

   if(!cnfg_obj) return;
   strcpy(version,"PROD");
   if(mode) {
     if(strlen(mode) >  0 && strlen(mode)<32) {
       strcpy(version,mode);
     }
   }
   if(version[0]=='p' || version[0]=='P') strcpy(version,"PROD");
   if(version[0]=='b' || version[0]=='B') strcpy(version,"BETA");
   if(version[0]=='c' || version[0]=='C') strcpy(version,"CUST");
   if(version[0]=='a' || version[0]=='A') strcpy(version,"CUST");
   if(strcmp(version,"PROD")==0) {
     strcpy(cnfg_obj->mode , "PROD");
     cnfg_obj->tpport = cnfg_obj->prod_tpport;
     for(i=0;i<12;i++) {
      cnfg_obj->comm_ports[i] = 7781 + i;
     }
   }
   if(strcmp(version,"BETA")==0) {
     strcpy(cnfg_obj->mode , "BETA");
     cnfg_obj->tpport = cnfg_obj->beta_tpport;
     for(i=0;i<12;i++) {
       cnfg_obj->comm_ports[i] = 7761 + i;
     }
   }
   if(strcmp(version,"CUST")==0) {
     strcpy(cnfg_obj->mode , "CUST");
     cnfg_obj->tpport = cnfg_obj->cust_tpport;
     for(i=0;i<12;i++) {
       cnfg_obj->comm_ports[i] = 7741 + i;
     }
   }

}

void tpiocnfg_prdtable(int ndrv, DrvTab *dtable)
{ int i;
  for(i=0;i<ndrv;i++) {
    printf("#tpiocnfg_prdtable: lname=%s dvice=%s host=%s pname=%s devno=%d\n",
     dtable[i].lname,dtable[i].dvice,dtable[i].host,
     dtable[i].pname, dtable[i].devno);
  }
}

short tpiocnfg_get_tpport(TpioCnfg *cnfg_obj)
{ if(!cnfg_obj) return -1;
  return cnfg_obj->tpport;
}

int tpiocnfg_drive_wait(TpioCnfg *cnfg_obj)
{ if(!cnfg_obj) return -1;
  return cnfg_obj->drive_wait;
}

char *tpiocnfg_config_file(TpioCnfg *cnfg_obj)
{ if(!cnfg_obj) return 0;
  return cnfg_obj->config_file;
}

/* 
 * In order of decreasing precedence
 * look for TAPE_CONFIG_FILE environment variable
 * look for prefix_path/tpio_config.dat if prefix_path is set
 * look for /usr/app/vendors/sps/etc/tpio_config.dat
 * look for /home/dayrs/tape/tpio_config.dat
 * will return NULL if no file is available
 */
char *tpiocnfg_find_config_file(char *prefix_path)
{ static char config_file[120];
  char  *retptr=0;
  char  *sp;
  int   istat;

  sp = getenv("TAPE_CONFIG_FILE");
  if(sp) {
    istat = access(sp,F_OK|R_OK);
    if(istat==0) {
      strcpy(config_file,sp);
      retptr = config_file;
      return retptr;
    }
  }

  if(prefix_path) {
    if(strlen(prefix_path) >0) {
      strcpy(config_file,prefix_path);
      strcat(config_file,"tpio_config.dat");
    } else {
      strcpy(config_file,"/usr/app/vendors/sps/etc/tpio_config.dat");
    }
  } else {
    strcpy(config_file,"/usr/app/vendors/sps/etc/tpio_config.dat");
  /*
    strcpy(config_file,"/home/dayrs/tape/tpio_config.dat");
   */
  }
  istat = access(config_file,F_OK|R_OK);
  if(istat==0) {
    retptr = config_file;
    return retptr;
  }

  if(!retptr) { /* last chance */
    strcpy(config_file,"/home/dayrs/tape/tpio_config.dat");
    istat = access(config_file,F_OK|R_OK);
    if(istat==0) {
      retptr = config_file;
    }
    
  }

  return retptr;
}

int  tpiocnfg_get_server_cnt(TpioCnfg *cnfg_obj)
{
  int cnt=0;
  if(!cnfg_obj) return cnt;
  while(cnfg_obj->server_hosts[cnt]) {
    cnt++;
  }
  return cnt;
}

char *tpiocnfg_get_server_n(TpioCnfg *cnfg_obj, int n)
{/* counts from 0 */
  int cnt;
  static char *sp;
  sp = NULL;
  if(!cnfg_obj) return sp;
  if(n < 1 ) return sp;
  if(n > tpiocnfg_get_server_cnt(cnfg_obj) ) return sp;
  cnt = 0;
  while(cnfg_obj->server_hosts[cnt]) {
    cnt++;
    if(cnt==n) {
      sp = cnfg_obj->server_hosts[cnt-1];
      break;
    }
  }
  return sp;
}

int tpiocnfg_get_nth_entry(DrvTab *dtab, int nth, char *lname, char *dvice,
    char *host, char *pname, int *devno)
{
  lname[0]='\0';
  pname[0]='\0';
  dvice[0]='\0';
  host[0] ='\0';
  if(!dtab) return -1;
  if(nth < 0 ) return -1;
  if(strlen(dtab[nth-1].lname)>0) strcpy(lname, dtab[nth-1].lname);
  if(strlen(dtab[nth-1].pname)>0) strcpy(pname, dtab[nth-1].pname);
  strcpy(dvice, dtab[nth-1].dvice);
  strcpy(host , dtab[nth-1].host);
  *devno = dtab[nth-1].devno;
  return 0;
}
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
