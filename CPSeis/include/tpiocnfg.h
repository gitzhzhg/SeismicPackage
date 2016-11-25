/****
!<CPS_v1 type="HEADER_FILE"/>
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
!                      C P S   H E A D E R   F I L E
!
! Name       : tpiocnfg.h
! Category   : io
! Written    : 2003-02-06   by: R.S.Day
! Revised    : 2007-05-03   by: Goodger
! Maturity   :beta
! Purpose    : Keep tape configuration information in a single place.
! Portability: Unix only
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
! Please see documentation in tpiocnfg.c.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                       HEADER FILE REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  6. 2007-05-03  Goodger      Add 3592HD.
!  5. 2005-09-12  Goodger      Add 3592.
!  4. 2003-09-29  R.S.Day      Changed some config file default locations.
!                              Added server_hosts and robot_nodes to the
!                              config file parameters. Added 
!                              tpiocnfg_get_server_cnt,tpiocnfg_get_server_n,
!                              tpiocnfg_get_nth_entry
!  3. 2003-06-17  R.S.Day      Added tpiocnfg_drive_wait variable and function
!                              prototype.
!  2. 2003-02-27  R.S.Day      Initial conversion of tapecnfg.h to
!                              tpiocnfg.h. Removed old tapecnfg externs.
!                              Added variables for LTO & NR3590 tape drive.
!                              Added typedef for TpioCnfg structure.
!                              Added typedef for DrvTab structure.
!                              Prototypes for tapecnfg_parse_config,
!                              tapecnfg_initialize, tapecnfg_destroy,
!                              tapecnfg_print, tapecnfg_get_drive_table and
!                              tapelib_prdtable.
!  1. 2003-02-06  R.S.Day      Initial version. Conversion of tapecnfg.h
!
!-------------------------------------------------------------------------------
!</history_doc>
**/

#ifndef _TPIOCNFG_H
#define _TPIOCNFG_H


enum { TAPECNFG_3590 = 0,
       TAPECNFG_8MM ,
       TAPECNFG_DLT,
       TAPECNFG_NR3590,
       TAPECNFG_LTO,
       TAPECNFG_3480,
       TAPECNFG_3592,
       TAPECNFG_3592HD};


/* Parse the drive_table file to obtain tape drive information.
 * Number of tape drives,ndrv, and dtable is set by
 * tpiocnfg_get_drive_table
 */
typedef struct DrvTab_ {
 char lname[8];     /* logical name such as MT45  */
 char pname[32];    /* physical name - used for open  */
 char dvice[8];     /* device type, e.g. 3590, DLT, etc. */
 char host[32];     /* host name that controls the device*/
 int  devno; } DrvTab;

typedef struct  _TpioCnfig {
  char  mode[8];         /* returns as PROD,BETA,CUST */
  char  config_file[120];/* decode style config file for tape IO */
  short prod_tpport;
  short beta_tpport;
  short cust_tpport;
  short tpport;
  short comm_ports[12];
  int   mt_num_types;          /* number of drive type supported */
  char  tape_type_node[8][16]; /* node type string. e.g. lto_node*/
  char  *server_hosts[9];      /* list of servers for client connections */

  int   nkeys;
  char *keys[32];              /* decode keyword= strings */
  char drive_table_path[120];
  char tapelib_path[120];
  char tapelib_log[120];
  char tapeop_path[120];
  char *robot_nodes[4];

  char tapecat_node[16];
  int  tapecat_port;
  char tapecat_log[120];
  char catalog_path[120];

  char lto_prog[120];
  char lto_file[120];
  char lto_devnam[120];

  int   mt_num_nodes[8];    /* count of each of the drive types */
  char *mt_3590_node[6];    /* aliases for 3590 host */
  char *mt_3592_node[6];    /* aliases for 3592 host */
  char *mt_8mm_node[6];     /* aliases for 8mm host */
  char *mt_dlt_node[6];     /* aliases for dlt host */
  char *mt_nr3590_node[6];  /* aliases for nr 3590 host */
  char *mt_lto_node[6];     /* aliases for lto host */
  char *mt_3480_node[6];    /* aliases for 3480 host */
  char *mt_3592HD_node[6];  /* aliases for 3592HD host */
  int  ndisable;
  char *disabled_lname[9];  /* remove drives from use:logical name */
  int  drive_wait;

} TpioCnfg;

TpioCnfg *tpiocnfg_parse_config(char *config_file);
    /*
     * read config_file and sets the TpioCnfg structure
     */
void tpiocnfg_destroy(TpioCnfg *cnfg_obj);
    /*
     * frees memory 
     */
void tpiocnfg_print(TpioCnfg *cnfg_obj);
    /*
     * prints configuration settings to stdout
     */
int  tpiocnfg_get_drive_table(TpioCnfg *cnfg_obj, int maxsiz, DrvTab *dtab);
    /*
     * returns the number of tape channels on local host
     * parses the drive_table for logical and physical device names
     */
void tpiocnfg_prdtable(int ndrv, DrvTab *dtab);
    /*
     * prints tape drive information to stdout
     */
int  tpiocnfg_get_server_cnt(TpioCnfg *cnfg_obj);
    /*
     * returns the number of tape servers listed in the tape config file
     */
char *tpiocnfg_get_server_n(TpioCnfg *cnfg_obj, int n);
   /*
    * return nth server host name( counting starts from one)
    * num is the returned number of host servers.
    * returns NULL if n is out of range or there is an error
    */
int tpiocnfg_get_nth_entry(DrvTab *dtab, int nth, char *lname, char *dvice,
    char *host, char *pname, int *devno);
   /*
    * return logical name and device type for the nth drive table entry
    */
short tpiocnfg_get_tpport(TpioCnfg *cnfg_obj);
   /*
    * return port value for parent-server and client
    */
int tpiocnfg_drive_wait(TpioCnfg *cnfg_obj);
   /*
    * return drive_wait parameter value(units of hours) 
    */
char *tpiocnfg_config_file(TpioCnfg *cnfg_obj);
   /*
    * return current config file setting
    */
char *tpiocnfg_find_config_file(char *prefix_path);
   /* - find a tape config file -
    * In order of decreasing precedence
    * look for TAPE_CONFIG_FILE environment variable
    * look for prefix_path/tpio_config.dat if prefix_path is set
    * look for /usr/app/vendors/sps/etc/tpio_config.dat
    * look for /home/dayrs/tape/tpio_config.dat
    * will return NULL if no file is available
    */
void tpiocnfg_set_ports(TpioCnfg *cnfg_obj, char *mode);
  /* 
   * - force port settings to PROD, BETA, or CUST/ALPHA settings
   */



#endif   /* _TPIOCNFG_H */
