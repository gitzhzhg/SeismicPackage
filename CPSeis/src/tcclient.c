/**
!<CPS_v1 type="PRIMITIVE"/>
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
!                        C P S   P R I M I T I V E
!
! Name       : tcclient (client side of tape catalogue)
! Category   : io
! Written    : 2000-07-24   by: R.S.Day
! Revised    : 2005-05-09   by: Tom Stoeckley
! Maturity   : production
! Purpose    : The client side API to the tape catalogue
! Portability: unix sockets
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION   
!
!        The client side API to the tape catalogue
!
!-------------------------------------------------------------------------------
!</descript_doc>
!
!<calling_doc>
!  See the calling doc in tapecat.c for the server side. The
!  calls below are the client side API.
!
!
!  int  tcclient_start()
!  return values:
!           a socket descriptor if the connection request is accepted
!           -1 if there is a fatal error.
!
!                                i        i 
!  void tcclient_send(int *s, char *to_serv)
!   s      = the socket descripter returned by tcclient_start
!   to_serv= A command string to pass to the server(see tapecat.c)
!
!                          i            o              i
!  int  tcclient_recv(int *s, char *from_serv, int *buff_len)
!   s        = The socket descripter returned by tcclient_start
!   from_serv= The server response to the to_serv command sent by
!              tcclient_send(see tapecat.c)
!   buff_len = Length of from_serv in chars.  buff_len must include room for
!              terminating '\0'.
!
!                                b
!  void tcclient_halt(int *s)
!   s      = The socket descripter returned by tcclient_start
!            sets s to -1 after closing the socket.
!
!</calling_doc>
!<history_doc>
!--------------------------------------------------------------------------
!                          REVISION HISTORY
!     Date        Author       Description
!     ----        ------       -----------
!  6. 2005-05-09  Stoeckley    Fix to compile with C++.
!  5. 2003-02-27  R.S.Day      Converted to tpiocnfg for tape configuration
!  4. 2002-10-10  Ed Schmauch  Get node and port from tapecnfg.
!  3. 2001-07-11  Ed Schmauch  Added buff_len argument to tcclient_recv.
!                              PRODUCTION.
!  2. 2001-05-14  Goodger      Improve error message in tcclient_send about
!                              writing to tape catalogue.
!                              Add and reorder tags.
!                              Add ident string.
!  1. 2000-09-28  R.S. Day     Initial version.
!--------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS 
!
! unix sockets
!
!-------------------------------------------------------------------------------
!</portability_doc>

!--------------------------"module" start ----------------------------------
!
**/
char tcclient_IDENT[100] =
"$Id: tcclient.c,v 1.6 2005/05/10 13:20:02 Stoeckley prod sps $";
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#ifdef sun
#include <sys/filio.h>
#endif
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include "c2f_interface.h"
#include "tpiocnfg.h"
 
#ifdef NEED_CAPITALS
#define tcclient_start TCCLIENT_START
#define tcclient_recv TCCLIENT_RECV
#define tcclient_send TCCLIENT_SEND
#define tcclient_halt TCCLIENT_HALT
#endif
#ifdef NEED_UNDERSCORE
#define tcclient_start tcclient_start_
#define tcclient_recv tcclient_recv_
#define tcclient_send tcclient_send_
#define tcclient_halt tcclient_halt_
#endif
 
#ifdef __cplusplus
extern "C" {
#endif

int  tcclient_start();
void tcclient_send(int *s, char *to_serv);
int  tcclient_recv(int *s, char *from_serv, int *buff_len);
void tcclient_halt(int *s);

int tcclient_start()
{/* returns socket number*/
 int clsock;
 char hostname[64];
 struct hostent *hp;
 struct sockaddr_in sin;
 char *sp;
 char config_file[160];
 TpioCnfg *cnfg_obj;
 clsock=-1;
 sp =  tpiocnfg_find_config_file(0);
 if(sp) strcpy(config_file,sp);
 cnfg_obj = tpiocnfg_parse_config(config_file);
 if(!cnfg_obj) {
    printf("tcclient_start: ERROR in  tpioclnt_parse_config\n");
    return -1;
 }

 printf("tcclient_start: server=%s\n",cnfg_obj->tapecat_node);

 gethostname(hostname, sizeof(hostname));
 if(( hp = gethostbyname(cnfg_obj->tapecat_node)) == NULL) {
   fprintf(stderr,"%s : host unknown\n",cnfg_obj->tapecat_node);
   return -1;
 } else {
   printf("tcclient_start: client=%s server=%s\n",hostname, hp->h_name);
   printf("tcclient_start: h_addrtype=%d\n",hp->h_addrtype);
   printf("tcclient_start: h_length=%d\n",hp->h_length);
 }

 if(( clsock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
   perror("tcclient_start: socket call failed");
   printf("tcclient_start: errno=%d\n",errno);
   tpiocnfg_destroy(cnfg_obj);
   return -1;
 }

 sin.sin_family = AF_INET;
 sin.sin_port = htons(cnfg_obj->tapecat_port);
 memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);
 if(connect(clsock,(struct sockaddr *) &sin, sizeof(sin)) < 0)  {
   perror("tcclient_start: connect");
   printf("tcclient_start: errno=%d\n",errno);
   tpiocnfg_destroy(cnfg_obj);
   return -1;
 } else {
   printf("tcclient_start: connection accepted\n\n");
 }

 tpiocnfg_destroy(cnfg_obj);
 return clsock;
}

void tcclient_send(int *s, char *to_serv)
{ char msg[256];
  int  stat;
  if(strlen(to_serv)>0) {
    strcpy(msg,to_serv);
    if(strstr(msg,"\n")==0) strcat(msg,"\n");
    stat = send(*s,msg,strlen(msg),0);
    if(stat<0) {
      perror("tcclient_send:");
      printf("tcclient_send: errno=%d",errno);
      printf("***********************************************************");
      printf("tcclient_send-->ERROR encountered writing to tape catalogue");
      printf("                Check tape catalogue");
      printf("***********************************************************");
    }
  }
}

int tcclient_recv(int *s, char *from_serv, int *buff_len)
{ 
 

  int stat;

  from_serv[0]='\0';
  if(*s<0) return 0;

  /*
   * *buff_len - 1 to allow room for the terminating '\0'.
   */
  stat = recv(*s, from_serv, *buff_len - 1, 0);
  if(stat > 0) from_serv[stat]='\0';
  return stat;
/*
  fprintf(stderr,"client_recv: recv error\n");
  usleep(1000);

  i=0;
  while(( c=fgetc(fpsock)) != EOF) {
   str[i]=c;
   i++;
   if(c=='\n') {
     str[i]='\0';
     str[511]='\0';
     strcpy(from_serv,str);
     printf("client_recv: str received=%s",str);
     return i;
   }
  }
 return 0;
*/
}

/* called by clients only! */
void tcclient_halt(int *s)
{if(*s<0) return;
 close(*s);
 *s= -1;
}

#ifdef __cplusplus
}
#endif

