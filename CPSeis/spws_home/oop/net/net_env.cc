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
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <strings.h>
#include "net/net_env.hh"
#include "bswap.h"
#include "cprim.h"
#include "tf_global.h"
#include "read_data.h"
#include "exptilde_crou.h"




HostList NetEnv::_host_list;



NetEnv::NetEnv(char *hostname, int port) : 
                      _net_status(Bad), _port(port), _hostname(NULL)
{
  _hostname= newstr(hostname);
  doConnection();
}

NetEnv::~NetEnv()
{
  if (_hostname) free(_hostname);
  /*
   * close socket here.
   */
  if (_net_status == Good) close(_net_socket); 
}


void NetEnv::doConnection()
{
  int                 stat;
  char                buff[1000];
  char               *uname=  getlogin();
  struct hostent     *hp;
  struct sockaddr_in  sin;

  if (_net_status == Good) close(_net_socket);

  if (uname) sprintf(buff, "%s\n", uname);
  else       sprintf(buff, "NeedsWork\n");

  hp = gethostbyname(_hostname);

  if (hp) {
      _net_socket= socket(AF_INET, SOCK_STREAM, 0);
      if (_net_socket >= 0) {
           sin.sin_family= AF_INET; 
           sin.sin_port=   htons(_port); 
           bcopy(hp->h_addr, (char*)&sin.sin_addr, hp->h_length);
           stat= connect(_net_socket, (sockaddr*)&sin, sizeof(sin) );
           if (stat >= 0) {
                _net_status= Good;
                _net_input= fdopen(_net_socket, "r"); 
                stat= send(_net_socket, buff, strlen(buff), 0);
                checkStat(stat);
           }
           else {
             perror("Netenv: could not connect");
           }
      }
      else {
        perror("Netenv: could not open socket");
      }

  }
  else {
        printf("Netenv: unknown hosts: %s\n", _hostname);
  }
}


NetEnv::NetStat NetEnv::redoConnection()
{
  doConnection();
  return _net_status;
}




NetEnv::NetStat NetEnv::networkStatus() { 
    if  (_net_status == Good) {
         char *command= "Alive\n";
         int stat= send(_net_socket, command, strlen(command), 0);
         checkStat(stat); 
    }
    if  (_net_status == Good) {
         char *result;
         char buff[100];
         result=  fgets(buff, 100, _net_input);
         checkStat(result); 
    }
    return _net_status;
 }


NetEnv *NetEnv::getNetEnv(char *hostname, int port)
{
  NetEnv *retval;
  void *x;
  char *host;
  HostNameElement *host_ele= _host_list.find(hostname, port, &x);
  if (host_ele) {
        retval= host_ele->getNetEnv();
  }
  else {
        retval= new NetEnv(hostname, port);
        _host_list.add(hostname, port, retval);
  }
  return retval;
}

char *NetEnv::hostname() {
   return _hostname;
}

void NetEnv::checkStat(int stat) {
   if (stat == -1) _net_status= Bad;
}

void NetEnv::checkStat(char *stat) {
   if (stat == NULL) _net_status= Bad;
}

static void get_data_prreq(char *s, char *filename, IO_request *req) {
 char line[200];
 if(!req) return;
 s[0]='\0';
 sprintf(line,"file=%s ,", filename);
 strcat(s,line);
 sprintf(line," ntot=%d , iskp=%d , ndo=%d , nskp=%d ,",
  req->ntot,req->iskp,req->ndo,req->nskp);
 strcat(s,line);
 sprintf(line," samp1=%d , nsamp=%d , sdec=%d ,",
  req->samp1,req->nsamp,req->sdec);
 strcat(s,line);
 sprintf(line," trnsps=%d , axis=%d , index=%d\n",
  req->trnsps,req->axis,req->index);
 strcat(s,line);
}




int NetEnv::netGetGlobalData(char      *fname,
                             TF_Global *G,
                             int       *istat)
{
  char *command= "GetGlobals\n";
  char *result;
  char buff[2000];
  int count= 0;
  int stat;
  
  *istat= 0;
  sprintf(buff, "%s\n", fname);

  stat = send(_net_socket, command, strlen(command), 0);
  checkStat(stat); 

  if  (_net_status == Good) {
         stat = send(_net_socket, buff, strlen(buff), 0);
         checkStat(stat); 
  }

  if (_net_status == Good) { 
         result= fgets(buff, 1000, _net_input);
         checkStat(result); 
  }

  if  (_net_status == Good) {
        sscanf(buff, "%d", &count);
        stat= fread(buff, count, 1, _net_input);
        if (stat == 0) _net_status= Bad;
  }

  buff[count]= '\0';
  if (_net_status == Good) {
     if (strstr(buff, "ERROR")) {
        sscanf(buff, "%*s %d", istat);
        *istat= -1;
     }
     else {   // success
        *istat= 0;
        get_global_tf_parsehd( G, fname, buff);
     }
  }

  if (_net_status == Bad) *istat= -1;
  
  return *istat;
}

int NetEnv::netReadData(char       *filename, 
                        int        *istat, 
                        IO_request *Cl, 
                        char        hd[],
                        char        tr[], 
                        float      *TASF)
{
  char *command= "ReadData\n";
  char *result;
  char  clstr[2000];
  char  buff[2000];
  int   count= 0;
  int   num_head;
  int   stat;
  int   num_tasf= 4 * (Cl->ntot + 1);

  *istat= 0;
  stat= send(_net_socket, command, strlen(command), 0);
  checkStat(stat); 

  if (_net_status == Good) { 
      get_data_prreq(clstr, filename, Cl);
      stat= send(_net_socket, clstr, strlen(clstr), 0);
      checkStat(stat); 
  }

  if (_net_status == Good) {
        result= fgets(buff, 1000, _net_input);
        sscanf(buff, "%d", &count);
        checkStat(result); 
  }

  if (_net_status == Good) {
       stat= fread(buff, count, 1, _net_input);
       if (stat == 0) _net_status= Bad;
  }
  buff[count]= '\0';

  if (_net_status == Good) {
       //I dont think this code is being used but if someone starts using
       //it they will need to deal with the hardwired number of headers.
       //M.L.Sherrill 06-2001
       printf("Line 239 of net_env.cc is using 64 for number of headers\n");
       num_head= Cl->ntot * 64 * 4;
       stat= fread(hd, num_head, 1, _net_input);
       if (stat == 0) _net_status= Bad;
       num_head /= 4;
       bswap(&num_head, (unsigned char *)hd); 
  }

  if (_net_status == Good) {
       stat= fread(tr, Cl->ntot * Cl->nsamp, 1, _net_input);
       if (stat == 0) _net_status= Bad;
  }
  
  if (_net_status == Good) {
       stat= fread(TASF, num_tasf, 1, _net_input);
       num_tasf /= 4;
       bswap(&num_tasf, (unsigned char *)TASF);
       if (stat == 0) _net_status= Bad;
  }

  if (_net_status == Bad) *istat= -1;
  *istat= 0; // ?

  return *istat;
}

int NetEnv::netReadDataHdsliceByname(char   *filename, 
                                     float  *hd, 
                                     int     trnsps)
{
  int stat= 0;
  char  command[500];
  char *result;
  char buff[2000];
  int count= 0;

  stat= 0;
  sprintf(command, "HdSlice %s %d\n", filename, trnsps);

  stat = send(_net_socket, command, strlen(command), 0);
  checkStat(stat); 

  if (_net_status == Good) {
       result=  fgets(buff, 1000, _net_input);
       checkStat(result); 
  }
  if (_net_status == Good) sscanf(buff, "%d", &count);

  if (_net_status == Good) {
       stat= fread(hd, count, 1, _net_input);
       count /= 4;
       bswap(&count, (unsigned char *)hd);
       if (stat == 0) _net_status= Bad;
  }

  return stat;
}

int NetEnv::netAccess(char *fname, int mask)
{
  char  command[100];
  char  buff[100];
  char *result;
  int   stat;
  int   file_stat= 0;

  sprintf(command, "Access %s  %d\n", fname, mask);
  stat= send(_net_socket, command, strlen(command), 0);
  checkStat(stat); 

  if (_net_status == Good) {
       result=  fgets(buff, 1000, _net_input);
       checkStat(result); 
  }

  if (_net_status == Good) sscanf(buff, "%d", &file_stat);
  return file_stat;
}

void NetEnv::netExpTilde(char *expstr, char *instr)
{
  char  command[800];
  char  buff[500];
  char *result;
  int   stat;

  sprintf(command, "ExpTilde %s\n", instr);
  stat= send(_net_socket, command, strlen(command), 0);
  checkStat(stat); 
  if (_net_status == Good) {
       result=  fgets(buff, 500, _net_input);
       checkStat(result); 
  }
  if (_net_status == Good) {
       buff[strlen(buff)-1]= '\0';
       strcpy(expstr, buff);
  }
  else {
       strcpy(expstr, instr);
  }
}

void NetEnv::netExpFile(char *expstr, char *instr)
{
  char  command[800];
  char  buff[500];
  char *result;
  int   stat;

  sprintf(command, "ExpFile %s\n", instr);
  stat= send(_net_socket, command, strlen(command), 0);
  checkStat(stat); 

  if (_net_status == Good) {
       result= fgets(buff, 1000, _net_input);
       checkStat(result); 
  }

  if (_net_status == Good) {
       buff[strlen(buff)-1]= '\0';
       strcpy(expstr, buff);
  }
  else {
       strcpy(expstr, instr);
  }

}

time_t NetEnv::netModTime(char *fname)
{
  time_t retval= 0;
  char   command[800];
  char   buff[1000];
  char  *result;
  int    stat;

  sprintf(command, "ModTime %s\n", fname);
  stat= send(_net_socket, command, strlen(command), 0);
  checkStat(stat); 

  if (_net_status == Good) {
       result= fgets(buff, 1000, _net_input);
       checkStat(result); 
  }
  if (_net_status == Good) sscanf(buff, "%d", &retval);

  return retval;
}

char **NetEnv::netDir(char          *dirname, 
                      int           *length, 
                      unsigned char *file_stat, 
                      char          *input_com )
{
 int     numfiles;
 char  **file_list= NULL;
 char    command[800];
 char    buff[1000];
  char  *result;
  int    stat;

 *file_stat= Good;  // decide when it is bad <<<<<<<<----------------
 sprintf(command, "%s %s\n", input_com, dirname);
 stat= send(_net_socket, command, strlen(command), 0);
 checkStat(stat); 

 if (_net_status == Good) {
       result=  fgets(buff, 1000, _net_input);
       checkStat(result); 
 }

 if (_net_status == Good) {
     sscanf(buff, "%d", &numfiles);
     if (numfiles > 0) {
         file_list= (char**)malloc( sizeof(char*) * numfiles);
         for(int i=0; (i<numfiles && _net_status == Good); i++) {
             result= fgets(buff, 1000, _net_input);
             checkStat(result);
             buff[strlen(buff) -1] = '\0';
             file_list[i]= newstr(buff); 
         } // end loop
     } // end if 
     *length= numfiles;
 }

 if (_net_status == Bad) {
      *length= 0;
 }

 return file_list;
}


int NetEnv::access(NetEnv *obj, char *fname, int mask)
{
  int stat;
  if (obj && obj->_net_status == Good) {
     stat= obj->netAccess(fname, mask);
  }
  else {
     stat= ::access((const char*)fname, mask);
  }
  return stat;

}



void NetEnv::expTilde(NetEnv *obj, char *expstr, char *instr)
{
  if (obj && obj->_net_status == Good) {
     obj->netExpTilde(expstr, instr);
  }
  else {
     exptilde_crou2(expstr, instr);
  }
}

void NetEnv::expFile(NetEnv *obj, char *expstr, char *instr)
{
  if (obj && obj->_net_status == Good) {
     obj->netExpFile(expstr, instr);
  }
  else {
     exp_file(expstr, instr);
  }
}

int NetEnv::tfioGetGlobalData(NetEnv    *obj,
                      char              *fname, 
                      TF_Global         *G, 
                      int               *istat)
{
  int stat;
  if (obj && obj->_net_status == Good) {
     stat= obj->netGetGlobalData(fname, G, istat); 
  }
  else {
     stat= get_global_data_(fname, G, istat);
  }

  return stat;
}

int NetEnv::tfioReadData(NetEnv     *obj,  
                         char       *filename, 
                         int        *istat, 
                         IO_request *Cl, 
                         char        hd[],
                         char        tr[], 
                         float      *TASF,
                         int        *lun,
                         int        open_file)
{

  int stat;
  if (obj && obj->_net_status == Good) {
     stat= obj->netReadData(filename, istat, Cl, hd, tr, TASF);
  }
  else {
     stat= read_data_(filename, istat, Cl, hd, tr, TASF, lun, open_file);
  }

  return stat;

}

int NetEnv::tfioReadDataHdsliceByname(NetEnv *obj, 
                                      char   *filename, 
                                      float  *hd, 
                                      int     trnsps)
{
  int stat;
  if (obj && obj->_net_status == Good) {
     stat= obj->netReadDataHdsliceByname(filename, hd, trnsps);
  }
  else {
     stat= read_data_hdslice_byname(filename, hd, trnsps);
  }

  return stat;
}


time_t NetEnv::modTime(NetEnv *obj, char *fname)
{
  time_t retval= 0;
  if (obj && obj->_net_status == Good) {
     retval= obj->netModTime(fname);
  }
  else {
        struct stat file_stats;
        if (stat(fname,&file_stats)==0)
              retval= file_stats.st_mtime;
  }

  return retval;
}


char **NetEnv::dir(NetEnv         *obj, 
                   char           *fname, 
                   int            *length, 
                   unsigned char  *stat)
{
  char **retval= NULL;
  if (obj && obj->_net_status == Good) {
     retval= obj->netDir(fname, length, stat, "Dir");
  }
  return retval;
}



char **NetEnv::dirOfDirectories(NetEnv        *obj, 
                                char          *dirname, 
                                int           *length,
                                unsigned char *stat )
{
  char **retval= NULL;
  if (obj && obj->_net_status == Good) {
     retval= obj->netDir(dirname, length, stat, "ListOfDir");
  }
  else
     *length= 0;
  return retval;
}
