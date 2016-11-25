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
#ifndef NETENV_HH
#define NETENV_HH


#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <utime.h>
#include <stdio.h>
#include "tfio.h"
#include "tfdefs.h"
#include "net/hostlist.hh"

#define NET_PORT 6060

class NetEnv {


  public:
      enum NetStat {Bad = 0, Good = 1};

  private:

      char             *_hostname;
      NetStat           _net_status; 
      int               _net_socket;
      int               _port;
      FILE             *_net_input;
      static HostList   _host_list;
      

  protected:
      void socketConnect();
      int netGetGlobalData(char       *fname,
                            TF_Global *G,
                            int       *istat);
      int netReadData(char       *filename, 
                      int        *istat, 
                      IO_request *Cl, 
                      char        hd[],
                      char        tr[], 
                      float      *TASF);
      int netReadDataHdsliceByname(char   *filename, 
                                   float  *hd, 
                                   int     trnsps);
      int netAccess(char *fname, int mask);
      void netExpTilde(char *expstr, char *instr);
      void netExpFile(char *expstr, char *instr);
      time_t netModTime(char *fname);
      char **netDir(char          *dirname, 
                    int           *length, 
                    unsigned char *stat, 
                    char          *input_com);
      void checkStat(int stat);
      void checkStat(char *stat);
      void doConnection();

  public:
      NetEnv(char *hostname, int port= NET_PORT);
      virtual ~NetEnv();
      NetStat networkStatus(); 
      NetStat redoConnection();
      char *hostname();

      static NetEnv *getNetEnv(char *hostname, int port= NET_PORT);

       

      /*
       * The following functions are used instead of tfio.
       * If netenv is NULL then they are just pass through functions to tfio.
       */
      static int tfioGetGlobalData(NetEnv *netenv, char *, TF_Global *, int *);
      static int tfioReadDataHdsliceByname(NetEnv *netenv, 
                                           char   *filename, 
                                           float  *hd, 
                                           int     trnsps);
                                           
      static int tfioReadData(NetEnv *netenv,  
                              char *filename, 
                              int  *istat, 
                              IO_request *Cl, 
                              char hd[],
                              char tr[], 
                              float *TASF,
                              int   *lun,
                              int   open_file);
      /*
       * The following functions are used instead various 
       * file utility functions
       * If netenv is NULL then they are just pass through to the utility
       */
      static time_t modTime(NetEnv   *netenv, char *fname);
      static int    access(NetEnv    *netenv, char *fname,  int mask);
      static void   expTilde(NetEnv  *netenv, char *expstr, char *instr);
      static void   expFile(NetEnv   *netenv, char *expstr, char *instr);

      static char **dir(NetEnv         *obj, 
                        char           *fname, 
                        int            *length, 
                        unsigned char  *stat);

      static char **dirOfDirectories(NetEnv        *obj, 
                                     char          *dirname, 
                                     int           *length,
                                     unsigned char *stat );

};

#endif
