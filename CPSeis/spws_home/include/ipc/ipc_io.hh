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
#ifndef IPC_IO_HH
#define IPC_IO_HH

#include <Xm/Xm.h>

// a socket based InterProcess Communication IO class

class IpcIO {

public:
  IpcIO ();                            /* Constructor                      */

  virtual ~IpcIO ();                   /* Destructor                       */

  class IpcInformer *informer ();      /* return an informer               */

  char *data ();                       /* return a data string             */

  int numServers ();                   /* Return number of servers         */

  void setApplAndChan                  /* Set application type ID (once!)  */
    (int server_id,                    /*   which application              */
     char *_channel);                  /*   channel number (like password) */

  void addServer                       /* Add server for this application  */
    (char *server_host,                /*   the server's hostname          */
     int server_appl,                  /*   the server's application ID    */
     char *server_chan);               /*   the server's channel           */

  void addServers                      /* Add a list of serving channels   */
    (char *servers);                   /*   resource list of servers       */

  char *parseServerHost                /* return server host name          */
    (char *server_str);                /*   server channel res str         */

  int parseServerAppl                  /* return server application ID     */
    (char *server_str);                /*   server channel res str         */

  char *parseServerChan                /* return server channel name       */
    (char *server_str);                /*   server channel res str         */

  int serving ();                      /* rtn 0 if not server for any appl */

  void serveClients ();                /* receive data from client appls   */

  void mustGetSomeRest                 /* sleep for non-integer seconds    */
    (float seconds = 0.1);             /*   number of float seconds        */

  int sendData                         /* send data to each of the clients */
    (char *data);                      /*   given data to send             */

  static Boolean workProcedure         /* Work Proc added to X main loop   */
    (XtPointer obj);                   /*   obj specified when registered  */

  static void workProc                 /* Work Proc added to X main loop   */
    (void *obj);                       /*   obj specified when registered  */

  XtWorkProcId addWorkProcToMainLoop   /* add a Work Proc to X main loop   */
    (class SLApp *appl);               /*   SLApp object                   */

  void removeWorkProcFromMainLoop ();  /* remove a Work Proc to X main loop*/

private:
  enum {
    HOST,                              /* Client's server host name        */
    APPL,                              /* Client's server application      */
    CHAN                               /* Client channel name              */
  };

  int resourceListCount                /* Convience resource list counter  */
    (char *list,                       /*   client server channel res str  */
     const char *parser = ",");        /*   token separator                */

  char *parseResourceList              /* Convience resource string parser */
    (char *list,                       /*   client server channel res str  */
     int which,                        /*   which token desired            */
     const char *parser = ",");        /*   token separator                */

  XtWorkProcId
    _xt_work_proc_id;                  /* Xt Work Proc ID added            */

  class IpcServer
    *_server;                          /* socket server                    */

  class IpcPorts
    *_ports;                           /* set of communication ports       */

  class IpcClients
    *_clients;                         /* set of client servers            */

  class IpcInformer
    *_informer;                        /* an IpcInformer object            */

  static int
    _debug;                            /* 0 not in debug mode              */

  char
    *_channel,                         /* channel for this application     */
    **_server_hosts,                   /* names of server hosts            */
    **_server_chans,                   /* channels of servers              */
    *_data_recv,                       /* server data stored after recving */
    *_data_sent;                       /* client data stored after sending */

  int
    _top_level_port,                   /* top level application type       */
    _server_id,                        /* application ID for this server   */
    *_server_appls,                    /* application IDs of servers       */
    _num_servers,                      /* # of servers to send requests to */
    _servers_size;                     /* size of servers array            */
};

#endif
