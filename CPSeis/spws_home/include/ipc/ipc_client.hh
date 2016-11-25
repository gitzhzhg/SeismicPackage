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
#ifndef IPC_CLIENT_HH
#define IPC_CLIENT_HH

#include "ipc/ipc_socket.hh"

#include <Xm/Xm.h>

// a socket client

typedef void Sigfunc (int); 

class IpcClient : public IpcSocket {

public:
  IpcClient                            /* Constructor                      */
    (class IpcPorts *ports,            /*   valid ports                    */
     char *server_hostname = 0);       /*   name of server host            */

  virtual ~IpcClient ();               /* Destructor                       */

  virtual int sOpen ();                /* Open function                    */

  virtual int sClose ();               /* Close function                   */

  static void sConnectAlarm            /* connect interrupt alarm function */
    (int signal_number);               /*   given an alarm signal number   */

  int serverAvailable ();              /* Rtns 0 if server not ready to wr */

  int sendData                         /* Send data to the server          */
    (char *data);                      /*   Data to send to server         */

  int connected ();                    /* Rtns 0 if not connected          */

private:
  int sTimedConnect                    /* connect to current port given    */
    (int usec);                        /*   # microseconds before timeout  */

  int sConnect ();                     /* connect to current port          */

  int putDataInPacket                  /* Put server data in packet        */
    (char *data);                      /*   data for server application    */

  Sigfunc *signal                      /* Returns current signal handler   */
    (int signal_number,                /*   sets up function for interrupt */
     Sigfunc *function);

  char
    *_server_hostname;                 /* server host name                 */

  int
    _connected;                        /* 0 if client is not connected     */

};

#endif
