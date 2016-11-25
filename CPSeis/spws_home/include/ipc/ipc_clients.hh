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
#ifndef IPC_CLIENTS_HH
#define IPC_CLIENTS_HH

// a group of clients

class IpcClients {

public:
  IpcClients                           /* Constructor                      */
    (
     class IpcPorts *ports,            /*   valid ports                    */
     char **server_hostnames,          /*   names of server hosts          */
     int *applications    ,            /*   application ids                */
     char **server_channels    ,       /*   names of server hosts          */
     int host_count);                  /*   number of hosts                */

  virtual ~IpcClients ();              /* Destructor                       */

  int Ok ();

  int sendData                         /* Send data to servers             */
    (char *data);                      /*   Data to send to server         */

private:
  int sendDataToHost                   /* Send data to servers on a host   */
    (char *host,                       /*   Host to send data to           */
     char *data);                      /*   Data to send to server         */

  class IpcPorts
    *_ports;                           /* valid port numbers               */

  class UniqueStringList
    *_unique_hosts;                    /* unique host structure array      */

  int
    _ok;                               /* 0 if error has occured           */
};

#endif
