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
#ifndef IPC_SOCKET_HH
#define IPC_SOCKET_HH

#include "ipc/ipc_constants.hh"

// Base class to do interprocess communication via sockets

class IpcSocket {

public:
  virtual ~IpcSocket ();               /* Destructor                       */

  virtual int sClose ();               /* Close function                   */

  void resetOk ();                     /* Clear error condition            */

  int Ok ();                           /* return error status              */

  int isOpen ();                       /* return 0 if socket is closed     */

  int portNumber ();                   /* return port number               */

  class IpcPort *port ();              /* return server port object        */

  int kind ();                         /* return flag for client or server */

  int socketNumber ();                 /* return socket number             */

protected:
  IpcSocket                            /* Constructor                      */
    (class IpcPorts *ports,            /*   valid ports                    */
     int kind = IPC::_CLIENT,          /*   client or server               */
     int domain = IPC::_DOMAIN,        /*   IPC domain to use              */
     int type = IPC::_TYPE,            /*   type of IPC to use             */
     int protocol = IPC::_PROTOCOL);   /*   which protocol to use          */

  virtual int sOpen ();                /* Open function                    */

  struct sockaddr_in
    _socket_address;                   /* socket structure                 */

  class IpcPorts
    *_ports;                           /* Given port objects               */

  class IpcPort
    *_port;                            /* Server port object               */

  char
    _buffer[IPC::MAX_SOCKET_BUF];      /* io buffer                        */

  static int
    _debug;                            /* 0 not in debug mode              */

  int
    _socket,                           /* socket file data set number      */
    _open,                             /* 0 if socket is not open          */
    _kind,                             /* _CLIENT or _SERVER               */
    _ok;                               /* 0 if error has occured           */

private:
  int
    _domain,                           /* network domain                   */
    _type,                             /* type of socket to use            */
    _protocol;                         /* type of protocol to use          */

};

#endif
