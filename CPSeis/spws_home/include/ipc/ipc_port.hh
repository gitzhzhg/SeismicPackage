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
#ifndef IPC_PORT_HH
#define IPC_PORT_HH

#include "ipc/ipc_constants.hh"

// stores ports

class IpcPort {

public:
  enum {                               /* possilbe flag settings           */
    AVAILABLE,                         /*   this port number is available  */
    DO_NOT_USE                         /*   this port is not available     */
  };

  IpcPort                              /* Constructor                      */
    (int number,                       /*   port number                    */
     class IpcSocket *socket = 0,      /*   socket pointer                 */
     int flag = AVAILABLE);            /*   flag                           */

  virtual ~IpcPort ();                 /* Destructor                       */

  int number ();                       /* Rtns the port number             */

  class IpcSocket *socket              /* Rtns socket pointer              */
    (int kind = IPC::_CLIENT);         /*   given kind of socket           */

  int isAvailable                      /* Rtns 0 if port not available     */
    (int kind = IPC::_CLIENT);         /*   given kind of port             */

  void setSocketTo                     /* Set socket to                    */
    (class IpcSocket *socket);         /*   a valid socket pointer         */

  void clearSocket
    (int kind = IPC::_CLIENT);         /*   given kind of port             */

  void makeAvailable ();               /* Set port flag to AVAILABLE       */

  void makeUnavailable ();             /* Set port flag to DO_NOT_USE      */

private:
  class IpcSocket
    *_server_socket,                   /* a server socket pointer          */
    *_client_socket;                   /* a client socket pointer          */

  int
    _number,                           /* given port number                */
    _flag;                             /* flag                             */
};

#endif
