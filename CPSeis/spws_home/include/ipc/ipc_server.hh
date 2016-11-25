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
#ifndef IPC_SERVER_HH
#define IPC_SERVER_HH

#include "ipc/ipc_socket.hh"

#include <Xm/Xm.h>

// a socket server

class IpcServer : public IpcSocket {

public:
  IpcServer                            /* Constructor                      */
    (class IpcPorts *ports);           /*   valid ports                    */

  virtual ~IpcServer ();               /* Destructor                       */

  virtual int sOpen ();                /* Open function                    */

  virtual int sClose ();               /* Close function                   */

  int dataAvailable ();                /* Returns 0 if no data ready to rd */

  char *receiveData ();                /* Rtns malloc'ed string of data    */

  char *receivePacket ();              /* Rtns malloc'ed packet string     */

  int bound ();                        /* Rtns 0 if server not bound       */

private:
  int sBind ();                        /* Rtns 0 if bind not possible      */

  int
    _bound;                            /* 0 if not bound                   */
};

#endif
