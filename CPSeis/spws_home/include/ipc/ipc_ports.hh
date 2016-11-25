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
#ifndef IPC_PORTS_HH
#define IPC_PORTS_HH

#include "ipc/ipc_constants.hh"

// returns valid port numbers
class IpcPorts {
private:
  enum {
    USE_CURRENT_VALUE = -2             /* convenient argument flag         */
  };

public:
  IpcPorts ();                         /* Constructor                      */

  virtual ~IpcPorts ();                /* Destructor                       */

  int top                              /* Return the first port number     */
    (int kind = IPC::_CLIENT);         /*   given kind of socket           */

  int valid                            /* Return 0 if port number is bad   */
    (int port_number = USE_CURRENT_VALUE);/*   given index of interest     */

  int next                             /* Return next port number          */
    (int kind = IPC::_CLIENT);         /*   given kind of socket           */

  class IpcPort *port                  /* Return port object quick         */
    (int index = USE_CURRENT_VALUE);   /*   given index of interest        */

  class IpcPort *forcePort             /* Return port object               */
    (int index = USE_CURRENT_VALUE);   /*   given index of interest        */

private:
  int loop                             /* next port number                 */
    (int kind,                         /*   given kind of socket           */
     int begin = 0);                   /*   given begining index           */

  int validIndex                       /* Return 0 if port index is bad    */
    (int index);                       /*   given index of interest        */

  int index                            /* Return index                     */
    (int port_number);                 /*   given port number              */

  int number                           /* Return port number               */
    (int port_number);                 /*   given index                    */

  class IpcPort
    **_ports;                            /* array of valid ports           */

  int
    _current;                          /* current port index                */
};

#endif
