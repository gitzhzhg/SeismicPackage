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
#include "ipc/ipc_socket.hh"
#include "ipc/ipc_ports.hh"
#include "ipc/ipc_port.hh"

#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

int IpcSocket::_debug = 0;

IpcSocket::IpcSocket (IpcPorts *ports, int kind, int domain, int type,
  int protocol) :
  _ports        (ports),
  _kind         (kind),
  _domain       (domain),
  _type         (type),
  _protocol     (protocol),
  _open         (0),
  _socket       (0),
  _port         (0),
  _ok           (1)
{
  assert (_ports);
}

IpcSocket::~IpcSocket ()
{
  assert (!_open);
}

int IpcSocket::sOpen ()
{
  assert (!_open);

// Create the socket.
  if (_ok) {
    _socket = socket (_domain, _type, _protocol);
    if (_ok = (_socket >= 0)) {

    // Create the address of the server.
      memset (&_socket_address, 0, sizeof(struct sockaddr_in));
      _socket_address.sin_family = _domain;
      _open = 1;
    }
    else {
      if (_debug) perror ("socket");
    }
  }
  return _open; // possible to be open but not ok
}

int IpcSocket::sClose ()
{
  assert (_open);

  close (_socket);
  _open = 0;
  return _ok;
}

void IpcSocket::resetOk ()
{
  _ok = 1;
}

int IpcSocket::Ok ()
{
  return _ok;
}

int IpcSocket::isOpen ()
{
  return _open;
}

int IpcSocket::portNumber ()
{
  int retval;

  if (_port) {
    retval = _port->number ();
  }
  else {
    retval = IPC::UNSUPPORTED_PORT;
  }
  return retval;
}

IpcPort *IpcSocket::port ()
{
  return _port;
}

int IpcSocket::kind ()
{
  return _kind;
}

int IpcSocket::socketNumber ()
{
  return _socket;
}
