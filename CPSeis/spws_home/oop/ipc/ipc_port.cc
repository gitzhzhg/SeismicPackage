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
#include "ipc/ipc_port.hh"
#include "ipc/ipc_socket.hh"

IpcPort::IpcPort (int number, IpcSocket *socket, int flag) :
  _number         (number),
  _flag           (flag),
  _server_socket  (0),
  _client_socket  (0)
{
  if (socket) {
    switch (socket->kind()) {
    case IPC::_SERVER:
      _server_socket = socket;
      break;
    case IPC::_CLIENT:
      _client_socket = socket;
      break;
    default:
      assert (0);
    }
  }
}

IpcPort::~IpcPort ()
{
}

int IpcPort::number ()
{
  return _number;
}

IpcSocket *IpcPort::socket (int kind)
{
  IpcSocket *retval;

  switch (kind) {
  case IPC::_SERVER:
    retval = _server_socket;
    break;
  case IPC::_CLIENT:
    retval = _client_socket;
    break;
  default:
    assert (0);
  }
  return retval;
}

int IpcPort::isAvailable (int kind)
{
  IpcSocket *socket;

  switch (kind) {
  case IPC::_SERVER:
    socket = _server_socket;
    break;
  case IPC::_CLIENT:
    socket = _client_socket;
    break;
  default:
    assert (0);
  }

  return _flag == AVAILABLE && !socket; // must not already have a socket
}

void IpcPort::setSocketTo (IpcSocket *socket)
{
  assert (socket);

  switch (socket->kind()) {
  case IPC::_SERVER:
    _server_socket = socket;
    break;
  case IPC::_CLIENT:
    _client_socket = socket;
    break;
  default:
    assert (0);
  }
}

void IpcPort::clearSocket (int kind)
{
  IpcSocket *socket;

  switch (kind) {
  case IPC::_SERVER:
    _server_socket = 0;
    break;
  case IPC::_CLIENT:
    _client_socket = 0;
    break;
  default:
    assert (0);
  }
}

void IpcPort::makeAvailable ()
{
  _flag = AVAILABLE;
}

void IpcPort::makeUnavailable ()
{
  _flag = DO_NOT_USE;
}
