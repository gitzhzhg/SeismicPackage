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
#include "ipc/ipc_client.hh"
#include "ipc/ipc_constants.hh"
#include "ipc/ipc_packet.hh"
#include "ipc/ipc_ports.hh"
#include "ipc/ipc_port.hh"

#include <netinet/in.h>
#include <netdb.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>

IpcClient::IpcClient (IpcPorts *ports, char *server_hostname) :
  IpcSocket (ports),
  _server_hostname  (0),
  _connected        (0)
{
  assert (server_hostname);

  _server_hostname = server_hostname; // count on the pointer always being
  _ok = (strlen(_server_hostname) > 0);
}

IpcClient::~IpcClient ()
{
  if (_open) sClose ();
}

int IpcClient::sOpen ()
{
  if (IpcSocket::sOpen()) {

    // Look up host's network address.
    struct hostent *hp;
    if (_ok = (hp = gethostbyname(_server_hostname)) != NULL) {

      // Create the address of the server.
      memcpy (&_socket_address.sin_addr, hp->h_addr_list[0], hp->h_length);

      // Connect this client socket to the server
      if (_ok = sTimedConnect(250000) > 0) {
	assert (_port);
	_port->setSocketTo (this);
      }
    }
    else {
      if (_debug) perror ("IpcClient::sOpen: Could not get server Hostname");
    }
  }
  return _open;  //  possible to be open but not ok
}

int IpcClient::sClose ()
{
  int retval;

  if (_port) _port->clearSocket (IPC::_CLIENT);
  _connected = 0;

  retval = IpcSocket::sClose ();
  return retval;
}

void IpcClient::sConnectAlarm (int signal_number)
{
  return; //  connect will return an error of EINTR
}

// useful when only one server will do, otherwise see IpcClients
int IpcClient::serverAvailable ()
{
  assert (_socket > 0);

  int retval;

  if (!_connected) sConnect ();

  if (_ok && _connected) {
    // Successfully connected to a listening server
    // Look for ability to write to the server through the socket
    fd_set writefds;
    FD_ZERO (&writefds);     // zero out the bits in the file data set bitmask
    int fds = _socket;       // the only file data set is the socket
    int maxfdp1 = fds + 1;   // the maximum file data set is trivial
    FD_SET (fds, &writefds); // set the bit in the file data set bitmask

    struct timeval tv;
    tv.tv_sec  = 0;          // # of seconds to wait for server
    tv.tv_usec = 250000;     // # of microseconds to wait for server

    retval = (select (maxfdp1, NULL, &writefds, NULL, &tv)) >= 0;
    retval = retval > 0 && FD_ISSET (fds, &writefds);
  }
  return retval;
}

int IpcClient::sendData (char *data)
{
  assert (_open && _ok);

  if (strlen(data)) {
    // Pack the data into the _buffer & copy the _buffer to the socket.
    if (putDataInPacket(data)) {
      _ok = (send (_socket, _buffer, strlen(_buffer), 0) >= 0);
      if (!_ok && _debug) {
	perror ("send");
      }
    }
  }
  return _ok;
}

int IpcClient::sTimedConnect (int usec)
{
  int retval;

  Sigfunc *signal_function;

  signal_function = signal (SIGALRM, IpcClient::sConnectAlarm);

  if (ualarm(usec,0) != 0) {
    perror ("IpcClient::sTimedConnect: alarm was already set");
  }

  if (!(retval = sConnect())) {
    // sConnect returned without success
    if (errno == EINTR) {
      // it was an interrupt so assume that a time out occured
      errno = ETIMEDOUT;
    }
    if (_debug && !_ok) perror ("IpcClient::sConnect: Client !OK");
    else if (_debug) perror ("connect");
  }
  ualarm (0, 0);                          // turn off alarm
  signal (SIGALRM, signal_function);   // restore previous signal handler
  return retval;
}

int IpcClient::connected ()
{
  return _connected;
}

// Warning: assumes that the current port number in _ports is to be used
int IpcClient::sConnect ()
{
  int retval;

  IpcPort *port = _ports->port (); // current port

  _socket_address.sin_port = htons (port->number());

  int len = sizeof (struct sockaddr_in);

  _connected
    = (connect(_socket,(struct sockaddr *)&_socket_address,len) >= 0);
  if (_ok && _connected) {
    _port = port;
    retval = 1;
  }
  else {
    _port = 0;
    retval = 0;
  }
  return retval;
}

// put the given data into the internal socket _buffer
int IpcClient::putDataInPacket (char *data)
{
  int retval;

  IpcPacket *packet = new IpcPacket ();
  retval = packet->makeWithLength (data, _buffer, IPC::MAX_SOCKET_BUF);
  delete packet;
  return retval;
}

Sigfunc *IpcClient::signal (int signal_number, Sigfunc *function)
{
  Sigfunc *retval;

  struct sigaction act, oact;

  act.sa_handler = function;
  sigemptyset (&act.sa_mask);
  act.sa_flags = 0;
  if (signal_number == SIGALRM) {
#ifdef SA_INTERRUPT
    act.sa_flags |= SA_INTERRUPT;  /* SunOS 4.x */
#endif
  }
  else {
#ifdef SA_RESTART
    act.sa_flags |= SA_RESTART;      /* SRV4, 4.4BSD */
#endif
  }
  if (sigaction (signal_number, &act, &oact) < 0) {
    retval = SIG_ERR;
  }
  else {
    retval = oact.sa_handler;
  }
  return retval;
}
