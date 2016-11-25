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
#include "ipc/ipc_server.hh"
#include "ipc/ipc_constants.hh"
#include "ipc/ipc_packet.hh"
#include "ipc/ipc_ports.hh"
#include "ipc/ipc_port.hh"

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/time.h>

#define MAX_BACKLOG 5

IpcServer::IpcServer (IpcPorts *ports) :
  IpcSocket (ports, IPC::_SERVER),
  _bound  (0)
{
}

IpcServer::~IpcServer ()
{
  if (_open) {
    sClose ();
  }
}

int IpcServer::sOpen ()
{
  if (IpcSocket::sOpen()) {
    // Use the wildcard address (permit data from all interfaces).
    int address = INADDR_ANY;
    memcpy (&_socket_address.sin_addr, &address, sizeof(long));

    // Bind the socket to the address.
    if (_ok = sBind () > 0) {
      assert (_port);
      _port->setSocketTo (this);
      // Listen for connections from clients.
      if (!(_ok = listen (_socket, MAX_BACKLOG) >= 0)) {
	if (_debug) perror ("listen");
      }
    }
  }
  return _open; // possible to be open but not ok
}

int IpcServer::sClose ()
{
  int retval;

  if (_port) _port->clearSocket (IPC::_SERVER);
  _bound = 0;

  retval = IpcSocket::sClose ();
  return retval;
}

int IpcServer::dataAvailable ()
{
  int retval;

  if (_open && _ok) {

    // Look for data from a client to read through the socket
    fd_set readfds;
    FD_ZERO (&readfds);     // zero out the bits in the file data set bitmask
    int fds = _socket;      // the only file data set is the socket
    int maxfdp1 = fds + 1;  // the maximum file data set is trivial
    FD_SET (fds, &readfds); // set the bit in the file data set bitmask

    struct timeval tv;
    tv.tv_sec  = 0;        // # of seconds to wait for data
    tv.tv_usec = 250000;   // # of microseconds to wait for data
    retval = (select (maxfdp1, &readfds, NULL, NULL, &tv)) >= 0;
    retval = retval > 0 && FD_ISSET (fds, &readfds);
  }
  else {
    retval = 0;
  }
  return retval;
}

// only reject data that is the wrong length
char *IpcServer::receiveData ()
{
  char *retval;

  char *tmp = receivePacket ();
  if (tmp) {
    IpcPacket *packet = new IpcPacket ();

    retval = (char *)malloc (strlen(tmp)+1);

    int len = packet->parseWithLength (tmp, retval);
    delete packet;

    if (!len) {
      free (retval);
      retval = 0;
    }
    free (tmp);
  }
  else {
    retval = 0;
  }
  return retval;
}

char *IpcServer::receivePacket ()
{
  assert (_open && _ok);

  char *retval;

  int len = sizeof (struct sockaddr_in);

  int socket;
  if (_ok =
      (socket = accept (_socket, (struct sockaddr *)&_socket_address,
#ifdef SGI64
      &len) >= 0)) { // SGI compiler prototype is errant
#else
      (socklen_t *)&len) >= 0)) {
#endif
    int not_finished = 1;
    int tlen = 0;
    retval = (char *)malloc (IPC::MAX_SOCKET_BUF+1);
    retval[0] = '\0'; // start with a zero length string;
    while (not_finished) {
      len = recv (socket, _buffer, IPC::MAX_SOCKET_BUF, 0);
      if (len <= IPC::MAX_SOCKET_BUF) {
	// maximum length was not exceeded for read buffer
	if (len < 0) {
	  // recv error occured try again!
	  if (_debug) perror ("recv");
	  not_finished = 0;
	  tlen = 0;
	}
	else if (len == 0) {
	  // no more data to receive
	  not_finished = 0;
	}
	else {
	  // data was received
	  tlen += len;
	  if (tlen <= IPC::MAX_SOCKET_BUF) {
	    // maximum length was not exceeded for return buffer
	    //   concatenate return buffer with read buffer
	    strncat (retval, _buffer, len);
	  }
	  else {
	    // exceeded return buffer
	    if (_debug) {
              perror ("IpcServer::receiveData: Exceeded return buffer");
	    }
	    not_finished = 0;
	    tlen = 0;
	  }
	}
      }
      else {
	// exceeded read buffer
	perror ("IpcServer::receiveData: Exceeded read buffer");
	not_finished = 0;
	tlen = 0;
      }
    }
    close (socket);
    if (tlen > 0) {
      retval[tlen] = '\0'; // make sure string is NULL terminated
    }
    else {
      free (retval);
      retval = 0;
    }
  }
  else {
    if (_debug) perror ("accept");
    retval = 0;
  }
  return retval;
}

int IpcServer::bound ()
{
  return _bound;
}

int IpcServer::sBind ()
{
  assert (_socket >= 0);

  int retval;
  int port_number;
  int len = sizeof (struct sockaddr_in);

  // try to find a port to establish as a server
  for (_bound = 0, port_number = _ports->top(IPC::_SERVER);
    !_bound && _ports->valid();) {
    _socket_address.sin_port = htons (port_number);
    if (!(_bound = (bind (_socket,(struct sockaddr *)&_socket_address,len)
      >= 0))) {
      // that port could not be bound, try another until success occurs or
      //   the number of valid ports have been exhausted!
      port_number = _ports->next (IPC::_SERVER);
    }
  }
  if (_bound) {
    _port = _ports->port (); // current port
    retval = 1;
  }
  else {
    // either a bind error or a port allocation error
    if (_debug) perror ("bind");
    _port = 0;
    retval = 0;
  }
  return retval;
}
