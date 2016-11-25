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
#include "ipc/ipc_clients.hh"
#include "ipc/ipc_client.hh"
#include "ipc/ipc_packet.hh"
#include "ipc/ipc_constants.hh"
#include "ipc/ipc_ports.hh"
#include "ipc/ipc_client_list.hh"
#include "ipc/unique_int_list.hh"
#include "ipc/unique_string_list.hh"

#include <sys/socket.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>

#define INCR_SIZE 10

IpcClients::IpcClients (IpcPorts *ports,
  char **server_hostnames, int *applications, char **server_channels,
  int host_count) :
  _ports              (ports),
  _unique_hosts       (0)
{
  assert (ports && server_hostnames && applications && server_channels &&
    (host_count > 0));

  // determine a list of the unique hosts
  _unique_hosts = new UniqueStringList ();
  int k2;
  for (k2 = 0; k2 < host_count; k2++) {
    _unique_hosts->add (server_hostnames[k2]);
  }
  _ok = (_unique_hosts->count() > 0);

  // for each unique host, determine a set of unique applications
  void *p, *q;
  char *host;
  int *application;
  UniqueIntList *unique_applications = 0;
  for (host = _unique_hosts->top(&p); _ok && host;
    host = _unique_hosts->next(&p)) {
    unique_applications = new UniqueIntList ();
    for (k2 = 0; k2 < host_count; k2++) {
      if (!strcmp(host,server_hostnames[k2])) {
	unique_applications->add (applications[k2]);
      }
    }
    if (_ok = (unique_applications->count() > 0)) {
      _unique_hosts->attachList (unique_applications);
    }
    else {
      delete unique_applications;
    }
  }

  UniqueStringList *unique_channels;

  // for each unique host and application, determine a set of unique channels
  for (host = _unique_hosts->top(&p); _ok && host;
    host = _unique_hosts->next(&p)) {
    unique_applications = (UniqueIntList *)_unique_hosts->list ();
    for (application = unique_applications->top(&q); _ok && application;
      application = unique_applications->next(&p)) {
      unique_channels = new UniqueStringList ();
      for (k2 = 0; k2 < host_count; k2++) {
	if (!strcmp(host,server_hostnames[k2]) &&
            (*application == applications[k2])   ) {
	  unique_channels->add (server_channels[k2]);
        }
      }
      if (_ok = (unique_channels->count() > 0)) {
        unique_applications->attachList (unique_channels);
      }
      else {
	delete unique_channels;
      }
    }
  }
}

IpcClients::~IpcClients ()
{
  void *p, *q, *s;
  char *host;
  int *appl;
  UniqueIntList *appls;
  UniqueStringList *chans;

  // delete the nested lists
  for (host = _unique_hosts->top(&p); host; host = _unique_hosts->next(&p)) {
    if (appls = (UniqueIntList *)_unique_hosts->list ()) {
      for (appl = appls->top(&q); appl; appl = appls->next(&q)) {
	if (chans = (UniqueStringList *)appls->list()) {
	  delete chans;
	}
      }
      delete appls;
    }
  }
  delete _unique_hosts;
}

int IpcClients::Ok ()
{
  return _ok;
}

int IpcClients::sendData (char *data)
{
  int retval;

  if (data && strlen(data) > 0) {

    char *host;
    void *p, *q, *r;
    UniqueIntList *unique_applications;
    UniqueStringList *unique_channels;
    int *application;
    char *channel;
    int some_sent;
    char *appl_data
      = (char *)malloc ((IPC::MAX_SOCKET_BUF+1)*sizeof(char));
    char *chan_appl_data
      = (char *)malloc ((IPC::MAX_SOCKET_BUF+1)*sizeof(char));
    IpcPacket *packet = new IpcPacket;

    assert (_unique_hosts);
    // loop through all the hosts
    for (host = _unique_hosts->top(&p), some_sent = 0;
      host; host = _unique_hosts->next(&p)) {

      unique_applications = (UniqueIntList *)_unique_hosts->list ();
      assert (unique_applications);
      // loop through the unique applications for this host
      for (application = unique_applications->top(&q); application;
	application = unique_applications->next(&q)) {
	unique_channels = (UniqueStringList *)unique_applications->list ();
	assert (unique_channels);
	// loop through the unique channels for this host and application
	for (channel = unique_channels->top(&r); channel;
	  channel = unique_channels->next(&r)) {
	  // create a packet with the data using this application and
	  //   channel as protocol strings
	  assert (packet->makeWithProtocol(
	    IPC::stringFromApplId(*application), data, appl_data,
            IPC::MAX_SOCKET_BUF) &&
	    packet->makeWithProtocol(channel, appl_data, chan_appl_data,
            IPC::MAX_SOCKET_BUF));
	  some_sent = some_sent || sendDataToHost (host, chan_appl_data);
	}
      }
    }
    free (chan_appl_data);
    free (appl_data);
    delete packet;
    retval = some_sent;
  }
  else {
    retval = 0;
  }
  return retval;
}

int IpcClients::sendDataToHost (char *host, char *data)
{
  int retval;

  int port_number, maxfdp1, some_sent, data_sent;
  IpcClientList *client_list;
  IpcClient *client;
  fd_set writefds;
  struct timeval tv;
  void *p;

  // loop through all the valid port numbers to establish a client list
  client_list = new IpcClientList ();
  for (port_number = _ports->top(), maxfdp1 = -1, some_sent = 0;
    _ports->valid() && client_list->count() < IPC::MAX_ALLOWED_PORTS;
    port_number = _ports->next()) {
    // try to connect a client socket using current host and port
    client = new IpcClient (_ports, host);
    if (client->sOpen() && client->connected()) {
      // a client was succesfully created for this host
      if (client->socketNumber() > maxfdp1) {
	// the maximum file data set number increased
	maxfdp1 = client->socketNumber ();
      }
      // add the current client to the list of clients
      client_list->add (client);
    }
    else {
      // this port could not be connected, so delete the client
      delete client;
    }
  }
  if (client_list->count() > 0) {
    int some_found;
    // some clients were successfully created for this host
    maxfdp1++;
    // zero out the bitmask
    FD_ZERO (&writefds);
    // loop through each client and set up the bitmask for write
    for (client = client_list->top(&p); client;
	 client = client_list->next(&p)) {
      FD_SET (client->socketNumber(), &writefds);
    }
    // look for writeability on the set of client sockets using select
    tv.tv_sec = 0;
    tv.tv_usec = 250000; // attempt write for 0.25 seconds
    if (select(maxfdp1,NULL,&writefds,NULL,&tv) < 0) {
      perror ("select");
      some_found = 0;
    }
    else {
      some_found = 1;
    }
    if (some_found) {
      // loop through each client to send data
      for (client = client_list->top(&p); client;
	   client = client_list->next(&p)) {
	if (FD_ISSET(client->socketNumber(),&writefds)) {
	  data_sent = client->sendData (data);
	  if (data_sent) some_sent = 1;
	}
	client->sClose ();
      }
    }
    // loop through each client to close and delete
    for (client = client_list->top(&p); client;
      client = client_list->next(&p)) {
      delete client;
    }
    retval = some_sent;
  }
  else {
    retval = 0;
  }
  delete client_list;
  return some_sent;
}
