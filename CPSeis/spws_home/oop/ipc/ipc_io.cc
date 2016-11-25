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
#include "ipc/ipc_io.hh"
#include "ipc/ipc_server.hh"
#include "ipc/ipc_ports.hh"
#include "ipc/ipc_clients.hh"
#include "ipc/ipc_constants.hh"
#include "ipc/ipc_packet.hh"
#include "ipc/ipc_informer.hh"
#include "ipc/str_tok.hh"

#include "sl/sl_app.hh"

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>

#define SIZE_INCR 10

static const char * const def_chan = "0";
int IpcIO::_debug = 0;

IpcIO::IpcIO () :
  _server           (0), // instance of a server class
  _server_id        (IPC::APPL_NOT_SET), // appl id of this appl
  _channel          (0), // channel over which this appl provides service
  _clients          (0), // instance of a clients class
  _server_hosts     (0), // array of hosts where service to this appl is avail
  _server_appls     (0), // array of appls that will serve this appl
  _server_chans     (0), // array of channels that will serve this appl
  _num_servers      (0), // number of servers
  _servers_size     (0), // size of servers array
  _data_recv        (0), // array for data received
  _data_sent        (0), // array for data sent
  _xt_work_proc_id  (0)  // Xt work procedure ID
{
  _informer = new IpcInformer ();
  _ports = new IpcPorts ();
}

IpcIO::~IpcIO ()
{
  delete _informer;
  delete _ports;

  if (_data_recv) free (_data_recv);
  if (_data_sent) free (_data_sent);
  if (_channel)   free (_channel);

  if (_server)    delete _server;
  if (_clients)   delete _clients;

  int k2;
  if (_num_servers) {
    for (k2 = 0; k2 < _num_servers; k2) {
      free (_server_hosts[k2]);
      free (_server_chans[k2]);
    }
    free (_server_hosts);
    free (_server_appls);
    free (_server_chans);
  }
}

IpcInformer *IpcIO::informer ()
{
  return _informer;
}

char *IpcIO::data ()
{
  return _data_recv;
}

int IpcIO::numServers ()
{
  return _num_servers;
}

void IpcIO::setApplAndChan (int server_id, char *channel)
{
  assert (!IPC::verifyApplication(_server_id) && !_channel &&
    IPC::verifyApplication(server_id) && channel);  // only one shot at this

  _server_id = server_id;

  _server = new IpcServer (_ports);
  _server->sOpen ();
  
  _channel = (char *)malloc (strlen(channel)+1);
  strcpy (_channel, channel);
}

void IpcIO::addServer (char *server_host, int server_appl, char *server_chan)
{
  // adjust size of list if necessary
  if (_servers_size == _num_servers) {
    char **tmp0 = (char **)malloc (_servers_size+SIZE_INCR);
    int   *tmp1 = (int   *)malloc (_servers_size+SIZE_INCR);
    char **tmp2 = (char **)malloc (_servers_size+SIZE_INCR);
    int k2;
    for (k2 = 0; k2 < _servers_size; k2++) {
      tmp0[k2] = _server_hosts[k2];
      tmp1[k2] = _server_appls[k2];
      tmp2[k2] = _server_chans[k2];
    }
    if (_servers_size) {
      free (_server_hosts);
      free (_server_appls);
      free (_server_chans);
    }
    _server_hosts = tmp0;
    _server_appls = tmp1;
    _server_chans = tmp2;
    _servers_size += SIZE_INCR;
  }

  // add to lists and establish defaults if necessary
  if (server_host) {
    _server_hosts[_num_servers] = (char *)malloc (strlen(server_host)+1);
    strcpy (_server_hosts[_num_servers], server_host);
  }
  else {
    _server_hosts[_num_servers]
      = (char *)malloc (IPC::MAX_HOSTNAME_LEN*sizeof(char)+1);
    if (!(gethostname(_server_hosts[_num_servers],
      IPC::MAX_HOSTNAME_LEN) == 0)) {
      // error occured doing uname
      if (_debug) perror ("IpcClient: No server Hostname");
      strcpy (_server_hosts[_num_servers],"");
    }
  }

  if (server_appl == IPC::APPL_NOT_SET) {
    _server_appls[_num_servers] = IPC::ANY_APPL;
  }
  else {
    _server_appls[_num_servers] = server_appl;
  }

  if (server_chan) {
    _server_chans[_num_servers] = (char *)malloc (strlen(server_chan)+1);
    strcpy (_server_chans[_num_servers], server_chan);
  }
  else {
    _server_chans[_num_servers] = (char *)malloc (strlen(def_chan)+1);
    strcpy (_server_chans[_num_servers], def_chan);
  }

  _num_servers++;
}

void IpcIO::addServers (char *servers)
{
  int count = resourceListCount (servers);

  int k2;
  char *server_str;

  for (k2 = 0; k2 < count; k2++) {
    server_str = parseResourceList (servers, k2);
    addServer (parseServerHost (server_str),
               parseServerAppl (server_str),
               parseServerChan (server_str));
  }

  _clients = new IpcClients (_ports, _server_hosts, _server_appls,
    _server_chans, _num_servers);
}

char *IpcIO::parseServerHost (char *server_str)
{
  return parseResourceList (server_str, HOST, ":");
}

int IpcIO::parseServerAppl (char *server_str)
{
  int retval;

  char *appl = parseResourceList (server_str, APPL, ":");
  if (!appl) {
    retval = IPC::APPL_NOT_SET;
  }
  else {
    retval = IPC::applIdFromString (appl);
  }
  return retval;
}

char *IpcIO::parseServerChan (char *server_str)
{
  return parseResourceList (server_str, CHAN, ":");
}

int IpcIO::serving ()
{
  int retval;

  retval = (_server && IPC::verifyApplication(_server_id) && _channel);

  return retval;
}

void IpcIO::serveClients ()
{
  if (_server && _server->dataAvailable()) {
    // data for someone are available
    char *tmp1 = 0, *tmp2 = 0;
    char *tmp0 = _server->receiveData ();
    if (tmp0) {
      // data were expected length
      IpcPacket *packet = new IpcPacket ();
      char *tmp1 = (char *)malloc ((IPC::MAX_SOCKET_BUF+1)*sizeof(char));
      char *tmp2 = (char *)malloc ((IPC::MAX_SOCKET_BUF+1)*sizeof(char));

      if (packet->parseWithProtocol(_channel,tmp0,tmp1)) {
	// encoded channel matches up with this one

	int found_appl;
	if (packet->protocolFound(IPC::stringFromApplId(_server_id),tmp1)) {
	  found_appl = 1;
	  packet->parseWithProtocol (IPC::stringFromApplId(_server_id),
	    tmp1, tmp2);
	}
	else if (packet->protocolFound(IPC::stringFromApplId(IPC::ANY_APPL),
          tmp1)) {
	  found_appl = 1;
	  packet->parseWithProtocol (IPC::stringFromApplId(IPC::ANY_APPL),
            tmp1, tmp2);
	}
	else {
	  found_appl = 0;
	}
	if (found_appl && (!_data_sent || strcmp(_data_sent,tmp2))) {
	  // data is for a server of this application type and did
	  //   not originate from this application

	  // Tell all informs that new data are about to be received
	  _informer->beforeIpc ();

	  // store new data locally
	  if (_data_recv) free (_data_recv);
	  _data_recv = (char *)malloc (strlen(tmp2)+1);
	  strcpy (_data_recv, tmp2);

	  // Test Test Test Test Test Test Test Test Test Test Test Test
	  fprintf (stderr, "%s\n", _data_recv);
	  // Test Test Test Test Test Test Test Test Test Test Test Test


	  // Tell all informs that new data have been received
	  _informer->afterIpc ();
	}
      }
      // don't leak anything
                 free (tmp0);
      if (tmp1 ) free (tmp1);
      if (tmp2 ) free (tmp2);
    }
  }
}

int IpcIO::sendData (char *data)
{
  int retval;

  if (_clients && _clients->Ok()) {
    if (_data_sent) free (_data_sent);
    _data_sent = (char *)malloc ((strlen(data)+1)*sizeof(char));
    strcpy (_data_sent, data);
    retval = _clients->sendData (_data_sent);
  }
  else {
    retval = 0;
  }
  return retval;
}

void IpcIO::mustGetSomeRest (float seconds)
{

  struct timeval tv;
  int whole_secs  = (int)floor ((double)seconds);
  int whole_usecs = (int)floor ((double)(seconds - whole_secs) * 1000000);
  tv.tv_sec  = whole_secs;  // # of seconds to sleep
  tv.tv_usec = whole_usecs; // # of microseconds to sleep
  select (1, NULL, NULL, NULL, &tv);
}

Boolean IpcIO::workProcedure (XtPointer obj)
{
  IpcIO *io = (IpcIO *)obj;
  if (io->serving()) {
    io->serveClients ();
  }
  else {
    io->mustGetSomeRest ();
  }
  return False; // continuously run this procedure everytime in the main loop
}

void IpcIO::workProc (void *obj)
{
  IpcIO *io = (IpcIO *)obj;
  if (io->serving()) {
    io->serveClients ();
  }
}

XtWorkProcId IpcIO::addWorkProcToMainLoop (SLApp *appl)
{
  // only one of these allowed in a Main Loop
  if (!_xt_work_proc_id) {
  // add an XtMainLoop work procedure enabling IPC server options
    _xt_work_proc_id
      = appl->addWorkProcToMainLoop ((XtWorkProc)IpcIO::workProcedure,
        (XtPointer)this);
  }
  return _xt_work_proc_id;
}

void IpcIO::removeWorkProcFromMainLoop ()
{
  // only remove if previously added
  //   this function should be called if IpcIO is created and deleted more
  //   than once within an application, however, it is intended that IpcIO be
  //   created once before the main class is instantiated, then passed to
  //   the constructor of the main class, and only deleted as the application
  //   goes out of scope. 
  if (_xt_work_proc_id) {
  // remove the XtMainLoop work procedure disabling IPC server options
    XtRemoveWorkProc (_xt_work_proc_id);
    _xt_work_proc_id = 0;
  }
}

int IpcIO::resourceListCount (char *list, const char *parser)
{
  int retval;

  StrTok *str_tok = new StrTok (list, parser);
  retval = str_tok->count ();
  delete str_tok;
  return retval;
}

// without altering list, return a specified substring in list.  it may be
//   delimited by a parser string.  for repeated parser strings, assume,
//   that empty string(s) should be returned
// unfortunately, strtok doesn't quite do the job, so use StrTok class.
char *IpcIO::parseResourceList (char *list, int which, const char *parser)
{
  char *retval;

  StrTok *str_tok = new StrTok (list, parser);
  const char *str = str_tok->get (which);
  if (str) {
    retval = strstr (list, str);
  }
  else {
    retval = 0;
  }
  delete str_tok;
  return retval;
}
