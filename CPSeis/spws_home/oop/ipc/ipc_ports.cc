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
#include "ipc/ipc_ports.hh"
#include "ipc/ipc_port.hh"

IpcPorts::IpcPorts () :
  _current  (IPC::UNSUPPORTED_PORT)
{
  // create the ports
  _ports = new IpcPort *[IPC::MAX_ALLOWED_PORTS];

  int k2;
  for (k2 = 0; k2 < IPC::MAX_ALLOWED_PORTS; k2++) {
    _ports[k2] = 0;
  }
}

IpcPorts::~IpcPorts ()
{
  int k2;
  for (k2 = 0; k2 < IPC::MAX_ALLOWED_PORTS; k2++) {
    if (_ports[k2]) delete _ports[k2];
  }
  delete [] _ports;
}

// return the first available port number (may not be valid)
int IpcPorts::top (int kind)
{
  return loop (kind);
}

int IpcPorts::valid (int port_number)
{
  int retval;

  if (port_number == USE_CURRENT_VALUE) {
    retval = validIndex (_current);
  }
  else {
    retval = port_number != IPC::UNSUPPORTED_PORT            &&
             validIndex (port_number-IPC::PORT_NUMBER_OFFSET   );
  }
  return retval;
}

// return next available port number (may not be valid)
int IpcPorts::next (int kind)
{
  return loop (kind, _current+1);
}

// quick port number object with no validation
IpcPort *IpcPorts::port (int index)
{
  if (index == USE_CURRENT_VALUE) index = _current;
  return _ports[index];
}

// return port number object with a bit of validation
IpcPort *IpcPorts::forcePort (int index)
{
  IpcPort *retval;

  if (index == USE_CURRENT_VALUE) index = _current;

  if (!validIndex(index)) {
    retval = 0;
  }
  else {
    if (!_ports[index]) {
      _ports[index] = new IpcPort (number(index));
    }
    retval = _ports[index];
  }
  return retval;
}

// return next available port number after begin index (the returned
//   port number may not be valid). this function resets current.
int IpcPorts::loop (int kind, int begin)
{
  int retval;

  int k2;
  for (k2 = begin, _current = IPC::UNSUPPORTED_PORT;
    k2 < IPC::MAX_ALLOWED_PORTS; k2++) {
    if (forcePort(k2)->isAvailable(kind)) {
      _current = k2;
      k2 = IPC::MAX_ALLOWED_PORTS;
    }
  }
  if (validIndex(_current)) {
    retval = port(_current)->number ();
  }
  else {
    retval = IPC::UNSUPPORTED_PORT;
  }
  return retval;
}

int IpcPorts::validIndex (int index)
{
  if (index == USE_CURRENT_VALUE) {
    index = _current;
  }

  return index != IPC::UNSUPPORTED_PORT  &&
         index >  -1                     &&
         index < IPC::MAX_ALLOWED_PORTS    ;
}

int IpcPorts::index (int port_number)
{
  return port_number - IPC::PORT_NUMBER_OFFSET;
}

int IpcPorts::number (int index)
{
  return index + IPC::PORT_NUMBER_OFFSET;
}
