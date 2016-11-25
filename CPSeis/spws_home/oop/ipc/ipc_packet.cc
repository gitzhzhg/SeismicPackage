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
#include "ipc/ipc_packet.hh"
#include "ipc/ipc_constants.hh"

#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// To pack and unpack packets containing front-end lengths and or protocols

IpcPacket::IpcPacket ()
{
}

IpcPacket::~IpcPacket ()
{
}

// It is assumed that packet has been created to have a max_packet_size+1
//   character string so as to accomodate the '\0' at the end
int IpcPacket::makeWithProtocol (char *protocol, char *data, char *packet,
  int max_packet_size)
{
  int retval;

  char comma[2];

  if (strlen(protocol)+1+strlen(data) < max_packet_size) {
    strcpy (packet, protocol);
    strcpy (comma, ",");
    strcat (packet, comma);
    strcat (packet, data);
// to make sure that packet is NULL terminated
    packet[strlen(packet)] = '\0';
    retval = strlen (packet);
  }
  else {
    retval = 0;
  }
  return retval;
}

int IpcPacket::makeWithLength (char *data, char *packet, int max_packet_size)
{
  int retval = 0; // actual length of packet

  // make a conservative estimate of the packet length
  //   notice:  packet length does not include '\0' at the end of the string
  // packet will be len,data\0
  int try_len = decimalStrLen (strlen(data)+1) + 1 + strlen (data);

  // attemp to build up the packet
  char *len_text = (char *)malloc (decimalStrLen(try_len)+1);
  sprintf (len_text, "%d", try_len);
  if (retval = makeWithProtocol(len_text,data,packet,max_packet_size)) {
    // size of packet is OK, check the test length against the actual length
    if (retval != try_len) {
      // the conservative estimate missed the mark, make an adjustment
      try_len = retval;
      free (len_text);
      len_text = (char *)malloc (decimalStrLen(try_len)+1);
      sprintf (len_text, "%d", try_len);
      if (retval = makeWithProtocol(len_text,data,packet,max_packet_size)) {
	// size of packet is still OK, insist the test length is the
        //   actual length!
	assert (try_len == retval);
	free (len_text);
      }
    }
  }
  return retval;
}

int IpcPacket::parseWithProtocol (char *protocol, char *packet, char *data)
{
  int retval;

  char *tmp = strtok (packet, ",");
  if (!IPC::strcmpCaseInsensitive(tmp,protocol)) {
    char *tmp1 = &packet[strlen(tmp)+1];
    strcpy (data, tmp1);
    retval = strlen (data);
  }
  else {
    // protocol did not match packet!
    retval = 0;
  }
  return retval;
}

char *IpcPacket::protocolFound (char *protocol, char *packet)
{
  char *retval;

  retval = strpbrk (packet, ","); // point at ","
  if (retval) {
    char *tmp;
    int len = strlen (packet) - strlen (retval);
    if (len > 0) {
      tmp = (char *)malloc ((len+1)*sizeof(char));
      strncpy (tmp, packet, len);
      tmp[len] = '\0';
      if (IPC::strcmpCaseInsensitive(tmp,protocol)) {
	// protocol did not match packet!
	retval = 0;
      }
      else {
	retval++; // point to first character after ","
      }
      free (tmp);
    }
    else {
      retval = 0;
    }
  }
  else {
    retval = 0;
  }
  return retval;
}

int IpcPacket::parseWithLength (char *packet, char *data)
{
  int retval;

  int len = strlen (packet);
  char *protocol = (char *)malloc (decimalStrLen(len));
  sprintf (protocol, "%d", len);

  retval = parseWithProtocol (protocol, packet, data);
  free (protocol);
  return retval;
}

int IpcPacket::decimalStrLen (int number)
{
  int retval;
  for (retval = 1; number > 0; retval++) number /= 10;
  return retval;
}
