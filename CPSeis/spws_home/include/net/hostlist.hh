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
#ifndef HOSTLIST_HH
#define HOSTLIST_HH


#include <string.h>
#include <stdlib.h>
#include <cprim.h>
#include "oprim/element.hh"
#include "oprim/ll_base.hh"


class NetEnv;


class HostNameElement : public Element
{
  friend class HostList;
  protected:
     char    *_hostname;
     int      _port;
     NetEnv  *_netenv;

     HostNameElement(char *hostname, int port, NetEnv *netenv) : 
                            _hostname(newstr(hostname)),
                            _netenv(netenv),
                            _port(port)  {}
     ~HostNameElement() { free(_hostname); }
     int operator ==(void * const hostname) const
                { return ( strcmp(_hostname, (char*)hostname) == 0 ); }
     virtual void print() const {}
  public:
     char   *getHostname() { return _hostname;}
     int     getPort()     { return _port;}
     NetEnv *getNetEnv()   { return _netenv;}
};



class HostList : public BaseLinkedList
{
  friend class HostNameElement;
  public:
     void    add(char *hostname, int port, NetEnv *netenv);
     void    remove(char *hostname) {BaseLinkedList::remove((void*) hostname);}
     HostNameElement *find(char *hostname, int port, void **ptr = (void **) 0);
     HostNameElement *top(void **ptr = (void **) 0);
     HostNameElement *bottom(void **ptr = (void **) 0);
     HostNameElement *next(void **ptr = (void **) 0);
     HostNameElement *prev(void **ptr = (void **) 0);
     HostNameElement *current(void **ptr = (void **) 0);
     //NetEnv *currentNetEnv(void **ptr = (void **) 0);
};


#endif
