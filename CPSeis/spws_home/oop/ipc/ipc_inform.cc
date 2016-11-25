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
#include "ipc/ipc_inform.hh"
#include "ipc/ipc_io.hh"
#include "ipc/ipc_informer.hh"

#include <assert.h>

IpcInform::IpcInform (IpcIO *io) :
  _io        (io),
  _informer  (io->informer()),
  _disabled  (0) // FALSE
{
  assert(_io && _informer);

  _io->informer()->addInformObject (this);
}

IpcInform::~IpcInform()
{
  _io->informer()->removeInformObject (this);
}

IpcIO *IpcInform::IO () const
{
  return _io;
}

IpcInformer *IpcInform::informer () const
{
  return _informer;
}

void IpcInform::disableMessages ()
{
  _disabled = 1;
}

void IpcInform::enableMessages ()
{
  _disabled = 0;
}

int IpcInform::messagesAreDisabled () const
{
  return _disabled;
}

int IpcInform::messagesAreEnabled () const
{
  return !_disabled;
}

void IpcInform::beforeIpc ()
{
}

void IpcInform::afterIpc ()
{
}
