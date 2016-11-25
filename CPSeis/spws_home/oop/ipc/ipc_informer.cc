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
#include "ipc/ipc_informer.hh"
#include "ipc/ipc_inform_list.hh"
#include "ipc/ipc_inform.hh"
#include "cprim.h"
#include <iostream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>

//#define DEBUG  TRUE
#define DEBUG  FALSE

// Patterned after Tom Stoeckley's design for GpInformer...

// (1) IpcIO does not ask IpcInform any questions.
//        (all IpcInform functions are type void)
//
// (2) IpcInformer does not ask IpcIO any questions.
//        (IpcInformer does not need the IpcIO header file)
//
// (3) IpcInformer does not tell IpcIO anything.
//        (IpcInformer does not need the IpcIO header file)
//
// (4) IpcInformer remembers all the information it needs for dispatching
//     messages, based on messages it gets from IpcIO.
//
// (5) IpcIO remembers everything it needs for its internal operation,
//     to reply to questions from outside; it does not rely on IpcInformer
//     for anything.

IpcInformer::IpcInformer () :
  _multiple  (0),
  _list      (NULL)
{
  _list = new IpcInformList ();
}

IpcInformer::~IpcInformer ()
{
  delete _list;
}

void IpcInformer::addInformObject (IpcInform *inform)
{
  _list->add (inform);
}

void IpcInformer::removeInformObject (IpcInform *inform)
{
  _list->remove (inform);
}

void IpcInformer::beforeIpc ()
{
  if (DEBUG) cout << "beforeIpc  multiple=" << _multiple << endl;
  _multiple++;

  if (_multiple > 1) return;  // this is not the first one

  void *p;
  // loop through and notify users from top down
  for (IpcInform *inform = _list->top(&p); inform;
    inform = _list->next(&p)) {
    if (inform->messagesAreEnabled()) inform->beforeIpc ();
  }
}

void IpcInformer::afterIpc ()
{
  if (DEBUG) cout << "afterIpc  multiple=" << _multiple << endl;
  _multiple--;

  if (_multiple > 0) return; // this is not the last one

  void *p;
  // loop through and notify users from bottom up
  for (IpcInform *inform = _list->bottom(&p); inform;
    inform = _list->prev(&p)) {
    if (inform->messagesAreEnabled()) inform->afterIpc ();
  }
}
