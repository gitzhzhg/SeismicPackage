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
#ifndef IPC_INFORMER_HH
#define IPC_INFORMER_HH

class IpcInformer
{
  friend class IpcInform;      // to allow calling add/removeInformObject.
  friend class IpcIO;          // to allow calling most function

private:   // to be called only by the IpcIO object.

  IpcInformer ();              // Constructor

  virtual ~IpcInformer ();     // Destructor

private:   // to be called only by the base class (friend) IpcInform
           //   constructor or destructor.

  void addInformObject         // add an inform object
    (class IpcInform *inform); //   the inform object to add

  void removeInformObject      // remove an inform object
    (class IpcInform *inform); //   the inform object to remove

public:
  void beforeIpc ();           // called by application routines before IPC

  void afterIpc  ();           // called by application routines after IPC

private:
  int _multiple;               // counter for multiple operations in progress.

  class IpcInformList *_list;  // linked list of IpcInform objects.

};

#endif
