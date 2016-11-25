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

//----------------- resource_list.hh -----------------------------//
//----------------- resource_list.hh -----------------------------//
//----------------- resource_list.hh -----------------------------//


//     header file for ResourceElement and ResourceList
//         derived from Element and BaseLinkedList
//                    subdirectory sl

//        supports linked lists of BaseGR objects


#ifndef _RESOURCE_LIST_HH_
#define _RESOURCE_LIST_HH_

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <X11/Intrinsic.h>

class BaseGR;


//----------------------- ResourceElement ------------------------//
//----------------------- ResourceElement ------------------------//
//----------------------- ResourceElement ------------------------//

class ResourceElement : public Element
{
private:

  BaseGR *_resource;

  friend class ResourceList;

  ResourceElement  (BaseGR *resource);
  ~ResourceElement () {}
  int operator ==  (void * const resource) const;
  void print       () const;
};


//---------------------- ResourceList ----------------------------//
//---------------------- ResourceList ----------------------------//
//---------------------- ResourceList ----------------------------//

class ResourceList : public BaseLinkedList
{
protected:

  ResourceList          ();
  virtual ~ResourceList () {}
  void    add           (BaseGR *resource);
  void    remove        (BaseGR *resource);
  BaseGR *find          (BaseGR *resource);
  BaseGR *top           ();
  BaseGR *bottom        ();
  BaseGR *next          ();
  BaseGR *prev          ();
  BaseGR *current       ();

  Boolean _allow_reload_default;
  Boolean _allow_reload_system_default;
      // Needed here so that resource_collection classes will be
      // able to reset these variables.

  void updatePacket (Boolean force = FALSE);
      // Needed here so that the resource_collection classes will
      // have access to this method.

  virtual void updateSelf (Boolean force = FALSE) = 0;
      // Needed here so that the resource_collection classes will
      // have access to this method, which must be overridden by
      // PrimSupport.

public:

  void allowReloadDefault       (Boolean allow = TRUE)
              { _allow_reload_default = allow; }
  void allowReloadSystemDefault (Boolean allow = TRUE)
              { _allow_reload_system_default = allow; }
};


//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//

#endif

//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

