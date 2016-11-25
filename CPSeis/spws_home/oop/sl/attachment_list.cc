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

//------------------ attachment_list.cc -----------------------------//
//------------------ attachment_list.cc -----------------------------//
//------------------ attachment_list.cc -----------------------------//

//  implementation file for AttachmentElement and AttachmentList
//           derived from Element and BaseLinkedList
//                      subdirectory sl


#include "sl/attachment_list.hh"
#include "sl/sl_delay.hh"
#include <iostream.h>


//--------------------- AttachmentElement ------------------------//
//--------------------- AttachmentElement ------------------------//
//--------------------- AttachmentElement ------------------------//

AttachmentElement::AttachmentElement(SLDelay *gui,
                      SLDelay *left , SLDelay *right  ,
                      SLDelay *top  , SLDelay *bottom ,
                      int      oleft, int      oright ,
                      int      otop , int      obottom)
              : Element(),
                  _gui   (gui)  ,
                  _left  (left) ,  _right   (right)  ,
                  _top   (top)  ,  _bottom  (bottom) ,
                  _oleft (oleft),  _oright  (oright) ,
                  _otop  (otop) ,  _obottom (obottom),
                  _attached(FALSE)
{
  if(!_gui)
       {
       cout << "AttachmentElement: trying to attach NULL child" << endl;
       return;
       }
  tryToAttach();
}


int AttachmentElement::operator ==(void * const att) const
{
  return (int)((AttachmentElement*)att == this);
}


void AttachmentElement::print() const
{
  cout << " " << (void*)this << endl;
}


void AttachmentElement::tryToAttach()
{
  if(_attached || !_gui) return;
  Widget w       = NULL;  if(_gui   ) w       = _gui   ->W();
  Widget wleft   = NULL;  if(_left  ) wleft   = _left  ->W();
  Widget wright  = NULL;  if(_right ) wright  = _right ->W();
  Widget wtop    = NULL;  if(_top   ) wtop    = _top   ->W();
  Widget wbottom = NULL;  if(_bottom) wbottom = _bottom->W();
  if(           !w      ) return;
  if(_left   && !wleft  ) return;
  if(_right  && !wright ) return;
  if(_top    && !wtop   ) return;
  if(_bottom && !wbottom) return;
  Widget parent = XtParent(w);
  if(_left   && _left  ->parentOfChildren() == parent)
                                wleft   = _left  ->parentOfChildren();
  if(_right  && _right ->parentOfChildren() == parent)
                                wright  = _right ->parentOfChildren();
  if(_top    && _top   ->parentOfChildren() == parent)
                                wtop    = _top   ->parentOfChildren();
  if(_bottom && _bottom->parentOfChildren() == parent)
                                wbottom = _bottom->parentOfChildren();
  attach_widget(w,  wleft,  wright,  wtop,  wbottom,
                   _oleft, _oright, _otop, _obottom);
  _attached = TRUE;
}




//-------------------- AttachmentList ----------------------//
//-------------------- AttachmentList ----------------------//
//-------------------- AttachmentList ----------------------//


AttachmentList::AttachmentList()
        : BaseLinkedList()
{
}


void AttachmentList::add(SLDelay *gui,
                         SLDelay *left , SLDelay *right  ,
                         SLDelay *top  , SLDelay *bottom ,
                         int      oleft, int      oright ,
                         int      otop , int      obottom)
{
  AttachmentElement *element = new AttachmentElement(gui,
                                       left,  right,  top,  bottom,
                                      oleft, oright, otop, obottom);
  BaseLinkedList::add(element);
}


void AttachmentList::remove(AttachmentElement *element)
{
  BaseLinkedList::remove((void*)element);
}


AttachmentElement *AttachmentList::find(AttachmentElement *element)
{
  Element *element2 = BaseLinkedList::find((void*)element);
  if(!element2) return NULL;
  return (AttachmentElement*)element2;
}


#define SHORTHAND(top)                             \
AttachmentElement *AttachmentList::top()           \
{                                                  \
  Element *element = BaseLinkedList::top();        \
  if(!element) return NULL;                        \
  return (AttachmentElement*)element;              \
}

  SHORTHAND(top)
  SHORTHAND(bottom)
  SHORTHAND(next)
  SHORTHAND(prev)
  SHORTHAND(current)


void AttachmentList::tryAllAttachments()
{
  for(AttachmentElement *element = top(); element;
                         element = next())
       {
       element->tryToAttach();
       }
}



//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

