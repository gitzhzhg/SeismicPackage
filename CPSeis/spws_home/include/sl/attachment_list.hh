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

//------------------- attachment_list.hh -----------------------------//
//------------------- attachment_list.hh -----------------------------//
//------------------- attachment_list.hh -----------------------------//

//     header file for AttachmentElement and AttachmentList
//           derived from Element and BaseLinkedList
//                      subdirectory sl    

//                  this is a linked list of
//             form widget attachment information


#ifndef _ATTACHMENT_LIST_HH_
#define _ATTACHMENT_LIST_HH_

#include "oprim/element.hh"
#include "oprim/ll_base.hh"
#include <X11/Intrinsic.h>

class SLDelay;


//-------------------- AttachmentElement ------------------------//
//-------------------- AttachmentElement ------------------------//
//-------------------- AttachmentElement ------------------------//

class AttachmentElement : public Element
{
private:

  SLDelay *_gui     ;     // GUI to attach within its form parent.
  SLDelay *_left    ;     // sibling (or form parent) to the left.
  SLDelay *_right   ;     // sibling (or form parent) to the right.
  SLDelay *_top     ;     // sibling (or form parent) to the top.
  SLDelay *_bottom  ;     // sibling (or form parent) to the bottom.
  int      _oleft   ;     // left   offset.
  int      _oright  ;     // right  offset.
  int      _otop    ;     // top    offset.
  int      _obottom ;     // bottom offset.
  Boolean  _attached;     // TRUE or FALSE.

  friend class AttachmentList;

  AttachmentElement  (SLDelay *gui,
                      SLDelay *left        , SLDelay *right   = NULL,
                      SLDelay *top   = NULL, SLDelay *bottom  = NULL,
                      int      oleft = 0   , int      oright  = 0,
                      int      otop  = 0   , int      obottom = 0);
  ~AttachmentElement () {}
  int  operator ==   (void * const element) const;
  void print         () const;
  void tryToAttach   ();
};


//------------------- AttachmentList ----------------------------//
//------------------- AttachmentList ----------------------------//
//------------------- AttachmentList ----------------------------//

class AttachmentList : public BaseLinkedList
{
public:

  AttachmentList   ();
  ~AttachmentList  () {}
  void        add  (SLDelay *gui,
                    SLDelay *left        , SLDelay *right   = NULL,
                    SLDelay *top   = NULL, SLDelay *bottom  = NULL,
                    int      oleft = 0   , int      oright  = 0,
                    int      otop  = 0   , int      obottom = 0);
  void tryAllAttachments();

private:

  void               remove  (AttachmentElement *element);
  AttachmentElement *find    (AttachmentElement *element);
  AttachmentElement *top     ();
  AttachmentElement *bottom  ();
  AttachmentElement *next    ();
  AttachmentElement *prev    ();
  AttachmentElement *current ();
};

//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//
//--------------------- end of classes ----------------------//

#endif

//------------------------- end --------------------------//
//------------------------- end --------------------------//
//------------------------- end --------------------------//

