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
#ifndef UNIQUE_STRING_LIST_HH
#define UNIQUE_STRING_LIST_HH

#include "oprim/element.hh"
#include "oprim/ll_base.hh"

class StringAndListElement : public Element {

private:
  friend class UniqueStringList;

  StringAndListElement
    (char *string);

  virtual ~StringAndListElement ();

  int operator ==
    (void * const string) const;

  void print () const;

  void attachList
    (BaseLinkedList *list);

  BaseLinkedList *list ();

  BaseLinkedList
    *_list;

  char
    *_string;
};

class UniqueStringList : public BaseLinkedList
{

public:
  UniqueStringList ();

  virtual ~UniqueStringList ();

  void add
    (char *string);

  void remove
    (char *string);

  char *find
    (char *string);

  char *top
    (void **p = 0);

  char *next
    (void **p = 0);

  void attachList
    (BaseLinkedList *list);

  BaseLinkedList *list ();

};

#endif
