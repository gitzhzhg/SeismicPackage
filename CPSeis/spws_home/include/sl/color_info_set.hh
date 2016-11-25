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
#ifndef COLOR_INFO_SET_HH
#define COLOR_INFO_SET_HH

#include "wproc.h"

class ColorInfoSet {

public:
  ColorInfoSet
    (ColorInfo *col,                   // ColorInfo used to define Colorset
     class ColorInfoSegment *segment,  // program element for ColorInfo
     int role_type = TWO_WAY);         // SENDER, RECEIVER, or TWO_WAY

  ~ColorInfoSet ();

  void update
    (ColorInfo *col = NULL,            // update local _col from this
     int broadcast = ALL);             // NO_BROADCAST, NO_SHARED_SETS, or ALL

  void updateColors
    (ColorInfo *col = NULL,            // update local _col's colors from this
     int broadcast = ALL);             // NO_BROADCAST, NO_SHARED_SETS, or ALL

  ColorInfo *colorInfo () const;

  class ColorInfoSegment *segment () const;

  int role
    (class ColorInfoSegment *segment = NULL) const; // role of program element

  class ColorInfoSet *matchesExactly         // matches _segment in this
    (class ColorInfoSegment *segment);       // program element for ColorInfo

  class ColorInfoSet *matches                // matches anything
    (class ColorInfoSegment *segment);       // program element for ColorInfo

  class ColorInfoElement *matchesElement     // matches an element
    (class ColorInfoSegment *segment) const; // program element using ColorInfo

  void applyUpdate ();

  void loopUpdate ();

  void addSharedSet
    (ColorInfoSet *set);           // ColorInfoSet with which to share

  void removeSharedSet
    (ColorInfoSet *set);           // ColorInfoSet to remove from sharing

  enum {                           // type of program element
    SEISPLOT,
    SEISCOLOR,
    COLORDESCRIPTOR,
    CBBCOLORSET,
    COLORTRANSLATE,
    PLOTIMAGE,
    XIMAGE,
    PIXMAPSET
  };

  enum {                           // role of program element
    SENDER,
    TWO_WAY
  };

  enum {                           // nature of program element call
    IMMEDIATE,
    ON_APPLY,
    IN_LOOP
  };

  enum {                           // who receives broadcast beyond instance
    NO_BROADCAST,
    NO_SHARED_SETS,
    ALL
  };

  void addSharedElement
    (class ColorInfoSegment *segment,// program element sharing ColorInfo
     int role = TWO_WAY);            // storer or user

  void removeSharedElement
    (class ColorInfoSegment *segment);// program element sharing ColorInfo

  void renderEmpty
    (class ColorInfoSegment *segment);// program element for ColorInfo

  void print () const;

private:
  int recursivelyFound             // is given set shared anywhere downward
    (ColorInfoSet *the_set);

  void doBroadcast
    (int how);                     // IMMEDIATE, ON_APPLY or IN_LOOP

  void doCallElement               // call all the elements in _list
    (int how);                     //   IMMEDIATE, ON_APPLY or IN_LOOP

  int different                    // rtns FALSE if col & _old_col same
    (ColorInfo *col);

  int differentPmsk                // rtns FALSE if pmsk: col & _old_col same
    (ColorInfo *col);

  int differentPix                 // rtns FALSE if pix: col & _old_col same
    (ColorInfo *col);

  void callElement                 // call a given program element based on
    (int how,                      //   IMMEDIATE, ON_APPLY or IN_LOOP
     int element_type,             //   given program element type
     void *pointer);               //   given program element ptr

  ColorInfo
    *_col,                         // internal ColorInfo object
    *_old_col;                     // internal ColorInfo object

  class ColorInfoSegment
    *_segment;                     // program element for _col

  class ColorInfoGroup
    *_shared_sets;                 // linked list of shared sets

  class ColorInfoList
    *_list;                        // linked list of program elements

  int
    _role;                         // SENDER or TWO_WAY
};

class ColorInfoSegment
{
public:
  ColorInfoSegment
    (void *pointer,                              // given program element ptr
     int element_type = ColorInfoSet::SEISPLOT); // given program element type

  virtual ~ColorInfoSegment ();

  void *pointer () const;

  int elementType () const;

private:
  void
    *_pointer;

  int
    _element_type;
};

#include "oprim/element.hh"

class ColorInfoElement : public Element
{
public:
  void *pointer () const;

  int elementType () const;

  int elementRole () const;

  void print () const;

private:
  friend class ColorInfoList;

  ColorInfoElement
    (class ColorInfoSegment *segment,// program element sharing ColorInfo
     int role);                      //   SENDER or TWO_WAY

  ~ColorInfoElement ();

  int operator ==
    (void * const segment) const;

  class ColorInfoSegment
    *_segment;

  int
    _role;

};

#include "oprim/ll_base.hh"

class ColorInfoList : public BaseLinkedList
{
public:
  ColorInfoList ();

  virtual ~ColorInfoList ();

  void add
    (class ColorInfoSegment *segment,// program element sharing ColorInfo
     int role);                      // storer or user

  void remove
    (class ColorInfoSegment *segment);// program element sharing ColorInfo

  ColorInfoElement *find
    (class ColorInfoSegment *segment);// program element sharing ColorInfo

  ColorInfoElement *top
    (void **p = NULL);

  ColorInfoElement *bottom
    (void **p = NULL);

  ColorInfoElement *next
    (void **p = NULL);

  ColorInfoElement *prev
    (void **p = NULL);

  ColorInfoElement *current
    (void **p = NULL);

  void print ();

};

class ColorInfoGroupElement : public Element
{
public:
  class ColorInfoSet *set () const;

private:
  friend class ColorInfoGroup;

  ColorInfoGroupElement
    (class ColorInfoSet *set);

  ~ColorInfoGroupElement ();

  int operator ==
    (void * const ColorInfoGroupElement) const;

  void print () const;

  class ColorInfoSet
    *_set;

};

class ColorInfoGroup : public BaseLinkedList
{
public:
  ColorInfoGroup ();

  virtual ~ColorInfoGroup ();

  void add
    (class ColorInfoSet *set);

  void remove
    (class ColorInfoSet *set);

  ColorInfoSet *find
    (class ColorInfoSet *set);

  ColorInfoSet *top
    (void **p = NULL);

  ColorInfoSet *bottom
    (void **p = NULL);

  ColorInfoSet *next
    (void **p = NULL);

  ColorInfoSet *prev
    (void **p = NULL);

  ColorInfoSet *current
    (void **p = NULL);

  void print ();
};

class ColorInfoCollection
{
public:
  static class ColorInfoSet *fetch
    (ColorInfo *col,                // ColorInfo defining which set is needed
     ColorInfoSegment *segment,     //   ColorInfoSegment
     int role = ColorInfoSet::TWO_WAY);
                                    // communication type SENDER or TWO_WAY

  static class ColorInfoSet *fetchExisting
    (ColorInfo *col);               // ColorInfo defining which set is needed

  static class ColorInfoSet *fetchIfExisting
    (ColorInfo *col);               // ColorInfo defining which set is needed

  static void updateEverywhere       // update the associated ColorInfoSet
    (ColorInfo *col);                // ColorInfo defining what to update

  static void updateDownward         // update the associated ColorInfoSet
    (ColorInfo *col);                // ColorInfo defining what to update

  static void updateNoBroadcast      // update the associated ColorInfoSet
    (ColorInfo *col);                // ColorInfo defining what to update

  static void updateColorsEverywhere // update the associated ColorInfoSet
    (ColorInfo *col);                // ColorInfo defining what to update

  static void updateColorsDownward   // update the associated ColorInfoSet
    (ColorInfo *col);                // ColorInfo defining what to update

  static void updateColorsNoBroadcast// update the associated ColorInfoSet
    (ColorInfo *col);                // ColorInfo defining what to update

  static void remove               // remove the associated ColorInfoSet
    (ColorInfo *col = NULL);       //   ColorInfo to remove (NULL means all)

  static void remove               // remove the associated shared element
    (ColorInfoSegment *segment);   //   ColorInfoSegment of shared element

  static void applyUpdate          // do apply type update as needed
    (ColorInfo *col = NULL);       // ColorInfo to apply (NULL means all)

  static void loopUpdate ();       // do loop type update as needed

  static int needsLoopService ();

  static void setLoopService
    (int flag = TRUE);             // TRUE if loop updating needs to happen

  static int useApply ();          // TRUE if SeisCbarPop Apply button used

  static void setApply
    (int flag = TRUE);             // TRUE if 

private:
  static void remove
    (ColorInfoSet *set);           // ColorInfo defining which set to remove

  static int findExisting
    (ColorInfo *col);              // ColorInfo defining which set is needed

  enum {                           // who receives broadcast from ColorInfoSet
    NO_BROADCAST,                  //   broadcast to no other prog elems beynd
    DOWNWARD,                      //   broadcast to prog elems base & below
    EVERYWHERE                     //   to all prog elems assoc w/ base segm
  };

  enum {
    UPDATE,
    UPDATE_COLORS
  };

  static void callUpdate
    (int direction,
     int modify_type,
     ColorInfo *col);              // ColorInfo defining what to update

  static void update
    (int modify_type,
     ColorInfo *col,               // ColorInfo defining what to update
     int broadcast);

  static void print ();

  static int
    NUM,
    NEEDS_LOOP_SERVICE,
    USE_APPLY;
};

class CheckColorSegments
{
public:
  CheckColorSegments ();

  ~CheckColorSegments ();

  void addColorSegment
    (const ColorInfoSegment *segment); //   ColorInfoSegment of shared element

  int removeColorSegment
    (const ColorInfoSegment *segment); //   ColorInfoSegment of shared element

  int isValid
    (const ColorInfoSegment *segment) const; // ColorInfoSegment of shared ele

  static int deleteSegment
    (ColorInfoSegment *segment);             // ColorInfoSegment of shared ele

private:
  int wasConstructed
    (const ColorInfoSegment *segment) const; // ColorInfoSegment of shared ele

  int wasDeleted
    (const ColorInfoSegment *segment) const; // ColorInfoSegment of shared ele

  int unique
    (const ColorInfoSegment *segment) const; // ColorInfoSegment of shared ele

  void clearFromDeleted
    (const ColorInfoSegment *segment);       // ColorInfoSegment of shared ele

  void shrinkIfNecessary ();

  ColorInfoSegment
    **_segs_constructed,
    **_segs_deleted;

  char
    _test_str[11];

  int
    _segs_con_alloc,
    _segs_con_count,
    _segs_del_alloc,
    _segs_del_count;
};

class TrackColorSegments
{
public:
  TrackColorSegments ();

  ~TrackColorSegments ();

  void addColorInfoSet
    (const ColorInfoSet *set,    //   ColorInfoSegment of shared element
     ColorInfoSegment *segment); //   ColorInfoSegment of shared ele

  int removeColorInfoSet
    (const ColorInfoSet *set); //   ColorInfoSegment of shared element

  int isUnique
    (ColorInfoSegment *segment) const; // ColorInfoSegment of shared ele

  void renderEmpty
    (ColorInfoSegment *segment);       // ColorInfoSegment of shared ele

private:
  int existingOwnerCount
    (ColorInfoSegment *segment) const; // ColorInfoSegment of shared ele

  int wasConstructed
    (const ColorInfoSet *set) const;   // ColorInfoSegment of shared ele

  int wasDeleted
    (const ColorInfoSet *set) const;   // ColorInfoSegment of shared ele

  ColorInfoSet
    **_sets_constructed,
    **_sets_deleted;

  ColorInfoSegment
    **_segs_owned;

  char
    _test_str[11];

  int
    _sets_con_alloc,
    _sets_con_count,
    _sets_del_alloc,
    _sets_del_count;
};

#endif
