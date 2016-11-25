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
#include "sl/color_info_set.hh"
#include "sl/colorset_collection.hh"
#include "sl/ximage.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_color.hh"
#include "color/color_descriptor.hh"
#include "color/cbb_color_set.hh"
#include "ipc/color_xlate.hh"
#include "plot_image.hh"
#include "pixmap_set.hh"

#include <stdio.h>
#include <assert.h>

static CheckColorSegments *CHECKCOLORSEGMENTS = NULL;
static TrackColorSegments *TRACKCOLORSEGMENTS = NULL;

// _role is necessary to know whether to block certain communications
// Note: ColorInfo *col is what makes an instance of this object unique.
//   Hense it is invalid to create an instance of ColorInfoSet if a previous
//   ColorInfoSet already exists for the given ColorInfo. One
//   ColorInfo can be linked to many ColorInfoSegment's. Also, other
//   ColorInfoSet's can be linked to another ColorInfoSet.
ColorInfoSet::ColorInfoSet (ColorInfo *col, ColorInfoSegment *segment,
  int role) :
  _col             (col),
  _old_col         (NULL),
  _segment         (segment),
  _shared_sets     (NULL),
  _list            (NULL),
  _role            (role)
{
  assert (col && !ColorInfoCollection::fetchIfExisting(col) && _segment);
  _shared_sets = new ColorInfoGroup ();
  _list = new ColorInfoList ();

  if (!TRACKCOLORSEGMENTS) {
    //printf ("In ColorInfoSet constructor, TRACKCOLORSEGMENTS defined\n");
    TRACKCOLORSEGMENTS = new TrackColorSegments ();
  }
  TRACKCOLORSEGMENTS->addColorInfoSet (this, segment);
}

ColorInfoSet::~ColorInfoSet ()
{
  // don't delete _col; Assume calling object will delete it.
  // don't delete _segment; Assume calling object will delete it.
  // segment should be unique after it is constructed and before
  //   it is deleted
  if (_old_col) delete _old_col;
  delete _shared_sets;
  delete _list;

  TRACKCOLORSEGMENTS->removeColorInfoSet (this);
}

// if col is NULL then use _col in place of col
// if col is different from _old_col
//   copy col into _col if necessary and
//   copy col into _old_col and
//   make an immediate call to all the program elements in _list and
//   make an update call to all shared sets
void ColorInfoSet::update (ColorInfo *col, int broadcast)
{
  if (different(col)) {
    // copy the all the contents from col to _col if necessary
    if (col && col != _col) {
      memcpy (_col, col, sizeof(ColorInfo));
    }
    // make the old ColorInfo equal to the current ColorInfo
    if (_old_col) delete _old_col, _old_col = NULL;
    _old_col = new ColorInfo ();
    memcpy (_old_col, _col, sizeof(ColorInfo));
    // make an immediate call to the sponsoring program element
    if (_segment) {
      callElement (IMMEDIATE, _segment->elementType(), _segment->pointer());
    }
    if (broadcast > NO_BROADCAST) {
      // broadcast an immediate call to the program elements
      doCallElement (IMMEDIATE);
    }
  }

  if (broadcast == ALL) {
    // call update on all shared ColorInfoSet objects
    void *p;
    ColorInfoSet *set;
    for (set = _shared_sets->top(&p); set; set = _shared_sets->next(&p)) {
      set->update (_col);
    }
  }
}

// if col is NULL then use _col in place of col
// if _old_col->pix is different from col->pix, do the following:
//   if col->cnum > _col->cnum, update _col->cnum
//   get the XColors from col
//   store the XColors from col into _col and use Colorsets to
//     keep _col and its Colorset synchronized
//   make _old_col->cnum equal _col->cnum
//   make an immediate call to all the program elements in _list
//   make a copyColorsFrom call to all shared sets
void ColorInfoSet::updateColors (ColorInfo *col, int broadcast)
{

  if (differentPix(col)) {
    if (col) {
      if (col != _col) {
	if ( col->cnum > _col->cnum) _col->cnum = col->cnum;
	if (col->cnum > 0) {
	  // use ColorsetCollection to make sure the Colorsets associated
	  //   with _col & col are consistent (e.g. their colormaps)
	  XColor *colors;
	  ColorsetCollection::retrieve ( col, &colors);
	  ColorsetCollection::store    (_col,  colors);
	  free (colors);
	}
      }
    }

    if (_col->cmap != _old_col->cmap) _old_col->cmap = _col->cmap;
    if (_col->cnum != _old_col->cnum) _old_col->cnum = _col->cnum;

    int k2;
    for (k2 = 0; k2 < _col->cnum; k2++) {
      _old_col->pix[k2] = _col->pix[k2];
    }
    // make an immediate call to the sponsoring program element
    if (_segment) {
      callElement (IMMEDIATE, _segment->elementType(), _segment->pointer());
    }
    if (broadcast > NO_BROADCAST) {
      // broadcast an immediate call to the program elements
      doCallElement (IMMEDIATE);
    }
  }

  if (broadcast == ALL) {
    // call updateColors on all shared ColorInfoSet objects
    void *p;
    ColorInfoSet *set;
    for (set = _shared_sets->top(&p); set; set = _shared_sets->next(&p)) {
      set->updateColors (_col);
    }
  }
}

ColorInfo *ColorInfoSet::colorInfo () const
{
  return _col;
}

ColorInfoSegment *ColorInfoSet::segment () const
{
  assert (_segment);
  return _segment;
}

int ColorInfoSet::role (ColorInfoSegment *segment) const
{
  if (!segment || _segment == segment) {
    return _role;
  }
  else {
    ColorInfoElement *element = matchesElement (segment);
    if (element) {
      return element->elementRole ();
    }
    else if (_segment) {
      assert (0); // programming error on a add/remove operation
    }
    else {
      return _role;
    }
  }
}

ColorInfoSet *ColorInfoSet::matchesExactly (ColorInfoSegment *segment)
{
  if (!_segment) return NULL;
  if (_segment->elementType() == segment->elementType() &&
      _segment->pointer    () == segment->pointer    ()   ) {
    assert (segment == _segment);
    return this;
  }
  else {
    return NULL;
  }
}

ColorInfoSet *ColorInfoSet::matches (ColorInfoSegment *segment)
{
  if (matchesExactly(segment) || matchesElement(segment)) return this;

  void *p;
  ColorInfoSet *set;
  for (set = _shared_sets->top(&p); set; set = _shared_sets->next(&p)) {
    if (set->matches(segment)) return set;
  }

  return NULL;
}

ColorInfoElement *ColorInfoSet::matchesElement (ColorInfoSegment *segment)
  const
{
  ColorInfoElement *element = _list->find (segment);
  if (element) {
    return element;
  }
  else {
    return NULL;
  }
}

// See SeisColor & PixmapSet are examples that may require this.
void ColorInfoSet::applyUpdate ()
{
  doBroadcast (ON_APPLY);
}

void ColorInfoSet::loopUpdate ()
{
  doBroadcast (IN_LOOP);
}

void ColorInfoSet::addSharedSet (ColorInfoSet *set)
{
  assert (this != set);
  if (_shared_sets->find(set)) return;
  // recursively look for this instance. if it is found
  //   do not allow the add so as to prevent infinite updates
  if (!recursivelyFound(set)) {
    _shared_sets->add (set);
  }
}

void ColorInfoSet::removeSharedSet (ColorInfoSet *set)
{
  if (!_shared_sets->find(set)) return;
  _shared_sets->remove (set);
}

void ColorInfoSet::addSharedElement (ColorInfoSegment *segment, int role)
{
  assert (_segment != segment);
  if (_list->find(segment)) return;
  _list->add (segment, role);
}

void ColorInfoSet::removeSharedElement (ColorInfoSegment *segment)
{
  if (!_list->find(segment)) return;
  _list->remove (segment);
}

void ColorInfoSet::renderEmpty (ColorInfoSegment *segment)
{
  assert (_segment == NULL || _segment == segment);
  _segment = NULL;
}

void ColorInfoSet::print () const
{
  printf ("ColorInfoSet: %ld////// Beginning ////////\n", this);

  _list->print ();

  _shared_sets->print ();

  printf ("ColorInfoSet: %ld////// Ending ////////\n", this);
}

int ColorInfoSet::recursivelyFound (ColorInfoSet *the_set)
{
  int found = this == the_set;

  ColorInfoSet *set;
  void *p;
  for (set = _shared_sets->top(&p); !found && set;
    set = _shared_sets->next(&p)) {
    found = set->recursivelyFound (the_set);
  }
  return found;
}

// broadcast to this instance and downward.
//   all appendages are called. you can manage what the individual
//   program elements do or don't do by one of two methods. you could have
//   made an earlier IMMEDIATE call and used an out-of-date scheme in the
//   appropriate program elements. alternatively, if the program element is
//   a SENDER then it will not be called.
void ColorInfoSet::doBroadcast (int how)
{
  doCallElement (how);

  // call doBroadcast on all shared ColorInfoSet objects
  //   work was done on addSharedSet to prevent infinite loop
  void *p;
  ColorInfoSet *set;
  for (set = _shared_sets->top(&p); set; set = _shared_sets->next(&p)) {
    set->doBroadcast (how);
  }

  if (_segment) {
    // Call the instigator last hoping that all the changes have settled out
    //   before the most important call
    callElement (how, _segment->elementType(), _segment->pointer());
  }
}

void ColorInfoSet::doCallElement (int how)
{
  void *p;
  ColorInfoElement *element;
  for (element = _list->top(&p); element; element = _list->next(&p)) {
    // SENDER's do not receive broadcasts! They merely store information
    if (element->elementRole() > SENDER) {
      callElement (how, element->elementType(), element->pointer());
    }
  }
}

int ColorInfoSet::different (ColorInfo *col)
{
  if (!col) col = _col;

  int both_null = !col && !_old_col;
  if (!both_null) {
    int only_one_null = !col || !_old_col;
    if (only_one_null                         ||
	col->colorsafe != _old_col->colorsafe ||
	col->numplanes != _old_col->numplanes ||
	col->cmap      != _old_col->cmap      ||
	col->cnum      != _old_col->cnum      ||
	differentPmsk(col)                    ||
	differentPix(col)                     ||
	col->shared    != _old_col->shared      ) {
      return TRUE;
    }
    else {
      return FALSE;
    }
  }
  else {
    return FALSE;
  }
}

int ColorInfoSet::differentPmsk (ColorInfo *col)
{
  if (!col) col = _col;

  if (col->numplanes != _old_col->numplanes) return TRUE;

  int retval;
  int k2;
  for (k2 = 0, retval = FALSE; !retval && k2 < col->numplanes; k2++) {
    retval = col->pmsk[k2] != _old_col->pmsk[k2];
  }
  return retval;
}

int ColorInfoSet::differentPix (ColorInfo *col)
{
  if (!col) col = _col;

  if (col->cnum != _old_col->cnum) return TRUE;

  int retval;
  int k2;
  for (k2 = 0, retval = FALSE; !retval && k2 < col->cnum; k2++) {
    retval = col->pix[k2] != _old_col->pix[k2];
  }
  return retval;
}

void ColorInfoSet::callElement (int how, int element_type, void *pointer)
{
  // To do synchronization through-out the main program
  switch (how) {
  case IMMEDIATE :                // quick things such as marking something
    switch (element_type) {         //   out-of-date or copying a small array
    case SEISPLOT :
      ((SeisPlot *)pointer)->colorInfoChangedImmediately (_col);
      break;
    case SEISCOLOR :
      ((SeisColor *)pointer)->colorInfoChangedImmediately (_col);
      break;
    case COLORDESCRIPTOR :
//      ((ColorDescriptor *)pointer)->colorInfoChangedImmediately (_col);
      break;
    case CBBCOLORSET :
      ((CBBColorSet *)pointer)->colorInfoChangedImmediately (_col);
      break;
    case COLORTRANSLATE :
//      ((ColorTranslate *)pointer)->colorInfoChangedImmediately (_col);
      break;
    case PLOTIMAGE :
//      ((PlotImage *)pointer)->colorInfoChangedImmediately (_col);
      break;
    case XIMAGE :
//      ((Ximage *)pointer)->colorInfoChangedImmediately (_col);
      break;
    case PIXMAPSET :
      ((PixmapSet *)pointer)->colorInfoChangedImmediately (_col);
      break;
    default:
      assert (0);
      break;
    }
    break;
  case ON_APPLY :                        // when an apply button is pressed
    switch (element_type) {
    case SEISPLOT :
      ((SeisPlot *)pointer)->colorInfoChangedFromApply (_col);
      break;
    case SEISCOLOR :
//      ((SeisColor *)pointer)->colorInfoChangedFromApply (_col);
      break;
    case COLORDESCRIPTOR :
//      ((ColorDescriptor *)pointer)->colorInfoChangedFromApply (_col);
      break;
    case CBBCOLORSET :
      assert (0);
//      ((CBBColorSet *)pointer)->colorInfoChangedFromApply (_col);
      break;
    case COLORTRANSLATE :
//      ((ColorTranslate *)pointer)->colorInfoChangedFromApply (_col);
      break;
    case PLOTIMAGE :
//      ((PlotImage *)pointer)->colorInfoChangedFromApply (_col);
      break;
    case XIMAGE :
//      ((Ximage *)pointer)->colorInfoChangedFromApply (_col);
      break;
    case PIXMAPSET :
//    ((PixmapSet *)pointer)->colorInfoChangedFromApply (_col);
      break;
    default:
      assert (0);
      break;
    }
    break;
  case IN_LOOP :                  // when a slider bar is moved
    switch (element_type) {
    case SEISPLOT :
      ((SeisPlot *)pointer)->colorInfoChangedFromLoop (_col);
      break;
    case SEISCOLOR :
//      ((SeisColor *)pointer)->colorInfoChangedFromLoop (_col);
      break;
    case COLORDESCRIPTOR :
//      ((ColorDescriptor *)pointer)->colorInfoChangedFromLoop (_col);
      break;
    case CBBCOLORSET :
//      ((CBBColorSet *)pointer)->colorInfoChangedFromLoop (_col);
      break;
    case COLORTRANSLATE :
//      ((ColorTranslate *)pointer)->colorInfoChangedFromLoop (_col);
      break;
    case PLOTIMAGE :
//      ((PlotImage *)pointer)->colorInfoChangedFromLoop (_col);
      break;
    case XIMAGE :
//      ((Ximage *)pointer)->colorInfoChangedFromLoop (_col);
      break;
    case PIXMAPSET :
//      ((PixmapSet *)pointer)->colorInfoChangedFromLoop (_col);
      break;
    default:
//      assert (0); // !?!crashes can occur when using SeisMultiPlotControl!?!
      break;
    }
    break;
  default:
    assert (0);
  }
}

////////////////////////////////////////////////////////////

ColorInfoSegment::ColorInfoSegment (void *pointer, int element_type) :
  _pointer       (pointer),
  _element_type  (element_type)
{
  if (!CHECKCOLORSEGMENTS) {
    //printf ("In ColorInfoSegment constructor, CHECKCOLORSEGMENTS defined\n");
    CHECKCOLORSEGMENTS = new CheckColorSegments ();
  }
  CHECKCOLORSEGMENTS->addColorSegment (this);
}

ColorInfoSegment::~ColorInfoSegment ()
{
  CHECKCOLORSEGMENTS->removeColorSegment (this);
}

void *ColorInfoSegment::pointer () const
{
  if (!CHECKCOLORSEGMENTS->isValid(this)) {
    printf ("Error in ColorInfoSegment: %u\n", this);
  }
  return _pointer;
}

int ColorInfoSegment::elementType () const
{
  if (!CHECKCOLORSEGMENTS->isValid(this)) {
    printf ("Error in ColorInfoSegment: %u\n", this);
  }
  return _element_type;
}

////////////////////////////////////////////////////////////////

void *ColorInfoElement::pointer () const
{
  return _segment->pointer ();
}

int ColorInfoElement::elementType () const
{
  return _segment->elementType ();
}

int ColorInfoElement::elementRole () const
{
  return _role;
}

void ColorInfoElement::print () const
{
  char element[20], role[30];
  switch (_segment->elementType()) {
  case ColorInfoSet::SEISPLOT:
    strcpy (element, "SeisPlot");
    break;
  case ColorInfoSet::SEISCOLOR:
    strcpy (element, "SeisColor");
    break;
  case ColorInfoSet::COLORDESCRIPTOR:
    strcpy (element, "ColorDescriptor");
    break;
  case ColorInfoSet::CBBCOLORSET:
    strcpy (element, "CBBColorSet");
    break;
  case ColorInfoSet::COLORTRANSLATE:
    strcpy (element, "ColorTranslate");
    break;
  case ColorInfoSet::PLOTIMAGE:
    strcpy (element, "PlotImage");
    break;
  case ColorInfoSet::XIMAGE:
    strcpy (element, "Ximage");
    break;
  case ColorInfoSet::PIXMAPSET:
    strcpy (element, "PixmapSet");
    break;
  default:
    strcpy (element, "Unknown");
    break;
  }

  switch (_role) {
  case ColorInfoSet::SENDER:
    strcpy (role, "a Sender");
    break;
  case ColorInfoSet::TWO_WAY:
    strcpy (role, "both Sender and Receiver");
    break;
  default:
    strcpy (role, "Unknown");
    break;
  }

  printf ("ColorInfoElement: %ld is a %s comunicating as %s\n",
    _segment->pointer(), element, role);
}

ColorInfoElement::ColorInfoElement (ColorInfoSegment *segment, int role) :
  _segment  (segment),
  _role     (role)
{
}

ColorInfoElement::~ColorInfoElement ()
{
}

int ColorInfoElement::operator == (void * const segment) const
{
  ColorInfoSegment *loc_segment = (ColorInfoSegment *)segment;
  if (_segment == loc_segment) {
    assert (_segment->    pointer() == loc_segment->    pointer() &&
            _segment->elementType() == loc_segment->elementType()   );
    return TRUE;
  }
  else {
    return FALSE;
  }
}

////////////////////////////////////////////////////////////////

ColorInfoList::ColorInfoList ()
{
}

ColorInfoList::~ColorInfoList ()
{
}

void ColorInfoList::add (ColorInfoSegment *segment, int role)
{
  Element *element = new ColorInfoElement (segment, role);
  BaseLinkedList::add (element);
}

void ColorInfoList::remove (ColorInfoSegment *segment)
{
  BaseLinkedList::remove (segment);
}

ColorInfoElement *ColorInfoList::find (ColorInfoSegment *segment)
{
  Element *element = BaseLinkedList::find ((void *)segment);
  if (!element) return NULL;
  return (ColorInfoElement *)element;
}

ColorInfoElement *ColorInfoList::top (void **ptr)
{
   ColorInfoElement* element
     = (ColorInfoElement*)BaseLinkedList::top (ptr);
   return element;
}

ColorInfoElement *ColorInfoList::bottom (void **ptr)
{
   ColorInfoElement* element
     = (ColorInfoElement*)BaseLinkedList::bottom (ptr);
   return element;
}

ColorInfoElement *ColorInfoList::next (void **ptr)
{
   ColorInfoElement* element
     = (ColorInfoElement*)BaseLinkedList::next (ptr);
   return element;
}

ColorInfoElement *ColorInfoList::prev (void **ptr)
{
   ColorInfoElement* element
     = (ColorInfoElement*)BaseLinkedList::prev (ptr);
   return element;
}

ColorInfoElement *ColorInfoList::current (void **ptr)
{
   ColorInfoElement* element
     = (ColorInfoElement*)BaseLinkedList::current (ptr);
   return element;
}

void ColorInfoList::print ()
{
  if (count() > 0) {
    printf ("////////// Program Elements Beginning //////\n");
    void *p;
    ColorInfoElement *element;
    for (element = top(&p); element; element = next(&p)) {
      element->print ();
    }
    printf ("////////// Program Elements Ending //////\n");
  }
}

////////////////////////////////////////////////////////////////

ColorInfoSet *ColorInfoGroupElement::set () const
{
  return _set;
}

ColorInfoGroupElement::ColorInfoGroupElement (ColorInfoSet *set) :
  _set  (set)
{
}

ColorInfoGroupElement::~ColorInfoGroupElement ()
{
}

int ColorInfoGroupElement::operator == (void * const set) const
{
  return (_set == (ColorInfoSet *)set);
}

void ColorInfoGroupElement::print () const
{
  printf ("ColorInfoSet: %ld\n", _set);
}

////////////////////////////////////////////////////////////////

ColorInfoGroup::ColorInfoGroup ()
{
}

ColorInfoGroup::~ColorInfoGroup ()
{
}

void ColorInfoGroup::add (ColorInfoSet *set)
{
  ColorInfoGroupElement *element
    = new ColorInfoGroupElement (set);
  BaseLinkedList::add (element);
}

void ColorInfoGroup::remove (ColorInfoSet *set)
{
  BaseLinkedList::remove (set);
}

ColorInfoSet *ColorInfoGroup::find (ColorInfoSet *set)
{
  Element *element = BaseLinkedList::find ((void *)set);
  if (!element) return NULL;
  return ((ColorInfoGroupElement *)element)->set ();
}

ColorInfoSet *ColorInfoGroup::top (void **ptr)
{
   ColorInfoGroupElement* element
     = (ColorInfoGroupElement*)BaseLinkedList::top (ptr);
   return (element ? element->set() : NULL);
}

ColorInfoSet *ColorInfoGroup::bottom (void **ptr)
{
   ColorInfoGroupElement* element
     = (ColorInfoGroupElement*)BaseLinkedList::bottom (ptr);
   return (element ? element->set() : NULL);
}

ColorInfoSet *ColorInfoGroup::next (void **ptr)
{
   ColorInfoGroupElement* element
     = (ColorInfoGroupElement*)BaseLinkedList::next (ptr);
   return (element ? element->set() : NULL);
}

ColorInfoSet *ColorInfoGroup::prev (void **ptr)
{
   ColorInfoGroupElement* element
     = (ColorInfoGroupElement*)BaseLinkedList::prev (ptr);
   return (element ? element->set() : NULL);
}

ColorInfoSet *ColorInfoGroup::current (void **ptr)
{
   ColorInfoGroupElement* element
     = (ColorInfoGroupElement*)BaseLinkedList::current (ptr);
   return (element ? element->set() : NULL);
}

void ColorInfoGroup::print ()
{
  if (count() > 0) {
    printf ("////////// Linked ColorInfoSets Beginning //////\n");
    void *p;
    ColorInfoSet *element;
    for (element = top(&p); element; element = next(&p)) {
      element->print ();
    }
    printf ("////////// Linked ColorInfoSets Ending //////\n");
  }
}

////////////////////////////////////////////////////////////////

#define MAXSETS 250

static ColorInfoSet *COLORINFOSETS[MAXSETS];
int ColorInfoCollection::NUM = 0;
int ColorInfoCollection::NEEDS_LOOP_SERVICE = FALSE;
int ColorInfoCollection::USE_APPLY = FALSE;

ColorInfoSet *ColorInfoCollection::fetch (ColorInfo *col,
  ColorInfoSegment *segment, int role)
{
  if (!col || !segment) return NULL;

  // look for preexisting ColorInfoSet
  int k2 = findExisting (col);
  if (k2 > -1) {
    if (!COLORINFOSETS[k2]->matches(segment)) {
      // the given program segment is not tied in any way to the given
      //   ColorInfo, so add it as a shared element
      COLORINFOSETS[k2]->addSharedElement (segment, role);
    }
    return COLORINFOSETS[k2];
  }

  COLORINFOSETS[NUM] = new ColorInfoSet (col, segment, role);
  NUM++;
  return COLORINFOSETS[NUM-1];
}

ColorInfoSet *ColorInfoCollection::fetchExisting (ColorInfo *col)
{
  ColorInfoSet *retval;
  assert (retval = fetchIfExisting(col));
  return retval;
}

ColorInfoSet *ColorInfoCollection::fetchIfExisting (ColorInfo *col)
{
  // look for preexisting ColorInfoSet
  int k2 = findExisting (col);
  if (k2 > -1) {
    return COLORINFOSETS[k2];
  }
  else {
    return NULL;
  }
}

void ColorInfoCollection::updateEverywhere (ColorInfo *col)
{
  callUpdate (EVERYWHERE, UPDATE, col);
}

void ColorInfoCollection::updateDownward (ColorInfo *col)
{
  callUpdate (DOWNWARD, UPDATE, col);
}

void ColorInfoCollection::updateNoBroadcast (ColorInfo *col)
{
  callUpdate (NO_BROADCAST, UPDATE, col);
}

void ColorInfoCollection::updateColorsEverywhere (ColorInfo *col)
{
  callUpdate (EVERYWHERE, UPDATE_COLORS, col);
}

void ColorInfoCollection::updateColorsDownward (ColorInfo *col)
{
  callUpdate (DOWNWARD, UPDATE_COLORS, col);
}

void ColorInfoCollection::updateColorsNoBroadcast (ColorInfo *col)
{
  callUpdate (NO_BROADCAST, UPDATE_COLORS, col);
}

void ColorInfoCollection::remove (ColorInfo *col)
{
  if (col) {
    // delete ColorInfoSet associated with the given ColorInfo
    ColorInfoSet *set = fetchIfExisting (col);
    remove (set);
  }
  else {
    // delete all ColorInfoSet's
    int k2;
    for (k2 = 0; k2 < NUM; k2++) {
      delete COLORINFOSETS[k2];
      COLORINFOSETS[k2] = NULL;
      // don't forget to do the corresponding
      //   ColorsetCollection::remove (col) in the calling program!
    }
    NUM = 0;
  }
}

// any time a program element is deleted, this needs to be called
void ColorInfoCollection::remove (ColorInfoSegment *segment)
{
  int k2;

  if (segment) {
    for (k2 = 0; k2 < NUM; k2++) {
      if (COLORINFOSETS[k2]->matchesElement(segment)) {
	COLORINFOSETS[k2]->removeSharedElement (segment);
      }
    }
////////////////////////////// SUSPECT ////////////////////////
/*
    int count, ids[5];
    for (k2 = 0, count = 0; k2 < NUM; k2++) {
      if (COLORINFOSETS[k2]->matchesExactly(segment)) {
        count++;
	ids[k2] = k2;
      }
    }
    if (count > 0) {
      assert (count <= 2);
      for (k2 = 0; k2 < count; k2++) {
	remove (COLORINFOSETS[ids[k2]]);
      }
    }
*/
////////////////////////////// SUSPECT ////////////////////////
  }
}

void ColorInfoCollection::applyUpdate (ColorInfo *col)
{
  int k2;
  if (col) {
    for (k2 = 0; k2 < NUM; k2++) {
      if (COLORINFOSETS[k2]->colorInfo() == col) {
	COLORINFOSETS[k2]->applyUpdate ();
	k2 = NUM;
      }
    }
  }
  else {
    for (k2 = 0; k2 < NUM; k2++) {
      COLORINFOSETS[k2]->applyUpdate ();
    }
  }
}

void ColorInfoCollection::loopUpdate ()
{
  if (!needsLoopService()) return;

  int k2;
  for (k2 = 0; k2 < NUM; k2++) {
    COLORINFOSETS[k2]->loopUpdate ();
  }
}

int ColorInfoCollection::needsLoopService ()
{
  int retval = NEEDS_LOOP_SERVICE;
  NEEDS_LOOP_SERVICE = FALSE;
  return retval;
}

void ColorInfoCollection::setLoopService (int flag)
{
  NEEDS_LOOP_SERVICE = flag;
}

int ColorInfoCollection::useApply ()
{
  return USE_APPLY;
}

void ColorInfoCollection::setApply (int flag)
{
  USE_APPLY = flag;
}

void ColorInfoCollection::remove (ColorInfoSet *set)
{
  int k2;

  ColorInfoSet *found_it;

  if (set) {
    for (k2 = 0, found_it = FALSE; k2 < NUM; k2++) {
      if (COLORINFOSETS[k2] == set) {
	found_it = COLORINFOSETS[k2];
	delete COLORINFOSETS[k2];
	// don't forget to do the corresponding
	//   ColorsetCollection::remove (col) in the calling program!
	NUM--;
	for (; k2 < NUM; k2++) {
	  COLORINFOSETS[k2] = COLORINFOSETS[k2+1];
	}
	COLORINFOSETS[k2] = NULL;
      }
    }

    assert (found_it);

    // remove the deleted ColorInfoSet from shared sets in all the other
    //   ColorInfoSet's if applicable
    for (k2 = 0; k2 < NUM; k2++) {
      COLORINFOSETS[k2]->removeSharedSet (found_it);
    }
  }
}

int ColorInfoCollection::findExisting (ColorInfo *col)
{
  if (col) {
    int k2;
    for (k2 = 0; k2 < NUM; k2++) {
      if (COLORINFOSETS[k2]->colorInfo() == col) {
	return k2;
      }
    }
    return -1;
  }
  else {
   return -1;
  }
}

void ColorInfoCollection::callUpdate (int direction, int modify_type,
  ColorInfo *col)
{
  ColorInfoSet *set, *loc_set;
  int k2;
  set = fetchIfExisting (col);
  if (!set) return;

  switch (direction) {
  case DOWNWARD :
    update (modify_type, col, ColorInfoSet::ALL);
    break;
  case EVERYWHERE :
    for (k2 = 0; k2 < NUM; k2++) {
      loc_set = COLORINFOSETS[k2];
      if (loc_set == set) {
	update (modify_type, col, ColorInfoSet::ALL);
      }
      else if (loc_set->matchesElement(set->segment())) {
	update (modify_type, col, ColorInfoSet::NO_SHARED_SETS);
      }
    }
    break;
  case NO_BROADCAST :
    update (modify_type, col, ColorInfoSet::NO_BROADCAST);
    break;
  default:
    assert (0);
  }
}

void ColorInfoCollection::update (int modify_type, ColorInfo *col,
  int broadcast)
{
  ColorInfoSet *set = fetchIfExisting (col);
  if (!set) return;

  switch (modify_type) {
  case UPDATE :
    set->update (col, broadcast);
    break;
  case UPDATE_COLORS :
    set->updateColors (col, broadcast);
    break;
  default:
    assert (0);
  }
}

void ColorInfoCollection::print ()
{
  int k2;
  for (k2 = 0; k2 < NUM; k2++) {
    COLORINFOSETS[k2]->print ();
  }
}

#define INCR 50
#define TEST_STR "ZZZZZ"
#define ABSMAX 100

CheckColorSegments::CheckColorSegments () :
  _segs_constructed  (NULL),
  _segs_deleted      (NULL),
  _segs_con_alloc    (0),
  _segs_con_count    (0),
  _segs_del_alloc    (0),
  _segs_del_count    (0)
{
  strcpy (_test_str, TEST_STR);
}

CheckColorSegments::~CheckColorSegments ()
{
  if (_segs_con_alloc > 0) {
    free (_segs_constructed);
  }
  if (_segs_del_alloc > 0) {
    free (_segs_deleted);
  }
}

void CheckColorSegments::addColorSegment (const ColorInfoSegment *segment)
{
  clearFromDeleted (segment);   // if on delete list, clear it
  if (!unique(segment)) return; // maintain unique list
  shrinkIfNecessary ();         // keep arrays reasonable length

  if (_segs_con_count == _segs_con_alloc) {
    _segs_con_alloc += INCR;
    ColorInfoSegment **segs = (ColorInfoSegment **)malloc (_segs_con_alloc
      *sizeof(ColorInfoSegment *));
    int k2 = 0;
    if (_segs_con_count > 0) {
      for (; k2 < _segs_con_count; k2++) {
        segs[k2] = _segs_constructed[k2];
      }
    }
    for (; k2 < _segs_con_alloc; k2++) {
      segs[k2] = NULL;
    }
    free (_segs_constructed);
    _segs_constructed = segs;
  }
  _segs_constructed[_segs_con_count] = (ColorInfoSegment *)segment;
  _segs_con_count++;
  char test_str[11];
  sprintf (test_str, "%u", segment);
  if (strstr(test_str,_test_str)) {
    printf ("constructed ColorInfoSegment: %u\n", segment);
  }
}

int CheckColorSegments::removeColorSegment (const ColorInfoSegment *segment)
{
  if (wasConstructed(segment)) {
    if (_segs_del_count == _segs_del_alloc) {
      _segs_del_alloc += INCR;
      ColorInfoSegment **segs = (ColorInfoSegment **)malloc (_segs_del_alloc
        *sizeof(ColorInfoSegment *));
      int k2 = 0;
      if (_segs_del_count > 0) {
	for (; k2 < _segs_del_count; k2++) {
	  segs[k2] = _segs_deleted[k2];
	}
      }
      for (; k2 < _segs_del_alloc; k2++) {
	segs[k2] = NULL;
      }
      free (_segs_deleted);
      _segs_deleted = segs;
    }
    _segs_deleted[_segs_del_count] = (ColorInfoSegment *)segment;
    _segs_del_count++;
    char test_str[11];
    sprintf (test_str, "%u", segment);
    if (strstr(test_str,_test_str)) {
      printf ("deleted ColorInfoSegment: %u\n", segment);
    }
    return TRUE;
  }
  else {
    return FALSE;
  }
}

int CheckColorSegments::isValid (const ColorInfoSegment *segment) const
{
  return wasConstructed(segment) && !wasDeleted(segment);
}

int CheckColorSegments::deleteSegment (ColorInfoSegment *segment)
{
  if (CHECKCOLORSEGMENTS == NULL) return FALSE;
/*
  if (!(CHECKCOLORSEGMENTS && TRACKCOLORSEGMENTS)) {
    printf ("CheckColorSegments::deleteSegment.CHECKCOLORSEGMENTS = %ld\n",
      CHECKCOLORSEGMENTS);
    printf ("CheckColorSegments::deleteSegment.TRACKCOLORSEGMENTS = %ld\n",
      TRACKCOLORSEGMENTS);
    assert (CHECKCOLORSEGMENTS && TRACKCOLORSEGMENTS);
  }
*/

  int is_unique;
  if (TRACKCOLORSEGMENTS != NULL) {
    TRACKCOLORSEGMENTS->renderEmpty (segment);
    is_unique = TRACKCOLORSEGMENTS->isUnique (segment);
  }
  else {
    is_unique = TRUE;
  }

  if (CHECKCOLORSEGMENTS->isValid (segment) && is_unique) {
    delete segment;
    return TRUE;
  }
  else {
    return FALSE;
  }
}

int CheckColorSegments::wasConstructed (const ColorInfoSegment *segment) const
{
  int k2;
  for (k2 = 0; k2 < _segs_con_count; k2++) {
    if (_segs_constructed[k2] == segment) {
      return TRUE;
    }
  }
  return FALSE;
}

int CheckColorSegments::wasDeleted (const ColorInfoSegment *segment) const
{
  int k2;
  for (k2 = 0; k2 < _segs_del_count; k2++) {
    if (_segs_deleted[k2] == segment) {
      return TRUE;
    }
  }
  return FALSE;
}

int CheckColorSegments::unique (const ColorInfoSegment *segment) const
{
  int k2;
  for (k2 = 0; k2 < _segs_con_count; k2++) {
    if (_segs_constructed[k2] == (ColorInfoSegment *)segment) {
      return FALSE;
    }
  }
  return TRUE;
}

void CheckColorSegments::clearFromDeleted (const ColorInfoSegment *segment)
{
  int existing;
  int k2;
  for (k2 = 0, existing = -1; k2 < _segs_del_count; k2++) {
    if (_segs_deleted[k2] == (ColorInfoSegment *)segment) {
      existing = k2;
      break;
    }
  }
  if (existing > -1) {
    for (k2 = existing+1; k2 < _segs_del_count; k2++) {
      _segs_deleted[k2-1] = _segs_deleted[k2];
    }
    _segs_del_count--;
  }
}

void CheckColorSegments::shrinkIfNecessary ()
{
  if (_segs_con_count >= ABSMAX && _segs_del_count > 0) {
    int k2, k3, count, found_it;
    for (k2 = 0, count = 0; k2 < _segs_del_count; k2++) {
      for (k3 = 0, found_it = FALSE; !found_it && k3 < _segs_con_count;
        k3++) {
        if (_segs_deleted[k2] == _segs_constructed[k3]) {
	  count++;
          _segs_constructed[k3] = NULL;
	  _segs_deleted[k2] = NULL;
          found_it = TRUE;
	}
      }
      assert (found_it); // must find all the deleted objs
    }
    _segs_del_count = 0;

    // consolidate the constructed objs that were not deleted
    ColorInfoSegment *tmp;
    for (k2 = 0; k2 < _segs_con_count; k2++) {
      for (k3 = k2+1; k3 < _segs_con_count; k3++) {
	if (_segs_constructed[k2] < _segs_constructed[k3]) {
	  tmp = _segs_constructed[k2];
	  _segs_constructed[k2] = _segs_constructed[k3];
	  _segs_constructed[k3] = tmp;
	}
      }
    }
    _segs_con_count -= count;
  }
}

TrackColorSegments::TrackColorSegments () :
  _sets_constructed  (NULL),
  _sets_deleted      (NULL),
  _segs_owned        (NULL),
  _sets_con_alloc    (0),
  _sets_con_count    (0),
  _sets_del_alloc    (0),
  _sets_del_count    (0)
{
  strcpy (_test_str, TEST_STR);
}

TrackColorSegments::~TrackColorSegments ()
{
  if (_sets_con_alloc > 0) {
    free (_sets_constructed);
    free (_segs_owned       );
  }
  if (_sets_del_alloc > 0) {
    free (_sets_deleted);
  }
}

void TrackColorSegments::addColorInfoSet (const ColorInfoSet *set,
  ColorInfoSegment *seg)
{
  if (_sets_con_count == _sets_con_alloc) {
    _sets_con_alloc += INCR;
    ColorInfoSet     **sets = (ColorInfoSet     **)malloc (_sets_con_alloc
      *sizeof(ColorInfoSet     *));
    ColorInfoSegment **segs = (ColorInfoSegment **)malloc (_sets_con_alloc
      *sizeof(ColorInfoSegment *));
    if (_sets_con_count > 0) {
      int k2;
      for (k2 = 0; k2 < _sets_con_count; k2++) {
        sets[k2] = _sets_constructed[k2];
        segs[k2] = _segs_owned      [k2];
      }
    }
    free (_sets_constructed);
    free (_segs_owned      );
    _sets_constructed = sets;
    _segs_owned       = segs;
  }
  _sets_constructed[_sets_con_count] = (ColorInfoSet     *)set;
  _segs_owned      [_sets_con_count] = (ColorInfoSegment *)seg;
  _sets_con_count++;
  char test_str[11];
  sprintf (test_str, "%u", set);
  if (strstr(test_str,_test_str)) {
    printf ("constructed ColorInfoSegment: %u\n", set);
  }
}

int TrackColorSegments::removeColorInfoSet (const ColorInfoSet *set)
{
  if (wasConstructed(set)) {
    if (_sets_del_count == _sets_del_alloc) {
      _sets_del_alloc += INCR;
      ColorInfoSet **sets = (ColorInfoSet **)malloc (_sets_del_alloc
        *sizeof(ColorInfoSet *));
      if (_sets_del_count > 0) {
	int k2;
	for (k2 = 0; k2 < _sets_del_count; k2++) {
	  sets[k2] = _sets_deleted[k2];
	}
      }
      free (_sets_deleted);
      _sets_deleted = sets;
    }
    _sets_deleted[_sets_del_count] = (ColorInfoSet *)set;
    _sets_del_count++;
    char test_str[11];
    sprintf (test_str, "%u", set);
    if (strstr(test_str,_test_str)) {
      printf ("deleted ColorInfoSegment: %u\n", set);
    }
    return TRUE;
  }
  else {
    return FALSE;
  }
}

int TrackColorSegments::isUnique (ColorInfoSegment *segment) const
{
  return existingOwnerCount(segment) <= 1;
}

void TrackColorSegments::renderEmpty (ColorInfoSegment *segment)
{
  int k2;

  for (k2 = 0; k2 < _sets_con_count; k2++) {
    if (_segs_owned[k2] == segment && !wasDeleted(_sets_constructed[k2])) {
      _sets_constructed[k2]->renderEmpty (segment);
    }
  }
}

int TrackColorSegments::existingOwnerCount (
  ColorInfoSegment *segment) const
{
  int retval, k2;

  for (k2 = 0, retval = 0; k2 < _sets_con_count; k2++) {
    if (_segs_owned[k2] == segment && !wasDeleted(_sets_constructed[k2])) {
      retval++;
    }
  }
  return retval;
}

int TrackColorSegments::wasConstructed (const ColorInfoSet *set) const
{
  int k2;
  for (k2 = 0; k2 < _sets_con_count; k2++) {
    if (_sets_constructed[k2] == set) return TRUE;
  }
  return FALSE;
}

int TrackColorSegments::wasDeleted (const ColorInfoSet *set) const
{
  int k2;
  for (k2 = 0; k2 < _sets_del_count; k2++) {
    if (_sets_deleted[k2] == set) return TRUE;
  }
  return FALSE;
}
