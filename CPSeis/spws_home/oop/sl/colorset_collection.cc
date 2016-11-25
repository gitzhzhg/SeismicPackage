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
#include "sl/colorset_collection.hh"
#include "sl/colorset.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define MAXSETS 250

static Colorset *COLORSETS[MAXSETS];
static const char *NAMES[MAXSETS];
int ColorsetCollection::NUM = 0;
int ColorsetCollection::RECOLOR_CURRENT_ONLY = TRUE;
Colorset *ColorsetCollection::SHARED_COLORSET = NULL;

Colorset *ColorsetCollection::fetch (ColorInfo *col)
{
  assert (col);

  int k2 = findExisting (col);
  if (k2 > -1) {
    return COLORSETS[k2];
  }

  assert (NUM < 1000);
  Paintset *paintset = PaintsetCollection::fetchExisting (col->cmap);
  Colorset *colorset = new Colorset (paintset);
  COLORSETS[NUM] = colorset;
  NAMES[NUM] = nameIs (paintset, col);
  NUM++;
  return colorset;
}

Colorset *ColorsetCollection::fetchExisting (ColorInfo *col)
{
  int k2 = findExisting (col);
  if (k2 > -1) {
    return COLORSETS[k2];
  }
  else {
    assert (0);
  }
}

// do this anytime a ColorInfo is deleted or freed
void ColorsetCollection::remove (ColorInfo *col)
{
  int k2;
  if (col) {
    k2 = findExisting (col);
    if (k2 > -1) {
      delete  COLORSETS   [k2] ;
      free   ((void*)NAMES[k2]);
      // don't forget to do the corresponding
      //   ColorInfoCollection::remove (col)
      NUM--;
      for (; k2 < NUM; k2++) {
	COLORSETS[k2] = COLORSETS[k2+1];
	NAMES    [k2] = NAMES    [k2+1];
      }
      COLORSETS[k2] = NULL;
      NAMES    [k2] = NULL;
    }
    else {
      // assert (0); // didn't find and Colorset associated with given col
    }
  }
  else {
    for (k2 = 0; k2 < NUM; k2++) {
      delete  COLORSETS   [k2] ;
      free   ((void*)NAMES[k2]);
      // don't forget to do the corresponding
      //   ColorInfoCollection::remove (col)
      COLORSETS[k2] = NULL;
      NAMES    [k2] = NULL;
    }
    NUM = 0;
  }
}

int ColorsetCollection::allocate (ColorInfo *col)
{
  Colorset *colorset = fetch (col);
  return allocate (colorset, col);
}

void ColorsetCollection::clear (ColorInfo *col)
{
  int k2 = findExisting (col);
  if (k2 > -1) {
    Colorset *colorset = COLORSETS[k2];
    if (colorset->numColors() > 0) {
      colorset->freeColorbar ();
      colorset->shareWith ();
      update (colorset, col);
    }
  }
}

void ColorsetCollection::store (ColorInfo *col, XColor *colors)
{
  Colorset *colorset = fetch (col);
  assert (!allocate(colorset,col));
  setColors (colorset, colors);
  update (colorset, col);
}

void ColorsetCollection::retrieve (ColorInfo *col, XColor **colors)
{
  Colorset *colorset = fetchExisting (col);
  int cnum = colorset->numColors ();
  assert (cnum > 0 && cnum == col->cnum);
  *colors = (XColor *)malloc (cnum*sizeof(XColor));
  getColors (colorset, *colors);
}

void ColorsetCollection::transferPixels (ColorInfo *from)
{
  Colorset *to = fetchExisting (from);
  assert (from->cmap == to->paintset()->colormap() &&
          !to->readOnly()                            );
  // warning: this is very dangerous!!!
  to->transferPixels (from->cnum, from->pix, from->numplanes?1:0);
}

int ColorsetCollection::readOnly (ColorInfo *col)
{
  Colorset *colorset = fetch (col);
  return colorset->readOnly ();
}

int ColorsetCollection::readOnly (Widget widget)
{
  Paintset *paintset = PaintsetCollection::fetchExistingByColormap (widget);
  return paintset->readOnly ();
}

void ColorsetCollection::setRecolorCurrentOnly (int flag)
{
  RECOLOR_CURRENT_ONLY = flag;
}

int ColorsetCollection::recolorCurrentOnly ()
{
  return RECOLOR_CURRENT_ONLY;
}

void ColorsetCollection::shareWith (ColorInfo *col)
{
  if (col) {
    Colorset *colorset = fetch (col);
    SHARED_COLORSET = colorset;
  }
  else {
    SHARED_COLORSET = NULL;
  }
}

void ColorsetCollection::update (Colorset *from, ColorInfo *to)
{
  to->numplanes = from->planeAllocated() ? 1 : 0;
  to->cmap      = from->paintset()->colormap ();
  to->shared    = shared (from);
  to->cnum      = from->numColors ();
  to->colorsafe = to->cnum > 0;
  int k2;
  if (to->numplanes > 0) {
    to->pmsk[0] = from->getPlane ();
    k2 = 1;
  }
  else {
    k2 = 0;
  }
  for (; k2 < 24; to->pmsk[k2++] = 0);
  for (k2 = 0; k2 < to->cnum; to->pix[k2] = from->getPixel(k2), k2++);
  for (; k2 < 500; to->pix[k2++] = 0);
}

void ColorsetCollection::setColors (Colorset *to, const XColor *colors)
{
  if (to->numColors() > 0) {
    if (colors) {
      int *reds, *greens, *blues;
      reds   = (int *)malloc (to->numColors()*sizeof(int));
      greens = (int *)malloc (to->numColors()*sizeof(int));
      blues  = (int *)malloc (to->numColors()*sizeof(int));
      int k2;
      for (k2 = 0; k2 < to->numColors(); k2++) {
	reds  [k2] = colors[k2].red  ;
	greens[k2] = colors[k2].green;
	blues [k2] = colors[k2].blue ;
      }
      to->setColors (reds, greens, blues);
      free (reds  );
      free (greens);
      free (blues );
    }
    else {
      to->setColors ();
    }
  }
}

void ColorsetCollection::getColors (Colorset *from, XColor *colors)
{
  Paintset *paintset = from->paintset ();
  int k2;
  for (k2 = 0; k2 < from->numColors(); k2++) {
    paintset->getXColor (from->getPixel(k2), &colors[k2]);
  }
}

int ColorsetCollection::shared (Colorset *colorset)
{
  Paintset *paintset = colorset->paintset ();
  if (paintset->readOnly()) return FALSE;

  int k2, count;
  for (k2 = 0, count = 0; k2 < NUM; k2++) {
    colorset = COLORSETS[k2];
    Paintset *paintset2 = colorset->paintset ();
    if (paintset2 == paintset) count++;
  }
  return count > 1;
}

int ColorsetCollection::allocate (Colorset *colorset, ColorInfo *col)
{
  int error;

  if (colorset->numColors() < col->cnum) {
    colorset->shareWith (SHARED_COLORSET);
    error = colorset->allocateColorbar (col->cnum, col->numplanes>0?1:0);
    Paintset *paintset = colorset->paintset ();
    if (!paintset->readOnly() && col->numplanes > 0) {
      col->pmsk[0] = colorset->getPlane (); // only works with the first plane
    }
    if (!error) {
      col->colorsafe = 1;
    }
    else {
      col->colorsafe = 0;
    }
  }
  else {
    error = 0;
  }
  return error;
}

int ColorsetCollection::findExisting (ColorInfo *col)
{
  assert (col);
  Paintset *paintset = PaintsetCollection::fetchExisting (col->cmap);
  char *name = (char *)nameIs (paintset, col);

  Colorset *colorset;
  int k2, retval;
  for (k2 = 0, retval = -1; k2 < NUM; k2++) {
    colorset = COLORSETS[k2];
    Paintset *paintset2 = colorset->paintset ();
    const char *name2 = NAMES[k2];
    if (paintset2 != paintset) continue;
    if (strcmp(name2,(const char *)name) != 0) continue;
    retval = k2;
    k2 = NUM;
  }
  free (name);
  return retval;
}

const char *ColorsetCollection::nameIs (Paintset *paintset, ColorInfo *col)
{
  char name[30], *retval;
  sprintf (name, "ColorInfo_%ld", col);
  retval = (char *)malloc ((strlen(name)+1)*sizeof(char));
  strcpy (retval, name);
  return (const char *)retval;
}

void ColorsetCollection::print ()
{
  if (NUM > 0) {
    printf ("////////// Colorset Collection Beginning //////////\n");
    int k2;
    for (k2 = 0; k2 < NUM; k2++) {
      printf ("Colorset: %ld named: %s\n", COLORSETS[k2], NAMES[k2]);
    }
    printf ("////////// Colorset Collection Ending //////////\n");
  }
}
