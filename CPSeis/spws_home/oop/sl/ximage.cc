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
#include "sl/ximage.hh"
#include "sl/paintset.hh"
#include "sl/colorset.hh"
#include "sl/colorset_collection.hh"
#include "sl/color_info_set.hh"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

Ximage::Ximage (ColorInfo *col) :
  _ximage    (NULL),
  _col       (NULL),
  _paintset  (NULL),
  _colorset  (NULL)
{
  assert (col);

  _col_segment = new ColorInfoSegment (this, ColorInfoSet::XIMAGE);

  create (_ximage, col);
}

Ximage::~Ximage ()
{
  ColorInfoSet *set = ColorInfoCollection::fetchIfExisting (_col);
  if (set) {
    set->removeSharedElement (_col_segment);
  }
  CheckColorSegments::deleteSegment (_col_segment);

  destroy ();
}

XImage *Ximage::update (XImage *ximage)
{
  XImage *retval;

  if (changed(ximage)) {
    Boolean preserve_data_array = dataAllocated (ximage);
    create (ximage);
    if (preserve_data_array) allocate (ximage);
  }
  return assignTo (ximage);
}

XImage *Ximage::allocate (XImage *ximage)
{
  XImage *retval;

  if (changed(ximage)) create (ximage);
  assert (_data == _ximage->data);
  if (_ximage->data) {
    _ximage->data = (char *)realloc (_ximage->data,
      _ximage->height*_ximage->bytes_per_line*sizeof(char));
  }
  else {
    _ximage->data = (char *)calloc (1, _ximage->height
      *_ximage->bytes_per_line*sizeof(char));
  }
  _data = _ximage->data;

  retval = _ximage->data ? assignTo(ximage) : NULL;

  return retval;
}

XImage *Ximage::deallocate (XImage *ximage)
{
  XImage *retval;

  if (ximage->data) {
    free (_ximage->data);
    ximage->data = NULL;
  }
  retval = update (ximage);
  return retval;
}

XImage *Ximage::createPixmap (Ximage *from_ximage_ptr, int depth)
{
  if (_ximage) destroy ();
  assert (from_ximage_ptr && from_ximage_ptr->_ximage &&
    from_ximage_ptr->_col);
  if (depth > 1) {
    _depth  = 24;
    _format = ZPixmap;
  }
  else {
    _depth  = 24;
    _format = XYBitmap;
  }
  _offset = 0;
  _xsize  = from_ximage_ptr->_ximage->width;
  _ysize  = from_ximage_ptr->_ximage->height;
  _pad    = 32;
  _bytes  = 0;
  _data   = NULL;
  get ();

  return _ximage;
}

Boolean Ximage::matches (Ximage *ximage_ptr)
{
  Boolean retval;

  if (!_ximage || !ximage_ptr->_ximage) {
    retval = False;
  }
  else {
    retval =
      _ximage->width == ximage_ptr->_ximage->width   &&
      _ximage->height == ximage_ptr->_ximage->height   ;
  }
  return retval;
}

Boolean Ximage::copyData (Ximage *from_ximage_ptr)
{
  assert (from_ximage_ptr);
  assert (
    _depth      != from_ximage_ptr->_ximage->depth            ||
    _format     != from_ximage_ptr->_ximage->format           ||
    _offset     != from_ximage_ptr->_ximage->xoffset          ||
    _xsize      != from_ximage_ptr->_ximage->width            ||
    _ysize      != from_ximage_ptr->_ximage->height           ||
    _pad        != from_ximage_ptr->_ximage->bitmap_pad       ||
    _bytes      != from_ximage_ptr->_ximage->bytes_per_line   ||
    _byte_order != from_ximage_ptr->_ximage->byte_order       ||
    _unit       != from_ximage_ptr->_ximage->bitmap_unit      ||
    _bit_order  != from_ximage_ptr->_ximage->bitmap_bit_order ||
    _pixel_size != from_ximage_ptr->_ximage->bits_per_pixel   ||
    _red_mask   != from_ximage_ptr->_ximage->red_mask         ||
    _grn_mask   != from_ximage_ptr->_ximage->green_mask       ||
    _blu_mask   != from_ximage_ptr->_ximage->blue_mask        ||
    _data       == from_ximage_ptr->_ximage->data               );
  // if _data was equal to from_ximage_ptr->_ximage->data then there
  //   would be no reason to call this function in the first place!!!!

  Boolean retval;

  if (_ximage->data) {
    free (_ximage->data);
    _ximage->data = NULL;
  }

  size_t num_bytes = _ximage->height * _ximage->bytes_per_line * sizeof(char);
  if (from_ximage_ptr->_ximage->data) {
    _ximage->data = (char *)malloc (num_bytes);

    _data = _ximage->data;

    if (_data) {
      memcpy (_ximage->data, from_ximage_ptr->_ximage->data, num_bytes);
    }
  }

  retval = _ximage->data ? True : False;
  return retval;
}

Boolean Ximage::changed (XImage *ximage)
{
  Boolean retval;

  if (ximage) {
    // compare internals to given XImage
    retval = _depth      != ximage->depth            ||
             _format     != ximage->format           ||
             _offset     != ximage->xoffset          ||
             _xsize      != ximage->width            ||
             _ysize      != ximage->height           ||
             _pad        != ximage->bitmap_pad       ||
             _bytes      != ximage->bytes_per_line   ||
             _byte_order != ximage->byte_order       ||
             _unit       != ximage->bitmap_unit      ||
             _bit_order  != ximage->bitmap_bit_order ||
             _pixel_size != ximage->bits_per_pixel   ||
             _red_mask   != ximage->red_mask         ||
             _grn_mask   != ximage->green_mask       ||
             _blu_mask   != ximage->blue_mask        ||
             _data       != ximage->data               ;
  }
  else if (_ximage) {
    // compare internals to internal XImage
    retval = _depth      != _ximage->depth            ||
             _format     != _ximage->format           ||
             _offset     != _ximage->xoffset          ||
             _xsize      != _ximage->width            ||
             _ysize      != _ximage->height           ||
             _pad        != _ximage->bitmap_pad       ||
             _bytes      != _ximage->bytes_per_line   ||
             _byte_order != _ximage->byte_order       ||
             _unit       != _ximage->bitmap_unit      ||
             _bit_order  != _ximage->bitmap_bit_order ||
             _pixel_size != _ximage->bits_per_pixel   ||
             _red_mask   != _ximage->red_mask         ||
             _grn_mask   != _ximage->green_mask       ||
             _blu_mask   != _ximage->blue_mask          ;
  }
  else {
    // internal XImage doesn't even exist now
    retval = True;
  }
  return retval;
}

XImage *Ximage::create (XImage *ximage, ColorInfo *col)
{
  if (_ximage) destroy ();
  set (ximage, col);
  get ();
  return assignTo (ximage);
}

XImage *Ximage::assignTo (XImage *ximage)
{
  XImage *retval;

  if (ximage) {
    assert (_ximage);
    assert (_ximage->data == _data);

    ximage->depth            = _ximage->depth           ;
    ximage->format           = _ximage->format          ;
    ximage->xoffset          = _ximage->xoffset         ;
    ximage->width            = _ximage->width           ;
    ximage->height           = _ximage->height          ;
    ximage->data             = _ximage->data            ;
    ximage->bitmap_pad       = _ximage->bitmap_pad      ;
    ximage->bytes_per_line   = _ximage->bytes_per_line  ;
    ximage->byte_order       = _ximage->byte_order      ;
    ximage->bitmap_unit      = _ximage->bitmap_unit     ;
    ximage->bitmap_bit_order = _ximage->bitmap_bit_order;
    ximage->bits_per_pixel   = _ximage->bits_per_pixel  ;
    ximage->red_mask         = _ximage->red_mask        ;
    ximage->green_mask       = _ximage->green_mask      ;
    ximage->blue_mask        = _ximage->blue_mask       ;
    ximage->obdata           = _ximage->obdata          ;
    ximage->f.create_image   = _ximage->f.create_image  ;
    ximage->f.destroy_image  = _ximage->f.destroy_image ;
    ximage->f.get_pixel      = _ximage->f.get_pixel     ;
    ximage->f.put_pixel      = _ximage->f.put_pixel     ;
    ximage->f.sub_image      = _ximage->f.sub_image     ;
    ximage->f.add_pixel      = _ximage->f.add_pixel     ;
    retval = ximage;
    // printf ("_ximage 0x%x assigned to ximage 0x%x\n", _ximage, ximage);
  }
  else {
    retval = _ximage;
  }
  return retval;
}

Paintset *Ximage::paintset ()
{
  return _paintset;
}

Colorset *Ximage::colorset ()
{
  return _colorset;
}

ColorInfo *Ximage::colorInfo ()
{
  return _col;
}

void Ximage::setColorInfo (ColorInfo *col)
{
  if (col == _col) return;

  ColorInfoSet *set;
  int role;
  if (_col) {
    set = ColorInfoCollection::fetchIfExisting (_col);

    if (set && set->matchesElement(_col_segment)) {
      role = set->role (_col_segment);
      set->removeSharedElement (_col_segment);
    }
    else {
      role = ColorInfoSet::SENDER;
    }
  }
  else {
    role = ColorInfoSet::SENDER;
  }

  _col = col;
  _colorset = ColorsetCollection::fetch (_col);
  _paintset = _colorset->paintset ();

  set = ColorInfoCollection::fetchExisting (_col);
  set->addSharedElement (_col_segment, role);
}

XImage *Ximage::get ()
{
  if (!_ximage) {
    _bytes = 0;
    // handle data outside of XCreateImage
    if (_depth > 1) _depth = 24;
    _ximage = XCreateImage (paintset()->display(), paintset()->visual(),
      _depth, _format, _offset, NULL, _xsize, _ysize, _pad, _bytes);
    if (_ximage) {
      _bytes      = _ximage->bytes_per_line;
      _byte_order = _ximage->byte_order;
      _unit       = _ximage->bitmap_unit;
      _bit_order  = _ximage->bitmap_bit_order;
      _pixel_size = _ximage->bits_per_pixel;
      _red_mask   = _ximage->red_mask;
      _grn_mask   = _ximage->green_mask;
      _blu_mask   = _ximage->blue_mask;
      _data       = NULL;
      assert (_data == _ximage->data);
      // printf ("_ximage created: 0x%x\n", _ximage);
    }
  }
  return _ximage;
}

// set only those parameters necessary
void Ximage::set (XImage *ximage, ColorInfo *col)
{
  if (col) {
    setColorInfo (col);
  }
  if (ximage) {
    _depth  = ximage->depth;
    _format = ximage->format;
    _offset = ximage->xoffset;
    _xsize  = ximage->width;
    _ysize  = ximage->height;
    _pad    = ximage->bitmap_pad;
    _bytes  = ximage->bytes_per_line;
    _data   = ximage->data;
  }
  else {
    _depth  = 1;
    _format = XYBitmap;
    _offset = 0;
    _xsize  = 0;
    _ysize  = 0;
    _pad    = 32;
    _bytes  = 0;
    _data   = NULL;
  }
}

void Ximage::destroy ()
{
  if (_ximage) {
    if (_ximage->data) {
      // free data outside of XDestroyImage
      // free (_ximage->data); // free needs to have been done externally!!
      _ximage->data = NULL;
      _data = NULL;
    }
    // printf ("_ximage destroyed: 0x%x\n", _ximage);
    XDestroyImage (_ximage);
    _ximage = NULL;
  }
}

Boolean Ximage::dataAllocated (XImage *ximage)
{
  Boolean retval;

  if (ximage) {
    retval = ximage->data != NULL;
  }
  else {
    assert (_data == _ximage->data);
    retval = _ximage->data != NULL;
  }
  return retval;
}
