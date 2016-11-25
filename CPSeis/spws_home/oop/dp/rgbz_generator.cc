// rgbz_generator.cc:  Implementation file for RGBZGenerator class
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
#include "dp/rgbz_generator.hh"

RGBZGenerator::RGBZGenerator () :

// initialize pointers to NULL
  _z_lut          (0),
  _rgbz           (0),
  _newz           (0),
  _rgbz_remapper  (0),

// initialize some constants
  _z_changed      (0),
  _z_lut_size     (0),
  _offset         (0),
  _z_lut_offset   (0),
  _rgbz_count     (0),
  _intensity      (10),
  _compression    (0),
  _error_status   (RGB_SUCCESSFUL)
{
}

RGBZGenerator::~RGBZGenerator ()
{
  reclaimMemory ();
}


int RGBZGenerator::setSize (int rgbz_count)
{
  if (rgbz_count < 1) {
    _error_status = RGB_BAD_INPUTS;
    return 0;
  }
  
// initialize as required
  _z_changed |= (rgbz_count != _rgbz_count);
  if (_z_changed) {
    if (_rgbz) {

// retain the old Z's
      int k2, offset = 3;
      for (k2 = 0; k2 < _rgbz_count; k2++) {
        _newz[k2] = _rgbz[offset];
        offset += 4;
      }
      delete [] _rgbz, _rgbz = 0;
      _rgbz = new float[rgbz_count*4];
      if (!_rgbz) {
        _error_status = RGB_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
      for (k2 = 0; k2 < rgbz_count*4; k2++)  // clear out RGBZ's
        _rgbz[k2] = 0;
      float step_size;
      if (rgbz_count < 2) {
        step_size = (float)0;
      }
      else {
        step_size = (float) (_rgbz_count - 1) / (float) (rgbz_count - 1);
      }
      float step = 0;
      for (k2 = 3; k2 < rgbz_count*4; k2 += 4) {
        offset = (int) (step + 0.5);
        _rgbz[k2] = _newz[offset];
        step += step_size;
      }
      _rgbz_count = rgbz_count;
      if (_rgbz_remapper) {
        if (!_rgbz_remapper->resetArray((const float *)_rgbz,_rgbz_count,
          (int)4)) {
          _error_status = _rgbz_remapper->errorStatus ();
          return 0;
        }
      }
      delete [] _newz, _newz = 0;
      _newz = new float[_rgbz_count];
      if (!_newz) {
        _error_status = RGB_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
      for (k2 = 0; k2 < _rgbz_count; k2++)  // clear out Z's
        _newz[k2] = 0;
    }

    else {
      _rgbz_count = rgbz_count;
// _rgbz contains the current RGB's and Z-values assoiciated with each.
      _rgbz = new float[_rgbz_count*4];
      if (!_rgbz) {
        _error_status = RGB_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
      int k2;
      for (k2 = 0; k2 < _rgbz_count*4; k2++)  // clear out RGBZ's
        _rgbz[k2] = 0;
// _newz contains the Z-values that are associated with each RGB.  They must
//   be a monotonic function of RGB.
      _newz = new float[_rgbz_count];
      if (!_newz) {
        _error_status = RGB_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
      for (k2 = 0; k2 < _rgbz_count; k2++)  // clear out Z's
        _newz[k2] = 0;
    }
  }
  return 1;
}

void RGBZGenerator::setOne (int index, float red, float green, float blue,
  float attribute)
{
// look to see if the incoming Z has changed
  _offset = index * 4;
  _z_changed |= (_rgbz[_offset+3] != attribute);

// copy the incoming values to the internal RGBZ element
  _rgbz[_offset]   = red;
  _rgbz[_offset+1] = green;
  _rgbz[_offset+2] = blue;
  _newz[index]     = attribute;
}

void RGBZGenerator::setNext (float red, float green, float blue,
  float attribute)
{
// look to see if the incoming Z has changed
  _offset += 4;
  _z_changed |= (_rgbz[_offset+3] != attribute);

// copy the incoming values to the next internal RGBZ element
  _rgbz[_offset]   = red;
  _rgbz[_offset+1] = green;
  _rgbz[_offset+2] = blue;
  _newz[_offset/4] = attribute;
}

void RGBZGenerator::getOne (int index, float *red, float *green, float *blue,
  float *attribute)
{
// copy the internal RGBZ element to the outgoing values
  _offset    = index * 4;
  *red       = _rgbz[_offset];
  *green     = _rgbz[_offset+1];
  *blue      = _rgbz[_offset+2];
  *attribute = _rgbz[_offset+3];
}

void RGBZGenerator::getNext (float *red, float *green, float *blue,
  float *attribute)
{
// copy the internal RGBZ element to the outgoing values
  _offset += 4;
  *red       = _rgbz[_offset];
  *green     = _rgbz[_offset+1];
  *blue      = _rgbz[_offset+2];
  *attribute = _rgbz[_offset+3];
}

int RGBZGenerator::setIntensity (long intensity)
{
// check for nothing to do
  if (intensity == _intensity) return (int)1;

  if (_rgbz) {
    if (intensity == (long)10 && _compression == (long)0) {
// no need for a remapper
      if (_rgbz_remapper) {
        delete _rgbz_remapper, _rgbz_remapper = (ArrayRemapper *)0;
        _intensity = (long)10;
        return (int)1;
      }
    }

    else if (!_rgbz_remapper) {
// need a new remapper
      _rgbz_remapper = new ArrayRemapper ((const float *)_rgbz, _rgbz_count,
        (int)4);
      if (!_rgbz_remapper) {
        _error_status = RGB_MEMORY_ALLOCATION_ERROR;
        return (int)0;
      }
      else if (_rgbz_remapper->failed()) {
        _error_status = _rgbz_remapper->errorStatus ();
        return (int)0;
      }
    }

// establish the gain necessary
    _intensity = intensity;
    _rgbz_remapper->setGain ((float)_intensity/(float)10);
    return (int)1;
  }
  else return (int)0; // _rgbz must exist first
}

int RGBZGenerator::setCompression (long compression)
{
// check for nothing to do
  if (compression == _compression) return (int)1;

  if (_rgbz) {
    if (compression == (long)0 && _intensity == (long)10) {
// no need for a remapper
      if (_rgbz_remapper) {
        delete _rgbz_remapper, _rgbz_remapper = (ArrayRemapper *)0;
        _compression = (long)0;
        return (int)1;
      }
    }

    else if (!_rgbz_remapper) {
// need a new remapper
      _rgbz_remapper = new ArrayRemapper ((const float *)_rgbz, _rgbz_count,
        (int)4);
      if (!_rgbz_remapper) {
        _error_status = RGB_MEMORY_ALLOCATION_ERROR;
        return (int)0;
      }
      else if (_rgbz_remapper->failed()) {
        _error_status = _rgbz_remapper->errorStatus ();
        return (int)0;
      }
    }

// establish the gain necessary
    _compression = compression;
    _rgbz_remapper->setCompression ((int)_compression);
    return (int)1;
  }
  else return (int)0; // _rgbz must exist first
}

int RGBZGenerator::failed ()
{
  return (int) (_error_status != RGB_SUCCESSFUL);
}

void RGBZGenerator::reset ()
{
  reclaimMemory ();
  _z_changed    = 0;
  _z_lut_size   = 0;
  _rgbz_count   = 0;
  _intensity    = 10;
  _compression  = 0;
  _error_status = RGB_SUCCESSFUL;
}

int RGBZGenerator::newZVaries ()
{
// if Z's have changed, see if they have changed to merely be a constant
//   value
  int variable_new_z = 0;
  if (_z_changed) {
    if (_rgbz_count < 2) {
      variable_new_z = 1;
    }
    else {
      for (int k2 = 1; k2 < _rgbz_count && !variable_new_z; k2++)
        variable_new_z |= _newz[k2] != _newz[k2-1];
    }
  }
  return variable_new_z;
}

int RGBZGenerator::ZVaries ()
{
// do Z's in _rgbz vary
  int variable_z = 0;
  for (int k2 = 7; k2 < _rgbz_count*4 && !variable_z; k2+=4)
    variable_z |= _rgbz[k2] != _rgbz[k2-4];
  return variable_z;
}

int RGBZGenerator::ZLUTAvailable (int z_lut_size)
{
  if (z_lut_size <= 0) {
    _error_status = RGB_BAD_INPUTS;
    return 0;
  }
// _z_lut stores the indices that connect generator with individual Z-values
  if (z_lut_size != _z_lut_size && _z_lut) delete [] _z_lut, _z_lut = 0;
  if (!_z_lut) {
    _z_lut = new int[z_lut_size];
    if (!_z_lut) {
      _error_status = RGB_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    _z_lut_size = z_lut_size;
  }
  return 1;
}

void RGBZGenerator::clearZs ()
{
  int k2;
  for (k2 = 3; k2 < _rgbz_count*4; k2+=4)
    _rgbz[k2] = 0;
}

void RGBZGenerator::reclaimMemory ()
{
  if (_z_lut)         delete [] _z_lut,         _z_lut         = 0;
  if (_rgbz)          delete [] _rgbz,          _rgbz          = 0;
  if (_newz)          delete [] _newz,          _newz          = 0;
  if (_rgbz_remapper) delete    _rgbz_remapper, _rgbz_remapper = 0;
}
