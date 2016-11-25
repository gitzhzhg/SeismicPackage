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
#ifndef _MODELDESC_HH
#define _MODELDESC_HH

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "model.h"
#include "oprim/base_data.hh"

#define MOD_STRUC 0
#define MOD_CELLP 1
#define MOD_CELLB 2
#define MOD_MDATA 3


class Vector;
class VectorLinkedList;
class VectorListData;
class CoordData;
class gridData;

/*********************************************************
 * C\USER DOC
 * The ModelDesc class describes a layered model.
 * It can also convert an ErsModel into a ModelDesc class.
 * C\END DOC 
 ********************************************************/

class ModelDesc: public BaseData
{
  public:

    ModelDesc();
    ModelDesc(ErsModel *);
    ~ModelDesc();

    
    char  *Name() { return _name; }
    void   setName(char *name);
    void   setMLimits(ModLimits *mlim)  { _mlim = mlim; }
    void   setGLimits(GridLimits *glim) { _glim = glim; }
    void   setCoordData(CoordData *cdata) { _csys_data = cdata; }
    void   setStructure(VectorListData *vld)  { _sdata = vld;}
    void   setPointers(VectorListData *vld)   { _pdata = vld;}
    void   setBoundaries(VectorListData *vld) { _bdata = vld;}
    void   setMaterials(VectorListData *vld)  { _mdata = vld;}
    void   setVLDComponent(int id, VectorListData *vld);
    void   setGridData(gridData *gd) { _grid_data = gd; }
    ErsTransforms *transforms();
    void           gettrans(ErsTransform **tx,ErsTransform **ty,
                   ErsTransform **tz);
    CoordData     *coord_data() { return _csys_data; }
    gridData      *GridData() { return _grid_data; }
    ModLimits     *MLimits() { return _mlim; }
    GridLimits    *GLimits() { return _glim; }
    VectorListData   *VLDComponent(int id);
    VectorLinkedList *VLLComponent(int id);

    int       transform(char *xname, char *yname, char *zname);
    ErsModel *toErsModel();
    ModelDesc *copy();

  protected:

    char             *_name;
    CoordData        *_csys_data;
    ModLimits        *_mlim;
    GridLimits       *_glim;
    VectorListData   *_pdata;
    VectorListData   *_sdata;
    VectorListData   *_mdata;
    VectorListData   *_bdata;
    gridData         *_grid_data;

    void setup();

  private:


};

#endif




