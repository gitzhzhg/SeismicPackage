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
//********************************************************
//Author Michael L. Sherrill
//A base class for the various FgQcPlot classes
//********************************************************

#ifndef FG_QC_PLOT_TYPE_H
#define FG_QC_PLOT_TYPE_H

#include "dp/auto_gridder_float.hh"
#include "dp/float_grid.hh"
#include "dp/float_grid_accessor.hh"
#include "dp/control_points.hh"
#include "dp/hill_shader.hh"
#include "geom/field_geometry.hh"
#include "fgqc/fgqc_plot.hh"
#include "fgqc/fgqc_pick_menu.hh"
#include "sp/do_abort.hh"

enum{CHECK_CONTROL, CHECK_AUTO, CHECK_GRID, CHECK_SHADER, CHECK_FGA};

class FgQcPlotType {

  private:
    
  protected:

    FgQcPlot         *_qcp;
    HillShader       *_hill_shader;
    long             _num_points;
    float            _minx;
    float            _maxx;
    float            _miny;
    float            _maxy;
    float            _minz;
    float            _maxz;
    float            _offset_min;
    float            _offset_max;
    Boolean          _shaded;
    FloatGrid         *_float_grid;
    FloatGridAccessor *_float_grid_accessor;
    FloatGrid         *_temp_grid;
    ControlPoints     *_control_points;
    AutoGridderFloat  *_auto_gridder_float;
    DoAbort           *_do_abort;
    float             *_fg_data;
    int               _dont_scale;
    int               _max_hits;
    int               _which_dimension;
    int               _image_col1;
    int               _image_row1;
    int               _image_ncols;
    int               _image_nrows;
    int               _image_coordinate_system;
    int               _texture_state;
    int               _point_size;
    float             _not_defined;
    float             _x_reach;
    float             _y_reach;
    Boolean checkGridStatus(int check_type, int dont_post=0);

  public:
    enum {BOTH_X_AND_Y, X_ONLY, Y_ONLY};//same as AutoGridderFloat
    FgQcPlotType( FgQcPlot *qcp );
    virtual ~FgQcPlotType();
    FgQcPickerMenu   *_picker_menu;
    virtual int plot() = 0;
    virtual int postPlot () { return 0; }
    virtual int editData() = 0;
    virtual int ValuesChanged(FieldGeometry *fg, long ixl,
                      int ident,         long index, 
                      long nrem,         long nins) = 0;
    virtual void postNewActiveFlag(FieldGeometry * /*fg*/, long /*ixl*/){};
    virtual void getCoordinates(float *x1, float *x2, float *y1, float *y2);
    virtual long getNumPoints()    { return _num_points;}
    int getCoordinateSystem()      { return _qcp->getCoordinateSystem();}
    int getImageCoordinateSystem() { return _image_coordinate_system; }
    HillShader *getHillShader()    { return _hill_shader;}
    FloatGrid *getFloatGrid()      { return _float_grid;}
    FgQcPickerMenu *getPickerMenu(){ return _picker_menu;}
    int Texture();
    void setTexture(int texture);
    int Pointsize();
    void setPointsize(int point_size);
    Boolean applyHillShader();
    Boolean removeHillShader();
    void setOffsets(float min,float max){_offset_min = min; _offset_max = max;}
    int getDimensions();
};

#endif
