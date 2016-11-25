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
//**************************************************************************
//                Author Michael L. Sherrill  08/95
//                   Elevation plot type class
//**************************************************************************

#ifndef FG_QC_GRID_H
#define FG_QC_GRID_H

#include "fgqc/fgqc_plot_type.hh"


class FgQcGridType : public FgQcPlotType {

  private:

  protected:
    void computeNumPoints(float xmin, float xmax, float ymin, float ymax);
    class SeisVectLinkedList *_vect_ll;
    class VectData           *_vect_data;
    class Vector             *_Vector;
    class GridLines          *_grid_lines;
    class FgMapAnno          *_fgmap_anno;
    float                    *_xvect;
    float                    *_yvect;
    int GridMatrix();

  public:
    FgQcGridType( FgQcPlot *fgqc_plot );
    virtual ~FgQcGridType();
    int plot();
    int editData();
    int ValuesChanged(FieldGeometry *fg, long ixl,
                      int ident,         long index, 
                      long nrem,         long nins);
    int modifyGridMatrix(float *xgrid, float *ygrid, long numpoints);
};

#endif
