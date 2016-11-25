//********************************************************
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
//Author Michael L. Sherrill 08/95
//A base class for the various FgQcPlot classes
//Elevations, Fold of Stack etc.
//********************************************************


#include "fgqc/fgqc_plot_type.hh"
#include "dp/grid_error_handler.hh"
#include "sp/do_abort.hh"
#include "fgqc/fgseis_color_pop.hh"
#include "named_constants.h"


FgQcPlotType::FgQcPlotType( FgQcPlot *qcp ) :
                           _qcp(qcp), _fg_data(0)
{
  _num_points              = 0;
  _not_defined             = 0;
  _minx                    = MAX_NUM;
  _maxx                    = MIN_NUM;
  _miny                    = MAX_NUM;
  _maxy                    = MIN_NUM;
  _minz                    = MAX_NUM;
  _maxz                    = MIN_NUM;
  _control_points          = NULL;
  _auto_gridder_float      = NULL;
  _hill_shader             = NULL;
  _float_grid              = NULL;
  _float_grid_accessor     = NULL;
  _temp_grid               = NULL;
  _picker_menu             = NULL;
  _image_coordinate_system = _qcp->getCoordinateSystem();
  _do_abort = new DoAbort (_qcp->plotWidget());
  _texture_state = _qcp->getFgColorPop()->Texture();
  _point_size    = _qcp->getFgColorPop()->Pointsize();
}


FgQcPlotType::~FgQcPlotType()
{
                                   delete _do_abort;
  if(_control_points      != NULL) delete _control_points;
  if(_auto_gridder_float  != NULL) delete _auto_gridder_float;
  if(_hill_shader         != NULL) delete _hill_shader;
  if(_picker_menu         != NULL) delete _picker_menu;
  if(_float_grid          != NULL) delete _float_grid;
  if(_float_grid_accessor != NULL) delete _float_grid_accessor;
  if(_temp_grid           != NULL) delete _temp_grid;
}

//============================================================================
//====================== Get hill shader data and plot it ====================
//============================================================================
Boolean FgQcPlotType::applyHillShader()
{
int frame_num = 1, num_frames = 1; //may want movies later


  if(_hill_shader == NULL) return(False);


  _shaded= _qcp->sp()->initArrayTypeData(frame_num, num_frames, _qcp->getNumx(),
                                         _qcp->getNumy(),
                                         _hill_shader->getResult()); 
  if(!_shaded) return(_shaded);

  _shaded = _qcp->sp()->plot();

  return(_shaded);
}



//============================================================================
//====================== Remove hill shader ==================================
//============================================================================
Boolean FgQcPlotType::removeHillShader()
{
Boolean stat;
int frame_num = 1, num_frames = 1; //may want movies later

  if(_hill_shader == NULL || _shaded == False) return(False);

  stat= _qcp->sp()->initArrayTypeData(frame_num, num_frames, _qcp->getNumx(),
                                     _qcp->getNumy(), _float_grid->getArray());
  if(!stat) return(stat);

  stat = _qcp->sp()->plot();

  if(stat) _shaded = False;

  return(stat);
}


Boolean FgQcPlotType::checkGridStatus( int check_type, int dont_post)
{
Boolean stat;

  switch(check_type)
    {
    case CHECK_CONTROL:
      if(_control_points->failed())
        {
        if (dont_post)
          _qcp->_grid_error = False;
        else
          new GridErrorHandler(_qcp->gridError(),"Error",
                               _control_points->errorStatus());
        delete _control_points;
        _control_points = NULL;
        stat = False;
        }
      else
        {
        stat = True;
        }
      break;

   
    case CHECK_AUTO:
      if(_auto_gridder_float->failed())
        {
        if (dont_post)
          _qcp->_grid_error = False;
        else
          new GridErrorHandler(_qcp->gridError(),"Error",
                                    _auto_gridder_float->errorStatus());
        delete _auto_gridder_float;
        _auto_gridder_float = NULL;
        stat = False;
        }
      else
        {
        stat = True;
        }
      break;


     case CHECK_FGA:
       if(_float_grid_accessor != NULL)
         {
         if(_float_grid_accessor->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _float_grid_accessor->errorStatus());
           delete _float_grid_accessor;
           _float_grid_accessor = NULL;
           stat = False;
           }
         else
           {
           stat = True;
           }
         }
       break;

     case CHECK_GRID:
       if(_float_grid != NULL)
         {
         if(_float_grid->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _float_grid->errorStatus());
           delete _float_grid;
           _float_grid = NULL;
           stat = False;
           }
         else
           {
           stat = True;
           }
         }

       if(_temp_grid != NULL)
         {
         if(_temp_grid->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _temp_grid->errorStatus());
           delete _temp_grid;
           _float_grid = NULL;
           stat = False;
           }
         else
           {
           stat = True;
           }
         }
       break;

     case CHECK_SHADER:
       if(_hill_shader->failed())
         {
         if (dont_post)
           _qcp->_grid_error = False;
         else
           new GridErrorHandler(_qcp->gridError(),"Error",
                                _hill_shader->errorStatus());
         delete _hill_shader;
         _hill_shader = NULL;
         stat = False;
         }
       else
         {
         stat = True;
         }
       break;


    }

  return(stat);

}

void FgQcPlotType::getCoordinates(float *x1, float *x2, float *y1, float *y2)
{

  *x1 = MinimumValue(_minx,_maxx);
  *x2 = MaximumValue(_minx,_maxx);
  *y1 = MinimumValue(_miny,_maxy);
  *y2 = MaximumValue(_miny,_maxy);

}
//=============================================================================
//============= Loop thru data to see if x or y is varying ====================
//=============================================================================
int FgQcPlotType::getDimensions()
{
double first_xval, first_yval;
int dimension = BOTH_X_AND_Y;
Boolean different_x = False, different_y = False;
long index;

  first_xval = _fg_data[0];
  first_yval = _fg_data[1];

  index = 0;
  for(long i = 0; i < _num_points; i++)
    {
    if(_fg_data[index]   != first_xval) different_x = True;
    if(_fg_data[++index] != first_yval) different_y = True;
    index += 2;
    if(different_x && different_y) i = _num_points; //stop loop
    }

  if(different_x && different_y)
    dimension = BOTH_X_AND_Y;
  else if(different_x && !different_y)
    dimension = X_ONLY;
  else if(different_y && !different_x)
    dimension = Y_ONLY;

  return(dimension);
  
}

int FgQcPlotType::Texture ()
{
  return _texture_state;
}

void FgQcPlotType::setTexture (int texture)
{
  _texture_state = texture;
}

int FgQcPlotType::Pointsize ()
{
  return _point_size;
}

void FgQcPlotType::setPointsize (int point_size)
{
  _point_size = point_size;
}
