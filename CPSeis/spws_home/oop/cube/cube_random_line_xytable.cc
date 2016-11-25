//********************************************
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
//Author Michael L. Sherrill 05/97
//Creates table to control random line generation
//********************************************
#include <stdlib.h>
#include <stdio.h>
#include "cube/cube.hh"
#include "cube/cube_display.hh"
#include "cube/cube_random_line_pop.hh"
#include "cube/cube_random_line.hh"
#include "cube/cube_random_line_picker.hh"
#include "cube/cube_random_line_xytable.hh"




//============================================================================
//=================== Random line xy table ===================================
//============================================================================
CubeRandomLineXYTable::CubeRandomLineXYTable
                     (SLDelay *slparent, char *name, 
                      CubeRandomLine *cube_random_line) 
                    : SLDatabox(slparent, name, NULL, 4)
{
  _cube_random_line = cube_random_line;
  _temp_xarray = (float *)calloc(1,(unsigned int)(
                   CubeRandomLine::INITIAL_LOCATIONS * sizeof(float)));
  _temp_yarray = (float *)calloc(1,(unsigned int)(
                   CubeRandomLine::INITIAL_LOCATIONS * sizeof(float)));
  _draw_vectors = True;
}

//============================================================================
//=================== Random line xy set traps etc ===========================
//============================================================================
void CubeRandomLineXYTable::makeHelper()
{
static long zero  = 0; 
static long one   = 1; 



  regArrays (rowUpdate, maxRowUpdate, 0,  0,   6,    35);

          //   ID         PROMPT       SWITCH   SWITCH   COL NCHAR NDEC
  regFarray (XARRAY, "X LOCATION"     , &zero ,  &one  ,   0,   10,   3);
  regFarray (YARRAY, "Y LOCATION"     , &zero ,  &one  ,   0,   10,   3);

  trapFvar      (XARRAY,  valueTrap);
  updateFvar    (XARRAY,  valueUpdate);
  updateSwitch  (XARRAY,  valueSwitchUpdate);

  trapFvar      (YARRAY,  valueTrap);
  updateFvar    (YARRAY,  valueUpdate);
  updateSwitch  (YARRAY,  valueSwitchUpdate);

  
}


//============================================================================
//=================== Random line xy number of rows update ===================
//============================================================================
long CubeRandomLineXYTable::rowUpdate(void *data)
{
  CubeRandomLineXYTable *table = (CubeRandomLineXYTable *)data;
  long n = 0;

  n = table->_cube_random_line->pickerSegments();

  return n;
}

//============================================================================
//=================== Random line xy max number of rows update ===============
//============================================================================
long CubeRandomLineXYTable::maxRowUpdate(void *data)
{
CubeRandomLineXYTable *table = (CubeRandomLineXYTable *)data;
long n;

  n = table->_cube_random_line->pickerSegments() + 1;

  return n;
}


//============================================================================
//=========== Random line xy value trap ======================================
//============================================================================
void CubeRandomLineXYTable::valueTrap(void *data, long ident, 
                                      long index, float value, 
                                      long nread, char *endkey)
{
int i;
float *xarray;
float *yarray;
float xmin,xmax,ymin,ymax;
float tempx, tempy;


  //if the value has not changed and user is not inserting or removing, return
  if(nread == 0)
    {
    if(strcmp(endkey,"INSERT") != 0 && strcmp(endkey,"REMOVE") != 0) return;
    }


  CubeRandomLineXYTable *table = (CubeRandomLineXYTable *)data;

  
  if(strcmp(endkey,"INSERT") == 0 || //inserting new row
     index == table->_cube_random_line->pickerSegments())
    {
    table->_cube_random_line->setNumberPickerSegments(
                   table->_cube_random_line->pickerSegments() + 1);    
    table->setNumberSegments(
                   table->_cube_random_line->pickerSegments());
    xarray = table->_cube_random_line->xarray();
    yarray = table->_cube_random_line->yarray();
    for(i = 0;i < table->_cube_random_line->pickerSegments(); i++)
      {
      table->_temp_xarray[i] = xarray[i];
      table->_temp_yarray[i] = yarray[i]; 
      }   
    for(i = 0; i < index; i++)
      {
      xarray[i] = table->_temp_xarray[i];
      yarray[i] = table->_temp_yarray[i];
      }
    if(strcmp(endkey,"INSERT") == 0)//inserting a row without a value
      {
      xarray[i] = 0.0;
      yarray[i] = 0.0;
      }
    else//Keying in a value into the extra last row
      {
      table->_cube_random_line->getCubeWorldCoordinates(&xmin, &xmax, 
                                                        &ymin, &ymax);
      if(ident == XARRAY)
        {
        if(value < xmin)
          {
          table->_cube_random_line->errorPopUp("X value reset to data minimum");
          value = xmin;
          }
        if(value > xmax)
          {
          table->_cube_random_line->errorPopUp("X value reset to data maximum");
          value = xmax;
          }
        xarray[i] = value;
        yarray[i] = 0.0;
        }
      else
        {
        if(value < ymin)
          {
          table->_cube_random_line->errorPopUp("Y value reset to data minimum");
          value = ymin;
          }
        if(value > ymax)
          {
          table->_cube_random_line->errorPopUp("Y value reset to data maximum");
          value = ymax;
          }
        xarray[i] = 0.0;
        yarray[i] = value;
        }
      }      
    for(i=(int)index+1;i<table->_cube_random_line->pickerSegments();i++)
      {
      xarray[i] = table->_temp_xarray[i-1];
      yarray[i] = table->_temp_yarray[i-1];
      }
    }
  else if(strcmp(endkey,"REMOVE") == 0)//removing a row of values
    {
   // if(index + 1 > table->_cube_random_line->pickerSegments())
   //   {
   //   table->_cube_random_line->setNumberPickerSegments(
   //                  table->_cube_random_line->pickerSegments() + 1);    
   //   table->setNumberSegments(
   //                  table->_cube_random_line->pickerSegments());
   //   }
    xarray = table->_cube_random_line->xarray();
    yarray = table->_cube_random_line->yarray();
    for(i = 0; i < table->_cube_random_line->pickerSegments(); i++)
      {
      table->_temp_xarray[i] = xarray[i];
      table->_temp_yarray[i] = yarray[i]; 
      }   
    for(i = 0; i < index; i++)
      {
      xarray[i] = table->_temp_xarray[i];
      yarray[i] = table->_temp_yarray[i];
      }
    for(i=(int)index;i< table->_cube_random_line->pickerSegments()-1; i++)
      {
      xarray[i] = table->_temp_xarray[i+1];
      yarray[i] = table->_temp_yarray[i+1];
      }
    table->_cube_random_line->setNumberPickerSegments(
                         table->_cube_random_line->pickerSegments() - 1);
    table->setNumberSegments(
                         table->_cube_random_line->pickerSegments());
    if(table->_draw_vectors)
      table->_cube_random_line->getPicker()->drawVectors(
                         table->_cube_random_line->getTimeSliceSP());
    }
  else//changing an existing value
    {
    xarray = table->_cube_random_line->xarray();
    yarray = table->_cube_random_line->yarray();
    table->_cube_random_line->getCubeWorldCoordinates(&xmin, &xmax, 
                                                      &ymin, &ymax);
    if(ident == XARRAY)
      {
      if(value < xmin)
        {
        table->_cube_random_line->errorPopUp("X value reset to data minimum");
        value = xmin;
        }
      if(value > xmax)
        {
        table->_cube_random_line->errorPopUp("X value reset to data maximum");
        value = xmax;
        }
      xarray[index] = value;
      }
    if(ident == YARRAY)
      {
      if(value < ymin)
        {
        table->_cube_random_line->errorPopUp("Y value reset to data minimum");
        value = ymin;
        }
      if(value > ymax)
        {
        table->_cube_random_line->errorPopUp("Y value reset to data maximum");
        value = ymax;
        }
      yarray[index] = value;
      }
    if(xarray[index] != 0.0 && yarray[index] != 0.0)//sort and draw picks
      {
      tempx = xarray[index];
      tempy = yarray[index];
      if(table->_draw_vectors)
        table->_cube_random_line->getPicker()->drawVectors(
                                 table->_cube_random_line->getTimeSliceSP());
      //if this location has been sorted to a new row put focus on new location
      xarray = table->_cube_random_line->xarray();
      yarray = table->_cube_random_line->yarray();
/*
       for(i=0;i<table->_cube_random_line->pickerSegments();i++)
          if(xarray[i] == tempx && yarray[i] == tempy)
            table->setFocus((int)ident,i-1); 
*/
      }
    }

}


//============================================================================
//===================== Random line xy value update ==========================
//============================================================================
float CubeRandomLineXYTable::valueUpdate(void *data, long ident, long index)
{
float *xarray, *yarray;
CubeRandomLineXYTable *table = (CubeRandomLineXYTable*)data;

  xarray = table->_cube_random_line->xarray();
  yarray = table->_cube_random_line->yarray();
  
  if(ident == XARRAY)
    return xarray[index];
  else
    return yarray[index];
}


//============================================================================
//===================== Random line xy value switch update ===================
//============================================================================
long CubeRandomLineXYTable::valueSwitchUpdate(void * /*data*/,
                                               long /*ident*/, long /*index*/)
{
//CubeRandomLineXYTable *table = (CubeRandomLineXYTable *)data;
//may need to do something later to return a 1 or 5 for Tom's SLDataBox ?
  return 1;
}


void CubeRandomLineXYTable::setNumberSegments(long i)
{


  _temp_xarray  =(float *)realloc(_temp_xarray,(unsigned int)
                                    ( i * sizeof(float)));
  _temp_yarray  =(float *)realloc(_temp_yarray,(unsigned int)
                                    ( i * sizeof(float)));

  if(_temp_xarray == NULL || _temp_yarray == NULL)
     {
     printf("couldnt allocate x or y arrays in CubeRandomLineXYTable\n");
     return;
     }

}
