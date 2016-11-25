#include "hardcopy/hardcopy_plot.hh"
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
#include "hardcopy/hardcopy_color_bar.hh"
#include <stdio.h>
#include <assert.h>





HardCopyColorBar::HardCopyColorBar(HardCopyPlot  *hcp, 
                                   long           num_colors,
                                   float         *rgbs) :

                 _hcp(hcp), _rgbs(rgbs), _num_colors(num_colors),
                 _num_deci_places(5)
{ }




void HardCopyColorBar::drawSquare9At(float x,     float y, 
                                     float width, float height)
{
  float color_width;
  float color_height;
  float color_x;
  float color_y;
  int i,j, rgb_start;
  int cindex;
  char numstr[40];
  HCpoint     rec_pts[5];

  assert(_num_colors == 10);

  color_width=  (width)/3;
  color_height= (height)/3;
  color_x=      x;
  color_y=      y;
  color_y= y+ color_height*2;
  _hcp->setFillType(HardCopyPlot::SolidFill);

  rec_pts[0].x=rec_pts[4].x= x;
  rec_pts[0].y=rec_pts[4].y= y;

  rec_pts[1].x= x + width;
  rec_pts[1].y= y;

  rec_pts[2].x= x+ width;
  rec_pts[2].y= y+ height;

  rec_pts[3].x= x;
  rec_pts[3].y= y+ height;

  cindex= HardCopyPlot::defineColorIndex( 1.0, 1.0, 1.0);
  _hcp->setFillColor(cindex);
  _hcp->drawFilledPolygon( rec_pts, 5, HardCopyPlot::INCHES); 

  _hcp->setTextColor(HardCopyPlot::blackColor() );
  _hcp->setTextHeight( color_height * .35 );
  _hcp->setLineWidth(1.0);
  _hcp->setLineColor(HardCopyPlot::blackColor());
  _hcp->setLabelPlacement(HardCopyPlot::DeadCenter);


  rgb_start=4;
  for(j= 0;(j<3); j++) {
       for(i= 0;(i<3); i++) {

          rec_pts[0].x= rec_pts[4].x= color_x;
          rec_pts[0].y= rec_pts[4].y= color_y;

          rec_pts[1].x= color_x+ color_width;
          rec_pts[1].y= color_y;

          rec_pts[2].x= color_x+ color_width;
          rec_pts[2].y= color_y+ color_height;

          rec_pts[3].x= color_x;
          rec_pts[3].y= color_y+ color_height;

          cindex= HardCopyPlot::defineColorIndex( _rgbs[rgb_start],
                                                  _rgbs[rgb_start+1],
                                                  _rgbs[rgb_start+2]);
          _hcp->setFillColor(cindex);
          _hcp->drawFilledPolygon( rec_pts, 5, HardCopyPlot::INCHES); 
          sprintf(numstr, "%1d", (int)_rgbs[rgb_start+3]);
          _hcp->writeText( color_x + (color_width * .5), 
                           color_y + (color_height* .5), 
                           numstr, HardCopyPlot::INCHES);
          color_x+= color_width;
          rgb_start+= 4;
     } // end loop
     color_x= x;
     color_y-= color_height;
  } // end loop
}



void HardCopyColorBar::drawCbarAt(float x, float y, float width, float height)
{
  float color_width;
  float color_height;
  float text_width;
  float text_height;
  float color_x;
  float color_y;
  float x_label;
  int i, rgb_start;
  int cindex;
  char numstr[40];
  HCpoint     rec_pts[5];


  color_width=  (width*.96)/3;
  color_height= (height*.94)/_num_colors;
  text_width=   2 * (width*.8/3);
  text_height=  color_height * .6;
  if ( text_height > 0.25) text_height= 0.25;
  color_x=      x + (width * .66);
  color_y=      y * 1.03;
  x_label=      x + (width*.55);
  _hcp->setFillType(HardCopyPlot::SolidFill);

  rec_pts[0].x=rec_pts[4].x= x;
  rec_pts[0].y=rec_pts[4].y= y;

  rec_pts[1].x= x + width;
  rec_pts[1].y= y;

  rec_pts[2].x= x+ width;
  rec_pts[2].y= y+ height;

  rec_pts[3].x= x;
  rec_pts[3].y= y+ height;

  cindex= HardCopyPlot::defineColorIndex( 1.0, 1.0, 1.0);
  _hcp->setFillColor(cindex);
  _hcp->drawFilledPolygon( rec_pts, 5, HardCopyPlot::INCHES); 
  _hcp->setTextColor(HardCopyPlot::blackColor() );
  _hcp->setTextHeight( text_height );
  _hcp->setLineWidth(1.0);
  _hcp->setLineColor(HardCopyPlot::blackColor());
  _hcp->setLabelPlacement(HardCopyPlot::LowerRight);


  for(i= 0, rgb_start=0; (i<_num_colors); i++, rgb_start+= 4) {

        rec_pts[0].x= rec_pts[4].x= color_x;
        rec_pts[0].y= rec_pts[4].y= color_y;

        rec_pts[1].x= color_x+ color_width;
        rec_pts[1].y= color_y;

        rec_pts[2].x= color_x+ color_width;
        rec_pts[2].y= color_y+ color_height;

        rec_pts[3].x= color_x;
        rec_pts[3].y= color_y+ color_height;

        color_y+= color_height;
        cindex= HardCopyPlot::defineColorIndex( _rgbs[rgb_start],
                                                _rgbs[rgb_start+1],
                                                _rgbs[rgb_start+2]);
        _hcp->setFillColor(cindex);
        _hcp->drawFilledPolygon( rec_pts, 5, HardCopyPlot::INCHES); 
        if(i < _num_colors - 1)//dont annotate the last box bottom edge
          {
          sprintf(numstr, "%*.*f", _num_deci_places+1, _num_deci_places,
                       _rgbs[rgb_start+3]);
          //_hcp->writeText( x_label, color_y, numstr, HardCopyPlot::INCHES);
          _hcp->writeConstrainedText( x_label, color_y, text_width, text_height,
                                      numstr, HardCopyPlot::INCHES);
          _hcp->drawLine( x, color_y, color_x, color_y, HardCopyPlot::INCHES);
          }
  }

}
