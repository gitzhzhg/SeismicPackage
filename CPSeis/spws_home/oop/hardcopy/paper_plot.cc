#include <string.h>
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
#include <assert.h>
#include <stdlib.h>
#include "hardcopy/paper_plot.hh"
#include "hardcopy/hardcopy_plot.hh"
#include "cgm.h"


int PaperPlot::_num_plots=0;


#define True  1
#define False 0

PaperPlot::PaperPlot( char    *outfile, 
                      float    total_width,
                      float    total_height,
                      FileType file_type) :

                      _total_width(total_width), 
                      _total_height(total_height)
{
  char typestr[30];

  //cgm_c_pip_init();
  cgmGopks(11);       /* Open GKS with logical unit 11 as error file */
  if (file_type == CGM) strcpy( typestr, "CGMPIP");
  cgmGopwk(1, outfile, typestr);
  cgmGacwk(1);
  HardCopyPlot::initColor();

  if (total_height <= 3.0) total_height= 3.0;
  if (total_width <= 3.0) total_width= 3.0;

  cgmGswkvp(1, 0.0, total_width, 0.0, total_height);

  /*
   * line initializations
   */
  cgmGsln(HardCopyPlot::SolidLine);    // Set line type solid
  cgmGslwsc(1);                        // Set line width to default
  cgmGsplci(HardCopyPlot::blackColor()); // Set line color to black

  /*
   * polygon initializations
   */
  cgmGsfaci(HardCopyPlot::blackColor());    // Set fill color to black
  cgmGsfais(HardCopyPlot::SolidFill);  // Set fill type to solid

  /*
   * text initializations
   */
  cgmGschxp(1.0);       // Set character expansion factors
  cgmGschsp(0.0);       // Set character spacings
  cgmGstxal(1,4);       // Set text justifications
  cgmGstxp(0);          // Set text paths
  cgmGschup(0.,1.);     // Set text up directions

  cgmGstxci(HardCopyPlot::blackColor()); // Set text color to black
  cgmGstxfp(HardCopyPlot::Helvetica,2);  // Set the font

  cgmSetMarkerFont(1);  // set markers to full congraf set


}



PaperPlot::~PaperPlot()
{
  cgmGdawk(1);   /* Deactivate workstation 1 */
  cgmGclwk(1);   /* Close workstation 1 */
  cgmGclks();    /* Close GKS */
  _num_plots= 0;
  HardCopyPlot::resetColor();
}


float PaperPlot::totalHeight()
{
  return _total_height;
}
float PaperPlot::totalWidth()
{
  return _total_width;
}


void PaperPlot::addPlot()   {_num_plots++;}
int  PaperPlot::plotCount() {return _num_plots;}
