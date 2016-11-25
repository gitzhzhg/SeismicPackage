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
#include "sp/seis_plot.hh"
#include "sp/seis_plot_under.hh"
#include "sl_cvm_app.hh"
#include "grid_data.hh"
#include "sl_cvm_inform.hh"

//==================================================================
//======================= CbytInform Methods =======================
//==================================================================
void SLCvmInform::newPlot(SeisPlot *sp)
{SeisPlot      *sp_target;
 SeisPlotUnder *spund;
 CvmAppBase    *cvmbase;
 int           ndisp,i;
 char          msg[80];
 if(!_cvmapp) return;
 printf("newPlot??\n");
 sp_target = (SeisPlot *) _cvmapp->cvm_get_seisplot(_cvmapp);
 spund     = (SeisPlotUnder *) _cvmapp->cvm_get_seisplotu(_cvmapp);
 if(sp != _sp) return;
 ndisp = _cvmapp->numRemotePlot();
 for(i=0;i<ndisp;i++)
  {
   cvmbase = _cvmapp->nthRemotePlot(i+1);
   if(cvmbase)
    { sp_target = (SeisPlot *) cvmbase->cvm_get_seisplot(cvmbase);
      sprintf(msg,"Wait: updating remote display %d",i+1);
      *sp_target = *sp;
      if(sp_target)
       {*sp_target = *sp;
        _cvmapp->setMessage(msg);
        sp_target->plot();
       }
    }
  }

 _cvmapp->setMessage("");
}

void SLCvmInform::noPlotDisplayed(SeisPlot */* sp */)
{
 printf("noPlot??\n");

}

void SLCvmInform::plotTypeChange(SeisPlot *sp, int nu, int old)
{SeisPlot      *spo;
 SeisPlotUnder *spu;
 gridData      *gd;
 float         *garr=NULL;
 float         d1=1.0,d2=1.0,d3=1.0,o1=0.0,o2=0.0,o3=0.0;
 float         X1,X2,Z1,Z2;
 int           n1=1,n2=1,n3=1;
 if(!sp || nu==old) return;
 spo= _cvmapp->getSeisPlot();
 spu= _cvmapp->getSeisPlotUnder();
 gd = _cvmapp->cvmGetGridData();
 if(gd) garr = (float *) gd->getGridData();
 if( spo == sp) 
  {
   if(old==PlotImage::PlotISO && nu < PlotImage::PlotCOLOR)
    {// grid is loaded on seisplot-move to SeisPlotUnder
     if(gd)      // reset for new data
      { gd->getGridVals(&n1,&o1,&d1,&n2,&o2,&d2,&n3,&o3,&d3);
        X1 = o2;
        X2 = X1 + (n2 - 1)*d2;
        Z1 = o1;
        Z2 = Z1 + (n1 - 1)*d1;
        spu->setGridXYS(X1,X2,Z1,Z2);
        spu->setTmin(Z1);
        spu->setTmax(Z2);
        if(garr)
         {
          spu->initArrayTypeData(1,1,n2,n1,garr); //after setGridXYS
         }
        sp->cancelArrayTypeData();
      }
    }
   if(old==PlotImage::PlotGRID && nu <= PlotImage::PlotCOLOR)
    {return; }
   if(old==PlotImage::PlotGRID && nu == PlotImage::PlotISO)
    {return; }
   printf("SeisPlot type is changing new=%d\n",nu);
  }
 if( spu == sp)
  {
   printf("SeisPlotUnder type is changing\n");
  }

}
