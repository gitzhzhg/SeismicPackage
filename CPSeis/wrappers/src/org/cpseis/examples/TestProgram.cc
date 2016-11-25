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
//-------------------------- TestProgram.cc ------------------------------//
//-------------------------- TestProgram.cc ------------------------------//
//-------------------------- TestProgram.cc ------------------------------//

#include "CpseisXp.hh"
#include "CpseisBase.hh"
#include "PCW.hh"
#include "GTW.hh"
#include <stdio.h>

/**
Main program to test C++ access to CPSeis modules.
*/

//------------------------------- main ------------------------------------//
//------------------------------- main ------------------------------------//
//------------------------------- main ------------------------------------//

int main()
{
  CpseisBase *module = new CpseisXp();

//----------initialize parameters:

  double         xorigin    = 100000.0;
  double         yorigin    = 700000.0;
  double         xwidth     = 100.0;
  double         ywidth     = 200.0;
  double         angle      = 89.0;
  int            handedness = 1;

  printf("\n");
  printf("C++ TestProgram: xorigin =================== %lf\n", xorigin);
  printf("C++ TestProgram: yorigin =================== %lf\n", yorigin);
  printf("C++ TestProgram: xwidth ==================== %lf\n", xwidth);
  printf("C++ TestProgram: ywidth ==================== %lf\n", ywidth);
  printf("C++ TestProgram: angle ===================== %lf\n", angle);
  printf("C++ TestProgram: handedness ================ %d\n", handedness);

  GTW *grid = new GTW();

  grid->setTransform(xorigin, yorigin, angle, xwidth, ywidth, handedness);

  int   nwih1  = 66;
  int   ndpt1  = 2001;
  int   numtr1 = 6;
  float tstrt  = 0.0f;
  float dt     = 0.004f;

//----------create and update:

  PCW::backendUpdate();

  PCW::put       ("win_inc"    , 0.44);
  PCW::putGlobal ("nwih"       , nwih1);
  PCW::putGlobal ("ndpt"       , ndpt1);
  PCW::putGlobal ("numtr"      , numtr1);
  PCW::putGlobal ("tstrt"      , tstrt);
  PCW::putGlobal ("dt"         , dt);
  PCW::putGlobal ("grid"       , grid);

  module->update();

  int nwih2  = nwih1;
  int ndpt2  = ndpt1;
  int numtr2 = numtr1;

  PCW::getGlobal ("nwih" , &nwih2);
  PCW::getGlobal ("ndpt" , &ndpt2);
  PCW::getGlobal ("numtr", &numtr2);

  PCW::backendExecute();

//----------initialize trace arrays:

  double  *hd1    = new double [nwih1];
  float   *tr1    = new float  [ndpt1];
  double  *hd2    = new double [nwih2];
  float   *tr2    = new float  [ndpt2];

//----------execute:

  int ntr = numtr1;

  for(int itr = 0; itr < ntr; itr++)
      {
      for(int i = 0; i < nwih1; i++) { hd1[i] = itr + 11.11  + i; }
      for(int i = 0; i < ndpt1; i++) { tr1[i] = itr + 33.33f + i; }
      printf("before %d %lf %f %f\n", itr, hd1[4], tr1[77], tr1[78]);
      module->putTrace (itr, hd1, tr1);
      }

  ntr = module->execute(ntr);

  printf("\n");
  for(int itr = 0; itr < ntr; itr++)
      {
      module->getTrace (itr, hd2, tr2);
      printf("after  %d %lf %f %f\n", itr, hd2[4], tr2[77], tr2[78]);
      }

//----------wrapup:

  delete module;
  PCW::restore();

  delete [] hd1;
  delete [] tr1;
  delete [] hd2;
  delete [] tr2;

  printf("finished with C++ test program\n");
}

//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//

