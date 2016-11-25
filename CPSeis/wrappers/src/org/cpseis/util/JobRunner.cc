//--------------------------- JobRunner.cc --------------------------//
//--------------------------- JobRunner.cc --------------------------//
//--------------------------- JobRunner.cc --------------------------//

#include "JobRunner.hh"
#include "CpseisBase.hh"
#include "Pauser.hh"
#include "PCW.hh"
#include "GTW.hh"
#include "named_constants.h"
#include <stdio.h>
#include <assert.h>

/**
This class runs a backend job from a standalone program.
The first module should be an input tool.
*/

//-------------------------- constructor ------------------------//
//-------------------------- constructor ------------------------//
//-------------------------- constructor ------------------------//

JobRunner::JobRunner()
       :
         _ntools  (0),
         _nwih    (0),
         _ndpt    (0),
         _numtr   (0),
         _modules (new CpseisBase* [100]),
         _pause   (0)
{
  PCW::backendUpdate();

  double         xorigin    = 100000.0;
  double         yorigin    = 700000.0;
  double         xwidth     = 100.0;
  double         ywidth     = 200.0;
  double         angle      = 89.0;
  int            handedness = 1;

  printf("\n");
  printf("java TestProgram: xorigin =================== %lf\n", xorigin);
  printf("java TestProgram: yorigin =================== %lf\n", yorigin);
  printf("java TestProgram: xwidth ==================== %lf\n", xwidth);
  printf("java TestProgram: ywidth ==================== %lf\n", ywidth);
  printf("java TestProgram: angle ===================== %lf\n", angle);
  printf("java TestProgram: handedness ================ %d\n", handedness);

  GTW *grid = new GTW();

  grid->setTransform(xorigin, yorigin, angle, xwidth, ywidth, handedness);


  int   nwih  = 66;
  int   ndpt  = 2001;
  int   numtr = 6;
  float tstrt = 0.0;
  float dt    = 0.004;

  PCW::putGlobal ("nwih"       , nwih);
  PCW::putGlobal ("ndpt"       , ndpt);
  PCW::putGlobal ("numtr"      , numtr);
  PCW::putGlobal ("tstrt"      , tstrt);
  PCW::putGlobal ("dt"         , dt);
  PCW::putGlobal ("grid"       , grid);
 
  delete grid;

  if(_pause) Pauser::pause();
}

//-------------------------- destructor ----------------------------//
//-------------------------- destructor ----------------------------//
//-------------------------- destructor ----------------------------//

JobRunner::~JobRunner()
{
  delete [] _modules;
  printf("modules array is deleted.\n");
}

//------------------------- update ---------------------------//
//------------------------- update ---------------------------//
//------------------------- update ---------------------------//

// Before calling this method:
//  (1) Put module parameters into the parameter cache.
//  (2) Create the module and pass it to this method.
// Call this method for each module in the job.
// The first module must be an input tool.

void JobRunner::update (CpseisBase *module)
{
  _modules[_ntools] = module;

  int nwih  = 0;
  int ndpt  = 0;
  int numtr = 0;

  PCW::getGlobal ("nwih"  , &nwih);
  PCW::getGlobal ("ndpt"  , &ndpt);
  PCW::getGlobal ("numtr" , &numtr);

  _nwih  = MAX(_nwih , nwih);
  _ndpt  = MAX(_ndpt , ndpt);
  _numtr = MAX(_numtr, numtr);

  _modules[_ntools]->update();

  PCW::getGlobal ("nwih"  , &nwih);
  PCW::getGlobal ("ndpt"  , &ndpt);
  PCW::getGlobal ("numtr" , &numtr);

  _nwih  = MAX(_nwih , nwih);
  _ndpt  = MAX(_ndpt , ndpt);
  _numtr = MAX(_numtr, numtr);

  if(_pause) Pauser::pause();

  _ntools++;
  PCW::next();
}

//--------------------------- run ----------------------------//
//--------------------------- run ----------------------------//
//--------------------------- run ----------------------------//

// Call this method after calling update for each module.

void JobRunner::run()
{
//-----------allocate trace buffers:

  double **hd = new double* [_numtr];
  float  **tr = new float * [_numtr];

  for(int itr = 0; itr < _numtr; itr++)
      {
      hd[itr] = new double [_nwih];
      tr[itr] = new float  [_ndpt];
      for(int i = 0; i < _nwih; i++) { hd[itr][i] = itr + 11.11  + i; }
      for(int i = 0; i < _ndpt; i++) { tr[itr][i] = itr + 33.33f + i; }
      }

  PCW::backendExecute();

//-----------run the engine:

  int ntr   = 0;
  int itool = 0;

  while(true)
     {
     privatePrint(itool, "before", ntr, hd, tr);

     for(int itr = 0; itr < ntr; itr++)
         {
         _modules[itool]->putTrace (itr, hd[itr], tr[itr]);
         }

     ntr = _modules[itool]->execute(ntr);

     for(int itr = 0; itr < ntr; itr++)
         {
         _modules[itool]->getTrace (itr, hd[itr], tr[itr]);
         }

     privatePrint(itool, "after ", ntr, hd, tr);

     if(ntr == FATAL_ERROR)
         {
         break;
         }
     else if(ntr == NEED_TRACES)
         {
         assert(itool > 0);
         itool--;
         }
     else if(ntr == NO_MORE_TRACES)
         {
         if(_pause) Pauser::pause();
         if(itool == _ntools - 1) break;
         itool++;
         }
     else if(itool == _ntools - 1)
         {
         assert(ntr > 0);
         ntr = NEED_TRACES;  // itool stays the same.
         }
     else
         {
         assert(ntr > 0);
         itool++;
         }
     }

//----------wrapup:

  for(int i = 0; i < _ntools; i++) { _modules[i]->wrapup(); }

  printf("wrapups are finished.\n");

  for(int i = 0; i < _ntools; i++) { delete _modules[i]; }

  printf("modules are deleted.\n");

  PCW::restore();

  printf("JobRunner is finished.\n");

  for(int itr = 0; itr < _numtr; itr++)
      {
      delete [] hd[itr];
      delete [] tr[itr];
      }
  delete [] hd;
  delete [] tr;

  printf("trace and header arrays are deleted.\n");
}

//----------------------- private print ------------------------//
//----------------------- private print ------------------------//
//----------------------- private print ------------------------//

void JobRunner::privatePrint (int itool, char *word, int ntr, double** hd, float** tr)
{
  printf("%s %s ntr = %d\n", _modules[itool]->getName(), word, ntr);
  for(int itr = 0; itr < ntr; itr++)
      {
      printf("%s %s %d  %lf  %f  %f\n", _modules[itool]->getName(), word, itr+1,
              hd[itr][0], tr[itr][77], tr[itr][78]);
      }
}

//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
