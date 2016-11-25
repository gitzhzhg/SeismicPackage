package org.cpseis.util;

import org.cpseis.util.CpseisBase;
import org.cpseis.util.Pauser;
import org.cpseis.util.GridTransform;
import org.cpseis.util.PC;
import org.cpseis.util.NC;

//----------------------- start of class ------------------------//
//----------------------- start of class ------------------------//
//----------------------- start of class ------------------------//

/**
This class runs a backend job from a standalone program.
The first module should be an input tool.
No exceptions are thrown by this class.
*/
public class JobRunner
{

//---------------------------- data ---------------------------//
//---------------------------- data ---------------------------//
//---------------------------- data ---------------------------//

private int          _ntools  = 0;
private int          _nwih    = 0;
private int          _ndpt    = 0;
private int          _numtr   = 0;
private CpseisBase[] _modules = new CpseisBase [100];
private boolean      _pause   = false;

//------------------------ set values ------------------------//
//------------------------ set values ------------------------//
//------------------------ set values ------------------------//

public void setPause (boolean pause) { _pause = pause; }

//-------------------------- constructor ------------------------//
//-------------------------- constructor ------------------------//
//-------------------------- constructor ------------------------//

public JobRunner()
{
  PC.backendUpdate();

  double         xorigin    = 100000.0;
  double         yorigin    = 700000.0;
  double         xwidth     = 100.0;
  double         ywidth     = 200.0;
  double         angle      = 89.0;
  int            handedness = 1;

  System.out.println("");
  System.out.println("java TestProgram: xorigin =================== " + xorigin);
  System.out.println("java TestProgram: yorigin =================== " + yorigin);
  System.out.println("java TestProgram: xwidth ==================== " + xwidth);
  System.out.println("java TestProgram: ywidth ==================== " + ywidth);
  System.out.println("java TestProgram: angle ===================== " + angle);
  System.out.println("java TestProgram: handedness ================ " + handedness);

  GridTransform grid = new GridTransform
                  (xorigin, yorigin, angle, xwidth, ywidth, handedness);

  int   nwih  = 66;
  int   ndpt  = 2001;
  int   numtr = 6;
  float tstrt = 0.0f;
  float dt    = 0.004f;

  PC.putGlobal ("nwih"       , nwih);
  PC.putGlobal ("ndpt"       , ndpt);
  PC.putGlobal ("numtr"      , numtr);
  PC.putGlobal ("tstrt"      , tstrt);
  PC.putGlobal ("dt"         , dt);
  PC.putGlobal ("grid"       , grid);

  if(_pause) Pauser.pause();
}

//------------------------- update ---------------------------//
//------------------------- update ---------------------------//
//------------------------- update ---------------------------//

// Before calling this method:
//  (1) Put module parameters into the parameter cache.
//  (2) Create the module and pass it to this method.
// Call this method for each module in the job.
// The first module must be an input tool.

public void update (CpseisBase module)
{
  _modules[_ntools] = module;

 _nwih  = Math.max(_nwih , PC.getGlobal ("nwih"  , 0));
 _ndpt  = Math.max(_ndpt , PC.getGlobal ("ndpt"  , 0));
 _numtr = Math.max(_numtr, PC.getGlobal ("numtr" , 0));

  _modules[_ntools].update();

 _nwih  = Math.max(_nwih , PC.getGlobal ("nwih"  , 0));
 _ndpt  = Math.max(_ndpt , PC.getGlobal ("ndpt"  , 0));
 _numtr = Math.max(_numtr, PC.getGlobal ("numtr" , 0));

  if(_pause) Pauser.pause();

  _ntools++;
  PC.next();
}

//--------------------------- run ----------------------------//
//--------------------------- run ----------------------------//
//--------------------------- run ----------------------------//

// Call this method after calling update for each module.

public void run()
{
//-----------allocate trace buffers:

  double[][] hd = new double [_numtr] [_nwih];
  float [][] tr = new float  [_numtr] [_ndpt];

  for(int itr = 0; itr < _numtr; itr++)
      {
      for(int i = 0; i < _nwih; i++) { hd[itr][i] = itr + 11.11  + i; }
      for(int i = 0; i < _ndpt; i++) { tr[itr][i] = itr + 33.33f + i; }
      }

  PC.backendExecute();

//-----------run the engine:

  int ntr   = 0;
  int itool = 0;

  while(true)
     {
     privatePrint(itool, "before", ntr, hd, tr);

     for(int itr = 0; itr < ntr; itr++)
         {
         _modules[itool].putTrace (itr, hd[itr], tr[itr]);
         }

     ntr = _modules[itool].execute(ntr);

     for(int itr = 0; itr < ntr; itr++)
         {
         _modules[itool].getTrace (itr, hd[itr], tr[itr]);
         }

     privatePrint(itool, "after ", ntr, hd, tr);

     if(ntr == NC.FATAL_ERROR)
         {
         break;
         }
     else if(ntr == NC.NEED_TRACES)
         {
         assert(itool > 0);
         itool--;
         }
     else if(ntr == NC.NO_MORE_TRACES)
         {
         if(_pause) Pauser.pause();
         if(itool == _ntools - 1) break;
         itool++;
         }
     else if(itool == _ntools - 1)
         {
         assert(ntr > 0);
         ntr = NC.NEED_TRACES;  // itool stays the same.
         }
     else
         {
         assert(ntr > 0);
         itool++;
         }
     }

//----------wrapup:

  for(int i = 0; i < _ntools; i++) { _modules[i].wrapup(); }

  System.out.println("wrapups are finished.");

  for(int i = 0; i < _ntools; i++) { _modules[i].destroy(); }

  System.out.println("modules are deleted.");

  PC.restore();

  System.out.println("JobRunner is finished.");
}

//----------------------- private print ------------------------//
//----------------------- private print ------------------------//
//----------------------- private print ------------------------//

private void privatePrint (int itool, String word, int ntr, double[][] hd, float[][] tr)
{
  System.out.println(_modules[itool].getName() + " " + word + " ntr = " + ntr);
  for(int itr = 0; itr < ntr; itr++)
      {
      System.out.println(_modules[itool].getName() + " " + word + " " + (itr+1) + "  "
              + hd[itr][0] + "  " + tr[itr][77] + " " + tr[itr][78]);
      }
}

//------------------------ end of class ------------------------//
//------------------------ end of class ------------------------//
//------------------------ end of class ------------------------//

}

//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
