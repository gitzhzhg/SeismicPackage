package org.cpseis.examples;

import org.cpseis.wrappers.CpseisXp;
import org.cpseis.util.CpseisBase;
import org.cpseis.util.PC;
import org.cpseis.util.GridTransform;

/**
Main program to test java access to CPSeis modules.
*/

//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//

public class TestProgram
{

//------------------------------- main ------------------------------------//
//------------------------------- main ------------------------------------//
//------------------------------- main ------------------------------------//

public static void main(String args[])
{
  CpseisBase module = new CpseisXp();

//----------initialize parameters:

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

  int   nwih1  = 66;
  int   ndpt1  = 2001;
  int   numtr1 = 6;
  float tstrt  = 0.0f;
  float dt     = 0.004f;

//----------create and update:

  PC.backendUpdate();

  PC.put       ("win_inc"    , 0.44);
  PC.putGlobal ("nwih"       , nwih1);
  PC.putGlobal ("ndpt"       , ndpt1);
  PC.putGlobal ("numtr"      , numtr1);
  PC.putGlobal ("tstrt"      , tstrt);
  PC.putGlobal ("dt"         , dt);
  PC.putGlobal ("grid"       , grid);

  module.update();

  int nwih2  = PC.getGlobal ("nwih" , nwih1);
  int ndpt2  = PC.getGlobal ("ndpt" , ndpt1);
  int numtr2 = PC.getGlobal ("numtr", numtr1);

  PC.backendExecute();

//----------initialize trace arrays:

  double[]   hd1    = new double [nwih1];
  float[]    tr1    = new float  [ndpt1];
  double[]   hd2    = new double [nwih2];
  float[]    tr2    = new float  [ndpt2];

//----------execute:

  int ntr = numtr1;

  for(int itr = 0; itr < ntr; itr++)
      {
      for(int i = 0; i < nwih1; i++) { hd1[i] = itr + 11.11  + i; }
      for(int i = 0; i < ndpt1; i++) { tr1[i] = itr + 33.33f + i; }
      System.out.println("before " + itr + " " + hd1[4] + " " + tr1[77] + tr1[78]);
      module.putTrace (itr, hd1, tr1);
      }

  ntr = module.execute(ntr);

  System.out.println("");
  for(int itr = 0; itr < ntr; itr++)
      {
      module.getTrace (itr, hd2, tr2);
      System.out.println("after  " + itr + " " + hd2[4] + " " + tr2[77] + tr2[78]);
      }

//----------wrapup:

  module.destroy();
  PC.restore();

  System.out.println("finished with java test program");
}

//--------------------------- end of class -------------------------------//
//--------------------------- end of class -------------------------------//
//--------------------------- end of class -------------------------------//

}

//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//

