package org.cpseis.examples;

import org.cpseis.util.CpseisBase;
import org.cpseis.util.PC;
import org.cpseis.util.Pauser;
import org.cpseis.util.JobRunner;
import org.cpseis.wrappers.CpseisRnsyn;
import org.cpseis.wrappers.CpseisGather;
import org.cpseis.wrappers.CpseisXp;
import org.cpseis.wrappers.CpseisHeadsum;
import org.cpseis.wrappers.CpseisFkfilt;
import org.cpseis.wrappers.CpseisTsort;
import org.cpseis.wrappers.CpseisParallelsort;

//----------------------- start of class ------------------------//
//----------------------- start of class ------------------------//
//----------------------- start of class ------------------------//

public class TestJob
{
  
//--------------------------- main ----------------------------//
//--------------------------- main ----------------------------//
//--------------------------- main ----------------------------//

public static void main (String[] args) throws Exception
{
  boolean pause = (args.length > 0 ? args[0].equals("PAUSE") : false);

  JobRunner runner = new JobRunner();

  runner.setPause(pause);

      PC.put("off_tot", 3);
      PC.put("opt_tr", "diff");

  runner.update(new CpseisRnsyn());

      PC.put("num_tr_max", 5);

  runner.update(new CpseisGather());

      PC.put("debri", "NO");

  runner.update(new CpseisXp());

  runner.update(new CpseisHeadsum());

  runner.update(new CpseisTsort());

      PC.put ("PRI_FIRST", -3);
      PC.put ("PRI_LAST",  3);
      PC.put ("OUTPUT_CHOICE",  "all nodes");
      PC.put ("ABORT_CHOICE",  "proceed");

  runner.update(new CpseisParallelsort());

      float[] coordinates = new float [2];
      float[] factors = new float [2];
      coordinates[0] = 0.0f;
      coordinates[1] = 0.01f;
      factors[0] = 10.0f;
      factors[1] = 1.0f;

      PC.put ("type_filt", "DIP");
      PC.put ("HDR_PANEL", 3);
      PC.put ("PANEL_INIT", 1.0);
      PC.put ("NUM_TR_IN", 1000);
      PC.put ("PANEL_INC", 1.0);
      PC.put ("COORDINATES", coordinates);
      PC.put ("FACTORS", factors);

  runner.update(new CpseisFkfilt());

  runner.run();
}

//------------------------ end of class ------------------------//
//------------------------ end of class ------------------------//
//------------------------ end of class ------------------------//

}

//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
