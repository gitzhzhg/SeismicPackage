//-------------------------- TestJob.cc -------------------------//
//-------------------------- TestJob.cc -------------------------//
//-------------------------- TestJob.cc -------------------------//

#include "CpseisBase.hh"
#include "PCW.hh"
#include "Pauser.hh"
#include "JobRunner.hh"
#include "CpseisRnsyn.hh"
#include "CpseisGather.hh"
#include "CpseisXp.hh"
#include "CpseisHeadsum.hh"
#include "CpseisFkfilt.hh"
#include "CpseisTsort.hh"
#include "CpseisParallelsort.hh"
#include "CpseisCc3d.hh"
#include "CpseisSetword.hh"
#include <stdio.h>
#include <string.h>

//--------------------------- main ----------------------------//
//--------------------------- main ----------------------------//
//--------------------------- main ----------------------------//

int main(int argc, char **argv)
{
  int pause = (argc > 1 ? strcmp(argv[1],"PAUSE") == 0 : 0);

  JobRunner runner;

  runner.setPause(pause);

      PCW::put("off_tot", 3);
      PCW::put("opt_tr", "diff");

  runner.update(new CpseisRnsyn());

      int values[2];
      values[0] = 3;
      values[1] = 46;
      char before[100];
      char after[100];
      strcpy(before, " #       1. START ( )");
      strcpy(after, " #       3. END ( )");
      PCW::put("before_tasks", before, 1);
      PCW::put("after_tasks", after, 1);
      PCW::put("taskseq", 2);
      PCW::put("taskcnt", 2);
      PCW::put("task", "COPY");
      PCW::put("function", "IDENT");
      PCW::put("value", values, 2);

  runner.update(new CpseisSetword());

      PCW::put("num_tr_max", 5);

  runner.update(new CpseisGather());

      PCW::put("debri", "NO");

  runner.update(new CpseisXp());

  runner.update(new CpseisHeadsum());

  runner.update(new CpseisTsort());

      PCW::put ("PRI_FIRST", -3);
      PCW::put ("PRI_LAST",  3);
      PCW::put ("OUTPUT_CHOICE",  "all nodes");
      PCW::put ("ABORT_CHOICE",  "proceed");

  runner.update(new CpseisParallelsort());

      float* coordinates = new float [2];
      float* factors = new float [2];
      coordinates[0] = 0.0f;
      coordinates[1] = 0.01f;
      factors[0] = 10.0f;
      factors[1] = 1.0f;

      PCW::put ("type_filt", "DIP");
      PCW::put ("HDR_PANEL", 3);
      PCW::put ("PANEL_INIT", 1.0);
      PCW::put ("NUM_TR_IN", 1000);
      PCW::put ("PANEL_INC", 1.0);
      PCW::put ("COORDINATES", coordinates, 2);
      PCW::put ("FACTORS", factors, 2);

      delete [] coordinates;
      delete [] factors;

  runner.update(new CpseisFkfilt());

      PCW::put ("PATH_SRC", "staticfile_src.cc3d");
      PCW::put ("DEAD_END", "NO");
      PCW::put ("SX_INIT", 0);
      PCW::put ("RX_INIT", 0);
      PCW::put ("CMPX_INIT", 0);
      PCW::put ("CMPY_INIT", 0);
      PCW::put ("NUM_ITER", 2);

  runner.update(new CpseisCc3d());

  runner.run();

  return 0;
}

//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
//---------------------------- end -----------------------------//
