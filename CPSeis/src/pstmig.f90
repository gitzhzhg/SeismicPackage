!<CPS_v1 type="PROCESS"/>
!!------------------------------ PSTMIG.f90 ---------------------------------!!
!!------------------------------ PSTMIG.f90 ---------------------------------!!
!!------------------------------ PSTMIG.f90 ---------------------------------!!
!
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>

!<brief_doc>
!-------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : PSTMIG
! Category   : Migrations
! Written    : 2000-01-13   by: Douglas Hanson
! Revised    : 2006-12-04   by: D. Glover
! Maturity   : production
! Purpose    : PHASE SHIFT, FK and Cascaded FK migration and modeling.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! PSTMIG uses Phaseshift, FK and Cascaded FK migration and modeling to image 
! poststack input data.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!                        INPUT PARAMETERS
!
! Name  Default  Valid    Description
! ----  -------  -----    -----------
!
!-----------------------------------------------------------------------
! This process is re-enterable.
!-----------------------------------------------------------------------
!                              NOTES
!
! 
! NOTE 0.
! 
! The following nomenclature is used in these notes:
! 
! An input bin - a trace bin defined by the parameters X_TOT, X_INIT,
! X_INC and Y_TOT, Y_INIT, Y_INC that is filled by an input traces.
! 
! An image bin - a trace bin defined by the parameters X_TOT, X_INIT,
! X_INC and Y_TOT, Y_INIT, Y_INC.  If an image bin is not filled by
! an input trace, it will be treated as a zero trace during imaging.  These
! traces can be output by setting ALL_OUT = YES.
! 
! A migration bin - a trace bin defined by the parameters X_FFT, X_INIT,
! X_INC and Y_FFT, Y_INIT, Y_INC.  Where X_FFT and Y_FFT are
! the spatial fft lengths.  Migration bins falling outside the range defined by
! X_TOT and Y_TOT are treated as zero traces during imaging.
! 
! NOTE 1.
! 
! PSTMIG is a 2D and 3D post stack phase shift, FK, cascaded FK 
! and common angle migration inverse migration (modeling) program.  
! In the phase shift and common angle mode it can
! migrate/model normal downgoing waves and/or turning maves.
! 
! 
! NOTE 2.  Parameters PATH_REF and REF_NAME - migration velocity function
! 
! PSTMIG uses a single velocity function for migration and modeling.  Input
! parameter PATH_REF defines the CPS velocity file conatining the reference
! velocity function.
! 
! Input parameter REF_NAME is the reference function name.  If REF_NAME = FIRST,
! the first function is used.
! 
! The reference velocity function used for migration is in a CPS velocity file
! defined by theand may be a VTRM, VTAV VTIN, VZRM, VZAV or VZIN function.
! 
! The input velocity function will be converted internally to a uniformly
! sampled VTIN function.
! 
! 
! NOTE 3.  Parameter OPT_DIR
! 
! PSTMIG uses a phase shift or FK  operation to either: migrate input data to an
! output image (OPT_DIR = MIGRATE) or model output data from an input image
! (OPT_DIR = MODEL)
! 
! 
! NOTE 4.  Parameter MIG_TYPE
! 
! PSTMIG can be used to phase shift image normal downgoing waves only, (MIG_TYPE
! = NORMAL), turning waves only,      (MIG_TYPE = TURNING), or both downgoing
! and turning waves, (MIG_TYPE = BOTH).  or FK or CASCADED FK migrate the input
! data.
! 
! 
! NOTE 5.  Multiple group migration.
! 
! PSTMIG will migrate traces within a defined grouping defined by the parameters
! OFF_HDR, OFF_INIT and OFF_INC.  If OFF_HDR is non zero PSTMIG will monitor the
! group location of each input trace.  When the group number changes the current
! group will be migrated and output.  After all of the current group image
! traces have been output PSTMIG will begin to input the traces for the next
! group.  Note the input data should be sorted by the group value so that all
! group 1 traces are together, followed by group 2 and so forth.  Group traces
! do not need to be gathered together.
! 
! 
! NOTE 6.  Input trace ordering.
! 
! If SORT_ORDER=X_IS_FAST:
! Traces whould be ordered by header words HDR_X, and HDR_Y within
! migration groups.  That is: header word HDR_X should change slowest and
! HDR_Y should change slowest.  All traces which share a common value of
! header word HDR_Y should be sorted together.  However within such a single
! line the header word HDR_X need not be ordered.
! The opposite is true s SORT_ORDER=Y_IS_FAST.
! 
! NOTE 7.
! 
! There should be a single trace within any single migration bin.  If PSTMIG
! finds more than one input trace in a single bin it will intentionaly abort.
! 
! 
! NOTE 8.  Output header words.
! 
! PSTMIG reset header words: HDR_X,HDR_Y, 1, 3, 4, 25 for all output
! traces.
! 
! 
! NOTE 9.  ALL_OUT - output all image bins.
! 
! When ALL_OUT = YES traces are generated and output for every image bin defined
! by X_TOT,X_INIT,X_INC and Y_TOT, Y_INIT, Y_INC regardless of
! whether or not there was a corresponding input trace.  For those output traces
! which did not have a corresponding input trace the header words other than
! those listed above are obtained by interpolating from the surrounding image
! trace bins which did have input traces.
! 
! For 3D the header words are first filled in in the X direction and then those
! that are still missing are filled in in the Y direction.
! 
! For example, suppose you had set the PSTMIG input parameters:
! 
! NDIM = 2, HDR_X = 7, X_TOT = 101, X_INIT = 0, X_INC = 1, HDR_Y
! = 8, Y_TOT = 1, Y_INIT = 0, Y_INC = 1,
! 
! and had read the input traces for X bins X = 10-20,30-90 into PSTMIG, Here
! traces for X bins (HDR_X = 7) X = 1-9, X = 21-29 and X = 91-101 are
! missing.
! 
! If you use ALL_OUT = NO only those image trace bins that were filled by input
! traces will be output, so you will get image traces for X bins X =
! 10-20,30-90.
! 
! If you use ALL_OUT = YES you will get an output trace for X bins 1-101. header
! words for the missing input traces will be interpolate from the surrounding
! input trace header words.  E.G.  for output traces X = 1-9 header words will
! be those of input trace X = 10.  for output traces X = 21-29 the header words
! will be linearly interpolated from input trace X = 20 and X = 30.  Header
! words for output traces X = 91-101 will be those of input trace X = 90.
! 
! NOTE 10 using STRETCH before and after PSTMIG.
! 
! Use CPS process STRETCH before and after PSTMIG to apply a Stolt stretch to
! correct for lateraly velocity variations.  Pre -migration use STRETCH with
! MODE = FORWARD  Post-migration use STRETCH with MODE = INVERSE In both cases
! use parameters PATH_REF and REF_NAME to point to the same reference velocity
! function used in PSTMIG.  NOte, however that STRETCH will stretch the data for
! the RMS versions of the input and reference velocity functions and PSTMIG will
! migrate with the interval velocity of the reference velocity functions.  You
! may use any CPS velocity type such as VTIN or VTRM for the velocity functions.
! PSTMIG and STRETCH convert from the input type to the required VTIN and VTRM
! types respectively.
! 
! 
! Note 11 MIGFILE, DISK_BUF
! 
! PSTMIG constructs a full volume migrated image from the input data.
! 
! If DISK_BUF = NO or NDIM = 2 PSTMIG stores this data in memory.  This is
! intended for use on the T3E where the memory can be allocated from many
! processors.  The J90 or C90 share memory and are much mor limited in this
! regard.  Normally you should use DISK_BUF = YES on the J90 or C90.  See note
! 12 for further discussion of its use on the T3E.
! 
! If DISK_BUF = YES, INPUT,MIGRATE or OUTPUT  PSTMIG stores the data in a disk
! file.  This will default to a tmp disk file whose name will be something like
! PSTMIG_XXX.data, where the XXX will be replaced by an integer corresponding to
! the PSTMIG process number within the job.  By default this will go to a tmp
! disk and will be deleted when the job completes.  For more details on how ow
! to use the INPUT,MIGRATE, OUTPUT procedure, see NOTE 14.
! 
! You can direct this work disk file to a ptmp file by includeing the ptmp path
! within the disk file name, e.g.  MIGFILE =
! /home/bdrt/ptmp/hansodw/PSTMIG.image
! 
! This will create a file using ptmp disk space.  This file will be removed
! automaticaly following job completion.  If a job aborts and you have used the
! ptmp disk you may need to delete the data file manually.
! 
! If you are going to run concurrent PSTMIG jobs, make sure to use a unique file
! name for MIGFILE for each separate PSTMIG job that uses ptmp disk space.
! 
! Make sure you use the same ptmp MIGFILE file name for each of the three jobs
! in a DISK_BUF =  INPUT, MIGRATE or OUTPUT job series.
! 
! The T3E has a very small tmp disk space.  Therfore you should always use a
! ptmp file name for MIGFILE on the T3E.
! 
! 
! NOTE 12.  DISK_BUF = INPUT, MIGRATE or OUTPUT series.
! 
! If NDIM = 2 or DISK_BUF = YES or DISK_BUF = NO the data will be input,
! migrated, and output all in one job.
! 
! On the T3E only 2 PEs are required to input and output the data.  But many PEs
! can be used to migrate it.  If the data is input, migrate and output all in
! one job most of the PEs are idle during data input and output.  Because you
! are charged for the entire time the PEs are allocated to a job, whether they
! are busy or not, this can seriously degrade PSTMIGs cost performance.
! 
! To improve efficiency, you may choose to break the migration process into
! discrete steps using DISK_BUF = INPUT, MIGRATE, OUTPUT.
! 
! To do this create 3 jobs including: data input, such as QTRIN, PSTMIG and data
! output such as QTROT.
! 
! The three jobs are the same except:
! 
! JOB 1 - DISK_BUF = INPUT
! 
! PSTMIG will store input data in MIGFILE but will not migrate or output it to
! the QTROT file.  Use 2 PEs on the T3E for this job.  Run this job first.
! 
! JOB 2 - DISK_BUF = MIGRATE
! 
! PSTMIG will migrate the data in MIGFILE but will not input or output it to the
! QTROT file.  Use many PEs on the T3E for this job.  Run this job after JOB 1
! has completed
! 
! JOB 3 - DISK_BUF = OUTPUT
! 
! PSTMIG will output the migrated data in MIGFILE.  Use 2 PEs on the T3E for
! this job.  Run this job after JOB 2 has completed.
! 
! You may choose to delete the QTRIN input file after JOB 1 completes.  A single
! trace input is required for JOB 2 and JOB 3 but these do not need to be real
! traces, you could generate them with RNSYN.
! 
! You may only input a single migration group using this option.
! 
! You should make sure MIGFILE will be placed on PTMP so it will be saved from
! one job to another.
! 
! For more details on how ow to use the INPUT,MIGRATE, OUTPUT procedure, see
! Note 14.
! 
! Note 13 FFT lengths
! 
! X_FFT, Y_FFT and TIM_FFT are the fft lengths in the X, Y and T 
! directionrespecively.  PSTMIG uses a mixed radix fft, so any of these can be 
! a product of power of 2, 3 and 5.
! 
! X_FFT must be -1 or > =  X_TOT, Similary Y_FFT must be -1 or >=
! Y_TOT
! 
! For OPT_DIR = MIGRATE - TIM_FFT must be -1 or > =  NDPT where NDPT is the
! input trace length.
! 
! For OPT_DIR = MODEL - TIM_FFT must be -1 or > =  TIM_TOT where TIM_TOT is 
! the output trace length.
! 
! If the input X fft length parameters X_FFT = -1 it defaults to the first
! power of 2 > =  X_TOT.  Y_FFT and TIM_FFT are treated similarly.
! 
! Note X_FFT and Y_FFT are the actual number of trace bins actually used
! in the migration.  By rounding the spatial ffts upwards PSTMIG has padded the
! input traces with a null zone from X_TOT+1 - X_FFT in the X direction
! and from Y_TOT+1 - Y_FFT in the Y direction.
! 
! For 2D migrations PSTMIG keeps the entire input data section and no data is
! written to disk.  For 3D migrations PSTMIG keeps (X_FFT/2+1)*Y_TOT
! complex traces on disk.  On Crays these traces are packed 2-1 on disk The
! length of these traces is the max of NDPT, NTOUT.  Hence the disk space is
! approximately: Disk size (bytes) = [ (X_FFT/2+1) * Y_TOT *
! MAX (NDPT,NTOUT) / 2] * 8
! 
! Note 14  Pre-Stack Zero-Offset Time Migration (PZOTM) using PSTMIG
! (Sometimes Called Pseudo Pre-Stack Time Migration (PPSTM))
! 
! PZOTM Methodology
! 
! The following is a description of the PZOTM method/approach and how it was
! implemented on LOBO 3D data.  The basic PZOTM processing sequence is as
! follows:
! 
! NMO - DMO - Migrate - VA - STK - Inverse Migrate - Final Migrate
! 
! The DMO and initial migration steps are performed on common offsets The
! initial migration and inverse migration are performed using a single average
! velocity function.  A single function is used in order to make the inverse
! more exact.  The choice of the single velocity function can be important,
! particularly in areas of complex geology.  Runnine some 2D tests using various
! single velocity functions is one way that may help in the selection of the
! initial migration velocity function.  The final migration can be performed
! using a smoothed version of the final stacking velocities, picked  after the
! initial migration.  However, this approach does not always work. See sections
! on Initial Migration (DPSTMIG8A) and Final Forward Migration for more details
! on velocities.  Note that VA stands for velocity analysis.
! 
! The Phase-Shift algorithm is used to do the forward and inverse migration
! steps.  The final migration step can be any migration algorithm (e.g.  FK,
! Phase-Shift, Kirchhoff)
! 
! DMO and STK are the composite steps.  No velocity analysis should be necessary
! after DMO, prior to the initial migration step, since no composite is being
! performed in the migration step itself.
! 
! 
! PZOTM in Practice
! 
! This section describes a series of jobs that were used to perform PZOTM on
! LOBO 3D data.  These jobs should be used as a guide for future PZOTM work. Of
! course, different data will require some variation on the parameters, but
! those will be obvious.  The focus here will be on the not so obvious
! parameters and how to avoid certain pitfalls.  Note that QTRIN and QTROT are
! the required I/O modules, and these are used in all the jobs with the
! exception of the stack job that uses PTRIN.  For reference, the CINF files can
! be found in USER1:[ROY.FNB.PPMSTK].
! 
! Initial Migration
! 
! This initial migration step requires that each common offset be migrated in a
! series of three jobs if the migration is performed on the T3E.  If the J90 is
! used, then the migration can be done in one job.  The reason three jobs are
! needed on the T3E is because the I/O steps in PSTMIG are very time consuming
! and thus are not able to use many PEs efficiently.  The following is the
! series of three jobs:
! 
! DPSTMIG8A
! 
! The input data is a single common offset volume after NMO and DMO application.
! This series of three jobs is run on each of the DMO offsets. In the LOBO
! case, there were 16 offsets, which meant running 48 jobs to migrate the data.
! Note that for most 2D data, all offsets can be migrated in one job by setting
! the OFF_HDR, OFF_MIG and OFF_INC parameters appropriately. All 2D jobs / tests
! should be run on the J90.
! 
! Some important parameter considerations are:
! 
! FSWP, DAP, X_FFT, Y_FFT and TIM_FFT.
! 
! It is recommended that these parameters be tested prior to making a production
! run.  Getting the right FFT lengths are critical to minimize wrap-around
! affects.  FSWP=0 and DAP=1 (the defaults) should be reasonable.  This job
! used values of 0.5 and 3, which appeared to work well.  In theory, 0 and 1
! should work fine.  FSWP should always be set to zero if any fold variations
! exist in the common offset volumes.  See the parameter descriptions in the
! docs for more information. When in doubt, run some 2D tests.
! 
! This initial job runs using only 2 PEs.  It saves no time or money to use more
! PEs.  The ALL_OUT parameter should be set to YES.  This will keep all data
! migrated into any gaps which might exist.  This is particularly important for
! land data, but can also be critical in marine data where significant cable
! feathering leads to fold variations.
! 
! DISK_BUF should be set to INPUT.  This will create and store a file on the
! PTMP disk that will be used in the remaining two jobs.  DO NOT store this file
! on the TMP disk as it can get quite large.
! 
! Finally, the migration velocity should be a simple single velocity function.
! As stated earlier, the choice of this function could be important,
! particularly in areas of complex geology.  It is also important if one wishes
! to skip the inverse migration step and do a residual migration instead.  In
! that case, one would want to be careful that the data were not over-migrated
! in the initial migration step.
! 
! DPSTMIG8B
! 
! This job is the same as 8A with the following exceptions:
! 
! Only 1 input trace is required, since the data for this job is already stored
! in the PTMP file specified in the first job.  DISK_BUF is set to MIGRATE.
! Multiple PEs can be used.  In this job, 40 PEs were used to do the migration.
! This job is the fastest of the three jobs.  The input and output steps are the
! most time consuming.
! 
! DPSTMIG8c
! 
! This is the final job in the initial migration series.  It is the same as 8B
! with the following exceptions:
! 
! DISK_BUF is set to OUTPUT.  The number of PEs used should be set to 2, as in
! 8A.  Note that the MIGFILE stored on PTMP is automatically deleted once the
! job completes.  One should account for the size of the MIGFILE and the QTROT
! output file when considering the amount of disk space needed to migrate each
! offset.
! 
! VA
! 
! Once all offsets are migrated, velocity analysis can be performed using the
! preferred method.  Jobs to do the velocity analysis have not been included in
! this discussion.  Prior to generating velocity panels (e.g. GVSs), the mute
! headers should be set so that data migrated into gaps will not be
! automatically muted in the GVS process.  Setting HW 2 = 0 and HW 64 = last
! sample in trace will avoid this problem.  The importance of setting the mute
! headers is discussed in more detail in the next section.
! 
! Note that the velocities picked on the migrated data are still just stacking
! velocities.  Smoothing these velocities and using them in the final forward
! migration may or may not be appropriate depending on the complexity of the
! data.
! 
! Stack (STK)
! 
! DGS130
! 
! Since the data are already in common offset order, PTRIN can be used as the
! input module for the stack job.  This is advantageous since PTRIN reads a
! single trace from a common bin of each offset etc., thus allowing efficient
! use of the process STK to stack the data.  GSTK should be avoided at all costs
! to stack the data since it is extremely Y when compared to STK.
! 
! Setting the mute parameters properly prior to stack is extremely important.
! The mute headers should be set as HW 2 = 0 and HW 64 = last sample in trace.
! Setting HW 64 is not as critical as setting HW 2.  This is necessary because
! in the migration step, data were migrated into traces where no data existed
! previously.  Those original dead traces had their mutes set to the bottom of
! the trace and if left unaltered, the STK process will think these traces are
! dead and not include the data in the stack. This is extremely important in
! data with large gaps in the common offsets, typical of land data.  It is less
! important for marine data, but can be an issue if significant cable feathering
! causes fold variations in the common offsets (e.g.  Gulf of Mexico Witch 3D
! data).  For the LOBO data, the mute on the stack of the migrated offsets was
! set using a GRAB file that was created from a DMO stack volume.  If no stack
! volume is available prior to migration, then a) create a stack and extract the
! mute index or b) pick the mute manually on the stack of the migrated offsets.
! For marine data, option b is feasible, but this may not be as easy on land
! data.  Further testing revealed that for our data it was not necessary to
! retain data beyond the survey edges after the stack.  We originally believed
! that this data needed to be kept for the inverse migration step.  The inverse
! migration step was performed on a stack volume in which we kept data beyond
! the survey edge and on one which we muted the data beyond the edge.  Both
! inverse migrated products were then forward migrated and compared.  No
! significant differences were seen in the two final volumes.
! 
! For the LOBO data, it did not make a significant difference if we retained the
! data beyond the survey edge prior to performing the inverse migration.
! However, the need to keep this extra data may be a function of dip direction
! around the survey perimeter and the single velocity function used to perform
! the initial migration.  Therefore, it is recommended that the data beyond the
! survey edges be retained after stack.  The amount of additional disk space
! this would require should be taken into consideration since it may not be
! trivial.
! 
! Prior to stack, the data should have NMc reverse applied using the velocities
! that were used to moveout the data prior to migration.  Do not use the
! velocity used in the initial migration in this step.  Of course, the NMc
! forward step should use the latest velocities picked in the velocity analysis
! performed on the migrated data.
! 
! Inverse Migration
! 
! DFIN125
! 
! The input to this job is the stack of the migrated offsets.  This job was run
! on the J90 using 16 CPUs.  Some important parameter considerations are:
! 
! FSWP and DAP should be set to 0 and 1 respectively, regardless of what was
! used in the initial migration job.  For example, using a DAP other than 1 for
! this and the final migration job would continue to discriminate against steep
! dips, which is undesirable.
! 
! X_TOT, Y_TOT and TIM_TOT should be set equal to X_FFT, Y_FFT and
! TIM_FFT The reasoning here is that we want to keep all data that is inverse
! migrated (modeled) into the padded area so that it exists for the final
! forward migration step.  Failure to keep this data will likely result in a
! degraded final migrated image since any dipping data modeled into the padded
! area would not exist and therefore not be migrated back into the 3D volume or
! 2D line. OPT_DIR should be set to MODEL to do the inverse migration.  Continue
! to use ALL_OUT = YES in order to keep data modeled into gaps and beyond the
! survey edge.  The same velocity function used in the initial forward migration
! should also be used in the inverse migration.
! 
! Final Forward Migration
! 
! DFIN126
! 
! The input to the final forward migration is the inverse migrated data set (the
! stack) output in the previous job.  This job was also run on the J90 using 16
! CPUs.  Some parameter considerations are:
! 
! Again, FSWP and DAP should be set to 0 and 1 respectively.  X_TOT,
! Y_TOT and TIM_TOT should be set to their original values from the initial
! migration job.  We only want to keep what we want in the final migration and
! not the padded data.  The STRETCH process needs to be included before and
! after PSTMIG for the final migration since a velocity field is used as opposed
! to a single velocity function.  The reference velocity we used for the final
! migration was a single average velocity function derived from the  smoothed
! stacking velocities that were picked on the migrated data.
! 
! Probably the most important consideration for the final forward migration is
! what velocity field to use.  There is no guarantee that using 100% of the
! smoothed final stacking velocities, picked on the migrated data, will be
! appropriate.  For LOBO, we used a 100% smoothed version of our final stacking
! velocities picked on the migrated data, and this worked pretty well.
! 
! However, this approach did not work very well with the Witch data.  In the
! Witch 3D survey, more structural complexity is present (salt structures
! flanked by mini-basins with significant lateral velocity variation between
! them).  When 100% of the smoothed final stacking velocities were used, data
! around the salt were migrated reasonably well, but data in the mini-basin were
! under-migrated significantly.  In fact, it was necessary to increase the
! migration velocity by near 20% in the mini-basin in order to migrate the data
! in that area properly.  One approach to solving this problem would be to
! generate migration velocity percentage panels for selected velocity lines and
! modify the velocities by the appropriate percentages.  This velocity field
! could then be used for the final migration.
! 
! NOTE 13 DAP parameter  99/01/14 The amplitude is scaled  by the DAP
! parameter in PSTMIG for modeling and migration.  The factor (Kz/W)**DAP has
! traditionally been used in FK migrations as a poor mans dip filter.  In FK
! migration the migration step applies this scale factor, and modeling applies
! the inverse of this weighting.  According to theory, the default value of
! DAP=1.  PSTMIG phase shift, in order to mimic FK migration behavior, provided
! the DAP parameter for dip filtering.  However a careful comparison of the
! phase shift and FK formulas indicates that, in principle, DAP=1 and that the
! scaling (or its inverse)  should only be applied to the modeling step.  Hence,
! the latest PSTMIG applies (Kz/W)** (DAP-1) weighting for migration, and
! (W/Kz)**DAP for modeling.  You must use the same DAP value for your modeling
! and migration steps if you want modeling followed by migration to be
! approximately the identity operatorc
! 
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! PSTMIG accepts and images traces unitl it senses a change in offset groups,
! PSTMIG then outputs the image volume for the current offset group.
! PSTMIG then reinitialize the image volume and returns to inputting and
! imageing traces for the next offset gorup.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! PSTMIG accepts and images traces unitl it senses a change in offset groups,
! PSTMIG then outputs the image volume for the current offset group.
! PSTMIG then reinitialize the image volume and returns to inputting and
! imageing traces for the next offset gorup.
! For common angle migration PSTMIG gets the 
! ray parameter from header word hdr_offset* 1 000 000.
! Hence the ray parameter units are microseconds per meter
! for velcoities in meters / second.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                          Action taken
! ----      -----------                          ------------
! NUMTR     max number of traces input/output    Set to 1
! GATHERED  whether traces are gathered          Set to .false.
! NWIH      number of words in trace header      used but not changed
! NDPT      number of sample values in trace     used and changed
! TSTRT     Trace starting time                  used and changed
! DP        Trace time smaple rate               used and changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! HDR     Description                Action taken
! ----    -----------                ------------
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!     Date        Author         Description
!     ----        ------         -----------
! 32. 2006-12-04  D. Glover      Added NULLIFY statements for Intel compiler
! 31. 2006-06-06  Stoeckley      Add pc_register_array_names for SeisSpace.
! 30. 2006-01-10  B. Menger      Removed Unused Variables.
! 29  2005-01-31  Douglas Hanson Use new headsave structure.
! 28  2003-04-01  Douglas Hanson Add common angle imaging.
! 27  2001-10-25  Goodger        Rename labels beginning with if to get around
!                                intel compiler bug.
! 26  2001-09-04  Douglas Hanson Fix bottom mute bug.
! 25  2001-08-29  RSDay          Eliminated update_state test in update
! 24  2001-08-24  RSDay          Fixed call to velutil_convert
!                                Fixed gui field for PATH_VEL
! 23  2001-08-23  Douglas Hanson Add pathhchoose.
! 22  2001-04-10  Douglas Hanson Adopt pi from named_constants.
! 21  2001-04-05  Douglas Hanson Correct frequency increment.
! 20  2001-01-02  Douglas Hanson Adapt to expanded pario
! 19  2000-12-15  Douglas Hanson Correct use of no_more_traces
! 18  2000-12-12  Douglas Hanson Add skip_wrapup 
! 17  2000-11-27  Douglas Hanson Fix OPT_DIR bug.
! 16  2000-10-24  Douglas Hanson Adopt X - Y parameters.
! 15  2000-09-19  Douglas Hanson No if test for control cards.
! 14  2000-09-06  Douglas Hanson Modify velgrid_size usage
! 13  2000-09-06  Douglas Hanson Improve documentation.
! 12  2000-08-28  Douglas Hanson Improve documentation.
! 11  2000-08-25  Douglas Hanson cpsfcr
! 10  1999-10-07  Douglas Hanson Use unique file names and optimal paths.
!  9  1999-09-08  Douglas Hanson Use integer arithmetic for efficiecency.
!  8  1999-09-01  Douglas Hanson Add ONEOUT parameter.
!  7  1999-08-31  Douglas Hanson Add Stolt forward and inverse DMO.
!  6  1999-08-24  Douglas Hanson Modify output trace counter for X_TOT=0.
!  5  1999-08-23  Douglas Hanson Disable report files.
!  4  1999-07-23  Douglas Hanson Add the ONEGRP parameter.
!  3  1999-07-23  Douglas Hanson Adopt offset conventions.
!  2  1999-07-22  Douglas Hanson Add VELOCITY_SCALE velocity analysis parameter.
!  1  1997-08-05  Douglas Hanson Original version
!-----------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
!
! This process uses a single set of trace and header arrays.
!
! Upon input ,NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
! Upon output ,NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
! None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
! References
!
! Jin, S. and Mosher, C., 2001, 
! Offset plane wave pseudo-screen migration, 
! 71st Ann. Internat. Mtg: Soc. of Expl. Geophys., 1029-1032. 
! 
! Mosher, C., Jin, S. and Foster, D., 2001, 
! Migration velocity analysis using common angle image gathers, 
! 71st Ann. Internat. Mtg: Soc. of Expl. Geophys., 889-892. 
! 
! Jin, S., Mosher, C. and Wu, R. S., 2000, 
! 3-D prestack wave equation common offset pseudoscreen depth migration, 
! 70th Ann. Internat. Mtg: Soc. of Expl. Geophys., 842-845. 
! 
! Mosher, C. and Foster, D., 2000, 
! Common angle imaging conditions for prestack depth migration, 
! 70th Ann. Internat. Mtg: Soc. of Expl. Geophys., 830-833. 
! 
! Mosher, C. C., Foster, D. J. and Hassanzadeh, S., 1997, 
! Common angle imaging with offset plane waves, 
! 67th Ann. Internat. Mtg: Soc. of Expl. Geophys., 1379-1382. 
!
! From common angle imaging with plane waves
! Mosher, Foster, Hasanzadeh
! 67 SEG expanded abstracts, 1997, page 1380, equation 12, 
! 
! ks = sqrt ( 4.*w**2/v**2 + kx**2 + ky**2 + 2 * w * ph * kx ) ! source   term
! kr = sqrt ( 4.*w**2/v**2 + kx**2 + ky**2 - 2 * w * ph * kx ) ! recevier term
! kz = .5 * ( ks + kr ) ! z wavenumber
! 
! Where 
! 
! w = frequency in radians / sec
! v = velocity in distnace / sec
! kx = x midpoint wavenumber in reciprocal distance
! ky = y midpoint wavenumber in reciprocal distance
! ph = offset rayparameter in the x dierection in reciprocal velocity units
! 
! for post stack we can have the following types of waves
!        normal  -          normal  
!        turning -          turning 
! for pre  stack we can have the following types of waves
! source normal  - recevier normal 
! source turning - recevier normal 
! source normal  - recevier turning 
! source turning - recevier turning 
!
! there are nt_vel velocity nodes and nt_tab table nodes
! the table node 1      is velocity node iv_vel + 1
! the table node nt_tab is velocity node nt_vel
! rv_vel is the velocity at each velocity node
! rv_tab is the velocity at each table    node
! i0_vot is the vleocity node for each table node
! i0_tov = 1 for velocity nodes 1 - iv_vel+1
! and increments by 1 thereafter
!
! initialize sinc interpolator coefficints.
! see notes of R. S. Day or W. S Harlan article 
! in SEP 30. May, 1982 pp 103-112
!
!-------------------------------------------------------------------------------
!</programming_doc>
!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!<gui_def>
!<NS PSTMIG/NC=80>
!     Phase shift, FK, Cascaded FK and common angle migration
! 
! HDR_X~~=`IIIIIIII HDR_Y~~=`IIIIIIII  OFF_HDR =`IIIIIIII
!   X_TOT=`IIIIIIII   X_INIT=`FFFFFFFF   X_LAST=`FFFFFFFF   X_INC=`FFFFFFFF 
!   Y_TOT=`IIIIIIII   Y_INIT=`FFFFFFFF   Y_LAST=`FFFFFFFF   Y_INC=`FFFFFFFF 
! TIM_TOT=`IIIIIIII TIM_INIT=`FFFFFFFF TIM_LAST=`FFFFFFFF TIM_INC=`FFFFFFFF 
! OFF_TOT=`IIIIIIII OFF_INIT=`FFFFFFFF OFF_LAST=`FFFFFFFF OFF_INC=`FFFFFFFF 
! RAY_TOT=`IIIIIIII RAY_INIT=`FFFFFFFF RAY_LAST=`FFFFFFFF RAY_INC=`FFFFFFFF 
!
! X_FFT~~=`IIIIIIII Y_FFT~~=`IIIIIIII TIM_FFT =`IIIIIIII
! X_TAPER=`IIIIIIII Y_TAPER=`IIIIIII 
! X_TERP =`IIIIIIII Y_TERP =`IIIIIIII 
! X_FILE =`IIIIIIII Y_FILE =`IIIIIIII
!
! CONST_VEL=`FFFFFFFFFFF
! SELECT PATH_VEL [PATH_VEL]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! SELECT PATH_STR [PATH_STR]`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! REF_NAME=`SSSSSSSSSSSSSSSSSSSS
! PATH_MIG=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
! DIMENSION=`CCCCCCC  SORT_ORDER=`CCCCCCCC PAR_IO~~=`CCCCCCC STR_TYPE~=`CCCCCCC
! MIG_TYPE=`CCCCCCC   OPT_DIR~~~=`CCCCCCC  OPT_OBLIQUITY=`CCCCCCC  
! ALL_OUT~~=`CC       DISK_BUF=`CCCCCCC   OPT_COMMON_ANGLE=`CC 
! AMP_PWR~~=`FFFFFFF  DIP_LIM =`FFFFFFF   VELOCITY_SCALE=`FFFFFFF
! FSWP~~~~~=`FFFFFFF  DAP~~~~=`FFFFFFF    
! NUM_PHASE_TABLE=`IIIIIIII 
!
! FREQ_TIM       FREQ_LOW_NONE   FREQ_HIGH_FULL
! `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
! `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
! `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
! `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!
!<PARMS PATH_VEL[/ML=128/XST]>
!<PARMS PATH_STR[/ML=128/XST]>
!<PARMS FREQ_TIM_ARRAYSET[/ML=128/XST/YST]>
!<PARMS PSTMIG [screen1]>
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="DIMENSION">
!<Tip> The dimensionality of the migration and data. </Tip>
! Default = 2D
! Allowed = 2D means 2D time migration.
! Allowed = 3D means 3D time migration.
!</Help>
!
!<Help KEYWORD="SORT_ORDER">
!<Tip> Type of input and output order. </Tip>
! Default = X_IS_FAST
! Allowed = X_IS_FAST data order is x line - CROSSLINE 
! Allowed = Y_X data order is CROSSLINE - x line
!</Help>
!
!<Help KEYWORD="MIG_TYPE">
!<Tip> Type of imaging. </Tip>
! Default = PHASE    - Phase shift time migration.
! Allowed = PHASE    - Phase shift time migration.
! Allowed = FK       - FK time migration.
! Allowed = CASCADED - Cascaded time migration.
!</Help>
!
!<Help KEYWORD="OPT_DIR">
!<Tip> Imaging direction. </Tip>
! Default = FORWARD
! Allowed = FORWARD migration or 
! Allowed = INVERSE migration, modeling.
!</Help>
!
!<Help KEYWORD="OPT_OBLIQUITY">
!<Tip> Obliquity correction option. </Tip>
! Default = NONE
! Allowed = NONE
! Allowed = SURFACE
! Allowed = FULL
!</Help>
!
!<Help KEYWORD="NUM_PHASE_TABLE">
!<Tip> Number of ray parameters in table. </Tip>
! Default = 500
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="STR_TYPE">
!<Tip> Type of stretching. </Tip>
! Default = NONE
! Allowed = NONE
! Currently PSTMIG does not do any internal stretching use STRETCH externaly 
! until this is changed.
!</Help>
!
!<Help KEYWORD="ALL_OUT">
!<Tip> Output all image traces defined by the image grid. </Tip>
! Default = YES
! Allowed = YES KMIG will output all image traces defined by the X,Y grid.
! Allowed = NO  KMIG will output only those image trace which have an input 
! within the image bin.
!</Help>
!
!<Help KEYWORD="OPT_COMMON_ANGLE">
!<Tip> Use common angle imaging. </Tip>
! Default = NO
! Allowed = YES PSTMIG will     use a comon angle imaging algorithm.
! Allowed = NO  PSTMIG will not use a comon angle imaging algorithm.
! within the image bin.
!</Help>
!
!<Help KEYWORD="DISK_BUF">
!<Tip> Image stage flag. </Tip>
! Default = YES
! Allowed = NO
! Allowed = YES
! Allowed = INPUT
! Allowed = MIGRATE
! Allowed = OUTPUT
! PSTMIG constructs a full volume migrated image from the input data.
! 
! If DISK_BUF = NO or NDIM = 2 PSTMIG stores this data in memory.  This is
! intended for use on the T3E where the memory can be allocated from many
! processors.  The J90 or C90 share memory and are much mor limited in this
! regard.  Normally you should use DISK_BUF = YES on the J90 or C90.  See note
! 12 for further discussion of its use on the T3E.
! 
! If DISK_BUF = YES, INPUT,MIGRATE or OUTPUT  PSTMIG stores the data in a disk
! file.  This will default to a tmp disk file whose name will be something like
! PSTMIG_XXX.data, where the XXX will be replaced by an integer corresponding to
! the PSTMIG process number within the job.  By default this will go to a tmp
! disk and will be deleted when the job completes.  For more details on how ow
! to use the INPUT,MIGRATE, OUTPUT procedure, see NOTE 14.
! 
! You can direct this work disk file to a ptmp file by includeing the ptmp path
! within the disk file name, e.g.  MIGFILE =
! /home/bdrt/ptmp/hansodw/PSTMIG.image
! 
! This will create a file using ptmp disk space.  This file will be removed
! automaticaly following job completion.  If a job aborts and you have used the
! ptmp disk you may need to delete the data file manually.
! 
! If you are going to run concurrent PSTMIG jobs, make sure to use a unique file
! name for MIGFILE for each separate PSTMIG job that uses ptmp disk space.
! 
! Make sure you use the same ptmp MIGFILE file name for each of the three jobs
! in a DISK_BUF =  INPUT, MIGRATE or OUTPUT job series.
! 
! The T3E has a very small tmp disk space.  Therfore you should always use a
! ptmp file name for MIGFILE on the T3E.
! 
!</Help>
! 
!<Help KEYWORD="PAR_IO">
!<Tip> Flag for parrellel IOI. </Tip>
! Default = NO
! Allowed = NO
! Allowed = YES
! Currently this is disabled.
!</Help>
!
!<Help KEYWORD="AMP_PWR">
!<Tip> Amplitude coefficient. </Tip>
! Default = 1.
! Allowed = real >= 0.
! for migration scale amplitudes by: 
! ( v ( 0 ) * Kz ( 0 ) ) / ( v ( t ) * Kz ( t ) )] ** AMP_PWR
! for modeling  scale amplitudes by: 
! ( v ( t ) * Kz ( t ) ) / ( v ( 0 ) * Kz ( 0 ) )] ** AMP_PWR
!</Help>
!
!<Help KEYWORD="DIP_LIM">
!<Tip> Maximum migration dip scaler. </Tip>
! default = 1.
! allowed = 0.<= real <= 1.
! PSTMIG will migrate up to DIP_LIM * the theoretical max dip 
! for each frequency.
!</Help>
!
!<Help KEYWORD="VELOCITY_SCALE">
!<Tip> Migration velocity scaler. </Tip>
! default = 1.
! allowed = real > 0.
! PSTMIG will scale the input migration velocity field by VELOCITY_SCALE
! before imaging.
!</Help>
!
!<Help KEYWORD="hdr_x">
!<Tip> Header word indicating the x line direction. </Tip>
! Default = 7
! Allowed = 1<=int<=64
! The number of image traces output may be less than X_TOT*Y_TOT 
! if no input traces are ever migrated to a portion of the image.
!</Help>
!
!<Help KEYWORD="X_TOT">
!<Tip> Number of image locations to migrate data to in the X direction.</Tip>
! Default = 101
! Allowed = int>0
! The number of image traces output may be less than X_TOT*Y_TOT 
! if no input traces are ever migrated to a portion of the image.
!</Help>
!
!<Help KEYWORD="X_INIT">
!<Tip> First X image location to migrate data to.  </Tip>
! Default = 0
! Allowed = real
! Units are determined by hdr_x.
!</Help>
!
!<Help KEYWORD="X_LAST">
!<Tip> Last X image location to migrate data to.   </Tip>
! Default = 100
! Allowed = real
! Units are determined by hdr_x.
!</Help>
!
!<Help KEYWORD="X_INC">
!<Tip> X image location spacing to migrate data to.   </Tip>
! Default = 1
! Allowed = real
! Units are determined by hdr_x.
!</Help>
!
!<Help KEYWORD="X_FFT">
!<Tip> Number of X fft samples. </Tip>
! Default = -1
! Allowed -1, integer >= X_TOT
! Using -1 sets X_FFT to the next power of 2 > X_TOT
! PSTMIG works best if X_FFT is a product of pwoers of 2,3 and 5
!</Help>
!
!<Help KEYWORD="X_TAPER">
!<Tip> X taper length. </Tip>
! Default = 0
! Allowed integer >= 0
! PSTMIG will taper the horizontal fft a X_TAPER traces forom either edge.
!</Help>
!
!<Help KEYWORD="X_TERP">
!<Tip> X interpolation factor. </Tip>
! Default = 1
! Allowed integer >= 1
! PSTMIG will interpolate the data in the X direction by a factor of
! X_TERP in the FFT domain.
!</Help>
!
!<Help KEYWORD="X_FILE">
!<Tip> Number of file decompositions in the X direction. </Tip>
! Default = 1
! Allowed integer >= 1
! During migration PSTMIG must transpose the input data from 
! X - Y order to Y - X order and back.
! To do this efficiently PSTMIG will write the data to X_FILE * Y_FILES 
! disk files and then transpose each of these subfiles.
! Increasing either X_FILE or Y_FILE reduces the total disk space.
!</Help>
!
!<Help KEYWORD="hdr_y">
!<Tip> Header word indicating the crossline direction. </Tip>
! Default = 7
! Allowed = 1<=int<=64
! The number of image traces output may be less than X_TOT*Y_TOT 
! if no input traces are ever migrated to a portion of the image.
!</Help>
!
!<Help KEYWORD="Y_TOT">
!<Tip> Number of image locations to migrate data to in the Y direction. 
!</Tip>
! Default = 1
! Allowed = int>0
! The number of image traces output may be less than X_TOT*Y_TOT 
! if no input traces are ever migrated to a portion of the image.
!</Help>
!
!<Help KEYWORD="Y_INIT">
!<Tip> First Y image location to migrate data to.   </Tip>
! Default = 0
! Allowed = real
! Units are determined by hdr_y.
!</Help>
!
!<Help KEYWORD="Y_LAST">
!<Tip> Last Y image location to migrate data to.   </Tip>
! Default = 0
! Allowed = real
! Units are determined by hdr_y.
!</Help>
!
!<Help KEYWORD="Y_INC">
!<Tip> Y image location spacing to migrate data to.   </Tip>
! Default = 1
! Allowed = real
! Units are determined by hdr_y.
!</Help>
!
!<Help KEYWORD="Y_FFT">
!<Tip> Number of Y fft samples. </Tip>
! Default = -1
! Allowed -1, integer >= Y_TOT
! Using -1 sets Y_FFT to the next power of 2 > Y_TOT
! PSTMIG works best if Y_FFT is a product of pwoers of 2,3 and 5
!</Help>
!
!<Help KEYWORD="Y_TAPER">
!<Tip> Y taper length. </Tip>
! Default = 0
! Allowed integer >= 0
! PSTMIG will taper the horizontal fft a Y_TAPER traces forom either edge.
!</Help>
!
!<Help KEYWORD="Y_TERP">
!<Tip> Y interpolation factor. </Tip>
! Default = 1
! Allowed integer >= 1
! PSTMIG will interpolate the data in the Y direction by a factor of
! Y_TERP in the FFT domain.
!</Help>
!
!<Help KEYWORD="Y_FILE">
!<Tip> Number of file decompositions in the Y direction. </Tip>
! Default = 1
! Allowed integer >= 1
! During migration PSTMIG must transpose the input data from 
! X - Y order to Y - X order and back.
! To do this efficiently PSTMIG will write the data to X_FILE * Y_FILES 
! disk files and then transpose each of these subfiles.
! Increasing either X_FILE or Y_FILE reduces the total disk space.
!</Help>
!
!<Help KEYWORD="TIM_TOT">
!<Tip> Number of image time points to migrate data to. </Tip>
! Default = NDPT
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="TIM_INIT">
!<Tip> Minimum time to migrate data to.  </Tip>
! Default = TSTRT
! Allowed = real
! Units are defined by the velocity.
!</Help>
!
!<Help KEYWORD="TIM_LAST">
!<Tip> Maximum time to migrate data to.  </Tip>
! Default = ( NDPT-1 )*DT+TSTRT
! Allowed = real>=TIM_INIT
! Units are defined by the velocity.
!</Help>
!
!<Help KEYWORD="TIM_INC">
!<Tip> Time increment to migrate data to.  </Tip>
! Default = DT
! Allowed = real>0
! Units are defined by the velocity.
!</Help>
!
!<Help KEYWORD="TIM_FFT">
!<Tip> Number of TIME fft samples. </Tip>
! Default = -1
! Allowed -1, integer >= TIM_TOT
! Using -1 sets TIM_FFT to the next power of 2 > TIM_TOT
! PSTMIG works best if TIM_FFT is a product of pwoers of 2,3 and 5
!</Help>
!
!<Help KEYWORD="OFF_HDR">
!<Tip> header word for offset groups. </Tip>
! Default = 6
! Allowed = 1<=int<=64
! For common angle migration PSTMIG gets the 
! ray parameter from header word hdr_offset* 1 000 000.
! Hence the ray parameter units are microseconds per meter
! for velcoities in meters / second.
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Number of offset groups. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<Tip> Minimum offset in the output image.   </Tip>
! Default = 0
! Allowed = real
! Units are defined by velocity.
! If an input traces offset is outside the range defined by 
! OFF_TOT,OFF_INIT,OFF_LAST,OFF_INC the trace is discarded without migrating it.
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Maximum offset in the output image.   </Tip>
! Default = 0
! Allowed = real
! Units are defined by velocity.
! If an input traces offset is outside the range defined by 
! OFF_TOT,OFF_INIT,OFF_LAST,OFF_INC the trace is discarded without migrating it.
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> Output image offset spacing.   </Tip>
! Default = 25
! Allowed = real
! Units are defined by vleocity.
! If an input traces offset is outside the range defined by NOFF, OFFMIN, OFFINC
! the trace is discarded without migrating it.
!</Help>
!
!<Help KEYWORD="RAY_TOT">
!<Tip> Number of ray parameter groups. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="RAY_INIT">
!<Tip> Minimum ray parameter in the output image.   </Tip>
! Default = 0
! Allowed = real
! Units are defined by velcoity and are in microseconds per distance unit.
! If an input traces ray parameter is outside the range defined by 
! RAY_TOT,RAY_INIT,RAY_LAST,RAY_INC the trace is discarded without migrating it.
!</Help>
!
!<Help KEYWORD="RAY_LAST">
!<Tip> Maximum ray parameter in the output image.   </Tip>
! Default = 0
! Allowed = real
! Units are defined by velcoity and are in microseconds per distance unit.
! If an input traces ray parameter is outside the range defined by 
! RAY_TOT,RAY_INIT,RAY_LAST,RAY_INC the trace is discarded without migrating it.
!</Help>
!
!<Help KEYWORD="RAY_INC">
!<Tip> Output image ray parameter spacing.   </Tip>
! Default = 2
! Allowed = real
! Units are defined by velcoity and are in microseconds per distance unit.
! If an input traces ray parameter is outside the range defined by 
! RAY_TOT,RAY_INIT,RAY_LAST,RAY_INC the trace is discarded without migrating it.
!</Help>
!
!<Help KEYWORD="PATH_MIG">
!<Tip> Name of migration data file. </Tip>
! Default = NONE
! Allowed = CHARACTER
!</Help>
!
!<Help KEYWORD="PATH_STR">
!<Tip> Name of stretch velocity file. </Tip>
! Default = NONE
! Allowed = CHARACTER
!</Help>
!
!<Help KEYWORD="SELECT_PATH_STR">
!<Tip> Name of stretch velocity file. </Tip>
! Default = NONE
! Allowed = CHARACTER
!</Help>
!
!<Help KEYWORD="CONST_VEL">
!<Tip> Constant velocity to use if PATH_VEL=NONE. </Tip>
! Default = 1
! Allowed = real > 0.
! pstmig will use a constant velcoity of CONST_VEL if PATH_VEL=NONE.
!</Help>
!
!<Help KEYWORD="SELECT_PATH_VEL">
!<Tip> Name of migration velocity file. </Tip>
! Default = NONE
! Allowed = CHARACTER
!</Help>
!<Help KEYWORD="PATH_VEL">
!<Tip> Name of migration velocity file. </Tip>
! Default = NONE
! Allowed = CHARACTER
!</Help>
!
!<Help KEYWORD="REF_NAME">
!<Tip> Name of migration velocity reference function. </Tip>
! Default = NONE
! Allowed = CHARACTER
!</Help>
!
!<Help KEYWORD="FSWP">
!<Tip> Fold of Stack Weighting Power. </Tip>
! Default = 0.5
! Allowed = 1.0 >= real >= 0.0 
! FSWP weighting causes traces to be weighted such that New Amplitude = Old
! Amplitude ** FSWP.  This is an effort to control lateral edge artifacts.
!</Help>
!
!<Help KEYWORD="DAP">
!<Tip> Dip Attenuation Parameter. </Tip>
! Default = 1
! Allowed = int >= 0
! FKTMIG can perform a sort of dip filtering in the F-K plane.  
! If DAP > 1 then steep dips will be attenuated 
! (with more attenuation for larger values of DAP ).  
! DAP = 1 produces no dip filtering.  DAP = 0 causes increased steeply 
! dipping energy.  
!
! DAP = 2 or 3 is often used to attenuate random noise if there is little 
! steeply dipping desirable signal.
!</Help>
!
!<Help KEYWORD="FREQ_TIM">
!<Tip> Time values for the time - frequency aperture. </Tip>
! Default = 0
! Allowed = real ( array )
! When MIG_TYPE=PHASE, PSTMIG migrates only the frequencies between 
! FREQ_LOW_NONE and FREQ_HIGH_FULL at time FREQ_TIM.  Frequency values are 
! linearly interpolated between input array values for each image time value.
! For MIG_TYPE=FK, CASCADED all frequencies are migrated at all times.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_NONE">
!<Tip> Lower frequency limit values for the time - frequency aperture. </Tip>
! Default = 0
! Allowed = real ( array )
! When MIG_TYPE=PHASE, PSTMIG migrates only the frequencies between 
! FREQ_LOW_NONE and FREQ_HIGH_FULL at time FREQ_TIM.  Frequency values are 
! linearly interpolated between input array values for each image time value.
! For MIG_TYPE=FK, CASCADED all frequencies are migrated at all times.
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_FULL">
!<Tip> Upper frequency limit values for the time - frequency aperture. </Tip>
! Default = 0
! Allowed = real ( array )
! When MIG_TYPE=PHASE, PSTMIG migrates only the frequencies between 
! FREQ_LOW_NONE and FREQ_HIGH_FULL at time FREQ_TIM.  Frequency values are 
! linearly interpolated between input array values for each image time value.
! For MIG_TYPE=FK, CASCADED all frequencies are migrated at all times.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------
module pstmig_module
  !
  use fft_module
  use getsys_module
  use headsave_module
  use interpolate_module
  use memfun_module
  use matfun_module
  use migfun_module
  use named_constants_module
  use cpucount_module
  use pario_module
  use pathcheck_module
  use pathchoose_module
  use pattern_module
  use pc_module
  use pcpsx_module
  use prnfun_module
  use string_module
  use taper_module
  use timeglob_module
  use velgrid_module
  use velio_module
  use velutil_module
  !
  implicit  none
  !
  private
  !
  public :: pstmig_create
  public :: pstmig_delete
  public :: pstmig_initialize
  public :: pstmig_update
  public :: pstmig_input_trap
  public :: pstmig
  public :: pstmig_wrapup
  public :: pstmig_get_status
  public :: pstmig_input
  public :: pstmig_output
  public :: pstmig_read_velocity
  public :: pstmig_set_velocity
  public :: pstmig_interpolate_velocity
  public :: pstmig_if_lim_compute
  public :: pstmig_ac_exp_compute
  public :: pstmig_fk_snc_compute
  public :: pstmig_table_compute
  public :: pstmig_table_compute_0
  public :: pstmig_shift_coef_compute
  public :: pstmig_set_dimension
  public :: pstmig_set_opt_dir
  public :: pstmig_set_mig_type
  public :: pstmig_set_disk_buf
  public :: pstmig_set_sort_order
  public :: pstmig_have_trace
  public :: pstmig_transpose_disk_n
  public :: pstmig_transpose_disk_1
  public :: pstmig_transpose_tile
  public :: pstmig_fill_head
  public :: pstmig_buf_to_pel_x
  public :: pstmig_pel_to_buf_x
  public :: pstmig_buf_to_pel_y
  public :: pstmig_pel_to_buf_y
  public :: pstmig_remove_n
  public :: pstmig_remove
  public :: pstmig_shift_head
  public :: pstmig_conjg_copy
  public :: pstmig_time_freq_fft
  public :: pstmig_time_freq_fft_1
  public :: pstmig_origin_shift
  public :: pstmig_image
  public :: pstmig_mig_or_mod
  public :: pstmig_migrate
  public :: pstmig_model
  public :: pstmig_compute_k1
  public :: pstmig_compute_k2
  public :: pstmig_mig_or_mod_coef
  public :: pstmig_image_fk
  public :: pstmig_image_fk_velocity
  public :: pstmig_image_fk_layer
  public :: pstmig_image_fk_rw_add
  public :: pstmig_image_fk_map_compute
  public :: pstmig_image_fk_map_apply
  public :: pstmig_mcfft
  public :: pstmig_horizontal_decimate
  public :: pstmig_horizontal_interp
  public :: pstmig_horizontal_interp_1
  public :: pstmig_horizontal_interp_sum
  public :: pstmig_open_disk_n
  public :: pstmig_open_disk_1
  public :: pstmig_open_disk
  public :: pstmig_read_disk
  public :: pstmig_write_disk
  public :: pstmig_mig_or_mod_scale
  public :: pstmig_stretch_setup
  public :: pstmig_stretch_setup_length
  public :: pstmig_stretch_setup_fk
  public :: pstmig_stretch_setup_phaseshift
  public :: pstmig_fft_length
  !
  ! functions
  !
  public :: pstmig_dx_wvn
  public :: pstmig_zero_or_nyq
  public :: pstmig_tw_fft
  public :: pstmig_xk_fft
  public :: pstmig_rw_add
  public :: pstmig_rw_fac
  public :: pstmig_rv_prc
  public :: pstmig_find_non_flag
  !
  ! rcs identifier string
  character ( len=100 ),public, save :: pstmig_ident = &
       '$Id: pstmig.f90,v 1.32 2006/12/04 13:29:54 Glover prod sps $'
  !
  type,  public :: pstmig_struct
    !
    private
    !                         
    type ( grid_struct )               :: grid_obj    ! transformation grid
    logical                            :: skip_wrapup ! wrapup flag.
    integer                            :: mh_inp
    integer                            :: mt_inp
    integer                            :: ipn
    integer                            :: n_inp
    integer                            :: ix_inp
    integer                            :: iy_inp
    real                               :: rx_inp
    real                               :: ry_inp
    integer                            :: nh_inp
    integer                            :: nt_inp
    real                               :: t0_inp
    real                               :: t1_inp
    real                               :: dt_inp
    integer                            :: io_grp
    integer                            :: jo_grp
    integer                            :: no_grp
    real                               :: ro_grp
    integer                            :: nx_mem
    integer                            :: ix_mig
    integer                            :: iy_mig
    real                               :: rx_mig
    real                               :: ry_mig
    real                               :: ro_mig
    real                               :: hd_top
    real                               :: hd_bot
    integer                            :: iy_mem
    integer                            :: jy_mem
    integer                            :: nx_rep
    integer                            :: nf_nyq
    real                               :: rf_nyq
    integer                            :: nf_mig
    real                               :: f0_mig
    real                               :: f1_mig
    real                               :: df_mig
    integer                            :: nw_mig
    real                               :: w0_mig
    real                               :: w1_mig
    real                               :: dw_mig
    !
    real                               :: rf_min
    real                               :: rf_max
    integer                            :: it_til
    !
    integer                            :: mh_buf
    integer                            :: md_buf
    integer                            :: mt_buf
    integer                            :: mx_buf
    integer                            :: nh_sav
    integer                            :: i_trin
    integer                            :: j_trin
    integer                            :: l_trin
    integer                            :: m_trin
    integer                            :: n_trin
    integer                            :: i_trot
    integer                            :: j_trot
    integer                            :: l_trot
    integer                            :: m_trot
    integer                            :: n_trot
    integer                            :: io_type
    integer                            :: n_pack
    integer                            :: i_pass
    integer                            :: n_online
    integer                            :: mx_til
    integer                            :: nx_til
    integer                            :: my_til
    integer                            :: ny_til
    integer                            :: mt_til
    integer                            :: nt_til
    integer                            :: mx_dsk
    integer                            :: nx_dsk
    integer                            :: my_dsk
    integer                            :: ny_dsk
    integer                            :: nt_dsk
    integer                            :: nx_ff2
    integer                            :: nx_pel
    integer                            :: ny_pel
    integer                            :: nt_pel
    integer                            :: n_xy_pel
    integer                            :: iw_shf
    integer                            :: nw_shf
    integer                            :: ip_shf
    integer                            :: np_shf
    integer                            :: i0_shf
    integer                            :: n0_shf
    integer                            :: n0_rnd
    integer                            :: np_tab
    real                               :: p0_tab
    real                               :: p1_tab
    real                               :: dp_tab
    real                               :: vp_tab
    real,                      pointer :: rp_tab ( : )
    real                               :: rp_off
    integer                            :: n1_snc
    integer                            :: n2_snc
    logical                            :: inp_flag
    logical                            :: out_flag
    logical                            :: mig_flag
    logical                            :: done_flag
    integer                            :: mx_fil
    integer                            :: my_fil
    character ( len=filename_length )  :: fn_sav
    character ( len=filename_length )  :: fn_x_y
    character ( len=filename_length )  :: fn_y_x
    integer                            :: n_xy_str
    integer                            :: hx_str
    integer                            :: hy_str
    integer                            :: nx_str
    real                               :: x0_str
    real                               :: x1_str
    real                               :: dx_str
    integer                            :: ny_str
    real                               :: y0_str
    real                               :: y1_str
    real                               :: dy_str
    integer                            :: ns_inp
    real                               :: s0_inp
    real                               :: s1_inp
    real                               :: ds_inp
    real,                      pointer :: rs_inp ( : )
    integer                            :: ns_out
    real                               :: s0_out
    real                               :: s1_out
    real                               :: ds_out
    real,                      pointer :: rs_out ( : )
    integer                            :: i_stretch_0
    integer                            :: i_stretch_1
    integer                            :: i_stretch_2
    integer                            :: n_xy_file
    integer                            :: n_xt_xf
    integer                            :: n_yt_xf
    ! note ac_exp is complex but for addressing reasons is declared real here
    integer                            :: nc_exp_m1
    real,                      pointer :: nn_exp_r ( : )
    real,                      pointer :: nn_exp_i ( : )
    real,                      pointer :: nt_exp_r ( : )
    real,                      pointer :: nt_exp_i ( : )
    real,                      pointer :: tn_exp_r ( : )
    real,                      pointer :: tn_exp_i ( : )
    real,                      pointer :: tt_exp_r ( : )
    real,                      pointer :: tt_exp_i ( : )
    integer                            :: nc_exp
    real                               :: dc_exp
    real                               :: vc_exp
    integer                            :: mt_lay
    integer                            :: nt_lay
    integer                            :: m_sto
    integer                            :: m_scr
    integer                            :: n_sto
    integer                            :: n_scr
    real                               :: rv_min
    real                               :: rv_max
    real                               :: rv_ave
    integer                            :: nt_dim
    real                               :: scale_fft
    ! input parameters
    integer                            :: n_dimension
    character ( len=8 )                :: dimension
    real                               :: fswp
    real                               :: dap
    real                               :: amp_pwr
    real                               :: amp_pwr_0
    real                               :: dip_lim
    real                               :: velocity_scale
    !
    ! image definition
    !
    integer                            :: off_hdr
    integer                            :: off_tot
    real                               :: off_init
    real                               :: off_last
    real                               :: off_inc
    !
    integer                            :: ray_tot
    real                               :: ray_init
    real                               :: ray_last
    real                               :: ray_inc
    !
    integer                            :: hdr_x
    integer                            :: x_tot
    integer                            :: x_fft
    integer                            :: x_taper
    integer                            :: x_terp
    integer                            :: x_file
    real                               :: x_init
    real                               :: x_last
    real                               :: x_inc
    !
    integer                            :: hdr_y
    integer                            :: y_tot
    integer                            :: y_fft
    integer                            :: y_taper
    integer                            :: y_terp
    integer                            :: y_file
    real                               :: y_init
    real                               :: y_last
    real                               :: y_inc
    !
    integer                            :: tim_tot
    integer                            :: tim_fft
    real                               :: tim_init
    real                               :: tim_last
    real                               :: tim_inc
    !
    integer                            :: ho_mig
    integer                            :: no_mig
    real                               :: o0_mig
    real                               :: o1_mig
    real                               :: do_mig
    !
    integer                            :: hx_mig
    integer                            :: nx_mig
    integer                            :: nx_fft
    integer                            :: nx_tap
    integer                            :: nx_trp
    integer                            :: nx_fil
    integer                            :: ix_dir
    real                               :: x0_mig
    real                               :: x1_mig
    real                               :: dx_mig
    real                               :: x0_scl
    !
    integer                            :: hy_mig
    integer                            :: ny_mig
    integer                            :: ny_fft
    integer                            :: ny_tap
    integer                            :: ny_trp
    integer                            :: ny_fil
    integer                            :: iy_dir
    real                               :: y0_mig
    real                               :: y1_mig
    real                               :: dy_mig
    real                               :: y0_scl
    !
    integer                            :: nt_vel
    real                               :: t0_vel
    real                               :: t1_vel
    real                               :: dt_vel
    integer                            :: iv_vel
    !
    integer                            :: nt_tab
    real                               :: t0_tab
    real                               :: t1_tab
    real                               :: dt_tab
    real,                      pointer :: rt_tab ( : )
    !
    integer,                   pointer :: i0_vot ( : ) ! vel for each tab
    integer,                   pointer :: i0_tov ( : ) ! tab for each vel
    !
    integer                            :: nt_fft
    real                               :: t0_fft
    real                               :: t1_fft
    real                               :: dt_fft
    !
    integer                            :: nt_mig
    real                               :: t0_mig
    real                               :: t1_mig
    real                               :: dt_mig
    !
    integer                            :: nt_lim
    integer                            :: n1_lim
    integer                            :: n2_lim
    !
    character ( len=filename_length )  :: path_mig
    character ( len=filename_length )  :: path_str
    type ( pathchoose_struct ),pointer :: path_str_button
    character ( len=filename_length )  :: path_vel
    type ( pathchoose_struct ),pointer :: path_vel_button
    real                               :: const_vel
    character ( len=8 )                :: ref_name
    character ( len=8 )                :: str_type
    character ( len=8 )                :: opt_dir
    character ( len=8 )                :: opt_obliquity
    character ( len=8 )                :: mig_type
    logical                            :: all_out
    logical                            :: opt_common_angle
    character ( len=8 )                :: disk_buf
    logical                            :: par_io
    character ( len=9 )                :: sort_order
    character ( len=8 )                :: crd80
    character ( len=8 )                :: vel_type
    character ( len=8 )                :: str_none
    character ( len=8 )                :: str_mode
    integer                            :: str_order
    integer                            :: num_phase_table
    integer                            :: mz_str
    integer                            :: nz_str
    integer                            :: lu_sav
    real,                      pointer :: t0_lim ( : )
    real,                      pointer :: f1_lim ( : )
    real,                      pointer :: f2_lim ( : )
    real,                      pointer :: rt_str ( : )
    real,                      pointer :: rv_str ( : )
    integer,                   pointer :: lu_x_y ( :, : )
    integer,                   pointer :: lu_y_x ( :, : )
    integer,                   pointer :: if_lim ( :, : )
    real,                      pointer :: rf_mig ( : )
    real,                      pointer :: rw_mig ( : )
    real,                      pointer :: vw_mig ( : )
    real,                      pointer :: rt_vel ( : )
    real,                      pointer :: rz_vel ( : )
    real,                      pointer :: rv_vel ( : ) ! velocity at vel node
    real,                      pointer :: rs_vel ( : ) ! slowness at vel node
    real,                      pointer :: rl_vel ( : ) ! sloth    at vel node
    real,                      pointer :: r2_vel ( : ) ! velocity squared
    real,                      pointer :: rt_lay ( : )
    real,                      pointer :: rv_lay ( : )
    real,                      pointer :: rv_tab ( : )
    integer,                   pointer :: tu_tab ( : )
    real,                      pointer :: tim_tab ( :, : )
    real,                      pointer :: amp_tab ( :, : )
    real,                      pointer :: fk_snc ( :, :, : )
    real,                      pointer :: ac_exp ( :, : )
    integer,                   pointer :: ix_hav ( : )
    integer,                   pointer :: iy_hav ( : )
    real,                      pointer :: tr_buf ( :, :, : )
    real,                      pointer :: tr_pel ( :, :, : )
    !
    type ( headsave_struct ),  pointer :: h  ! headsave structure
    type ( cpucount_struct ),  pointer :: c  ! cpucount structure
    type ( fft_struct ),       pointer :: fft_obj_tw
    type ( fft_struct ),       pointer :: fft_obj_wt
    !
    integer                            :: c_pstmig_total       ! total  cpu time
    integer                            :: c_pstmig_input       ! input  cpu time
    integer                            :: c_pstmig_output      ! output cpu time
    integer                            :: c_pstmig_image       ! image  cpu time
    integer                            :: c_pstmig_mig_or_mod  ! m or m cpu time
    integer                            :: c_pstmig_migrate     ! mig ps cpu time
    integer                            :: c_pstmig_model       ! mod ps cpu time
    integer                            :: c_pstmig_migrate_1   ! mig 1  cpu time
    integer                            :: c_pstmig_model_1     ! mod 1  cpu time
    integer                            :: c_pstmig_coef        ! coef   cpu time
    integer                            :: c_pstmig_phase       ! phase  cpu time
    !
    real                               :: a0_tpr
    real                               :: rp_scl
    integer                            :: if_mig
    real                               :: rk_loc
    real                               :: rp_loc
    integer                            :: it_top
    integer                            :: it_bot
    !
    integer                            :: nt_mid
    integer                            :: iw_mid
    !
    integer                            :: nt_src
    integer                            :: iw_src
    integer                            :: ip_src_1
    integer                            :: ip_src_2
    real                               :: rp_src_1
    real                               :: rp_src_2
    real                               :: ra_src_1
    real                               :: ra_src_2
    real                               :: rt_src_1
    real                               :: rt_src_2
    !
    integer                            :: nt_rec
    integer                            :: iw_rec
    integer                            :: ip_rec_1
    integer                            :: ip_rec_2
    real                               :: rp_rec_1
    real                               :: rp_rec_2
    real                               :: ra_rec_1
    real                               :: ra_rec_2
    real                               :: rt_rec_1
    real                               :: rt_rec_2
    !
    logical                            :: pstmig_pos_and_neg 
    logical                            :: pstmig_one_y
    logical                            :: pstmig_image_frequency
    !
    real                               :: rw_loc
    real                               :: vw_loc
    real                               :: rp_src ! source   ray parameter
    real                               :: rp_rec ! recevier ray parameter
    real                               :: rk_src ! source   wavenumber
    real                               :: rk_rec ! recevier wavenumber
    real                               :: ks_sqr ! source   wavenumber squared
    real                               :: kr_sqr ! recevier wavenumber squared
    integer                            :: ix_fft
    real                               :: dx_fft
    real                               :: px_loc
    real                               :: kx_loc
    real                               :: dx_wvn
    integer                            :: iy_fft
    real                               :: py_loc
    real                               :: ky_loc
    real                               :: dy_wvn
    real                               :: dy_fft
    integer                            :: i1_fft
    integer                            :: j1_fft
    integer                            :: n1_fft
    real                               :: d1_fft
    integer                            :: i2_fft
    integer                            :: n2_fft
    real                               :: d2_fft
    !
    logical                            :: pstmig_post_stack
    logical                            :: pstmig_pre_stack
    logical                            :: pstmig_norm_norm
    logical                            :: pstmig_norm_turn
    logical                            :: pstmig_turn_norm
    logical                            :: pstmig_turn_turn
    !
  end type pstmig_struct
  !
  type ( pstmig_struct ) ,pointer, save :: object ! needed for traps.
  !
  integer    , parameter :: dimension_n = 2
  character ( len=8 ), save :: dimension_c ( dimension_n )         &
  = (/ '2D      ', '3D      ' /)
  !
  integer, parameter :: str_type_n = 2
  character ( len=8 ), save :: str_type_c ( str_type_n )   &
  = (/ 'NONE    ', 'YES     ' /)
  !
  integer, parameter :: opt_dir_n = 2
  character ( len=8 ), save :: opt_dir_c ( opt_dir_n )     &
  = (/ 'FORWARD ', 'INVERSE ' /)
  !
  integer, parameter :: opt_obliquity_n = 3
  character ( len=8 ), save :: opt_obliquity_c ( opt_obliquity_n )     &
  = (/ 'NONE    ', 'SURFACE ', 'FULL    ' /)
  !
  integer, parameter :: mig_type_n = 5
  character ( len=8 ), save :: mig_type_c ( mig_type_n )    &
  = (/ 'NORMAL  ','TURNING ','BOTH    ','FK      ','CASCADED' /)
  !
  integer, parameter :: disk_buf_n = 5
  character ( len=8 ), save :: disk_buf_c ( disk_buf_n )    &
  = (/ 'NO      ','YES     ', 'INPUT   ', 'MIGRATE ', 'OUTPUT  ' /)
  !
  integer, parameter :: yes_no_n = 2
  character ( len=8 ), save :: yes_no_c ( yes_no_n )   &
  = (/ 'YES     ', 'NO      ' /)
  !
  integer, parameter :: sort_order_n = 2
  character ( len=9 ), save :: sort_order_c ( sort_order_n )         &
  = (/ 'X_IS_FAST', 'Y_IS_FAST' /)
  !
  contains
  !
  subroutine pstmig_create ( o )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    integer                            :: i_err
    !
    !
    allocate ( o )
    !
    call pstmig_cpucount_create ( o, i_err )
    !
    nullify (o%t0_lim) ! jpa
    nullify (o%f1_lim) ! jpa
    nullify (o%f2_lim) ! jpa
    nullify (o%rp_tab) ! jpa
    nullify (o%rs_inp) ! jpa
    nullify (o%rs_out) ! jpa
    nullify (o%rt_str) ! jpa
    nullify (o%rv_str) ! jpa
    nullify (o%lu_x_y) ! jpa
    nullify (o%lu_y_x) ! jpa
    nullify (o%if_lim) ! jpa
    nullify (o%rf_mig) ! jpa
    nullify (o%rw_mig) ! jpa
    nullify (o%vw_mig) ! jpa
    nullify (o%rt_vel) ! jpa
    nullify (o%rz_vel) ! jpa
    nullify (o%rv_vel) ! jpa
    nullify (o%rs_vel) ! jpa
    nullify (o%rl_vel) ! jpa
    nullify (o%r2_vel) ! jpa
    nullify (o%rt_lay) ! jpa
    nullify (o%rv_lay) ! jpa
    nullify (o%rv_tab) ! jpa
    nullify (o%rt_tab) ! jpa
    nullify (o%nn_exp_r) ! jpa
    nullify (o%nn_exp_i) ! jpa
    nullify (o%nt_exp_r) ! jpa
    nullify (o%nt_exp_i) ! jpa
    nullify (o%tn_exp_r) ! jpa
    nullify (o%tn_exp_i) ! jpa
    nullify (o%tt_exp_r) ! jpa
    nullify (o%tt_exp_i) ! jpa
    nullify (o%i0_vot) ! jpa
    nullify (o%i0_tov) ! jpa
    nullify (o%tu_tab) ! jpa
    nullify (o%tim_tab) ! jpa
    nullify (o%amp_tab) ! jpa
    nullify (o%fk_snc) ! jpa
    nullify (o%ac_exp) ! jpa
    nullify (o%ix_hav) ! jpa
    nullify (o%iy_hav) ! jpa
    nullify (o%tr_buf) ! jpa
    nullify (o%tr_pel) ! jpa
    nullify (o%h) ! jpa
    nullify (o%c) ! jpa
    nullify (o%fft_obj_tw) ! jpa
    nullify (o%fft_obj_wt) ! jpa
    nullify (o%path_str_button) ! jpa
    nullify (o%path_vel_button) ! jpa
    !
    call memfun_nul ( o%t0_lim )
    call memfun_nul ( o%f1_lim )
    call memfun_nul ( o%f2_lim )
    call memfun_nul ( o%rp_tab )
    call memfun_nul ( o%rs_inp )
    call memfun_nul ( o%rs_out )
    call memfun_nul ( o%rt_str )
    call memfun_nul ( o%rv_str )
    call memfun_nul ( o%lu_x_y )
    call memfun_nul ( o%lu_y_x )
    call memfun_nul ( o%if_lim )
    call memfun_nul ( o%rf_mig )
    call memfun_nul ( o%rw_mig )
    call memfun_nul ( o%vw_mig )
    call memfun_nul ( o%rt_vel )
    call memfun_nul ( o%rz_vel )
    call memfun_nul ( o%rv_vel )
    call memfun_nul ( o%rs_vel )
    call memfun_nul ( o%rl_vel )
    call memfun_nul ( o%r2_vel )
    call memfun_nul ( o%rt_lay )
    call memfun_nul ( o%rv_lay )
    call memfun_nul ( o%rv_tab )
    call memfun_nul ( o%rt_tab )
    call memfun_nul ( o%nn_exp_r )
    call memfun_nul ( o%nn_exp_i )
    call memfun_nul ( o%nt_exp_r )
    call memfun_nul ( o%nt_exp_i )
    call memfun_nul ( o%tn_exp_r )
    call memfun_nul ( o%tn_exp_i )
    call memfun_nul ( o%tt_exp_r )
    call memfun_nul ( o%tt_exp_i )
    call memfun_nul ( o%i0_vot )
    call memfun_nul ( o%i0_tov )
    call memfun_nul ( o%tu_tab )
    call memfun_nul ( o%tim_tab )
    call memfun_nul ( o%amp_tab )
    call memfun_nul ( o%fk_snc )
    call memfun_nul ( o%ac_exp )
    call memfun_nul ( o%ix_hav )
    call memfun_nul ( o%iy_hav )
    call memfun_nul ( o%tr_buf )
    call memfun_nul ( o%tr_pel )
    call pathchoose_create ( o%path_str_button, 'path_str', '*' )
    call pathchoose_create ( o%path_vel_button, 'path_vel', '*' )
    
    call pstmig_initialize ( o )
    !
    return
    !
  end subroutine pstmig_create
  !
  subroutine pstmig_delete ( o )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    call headsave_delete ( o%h )
    call cpucount_delete  ( o%c )
    !
    call memfun_del ( o%t0_lim )
    call memfun_del ( o%f1_lim )
    call memfun_del ( o%f2_lim )
    call memfun_del ( o%rp_tab )
    call memfun_del ( o%rs_inp )
    call memfun_del ( o%rs_out )
    call memfun_del ( o%rt_str )
    call memfun_del ( o%rv_str )
    call memfun_del ( o%lu_x_y )
    call memfun_del ( o%lu_y_x )
    call memfun_del ( o%if_lim )
    call memfun_del ( o%rf_mig )
    call memfun_del ( o%rw_mig )
    call memfun_del ( o%vw_mig )
    call memfun_del ( o%rt_vel )
    call memfun_del ( o%rz_vel )
    call memfun_del ( o%rv_vel )
    call memfun_del ( o%rs_vel )
    call memfun_del ( o%rl_vel )
    call memfun_del ( o%r2_vel )
    call memfun_del ( o%rt_lay )
    call memfun_del ( o%rv_lay )
    call memfun_del ( o%tu_tab )
    call memfun_del ( o%rv_tab )
    call memfun_del ( o%rt_tab )
    call memfun_del ( o%nn_exp_r )
    call memfun_del ( o%nn_exp_i )
    call memfun_del ( o%nt_exp_r )
    call memfun_del ( o%nt_exp_i )
    call memfun_del ( o%tn_exp_r )
    call memfun_del ( o%tn_exp_i )
    call memfun_del ( o%tt_exp_r )
    call memfun_del ( o%tt_exp_i )
    call memfun_del ( o%i0_vot )
    call memfun_del ( o%i0_tov )
    call memfun_del ( o%tim_tab )
    call memfun_del ( o%amp_tab )
    call memfun_del ( o%fk_snc )
    call memfun_del ( o%ac_exp )
    call memfun_del ( o%ix_hav )
    call memfun_del ( o%iy_hav )
    call memfun_del ( o%tr_buf )
    call memfun_del ( o%tr_pel )
    !
    ! delete the fft structures
    !
    call fft_delete ( o%fft_obj_tw )
    !
    call fft_delete ( o%fft_obj_wt )
    !
    if ( associated ( o%path_str_button ) )  &
    call pathchoose_delete ( o%path_str_button )
    !
    if ( associated ( o%path_vel_button ) )  &
    call pathchoose_delete ( o%path_vel_button )
    !
    deallocate ( o )
    !
    return
    !
  end subroutine pstmig_delete
  !
  subroutine pstmig_initialize ( o )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    o%off_hdr   = 0            ! offset or group header word
    o%off_tot   = 1            ! number of offset or group bin center
    o%off_init  = 0            ! min offset or group bin center
    o%off_last  = 1e6          ! max or group bin spacing
    o%off_inc   = 1e6          ! offset or group bin spacing
    !
    o%ray_tot   = 1            ! number of ray parameter or group bin center
    o%ray_init  = 0            ! min ray parameter or group bin center
    o%ray_last  = 1e6          ! max ray parameter or group bin spacing
    o%ray_inc   = 2            ! ray parameter or group bin spacing
    !
    o%hdr_x  = 7            ! image x header word
    o%x_tot  = 101          ! number of image x bins
    o%x_inc  = 1            ! image x bin spacing
    o%x_init = 0            ! first image x bin center
    o%x_last = 0            ! last image x bin center
    o%x_fft  = -1           ! x fft length
    o%x_taper= 0            ! x spatial taper length
    o%x_terp = 1            ! x interpolation ratio
    o%x_file = 1            ! number of files in x direction
    !
    o%hdr_y  = 8            ! image y header word
    o%y_tot  = 101          ! number of image y bins
    o%y_inc  = 1            ! image y bin spacing
    o%y_init = 0            ! first image y bin center
    o%y_last = 0            ! last image y bin center
    o%y_fft  = -1           ! y fft length
    o%y_taper= 0            ! y spatial taper length
    o%y_terp = 1            ! y interpolation ratio
    o%y_file = 1            ! number of files in y direction
    !
    ! set the output migration time values
    !
    call timeglob_get ( o%nt_mig, o%t0_mig, o%dt_mig )
    !
    o%t1_mig = ( o%nt_mig - 1 ) * o%dt_mig + o%t0_mig 
    !
    o%tim_fft  = -1           ! t fft length
    !
    o%nt_lim    = 0
    !
    o%num_phase_table = 500  ! number of ray parameters for table
    !
    o%path_mig   = 'NONE'       ! migration image disk file name
    o%path_str   = 'NONE'       ! stretch velocity file name
    o%path_vel   = 'NONE'       ! migration velocity file name
    o%const_vel  = 2000.        ! constant velocity
    o%ref_name   = 'FIRST'      ! migration velocity function name
    o%opt_dir    = 'FORWARD'    ! flag for migration or modeling
    o%opt_obliquity = 'NONE'    ! flag for obliquity correction
    o%mig_type   = 'NORMAL'     ! flag for normal waves ,turning waves 
    o%str_type   = 'NONE'       ! stretch type
    o%all_out    = .true.       ! flag for what traces are output
    o%opt_common_angle= .false. ! flag for common angle imaging
    o%disk_buf   = 'NO'         ! flag for storing data on disk or memory
    o%par_io     = .false.      ! flag for parrellel IO
    o%sort_order = 'X_IS_FAST'  ! data ordering
    !
    o%dimension  = '2D '       ! migration dimension, 2D, 3D
    o%n_dimension= 2           ! dimensionality 2D and 3D
    o%fswp      = 1.           ! fold of stack scaler
    o%dap       = 1.           ! dip scaler
    o%dip_lim   = 1.           ! dip limit parameter
    o%velocity_scale   = 1.    ! mig vel = inp vel * velocity_scale
    o%amp_pwr   = 1.           ! amplitude coefficient
    !
    o%ix_fft = 1
    o%iy_fft = 1
    o%rp_off = 0.
    o%kx_loc = 0.
    o%ky_loc = 0.
    o%rk_src = 0.
    o%rk_rec = 0.
    o%ks_sqr = 0.
    o%kr_sqr = 0.
    !
    o%pstmig_post_stack = .not. o%opt_common_angle
    o%pstmig_pre_stack  =       o%opt_common_angle
    o%pstmig_norm_norm = .false. 
    o%pstmig_norm_turn = .false. 
    o%pstmig_turn_norm = .false. 
    o%pstmig_turn_turn = .false. 
    !
    call pstmig_update ( o )
    !
    return
    !
  end subroutine pstmig_initialize
  !
  subroutine pstmig_update ( o )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !

    !        







    !
    object => o               ! needed for traps.
    !
    o%skip_wrapup = .true.    ! needed for the wrapup routine.

      call pc_register_array_names ("freq_tim_arrayset", (/  &
                                    "freq_tim      ",        &
                                    "freq_low_none ",        &
                                    "freq_high_full" /))

    !
    ! get parameters
    !
    call pstmig_parameter_get ( o )
    !
    ! verify parameters
    !
    call pstmig_parameter_verify ( o )
    !
    ! put parameters
    !
    call pstmig_parameter_put ( o )
    !
    if ( pc_do_not_process_traces() ) return
    !
    ! prep parameters for processing
    !
    call pstmig_parameter_prep ( o )
    !
    return
    !
  end subroutine pstmig_update
  !
  subroutine pstmig_parameter_get ( o )
    !
    ! get parameters
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    ! get the time globals
    !
    call timeglob_get ( o%nt_inp, o%t0_inp, o%dt_inp )
    !
    o%t1_inp = ( o%nt_inp - 1 ) * o%dt_inp + o%t0_inp 
    !
    ! get the rotation grid object
    !
    call pc_get_global ( 'GRID' , o%grid_obj )
    !
    ! get the number of words in the header
    !
    call pc_get_global ( 'NWIH' , o%nh_inp )
    if ( pathchoose_update ( o%path_str_button,   o%path_str   ) ) return
    if ( pathchoose_update ( o%path_vel_button,   o%path_vel   ) ) return
    call pc_get ( 'dimension',      o%dimension      )
    call pc_get ( 'fswp',           o%fswp           )
    call pc_get ( 'dap',            o%dap            )
    call pc_get ( 'amp_pwr',        o%amp_pwr        )
    call pc_get ( 'dip_lim',        o%dip_lim        )
    call pc_get ( 'velocity_scale', o%velocity_scale )
    !
    call pc_get ( 'off_hdr',        o%off_hdr        )
    call pc_get ( 'off_tot',        o%off_tot        )
    call pc_get ( 'off_init',       o%off_init       )
    call pc_get ( 'off_last',       o%off_last       )
    call pc_get ( 'off_inc',        o%off_inc        )
    call pc_get ( 'ray_tot',        o%ray_tot        )
    call pc_get ( 'ray_init',       o%ray_init       )
    call pc_get ( 'ray_last',       o%ray_last       )
    call pc_get ( 'ray_inc',        o%ray_inc        )
    call pc_get ( 'hdr_x',          o%hdr_x          )
    call pc_get ( 'x_tot',          o%x_tot          )
    call pc_get ( 'x_init',         o%x_init         )
    call pc_get ( 'x_last',         o%x_last         )
    call pc_get ( 'x_inc',          o%x_inc          )
    call pc_get ( 'x_fft',          o%x_fft          )
    call pc_get ( 'x_taper',        o%x_taper        )
    call pc_get ( 'x_terp',         o%x_terp         )
    call pc_get ( 'x_file',         o%x_file         )
    call pc_get ( 'hdr_y',          o%hdr_y          )
    call pc_get ( 'y_tot',          o%y_tot          )
    call pc_get ( 'y_init',         o%y_init         )
    call pc_get ( 'y_last',         o%y_last         )
    call pc_get ( 'y_inc',          o%y_inc          )
    call pc_get ( 'y_fft',          o%y_fft          )
    call pc_get ( 'y_taper',        o%y_taper        )
    call pc_get ( 'y_terp',         o%y_terp         )
    call pc_get ( 'y_file',         o%y_file         )
    call pc_get ( 'tim_tot',        o%tim_tot        )
    call pc_get ( 'tim_init',       o%tim_init       )
    call pc_get ( 'tim_last',       o%tim_last       )
    call pc_get ( 'tim_inc',        o%tim_inc        )
    call pc_get ( 'tim_fft',        o%tim_fft        )
    call pc_get ( 'path_mig',       o%path_mig       )
    call pc_get ( 'path_str',       o%path_str       )
    call pc_get ( 'path_vel',       o%path_vel       )
    call pc_get ( 'const_vel',      o%const_vel      )
    call pc_get ( 'ref_name',       o%ref_name       )
    call pc_get ( 'str_type',       o%str_type       )
    call pc_get ( 'opt_dir',        o%opt_dir        )
    call pc_get ( 'opt_obliquity',  o%opt_obliquity  )
    call pc_get ( 'mig_type',       o%mig_type       )
    call pc_get ( 'all_out',        o%all_out        )
    call pc_get ( 'opt_common_angle', o%opt_common_angle )
    call pc_get ( 'disk_buf',       o%disk_buf       )
    call pc_get ( 'par_io',         o%par_io         )
    call pc_get ( 'sort_order',     o%sort_order     )
    call pc_get ( 'num_phase_table', o%num_phase_table )
    o%n1_lim = o%nt_lim
    o%n2_lim = o%nt_lim
    call pc_alloc ( 'freq_tim',       o%t0_lim, o%nt_lim )
    call pc_alloc ( 'freq_low_none',  o%f1_lim, o%n1_lim )
    call pc_alloc ( 'freq_high_full', o%f2_lim, o%n2_lim )
    !
  end subroutine pstmig_parameter_get
  !
  subroutine pstmig_parameter_verify ( o )
    !
    ! verify parameters
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    integer                            :: i_stat
    !
    if_nt_lim_err : &
    if ( o%nt_lim .ne. o%n1_lim &
    .or. o%nt_lim .ne. o%n2_lim ) then
      !
      write ( pc_get_lun(), ' ( &
      & /, " error in pstmig_update pe=" ,i8, &
      & /, " nt_lim .ne. n1_lim, n2_lim " &
      & /, " nt_lim=", i8, " n1_lim=", i8, " n2_lim=", i8 &
      & )' ) &
      pcpsx_i_pel(), &
      o%nt_lim, o%n1_lim, o%n2_lim
      !
      xxif_do_not_process : if ( pc_do_not_process_traces() ) then
        !
        call pc_print ( 'pstmig_update 986 error nt_lim=',o%nt_lim )
        call pc_print ( 'n1_lim=',o%n1_lim, 'n2_lim=',o%n2_lim )
        !
        o%nt_lim = min ( o%nt_lim, o%n1_lim, o%n2_lim )
        o%n1_lim = o%nt_lim
        o%n2_lim = o%nt_lim
        !
      else xxif_do_not_process
        !
        call pc_error ( 'pstmig_update 986 error nt_lim=',o%nt_lim )
        call pc_error ( 'n1_lim=',o%n1_lim, 'n2_lim=',o%n2_lim )
        !
        go to 999
        !
      end if xxif_do_not_process
      !
    end if if_nt_lim_err
    !
    call pathcheck ( 'path_mig',o%path_mig,required=.false. )
    !
    if ( o%hdr_y <= 0 ) then    
      call pc_error ( 'pstmig : hdr_y must be > 0 ' )  
    end if
    !
    if ( o%hdr_y > o%nh_inp ) then    
      call pc_error ( 'pstmig : hdr_y must be < NWIH ' )  
    end if
    !
    if ( o%y_tot <= 0 ) then   
      call pc_error ( 'pstmig : y_tot must be > 0' )
    end if 
    !
    if ( o%y_inc == 0.0 ) then   
      call pc_error ( 'pstmig : y_inc must not be 0 ' )
    end if 
    !
    if ( o%hdr_x <= 0 ) then    
      call pc_error ( 'pstmig : hdr_x must be > 0' )
    end if 
    !
    if ( o%hdr_x > o%nh_inp ) then    
      call pc_error ( 'pstmig : hdr_x must be < NWIH' )
    end if 
    !
    if ( o%x_tot <= 0 ) then    
      call pc_error ( 'pstmig : x_tot must be > 0' )
    end if 
    !
    if ( o%x_inc == 0.0 ) then   
      call pc_error ( 'pstmig : x_inc must not be 0' )
    end if 
    !
    ! make sure grids are well behaved
    i_stat = pattern_stop2 ( 'pstmig:', .true., &
       o%x_init, o%x_inc, o%x_last, o%x_tot, &
           'x_init',  'x_inc',   'x_last',   'x_tot', &
       pc_verify_scalar ( 'x_init' ), pc_verify_scalar ( 'x_inc' ), &
       pc_verify_scalar ( 'x_last' ), pc_verify_scalar ( 'x_tot' ) ) 

    i_stat = pattern_stop2 ( 'pstmig:', .true., &
       o%y_init, o%y_inc, o%y_last, o%y_tot, &
          'y_init',   'y_inc',   'y_last',   'y_tot', &
       pc_verify_scalar ( 'y_init' ), pc_verify_scalar ( 'y_inc' ), &
       pc_verify_scalar ( 'y_last' ), pc_verify_scalar ( 'y_tot' ) )   
    !
    i_stat = pattern_stop2 ( 'pstmig:', .true., &
       o%tim_init, o%tim_inc, o%tim_last, o%tim_tot, &
          'tim_init',   'tim_inc',   'tim_last',   'tim_tot', &
       pc_verify_scalar ( 'tim_init' ), pc_verify_scalar ( 'tim_inc' ), &
       pc_verify_scalar ( 'tim_last' ), pc_verify_scalar ( 'tim_tot' ) )   
    !
    if ( o%off_hdr .gt. 0 ) &
    i_stat = pattern_stop2 ( 'pstmig:', .true., &
       o%off_init, o%off_inc, o%off_last, o%off_tot, &
          'off_init',   'off_inc',   'off_last',   'off_tot', &
       pc_verify_scalar ( 'off_init' ), pc_verify_scalar ( 'off_inc' ), &
       pc_verify_scalar ( 'off_last' ), pc_verify_scalar ( 'off_tot' ) )   
    !
    if ( o%off_hdr .gt. 0 ) &
    i_stat = pattern_stop2 ( 'pstmig:', .true., &
       o%ray_init, o%ray_inc, o%ray_last, o%ray_tot, &
          'ray_init',   'ray_inc',   'ray_last',   'ray_tot', &
       pc_verify_scalar ( 'ray_init' ), pc_verify_scalar ( 'ray_inc' ), &
       pc_verify_scalar ( 'ray_last' ), pc_verify_scalar ( 'ray_tot' ) )   
    !
    call pstmig_set_dimension  ( o%dimension, o%n_dimension )
    call pstmig_set_opt_dir    ( o%opt_dir )
    call pstmig_set_mig_type   ( o%mig_type )
    call pstmig_set_disk_buf   ( o%disk_buf, o%n_dimension )
    call pstmig_set_sort_order ( o%sort_order )
    o%pstmig_post_stack = .not. o%opt_common_angle
    o%pstmig_pre_stack  =       o%opt_common_angle
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep " &
    & )' )
    !
    call pc_error ( ' pstmig_parameter_verify error ' )
    !
    return
    !
  end subroutine pstmig_parameter_verify
  !
  subroutine pstmig_parameter_put ( o )
    !
    ! put parameters
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    call pc_put_options_field ( 'str_type',   str_type_c,   str_type_n   )
    call pc_put_options_field ( 'opt_dir',    opt_dir_c,    opt_dir_n    )
call pc_put_options_field ( 'opt_obliquity', opt_obliquity_c, opt_obliquity_n )
    call pc_put_options_field ( 'mig_type',   mig_type_c,   mig_type_n   )
    call pc_put_options_field ( 'all_out',    yes_no_c,     yes_no_n     )
    call pc_put_options_field ( 'par_io',     yes_no_c,     yes_no_n     )
    call pc_put_options_field ( 'disk_buf',   disk_buf_c,   disk_buf_n   )
    call pc_put_options_field ( 'sort_order', sort_order_c, sort_order_n )
    call pc_put_options_field ( 'dimension',  dimension_c,  dimension_n  )
    !
    call pc_put ( 'dimension',      o%dimension      )
    call pc_put ( 'fswp',           o%fswp           )
    call pc_put ( 'dap',            o%dap            )
    call pc_put ( 'amp_pwr',        o%amp_pwr        )
    call pc_put ( 'dip_lim',        o%dip_lim        )
    call pc_put ( 'velocity_scale', o%velocity_scale )
    !
    call pc_put ( 'off_hdr',        o%off_hdr        )
    call pc_put ( 'off_tot',        o%off_tot        )
    call pc_put ( 'off_init',       o%off_init       )
    call pc_put ( 'off_last',       o%off_last       )
    call pc_put ( 'off_inc',        o%off_inc        )
    !
    call pc_put ( 'ray_tot',        o%ray_tot        )
    call pc_put ( 'ray_init',       o%ray_init       )
    call pc_put ( 'ray_last',       o%ray_last       )
    call pc_put ( 'ray_inc',        o%ray_inc        )
    !
    call pc_put ( 'hdr_x',          o%hdr_x          )
    call pc_put ( 'x_tot',          o%x_tot          )
    call pc_put ( 'x_init',         o%x_init         )
    call pc_put ( 'x_last',         o%x_last         )
    call pc_put ( 'x_inc',          o%x_inc          )
    call pc_put ( 'x_fft',          o%x_fft          )
    call pc_put ( 'x_taper',        o%x_taper        )
    call pc_put ( 'x_terp',         o%x_terp         )
    call pc_put ( 'x_file',         o%x_file         )
    !
    call pc_put ( 'hdr_y',          o%hdr_y          )
    call pc_put ( 'y_tot',          o%y_tot          )
    call pc_put ( 'y_init',         o%y_init         )
    call pc_put ( 'y_last',         o%y_last         )
    call pc_put ( 'y_inc',          o%y_inc          )
    call pc_put ( 'y_fft',          o%y_fft          )
    call pc_put ( 'y_taper',        o%y_taper        )
    call pc_put ( 'y_terp',         o%y_terp         )
    call pc_put ( 'y_file',         o%y_file         )

    call pc_put ( 'tim_tot',        o%tim_tot        )
    call pc_put ( 'tim_init',       o%tim_init       )
    call pc_put ( 'tim_last',       o%tim_last       )
    call pc_put ( 'tim_inc',        o%tim_inc        )
    call pc_put ( 'tim_fft',        o%tim_fft        )

    call pc_put ( 'path_mig',       o%path_mig       )
    call pc_put ( 'path_str',       o%path_str       )
    call pc_put ( 'path_vel',       o%path_vel       )
    call pc_put ( 'const_vel',      o%const_vel      )
    call pc_put ( 'ref_name',       o%ref_name       )
    call pc_put ( 'str_type',       o%str_type       )
    call pc_put ( 'opt_dir',        o%opt_dir        )
    call pc_put ( 'opt_obliquity',  o%opt_obliquity  )
    call pc_put ( 'mig_type',       o%mig_type       )
    call pc_put ( 'all_out',        o%all_out        )
    call pc_put ( 'opt_common_angle', o%opt_common_angle )
    call pc_put ( 'disk_buf',       o%disk_buf       )
    call pc_put ( 'par_io',         o%par_io         )
    call pc_put ( 'sort_order',     o%sort_order     )
    call pc_put ( 'num_phase_table', o%num_phase_table )
    !
    call pc_put ( 'freq_tim',       o%t0_lim, o%nt_lim )
    call pc_put ( 'freq_low_none',  o%f1_lim, o%n1_lim )
    call pc_put ( 'freq_high_full', o%f2_lim, o%n2_lim )
    !
    if ( pc_get_update_state() .eq. PC_FRONTEND &
    .or. pc_get_update_state() .eq. PC_BACKEND ) then
    end if
    !
    o%n_scr      = 1000000
    o%n_sto      = 1000000
    !
    ! set the velcoity scaler globals
    !
    xxif_one_velocity_scale : if ( o%velocity_scale .ne. 1. ) then
      !
      call pc_put_global  ( 'path_vel',      o%path_vel )
      call pc_put_global  ( 'modifier_vel', 'MULTIPLICATIVE' )
      call pc_put_global  ( 'type_vel',     'VTRM' )
      !
    end if xxif_one_velocity_scale 
    !
    ! put the current globals
    !
    call timeglob_put ( o%nt_mig ,o%t0_mig ,o%dt_mig )
    call pc_put_global  ( 'gathered'  ,   .false. )
    call pc_put_control ( 'need_request', .true.  )
    call pc_put_control ( 'need_label',   .true.  )
    call pc_put_control ( 'nscratch'  ,   o%n_scr   )
    call pc_put_control ( 'nstore'    ,   o%n_sto   )
    !
  end subroutine pstmig_parameter_put
  !
  subroutine pstmig_parameter_prep ( o )
    !
    ! prep parameters for processing
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !

    !        
    integer                            :: i_err



    integer                            :: nt_tmp
    real                               :: t0_tmp
    real                               :: dt_tmp
    real                               :: rf_tmp
    !
    if ( pc_do_not_process_traces() ) return
    o%skip_wrapup = .false.
    !
    write ( pc_get_lun(), ' ( &
    & /, " pstmig_parameter_prep ", /, " REVISION: ", &
    & " 29  2003-04-11  Douglas Hanson Use new headsave structure. " &
    & )' )
    !
    ! copy the input parameters to the computational parameters
    ! copy the input parameters to the computational parameters
    !
    xxif_sort_order : &
    if ( string_upper_compare ( o%sort_order,  'X_IS_FAST' ) ) then
      !
      o%hx_mig = o%hdr_x
      o%nx_mig = o%x_tot
      o%x0_mig = min ( o%x_init, o%x_last )
      o%x1_mig = max ( o%x_init, o%x_last )
      o%dx_mig = abs ( o%x_inc )
      !
      o%nx_tap = o%x_taper
      o%nx_fft = o%x_fft
      o%nx_trp = o%x_terp
      o%nx_fil = o%x_file
      !
      o%hy_mig = o%hdr_y
      o%ny_mig = o%y_tot
      o%y0_mig = min ( o%y_init, o%y_last )
      o%y1_mig = max ( o%y_init, o%y_last )
      o%dy_mig = abs ( o%y_inc )
      !
      o%ny_tap = o%y_taper
      o%ny_fft = o%y_fft
      o%ny_trp = o%y_terp
      o%ny_fil = o%y_file
      !
      xxif_ix_dir : if ( o%x_init .le. o%x_last ) then
        !
        o%ix_dir = +1
        !
      else xxif_ix_dir 
        !
        o%ix_dir = -1
        !
      end if xxif_ix_dir 
      !
      xxif_iy_dir : if ( o%y_init .le. o%y_last ) then
        !
        o%iy_dir = +1
        !
      else xxif_iy_dir 
        !
        o%iy_dir = -1
        !
      end if xxif_iy_dir 
      !
    else xxif_sort_order
      !
      o%hy_mig = o%hdr_x
      o%ny_mig = o%x_tot
      o%y0_mig = min ( o%x_init, o%x_last )
      o%y1_mig = max ( o%x_init, o%x_last )
      o%dy_mig = abs ( o%x_inc )
      !
      o%ny_tap = o%x_taper
      o%ny_fft = o%x_fft
      o%ny_trp = o%x_terp
      o%ny_fil = o%x_file
      !
      o%hx_mig = o%hdr_y
      o%nx_mig = o%y_tot
      o%x0_mig = min ( o%y_init, o%y_last )
      o%x1_mig = max ( o%y_init, o%y_last )
      o%dx_mig = abs ( o%y_inc )
      !
      o%nx_tap = o%y_taper
      o%nx_fft = o%y_fft
      o%nx_trp = o%y_terp
      o%nx_fil = o%y_file
      !
      ! determine the direction of the x and y output
      !
      xxif_ix_dir_2 : if ( o%x_init .le. o%x_last ) then
        !
        o%iy_dir = +1
        !
      else xxif_ix_dir_2 
        !
        o%iy_dir = -1
        !
      end if xxif_ix_dir_2 
      !
      xxif_iy_dir_2 : if ( o%y_init .le. o%y_last ) then
        !
        o%ix_dir = +1
        !
      else  xxif_iy_dir_2 
        !
        o%ix_dir = -1
        !
      end if xxif_iy_dir_2 
      !
    end if xxif_sort_order
    !
    o%ho_mig  = o%off_hdr
    !
    xxif_common_angle : if ( o%pstmig_pre_stack ) then
      !
      o%ho_mig  = hdr_offset
      !
      o%no_mig  = o%ray_tot
      o%o0_mig  = min ( o%ray_init, o%ray_last )
      o%o1_mig  = max ( o%ray_init, o%ray_last )
      o%do_mig  = abs ( o%ray_inc )
      !
    else xxif_common_angle 
      !
      o%no_mig  = o%off_tot
      o%o0_mig  = min ( o%off_init, o%off_last )
      o%o1_mig  = max ( o%off_init, o%off_last )
      o%do_mig  = abs ( o%off_inc )
      !
    end if xxif_common_angle 
    !
    o%nt_mig  = o%tim_tot
    o%t0_mig  = min ( o%tim_init, o%tim_last )
    o%t1_mig  = max ( o%tim_init, o%tim_last )
    o%dt_mig  = abs ( o%tim_inc )
    o%nt_fft  = o%tim_fft
    !
    ! put the memory sum counter
    !
    call memfun_sum_put ( o%n_sto )

    if ( o%nt_lim .le. 0 ) then

      o%nt_lim     = 2
      call memfun_all ( o%t0_lim, o%nt_lim, 't0_lim', i_err )
      if ( i_err .ne. 0 ) go to 999
      call memfun_all ( o%f1_lim, o%nt_lim, 'f1_lim', i_err )
      if ( i_err .ne. 0 ) go to 999
      call memfun_all ( o%f2_lim, o%nt_lim, 'f2_lim', i_err )
      if ( i_err .ne. 0 ) go to 999

      o%t0_lim ( 1 )   = 0
      o%t0_lim ( 2 )   = max ( ( o%ns_inp-1 )*o%ds_inp+o%s0_inp , &
                               ( o%ns_out-1 )*o%ds_out+o%s0_out )

        o%f1_lim ( 1 ) = 5
        o%f1_lim ( 2 ) = 5

        o%f2_lim ( 1 ) = 60
        o%f2_lim ( 2 ) = 60

      end if    ! if ( o%nt_lim .le. 0 ) then

      o%n1_lim = o%nt_lim
      o%n2_lim = o%nt_lim

! reset input parameters if need be
      call string_to_upper ( o%ref_name )

      o%mx_fil = 100
      o%my_fil = 100
      o%fswp    = max ( 0., min ( 1., o%fswp ) )     ! fold of stack scaler
      o%dap     = max ( 0., o%dap )                  ! dip scaler
      o%dip_lim = max ( 0., min ( 1., o%dip_lim ) )  ! migrate dip_lim*theoretic
      o%velocity_scale = max ( 0., o%velocity_scale )! mig vel=inpvel*vscale
      o%nx_trp  = max ( 1, min ( 8, o%nx_trp ) )     ! x interpolation ratio
      o%nx_fil  = max ( 1, min ( o%mx_fil, o%nx_fil, o%nx_mig ) )! number of x
      o%ny_trp  = max ( 1, min ( 8, o%ny_trp ) )     ! y interpolation ratio
      o%ny_fil  = max ( 1, min ( o%my_fil, o%ny_fil, o%ny_mig ) )! number of y
      o%n_xy_file = o%nx_fil * o%ny_fil
      o%path_str = 'NONE'                  ! no stretch right now

      if ( o%nx_mig .lt. 1 ) go to 990

! print input values
    if ( pcpsx_i_pel() .eq. 0 ) then

      write ( pc_get_lun() ,' ( &
      & /, " pstmig input parameters " &
      & )' )

      call prnfun ( pc_get_lun(), 'n_dimension'    , o%n_dimension     )
      call prnfun ( pc_get_lun(), 'fswp'     , o%fswp      )
      call prnfun ( pc_get_lun(), 'dap'     , o%dap      )
      call prnfun ( pc_get_lun(), 'dip_lim'  , o%dip_lim   )
      call prnfun ( pc_get_lun(), 'velocity_scale'  , o%velocity_scale   )
      call prnfun ( pc_get_lun(), 'amp_pwr'  , o%amp_pwr   )

      call prnfun ( pc_get_lun(), 'ho_mig'   , o%ho_mig    )
      call prnfun ( pc_get_lun(), 'no_mig'   , o%no_mig    )
      call prnfun ( pc_get_lun(), 'o0_mig'   , o%o0_mig    )
      call prnfun ( pc_get_lun(), 'o1_mig'   , o%o1_mig    )
      call prnfun ( pc_get_lun(), 'do_mig'   , o%do_mig    )

      call prnfun ( pc_get_lun(), 'hx_mig'   , o%hx_mig    )
      call prnfun ( pc_get_lun(), 'nx_mig'   , o%nx_mig    )
      call prnfun ( pc_get_lun(), 'x0_mig'   , o%x0_mig    )
      call prnfun ( pc_get_lun(), 'x1_mig'   , o%x1_mig    )
      call prnfun ( pc_get_lun(), 'dx_mig'   , o%dx_mig    )
      call prnfun ( pc_get_lun(), 'nx_fft'   , o%nx_fft    )
      call prnfun ( pc_get_lun(), 'nx_tap'   , o%nx_tap    )
      call prnfun ( pc_get_lun(), 'nx_trp'   , o%nx_trp    )

      call prnfun ( pc_get_lun(), 'hy_mig'   , o%hy_mig    )
      call prnfun ( pc_get_lun(), 'ny_mig'   , o%ny_mig    )
      call prnfun ( pc_get_lun(), 'y0_mig'   , o%y0_mig    )
      call prnfun ( pc_get_lun(), 'y1_mig'   , o%y1_mig    )
      call prnfun ( pc_get_lun(), 'dy_mig'   , o%dy_mig    )
      call prnfun ( pc_get_lun(), 'ny_fft'   , o%ny_fft    )
      call prnfun ( pc_get_lun(), 'ny_tap'   , o%ny_tap    )
      call prnfun ( pc_get_lun(), 'ny_trp'   , o%ny_trp    )

      call prnfun ( pc_get_lun(), 'nt_mig'   , o%nt_mig    )
      call prnfun ( pc_get_lun(), 't0_mig'   , o%t0_mig    )
      call prnfun ( pc_get_lun(), 't1_mig'   , o%t1_mig    )
      call prnfun ( pc_get_lun(), 'dt_mig'   , o%dt_mig    )
      call prnfun ( pc_get_lun(), 'nt_fft'   , o%nt_fft    )

      call prnfun ( pc_get_lun(), &
      'nt_lim', o%nt_lim, o%t0_lim, o%f1_lim, o%f2_lim )

      call prnfun ( pc_get_lun(), 'path_mig'  , o%path_mig  )
      call prnfun ( pc_get_lun(), 'path_str'  , o%path_str  )
      call prnfun ( pc_get_lun(), 'path_vel'  , o%path_vel  )
      call prnfun ( pc_get_lun(), 'ref_name'  , o%ref_name  )
      call prnfun ( pc_get_lun(), 'str_type'  , o%str_type  )
      call prnfun ( pc_get_lun(), 'opt_dir'   , o%opt_dir   )
      call prnfun ( pc_get_lun(), 'mig_type'  , o%mig_type  )
      call prnfun ( pc_get_lun(), 'all_out'   , o%all_out   )
      call prnfun ( pc_get_lun(), 'disk_buf'  , o%disk_buf  )
      call prnfun ( pc_get_lun(), 'par_io'    , o%par_io   )
      call prnfun ( pc_get_lun(), 'opt_obliquity', o%opt_obliquity )
      call prnfun ( pc_get_lun(), 'num_phase_table', o%num_phase_table )
      !
    end if    ! if ( pcpsx_i_pel() .eq. 0 ) then
    !
    ! set the amplitude power for migration and modeling
    !
    xxif_forward : if ( string_upper_compare ( o%opt_dir, 'FORWARD' ) ) then
      !
      o%amp_pwr_0 = + o%amp_pwr
      !
    else xxif_forward 
      !
      o%amp_pwr_0 = - o%amp_pwr
      !
    end if xxif_forward 
    !
    o%n_dimension  = max ( 2, min ( 3, o%n_dimension ) )
    !
    o%nx_tap = max ( 0, min ( o%nx_tap, o%nx_mig/4 ) )
    !
    o%ny_tap = max ( 0, min ( o%ny_tap, o%ny_mig/4 ) )
    !
    if ( o%n_dimension .eq. 2 .and. o%nx_mig .gt. 1 ) o%ny_fft = 1
    !
    if ( o%n_dimension .eq. 2 .and. o%ny_mig .gt. 1 ) o%nx_fft = 1
    !
    if ( o%n_dimension .eq. 2 ) o%disk_buf = '2D'
    !
    call pstmig_fft_length ( ( o%nx_mig-1 )*o%nx_trp+1, o%nx_fft )
    !
    call pstmig_fft_length ( ( o%ny_mig-1 )*o%ny_trp+1, o%ny_fft )
    !
    ! check to tmake sure the x,y header words are correct
    !
    call migfun_check_xy_header_words ( &
    'pstmig ', pc_get_lun(), o%hx_mig, o%hy_mig, i_err )
    !
    if ( i_err .ne. 0 ) go to 988
    !
    ! get the y scale distance coefficient
    !
    call migfun_get_scale ( &
    'pstmig x', pc_get_lun(), o%grid_obj, o%hx_mig, o%x0_scl, i_err )
    !
    if ( i_err .ne. 0 ) go to 987
    !
    ! get the y scale distance coefficient
    !
    call migfun_get_scale ( &
    'pstmig y', pc_get_lun(), o%grid_obj, o%hy_mig, o%y0_scl, i_err )
    !
    if ( i_err .ne. 0 ) go to 987
    !
    ! scale all x values by x0_scl
    !
    o%x0_mig = o%x0_mig * o%x0_scl
    !
    o%x1_mig = o%x1_mig * o%x0_scl
    !
    o%dx_mig = o%dx_mig * o%x0_scl
    !
    ! scale all y values by y0_scl
    !
    o%y0_mig = o%y0_mig * o%y0_scl
    !
    o%y1_mig = o%y1_mig * o%y0_scl
    !
    o%dy_mig = o%dy_mig * o%y0_scl
    !
    ! set a flag for stretching yes - i_stretch_0=0 no i_stretch_0 = 1
    ! get the stretch process numbers i_stretch_1 and i_stretch_2
    ! determine the optimal stretch time sample chracteristics
    !
    call pstmig_stretch_setup ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 991
    !
    ! setup the forward stretch
    !
    if ( o%i_stretch_0 .eq. 0 ) then
      !
      ! get the forward stretch process numbers
      !call getpn ( o%i_stretch_1 )
      ! setup forward stretch
      o%str_none  = 'NONE'
      o%str_mode  = 'FORWARD'
      o%str_order = -1
      !call stretch_internao%l_setup ( &
      !o%i_stretch_1, &
      !o%str_order, &
      !o%ns_inp, o%s0_inp, o%ds_inp, &
      !o%str_mode, o%ref_name, o%str_none, o%path_str, &
      !o%nz_str, o%rt_str, o%rv_str, &
      !*984 )
      !
    end if    ! if ( o%i_stretch_0 .eq. 0 ) then
    !
    ! for migration the input data is fft'd from time to frequency
    ! and o%nt_fft >= o%nt_inp
    !
    if ( string_upper_compare ( o%opt_dir, 'FORWARD' ) ) then
      !
      ! include the non zero time origin in the fft length
      !
      call pstmig_fft_length ( &
      o%ns_inp + max ( 0, nint ( o%s0_inp / o%ds_inp ) ), o%nt_fft )
      !
      o%t0_fft = o%s0_inp
      o%dt_fft = o%ds_inp
      !
      o%nt_tab = o%ns_out
      o%t0_tab = o%s0_out
      o%dt_tab = o%ds_out
      !
      ! for modeling the output data is fft'd from frequency to time
      ! and o%nt_fft >= o%ns_out
      !
    else    ! if ( string_upper_compare ( o%opt_dir, 'FORWARD' ) ) then
      !
      ! include the non zero time origin in the fft length
      !
      call pstmig_fft_length ( &
      o%ns_out + max ( 0, nint ( o%s0_out / o%ds_out ) ), o%nt_fft )
      !
      o%t0_fft = o%s0_out
      o%dt_fft = o%ds_out
      !
      o%nt_tab = o%ns_inp
      o%t0_tab = o%s0_inp
      o%dt_tab = o%ds_inp
      !
    end if    ! if ( string_upper_compare ( o%opt_dir, 'FORWARD' ) ) then
    !
    o%t1_fft  = ( o%nt_fft - 1 ) * o%dt_fft  + o%t0_fft
    !
    o%t1_tab  = ( o%nt_tab - 1 ) * o%dt_tab  + o%t0_tab
    !
    call matfun_line ( 'rs_inp', o%ns_inp, o%s0_inp, o%ds_inp, o%rs_inp, i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    call matfun_line ( 'rs_out', o%ns_out, o%s0_out, o%ds_out, o%rs_out, i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! for fk or cascaded migration we need the longer of input and output traces
    !
    if ( o%mig_type ( 1:2 ) .eq. 'FK' &
    .or. o%mig_type ( 1:8 ) .eq. 'CASCADED' ) &
    call pstmig_fft_length ( max ( &
    o%ns_inp+max ( 0, nint ( o%s0_inp/o%ds_inp ) ), &
    o%ns_out+max ( 0, nint ( o%s0_out/o%ds_out ) ) ), &
    o%nt_fft )
    o%dx_fft = o%dx_mig / o%nx_trp
    o%dy_fft = o%dy_mig / o%ny_trp
    o%nx_ff2 = o%nx_fft / 2 + 1
    !
    xxif_2d : if ( o%n_dimension .eq. 2 ) then
      !
      o%nx_til = 0
      o%ny_til = 0
      o%nt_til = 0
      !
    else xxif_2d 
      !
      o%nt_til = min ( o%nx_ff2, o%ny_mig, &
      nint ( sqrt ( float ( max ( o%nx_ff2, o%ny_fft ) ) ) ) )
      o%mx_til = max ( 1, ( o%nx_ff2+o%nt_til-1 )/o%nt_til )
      o%my_til = max ( 1, ( o%ny_mig+o%nt_til-1 )/o%nt_til )
      o%nx_til = matfun_rinw ( o%mx_til, o%nx_fil ) ! number of x tiles in 
      o%mx_til = o%nx_til * o%nx_fil          ! number of x tiles in obj
      o%ny_til = matfun_rinw ( o%my_til, o%ny_fil ) ! number of y tiles in 
      o%my_til = o%ny_til * o%ny_fil          ! number of y tiles in obj
      !
    end if xxif_2d 
    !
    o%nx_dsk = o%nx_til * o%nt_til  ! number of x values in a single fil
    o%ny_dsk = o%ny_til * o%nt_til  ! number of y values in a single fil
    o%mx_dsk = o%nx_dsk * o%nx_fil  ! total number of x values in o%nx
    o%my_dsk = o%ny_dsk * o%ny_fil  ! total number of y values in o%ny
    !
    o%rf_min = minval ( o%f1_lim ( 1:o%nt_lim ) )
    o%rf_max = maxval ( o%f2_lim ( 1:o%nt_lim ) )
    !
    o%nf_nyq = o%nt_fft / 2 + 1                     ! nyquist index
    o%rf_nyq = .5 / o%dt_fft                        ! nyquist frequency
    o%df_mig = o%rf_nyq / ( o%nf_nyq - 1 )        ! fft frequency i
    !
    rf_tmp = o%rf_min
    o%rf_min = nint ( o%rf_min / o%df_mig ) * o%df_mig ! put on a freq node
    if ( o%rf_min .lt. rf_tmp ) o%rf_min = o%rf_min + o%df_mig 
    !
    rf_tmp = o%rf_max
    o%rf_max = nint ( o%rf_max / o%df_mig ) * o%df_mig ! put on a freq node
    !
    if ( o%rf_max .gt. rf_tmp ) o%rf_max = o%rf_max - o%df_mig 
    o%f0_mig = o%rf_min                              ! first migration freq
    o%nf_mig = nint ( ( o%rf_max-o%rf_min )/o%df_mig ) + 1 ! number of freq
    o%f1_mig  = ( o%nf_mig  - 1 ) * o%df_mig  + o%f0_mig 
    !
    ! for fk or cascaded migration and modeling
    !
    if ( o%mig_type ( 1:2 ) .eq. 'FK' &
    .or. o%mig_type ( 1:8 ) .eq. 'CASCADED' ) then
      !
      o%nf_mig = o%nt_fft / 2 + 1
      o%f0_mig = 0.
      o%f1_mig  = ( o%nf_mig  - 1 ) * o%df_mig  + o%f0_mig 
      !
      o%rf_min = o%f0_mig
      o%rf_max = ( o%nf_mig - 1 ) * o%df_mig + o%f0_mig
      !
    end if    ! if ( o%mig_type ( 1:2 ) .eq. 'FK'
    !
    o%nw_mig = o%nf_mig 
    o%w0_mig = o%f0_mig * 2. * pi  ! first frequency in radians /sec
    o%dw_mig = o%df_mig * 2. * pi  ! frequency increment in radians / sec
    o%w1_mig  = ( o%nw_mig  - 1 ) * o%dw_mig  + o%w0_mig 
    !
    ! x_y and y_x aqio arrays
    ! inverse transform from kx_loc to x
    !
    o%mh_buf    = o%nh_inp * 2                       ! dimension of head b
    o%md_buf    = max ( o%nf_mig, o%ns_inp, o%ns_out ) * 2! dimension
    o%mt_buf    = o%mh_buf + o%md_buf              ! dimension of he
    o%nh_sav    = o%nh_inp                           ! dimension of head b
    o%nt_dsk    = o%mt_buf * o%nt_til              ! number pts in a
    o%nx_pel    = matfun_rinw ( o%nx_ff2, pcpsx_n_pel() ) ! # of x's on each
    o%ny_pel    = o%ny_mig                             ! # of y's on each
    o%nt_pel    = o%mt_buf                             ! # of packed sampl
    !
    ! keep the data ( w, kx, y ) on the pe's
    !
    if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then
      !
      o%n_xy_pel = o%nx_pel * o%ny_pel                 ! trace memory bu
      !
    else    ! if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then
      !
      o%n_xy_pel = 0                               ! trace memory buffer
      !
    end if    ! if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then
    !
    o%mt_lay   = 100                                       ! maximum no of v
    !
    ! get the stretch function sizes
    ! determine the size of the new migration velocity file
    !
    o%hx_str = o%hx_mig
    o%hy_str = o%hy_mig
    !
    call velgrid_size_p ( &
                            0, .true., &
                            pc_get_lun(), &
                            'pstmig migration velocity function', &
                            o%path_str, o%vel_type, &
                            o%n_xy_str, o%hx_str, o%hy_str, &
                            o%nx_str, o%x0_str, o%dx_str, &
                            o%ny_str, o%y0_str, o%dy_str, &
                              nt_tmp,   t0_tmp,   dt_tmp, &
                            i_err &
                          )
    !
    o%x1_str  = ( o%nx_str - 1 ) * o%dx_str  + o%x0_str 
    o%y1_str  = ( o%ny_str - 1 ) * o%dy_str  + o%y0_str 
    o%mx_buf = max ( o%nx_fft, o%ny_fft, o%nx_dsk, o%ny_dsk )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call headsave_create ( o%h, 'pstmig', o%nh_inp, i_err )
    if ( i_err .ne. 0 ) go to 998
    call memfun_all ( o%lu_x_y, o%nx_fil, o%ny_fil,    'lu_x_y', i_err )
    call memfun_all ( o%lu_y_x, o%ny_fil, o%nx_fil,    'lu_y_x', i_err )
    call memfun_all ( o%ix_hav, o%nx_mig,              'ix_hav', i_err )
    call memfun_all ( o%iy_hav, o%ny_mig,              'iy_hav', i_err )
    call memfun_all ( o%tr_buf, 2, o%mt_buf, o%mx_buf, 'tr_buf', i_err )
    call memfun_all ( o%tr_pel, o%nt_pel, o%nx_pel, o%ny_pel, 'tr_pel', i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( 'rf_mig', o%nf_mig, o%f0_mig, o%df_mig, o%rf_mig, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( 'rw_mig', o%nw_mig, o%w0_mig, o%dw_mig, o%rw_mig, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( 'vw_mig', o%nw_mig, o%w0_mig, o%dw_mig, o%vw_mig, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    o%vw_mig = 1. / o%rw_mig 
    !
    ! get the memory sum counter
    !
    call memfun_sum_get ( o%n_sto )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " Phase Shift initialization results ", &
    & /, " pstmig memory utilization ", &
    & /, " n_sto   =", i10 &
    & )' ) &
    o%n_sto
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " number of pes=", i8, &
    & /, " i_stretch_0=", i4, &
    & " i_stretch_1=", i4, " i_stretch_2=", i4, &
    & /, " nx_pel=", i8, " ny_pel=", i8, " nt_pel=", i8, &
    & /, " nf_mig=", i10, " df_mig=", g12.6, &
    & /, " f0_mig=", f10.2, " f1_mig=", f10.2, &
    & /, " rf_min =", f10.2, " rf_max =", f10.2, &
    & /, " pstmig_tw_fft=", i10, " pstmig_xk_fft=", i10, &
    & /, " ho_mig=", i2, " o0_mig=", f10.2, " do_mig=", f10.2, &
    & /, " hx_mig=", i2, " nx_fft=", i8, " nx_tap=", i3, &
    & " x0_mig=", f10.2, " dx_mig=", g12.6, &
    & /, " hy_mig=", i2, " ny_fft=", i8, " ny_tap=", i3, &
    & " y0_mig=", f10.2, " dy_mig=", g12.6 &
    & )' ) &
    pcpsx_n_pel(), &
    o%i_stretch_0, o%i_stretch_1, o%i_stretch_2, &
    o%nx_pel, o%ny_pel, o%nt_pel, &
    o%nf_mig, o%df_mig, o%f0_mig, &
    o%f0_mig+ ( o%nf_mig-1 )*o%df_mig, &
    o%rf_min, o%rf_max, &
    pstmig_tw_fft(), pstmig_xk_fft(), &
    o%ho_mig, o%o0_mig, o%do_mig, &
    o%hx_mig, o%nx_fft, o%nx_tap, o%x0_mig, o%dx_mig, &
    o%hy_mig, o%ny_fft, o%ny_tap, o%y0_mig, o%dy_mig
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, "  length of head  buffer=", i8, &
    & /, "  length of data  buffer=", i8, &
    & /, "  length of trace buffer=", i8, &
    & /, " nx_fil=", i8, " ny_fil=", i8, &
    & /, " mx_dsk=", i8, " my_dsk=", i8, &
    & /, " nx_dsk=", i8, " ny_dsk=", i8, " nt_dsk=", i8, &
    & /, " nx_til=", i8, " ny_til=", i8, " nt_til=", i8, &
    & /, " nt_inp=", i8, " t0_inp=", g12.6, " t1_inp=", g12.6, &
    & " dt_inp=", g12.6, &
    & /, " nt_mig=", i8, " t0_mig=", g12.6, " t1_mig=", g12.6, &
    & " dt_mig=", g12.6, &
    & /, " nt_fft=", i8, " t0_fft=", g12.6, " t1_fft=", g12.6, &
    & " dt_fft=", g12.6, &
    & /, " nt_tab=", i8, " t0_tab=", g12.6, " t1_tab=", g12.6, &
    & " dt_tab=", g12.6, &
    & /, " nt_st1=", i8, " t0_st1=", g12.6, " t1_st1=", g12.6, &
    & " dt_st1=", g12.6, &
    & /, " nt_st2=", i8, " t0_st2=", g12.6, " t2_st1=", g12.6, &
    & " dt_st2=", g12.6 &
    & )' ) &
    o%mh_buf, o%md_buf, o%mt_buf, &
    o%nx_fil, o%ny_fil, &
    o%mx_dsk, o%my_dsk, &
    o%nx_dsk, o%ny_dsk, o%nt_dsk, &
    o%nx_til, o%ny_til, o%nt_til, &
    o%nt_inp, o%t0_inp, o%t1_inp, o%dt_inp, &
    o%nt_mig, o%t0_mig, o%t1_mig, o%dt_mig, &
    o%nt_fft, o%t0_fft, o%t1_fft, o%dt_fft, &
    o%nt_tab, o%t0_tab, o%t1_tab, o%dt_tab, &
    o%ns_inp, o%s0_inp, o%s1_inp, o%ds_inp, &
    o%ns_out, o%s0_out, o%s1_out, o%ds_out
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & " pstmig disk space for one file  in gigabytes =", f12.6, &
    & /, " pstmig disk space for all files in gigabytes =", f12.6&
    & )' ) &
    ( float ( o%nt_dsk )*float ( o%nx_til )*float ( o%ny_dsk )/2. )*8./1.e9, &
    ( float ( o%nx_fil*o%ny_fil ) &
    * float ( o%nt_dsk )*float ( o%nx_til )*float ( o%ny_dsk )/2. )*8./1.e9
    !
    ! read the velocity model
    !
    call pstmig_read_velocity ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 992
    !
    ! compute time - frequency limits
    !
    call pstmig_if_lim_compute ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 2994
    !
    ! compute the complex phase table - o%ac_exp
    ! for migration - s_phase = + 1
    ! for modeling  - s_phase = - 1
    !      o%dc_exp = 2. * pi / o%nc_exp
    !      phase = float ( s_phase ) * ( ic_exp  - 1 ) * o%dc_exp
    !      o%ac_exp ( ic_exp  ) = cmplx ( cos ( phase ), sin ( phase ) )
    !
    call pstmig_ac_exp_compute ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 1994
    !
    ! compute a 2d phase shift table
    ! the 2 dimension are wavenumber / frequency and vertical time
    ! the phase shift for any particular frequenc ,ywavenumber and time
    ! will be obtained by linear interpolation from this table
    ! rv_ave = average slowness between two vertical times
    ! rp_loc = wavenumber / frequency ratio
    ! max rp_loc = max slowness ,dp_tab = max slowness / np_tab
    !          tim_tab ( it_tab, ip_tab ) = tim_tab ( jt_tab, ip_tab ) &
    !        + dt_tab * sqrt ( 1 - min ( 1., rp_loc**2*o%rv_ave ) )
    !
    call pstmig_table_compute ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 3994
    !
    ! compute the fk sinc table
    !
    call pstmig_fk_snc_compute ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 994
    !
    o%i_pass   = 1    ! processing flag 1=input ,2=migrate ,3=output ,4=done
    o%i_trin   = 0    ! number of input traces this group
    o%j_trin   = 0    ! number of input traces this group
    o%n_trin   = 0    ! number of input traces in filled group
    o%m_trin   = 0    ! total number of input traces in filled group
    o%l_trin   = 0    ! total number of input traces saved
    !
    o%i_trot   = 0    ! number of output traces this group
    o%j_trot   = 0    ! number of output traces this group
    o%n_trot   = 0    ! number of output traces in filled group
    o%m_trot   = 0    ! number of output groups
    o%l_trot   = 0    ! total number of output traces
    !
    o%n_online = 16   ! how often to send report files
    o%no_grp   = 0    ! which input group
    o%io_grp   = 0    ! which input group
    o%jo_grp   = 0    ! which input group
    o%nx_mem   = 0    ! how many traces in memory line
    o%iy_mem   = 0    ! which line is in memory
    !
    ! get the disk file unit number ,open the file and initialize it to zero
    ! nx_dsk = number of x locations on disk
    ! ny_dsk = number of y locations on disk
    ! data   = trace array
    !
    if ( o%path_mig ( 1:4 ) .eq. 'NONE' ) o%path_mig = 'pstmig.mig'
    !
    o%ipn = pc_get_ipn()
    !
    o%fn_sav = trim ( o%path_mig )//'_save'
    !
    o%fn_x_y = trim ( o%path_mig )//'_pxy_'
    !
    o%fn_y_x = trim ( o%path_mig )//'_pyx_'
    !
    call pario_name_1 ( o%fn_x_y, o%fn_x_y, o%ipn )
    !
    call pario_name_1 ( o%fn_y_x, o%fn_y_x, o%ipn )
    !
    ! for 3d data and disk buffer open the x_y image disk file
    ! o%nx_til*o%ny_dsk = o%nx_dsk*o%ny_til = o%nx_til*o%ny_til*o%nt_
    ! for DISK_BUF=OUTPUT this file will be opened 
    ! old in pstmig_transpose_disk_1 in pstmig_image
    !
    i_err = 0
    !
    ! for the yes and input options open the x, y files here
    !
    xxif_yes_or_input : &
    if ( string_upper_compare ( o%disk_buf, 'YES' ) &
    .or. string_upper_compare ( o%disk_buf, 'INPUT' ) ) then
      !
      call pstmig_open_disk_n ( &
                                  o%ipn, o%ny_fil, o%nx_fil, &
                                  o%lu_x_y, o%fn_x_y, 0, &
                                  o%nx_til, o%ny_dsk, o%nt_dsk, &
                                  i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
    end if xxif_yes_or_input 
    !
    ! for the output and migrate options open the y, x files here
    ! for other options this will be opened by 
    ! the first call to pstmig_transpose_d
    !
    xxif_output_or_forward : &
    if ( string_upper_compare ( o%disk_buf, 'OUTPUT' ) &
    .or. string_upper_compare ( o%disk_buf, 'FORWARD' ) ) then
      !
      call pstmig_open_disk_n ( &
                                o%ipn, o%ny_fil, o%nx_fil, &
                                o%lu_y_x, o%fn_y_x, -1, &
                                o%ny_til, o%nx_dsk, o%nt_dsk, &
                                i_err &
                              )
      !
      if ( i_err .ne. 0 ) go to 995
      !
    end if xxif_output_or_forward 
    !
    ! open the saved trace disk file
    ! for the first pe open as a new file
    ! open the save file on pe 0
    ! open the file on pe 0 with broadcast
    !
    call pario_open_2 ( &
    0, .true., o%lu_sav, o%fn_sav, 'w+', o%nh_inp, o%ns_inp, i_err )
    !
    ! initalize the file to zero
    ! use pe 0 and broadcast to other pes
    !
    if ( i_err .eq. 0 ) &
    call pario_zero_2 ( 0, .true., o%lu_sav, 1, o%nh_inp, o%ns_inp, i_err )
    !
    call pcpsx_broadcast ( 0, i_err )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    call pcpsx_broadcast ( 0, o%lu_sav )
    !
    ! set globals from output values ( o%nt_mig etc. )
    !
    call timeglob_put ( o%nt_mig, o%t0_mig, o%dt_mig )
    !
    ! setup the inverse stretch
    !
    xxif_stretch : if ( o%i_stretch_0 .eq. 0 ) then
      !
      ! get the inverse stretch process numbers
      !
      !call getpn ( o%i_stretch_2 )
      !
      ! setup inverse stretch
      !
      o%str_none  = 'NONE'
      o%str_mode  = 'INVERSE'
      o%str_order = -1
      !
      !call stretch_internal_setup ( &
      !o%i_stretch_2, &
      !o%str_order, &
      !o%nt_mig, o%t0_mig, o%dt_mig, &
      !o%str_mode, o%ref_name, o%str_none, o%path_str, &
      !o%nz_str, o%rt_str, o%rv_str, &
      !*984 )
      !
    end if xxif_stretch 
    !
    ! dim of automatic arrays
    !
    o%nt_dim  = max ( o%ns_inp, o%ns_out, o%nt_tab, o%nt_fft )
    !
    ! scale factor to account for time fft, spatial ffts are scaled in mcfft
    ! for fk or cascaded migration and modeling
    !
    xxif_fk_or_casacded : if ( o%mig_type ( 1:2 ) .eq. 'FK' &
                          .or. o%mig_type ( 1:8 ) .eq. 'CASCADED' ) then
      !
      ! fk and cascaded have two ffts
      !
      o%scale_fft = sqrt ( 1. / float ( o%nt_fft ) )
      !
    else xxif_fk_or_casacded 
      !
      ! phase shift has a single ffts
      !
      o%scale_fft = 1. / float ( o%nt_fft )
      !
    end if xxif_fk_or_casacded 
    !
    ! initialize the fft tables
    ! create the time to frequency fft object o%fft_obj_tw
    !
    i_err =  fft_create ( o%fft_obj_tw, +pstmig_tw_fft(), o%nt_fft, 'ctoc' )
    !
    if ( i_err .ne. 0 ) go to 986
    !
    ! create the frequency to time fft object o%fft_obj_wt
    !
    i_err =  fft_create ( o%fft_obj_wt, -pstmig_tw_fft(), o%nt_fft, 'ctoc' )
    !
    if ( i_err .ne. 0 ) go to 985
    !
    ! compute the x line wavenumber increment
    !
    o%ix_fft = 1
    !
    o%iy_fft = 1
    !
    !call pstmig_compute_k2 ( &
    !                         o%ix_fft, o%nx_fft, o%dx_fft, o%kx_loc, &
    !                         o%iy_fft, o%ny_fft, o%dy_fft, o%ky_loc, &
    !                         o%rk_loc &
    !                       )
    !
    o%pstmig_one_y = .false.
    !
    if ( o%ny_fft .le. 1 ) o%pstmig_one_y = .true.
    !
    xxif_one_y : if ( o%pstmig_one_y ) then
      !
      o%n1_fft = o%nx_fft 
      o%d1_fft = o%dx_fft 
      !
      o%n2_fft = o%ny_fft 
      o%d2_fft = o%dy_fft 
      !
    else xxif_one_y 
      !
      o%n1_fft = o%ny_fft 
      o%d1_fft = o%dy_fft 
      !
      o%n2_fft = o%nx_fft 
      o%d2_fft = o%dx_fft 
      !
    end if xxif_one_y 
    !
    write ( pc_get_lun(), ' ( &
    & /, " normal return pstmig_parameter_prep pe=", i8 &
    & )' ) &
    pcpsx_i_pel()
    !
    return
    !
985 continue
    !
    write ( pc_get_lun(),' ( &
    & /, " error in pstmig_parameter_prep pe=", i8, &
    & /, " during frequency to time fft_create " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 985 error ' )
    !
    go to 999
    !
986 continue
    !
    write ( pc_get_lun(),' ( &
    & /, " error in pstmig_parameter_prep pe=", i8, &
    & /, " during time to frequency fft_create " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 986 error ' )
    !
    go to 999
    !
987 continue
    !
    write ( pc_get_lun(),' ( &
    & /, " error in pstmig_parameter_prep pe=", i8, &
    & /, " during migfun_get_scale ", &
    & /, " hx_mig=", i8, " hy_mig=", i8 &
    & )' ) &
    pcpsx_i_pel(), o%hx_mig, o%hy_mig
    !
    call pc_error ( 'pstmig_parameter_prep 987 error ' )
    !
    go to 999
    !
988 continue
    !
    write ( pc_get_lun(),' ( &
    & /, " error in pstmig_parameter_prep pe=", i8, &
    & /, " during migfun_check_xy_header_words " &
    & /, " hx_mig=", i8, " hy_mig=", i8 &
    & )' ) &
    pcpsx_i_pel(), o%hx_mig, o%hy_mig
    !
    call pc_error ( 'pstmig_parameter_prep 988 error ' )
    !
    go to 999
    !
989 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during set character strings " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 989 error ' )
    !
    go to 999
    !
990 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " nx_mig<0 nx_mig=", i8 &
    & )' ) &
    pcpsx_i_pel(), &
    o%nx_mig
    !
    call pc_error ( 'pstmig_parameter_prep 990 error ' )
    !
    go to 999
    !
991 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during pstmig_stretch_setup " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 991 error ' )
    !
    go to 999
    !
992 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during pstmig_read_velocity " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 992 error ' )
    !
    go to 999
    !
993 continue
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during pstmig_zero_file " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 993 error ' )
    !
    go to 999
    !
3994 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during pstmig_ac_exp_compute " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 3994 error ' )
    !
    go to 999
    !
2994 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during pstmig_if_lim_compute " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 2994 error ' )
    !
    go to 999
    !
1994 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during pstmig_table_compute " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 1994 error ' )
    !
    go to 999
    !
994 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during pstmig_fk_snc_compute " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 994 error ' )
    !
    go to 999
    !
995 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during pstmig_open_disk_n " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 995 error ' )
    !
    go to 999
    !
996 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during pario_open_2 ", &
    & /, " for save file open " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 996 error ' )
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep pe=" ,i8, &
    & /, " during velgrid_size " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( 'pstmig_parameter_prep 998 error ' )
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_parameter_prep " &
    & )' )
    !
    call pc_error ( 'pstmig_parameter_prep error ' )
    !
    return
    !
  end subroutine pstmig_parameter_prep
  !
  subroutine pstmig_input_trap ( )
    !
    return
    !
  end subroutine pstmig_input_trap
  !
  subroutine pstmig ( o, n_inp, hd_inp, tr_inp )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    integer,             intent(inout) :: n_inp           ! num traces
    double precision,    intent(inout) :: hd_inp ( :, : ) ! headers
    real,                intent(inout) :: tr_inp ( :, : ) ! traces
    !
    integer                            :: i_err

    !
    call cpucount ( o%c, o%c_pstmig_total, 1 )
    !
    o%mh_inp = size ( hd_inp, 1 )
    o%mt_inp = size ( tr_inp, 1 )
    !
    i_err = 0
    !
    ! apply the forward stretch
    !
    xxif_stretch : if ( o%i_stretch_0 .eq. 0 .and. o%i_pass .eq. 1 ) then
      !
      !call stretch ( o%i_stretch_1, n_inp, hd_inp, tr_inp )
      !
      if ( n_inp .lt. no_more_traces ) go to 998
      !
    end if xxif_stretch 
    !
    ! if pstmig_output is done outputting traces
    ! come back to pstmig_get_status to figure out the next step
    !
  1 continue
    !
    call pstmig_get_status ( o, n_inp, hd_inp, tr_inp, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! data input
    !
    xxif_input : if ( o%inp_flag ) then
      !
      call pstmig_input ( o, n_inp, hd_inp, tr_inp, o%tr_buf, i_err )
      !
      if ( i_err .ne. 0 ) go to 996
      !
    end if xxif_input 
    !
    ! data x - kx_loc transform and migration
    !
    xxif_mig_flag : if ( o%mig_flag ) then
      !
      call pstmig_image ( o, o%tr_buf, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
      ! if pstmig is stopping set the appropriate flags
      !
      xxif_pstmig_done : if ( o%done_flag ) then
        !
        o%out_flag = .false.
        !
        o%i_pass = 4
        !
        n_inp = no_more_traces
        !
      end if xxif_pstmig_done 
      !
    end if xxif_mig_flag 
    !
    ! data output
    !
    xxif_out_flag : if ( o%out_flag ) then
      !
      call pstmig_output ( o, n_inp, hd_inp, tr_inp, o%tr_buf, i_err )
      !
      if ( i_err .ne. 0 ) go to 994
      !
      ! if pstmig_output is done outputting traces
      ! go back to pstmig_get_status to figure out the next step
      !
      if ( n_inp .eq. no_more_traces ) go to 1
      !
    end if xxif_out_flag 
    !
    call cpucount ( o%c, o%c_pstmig_total, 2 )
    !
    xxif_i_pass_1 : if ( o%i_pass .eq. 1 ) then
      !
      n_inp = need_traces
     !
   else if ( o%i_pass .eq. 2 ) then   ! return below for data output
     !
     ! apply the inverse stretch
     !
     xxif_stretch_2 :  if ( o%i_stretch_0 .eq. 0 ) then
        !
        !call stretch ( o%i_stretch_2, n_inp, hd_inp, tr_inp )
        !
        if ( n_inp .lt. no_more_traces ) go to 998
        !
      end if xxif_stretch_2 
      !
    else xxif_i_pass_1 
      !
      n_inp = no_more_traces
      !
    end if xxif_i_pass_1 
    !
1999 continue
    !
    if ( n_inp .eq. NO_MORE_TRACES .or. n_inp .eq. FATAL_ERROR ) &
    !
    call pstmig_wrapup ( o )
    !
    return
    !
994 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig pe=" ,i8, &
    & /, " during pstmig_output " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
995 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig pe=" ,i8, &
    & /, " during pstmig_image " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
996 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig pe=" ,i8, &
    & /, " during pstmig_input " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig pe=" ,i8, &
    & /, " during pstmig_get_status " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig pe=" ,i8, &
    & /, " during stretch " &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( ' error in pstmig ' )
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig pe=" ,i8 &
    & )' ) &
    pcpsx_i_pel()
    !
    call pc_error ( ' error in pstmig ' )
    !
    !call pcpsx_flush_file ( pc_get_lun() )
    !
    n_inp = fatal_error
    !
    go to 1999
    !
  end subroutine pstmig
  !
  subroutine pstmig_wrapup ( o )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    if ( o%skip_wrapup ) return
    !
    o%skip_wrapup = .true.
    !
    return
    !
  end subroutine pstmig_wrapup
  !
  subroutine pstmig_get_status ( o, n_inp, hd_inp, tr_inp, i_err )
    !
    ! set the operation flag, i_pass, indicating what whould be done next
    ! 1=input, 2=migrate, 3=output, 4=done
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: n_inp      ! num input traces
    double precision,    intent(inout) :: hd_inp ( :, : ) ! input headers
    real,                intent(inout) :: tr_inp ( :, : ) ! input traces
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! Local variables
    !
    integer                            :: i_inp
    integer                            :: i_next

    logical                            :: remove_flag
    real                               :: o0_inp
    !
    o%inp_flag  = .false. ! input traces     
    !
    o%mig_flag  = .false. ! migrate traces   
    !
    o%out_flag  = .false. ! output traces    
    !
    remove_flag = .false.! remove files     
    !
    if ( o%i_pass .eq. 1 &
    .and. ( o%disk_buf ( 1:7 ) .eq. 'FORWARD' &
      .or. o%disk_buf ( 1:6 ) .eq. 'OUTPUT' ) ) then
      !
      o%io_grp = 1
      !
      o%n_trin = o%nx_mig * o%ny_mig
      !
      n_inp = no_more_traces
      !
    end if    ! if ( disk_buf ( 1:7 ) .eq. 'FORWARD'
    !
  1 continue
    !
    i_err  = 0
    !
    ! inputing data
    !
    if ( o%i_pass .eq. 1 ) then
      !
      o%l_trin = 0
      !
      i_next = 0
      !
      o%jo_grp = 0
      !
      ! check each input trace to see if it is part of a new group
      ! if so save the remaining traces on disk
      !
      do_input_traces : do i_inp = 1 , n_inp
        !
        ! if the group header is nonzero set the current group value, jo_grp
        ! compute the offset group bin for this input trace, io_inp
        !
        if ( o%ho_mig .ne. 0 ) &
        call migfun_trace_location ( &
        hd_inp ( o%ho_mig, i_inp ), o%o0_mig, o%do_mig, 1., o%jo_grp, o0_inp )
        !
        ! if this is the first input trace 
        ! initalize the in memory group value., io_grp
        !
        if ( o%n_trin .eq. 0 .and. i_inp .eq. 1 ) o%io_grp = o%jo_grp
        !
        !write ( pc_get_lun(), ' ( &
        !& /, " pstmig_get_status i_trin=", i8, " i_inp=", i8, &
        !& " jo_grp=", i8, " io_grp=", i8, " h7=", f10.2&
        !& )' ) &
        !o%n_trin+i_inp, i_inp, o%jo_grp, o%io_grp, hd_inp ( 7, i_inp )
        !
        ! if this trace is part of a new group 
        ! we set the next group counter, i_next and drop down to 2
        ! There any extra traces will be written to disk
        ! and we will process the traces from the current group
        !
        xxif_new_group : if ( o%jo_grp .ne. o%io_grp ) then
          !
          i_next = i_inp
          !
          go to 2
          !
        end if xxif_new_group 
        !
      end do do_input_traces 
      !
  2   continue
      !
      ! if this is a new group or it is the end of data input...
      !
      xxif_new_or_end : &
      if ( i_next .gt. 0 .or. n_inp .eq. no_more_traces ) then
        !
        ! set the number of input trace from the next group
        !
        o%l_trin = n_inp - i_next + 1
        !
        if ( n_inp .eq. no_more_traces ) o%l_trin = 0
        !
        ! if this is a 3 stage migration we can only handle a single group
        !
        if ( o%disk_buf ( 1:5 ) .eq. 'INPUT' &
          .or. o%disk_buf ( 1:7 ) .eq. 'FORWARD' &
          .or. o%disk_buf ( 1:6 ) .eq. 'OUTPUT' ) o%l_trin = 0
        !
        n_inp  = i_next - 1
        !
        o%no_grp = o%no_grp + 1
        !
        ! write the rest of the input traces to disk
        ! use pe 0 and broadcast to other pes
        !
        call pario_write_2 ( &
                             0, .true., o%lu_sav, &
                             o%l_trin, 1, 1, &
                             o%mh_inp, o%nh_inp, hd_inp ( :, i_next ), &
                             o%mt_inp, o%nt_inp, tr_inp ( :, i_next ), &
                             i_err &
                           )
        !
        if ( i_err .ne. 0 ) go to 998
        !
        ! set information for outputting the currect group of traces
        !
        o%j_trot = 0                        ! current output trace counter
        !
        o%l_trot = 0                        ! current output trace counter
        !
        xxif_all_out : if ( o%all_out ) then
          !
          o%n_trot = o%nx_mig * o%ny_mig    ! number of output traces in group
          !
        else xxif_all_out 
          !
          o%n_trot = min ( o%nx_mig*o%ny_mig, o%n_trin )! num of out trc in grp
          !
        end if xxif_all_out 
        !
        o%m_trot   = o%m_trot + o%n_trot ! total number of output traces
        !
        xxif_first_trace : if ( o%n_trin .gt. 0 ) then
          !
          o%mig_flag = .true.    ! migrate the current group
          !
          o%out_flag = .true.    ! output the next trace
          !
          o%i_pass   = 2         ! migrate flag
          !
        else xxif_first_trace 
          !
          if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
          & /, " pstmig_get_status warning ", &
          & " there are no input traces for this group " &
          & )' )
          !
          o%i_pass   = 3    ! all done
          !
          remove_flag   = .true.
          !
        end if xxif_first_trace 
        !
        call prnfun_clock ( pc_get_lun(), ' pstmig end of group' )
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        write ( pc_get_lun(), ' ( &
        & /, " pstmig_get_status has found the end of a group ", &
        & /, " no_grp=", i8, " jo_grp=", i8, " io_grp=", i8, &
        & /, " n_inp =", i8, " l_trin=", i8, &
        & /, " i_trin=", i8, " j_trin=", i8, &
        & " n_trin=", i8, " m_trin=", i8, &
        & /, " i_trot=", i8, " j_trot=", i8, &
        & " n_trot=", i8, " m_trot=", i8 &
        & )' ) &
        o%no_grp, o%jo_grp, o%io_grp, &
        n_inp+o%l_trin, o%l_trin, &
        o%i_trin, o%j_trin, o%n_trin, o%m_trin, &
        o%i_trot, o%j_trot, o%n_trot, o%m_trot
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        write ( pc_get_lun(), ' ( &
        & /, " number of groups    =", i8, &
        & /, " old group number    =", i8, &
        & /, " new group number    =", i8, &
        & /, " total input         =", i8, &
        & /, " total input in group=", i8, &
        & /, " total saved         =", i8, &
        & /, " total saved in group=", i8 &
        & )' ) &
        o%no_grp, o%io_grp, o%jo_grp, o%i_trin, o%j_trin, o%m_trin, o%n_trin
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        write ( pc_get_lun(), ' ( &
        &   " total number of traces output         =", i8, &
        & /, " total number of traces output in group=", i8 &
        & )' ) &
        o%m_trot, o%n_trot
        !
        ! print the current group of input header words
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        call headsave_print ( o%h, ' end of group ', 1 )
        !
        ! for 3d data send back a report file
        !
        !if ( n_dimension .eq. 3 ) &
        !call report_online ( ' end of group', ' ', 'pstmig' )
        !
      end if xxif_new_or_end 
      !
      if ( n_inp .ne. no_more_traces ) o%inp_flag = .true.
      !
      ! outputting data
      !
    else if ( o%i_pass .eq. 2 ) then
      !
      ! we have more output traces output the next one
      !
      if ( o%j_trot .lt. o%n_trot ) then
        !
        o%out_flag = .true.
        !
        ! all traces have been output from the current group
        ! reset the memory counters 
        ! and read the extra trace from disk back into memory
        ! the go back to the top
        !
      else    ! if ( j_trot .lt. n_trot ) then
        !
        ! reset line memory counter
        !
        o%nx_mem = 0
        !
        o%iy_mem = 0
        !
        ! print the current group of output header words
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        call headsave_print ( o%h, ' pstmig_get_status ', 9 )
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        call cpucount_print ( o%c, ' pstmig_get_status ' )
        !
        ! read the saved input traces from disk back into hd_inp, tr_inp
        ! use pe 0 and broadcast to other pes
        !
          call pario_read_2 ( &
                              0, .true., o%lu_sav, &
                              o%l_trin, 1, 1, &
                              o%mh_inp, o%nh_inp, hd_inp ( :, : ), &
                              o%mt_inp, o%nt_inp, tr_inp ( :, : ), &
                              i_err &
                            )
        !
        if ( i_err .ne. 0 ) go to 997
        !
        ! reset the trace counters
        !
        n_inp    = o%l_trin    ! number of input traces
        !
        o%l_trin = 0         ! number of extra traces
        !
        o%j_trin = 0         ! trace in group counter
        !
        o%n_trin = 0         ! number of traces in group
        !
        o%io_grp = o%jo_grp    ! current group in memory flag
        !
        ! if the number of input traces is 0 we are all done
        !
        if ( n_inp .eq. no_more_traces ) then
          !
          o%i_pass   = 3    ! all done
          !
          remove_flag   = .true. ! remove files     0 = no 1 = yes
          !
          ! otherwise we are ready for data input of the next group
          !
        else    ! if ( n_inp .eq. no_more_traces ) then
          !
          o%i_pass = 1
          !
          ! zero the the image disk
          ! nx_til*ny_dsk = nx_dsk*ny_til = 
          ! nx_til*ny_til*nt_til records each nt_dsk long
          !
          if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
          .or. o%disk_buf ( 1:5 ) .eq. 'INPUT' ) then
            !
            ! open the x, y disk files again, zero them
            !
            call pstmig_open_disk_n ( &
                                      o%ipn, o%nx_fil, o%ny_fil, &
                                      o%lu_x_y, o%fn_x_y, 0, &
                                      o%nx_til, o%ny_dsk, o%nt_dsk, &
                                      i_err &
                                    )
            !
            if ( i_err .ne. 0 ) go to 996
            !
          else if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then 
            !
            ! zero the memory buffer
            !
            o%tr_pel = 0.
            !
          end if    ! if ( disk_buf ( 1:3 ) .eq. 'YES'
          !
        end if    ! if ( n_inp .eq. no_more_traces ) then
        !
        go to 1
        !
      end if    ! if ( j_trot .lt. n_trot ) then
      !
    end if    ! if ( i_pass .eq. 1 ) then
    !
    ! remove files     
    !
    if ( remove_flag ) then
      !
      ! remove the y_x data file, unless we need to save it for output
      ! The x, y file is removed by the second call to pstmig_transpose_disk_n
      !
      if ( o%disk_buf ( 1:7 ) .ne. 'FORWARD' ) &
      call pstmig_remove_n ( o%ny_fil, o%nx_fil, o%lu_y_x, o%fn_y_x )
      !
      ! Remove the output x, y file
      !
      if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
      .or. o%disk_buf ( 1:6 ) .eq. 'OUTPUT' ) &
      call pstmig_remove_n ( o%nx_fil, o%ny_fil, o%lu_x_y, o%fn_x_y )
      !
      ! close and remove the save data file on pe 0 broadcast to other pes
      !
      call pario_close ( 0, .true., o%lu_sav, .true., i_err )
      !
      i_err = 0
      !
    end if    ! 
    !
    if ( o%i_pass .eq. 3 ) n_inp = no_more_traces
    !
    return
    !
996 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( " error in pstmig_get_status ", &
    & /, " during pstmig_open_disk_n " &
    & )' )
    !
    go to 999
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & " error in pstmig_get_status during read " &
    & )' )
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & " error in pstmig_get_status during write " &
    & )' )
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( " error in pstmig_get_status " &
    & )' )
    o%i_pass = 4    ! all done
    !
    i_err = i_err + 1
    !
    return
    !
  end subroutine pstmig_get_status
  !
  subroutine pstmig_input ( o, n_inp, hd_inp, tr_inp, tr_buf, i_err )
    !
    ! input the next trace
    ! if n_inp is zero set the processing flag to the next step
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: n_inp      ! num input traces
    double precision,    intent(inout) :: hd_inp ( :, : ) ! input headers
    real,                intent(inout) :: tr_inp ( :, : ) ! input traces
    real,                intent(inout) :: tr_buf ( 2, o%mt_buf/2, * )
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! Local variables
    integer                            :: i_inp

    real                               :: a_scale
    !
    i_err  = 0
    !
    ! check each input trace and put into buffer
    !
    do_input_traces : do i_inp = 1 , n_inp
      !
      o%i_trin = o%i_trin + 1    ! input trace counter total
      !
      o%j_trin = o%j_trin + 1    ! input trace counter in group
      !
      ! save headers for input traces ( locations 1 - 4 )
      !
      call headsave_store ( o%h, o%j_trin, 1, hd_inp ( :, i_inp ) )
      !
      ! get the trace x location
      !
      call migfun_trace_location ( &
                              hd_inp ( o%hx_mig, i_inp ), o%x0_mig, o%dx_mig, &
                              o%x0_scl, o%ix_inp, o%rx_inp &
                                 )
      !
      ! get the input trace y location
      !
      call migfun_trace_location ( &
                              hd_inp ( o%hy_mig, i_inp ), o%y0_mig, o%dy_mig, &
                              o%y0_scl, o%iy_inp, o%ry_inp &
                                 )
      !
      ! get the ray parameter
      !
      if ( o%pstmig_pre_stack ) &
      o%rp_off = hd_inp ( hdr_offset, i_inp ) * 1.e-6
      !
      o%ro_grp = hd_inp ( hdr_offset, i_inp )
      !
      ! apply the fold of stack scaling
      !
      a_scale = max ( 1., real ( hd_inp ( 5, i_inp ) ) ) ** o%fswp
      !
      tr_inp ( 1:o%nt_inp, i_inp ) = tr_inp ( 1:o%nt_inp, i_inp ) * a_scale
      !
      !print' ( " input i_trin=", i8, " n_trin=", i8, &
      !& " ix_inp=", i8, " iy_inp=", i8, " h64=", g12.6, " tr=", g12.6 )', &
      !o%i_trin, o%n_trin, o%ix_inp, o%iy_inp, hd_inp ( 64, i_inp ),&
      !matfun_amax ( o%nt_inp, tr_inp ( :, i_inp ) ) 
      !
      !!print*,' hx=', hd_inp ( o%hx_mig, i_inp ) , &
      !' rx_inp=',o%rx_inp,' x0_mig=',o%x0_mig,' dx_mig=',o%dx_mig
      !
      !!print*,' hy=', hd_inp ( o%hy_mig, i_inp ) , &
      !' ry_inp=',o%ry_inp,' y0_mig=',o%y0_mig,' dy_mig=',o%dy_mig
      !
      ! if this trace falls within the image volume keep and migrate it
      !
      if ( o%ix_inp .ge. 1 .and. o%ix_inp .le. o%nx_mig &
     .and. o%iy_inp .ge. 1 .and. o%iy_inp .le. o%ny_mig ) then
        !
        o%n_trin = o%n_trin + 1               ! saved trace number in group
        o%m_trin = o%m_trin + 1               ! saved trace number total
        !
        ! reapply mute
        !
        call migfun_mute_apply ( &
                                 o%mh_inp, hd_inp ( :, i_inp ), &
                                 o%nt_inp, tr_inp ( :, i_inp ) &
                               )
        !
        ! save headers for saved traces ( locations 5 - 8 )
        !
        call headsave_store ( o%h, o%n_trin, 5, hd_inp ( :, i_inp ) )
        !
        ! set the input header word
        hd_inp ( 1, i_inp ) = ( o%iy_inp - 1 ) * o%nx_mig + o%ix_inp
        !
        ! if this is a new y line in memory set some counters
        !
        if ( o%iy_mem .eq. 0 ) then
          !
          ! set the line in memory flag
          !
          o%iy_mem = o%iy_inp
          !
          ! clear the ix_hav buffer
          !
          o%ix_hav ( 1:o%nx_mig ) = 0
          !
          ! clear the iy_hav buffer
          !
          o%iy_hav ( 1:o%ny_mig ) = 0
          !
          ! clear the input header and trace buffers
          !
          tr_buf ( :, :, 1:o%nx_fft ) = 0.
          !
        end if    ! if ( iy_mem .eq. 0 ) then
        !
        ! if this  input trace is part of a line not in memory
        ! fft the data currently in the buffer 
        ! from x to kx_loc and write it out to disk
        ! and then initalize the header and data buffers to zero
        !
        if ( o%iy_mem .ne. o%iy_inp ) then
          !
          ! take the multitasked fft in the x direction 
          ! of the line currently in memory
          ! mt_buf  = dimension of 2d data array tr_buf ( 2, mt_buf/2, nx_fft )
          ! nt_inp  = number off ffts to take
          ! nx_fft  = fft length
          ! table   = fft table
          !
          call pstmig_mcfft ( &
                              +1, &
                              o%nx_trp, o%nx_mig, o%nx_fft, &
                              o%mt_buf, o%mh_inp, o%nt_inp, tr_buf, &
                              i_err &
                            )
          !
          if ( i_err .ne. 0 ) go to 999
          !
          ! fill in the headers in the x direction
          !
          call pstmig_fill_head (  1, o%mh_inp, o%mt_buf, o%nx_fft, tr_buf )
          !
          ! shift header words from negative wavenumber to positive wavenumber
          !
          call pstmig_shift_head ( +1, o%mh_inp, o%mt_buf, o%nx_fft, tr_buf )
          !
          ! if 3d write this line to disk
          ! nx_til tiles nt_dsk long, starting at ( iy_mem-1 )*nx_til+1, 
          ! incrementing by 1
          !
          if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
          .or. o%disk_buf ( 1:5 ) .eq. 'INPUT' ) then
            !
            if ( pcpsx_i_pel() .eq. 0 ) then
              !
              call pstmig_write_disk ( &
                                       o%nx_fil, o%ny_fil, o%lu_x_y, &
                                       o%nx_til, o%iy_mem, o%ny_dsk, &
                                       o%nt_dsk, o%nt_dsk, tr_buf, &
                                       i_err &
                                     )
              !
              if ( i_err .ne. 0 ) go to 999
              !
            end if    ! if ( pcpsx_i_pel() .eq. 0 ) then
            !
            ! pack from tr_buf to tr_pel
            !
          else if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then
            !
            call pstmig_buf_to_pel_x ( &
                              o%nx_fft, o%mt_buf, tr_buf, o%iy_mem, &
                              o%nx_pel, o%nt_pel, o%tr_pel ( :, :, o%iy_mem ) &
                                     )
            !
          end if    ! if ( disk_buf ( 1:3 ) .eq. 'YES'
          !
          ! clear the input header and trace buffers
          !
          tr_buf ( :, :, 1:o%nx_fft ) = 0.
          !
          ! reset the line in memory pointer to the current line
          !
          o%iy_mem = o%iy_inp
          !
          ! clear the ix_hav buffer
          !
          o%ix_hav ( 1:o%nx_mig ) = 0
          !
          ! check the iy_hav buffer
          !
          call pstmig_have_trace ( &
                                   'y', &
                                   o%n_trin, o%m_trin, &
                                   o%iy_inp, o%ny_mig, o%iy_hav, &
                                   i_err &
                                 )
          !
          if ( i_err .ne. 0 ) go to 997
          !
        end if    ! if ( iy_mem .ne. iy_inp ) then
        !
        ! make sure we have not input two traces for one bin
        !
        call pstmig_have_trace ( &
                                 'x', &
                                 o%n_trin, o%m_trin, &
                                 o%ix_inp, o%nx_mig, o%ix_hav, &
                                 i_err &
                               )
        !
        if ( i_err .ne. 0 ) go to 997
        !
        ! apply xy spatial taper to this trace
        !
        call taper_xy ( &
                  o%ix_inp, o%iy_inp, o%nx_mig, o%ny_mig, o%nx_tap, o%ny_tap, &
                  o%nt_inp, tr_inp ( :, i_inp ) &
                      )
        !
        tr_buf ( :, :, o%ix_inp ) = 0.
        !
        ! for migration -
        ! copy header and trace to buffers
        ! copy header to buffer
        !
        tr_buf ( 1, 1:o%mh_inp, o%ix_inp ) = hd_inp ( 1:o%mh_inp, i_inp )
        !
        ! copy real trace into complex trace
        !
        tr_buf ( 1, o%mh_inp+1:o%mh_inp+o%nt_inp, o%ix_inp ) = &
        tr_inp ( 1:o%nt_inp, i_inp )
        !
        ! increment the number of x traces in memory counter
        !
        o%nx_mem = o%nx_mem + 1
        !
        !print' ( " input pe=", i4, " n=", i6, " tr=", g12.5, &
        !& " t=", g12.5 )', &
        !pcpsx_i_pel(), n_trin, &
        !matfun_amax ( nt_inp, tr_inp ( :, i_inp ) ), &
        !matfun_amax ( nt_inp*2, tr_buf ( :, mh_inp+1, ix_inp ) )
        !
      end if    ! if ( ix_inp .ge. 1 .and. ix_inp le. nx_mig
      !
    end do do_input_traces 
    !
!print'(" end input ii=",i5," ni=",i5," io=",i5," no=", i5," ro_grp=",g12.6)', &
!o%i_trin, o%n_trin, o%i_trot, o%n_trot, o%ro_grp
    !
    return
    !
996 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( " error in pstmig_input during stretch " &
    & )' )
    !
    go to 999
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( " error in pstmig_input in data ordering " &
    & )' )
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( " error in pstmig_input during write " &
    & )' )
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( " error in pstmig_input " &
    & )' )
    i_err = i_err - 1
    !
    return
    !
  end subroutine pstmig_input
  !
  subroutine pstmig_output ( o, n_inp, hd_inp, tr_inp, tr_buf, i_err )
    !
    ! output the next trace
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: n_inp      ! num input traces
    double precision,    intent(inout) :: hd_inp ( :, : ) ! input headers
    real,                intent(inout) :: tr_inp ( :, : ) ! input traces
    real,                intent(inout) :: &
    tr_buf ( 2, o%mt_buf/2, max ( o%nx_fft, o%ny_fft, o%nx_dsk, o%ny_dsk ) )
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    i_err = 0
    !
    ! increment output trace counters
    !
    o%i_trot = o%i_trot + 1    ! output trace counter total
    !
    o%j_trot = o%j_trot + 1    ! output trace counter in group
    !
    ! read the next bin - only traces bins which were filled on input
    ! will be output.  this is decided by looking at header word 1
    !
  1 continue
    !
    if ( o%l_trot .lt. o%nx_mig * o%ny_mig ) then
      !
      o%l_trot = o%l_trot + 1    ! output trace counter on disk
      !
      ! output trace x and y values
      !
      o%iy_mig = max ( 1, min ( o%ny_mig,   ( o%l_trot-1 )/o%nx_mig+1 ) )
      !
      o%ix_mig = max ( 1, min ( o%nx_mig, mod ( o%l_trot-1, o%nx_mig )+1 ) )
      !
      !!print*,' a l_trot=',l_trot,' ix_mig=',ix_mig,' iy_mig=',iy_mig
      !
      if ( o%iy_dir .lt. 0 ) o%iy_mig = o%ny_mig - o%iy_mig + 1
      !
      if ( o%ix_dir .lt. 0 ) o%ix_mig = o%nx_mig - o%ix_mig + 1
      !
      ! if this output trace is part of a line not in memory
      ! read the next line into memory and take fft from kx_loc to x
      !
      if ( o%iy_mem .ne. o%iy_mig ) then
        !
        ! reset the line in memory pointer to the current line
        !
        o%iy_mem = o%iy_mig
        !
        o%nx_mem = o%nx_fft
        !
        ! initalize the data buffer to zero
        !
        tr_buf ( :, :, 1:o%nx_fft ) = 0.
        !
        ! nx_til tiles nt_dsk long, 
        ! starting at ( iy_mem-1 )*nx_til+1, incrementing by 1
        !
        if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
        .or. o%disk_buf ( 1:6 ) .eq. 'OUTPUT' ) then
          !
          if ( pcpsx_i_pel() .eq. 0 ) then
            !
            call pstmig_read_disk ( &
                                    o%nx_fil, o%ny_fil, o%lu_x_y, &
                                    o%nx_til, o%iy_mem, o%ny_dsk, &
                                    o%nt_dsk, o%nt_dsk, tr_buf, &
                                    i_err &
                                  )
            !
            if ( i_err .ne. 0 ) go to 998
            !
          end if
          !
         else if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then
          !
call pstmig_pel_to_buf_x ( o%nx_fft, o%mt_buf, tr_buf, o%iy_mem, &
                           o%nx_pel, o%nt_pel, o%tr_pel ( :, :, o%iy_mem ) )
          !
        end if    ! if ( disk_buf ( 1:3 ) .eq. 'YES'
        !
        ! broadcast the output image to all pes
        !
        call pcpsx_broadcast ( 0, o%nx_til, tr_buf )
        !
        ! unshift header words from positive wavenumber to negative wavenumber
        !
        call pstmig_shift_head ( -1, o%mh_inp, o%mt_buf, o%nx_fft, tr_buf )
        !
        ! copy conjugate of positive wavenumbers into negative wavenumbers
        !
  call pstmig_conjg_copy ( o%mt_buf, o%mh_inp, o%nt_mig, o%nx_fft, tr_buf )
        !
        ! take the fft in the x direction of the line currently in memory
        !
        call pstmig_mcfft ( &
                            -1, &
                            o%nx_trp, o%nx_mig, o%nx_fft, &
                            o%mt_buf, o%mh_inp, o%nt_mig, tr_buf, &
                            i_err &
                          )
        !
        if ( i_err .ne. 0 ) go to 997
        !
      end if    ! if ( iy_mem .ne. iy_mig ) then
      !
      ! read data from disk into ct_fft there are nt_mig complex values
      !
      hd_inp ( 1:o%mh_inp, 1 ) = tr_buf ( 1, 1:o%mh_inp, o%ix_mig )
      !
      ! pack from complex tr_buf into real tr_inp
      !
      tr_inp ( 1:o%nt_mig, 1 ) = &
      tr_buf ( 1, o%mh_inp+1:o%mh_inp+o%nt_mig, o%ix_mig )
      !
      ! decrement the number of x traces in memory counter
      !
      o%nx_mem = o%nx_mem - 1
      !
      ! if this trace was not input skip it and get the next
      !
      if ( .not. o%all_out .and. nint ( hd_inp ( 1, 1 ) ) .le. 0 ) go to 1
      !
      o%rx_mig = ( o%ix_mig - 1 ) * o%dx_mig + o%x0_mig
      !
      o%ry_mig = ( o%iy_mig - 1 ) * o%dy_mig + o%y0_mig
      !
      o%ro_mig = o%ro_grp
      !
      !o%ro_mig = hd_inp ( hdr_offset, 1 )
      !
      o%hd_top = hd_inp ( hdr_top_mute, 1 )
      !
      o%hd_bot = hd_inp ( hdr_bottom_mute, 1 )
      !
!print'(" ix=",i8," iy=",i8," h1=",i6," it=",i6," ib=",i6," ro=",g12.6)',&
!o%ix_mig, o%iy_mig, nint(hd_inp ( 1, 1 ) ), &
!nint(o%hd_top), nint(o%hd_bot), o%ro_mig
      !
      if ( nint ( hd_inp ( 1, 1 ) ) .le. 0 ) &
      o%hd_top = 1
      !
      if ( nint ( hd_inp ( 1, 1 ) ) .le. 0 ) &
      o%hd_bot = o%ns_out
      o%hd_top = 1
      o%hd_bot = o%ns_out
      !
      ! set the output headers
      !
      call migfun_output_headers ( &
                                 o%grid_obj, &
                                 o%i_trot, o%io_grp, o%j_trot, &
                                 o%hx_mig, o%rx_mig, o%x0_scl, &
                                 o%hy_mig, o%ry_mig, o%y0_scl, &
                                 o%ro_mig, 0., &
                                 -1, -1, &
                                 0, 1., tr_inp(:,1), &
                                 1, o%velocity_scale, &
                                 o%mh_inp, hd_inp(:,1), &
                                 o%nt_mig, tr_inp(:,1), &
                                 i_err &
                               )
      !
      if ( i_err .ne. 0 ) go to 999
      !
      ! shift the mute header words to account 
      ! for the change in time coordinates
      ! top mute
      !
      o%hd_top = max ( 1., min ( float ( o%nt_mig+1 ), &
  ( ( o%hd_top - 1. ) * o%dt_inp + o%t0_inp - o%t0_mig ) / o%dt_mig + 1. ) )
      !
      hd_inp ( hdr_top_mute, 1 )    = o%hd_top
      !
      ! bot mute
      !
      o%hd_bot = max ( 1., min ( float ( o%nt_mig+1 ), &
  ( ( o%hd_bot - 1. ) * o%dt_inp + o%t0_inp - o%t0_mig ) / o%dt_mig + 1. ) )
      !
      hd_inp ( hdr_bottom_mute, 1 ) = o%hd_bot
      !
      ! save headers for output traces ( locations 9 - 12 )
      !
      call headsave_store ( o%h, o%j_trot, 9, hd_inp ( :, 1 ) )
      !
      n_inp = 1                              ! the number of output traces
      !
      !!print*,' l_trot=',l_trot,' hx=', hd_inp ( hx_mig, 1 ) , &
      !' hy=', hd_inp ( hy_mig, 1 )
      !
!print' ( " output pe=", i4, " i=", i6, " tr=", g12.5, " tb=", g12.5 )', &
!pcpsx_i_pel(), o%i_trot, matfun_amax ( o%nt_mig, tr_inp ), &
!matfun_amax ( o%nt_mig*2, tr_buf ( :, o%mh_inp+1, o%ix_mig ) )
      !
    else    ! if ( l_trot .lt. o%nx_mig * o%ny_mig ) then
      !
      n_inp = 0                              ! the number of output traces
      !
    end if    ! if ( l_trot .lt. o%nx_mig * o%ny_mig ) then
    !
    return
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_output pe=", i8, &
    & /, " during pstmig_read_disk " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_output pe=", i8, &
    & /, " during pstmig_mcfft " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_output pe=", i8, &
    & /, " j_trot=", i8, " l_trot=", i8, " n_trot=", i8, &
    & /, " nx_mig=", i8, " ny_mig=", i8, " iy_mem=", i8&
    & )' ) &
    pcpsx_i_pel(), &
    o%j_trot, o%l_trot, o%n_trot, o%nx_mig, o%ny_mig, o%iy_mem
    !
    i_err = -1
    !
    return
    !
  end subroutine pstmig_output
  !
  subroutine pstmig_read_velocity ( o, i_err )
    !
    ! get the reference velocity function
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! Local variables
    !
    character ( len=80 )               :: crd80
    character ( len=8 )                :: inp_name
    character ( len=4 )                :: inp_type
    integer                            :: i_xy_inp, n_xy_inp
    integer                            :: hx_inp, hy_inp
    real                               :: rx_inp, ry_inp
    real                               :: rt_inp ( o%mt_lay ) ! auomatic array
    real                               :: rv_inp ( o%mt_lay ) ! auomatic array
    type ( velio_struct ),     pointer :: velio_obj ! VELIO data structur
    !
    integer                            :: it_vel
    real                               :: rz_vel
    real                               :: rs_vel
    !
    nullify ( velio_obj ) ! pointer to VELIO data structure.
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    o%t0_vel = 0.
    o%t1_vel = o%t1_tab
    o%dt_vel = o%dt_tab
    o%nt_vel = nint ( ( o%t1_vel - o%t0_vel ) / o%dt_vel ) + 1
    o%iv_vel = o%nt_vel - o%nt_tab
    !
    xxif_nt_vel_error : &
    if ( o%nt_vel .ne. o%nt_tab + nint ( o%t0_tab / o%dt_tab ) ) then
      !
      print'( /, " pstmig_read_velocity nt_vel_error ", &
      & /, " nt_vel=",i8," should be=",i8, &
      & /, " iv_vel=",i8, &
      & /, " nt_vel=",i8," t0_vel=",g12.6," t1_vel=",g12.6," dt_vel=",g12.6, &
      & /, " nt_tab=",i8," t0_tab=",g12.6," t1_tab=",g12.6," dt_tab=",g12.6 &
      & )', &
      o%nt_vel, o%nt_tab + nint ( o%t0_tab / o%dt_tab ), &
      o%iv_vel, &
      o%nt_vel, o%t0_vel, o%t1_vel, o%dt_vel, &
      o%nt_tab, o%t0_tab, o%t1_tab, o%dt_tab
      !
      stop
      !
    end if xxif_nt_vel_error 
    !
    call memfun_all ( o%rt_lay, o%mt_lay,  'rt_lay', i_err ) ! layer time
    call memfun_all ( o%rv_lay, o%mt_lay,  'rv_lay', i_err ) ! layer velocity
    call memfun_all ( o%rt_vel, o%nt_vel,  'rt_vel', i_err ) ! gridded time
    call memfun_all ( o%rz_vel, o%nt_vel,  'rz_vel', i_err ) ! gridded depth
    call memfun_all ( o%rv_vel, o%nt_vel,  'rv_vel', i_err ) ! gridded velocity
    call memfun_all ( o%rs_vel, o%nt_vel,  'rs_vel', i_err ) ! gridded slowness
    call memfun_all ( o%rl_vel, o%nt_vel,  'rl_vel', i_err ) ! gridded sloth
    call memfun_all ( o%r2_vel, o%nt_vel,  'r2_vel', i_err ) ! gridded vel**2
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! read the velocity on a single pe 
    ! and broadcast the info to the remaining pes
    !
    xxif_root_pe : if ( pcpsx_i_pel() .eq. 0 ) then
      !
      ! create a constant velocity
      !
      xxif_const_vel : &
      if ( string_upper_compare ( o%path_vel, pathcheck_empty ) ) then
        !
        call pstmig_set_velocity ( &
        o%mt_lay, o%nt_lay, o%rt_lay, o%rv_lay, o%const_vel )
        !
        ! read the input velocity file
        !
      else xxif_const_vel
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        write ( pc_get_lun(), ' ( " pstmig velocity file=", a &
        & )' ) &
        trim ( o%path_vel )
        !
        ! open the velocity file
        !
        call velio_open_read ( &
        velio_obj, o%path_vel, n_xy_inp, i_err, crd80, hx_inp, hy_inp )
        !
        if ( i_err .ne. 0 ) go to 998
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        write ( pc_get_lun(), ' ( " pstmig velocity file=", a, &
        & /, " n_xy_inp=", i5, " hx_inp=", i2, " hy_inp=", i2&
        & )' ) &
        trim ( o%path_vel ), n_xy_inp, hx_inp, hy_inp
        !
        ! search through the input functions to find the creference function
        !
        do_i_xy_inp : do i_xy_inp = 1 , n_xy_inp
          !
          ! read the input velocity file
          ! read the functin into t_inp, v_inp
          !
          call velio_read_velfun ( &
          velio_obj, rx_inp, ry_inp, o%nt_lay, rt_inp, rv_inp, i_err, &
          crd80, inp_name, inp_type )
          !
          if ( i_err .ne. 0 ) go to 997
          !
          if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
          & " pstmig velocity name=", a8, " inp_type=", a4, &
          & /, " x=", f10.2, " y=", f10.2&
          & )' ) &
          inp_name, inp_type, rx_inp, ry_inp
          !
          call string_to_upper ( inp_name )
          !
          if ( string_upper_compare ( o%ref_name, inp_name ) &
          .or. string_upper_compare ( o%ref_name, 'first' ) ) go to 1
          !
        end do do_i_xy_inp 
        !
        go to 996
        !
1     continue
        !
        ! convert from input type to VTIN
        if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
        & /, " pstmig_read_velocity ", &
        & /, " reference velocity name=", a8, &
        &  " x=", f10.2, " y=", f10.2, &
        & /, " converting from input type =", a4, " to VTIN type " &
        & )' ) &
        inp_name, rx_inp, ry_inp, inp_type
        !
        ! convert from input type to VTIN - Croeccted from VTRM, RSDay
        !
        call velutil_convert ( &
        inp_type, o%nt_lay, rt_inp, rv_inp, 'VTIN', o%rt_lay, o%rv_lay, i_err )
        !
        if ( i_err .ne. 0 ) go to 995
        !
      end if xxif_const_vel
      !
    end if xxif_root_pe
    !
    ! broadcast the velocity from pe 0 to the rest
    !
    call pcpsx_broadcast ( 0, o%nt_lay )
    !
    call pcpsx_broadcast ( 0, o%nt_lay,  o%rt_lay )
    !
    call pcpsx_broadcast ( 0, o%nt_lay,  o%rv_lay )
    !
    ! scale the velocities mig velocity = inp velocity * velocity_scale
    !
    o%rv_lay ( 1:o%nt_lay ) = o%rv_lay ( 1:o%nt_lay ) * o%velocity_scale
    !
    ! interpolate output velocity to a uniform time grid
    ! this will be done in slowness
    !
    call pstmig_interpolate_velocity ( &
                                       o%nt_lay, o%rt_lay, o%rv_lay, &
                                       o%nt_vel, 0., o%dt_mig, o%rv_vel )
    !
    ! slowness and sloth
    !
    o%rs_vel = 1. / o%rv_vel 
    !
    o%rl_vel = o%rs_vel ** 2
    !
    o%r2_vel = o%rv_vel ** 2
    !
    call matfun_line ( 'rt_vel', o%nt_vel, o%t0_vel, o%dt_vel, o%rt_vel, i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    rz_vel = 0.
    !
    rs_vel = o%rs_vel ( 1 )
    !
    do_it_vel : do it_vel = 1 , o%nt_vel 
      !
      o%rv_vel ( it_vel ) = 2. / ( o%rs_vel ( it_vel ) + rs_vel )
      !
      o%rz_vel ( it_vel ) = rz_vel
      !
      rz_vel = rz_vel + .5 * o%dt_vel * o%rv_vel ( it_vel ) 
      !
      rs_vel = o%rs_vel ( it_vel )
      !
    end do do_it_vel 
    !
    ! get min, max velocity
    !
    o%rv_min = minval ( o%rv_vel )
    !
    o%rv_max = maxval ( o%rv_vel )
    !
1999 continue
    !
    call velio_close ( velio_obj )
    !
    return
    !
995 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & " error in pstmig_read_velocity pe=", i8 &
    & /, " during velutil_convert " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
996 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & " error in pstmig_read_velocity pe=", i8 &
    & /, " searching for reference function ", &
    & /, " ref_name=", a &
    & )' ) &
    pcpsx_i_pel(), &
    trim ( o%ref_name )
    !
    go to 999
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & " error in pstmig_read_velocity pe=", i8 &
    & /, " during velio_read_velfun ", &
    & /, " crd80=", a &
    & )' ) &
    pcpsx_i_pel(), &
    trim ( crd80 )
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & " error in pstmig_read_velocity pe=", i8 &
    & /, " during velio_open_read ", &
    & /, " crd80=", a &
    & )' ) &
    pcpsx_i_pel(), &
    trim ( crd80 )
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & " error in pstmig_read_velocity pe=", i8, &
    & /, " pstmig velocity file=", a &
    & )' ) &
    pcpsx_i_pel(), &
    trim ( o%path_vel )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine pstmig_read_velocity
  !
  subroutine pstmig_set_dimension ( dimension, n_dimension )
! set the migration direction flag to an allowed value - MIGRATE, MODEL

    character(len=*)  :: dimension
    integer   n_dimension

      call string_to_upper ( dimension )

      if ( dimension ( 1:1 ) .eq. '2' ) then

        dimension = '2D'
        n_dimension = 2

      else

        dimension = '3D'
        n_dimension = 3

      end if

    return
  end subroutine pstmig_set_dimension
  !
  subroutine pstmig_set_velocity ( mt_vel, nt_vel, rt_vel, rv_vel, const_vel )
    ! read a default velocity function
    integer   mt_vel, nt_vel
    real      rt_vel ( mt_vel )
    real      rv_vel ( mt_vel )
    real      const_vel
    integer   it_vel
    nt_vel = 1
    if ( mt_vel .lt. nt_vel ) go to 999
    rt_vel ( 01 ) = 0.0000
    rv_vel ( 01 ) = const_vel
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " pstmig_set_velocity ", &
    & /, " nt_vel=", i8, " mt_vel=", i8, &
    & /, " it_vel  t_vel  v_vel " &
    & )' ) &
      nt_vel, mt_vel
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( 1x, i8, 1x, f12.4, 1x, f12.2&
    & )' ) &
    & ( it_vel, rt_vel ( it_vel ), rv_vel ( it_vel ), it_vel = 1, nt_vel )
    return
999 continue
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( " error in pstmig_set_velocity ", &
    & /, " nt_vel=", i8, " mt_vel=", i8&
    & )' )&
      nt_vel, mt_vel
    stop
  end subroutine pstmig_set_velocity
  !
  subroutine pstmig_interpolate_velocity ( nt_inp, rt_inp, rv_inp, &
                                           nt_out, t0_out, dt_out, rv_out )
    !
    ! interpolate the reference velocity to the image times
    !
    integer   nt_inp
    integer   nt_out
    real      rt_inp ( nt_inp ), rv_inp ( nt_inp )
    real      t0_out
    real      dt_out
    real      rv_out ( nt_out )
    !
    integer   it_inp
    integer   it_out
    !
    integer   it_inp_1 ( nt_out )
    integer   it_inp_2 ( nt_out )
    real      ft_inp_1 ( nt_out )
    real      ft_inp_2 ( nt_out )
    !
    ! invert the input velocity to slowness
    !
    call matfun_invert ( nt_inp, rv_inp )
    !
    call interpolate_i_to_r ( nt_inp, rt_inp, rv_inp, &
                              nt_out, t0_out, dt_out, rv_out )
    !
    ! invert slowness to velocity
    !
    call matfun_invert ( nt_inp, rv_inp )
    !
    call matfun_invert ( nt_out, rv_out )
    !
    ! print the velocities
    !
    xxif_root_pe : if ( pcpsx_i_pel() .eq. 0 ) then
      !
      call interpolate_find_index_g ( nt_inp, rt_inp, &
                                      nt_out, t0_out, dt_out, &
                                      it_inp_1, it_inp_2, &
                                      ft_inp_1, ft_inp_2 )
      !
      write ( pc_get_lun(), ' ( &
      & /, " pstmig_interpolate_velocity ", &
      & /, " original layered velocity function, nt_inp=", i8, &
      & /, "  it_inp time    velocity " &
      & )' )&
      nt_inp
      !
      do_it_inp : do it_inp = 1 , nt_inp
        !
        if ( mod ( it_inp, 20 ) .eq. 1 .or. it_inp .eq. nt_inp ) &
        write ( pc_get_lun(), ' ( 1x, i5, 1x, f8.4, 1x, f8.1&
        & )' )&
        it_inp, rt_inp ( it_inp ), rv_inp ( it_inp )
        !
      end do do_it_inp
      !
      write ( pc_get_lun(), ' ( &
      & /, " final interpolated velocity function, nt_out=", i8, &
      & /, "  it_out time    velocity   ", &
      & " i1    i2   t1     t2         v1       v2 " &
      & )' ) &
      nt_out
      !
      do_it_out : do it_out = 1 , nt_out
        !
        if ( mod ( it_out, 20 ) .eq. 1 .or. it_out .eq. nt_out ) &
        write ( pc_get_lun(), ' ( &
        & 1x, i5, 1x, f8.4, 1x, f8.1, &
        & 1x, f8.4, 1x, f8.4, 1x, f8.1, 1x, f8.1 &
        & )' ) &
        it_out, ( it_out - 1 ) * dt_out + t0_out, rv_out ( it_out ), &
        rt_inp ( it_inp_1 ( it_out ) ), rt_inp ( it_inp_2 ( it_out ) ), &
        rv_inp ( it_inp_1 ( it_out ) ), rv_inp ( it_inp_2 ( it_out ) )
        !
      end do do_it_out
      !
    end if xxif_root_pe 
    !
    return
    !
  end subroutine pstmig_interpolate_velocity
  !
  subroutine pstmig_if_lim_compute ( o, i_err )
    !
    ! determine the min and max frequencies for each time
    !
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! Local variables
    !
    !
    integer                            :: if_mig
    integer                            :: it_mig
    integer                            :: it_lim
    real                               :: rf_mig
    real                               :: foft_1 ( o%nt_mig ) ! automatic arrays
    real                               :: foft_2 ( o%nt_mig ) ! automatic arrays
    !
    i_err = 0
    !
    call memfun_all ( o%if_lim, o%nf_mig, 2,           'if_lim', i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " pstmig_if_lim_compute ", &
    & /, " nf_mig=", i8, " df_mig=", g12.6, " f0_mig=", f10.2, &
    & " f1_mig=", f10.2, &
    & /, " nt_lim=", i8, &
    & /, "   it_lim    t0_lim       f1_lim   f2_lim " &
    & )' ) &
    o%nf_mig, o%df_mig, o%f0_mig, o%f1_mig, o%nt_lim
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & 1x, i5, 1x, f12.6, 1x, f10.2, 1x, f10.2&
    & )' ) &
    ( it_lim, o%t0_lim ( it_lim ), o%f1_lim ( it_lim ), o%f2_lim ( it_lim ), &
    it_lim = 1, o%nt_lim )
    !
    do_it_lim  : do it_lim = 2 , o%nt_lim
      !
      xxif_t0_lim : if ( o%t0_lim ( it_lim ) .lt. o%t0_lim ( it_lim - 1 ) ) then
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        write ( pc_get_lun(), ' ( &
        & /, " pstmig error in definition of frequency time limits ", &
        & " it_lim=", i10, " t0_lim=", f12.4&
        & )' )&
        it_lim, o%t0_lim ( it_lim )
        !
        i_err = i_err - 1
        !
      end if xxif_t0_lim 
      !
    end do do_it_lim
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! find the maximum time this frequecny is migrated to
    !
    call interpolate_i_to_r ( &
    o%nt_lim, o%t0_lim, o%f1_lim, o%nt_mig, o%t0_mig, o%dt_mig, foft_1 )
    !
    call interpolate_i_to_r ( &
    o%nt_lim, o%t0_lim, o%f2_lim, o%nt_mig, o%t0_mig, o%dt_mig, foft_2 )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, "      if_mig  if1_lim if2_lim  ", &
    & " rf_mig    foft_1     foft_2 " &
    & )' )
    !
    do_if_mig : do if_mig = 1 , o%nf_mig
      !
        rf_mig            = ( if_mig - 1 ) * o%df_mig + o%f0_mig
      !
      o%rf_mig ( if_mig ) = ( if_mig - 1 ) * o%df_mig + o%f0_mig
      !
      o%rw_mig ( if_mig ) = ( if_mig - 1 ) * o%dw_mig + o%w0_mig
      !
      o%if_lim ( if_mig, 1 ) = o%nt_mig + 1
      !
      o%if_lim ( if_mig, 2 ) = 0
      !
      do_it_mig_1 : do it_mig = o%nt_mig , 1 , -1
        !
        if ( rf_mig .ge. foft_1 ( it_mig ) ) o%if_lim ( if_mig, 1 ) = it_mig
        !
      end do do_it_mig_1 
      !
      do_it_mig_2 : do it_mig = 1 , o%nt_mig
        !
        if ( rf_mig .le. foft_2 ( it_mig ) ) o%if_lim ( if_mig, 2 ) = it_mig
        !
      end do do_it_mig_2 
      !
      if ( pcpsx_i_pel() .eq. 0 &
      .and. ( mod ( if_mig, 20 ) .eq. 1 .or. if_mig .eq. o%nf_mig ) ) &
      write ( pc_get_lun(), ' ( &
      & 1x, i8, 1x, i8, 1x, i8, 1x, f10.2, 1x, g12.6, 1x, g12.6 &
      & )' ) &
      if_mig, o%if_lim ( if_mig, 1 ), o%if_lim ( if_mig, 2 ), &
      rf_mig, &
      foft_1 ( o%if_lim ( if_mig, 1 ) ), &
      foft_2 ( o%if_lim ( if_mig, 2 ) )
      !
    end do do_if_mig 
    !
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( " error in pstmig_if_lim_compute i_err=", i10, &
    & /, " frequency time limit values must increase " &
    & )' )&
    i_err
    !
    return
    !
  end subroutine pstmig_if_lim_compute
  !
  subroutine pstmig_ac_exp_compute ( o, i_err )
    !
    ! compute a table of complex phases
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! Local variables
    !
    integer                            :: ic_exp 
    real                               :: r_phase
    !
    i_err = 0
    !
    o%nc_exp   = 2048                                  ! number of complex t
    !
    o%nc_exp_m1 = o%nc_exp - 1
    !
    call memfun_all ( o%ac_exp, 2, o%nc_exp, 'ac_exp', i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    o%dc_exp = 2. * pi / o%nc_exp
    !
    o%vc_exp = 1. / o%dc_exp 
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write (pc_get_lun(), '( &
    & /, " pstmig_ac_exp_compute nc_exp=" ,i8, " dc_exp=" ,g12.6 &
    & )') &
    o%nc_exp, o%dc_exp
    !
    do_ic_exp  : do ic_exp  = 1 , o%nc_exp
      !
      r_phase = ( ic_exp  - 1 ) * o%dc_exp
      !
      o%ac_exp ( 1, ic_exp  ) = cos ( r_phase )
      !
      o%ac_exp ( 2, ic_exp  ) = sin ( r_phase )
      !
    end do do_ic_exp 
    !
    return
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_ac_exp_compute ", &
    & /, " during memory allocation " &
    & )' )
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_ac_exp_compute " &
    & )' )
    !
    i_err = -1
    !
  end subroutine pstmig_ac_exp_compute
  !
  subroutine pstmig_fk_snc_compute ( o, i_err )
    !
    ! initialize sinc interpolator coefficints.
    ! see notes of R. S. Day or W. S Harlan article 
    ! in SEP 30. May, 1982 pp 103-112
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! Local variables
    !
    integer                            :: i1_snc
    integer                            :: i2_snc
    complex                            :: fk_snc_c
    complex                            :: exp_snc
    complex                            :: shift_snc
    real                               :: amp_snc
    real                               :: delsh
    real                               :: denom
    real                               :: a2_snc
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    o%n1_snc   = 10                                        ! fk sinc interpo
    !
    o%n2_snc   = 21                                        ! fk sinc interpo
    !
    call memfun_all ( o%fk_snc, 2, o%n1_snc, o%n2_snc, 'fk_snc', i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! check the operator length
    if ( mod ( o%n1_snc, 2 ) .ne.  0 &
          .or. o%n1_snc    .gt. 20 &
          .or. o%n1_snc    .lt.  2 ) go to 999
    !
    ! shift interpolation window 10 per cent of the trace length
    !
    delsh = .1
    !
    do_n2_snc : do i2_snc = 1 , o%n2_snc
      !
      a2_snc =  ( i2_snc-1 ) / float ( o%n2_snc-1 )
      !
      exp_snc = pstmig_tw_fft() &
      * cmplx ( 0., a2_snc * pi - 2 * pi * delsh * ( a2_snc + o%n1_snc / 2 ) )
      !
      do_n1_snc : do i1_snc = 1 , o%n1_snc
        !
        ! shift interpolation window 10 per cent of the trace length
        !
        shift_snc = cexp ( pstmig_tw_fft()*cmplx ( 0., 2*pi*i1_snc*delsh ) )
        !
        denom = ( o%n1_snc / 2 - i1_snc + a2_snc )
        !
        amp_snc = 0.
        !
        xxif_denom : &
        if ( denom .ne. 0 .and. i2_snc .ne. 1 .and. i2_snc .ne. o%n2_snc ) then
          !
          amp_snc =  sin ( pi*a2_snc ) &
          * ( ( o%n1_snc/2+1 ) - abs ( denom ) ) &
          / ( ( o%n1_snc/2+1 ) * denom * pi )
          !
        else if ( i2_snc .eq. 1.and. i1_snc .eq. o%n1_snc / 2 ) then
          !
          amp_snc = 1.
          !
        else if ( i2_snc .eq. o%n2_snc &
            .and. i1_snc .eq. o%n1_snc / 2 + 1 ) then
          !
          amp_snc =  -1.
          !
        end if xxif_denom 
        !
        fk_snc_c = amp_snc * shift_snc * cexp ( exp_snc )
        !
        o%fk_snc ( 1, i1_snc, i2_snc ) =  real ( fk_snc_c )
        !
        o%fk_snc ( 2, i1_snc, i2_snc ) = aimag ( fk_snc_c )
        !
      end do do_n1_snc
      !
    end do do_n2_snc
    !
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_fk_snc_compute ", &
    & " in the operator length =", i8, &
    & /, " should have operator length even and ", &
    & /, "          <2 operator length < 20 " &
    & )' ) &
    o%n1_snc
    !
    i_err = -1
    !
    return
    !
  end subroutine pstmig_fk_snc_compute
  !
  subroutine pstmig_table_compute ( o, i_err )
    !
    ! compute the phase shift table o%tim_tab and the turning time o%tu_tab
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! Local variables
    !
    integer                            :: it_vel
    integer                            :: it_tab

    !
    ! Local variables
    !
    !
    i_err = 0
    !
    o%kx_loc = 0.
    !
    o%ky_loc = 0.
    !
    o%np_tab   = min ( 1001, 4 * max ( o%nx_fft, o%ny_fft ) )! phase shift tab
    !
    o%p0_tab = 0.
    !
    o%p1_tab = 2. / o%rv_min ! max value of wavenumber / frequency
    !
    o%dp_tab = ( o%p1_tab - o%p0_tab ) / float ( o%np_tab - 1 )
    !
    o%p1_tab = ( o%np_tab - 1 ) * o%dp_tab + o%p0_tab 
    !
    o%vp_tab = 1. / o%dp_tab
    !
    ! set the phase and frequency shifting coefficients here
    ! these are used to replace the nint and mod functions
    ! in the phase index computation
    !
    call memfun_all ( o%tu_tab,           o%np_tab,    'tu_tab', i_err )
    call memfun_all ( o%tim_tab, o%nt_tab, o%np_tab,    'tim_tab', i_err )
    call memfun_all ( o%amp_tab, o%nt_tab, o%np_tab,    'amp_tab', i_err )
    call memfun_all ( o%nn_exp_r, o%nt_tab, 'o%nn_exp_r', i_err ) ! real normal
    call memfun_all ( o%nn_exp_i, o%nt_tab, 'o%nn_exp_i', i_err ) ! imag normal
    call memfun_all ( o%nt_exp_r, o%nt_tab, 'o%nn_exp_r', i_err ) ! real normal
    call memfun_all ( o%nt_exp_i, o%nt_tab, 'o%nn_exp_i', i_err ) ! imag normal
    call memfun_all ( o%tn_exp_r, o%nt_tab, 'o%nn_exp_r', i_err ) ! real normal
    call memfun_all ( o%tn_exp_i, o%nt_tab, 'o%nn_exp_i', i_err ) ! imag normal
    call memfun_all ( o%tt_exp_r, o%nt_tab, 'o%tt_exp_r', i_err ) ! real turnal
    call memfun_all ( o%tt_exp_i, o%nt_tab, 'o%tt_exp_i', i_err ) ! imag turnal
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( 'rp_tab', o%np_tab, o%p0_tab, o%dp_tab, o%rp_tab, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( 'rp_tab', o%np_tab, o%p0_tab, o%dp_tab, o%rp_tab, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( 'rv_tab', o%nt_tab, o%t0_tab, o%dt_tab, o%rv_tab, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( 'rt_tab', o%nt_tab, o%t0_tab, o%dt_tab, o%rt_tab, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( 'i0_vot', o%nt_tab, 1, 1, o%i0_vot, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( 'i0_tov', o%nt_vel, 1, 1, o%i0_tov, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! there are nt_vel velocity nodes and nt_tab table nodes
    ! the table node 1      is velocity node iv_vel + 1
    ! the table node nt_tab is velocity node nt_vel
    ! rv_vel is the velocity at each velocity node
    ! rv_tab is the velocity at each table    node
    ! i0_vot is the vleocity node for each table node
    ! i0_tov = 1 for velocity nodes 1 - iv_vel+1
    ! and increments by 1 thereafter
    !
    o%i0_vot = o%i0_vot + o%iv_vel
    !
    do_it_vel : do it_vel = 1 , o%nt_vel
      !
      o%i0_tov ( it_vel ) = max ( 1, o%i0_tov ( it_vel ) - o%iv_vel )
      !
    end do do_it_vel 
    !
    do_it_tab : do it_tab = 1 , o%nt_tab
      !
      o%rv_tab ( it_tab ) = o%rv_vel ( o%iv_vel+it_tab )
      !
    end do do_it_tab 
    !
    xxif_it_tab_error : &
    if ( minval ( o%i0_tov ( 1:o%iv_vel+1 ) ) .ne. 1 &
    .or. maxval ( o%i0_tov ( 1:o%iv_vel+1 ) ) .ne. 1 &
    .or.          o%i0_tov (   o%nt_vel   ) .ne. o%nt_tab &
    .or.          o%i0_vot ( 1            ) .ne. o%iv_vel+1 &
    .or.          o%i0_vot (   o%nt_tab   ) .ne. o%nt_vel &
    .or.          o%iv_vel+o%nt_tab .ne. o%nt_vel ) then
      !
      print'(" pstmig_table_compute it_tab_error ", &
      & " iv_tvel=",i8," nt_vel=",i8," nt_tab=",i8 )', &
      o%iv_vel, o%nt_vel, o%nt_tab
      !
      print'(" it_tab=", i8, " i0_vot=", i8 ) ', &
      ( it_tab, o%i0_vot(it_tab), it_tab = 1 , o%nt_tab )
      !
      print'(" it_vel=", i8, " i0_tov=", i8 ) ', &
      ( it_vel, o%i0_tov(it_vel), it_vel = 1 , o%nt_vel )
      !
      stop
      !
    end if xxif_it_tab_error 
    !
    call pstmig_shift_coef_compute ( o )
    !
    ! comute the turning time, time and amplitude tables
    !
    call pstmig_table_compute_0 ( o )
    !
    ! print out the table values
    !
    call pstmig_table_print ( o, 'pstmig_table_compute' )
    !
    return
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_table_compute ", &
    & /, " during memory allocation " &
    & )' )
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_table_compute " &
    & )' )
    !
    i_err = -1
    !
    return
    !
  end subroutine pstmig_table_compute
  !
  subroutine pstmig_table_compute_0 ( o )
    !
    ! compute the amplitude table o%amp_tab
    !
    ! References:
    ! Stolt & Benson, 1986, Seismic Migration
    ! Hale, 1990, Colo School Of Mines Report
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    ! Local variables
    !
    integer                            :: it_vel
    integer                            :: it_tab
    integer                            :: ip_tab
    real                               :: rp_loc
    real                               :: rp_sqr
    real                               :: rt_tmp
    real                               :: ra_tmp
    real                               :: pz_top
    real                               :: pz_bot
    real                               :: pz_sqr
    !
    ! initialize the amplitude to 0.
    !
    o%tu_tab = 0
    !
    o%amp_tab = 0.
    !
    o%tim_tab = 0.
    !
    ! for each it_tab and ip_tab determine the amplitude
    ! rp_tab = .5 * k / w
    !
    do_ip_tab : do ip_tab = 1 , o%np_tab
      !
      rp_loc = o%rp_tab ( ip_tab ) ! .5 * kx / w
      !
      rp_sqr = rp_loc ** 2
      !
      rt_tmp = 0. ! one way travel time
      !
      ! compute the amplitude and phase down to the turning wave point
      !
      do_it_vel : do it_vel = 1 , o%iv_vel + o%nt_tab
        !
        it_tab = o%i0_tov ( it_vel )
        !
        pz_sqr = 1. - rp_sqr * o%r2_vel ( it_vel ) ! 1.- (k*v/2w)**2 = cos angle
        !
        if ( pz_sqr .lt. 0. ) go to 1
        !
        ! set the turning time index, tu_tab
        !
        o%tu_tab ( ip_tab ) = it_tab
        !
        pz_bot = sqrt ( pz_sqr )
        !
        xxif_obliquity_none : &
        if ( string_upper_compare ( o%opt_obliquity, 'none' ) ) then
          !
          ra_tmp = 1.0
          !
         else if ( string_upper_compare ( o%opt_obliquity, 'surface' ) &
                    .and. it_vel .gt. 1 ) then
          !
          ra_tmp = o%amp_tab ( 1, ip_tab )
          !
        else xxif_obliquity_none 
          !
          ra_tmp = pz_top / max ( 1.e-6, pz_bot )
          !
        end if xxif_obliquity_none 
        !
        o%amp_tab ( it_tab, ip_tab ) = ra_tmp
        !
        o%tim_tab ( it_tab, ip_tab ) = rt_tmp
        !
        rt_tmp = rt_tmp + .5 * o%dt_tab * pz_bot 
        !
      end do do_it_vel 
      !
  1   continue
      !
    end do do_ip_tab
    !
    ! for posat stack, square the amplitude term and double the time
    ! to change from one way coefficients for prestack
    ! to two way coefficients for poststack
    !
    xxif_post_stack : if ( o%pstmig_post_stack ) then
      !
      o%amp_tab ( :, : ) = o%amp_tab ( :, : ) ** 2
      !
      o%tim_tab ( :, : ) = o%tim_tab ( :, : ) *  2
      !
    end if xxif_post_stack 
    !
    o%amp_tab ( :, : ) = o%amp_tab ( :, : ) ** o%amp_pwr_0
    !
    ! handle the special case of constant amplitude
    !
    if ( abs ( o%amp_pwr ) .le. .0001 ) o%amp_tab = 1.
    !
    return
    !
  end subroutine pstmig_table_compute_0
  !
  subroutine pstmig_table_print ( o, c_title )
    !
    ! print the table values
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    character(len=*),    intent(in   ) :: c_title    ! character title
    !
    ! Local variables
    !

    integer                            :: it_tab
    integer                            :: ip_tab

    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " pstmig_table_print ", a &
    & )' ) &
    trim ( c_title )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " np_tab=", i8, " dp_tab=", g12.6, " p1_tab=", g12.6, &
    & /, " np_tab=", i8, " dp_tab=", g12.6, " p1_tab=", g12.6, &
    & /, " nt_tab=", i8, " t0_tab=", g12.6, " dt_tab=", g12.6, &
    & /, " nt_vel=", i8, " iv_vel=", i8, &
    & /, " rv_min=", f10.2, " rv_max=", f10.2," rp_off=", g12.6 &
    & )' ) &
    o%np_tab, o%dp_tab, o%p1_tab, &
    o%np_tab, o%dp_tab, o%p1_tab, &
    o%nt_tab, o%t0_tab, o%dt_tab, &
    o%nt_vel, o%iv_vel, &
    o%rv_min, o%rv_max, o%rp_off
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " phase table ", &
    & /, " ip_shf=", i8, " np_shf=", i8, &
    & /, " iw_shf=", i8, " nw_shf=", i8, &
    & /, " i0_shf=", i8, " n0_shf=", i8, &
    & /, " n0_rnd=", i8&
    & )' ) &
    o%ip_shf, o%np_shf, &
    o%iw_shf, o%nw_shf, &
    o%i0_shf, o%n0_shf, &
    o%n0_rnd
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, "  it_tab  rt_tab   rv_vel    amp_tab " &
    & )' )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & 1x, i5, 1x, g12.6, &
    & 1x, f12.4, 1x, f12.4, 1x, f12.4, 1x, f12.4 &
    & )' ) &
    ( it_tab, o%rt_tab(it_tab), &
    o%rv_tab ( it_tab ), &
    o%amp_tab ( it_tab, 1 ), o%amp_tab ( it_tab, o%np_tab/2+1 ), &
    o%amp_tab ( it_tab, o%np_tab ), &
    it_tab = 1 ,  o%nt_tab ,  20 )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, "  it_tab  rt_tab   rv_vel    tim_tab " &
    & )' )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & 1x, i5, 1x, g12.6, &
    & 1x, f12.4, 1x, f12.4, 1x, f12.4, 1x, f12.4&
    & )' ) &
    ( it_tab, o%rt_tab(it_tab), &
    o%rv_tab ( it_tab ), &
    o%tim_tab ( it_tab, 1 ), o%tim_tab ( it_tab, o%np_tab/2+1 ), &
    o%tim_tab ( it_tab, o%np_tab ), &
    it_tab = 1 ,  o%nt_tab ,  20 )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, "  ip_tab  rp_tab   tu_tab   v_above    v_below  tim_tab " &
    & )' )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & 1x, i5, 1x, g12.6, 1x, i6, 1x, f10.2, 1x, f10.2, &
    & 1x, g12.6, 1x, g12.6, 1x, g12.6&
    & )' ) &
    ( ip_tab, o%rp_tab(ip_tab), o%tu_tab ( ip_tab ), &
    o%rv_tab (              o%tu_tab ( ip_tab )     ), &
    o%rv_tab ( min(o%nt_tab,o%tu_tab ( ip_tab )+1 ) ), &
    o%tim_tab ( max ( o%tu_tab ( ip_tab ), 1 ), ip_tab ), &
    o%rp_tab(ip_tab)*o%rv_tab (              o%tu_tab ( ip_tab )     ), &
    o%rp_tab(ip_tab)*o%rv_tab ( min(o%nt_tab,o%tu_tab ( ip_tab )+1 ) ), &
    ip_tab = 1 ,  o%np_tab ,  40 )
    !
    !if ( mod ( ip_tab, 20 ) .eq. 1 &
    !.and. mod ( it_tab, 20 ) .eq. 1 ) &
    !print'(" ip_tab=",i8," it_tab=",i8," amp_tab=",g12.6)', &
    !ip_tab, it_tab, o%amp_tab ( it_tab, ip_tab ) 
    !
  end subroutine pstmig_table_print
  !
  subroutine pstmig_shift_coef_compute ( o )
    !
    ! set the phase and frequency shifting coefficients here
    ! these are used to replace the nint and mod functions
    ! in the phase index computation
    ! set the phase and frequency shifting coefficients here
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    ! Local variables
    !
    o%ip_shf = 8                 ! number of bits to shift the phase array
    !
    !o%iw_shf = sizeof_real()    ! number of bits to shift the wavenumber coef
    !
    o%iw_shf = 4                 ! number of bits to shift the wavenumber coef
    !
    o%np_shf = 2 ** o%ip_shf       ! value of 2**o%ip_shf
    !
    o%nw_shf = 2 ** o%iw_shf       ! value of 2**o%iw_shf
    !
    o%i0_shf = ( o%ip_shf + o%iw_shf ) ! total bits to shift interpolated phase
    !
    o%n0_shf = 2 ** o%i0_shf       ! value of 2**o%i0_shf
    !
    o%n0_rnd = o%n0_shf / 2        ! rounding value
    !
    return
    !
  end subroutine pstmig_shift_coef_compute
  !
  subroutine pstmig_set_opt_dir ( opt_dir )
    !
    ! set opt_dir to FORWARD or INVERSE
    !
    character(len=*) :: opt_dir
    !
    call string_to_upper ( opt_dir )
    !
    xxif_forward : if ( string_upper_compare ( opt_dir ( 1:1 ), 'F' ) ) then
      !
      opt_dir = 'FORWARD'
      !
    else xxif_forward 
      !
      opt_dir = 'INVERSE'
      !
    end if xxif_forward 
    !
    return
    !
  end subroutine pstmig_set_opt_dir
  !
  subroutine pstmig_set_mig_type ( mig_type )
    !
    ! set mig_type to NORMAL, TURING, BOTH, FK or CASCADED
    !
    character(len=*) :: mig_type
    !
    call string_to_upper ( mig_type )
    !
    xxif_mig_type : if ( mig_type ( 1:1 ) .eq. 'N' ) then
      !
      mig_type = 'NORMAL'
      !
    else if ( mig_type ( 1:1 ) .eq. 'T' ) then
      !
      mig_type = 'TURNING'
      !
    else if ( mig_type ( 1:1 ) .eq. 'B' ) then
      !
      mig_type = 'BOTH'
      !
    else if ( mig_type ( 1:1 ) .eq. 'F' ) then
      !
      mig_type = 'FK'
      !
    else xxif_mig_type 
      !
      mig_type = 'CASCADED'
      !
    end if xxif_mig_type 
    !
    return
    !
  end subroutine pstmig_set_mig_type
  !
  subroutine pstmig_set_disk_buf ( disk_buf, n_dimension )
    !
    ! set disk_buf to YES or NO
    !
    character(len=*) :: disk_buf
    !
    integer   n_dimension
    !
    call string_to_upper ( disk_buf )
    !
    xxif_disk_buf : &
    if ( disk_buf ( 1:1 ) .eq. 'N' ) then
      !
      disk_buf = 'NO'
      !
    else if ( disk_buf ( 1:1 ) .eq. 'Y' ) then
      !
      disk_buf = 'YES'
      !
    else if ( disk_buf ( 1:1 ) .eq. 'I' ) then
      !
      disk_buf = 'INPUT'
      !
    else if ( disk_buf ( 1:1 ) .eq. 'O' ) then
      !
      disk_buf = 'OUTPUT'
      !
    else if ( disk_buf ( 1:1 ) .eq. 'M' ) then
      !
      disk_buf = 'FORWARD'
      !
    else if ( disk_buf ( 1:1 ) .eq. '2' ) then
      !
      disk_buf = '2D'
      !
    else xxif_disk_buf 
      !
      disk_buf = 'NO'
      !
    end if xxif_disk_buf 
    !
    if ( disk_buf ( 1:1 ) .eq. '2' .and. n_dimension .eq. 3 ) disk_buf = 'YES'
    !
    return
    !
  end subroutine pstmig_set_disk_buf
  !
  subroutine pstmig_set_sort_order ( sort_order )
    !
    ! set the sort_order parameter to allowed values
    !
    character(len=*),  intent(inout) :: sort_order
    !
    xxif_y_is_fast : &
    if ( string_upper_compare ( sort_order ( 1:1 ),  'Y' ) ) then
      !
      sort_order = 'Y_IS_FAST'
      !
    else xxif_y_is_fast 
      !
      sort_order = 'X_IS_FAST'
      !
    end if xxif_y_is_fast 
    !
    return
    !
  end subroutine pstmig_set_sort_order
  !
  subroutine pstmig_have_trace ( c_title, &
                                 n_trin, m_trin, &
                                 ix_inp, nx_mig, ix_hav, &
                                 i_err )
    !
    ! check the ix_hav buffer to make sure a bin is filled only once
    !
    character(len=*),    intent(in   ) :: c_title
    integer,             intent(in   ) :: n_trin
    integer,             intent(in   ) :: m_trin
    integer,             intent(in   ) :: ix_inp
    integer,             intent(in   ) :: nx_mig
    integer,             intent(inout) :: ix_hav ( : )
    integer,             intent(inout) :: i_err
    !
    integer                            :: jx_inp
    integer, save                      :: n_print = 0
    integer, save                      :: m_print = 100
    !
    i_err = 0
    !
    jx_inp = max ( 1, min ( nx_mig, ix_inp ) )
    !
    if ( ix_inp .lt. 1 .or. ix_inp .gt. nx_mig &
    .or. ix_hav ( jx_inp ) .ne. 0 ) go to 999
    ix_hav ( ix_inp ) = m_trin
    !
    return
    !
999 continue
    !
    if ( n_print .le. m_print ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_have_trace ", &
    & /, " you already have a trace in this bin ", &
    & /, " you should sort and stack data before pstmig ", &
    & /, " c_title=", a, &
    & /, " n_trin=", i8, " m_trin=", i8, &
    & /, " nx_mig=", i8, " ix_inp=", i8, " ix_hav=", i8 &
    & )' ) &
    trim ( c_title ), &
    n_trin, m_trin, &
    nx_mig, ix_inp, ix_hav ( jx_inp )
    !
    n_print = n_print + 1
    !
    i_err = -1
    !
    return
    !
  end subroutine pstmig_have_trace
  !
  subroutine pstmig_transpose_disk_n ( &
                                       ipn, nx_fil, ny_fil, &
                                       lu_x_y, fn_x_y, &
                                       lu_y_x, fn_y_x, &
                                       nx_dsk, ny_dsk, nt_dsk, &
                                       nx_til, ny_til, nt_til, &
                                       tr_dsk, &
                                       i_err )
! transpose nx_fil * ny_fil disk files
! into a different set of output disk files
! n_pack = packing level of data on disk = 1, 2
! nx_fil = number of files in the x direction
! ny_fil = number of files in the y direction
! lu_x_y = x, y ( input ) disk file logical unit number
! fn_x_y = x, y file name prefix
! lu_y_x = y, x ( output ) disk file logical unit number
! fn_y_x = y, x file name prefix
! nx_dsk = number of x bins on disk = nx_til * nt_til ( fast direction input )
! ny_dsk = number of y bins on disk = ny_til * nt_til ( fast direction output )
! nt_dsk = number of t bins on disk
! nx_til = number of x tiles
! ny_til = number of y tiles
! nt_til = number of traces within a record
! tr_dsk = buffer for loading nt_til x nt_til traces = nt_til records
! there are a total of ny_dsk * nx_til = nx_dsk * ny_til records on disk
! the tiles themselves are always ordered x then y

    integer   ipn
    integer   nx_fil, ny_fil
    integer   lu_x_y ( nx_fil, ny_fil )
    character(len=*) :: fn_x_y
    integer   lu_y_x ( ny_fil, nx_fil )
    character(len=*) :: fn_y_x

    integer   nx_dsk, ny_dsk, nt_dsk
    integer   nx_til, ny_til, nt_til
    real      tr_dsk ( nt_dsk/nt_til, nt_til, nt_til )
    integer   i_err

    integer   j_err
    integer   j_pel
    integer   ix_fil, iy_fil
    integer   i_xy_fil, i_xy_fil_1, i_xy_fil_2, n_xy_fil_pe

      i_err = 0

! print timeing info
      call prnfun_clock ( pc_get_lun(), ' top of transpose' )

      if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
      & /, " pstmig_transpose_disk_n pe=", i8, &
      & /, " ipn=", i8, &
      & /, " lu_x_y=", i8, " lu_y_x=", i8, &
      & /, " nx_fil=", i8, " ny_fil=", i8, &
      & /, " nx_dsk=", i8, " ny_dsk=", i8, " nt_dsk=", i8, &
      & /, " nx_til=", i8, " ny_til=", i8, " nt_til=", i8 &
      & )' ) &
      pcpsx_i_pel(), &
      ipn, lu_x_y ( 1, 1 ), lu_y_x ( 1, 1 ), &
      nx_fil, ny_fil, &
      nx_dsk, ny_dsk, nt_dsk, &
      nx_til, ny_til, nt_til

! make sure the disk and tile sizes are consistent with asumptions
      if ( nx_til * ny_dsk .ne. ny_til * nx_dsk ) go to 998

! open the y, x disk files, zero them
      call pstmig_open_disk_n ( &
                                ipn, ny_fil, nx_fil, &
                                lu_y_x, fn_y_x, 0, &
                                ny_til, nx_dsk, nt_dsk, j_err )
      i_err = pcpsx_min_all_reduce ( j_err )
      if ( i_err .ne. 0 ) go to 997

! divide the files between the pes
n_xy_fil_pe = matfun_rinw ( nx_fil*ny_fil, pcpsx_n_pel() )  ! number of fil
i_xy_fil_1 = n_xy_fil_pe * pcpsx_i_pel() + 1             ! first file this
i_xy_fil_2 = min ( nx_fil*ny_fil, i_xy_fil_1+n_xy_fil_pe-1 )! last file this
      n_xy_fil_pe = max ( 0, i_xy_fil_2-i_xy_fil_1+1 )

      !call pcpsx_flush_file ( pc_get_lun() )    ! flush the online
      do j_pel = 0 , pcpsx_n_pel()-1

        if ( j_pel .eq. pcpsx_i_pel() ) write ( pc_get_lun(), ' ( &
     & /, " pe=", i8, &
     & " n_xy_fil_pe=", i8, " i_xy_fil_1=", i8, " i_xy_fil_2=", i8&
     & )' ) &
     pcpsx_i_pel(), &
     n_xy_fil_pe, i_xy_fil_1, i_xy_fil_2
        !call pcpsx_flush_file ( pc_get_lun() )    ! flush the online
        !j_err = pcpsx_barrier()

      end do    !       do j_pel = 0 , pcpsx_n_pel()-1

! cycle over files for this pe
      do i_xy_fil = i_xy_fil_1 , i_xy_fil_2

        iy_fil = ( i_xy_fil - 1 ) / nx_fil + 1
        ix_fil = mod ( i_xy_fil-1, nx_fil ) + 1

        call pstmig_transpose_disk_1 ( &
                                       ipn, ix_fil, iy_fil, &
                                       lu_x_y ( ix_fil, iy_fil ), fn_x_y, &
                                       lu_y_x ( iy_fil, ix_fil ), fn_y_x, &
                                       nx_dsk, ny_dsk, nt_dsk, &
                                       nx_til, ny_til, nt_til, &
                                       tr_dsk, &
                                       j_err )
        if ( j_err .ne. 0 ) go to 1996

! remove the x_y data file
        call pstmig_remove ( ix_fil, iy_fil, lu_x_y ( ix_fil, iy_fil ), fn_x_y )

      end do    ! do i_xy_fil = i_xy_fil_1 , i_xy_fil_2

 1996 continue
      i_err = pcpsx_min_all_reduce ( j_err )
      if ( i_err .ne. 0 ) go to 996

! print timeing info
      call prnfun_clock ( pc_get_lun(), ' before remove ' )

! remove the x_y data files
      call pstmig_remove_n ( nx_fil, ny_fil, lu_x_y, fn_x_y )

! print timeing info
      call prnfun_clock ( pc_get_lun(), ' end of transpose' )

    return

996 continue
      write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_transpose_disk_n pe=", i8, &
     & /, " during pstmig_transpose_disk_1 " &
     & )' ) &
     pcpsx_i_pel()
      go to 999

997 continue
      write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_transpose_disk_n pe=", i8, &
     & /, " during open " &
     & )' ) &
     pcpsx_i_pel()
      go to 999

998 continue
      write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_transpose_disk_n pe=", i8, &
     & /, " the tile size is inconsistent with the filled disk space ", &
     & /, " should have nx_til*ny_dsk=ny_til*nx_dsk ", &
     & /, " nx_til=", i8, " ny_til=", i8, &
     & /, " nx_dsk=", i8, " ny_dsk=", i8, &
     & /, " nx_til*ny_dsk=", i8, " ny_til*nx_dsk=", i8 &
     & )' ) &
     pcpsx_i_pel(), &
     nx_til, ny_til, &
     nx_dsk, ny_dsk, &
     nx_til*ny_dsk, ny_til*nx_dsk
      go to 999

999 continue
      write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_transpose_disk_n pe=", i8, &
     & /, " nx_fil=", i8, " ny_fil=", i8, &
     & /, " nx_dsk=", i8, " ny_dsk=", i8, " nt_dsk=", i8, &
     & /, " nx_til=", i8, " ny_til=", i8, " nt_til=", i8 &
     & )' ) &
     pcpsx_i_pel(), &
     nx_fil, ny_fil, &
     nx_dsk, ny_dsk, nt_dsk, &
     nx_til, ny_til, nt_til
    i_err = -1
    return

  end subroutine pstmig_transpose_disk_n
  !
  subroutine pstmig_transpose_disk_1 ( &
                                       ipn, ix_fil, iy_fil, &
                                       lu_x_y, fn_x_y, &
                                       lu_y_x, fn_y_x, &
                                       nx_dsk, ny_dsk, nt_dsk, &
                                       nx_til, ny_til, nt_til, &
                                       tr_dsk, &
                                       i_err )
! transpose a single input disk file into a different output disk file
! n_pack = packing level of data on disk = 1, 2
! ix_fil = file index in the x direction
! iy_fil = file index in the y direction
! lu_x_y = x, y ( input ) disk file logical unit number
! fn_x_y = x, y file name prefix
! lu_y_x = y, x ( output ) disk file logical unit number
! fn_y_x = y, x file name prefix
! nx_dsk = number of x bins on disk = nx_til * nt_til ( fast direction input )
! ny_dsk = number of y bins on disk = ny_til * nt_til ( fast direction output )
! nt_dsk = number of t bins on disk
! nx_til = number of x tiles
! ny_til = number of y tiles
! nt_til = number of traces within a record
! tr_dsk = buffer for loading nt_til x nt_til traces = nt_til records
! there are a total of ny_dsk * nx_til = nx_dsk * ny_til records on disk
! the tiles themselves are always ordered x then y

    integer   ipn

    integer   ix_fil, iy_fil

    integer   lu_x_y
    character(len=*) :: fn_x_y

    integer   lu_y_x
    character(len=*) :: fn_y_x

    integer   nx_dsk, ny_dsk, nt_dsk
    integer   nx_til, ny_til, nt_til
    real      tr_dsk ( nt_dsk/nt_til, nt_til, nt_til )
    integer   i_err

    integer   ix_til, iy_til, it_til
    integer   mt_fft

! print timeing info
!      call prnfun_clock ( pc_get_lun(), ' top of transpose of single file' )

      write ( pc_get_lun(), ' ( &
      & /, " pstmig_transpose_disk_1 pe=", i8, &
      & /, " lu_x_y=", i8, " fn_x_y=", a, &
      & /, " lu_y_x=", i8, " fn_y_x=", a, &
      & /, " ix_fil=", i8, " iy_fil=", i8, &
      & /, " nx_dsk=", i8, " ny_dsk=", i8, " nt_dsk=", i8, &
      & /, " nx_til=", i8, " ny_til=", i8, " nt_til=", i8 &
      & )' ) &
      pcpsx_i_pel(), &
      lu_x_y, trim ( fn_x_y ), &
      lu_y_x, trim ( fn_y_x ), &
      ix_fil, iy_fil, &
      nx_dsk, ny_dsk, nt_dsk, &
      nx_til, ny_til, nt_til

! make sure the disk and tile sizes are consistent with asumptions
      if ( nx_til * ny_dsk .ne. ny_til * nx_dsk ) go to 998

! the total number of words in one record is nt_dsk
      mt_fft = nt_dsk / nt_til

      do iy_til = 1 , ny_til

        do ix_til = 1 , nx_til

! read nt_til y slices each nt_til x traces long
! the first record in this tile is it_til
! the skip between records is nx_til

! this tile has x values ( ix_til-1 )*nt_til+1 to ix_til*nt_til
!            and y values ( iy_til-1 )*nt_til+1 to iy_til*nt_til
! in x, y order
          it_til = ( iy_til - 1 ) * nt_til * nx_til + ix_til
! use this pe and do not broadcast to other pes
          call pario_read ( &
                            pcpsx_i_pel(), .false., lu_x_y, &
                            nt_til, it_til, nx_til, &
                            nt_dsk, nt_dsk, tr_dsk ( :, :, 1 ) , &
                            i_err )
          if ( i_err .ne. 0 ) go to 997

! transpose the traces within this tile
          call pstmig_transpose_tile ( mt_fft, nt_til, tr_dsk )

! write the rearanged data back to disk
! write nt_til x slices each nt_til t traces long
! the first record in this tile is it_til
! the skip between records is ny_til
! this tile has x values ( ix_til-1 )*nt_til+1 to ix_til*nt_til
!            and y values ( iy_til-1 )*nt_til+1 to iy_til*nt_til
! in y, xorder
          it_til = ( ix_til - 1 ) * nt_til * ny_til + iy_til
! use this pe and do not broadcast to other pes
          call pario_write ( &
     pcpsx_i_pel(), .false., lu_y_x, &
       nt_til, it_til, ny_til, &
       nt_dsk, nt_dsk, tr_dsk ( :, :, 1 ) , &
     i_err )
          if ( i_err .ne. 0 ) go to 996

        end do    ! do ix_til = 1 , nx_til

      end do    ! do iy_til = 1 , ny_til

! print timeing info
!      call prnfun_clock ( pc_get_lun(), ' end of transpose of single file' )

    return

996 continue
      write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_transpose_disk_1 pe=", i8, &
     & /, " during write ix_til=", i8, " iy_til=", i8&
     & )' ) &
     pcpsx_i_pel(), &
     ix_til, iy_til
      go to 999

997 continue
      write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_transpose_disk_1 pe=", i8, &
     & /, " during read ix_til=", i8, " iy_til=", i8&
     & )' ) &
     pcpsx_i_pel(), &
     ix_til, iy_til
      go to 999

998 continue
      write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_transpose_disk_1 pe=", i8, &
     & /, " the tile size is inconsistent with the filled disk space ", &
     & /, " should have nx_til*ny_dsk=ny_til*nx_dsk ", &
     & /, " nx_til=", i8, " ny_til=", i8, &
     & /, " nx_dsk=", i8, " ny_dsk=", i8, &
     & /, " nx_til*ny_dsk=", i8, " ny_til*nx_dsk=", i8 &
     & )' ) &
     pcpsx_i_pel(), &
       nx_til, ny_til, &
       nx_dsk, ny_dsk, &
       nx_til*ny_dsk, ny_til*nx_dsk
      go to 999

999 continue
      write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_transpose_disk_1 pe=", i8, &
     & /, " nx_dsk=", i8, " ny_dsk=", i8, " nt_dsk=", i8, &
     & /, " nx_til=", i8, " ny_til=", i8, " nt_til=", i8 &
     & )' ) &
     pcpsx_i_pel(), &
       nx_dsk, ny_dsk, nt_dsk, &
       nx_til, ny_til, nt_til
    i_err = -1
    return

  end subroutine pstmig_transpose_disk_1
  !
  subroutine pstmig_transpose_tile ( mt_fft, nt_til, tr_dsk )
! transpose traces within a tile
! nt_til = number of traces within a record

    integer   mt_fft
    integer   nt_til
    real      tr_dsk ( mt_fft, nt_til, nt_til )
    real      tr_tmp ( mt_fft ) ! automatic array

    integer   ix_til, iy_til

!      if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
!     & /, " pstmig_transpose_tile ", &
!     & /, " mt_fft=", i8, " nt_til=", i8 &
!     & )' ) &
!     mt_fft, nt_til

! cycle over traces
      do iy_til = 1 , nt_til

        do ix_til = 1 , iy_til-1

! copy column ix_til, iy_til to tr_tmp
          tr_tmp ( : ) = tr_dsk ( :, ix_til, iy_til )

! copy column iy_til, ix_til to ix_til, iy_til
          tr_dsk ( :, ix_til, iy_til ) = tr_dsk ( :, iy_til, ix_til )

! copy tr_tmp ( original column ix_til, iy_til ) to column iy_til, ix_til to
          tr_dsk ( :, iy_til, ix_til ) = tr_tmp ( : )

        end do    ! do ix_til = 1 , iy_til-1

      end do    ! do iy_til = 1 , nt_til

    return

  end subroutine pstmig_transpose_tile
  !
  subroutine pstmig_fill_head ( nh_do, mh_inp, mt_buf, nx_inp, tr_buf )
    !
    ! interpolate unset header words
    !
    integer,             intent(in   ) :: nh_do
    integer,             intent(in   ) :: mh_inp
    integer,             intent(in   ) :: mt_buf
    integer,             intent(in   ) :: nx_inp
    real,                intent(inout) :: tr_buf ( 2, mt_buf/2, nx_inp )
    !
    integer                            :: ih_do
    integer                            :: ix_inp_1
    integer                            :: ix_inp_2
    integer                            :: ix_inp_3
    integer                            :: ih_inp
    integer                            :: ix_inp
    integer                            :: nx_fill
    real                               :: hd_flag
    real                               :: dh_dx
    !
    do_ih_do : do ih_do = 1 , nh_do
      !
      hd_flag = 0.
      !
      nx_fill = 0
      !
      !print' ( /, " pstmig_fill_head ", &
      !& /, " nx_inp=", i8, " mt_buf=", i8 )', nx_inp, mt_buf
      !print' ( " ix_inp      h7      h8     h17     h18 " )'
      !print' ( 1x, i8, 1x, f10.2, 1x, f10.2, 1x, f10.2, 1x, f10.2 )', &
      !( ix_inp, &
      !tr_buf ( ih_do, 07, ix_inp ), tr_buf ( ih_do, 02, ix_inp ), &
      !tr_buf ( ih_do, 17, ix_inp ), tr_buf ( ih_do, 64, ix_inp ), &
      !ix_inp=1, nx_inp )
      !
      ! find the first non flag value of header word ih_do
      ! searching from 1 to nx_inp
      !
      ix_inp_1 = pstmig_find_non_flag ( &
                                        1, nx_inp, ih_do, &
                                        mt_buf, tr_buf ( :, :, 1 ) , hd_flag &
                                      )
      !
      !print' ( " ix_inp_1=", i8 )', ix_inp_1
      !
      if ( ix_inp_1 .le. 0 ) go to 2
      !
      ! set the headers from 1 to ix_inp_1 to the value at ix_inp_1
      ! 
      !print' ( " ix_inp_1=", i8, " hd=", f10.2 )', &
      !ix_inp_1, tr_buf ( ih_do, 1, ix_inp_1 )
      ! 
      do_ih_inp_1 : do ih_inp = 2 , mh_inp
        ! 
        do_ix_inp_1 : do ix_inp = 1 , ix_inp_1-1
          ! 
          tr_buf ( ih_do, ih_inp, ix_inp ) = tr_buf ( ih_do, ih_inp, ix_inp_1 )
          ! 
        end do do_ix_inp_1
        ! 
      end do do_ih_inp_1
      ! 
      nx_fill = nx_fill + max ( 0, ix_inp_1-1 )
      ! 
      ! find the last non flag value of header word ih_do
      ! searching from nx_inp to 1
      ! 
      ix_inp_2 = pstmig_find_non_flag ( &
                                          nx_inp, 1, ih_do, &
                                          mt_buf, tr_buf ( :, :, 1 ), hd_flag &
                                        )
      ! 
     !print' ( " ix_inp_2=", i8, " hd=", f10.2 )', &
     !ix_inp_2, tr_buf ( ih_do, 1, ix_inp_2 )
      ! 
      ! set the headers from ix_inp_2+1 to nx_inp to the value at ix_inp_2
      ! 
      do_ih_inp_2 : do ih_inp = 2 , mh_inp
        ! 
        do_ix_inp_2 : do ix_inp = ix_inp_2+1 , nx_inp
          ! 
          tr_buf ( ih_do, ih_inp, ix_inp ) = tr_buf ( ih_do, ih_inp, ix_inp_2 )
          ! 
        end do do_ix_inp_2 
        ! 
      end do do_ih_inp_2 
      ! 
      nx_fill = nx_fill + ( nx_inp - ix_inp_2 )
      ! 
      ix_inp_3 = ix_inp_2
      ! 
  1   continue
      ! 
      !print' ( " ix_inp_3=", i8, " hd=", f10.2 )', &
      !ix_inp_3, tr_buf ( ih_do, 1, ix_inp_3 )
      ! 
      ! jump out of this loop if all nx_inp have been set
      ! 
      if ( ix_inp_1 .ge. ix_inp_3 ) go to 2
      ! 
      ! find the next non flagged value of header word ih_do
      ! searching from ix_inp+1 to nx_inp
      ! 
      ix_inp_2 = pstmig_find_non_flag ( &
                                        ix_inp_1+1, nx_inp, ih_do, &
                                        mt_buf, tr_buf ( :, :, 1 ), hd_flag &
                                      )
      ! 
      !print' ( " ix=", i8, 1x, i8, &
      !& " h1=", f10.2, 1x, f10.2, &
      !& " h7=", f10.2, 1x, f10.2, &
      !& " h8=", f10.2, 1x, f10.2 &
      !& )', &
      !ix_inp_1, ix_inp_2, &
      !tr_buf ( ih_do, 1, ix_inp_1 ), tr_buf ( ih_do, 1, ix_inp_2 ), &
      !tr_buf ( ih_do, 7, ix_inp_1 ), tr_buf ( ih_do, 7, ix_inp_2 ), &
      !tr_buf ( ih_do, 8, ix_inp_1 ), tr_buf ( ih_do, 8, ix_inp_2 )
      ! 
      do_ih_inp_3 : do ih_inp = 2 , mh_inp
        ! 
        dh_dx = ( tr_buf ( ih_do, ih_inp, ix_inp_2 ) &
                - tr_buf ( ih_do, ih_inp, ix_inp_1 ) ) &
                / max ( 1, ix_inp_2 - ix_inp_1 )
        ! 
        do_ix_inp_3 : do ix_inp = ix_inp_1+1 , ix_inp_2-1
          ! 
          tr_buf ( ih_do, ih_inp, ix_inp ) = &
          tr_buf ( ih_do, ih_inp, ix_inp+1 ) + ( ix_inp - ix_inp_1 ) * dh_dx
          ! 
        end do do_ix_inp_3 
        ! 
      end do do_ih_inp_3 
      ! 
      nx_fill = nx_fill + max ( 0, ix_inp_2-ix_inp_1-1 )
      ! 
      ix_inp_1 = ix_inp_2
      ! 
      go to 1
      ! 
    2 continue
      ! 
      !if ( pc_get_lun() .ge. 0 ) &
      !write ( pc_get_lun(), ' ( " nx_inp=", i10, " nx_fill=", i10&
      !& )' )nx_inp, nx_fill
      !print' ( " nx_inp=", i10, " nx_fill=", i10 )', nx_inp, nx_fill
      !print' ( " ix_inp      h7      h8     h17     h18 " )'
      !print' ( 1x, i8, 1x, f10.2, 1x, f10.2, 1x, f10.2, 1x, f10.2 )', &
      ! ( ix_inp, &
      !tr_buf ( ih_do, 07, ix_inp ), tr_buf ( ih_do, 08, ix_inp ), &
      !tr_buf ( ih_do, 17, ix_inp ), tr_buf ( ih_do, 64, ix_inp ), &
      !ix_inp=1, nx_inp )
      ! 
    end do do_ih_do 
    ! 
    return
    ! 
  end subroutine pstmig_fill_head
  !
  subroutine pstmig_buf_to_pel_x ( &
                                   nx_buf, mt_buf, tr_buf, iy_mem, &
                                   nx_pel, nt_pel, tr_pel &
                                 )
! pack from tr_buf into tr_pel

    integer   nx_buf, mt_buf
    real      tr_buf ( mt_buf, nx_buf )

    integer   nx_pel, nt_pel
    real      tr_pel ( nt_pel, nx_pel )

    integer   iy_mem
    integer   ix_buf, ix_pel

! save x values
      !print' ( " buf_to_pel_x i_pel=", i8, &
      !& " nx_buf=", i8, " iy_mem=", i8 )', pcpsx_i_pel(), nx_buf, iy_mem

      do ix_buf = pcpsx_i_pel()+1 , nx_buf/2+1 , pcpsx_n_pel()

      !print' ( " ix_buf=", i8, " ix_pel=", i8, " ixy=", i8, &
      !& " tr_buf=", g16.9 )', &
      !ix_buf, ix_pel, &
      ! ( iy_mem-1 )*nx_buf+ix_buf, matfun_amax ( mt_buf, tr_buf ( :, ix_buf ) )

        ix_pel = ( ix_buf - 1 ) / pcpsx_n_pel() + 1
        tr_pel ( 1:mt_buf, ix_pel ) = tr_buf ( 1:mt_buf, ix_buf )

      !print' ( " ix_buf=", i8, " ix_pel=", i8, " tr_buf=", g16.9 )', &
      !ix_buf, ix_pel, matfun_amax ( mt_buf, tr_buf ( 1, ix_buf ) )

      end do    ! do ix_buf = pcpsx_i_pel()+1 , nx_buf , pcpsx_n_pel()

      !print' ( " buf_to_pel_x i_pel=", i4, " iy_mem=", i5, &
      !& " nx_buf=", i5, " tr_buf=", g16.9 )', &
      !pcpsx_i_pel(), iy_mem, nx_buf, matfun_amax ( nx_buf*mt_buf, tr_buf )

    return

  end subroutine pstmig_buf_to_pel_x
  !
  subroutine pstmig_pel_to_buf_x ( &
                                   nx_buf, mt_buf, tr_buf, iy_mem, &
                                   nx_pel, nt_pel, tr_pel &
                                 )
! unpack from tr_pel into tr_buf

    integer   nx_buf, mt_buf
    real      tr_buf ( mt_buf, nx_buf )
    integer   nx_pel, nt_pel
    real      tr_pel ( nt_pel, nx_pel )

    integer   iy_mem
    integer   ix_buf, ix_pel
    integer   i_pel

      !print' ( " pel_to_buf_x i_pel=", i8, &
      !& " nx_buf=", i8, " iy_mem=", i8 )', pcpsx_i_pel(), nx_buf, iy_mem

      do ix_buf = pcpsx_i_pel()+1 , nx_buf/2+1 , pcpsx_n_pel()

        ix_pel = ( ix_buf - 1 ) / pcpsx_n_pel() + 1
        tr_buf ( 1:mt_buf, ix_buf ) = tr_pel ( 1:mt_buf, ix_pel )

      !print' ( " ix_buf=", i8, " ix_pel=", i8, " ixy=", i8, &
      !& " tr_buf=", g16.9 )', &
      !ix_buf, ix_pel, &
      ! ( iy_mem-1 )*nx_buf+ix_buf, matfun_amax ( mt_buf, tr_buf ( 1, ix_buf ) )

      end do    ! do ix_buf = pcpsx_i_pel()+1 , nx_buf/2+1 , pcpsx_n_pel()

      !i_err = pcpsx_barrier()

! broadcast to all pe's
      do ix_buf = 1 , nx_buf/2+1

        i_pel = mod ( ix_buf-1, pcpsx_n_pel() )
        call pcpsx_broadcast ( i_pel, mt_buf, tr_buf ( :, ix_buf ) )

      !print' ( " ix_buf=", i8, "  i_pel=", i3, 2x, i3, " ixy=", i8, &
      !& " tr_buf=", g16.9 )', &
      !ix_buf, i_pel, pcpsx_i_pel(), &
      ! ( iy_mem-1 )*nx_buf+ix_buf, matfun_amax ( mt_buf, tr_buf ( 1, ix_buf ) )

      end do    ! do ix_buf = 1 , nx_buf/2+1

      !i_err = pcpsx_barrier()

      !print' ( " pel_to_buf_x i_pel=", i4, " iy_mem=", i5, &
      !& " nx_buf=", i5, " tr_buf=", g16.9 )', &
      !pcpsx_i_pel(), iy_mem, nx_buf, matfun_amax ( nx_buf*mt_buf, tr_buf )

    return
  end subroutine pstmig_pel_to_buf_x
  !
  subroutine pstmig_buf_to_pel_y ( &
                                   ix_buf, ny_buf, mt_buf, tr_buf, &
                                   nx_pel, ny_pel, nt_pel, tr_pel &
                                 )
! pack from tr_buf into tr_pel

    integer   ix_buf, ny_buf, mt_buf
    real      tr_buf ( mt_buf, ny_buf )

    integer   nx_pel, ny_pel, nt_pel
    real      tr_pel ( nt_pel, nx_pel, ny_pel )

    integer   ix_pel, iy_buf

      ix_pel = ( ix_buf - 1 ) / pcpsx_n_pel() + 1

      !print' ( " buf_to_pel_y i_pel=", i4, &
      !& " ix_pel=", i5, " ix_buf=", i5, " ny_buf=", i5 )', &
      !pcpsx_i_pel(), ix_pel, ix_buf, ny_buf

      do iy_buf = 1 , ny_buf

      !print' ( " ix_pel=", i8, " iy_buf=", i5, " tr_buf=", g16.9 )', &
      !ix_pel, iy_buf, matfun_amax ( mt_buf, tr_buf ( 1       ,iy_buf ) )

       tr_pel ( 1:mt_buf, ix_pel, iy_buf ) = tr_buf ( 1:mt_buf, iy_buf )

      end do    ! do iy_buf = 1 , ny_buf

      !print' ( " buf_to_pel_y i_pel=", i4, " ix_pel=", i5, &
      !& " ix_buf=", i5, " ny_buf=", i5, " tr_buf=", g16.9 )', &
      !pcpsx_i_pel(), ix_pel, ix_buf, ny_buf, &
      !matfun_amax ( ny_buf*mt_buf, tr_buf )

    return
  end subroutine pstmig_buf_to_pel_y
  !
  subroutine pstmig_pel_to_buf_y ( &
                                   ix_buf, ny_buf, mt_buf, tr_buf, &
                                   nx_pel, ny_pel, nt_pel, tr_pel &
                                 )
! unpack from tr_pel into tr_buf

    integer   ix_buf, ny_buf, mt_buf
    real      tr_buf ( mt_buf, ny_buf )

    integer   nx_pel, ny_pel, nt_pel
    real      tr_pel ( nt_pel, nx_pel, ny_pel )

    integer   ix_pel, iy_buf

      ix_pel = ( ix_buf - 1 ) / pcpsx_n_pel() + 1

    !print' ( " pel_to_buf_y i_pel=", i4, &
    !& " ix_pel=", i5, " ix_buf=", i5, " ny_buf=", i5 )', &
    !pcpsx_i_pel(), ix_pel, ix_buf, ny_buf

      do iy_buf = 1 , ny_buf

       tr_buf ( 1:mt_buf, iy_buf ) = tr_pel ( 1:mt_buf, ix_pel, iy_buf )

      !print' ( " ix_pel=", i8, " iy_buf=", i5, " tr_buf=", g16.9 )', &
      !ix_pel, iy_buf, matfun_amax ( mt_buf, tr_buf ( 1       ,iy_buf ) )

      end do    ! do iy_buf = 1 , ny_buf

      !print' ( " pel_to_buf_y i_pel=", i4, " ix_pel=", i5, &
      !& " ix_buf=", i5, " ny_buf=", i5, " tr_buf=", g16.9 )', &
      !pcpsx_i_pel(), ix_pel, ix_buf, ny_buf, &
      !matfun_amax ( ny_buf*mt_buf, tr_buf )

    return
  end subroutine pstmig_pel_to_buf_y
  !
  subroutine pstmig_remove_n ( nx_fil, ny_fil, lu_x_y, fn_x_y )
! remove disk files

    integer   nx_fil, ny_fil
    integer   lu_x_y ( nx_fil, ny_fil )
    character(len=*) :: fn_x_y


    integer   ix_fil, iy_fil

      if ( pcpsx_i_pel() .eq. 0 ) &
     write ( pc_get_lun(), ' ( " pstmig_remove_n ", &
     & /, " nx_fil=", i8, " ny_fil=", i8, " fn_x_y=", a&
     & )' ) &
       nx_fil, ny_fil, trim ( fn_x_y )

! for this file...
! 1 construct the file name
! 2 remove the file

      do iy_fil = 1 , ny_fil

        do ix_fil = 1 , nx_fil

! construct the file name, close and remove the file
    call pstmig_remove ( ix_fil, iy_fil, lu_x_y ( ix_fil, iy_fil ), fn_x_y )

        end do    ! do ix_fil = 1 , nx_fil

      end do    ! do iy_fil = 1 , ny_fil

    return

  end subroutine pstmig_remove_n
  !
  subroutine pstmig_remove ( ix_fil, iy_fil, lu_x_y, fn_x_y )
! remove disk files

    integer   lu_x_y
    integer   ix_fil, iy_fil
    character(len=*) :: fn_x_y

    integer   i_err

    character ( len=filename_length )  :: file_name

      i_err = 0

! for this file...
! 1 construct the file name
! 2 remove the file

! construct the file name
      call pario_name_2 ( fn_x_y, file_name, ix_fil, iy_fil )

      if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
     & /, " pstmig_remove lu_x_y=", i16, " fn_x_y=", a&
     & )' ) &
     lu_x_y, trim ( file_name )

! close and remove the file
! use this pe and do not broadcast to other pes, do remove
!      call pario_close ( pcpsx_i_pel(), .false., lu_x_y, .false., i_err )
!      call pario_close ( pcpsx_i_pel(), .false., lu_x_y, .true., i_err )

      if ( i_err .ne. 0 ) &
     write ( pc_get_lun(), ' ( &
     & /, " error during remove ", &
     & /, " unit=", i4, " file=", a, &
     & /, " i_err=", i8 &
     & )' ) &
     lu_x_y, trim ( file_name ), i_err

    return

  end subroutine pstmig_remove
  !
  subroutine pstmig_shift_head ( i_dir, mh_inp, mt_buf, nx_fft, tr_buf )
    ! shift header words from negative ky_loc values to positive and back
    integer   i_dir
    integer   mh_inp, mt_buf, nx_fft
    real      tr_buf ( 2, mt_buf/2, nx_fft )
    integer   ix_fft, jx_fft
! shift negative from original to positive locations
    if ( i_dir .eq. +1 ) then
      do ix_fft = 2 , nx_fft/2
        jx_fft = nx_fft - ix_fft + 2
        tr_buf ( 2, 1:mh_inp, ix_fft ) = tr_buf ( 1, 1:mh_inp, jx_fft )
      end do    ! do ix_fft = 2 , nx_fft/2
! shift negative back to original from positive locations
    else    ! if ( i_dir .eq. +1 ) then
      do ix_fft = 2 , nx_fft/2
        jx_fft = nx_fft - ix_fft + 2
        tr_buf ( 1, 1:mh_inp, jx_fft ) = tr_buf ( 2, 1:mh_inp, ix_fft )
      end do    ! do ix_fft = 2 , nx_fft/2
    end if    ! if ( i_dir .eq. +1 ) then
    return
  end subroutine pstmig_shift_head
  !
  subroutine pstmig_conjg_copy ( mt_buf, nh_buf, nt_buf, nx_fft, tr_buf )
! copy conjugate of positive wavenumbers into negative wavenumbers
    integer   mt_buf, nh_buf, nt_buf, nx_fft
    real      tr_buf ( 2, mt_buf/2, nx_fft )
    integer   ix_fft, jx_fft
      do ix_fft = 2 , nx_fft/2
        jx_fft = nx_fft - ix_fft + 2
        tr_buf ( 1, nh_buf+1:nh_buf+nt_buf, jx_fft ) = &
       +tr_buf ( 1, nh_buf+1:nh_buf+nt_buf, ix_fft )
        tr_buf ( 2, nh_buf+1:nh_buf+nt_buf, jx_fft ) = &
       -tr_buf ( 2, nh_buf+1:nh_buf+nt_buf, ix_fft )
      end do    ! do ix_fft = 2 , nx_fft/2
    return
  end subroutine pstmig_conjg_copy
  !
  subroutine pstmig_time_freq_fft ( &
                                    i_dir, scale_fft, &
                                    nt_mig, t0_mig, dt_mig, &
                                    nf_mig, f0_mig, df_mig, &
                                    nt_fft, fft_obj_tw, fft_obj_wt, &
                                    rt_tp_kp, rt_fp_kp, rt_fn_kp, &
                                    rt_tp_kn, rt_fp_kn, rt_fn_kn &
                                  )
    !
    ! take 1d time to frequency ( i_dir=+1 ) 
    ! or frequency to time ( i_dir=-1 ) fft
    ! taking into account non zero time and frequency origins
    ! frequency data is broken into positive and negative pieces
    !
    integer,             intent(in   ) :: i_dir
    real,                intent(in   ) :: scale_fft
    integer,             intent(in   ) :: nf_mig
    real,                intent(in   ) :: f0_mig
    real,                intent(in   ) :: df_mig
    integer,             intent(in   ) :: nt_mig
    real,                intent(in   ) :: t0_mig
    real,                intent(in   ) :: dt_mig
    integer,             intent(in   ) :: nt_fft
    type ( fft_struct ),       pointer :: fft_obj_tw
    type ( fft_struct ),       pointer :: fft_obj_wt
    !
    real,                intent(inout) :: rt_tp_kp ( 2, nt_mig )
    real,                intent(inout) :: rt_fp_kp ( 2, nf_mig )
    real,                intent(inout) :: rt_fn_kp ( 2, nf_mig )
    !
    real,                intent(inout) :: rt_tp_kn ( 2, nt_mig )
    real,                intent(inout) :: rt_fp_kn ( 2, nf_mig )
    real,                intent(inout) :: rt_fn_kn ( 2, nf_mig )
    !
    ! Local variables
    !
    !
    ! fft positive wavenumber
    !
    call pstmig_time_freq_fft_1 ( &
                                  i_dir, scale_fft, &
                                  nt_mig, t0_mig, dt_mig, &
                                  nf_mig, f0_mig, df_mig, &
                                  nt_fft, fft_obj_tw, fft_obj_wt, &
                                  rt_tp_kp, rt_fp_kp, rt_fn_kp &
                                )
    !
    ! fft negative wavenumber
    !
    call pstmig_time_freq_fft_1 ( &
                                  i_dir, scale_fft, &
                                  nt_mig, t0_mig, dt_mig, &
                                  nf_mig, f0_mig, df_mig, &
                                  nt_fft, fft_obj_tw, fft_obj_wt, &
                                  rt_tp_kn, rt_fp_kn, rt_fn_kn &
                                )
    !
    return
    !
  end subroutine pstmig_time_freq_fft
  !
  subroutine pstmig_time_freq_fft_1 ( &
                                      i_dir, scale_fft, &
                                      nt_mig, t0_mig, dt_mig, &
                                      nf_mig, f0_mig, df_mig, &
                                      nt_fft, fft_obj_tw, fft_obj_wt, &
                                      rt_tp_kk, rt_fp_kp, rt_fn_kp &
                                    )
    !
    ! take 1d time to frequency ( i_dir=+1 ) 
    ! or frequency to time ( i_dir=-1 ) fft
    ! taking into account non zero time and frequency origins
    ! frequency data is broken into positive and negative pieces
    !
    integer,             intent(in   ) :: i_dir
    real,                intent(in   ) :: scale_fft
    integer,             intent(in   ) :: nf_mig
    real,                intent(in   ) :: f0_mig
    real,                intent(in   ) :: df_mig
    integer,             intent(in   ) :: nt_mig
    real,                intent(in   ) :: t0_mig
    real,                intent(in   ) :: dt_mig
    integer,             intent(in   ) :: nt_fft
    !
    real,                intent(inout) :: rt_tp_kk ( 2, nt_mig )
    real,                intent(inout) :: rt_fp_kp ( 2, nf_mig )
    real,                intent(inout) :: rt_fn_kp ( 2, nf_mig )
    !
    type ( fft_struct ),       pointer :: fft_obj_tw
    type ( fft_struct ),       pointer :: fft_obj_wt
    !
    ! Local variables
    !
    integer                            :: it_mig
    complex                            :: ct_fft ( nt_fft ) ! automatic array
    !
    ! initialize the fft buffer to zero
    !
    ct_fft = cmplx ( 0., 0. )
    !
    ! non zero time origin shift
    !
    it_mig = max ( 1, nint ( t0_mig / dt_mig + 1. ) )
    !
    ! time to frequency transform
    !
    if ( i_dir .eq. +1 ) then
      !
      ! copy input trace to ct_fft
      !
      ct_fft ( it_mig:it_mig+nt_mig-1 ) = &
      cmplx ( rt_tp_kk ( 1, 1:nt_mig ), rt_tp_kk ( 2, 1:nt_mig ) ) * scale_fft
      !
      ! fourier transform from time to frequency
      !
      call fft_cc_transform ( fft_obj_tw, ct_fft ) ! complex to complex, inplac
      !
    end if    ! if ( i_dir .eq. +1 ) then
    !
    ! copy the positive and negatve frequencies
    ! apply phase shift of t0_mig to nf_mig frequencies starting at f0_mig
    ! replace time origin correction frequency shift by it_mig time shift
    ! until it can be checked out better, &
    !       nt_fft, t0_mig, dt_mig
    !
    call pstmig_origin_shift ( &
                               i_dir, &
                               nt_fft, 0., dt_mig, &
                               nf_mig, f0_mig, df_mig, &
                               ct_fft, rt_fp_kp, rt_fn_kp &
                             )
    !
    ! frequency to time fft
    !
    if ( i_dir .eq. -1 ) then
      !
      ! fourier transform from frequency to time
      !
      call fft_cc_transform ( fft_obj_wt, ct_fft ) ! complex to complex, inpla
      !
      ! copy from ct_fft to input trace
      !
      rt_tp_kk ( 1, 1:nt_mig ) = &
       real ( ct_fft ( it_mig:it_mig+nt_mig-1 ) ) * scale_fft
      !
      rt_tp_kk ( 2, 1:nt_mig ) = &
      aimag ( ct_fft ( it_mig:it_mig+nt_mig-1 ) ) * scale_fft
      !
    end if    ! if ( i_dir .eq. -1 ) then
    !
    return
    !
  end subroutine pstmig_time_freq_fft_1
  !
  subroutine pstmig_origin_shift ( &
                                   i_dir, &
                                   nt_fft, t0_mig, dt_mig, &
                                   nf_mig, f0_mig, df_mig, &
                                   ct_fft, ct_pos, ct_neg &
                                 )
    !
    ! copy the positive and negatve frequencies
    ! apply phase shift of t0_mig to nf_mig frequencies starting at f0_mig
    !
    integer,             intent(in   ) :: i_dir
    integer,             intent(in   ) :: nf_mig
    real,                intent(in   ) :: f0_mig
    real,                intent(in   ) :: df_mig
    integer,             intent(in   ) :: nt_fft
    real,                intent(in   ) :: t0_mig
    real,                intent(in   ) :: dt_mig
    complex,             intent(inout) :: ct_fft ( nt_fft )
    real,                intent(inout) :: ct_pos ( 2, nf_mig )
    real,                intent(inout) :: ct_neg ( 2, nf_mig )
    !
    integer                            :: if_mig, if_pos, if_neg
    integer                            :: if_pos_1, if_pos_2
    integer                            :: jf_pos_1, jf_pos_2
    complex                            :: c_phase
    complex                            :: c_pos, c_neg
    real                               :: c_arg
    !
    if_pos_1 = nint ( f0_mig/df_mig ) + 1  ! first frequency input sample
    !
    if_pos_2 = if_pos_1 + nf_mig - 1   ! last  frequency input sample
    !
    jf_pos_1 = max ( 2, if_pos_1 )         ! do limits - avoid zero and nyquist
    !
    jf_pos_2 = min ( nt_fft/2, if_pos_2 )  ! do limits - avoid zero and nyquist
    !
    ! apply a phase shift to account for the nonzero origin
    ! time to frequency transform
    !
    xxif_i_dir : if ( i_dir .eq. +1 ) then
      !
      c_arg = + pstmig_tw_fft() * t0_mig * 2. * pi
      !
      ! positive and - frequencies in 1 loop
      !
      do_if_pos_1 :  do if_pos = jf_pos_1 , jf_pos_2  !+freq index within ct_fft
        !
        if_neg = nt_fft - if_pos + 2   !-freq index within ct_fft
        !
        if_mig = if_pos - if_pos_1 + 1 ! index within ct_pos, ct_neg
        !
        c_phase = c_arg * ( ( if_mig-1 )*df_mig+f0_mig )        ! complex phase
        !
        c_pos = ct_fft ( if_pos ) * cexp ( +c_phase )  !+freq
        !
        c_neg = ct_fft ( if_neg ) * cexp ( -c_phase )  !-freq
        !
        ct_pos ( 1, if_mig ) =  real ( c_pos )
        !
        ct_pos ( 2, if_mig ) = aimag ( c_pos )
        !
        ct_neg ( 1, if_mig ) =  real ( c_neg )
        !
        ct_neg ( 2, if_mig ) = aimag ( c_neg )
        !
       !ct_pos ( if_mig ) = ct_fft ( if_pos ) * cexp ( +c_phase )  !+freq
        !
       !ct_neg ( if_mig ) = ct_fft ( if_neg ) * cexp ( -c_phase )  !-freq
        !
      end do do_if_pos_1 
      !
      ! handle the zero frequency separately
      !
      xxif_if_pos_1 : if ( if_pos_1 .eq. 1 ) then
        !
        if_pos = if_pos_1              !+freq index within ct_fft
        !
        if_mig = if_pos - if_pos_1 + 1 ! index within ct_pos, ct_neg
        !
        c_phase = c_arg * ( ( if_mig-1 )*df_mig+f0_mig )        ! complex phase
        !
        c_pos = ct_fft ( if_pos ) * cexp ( +c_phase )  !+freq
        !
        c_neg = ct_fft ( if_neg ) * cexp ( -c_phase )  !-freq
        !
        ct_pos ( 1, if_mig ) =  real ( c_pos )
        !
        ct_pos ( 2, if_mig ) = aimag ( c_pos )
        !
        ct_neg ( 1, if_mig ) = 0.                             !-freq, real
        !
        ct_neg ( 2, if_mig ) = 0.                             !-freq, imag
        !
       !ct_pos ( if_mig ) = ct_fft ( if_pos ) * cexp ( +c_phase )  !+freq
        !
       !ct_neg ( if_mig ) = cmplx ( 0., 0. )                    !-freq
        !
      end if xxif_if_pos_1 
      !
      ! handle the nyquist frequency separately
      !
      xxif_if_pos_2 : if ( if_pos_2 .eq. nt_fft/2+1 ) then
        !
        if_pos = if_pos_2              !+freq index within ct_fft
        !
        if_mig = if_pos - if_pos_1 + 1 ! index within ct_pos, ct_neg
        !
        c_phase = c_arg * ( ( if_mig-1 )*df_mig+f0_mig )        ! complex phase
        !
        c_pos = ct_fft ( if_pos ) * cexp ( +c_phase )  !+freq
        !
        c_neg = ct_fft ( if_neg ) * cexp ( -c_phase )  !-freq
        !
        ct_pos ( 1, if_mig ) =  real ( c_pos )
        !
        ct_pos ( 2, if_mig ) = aimag ( c_pos )
        !
        ct_neg ( 1, if_mig ) = 0.                             !-freq, real
        !
        ct_neg ( 2, if_mig ) = 0.                             !-freq, imag
        !
       !ct_pos ( if_mig ) = ct_fft ( if_pos ) * cexp ( +c_phase )  !+freq
        !
       !ct_neg ( if_mig ) = cmplx ( 0., 0. )                    !-freq
        !
      end if xxif_if_pos_2 
      !
      ! frequency to time transform
      !
    else xxif_i_dir 
      !
      c_arg = - pstmig_tw_fft() * t0_mig * 2. * pi
      !
      ! positive and - frequencies in separate loops
      !
      do_if_pos_2 : do if_pos = jf_pos_1 , jf_pos_2  !+freq index within ct_fft
        !
        if_neg = nt_fft - if_pos + 2   !-freq index within ct_fft
        !
        if_mig = if_pos - if_pos_1 + 1 ! index within ct_pos, ct_neg
        !
        c_phase = c_arg * ( ( if_mig-1 )*df_mig+f0_mig )        ! complex phase
        !
        ct_fft ( if_pos ) = cmplx ( ct_pos ( 1, if_mig ), &
        ct_pos ( 2, if_mig ) ) * cexp ( +c_phase )  !+freq
        !
       !ct_fft ( if_pos ) = ct_pos ( if_mig ) * cexp ( +c_phase )  !+freq
        !
      end do do_if_pos_2 
      !
      ! positive and - frequencies in separate loops
      !
      do_if_pos_3 : do if_pos = jf_pos_1 , jf_pos_2  !+freq index within ct_fft
        !
        if_neg = nt_fft - if_pos + 2   !-freq index within ct_fft
        !
        if_mig = if_pos - if_pos_1 + 1 ! index within ct_pos, ct_neg
        !
        c_phase = c_arg * ( ( if_mig-1 )*df_mig+f0_mig )        ! complex phase
        !
        ct_fft ( if_neg ) = cmplx ( ct_neg ( 1, if_mig ), &
        ct_neg ( 2, if_mig ) ) * cexp ( -c_phase )  !+freq
       !ct_fft ( if_neg ) = ct_neg ( if_mig ) * cexp ( -c_phase )  !-freq
        !
      end do do_if_pos_3 
      !
      ! handle the zero frequency separately
      !
      xxif_if_pos_3 : if ( if_pos_1 .eq. 1 ) then
        !
        if_pos = if_pos_1              !+freq index within ct_fft
        !
        if_mig = if_pos - if_pos_1 + 1 ! index within ct_pos, ct_neg
        !
        c_phase = c_arg * ( ( if_mig-1 )*df_mig+f0_mig )        ! complex phase
        !
        ct_fft ( if_pos ) = cmplx ( ct_pos ( 1, if_mig ), &
        ct_pos ( 2, if_mig ) ) * cexp ( +c_phase )  !+freq
       !ct_fft ( if_pos ) = ct_pos ( if_mig ) * cexp ( +c_phase )  !+freq
        !
      end if xxif_if_pos_3 
      !
      ! handle the nyquist frequency separately
      !
      xxif_if_pos_4 : if ( if_pos_2 .eq. nt_fft/2+1 ) then
        !
        if_pos = if_pos_2              !+freq index within ct_fft
        !
        if_mig = if_pos - if_pos_1 + 1 ! index within ct_pos, ct_neg
        !
        c_phase = c_arg * ( ( if_mig-1 )*df_mig+f0_mig )        ! complex phase
        !
        ct_fft ( if_pos ) = cmplx ( ct_pos ( 1, if_mig ), &
                                        ct_pos ( 2, if_mig ) ) &
                                      * cexp ( +c_phase )  !+freq
       !ct_fft ( if_pos ) = ct_pos ( if_mig ) * cexp ( +c_phase )  !+freq
        !
      end if xxif_if_pos_4 
      !
    end if xxif_i_dir 
    !
    return
    !
  end subroutine pstmig_origin_shift
  !
  subroutine pstmig_image ( o, tr_buf, i_err )
    !
    ! image all of the input traces, either migration or modeling
    ! on  input data will hold the latest x line
    ! on output data will hold the first  x line
    ! for 2d this will be the only line
    ! for 3d the other lines are on disk
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    real, intent(inout) :: tr_buf ( o%mt_buf, max ( o%nx_fft, o%ny_fft ) )
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    integer                            :: ix_fft


    real                               :: t1_cpu
    real                               :: t2_cpu
    character ( len=80 )               :: crd80
    !
    i_err = 0
    !
    call cpucount ( o%c, o%c_pstmig_image, 1 )
    !
    o%done_flag = .false.
    !
    t1_cpu = getsys_seconds()
    !
    ! print timeing info
    !
    call prnfun_clock ( pc_get_lun(), ' top of pstmig_image' )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " pstmig_image ", &
    & /, " opt_dir           =", a, &
    & /, " mig_type          =", a, &
    & /, " opt_common_angle  =", l2, &
    & /, " pstmig_pre_stack  =", l2, &
    & /, " pstmig_post_stack =", l2, &
    & /, " start of 2d x, y fft cpu=", f12.4, &
    & /, " n_dimension=", i2, &
    & /, " iy_mem=", i8, &
& /, " nf_mig=", i8, " f0_mig=", g12.6, " f1_mig=", g12.6, " df_mig=", g12.6,&
& /, " ns_inp=", i8, " s0_inp=", g12.6, " s1_inp=", g12.6, " ds_inp=", g12.6,&
& /, " ns_out=", i8, " s0_out=", g12.6, " s1_out=", g12.6, " ds_out=", g12.6,&
& /, " nt_tab=", i8, " t0_tab=", g12.6, " t1_tab=", g12.6, " dt_tab=", g12.6,&
& /, " nt_fft=", i8, " t0_fft=", g12.6, " t1_fft=", g12.6, " dt_fft=", g12.6,&
  & /, " nx_fft=", i8, " ny_fft=", i8 &
    & )' ) &
    trim ( o%opt_dir ), &
    trim ( o%mig_type ), &
    o%opt_common_angle, &
    o%pstmig_pre_stack, &
    o%pstmig_post_stack, &
    t1_cpu, &
    o%n_dimension, &
    o%iy_mem, &
    o%nf_mig, o%f0_mig, o%f1_mig, o%df_mig, &
    o%ns_inp, o%s0_inp, o%s1_inp, o%ds_inp, &
    o%ns_out, o%s0_out, o%s1_out, o%ds_out, &
    o%nt_tab, o%t0_tab, o%t1_tab, o%dt_tab, &
    o%nt_fft, o%t0_fft, o%t1_fft, o%dt_fft, &
    o%nx_fft, o%ny_fft
    !
    !print' ( " bef mcfft a pe=", i8, " ix=", i8, " tr=", g12.5 )', &
    !( pcpsx_i_pel(), ix_fft, &
    !matfun_amax ( o%ns_inp*2, tr_buf ( 2*o%mh_inp+1, ix_fft ) ), &
    !ix_fft = 1 , o%nx_fft )
    !
    ! take the fft in the x direction of the line currently in memory
    !
    call pstmig_mcfft ( &
                          +1, &
                          o%nx_trp, o%nx_mig, o%nx_fft, &
                          o%mt_buf, o%mh_inp, o%ns_inp, o%tr_buf, &
                          i_err &
                        )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    !print' ( " aft mcfft a pe=", i8, " ix=", i8, " tr=", g12.5 )', &
    !( pcpsx_i_pel(), ix_fft, &
    !matfun_amax ( o%ns_inp*2, tr_buf ( 2*o%mh_inp+1, ix_fft ) ), &
    !ix_fft = 1 , o%nx_fft )
    !
    ! send a report back every nx_rep x lines
    !
    o%nx_rep = 1
    !
    ! get the currect cpu usage
    !
    t2_cpu = getsys_seconds()
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & " pstmig_image after x fft cpu=", f12.4, " d_cpu=", f12.4&
    & )' ) &
    & t2_cpu, t2_cpu-t1_cpu
    !
    t1_cpu = t2_cpu
    !
    o%jy_mem = 1
    !
    ! fill in the headers in the x direction
    !
    call pstmig_fill_head (  1, o%mh_inp, o%mt_buf, o%nx_fft, tr_buf )
    !
    ! shift header words from negative wavenumber to positive wavenumber
    !
    call pstmig_shift_head ( +1, o%mh_inp, o%mt_buf, o%nx_fft, tr_buf )
    !
    ! for 2d we migrate the line already in memory
    !
    xxif_one_y : if ( o%pstmig_one_y ) then
      !
      !print' ( " pe=", i8, " during 2d migration " )', pcpsx_i_pel()
      !
      !print'(" opt_common_angle=",l2)', o%opt_common_angle 
      !
      ! if pstmig_mig_or_mod is not called the output data 
      ! should be the same as the input datac
      ! apply phase shift 
      ! transform from t_inp, ky_loc, kx_loc to t_out, ky_loc, kx_loc
      ! note we use iy_fft = 0 or the y wavenumber, ky_loc = 0
      !
      o%i2_fft = 1
      !
      call pstmig_mig_or_mod ( o, tr_buf, i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
      ! for 3d we write the line already in memory back to disk
      ! then for each cross line we ..
      ! 1  read the cross line from disk
      ! 2  fft for y to ky_loc
      ! 3  migrate the cross line
      ! 4  fft from ky_loc back to y
      ! 5  write the cross line back to disk
      ! at the end we will read the first x line back into memory
    else xxif_one_y 
      !
      ! write the current line in memory out to disk
      ! nx_til tiles nt_dsk long, 
      ! starting at ( iy_mem-1 )*nx_til+1, incrementing by 1
      !
      xxif_yes_or_input_1 : if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
                            .or. o%disk_buf ( 1:5 ) .eq. 'INPUT' ) then
        !
        !write ( pc_get_lun(), ' ( &
        !& " bef write pe=", i8, " iy_mem=", i8, " nx_til=", i8 &
        !& )' ) &
        !pcpsx_i_pel(), o%iy_mem, o%nx_til
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        call pstmig_write_disk ( &
                                 o%nx_fil, o%ny_fil, o%lu_x_y, &
                                 o%nx_til, o%iy_mem, o%ny_dsk, &
                                 o%nt_dsk, o%nt_dsk, tr_buf, &
                                 i_err &
                               )
        !
        if ( i_err .ne. 0 ) go to 996
        !
       else if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then
        !
call pstmig_buf_to_pel_x ( o%nx_fft, o%mt_buf, tr_buf, o%iy_mem, &
                           o%nx_pel, o%nt_pel, o%tr_pel ( :, :, o%iy_mem ) )
        !
      end if xxif_yes_or_input_1 
      !
      ! transpose the data from x, y order to y, x order
      ! this will open the y, x file and remove the x, y file
      !
      xxif_yes_or_input_2 : if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
                            .or. o%disk_buf ( 1:5 ) .eq. 'INPUT' ) then
        !
        call prnfun_clock ( pc_get_lun(), ' pstmig before transpose 1' )
        !
        call pstmig_transpose_disk_n ( &
                                       o%ipn, o%nx_fil, o%ny_fil, &
                                       o%lu_x_y, o%fn_x_y, &
                                       o%lu_y_x, o%fn_y_x, &
                                       o%nx_dsk, o%ny_dsk, o%nt_dsk, &
                                       o%nx_til, o%ny_til, o%nt_til, &
                                       tr_buf, &
                                       i_err &
                                     )
        !
        call pcpsx_broadcast ( 0, i_err )
        !
        if ( i_err .ne. 0 ) go to 995
        !
        call prnfun_clock ( pc_get_lun(), ' pstmig after  transpose 1' )
        !
      end if xxif_yes_or_input_2 
      !
      ! stop here if this is data input only
      !
      if ( o%disk_buf ( 1:5 ) .eq. 'INPUT' ) go to 901
      !
      xxif_output_1 : if ( o%disk_buf ( 1:6 ) .ne. 'OUTPUT' ) then
        !
        ! cycle over x slices
        ! within this loop all pes are independant of one another
        ! pe pcpsx_i_pel() does ix_fft slice pcpsx_i_pel()+1
        !
        do_ix_fft : &
        do ix_fft = pcpsx_i_pel()+1 , o%nx_fft/2+1 , pcpsx_n_pel()
          !
          o%ix_fft = ix_fft
          !
          o%i2_fft = ix_fft
          !
          ! compute the first tile location for the start of this y line
          ! the skip between tiles is nx_dsk = nx_til * nt_til
          !
          o%it_til = ( o%ix_fft - 1 ) * o%ny_til + 1
          !
          ! intiialize the image memory to zero
          !
          tr_buf ( :, 1:o%ny_fft ) = 0.
          !
          ! read data from disk
          ! ny_til tiles nt_dsk long, starting at ix_fft, incrementing by 1
          !
          xxif_yes_or_forward_1 : if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
                                  .or. o%disk_buf ( 1:7 ) .eq. 'FORWARD' ) then
            !
            !write ( pc_get_lun(), ' ( " bef read ix=", i8, " it_til=", i8, &
            !& " i_pel=", i8&
            !& )' ) &
            !o%ix_fft, o%it_til, pcpsx_i_pel()
            !
            ! ny_til tiles nt_dsk long, starting at it_til, incrementing by 1
            !
            call pstmig_read_disk ( &
                                    o%ny_fil, o%nx_fil, o%lu_y_x, &
                                    o%ny_til, o%ix_fft, o%nx_dsk, &
                                    o%nt_dsk, o%nt_dsk, tr_buf, &
                                    i_err &
                                  )
            !
            if ( i_err .ne. 0 ) go to 994
            !
          else if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then
            !
            call pstmig_pel_to_buf_y ( o%ix_fft, o%ny_mig, o%mt_buf, tr_buf, &
                                       o%nx_pel, o%ny_pel, o%nt_pel, o%tr_pel &
                                     )
            !
          end if xxif_yes_or_forward_1 
          !
          !write ( pc_get_lun(), ' ( &
          !& " bef fft ix=", i8, " i_pel=", i8, " tr=", g16.9&
          !& )' ) &
          !o%ix_fft, pcpsx_i_pel(), matfun_amax ( o%ny_fft*o%mt_buf, tr_buf )
          !
          ! take fft from t, kx_loc, y to t, kx_loc, ky_loc
          !
          call pstmig_mcfft ( &
                              +1, &
                              o%ny_trp, o%ny_mig, o%ny_fft, &
                              o%mt_buf, o%mh_inp, o%ns_inp, tr_buf, &
                              i_err &
                            )
          !
          if ( i_err .ne. 0 ) go to 993
          !
          !write ( pc_get_lun(), ' ( &
          !& " aft mig ix=", i8, " i_pel=", i8, " tr=", g16.9&
          !& )' ) &
          !o%ix_fft, pcpsx_i_pel(), matfun_amax ( o%ny_fft*o%mt_buf, tr_buf )
          !
          ! apply phase shift 
          ! transform from w, ky_loc, kx_loc to t, ky_loc, kx_loc
          !
          call pstmig_mig_or_mod ( o, tr_buf, i_err )
          !
          if ( i_err .ne. 0 ) go to 992
          !
          !write ( pc_get_lun(), ' ( &
          !& " aft mig ix=", i8, " i_pel=", i8, " tr=", g16.9&
          !& )' ) &
          !o%ix_fft, pcpsx_i_pel(), matfun_amax ( o%ny_fft*o%mt_buf, tr_buf )
          !
          ! take fft from t, ky_loc, kx_loc to t, kx_loc, y
          ! there are now nt_out complex samples
          !
          call pstmig_mcfft ( &
                              -1, &
                              o%ny_trp, o%ny_mig, o%ny_fft, &
                              o%mt_buf, o%mh_inp, o%ns_out, tr_buf, &
                              i_err &
                            )
          !
          if ( i_err .ne. 0 ) go to 991
          !
          !write ( pc_get_lun(), ' ( &
          !& " aft fft ix=", i8, " i_pel=", i8, " tr=", g16.9&
          !& )' ) &
          !o%ix_fft, pcpsx_i_pel(), matfun_amax ( o%ny_fft*o%mt_buf, tr_buf )
          !
          ! fill in the headers in the y direction
          !
          call pstmig_fill_head (  2, o%mh_inp, o%mt_buf, o%ny_fft, tr_buf )
          !
          ! write data back to disk in t, y, kx_loc form
          ! ny_til tiles nt_dsk long, starting at it_til, incrementing by 1
          !
          xxif_yes_or_forward_2 : &
          if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
          .or. o%disk_buf ( 1:7 ) .eq. 'FORWARD' ) then
            !
            !write ( pc_get_lun(), ' ( &
            !& " bef write pe=", i8, " ix_fft=", i8, " ny_til=", i8 &
            !& )' ) &
            !pcpsx_i_pel(), o%ix_fft, o%ny_til
            !
            call pstmig_write_disk ( &
                                     o%ny_fil, o%nx_fil, o%lu_y_x, &
                                     o%ny_til, o%ix_fft, o%nx_dsk, &
                                     o%nt_dsk, o%nt_dsk, tr_buf, &
                                     i_err &
                                   )
            !
            if ( i_err .ne. 0 ) go to 990
            !
          else if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then
            !
            call pstmig_buf_to_pel_y ( &
                                       o%ix_fft, o%ny_mig, o%mt_buf, tr_buf, &
                                       o%nx_pel, o%ny_pel, o%nt_pel, o%tr_pel &
                                     )
            !
          end if xxif_yes_or_forward_2 
          !
          ! write some timeing information
          !
          xxif_write_timing_1 : &
          if ( mod ( o%ix_fft-1, 10 ) .eq. 0 .or. o%ix_fft .eq. o%nx_fft &
          .or. mod ( o%ix_fft-1, o%nx_rep ) .eq. 0 ) then
            !
            crd80 = ' '
            !
            write ( crd80, ' ( " pe=", i6, " ix=", i6 , " /", i6 )' ) &
            pcpsx_i_pel(), o%ix_fft, o%nx_fft
            !
            call prnfun_clock ( pc_get_lun(), crd80 )
            !
          end if xxif_write_timing_1 
          !
        end do do_ix_fft 
        !
      end if xxif_output_1 
      !
      ! stop here if this is data migration only
      !
      if ( o%disk_buf ( 1:7 ) .eq. 'FORWARD' ) go to 901
      !
      ! transpose the y, x file to the x, y file
      ! this will open the x, y file and remove the y, x file
      !
      xxif_yes_or_output_1 : &
      if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
      .or. o%disk_buf ( 1:6 ) .eq. 'OUTPUT' ) then
        !
        !write ( pc_get_lun(), ' ( &
        !& /, " bef transpose ", &
        !& /, " nx_dsk=", i8, " ny_dsk=", i8, " nt_dsk=", i8, &
        !& /, " nx_til=", i8, " ny_til=", i8, " nt_til=", i8 &
        !& )' ) &
        !o%nx_dsk, o%ny_dsk, o%nt_dsk, &
        !o%nx_til, o%ny_til, o%nt_til
        !
        call prnfun_clock ( pc_get_lun(), ' pstmig before transpose 2' )
        !
        call pstmig_transpose_disk_n ( &
                                       o%ipn,    o%ny_fil, o%nx_fil, &
                                       o%lu_y_x, o%fn_y_x, &
                                       o%lu_x_y, o%fn_x_y, &
                                       o%ny_dsk, o%nx_dsk, o%nt_dsk, &
                                       o%ny_til, o%nx_til, o%nt_til, &
                                       tr_buf, &
                                       i_err &
                                     )
        !
        call pcpsx_check_worker_errors ( i_err )
        !
        if ( i_err .ne. 0 ) go to 989
        !
        call prnfun_clock ( pc_get_lun(), ' pstmig after  transpose 2' )
        !
      end if xxif_yes_or_output_1 
      !
      ! initalize the data buffer to zero
      !
      tr_buf ( :, 1:o%nx_fft ) = 0.
      !
      ! read first line from from disk
      ! nx_til tiles nt_dsk long, 
      ! starting at ( jy_mem-1 )*nx_til+1, incrementing by 1
      !
      xxif_yes_or_output_2 : &
      if ( o%disk_buf ( 1:3 ) .eq. 'YES' &
      .or. o%disk_buf ( 1:6 ) .eq. 'OUTPUT' ) then
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        call pstmig_read_disk ( &
                                o%nx_fil, o%ny_fil, o%lu_x_y, &
                                o%nx_til, o%jy_mem, o%ny_dsk, &
                                o%nt_dsk, o%nt_dsk, tr_buf, &
                                i_err &
                              )
        !
        call pcpsx_broadcast ( 0, i_err )
        !
        if ( i_err .ne. 0 ) go to 988
        !
      else if ( o%disk_buf ( 1:2 ) .eq. 'NO' ) then
        !
call pstmig_pel_to_buf_x ( o%nx_fft, o%mt_buf, tr_buf, o%jy_mem, &
                           o%nx_pel, o%nt_pel, o%tr_pel ( :, :, o%jy_mem ) )
        !
      end if xxif_yes_or_output_2 
      !
      ! broadcast the first output image to all pes
      !
      call pcpsx_broadcast ( 0, o%nx_til, tr_buf )
      !
    end if xxif_one_y 
    !
    ! print current cpu usage
    !
    t2_cpu = getsys_seconds()
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & " pstmig_image after migration cpu=", f12.4, " d_cpu=", f12.4&
    & )' ) &
    t2_cpu, t2_cpu-t1_cpu
    !
    t1_cpu = t2_cpu
    !
    !print' ( " bef mcfft b pe=", i8, " tr=", g12.5, 1x, g12.5 )', &
    !pcpsx_i_pel(), &
    !matfun_amax ( nt_out*2, tr_buf ( 2*mh_inp+1, 1 ) ), &
    !matfun_amax ( nt_out*2, tr_buf ( 2*mh_inp+1:, nx_fft ) )
    !
    !   shift header words from negative wavenumber to positive wavenumber
    ! unshift header words from positive wavenumber to negative wavenumber
    !
    call pstmig_shift_head ( -1, o%mh_inp, o%mt_buf, o%nx_fft, tr_buf )
    !
    ! copy conjugate of positive wavenumbers into negative wavenumbers
    !
    call pstmig_conjg_copy ( o%mt_buf, o%mh_inp, o%ns_out, o%nx_fft, tr_buf )
    !
    ! take fft from t, kx_loc, y to t, x, y for this line
    !
    call pstmig_mcfft ( &
                        -1, &
                        o%nx_trp, o%nx_mig, o%nx_fft, &
                        o%mt_buf, o%mh_inp, o%ns_out, tr_buf, &
                        i_err &
                      )
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    if ( i_err .ne. 0 ) go to 987
    !
1999 continue
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    !print' ( " aft mcfft b pe=", i8, " tr=", g12.5, 1x, g12.5 )', &
    !pcpsx_i_pel(), &
    !matfun_amax ( nt_out*2, tr_buf ( 2*mh_inp+1, 1 ) ), &
    !matfun_amax ( nt_out*2, tr_buf ( 2*mh_inp+1, nx_fft ) )
    !
    ! set the flag for line in memory and the number of x values in memory
    !
    o%iy_mem = 1
    !
    o%nx_mem = o%nx_fft
    !
    call prnfun_clock ( pc_get_lun(), ' end of pstmig_image' )
    !
    call cpucount ( o%c, o%c_pstmig_image, 2 )
    !
    return
    !
901 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " pstmig_image has completed data processing for ", &
    & " disk_buf=", a, &
    & /, " pstmig will now stop without outputting any traces. " &
    & )' ) &
    trim ( o%disk_buf )
    !
    o%done_flag = .true.
    !
    go to 1999
    !
987 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during output pstmig_mcfft " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
988 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during xxif_yes_or_output_2 pstmig_read_disk " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
989 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during pstmig_transpose_disk_n after mig_or_mod " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
990 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during xxif_yes_or_forward_2 pstmig_write_disk " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
991 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during pstmig_mcfft after mig_or_mod " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
992 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during second pstmig_mig_or_mod " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
993 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during pstmig_mcfft before mig_or_mod " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
994 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during xxif_yes_or_forward_1 pstmig_read_disk " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
995 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during pstmig_transpose_disk_n before mig_or_mod " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
996 continue
    !
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_update pe=" ,i8, &
    & /, " during xxif_yes_or_input_1 pstmig_write_disk " &
    & )' ) &
    pcpsx_i_pel()
    !
    go to 999
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_image ", &
    & /, " during first pstmig_mig_or_mod " &
    & )' )
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_image ", &
    & /, " during input pstmig_mcfft " &
    & )' )
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_image " &
    & )' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine pstmig_image
  !
  subroutine pstmig_image_fk ( &
                               o, &
                               ix_fft, nx_fft, dx_fft, &
                               iy_fft, ny_fft, dy_fft, &
                               rt_tp_kp, &
                               rt_tp_kn, &
                               mig_type, opt_dir, dap, &
                               rw_add, rw_fac, rv_prc, &
                               scale_fft, &
                               nt_inp, t0_inp, dt_inp, &
                               nt_out, t0_out, dt_out, &
                               nf_mig, f0_mig, df_mig, &
                               nt_fft, fft_obj_tw, fft_obj_wt, &
                               nt_lay, rt_lay, rv_lay, &
                               n1_snc, n2_snc, fk_snc, &
                               mt_buf, nt_dim &
                             )
! rt_tp_kp - the complex input trace at +wavenumber.
! rt_tp_kn - the complex input trace at -wavenumber. nt_out
! fk_scp ( nt_fft/2+1 ), fk_map, fk_amp real,   nt_fft
! di_fp_kn, di_fp_kp complex, nt_fft
! fk_snc - fk_snc ( n1_snc, n2_snc ) is the complex sinc interpolator which is
!           precomputed by integer function fkmigop1 ( l, fk_snc )

    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    character(len=*) :: mig_type
    character(len=*) :: opt_dir
    real      dap
    logical   rw_add
    real      rw_fac
    real      rv_prc
    real      scale_fft

    integer   ix_fft, nx_fft
    real      dx_fft

    integer   iy_fft, ny_fft
    real      dy_fft

    integer   nt_inp
    real      t0_inp, dt_inp

    integer   nt_out
    real      t0_out, dt_out

    integer   nf_mig
    real      f0_mig, df_mig

    integer   nt_fft
    type ( fft_struct ), pointer :: fft_obj_tw
    type ( fft_struct ), pointer :: fft_obj_wt

    integer   nt_lay
    real      rt_lay ( nt_lay )
    real      rv_lay ( nt_lay )

    integer   n1_snc, n2_snc
    real      fk_snc ( 2, n1_snc, n2_snc )

    integer   mt_buf, nt_dim

    real      rt_tp_kp ( 2, mt_buf )
    real      dq_tp_kp ( 2, nt_dim )
    real      di_fp_kp ( 2, nt_fft/2+1 ), di_fn_kp ( 2, nt_fft/2+1 )
    real      do_fp_kp ( 2, nt_fft/2+1 ), do_fn_kp ( 2, nt_fft/2+1 )

    real      rt_tp_kn ( 2, mt_buf )
    real      dq_tp_kn ( 2, nt_dim )
    real      di_fp_kn ( 2, nt_fft/2+1 ), di_fn_kn ( 2, nt_fft/2+1 )
    real      do_fp_kn ( 2, nt_fft/2+1 ), do_fn_kn ( 2, nt_fft/2+1 )

    integer   fk_map ( nt_fft/2+1 )
    integer   fk_scp ( nt_fft/2+1 )
    integer   fk_scn ( nt_fft/2+1 )
    real      fk_amp ( nt_fft/2+1 )

    integer   it_top, it_bot
    integer   it_lay, lrt_lay
    real      v0_lay
    real      t0_inp_0, t0_out_0

! for fk migration use a single velocity
      if ( mig_type ( 1:2 ) .eq. 'FK' ) then

        lrt_lay = 1

! for cascaded velocity use them all
      else    ! if ( mig_type ( 1:2 ) .eq. 'FK' ) then

        lrt_lay = nt_lay

      end if    ! if ( mig_type ( 1:2 ) .eq. 'FK' ) then

! loop over the layers.
! conventional f-k migration has only 1 layer so nt_lay should = 1,
! and the velocity will be rv_lay ( 1 ).

      it_bot = 0

! cycle over cascade layers
      do it_lay = 1 , max ( 1, lrt_lay-1 )

        if ( lrt_lay .eq. 1 ) then

          v0_lay = rv_prc * rv_lay ( 1 )
          it_top =  1
          it_bot = nt_out

        else    ! if ( lrt_lay .eq. 1 ) then

          call pstmig_image_fk_velocity ( it_lay, lrt_lay, rt_lay, rv_lay, &
     it_top, it_bot, nt_out, dt_out, v0_lay, rv_prc )

        end if    ! if ( lrt_lay .eq. 1 ) then

! time origin
       if ( it_lay .eq. 1 ) then

          t0_inp_0 = t0_inp
          t0_out_0 = t0_out

       else    ! if ( it_lay .eq. 1 ) then

          t0_inp_0 = 0.
          t0_out_0 = 0.

       end if    ! if ( it_lay .eq. 1 ) then

! migrate this layer using velocity = v0_lay
        if ( v0_lay .gt. 0. .and. it_top .le. nt_out ) then

          call pstmig_image_fk_layer ( &
                                       opt_dir, dap, rw_add, &
                                       v0_lay, rw_fac, &
                                       scale_fft, &
                                       ix_fft, nx_fft, dx_fft, &
                                       iy_fft, ny_fft, dy_fft, &
                                       nt_inp-it_top+1, t0_inp_0, dt_inp, &
                                       nt_out-it_top+1, t0_out_0, dt_out, &
                                       nf_mig, f0_mig, df_mig, &
                                       nt_fft, fft_obj_tw, fft_obj_wt, &
                                       n1_snc, n2_snc, fk_snc, &
                                       rt_tp_kp ( :, it_top ), dq_tp_kp, &
                                       di_fp_kp, di_fn_kp, &
                                       do_fp_kp, do_fn_kp, &
                                       rt_tp_kn ( :, it_top ), dq_tp_kn, &
                                       di_fp_kn, di_fn_kn, &
                                       do_fp_kn, do_fn_kn, &
                                       fk_map, fk_scp, fk_scn, fk_amp &
                                     )

        end if    ! if ( v0_lay .gt. 0. .and. it_top .le. nt_out ) then

      end do    ! do it_lay = 1 , max ( 1, lrt_lay-1 )

    return

  end subroutine pstmig_image_fk
  !
  subroutine pstmig_image_fk_velocity ( &
                                        it_lay, nt_lay, rt_lay, rv_lay, &
                                        it_top, it_bot, nt_out, dt_out, &
                                        v0_lay, rv_prc &
                                      )
! given the dix velocity function defined by t ( n ), v ( n ), n>0
! return the velocity v0_lay for the it_lay layer of a cascaded migration.
! f is a multiplicative scale factor to apply to the input velocity.
! nt_out, dt_out define the grid for the migration calculation.
! it_top, it_bot are the grid interval where v0_lay applies
! note: by convention v ( i+1 ) is from t ( i ) to t ( i+1 )

    integer   it_lay, nt_lay
    real      rt_lay ( nt_lay ), rv_lay ( nt_lay )

    integer   it_top, it_bot
    integer   nt_out
    real      dt_out
    real      v0_lay, rv_prc

    integer   kt_lay, jt_lay
    real      vsqsum

      v0_lay = 0.
      it_top = 1
      it_bot = nt_out

      if ( nt_lay .le. 0 ) then

        go to 1

       else if ( nt_lay .eq. 1 ) then

       v0_lay = rv_lay ( 1 )

      else    ! if ( nt_lay .le. 0 ) then

        it_bot = 0
        vsqsum = 0.

        do kt_lay = 1 , max ( 1, nt_lay-1 )

          jt_lay = kt_lay + 1

          ! nonzero start in v
          if ( rt_lay ( 1 ) .gt. dt_out .and. nt_lay .gt. 1 &
         .and. rv_lay ( 1 ) .ne. rv_lay ( 2 ) ) jt_lay = kt_lay 

          it_top = it_bot + 1    !head to tail intervals

          if ( kt_lay .eq. 1 ) then

           it_bot = it_top + rt_lay ( jt_lay ) / dt_out

          else    ! if ( kt_lay .eq. 1 ) then

           it_bot = &
           it_top + ( rt_lay ( jt_lay ) - rt_lay ( jt_lay-1 ) ) / dt_out

          end if    ! if ( kt_lay .eq. 1 ) then

          it_bot = min ( it_bot, nt_out )
          v0_lay = max ( 0., ( rv_prc*rv_lay ( jt_lay )**2-vsqsum ) )

          if ( v0_lay .gt. 0. ) then

           v0_lay = sqrt ( v0_lay )

          else    ! if ( v0_lay .gt. 0. ) then

           go to 1

          end if    ! if ( v0_lay .gt. 0. ) then

          vsqsum = vsqsum + v0_lay * v0_lay

          if ( kt_lay .eq. it_lay ) go to 1

        end do    ! do kt_lay = 1 , max ( 1, nt_lay-1 )

      end if    ! if ( nt_lay .le. 0 ) then

    1 continue

    return
  end subroutine pstmig_image_fk_velocity
  !
  subroutine pstmig_image_fk_layer ( &
                                     opt_dir, dap, rw_add, &
                                     v0_lay, rw_fac, &
                                     scale_fft, &
                                     ix_fft, nx_fft, dx_fft, &
                                     iy_fft, ny_fft, dy_fft, &
                                     nt_inp, t0_inp, dt_inp, &
                                     nt_out, t0_out, dt_out, &
                                     nf_mig, f0_mig, df_mig, &
                                     nt_fft, fft_obj_tw, fft_obj_wt, &
                                     n1_snc, n2_snc, fk_snc, &
                                     rt_tp_kp, dq_tp_kp, &
                                     di_fp_kp, di_fn_kp, &
                                     do_fp_kp, do_fn_kp, &
                                     rt_tp_kn, dq_tp_kn, &
                                     di_fp_kn, di_fn_kn, &
                                     do_fp_kn, do_fn_kn, &
                                     fk_map, fk_scp, fk_scn, fk_amp &
                                   )
! migrate rt_tp_kp and rt_tp_kn through a single velocity

    character(len=*) :: opt_dir

    real      dap
    logical   rw_add
    real      scale_fft

    real      v0_lay, rw_fac

    integer   ix_fft, nx_fft
    real      dx_fft

    integer   iy_fft, ny_fft
    real      dy_fft

    integer   nt_inp
    real      t0_inp, dt_inp

    integer   nt_out
    real      t0_out, dt_out

    integer   nf_mig
    real      f0_mig, df_mig

    integer   nt_fft
    type ( fft_struct ), pointer :: fft_obj_tw
    type ( fft_struct ), pointer :: fft_obj_wt

    integer   n1_snc, n2_snc
    real      fk_snc ( 2, n1_snc, n2_snc )

    real      rt_tp_kp ( 2, nt_inp )
    real      dq_tp_kp ( 2, nt_inp )
    real      di_fp_kp ( nt_fft/2+1 ), di_fn_kp ( nt_fft/2+1 )
    real      do_fp_kp ( nt_fft/2+1 ), do_fn_kp ( nt_fft/2+1 )
    real      rt_tp_kn ( 2, nt_inp )
    real      dq_tp_kn ( 2, nt_inp )
    real      di_fp_kn ( nt_fft/2+1 ), di_fn_kn ( nt_fft/2+1 )
    real      do_fp_kn ( nt_fft/2+1 ), do_fn_kn ( nt_fft/2+1 )
    integer   fk_map ( nt_fft/2+1 )
    integer   fk_scp ( nt_fft/2+1 )
    integer   fk_scn ( nt_fft/2+1 )
    real      fk_amp ( nt_fft/2+1 )

    integer   iw_nyq, iw_top, iw_bot
    real      r0_amp
    !
    ! copy the input data to the time work arrays
    !
    dq_tp_kn = 0.
    !
    dq_tp_kp ( :, 1:nt_inp ) = rt_tp_kp ( : ,1:nt_inp )
    !
    if ( .not. pstmig_zero_or_nyq ( ix_fft, nx_fft ) ) &
    dq_tp_kn ( :, 1:nt_inp ) = rt_tp_kn ( :, 1:nt_inp )
    !
    ! transform the input data from time to frequency
    ! fourier transform the output from frequency to time
    !
    call pstmig_time_freq_fft ( &
                                +1, scale_fft, &
                                nt_inp, t0_inp, dt_inp, &
                                nf_mig, f0_mig, df_mig, &
                                nt_fft, fft_obj_tw, fft_obj_wt, &
                                dq_tp_kp, di_fp_kp, di_fn_kp, &
                                dq_tp_kn, di_fp_kn, di_fn_kn &
                              )

    !
    ! compute the mapping functions
    !
    call pstmig_image_fk_map_compute ( &
                                       opt_dir, dap, rw_add, &
                                       v0_lay, rw_fac, r0_amp, &
                                       iw_nyq, iw_top, iw_bot, &
                                       ix_fft, nx_fft, dx_fft, &
                                       iy_fft, ny_fft, dy_fft, &
                                       nf_mig, f0_mig, df_mig, &
                                       nt_fft, &
                                       n1_snc, n2_snc, &
                                       fk_map, fk_scp, fk_scn, fk_amp &
                                     )
    !
    ! map from dinp to dout to do the migration
    !
    call pstmig_image_fk_map_apply ( &
                                     iw_top, iw_bot, nt_fft, &
                                     n1_snc, n2_snc, fk_snc, &
                                     di_fp_kp, di_fn_kp, &
                                     di_fp_kn, di_fn_kn, &
                                     do_fp_kp, do_fn_kp, &
                                     do_fp_kn, do_fn_kn, &
                                     fk_map, fk_scp, fk_scn, fk_amp &
                                   )
    !
    ! add back the original spectrum from the evanescent region of the f-k plane
    !
    if ( rw_add ) &
    call pstmig_image_fk_rw_add ( &
                                  r0_amp, &
                                  iw_nyq, nt_fft, &
                                  di_fp_kp, di_fn_kp, &
                                  di_fp_kn, di_fn_kn, &
                                  do_fp_kp, do_fn_kp, &
                                  do_fp_kn, do_fn_kn &
                                )
    !
    ! transform the migrated data from frequency to time
    !
    call pstmig_time_freq_fft ( &
                                -1, scale_fft, &
                                nt_out, t0_out, dt_out, &
                                nf_mig, f0_mig, df_mig, &
                                nt_fft, fft_obj_tw, fft_obj_wt, &
                                dq_tp_kp, do_fp_kp, do_fn_kp, &
                                dq_tp_kn, do_fp_kn, do_fn_kn &
                              )
    !
    ! copy the output to the trace arrays
    !
    rt_tp_kp ( :, 1:nt_out ) = dq_tp_kp ( :, 1:nt_out )
    !
    if ( .not. pstmig_zero_or_nyq ( ix_fft, nx_fft ) ) &
    rt_tp_kn ( :, 1:nt_out ) = dq_tp_kn ( :, 1:nt_out )
    !
    return
    !
  end subroutine pstmig_image_fk_layer
  !
  subroutine pstmig_image_fk_rw_add ( &
                                     r0_amp, &
                                     iw_nyq, nt_fft, &
                                     di_fp_kp, di_fn_kp, &
                                     di_fp_kn, di_fn_kn, &
                                     do_fp_kp, do_fn_kp, &
                                     do_fp_kn, do_fn_kn &
                                   )
    !
    ! add back the evanescent zone from frequency 1 to iw_nyq
    !
    real      r0_amp
    integer   iw_nyq, nt_fft
    real      di_fp_kp ( 2, nt_fft/2+1 ), di_fn_kp ( 2, nt_fft/2+1 )
    real      di_fp_kn ( 2, nt_fft/2+1 ), di_fn_kn ( 2, nt_fft/2+1 )
    real      do_fp_kp ( 2, nt_fft/2+1 ), do_fn_kp ( 2, nt_fft/2+1 )
    real      do_fp_kn ( 2, nt_fft/2+1 ), do_fn_kn ( 2, nt_fft/2+1 )
    !
    integer   iw_pos_1, iw_pos_2
    integer   iw_neg_1, iw_neg_2
    !
    ! positive frequency limits
    !
    iw_pos_1 = 1
    iw_pos_2 = iw_nyq
    !
    ! negative frequency limits
    !
    iw_neg_1 = 1
    iw_neg_2 = min ( nt_fft/2, iw_nyq )
    !
    ! positive wavenumber, positive frequency
    !
    do_fp_kp ( :, iw_pos_1:iw_pos_2 ) = &
    do_fp_kp ( :, iw_pos_1:iw_pos_2 ) + &
    di_fp_kp ( :, iw_pos_1:iw_pos_2 ) * r0_amp
    !
    ! negative wavenumber, positive frequency
    !
    do_fp_kn ( :, iw_pos_1:iw_pos_2 ) = &
    do_fp_kn ( :, iw_pos_1:iw_pos_2 ) + &
    di_fp_kn ( :, iw_pos_1:iw_pos_2 ) * r0_amp
    !
    ! positive wavenumber, negative frequency
    !
    do_fn_kp ( :, iw_neg_1:iw_neg_2 ) = &
    do_fn_kp ( :, iw_neg_1:iw_neg_2 ) + &
    di_fn_kp ( :, iw_neg_1:iw_neg_2 ) * r0_amp
    !
    ! negative wavenumber, negative frequency
    !
    do_fn_kn ( :, iw_neg_1:iw_neg_2 ) = &
    do_fn_kn ( :, iw_neg_1:iw_neg_2 ) + &
    di_fn_kn ( :, iw_neg_1:iw_neg_2 ) * r0_amp
    !
    return
    !
  end subroutine pstmig_image_fk_rw_add
  !
  subroutine pstmig_image_fk_map_compute ( &
                                           opt_dir, dap, rw_add, &
                                           v0_lay, rw_fac, r0_amp, &
                                           iw_nyq, iw_top, iw_bot, &
                                           ix_fft, nx_fft, dx_fft, &
                                           iy_fft, ny_fft, dy_fft, &
                                           nf_mig, f0_mig, df_mig, &
                                           nt_fft, &
                                           n1_snc, n2_snc, &
                                           fk_map, fk_scp, fk_scn, fk_amp &
                                         )
! compute the mapping function

    character(len=*) :: opt_dir

    real      dap
    logical :: rw_add
    real      v0_lay, rw_fac, r0_amp

    integer   iw_nyq, iw_top, iw_bot

    integer   ix_fft, nx_fft
    real      dx_fft

    integer   iy_fft, ny_fft
    real      dy_fft

    integer   nf_mig
    real      f0_mig, df_mig

    integer   nt_fft

    integer   n1_snc, n2_snc

! automatic arrays from stack - f90
    integer   fk_map ( nt_fft/2+1 )
    integer   fk_scp ( nt_fft/2+1 )
    integer   fk_scn ( nt_fft/2+1 )
    real      fk_amp ( nt_fft/2+1 )
    real      rw_inp ( nt_fft/2+1 )    ! automatic array
    real      rw_amp ( nt_fft/2+1 )    ! automatic array

    real      w2, w2i, w1w2i
    real      r0_map
    real      hv0, hsq
    real      w2_hsq_k_sq
    real      rw_tmp
    real      rw_eps, r_eps

    real      rk_loc, k_sq
    real      dw_mig, w0_nyq
    real      rw_out

    integer   iw_out
    real      f2_snc

    integer   nw_tpr, iw_tpr
    real      aw_tpr


    real                               :: kx_loc
    real                               :: ky_loc

    ! compute the sinc interpolation factors
    f2_snc = float ( n2_snc )
    ! dx_wvn = 2.0*pi/ ( ( nx_fft-1 )*dx_fft )    
    ! dy_wvn = 2.0*pi/ ( ( ny_fft-1 )*dy_fft )
    call pstmig_compute_k1 ( ix_fft, nx_fft, dx_fft, kx_loc )
    call pstmig_compute_k1 ( iy_fft, ny_fft, dy_fft, ky_loc )
    rk_loc = sqrt ( kx_loc**2 + ky_loc**2 )     ! total wavenumber
    ! compute the frequency incrment
      rw_eps  = 1.e-6
      r_eps  = 1.0001                 ! frequency epsilon
      dw_mig = df_mig * 2. * pi       ! frequency increment in radians / sec
      w0_nyq = dw_mig * nt_fft / 2    ! nyquist frequency
! compute the stolt frequency factors
      w2     = 2. - rw_fac
      w2i    = 1. / w2
      w1w2i  = ( 1.0-rw_fac )/ ( 2.0-rw_fac )
! compute frequency stretch and migration operator
      hv0  = 0.5 * v0_lay
      hsq = hv0**2
      k_sq = rk_loc**2
      w2_hsq_k_sq = w2*hsq*k_sq
      iw_top = 1
      if ( w0_nyq**2-hsq*k_sq .gt. 0. ) then
        iw_bot = sqrt ( w0_nyq**2-hsq*k_sq )/dw_mig + 1 ! max allowed rw_out
      else    ! if ( w0_nyq**2-hsq*k_sq .gt. 0. ) then
        iw_bot  = 0
      end if    ! if ( w0_nyq**2-hsq*k_sq .gt. 0. ) then
      iw_bot = min ( iw_bot, nt_fft/2+1 )
      if ( string_upper_compare ( opt_dir, 'INVERSE' ) ) iw_bot = nt_fft/2+1
iw_nyq = max ( 2, min ( int ( hv0*rk_loc/dw_mig )+1, nt_fft/2+1 ) )!min inp freq
      nw_tpr = 20                   ! frequency taper length
      aw_tpr = 1. / nw_tpr          ! frequency taper amplitude
      iw_tpr = max ( 1, iw_bot-nw_tpr ) ! frequency taper start
      if ( string_upper_compare ( opt_dir, 'INVERSE' ) ) fk_map ( 1 ) = nt_fft
      if ( .not. string_upper_compare ( opt_dir, 'INVERSE' ) ) then
        do iw_out = iw_top , iw_bot
rw_out = ( iw_out - 1 ) * dw_mig                      ! output frequency
rw_tmp = sqrt ( rw_out*rw_out + w2_hsq_k_sq )
rw_inp ( iw_out ) = rw_out * w1w2i + w2i * rw_tmp        ! input frequency
rw_amp ( iw_out ) = rw_out / max ( rw_eps, w2 * rw_tmp )      ! amplitude term
        end do    ! do iw_out = iw_top , iw_bot
      else    ! if ( .not. string_upper_compare ( opt_dir, 'INVERSE' ) ) then
        do iw_out = iw_top , iw_bot
          rw_out = ( iw_out - 1 ) * dw_mig                 ! output frequency
          rw_inp ( iw_out ) = rw_out*rw_out - w2_hsq_k_sq  ! input frequency
          if ( rw_inp ( iw_out ) .gt. 0 ) then
rw_inp ( iw_out ) = sqrt ( rw_inp ( iw_out ) ) ! input frequency
rw_amp ( iw_out ) = rw_inp ( iw_out ) / max ( rw_eps, rw_out ) ! amplitude term
          else    ! if ( rw_out .gt. 0 ) then
rw_inp ( iw_out ) = dw_mig * nt_fft ! input frequency
rw_amp ( iw_out ) = 1.              ! amplitude term
          end if    ! if ( rw_out .gt. 0 ) then
        end do    ! do iw_out = iw_top , iw_bot
      end if    ! if ( .not. string_upper_compare ( opt_dir, 'INVERSE' ) ) then
      do iw_out = iw_top , iw_bot
        rw_out = ( iw_out - 1 ) * dw_mig            ! output frequency
        r0_map = rw_inp ( iw_out ) / dw_mig + r_eps    ! grid point
r0_amp = ( 1. - aw_tpr*max ( 0, iw_out-iw_tpr ) ) * rw_amp ( iw_out )**dap
! input frequency index
        fk_map ( iw_out ) = r0_map
! sinc interpolation index
fk_scp ( iw_out ) = nint ( ( f2_snc-1 )*(     ( r0_map-fk_map ( iw_out ) ) )+1.)
fk_scn ( iw_out ) = nint ( ( f2_snc-1 )*( 1.- ( r0_map-fk_map ( iw_out ) ) )+1.)
! amplitude term
        if ( string_upper_compare ( opt_dir, 'FORWARD' ) ) then
          fk_amp ( iw_out ) = r0_amp
        else    ! if ( string_upper_compare ( opt_dir, 'FORWARD' ) ) then
          fk_amp ( iw_out ) = 1. / max ( .2, r0_amp )
        end if    ! if ( string_upper_compare ( opt_dir, 'FORWARD' ) ) then
      end do    ! do iw_out = iw_top , iw_bot
! handle zero frequency separately
      if ( iw_top .eq. 1 ) then
        fk_map ( iw_top ) = int ( hv0 * rk_loc / dw_mig + r_eps )
        fk_scp ( iw_top ) = 1
        fk_scn ( iw_top ) = 1
        fk_amp ( iw_top ) = 1.
      end if    ! if ( iw_top .eq. 1 ) then
! handle nyquist frequency separately
      if ( iw_bot .eq. nt_fft/2+1 ) then
        fk_map ( iw_bot ) = nt_fft/2+1
        fk_scp ( iw_bot ) = 1
        fk_scn ( iw_bot ) = 1
        fk_amp ( iw_bot ) = 0.
      end if    ! if ( iw_bot .eq. nt_fft/2+1 ) then
! compute the evanescent replacement amplitude
      r0_amp = max ( .2, min ( 1., fk_amp ( min ( iw_nyq, iw_bot ) ) ) )
    return
  end subroutine pstmig_image_fk_map_compute
  !
  subroutine pstmig_image_fk_map_apply ( &
                                         iw_top, iw_bot, nt_fft, &
                                         n1_snc, n2_snc, fk_snc, &
                                         di_fp_kp, di_fn_kp, &
                                         di_fp_kn, di_fn_kn, &
                                         do_fp_kp, do_fn_kp, &
                                         do_fp_kn, do_fn_kn, &
                                         fk_map, fk_scp, fk_scn, fk_amp &
                                       )
! map dinp into dout

    integer   iw_top, iw_bot
    integer   nt_fft

    integer   n1_snc, n2_snc
    real      fk_snc ( 2, n1_snc, n2_snc )

! automatic arrays from stack - f90
    real      di_fp_kp ( 2, nt_fft/2+1 ), di_fn_kp ( 2, nt_fft/2+1 )
    real      do_fp_kp ( 2, nt_fft/2+1 ), do_fn_kp ( 2, nt_fft/2+1 )

    real      di_fp_kn ( 2, nt_fft/2+1 ), di_fn_kn ( 2, nt_fft/2+1 )
    real      do_fp_kn ( 2, nt_fft/2+1 ), do_fn_kn ( 2, nt_fft/2+1 )

    integer   fk_map ( nt_fft/2+1 )
    integer   fk_scp ( nt_fft/2+1 )
    integer   fk_scn ( nt_fft/2+1 )
    real      fk_amp ( nt_fft/2+1 )

    integer   iw_inp, iw_out, jw_inp
    integer   i1_snc
    integer   j1_snc
    real      f2_snc

! apply the mapping function

! initialize the positive wavenumber positive frequency output
      do_fp_kp = 0.

! initialize the positive wavenumber negative frequency output
      do_fn_kp = 0.

! initialize the negative wavenumber positive frequency output
      do_fp_kn = 0.

! initialize the negative wavenumber negative frequency output
      do_fn_kn = 0.

! interpolate from dinp to dout to do the migration

! convolutional sum to do sinc interpolation for output kz.
! for a given kz we need the input data at w ( kz ).
! interpolate to w ( kz ) = ( fk_map ( kz )-1 )*dw + fk_scp ( j ( kz ) )
! the remainder or error in the mapping is rounded to the nearest
! 1./n2_snc of a dw. the sinc interpolator has been precomputed and
! saved in the array fk_snc ( n1_snc, n2_snc ).
! it will interpolate to the nearest 1./n2_snc of a dw with operator length n1_s
      f2_snc = n2_snc

      do i1_snc = 1 , n1_snc

        j1_snc = i1_snc - n1_snc / 2

! process positive frequencies
        do iw_out = iw_top , iw_bot

          iw_inp = fk_map ( iw_out ) + j1_snc
          jw_inp = fk_map ( iw_out ) - j1_snc + 1

          if ( iw_inp .ge. 1 .and. iw_inp .le. nt_fft/2+1 ) then
! positive wavenumber, positive frequency
! real
            do_fp_kp ( 1, iw_out ) = &
            do_fp_kp ( 1, iw_out ) &
          + di_fp_kp ( 1, iw_inp ) * fk_snc ( 1, i1_snc, fk_scp ( iw_out ) ) &
          - di_fp_kp ( 2, iw_inp ) * fk_snc ( 2, i1_snc, fk_scp ( iw_out ) )

! imaginary
            do_fp_kp ( 2, iw_out ) = &
            do_fp_kp ( 2, iw_out ) &
          + di_fp_kp ( 1, iw_inp ) * fk_snc ( 2, i1_snc, fk_scp ( iw_out ) ) &
          + di_fp_kp ( 2, iw_inp ) * fk_snc ( 1, i1_snc, fk_scp ( iw_out ) )

!            do_fp_kp ( iw_out ) = do_fp_kp ( iw_out ) &
!                             + di_fp_kp ( iw_inp ) &
!                             * fk_snc ( i1_snc, fk_scp ( iw_out ) )

! negative wavenumber, positive frequency
! real
            do_fp_kn ( 1, iw_out ) = &
            do_fp_kn ( 1, iw_out ) &
          + di_fp_kn ( 1, iw_inp ) * fk_snc ( 1, i1_snc, fk_scp ( iw_out ) ) &
          - di_fp_kn ( 2, iw_inp ) * fk_snc ( 2, i1_snc, fk_scp ( iw_out ) )

! imaginary
            do_fp_kn ( 2, iw_out ) = &
            do_fp_kn ( 2, iw_out ) &
          + di_fp_kn ( 1, iw_inp ) * fk_snc ( 2, i1_snc, fk_scp ( iw_out ) ) &
          + di_fp_kn ( 2, iw_inp ) * fk_snc ( 1, i1_snc, fk_scp ( iw_out ) )

!            do_fp_kn ( iw_out ) = do_fp_kn ( iw_out ) &
!                             + di_fp_kn ( iw_inp ) &
!                             * fk_snc ( i1_snc, fk_scp ( iw_out ) )

          end if    ! if ( iw_inp .ge. 1 .and. iw_inp .le. nt_fft/2+1 ) then

          if ( jw_inp .ge. 1 .and. jw_inp .le. nt_fft/2+1 ) then

! positive wavenumber, negative frequency
! real
            do_fn_kp ( 1, iw_out ) = &
            do_fn_kp ( 1, iw_out ) &
          + di_fn_kp ( 1, jw_inp ) * fk_snc ( 1, i1_snc, fk_scn ( iw_out ) ) &
          - di_fn_kp ( 2, jw_inp ) * fk_snc ( 2, i1_snc, fk_scn ( iw_out ) )

! imaginary
            do_fn_kp ( 2, iw_out ) = &
            do_fn_kp ( 2, iw_out ) &
          + di_fn_kp ( 1, jw_inp ) * fk_snc ( 2, i1_snc, fk_scn ( iw_out ) ) &
          + di_fn_kp ( 2, jw_inp ) * fk_snc ( 1, i1_snc, fk_scn ( iw_out ) )

!            do_fn_kp ( iw_out ) = do_fn_kp ( iw_out ) &
!                             + di_fn_kp ( jw_inp ) &
!                             * fk_snc ( i1_snc, fk_scn ( iw_out ) )

! negative wavenumber, negative frequency
! real
            do_fn_kn ( 1, iw_out ) = &
            do_fn_kn ( 1, iw_out ) &
          + di_fn_kn ( 1, jw_inp ) * fk_snc ( 1, i1_snc, fk_scn ( iw_out ) ) &
          - di_fn_kn ( 2, jw_inp ) * fk_snc ( 2, i1_snc, fk_scn ( iw_out ) )

! imaginary
            do_fn_kn ( 2, iw_out ) = &
            do_fn_kn ( 2, iw_out ) &
          + di_fn_kn ( 1, jw_inp ) * fk_snc ( 2, i1_snc, fk_scn ( iw_out ) ) &
          + di_fn_kn ( 2, jw_inp ) * fk_snc ( 1, i1_snc, fk_scn ( iw_out ) )

!            do_fn_kn ( iw_out ) = do_fn_kn ( iw_out ) &
!                             + di_fn_kn ( jw_inp ) &
!                             * fk_snc ( i1_snc, fk_scn ( iw_out ) )

          end if    ! if ( jw_inp .ge. 1 .and. jw_inp .le. nt_fft/2+1 ) then

        end do    ! do iw_out = iw_top , iw_bot

      end do    ! do i1_snc = 1 , n1_snc
!   apply the amplitude scaling
! positive wavenumber, positive frequency
    do_fp_kp ( 1, iw_top:iw_bot ) = &
    do_fp_kp ( 1, iw_top:iw_bot ) * fk_amp ( iw_top:iw_bot )

    do_fp_kp ( 2, iw_top:iw_bot ) = &
    do_fp_kp ( 2, iw_top:iw_bot ) * fk_amp ( iw_top:iw_bot )

! positive wavenumber, negative frequency
    do_fn_kp ( 1, iw_top:iw_bot ) = &
    do_fn_kp ( 1, iw_top:iw_bot ) * fk_amp ( iw_top:iw_bot )

    do_fn_kp ( 2, iw_top:iw_bot ) = &
    do_fn_kp ( 2, iw_top:iw_bot ) * fk_amp ( iw_top:iw_bot )

! negative wavenumber, positive frequency
    do_fp_kn ( 1, iw_top:iw_bot ) = &
    do_fp_kn ( 1, iw_top:iw_bot ) * fk_amp ( iw_top:iw_bot )

    do_fp_kn ( 2, iw_top:iw_bot ) = &
    do_fp_kn ( 2, iw_top:iw_bot ) * fk_amp ( iw_top:iw_bot )

    ! negative wavenumber, negative frequency
    do_fn_kn ( 1, iw_top:iw_bot ) = &
    do_fn_kn ( 1, iw_top:iw_bot ) * fk_amp ( iw_top:iw_bot )

    do_fn_kn ( 2, iw_top:iw_bot ) = &
    do_fn_kn ( 2, iw_top:iw_bot ) * fk_amp ( iw_top:iw_bot )

    return

  end subroutine pstmig_image_fk_map_apply
  !
  subroutine pstmig_mcfft ( &
                            i_dir, &
                            nx_trp, nx_mig, nx_fft, &
                            mt_buf, nh_buf, nt_buf, tr_buf, &
                            i_err &
                          )

! take spatial fft including interpolation

    integer   i_dir
    integer   nx_trp, nx_mig, nx_fft


    integer   mt_buf, nh_buf, nt_buf
    real      tr_buf ( mt_buf, nx_fft )

    integer   i_err

    real      scale_fft

      i_err = 0    ! initialize error flag

! spatial fft scale
      scale_fft = 1. / sqrt ( float ( nx_fft ) )

! interpolate from nx_mig to nx_out traces
      if ( i_dir .gt. 0 ) &
      call pstmig_horizontal_interp ( &
                                      nx_trp, nx_mig, &
                                      mt_buf, nh_buf, nt_buf, tr_buf, &
                                      i_err &
                                    )
      if ( i_err .ne. 0 ) go to 998

      ! take fft from t, kx_loc, y to t, x, y for this line
      !
      ! - take complex to complex fft in the x direction of nt slices
      !
      !   fft_sign  = -1 forward transform , 1 - inverse transform
      !   fft_scale = scale for fft
      !   nx_fft    = number of points in fft
      !   ix_do_1   = first point to fft
      !   nx_do     = number of vectors to fft
      !   x_inp     = input vectors
      !   ix_inp_1  = stride between elements of a single input vector
      !   ix_inp_2  = stride between input vectors
      !   x_out     = output vectors
      !   ix_out_1  = stride between elements of a single output vector
      !   ix_out_2  = stride between output vectors
      !
      call matfun_mcfft ( &
                          fft_sign  = i_dir*pstmig_xk_fft(), &
                          nx_fft    = nx_fft,                 &
                          ix_do_1   = nh_buf+1,               &
                          nx_do     = nt_buf,                 &
                          fft_scale = scale_fft,              &
                          x_inp     = tr_buf,                 &
                          ix_inp_1  = mt_buf/2,               &
                          ix_inp_2  = 1,                      &
                          x_out     = tr_buf,                 &
                          ix_out_1  = mt_buf/2,               &
                          ix_out_2  = 1,                      &
                          i_err     = i_err                   &
                        )
      if ( i_err .ne. 0 ) go to 997

! decimate from ( nx_mig-1 )*nx_trp+1 to nx_mig traces
      if ( i_dir .lt. 0 ) &
      call pstmig_horizontal_decimate ( &
                                        nx_trp, nx_mig, &
                                        mt_buf, nh_buf, nt_buf, tr_buf &
                                      )
      if ( i_err .ne. 0 ) go to 998

    return

997 continue
      if ( pcpsx_i_pel() .eq. 0 ) &
     write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_mcfft ", &
     & /, " during matfun_mcfft " &
     & )' )
      go to 999

998 continue
      if ( pcpsx_i_pel() .eq. 0 ) &
     write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_mcfft ", &
     & /, " during pstmig_horizontal_interp " &
     & )' )
      go to 999

999 continue
      if ( pcpsx_i_pel() .eq. 0 ) &
     write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_mcfft ", &
     & /, " nx_trp=", i8, " nx_mig=", i8, " nx_fft=", i8, &
     & /, " mt_buf=", i8, " nh_buf=", i8, " nt_buf=", i8 &
     & )' ) &
       nx_trp, nx_mig, nx_fft, &
     mt_buf, nh_buf, nt_buf
    i_err = -1
    return

  end subroutine pstmig_mcfft
  !
  subroutine pstmig_horizontal_decimate ( &
                                          nx_trp, nx_mig, &
                                          mt_buf, nh_buf, nt_buf, tr_buf &
                                        )
! decimate a set of complex traces from ( nx_mig-1 )*nx_trp+1 to nx_mig

    integer   nx_trp, nx_mig

    integer   mt_buf, nh_buf, nt_buf
    real      tr_buf ( 2, mt_buf/2, * )

    integer   ix_inp, ix_mig

! do nothing for nx_trp = 1
      if ( nx_trp .eq. 1 ) then

      else    !       if ( nx_trp .eq. 1 ) then

! make sure the number of input and output traces is well defined
        do ix_mig = 1 , nx_mig

! input location
          ix_inp = ( ix_mig - 1 ) * nx_trp + 1

! shift the trace
          tr_buf ( :, nh_buf+1:nh_buf+nt_buf, ix_mig ) = &
          tr_buf ( :, nh_buf+1:nh_buf+nt_buf, ix_inp )

        end do    ! do ix_mig = 1 , nx_mig

      end if    ! if ( nx_trp .eq. 1 ) then

    return

  end subroutine pstmig_horizontal_decimate
  !
  subroutine pstmig_horizontal_interp ( &
                                        nx_trp, nx_mig, &
                                        mt_buf, nh_buf, nt_buf, tr_buf, &
                                        i_err &
                                      )
! Interpolate nx_mig complex input traces
! to ( nx_mig-1 )*nx_trp+1 complex output traces.
!
! currently, 04-07-98 this uses a horizontal spline interpolation ala' SPIT.
! In the future this will be replaced with an f-x interpolation ala' Chiu.
!
! At input the original input traces occupy locations 1, 2, 3, ... , nx_mig
! within tr_buf.
!
! At output these original traces have been shifted to locations
! 1, nx_trp*1+1, nx_trp*2+1, ... , nx_trp* ( nx_mig-1 )+1
! and the interpolated traces filled in between.
!
! Each pair of original input traces will have nx_trp-1 interpolated
! traces bewteen them.
!
! input=i, output=o, both=b
! nx_trp   int  i interpolation factor
! nx_mig   int  i number of input traces
! mt_buf   int  i dimension of trace array tr_buf
! nt_buf   int  i number of input time samples in traces
! tr_buf   real b trace array - traces within tr_buf are complex
! i_err    int  o error flag, normal ( i_err=0 ), error ( i_err=-1 )
!
! Note there is information in tr_buf beyond the trace limit of
! 2*nt_buf and the buffer length mt_buf that should be shifted
! from the original input trace to the final output trace location
! but that should not be otherwise altered.  The information in the
! interpolated traces in this location can be left at 0.

    integer   nx_trp, nx_mig

    integer   mt_buf, nh_buf, nt_buf
    real      tr_buf ( 2, mt_buf/2, * )

    integer   i_err

    integer   ix_trp, mx_trp
    integer   ix_inp, ix_out, nx_out
    real      x0_trp, dx_trp

      i_err = 0                      ! initialize error flag
      mx_trp = ( nx_mig-1 )* ( nx_trp-1 ) ! total number of interpolated traces
      dx_trp = 1.                    ! input trace spaceing
      x0_trp = 10.                   ! spline interpolation window width

! do nothing for nx_mig = nx_out
      if ( nx_trp .eq. 1 ) then

! nx_mig < nx_out interpolate from  nx_mig to nx_out traces
      else    ! if ( nx_trp .eq. 1 ) then

! number of output traces
        nx_out = ( nx_mig - 1 ) * nx_trp + 1

! initalize the extra output traces to zero
        tr_buf ( :, nh_buf+1:nh_buf+nt_buf, nx_mig+1:nx_out ) = 0.

! shift the input traces to the output trace locations
! go from nx_mig to 1 so we can do this in place
! input locations 1, 2, 3, ..., nx_mig are shifted to output locations
! 1, nx_trp+1, nx_trp*2+1, ... , nx_trp* ( nx_mig-1 )+1
! the original input locations are zeroed if the trace is really moved
        ix_out = nx_out

        do ix_inp = nx_mig , 1 , -1

! shift the original input trace to its output trace location
          tr_buf ( :, nh_buf+1:nh_buf+nt_buf, ix_out ) = &
          tr_buf ( :, nh_buf+1:nh_buf+nt_buf, ix_inp )

! zero the original input trace location
          if ( ix_inp .ne. ix_out ) &
          tr_buf ( :, nh_buf+1:nh_buf+nt_buf, ix_inp ) = 0.

! decrement the output trace counter
          ix_out = ix_out - nx_trp

        end do    ! do ix_inp = nx_mig , 1 , -1

! cycle over output traces in a multitasked loop
! the original traces are in locations
! 1, nx_trp+1, nx_trp*2+1, ... , nx_trp* ( nx_mig-1 )+1
! the interpolated traces will be in the gaps
        do ix_trp = 1 , mx_trp

          ix_out = ix_trp + ( ix_trp-1 ) / ( nx_trp-1 ) + 1

! interpolate this output trace from all input traces
          call pstmig_horizontal_interp_1 ( &
     ix_out, x0_trp, dx_trp, &
       nx_trp, nx_mig, &
     mt_buf, nh_buf, nt_buf, tr_buf, &
     & tr_buf ( :, :, ix_out ) &
     & )

        end do    ! do ix_trp = 1 , mx_trp

      end if    ! if ( nx_trp .eq. 1 ) then

! currently there is no possible erro condition
      if ( i_err .ne. 0 ) go to 999

    return

999 continue
      if ( pcpsx_i_pel() .eq. 0 ) &
     write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_horizontal_interp ", &
     & /, " nx_trp=", i8, " nx_mig=", i8, &
     & /, " mt_buf=", i8, " nt_buf=", i8 &
     & )' ) &
       nx_trp, nx_mig, &
     mt_buf, nt_buf
    i_err = -1
    return

  end subroutine pstmig_horizontal_interp
  !
  subroutine pstmig_horizontal_interp_1 ( &
                                          ix_out, x0_trp, dx_trp, &
                                          nx_trp, nx_mig, &
                                          mt_buf, nh_buf, nt_buf, tr_buf, &
                                          tr_sum &
                                        )
! interpolate a single complex trace at output location x_out
! from nx_mig input traces located at
! 1, nx_trp+1, nx_trp*2+1, ( nx_mig-1 )*nx_trp+1
! input=i, output=o, both=b
! ix_out     int  i output trace node location - may not be an input location
! x0_trp     real i spline interpolation window width
! dx_trp     real i input trace spaceing
! nx_trp     int  i interpolation factor
! nx_mig     int  i number of input traces
! nt_buf     int  i number of input time samples in traces
! mt_buf     int  i dimension of trace array tr_buf
! tr_buf     real i  input trace array - traces within tr_buf are complex
! tr_sum     cmpl o output trace array - traces within tr_sum are complex

    integer   ix_out
    real      x0_trp, dx_trp
    integer   nx_trp, nx_mig

    integer   mt_buf, nh_buf, nt_buf
    real      tr_buf ( 2, mt_buf/2, * )
    real      tr_sum ( 2, mt_buf/2 )


    integer   ix_inp, jx_inp
    real      x_inp, x_out
    real      w0_sum

! real output location from 1 to ( nx_mig-1 )*nx_trp+1
      x_out = ix_out

! intialize the output trace and weights, note nt_buf for complex trace
      tr_sum ( :, nh_buf+1:nh_buf+nt_buf ) = 0.
      w0_sum = 0.

! add each input trace into the output trace location
      do jx_inp = 1 , nx_mig

        ix_inp = ( jx_inp - 1 ) * nx_trp + 1
        x_inp = ix_inp

! sum this input trace, tr_buf into the spline sum, tr_sum and weight, w0_sum
! note tr_buf and tr_sum are complex so we sum nt_buf samples
        call pstmig_horizontal_interp_sum ( &
     & x0_trp, dx_trp, x_inp, x_out, &
     mt_buf, nh_buf, nt_buf, tr_buf ( :, :, ix_inp ), tr_sum, w0_sum )

      end do    ! do ix_inp = 1 , nx_mig

! scale this spline sum, tr_sum by the weight, w0_sum
! note tr_sum is complex so we scale nt_buf samples
      if ( w0_sum .ne. 0. ) tr_sum ( :, nh_buf+1:nh_buf+nt_buf ) = &
                            tr_sum ( :, nh_buf+1:nh_buf+nt_buf ) / w0_sum

    return

  end subroutine pstmig_horizontal_interp_1
  !
  subroutine pstmig_horizontal_interp_sum ( &
                                            x0_trp, dx_trp, x_inp, x_out, &
                                            mt_buf, nh_buf, nt_buf, &
                                            tr_buf, tr_sum, w0_sum &
                                          )
! sum a weighted real input trace into an real output trace
! input=i, output=o, both=b
! x0_trp     real i spline interpolation window width
! dx_trp     real i input trace spaceing
!   x_inp     real i  input trace location
!   x_out     real i output trace location
! mt_buf     int  i number of input time samples in traces
! nh_buf     int  i number of input time samples in traces
! nt_buf     int  i number of input time samples in traces
! tr_buf     real i  input trace array - traces within tr_buf are complex
! tr_sum     real b output trace array - traces within tr_sum are complex
! w0_sum     real b weight sum

    real      x0_trp, dx_trp, x_inp, x_out
    integer   mt_buf, nh_buf, nt_buf
    real      tr_buf ( 2, mt_buf/2 )
    real      tr_sum ( 2, mt_buf/2 )
    real      w0_sum

    real      dx_min, dx_max, dx_int, dx_ham, weight

      dx_min = pi * x0_trp / dx_trp * 1.e-6     ! min separation
      dx_max = pi * x0_trp / dx_trp             ! max separation
      dx_int = pi * ( x_out - x_inp ) / dx_trp    ! vector separation
      dx_ham = pi * ( x_out - x_inp ) / x0_trp    ! hamming cosine

! compute interpolator weight ( sinc windowed with Hamming window )
      if     ( abs ( dx_int ) .lt. dx_min ) then

        weight = 1.

       else if ( abs ( dx_int ) .gt. dx_max ) then

        weight = 0.

      else    ! if     ( abs ( dx_int ) .lt. dx_min ) then

        weight = ( .54 + .46 * cos ( dx_ham ) ) * sin ( dx_int )/dx_int

      end if    ! if     ( abs ( dx_int ) .lt. dx_min ) then

      !print' ( " x_inp=", f10.2, " x_out=", f10.2, " weight=", g12.6 )', &
      !x_inp, x_out, weight

! sum the seighted input vector into the output vector
      if ( weight .ne. 0. ) then

        w0_sum = w0_sum + weight

        tr_sum ( :, nh_buf+1:nh_buf+nt_buf ) = &
        tr_sum ( :, nh_buf+1:nh_buf+nt_buf ) &
      + tr_buf ( :, nh_buf+1:nh_buf+nt_buf ) * weight

      end if    ! if ( weight .ne. 0. ) then

    return
    !
  end subroutine pstmig_horizontal_interp_sum
  !
  subroutine pstmig_open_disk_n ( &
                                  ipn, nx_fil, ny_fil, &
                                  lu_x_y, fn_x_y, i_zero, &
                                  nx_til, ny_dsk, nt_dsk, &
                                  i_err &
                                )
    !
    integer,             intent(in   ) :: ipn
    integer,             intent(in   ) :: nx_fil
    integer,             intent(in   ) :: ny_fil
    integer,             intent(inout) :: lu_x_y ( :, : )
    character(len=*),    intent(inout) :: fn_x_y
    integer,             intent(in   ) :: i_zero
    integer,             intent(in   ) :: nx_til
    integer,             intent(in   ) :: ny_dsk
    integer,             intent(in   ) :: nt_dsk
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    integer                            :: ix_fil
    integer                            :: iy_fil
    !
    do_ix_fil : do ix_fil = 1 , nx_fil
      !
      do_iy_fil : do iy_fil = 1 , ny_fil
        !
        ! construct the file name
        !
        call pstmig_open_disk_1 ( &
                                  ipn, ix_fil, iy_fil, &
                                  lu_x_y ( ix_fil, iy_fil ), fn_x_y, i_zero, &
                                  nx_til, ny_dsk, nt_dsk, &
                                  i_err &
                                )
        !
        if ( i_err .ne. 0 ) go to 999
        !
      end do do_iy_fil
      !
    end do do_ix_fil
    !
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_open_disk_n " &
    & )' )
    !
    return
    !
  end subroutine pstmig_open_disk_n
  !
  subroutine pstmig_open_disk_1 ( &
                                  ipn, ix_fil, iy_fil, &
                                  lu_x_y, fn_x_y, i_zero, &
                                  nx_til, ny_dsk, nt_dsk, &
                                  i_err &
                                )

    !
    integer,             intent(in   ) :: ipn
    integer,             intent(in   ) :: ix_fil
    integer,             intent(in   ) :: iy_fil
    integer,             intent(inout) :: lu_x_y 
    character(len=*),    intent(inout) :: fn_x_y
    integer,             intent(in   ) :: i_zero
    integer,             intent(in   ) :: nx_til
    integer,             intent(in   ) :: ny_dsk
    integer,             intent(in   ) :: nt_dsk
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    character ( len=filename_length )  :: file_name
    !
    ! construct the file name
    !
    call pario_name_2 ( fn_x_y, file_name, ix_fil, iy_fil )
    !
    ! open the file
    !
    call pstmig_open_disk ( &
                            ipn, lu_x_y, file_name, i_zero, &
                            nx_til, ny_dsk, nt_dsk, &
                            i_err &
                          )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_open_disk_1 " &
    & )' )
    !
    return
    !
  end subroutine pstmig_open_disk_1
  !
  subroutine pstmig_open_disk ( &
                                ipn, lu_x_y, fn_x_y, i_zero, &
                                nx_til, ny_dsk, nt_dsk, &
                                i_err &
                              )
    !
    integer,             intent(in   ) :: ipn
    integer,             intent(inout) :: lu_x_y 
    character(len=*),    intent(inout) :: fn_x_y
    integer,             intent(in   ) :: i_zero
    integer,             intent(in   ) :: nx_til
    integer,             intent(in   ) :: ny_dsk
    integer,             intent(in   ) :: nt_dsk
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !

    character ( len=3 )                :: fn_stat

    !
    i_err   = 0
    !
    ! for the first pe open with fn_stat then wait for the rest of the pe
    !
    xxif_zero : if ( i_zero .eq. 0 ) then
      !
      fn_stat = 'w+'
      !
      !if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
      !& " pstmig disk space in gigabytes     =", f12.6&
      !& )' ) &
      !( float ( nt_dsk )*float ( nx_til )*float ( ny_dsk )/2. )*8./1.e9
      !
      ! for other pes, wait for pe 0 then open as an old file
      !
    else xxif_zero
      !
      fn_stat = 'r+'
      !
    end if xxif_zero
    !
    ! all pes .ne. 0 wait here until pe 0 opens this file new
    ! the others will open the file old after pe 0 is done
    ! and catch up at the next barrier
    !
    !if ( pcpsx_i_pel() .ne. 0 ) j_err = pcpsx_barrier()
    !
    ! initialize the unit number to a negative
    ! to ensure dskio will recognize the open
    !
    lu_x_y = -1
    !
    ! open the file on pe pcpsx_i_pel without broadcast
    !
    call pario_open ( pcpsx_i_pel(), .false., &
                      lu_x_y, fn_x_y, fn_stat, nt_dsk, i_err )
    !
    !write ( pc_get_lun(), ' ( &
    !& /, " bef zero pe=", i8, &
    !& " nt_dsk=", i8, " i_err=", i8 &
    !& )' ) &
    !pcpsx_i_pel(), nt_dsk, i_err
    !
    ! when pe 0 reaches here the other pes will open the file old.
    ! pe 0 will wait here until the others catch up
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! initialize the file to zero on pe pcpsx_i_pel without broadcast
    !
    if ( i_zero .eq. 0 ) call pario_zero ( &
    pcpsx_i_pel(), .false., lu_x_y, nx_til*ny_dsk, nt_dsk, i_err )
    !
    !write ( pc_get_lun(), ' ( &
    !& /, " aft zero pe=", i8, &
    !& " nx_til=", i8, " i_err=", i8 &
    !& )' ) &
    !pcpsx_i_pel(), nx_til, i_err
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! get the minimum value for the error flags
    !
 1999 continue
    !
    i_err = pcpsx_min_all_reduce ( i_err )
    !
    write ( pc_get_lun(), ' ( &
    & /, " pstmig_open_disk pe=", i8, " lu_x_y=", i8, &
    & " i_err=", i8, " fn_stat=", a, &
    & /, " fn_x_y=", a )' ) &
    pcpsx_i_pel(), lu_x_y, i_err, trim ( fn_stat ), &
    trim ( fn_x_y )
    !
    return
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_open_disk pe=", i8, &
    & /, " during pario_zero " &
    & )' ) &
    pcpsx_i_pel()
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_open_disk pe=", i8, &
    & /, " during pario_open " &
    & )' ) &
    pcpsx_i_pel()
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_open_disk pe=", i8 &
    & )' ) &
    pcpsx_i_pel()
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine pstmig_open_disk
  !
  subroutine pstmig_read_disk ( &
                                nx_fil, ny_fil, lu_x_y, &
                                nx_til, iy_dsk, ny_dsk, &
                                mt_dsk, nt_dsk, tr_buf, &
                                i_err &
                              )
! read y line iy_dsk from nx_fil disk files

    integer   nx_fil, ny_fil
    integer   lu_x_y ( nx_fil, ny_fil )
    integer   nx_til, iy_dsk, ny_dsk
    integer   mt_dsk, nt_dsk
    real      tr_buf ( mt_dsk, nx_til, 1 )
    integer   i_err

    integer   ix_fil, iy_fil, iy_til, iy_inc

      iy_fil = ( iy_dsk - 1 ) / ny_dsk + 1       ! y file containing this y line
      iy_til = mod ( iy_dsk-1, ny_dsk ) * nx_til + 1! first rec for this y line
      iy_inc = 1                                 ! increment between records

      do ix_fil = 1 , nx_fil

! use pe pcpsx_i_pel without broadcast
        call pario_read ( &
     pcpsx_i_pel(), .false., lu_x_y ( ix_fil, iy_fil ), &
       nx_til, iy_til, iy_inc, &
     mt_dsk, nt_dsk, tr_buf ( :, :, ix_fil ), &
     i_err )
        if ( i_err .ne. 0 ) go to 999

      end do    ! do ix_fil = 1 , nx_fil

    return

999 continue
      write ( pc_get_lun(), ' ( &
     & /, " error in pstmig_read_disk pe=", i8, &
     & /, " during pario_read ", &
     & /, " lu_x_y=", i8, &
     & /, " mt_dsk=", i8, " nt_dsk=", i8, &
     & /, " ny_dsk=", i8, " iy_dsk=", i8, &
     & /, " nx_til=", i8, " iy_til=", i8, &
     & /, " nx_fil=", i8, " ny_fil=", i8, &
     & /, " ix_fil=", i8, " iy_fil=", i8 &
     & )' ) &
     pcpsx_i_pel(), &
     lu_x_y ( ix_fil, iy_fil ), &
     mt_dsk, nt_dsk, &
     ny_dsk, iy_dsk, &
     nx_til, iy_til, &
     nx_fil, ny_fil, &
     ix_fil, iy_fil
    i_err = -1
    return

  end subroutine pstmig_read_disk
  !
  subroutine pstmig_write_disk ( &
                                 nx_fil, ny_fil, lu_x_y, &
                                 nx_til, iy_dsk, ny_dsk, &
                                 mt_dsk, nt_dsk, tr_buf, &
                                 i_err &
                               )
! write y line iy_dsk to nx_fil disk files

    integer   nx_fil, ny_fil
    integer   lu_x_y ( nx_fil, ny_fil )

    integer   nx_til, iy_dsk, ny_dsk
    integer   mt_dsk, nt_dsk
    real      tr_buf ( mt_dsk, nx_til, 1 )

    integer   i_err

    integer   ix_fil, iy_fil, iy_til, iy_inc

      iy_fil = ( iy_dsk - 1 ) / ny_dsk + 1       ! y file containing this y line
      iy_til = mod ( iy_dsk-1, ny_dsk ) * nx_til + 1! first rec for this y line
      iy_inc = 1                                 ! increment between records

      do ix_fil = 1 , nx_fil

! use pe pcpsx_i_pel without broadcast
        call pario_write ( &
     pcpsx_i_pel(), .false., lu_x_y ( ix_fil, iy_fil ), &
     nx_til, iy_til, iy_inc, &
     mt_dsk, nt_dsk, tr_buf ( :, :, ix_fil ), &
     i_err )
        if ( i_err .ne. 0 ) go to 999

      end do    ! do ix_fil = 1 , nx_fil

    return

999 continue
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_write_disk pe=", i8, &
    & /, " during pario_write ", &
    & /, " lu_x_y=", i8, &
    & /, " mt_dsk=", i8, " nt_dsk=", i8, &
    & /, " ny_dsk=", i8, " iy_dsk=", i8, &
    & /, " nx_til=", i8, " iy_til=", i8, &
    & /, " nx_fil=", i8, " ny_fil=", i8, &
    & /, " ix_fil=", i8, " iy_fil=", i8 &
    & )' ) &
    pcpsx_i_pel(), &
    lu_x_y ( ix_fil, iy_fil ), &
    mt_dsk, nt_dsk, &
    ny_dsk, iy_dsk, &
    nx_til, iy_til, &
    nx_fil, ny_fil, &
    ix_fil, iy_fil
    i_err = -1
    return

  end subroutine pstmig_write_disk
  !
  subroutine pstmig_stretch_setup ( o, i_err )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! - Local variables
    !
    i_err = 0
    !
    ! read the reference function into rt_str, rv_str make sure it is
    ! interpolation polynomial order, -1 = linear interpolation
    !
    ! get space for the stretch velocity
    !
    o%mz_str = 1000
    !
    call memfun_all ( o%rt_str, o%mz_str, 'rt_str', i_err )  ! 
    !
    call memfun_all ( o%rv_str, o%mz_str, 'rv_str', i_err )  ! 
    !
    if ( i_err .ne. 0 ) go to 998
    !
    o%str_order  = -1
    !
    ! get the stretch trace lengths
    !
    call pstmig_stretch_setup_length ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! initialize the stretch process numbers
    !
    o%i_stretch_1 = 0
    !
    o%i_stretch_2 = 0
    !
    ! get the stretch flag apply stretch yes-i_stretch_0=0, no-i_stretch_0=1
    !
    xxif_none : &
    if ( .not. string_upper_compare ( o%path_str, 'NONE' ) ) then
      !
      o%i_stretch_0 = 0
      !
    else  xxif_none 
      !
      o%i_stretch_0 = 1
      !
    end if xxif_none 
    !
    o%nz_str = 1
    !
    o%rt_str ( 1 ) = 1.
    !
    o%rv_str ( 1 ) = 2000.
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & /, " pstmig_stretch_setup ", &
    & /, " i_stretch_0=", i4, " i_stretch_1=", i4, " i_stretch_2=", i4, &
    & /, " nt_inp=", i8, " t0_inp=", g12.6, " t1_inp=", g12.6, &
    & " dt_inp=", g12.6, &
    & /, " nt_mig=", i8, " t0_mig=", g12.6, " t1_mig=", g12.6, &
    & " dt_mig=", g12.6, &
    & /, " ns_inp=", i8, " s0_inp=", g12.6, " s1_inp=", g12.6, &
    & " ds_inp=", g12.6, &
    & /, " ns_out=", i8, " s0_out=", g12.6, " s1_out=", g12.6, &
    & " ds_out=", g12.6 &
    & )' ) &
    o%i_stretch_0, o%i_stretch_1, o%i_stretch_2, &
    o%nt_inp, o%t0_inp, o%t1_inp, o%dt_inp, &
    o%nt_mig, o%t0_mig, o%t1_mig, o%dt_mig, &
    o%ns_inp, o%s0_inp, o%s1_inp, o%ds_inp, &
    o%ns_out, o%s0_out, o%s1_out, o%ds_out
    !
    return
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_stretch_setup ", &
    & /, " during pstmig_stretch_setup_length " &
    & )' )
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_stretch_setup ", &
    & /, " during memory allocation " &
    & )' )
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_stretch_setup " &
    & )' )
    !
    i_err = -1
    !
    return
    !
  end subroutine pstmig_stretch_setup
  !
  subroutine pstmig_stretch_setup_length ( o, i_err )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! - Local variables
    !
    i_err = 0
    !
    ! set the stretch time parameters
    !
    xxif_fk_or_cascaded : &
    if ( o%mig_type ( 1:2 ) .eq. 'FK' &
    .or. o%mig_type ( 1:8 ) .eq. 'CASCADED' ) then
      !
      ! compute the optimal stretch time statistics for fk or cascaded migration
      call pstmig_stretch_setup_fk ( o, i_err )
      !
      if ( i_err .ne. 0 ) go to 998
      !
    else xxif_fk_or_cascaded 
      !
      ! compute the optimal stretch time statistics for phase shift migration
      !
      call pstmig_stretch_setup_phaseshift ( o, i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
    end if xxif_fk_or_cascaded 
    !
    return
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_stretch_setup_length ", &
    & /, " during pstmig_stretch_setup_phaseshift " &
    & )' )
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_stretch_setup_length ", &
    & /, " during pstmig_stretch_setup_fk " &
    & )' )
    !
    go to 999
    !
999 continue
    if ( pcpsx_i_pel() .eq. 0 )  write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_stretch_setup_length " &
    & )' )
    !
    i_err = -1
    !
    return
    !
  end subroutine pstmig_stretch_setup_length
  !
  subroutine pstmig_stretch_setup_fk ( o, i_err )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! - Local variables
    !
    real                                :: t0_tmp
    real                                :: t1_tmp
    real                                :: dt_tmp
    !
    i_err = 0
    !
    ! set the stretch time parameters
    !
    o%t1_inp = ( o%nt_inp - 1 ) * o%dt_inp + o%t0_inp
    o%t1_mig = ( o%nt_mig - 1 ) * o%dt_mig + o%t0_mig
    !
    xxif_none : if ( string_upper_compare ( o%path_str, 'NONE' ) ) then
      !
      o%ns_inp = o%nt_inp
      o%s0_inp = o%t0_inp
      o%s1_inp = o%t1_inp
      o%ds_inp = o%dt_inp
      !
      o%ns_out = o%nt_mig
      o%s1_out = o%t1_mig
      o%s0_out = o%t0_mig
      o%ds_out = o%dt_mig
      !
    else xxif_none
      !
      t0_tmp = min ( o%t0_inp, o%t0_mig  )
      t1_tmp = max ( o%t1_inp, o%t1_mig  )
      dt_tmp = max ( o%dt_inp, o%dt_mig  )
      !
      o%s0_inp = t0_tmp
      o%s1_inp = t1_tmp * 2.
      o%ds_inp = dt_tmp / 2.
      o%ns_inp = nint ( ( o%s1_inp - o%s0_inp ) / o%ds_inp ) + 1
      !
      o%ns_out = o%nt_mig
      o%s0_out = o%t0_mig
      o%s1_out = o%t1_mig
      o%ds_out = o%dt_mig
      !
    end if xxif_none
    !
    if ( i_err .ne. 0 ) go to 999
    !
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_stretch_setup_fk " &
    & )' )
    !
    i_err = -1
    !
    return
    !
  end subroutine pstmig_stretch_setup_fk
  !
  subroutine pstmig_stretch_setup_phaseshift ( o, i_err )
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! - Local variables
    !
    real                                :: t0_tmp
    real                                :: t1_tmp
    real                                :: dt_tmp
    !
    i_err = 0
    !
    ! set the stretch time parameters
    !
    o%t1_inp = ( o%nt_inp - 1 ) * o%dt_inp + o%t0_inp
    o%t1_mig = ( o%nt_mig - 1 ) * o%dt_mig + o%t0_mig
    !
    xxif_none : if ( string_upper_compare ( o%path_str, 'NONE' ) ) then
      !
      o%ns_inp = o%nt_inp
      o%s0_inp = o%t0_inp
      o%s1_inp = o%t1_inp
      o%ds_inp = o%dt_inp
      !
      o%ns_out = o%nt_mig
      o%s0_out = o%t0_mig
      o%s1_out = o%t1_mig
      o%ds_out = o%dt_mig
      !
    else xxif_none
      !
      t0_tmp = min ( o%t0_inp, o%t0_mig  )
      t1_tmp = max ( o%t1_inp, o%t1_mig  )
      dt_tmp = max ( o%dt_inp, o%dt_mig  )
      !
      o%s0_inp = t0_tmp
      o%s1_inp = t1_tmp * 2.
      o%ds_inp = dt_tmp / 2.
      o%ns_inp = nint ( ( o%s1_inp - o%s0_inp ) / o%ds_inp ) + 1
      !
      o%ns_out = o%nt_mig
      o%s0_out = o%t0_mig
      o%s1_out = o%t1_mig
      o%ds_out = o%dt_mig
      !
    end if xxif_none
    !
    if ( i_err .ne. 0 ) go to 999
    !
    return
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_stretch_setup_phaseshift " &
    & )' )
    i_err = -1
    !
    return
    !
  end subroutine pstmig_stretch_setup_phaseshift
  !
  subroutine pstmig_compute_k1 ( ix_fft, nx_fft, dx_fft, kx_loc )
    !
    ! compute wavenumber value from its index and the spatial characteristics
    !
    integer,             intent(in   ) :: ix_fft
    integer,             intent(in   ) :: nx_fft
    real,                intent(in   ) :: dx_fft
    real,                intent(inout) :: kx_loc
    !
    real                               :: dx_wvn
    !
    dx_wvn = pstmig_dx_wvn ( nx_fft, dx_fft )
    !
    xxif_nyquist : if ( ix_fft .le. nx_fft/2+1 ) then
      !
      kx_loc = ( ix_fft - 1 ) * dx_wvn
      !
    else xxif_nyquist 
      !
      kx_loc = - ( nx_fft - ix_fft + 1 ) * dx_wvn
      !
    end if xxif_nyquist 
    !
    return
    !
  end subroutine pstmig_compute_k1
  !
  subroutine pstmig_compute_k2 ( &
                                 ix_fft, nx_fft, dx_fft, kx_loc, &
                                 iy_fft, ny_fft, dy_fft, ky_loc, &
                                 rk_loc &
                               )
    !
    ! compute the x, y wavenumber
    ! compute the minimum frequency and sample rate in radians
    ! compute rk_loc = kx_loc**2 + ky_loc**2
    !
    integer,             intent(in   ) :: ix_fft
    integer,             intent(in   ) :: nx_fft
    real,                intent(in   ) :: dx_fft
    real,                intent(inout) :: kx_loc
    !
    integer,             intent(in   ) :: iy_fft
    integer,             intent(in   ) :: ny_fft
    real,                intent(in   ) :: dy_fft
    real,                intent(inout) :: ky_loc
    !
    real,                intent(inout) :: rk_loc
    !
    ! compute the x line wavenumber increment
    !
    call pstmig_compute_k1 ( ix_fft, nx_fft, dx_fft, kx_loc )
    !
    ! compute the crossline wavenumber increment
    !
    call pstmig_compute_k1 ( iy_fft, ny_fft, dy_fft, ky_loc )
    !
    rk_loc  = sqrt ( kx_loc**2 + ky_loc**2 )     ! total wavenumber
    !
    return
    !
  end subroutine pstmig_compute_k2
  !
  subroutine pstmig_fft_length ( nx_mig, nx_fft )
    !
    ! compute an fft length nx_fft>=nx_mig
    ! if nx_fft <=0 on input use a power of 2
    ! otherwise use a product of powers of 2, 3, 5
    !
    !
    integer,             intent(in   ) :: nx_mig
    integer,             intent(inout) :: nx_fft
    !
    !
    xxif_nx_fft : if ( nx_fft .lt. 0 .or. nx_fft .lt. nx_mig ) then
      !
      nx_fft = matfun_pow2 ( nx_mig )
      !
    else if ( nx_fft .ne. 1 ) then
      !
      nx_fft = matfun_pown ( nx_fft )
      !
    end if xxif_nx_fft 
    !
    return
    !
  end subroutine pstmig_fft_length
  !
  real function pstmig_dx_wvn ( nx_fft, dx_fft )
    !
    ! compute wavenumber spacing from the spatial characteristics
    !
    integer,             intent(in   ) :: nx_fft
    real,                intent(in   ) :: dx_fft
    !
    ! local variables
    !
    integer                            :: nx_nyq
    real                               :: rx_nyq
    !
    xxif_nx_fft : if ( nx_fft .gt. 1 ) then
      !
      rx_nyq = .5 / dx_fft                           ! nyquist 
      !
      nx_nyq = nx_fft / 2 + 1                        ! num to nyquist
      !
      pstmig_dx_wvn = 2. * pi * rx_nyq / ( nx_nyq - 1 )  ! wavenumber increment
      !
    else xxif_nx_fft 
      !
      pstmig_dx_wvn = 1.                                 ! wavenumber increment
      !
    end if xxif_nx_fft 
    !
    return
    !
  end function pstmig_dx_wvn
  !
  logical function pstmig_zero_or_nyq ( ix_fft, nx_fft )
    !
    ! set pstmig_zero_or_nyq = .true. if ix_fft = 1 or nyquist
    ! set pstmig_zero_or_nyq = .false. otherwise
    !
    integer,             intent(in   ) :: ix_fft
    integer,             intent(in   ) :: nx_fft
    !
    xxif_ix_one_or_nyquist : &
    if ( ix_fft .eq. 1 .or. ix_fft .eq. nx_fft/2+1 ) then
      !
      pstmig_zero_or_nyq = .true.
      !
    else xxif_ix_one_or_nyquist 
      !
      pstmig_zero_or_nyq = .false.
      !
    end if xxif_ix_one_or_nyquist 
    !
    return
    !
  end function pstmig_zero_or_nyq
  !
  integer function pstmig_tw_fft ( )
    !
    ! sign of time to frequency fft
    !
    pstmig_tw_fft = -1
    !
    return
    !
  end function pstmig_tw_fft
  !
  integer function pstmig_xk_fft ( )
    !
    ! sign of space to wavenumber fft
    !
    pstmig_xk_fft = +1
    !
    return
    !
  end function pstmig_xk_fft
  !
  logical function pstmig_rw_add ( )
    !
    ! add the evanescent zone flag 
    !
    pstmig_rw_add = .true.
    !
    return
    !
  end function pstmig_rw_add
  !
  real function pstmig_rw_fac ( )
    !
    ! stoltian factor for non-constant v compensation
    ! 1. is constant v limit
    !
    pstmig_rw_fac = 1.
    !
    return
    !
  end function pstmig_rw_fac
  !
  real function pstmig_rv_prc ( )
    ! fk velocity scaler
    !
    pstmig_rv_prc = 1.
    !
    return
    !
  end function pstmig_rv_prc
  !
  integer function pstmig_find_non_flag ( i1, i2, ix, mx, x, x_flag )
    !
    ! find the index, i_flag within array x whose first value is x_flag
    ! search from i1, i2 incrementing by inc
    !

    !
    integer,             intent(in   ) :: i1
    integer,             intent(in   ) :: i2
    integer,             intent(in   ) :: ix
    integer,             intent(in   ) :: mx
    real,                intent(in   ) :: x ( :, : )
    real,                intent(in   ) :: x_flag
    !
    integer                            :: i_flag

    integer                            :: j
    integer                            :: inc_sign
    real                               :: eps
    !
    i_flag = 0
    !
    eps = abs ( x_flag ) * 1e-10
    !
    if ( eps .eq. 0. ) eps= 1e-10
    !
    inc_sign = isign ( 1, i2-i1 )
    !
    ! find the first non flag value
    !
    do_j : do j = i1 , i2 , inc_sign
      !
      xxif_flag : if ( abs ( x ( ix, j )-x_flag ) .gt. eps ) then
        !
        i_flag = j
        !
        go to 1
        !
      end if xxif_flag 
      !
    end do do_j 
    !
  1 continue
    !
    pstmig_find_non_flag = i_flag
    !
    return
    !
  end function pstmig_find_non_flag
  !
  subroutine pstmig_cpucount_create ( o, i_err )
    !
    ! setup the cpucount structure
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    integer,             intent(inout) :: i_err      ! error flag 0 O.K. -1 err
    !
    ! - Local variables
    !
    i_err = 0
    !
    call cpucount_create ( o%c, 40 )
    !
    call cpucount_add ( o%c, 'pstmig_total',      o%c_pstmig_total,      i_err )
    call cpucount_add ( o%c, 'pstmig_input',      o%c_pstmig_input,      i_err )
    call cpucount_add ( o%c, 'pstmig_output',     o%c_pstmig_output,     i_err )
    call cpucount_add ( o%c, 'pstmig_image',      o%c_pstmig_image,      i_err )
    call cpucount_add ( o%c, 'pstmig_mig_or_mod', o%c_pstmig_mig_or_mod, i_err )
    call cpucount_add ( o%c, 'pstmig_migrate',    o%c_pstmig_migrate,    i_err )
    call cpucount_add ( o%c, 'pstmig_model',      o%c_pstmig_model,      i_err )
    call cpucount_add ( o%c, 'pstmig_migrate_1',  o%c_pstmig_migrate_1,  i_err )
    call cpucount_add ( o%c, 'pstmig_model_1',    o%c_pstmig_model_1,    i_err )
    call cpucount_add ( o%c, 'pstmig_coef',       o%c_pstmig_coef,       i_err )
    call cpucount_add ( o%c, 'pstmig_phase',      o%c_pstmig_phase,      i_err )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    call cpucount_print ( o%c, ' pstmig_cpucount_create ' )
    !
    return
    !
  end subroutine pstmig_cpucount_create 
  !
  subroutine pstmig_mig_or_mod_scale ( o )
    !
    ! compute a dip dependant weighting factor using an average velocity
    ! of ( kz / rw_mig ) ** ( dap ) with a minimum value of .01
    ! using dap and either the new or old algorithm
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    real                               :: rw_eps
    real                               :: adap
    real                               :: a_tmp
    real                               :: b_tmp
    !
    rw_eps = 1.0e-6
    !
    adap = abs ( o%dap )
    !
    a_tmp = .5 * o%rk_loc * o%rv_vel ( 1 ) 
    !
    xxif_rw_loc : if ( o%rw_loc .gt. a_tmp ) then
      !
      b_tmp = sqrt ( o%rw_loc **2 - a_tmp ** 2 ) / max ( rw_eps, o%rw_loc )
      !
      xxif_inverse : if ( string_upper_compare ( o%opt_dir, 'INVERSE' ) ) then 
        !
        o%rp_scl = o%a0_tpr * b_tmp ** adap 
        !
        o%rp_scl = 1.0 / max ( 0.2, o%rp_scl ) ! modeling scaling
        !
      else if ( adap .gt. 1.0005 ) then
        !
        o%rp_scl = o%a0_tpr * b_tmp ** ( adap - 1 ) ! migration scaling
        !
      else xxif_inverse 
        !
        o%rp_scl = 1.0   ! migration scaling
        !
      end if xxif_inverse 
      !
    else xxif_rw_loc 
      !
      o%rp_scl = 1.
      !
    end if xxif_rw_loc 
    !
    return
    !
  end subroutine pstmig_mig_or_mod_scale
  !
  subroutine pstmig_mig_or_mod ( o, tr_buf, i_err )
    !
    ! set up the multi tasked loop for either migration or modeling
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    real,                intent(inout) :: tr_buf ( 2, o%mt_buf/2, * )
    integer,             intent(inout) :: i_err
    !
    ! Local variables
    !
    integer                            :: i1_fft
    !
    ! we will do either all x's if ny=1 or all y'x if ny>1
    !
    ! compute the x, kx_loc, and y, ky_loc, and total, rk_loc wavenumbers
    !
    o%iy_fft = 1
    !
    !print'(" pstmig_mig_or_mod iy_fft=",i8," nx_fft=",i8)', iy_fft, nx_fft
    !
    call cpucount ( o%c, o%c_pstmig_mig_or_mod, 1 )
    !
    i_err = 0
    !
    if ( o%nt_fft .lt. o%nf_mig ) call pc_error ( ' nt_fft error', o%nt_fft )
    !
    ! cycle over the n1_fft wave numbers
    ! for i1_fft values 2 : n1_fft/2 both pos and neg wavenumber values
    ! will be migrated at the same time - these are columns i1_fft and j1_fft
    !
    do_i1_fft : do i1_fft = 1 , o%n1_fft/2+1
      !
      ! fourier transform the + wavenumbers input from time to frequency
      ! migrate or model each of the wavenumbers
      ! the negative 1_fft column
      !
      o%i1_fft = i1_fft
      !
      o%j1_fft = max ( 1, min ( o%n1_fft, o%n1_fft-o%i1_fft+2 ) )! -wavenumber 
      !
      ! compute the inline midpoint wavenumber indices
      !
      xxif_one_y : if ( o%pstmig_one_y ) then
        !
        o%ix_fft = o%i1_fft
        !
        o%iy_fft = o%i2_fft
        !
      else xxif_one_y 
        !
        o%ix_fft = o%i2_fft
        !
        o%iy_fft = o%i1_fft
        !
      end if xxif_one_y 
      !
!print'(" bef mig ix_fft=",i8," iy_fft=",i8," tr=",g12.6,1x,g12.6)',&
!o%i1_fft, o%i2_fft, &
!maxval(abs(tr_buf(1:2, o%mh_inp+1:, o%i1_fft))), &
!maxval(abs(tr_buf(1:2, o%mh_inp+1:, o%j1_fft)))
      !
      ! compute the x, kx_loc, and y, ky_loc, and total, rk_loc wavenumbers
      !
      call pstmig_compute_k2 ( &
                               o%ix_fft, o%nx_fft, o%dx_fft, o%kx_loc, &
                               o%iy_fft, o%ny_fft, o%dy_fft, o%ky_loc, &
                               o%rk_loc &
                             )
      !
!print'(" kx_loc=",g12.6," ky_loc=",g12.6," rk_loc=",g12.6," rp_off=",g12.6)',&
!o%kx_loc, o%ky_loc, o%rk_loc, o%rp_off
      !
      o%pstmig_pos_and_neg = .not. pstmig_zero_or_nyq ( o%i1_fft, o%n1_fft )
      !
      ! for fk or cascaded migration and modeling
      !
      xxif_fk_or_casacded_2 : if ( o%mig_type ( 1:2 ) .eq. 'FK' &
                              .or. o%mig_type ( 1:8 ) .eq. 'CASCADED' ) then
        !
        ! migrate or model the data using fk or cascaded 
        !
        call pstmig_image_fk ( &
                               o, &
                               o%i1_fft, o%n1_fft, o%d1_fft, &
                               o%i2_fft, o%n2_fft, o%d2_fft, &
                               tr_buf ( 1, o%mh_inp+1, o%i1_fft ), &
                               tr_buf ( 1, o%mh_inp+1, o%j1_fft ), &
                               o%mig_type, o%opt_dir, o%dap, &
                               pstmig_rw_add(), &
                               pstmig_rw_fac(), pstmig_rv_prc(), &
                               o%scale_fft, &
                               o%ns_inp, o%s0_inp, o%ds_inp, &
                               o%ns_out, o%s0_out, o%ds_out, &
                               o%nf_mig, o%f0_mig, o%df_mig, &
                               o%nt_fft, o%fft_obj_tw, o%fft_obj_wt, &
                               o%nt_lay, o%rt_lay, o%rv_lay, &
                               o%n1_snc, o%n2_snc, o%fk_snc, &
                               o%mt_buf, o%nt_dim &
                             )
        !
      else if ( string_upper_compare ( o%opt_dir, 'FORWARD' ) ) then 
        !
        ! migrate the data using zero offset phase shift
        !
        call pstmig_migrate ( o, &
                                 tr_buf ( 1, o%mh_inp+1, o%i1_fft ), &
                                 tr_buf ( 1, o%mh_inp+1, o%j1_fft )  &
                            )
        !
      else xxif_fk_or_casacded_2 
        !
        ! forward model the data using zero offset phase shift
        !
        call pstmig_model ( &
                            o, &
                            tr_buf ( 1, o%mh_inp+1, o%i1_fft ), &
                            tr_buf ( 1, o%mh_inp+1, o%j1_fft )  &
                          )
        !
      end if xxif_fk_or_casacded_2 
      !
!print'(" aft mig ix_fft=",i8," iy_fft=",i8," tr=",g12.6,1x,g12.6)',&
!o%i1_fft, o%i2_fft, &
!maxval(abs(tr_buf(1:2, o%mh_inp+1:, o%i1_fft))), &
!maxval(abs(tr_buf(1:2, o%mh_inp+1:, o%j1_fft)))
      !
    end do do_i1_fft
    !
1999 continue
    !
    call cpucount ( o%c, o%c_pstmig_mig_or_mod, 2 )
    !
    return
    !
997 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_mig_or_mod ", &
    & /, " during the frequency to time fft_create " &
    & )' )
    !
    go to 999
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_mig_or_mod ", &
    & /, " during the time to frequency fft_create " &
    & )' )
    !
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in pstmig_mig_or_mod " &
    & )' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine pstmig_mig_or_mod
  !
  subroutine pstmig_mig_or_mod_coef ( o )
    !
    ! compute the coefficients needed for migration and modeling
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    ! Local variables
    !
    call cpucount ( o%c, o%c_pstmig_coef, 1 )
    !
    o%rw_loc = o%rw_mig ( o%if_mig )
    !
    o%vw_loc = o%vw_mig ( o%if_mig )
    !
    o%rp_loc = o%rk_loc * o%vw_loc
    !
    o%px_loc = o%kx_loc * o%vw_loc
    !
    o%py_loc = o%ky_loc * o%vw_loc
    !
    o%rp_src = sqrt ( ( o%px_loc + 2. * o%rp_off ) ** 2 + ( o%py_loc ) ** 2 )
    !
    o%rp_rec = sqrt ( ( o%px_loc - 2. * o%rp_off ) ** 2 + ( o%py_loc ) ** 2 ) 
    !
    o%rk_src = o%rp_src * o%rw_loc
    !
    o%rk_rec = o%rp_rec * o%rw_loc
    !
    o%pstmig_image_frequency = .true. 
    !
    o%pstmig_norm_norm = .false. 
    o%pstmig_norm_turn = .false. 
    o%pstmig_turn_norm = .false. 
    o%pstmig_turn_turn = .false. 
    !
    if ( o%rp_loc .gt. o%dip_lim * o%p1_tab &
    .or. o%rp_src .gt. o%dip_lim * o%p1_tab &
    .or. o%rp_rec .gt. o%dip_lim * o%p1_tab ) &
    o%pstmig_image_frequency = .false.
    !
    xxif_image_frequency : if ( o%pstmig_image_frequency ) then
      !
      ! compute a taper at the end of the frequency window
      ! that starts at 1. at if_mig=o%nf_mig-20 
      ! and goes to 0. at if_mig=o%nf_mig
      !
      o%a0_tpr = 1. - max ( 0., min ( 1., .05 * ( o%if_mig-o%nf_mig+20 ) ) )
      !
      ! compute the source coefficients
      ! for post stack these will be for two way travel
      ! for pre  stack these will be for one way travel
      !
      call pstmig_mig_or_mod_coef_0 ( &
                                      o, &
                                      'src', o%rk_src, o%rp_src, &
                                      o%ip_src_1, o%ip_src_2, &
                                      o%rp_src_1, o%rp_src_2, &
                                      o%ra_src_1, o%ra_src_2, &
                                      o%rt_src_1, o%rt_src_2, &
                                      o%nt_src,   o%iw_src &
                                    )
      !
      ! compute the receiver coefficients
      ! only for pre stack
      !
      if ( o%pstmig_pre_stack ) &
      call pstmig_mig_or_mod_coef_0 ( &
                                      o, &
                                      'rec', o%rk_rec, o%rp_rec, &
                                      o%ip_rec_1, o%ip_rec_2, &
                                      o%rp_rec_1, o%rp_rec_2, &
                                      o%ra_rec_1, o%ra_rec_2, &
                                      o%rt_rec_1, o%rt_rec_2, &
                                      o%nt_rec,   o%iw_rec &
                                    )
      !
      xxif_common_angle : if ( o%pstmig_pre_stack ) then
        !
        o%nt_mid = min ( o%nt_src, o%nt_rec ) ! turning depth index
        !
        o%iw_mid = min ( o%iw_src, o%iw_rec ) ! max frequency index
        !
      else xxif_common_angle 
        !
        o%nt_mid = o%nt_src ! turning depth index
        !
        o%iw_mid = o%iw_src ! max frequency index
        !
      end if xxif_common_angle 
      !
      o%it_top = o%if_lim ( o%if_mig, 1 )  ! top inp time index
      !
      o%it_bot = o%if_lim ( o%if_mig, 2 )  ! bot inp time index
      !
      o%it_bot = min ( o%nt_mid, o%nt_tab, o%it_bot )  ! bot inp time index
      !
      !if ( mod ( o%if_mig, 20 ) .eq. 1 .and. mod ( o%i1_fft, 20 ) .eq. 1 ) &
      !print'(" af=",i5," x=",i5," n=",i5,1x,i5,1x,i5, &
      !& " t=",i5, " b=",i5, " s=", i6)',&
      !o%if_mig, o%i1_fft, &
      !o%nt_mid, o%nt_src, o%nt_rec, o%it_top, o%it_bot, o%ns_out
      !
      !if ( mod ( o%if_mig, 20 ) .eq. 1 .and. mod ( o%i1_fft, 20 ) .eq. 1 ) &
      !print'(" af=",i5," w=",g12.6," k=",g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
      !o%if_mig, o%rw_loc, o%rk_loc, o%kx_loc, o%ky_loc, o%rp_off
      !
      !if ( mod ( o%if_mig, 20 ) .eq. 1 .and. mod ( o%i1_fft, 20 ) .eq. 1 ) &
      !print'(" af=",i5," w=",g12.6," p=",g12.6,1x,g12.6,1x,g12.6,1x,g12.6)',&
      !o%if_mig, o%rw_loc, &
      !o%rp_loc*1.e6, o%rp_src*1.e6, o%rp_rec*1.e6, o%rp_off*1.e6
      !
      ! compute a dip dependant weighting factor using an average velocity
      !
      ! of ( kz / rw_mig ) ** ( o%dap - 1 ) with a minimum value of .01
      !
      call pstmig_mig_or_mod_scale ( o )
      !
      ! compute the flags indicating which waves can be imaged
      !
      if ( &
           ( string_upper_compare ( o%mig_type, 'NORMAL'  ) &
        .or. string_upper_compare ( o%mig_type, 'BOTH'    ) ) &
        ) &
      o%pstmig_norm_norm = .true.
      !
      if ( &
           ( string_upper_compare ( o%mig_type, 'TURNING' ) &
        .or. string_upper_compare ( o%mig_type, 'BOTH'    ) ) &
           .and. o%nt_mid .lt. o%nt_tab &
        ) &
      o%pstmig_turn_turn = .true.
      !
      if ( &
           ( string_upper_compare ( o%mig_type, 'TURNING' ) &
        .or. string_upper_compare ( o%mig_type, 'BOTH'    ) ) &
           .and. o%nt_mid .lt. o%nt_tab &
           .and. o%pstmig_pre_stack &
        ) &
      o%pstmig_norm_turn = .true.
      !
      if ( &
           ( string_upper_compare ( o%mig_type, 'TURNING' ) &
        .or. string_upper_compare ( o%mig_type, 'BOTH'    ) ) &
           .and. o%nt_mid .lt. o%nt_tab &
           .and. o%pstmig_pre_stack &
        ) &
      o%pstmig_turn_norm = .true.
      !
      ! get the top and bottom times this frequency will contribute to
      ! bot time is the min of the turning time, trace length and freq limit
      !
      ! compute the phase cefficients, ac_exp_r and ac_exp_i
      !
      call pstmig_mig_or_mod_compute_phase ( o )
      !
    end if xxif_image_frequency 
    !
    call cpucount ( o%c, o%c_pstmig_coef, 2 )
    !
    return
    !
  end subroutine pstmig_mig_or_mod_coef 
  !
  subroutine pstmig_mig_or_mod_coef_0 ( &
                                        o, &
                                        c_title, rk_mig, rp_loc, &
                                        ip_tab_1, ip_tab_2, &
                                        rp_tab_1, rp_tab_2, &
                                        ra_tab_1, ra_tab_2, &
                                        rt_tab_1, rt_tab_2, &
                                        nt_turn,  iw_turn &
                                      )
    !
    ! compute the two phase shift column indices and ocefficients
    ! for a single p value
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    character(len=*),    intent(in   ) :: c_title
    real,                intent(in   ) :: rk_mig     ! wavenumber
    real,                intent(inout) :: rp_loc     ! ray parameter
    integer,             intent(inout) :: ip_tab_1   ! table index 1
    integer,             intent(inout) :: ip_tab_2   ! table index 2
    real,                intent(inout) :: rp_tab_1   ! table value 1
    real,                intent(inout) :: rp_tab_2   ! table value 2
    real,                intent(inout) :: ra_tab_1   ! amp interp coef 1
    real,                intent(inout) :: ra_tab_2   ! amp interp coef 2
    real,                intent(inout) :: rt_tab_1   ! tim interp coef 1
    real,                intent(inout) :: rt_tab_2   ! tim interp coef 2
    integer,             intent(inout) :: nt_turn    ! turning time index
    integer,             intent(inout) :: iw_turn    ! max frequency index
    !
    real                               :: rp_tmp     ! ray parameter
    !
    rp_loc = rk_mig / o%rw_loc          ! ray parameter
    !
    rp_tmp = min ( .5*rp_loc, o%p1_tab )! ray parameter limited to table range
    !
    ! compute the two phase shift column indices
    !
    ip_tab_1 = min ( o%np_tab, int ( rp_tmp * o%vp_tab ) + 1 )
    !
    rp_tab_1 = ( ip_tab_1 - 1 ) * o%dp_tab
    !
    if ( rp_tab_1 .gt. rp_tmp ) ip_tab_1 = max ( 1, ip_tab_1 - 1 )
    !
    ip_tab_2 = min ( o%np_tab, ip_tab_1 + 1 )
    !
    ! compute the two phase shift column weights 
    ! note the o%dc_exp normalization for rt_tab
    !
    rp_tab_1 = ( ip_tab_1 - 1 ) * o%dp_tab
    !
    rp_tab_2 = ( ip_tab_2 - 1 ) * o%dp_tab
    !
    ! compute the two amplitude interpolation coefficients
    !
    ra_tab_2 = max ( 0., min ( 1., ( rp_tmp - rp_tab_1 ) * o%vp_tab ) )
    !
    ra_tab_1 = 1. - ra_tab_2
    !
    ! compute the two time interpolation coefficients
    ! this includes the frequency, rw_mig and the cosine increment, vc_exp
    !
    rt_tab_1 = ra_tab_1 * o%rw_loc * o%vc_exp 
    !
    rt_tab_2 = ra_tab_2 * o%rw_loc * o%vc_exp 
    !
    if ( rp_tab_1 .gt. rp_tmp &
    .or. rp_tab_2 .lt. rp_tmp &
    ) go to 999
    !
 998 continue
    !
    ! get the vertical turning time index
    !
    nt_turn = min ( o%tu_tab ( ip_tab_1 ), o%tu_tab ( ip_tab_2 ) ) 
    !
    ! compute the curved phase at the vertical turning time
    !
    iw_turn = int ( 2. &
                    * ( rt_tab_1 * o%tim_tab ( nt_turn, ip_tab_1 ) &
                      + rt_tab_2 * o%tim_tab ( nt_turn, ip_tab_2 ) ) )
    !
!if ( mod ( o%if_mig, 20 ) .eq. 1 .and. mod ( o%i1_fft, 20 ) .eq. 1 ) &
!print'(" zf=",i5," x=",i5," n=",i5," ip=",i5,1x,i5," np=",i6,1x,i5,1x,i5)', &
!o%if_mig, o%i1_fft, &
!nt_turn, ip_tab_1, ip_tab_2, o%np_tab, &
!minval ( o%tu_tab ), maxval ( o%tu_tab )
    !
    !if ( rp_tmp .gt. o%p1_tab ) nt_turn = 0
    !
    return
    !
999 continue
    !
    if ( ip_tab_1 .eq. -999 ) &
    print'(" pstmig_mig_or_mod_coef_0 error ", &
    & /, " c_title=", a, &
    & /, " vc_exp=", g12.6, &
    & /, " if_mig=", i12, " ix_fft=", i8," iy_fft=", i8, &
    & /, " rw_mig=", g12.6, " rk_mig=", g12.6, &
    & /, " np_tab=", i12, " p1_tab  =", g12.6, &
    & /, " rp_loc=", g12.6, " rp_tmp=", g12.6, &
    & /, " p1_tab - rp_tmp=", g12.6, &
    & /, " ip_tab_1=", i12,   " ip_tab_2=", i12, &
    & /, " rp_tab_1=", g12.6, " rp_tab_2=", g12.6, &
    & /, " ra_tab_1=", g12.6, " ra_tab_2=", g12.6, &
    & /, " rt_tab_1=", g12.6, " rt_tab_2=", g12.6 &
    & )',&
    trim ( c_title ), &
    o%vc_exp, &
    o%if_mig, o%ix_fft, o%iy_fft, &
    o%rw_loc, rk_mig, &
    o%np_tab, o%p1_tab, &
    rp_loc, rp_tmp, &
    o%p1_tab - rp_tmp, &
    ip_tab_1, ip_tab_2, &
    rp_tab_1, rp_tab_2, &
    ra_tab_1, ra_tab_2, &
    rt_tab_1, rt_tab_2
    !
    go to 998
    !
    !stop
    !
  end subroutine pstmig_mig_or_mod_coef_0 
  !
  subroutine pstmig_migrate ( o, rt_tp_kp, rt_tp_kn )
    !
    ! migrate a single pair of +-wavenumber wavenumbers via phase shift
    ! there are loops for the special case of:
    ! one wavenumber  is  migrated                  - zero or nyquist
    ! two wavenumbers are migrated at the same time - positive and negative
    ! and for each of these:
    !      normal waves only
    !      turning waves only
    !      both normal and turning waves
    !
    ! I = input, O = output , B = both , W = work or temporary
    !
    ! B do_fp_kp = + wavenumber trace - complex  do_fp_kp ( o%nf_mig )
    ! B do_fp_kn = - wavenumber trace - complex  do_fp_kn ( o%nf_mig )
    ! 
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    real,                intent(inout) :: rt_tp_kp ( 2, o%mt_buf )
    real,                intent(inout) :: rt_tp_kn ( 2, o%mt_buf )
    !
    ! local variables
    !
    real                               :: dq_tp_kp ( 2, o%nt_dim )
    real                               :: dq_tp_kn ( 2, o%nt_dim )
    real                               :: do_fp_kp ( 2, o%nf_mig )
    real                               :: do_fn_kp ( 2, o%nf_mig )
    real                               :: do_fp_kn ( 2, o%nf_mig )
    real                               :: do_fn_kn ( 2, o%nf_mig )
    !


    !
    integer                            :: if_mig
    integer                            :: it_out




    real                               :: do_fp_kp_r
    real                               :: do_fn_kp_r
    real                               :: do_fp_kn_r
    real                               :: do_fn_kn_r
    real                               :: do_fp_kp_i
    real                               :: do_fn_kp_i
    real                               :: do_fp_kn_i
    real                               :: do_fn_kn_i
    !
    real                               :: dp_pn_kp_r 
    real                               :: dm_pn_kp_r 
    real                               :: dp_pn_kp_i 
    real                               :: dm_pn_kp_i 
    real                               :: dp_pn_kn_r 
    real                               :: dm_pn_kn_r 
    real                               :: dp_pn_kn_i 
    real                               :: dm_pn_kn_i 
    !
    call cpucount ( o%c, o%c_pstmig_migrate, 1 )
    !
    !if ( pcpsx_i_pel() .eq. 0 ) &
    !write ( pc_get_lun(), ' ( &
    !& /, " pstmig_migrate ", &
    !& /, " ix_fft=", i8, " iy_fft=", i8, &
    !& /, " o%ns_inp=", i8, " o%s0_inp=", g12.6, " o%ds_inp=", g12.6, &
    !& /, " o%ns_out=", i8, " o%s0_out=", g12.6, " o%ds_out=", g12.6, &
    !& /, " o%nt_tab=", i8, " o%t0_tab=", g12.6, " o%dt_tab=", g12.6, &
    !& /, " o%nf_mig=", i8, " o%f0_mig=", g12.6, " o%df_mig=", g12.6, &
    !& /, " o%nt_fft=", i8 &
    !& )' ) &
    !o%ix_fft, o%iy_fft, &
    !o%ns_inp, o%s0_inp, o%ds_inp, &
    !o%ns_out, o%s0_out, o%ds_out, &
    !o%nt_tab, o%t0_tab, o%dt_tab, &
    !o%nf_mig, o%f0_mig, o%df_mig, &
    !o%nt_fft
    !
    ! phase = sign ( w ) * 
    ! integral dt * sqrt ( w**2 - ( kx**2 + ky**2 ) * v**2 / 4. )
    ! for ix_fft = 1 , nx_fft/2+1 migrate a single wavenumber
    ! for other ix_fft migrate  both + and - wavenumbers
    ! copy the input data to the time work arrays
    !
    dq_tp_kn = 0.
    !
    dq_tp_kp ( :, 1:o%ns_inp ) = rt_tp_kp ( : ,1:o%ns_inp )
    !
    if ( o%pstmig_pos_and_neg ) &
    dq_tp_kn ( :, 1:o%ns_inp ) = rt_tp_kn ( :, 1:o%ns_inp )
    !
    ! fourier transform the input from time to frequency
    !
    call pstmig_time_freq_fft ( &
                                +1, o%scale_fft, &
                                o%ns_inp, o%s0_inp, o%ds_inp, &
                                o%nf_mig, o%f0_mig, o%df_mig, &
                                o%nt_fft, o%fft_obj_tw, o%fft_obj_wt, &
                                dq_tp_kp, do_fp_kp, do_fn_kp, &
                                dq_tp_kn, do_fp_kn, do_fn_kn &
                              )
    !
    ! initialize the output imaged time traces
    !
    do_it_out_initialze : do it_out = 1 , o%ns_out
      !
      dq_tp_kp ( 1, it_out ) = 0. ! +wavenumber
      !
      dq_tp_kp ( 2, it_out ) = 0. ! +wavenumber
      !
      dq_tp_kn ( 1, it_out ) = 0. ! -wavenumber
      !
      dq_tp_kn ( 2, it_out ) = 0. ! -wavenumber
      !
    end do do_it_out_initialze 
    !
    do_if_mig_image : do if_mig = 1 , o%nf_mig ! cycle over frequencies
      !
      o%if_mig = if_mig
      !
      call pstmig_mig_or_mod_coef ( o )
      !
      xxif_image_frequency : if ( o%pstmig_image_frequency ) then
        !
        ! scale the frequency data by o%rp_scl
        ! image both positive and negative wavenumbers
        !
        do_fp_kp_r = do_fp_kp ( 1, if_mig ) * o%rp_scl
        !
        do_fp_kp_i = do_fp_kp ( 2, if_mig ) * o%rp_scl
        !
        do_fp_kn_r = do_fp_kn ( 1, if_mig ) * o%rp_scl
        !
        do_fp_kn_i = do_fp_kn ( 2, if_mig ) * o%rp_scl
        !
        do_fn_kp_r = do_fn_kp ( 1, if_mig ) * o%rp_scl
        !
        do_fn_kp_i = do_fn_kp ( 2, if_mig ) * o%rp_scl
        !
        do_fn_kn_r = do_fn_kn ( 1, if_mig ) * o%rp_scl
        !
        do_fn_kn_i = do_fn_kn ( 2, if_mig ) * o%rp_scl
        !
        dp_pn_kp_r = do_fp_kp_r + do_fn_kp_r
        !
        dm_pn_kp_r = do_fp_kp_r - do_fn_kp_r
        !
        dp_pn_kp_i = do_fp_kp_i + do_fn_kp_i
        !
        dm_pn_kp_i = do_fp_kp_i - do_fn_kp_i
        !
        dp_pn_kn_r = do_fp_kn_r + do_fn_kn_r
        !
        dm_pn_kn_r = do_fp_kn_r - do_fn_kn_r
        !
        dp_pn_kn_i = do_fp_kn_i + do_fn_kn_i
        !
        dm_pn_kn_i = do_fp_kn_i - do_fn_kn_i
        !
        ! normal - normal wave
        !
        if ( o%pstmig_norm_norm ) &
        call pstmig_migrate_1 ( &
                                o, &
                                o%it_top,   o%it_bot, &
                                o%nn_exp_r, o%nn_exp_i, &
                                dp_pn_kp_r, dp_pn_kp_i, &
                                dm_pn_kp_r, dm_pn_kp_i, &
                                dp_pn_kn_r, dp_pn_kn_i, &
                                dm_pn_kn_r, dm_pn_kn_i, &
                                dq_tp_kp,   dq_tp_kn &
                              )
        !
        ! normal  - turning wave
        !
        if ( o%pstmig_norm_turn ) &
        call pstmig_migrate_1 ( &
                                o, &
                                o%it_top,   o%it_bot, &
                                o%nt_exp_r, o%nt_exp_i, &
                                dp_pn_kp_r, dp_pn_kp_i, &
                                dm_pn_kp_r, dm_pn_kp_i, &
                                dp_pn_kn_r, dp_pn_kn_i, &
                                dm_pn_kn_r, dm_pn_kn_i, &
                                dq_tp_kp,   dq_tp_kn &
                              )
        !
        ! turning - normal  wave
        !
        if ( o%pstmig_turn_norm ) &
        call pstmig_migrate_1 ( &
                                o, &
                                o%it_top,   o%it_bot, &
                                o%tn_exp_r, o%tn_exp_i, &
                                dp_pn_kp_r, dp_pn_kp_i, &
                                dm_pn_kp_r, dm_pn_kp_i, &
                                dp_pn_kn_r, dp_pn_kn_i, &
                                dm_pn_kn_r, dm_pn_kn_i, &
                                dq_tp_kp,   dq_tp_kn &
                              )
          !
        !
        ! turning - turning wave
        !
        if ( o%pstmig_turn_turn ) &
        call pstmig_migrate_1 ( &
                                o, &
                                o%it_top,   o%it_bot, &
                                o%tt_exp_r, o%tt_exp_i, &
                                dp_pn_kp_r, dp_pn_kp_i, &
                                dm_pn_kp_r, dm_pn_kp_i, &
                                dp_pn_kn_r, dp_pn_kn_i, &
                                dm_pn_kn_r, dm_pn_kn_i, &
                                dq_tp_kp,   dq_tp_kn &
                              )
          !
        !
      end if xxif_image_frequency 
      !
    end do do_if_mig_image 
    !
    ! copy the output to the trace arrays
    !
    rt_tp_kp ( :, 1:o%ns_out ) = dq_tp_kp ( :, 1:o%ns_out )
    !
    if ( o%pstmig_pos_and_neg ) &
    rt_tp_kn ( :, 1:o%ns_out ) = dq_tp_kn ( :, 1:o%ns_out )
    !
    call cpucount ( o%c, o%c_pstmig_migrate, 2 )
    !
    return
    !
  end subroutine pstmig_migrate 
  !
  subroutine pstmig_model ( o, rt_tp_kp, rt_tp_kn )
    !
    ! forward model a single pair of +-wavenumber wavenumbers via phase shift
    ! there are loops for the special case of:
    ! one wavenumber  is  migrated                  - zero or nyquist
    ! two wavenumbers are migrated at the same time - positive and negative
    ! and for each of these:
    !      normal waves only
    !      turning waves only
    !      both normal and turning waves
    !
    ! I = input, O = output , B = both , W = work or temporary
    !
    ! B do_fp_kp = + wavenumber trace - complex  do_fp_kp ( o%nf_mig )
    ! B do_fp_kn = - wavenumber trace - complex  do_fp_kn ( o%nf_mig )
    ! 
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    real,                intent(inout) :: rt_tp_kp ( 2, o%mt_buf )
    real,                intent(inout) :: rt_tp_kn ( 2, o%mt_buf )
    !
    ! local variables
    !
    ! W dq_tp_kp = normal  + wavenumber - complex dq_tp_kp ( o%nt_tab )
    ! W dq_tp_kn = normal  - wavenumber - complex dq_tp_kn ( o%nt_tab )
    ! W dq_tp_kp = turning + wavenumber - complex dq_tp_kp ( o%nt_tab )
    ! W dq_tp_kn = turning - wavenumber - complex dq_tp_kn ( o%nt_tab )
    !
    real                               :: dq_tp_kp ( 2, o%nt_dim )
    real                               :: dq_tp_kn ( 2, o%nt_dim )
    real                               :: do_fp_kp ( 2, o%nf_mig )
    real                               :: do_fn_kp ( 2, o%nf_mig )
    real                               :: do_fp_kn ( 2, o%nf_mig )
    real                               :: do_fn_kn ( 2, o%nf_mig )
    !


    !
    integer                            :: if_mig





    real                               :: do_fp_kp_r
    real                               :: do_fn_kp_r
    real                               :: do_fp_kn_r
    real                               :: do_fn_kn_r
    real                               :: do_fp_kp_i
    real                               :: do_fn_kp_i
    real                               :: do_fp_kn_i
    real                               :: do_fn_kn_i
    !
    call cpucount ( o%c, o%c_pstmig_model, 1 )
    !
    ! phase = sign ( w ) * 
    ! integral dt * sqrt ( w**2 - ( kx_loc**2 + ky_loc**2 ) * v**2 / 4. )
    ! for ix_fft = 1 , nx_fft/2+1 migrate a single kx_loc wavenumber
    ! for other ix_fft migrate  both + and - kx_loc wavenumbers
    ! copy the input data to the time work arrays
    !
    dq_tp_kn = 0.
    !
    dq_tp_kp ( :, 1:o%ns_inp ) = rt_tp_kp ( : ,1:o%ns_inp )
    !
    if ( o%pstmig_pos_and_neg ) &
    dq_tp_kn ( :, 1:o%ns_inp ) = rt_tp_kn ( :, 1:o%ns_inp )
    !
    ! initialize the output frequency arrays to zero
    !
    do_if_mig_initialize : do if_mig = 1 , o%nf_mig
      !
      do_fp_kp ( 1, if_mig ) = 0. ! +wavenumber
      !
      do_fp_kp ( 2, if_mig ) = 0. ! +wavenumber
      !
      do_fp_kn ( 1, if_mig ) = 0. ! +wavenumber
      !
      do_fp_kn ( 2, if_mig ) = 0. ! +wavenumber
      !
      do_fn_kp ( 1, if_mig ) = 0. ! -wavenumber
      !
      do_fn_kp ( 2, if_mig ) = 0. ! -wavenumber
      !
      do_fn_kn ( 1, if_mig ) = 0. ! -wavenumber
      !
      do_fn_kn ( 2, if_mig ) = 0. ! -wavenumber
      !
    end do do_if_mig_initialize 
    !
    do_if_mig_image : do if_mig = 1 , o%nf_mig ! cycle over frequencies
      !
      o%if_mig = if_mig
      !
      call pstmig_mig_or_mod_coef ( o )
      !
      xxif_image_frequency : if ( o%pstmig_image_frequency ) then
        !
        do_fp_kp_r = do_fp_kp ( 1, if_mig )
        !
        do_fp_kp_i = do_fp_kp ( 2, if_mig )
        !
        do_fp_kn_r = do_fp_kn ( 1, if_mig )
        !
        do_fp_kn_i = do_fp_kn ( 2, if_mig )
        !
        do_fn_kp_r = do_fn_kp ( 1, if_mig )
        !
        do_fn_kp_i = do_fn_kp ( 2, if_mig )
        !
        do_fn_kn_r = do_fn_kn ( 1, if_mig )
        !
        do_fn_kn_i = do_fn_kn ( 2, if_mig )
        !
        ! image both positive and negative wavenumbers
        !
        ! normal  - normal  wave
        !
        if ( o%pstmig_norm_norm ) &
        call pstmig_model_1 ( &
                              o, &
                              o%it_top,   o%it_bot, &
                              o%nn_exp_r, o%nn_exp_i, &
                              do_fp_kp_r, do_fp_kp_i, &
                              do_fn_kp_r, do_fn_kp_i, &
                              do_fp_kn_r, do_fp_kn_i, &
                              do_fn_kn_r, do_fn_kn_i, &
                              dq_tp_kp,   dq_tp_kn &
                            )
        !
        ! normal  - turning wave
        !
        if ( o%pstmig_norm_turn ) &
        call pstmig_model_1 ( &
                              o, &
                              o%it_top,   o%it_bot, &
                              o%nt_exp_r, o%nt_exp_i, &
                              do_fp_kp_r, do_fp_kp_i, &
                              do_fn_kp_r, do_fn_kp_i, &
                              do_fp_kn_r, do_fp_kn_i, &
                              do_fn_kn_r, do_fn_kn_i, &
                              dq_tp_kp,   dq_tp_kn &
                            )
        !
        ! turning - normal  wave
        !
        if ( o%pstmig_turn_norm ) &
        call pstmig_model_1 ( &
                              o, &
                              o%it_top,   o%it_bot, &
                              o%tn_exp_r, o%tn_exp_i, &
                              do_fp_kp_r, do_fp_kp_i, &
                              do_fn_kp_r, do_fn_kp_i, &
                              do_fp_kn_r, do_fp_kn_i, &
                              do_fn_kn_r, do_fn_kn_i, &
                              dq_tp_kp,   dq_tp_kn &
                            )
        !
        ! turning - turning wave
        !
        if ( o%pstmig_turn_turn ) &
        call pstmig_model_1 ( &
                              o, &
                              o%it_top,   o%it_bot, &
                              o%tt_exp_r, o%tt_exp_i, &
                              do_fp_kp_r, do_fp_kp_i, &
                              do_fn_kp_r, do_fn_kp_i, &
                              do_fp_kn_r, do_fp_kn_i, &
                              do_fn_kn_r, do_fn_kn_i, &
                              dq_tp_kp,   dq_tp_kn &
                            )
        !
        ! scale by o%rp_scl
        !
        do_fp_kp ( 1, if_mig ) = do_fp_kp_r * o%rp_scl ! +freq +wavenumber
        !
        do_fp_kp ( 2, if_mig ) = do_fp_kp_i * o%rp_scl ! +freq +wavenumber
        !
        do_fp_kn ( 1, if_mig ) = do_fp_kn_r * o%rp_scl ! +freq -wavenumber
        !
        do_fp_kn ( 2, if_mig ) = do_fp_kn_i * o%rp_scl ! +freq -wavenumber
        !
        do_fn_kp ( 1, if_mig ) = do_fn_kp_r * o%rp_scl ! -freq +wavenumber
        !
        do_fn_kp ( 2, if_mig ) = do_fn_kp_i * o%rp_scl ! -freq +wavenumber
        !
        do_fn_kn ( 1, if_mig ) = do_fn_kn_r * o%rp_scl ! -freq -wavenumber
        !
        do_fn_kn ( 2, if_mig ) = do_fn_kn_i * o%rp_scl ! -freq -wavenumber
        !
      end if xxif_image_frequency 
      !
    end do do_if_mig_image 
    !
    ! fourier transform the output from frequency to time
    !
    call pstmig_time_freq_fft ( &
                                -1, o%scale_fft, &
                                o%ns_out, o%s0_out, o%ds_out, &
                                o%nf_mig, o%f0_mig, o%df_mig, &
                                o%nt_fft, o%fft_obj_tw, o%fft_obj_wt, &
                                dq_tp_kp, do_fp_kp, do_fn_kp, &
                                dq_tp_kn, do_fp_kn, do_fn_kn &
                              )
    !
    ! copy the output to the trace arrays
    !
    rt_tp_kp ( :, 1:o%ns_out ) = dq_tp_kp ( :, 1:o%ns_out )
    !
    if ( o%pstmig_pos_and_neg ) &
    rt_tp_kn ( :, 1:o%ns_out ) = dq_tp_kn ( :, 1:o%ns_out )
    !
    call cpucount ( o%c, o%c_pstmig_model, 2 )
    !
    return
    !
  end subroutine pstmig_model 
  !
  subroutine pstmig_migrate_1 ( &
                                o, &
                                it_top,     it_bot, &
                                ac_exp_r,   ac_exp_i, &
                                dp_pn_kp_r, dp_pn_kp_i, &
                                dm_pn_kp_r, dm_pn_kp_i, &
                                dp_pn_kn_r, dp_pn_kn_i, &
                                dm_pn_kn_r, dm_pn_kn_i, &
                                dq_tp_kp,   dq_tp_kn &
                              )
    !
    ! migrate a single pair of +-wavenumber wavenumbers via phase shift
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    integer,             intent(in   ) :: it_top            ! top t index
    integer,             intent(in   ) :: it_bot            ! bot t index
    real,                intent(in   ) :: ac_exp_r ( : )    ! real coef
    real,                intent(in   ) :: ac_exp_i ( : )    ! imag coef
    real,                intent(in   ) :: dp_pn_kp_r        ! +f,+k,real
    real,                intent(in   ) :: dp_pn_kp_i        ! +f,+k,imag
    real,                intent(in   ) :: dm_pn_kp_r        ! -f,+k,real
    real,                intent(in   ) :: dm_pn_kp_i        ! -f,+k,imag
    real,                intent(in   ) :: dp_pn_kn_r        ! +f,-k,real
    real,                intent(in   ) :: dp_pn_kn_i        ! +f,-k,imag
    real,                intent(in   ) :: dm_pn_kn_r        ! -f,-k,real
    real,                intent(in   ) :: dm_pn_kn_i        ! -f,-k,imag
    real,                intent(inout) :: dq_tp_kp ( :, : ) ! kp data
    real,                intent(inout) :: dq_tp_kn ( :, : ) ! kn data
    !
    ! local variables
    !
    integer                            :: it_out            ! time index
    !
    call cpucount ( o%c, o%c_pstmig_migrate_1, 1 )
    !
    !if ( mod ( o%if_mig, 20 ) .eq. 1 .and. mod ( o%i1_fft, 20 ) .eq. 1 ) &
    !print'(" migrate_1 if=",i5," x=",i5," tu=",i5,1x,i5)', &
    !o%if_mig, o%i1_fft, minval ( o%tu_tab ), maxval ( o%tu_tab )
    !
    do_time : do it_out = it_top , it_bot
      !
      ! positive wavenumber, positive and negative frequency
      !
      dq_tp_kp ( 1, it_out ) = dq_tp_kp ( 1, it_out ) &
                             + dp_pn_kp_r * ac_exp_r ( it_out ) &
                             - dm_pn_kp_i * ac_exp_i( it_out ) 
      !
      dq_tp_kp ( 2, it_out ) = dq_tp_kp ( 2, it_out ) &
                             + dp_pn_kp_i * ac_exp_r ( it_out ) &
                             + dm_pn_kp_r * ac_exp_i ( it_out ) 
      !
      ! negative wavenumber, positive and negative frequency
      !
      dq_tp_kn ( 1, it_out ) = dq_tp_kn ( 1, it_out ) &
                             + dp_pn_kn_r * ac_exp_r ( it_out ) &
                             - dm_pn_kn_i * ac_exp_i ( it_out ) 
      !
      dq_tp_kn ( 2, it_out ) = dq_tp_kn ( 2, it_out ) &
                             + dp_pn_kn_i * ac_exp_r ( it_out ) &
                             + dm_pn_kn_r * ac_exp_i ( it_out ) 
      !
    end do do_time
    !
    call cpucount ( o%c, o%c_pstmig_migrate_1, 2 )
    !
    return
    !
  end subroutine pstmig_migrate_1 
  !
  subroutine pstmig_model_1 ( &
                                o, &
                                it_top,     it_bot, &
                                ac_exp_r,   ac_exp_i, &
                                do_fp_kp_r, do_fp_kp_i, &
                                do_fn_kp_r, do_fn_kp_i, &
                                do_fp_kn_r, do_fp_kn_i, &
                                do_fn_kn_r, do_fn_kn_i, &
                                dq_tp_kp,   dq_tp_kn &
                              )
    !
    ! model a single pair of +-wavenumber wavenumbers via phase shift
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    integer,             intent(in   ) :: it_top            ! top t index
    integer,             intent(in   ) :: it_bot            ! bot t index
    real,                intent(in   ) :: ac_exp_r ( : )    ! real coef
    real,                intent(in   ) :: ac_exp_i ( : )    ! imag coef
    real,                intent(inout) :: do_fp_kp_r        ! +f,+k,real
    real,                intent(inout) :: do_fp_kp_i        ! +f,+k,imag
    real,                intent(inout) :: do_fn_kp_r        ! -f,+k,real
    real,                intent(inout) :: do_fn_kp_i        ! -f,+k,imag
    real,                intent(inout) :: do_fp_kn_r        ! +f,-k,real
    real,                intent(inout) :: do_fp_kn_i        ! +f,-k,imag
    real,                intent(inout) :: do_fn_kn_r        ! -f,-k,real
    real,                intent(inout) :: do_fn_kn_i        ! -f,-k,imag
    real,                intent(in   ) :: dq_tp_kp ( :, : ) ! kp data
    real,                intent(in   ) :: dq_tp_kn ( :, : ) ! kn data
    !
    ! local variables
    !
    integer                            :: it_inp            ! time index
    !
    call cpucount ( o%c, o%c_pstmig_model_1, 1 )
    !
    !if ( mod ( o%if_mig, 20 ) .eq. 1 .and. mod ( o%i1_fft, 20 ) .eq. 1 ) &
    !print'(" model_1 if=",i5," x=",i5," tu=",i5,1x,i5)', &
    !o%if_mig, o%i1_fft, minval ( o%tu_tab ), maxval ( o%tu_tab )
    !
    do_time : do it_inp = it_top , it_bot
      !
      ! normal wave
      !
      do_fp_kp_r = do_fp_kp_r &
                 + dq_tp_kp ( 1, it_inp ) * ac_exp_r ( it_inp ) &
                 + dq_tp_kp ( 2, it_inp ) * ac_exp_i ( it_inp )
      !
      do_fp_kp_i = do_fp_kp_i &
                 + dq_tp_kp ( 2, it_inp ) * ac_exp_r ( it_inp ) &
                 - dq_tp_kp ( 1, it_inp ) * ac_exp_i ( it_inp )
      !
      do_fn_kp_r = do_fn_kp_r &
                 + dq_tp_kp ( 1, it_inp ) * ac_exp_r ( it_inp ) &
                 - dq_tp_kp ( 2, it_inp ) * ac_exp_i ( it_inp )
      !
      do_fn_kp_i = do_fn_kp_i &
                 + dq_tp_kp ( 2, it_inp ) * ac_exp_r ( it_inp ) &
                 + dq_tp_kp ( 1, it_inp ) * ac_exp_i ( it_inp )
      !
      do_fp_kn_r = do_fp_kn_r &
                 + dq_tp_kn ( 1, it_inp ) * ac_exp_r ( it_inp ) &
                 + dq_tp_kn ( 2, it_inp ) * ac_exp_i ( it_inp )
      !
      do_fp_kn_i = do_fp_kn_i &
                 + dq_tp_kn ( 2, it_inp ) * ac_exp_r ( it_inp ) &
                 - dq_tp_kn ( 1, it_inp ) * ac_exp_i ( it_inp )
      !
      do_fn_kn_r = do_fn_kn_r &
                 + dq_tp_kn ( 1, it_inp ) * ac_exp_r ( it_inp ) &
                 - dq_tp_kn ( 2, it_inp ) * ac_exp_i ( it_inp )
      !
      do_fn_kn_i = do_fn_kn_i &
                 + dq_tp_kn ( 2, it_inp ) * ac_exp_r ( it_inp ) &
                 + dq_tp_kn ( 1, it_inp ) * ac_exp_i ( it_inp )
      !
    end do do_time
    !
    call cpucount ( o%c, o%c_pstmig_model_1, 2 )
    !
    return
    !
  end subroutine pstmig_model_1 
  !
  subroutine pstmig_mig_or_mod_compute_phase ( o )
    !
    ! compute the two phase shift column indices and ocefficients
    ! for a single p value
    !
    type ( pstmig_struct ),    pointer :: o          ! pstmig structure
    !
    integer                            :: it_mig     ! time index
    integer                            :: nn_mid
    integer                            :: nt_mid
    integer                            :: tn_mid
    integer                            :: tt_mid
    real                               :: t0_mid
    real                               :: a0_mid
    !


    real                               :: t0_src
    real                               :: a0_src
    !


    real                               :: t0_rec
    real                               :: a0_rec
    !
    call cpucount ( o%c, o%c_pstmig_phase, 1 )
    !
    xxif_turn : if ( o%pstmig_norm_turn &
                .or. o%pstmig_turn_norm &
                .or. o%pstmig_turn_turn ) then
      !
      xxif_pstmig_pre_stack_turn : if ( o%pstmig_pre_stack ) then
        !
        do_pre_turn : do it_mig = o%it_top , o%it_bot
          !
          a0_src = o%ra_src_1 * o%amp_tab ( it_mig, o%ip_src_1 ) &
                 + o%ra_src_2 * o%amp_tab ( it_mig, o%ip_src_2 ) 
          !
          a0_rec = o%ra_rec_1 * o%amp_tab ( it_mig, o%ip_rec_1 ) &
                 + o%ra_rec_2 * o%amp_tab ( it_mig, o%ip_rec_2 ) 
          !
          ! compute the scaled phase as an integer
          !
          t0_src = o%rt_src_1 * o%tim_tab ( it_mig, o%ip_src_1 ) &
                 + o%rt_src_2 * o%tim_tab ( it_mig, o%ip_src_2 ) 
          !
          t0_rec = o%rt_rec_1 * o%tim_tab ( it_mig, o%ip_rec_1 ) &
                 + o%rt_rec_2 * o%tim_tab ( it_mig, o%ip_rec_2 ) 
          !
          a0_mid = a0_src * a0_rec
          !
          t0_mid = t0_src + t0_rec + 0.5
          !
          nn_mid = t0_mid
          !
          nn_mid = 1 + iand ( o%nc_exp_m1,                       nn_mid )
          !
          nt_mid = 1 + iand ( o%nc_exp_m1,            o%iw_rec - nn_mid )
          !
          tn_mid = 1 + iand ( o%nc_exp_m1, o%iw_src            - nn_mid )
          !
          tt_mid = 1 + iand ( o%nc_exp_m1, o%iw_src + o%iw_rec - nn_mid )
          !
          ! normal  - normal  wave
          !
          o%nn_exp_r ( it_mig ) = o%ac_exp ( 1, nn_mid ) * a0_mid
          !
          o%nn_exp_i ( it_mig ) = o%ac_exp ( 2, nn_mid ) * a0_mid
          !
          ! normal  - turning wave
          !
          o%nt_exp_r ( it_mig ) = o%ac_exp ( 1, nt_mid ) * a0_mid
          !
          o%nt_exp_i ( it_mig ) = o%ac_exp ( 2, nt_mid ) * a0_mid
          !
          ! turning - normal  wave
          !
          o%tn_exp_r ( it_mig ) = o%ac_exp ( 1, tn_mid ) * a0_mid
          !
          o%tn_exp_i ( it_mig ) = o%ac_exp ( 2, tn_mid ) * a0_mid
          !
          ! turning - turning wave
          !
          o%tt_exp_r ( it_mig ) = o%ac_exp ( 1, tt_mid ) * a0_mid
          !
          o%tt_exp_i ( it_mig ) = o%ac_exp ( 2, tt_mid ) * a0_mid
          !
        end do do_pre_turn 
        !
      else xxif_pstmig_pre_stack_turn 
        !
        do_post_turn : do it_mig = o%it_top , o%it_bot
          !
          a0_mid = ( o%ra_src_1 * o%amp_tab ( it_mig, o%ip_src_1 )   &
                   + o%ra_src_2 * o%amp_tab ( it_mig, o%ip_src_2 ) )
          !
          ! compute the scaled phase as an integer
          !
          t0_mid = ( o%rt_src_1 * o%tim_tab ( it_mig, o%ip_src_1 )   &
                   + o%rt_src_2 * o%tim_tab ( it_mig, o%ip_src_2 ) ) &
                   + 0.5
          !
          nn_mid = t0_mid
          !
          nn_mid = 1 + iand ( o%nc_exp_m1, nn_mid )
          !
          tt_mid = 1 + iand ( o%nc_exp_m1, o%iw_mid - nn_mid )
          !
          ! normal  -  normal wave
          !
          o%nn_exp_r ( it_mig ) = o%ac_exp ( 1, nn_mid ) * a0_mid
          !
          o%nn_exp_i ( it_mig ) = o%ac_exp ( 2, nn_mid ) * a0_mid
          !
          ! turning - turning wave
          !
          o%tt_exp_r ( it_mig ) = o%ac_exp ( 1, tt_mid ) * a0_mid
          !
          o%tt_exp_i ( it_mig ) = o%ac_exp ( 2, tt_mid ) * a0_mid
          !
        end do do_post_turn 
        !
      end if xxif_pstmig_pre_stack_turn 
      !
    else xxif_turn 
      !
      xxif_pstmig_pre_stack_norm : if ( o%pstmig_pre_stack ) then
        !
        do_pre_norm  : do it_mig = o%it_top , o%it_bot
          !
          a0_src = o%ra_src_1 * o%amp_tab ( it_mig, o%ip_src_1 ) &
                 + o%ra_src_2 * o%amp_tab ( it_mig, o%ip_src_2 ) 
          !
          a0_rec = o%ra_rec_1 * o%amp_tab ( it_mig, o%ip_rec_1 ) &
                 + o%ra_rec_2 * o%amp_tab ( it_mig, o%ip_rec_2 ) 
          !
          ! compute the scaled phase as an integer
          !
          t0_src = o%rt_src_1 * o%tim_tab ( it_mig, o%ip_src_1 ) &
                 + o%rt_src_2 * o%tim_tab ( it_mig, o%ip_src_2 ) 
          !
          t0_rec = o%rt_rec_1 * o%tim_tab ( it_mig, o%ip_rec_1 ) &
                 + o%rt_rec_2 * o%tim_tab ( it_mig, o%ip_rec_2 ) 
          !
          a0_mid = a0_src * a0_rec
          !
          t0_mid = t0_src + t0_rec + 0.5
          !
          nn_mid = t0_mid
          !
          nn_mid = 1 + iand ( o%nc_exp_m1,                       nn_mid )
          !
          ! normal  - normal  wave
          !
          o%nn_exp_r ( it_mig ) = o%ac_exp ( 1, nn_mid ) * a0_mid
          !
          o%nn_exp_i ( it_mig ) = o%ac_exp ( 2, nn_mid ) * a0_mid
          !
        end do do_pre_norm  
        !
      else xxif_pstmig_pre_stack_norm 
        !
        do_post_norm  : do it_mig = o%it_top , o%it_bot
          !
          a0_mid = ( o%ra_src_1 * o%amp_tab ( it_mig, o%ip_src_1 )   &
                   + o%ra_src_2 * o%amp_tab ( it_mig, o%ip_src_2 ) )
          !
          ! compute the scaled phase as an integer
          !
          t0_mid = ( o%rt_src_1 * o%tim_tab ( it_mig, o%ip_src_1 )   &
                   + o%rt_src_2 * o%tim_tab ( it_mig, o%ip_src_2 ) ) &
                   + 0.5
          !
          nn_mid = t0_mid
          !
          nn_mid = 1 + iand ( o%nc_exp_m1, nn_mid )
          !
          o%nn_exp_r ( it_mig ) = o%ac_exp ( 1, nn_mid ) * a0_mid
          !
          o%nn_exp_i ( it_mig ) = o%ac_exp ( 2, nn_mid ) * a0_mid
          !
        end do do_post_norm 
        !
      end if xxif_pstmig_pre_stack_norm 
      !
    end if xxif_turn 
    !
    call cpucount ( o%c, o%c_pstmig_phase, 2 )
    !
    return
    !
  end subroutine pstmig_mig_or_mod_compute_phase
  !
end module pstmig_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
