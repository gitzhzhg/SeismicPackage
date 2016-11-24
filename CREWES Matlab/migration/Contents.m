% CREWES migration toolbox
%
% Scripts (working demos)
%  DEMO_RAYMIG: demonstrate migration by raytracing
%  DEMO_FKMIG: fk migration illustrates basic effects and principles
%  DEMO_FK_PHASESHIFT: tutorial on wavefield extrapolation by phase shift
%  DEMO_TIME_MIGRATION: tutorial on time migration
%  DEMO_DEPTHMIG: post stack depth migration with PSPI_STACK
%  VZ_MOD_SIM: script to demo vz_fkmod
%  VZ_FK_SIM: script to demo vz_fkmig
%  THRUST_TIME: demo Kirchhoff time migration of the thrust model
%  TEST_PSPI_SHOT: script to demo prestack depth migration using PSPI_SHOT
%  DEMO_PSPI_STACK: script to demonstrate post-stack depth migration with PSPI_SHOT
%
% Post Stack migration
%  FD15MIG: 15 degree finite-difference time migration (2D)
%  FKMIG: Stolt's fk migration (2D)
%  KIRK: simplified Kirchhoff time migration (2D)
%  KIRK_MIG: full-featured Kirchhoff time migration (2D)
%  KIRK_MIG3D: 3D post stack Kirchhoff time migration
%  PS_MIGT: time migration by Gazdag phase shift (2D)
%  PSPI_STACK: Exploding reflector depth migration by PSPI (2D)
%  PSPI_STACK_TMIG: time migration by phase shift (2D)
%  SPLITSTEPF_MIG: Split-step Fourier depth migration (2D)
%  VZ_FKMIG: fk migration for v(z) (2D)
%  VZ_FKMOD: V(z) modelling by an fk technique (2D)
%  RAYMIG: Interactive normal incidence raytrace migration and modelling
%
% Prestack migration
%  KIRK_SHOT: 2D prestack Kirchhoff migration of a single shot gather
%  KIRK_SHOT3D: 3D prestack Kirchhoff migration of a single shot gather
%  KIRK_SHOT3DFZ: 3D prestack Kirchhoff migration into depth planes. More
%       accurate but slower than KIRK_SHOT3D
%  PSPI_SHOT: depth migration of a single shot using PSPI (2D)
%  PSPI_SHOT_CWAVE: depth migration of converted waves using PSPI (2D)
%
% Codes from RJ Ferguson
% IPS: Isotropic phase shift extrapolation (stationary)
% IPSF: Version of IPS that applies the focussing phase shift only (time migration)
% FX_IPS: Isotropic 80 degree f-x extrapolation
% FX_MIG: prestack 80 degree fx depth migration
% FX_ZERO_MIG: fx zero offset migration
% GAZ_3C_MIG: prestack gazdag depth 3C migration
% GAZ_MIG: prestack gazdag depth migration
% GAZ_ZERO_MIG: generalized screen zero-offset migration
% GS_IPS: Isotropic generalized phase extrapolation
% GS_MIG: prestack generalized screen depth migration
% GS_ZERO_MIG: generalized screen zero-offset migration
% PSPI_3C_MIG: prestack pspi 3c depth migration
% PSPI_IPS: Isotropic pspi extrapolation
% PSPI_IPSF: version of PSPI_IPS used for time migration (focussing only)
% PSPI_MIG: prestack pspi depth migration
% PSPI_ZERO_MIG: pspi zero-offset migration
% SS_3C_MIG: prestack splitestep 3c depth migration
% SS_IPS: Isotropic split step extrapolation
% SS_MIG: prestack splitestep depth migration
% SS_ZERO_MIG: generalized screen zero-offset migration
% UNIQUE_VELS: tba
%
% Utilities
%  CONV45: used by KIRK_MIG for 45 degree phase shift
%  COS_TAPER: used by KIRK_MIG
%  CLININT: Complex-valued linear interpolation (used by FKMIG)
%  CSINCI: complex valued sinc function interpolation
%  STRETCH: 1D t->z conversion or the reverse
%  GREENSEED: calculate a 3D Helmholtz Green's function
%  GREENSEED2: calculate a 2D Helmholtz Green's function
%  SLICE3D: extract a horizontal slice from a 3D volume using interpolation
%  MIGSTACK: Utility to stack depth migrated shot records
%  GAINCC: Utility to gain a depth migrated shot record where the crosscorrelation imaging condition was used
%  BAGAINI: find a piecewise constant velocity approximation for PSPI
%  GAUSSIAN_SMOOTHER: smooth a velocity model by conv2 with a Gaussian
%
% Scripts from RJ Ferguson  (working demos)
% Please see 'Ferguson_Margrave_2005.pdf' or 'Ferguson and Margrave, 2005,
% Planned seismic imaging using explicit one-way operators, Geophysics, V70
% regarding the following scripts:
%
% fx_salt_psdm_script         
% fx_salt_zero_script
% gs_salt_psdm_script
% gs_salt_zero_script
% gaz_salt_3c_psdm_script
% gaz_salt_psdm_script
% gaz_salt_zero_script
% pspi_salt_3c_psdm_script
% pspi_salt_psdm_script
% pspi_salt_zero_script
% ss_salt_3c_psdm_script                 
% ss_salt_psdm_script                 
% ss_salt_zero_script                 
%
