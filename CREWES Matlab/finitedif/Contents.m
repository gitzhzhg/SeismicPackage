% CREWES finite difference seismic modelling
% Acoustic finite difference modelling toolbox
%
% Sample scripts and demo's
% HIGHV_WEDGE ... model an anticline beneath a high velocity wedge
% VZANTICLINE ... model an anticline beneath a v(z) medium
% CHANNEL ... model a channel beneath a few layers
% THRUST_EXPLODE ... exploding reflector model of high-angle thrust
% SYNCLINE ... demo buried syncline with and without reverse-time branch
% DEMO_SHOOTLINE ... demo shooting a 2D line and stacking it
% DEMO_AFD_MAKESNAPSHOTS ... demo making wave propagation snapshots and displaying them in plotgathers
%
% Seismograms and simulations
% AFD_SHOTREC ... makes finite difference shot records
% AFD_SHOTREC_ALT ... alternate version which inputs the wavelet differently
% NOTE: afd_shotrec propagates an impulse and then convolves the wavelet
% with the result. In contrast, afd_shotrec_alt injects the wavelet sample-by-sample 
% at each time step. Mathematically, these should be the same but there are
% numerical differences.
% AFD_EXPLODE ... makes exploding reflector models
% AFD_SHOOTLINE ... shoot a 2D line over a given model
% AFD_MAKESNAPSHOTS ... make a series of wave propagation snapshots, you
%                       can view these with either plotsnaps or plotgathers. 
%                       See demo_afd_makesnapshots.
%
% Simple Models
% CHANNELMODEL... build a model representing a channel in a stratigraphic sequence
% WEDGEMODEL  ... build a simple 2D model of a high-velocity wedge over an anticline
% THRUSTMODEL ... build a simple model of a high-angle thrust
% DIPMODEL ... build a simple model of a dipping reflector
% SANDWEDGE ... simple model of a sand wedge beneath a gradient overburden
% STACKEDCHANNEL ... a stacked set of channels beneath a gradient overburden
% MARMOUSIMODEL ... a very structually complex and famous model used for imaging and inversion tests
% HUSSARMODEL ... a model with complex stratigraphy from well logs
%
% Utilities
% AFD_VMODEL ... makes simple polygonal velocity models
% AFD_VELCREATE ... GUI-based velocity model building
% AFD_SOURCE ... generates a source array for uses with AFD_SHOTREC
% CHANGE_GRID_SPACING ... example script to interpolate a velocity model
% AFD_REFLECT ... calculate the reflectivity from a velocity model
% AFD_MOVIE ... make movies of wavefield propagation (see demo_afd_makesnapshots
%                       for an alternative to this)
% maxtimestep ... compute the maximum stable timestep for a given simulation
% 
% Basic time-stepping
% AFD_SNAP ... take one finite difference time step
% AFD_SNAPN ... time steps a wavefield "n" steps
% AFD_SNAPN_ALT ... alternate version
% DEL2_5PT ... compute the 5 point Laplacian
% DEL2_9PT ... compute the 9 point Laplacian
% AFD_BC_OUTER ... apply absorbing boundary condition to outer boundary
% AFD_BC_INNER ... apply absorbing bcs to inner boundary
%
