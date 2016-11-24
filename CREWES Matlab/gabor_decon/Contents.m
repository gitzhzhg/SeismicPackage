% Gabor Deconvolution Toolbox
%
% Test scripts
%  TEST_GABORDECON: script builds a nonstationary sysnthetic and then
%               deconvolves same with Gabor decon and Wiener decon.
%               Comparison plots are produced.
%  TEST_FBORGA: script to demo some of the features of the Borga transform.
%  TEST_GABORSLICE: script to demo gaborslice.
%
% Transforms
%  FGABOR: forward Gabor transform with modified Gaussian analysis
%               windowing 
%  IGABOR: inverse Gabor transform with modified Gaussian
%               synthesis windowing 
%  FBORGA: forward Borga transform. This is provided for conceptual 
%               interest and is not used by gabordecon. The Borga transform
%               is the formal adjoint to the Gabor transform and
%               essentially amounts to a complete set of filter slices.
%  GABORSLICE: creates time-domain Gabor slices. These sum to form the
%               original trace while a Fourier transform of the slices
%               creates the Gabor transform.
%
% Deconvolution
%  GABORDECON: seismic deconvolution using the Gabor transform with Fourier
%               operator design. 
%  GABORDECONB: seismic deconvolution using the Gabor transform with Burg 
%               operator design.
%  GABORDECON_PRO: profile mode Gabor decon. Designs a single operator to
%               apply to an entire gather. 
%  GABORDECON_SHOT: Applies single-channel gabordecon to each trace in a 
%               shot or in a cell array of many shots
%  GABORDECON_STACK: post-stack single channel gabor decon
%
% Utilities
%  GAUSSIAN_UPOU: Design a uniform partition of unity (POU) using modified
%           Gaussians.This is invoked by fgabor and igabor.
%  AGAUSSIAN_UPOU: Similare to gaussian_upou except that the windows are
%           allowed to be asymmetric. Not currently used by fgabor or
%           fborga.
%  FILT_HYP: Bandpass filtering with filter parameters following
%           time-frequency hyperbolae. Useful after Gabor decon.
%  HYPERSMOOTH: smooth a time variant spectrum along hyperbolic contours.
%           Invoked by gabordecon and gabordeconb
%  
