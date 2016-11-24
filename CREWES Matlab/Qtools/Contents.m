%Contents of Qtools
%
% These are a collection of tools intended to illustrate and explore the
% constant Q model of attenuation.
%
% Functions: 
% EINAR ... creates the constant Q impulse response (wavelet) based on
%           Kjartansson's 1979 paper
% QMATRIX ... creates a matrix which can be applied to a reflectivity or to a
%           stationary trace to install minimum phase time-frequency decay.
%           This is the transmission effect of Q. Options exist to model
%           drift delay relative to Nyquist or, more realistically,
%           relative to sonic logging frequencies.
% INVQ ... design an inverse Q matrix. This is done by finding a pseudo
%           inverse to the forward problem (qmatrix).  The pseudo
%           inverse is used to allow thresholding of the singular values.
% FAKEQ ... given velocity and density logs, invent a Q log based on
%           empirical rules.
% TDRIFT ... calculate the drift time. This is the traveltime difference at
%           seismic frequencies minus that at logging frequencies.
% DRIFT_CORR ... given a seismogram computed with logging velocities, apply
%           the drift delay to simulate having done the computation with
%           check-shot corrected velocities.
% VELF ... calculate frequency-dependent phase velocity given Q
% QESTIMATOR ... estimate Q by various methods. 
% Q_SPECRAT ... estimate Q by the spectral ratio method (used by
%           qestimator)
% Q_SPECMATCH ... estimate Q by the spectral matching method (used by
%           qestimator)
% Q_CENTROID ... estimate Q by the centroid (dominant frequency) method
%           (used by qestimator)
% QZ2QINT ... given a finely layered Q model as a function of z, compute 
%           the effective interval Q over a large interval. This is the
%           expected result of a Q measurement.
% VSPMODELQ ... compute a 1D vsp given a Q model and velocity and density
%           logs. Surface source. Based on Ganley 1981.
% VSPMODELQS ... compute a 1D vsp given a Q model and velocity and density
%           logs. Burried source. Based on Ganley 1981.
% PLOTQINT ... utility to plot interval Q measures showing the interval size
%
% Demonstration scripts 
% DEMO_Q_WAVELETS ... show the basic nature of Q wavelets using einar.
% DEMO_CONVMTX_QMTX ... illustrate the creation of stationary and 
%           nonstationary synthetic seismograms using Toeplitz convolution
%           matrices and nonstationary Q matrices.
% DEMO_INVQ ... shows the use of the invq command to render a nonstationary
%           seismogram stationary.
% TEST_QESTIMATOR ... demo the use of qestimator. 
% TEST_VSPMODELQ ... demo the creation of vsp synthetics using vspmodelq.
% TEST_VSPMODELQS ... demo the creation of vsp synthetics using vspmodelqs.