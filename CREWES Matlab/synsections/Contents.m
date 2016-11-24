% CREWES synthetic sesimic by diffraction superposition
% The SYNSECTIONS toolbox provides a suite of tools to create synthetic seismic
% sections by superposition of hyperbolae and other events. Each function is
% designed to accept a matrix and to insert a single event into it. The event is
% always added (superimposed) on top of what is already there.
%
%Tools
% EVENT_DIP: inserts a dipping (linear) event in a matrix
% EVENT_DIPH: constructs a dipping event by diffraction superposition
% EVENT_DIPH2: constructs a dipping event with sparse diffraction superposition
% EVENT_HYP: inserts a hyperbolic event in a matrix.
% EVENT_PWLINH: diffraction superposition along a piecewise linear track
% EVENT_SPIKE: inserts a spike in a matrix
%
%Standard sections
% MAKESTDSYN: Make a non-diffraction synthetic to demo migration codes
% MAKESTDSYNH: Make a diffraction synthetic to demo migration codes
% DIFFRACTION_SECTION: make a section full of diffractions spaced on a 
%                       regular grid
%
%Demos
% MAKESECTIONS: demo the use of the section tools
% DEMO_HYPERBOLAS: demo the superposition of diffraction hyperbolae
%
%3D Rayleigh-Sommerfeld modelling
% SHOT_MODEL3D ... model a 3D shot
% STACK_MODEL3D ... model a 3D stack
%
 
