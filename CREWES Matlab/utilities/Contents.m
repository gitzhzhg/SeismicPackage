% CREWES Utilites toolbox
%
%Logical tests
% BETWEEN: logical test, finds sampls in vector between given bounds
% INSIDEPOLY: identify points inside a polygonal region in 2D.
% ISCOMPLEX: logical test for presence of complex numbers
% NEAR: return indices of those samples in a vector nearest given bounds
% NOTBETWEEN: logical test, the negation of BETWEEN
% SURROUND: analyze how a vector surrounds some test points
% WITHIN: test a point to see if it is inside a polygon or not
%
%Graphical
% CLEARLINES: clear (delete) the rays in a figure
% CLICKALINE: simple utility to draw a line by clicking
% COLORVIEW: puts up and interactive widget to manipulate colormaps
% DRAGLINE: simple dragging of a line with constraints. Better than moveline.
% DRAWLINEINIT: initialize line drawing
% DRAWLINEFINI: finalize line drawing
% DRAWLINE: draw a line on a figure window
% DRAWPICK: draw a line on top of an image (e.g. seismic)
% EDITLINESFINI: used by EDITLINES to finish its work
% EDITLINESINIT: called to initiate editing with EDITLINES.
% EDITLINES: a general digital editing tool for line data.
% FIGSIZE: set figure size as a fraction of screen size
% HARDZOOM: subset a matrix according to axis limit specs
% MOVELINE: Utility to move a line by clicking and dragging
% SCA: set current axis utility
% SELBOX: draw a selection box on a figure window
% SELBOXFINI: finalize selection box drawing
% SELBOXINIT: initialize selection box drawing
% SIGNATURE: put a signature on a figure window
% SIMPLEDIT: can be used for simple graphical editing of line graphs
% SIMPLEZOOM: figure zooming utility using SELBOX

% XTICK: adjust the location of tick marks on the x axis
% YTICK: adjust the location of tick marks on the y axis
% YTICKLBLOFF: turn off ytick labels
%
%Interpolation
% PCINT: piecewise constant interpolation
% PWLINT: piecewise linear interpolation (much faster than interp1)
%
%Windowing, padding, trace conversions
% BOXKAR: create a boxcar window with optional taper
% FROMDB: convert from (db,phase) to (real,imaginary)
% GAUSSIAN: create a Gaussian window
% HILBM: Hilbert transform
% GWINDOW: like mwindow except that you get a Gaussian
% MWINDOW: creates an mwindow (boxcar with raised-cosine tapers)
% MWHALF: half an mwindow (boxcar with raised-cosing taper on one end)
% PAD_TRACE: pads (truncates) one trace with zeros to be the length of another
% PADPOW2: pad a trace with zeros to the next power of 2 in legth
% TODB: converts from (real,imaginary) to (decibels, phase)
% TRIANGLE: create a triangle window
%
%Other stuff
% GAUSS: returns a gaussian distribution sampled in frequency
% NUM2STRMAT: convert a vector of numbers to a string matrix
% TIME2STR: works like num2str except always shows to the millisecond
% SLICEMAT slices a matrix along a trajectory with a fairway
% TIME2STR: convert a time to a string with decimals to the nearest millisecond
% XCOORD: create a coordinate vector given start, increment, and number
% SAMPLE_STARTUP: example of a startup file to use with the CREWES toolbox
%
%Other stuff: RJ Ferguson:
% BLTIFFT: returns time-domain data given a positive sided, band-limited spectrum
% TI_IMPULSE: plots and returns 2-D coordinates of a migration impulse
% response for a homogeneous TI medium
% 
%Other stuff: KWH
% CRVERSION: returns CREWES toolbox version (exists in toolboxes downloaded 
%            from website
% FINDDIR: recursively finds a directory and returns the full path to it
% CRISGRAPHICS: return logical true if h is a graphics handle or logical
%           false if it is not
%
