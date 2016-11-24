% CREWES display tools
%
% Seismic plotting
% NOTE: If you are unsure, then use PLOTIMAGE as your default seismic
%  viewer. However, if you have only a few traces then TRPLOT and DBSPEC give
%  useful time domain and frequency domain views.
%
%  DBSPEC ... compute and plot the Fourier amplitude spectrum of a trace using a decibel scale
%  TVDBSPEC ... examine spectra in 3 different windows
%  MAKE3DVOL ... reshape a 3D dataset into a 3D matrix for viewing with
%               plotimage3D
%  PLOTIMAGE ... Image display utility for seismic matrices (fast and full featured, new figure window)
%  PLOTIMAGE3D ... Image display for 3D seismic matrices
%  PLOTSEIS ... Plot a seismic matrix (trace gather) using WTVA format (rudimentary display in current figure)
%  PLOTSEISMIC ... Plot a seismic matrix with WTVA format and UI controls (new figure window)
%  PLOTGATHERS ... Display a suite of seismic gathers in a way that facilitates comparison (new figure window)
%  PLOTSNAPS ... Display a set of wavefield snapshots superimposed on a velocity model (new figure window)
%  PLOTAXES ... Display a set of similar x-y plots in a vertical set of axes for easy comparison (new figure window)
%  TRPLOT ... utility to make a comparative plot of a few traces. Similar to DBSPEC but in the time domain
%  WTVA ... plot a seismic trace in wiggle-trace variable-area format
%  CLIPPING ... automatically determine clipping levels when using imagesc
%
% NOTE: PLOTGATHERS, PLOTSNAPS, and PLOTAXES are similar tools that all
% facilitate the anaysis of suites of data. They also make movies and
% PowerPoint slides very easy.  See the help for each of them for more
% information.
%
% Utilities
%  ALPINE ... a colormap that grades for green throuh blue to white
%  AXESLABELSIZE ... change the size of axes labels
%  AXESTITLESIZE ... change the size of axes titles
%  BIGFIG ... enlarges a figure to an optimal size for a slide
%  BIGFONT ... increases (or decreases) fontsize in a figure
%  BOLDLINES ... changes the thickness of lines and the size of markers
%  CLEARPICKS ... clear (delete) the picks in a figure
%  COPPERUD ... colormap the same as COPPER but upside down
%  DBSPEC ... plots a Fourier amplitude spectrum using a decibel scale
%  FLIPX ... script to flip the direction of the x (horizontal) axis
%  FLIPY ... script to flip the direction of the y (vertical) axis
%  GREYFIG ... changes the current figure's background to grey
%  HIDEUI ... hide (or restore) user interface controls
%  LEGENDFONTSIZE ... change the font size in a legend
%  LINESGRAY ... gray level plotting of line data for publications
%  PLOTBLOCKY ... plot a piecewise constant function as a staircase
%  PLOTIMAGE ... Image display utility for seismic matrices
%  PLOTSEIS ... plot a seismic trace gather using WTVA format
%  PLOTSEISMIC ...A wtva seismic viewer with GUI controls
%  PLOTSEISVA ... Image display utility for seismic matrices
%  PITITLE ... like Matlab's title but also changes the Figure name.
%  POSNFIG ... position a figure anywhere. Default is center of screen.
%  PREPFIG ... simple utility to prepare a graphic for publication
%  PREPFIGA ... alternative to prepfig that enlarges fonts a bit less
%  SEISCLRS ... creates the default color map for plotimage
%  TITLEFONTSIZE ... change the font size in figure/axes titles
%  UNHIDEUI ... restore user interface controls
%  WHITEFIG ... change a figure background to white (for publication)
%  WINEXTRACTOR ... visualize and adjust a windowed area on a single trace
%  WTVA ... plot a seismic trace in wiggle-trace variable-area format
%  XOFF: turn off the x axis labels and tick marks
%  YOFF: turn off the y axis labels and tick marks
%  
