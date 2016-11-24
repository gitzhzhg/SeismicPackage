function s_plot(seismic)
% "Quick-look" plot of seismic data. Color plot if there are more than 
% S4M.ntr_wiggle2color (usually 101) traces; otherwise wiggle trace plot.
% Seismic traces are individually scaled.
% For more sophisticated plots use functions "s_wplot" or "s_cplot" or s_iplot.
%
% Written by: E. Rietsch: August 3, 2001
% Last updated: June 28, 2006: More extensive input check
%
% 	   s_plot(seismic)
% INPUT
% seismic  seismic structure or an array
% 
% EXAMPLES
%          seismic=s_data;
%          s_plot(seismic)           % Seismic input
%          s_plot(seismic.traces)    % Matrix input

global S4M

if isempty(S4M)
   presets
end

if istype(seismic,'seismic')
   ntr=size(seismic.traces,2);
elseif isnumeric(seismic)
   ntr=size(seismic,2);
else
   error('The input argument must be a seismic data set or a matrix.')
end

if ntr > S4M.ntr_wiggle2color
   s_cplot(seismic,{'scale','no'})
else
   s_wplot(seismic,{'scale','no'})
end
