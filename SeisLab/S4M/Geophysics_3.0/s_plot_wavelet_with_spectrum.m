function aux=s_plot_wavelet_with_spectrum(wavelet,varargin)
% Plot wavelet and its amplitude spectrum in one figure
%
% Written by: E. Rietsch: June 5, 2004
% Last updated: March 26, 2007: add 'single' option
%
%          aux=s_plot_wavelet_with_spectrum(wavelet,varargin)
% INPUT
% wavelet  wavelet
% varargin  one or more cell arrays; the first element of each cell array
%          is a keyword, the other elements are parameters. 
%          Presently, keywords are:
%     'frequencies'  start and end frequency to display; either two values or 
%          a vector of two values.
%          Default: {'frequencies',0,500/seismic.step}
%     'normalize'  Establish if the amplitude spectrum is to be normalized.
%          Possible values: 'yes' and 'no'. 
%          Default: {'normalize','yes'}
%     'orient'  Figure orientation; possible values are "portrait' and
%          'landscape'
%          Default: {orient','portrait'}
%     'scale'   Set linear (amplitude), power, or logarithmic scale (dB) 
%          for amplitude spectrum.
%          Possible values: 'linear', 'log'. 
%          Default: {'scale','linear'}
%     'single'  Option to plot spectra of multiple traces individually.
%          Default: {'single','yes'}
%     'times'  start and end time to display (the spectrum is computed
%          from all the samples of the dataset); either two values or 
%          a vector of two values.
%          Default: {'times',wavelet.first,wavelet.last}     which is the same as
%                   {'times',[wavelet.first,wavelet.last]}
% OUTPUT
% aux      structure with handles
%     'figure_handle'   figure handles
%     'axis_handles'    vector with handles of the two axes
%     'legend_handle'   handle of the legend of the spectrum subplot
%
% EXAMPLE
%      wavelet=s_create_wavelet;
%      s_plot_wavelet_with_spectrum(wavelet) 
%      aux=s_plot_wavelet_with_spectrum(wavelet,{'frequencies',0,90}, ...
%          {'orient','landscape'},{'normalize','no'},{'scale','log'});


%       Set defaults for input arguments
param.frequencies=[0,500/wavelet.step];
param.normalize='yes';
param.orient='portrait';
param.scale='linear';
param.single='yes';
param.times=[wavelet.first,wavelet.last];

%       Replace default input arguments by actual ones
param=assign_input(param,varargin);

if iscell(param.frequencies)
   param.frequencies=cell2num(param.frequencies);
end
if iscell(param.times)
   param.times=cell2num(param.times);
end

if strcmp(param.orient,'portrait')
   figure_handle=pfigure;
   haxis1=subplot(2,1,1);
   haxis2=subplot(2,1,2);
else
   figure_handle=lfigure;
   axis off
   rect1=[0.09, 0.12, 0.20, 0.70];
   haxis1=axes('position',rect1);
   rect2=[0.39, 0.12, 0.55, 0.70];
   haxis2=axes('position',rect2);
end

axes(haxis1)
s_wplot(wavelet,{'figure','old'},{'orient','portrait'},{'times',param.times},{'deflection',0.9})
title(mnem2tex(wavelet.name))
ylabel('Time (ms)')

axes(haxis2)
s_spectrum(wavelet,{'figure','old'}, ...
          {'frequencies',param.frequencies(1),param.frequencies(2)}, ...
          {'normalize',param.normalize},{'scale',param.scale}, ...
          {'single',param.single},{'linewidth',3})
axis tight
legend_handle=legend(wavelet.name);
mytitle('Spectrum')

% figure_export_menu(figure_handle)
zoom on

if nargout > 0
   aux.figure_handle=figure_handle;
   aux.axis_handles=[haxis1,haxis2];
   aux.legend_handle=legend_handle;
end
