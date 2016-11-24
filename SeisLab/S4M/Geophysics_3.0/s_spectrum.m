function aux=s_spectrum(varargin)
% Display the spectrum of one or more seismic input data sets. 
% Null values in any data set are replaced by zeros. A maximum of 100 curves 
% can be displayed.
%
% Written by: E. Rietsch: July 3, 2000 (original version)
% Last updated: April 15, 2008: Add option to apply a taper window to the datset
% 
%          aux=s_spectrum{varargin}
% INPUT
%         The first input parameters are seismic data sets (seismic data
%         structures) or matrices; if they are matrices they are, internally,
%         converted to seismic structures with units 'samples'.
%         It is important that all seismic input data sets have the same units.
%         The seismic data sets may be followed by cell arrays which consist of a 
%         keyword and one or more parameters
% seis1   seismic structure or matrix
% seis2   seismic structure or matrix
%  ...
% seisn   seismic structure or matrix
%          
% parameters   one or more cell arrays; the first element of each cell array is
%              a keyword, the other elements are parameters.
%              Presently, keywords are:
%     'average'  Possible values are 'yes' (or true) or 'no' (or false).
%              Only relevant for multi-trace datasets.
%              If {'average',true} then the average spectrum of the traces is
%                  computed and displayed;
%              if {'average',false} the spectra of the individual traces 
%                 are computed and displayed
%              Default: {'average',true}
%     'colors'  Colors to be used for consecutive curves.
%              Possible values: any permissible color and line style combinations;
%              if there are more datasets than colors/line styles (default 21) the
%              colors/line styles will be repeated
%              Default: {'colors','r','b','g','m','k','c','y',...
%                            'r--','b--','g--','m--','k--','c--','y--' ...
%                            'r:','b:','g:','m:','k:','c:','y:'};
%     'figure'  Specifies if new figure should be created or if the seismic traces 
%              should be plotted to an existing figure. Possible values are 'new' 
%              and any other string. 
%              Default: {'figure','new'} 
%     'frequencies'  Two positive numbers representing the range of frequencies to 
%              display. The first number must be non-negative and smaller than 
%              the second. If the second number is greater than the Nyquist 
%              frequency of the data set with the smallest sample interval, it 
%              is set to the Nyquist frequency.
%              Default: {'frequencies',0,inf}
%     'legend'   Figure legend (curve annotation).
%              Default: names of the seismic input data sets.
%     'lloc'   Location of figure legend. For possible values see: help legend
%              Default: {'lloc','best'}
%     'linewidth'  Line width of curves. Default: {'linewidth',2}
%     'orient'   Plot orientation. Possible values are: 'portrait' and 'landscape'
%              Default: {'orient','landscape'}
%     'plot'   Types of plot(s) to create. Possible values are: 
%              'amp'    plot amplitude spectrum
%              'phase'  plot phase spectrum
%              'both'   plot amplitude and phase spectrum
%              Default: {'plot','amp'} ... plot amplitude spectrum only
%     'padding'  Traces with fewer than "padding" samples are padded with
%              zeros. This parameter is ignored if the number of samples 
%              per trace exceeds "padding". 
%              Default:{'padding',256}
%
%          Keywords relevant for amplitude spectrum only:
%     'normalize'  Establish if or how the amplitude spectra are to be 
%              normalized.
%              Possible values are: 
%              'amplitude'  scale data so that the mean spectral amplitude is 
%                       the same
%              'peak'   scale spectra so that their peaks are the same
%              'power'  scale spectra so that the area under the power spectrum
%                       is the same for all signals.
%              'no'     do not scale spectra            
%              Default: {'normalize','power'}
%              The "normalize" parameter is ignored if the scale parameter
%              (see below) is 'dB'.
%     'scale'  Set the scale of the y-axis. Possible values are:
%              'linear'  plot amplitude spectrum
%              'power'  plot power spectrum
%              'log'    power spectrum in logarithmic scale
%              'dB'     power spectrum in dB. (the parameter value is not case 
%                       sensitive; 'db' or 'DB' are also allowed)
%              Default: {'scale','linear'}
% 
%          Keywords relevant for phase spectrum only:
%     'tiepoint'  since the phase is only determined up to a multiple of 2*pi 
%              (360 degrees) this parameter specifies where the phase is forced 
%              to be in the range of (-180,180]. For arbitrary wavelets this is
%              usually the dominant frequency (e.g. the term 60-degree wavelet)
%              For a minimum-phase wavelet one would prefer DC (zero frequency).
%              Hence "tiepoint" has two values "zero' and 'peak'.
%              Default: {'tiepoint','peak'}
%     'timezero'   Zero-time used for phase calculation. Possible values 
%              are 'best' and 'actual'.
%              'actual': The actual time zero of a dataset (e.g.seismic.first)
%              is used to compute the phase. In this case the phase may 
%              include a linear component
%              'best': The dataset is shifted so that time zero is a 
%              the peak of its Hilbert transform. There is no linear 
%              component.
%              Default: {'timezero','best'}
%     'window'  string with name of taper window; see function "s_window" for 
%              possible values.
%              Default: {'window','none'}
% OUTPUT
% aux    Structure
%     'figure_handle'   handle to figure
%     'handles4amp'     handle of the axes for the amplitude spectrum (if any)
%     'handles4phase'   handle of the axes for the phase spectrum (if any) 
%
% EXAMPLES
%     wavelet=-s_create_wavelet({'type','min-phase'});
%     s_spectrum(wavelet,{'plot','amp'},{'frequencies',0,80},{'padding',128})
%
%     lfigure
%     subplot(1,2,1)
%     s_spectrum(wavelet,{'plot','phase'},{'timezero','best'},{'figure','old'})
%     mytitle('best')
%     subplot(1,2,2)
%     s_spectrum(wavelet,{'plot','phase'},{'timezero','actual'},{'figure','old'})
%     mytitle('actual')
%     mysuptitle('Two options for phase plot using parameter "timezero"',{'color','blue'})
%
%     seismic=s_data;
%     s_spectrum(seismic,wavelet,{'scale','log'})

% UPDATE HISTORY
%     October 16, 2007:  Use TeX-style text in legend
%     December 21, 2007: remove restrictions
%     January 21, 2008:  preserve DC component


%       Find number of input seismic data sets or matrices; convert matrices 
%       to seismic datasets 
nseis=nargin;
types=true(1,nseis);
for ii=1:nargin
   if iscell(varargin{ii})
      nseis=ii-1;
      break

   else
      if isstruct(varargin{ii})
         % Do nothing
      elseif isnumeric(varargin{ii})
         varargin{ii}=s_convert(varargin{ii},1,1,' ','samples');
         varargin{ii}.name=['# ',num2str(ii)];
         types(ii)=false;
      else
         error('Input arguments must be structures, numeric arrays or cell arrays.')
      end
   end
end

if nseis == 0
   error(' At least the first input argument must be a seismic data set or a matrix.')
end

%       Check if input datasets are either all matrices or seismic datasets
%       (no mixing of the two)
isseismic=all(types(1:nseis));
ismatrix=all(~types(1:nseis));
if ~isseismic  && ~ismatrix
   error('Seismic datasets and matrices cannot be mixed.')
end
   

%       Create cell array with datasets whose spectra are to be plotted
maxcurves=1000;
datasets=cell(1,maxcurves);
number_of_traces=zeros(1,maxcurves);
steps=zeros(1,maxcurves);
ik=0;
for ii=1:nseis
   lds=length(varargin{ii});
   if  lds > 1
      for jj=1:lds
         ik=ik+1;
         temp=varargin{ii}(jj);
         temp.name=[temp.name,' (',num2str(jj),')']; % Create distinctive name for plot label
         number_of_traces(ik)=size(temp.traces,2);
         steps(ii)=temp.step;
         datasets{ik}=temp;
      end
   else
      ik=ik+1;
      datasets{ik}=varargin{ii};
      steps(ik)=datasets{ik}.step;
      number_of_traces(ik)=size(datasets{ik}.traces,2);
   end
end

steps(ik+1:end)=[];
datasets(ik+1:end)=[];
param.steps=steps(1:ik);       % Sample intervals for all input datasets
number_of_traces(ik+1:end)=[];
total_number_of_traces=sum(number_of_traces);


if ik > maxcurves
   disp([' Spectra of ',num2str(ik),' datasets requested.'])
   disp([' The maximum number of individual spectra is ',num2str(maxcurves),'.'])
   error('Abnormal termination.')
end

       
%       Define defaults for parameters
param.average=true;
param.bestshift='true';
param.colors={'r','b','g','k','c','m','y', ...
              'r--','b--','g--','k--','c--','m--','y--', ...
              'r:','b:','g:','k:','c:','m:','y:'};
param.figure='new';
param.legend=[];
param.lloc='best';
param.linestyle='-';
param.linewidth=2;
param.normalize='power';
param.frequencies={0,inf};
param.padding=256;
param.option='average';
param.orient='landscape';
param.plot='amp';
param.scale='linear';
param.single='';        % Deprecated
param.tiepoint='peak';
param.timezero='best';
param.window='none';

%       Replace defaults by actual input arguments
if nseis < nargin
   param=assign_input(param,{varargin{nseis+1:nargin}});
end

if ~isempty(param.single)
   alert('Parameter "single" is obsolete: please use parameter "average" instead.')
   param.average=~isyes(param.single);
end

%       Convert input data to one-trace datasets if no averaging is requested
if ~isyes(param.average)
    if total_number_of_traces > maxcurves
       disp([' Individual spectra of ',num2str(total_number_of_traces),' traces requested.'])
       disp([' The maximum number of individual spectra computed is ',num2str(maxcurves),'.'])
       error('Abnormal termination.')
    end
    if total_number_of_traces > ik
       ie=cumsum(number_of_traces);
       ia=[1,ie(1:end-1)+1];
       for ii=length(ia):-1:1
          temp=s_ds2dsvector(datasets{ii});
          if length(temp) == 1
             datasets{ia(ii)}=temp;
          else
             ik=0;
             for jj=ia(ii):ie(ii)
                ik=ik+1;
                temp(ik).name=[temp(ik).name,' ',num2str(ik)];
                datasets{jj}=temp(ik);
             end
          end
       end
    end
    nseis=total_number_of_traces;
else
    nseis=ik;
end

%       Prepare input arguments for actual use
if ~iscell(param.colors)
   param.colors={param.colors};
end
[param.colors,param.linestyles]=split_colors_linestyles(param.colors);

%       Define frequency range to plot
if isinf(param.frequencies{2}) 
   param.frequencies{2}=500/min(steps);
end
if param.frequencies{1} < 0;
   param.frequencies{1}=0;
end
if param.frequencies{1} >= param.frequencies{2}
   error([' Incompatible spectrum frequencies: ', ...
       num2str(param.frequencies{1}),' ',num2str(param.frequencies{2})])
end

%       Create/get figure window
if strcmpi(param.figure,'new')
   if strcmpi(param.orient,'landscape')
      figure_handle=lfigure;
   else
      figure_handle=pfigure;
   end
else
   figure_handle=gcf;
end

ltext=cell(nseis,1);    % Reserve room for legend


%       Units of x-axis
param.xunits=units2frequencydomain(datasets{1}.units);
param.label4x=['Frequency (',param.xunits,')'];

switch param.plot
case 'amp'
   handle4amp_axis=gca;
   if strcmpi(param.scale,'log')
      set(handle4amp_axis,'Yscale','log')
   end
   for ii=1:nseis
      ltext{ii}=mnem2tex(datasets{ii}.name);
      plot_amplitude_spectrum_no1(s_rm_trace_nulls(datasets{ii}),param,ii);
   end
   finish_amplitude_plot_no3(param,ltext)

case 'phase'
   handle4phase_axis=gca;
   for ii=1:nseis
      plot_phase_spectrum_no2(s_rm_trace_nulls(datasets{ii}),param,ii);
      ltext{ii}=mnem2tex(datasets{ii}.name);
   end
   finish_phase_plot_no4(param,ltext)

case 'both'
   if ~strcmpi(param.figure,'new')
      error('If amplitude and phase are to be plotted the figure parameter must be set to ''new''.')
   end

   %    Create subplot axes
   if strcmpi(param.orient,'portrait')
      handle4amp_axis=subplot(2,1,1);
      handle4phase_axis=subplot(2,1,2);
   else
      handle4amp_axis=subplot(1,2,1);
      handle4phase_axis=subplot(1,2,2);
   end

   for ii=1:nseis
      ltext{ii}=mnem2tex(datasets{ii}.name);
      axes(handle4amp_axis)
      plot_amplitude_spectrum_no1(s_rm_trace_nulls(datasets{ii}),param,ii);
      axes(handle4phase_axis)
      plot_phase_spectrum_no2(s_rm_trace_nulls(datasets{ii}),param,ii);
   end
   finish_phase_plot_no4(param,ltext)
   axes(handle4amp_axis)
   finish_amplitude_plot_no3(param,ltext)

%       Link the frequency axes of the two plots
   linkaxes([handle4amp_axis,handle4phase_axis],'x')
%{
   if nargout > 0
      aux.zoom_handles=disable_zoom(figure_handle);
   else
      disable_zoom(figure_handle)
   end
%}
otherwise
   error(['Unknown plot option: "',param.plot,'".'])
end


if nargout > 0
   aux.figure_handle=figure_handle;
   try
      aux.handle4amp_axis=handle4amp_axis;
   catch  %#ok
   end
   try
      aux.handle4phase_axis=handle4phase_axis;
   catch  %#ok
   end

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function aux=plot_amplitude_spectrum_no1(dataset,param,index)
% Plot amplitude spectrum

%     Apply taper window if required
switch param.window
   case {'none','rect'}
      % Do nothing
   otherwise
      dataset=s_window(dataset,param.window);
end

%       Compute FFT and amplitude spectrum
nsamp=size(dataset.traces,1);
nfft=max(nsamp,param.padding);
ftseis=fft(dataset.traces,nfft);

%       Compute frequencies values
if mod(nfft,2) == 0
   nffth=nfft/2+1;
else
   nffth=(nfft+1)/2;
end
ftseis(nffth+1:end,:)=[];
aftseis=mean(abs(ftseis),2)/nfft;

%       Normalize the spectra (if requested)
switch param.normalize
case 'peak'
   aftseis=aftseis/max(aftseis);
case 'power'
   aftseis=aftseis/norm(aftseis);
case 'amplitude'
   aftseis=aftseis/mean(aftseis);
case 'no'
   % Do nothing
otherwise
   disp(['Alert from S_SPECTRUM: Unknown normalization parameter: "',param.normalize,'". Spectrum is not normalized.'])
end


%       Set the scale to be plotted
switch lower(param.scale)
case {'power','log'}
   aftseis=aftseis.^2;
otherwise
   % do nothing
end


%       Set the scale to be plotted
%aftseis(1)=0;
switch lower(param.scale)
case 'log'
   aftseis=max(aftseis,eps*max(aftseis));
case 'db'
   aftseis=max(aftseis,eps*max(aftseis));
   aftseis=20*log10(aftseis);
   aftseis=aftseis-max(aftseis);
otherwise
   % do nothing
end

%       Determine abscissa values
nyquist=500/dataset.step;
freq=(0:2:nfft)*nyquist/nfft;
bool=freq >= param.frequencies{1} &  freq <= param.frequencies{2};
freq=freq(bool);
aftseis=aftseis(bool);

%       Plot amplitude spectrum
idx=mod(index-1,length(param.linestyles))+1;
line(freq,aftseis,'LineWidth',param.linewidth,'Color',param.colors{idx}, ...
   'LineStyle',param.linestyles{idx});

if nargout > 0
   aux.ftseis=ftseis;
   aux.freq=freq;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function aux=plot_phase_spectrum_no2(dataset,param,index)
% Plot phase spectrum

[nsamp,ntr]=size(dataset.traces);
nfft=max(param.padding,nsamp);

switch param.timezero
case 'best'
   timezero=[];
case 'actual'
   timezero=dataset.first;
otherwise
   error(['Unknown value of parameter "timezero": ',param.timezero])
end

if ntr > 1
  for ii=1:ntr
      [phase,aux]=signal_phase_spectrum(dataset.traces(:,ii),nfft, ...
                  timezero,param.tiepoint);
      if ii==1;
         avphase=phase;
      else
         avphase=avphase+phase;
      end
   end
   phase=avphase/ntr;
   
else
   [phase,aux]=signal_phase_spectrum(dataset.traces,nfft, ...
        timezero,param.tiepoint);
end

%       Determine abscissa values
nyquist=500/dataset.step;
freq=aux.freq*nyquist;
%freq=(0:2:nfft)*nyquist/nfft;
bool=freq >= param.frequencies{1} &  freq <= param.frequencies{2};
freq=freq(bool);
phase=phase(bool);
ik=true;
while all(phase < 0) && any(phase < 180)
   phase=phase+360;
   ik=false;
end
if ik
   while all(phase > 0) && any(phase > 180)
      phase=phase-360;
   end
end

%       Plot phase spectrum
idx=mod(index-1,length(param.linestyles))+1;
line(freq,phase,'LineWidth',param.linewidth,'Color',param.colors{idx}, ...
   'LineStyle',param.linestyles{idx});

if nargout > 0
   aux.phase=phase;
   aux.freq=freq;
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function finish_amplitude_plot_no3(param,ltext)

%       Create axis labels
xlabel(param.label4x)

switch lower(param.scale)
case 'linear'
   switch lower(param.normalize)
   case {'peak','power'}
      label4y='Normalized spectral amplitude';
   otherwise
      label4y='Spectral amplitude';
   end

case 'power'
   switch lower(param.normalize)
   case {'peak','power'}
      label4y='Normalized spectral power';
   otherwise
      label4y='Spectral power';
   end

case 'log'
   switch lower(param.normalize)
   case {'peak','power'}
      label4y='Normalized spectral power';
   otherwise
      label4y='Spectral power';
   end

case 'db'
   switch lower(param.normalize)
   case {'peak','power'}
      label4y='Normalized spectral power (dB)';
   otherwise
      label4y='Spectral power (dB)';
   end

otherwise
   label4y='';
end

%{
switch param.normalize
case 'peak'
   label4y=[label4y,' (normalized peak amplitude)'];
case 'power'
   label4y=[label4y,' (normalized energy)'];
otherwise
   % do nothing
end
%}

ylabel(label4y)

grid on
box on
bgGray
axis tight

legend(ltext,'Location',param.lloc)

initiate_2d_tracking({'Freq.',param.xunits,'Frequency'},{'Amp.','n/a','Amplitude'})


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function finish_phase_plot_no4(param,ltext)

%       Create axis labels
xlabel(param.label4x)

if isempty(param.timezero)
   ylabel('Phase in degrees (zero-time at peak of Hilbert transform)')
else
   ylabel('Phase in degrees')
end

grid on
box on
bgGray
axis tight

legend(ltext,'Location',param.lloc)

initiate_2d_tracking({'Freq.',param.xunits,'Frequency'},{'Phase.','degrees','Phase'})
