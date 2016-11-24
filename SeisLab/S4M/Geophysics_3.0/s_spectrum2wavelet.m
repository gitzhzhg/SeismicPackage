function wavelet=s_spectrum2wavelet(freq,amps,varargin)
% Compute zero-phase wavelet from its amplitude spectrum; 
% unless spectral amplitudes are defined for zero frequency and/or Nyquist
% frequency (i.e. freq(1) == 0 and/or freq(end) == 500/step) they are set
% to zero.
%
% Written by: E. Rietsch: November 27, 2004
% Last updated: March 30, 2008: bug fix%
%
%          wavelet=s_spectrum2wavelet(freq,amps,varargin)
% INPUT
% freq     frequency values at which the spectrum is defined
% amps     associated values of the amplitude spectrum
% varargin   one or more cell arrays; the first element of each cell array
%          is a keyword, the other elements are parameters.
%          Presently, keywords are:
%     'dc_removal'  should DC be removed. Possible values: 'yes' and 'no'.
%          Default: {'dc_removal','yes'}
%     'method'  Interpolation method used for spectrum; possible values are 
%          parameters allowed for function "interp1".
%          Default: {'method','linear'}
%     'step'  sample interval of the seismic
%          Default: {'step',4}
%     'window'  Type of window to apply to the wavelet. Possible windows are 
%          in function "mywindow". Use 'rect' or 'none' if you do not
%          want a window.
%          Default: {'window','trapezoid'}            
%     'wlength'  wavelet length
%          Default: {'wlength',100} 
% OUTPUT
% wavelet  zero-phase wavelet with spectrum defined by "freq" and "amps".
%
% EXAMPLE
%          wavelet=s_spectrum2wavelet([10,20,40,60],[0,1,1,0],{'wlength',80})
%          s_spectrum(wavelet)

% UPDATE HISTORY
%          August 3, 2006: Added option to apply window to wavelet;
%                               better DC removal

global S4M

%	Set defaults of input arguments
param.dc_removal='yes';
param.method='linear';
param.step=4;
param.window='trapezoid';
param.wlength=100;

%	Replace defaults by actual input parameters
param=assign_input(param,varargin);

nsamp=odd(param.wlength/param.step);
ansamp1=nsamp-1;
awlength=ansamp1*param.step;

%	 Wavelet length used for spectrum interpolation
nsamp1=4*ansamp1;
nsamp=nsamp1+1;

%	Sample interval in the frequency domain
equidist=(0:2:nsamp)*500/(param.step*nsamp);
equidist(end)=min(equidist(end),freq(end));
if freq(1) > 0
   freq=[0;freq(:)];
   amps=[0;amps(:)];
end
if freq(end) < 500/param.step;
   freq=[freq(:);equidist(end)];
   amps=[amps(:);0];
else
   freq(end)=equidist(end);
end
   
[freq,index]=unique(freq);
amps=amps(index);

aspectrum=reshape(interp1(freq,amps,equidist,param.method),[],1);
aspectrum(isnan(aspectrum))=0;

aspectrum=[aspectrum;aspectrum(end:-1:2)];

wavelet.type='seismic';
wavelet.tag='wavelet';
wavelet.name='Wavelet with defined spectrum';
wavelet.first=-awlength/2;
wavelet.last=-wavelet.first;
wavelet.step=param.step;
wavelet.units='ms';
traces=fftshift(real(ifft(aspectrum)));
inc=(nsamp1-ansamp1)/2;
wavelet.traces=traces(inc+1:end-inc);
% keyboard
%       Check for null values
if any(isnan(wavelet.traces))
   wavelet.null=NaN;
else
   wavelet.null=[];
end

%       Apply window to wavelet
if ~ismember(lower(param.window),{'rect','none'})
   wavelet.traces=wavelet.traces.*mywindow(length(wavelet.traces),param.window);
else
   wavelet.traces([1,end])=wavelet.traces([1,end])*0.5;
end

%       Remove DC
if strcmpi(param.dc_removal,'yes')
   wavelet.traces=lf_dc_removal(wavelet.traces,2);
%   wavelet.traces=wavelet.traces-sum(wavelet.traces)/(length(wavelet.traces)-1);
end

if strcmp(S4M.precision,'single')
   wavelet=single(wavelet);
else
   wavelet=double(wavelet);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function m=odd(m)

m=2*round((m-1)*0.5)+1;
