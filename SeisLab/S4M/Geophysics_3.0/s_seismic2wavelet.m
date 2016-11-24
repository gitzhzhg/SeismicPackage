function wavelet=s_seismic2wavelet(seismic,varargin)
% Function computes wavelet with about the spectrum expected on the basis of the
% seismic data. The wavelet can be minimum-phase, zero-phase, or
% maximum-phase. The amplitude is based on the windowed autocorrelation of the
% seismic data.
%
% Written by: E. Rietsch: June 15, 2001
% Last updated: September 2, 2007: general update
%
%           wavelet=s_seismic2wavelet(seismic,varargin)
% INPUT
% seismic   seismic structure
% varargin    one or more cell arrays; the first element of each cell array is a
%             keyword,the other elements are parameters. Presently, keywords are:
%       'nodc'  Determines if DC component of wavelet should be removed (no DC).
%             Possible values are: true (yes) and false (no)
%             Default: {'nodc',true}
%       'wlength' wavelet length. For symmetric wavelets this should be an even
%             multiple of the sample interval. An optional second parameter
%             can be used to indicate if the wavelet length is to be exact 
%             ('exact') or an approximation ('approx'). In the latter case
%             the actual length of the wavelet is chosen so that it ends 
%             just prior to a zero crossing 
%             Default: {'length',80,'approx'}
%       'color' Color of reflectivity. Possible values are 'white' and 'blue'.
%             If 'blue', the amplitude spectrum of the reflectivity is assumed to be 
%             proportional to the square root of the frequency.
%             Default: {'color','blue'} 
%       'type'  type of wavelet. Possible options are:
%             'zero-phase'    zero-phase wavelet
%             'min-phase'     minimum-phase wavelet
%             'max-phase'     maximum-phase wavelet
%             Default: {'type','zero-phase'}
%       'window' type of window to use.  Possible values are (not case-sensitive): 	
%                    'Hamming', 'Hanning', 'Nuttall',  'Papoulis', 'Harris',
% 	             'Rect',    'Triang',  'Bartlett', 'BartHann', 'Blackman'
% 	             'Gauss',   'Parzen',  'Kaiser',   'Dolph',    'Hanna',
% 	             'Nutbess', 'spline',  'none'
%                    (the empty string means no window, 'Rect' and 'none'  are 
%                     equivalent)
%                Default: {'window','Hanning'}
%                         
% OUTPUT
% wavelet      seismic structure with desired wavelet; the wavelet is scaled
%              in such a way that the maximum of the absolute value of the
%              wavelet samples is equal to 1.
%
% EXAMPLE
%             %    Compute minimum-phase wavelet from seismic data
%             seismic=s_data;
%             minwav=s_seismic2wavelet(seismic,{'color','blue'},{'type','min-phase'},{'wlength',80});
%             s_wplot(minwav)
%             s_spectrum(minwav,{'plot','both'},{'timezero','actual'})

global S4M

history=S4M.history;
S4M.history=false;

%	Assign default values of input arguments
param.color='blue';
param.dc=[];
param.length=[];
param.nodc=true;
param.type='zero-phase';
param.window='Hanning';
param.wlength=80;

%       Decode and assign input arguments
param=assign_input(param,varargin,'s_seismic2wavelet');

%	Handle legacy parameter
if ~isempty(param.length)
   alert('Use of keyword "length" is obsolete. Use "wlength" instead.')
   param.wlength=param.length;
end
if ~isempty(param.dc)
   alert('Use of keyword "dc" is obsolete. Please use "nodc" instead (see help s_seismic2wavelet.')
   param.nodc=param.dc;
end

%	Check if "wlength" has a second parameter.
if iscell(param.wlength)
   len=param.wlength{1};
   param.lopt=param.wlength{2};
else
   len=param.wlength;
   param.lopt='approx';
end
param.nlag=round(0.5*len/seismic.step);
param.lag=param.nlag*seismic.step;

if strcmpi(param.window,'none')
   param.window='rect';
end

%ntr=size(seismic.traces,2);

%       Remove leading and trailing null values in the trace data and replace others by zeros
if isnull(seismic)
   seismic=s_rm_trace_nulls(seismic);
   disp(['Dataset "',seismic.name,'" has null values; they have been replaced by zeros.'])
end

switch param.type

                case 'zero-phase'
wavelet=zero_phase_wavelet_no1(seismic,param);

                case {'min-phase','max-phase'}
wavelet=min_max_phase_wavelet_no2(seismic,param);

                otherwise
error([' Unknown or un-implemented type: "',param.type,'"'])
      
end		% End of switch block

%       Scaling
wavelet.traces=wavelet.traces/max(abs(wavelet.traces));

S4M.history=history;
wavelet.tag='wavelet';
wavelet.window_type=param.window;

if strcmpi(param.color,'blue')
   wavelet.info={'Wavelet estimation method: ', ...
                    'Wavelet estimate from seismic (blue reflectivity)'};
else
   wavelet.info={'Wavelet estimation method: ', ...
                    'Wavelet estimate from seismic (white reflectivity)'};
end

if isfield(seismic,'history')
   wavelet.history=seismic.history;
   wavelet=s_history(wavelet,'append',['Window-type: ',param.window, ...
                                      '; length: ',num2str(len),' ms']);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wavelet=zero_phase_wavelet_no1(seismic,param)
% Compute zero-phase wavelet from seismic

%	Compute the spectrum of the wavelet
aspectrum=compute_spectrum_no3(seismic,param);

%	Raw wavelet
filt=fftshift(real(ifft(aspectrum)));

%       Determination of wavelet length
nlag=param.nlag;
if strcmp(param.lopt,'approx')
   index=find(filt(1:nlag+1).*filt(2:nlag+2) <= 0,1,'last');
   if isempty(index)
      filt=filt(nlag+1:end-nlag);
   else
      nlag=index;
      filt=filt(nlag+1:end-nlag);
   end
else
   filt=filt(nlag+1:end-nlag);
end

if isyes(param.nodc)	% Remove DC component
   filt=filt-0.5*(filt(1)+filt(end));
   filt=lf_dc_removal(filt,1);
else
   filt([1,end])=0.5*filt([1,end]);
end

htext='Zero phase wavelet';
first=0.5*(1-length(filt))*seismic.step;
wavelet=s_convert(filt,first,seismic.step,htext,seismic.units);
wavelet.name='Zero-phase wavelet from seismic';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wavelet=min_max_phase_wavelet_no2(seismic,param)
% Compute minimum-phase or maximum-phase wavelet from seismic


%	Compute the spectrum of the wavelet
aspectrum=compute_spectrum_no3(seismic,param,64*param.nlag);

nlag2=round(param.wlength/seismic.step)+1;
filt=minimum_phase(aspectrum,nlag2);
%sort(abs(roots(filt)))'

if strcmp(param.lopt,'approx')
   index=find(filt(nlag2-1:end-1).*filt(nlag2:end) <= 0,1);
   if ~isempty(index)
      nlag2=index+nlag2-2;
      filt=filt(1:nlag2);
   end
end

if strcmpi(param.type,'min-phase')
   htext='Minimum-phase wavelet';
   wavelet=s_convert(filt,0,seismic.step,htext,seismic.units);
   wavelet.name='Minimum-phase wavelet from seismic';

else
   htext='Maximum-phase wavelet';
   wavelet=s_convert(flipud(filt),(1-nlag2)*seismic.step,seismic.step,htext, ...
      seismic.units);
      wavelet.name='Maximum-phase wavelet from seismic';
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function aspectrum=compute_spectrum_no3(seismic,param,nfft)
%	Compute the amplitude spectrum of a wavelet from seismic data
lag2=2*param.lag;

%	Compute the mean autocorrelation of the seismic
temp=s_correlate(seismic,seismic,{'lags',-lag2,lag2},{'normalize','no'}, ...
       {'option','corresponding'});

[nsamp,ntr]=size(temp.traces);
if ntr > 1
   traces=mean(temp.traces,2);
end

%	Apply taper (if requested)
if ~isempty(param.window) || strcmpi(param.window,'none')  || strcmpi(param.window,'rect') 
   w=mywindow(nsamp,param.window);
   traces=w.*traces;
end

%	Remove the DC component (if requested)
if isyes(param.nodc)
   traces=lf_dc_removal(traces,1);
end

%	Compute spectrum of wavelet
if nargin > 2
   aspectrum=sqrt(abs(fft(traces,nfft)));
else
   aspectrum=sqrt(abs(fft(traces)));
end

%	Correct the spectrum for the color of the reflectivity
switch param.color
case 'blue'
   nsamp=length(aspectrum)-1;
   refl=min((1:nsamp)',(nsamp:-1:1)'); %#ok This is still the better approach 
   refl=sqrt(refl);
   aspectrum(2:end)=aspectrum(2:end)./refl;
   aspectrum(1)=0;
case 'bluish'      % Mixture of white and blue
   nsamp=length(aspectrum)-1;
   refl=min((1:nsamp)',(nsamp:-1:1)'); %#ok This is still the better approach 
   refl=sqrt(refl);
   [dummy,index]=max(aspectrum);       %#ok First output argument is not required
   refl=refl+refl(index);
   aspectrum(2:end)=aspectrum(2:end)./refl;
   aspectrum(1)=0;
case 'white'
   % do nothing
otherwise
   error('Unknown color of reflectivity has been specified. Possible values are: "blue" and "white".')
end  

