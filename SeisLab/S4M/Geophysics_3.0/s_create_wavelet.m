function wav=s_create_wavelet(varargin)
% Function computes wavelet and stores it in a seismic data structure; The
% wavelet is scaled in such a way that its maximum amplitude (maximum of the
% instantaneous amplitude in case of a non-zero-phase wavelet) is equal to 1.
%
% See also: s_create_spike
%
% Written by: E. Rietsch: April 15, 2000
% Last updated: May 19, 2008: bug fix
%
%           wav=s_create_wavelet(varargin)
% INPUT
% varargin  one or more cell arrays; the first element of each cell array is a 
%           keyword, the other elements are parameters. Presently, keywords are:
%     'type'    Type of wavelet. Possible options are:
%           Ormsby wavelets with:
%           'zero-phase'    zero-phase wavelet with trapezoidal  
%                           spectrum defined via keyword 'frequencies'
%           'min-phase'     minimum-phase wavelet with trapezoidal  
%                           spectrum defined via keyword 'frequencies'
%           'max-phase'     maximum-phase wavelet with trapezoidal 
%                           spectrum defined via keyword 'frequencies'
%           or
%           'ricker'        Ricker wavelet
%           Default: {'type','zero-phase'}
%     'frequencies'  For Ormsby wavelets:
%           the four corner frequencies, f1,f2,f3,f4, of a trapezoidal 
%           amplitude spectrum. The corner frequencies must satisfy  
%           the condition 0 <= f1 <= f2 <= f3 <= f4.
%           Default: {'frequencies',10,20,40,60} or, in equivalent notation,
%                    {'frequencies',[10,20,40,60]}
%           For Ricker wavelet: the location of the spectral peak
%           Default: {'frequencies',30}
%     'wlength'  the length of the filter in ms (if wavelet "type" is 
%           'zero-phase' or 'ricker' then 'length' should be an even
%           multiple of the sample interval);
%           Default: {'length',120};
%           If this is not the case it is increased by the sample interval.
%     'step'   sample interval in ms. Default: {'step',4}
%     'option'  In the trade-off between zero-phase property and match to 
%           the design amplitude spectrum this parameter determines 
%           what to favor. 
%           Possible values are: 'amp', 'phase' (requires long wavelet)
%           Default: {'option'.'amp'}
%           Ignored for Ricker wavelet
%     'window'  Apply a window to the wavelet to reduce end effects (only 
%           for Ormsby wavelets; a half window is used for minimum-phase
%           and maximum-phase wavelets). Makes the wavelet spectrum smoother.
%           Possible windows are (case-insensitive):
%           'Hamming', 'Hanning', 'Nuttall',  'Papoulis', 'Harris',
%           'Rect',    'Triang',  'Bartlett', 'BartHann', 'Blackman'
%           'Gauss',   'Parzen',  'Kaiser',   'Dolph',    'Hanna'.
%           'Nutbess', 'spline', 'none'
%           Default: {'window','Parzen'}
%    'dc_removal' Remove DC component (only for zero-phase wavelet)          
%           Possible values are: 'yes' and 'no'
%           Default: {'dc_removal','yes'}
% OUTPUT
% wav     wavelet in form of a seismic structure; the wavelet is scaled in 
%         such a way that the maximum of its Hilbert amplitude is 1.
%         For wavelets of type 'Ricker' and type 'zero-phase' this means
%         that the maximum amplitude of the wavelet is 1 as well.
% 
% EXAMPLES
%         wav=s_create_wavelet
%         wav=s_create_wavelet({'type','min-phase'},{'frequencies',10,10,40,80},{'step',2})
%         wavelet=s_create_wavelet({'type','ricker'},{'frequencies',25'})

% UPDATE HISTORY
%         September 20, 2007: Prevent window name from being added to
%                                   dataset name


global S4M 

run_presets_if_needed

history=S4M.history;
S4M.history=0;

%       Set defaults
param.dc_removal='yes';  % Remove DC component (only for zero=phase wavelets)
param.frequencies=[];
param.length=[];	 % Deprecated parameter
param.option='amp';
param.step=4;
param.type='zero-phase';
param.units='ms';
param.window='Parzen';
param.wlength=120;


%       Decode and assign input arguments
param=assign_input(param,varargin,'s_create_wavelet');

if ~isempty(param.length)
   param.wlength=param.length;
   disp(' Parameter "length" has been deprecated. In the future please use "wlength" instead.')
end

wav.type='seismic';
wav.tag='wavelet';
wav.name='';

%               Compute Ricker wavelet

if strcmpi(param.type,'ricker') 
   if isempty(param.frequencies)
      param.frequencies=30;
   end  
   wav.name=['Ricker wavelet (',num2str(param.frequencies),' Hz)'];
   wl2=round(0.5*param.wlength/param.step)*param.step;
   wav.first=-wl2;
   wav.last=wl2;
   wav.step=param.step;
   wav.units=param.units;
  
%       Compute Ricker wavelet
   beta=((-wl2:param.step:wl2)'*(param.frequencies*pi*0.001)).^2;
   wav.traces=(1.-beta.*2).*exp(-beta);
   wav.traces=wav.traces./max(wav.traces);
   wave.traces.null=[];

%     Create history field
   if S4M.history
      wav=s_history(wav,'add',['Ricker wavelet with peak frequency at ',num2str(param.frequencies)]);
   end

   %	Convert to requested precision
   if strcmpi(S4M.precision,'single')
      wav=single(wav);
   end

   return
end

%               Compute Ormsby wavelets

if isempty(param.frequencies)
   param.frequencies=[10,20,40,60];
end

if iscell(param.frequencies)  &&  length(param.frequencies) == 4
   f1=param.frequencies{1}; 
   f2=param.frequencies{2}; 
   f3=param.frequencies{3};
   f4=param.frequencies{4};

else
   if iscell(param.frequencies)
      param.frequencies=cell2mat(param.frequencies);
   end
   f1=param.frequencies(1); 
   f2=param.frequencies(2); 
   f3=param.frequencies(3); 
   f4=param.frequencies(4);
end

if any(diff([f1;f2;f3;f4]) < 0)
   htext=num2str([f1,f2,f3,f4]);
   error([' Corner frequencies for wavelet spectrum are not monotonic: ',htext]);
end
nsamp=floor(param.wlength/param.step)+1;

wav.name=['Ormsby (' param.type,', ',num2str(f1),'-',num2str(f2),'-', ...
                        num2str(f3),'-',num2str(f4),' Hz)']; 

switch param.type

case 'zero-phase'

   htext=num2str([f1,f2,f3,f4]);
   wav.first=-fix(nsamp/2)*param.step;
   wav.last=-wav.first;

   if strcmpi(param.option,'amp')
      wav.traces=zero_phase_wavelet(nsamp,param.step,f1,f2,f3,f4);
   elseif strcmpi(param.option,'phase')
      wav.traces=zss_wavelet(nsamp,param.step,f1,f2,f3,f4);
   else
      error([' Unknown value (',param.option,') for parameter "option"'])
   end
   htext=['Zero-phase wavelet: corner frequencies ',htext];

   if ~strcmp(param.window,'none')  ||  ~strcmp(param.window,'no')
      wav=s_window(wav,param.window,{'add_window2name',false});
   end

   if isyes(param.dc_removal)
      wav.traces=lf_dc_removal(wav.traces,2);
      % times=linspace(wav.first,wav.last,length(wav.traces))';
      % aa=cos(0.5*pi*times/times(end)).^2;
      % wav.traces=wav.traces-(sum(wav.traces)/sum(aa))*aa;
   end

   wav.traces=wav.traces/max(wav.traces);


case {'min-phase','max-phase'}
   htext=num2str([f1,f2,f3,f4]);
   wav.first=0;
   wav.last=param.wlength;
   wav.traces=minimum_phase_wavelet(nsamp,param.step,f1,f2,f3,f4);
   if strcmpi(param.type,'min-phase')
      htext=['Minimum-phase wavelet: corner frequencies ',htext];
      if ~strcmp(param.window,'none')  ||  ~strcmp(param.window,'no')
         wav=s_window(wav,param.window,{'option',-1},{'add_window2name',false});
      end

   else
      htext=['Maximum-phase wavelet: corner frequencies ',htext];
      wav.first=-param.wlength;
      wav.last=0;
      wav.traces=flipud(wav.traces);
      if ~strcmp(param.window,'none')  ||  ~strcmp(param.window,'no')
         wav=s_window(wav,param.window,{'option',1},{'add_window2name',false});
      end
   end

   wav.traces=wav.traces/max(abs(myhilbert(wav.traces)));


otherwise
   disp([char(13),' Unknown wavelet type: ',param.type])
   disp(' Possible values are: zero-phase, min-phase, max-phase')
   error(' Abnormal termination')

end

wav.step=param.step;
wav.units=param.units;
wav.null=[];

%	Create history field
S4M.history=history;
wav=s_history(wav,'add',htext);

%	Convert to requeste precision
if strcmpi(S4M.precision,'single')
   wav=single(wav);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wav=zero_phase_wavelet(nsamp,dt,f1,f2,f3,f4)
% Function computes zero-phase wavelet for a trapezoidal spectrum. 
% For use within a subroutine; therefore no argument checks
%
% INPUT   
% nsamp    number of samples (if even a sample will be appended to male it odd)
% dt       sample interval
% f1, f2, f3, f4   corner frequencies of trapezoid
% OUTPUT
% wav      zero-phase wavelet
     
if ~mod(nsamp,2)
   nsamp=nsamp+1;
end
temp=zeros(nsamp,1);
temp(fix(nsamp/2)+1)=1;
wav=ormsby(temp,dt,f1,f2,f3,f4);
wav([1,end])=wav([1,end])*0.5;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fa=zss_wavelet(nsamp,dt,f1,f2,f3,f4)
% Function computes zero-phase wavelet with trapezoidal corner 
% frequencies f1, f2, f3, f4 as the correlation of a zero-phase wavelet
% of half the length and and with the square root of the design spectrum.
% The function is for internal use and performs no error checking
%
% INPUT
% nsamp  number of samples
% dt  sample interval in ms
% f1 f2 f3 f4  corner frequencies (0 <= f1 <= f2 <= f3 <= f4 <= fnyquist)
% OUTPUT
% fa  filtered input array
%       fa=ormsby(nsamp,dt,f1,f2,f3,f4)

n=fix(nsamp/2)+1; 
nh=fix((n+1)/2);
fnyquist=500/dt; 
% df=2*fnyquist/n;
% f=[0:df:fnyquist];
f=(0:2:n)*fnyquist/n;

%    Compute trapezoidal window to apply to spectrum
trapez=zeros(n,1);
idx=find(f >= f1 & f <= f4);
f=f(idx);
eps1000=1000*eps;
b1=(f-f1+eps1000)/(f2-f1+eps1000);
b2=ones(1,length(b1));
b3=(f4-f+eps1000)/(f4-f3+eps1000);
trapez(idx)=min([b1;b2;b3]);
trapez(n:-1:n-nh+2)=trapez(2:nh);

%   Perform inverse FFT of the square root of the trapezoidal spectrum
%   and form autocorrelation of the resulting wavelet
gh=sqrt(trapez);
fa=real(ifft(gh));
fa=[fa(nh+1:end);fa(1:nh)];
fa=correlate(fa,fa);
%fa=corr(fa,fa);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wav=minimum_phase_wavelet(nsamp,dt,f1,f2,f3,f4)
% Function computes minimum-phase wavelet for a trapezoidal spectrum. 
% For use within a subroutine; therefore no argument checks
% INPUT   
% nsamp    number of samples (if even a sample will be appended to make it odd)
% dt       sample interval
% f1, f2, f3, f4   corner frequencies of trapezoid
% OUTPUT
% wav      minimum-phase wavelet
     
wav0=zero_phase_wavelet(nsamp,dt,f1,f2,f3,f4);

nfft=pow2(nextpow2(nsamp)+1);
fwav=fft(wav0,nfft);
fwav=abs(fwav);

% amp=convolve([0.25;0.5;0.25],[fwav(end);fwav;fwav(1)]);
amp=convolve([0.25;0.5;0.25],[fwav(1);fwav;0]);

wav=minimum_phase(amp(3:end-2),nsamp);
[dummy,index]=max(abs(wav));

if wav(index) > 0
   wav=wav*norm(wav0)/norm(wav);
else
   wav=-wav*norm(wav0)/norm(wav);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wav=minimum_phase(amp,nsamp)
% Function computes minimum-phase wavelet with given amplitude spectrum
% INPUT
% amp    amplitude spectrum
% nsamp  number of samples of desired wavelet
% dt     sample interval of wavelet
% OUTPUT
% wav    minimum-phase wavelet with amplitude spectrum "amp"

temp=fft(log(amp));
namp=length(amp);
namph=fix(namp/2);

temp=real(temp(2:namph+1)).*(1:namph)'/namph;
wav=ones(nsamp,1);
wav(2)=temp(1);
for ii=2:nsamp-1
   wav(ii+1)=sum(wav(ii:-1:1).*temp(1:ii))/ii;
end 
