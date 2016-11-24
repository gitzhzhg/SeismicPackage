function attributes=s_frequency_attributes(seismic,varargin)
% Compute median frequency, bandwidth, and power spectrum of seismic input data
% Written by: E. Rietsch: February 22, 2005
% Last updated: March 7, 2005: Handle also matrices
%
%              attributes=s_frequency_attributes(seismic,varargin)
% INPUT
% seismic      seismic data or matrix
% varargin     one or more cell arrays; the first element of each cell array is a keyword,
%              the other elements are parameters. Presently, keywords are:
%     'window'   window to use for spectrum computation. Possible window names are:
%	      'Hamming', 'Hanning', 'Nuttall',  'Papoulis', 'Harris',
%	      'Rect',    'Triang',  'Bartlett', 'BartHann', 'Blackman'
%	      'Gauss',   'Parzen',  'Kaiser',   'Dolph',    'Hanna'.
%	      'Nutbess', 'spline',  'none'
%             'Rect' and 'none' have the same effect as has {'window',[]};
%             Case is irrelevant (e.g. 'Hanning' and 'hanning' are the same)
%             If the number of traces goes into the thousands then windowing 
%             becomes time consuming and may not be necessary.
%             Default: {'window','none'}
% OUTPUT
% attributes   structure with fields
%              'bandwidth'         average bandwidth of the seismic data set
%              'bandwidth_octaves' bandwidth in octaves
%              'median_frequency'  median frequency
%              'power_spectrum'    power spectrum
%              'frequencies'       frequency associated with each sample 
%                                  of "power_spectrum"
%             If the input data set is a matrix, frequencies and bandwidth 
%             are in fractions of the Nyquist frequency
%             If no output argument is supplied the function prints out 
%             bandwidth and median frequency.

global ABORTED S4M

param.window=[];
param.average='yes';

param=assign_input(param,varargin);

if isnumeric(seismic)  &&  size(seismic,1) > 1 % "seismic" is matrix}
   seismic=s_convert(seismic,0,500);
end
    
[nsamp,ntr]=size(seismic.traces);
nfft=max(2^nextpow2(nsamp),256);

%     Compute bandwidth, median frequency, and integrated spectrum
if isempty(param.window)  ||  strcmpi(param.window,'none')  ||  strcmpi(param.window,'rect')
   ftraces=fft(seismic.traces,nfft);
else
%       Set up display of progress bar
   gui_active(1);       % Add an abort button
   if ~S4M.deployed     % Reset persistent variable in "progressbar"
      clear progressbar % Commented out to allow compilation
   end

   h=progressbar([],0,'Computing windowed spectrum ...');
   wind=mywindow(nsamp,param.window);
   ftraces=zeros(nfft,ntr);
   for ii=1:ntr
      if ishandle(h)
         h=progressbar(h,1/ntr);
      end
      if ~gui_active
         ABORTED=true;
         progressbar(h,-1);
         return
      end
      drawnow

      ftraces(:,ii)=fft(seismic.traces(:,ii).*wind,nfft);
   end
   progressbar(h,-1);
end

nfreq=nfft/2+1; 	                     % Length(ftraces) is even by design
power_spectrum=abs(ftraces(1:nfreq,:)).^2;   % Keep only positive frequencies 

if strcmpi(param.average,'yes')
   power_spectrum=sum(power_spectrum,2);
   ntr=1;
end

cs=cumsum(power_spectrum);

fnyquist=500/seismic.step;
df=2*fnyquist/nfft;
freq=(0:1:nfreq-1)*df;


med_freq=NaN(1,ntr);
for ii=1:ntr
   if cs(end,ii) > 0
      index=find(diff(cs(:,ii)) > 0);
      med_freq(ii)=interp1(cs(index,ii),freq(index),cs(end,ii)*0.5);
   end
end

%	Compute bandwidth
bandwidth=NaN*zeros(1,ntr);
temp=max(power_spectrum);
idx=find(temp > 0);
bandwidth(idx)=df*cs(end,idx)./temp(idx);  % Mean spectrum divided by spectral maximum
octaves=log2((med_freq+0.5*bandwidth)/(max(med_freq-0.5*bandwidth,eps)));

if nargout > 0
   attributes.bandwidth=bandwidth;
   attributes.bandwidth_octaves=octaves;
   attributes.median_frequency=med_freq;
   attributes.power_spectrum=power_spectrum;
   attributes.frequencies=freq;
else
   disp(['Bandwidth = ',num2str(round(bandwidth)),' Hz'])
   disp(['Bandwidth in octaves = ',num2str(round(100*octaves)/100)])
   disp(['Median frequency = ',num2str(round(med_freq)),' Hz'])
end

%figure
%plot(freq,power_spectrum)
%keyboard
