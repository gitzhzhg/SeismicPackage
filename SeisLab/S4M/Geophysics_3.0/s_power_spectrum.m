function powerspect=s_power_spectrum(seismic,varargin)
% Power spectrum (amplitude spectrum) of seismic traces
% using the windowed autocorrelation function.
%
% Written by: E. Rietsch: November 3, 2004
% Last updated: December 1, 2007: revamped input argumnets
%
%             powerspect=s_power_spectrum(seismic,varargin)
% INPUT
% seismic     seismic data set
% varargin    one or more cell arrays; the first element of each cell array
%             is a keyword, the other elements are parameters. 
%             Presently, keywords are:
%     'df'    Sample interval in the frequency domain in herz; 
%             "df" must be less than "df0" where df0 = 1000/wlength.
%             If this is not the case it will be set to "df0".
%             Default: {'df',[]} which means that "df" will be set to "df0".
%     'type'  type of spectrum to compute; possible values are 'amplitude' 
%             and 'power'.
%             Default: {'type','power'}
%     'window'  window type 
%             Possible names are :
%             'Hamming', 'Hanning', 'Nuttall',  'Papoulis', 'Harris',
%             'Rect',    'Triang',  'Bartlett', 'BartHann', 'Blackman'
%             'Gauss',   'Parzen',  'Kaiser',   'Dolph',    'Hanna',
%             'Nutbess', 'spline',  'none'
%             Default" {'window','Papoulis'} 
%     'wlength', window length (essentially length of the autocorrelation function
%             used for spectrum estimation); 
%             generally recommended: effective wavelet length
%             Default: {'wlength',seismic.last-seismic.first}
% OUTPUT
% powerspect  Power spectrum (or amplitude spectrum) of the input traces
%             one output trace for each input trace
% EXAMPLE
%             seismic=s_data;
%             powerspect=s_power_spectrum(seismic,{'window','papoulis'});
%             s_wplot(s_stack(powerspect))


%       Set default values for input parameters
param.df=[];
% param.nfft=max(1024,nsamp);
param.wlength=seismic.last-seismic.first;
param.type='power';
param.window='Papoulis';

%       Replace defaults by input parameters
param=assign_input(param,varargin);

[nsamp,ntr]=size(seismic.traces);
df0=1000/param.wlength;
if isempty(param.df)
   param.df=df0;
elseif param.df > df0
   param.df=df0;
end

corr=zeros(2*nsamp-1,ntr);
for ii=1:ntr
   corr(:,ii)=correlate(seismic.traces(:,ii),seismic.traces(:,ii),true);
end
corr=corr/nsamp;
  
nsamp4corr=min(round(param.wlength/(2*seismic.step)),nsamp-1)+1;
ia=nsamp-nsamp4corr+1;

corr=corr(ia:2*nsamp-ia,:);
switch lower(param.window)
case {'none','rect'}
   % Do nothing
case {'hamming', 'hanning', 'nuttall',  'papoulis', 'harris', ...
      'triang',  'bartlett', 'barthann', 'blackman', ...
      'gauss',   'parzen',  'kaiser',   'dolph',    'hanna', ...
      'nutbess', 'spline'};  
   wndw=mywindow(2*nsamp4corr-1,param.window);
   for ii=1:ntr
      corr(:,ii)=corr(:,ii).*wndw;
   end
%   corr=vmt(corr(ia:2*nsamp-ia,:),wndw(:));

otherwise
   disp([' Unknown window type:', param.window])
   disp(' Possible windows are:')
   disp(wndws)
   error('Abnormal termination')
end

nfft=max(size(corr,1),1000/(seismic.step*param.df));
%param.nfft=max(param.nfft,size(corr,1));
temp=fft(corr,nfft);

%	Recompute the possibly adjusted sample interval in the frequency domain
step=1000/(seismic.step*nfft);

if mod(nfft,2) == 0
   nsamp4power=nfft/2+1;
else
   nsamp4power=(nfft+1)/2;
end

powerspect.type='seismic';
powerspect.tag='spectrum';
powerspect.name=['Spectrum (',seismic.name,')'];
powerspect.first=0;
powerspect.last=step*(nsamp4power-1);
powerspect.step=step;
powerspect.units='Hz';
powerspect.traces=abs(temp(1:nsamp4power,:));
if strcmpi(param.type,'power')
   powerspect.name=['Power spectrum (',seismic.name,')'];
   powerspect.traces=abs(temp(1:nsamp4power,:));
else
   powerspect.name=['Amplitude spectrum (',seismic.name,')'];
   powerspect.traces=sqrt(abs(temp(1:nsamp4power,:)));
end

if ~any(isnan(powerspect.traces))
   powerspect.null=[];
else
  powerspect.null=NaN;
end

%    Append history field
if isfield(seismic,'history')
   powerspect.history=seismic.history;
   powerspect=s_history(powerspect,'append',param.window);
end
