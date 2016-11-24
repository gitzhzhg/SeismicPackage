function ftseismic=s_fft(seismic,varargin)
% Compute the amplitude spectrum or the Fourier transform
% of the traces of the seismic input data set(s).
% The phase is corrected so that the Fourier transform honors time zero; this
% means that a seismic trace symmetric about time zero will have a real
% Fourier transform; analogously, a trace anti-symmetric about time zero will have
% an imaginary Fourier transform (assuming no rounding errors).
%
% See also: s_edft
%
% Written by: E. Rietsch: February 19, 2001
% Last updated: November 30, 2007: modify frequency sample interval if it 
%                                  was chosen too large; bug fix
%
%	    ftseismic=s_fft(seismic,varargin)
% INPUT
% seismic   seismic dataset or dataset array
% varargin  one or more cell arrays; the first element of each cell array is a
%           keyword, the other elements are parameters. Presently, keywords are:
%      'output' type of output. Possible values are:
%              'amp'    amplitude spectrum (absolute value of Fourier transform)
%              'ft'     Fourier transform (complex)
%           Default: {'output','amp'}
%      'df' sample interval in the frequency domain (achieved by padding)
%           only used if it is less than the sample interval computed as:
%                1000/(seismic.last-seismic.first+seismic.step).
%           This sample interval is also used if {'df',[]}.
%           If one wants to define the number of frequency samples, "nfft",  
%           then "df" needs to be set to
%           df=1000/(step*nfft) where step is the sample interval in time;
%           alternatively: set {'df',-nfft} and have "df" computed internally
%           from "nfft".
%           Default: {'df',2}   
% OUTPUT
% ftseismic  Amplitude spectrum or Fourier transform of traces of 
%            seismic input data
%
% EXAMPLE
%           seismic=s_data;
%           ftseismic=s_fft(seismic,{'output','ft'});
%           s_compare(real(ftseismic),imag(ftseismic),{'times',0,60})
%           mytitle('Real part (black) and imaginary part (red) of FT of seismic data')

% UPDATE HISTORY
%          April 17, 2007: modify Fourier transform to honor time zero

if ~istype(seismic,'seismic')
   if isnumeric(seismic)
      if size(seismic,1) > 1
         seismic=s_convert(seismic,1,1,[],'samples');
      else
         error('The first input argument must be a seismic dataset or a matrix with more than one row.')  
      end
   else
      error('The first input argument must be a seismic dataset or a matrix.')  
   end
end

%	Check if the seismic traces are real
for ii=1:length(seismic)
   if ~isreal(seismic(ii).traces)
      error('Traces of seismic dataset are assumed to be real.')
   end
end

%	Set default parameters
param.output='amp';
param.df=2;
param.window=[];

%	Decode input arguments
param=assign_input(param,varargin);

if ~isempty(param.df)
   if param.df < 0
      param.df=-1000/(seismic.step*param.df);
   end
   
   df0=1000/max([seismic.last]-[seismic.first]+[seismic.step]);
   if param.df > df0
      param.df=df0;
   end
end

for ii=length(seismic):-1:1
   ftseismic(ii)=fft_no1(seismic(ii),param);  %#ok "ftseismic" is not growing 
                           % since it is computed from the highest index down 
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ftseismic=fft_no1(seismic,param)
% Perform the actual FFT

if isnull(seismic)
   seismic=s_rm_trace_nulls(seismic);
end

nsamp=size(seismic.traces,1);
nyquist=500/seismic.step;
if ~isempty(param.df) && param.df > 0
   nfft=2*nyquist/param.df;
else
   nfft=nsamp;
end
freq=(0:2:nfft)*nyquist/nfft;

ftseismic=seismic;
ftseismic.first=0;
ftseismic.last=freq(end);
ftseismic.step=freq(2);
ftseismic.units=units2frequencydomain(seismic.units);
if isempty(param.window)
   temp=seismic.traces;
else
   try
      wndw=mywindow(nsamp,param.window);
      temp=zeros(nsamp,ntr);
      for ii=1:ntr
         temp(:,ii)=seismic.traces(:,ii).*wndw;
      end
   catch
      temp=seismic.traces;
      alert(['Window "',param.window,'" could not be applied'])
   end
end


%       Number of samples in the frequency domain
nsamp=round(ftseismic.last/ftseismic.step)+1;

ftseismic.traces=fft(temp,nfft);
ftseismic.traces=ftseismic.traces(1:nsamp,:);


switch param.output

case 'amp'
   ftseismic.traces=abs(ftseismic.traces);
   ftseismic.name=['Amplitude spectrum of "',seismic.name,'"'];
   htext='Amplitude spectrum';

case 'ft'
%        Use "twiddle" factor to correct the phase so that the spectrum
%        honors time zero; consequently, a data set symmetric about time zero
%        will have a zero imaginary part; one anti-symmetric about time zero will
%        have a purely imaginary Fourier transform (within rounding errors)
   twf=exp((-2*pi*seismic.first/(nfft*seismic.step)*i)*(0:nsamp-1)).';
   for ii=1:size(seismic.traces,2)
      ftseismic.traces(:,ii)=ftseismic.traces(:,ii).*twf;
   end
   htext='Fourier transform';
   ftseismic.name=['Fourier transform of "',seismic.name,'"'];

otherwise
   error(['Unknown output option: ',param.output])

end

%    Append history field
if isfield(seismic,'history')
   ftseismic=s_history(ftseismic,'append',htext);
end
