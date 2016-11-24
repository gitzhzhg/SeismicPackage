function seismic=s_convert2zero_phase(seismic,wlength)
% Create the zero-phase equivalent of the input data set (usually a wavelet).
% The output data set is centered at time 0; if the input data set has an
% even number of samples the output data set has one more sample. Likewise,
% the input parameter "wlength" will be increased if it would lead to an 
% even number of samples for the output dataset.
%
% Written by: E. Rietsch: August 3, 2004
% Last updated: December 27, 2006: Handle multi-trace dataset
%
%          seismic=s_convert2zero_phase(seismic,wlength)
% INPUT
% seismic  seismic data set to be converted to zero-phase
% wlength  optional argument to specify the length of the zero-phase output;
%          by default (if not given) it is the length of the input dataset 
%          or has one sample more.
%          Making the output dataset longer than the input data set allows
%          better agreement between the amplitude spectra of input and output
% OUTPUT
% seismic  seismic data set zero-phased
%
% EXAMPLE
%      wav1=s_create_wavelet({'type','min-phase'});
%      wav0=s_convert2zero_phase(wav1);
%      s_compare(wav1,wav0)

seismic=s_rm_trace_nulls(seismic);

nsamp=size(seismic.traces,1);

%	Establish the number of samples of the output dataset
if nargin == 2
   nsamp=round(wlength/seismic.step);
end
if mod(nsamp,2) == 0
   nsamph=nsamp/2+1;
   nsamp=nsamp+1;
else
   nsamph=(nsamp+1)/2;
end

temp=fft(seismic.traces,nsamp);

%       Remove zeros in spectrum
temp=abs(temp);

temp=complex(temp,zeros(size(temp)));
temp=real(ifft(temp));
seismic.traces=[temp(nsamph+1:end,:);temp(1:nsamph,:)];
seismic.first=-(nsamph-1)*seismic.step;
seismic.last=-seismic.first;
seismic.name=[seismic.name,' (zero-phased)'];

%	Update history field of output dataset
seismic=s_history(seismic,'append');
