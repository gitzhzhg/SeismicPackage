function seismic=s_convert2minimum_phase(seismic,wlength)
% Create the minimum-phase equivalent of the input data set (usually a wavelet).
% The output data set starts at time 0 and ends at time "wlength".
% 
% Written by: E. Rietsch: March 30, 2006
% Last updated:
%
%          seismic=s_convert2minimum_phase(seismic,wlength)
% INPUT
% seismic  seismic data set to be converted to it minimum-phase equivalent
% wlength  optional argument to specify the length of the minimum-phase output;
%          by default (if not given), it is the length of the input dataset.
%          Making the output dataset longer than the input data set allows
%          better agreement between the amplitude spectra of input and output
% OUTPUT
% seismic  seismic data set converted to minimum phase
%
% EXAMPLE
%      wav0=s_create_wavelet({'type','ricker'});
%      wav1=s_convert2minimum_phase(wav0,60);
%      s_compare(wav0,wav1)
%      s_spectrum(wav0,wav1)

bool=isa(seismic.traces,'single');

%       Remove any null values 
seismic=s_rm_trace_nulls(seismic);

%       Determine the number of samples of the output wavelet
if nargin > 1
   nsamp=round(wlength/seismic.step)+1;
   ntr=size(seismic.traces,2);
else
   [nsamp,ntr]=size(seismic.traces); 
end

%       Compute traces of minimum-phase wavelet
temp=zeros(nsamp,ntr);
for ii=1:ntr
   temp(:,ii)=minimum_phase_wavelet(double(seismic.traces(:,ii)),nsamp);
end
seismic.traces=temp;
seismic.last=(nsamp-1)*seismic.step;
seismic.first=0;
seismic.name=[seismic.name,' (minimum-phased)'];

%	Update history field of output dataset
seismic=s_history(seismic,'append');

if bool
   seismic=single(seismic);
end
