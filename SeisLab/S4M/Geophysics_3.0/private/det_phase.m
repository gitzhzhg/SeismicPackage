function [phase,t0]=det_phase(signals,indegrees)
% Compute the "phase" of the signals in the columns of matrix "signals"
% The phase is computed from the phase angle at the peak spectral amplitude
% at the best time shift (origin at the center of signal energy) 
% and "signal" at the maximum of the instantaneous amplitude.
% It is assumed that each column of "signal" has only one such maximum
% See also:  det_phase6
%
% Written by: E. Rietsch: March 14, 2004
% Last updated: May 29, 2007: Add a second input argument
%
%         [phase,t0]=det_phase(signals,indegrees)
% INPUT
% signal  matrix; each column represents the samples of a time series
% indegrees   optional second input argument; logical; if given and "true"
%         then the phase output will be in degrees
% OUTPUT
% phase   phase (in radians or degrees) of each column of "signals"; row vector;
%         -pi <= phase <= pi if there is only one input argument or if the 
%         second input argument is "false";
%         otherwise: phase is in degrees and -180 <= phase <= 180
% t0      index to the mean of signal energy (must be multiplied by 
%         the sample interval to get time);
%         the index of the first sample is zero
%
% EXAMPLE
%        wavelet=s_create_wavelet({'type','min-phase'});
%        phase=(180/pi)*det_phase(wavelet.traces)


%       If input is a seismic dataset then use field "traces".
if istype(signals,'seismic')
   signals=signals.traces;
end

signals=shiftdim(signals);  % Make sure that first dimension is not singleton

[nsamp,ntr]=size(signals);
nfft=256;

%       Compute best start time for phase unwrapping
val2=signals.^2;
t0(1,ntr)=0;
for ii=1:ntr
   t0(ii)=(0:nsamp-1)*val2(:,ii)/sum(val2(:,ii));
end

nsamp=max(nsamp,nfft);
freq=(0:2:nsamp)/nsamp;

%	Compute Fourier transform of input data and location of peak of 
%       amplitude spectrum
nffth=ceil(nfft*0.5);
temp=fft(signals,nsamp);
amp=abs(temp(1:nffth,:));
[dummy,idx]=max(amp);   % Find location of maximum of amplitude spectrum

%	Convert index pair into a single index
index=sub2ind(size(temp),idx,1:ntr);
linphase=pi*freq(idx).*t0;

phase=mod(angle(temp(index))+linphase+pi,2*pi)-pi;

if nargin == 2  &&  indegrees
   phase=phase*(180/pi);
   phase(phase==-180)=180;
end
