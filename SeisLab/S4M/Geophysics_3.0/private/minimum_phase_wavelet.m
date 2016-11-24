function mph_wavelet=minimum_phase_wavelet(wavelet,nsamp)
% Compute the minimum-phase equivalent of the input wavelet (a vector with 
% the wavelet samples)
%
% Written by: E. R.: March 30, 2006
% Last updated:
%
%           mph_wavelet=minimum_phase_wavelet(wavelet)
% INPUT
% wavelet   column vector of samples of an arbitrary wavelet
% nsamp     number of samples of the minimum-phase wavelet
% OUTPUT
% mph_wavelet   column vector of the samples of the minimum-phase equivalent
%           of the input wavelet

nfft=pow2(nextpow2(nsamp)+2);
fwav=abs(fft(wavelet,nfft));

%       Smooth the spectrum
amp=anysmooth([0;fwav;0],[1 2 1],0);

mph_wavelet=minimum_phase(amp(2:end-1),nsamp);

%       Scale output wavelet to input wavelet
mph_wavelet=mph_wavelet*norm(wavelet)/norm(mph_wavelet);
