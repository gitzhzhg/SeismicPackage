function [phase,aux]=signal_phase_spectrum(signal,nfft,timezero,tiepoint)
% Compute phase spectrum and, if also requested, amplitude spectrum) of a signal. 
%
% Written by: E. Rietsch: September 2, 2007
% Last updated: September 29, 2009: Force phase to be zero at DC for real
%                                   signals
%
%         [phase,aux]=signal_phase_spectrum(signal,nfft,timezero,tiepoint)
%                      
% INPUT
% signal  vector with signal samples
% nfft    minimum length of samples of the FFT
% timezero time of first sample; if it is empty the zero-time of the signal is
%         shifted to that sample where the Hilbert amplitude has its maximum
% tiepoint  since the phase is only determined up to a multiple of 2*pi 
%         (360 degrees) this paramter specifies where the phase is forced 
%         to be in the range of (-180,180]. For arbitrary wavelets this is
%         usually the dominant frequency (e.g. the term "60-degree wavelet")
%         For a minimum-phase wavelet one would prefer DC (zero frequency).
%         Hence "tiepoint" has two values "zero' and 'peak'.
% OUTPUT
% phase   column vector with phase (degrees); unwrapped
% aux     structure with auxiliary output; the following fields are present:
%    'index'   sample associated with zero-time (peak of Hilbert transform); 
%         if applyShift is false this index is set to 1 (first sample);
%         otherwise it is the sample where the Hilbert amplitude has its
%         maximum.
%    'freq'    frequencies as a fraction of the Nyquist frequency
%    'even'  true if "signal" has an even number of samples; false otherwise
%
% EXAMPLE
%         [phase,aux]=signal_phase_spectrum([-2,-1,0,1,2],8,true,'peak')

% UPDATE HISTORY
%         October 8, 2007: bug fix in phase rescaling
         
         
[signal,nshifts]=shiftdim(signal);  % Make sure that first dimension is not singleton

nfft=max(nfft,length(signal));

if isempty(timezero)
%       Find the maximum Hilbert amplitude and circularly shift the signal
%       so that the the sample with the peak of the Hilbert transform is
%       the first sample
   hp=abs(myhilbert([0;signal;0]));   % Pad signal to improve Hilbert transform
   [dummy,index]=max(hp);             %#ok  First output argument is not required
   index=index-1;
   temp(nfft)=0;
   temp(nfft-index+2:nfft)=signal(1:index-1);
   temp(1:length(signal)-index+1)=signal(index:end);
   ftsignal=fft(temp);
   
else
   index=1;
   ftsignal=fft(signal,nfft);
end

%       Compute frequency samples
nyquist=1;
even=mod(nfft,2) == 0;
if even
   nsamp=nfft/2;
   freq=(0:nsamp)*(nyquist/nsamp);
else
   nsamp=(nfft-1)/2;
   freq=(0:nsamp)*(nyquist/(nsamp+0.5));
end
nsamp=nsamp+1;

ftsignal=ftsignal(1:nsamp);

%       Compute unwrapped phase
rawphase=atan2(imag(ftsignal),real(ftsignal));
if isreal(signal)     % If the signal is real the phase at zero-frequancy must be zero
   rawphase(1)=0;
end
phase=(180/pi)*(unwrap_phase(rawphase));
phase=shiftdim(phase,nshifts);

switch tiepoint
case 'peak'
%	Find peak of the amplitude spectrum and add/subtract multiples of 2*pi so
%       that the phase there is in the interval (-pi,pi]
   aft=abs(ftsignal);
   [dummy,idx]=max(aft);   %#ok First argument is not required
   phase0=phase(index);
   phasex=mod(phase0+180,360)-180;
   dphase=phasex-phase0;
   phase=phase+dphase;
   
case 'zero'
%  Do nothing

otherwise
   disp(['Illegal value for input argument "tiepoint": ',tiepoint])
   phase=[];
end

if nargout > 1
   aux.index=index;
   aux.freq=freq;
   aux.even=even;
end
