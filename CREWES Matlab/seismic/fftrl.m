function [spec,f]= fftrl(s,t,percent,npad)
% FFTRL: forward Fourier transform for real-valued signals.
%
% [spec,f]= fftrl(s,t,percent,npad)
% [spec,f]= fftrl(s,t,percent)
% [spec,f]= fftrl(s,t)
%
% Forward fourier transform of a real-valued signal. Relative to
% MATLAB's fft it returns only the positive frequencies in an array
% roughly half the size. If there are n input time samples, then there
% will be floor(n/2+1) frequency samples. This means that if n is an even
% number, then a time series of length n and one of length n+1 will produce
% frequency spectra of the same length. However, only the first case will
% have a sample at Nyquist. If the input trace is a vector, then the return
% is simply the transform of that vector. If a matrix, then each 
% column of the matrix is transformed. The inverse is accomplished by
% IFFTRL.
% NOTE: FFTRL, like fft, uses the + sign in the complex Fourier
% exponential, while ifftrl uses the - sign and divides by n.
%
% s= input signal (trace or gather, i.e. vector or matrix or 3D array) 
% t= input time coordinate vector
% percent= specifies the length of a raised cosine taper to be
%          applied to s (both ends) prior to any padding. 
%          Taper is a percent of the length of s. Taper is 
%          applied using MWINDOW.
%		********** Default=0% ***********
% npad= length (in samples) to which the input trace is to be padded with zeros.
%     ********** Default is the input length (no pad) ***********
% NOTE: a value of 0 for npad is taken to mean no pad is desired.
%  
% spec= output spectrum
% f= output frequency sample vector
%
% by G.F. Margrave, May 1991, updated June 2004
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE
 
% set defaults
 if(nargin<4)
		npad=length(t);
 else
     if(npad==0)
         npad=length(t);
     end
 end
 if(nargin<3)
   percent=0.0;
 end
 
 %test for 3D
%  test=size(s);
%  if(length(test)==3)
%      threeD=1;
%  else
%      threeD=0;
%  end
 
% determine number of traces in ensemble
 [l,m1,m2]=size(s);
 nx=1;
 ny=1;
 %test for row vector
 itr=0; %transpose flag
 if(l==1 && m2==1) 
     nsamps=m1; itr=1; s=s(:); %switch to column vectors
 elseif(m1==1 && m2==1) 
     nsamps=l;
 elseif(m1~=1)
 	nsamps=l; nx=m1; ny=m2;
 else
     error('unrecognizable data array')
 end
 if(nsamps~=length(t))
		t=t(1)+(t(2)-t(1))*(0:nsamps-1);
        if(nargin<4)
            npad=nsamps;
        end
	%error(' time vector and trace matrix don''t match in length');
 end
 
% apply the taper
 if(percent>0)
	 mw=mwindow(nsamps,percent);
	 mw=mw(:,ones(1,nx),ones(1,ny));
	 s=s.*mw;
	 clear mw
 end
% pad s if needed : NOT NEEDED, fft will pad
%  if (nsamps<n),
% 	s=[s;zeros(n-nsamps,ntraces)];
%   	nsamps=n; 
%  end

% transform the array
	spec=fft(s,npad);
    spec=spec(1:floor(npad/2+1),:,:);% save only the positive frequencies
%     if(threeD)
%         spec=spec(1:floor(n/2+1),:,:);% save only the positive frequencies
%     else
%         spec=spec(1:floor(n/2+1),:);% save only the positive frequencies
%     end
	clear s;
    
% build the frequency vector
fnyq=1. / (2*(t(2)-t(1)));
 nf=size(spec,1);
 df=2*fnyq/npad;
 %f=linspace(0.,fnyq,nf)';
 f=df*(0:nf-1)';
 
 if(itr)
 	f=f';
 	spec=spec.';
 end