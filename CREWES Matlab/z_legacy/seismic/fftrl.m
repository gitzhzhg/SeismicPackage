function [spec,f]= fftrl(s,t,percent,n)

% [spec,f]= fftrl(s,t,percent,n)
% [spec,f]= fftrl(s,t,percent)
% [spec,f]= fftrl(s,t)
%
% Forward fourier transform of a real trace. This is done in brute
% force fashion to mimic the result of Vern Herbert's real to
% complex FFT. A penalty of a factor of two is paid. Relative to
% MATLAB's fft it returns only the positive frequencies in an array
% half the size. If the input trace is a vector, then the return
% is simply the transform of that trace. If a matrix, then each 
% column of the matrix is transformed.
%
% s= input trace (or gather) 
% t= input time coordinate vector
% percent= specifies the length of a raised coisine taper to be
%          applied to s prior to any padding. Taper is a percent
%          of the length of s. Taper is applied using "mwindow"
%          from the seismic toolbox. Default=0%
% n= length to which the input trace is to be padded with zeros.
%  
% spec= output spectrum
% f= output frequency sample vector
%
% by G.F. Margrave, May 1991
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
		n=length(t);
 end
 if(nargin<3)
   percent=0.0;
 end
% determine number of traces in ensemble
 [l,m]=size(s);
 ntraces=1;
 itr=0; %transpose flag
 if(l==1) nsamps=m; itr=1; s=s(:); %switch to column vectors
 elseif(m==1) nsamps=l;
 else
 	nsamps=l; ntraces=m;
 end
 if(nsamps~=length(t))
		t=t(1)+(t(2)-t(1))*(0:nsamps-1);
		 if(nargin<4)
			n=length(t);
		 end
	%error(' time vector and trace matrix don''t match in length');
 end
 
% apply the taper
 if(percent>0)
	 mw=mwindow(nsamps,percent)';
	 mw=mw(:,ones(1,ntraces));
	 s=s.*mw;
	 clear mw;
	 %for k=1:ntraces,
		%s(:,k)=s(:,k).*mw';
	 %end
 end
% pad s if needed
 if (nsamps<n),
	s=[s;zeros(n-nsamps,ntraces)];
  	nsamps=n; 
 end

% transform the array, This used to be done in a loop to conserve memory
	%spc=ones(size(s))+i*ones(size(s));
	spec=fft(s,nsamps);
	spec=spec(1:round(n/2+1),:);% save only the positive frequencies
	clear s;
    %spec=ones(nsamps/2+1,ntraces)+i*ones(nsamps/2+1,ntraces);
    %for k=1:ntraces,
         %temp=fft(s(:,k));
         %spc(:,k)=temp;
         %spec(:,k)=temp(1:round(n/2+1)); % save only the positive frequencies
    %end
% build the frequency vector
aafnyq=find( t > 0 ); % ever heard of negative time ?
aa1=min(aafnyq); aa2=min(aafnyq)+1;
 fnyq=1./( 2*( t(aa2) - t(aa1) ) );
 nf=size(spec,1);
 f=linspace(0.,fnyq,nf)';
 
 if(itr)
 	f=f';
 	spec=spec.';
 end