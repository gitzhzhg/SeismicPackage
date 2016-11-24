function trout=tvfilt(trin,t,tvs,trow,fcol,fwidth,fmin,fmax)

%   trout=tvfilt(trin,t,tvs,trow,fcol,fwidth,fmin,fmax)
% 
% TVFILT applies a time variant filter in the frequency domain
% 
% trin= input trace
% t= time coordinate vector
% tvs= input time variant filter matrix (real or complex)
% trow= row vector specifying the time of each row of tvs
% fcol= row vector specifying the frequency of each column of tvs
% fwidth= half-width of frequencies retained about the diagonal
% 
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

%

%force a row vector in. The return will be forced to a column vector
trin=trin(:)';
t=t(:)';

% transform the input trace
  [Trin,f]= fftrl(trin,t);
% error check
  if length(f) ~= length(fcol)
    error(' tvs frequencies incompatible with input trace')
  end
% remove the average temporal amplitude variation
  [nrows,ncols]=size(tvs);
  df=f(2)-f(1);
 % nf1=fix(fmin/df)+1;
 % nf2=fix(fmax/df)+1;
%  avattn=sum(abs(tvs(:,nf1:nf2).'));
%  ind=find(avattn>100*eps);
%  avattn(ind)=1. ./avattn(ind);
%  for jr=1:ncols
%    tvs(:,jr)=tvs(:,jr).*avattn';
%  end 

%
% transform the columns of the tvs 
%
  %nrows=2.^nextpow2(nrows-1);
  Tvs=fft(tvs,nrows);
  Tvs=fftshiftc(Tvs);
% Tvs= fftshiftm(fft(tvs,nrows));
  fprime=freqfft(trow,nrows);

% error check
  chk=abs((fprime(2)-fprime(1))-(f(2)-f(1)));
  if chk > .00001
   error(' tvs times range incompatible with input trace')
  end
% determine the required frequencies (fprime)
  if fwidth>=abs(fprime(1))
    fwidth=abs(fprime(2)); % This ensures an odd length operator
  end
  indicies=find(abs(fprime)<=fwidth);
% Form the frequency domain Time Variant operator
  Tvop=zeros(ncols,length(indicies)); 
  for icol=1:ncols
   Tvop(icol,:)=Tvs(indicies,icol).';
  end
% apply the operator
  Trout=zeros(1,length(Trin));
  op_length=length(indicies);
  pad=fix(op_length/2);
  nTrin=length(Trin);
   Trin=[conj(Trin(pad+1:-1:2)) Trin conj(Trin(nTrin-1:-1:nTrin-pad))];
 %  Trin=[zeros(1,pad) Trin zeros(1,pad)];


  for kf=1:length(Trout)
    Trout(kf) = Tvop(kf,op_length:-1:1)*Trin(kf:kf+op_length-1).';
  end
% inverse fft
  trout=ifftrl(Trout,f);
  trout=trout(:);
% apply the average attenuation
%  avattn=spline(trow,avattn,t);
%  ma=max(avattn);
%  small=.00001*ma;
%  ind=find(avattn<small);
%  if(ind~=[])
%	avattn(ind)=small*ones(size(ind));
%  end
%  trout=trout./avattn;
%    

  
  
       
      






