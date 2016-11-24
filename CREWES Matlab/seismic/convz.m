function s=convz(r,w,nzero,nout,flag,pct)
% CONVZ: convolution then truncation for non-min phase filters
%
% s= convz(r,w,nzero,nout,flag,pct)
% s= convz(r,w,nzero,nout,flag)
% s= convz(r,w,nzero,nout)
% s= convz(r,w,nzero)
% s= convz(r,w)
%
% CONVZ is designed for a convenient convolution of a seismic
% trace with a zero phase (no time delay) wavelet. This can 
% actually be used with any non-causal wavelet by specifying the
% nzero sample which is the sample number of zero time. It
% defaults to the middle of w but can be placed anywhere. 
% If the first input argument is either a row or column vector, then 
% the output will be a similar vector. If the first argument is a matrix,
% then the output is a matrix of similar size where w has been convolved
% with each column of r.
% Uses MATLAB's CONV function.
%
% s= output trace of length nout
% r= input trace (reflectivity)
% w= input wavelet
% nzero= sample number of zero time value for wavelet
%  *********** default=ceil((length(wavelet)+1)/2) ***************
% NOTE: nzero may be input as the time-coordinate vector for w. In this case, its length must equal
%   that of w and there must be a t==0 sample somewhere in this vector. convz will find this
%   sample and use it for nzero.
% nout= length of output trace. 
%   ********** default=length(r) ************
% flag= 1 --> apply a cosine taper at the beginning and end of the
%            output trace
%     = 0 --> don't apply a taper
%      ********* default= 1 **********
% pct= percent taper applied at both ends of the result to reduce
%       trunctation effects. See mwindow.
% ********** default= 5% ************
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


 [nsamps,ntr]=size(r);
 %convert to column vectors
 transpose=0;
 if(nsamps==1); r=r.'; ntr=1; nsamps=length(r); transpose=1;end
 if(nargin<6)
     pct=5;
 end
 if(nargin<5)
     flag=1;
 end
 if(nargin<4)
     nout=nsamps;
 end
 small=100*eps;
 if(nargin>=3 && length(nzero)>1)
     if(length(nzero)~=length(w))
         error('if nzero is a vector it must equal w in length')
     end
    ind=near(nzero,0);
    if(abs(nzero(ind(1)))>small)
        error('no time zero sample found in vector nzero')
    end
    nzero=ind(1);
 end
 if(nargin<3)
     %nzero=round((length(w)+1)/2);
     nzero=ceil((length(w)+1)/2);
 end
%
% 
w=w(:);
s=zeros(nout,ntr);
for k=1:ntr
    temp=conv(r(:,k),w);
    if(flag~=1)
        s(:,k)=temp(nzero:nout+nzero-1);
    else
        s(:,k)=temp(nzero:nout+nzero-1).*mwindow(nout,pct);
    end
end

if(transpose)
	s=s.';
end
   