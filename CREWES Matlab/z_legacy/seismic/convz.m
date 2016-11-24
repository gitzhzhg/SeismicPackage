function s=convz(r,w,nzero,nout,flag)

% s= convz(r,w,nzero,nout,flag)
% s= convz(r,w,nzero,nout)
% s= convz(r,w,nzero)
% s= convz(r,w)
%
% function is designed for a convenient convolution of a seismic
% trace with a zero phase (no time delay) wavelet. This can 
% actually be used with any non-causal wavelet by specifying the
% nzero sample which is the sample number of zero time. It
% defaults to the middle of w but can be placed anywhere. Also, 
% this is designed to produce an output vector of length equal
% to the first input vector (r). Uses MATLAB's CONV function.
%
% s= output trace of length nout
% r= input trace (reflectivity)
% w= input wavelet
% nzero= sample number of zero time value for wavelet
%  *********** default=round((length(wavelet)+1)/2)
% nout= length of output trace. 
%   ********** default=length(r)
% flag= 1 --> apply a cosine taper at the beginning and end of the
%            output trace
%     = 0 --> don't apply a taper
%      ********* default= 0 **********
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

 if(nargin<=4)
     flag=0;
  end
 if(nargin<=3)
     nout=length(r);
 end
 if(nargin==2)
     nzero=round((length(w))/2);
 end
%
% 
%convert to column vectors
[a,b]=size(r);
if(a==1) r=r.'; end
w=w(:);

temp=conv(r,w);
s=temp(nzero:nout+nzero-1);
if(flag==1)
   s=s.*(mwindow(nout,4).');
end

if(a==1)
	s=s.';
end
   