function a = auto(v,n,flag)
% AUTO: single-sided autocorrelation
%
% a=auto(v,n,flag)
% a=auto(v,n)
% a=auto(v)
%
% auto computes n lags of the one sided autocorrelation of 
% the vector 'v'. The first lag, a(1), is termed the 'zeroth lag'
% 
% v= input vector
% n= number of lags desired (can be no larger than length(v)).
%    ********* default =length(v) *********
% flag= 1.0 ... normalize the 'zero lag' (first returned value)
%               to 1.0.
%        anything else ... don't normalize
%       ******* default =1.0 ******
% a= one sided autocorrelation returned as a row vvector. a(1) is zero lag.
%
% NOTE: A two sided autocorrelation or a cross correlation can be
%       computed with XCORR in the signal toolbox.
%
%   by G.F. Margrave, May 1991
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
 if (nargin==2)
  flag=1.0;
 end
 if (nargin==1)
   n=length(v);
   flag=1.0;
 end 
% 
% master loop
% 

 [l,m]=size(v);
 done=0;
 a=zeros(1,n);
% for row vectors
 if l==1 
   u=v';
   for k=1:n
     a(k)=v*u;
     v=[0.0 v(1:length(v)-1)];
   end
   done=1;
 end
% for column vectors
 if m==1 
   u=v';
   for k=1:n
     a(k)=u*v;
     u=[0.0 u(1:length(u)-1)];
   end
   done=1;
 end
 if done==0
   error(' input not a vector')
 end
% normalize
 if flag==1.0
   a=a/max(a);
 end

			     