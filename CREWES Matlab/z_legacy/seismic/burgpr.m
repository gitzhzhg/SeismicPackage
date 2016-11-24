function pfilt=burgpr(trin,lc)
% pfilt= burgpr(trin,lc)
%
% returns an lc length Burg prediction error filter (unit lag)
%
% trin= input trace 
% lc= number of points in prediction filter 
% pfilt= Burg prediction error filter
%
%
% adapted from Claerbout 1976: Fundamentals of Geophysical Data
% Processing, p 137
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
if lc>length(trin)
	error(' Please check input parameters again');
else
s=[];
% set defaults
%  n=round(length(trin)/2+1);
%
%  specb=zeros(1,n);
%  cpr=zeros(1,lc-1);
 a=[1.0 zeros(1,lc-1)];
 c=zeros(1,lc);
 em=trin;
 ep=trin;
 lx=length(trin);
 for j=2:lc
   bot=ep(j:lx)*ep(j:lx)'+em(1:lx-j+1)*em(1:lx-j+1)';
   top=ep(j:lx)*em(1:lx-j+1)';
   c(j)=2.*top/bot;
   epp=[ep(1:j-1) ep(j:lx)-c(j)*em(1:lx-j+1)];
   em=[em(1:lx-j+1)-conj(c(j))*ep(j:lx) em(lx-j+2:lx)];
   ep=epp;
   s(1:j)=a(1:j)-c(j)*conj(a(j:-1:1));
   a(1:j)=s(1:j);
 end
 pfilt=s;
end
 