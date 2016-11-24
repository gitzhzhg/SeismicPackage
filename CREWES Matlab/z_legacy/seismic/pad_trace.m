function trout=pad_trace(trin,trdsign,flag)
% trout=pad_trace(trin,trdsign,flag)
%
% pad_trace pads (or truncates) trin to the same length as trdsign
%
% trin= input trace to be padded (truncated)
% trdsign= design trace to give desired length
%   if trdsign is a scalar, then it is assumed to give the desired
%   length.
% flag=0 the pad is added to the end of trin
%     =1 the pad is added such that the central sample of trin
%        stays in the middle.
% ***********default=0 *************
% trout= output trace
%
% by G.F. Margrave, June 1991
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
 if nargin<3, flag=0; end
 [m,n]=size(trin);
 nout=length(trdsign);
 if(nout==1) nout = trdsign; end
if flag==0
 if n>m
   if nout>=n
     trout=[trin zeros(1,nout-n)];
   else
     trout=trin(1:nout);
   end
 else
   if nout>=m
     trout=[trin;zeros(nout-m,1)];
   else
     trout=trin(1:nout);
   end
 end
else
 if n>m
   if nout>=n
     npad=nout-n;
     nh=fix(npad/2);
     trout=[zeros(1,nh) trin zeros(1,npad-nh)];
   else
     ncut=n-nout;
     trout=trin(fix(ncut/2):fix(ncut/2)+nout-1);
   end
 else
   if nout>=m
     npad=nout-m;
     nh=fix(npad/2);
     trout=[zeros(nh,1);trin;zeros(npad-nh,1)];
   else
     ncut=nout-m;
     trout=trin(fix(ncut/2):fix(ncut/2)+nout-1);
   end
 end
end