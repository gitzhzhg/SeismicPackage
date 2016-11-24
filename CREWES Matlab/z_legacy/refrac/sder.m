function[delts1,delts2,avgmdxs1,avgmdxs2]=sderv(mddiff3,mddiff4,avgmdx1,...
avgmdx2,delt1,delt2)
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

clear delts1
%clear dts2
[m,n]=size(avgmdx1);
if( size(mddiff3) ~= 0 )
  delts1=delt1(1:n-2)-delt1(3:n);
  avgmdxs1=(avgmdx1(1:n-2)+avgmdx1(3:n))/2;
%  dts1 = clip(delts1,1);
else
  avgmdxs1 = [];
end
clear delts2
%clear dts2
[m,n]=size(avgmdx2);
if( size(mddiff4) ~= 0 )
  delts2=delt2(1:n-2)-delt2(3:n);
  avgmdxs2=(avgmdx2(1:n-2)+avgmdx2(3:n))/2;
%  dts2 = clip(delts2,1);
else 
  avgmdxs2 = [];
end