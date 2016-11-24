function [aryout]=conv45(aryin)
%  CONV45 performs 45 degree phase shift for the
%         traces of the input dataset. The process
%         is fulfilled in time domain.
%
%  USAGE:
%
%  function [aryout]=conv45(aryin)
%
%  By Xinxiang Li.
%  CREWES Project, University of Calgary
%  1996
% 
%  The filter vector is from a relative
%  FORTRAN routine by Dr. John C. Bancroft.
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
itrans = 0;
[nrow,nvol]=size(aryin);
if nrow == 1
	aryin=aryin';
	nrow = nvol;
	nvol = 1;
	itrans = 1;
end
aryout=zeros(size(aryin));
filt = [-0.0010 -0.0030,-0.0066,-0.0085,-0.0060, -0.0083, -0.0107,...
-0.0164,-0.0103,-0.0194,-0.0221,-0.0705,0.0395,-0.2161,-0.3831,...
0.5451,0.4775,-0.1570,0.0130,0.0321,-0.0129]';
for j = 1:nvol
	conv1=conv(aryin(:,j),filt);
	aryout(:,j)=conv1(16:nrow+15);
end
if itrans
	aryout = aryout';
end