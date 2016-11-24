function [Ux,Uxt0,Uz,Uzt0].....
    = fdInitArrayB(nxf,nzf)
% function [Ux,Uxt0,Uz,Uzt0].....
%     = fdInitArrayB(nxf,nzf)
%Declare space for arrays carried from step to step
%Initialize to zero only at first step
%The input parameters are
%nxf     .... Number of X samples including border
%nzf     .... Number of Z samples including border
%The output parameters are
%Ux   ....... X displacements in an X/Y grid
%Uxt0   ..... X displacements in an X/Y grid, previous time step
%Uz   ....... Z displacements in an X/Y grid
%Uzt0   ..... Z displacements in an X/Y grid, previous time step
%
% P.M. Manning, Dec 2011
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

%nzp = nz+1;
Ux = zeros(nxf,nzf);
Uxt0 = zeros(nxf,nzf);
Uz = zeros(nxf,nzf);
Uzt0 = zeros(nxf,nzf);