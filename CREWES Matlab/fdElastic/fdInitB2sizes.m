function [ix1,iz1,nxf,nzf,nxplot,initzp,nzplot].....
    = fdInitB2sizes(Dxz,nx,nz,mvXmax,mvZtop,mvZmax,wncMat,iLbnd,shotX)
%Set up index arrays to be used in computations
%Allow for displacements on the left which simulate a 'mirrored surface'
%
%The input parameters are
%Dxz     .... FD sample rate in (metres)
%nx      .... Number of spatial samples in the X direction
%nz      .... Number of spatial samples in the Z direction
%mvXmax  .... X-length of movie frames to display or plot
%mvZtop  .... Z-level at top of each movie frame, often 0.
%mvZmax  .... Z-depth of movie frames to display or plot
%wncmat  .... A set of FD correction matrices, for particular Vp and Vs
%iLbnd   .... Boundary code left
%shotX   .... X (from the FD model left) of the initializing source
%
%The output parameters are
%ix1   ...... X co-ordinate of start of model in arrays
%iz1   ...... Z co-ordinate of start of model in arrays
%nxf     .... Number of X samples including border
%nzf     .... Number of Z samples including border
%nxplot  .... No. of X points to plot
%initzp  .... Initial Z point to plot
%nzplot  .... No. of Z points to plot
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

% nzm = nz-1;
% if iBrigid<=0
%     nzm = nz;
% end
% ixm = 1:nxf-1;
% ixm2 = 2:nxf-1;
% izm = 1:nzm;
% izm2 = 2:nzm;
% izb = nz+1;
% ivxi = ix1:ix1*2-2;     %Indicies to fill in symmetric displacements on left
% ivxo = ix1-1:-1:1;

%Allow for the extra displacement points needed for correction filtering

%disp(wncMat)
[nxC,nzC,nTerms] = size(wncMat);
%disp([nxC,nzC,nTerms])
% nXextra = 1;
% nZextra = 1;
ix = round(shotX/Dxz);      %Find 'edge' shots
nXextra = 0;
nZextra = 0;
%This version - only specify left cross-boundary points for corrections
if nTerms == 5
    if (iLbnd == 7) && (ix<3)
        nXextra = (nxC-1)/2;    %Not necessary for interior source
    end
%     nZextra = (nzC-1)/2;
end
%     iExtrai = 1:nExtra+1;
%     iExtrao = -1:-1:-(nExtra+1);
%     iExtra = 1:nExtra;
%     zFill = zeros(1,nExtra);
%disp(nExtra)
% ix1 = nXextra + 1;
% iz1 = nZextra + 1;
% nxf = nx + nXextra*2;
% nzf = nz + nZextra*2;
ix1 = nXextra + 2;          %One extra column for the FD
iz1 = nZextra + 2;
% nxf = nx + nXextra*2+2;
% nzf = nz + nZextra*2+2;
nxf = nx + nXextra+2;
nzf = nz + 2;
nxplot = round(mvXmax/Dxz)+ix1; 
initzp = round(mvZtop/Dxz)+iz1; 
nzplot = round(mvZmax/Dxz)+iz1;
%Will be used as follows
    %Uz(ix1+iExtrao) = Uz(ix1+iExtrai);     %Symmetric edge
    %Ux(ix1+iExtrao) = Ux(ix1+iExtrai-1);   %Symmetric edge
    %Uz(nx+iExtra+1) = zFill;               %Rigid edge
    %Ux(nx+iExtra+1) = zFill;               %Rigid edge