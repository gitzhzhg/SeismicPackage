function [ix,iz] = zbounds2(actiF,pvel,ix1,iz1,Dxz,xMin,zMin,plLim)
%function [ix,iz] = zbounds(actiF,pvel,ix1,iz1,Dxz,xMin)
%Find boundaries (steps) within a velocity model (z direction only) and plot
%
%The input parameters are
%   actiF ... Active figure (object)
%   pvel .... The displacement matrix of the 'stepped' component
%   ix1 ..... Initial X entry to plot
%   iz1 ..... Initial Z entry to plot
%   Dxz ..... Spatial sample rate
%   xMin .... Initial X offset
%   zMin .... Top of geological model
%   plLim ... The cutoff level of a 'step'
%The output parameters are
%   ix ...... X index of a vertical dicontinuity
%   iz ...... Z index of a vertical dicontinuity - to plot boundaries
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

% figure
% plot(pvel(10,:))
% figure
[nx,nz] = size(pvel);
pdiff = pvel(ix1:nx,iz1:nz)-pvel(ix1:nx,iz1-1:nz-1);
%disp(pdiff(100:109,100))
%disp(pvel(100,100:109))
%disp(pvel(1,:))
%disp(pdiff(1,:))
%[ix,iz] = find(abs(pdiff)>1000000);       %Discontinuity cutoff
clear ix iz
[ix,iz] = find(abs(pdiff)>plLim);       %Discontinuity cutoff
figure(actiF)
hold on
xlim = get(gca,'xlim');
ylim = get(gca,'ylim');
npl = length(ix);
%disp(iz(1:10:npl)')
%disp(ix(1:10:npl)')
for ipl = 1:npl
    xpl = ix(ipl)*Dxz+xMin;
    %zpl = (iz(ipl)-iz1)*Dxz+Dxz*0.5;
    zpl = iz(ipl)*Dxz+zMin;
    line([xpl-Dxz*0.5,xpl+Dxz*0.5],[zpl,zpl])
end
    %disp(iz(1:10:npl)')
set(gca,'xlim',xlim,'ylim',ylim)
%disp([xlim,ylim])
%get(actiF)
%     %pvel is the velocity model, zcontrast is the exact pvel contrast
% function [xv,zv] = zbounds(pvel,zcontrast)
% [nx,nz] = size(pvel);
% pdiff = pvel(:,2:nz)-pvel(:,1:nz-1);
% %disp(pvel(1,:))
% %[xv,zv] = find(abs(pdiff)>10);
% [xvh,zvh] = find(pdiff==zcontrast);
% [xv,ix] = sort(xvh);
% zv = zvh(ix);
%disp(xv)
%stop