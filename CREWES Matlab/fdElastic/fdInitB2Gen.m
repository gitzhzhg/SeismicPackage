function [wncmat,iZlvl,thrat].....
    = fdInitB2Gen(mvPlot,Dt,Dxz,wncvar)
%function [wncmat,thrat].....
%    = fdInitB2Gen(mvPlot,Dt,Dxz,wncvar)
% function [nxplot,initzp,nzplot,wncmat,thrat].....
%     = fdInitB2Gen(mvXmax,mvZtop,mvZmax,mvPlot,Dt,Dxz,wncvar)
%Initialize general conditions for an FD model
%
%The input parameters are
% %mvXmax  .... X-length of movie frames to display or plot
% %mvZtop  .... Z-level at top of each movie frame, often 0.
% %mvZmax  .... Z-depth of movie frames to display or plot
%mvPlot  .... The code number of the movie snapshot plot
%Dt      .... FD sample rate in seconds
%Dxz     .... FD sample rate in (metres)
%wncvar  .... The wavenumber correction file (within quotes), ('' is none) 
%
%The output parameters are
% %nxplot  .... No. of X points to plot
% %initzp  .... Initial Z point to plot
% %nzplot  .... No. of Z points to plot
%wncmat  .... A set of FD correction matrices, for particular Vp and Vs
%thrat   .... (Dt/Dxz)^2, used in the FD calculations
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

% nxplot = round(mvXmax/Dxz)+1; 
% initzp = round(mvZtop/Dxz)+1; 
% nzplot = round(mvZmax/Dxz)+1;
%disp([nxplot,initzp,nzplot])
%wnc = [];
%nShot = 1;      %For now
% wnc = 1;
% icorr = 0;
% load wncUnity
% wncU = wnc;
% %disp(wnc)
% load wncEdge
% %disp(wnc)
%supp = 0;
% if iTbnd > 0
%     supp = suppress;                %Set up supp variable here
% end
wncmat = 1; iZlvl = 1;
%small = 0.0000001;   %0.00001;
if ~isempty(wncvar)
    loadStr = ['load ' wncvar];
    disp(loadStr)
    eval(loadStr)       %Corrections in variable 'wncmat'
    disp(size(wncmat)); disp(size(iZlvl));
    
    %disp(wncmat)
end
% figure
% flipy
set(gca,'NextPlot','replacechildren')
%jfr = 0;
% %mint = round((nstep-mvTinit)/nframe);
% mint = round(nstep/nframes);
if mvPlot>=0
    set(gca,'xlimmode','manual')
end
thrat = (Dt/Dxz)^2;