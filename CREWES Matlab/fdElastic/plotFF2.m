function plotFF2(mvPlot,mvClip,mvAmp,Dxz,Ux,Uz,ix1,xMin,nxplot,iz1,zMin,nzplot,....
    Dt,nstep,parmFile,ixp,izp)
%Dt,nstep,modelDir,parmFile,ixp,izp)
% function plotFF2(mvPlot,mvClip,mvAmp,Dxz,Ux,Uz,ix1,xMin,nxplot,iz1,zMin,nzplot,....
%     Dt,nstep,parmFile,ixp,izp)
%Key code 5 - Plot final snapshot *********************************************
%The input parameters are
%mvPlot .. Plot type
%mvClip .. Clipping level (of maximum value)
%mvAmp ... Absolute amplitude (for mvPlot = 4)
%Dxz ..... Spatial sample rate
%Ux ...... X displacement matrix
%Uz ...... Z displacement matrix
%ix1 ..... Initial X entry to plot
%xMin .... Initial X offset
%nxplot .. Number of X entries to plot
%iz1 ..... Initial Z entry to plot
%zMin .... Top of geological model
%nzplot .. Number of Z entries to plot
%Dt   .... FD sample rate in seconds
%nstep ... The number of time steps to the frame (for title)
%parmFile. Name of file containing finite-difference parameters,
%ending in .parm, used as identification
%ixp ..... X index of a vertical dicontinuity
%izp ..... Z index of a vertical dicontinuity - to plot boundaries
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
% disp(nstep)
% disp(ixp);disp(izp);
% stop

plotT(1,:) = 'Uz          ';
plotT(2,:) = 'Ux          ';
plotT(3,:) = 'Displacement';
plotT(4,:) = 'Press/twist ';
plotT(5,:) = 'Abs displace';
plotT(6,:) = 'Arrows      ';
str1T = 'Choose plot';

msec = nstep*Dt*1000;
frameTif = ''; %'C'; %'A'; %'' to skip;
  xMax = (nxplot+1-ix1)*Dxz+xMin;
  zMax = (nzplot+1-iz1)*Dxz;
  
  %str1 = 'Change parameter';
  str1 = 'Change snapshot';
  str2 = ['Plot ',plotT(mvPlot+1,:)];
  str3 = ['Clip = ',num2str(mvClip)];
  str4 = ['Xmin = ',num2str(xMin)];
  str5 = [' Xmax = ',num2str(xMax)];
  str6 = [' Zmax = ',num2str(zMax)];
  str7 = [' tiff tag = ',frameTif];
  str99 = 'All OK';
mvAmpt = mvAmp; ix1t = ix1; nxplott = nxplot; nzplott = nzplot; xMint = xMin;
xMaxt = xMax; zMaxt = zMax; mvPlott = mvPlot; mvClipt = mvClip;
kOptn = 0;
while kOptn < 7  
    kOptn = menu(str1,str2,str3,str4,str5,str6,str7,str99);
    if kOptn==1
        mvPlott = menu(str1T,plotT(1,:),plotT(2,:),plotT(3,:),.....
            plotT(4,:),plotT(5,:),plotT(6,:));
        mvPlott = mvPlott-1;
        str2 = ['Plot ',plotT(mvPlott+1,:)];
        if mvPlott==4
            mvAmpt = mvAmp;
            valu = input(['Amp is ',num2str(mvAmp),', type new ']);
            if (exist('valu','var')==1)
                if (valu>0)
                    mvAmpt = valu;
                end
            end
            %disp(mvAmpt)
        end
    end
    if kOptn==2
        mvClipt = input('Type clip level ');
        str3 = ['Clip = ',num2str(mvClipt)];
    end
    if kOptn==3
        xMint = input('Type value for Xmin ');
        xMint = max(xMin,xMint);
        ix1t = ix1+round((xMint-xMin)/Dxz);
        str4 = ['Xmin = ',num2str(xMint)];
    end
    if kOptn==4
        xMaxt = input('Type value for Xmax ');
        xMaxt = min(xMax,xMaxt);
        nxplott = round((xMaxt-xMin)/Dxz)+ix1-1;
        str5 = [' Xmax = ',num2str(xMaxt)];
    end
    if kOptn==5
        zMaxt = input('Type value for Zmax ');
        zMax = min(zMax,zMaxt);
        nzplott = round(zMax/Dxz)+iz1;
        str6 = [' Zmax = ',num2str(zMaxt)];
    end
    if kOptn==6
        frameTif = input('Type tiff tag (in single quotes) ');
        str7 = [' tiff tag = ',frameTif];
    end
end

clear figure
actiF = figure;
%h = figure;
%disp(xMint)
plotPack2(actiF,mvPlott,mvClipt,mvAmpt,Dxz,Ux,Uz,ix1t,xMint,nxplott,...
    iz1,zMin,nzplott)
hold on
%Plot all the line segments separating cells with vertical Vp contrasts
%[ixp,izp] = zbounds2(actiF,Lp2m,ix1,iz1,Dxz,xMin,zMin);
npl = length(ixp);
%disp(izp(1:10:npl)')
for ipl = 1:npl
    xpl = ixp(ipl)*Dxz+xMin;
    %zpl = (izp(ipl)-iz1)*Dxz+Dxz*0.5;
    zpl = izp(ipl)*Dxz+zMin;
    %zpl = izp(ipl)*Dxz+Dxz*0.5;
    line([xpl-Dxz*0.5,xpl+Dxz*0.5],[zpl,zpl])
%     if(xpl>=xMint)&&(xpl<=xMaxt)&&(zpl<=zMaxt)
%         plot([xpl-Dxz*0.5,xpl+Dxz*0.5],[zpl,zpl])
%     end
end
% npl = length(ix);
% %disp(iz(1:10:npl)')
% %disp(ix(1:10:npl)')
% for ipl = 1:npl
%     xpl = ix(ipl)*Dxz+xMin;
%     %zpl = (iz(ipl)-iz1)*Dxz+Dxz*0.5;
%     zpl = iz(ipl)*Dxz+zMin;
%     line([xpl-Dxz*0.5,xpl+Dxz*0.5],[zpl,zpl])
% end
%print -depsc -r150 flatm.eps
xlabel('X co-ordinate (m)')
ylabel('Depth (m)')
%boldlines
bigfont(gca,1.5,2,1)
whitefig
grid off
if ~isempty(frameTif)
  %fileName = [modelDir '\' parmFile '_' frameTif int2str(msec) 'F.tif'];
  fileName = [parmFile '_' frameTif int2str(msec) 'F.tif'];
  eval(['print -dtiff -r150 ' fileName])
  disp(fileName)
end