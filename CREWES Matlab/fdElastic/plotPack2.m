function plotPack2(actiF,mvPlot,mvClip,mvAmp,Dxz,Ux,Uz,ix1,xMin,nxplot,...
    iz1,zMin,nzplot)
%plotPack(actiF,mvPlot,mvClip,mvAmp,Dxz,Ux,Uz,ix1,xMin,nxplot,iz1,zMin,nzplot)
%Input parameters
%actiF ... Active figure (object)
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


    figure(actiF)
    %figure
    %Set up axes plots
    xAx = xMin:Dxz:(nxplot-ix1)*Dxz+xMin;
    %zAx = (1-iz1)*Dxz:Dxz:(nzplot-iz1)*Dxz;
    zAx = zMin:Dxz:(nzplot-iz1)*Dxz+zMin;
    %disp(xAx); %disp(zAx);
    intplx = round(nxplot/40);  %Plot about 40 traces (mvPlot 0 or 1)
    if intplx<1; intplx = 1; end
    xAx2 = xMin:Dxz*intplx:(nxplot-ix1)*Dxz+xMin;
    zAx2 = zMin:Dxz:(nzplot-1)*Dxz+zMin;
    if mvPlot==0
    	%plotwiglx(Uz(ix1:intplx:nxplot,iz1:nzplot)',1:nzplot+1-iz1,1:intplx:nxplot+1-ix1)
    	%plotwiglx(Uz(ix1:intplx:nxplot,iz1:nzplot)',1:nzplot+1-iz1,xAx2)
    	%plotwiglx(Uz(ix1:intplx:nxplot,1:nzplot)',zAx,xAx2)
    	plotwiglx(Uz(ix1:intplx:nxplot,1:nzplot)',zAx2,xAx2)
        axis ([xAx2(1) xAx2(end) zAx(1) zAx(end)])
        title('Displacement in Z')
    end
    if mvPlot==1
    	%plotwiglx(Ux(ix1:intplx:nxplot,iz1:nzplot)',1:nzplot+1-iz1,1:intplx:nxplot+1-ix1)
    	plotwiglx(Ux(ix1:intplx:nxplot,1:nzplot)',zAx2,xAx2)
        axis ([xAx2(1) xAx2(end) zAx(1) zAx(end)])
        title('Displacement in X')
    end
    %disp(xAx2)
    %disp(zAx)
    axis ij
    %flipy
    if mvPlot==2
       plotcolourk(Ux(ix1:nxplot,1:nzplot),Uz(ix1:nxplot,1:nzplot).....
	      ,mvClip,xAx,zAx,Dxz,1)
        title('Colour coded displacement')
        axis equal
    end
    if mvPlot==3
        %disp([size(Ux),nxplot,nzplot])
        plotptwist2(Ux(ix1:nxplot,1:nzplot),Uz(ix1:nxplot,1:nzplot).....
	      ,mvClip,xAx,zAx,Dxz,1)
        title('Colour interpretation of pressure/twist')
        axis equal
    end
    if mvPlot==4
       %plotcoloura(Ux(ix1:nxplot,iz1:nzplot),Uz(ix1:nxplot,iz1:nzplot).....
       plotcoloura(Ux(ix1:nxplot,1:nzplot),Uz(ix1:nxplot,1:nzplot).....
	      ,mvClip,xAx,zAx,Dxz,1,0,mvAmp)
        title('Colour coded displacement, fixed amplitude')
        axis equal
    end
    axis ([xAx(1) xAx(end) zAx(1) zAx(end)])
    if mvPlot>4
%         intplx = round((nxplot-ix1)/60);
%         intplz = round((nzplot-iz1)/60);
        intplx = round(nxplot/60);
        intplz = round(nzplot/60);
        if intplx<1
          intplx=1;
        end
        if intplz<1
          intplz=1;
        end
        
        %disp([intplx,intplz])
        maxint = max([intplx intplz]);
        vqx = (ix1:maxint:nxplot+ix1-1);
        vqz = (iz1:maxint:nzplot+iz1-1);
        xMax = Dxz*maxint*nxplot+xMin;
        zMax = Dxz*maxint*nzplot+zMin;
        chAxis = [xMin xMax zMin zMax];
        disp(chAxis)
        axis(chAxis)
        %axis ([xMin xMax zMin zMax])
        %axis ij
        %disp(size(Ux));disp(vqx);disp(vqz)
        %disp(size(Ux(vqx,vqz)));disp([ix1,nxplot,iz1,nzplot])
        %pause
        quiver((vqx-ix1)*Dxz+xMin,(vqz-iz1)*Dxz+zMin,Ux(vqx,vqz)',Uz(vqx,vqz)','k')
        %pause
        title('Vector displacement')
        flipy
        axis equal
    end