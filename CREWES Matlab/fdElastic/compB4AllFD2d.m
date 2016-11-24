function [M,Ux,Uxt0,Uz,Uzt0,isfZ,iweX,zMin,...
        jfr,gfdFile,ix1,nxplot,iz1,nzplot,ixp,izp,mint,shotVp,shotVs] = .....
    compB4AllFD2d(Dt,Dxz,nx,nz,nstep,nframes,shotDepth,shotX,energySrc,xMin,wave,tw,....
        iLbnd,iRbnd,iTbnd,iBbnd,wncvar,mvTif,contBlk,modelDir,...
        iTrAcq,trX,trZ,isfZ,iweX,...
        mvZtop,mvXmax,mvZmax,mvPlot,mvAmp,mvClip,initStep,actiF,...
        M,Ux,Uxt0,Uz,Uzt0,jfr,gfdFile,parmFile,mint)
% function [M,Ux,Uxt0,Uz,Uzt0,isfZ,iweX,zMin,...
%         jfr,gfdFile,ix1,nxplot,iz1,nzplot,ixp,izp,mint,shotVp,shotVs] = .....
%     compB4AllFD2d(Dt,Dxz,nx,nz,nstep,nframes,shotDepth,shotX,energySrc,xMin,wave,tw,....
%         iLbnd,iRbnd,iTbnd,iBbnd,wncvar,mvTif,contBlk,mvTinit,...
%         iTrAcq,trX,trZ,isfZ,iweX,...
%         mvZtop,mvXmax,mvZmax,mvPlot,mvAmp,mvClip,initStep,actiF,...
%         M,Ux,Uxt0,Uz,Uzt0,jfr,gfdFile,parmFile,mint)
%All steps between movie frames (at 'mint' intervals) are computed
% 
%These are the input variables (with *'s also outputs)
%
%Dt      .... FD sample rate in seconds
%Dxz     .... FD sample rate in (metres)
%nx      .... Number of spatial samples in the X direction
%nz      .... Number of spatial samples in the Z direction
%nstep   .... Number of FD time steps
%nframes .... Number of movie frames to display or plot
%shotDepth  . Z (depth) of the FD initializing source
%shotX   .... X (from the FD model left) of the initializing source
%energySrc  . The directivity code of the energy source
%xMin    .... Left edge of the FD model extracted from the geological model
%wave    .... The wavelet amplitudes in sequence
%tw      .... The wavelet time series in sequence
%iLbnd   .... Boundary code left
%iRbnd   .... Boundary code right
%iTbnd   .... Boundary code top
%iBbnd   .... Boundary code bottom
%wncvar  .... The wavenumber correction file (within quotes), ('' is none) 
%mvTif   .... The root name of the movie tiff file (within quotes), ('' is none)
%contBlk .... The indicator of a 'cont' (continuous) or 'block' (blocked)
%mvTinit .... Number of first movie frame to be recorded, usually 1.
%iTrAcq  .... Not used (indicator of structured surface)
%trX     .... X-position of the 'well' where trace (time) data will be collected
%trZ     .... Z-depth of the 'line' where trace (time) data will be collected
%isfZ    ...* From trZ (if no structured surface)
%iweX    ...* From trX (if no structured surface)
%mvZtop  .... Z-level at top of each movie frame, often 0.
%mvXmax  .... X-length of movie frames to display or plot
%mvZmax  .... Z-depth of movie frames to display or plot
%mvPlot  .... The code number of the movie snapshot plot
%   0-Uz (displacement) in wiggle trace.
%   1-Ux (displacement) in wiggle trace.
%       2-colour, with the shade showing displacement direction.
%       mvClip reduces high amplitudes as fraction of maximum.
%   3-p/t colour, with the shade distinguishing P and S energy:
%       red/green shades showing compressional energy,
%       blue/yellow shades showing twisted energy.
%       mvClip reduces high amplitudes as fraction of maximum.
%   4-absolute amplitude controled version of mvPlot = 2.
%       mvAmp translates displacement to colour intensity (100 to 300?)
%       This plot is useful for movies because the scaling is consistent
%       between frames.
%   9-Matlab quiver plot. Displacements shown as arrows.
%Default quiver scaling used.
%mvAmp   .... The amplitude for mvPlot=4 (larger is higher amplitude)
%mvClip  .... The movie amplitude clip level (1 is unclipped)
%initStep ... When starting = 1, will be set suitably when continuing
%actiF   .... Figure controller for plots
%M    ......* Matlab movie file, may be played 5 times with 'movie(M,5)'
%Ux   ......* X displacements in an X/Y grid
%Uxt0   ....* X displacements in an X/Y grid, previous time step
%Uz   ......* Z displacements in an X/Y grid
%Uzt0   ....* Z displacements in an X/Y grid, previous time step
%jfr    ....* The index of the next frame (for movies)
%gfdFile ...* The geological definition file (within quotes)
%parmFile ... Name of file containing finite-difference parameters, ending in .parm
%mint    ...* The interval at which a movie frame is output or saved
%
%The variables above with *'s define the state of the modelling computations
%They are also outputs, which can then be input to continue model calculations
%
%The following output variables are used for plotting
%zMin    .... Top of geological model
%ix1   ...... X index of start of model in arrays
%nxplot   ... No. of X values to the end of model in arrays
%iz1   ...... Z index of start of model in arrays
%nzplot   ... No. of Z values to the bottom of model in arrays
%ixp    ..... X index of a vertical dicontinuity
%izp    ..... Z index of a vertical dicontinuity - to plot boundaries
%shotVp  .... The P wave velocity near the source point
%shotVs  .... The S wave velocity near the source point
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


%Load bottom correction file if it is not empty (= '')
%clear wnc
%Initialize general conditions ******************************************
        [wncMat,iZlvl,thrat] =.....
    fdInitB2Gen(mvPlot,Dt,Dxz,wncvar);
%    fdInitB2Gen(mvXmax,mvZtop,mvZmax,mvPlot,Dt,Dxz,wncvar);
%Get the geological model from disc *************************************
%         [pvel,svel,density,gfdFile,iTopOrdD,iTopOrdC,iTopOrdR,ix1,iz1,...
%                 zeroXx,zeroXz,zeroZx,zeroZz,XcolTop,ZcolTop] = ...
%     fdInitModSt(gfdFile,Dxz,nx,nz,xMin);
%         [ix1,iz1,nxf,nzf] = ....
%     fdInitBsizes(nx,nz,wnc);
%         [ix1,iz1,nxf,nzf,nxplot,initzp,nzplot] =.....
%     fdInitB2sizes(Dxz,nx,nz,mvXmax,mvZtop,mvZmax,wncMat,iLbnd,shotX);
        [ix1,iz1,nxf,nzf,nxplot,initzp,nzplot] =.....
    fdInitB3sizes(Dxz,nx,nz,mvXmax,mvZtop,mvZmax,wncMat); %,iLbnd,shotX);
%         [Lp2m,Mu,Rho,LKrat,gfdFile,zMin] =...
%     fdInitModB6(gfdFile,Dxz,nx,nz,ix1,iz1,nxf,nzf,xMin,contBlk);
        [Lp2m,Mu,Rho,LKrat,gfdFile,zMin] =...
    fdInitModB7(modelDir,gfdFile,Dxz,ix1,iz1,nxf,nzf,xMin,contBlk);
%disp(Lp2m(1:7,1:6))
%disp(LKrat(1:7,1:6))
%stop
%disp(gfdFile)
% figure
% plot(Mu(300,:))
% stop
%Find the velocities at the source point
    ix = round(shotX/Dxz)+ix1;
    iz = round(shotDepth/Dxz)+iz1;
    shotVp = sqrt(Lp2m(ix,iz)/Rho(ix,iz));
    shotVs = sqrt(Mu(ix,iz)/Rho(ix,iz));
%disp('here')
    if ~isempty(mvTif)
          gfdF = parmFile;
          n = findstr('.',gfdF);
          gfdF(n) = '_';
          fileName = [gfdF '_' mvTif];
          disp(['Output movie file "' fileName '#M.png"'])
    end
        %disp(XcolTop)
%Pad around borders and reset indicies **********************************
%         [nxr,ix1m,iz1m,pvel,svel,density,nx,nxf,nxplot,nz,nzf,nzplot,.....
%         Rho,Lp2m,Mu,LKrat,LKratF,vdtdx,vsdtdx] = .....
%     fdInitPadSt(pvel,svel,density,nx,nxplot,nz,nzplot,Dt,Dxz,iRbnd,ix1,iz1);
%Do first time step initialization
% disp('here')
% stop
if initStep==1
        %Set up zero arrays for recording (surface) traces ***************
            [jfr,iweX,isfZ] =.....
        fdInit5Trace(iTrAcq,trX,trZ,Dxz,ix1,iz1);
        mint = round(nstep/nframes);
        %Iinitilize fd time step state arrays to zero %*******************
            [Ux,Uxt0,Uz,Uzt0] =.....
        fdInitArrayB(nxf,nzf);
%         UxR = Ux(:,2);Uxt0R = Uxt0(:,2);UzR = Uz(:,2);Uzt0R = Uzt0(:,2);
%         UxF = Ux(:,2);Uxt0F = Uxt0(:,2);UzF = Uz(:,2);Uzt0F = Uzt0(:,2);
end
% %Add space for 'mirror' boundary on left ********************************
%         %[ixm,ixm2,izm,izm2,izb,ivxi,ivxo,nExtra,iExtrai,iExtrao,iExtra,zFill] =.....
%         [ivxi,ivxo] = .....
%     fdInitBound(nz,iBbnd,nxf,ix1,wncMat);
%Set up array where initializing forces will be applied *****************
        [nfmax,force,ampfx,ampfz,ixxA,ixzA,izxA,izzA] =....
    fdInit4Force5(shotDepth,shotX,energySrc,Dxz,ix1,iz1,wave,mint,modelDir);
%disp(force)

%ipv = 1:26;
% %Prepare to draw shear velocity ring
%     waveLen = length(wave)*Dt;
%     ang = (0:.01:pi);
%     ix = round(shotX/Dxz)+ix1;
%     iz = round(shotDepth/Dxz)+iz1;
%     srcSvel = svel(ix,iz);
%     shiftSp = waveLen*srcSvel*0.5;
%     ntStart = waveLen*0.5/Dt+20;
    
%Start looping through frames
% nfr = ceil((nstep+1-initStep)/mint);
% lastFr = initFr+nfr-1;
% initSt = initStep;
nSt = mint;
iFrce = 99;
frce = 0;
iFrame = 99;
%initFr = floor((initStep-1)/mint)+1;    % -1 ?
initFr = ceil(initStep/mint);
lastFr = ceil(nstep/mint);
% initSt = (initFr-1)*mint+1;
% if initSt<initStep
%     nSt = initStep-initSt;
% end
initSt = initStep;
%lastSt = initSt+mint-1;
nResid = rem(initStep-1,mint);
if nResid ~= 0
    nSt = nResid;
end
figure
for ifr = initFr:lastFr
    %disp(initSt)
    %lastSt = initSt+mint-1;
    lastSt = initSt+nSt-1;
    if lastSt > nstep
        lastSt = nstep;
        nSt = nstep+1-initSt;
        iFrame = 0;
    end
    %Check whether energy still being applied
    if initSt <= nfmax
        frce = force(initSt:lastSt);
    else
        iFrce = 0;
    end
%     if initSt>400
%         iFrce = 0;
%         %iFrce = -99;       %Directs to D plots
%     end
    %Do computation between frames
%         [Ux,Uz,Uxt0,Uzt0,srfUx,srfUz,wllUx,wllUz] = ...
%     fdCompB2Gilga(Ux,Uz,Uxt0,Uzt0,iLbnd,iRbnd,iTbnd,iBbnd,...
%         nSt,iFrce,frce,ampfx,ampfz,ixxA,ixzA,izxA,izzA,isfZ,iweX,...
%         Lp2m,Mu,LKrat,Rho,thrat,ix1,iz1,supp,wncMat);
%         supp = 0;
%         [Ux,Uz,Uxt0,Uzt0,srfUx,srfUz,wllUx,wllUz] = ...
%     fdCompB6Gilga(Ux,Uz,Uxt0,Uzt0,iLbnd,iRbnd,iTbnd,iBbnd,...
%         nSt,iFrce,frce,ampfx,ampfz,ixxA,ixzA,izxA,izzA,isfZ,iweX,...
%         Lp2m,Mu,LKrat,Rho,thrat,ix1,iz1,supp,wncMat);
        
        [Ux,Uz,Uxt0,Uzt0,srfUx,srfUz,wllUx,wllUz] = ...
    fdCompB8Gilga(Ux,Uz,Uxt0,Uzt0,iLbnd,iRbnd,iTbnd,iBbnd,...
        nSt,iFrce,frce,ampfx,ampfz,ixxA,ixzA,izxA,izzA,isfZ,iweX,...
        Lp2m,Mu,LKrat,Rho,thrat,ix1,iz1,wncMat,iZlvl);

%         [Ux,Uz,Uxt0,Uzt0,srfUx,srfUz,wllUx,wllUz,...
%         UxR,UzR,Uxt0R,Uzt0R,UxF,UzF,Uxt0F,Uzt0F] = ...
%     fdCompB4Gilga(Ux,Uz,Uxt0,Uzt0,iLbnd,iRbnd,iTbnd,iBbnd,...
%         UxR,UzR,Uxt0R,Uzt0R,UxF,UzF,Uxt0F,Uzt0F,...
%         nSt,iFrce,frce,ampfx,ampfz,ixxA,ixzA,izxA,izzA,isfZ,iweX,...
%         Lp2m,Mu,LKrat,Rho,thrat,ix1,iz1,supp,wncMat);
    
%     if lastSt>8
%         FSdebug(Uxt0(2:6,1:3),Uzt0(3:7,1:3))
%         pause
%     end
    %Assign unique id's to the displacements
    alphNum = num2str(ifr);
    alphSfUx = ['srfUx' alphNum];
    alphSfUz = ['srfUz' alphNum];
    alphWeUx = ['wllUx' alphNum];
    alphWeUz = ['wllUz' alphNum];
    eval([alphSfUx '=srfUx;'])
    eval([alphSfUz '=srfUz;'])
    eval([alphWeUx '=wllUx;'])
    eval([alphWeUz '=wllUz;'])
    if ifr==initFr
        [mUseless,iWavePk] = max(wave);
        shift = (iWavePk+1)*Dt*1000;
        fact = 1.5;
        %disp('here');disp(parmFile)
        save([modelDir,'\headrFile'],'parmFile','Dt','Dxz','xMin','zMin','trX','trZ',....
            'shotDepth','shotX','ix1','iz1','nxf','nzf','nstep',....
            'shotVp','shotVs','shift','fact')
    end
    if ifr==1
%         save('surfFileX',alphSfUx)
%         save('surfFileZ',alphSfUz)
%         save('wellFileX',alphWeUx)
%         save('wellFileZ',alphWeUz)
        save([modelDir,'\surfFileX'],alphSfUx)
        save([modelDir,'\surfFileZ'],alphSfUz)
        save([modelDir,'\wellFileX'],alphWeUx)
        save([modelDir,'\wellFileZ'],alphWeUz)
    else
%         save('surfFileX',alphSfUx,'-append')
%         save('surfFileZ',alphSfUz,'-append')
%         save('wellFileX',alphWeUx,'-append')
%         save('wellFileZ',alphWeUz,'-append')
        save([modelDir,'\surfFileX'],alphSfUx,'-append')
        save([modelDir,'\surfFileZ'],alphSfUz,'-append')
        save([modelDir,'\wellFileX'],alphWeUx,'-append')
        save([modelDir,'\wellFileZ'],alphWeUz,'-append')
    end
%     surfUx(:,initSt:lastSt) = srfUx;
%     surfUz(:,initSt:lastSt) = srfUz;
%     wellUx(:,initSt:lastSt) = wllUx;
%     wellUz(:,initSt:lastSt) = wllUz;
    initSt = lastSt+1;
    hold off
%     iFrame = 0;
%     disp(lastSt)
%     [vMxinC,iMxinC] = max(abs(Ux));
%     [vMxx,iMxinR] = max(abs(vMxinC));
%     disp([iMxinR*Dxz iMxinC(iMxinR)*Dxz]);
    if iFrame>0
        plotPack2(actiF,mvPlot,mvClip,mvAmp,Dxz,Ux,Uz,ix1,xMin,nxplot,....
            iz1,zMin,nzplot)
        if ifr <= initFr
          if (mvPlot>1)&&(mvPlot<5)
            %set(gca,'xlim',[xMin (nxplot-ix1m)*Dxz+xMin],'ylim',[initzp*Dxz (nzplot-iz1m)*Dxz]);
            set(gca,'xlim',[xMin (nxplot-ix1)*Dxz+xMin],'ylim',....
                [zMin (nzplot-iz1)*Dxz+zMin]);
                %[initzp*Dxz (nzplot-1)*Dxz]);
          elseif mvPlot==0
            %set(gca,'xlim',[0 nxplot-ix1m],'ylim',[initzp nzplot-iz1m]);
            set(gca,'xlim',[0 nxplot-ix1],'ylim',[initzp nzplot-1]);
          end
        end
      %end
        %Draw shear velocity ring
%             if lastSt > ntStart
%                 hold on
%                 radus = lastSt*Dt*srcSvel-shiftSp;
%                 xsin = shotX+sin(ang).*radus;
%                 zcos = shotDepth-cos(ang).*radus;
%                 plot(xsin,zcos,'k')
%             end
        title(['Time =' int2str(lastSt)])
        ylabel('Depth')
        xlabel('Offset')
        if mvPlot>=0
            axis equal
            %axis off
        end
        %M(:,jfr) = getframe;
        M(:,ifr) = getframe;
        if ~isempty(mvTif)
          eval(['print -dpng -r150 ' fileName int2str(ifr) 'M.png'])
          %eval(['print -dtiff -r150 ' mvTif Ajfr(jfr) 'M.tif'])
        end
    end
    nSt = mint;
end
%disp(size(Ux2)); disp(size(Ux)); 
%disp(max(max(surfUz)))
%disp(xv(1:5))
hold on
%disp(pvel(1,:))
plLim = 1000000;
ixp = 0; izp = 0;
if(~strncmpi('log',contBlk,3))
    %plLim = 10000000000000000;
    [ixp,izp] = zbounds2(actiF,Lp2m,ix1,iz1,Dxz,xMin,zMin,plLim);
end
%disp(ixp')
% figure
% %surface(pvel(1:2:200,1:2:150),jet)
% %contourf(pvel(1:2:200,1:2:150),10)
% mesh(pvel(1:4:200,1:4:150))
% view(-80,30)
% colormap jet