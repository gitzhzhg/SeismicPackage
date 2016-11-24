function [Lp2m,Mu,Rho,LKrat,oldFile,zMin] =...
    fdInitModB7(modelDir,oldFile,Dxz,ix1,iz1,nxf,nzf,xMin,contBlk)
% function [Lp2m,Mu,Rho,LKrat,oldFile,zMin] =...
%     fdInitModB6(oldFile,Dxz,nx,nz,ix1,iz1,nxf,nzf,xMin,contBlk)
%Get the geological model from disc - fill in FD grid
%The input parameters are
%oldFile .... The geological definition file (within quotes) (gfdfile)
%Dxz     .... FD sample rate in (metres)
%nx
%nz
%ix1   ...... X co-ordinate of start of model in arrays
%iz1   ...... Z co-ordinate of start of model in arrays
%nxf     .... Number of X samples including border
%nzf     .... Number of Z samples including border
%xMin    .... Left edge of the FD model extracted from the geological model
%contBlk .... The indicator of a 'cont' (continuous) or 'block' (blocked)
%               geological model
%The output parameters are
%Lp2m    .... Lamda plus 2*Mu
%Mu      .... Mu
%Rho     .... Density
%LKrat   .... (Lp2m-2*Mu)./Lp2m
%oldFile .... The geological definition file (within quotes) (gfdfile)
%zMin    .... Top of geological model
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

if(~strncmp(contBlk,'log',3))       %If 'log' go to bottom of listing
    clear nTops
    UpDown = 'Down';
    disp(oldFile)
    [generic] = fdReadgeo2(modelDir,oldFile);
    load(generic,'-mat')
    Lp2m = zeros(nxf,nzf);
    Mu = zeros(nxf,nzf);
    Rho = zeros(nxf,nzf);
    Lp2mH = rho.*Vp.^2;     %Take dimensions of input vectors
    MuH = rho.*Vs.^2;

    nxu = nxf;
    nzu = nzf;

    %Fill whole area with top values
    Lp2m(1:nxf,1:nzf) = ones(nxf,nzf)*Lp2mH(1);
    Mu(1:nxf,1:nzf) = ones(nxf,nzf)*MuH(1);
    Rho(1:nxf,1:nzf) = ones(nxf,nzf)*rho(1);

    Wons = ones(1,nzu);
    %nTops = size(Xtops) - nBoxes;
    %disp('size(Vp)'); disp(size(Vp)); disp('size(Vp)'); 
    [nTops,xxx] = size(Vp); % - nBoxes;
    %disp('nTops'); disp(nTops); disp('nTops');
    nTops = nTops - nBoxes;
    %disp(size(Vp))
    %disp(Vp)
    zMin = Ztops(1,1);
    %whos
    Lp2mG = zeros(size(Vp));
    MuG = Lp2mG; RhoG = Lp2mG;
    %disp(size(Lp2m)); disp(size(Ztops)); 
    if strncmpi('cont',contBlk,4)
        for iTz = 1:nTops-1
            Zdist = Ztops(iTz+1,1)-Ztops(iTz,1);
            %disp(Zdist)
            Lp2mG(iTz) = (Vp(iTz+1).^2*rho(iTz+1)-Vp(iTz).^2*rho(iTz))/Zdist;
            MuG(iTz)   = (Vs(iTz+1).^2*rho(iTz+1)-Vs(iTz).^2*rho(iTz))/Zdist;
            RhoG(iTz) = (rho(iTz+1)-rho(iTz))/Zdist;
        end
    end
    %disp(Lp2mG)
    %for iT=2:nTops              %Go from the second row
    for iT=1:nTops              %Go from the first row (no elevation profile)
        xT = xMin;
        for ix=ix1:nxu
            ig = find(Xtops(iT,1:nTopMax)>xT);
            zTp = Ztops(iT,ig(1))+(xT-Xtops(iT,ig(1)))*(Ztops(iT,ig(1))-Ztops(iT,ig(1)-1)).....
                /(Xtops(iT,ig(1))-Xtops(iT,ig(1)-1));
            iTz = ceil(zTp/Dxz)+iz1;    %Index of first sample below top iT
                                        %New velocities from here
            ziTz = (iTz-iz1)*Dxz;
            %disp(iTz)
            %stop
                if iTz<nzf
                    %if iTz<1; iTz = 1; end
                    if ~strcmpi('cont',contBlk)         %Blocked
                        Lp2m(ix,iTz:nzu) = Lp2mH(iT)*Wons(iTz:nzu);
                        Mu(ix,iTz:nzu) = MuH(iT)*Wons(iTz:nzu);
                        Rho(ix,iTz:nzu) = rho(iT)*Wons(iTz:nzu);
                    else                                %Continuous
                        for iz = iTz:nzu
                            Lp2m(ix,iz) = Lp2mH(iT)+(ziTz-zTp)*Lp2mG(iT);
                            Mu(ix,iz) = MuH(iT)+(ziTz-zTp)*MuG(iT);
                            Rho(ix,iz) = rho(iT)+(ziTz-zTp)*RhoG(iT);
                            ziTz = ziTz+Dxz;
    %                         disp(Rho(1,iz))
    %                         pause
                        end
                        %stop
                    end
                end
    %         end
            xT = xT+Dxz;
        end
        %disp(iTz)
    end
    %Fill in parameters at top
    for iz = 1:iz1-1
        Lp2m(:,iz) = Lp2m(:,iz1+1);
        Mu(:,iz) = Mu(:,iz1+1);
        Rho(:,iz) = Rho(:,iz1+1);
    end
    if nBoxes > 0   %Interpret areas defined within boxes
%         disp(['Boxes ' num2str(nBoxes)])
%         %disp(nTops)
%         %disp(size(Xtops))
%         %iL = nTops + 1 - nBoxes;
%         for iBox = 1:nBoxes
%             disp(nTbox(iBox))
%             iL = nTops + iBox;
%             for iB = 1:nTbox(iBox)
%                 disp(iL); disp(iB)
%                 disp(Xtops(iL,iB))
%             end
%         end
        for iBox = 1:nBoxes
            iL = nTops + iBox;
            nTb = nTbox(iBox);
            %disp(Xtops(iL,1:nTb));
            boxXmin = min(Xtops(iL,1:nTb));
            boxXmax = max(Xtops(iL,1:nTb));
            boxZmin = min(Ztops(iL,1:nTb));
            boxZmax = max(Ztops(iL,1:nTb));
            minXpt = ix1 + floor((boxXmin - xMin)/Dxz);
            minZpt = iz1 + floor(boxZmin/Dxz);
            nXpts = round((boxXmax - boxXmin)/Dxz);
            nZpts = round((boxZmax - boxZmin)/Dxz);
            boxMat = zeros(nXpts,nZpts);
            for iC = 1:nTb - 1
                %disp(iC)
                %B = round((Ztops(iL,iC) - boxZmin)/Dxz) + 1;
                A = (Ztops(iL,iC+1) - Ztops(iL,iC))/(Xtops(iL,iC+1) - Xtops(iL,iC));
                %disp(A)
                dir = Xtops(iL,iC+1) - Xtops(iL,iC);
                %disp(dir)
                if dir ~= 0
                    %iX1 = round(Xtops(iL,iC)/Dxz);
                    %iX9 = round(Xtops(iL,iC+1)/Dxz);
                    %iX1 = round((Xtops(iL,iC)-boxXmin)/Dxz + 1);
                    %iX9 = round((Xtops(iL,iC+1)-boxXmin)/Dxz + 1);
                    %iZ1 = round((Ztops(iL,iC)-boxZmin)/Dxz + 1);
                    iX1 = round((Xtops(iL,iC)-boxXmin)/Dxz+1);
                    iX9 = round((Xtops(iL,iC+1)-boxXmin)/Dxz+1);
                    iZ1 = round((Ztops(iL,iC)-boxZmin)/Dxz+1);
                    %disp([iX1,iX9])
                    B = iZ1 - A * iX1;
                    %disp(B)
                    inc = 1;
                    if iX9 < iX1; inc = -1; end
                    for iX = iX1:inc:iX9
                        Z = A*iX + B;
                        iZ = round(Z);
                        if dir > 0
                            for iZr = iZ:nZpts
                                boxMat(iX,iZr) = 1;
                            end
                        else
                            for iZr = iZ:nZpts
                                boxMat(iX,iZr) = 0;
                            end
                        end
                    end
                end
            end
            MuH1 = rho(iL).*Vs(iL).^2;
            Lp2mH1 = rho(iL).*Vp(iL).^2;
            RhoH1 = rho(iL);
            %disp(RhoH1)
            %disp(boxMat)
            for iX = 1:nXpts
                iXu = iX + minXpt - 1;
                for iZ = 1:nZpts
                   iZu = iZ + minZpt - 1;
                   hold = boxMat(iX,iZ);
                   if hold > 0
                      Lp2m(iXu,iZu) = Lp2mH1; 
                      Mu(iXu,iZu) = MuH1; 
                      Rho(iXu,iZu) = RhoH1; 
                   end
                end
            end
        end
    end
    %stop
    %Fill in elastic constants for border areas - 
        %extending from defined areas
    %Top and bottom
    izb = nzf + 1 - iz1;
    for iz = 1:iz1-1
        izO = iz + izb;
        for ix = 1:nxf
            Lp2m(ix,iz) = Lp2m(ix,iz1);
            Mu(ix,iz)   = Mu(ix,iz1);
            Rho(ix,iz)  = Rho(ix,iz1);
            Lp2m(ix,izO) = Lp2m(ix,izb);
            Mu(ix,izO)   = Mu(ix,izb);
            Rho(ix,izO)  = Rho(ix,izb);
        end
    end
    %Left and right
    ixr = nxf + 1 - ix1;
    for ix = 1:ix1-1
        ixO = ixr + ix;
        for iz = 1:nzf
            Lp2m(ix,iz) = Lp2m(ix1,iz);
            Mu(ix,iz)   = Mu(ix1,iz);
            Rho(ix,iz)  = Rho(ix1,iz);
            Lp2m(ixO,iz) = Lp2m(ixr,iz);
            Mu(ixO,iz)   = Mu(ixr,iz);
            Rho(ixO,iz)  = Rho(ixr,iz);
        end
    end
%     %Plot the velocity profile
%         zVect = 1:nzf;
%         Xpos = 1200;        %Metres
%         iXpos = round(Xpos/Dxz)+ ix1;
%         %iXpos = round(nxf*0.5);
%         disp(Lp2m(iXpos,1:9))
%         figure
%         plot(zVect,Lp2m(iXpos,:),zVect,Mu(iXpos,:))
%         %disp('Lp2m'); disp(Lp2m(iXpos,nzf-9:nzf));
%         %disp('Mu'); disp(Mu(iXpos,nzf-9:nzf));
%         title(['Lp2m and Mu profiles at ',num2str(Xpos),' metres'])
%         %plot(zVect,Rho(iXpos,:))
%         %disp(Rho(1,1:9))
% %         stop
else                %This 'log' option models space from one log
    load(oldFile)
    nzLog = length(vpblk);
    %disp(vpblk(nzLog-9:nzLog))      %Likely highest velocities
    avP = sum(vpblk)/nzLog;
    avS = sum(vsblk)/nzLog;
    disp(['Average P = ',num2str(avP)])
    disp(['Average S = ',num2str(avS)])
    VpH = ones(1,nzf)*vpblk(nzLog);
    VsH = ones(1,nzf)*vsblk(nzLog);
    rhoH = ones(1,nzf)*rhoblk(nzLog);
    if nzLog>nzf
        nzLog = nzf;
    end
    rhoH(1:nzLog) = rhoblk(1:nzLog);
    VpH(1:nzLog) = vpblk(1:nzLog);
    VsH(1:nzLog) = vsblk(1:nzLog);
    Lp2mH(1:nzLog) = rhoblk(1:nzLog).*VpH.^2;
    MuH(1:nzLog) = rhoblk(1:nzLog).*VsH.^2;
    Wons = ones(nxf,1);
    Rho = Wons*rhoH;
    Lp2m = Wons*Lp2mH;
    Mu = Wons*MuH;
    zMin = zblk(1);
end
%figure
%plot(Lp2m(1,:))
LKrat = (Lp2m-2*Mu)./Lp2m;
%disp('fdInitModB7')
%stop