function [Ux,Uz,Uxt0,Uzt0,surfUx,surfUz,wellUx,wellUz] = ...
    fdCompB7Gilga(Ux,Uz,Uxt0,Uzt0,iLbnd,iRbnd,iTbnd,iBbnd,...
    nSt,iFrce,force,ampfx,ampfz,ixxA,ixzA,izxA,izzA,isfZ,iweX,...
    Lp2m,Mu,LKrat,Rho,thrat,ix1,iz1,wncMat)
% function [Ux,Uz,Uxt0,Uzt0,surfUx,surfUz,wellUx,wellUz] = ...
%     fdCompB5Gilga(Ux,Uz,Uxt0,Uzt0,iLbnd,iRbnd,iTbnd,iBbnd,...
%     nSt,iFrce,force,ampfx,ampfz,ixxA,ixzA,izxA,izzA,isfZ,iweX,...
%     Lp2m,Mu,LKrat,Rho,thrat,ix1,iz1,wncMat)
%Finite difference execution code
%The input parameters are
%Ux   ......* X displacements in an X/Y grid
%Uz   ......* Z displacements in an X/Y grid
%Uxt0   ....* X displacements in an X/Y grid, previous time step
%Uzt0   ....* Z displacements in an X/Y grid, previous time step
%iLbnd   .... Boundary code left
%iRbnd   .... Boundary code right
%iTbnd   .... Boundary code top
%iBbnd   .... Boundary code bottom
%nSt     .... No. of time steps
%iFrce   .... Force switch, 0 = no
%force   .... The wavelet amplitudes in sequence
%ampfx   .... The relative indices of source forces in the Ux direction
%ampfz   .... The relative indices of source forces in the Uz direction
%ixxA    .... Source position: x index for the Ux component
%ixzA    .... Source position: z index for the Ux component
%izxA    .... Source position: x index for the Uz component
%izzA    .... Source position: z index for the Uz component
%isfZ    .... Z-depth of the 'line' where trace (time) data will be collected
%iweX    .... X-position of the 'well' where trace (time) data will be collected
%thrat   .... (Dt/Dxz)^2, used in the FD calculations
%Lp2m    .... Lamda plus 2*Mu
%Mu      .... Mu
%LKrat   .... (Lp2m-2*Mu)./Lp2m
%Rho     .... Density
%ix1   ...... X co-ordinate of start of model in arrays
%iz1   ...... Z co-ordinate of start of model in arrays
%supp    .... = 0 for now
%wncMat  .... A set of FD correction matrices, for particular Vp and Vs
%
%The variables above with *'s define the state of the modelling computations
%They are also outputs, which can then be input to continue model calculations
%
%The other output parameters are
%surfUx  .... The Ux displacements collected in time on a horizontal line
%surfUz  .... The Uz displacements collected in time on a horizontal line
%wellUx  .... The Ux displacements collected in time on a vertical line
%wellUz  .... The Uz displacements collected in time on a vertical line
%
%nzp = number of Z points in the partition
% (At present - all Z points are calculated together)
% ( - affects mainly top and bottom boundary conditions)
%iz1 to iz9 cover the output Z points which are computed within this code
%Input displacements must cover a Z range from iz1-1 to iz9+1
%                   analogous relationships apply in X
% Edge values must be replaced before the next step
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
[nxf,nzf] = size(Ux);
%disp([ix1,iz1,nxf,nzf])
%stop
%Use for limits of the decon 
% ix9 = nxf+1-ix1;
% iz9 = nzf+1-iz1;
ix9 = nxf-1;
iz9 = nzf-1;
[nxC,nzC,nTerms] = size(wncMat);
ixm2 = 2:ix9;
if iLbnd == 7
    ixm2 = 1:ix9;
end
thrRt = sqrt(thrat);
nxl = nxf-1;
if iLbnd == 1   %Clayton Engquist boundary conditions preparation
    %disp(Lp2m(1:9,300))
    %disp(Mu(1:9,300));disp(Rho(1:9,300))
    %stop
    AlphaL = sqrt(Lp2m(ix1,:)./Rho(ix1,:));
    BetaL = sqrt(Mu(ix1,:)./Rho(ix1,:));
    LBUz = BetaL./(BetaL+1/thrRt);
    LBUzt0 = 1./(thrRt*BetaL+1);
    LAUx = AlphaL./(AlphaL+1/thrRt);
    LAUxt0 = 1./(thrRt*AlphaL+1);
end
if iRbnd == 1   %Clayton Engquist boundary conditions preparation
    AlphaR = sqrt(Lp2m(nxl,:)./Rho(nxl,:));
    BetaR = sqrt(Mu(nxl,:)./Rho(nxl,:));
    %AlphaR = BetaR;                        %Note error!
    RBUz = BetaR./(BetaR+1/thrRt);
    RBUzt0 = 1./(thrRt*BetaR+1);
    RAUx = AlphaR./(AlphaR+1/thrRt);
    RAUxt0 = 1./(thrRt*AlphaR+1);

end
if iTbnd == 1
    AlphaT = sqrt(Lp2m(:,1)./Rho(:,1));
    BetaT = sqrt(Mu(:,1)./Rho(:,1));
    TBUx = BetaT./(BetaT+1/thrRt);
    TBUxt0 = 1./(thrRt*BetaT+1);
    TAUz = AlphaT./(AlphaT+1/thrRt);
    TAUzt0 = 1./(thrRt*AlphaT+1);
end
nzl = nzf-1;
ixf = nxf+2-ix1;    %Index of nearest cross border point
izf = nzf+2-iz1;    %Index of nearest cross border point

if iBbnd == 1   %Clayton Engquist boundary conditions preparation
    AlphaB = sqrt(Lp2m(:,nzl)./Rho(:,nzl));
    BetaB = sqrt(Mu(:,nzl)./Rho(:,nzl));
    BBUx = BetaB./(BetaB+1/thrRt);
    BBUxt0 = 1./(thrRt*BetaB+1);
    BAUz = AlphaB./(AlphaB+1/thrRt);
    BAUzt0 = 1./(thrRt*AlphaB+1);
        %disp(AlphaB(1:6)); disp(BetaB(1:6)); 
end
%stop
izm2 = 2:iz9;
nzp = nzf;
surfUx = zeros(nxf,nSt);
surfUz = zeros(nxf,nSt);
wellUx = zeros(nzf,nSt);
wellUz = zeros(nzf,nSt);
DUxDx = zeros(nxf,nzp);
DUxDz = zeros(nxf,nzp);
DUzDx = zeros(nxf,nzp);
DUzDz = zeros(nxf,nzp);
D2UxDx = zeros(nxf,nzp);
D2UxDz = zeros(nxf,nzp);
D2UxDxzP = zeros(nxf,nzp);
D2UxDxzS = zeros(nxf,nzp);
D2UzDx = zeros(nxf,nzp);
D2UzDz = zeros(nxf,nzp);
D2UzDxzP = zeros(nxf,nzp);
D2UzDxzS = zeros(nxf,nzp);
h1 = zeros(nxf,nzp);
h2 = zeros(nxf,nzp);
h3 = zeros(nxf,nzp);
h4 = zeros(nxf,nzp);

%For mirror left boundary
iLin = ix1:ix1*2-2;
iLout = ix1-1:-1:1;

%For right boundary
%iRout = nxf+1-ix1:nxf;
%For top boundary
%iTout = iz1-1:-1:1;
%For bottom boundary
%iBout = nzf+1-iz1:nzf;
%iBout = nzf;
%disp(ampfx);disp(ampfz)
% figure;disp(force)
% stop
% a0 = zeros(nxf,1);
% fr = 0.94; fx = 1-fr;      %fr=1 - free,  fx - fixed Ux, Uz
for its = 1:nSt
    %Add source over time component
    if iFrce > 0
        if force(its) ~= 0
            Ux(ixxA,ixzA) = force(its).*ampfx + Ux(ixxA,ixzA);
            Uz(izxA,izzA) = force(its).*ampfz + Uz(izxA,izzA);
        end
    end
    izC = 1;    
    
    %Boundary condition for finite-difference calculation
        %Top boundary condition
            %FSdebug(Ux(3:5,1:2),Uz(3:5,1:2))
        if iTbnd <= 0
            %Ux(:,1) = 0;                    % Rigid
            %Uz(:,1) = -Uz(:,2);             % Zaiming condition
            Ux(:,iz1-1) = 0;                    % Rigid
            Uz(:,iz1-1) = -Uz(:,iz1);           % Zaiming condition
            if iz1 == 3
                Ux(:,iz1-2) = -Ux(:,iz1);
                Uz(:,iz1-2) = -Uz(:,iz1+1);
            end
        end
        if iTbnd == 1
            %Ux(:,1) = TBUx.*Ux(:,2)+TBUxt0.*Uxt0(:,1);
            %Uz(:,1) = TAUz.*Uz(:,2)+TAUzt0.*Uzt0(:,1);
            Ux(:,iz1-1) = TBUx.*Ux(:,iz1)+TBUxt0.*Uxt0(:,iz1-1);
            Uz(:,iz1-1) = TAUz.*Uz(:,iz1)+TAUzt0.*Uzt0(:,iz1-1);
            if iz1 == 3
                Ux(:,iz1-2) = TBUx.*Ux(:,iz1-1)+TBUxt0.*Uxt0(:,iz1-2);
                Uz(:,iz1-2) = TAUz.*Uz(:,iz1-1)+TAUzt0.*Uzt0(:,iz1-2);
            end
        end
        if iTbnd == 8
            %Free surface at top of model
            ixF = 1:nxf-1;
%                FSdebug(Ux(2:4,1:3),Uz(3:5,1:3))
            Ux(ixF,iz1-1) = Ux(ixF,iz1)+Uz(ixF+1,iz1)-Uz(ixF,iz1);
            Uz(ixF+1,iz1-1) = Uz(ixF+1,iz1)+LKrat(ixF,iz1-1).*....
                (Ux(ixF+1,iz1-1)-Ux(ixF,iz1-1));
            Uz(1,iz1-1) = Uz(3,iz1-1);
            if iz1==3
                Ux(ixF,iz1-2) = Ux(ixF,iz1-1)+Uz(ixF+1,iz1-1)-Uz(ixF,iz1-1);
                Uz(ixF+1,iz1-2) = Uz(ixF+1,iz1-1)+LKrat(ixF,iz1-2).*....
                    (Ux(ixF+1,iz1-2)-Ux(ixF,iz1-2));
                Uz(1,iz1-2) = Uz(3,iz1-2);
            end
        end
            %FSdebug(Ux(3:5,1:2),Uz(3:5,1:2))
            %pause


        %Bottom boundary conditions
        if iBbnd<=0                         % Rigid
            %Uz(:,nzf) = 0;
            %Ux(:,nzf) = -Ux(:,nzl);         % Zaiming condition
            Ux(:,izf) = 0;                    % Rigid
            Uz(:,izf) = -Uz(:,izf-1);           % Zaiming condition
            if iz1 == 3
                Ux(:,izf+1) = -Ux(:,izf-1);
                Uz(:,izf+1) = -Uz(:,izf-2);
            end

        end
        if iBbnd == 1   %%Clayton Engquist boundary conditions
            %Ux(:,nzf) = BBUx.*Ux(:,nzl)+BBUxt0.*Uxt0(:,nzf);
            %Uz(:,nzf) = BAUz.*Uz(:,nzl)+BAUzt0.*Uzt0(:,nzf);
            %Try tapering amplitudes near the bottom boundary
%             fact = 1.0; dfact = 0.002;
%             for iz = nzf-(10+iz1):nzf+1-iz1
%                 fact = fact - dfact;
%                 Ux(:,iz) = Ux(:,iz)*fact;
%                 Uxt0(:,iz) = Uxt0(:,iz)*fact;
%                 Uz(:,iz) = Uz(:,iz)*fact;
%                 Uzt0(:,iz) = Uzt0(:,iz)*fact;
%             end
            %Continue with Clayton Engquist
            Ux(:,izf) = BBUx.*Ux(:,izf-1)+BBUxt0.*Uxt0(:,izf);
            Uz(:,izf) = BAUz.*Uz(:,izf-1)+BAUzt0.*Uzt0(:,izf);
            if iz1 == 3
                Ux(:,izf+1) = BBUx.*Ux(:,izf)+BBUxt0.*Uxt0(:,izf+1);
                Uz(:,izf+1) = BAUz.*Uz(:,izf)+BAUzt0.*Uzt0(:,izf+1);
            end
        end
        if iBbnd == 8                       % Free surface
            Ux(iLout,nzl) = -Ux(iLin,nzl);       % Here assume left mirror edge
            Uz(iLout,nzl) = Uz(iLin+1,nzl);
            ixF = 2:nxf;     %1:nxf-1;
                Uz(ixF,nzf) = Uz(ixF,nzl)+LKrat(ixF,nzl).*(Ux(ixF-1,nzl)-Ux(ixF,nzl));
                Uz(iLout,nzf) = Uz(iLin+1,nzf);       % Here assume left mirror edge
                Ux(ixF-1,nzf) = Ux(ixF-1,nzl)+Uz(ixF-1,nzf)-Uz(ixF,nzf);
        end
        
        %Left boundary conditions
        if iLbnd == 0
            %Ux(iLout,:) = 0;                % Rigid edge
            %Uz(iLout,:) = 0;
            %Uz(ix1-1,:) = -Uz(ix1,:);
            Ux(ix1-1,:) = 0;                    % Rigid
            Uz(ix1-1,:) = -Uz(ix1,:);           % Zaiming condition
            if ix1 == 3
                Ux(ix1-2,:) = -Ux(ix1,:);
                Uz(ix1-2,:) = -Uz(ix1+1,:);
            end

        end
        if iLbnd == 1   %%Clayton Engquist boundary conditions
            %Ux(1,:) = 0;
            %ix2 = ix1+1;
            %Ux(ix1,:) = LAUx.*Ux(ix2,:)+LAUxt0.*Uxt0(ix1,:);
            %Uz(ix1,:) = LBUz.*Uz(ix2,:)+LBUzt0.*Uzt0(ix1,:);
            Ux(ix1-1,:) = LAUx.*Ux(ix1,:)+LAUxt0.*Uxt0(ix1-1,:);
            Uz(ix1-1,:) = LBUz.*Uz(ix1,:)+LBUzt0.*Uzt0(ix1-1,:);
            if ix1 == 3
                Ux(ix1-2,:) = LAUx.*Ux(ix1-1,:)+LAUxt0.*Uxt0(ix1-2,:);
                Uz(ix1-2,:) = LBUz.*Uz(ix1-1,:)+LBUzt0.*Uzt0(ix1-2,:);
            end
        end
        if iLbnd == 7
            Ux(iLout,:) = -Ux(iLin,:);      % Mirror edge
            Uz(iLout,:) = Uz(iLin+1,:);
        end

        %Right boundary condition
          nxl = nxf-1;
        if iRbnd <= 0
            %Uz(nxf,:) = 0;                  % Rigid right edge
            %Ux(nxf,:) = -Ux(nxl,:);
            Uz(ixf,:) = 0;                    % Rigid
            Ux(ixf,:) = -Ux(ixf-1,:);           % Zaiming condition
            if ix1 == 3
                Uz(ixf+1,:) = -Uz(ix1-1,:);
                Ux(ixf+1,:) = -Ux(ix1-2,:);
            end
        end
        if iRbnd == 1   %%Clayton Engquist boundary conditions
            %Ux(nxf,:) = RAUx.*Ux(nxl,:)+RAUxt0.*Uxt0(nxf,:);
            %Uz(nxf,:) = RBUz.*Uz(nxl,:)+RBUzt0.*Uzt0(nxf,:);
            Ux(ixf,:) = RAUx.*Ux(ixf-1,:)+RAUxt0.*Uxt0(ixf,:);
            Uz(ixf,:) = RBUz.*Uz(ixf-1,:)+RBUzt0.*Uzt0(ixf,:);
            if ix1 == 3
                Ux(ixf+1,:) = RAUx.*Ux(ixf,:)+RAUxt0.*Uxt0(ixf+1,:);
                Uz(ixf+1,:) = RBUz.*Uz(ixf,:)+RBUzt0.*Uzt0(ixf+1,:);
            end
        end
        if iRbnd == 8
            %Free surface at right of model
            izF = 1:nzf-1;
                Uz(nxf,1) = Uz(nxl,1)-Ux(nxl,1);
                Uz(nxf,izF+1) = Uz(nxl,izF+1)+Ux(nxl,izF)-Ux(nxl,izF+1);
                Ux(nxf,izF) = Ux(nxl,izF)+LKrat(nxf,izF).*(Uz(nxf,izF)-Uz(nxf,izF+1));
                Ux(nxf,nzf) = Ux(nxl,nzf);
        end


    %Do the finite-difference calculations
    for iz = izC:nzf                % Loop through all Z values
      %Calculate derivatives of Ux with respect to x  (1)
        %for ix = ix1:ix9+1
        for ix = 2:nxf
            %DUxDx(ix+1,iz) = (Ux(ix+1,iz)-Ux(ix,iz)).*Lp2m(ix+1,iz);
            DUxDx(ix,iz) = (Ux(ix,iz)-Ux(ix-1,iz)).*Lp2m(ix,iz);
        end
        %for ix = ix1:ix9
        for ix = 2:ix9
            D2UxDx(ix,iz) = DUxDx(ix+1,iz)-DUxDx(ix,iz);
        end
      %Calculate derivatives of Uz with respect to x  (8)
        %for ix = ix1-1:ix9
        for ix = 1:ix9
            DUzDx(ix,iz) = (Uz(ix+1,iz)-Uz(ix,iz)).*Mu(ix,iz);
        end
        %for ix = ix1:ix9
        for ix = 2:ix9
            D2UzDx(ix,iz) = DUzDx(ix,iz)-DUzDx(ix-1,iz);
        end
    end
    %for iz = iz1:iz9+1
    for iz = izC+1:nzf
        %Calculate derivatives of Ux with respect to z  (3)
        DUxDz(:,iz) = (Ux(:,iz)-Ux(:,iz-1)).*Mu(:,iz);
        %Calculate derivatives of Uz with respect to z  (7)
        DUzDz(:,iz-1) = (Uz(:,iz)-Uz(:,iz-1)).*Lp2m(:,iz); %1 shift
        %for ix = 1:nxf-1
        for ix = 1:ix9
            D2UxDxzS(ix+1,iz) = (DUxDz(ix+1,iz)-DUxDz(ix,iz));
            D2UxDxzP(ix+1,iz) = (DUxDx(ix+1,iz)-DUxDx(ix+1,iz-1));
            D2UzDxzS(ix,iz-1) = (DUzDx(ix,iz)-DUzDx(ix,iz-1));
            D2UzDxzP(ix,iz-1) = +(DUzDz(ix+1,iz-1)-DUzDz(ix,iz-1));
        end
    end
    %for iz = iz1:iz9
    for iz = izC+1:iz9
        D2UxDz(:,iz) = DUxDz(:,iz+1)-DUxDz(:,iz);
        D2UzDz(:,iz) = DUzDz(:,iz)-DUzDz(:,iz-1);         %Check this
    end
    if nTerms == 6

        h1(ixm2,izm2) = conv2(D2UxDx(ixm2,izm2),wncMat(:,:,5),'same');
        h4(ixm2,izm2) = conv2(D2UxDz(ixm2,izm2),wncMat(:,:,6),'same');
        h2(ixm2,izm2) = conv2(D2UzDxzP(ixm2,izm2),wncMat(:,:,2),'same');
        h3(ixm2,izm2) = conv2(D2UzDxzS(ixm2,izm2),wncMat(:,:,3),'same');
        spacex = (h1+h4+h2-h3);
        h1(ixm2,izm2) = conv2(D2UzDz(ixm2,izm2),wncMat(:,:,1),'same');
        h4(ixm2,izm2) = conv2(D2UzDx(ixm2,izm2),wncMat(:,:,4),'same');
        h2(ixm2,izm2) = conv2(D2UxDxzP(ixm2,izm2),wncMat(:,:,2),'same');
        h3(ixm2,izm2) = conv2(D2UxDxzS(ixm2,izm2),wncMat(:,:,3),'same');
        spacez = (h1+h4+h2-h3);
    else
        spacex = D2UxDx+D2UxDz+D2UzDxzP-D2UzDxzS;
        spacez = D2UzDz+D2UzDx+D2UxDxzP-D2UxDxzS;
    end
        %disp(size(Rho)); disp(size(Ux)); disp(size(D2UxDx));
        Ux2=Ux*2-Uxt0+thrat*spacex./Rho;
        Uz2=Uz*2-Uzt0+thrat*spacez./Rho;
    if isfZ > 0
        surfUx(:,its) = Ux2(:,isfZ);    %Modify if structured surface (as above)
        surfUz(:,its) = Uz2(:,isfZ);
        %disp(max(Ux2(:,2)))
    end
    if iweX > 0
        wellUx(:,its) = Ux2(iweX,:);
        wellUz(:,its) = Uz2(iweX,:);
    end
    Uxt0 = Ux;
    Ux  = Ux2;
    Uzt0 = Uz;
    Uz  = Uz2;
end