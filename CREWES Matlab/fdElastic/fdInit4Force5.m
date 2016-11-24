function [nfmax,force,Ampfx,Ampfz,ixxA,ixzA,izxA,izzA] = ...
    fdInit4Force5(shotDepth,shotX,energySrc,Dxz,ix1,iz1,wave,mint,modelDir)
%function [nfmax,force,Ampfx,Ampfz,ixxA,ixzA,izxA,izzA] = ...
%    fdInit4Force4(shotDepth,shotX,energySrc,Dxz,ix1,iz1,wave,mint)
% Specify positioning of forces over time, in x, y and z
% Copy of 3D version to ensure compatibility
%The input parameters are
%shotDepth  . Z (depth) of the FD initializing source
%shotX   .... X (from the FD model left) of the initializing source
%energySrc  . The directivity code of the energy source
%Dxz     .... FD sample rate in (metres)
%ix1   ...... X co-ordinate of start of model in arrays
%iz1   ...... Z co-ordinate of start of model in arrays
%wave    .... The wavelet amplitudes in sequence
%mint    .... The interval at which a movie frame is output or saved
%The output parameters are
%nfmax   .... The length of the source time series
%force   .... The same as 'wave'
%Ampfx   .... The relative indices of source forces in the Ux direction
%Ampfz   .... The relative indices of source forces in the Uz direction
%ixxA    .... Source position: x index for the Ux component
%ixzA    .... Source position: z index for the Ux component
%izxA    .... Source position: x index for the Uz component
%izzA    .... Source position: z index for the Uz component
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
  
%Set up for a source at an X, Y, Z position
  ix = round(shotX/Dxz)+ix1;
  %iy = round(shotY/Dxz)+iy1;
  iz = round(shotDepth/Dxz)+iz1;
  iy = 1;                                  % - 2D change
  
% ampfx modifies Ux, ampfy modifies Uy, ampfz modifies Uz

if energySrc==10 
  %Define a compressional source point (3 vector dipoles)
    %Specify sizes of the couples
    ampfx = ones(2,1,1);  %Opposed X couple in the X direction
    ampfy = ones(1,1,1);  %Opposed Y couple in the Y direction
    ampfz = ones(1,1,2);  %Opposed Z couple in the Z direction
    Ampfx = ones(2,1);  %Opposed X couple in the X direction - 2D addition
    Ampfz = ones(1,2);  %Opposed Z couple in the Z direction - 2D addition
    
    %Specify amplitudes of the couples
    ampfx(:,1,1) = [-1,1]; %Will work at mirror boundary
    ampfy(1,:,1) = 0; %[-1,1]; %Will work at mirror boundary - 2D change
    ampfz(1,1,:) = [-1,1];
    Ampfx(:,1) = [-1,1]; %Will work at mirror boundary - 2D addition
    Ampfz(1,:) = [-1,1];                            % - 2D addition
    %Position couples to fit the conventions of the 3D grid
    ixxm = ix-1;
    iyym = iy-1;
    izzp = iz+1;
    %disp(ampfx)
    sourcePattern = 'explosion';
end
if energySrc==11 
  %Define a volume rupture in Z (1 vector dipole)
    %Specify sizes of the couples
      ampfx = ones(1,1,1);
      ampfy = ones(1,1,1);
      ampfz = ones(1,1,2);  %Opposed Z couple in the Z direction
      Ampfx = ones(1,1);                                    % - 2D addition
      Ampfz = ones(1,2);  %Opposed Z couple in the Z direction - 2D addition
      %Specify amplitudes of the couples
      ampfx(:,1,1) = 0; %[0,0];  %No action in the X direction
      ampfy(1,:,1) = 0; %[0,0];  %No action in the Y direction
      ampfz(1,1,:) = [-1,1]; %Opposed couple, upper +, lower -
      Ampfx(:,1) = 0; %[0,0];  %No action in the X direction - 2D addition
      Ampfz(1,:) = [-1,1]; %Opposed couple, upper +, lower - - 2D addition
      %Position couples to fit the conventions of the 3D grid
      ixxm = 0;      %Irrelevant
      iyym = 0;      %Irrelevant
      izzp = iz+1;
    sourcePattern = 'Z_rupture';
end
if energySrc==12
%Looks like the Aki-Richards double couple
    %Specify sizes of the couples
    ampfx = ones(1,1,2);  %Ux will be modified at two adjacent Z points
    ampfy = ones(1,1,1);
    ampfz = ones(2,1,1);  %Uz will be modified at two adjacent X points
    Ampfx = ones(1,2);  %Ux will be modified at two adjacent Z points - 2D addition
    Ampfz = ones(2,1);  %Uz will be modified at two adjacent X points - 2D addition
    %Specify amplitudes of the couples
    ampfx(1,1,:) = [1,-1]; %Opposed couple, upper +, lower -
    ampfy(1,:,1) = 0; %[0,0];  %No action in the Y direction
    ampfz(:,1,1) = [1,-1]; %Opposed couple, left +, right -
    Ampfx(1,:) = [1,-1]; %Opposed couple, upper +, lower - - 2D addition
    Ampfz(:,1) = [1,-1]; %Opposed couple, left +, right - - 2D addition
    %Position couples to fit the conventions of the 3D grid
    ixxm = ix+1;
    iyym = 0;      %Irrelevant
    izzp = iz-1;
    sourcePattern = 'doubleCouple';
end
if energySrc==13 
  %Define a squeeze and bulge source point (2 vector dipoles)
    %Specify sizes of the couples
    ampfx = ones(2,1,1);  %Opposed X couple in the X direction
    ampfy = ones(1,1,1);  %Opposed Y couple in the Y direction
    ampfz = ones(1,1,2);  %Opposed Z couple in the Z direction
    Ampfx = ones(2,1);  %Opposed X couple in the X direction - 2D addition
    Ampfz = ones(1,2);  %Opposed Z couple in the Z direction - 2D addition
    %Specify amplitudes of the couples
    ampfx(:,1,1) = [1,-1]; %Squeeze
    ampfy(1,:,1) = 0;
    ampfz(1,1,:) = [-1,1]; %Bulge
    Ampfx(:,1) = [1,-1]; %Squeeze - 2D addition
    Ampfz(1,:) = [-1,1]; %Bulge - 2D addition
    %Position couples to fit the conventions of the 3D grid
    ixxm = ix-1;
    iyym = iy-1;
    izzp = iz+1;
    %disp(ampfx)
    sourcePattern = 'SqBulge';
end
if energySrc==21 
  %Define an external force in Z (1 vector dipole)
    %Specify sizes of the couples
      ampfx = ones(1,1,1);
      ampfy = ones(1,1,1);
      ampfz = ones(1,1,1);  % No opposed Z couple in the Z direction
      %ampfz = ones(2,1,1);  % Double source
      Ampfx = ones(1,1);                                     % - 2D addition
      Ampfz = ones(1,1);  %No opposed Z couple in the Z direction - 2D addition
      %Ampfz = ones(2,1);  % Double source
      %Specify amplitudes of the couples
      ampfx(:,1,1) = 0; %[0,0];  %No action in the X direction
      ampfy(1,:,1) = 0; %[0,0];  %No action in the Y direction
      ampfz(1,1,:) = 1; %[-1,1]; %Opposed couple, upper +, lower -
      %ampfz(:,1,1) = [1,1]; % Double source
      Ampfx(:,1) = 0; %[0,0];  %No action in the X direction - 2D addition
      Ampfz(1,:) = 1; %[0,1]; %monopole, upper zero, lower down - 2D addition
      %Ampfz(:,1) = [1,1]; % Double source
      %Position couples to fit the conventions of the 3D grid
    ixxm = ix-1;
    iyym = iy-1;
    izzp = iz+1;
    sourcePattern = 'Z_monopole';
end
if energySrc==22 
  %Define an external force in X (1 vector dipole)
    %Specify sizes of the couples
      ampfx = ones(1,1,1);
      ampfy = ones(1,1,1);
      ampfz = ones(1,1,1);  % No opposed Z couple in the Z direction
      Ampfx = ones(1,1);                                     % - 2D addition
      Ampfz = ones(1,1);  %No opposed Z couple in the Z direction - 2D addition
      %Specify amplitudes of the couples
      ampfx(:,1,1) = 1; %[0,0];  
      ampfy(1,:,1) = 0; %[0,0];  %No action in the Y direction
      ampfz(1,1,:) = 0; %[-1,1]; %No action in the Z direction
      Ampfx(:,1) = 1; %[0,0];  %
      Ampfz(1,:) = 0; %[0,1]; %No action in the Z direction - 2D addition
      %Position couples to fit the conventions of the 3D grid
    ixxm = ix-1;
    iyym = iy-1;
    izzp = iz+1;
    sourcePattern = 'X_monopole';
end
if energySrc==23
%This is the external twist
    %Specify sizes of the couples
    ampfx = ones(1,1,2);  %Ux will be modified at two adjacent Z points
    ampfy = ones(1,1,1);
    ampfz = ones(2,1,1);  %Uz will be modified at two adjacent X points
    Ampfx = ones(1,2);  %Ux will be modified at two adjacent Z points - 2D addition
    Ampfz = ones(2,1);  %Uz will be modified at two adjacent X points - 2D addition
    %Specify amplitudes of the couples
    ampfx(1,1,:) = [1,-1]; %Opposed couple, upper +, lower -
    ampfy(1,:,1) = 0; %[0,0];  %No action in the Y direction
    ampfz(:,1,1) = [-1,1]; %Opposed couple, left +, right -
    Ampfx(1,:) = [1,-1]; %Opposed couple, upper +, lower - - 2D addition
    Ampfz(:,1) = [-1,1]; %Opposed couple, left +, right - - 2D addition
    %Position couples to fit the conventions of the 3D grid
    ixxm = ix+1;
    iyym = 0;      %Irrelevant
    izzp = iz-1;
    sourcePattern = 'XZ-twist';
end
if energySrc==24 
  %Define an external force in X (1 vector dipole)
    %Specify sizes of the couples
      ampfx = ones(1,1,1);
      ampfy = ones(1,1,1);
      ampfz = ones(1,1,2);  % Opposed Z couple in the Z direction
      Ampfx = ones(1,1);                                     % - 2D addition
      Ampfz = ones(1,2);  %Opposed Z couple in the Z direction - 2D addition
      %Specify amplitudes of the couples
      ampfx(:,1,1) = 1; %[0,0];  
      ampfy(1,:,1) = 0; %[0,0];  %No action in the Y direction
      ampfz(1,1,:) = [-1,1]*0.5; %Opposed Z couple in the Z direction
      Ampfx(:,1) = 1; %[0,0];  %
      Ampfz(1,:) = [-1,1]*0.5; %Opposed Z couple in the Z direction - 2D addition
      %Position couples to fit the conventions of the 3D grid
    %ixxm = ix-1;
    ixxm = ix;
    iyym = iy-1;
    izzp = iz+1;
    sourcePattern = 'X_monopole_modified';
end
if energySrc==31 
  %Define a compressional source point (3 vector dipoles)
    %Specify sizes of the couples
    ampfx = ones(2,1,1);  %Opposed X couple in the X direction
    ampfy = ones(1,1,1);  %Opposed Y couple in the Y direction
    ampfz = ones(1,1,2);  %Opposed Z couple in the Z direction
    Ampfx = ones(2,1);  %Opposed X couple in the X direction - 2D addition
    Ampfz = ones(1,2);  %Opposed Z couple in the Z direction - 2D addition
    
    %Specify amplitudes of the couples
    ampfx(:,1,1) = [-1,1]*0.5; %Will work at mirror boundary
    ampfy(1,:,1) = 0; %[-1,1]; %Will work at mirror boundary - 2D change
    ampfz(1,1,:) = [-1,1];
    Ampfx(:,1) = [-1,1]*0.5; %Will work at mirror boundary - 2D addition
    Ampfz(1,:) = [-1,1];                            % - 2D addition
    %Position couples to fit the conventions of the 3D grid
    ixxm = ix-1;
    iyym = iy-1;
    izzp = iz+1;
    %disp(ampfx)
    sourcePattern = 'vertExpl';
end
  nfmax = length(wave);
  force = zeros(1,nfmax+mint);
  force(1:nfmax) = wave;
%   disp(force)
%   stop
%Set up index increment arrays for inserting energy into displacement arrays
ixxA = ix; ixyA = iy; ixzA = iz; 
iyxA = ix; iyyA = iy; iyzA = iz; 
izxA = ix; izyA = iy; izzA = iz; 
    iShX = ixxm-ix; iShY = iyym-iy; iShZ = izzp-iz;
    %disp(iShX)
if ampfx~=0
    sz = size(ampfx);
    %disp(ampfx)
    [C,iDim] = max(sz);
    if (iDim==1)
        if(iShX<0); ixxA=ixxA-1; end
        ixxA = (ixxA:ixxA+1);
        %disp(ixxA)
    end
    if (iDim==2)
        if(iShY<0); ixyA=ixyA-1; end
        ixyA = (ixyA:ixyA+1);
    end
    if (iDim==3)
        if(iShZ<0); ixzA=ixzA-1; end
        ixzA = (ixzA:ixzA+1);
    end
end
if ampfy~=0
    sz = size(ampfy);
    [C,iDim] = max(sz);
    if (iDim==1)
        if(iShX<0); iyxA=iyxA-1; end
        iyxA = (iyxA:iyxA+1);
    end
    if (iDim==2)
        if(iShY<0); iyyA=iyyA-1; end
        iyyA = (iyyA:iyyA+1);
    end
    if (iDim==3)
        if(iShZ<0); iyzA=iyzA-1; end
        iyzA = (iyzA:iyzA+1);
    end
end
if ampfz~=0
    sz = size(ampfz);
    [C,iDim] = max(sz);
    if (iDim==1)
        if(iShX<0); izxA=izxA-1; end
        izxA = (izxA:izxA+1);
    end
    if (iDim==2)
        if(iShY<0); izyA=izyA-1; end
        izyA = (izyA:izyA+1);
    end
    if (iDim==3)
        if(iShZ<0); izzA=izzA-1; end
        izzA = (izzA:izzA+1);
    end
end
iPlot = 99; %99; %0;
if iPlot~=0
    %Plot vectors of the energy sources
    ngx = 3; ngy = 3; ngz = 4;
    ixA = 2; iyA = 2; izA = 2;
    leng = .5; %.25;
    iShX = ixxm-ix; iShY = 0; iShZ = izzp-iz;
    %disp(iShX)
    hold off
    figure
    %X displacement vectors
    xyzInd = 'x'; ampf = ampfx;
    StGrSour(xyzInd,ampf,iShX,iShY,iShZ,ngx,ngy,ngz,ixA,iyA,izA,leng)
    %Y displacement vectors
    xyzInd = 'y'; ampf = ampfy;
    StGrSour(xyzInd,ampf,iShX,iShY,iShZ,ngx,ngy,ngz,ixA,iyA,izA,leng)
    %Z displacement vectors
    xyzInd = 'z'; ampf = ampfz;
    StGrSour(xyzInd,ampf,iShX,iShY,iShZ,ngx,ngy,ngz,ixA,iyA,izA,leng)
    axis equal
    set(gca,'zdir','rev','xticklabel',[' ';' ';' ';' '])
    set(gca,'yticklabel',[' ';' ';' ';' '],'zticklabel',[' ';' ';' ';' '])
    xlabel('X axis'); ylabel('Y axis'); zlabel('Z axis')
    plotTag = 'B'; %'';
    if ~isempty(plotTag)
        plotSrc = [modelDir '\' plotTag num2str(energySrc)];
      bigfont(gcf,1.5,1)
      %set(gcf,'Color',[0 .8 .608],'InvertHardcopy','off')
      %fileName = [sourcePattern plotSrc '.tif'];
      fileName = [ plotSrc '.tif'];
      disp(fileName)
      %eval(['print -dtiff -r150 ' fileName])
    end
end