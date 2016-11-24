function fdQCplot(modelDir)
    [parmFile] = fdFindParms(modelDir);
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

    %[~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,~,....
    [Dt,Dxz,xMin,lengthX,lengthZ,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,....
        gfdF,wncvar,mvTif,contBlk] = readParmsB(parmFile);
        nx = round(lengthX/Dxz)+1;
        nz = round(lengthZ/Dxz)+1;
        %gfdFile] = readParmsB(parmFile);
        gfdFile = [modelDir,'\',gfdF];
        [ix1,iz1,nxf,nzf,nxplot,initzp,nzplot] =.....
    fdInitB3sizes(Dxz,nx,nz,lengthX,0,lengthZ,wncvar); %,iLbnd,shotX);
    %fdInitB3sizes(Dxz,nx,nz,mvXmax,mvZtop,mvZmax,wncMat); %,iLbnd,shotX);
        [Lp2m,Mu,Rho,LKrat,gfdFile,zMin] =...
    fdInitModB7(modelDir,gfdFile,Dxz,ix1,iz1,nxf,nzf,xMin,contBlk);
    Htitle = ['  Vp   ';'  Vs   ';'Density'];
    Vel(:,:,1) = sqrt(Lp2m./Rho);
    Vel(:,:,2) = sqrt(Mu./Rho);
    Vel(:,:,3) = Rho;
    vectX = [1:nxf]*Dxz;
    vectZ = [1:nzf]*Dxz;
    for iP = 1:3
        figure
        contourf(vectX,vectZ,Vel(:,:,iP)')
        title(Htitle(iP,:))
        axis ij
        colormap jet
        colorbar
        xlabel('X co-ordinate')
        ylabel('Depth')
        %boldlines
        bigfont(gca,1.5,2,1)
        whitefig
        grid off
    end