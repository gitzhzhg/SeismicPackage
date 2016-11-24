%Program to build 6 finite difference correction matricies for convolution
% To plot correction multipliers in the wavenumber domain
%  for the ideal and as implemented with these finite spatial filters
%   specify which term (from 1 to 6)
%Several sets of corrections are allowed - as yet specified by code in
%       fdCompB8Gilga.m
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

%Parameters are given in a .parc file
clear
parmFile = 'test.parc'; %'general.parc'; %'RaylAv.parc'; %'general.parc';
[Dt,Dxz,fractx,nWide,iPlTerm,wncvar,iZlvl,pvel,svel] = ....
            readParmsCr4(parmFile);
        disp(wncvar)
        disp(size(wncvar'))
nPv = length(pvel);
nSv = length(svel);
nZl = length(iZlvl);
nCorr = nPv;
if nPv ~= nSv; error('The number of P and S velocities don''t match'); end
if (nPv > 1) && (nPv > nZl); error('Not enough depth points'); end
disp(nCorr)
nspacopx = round(0.5*(nWide+1));

fractz = fractx; %.5;   %.5;    %.75;		% Fraction of z spectrum to match
nspacopz = nspacopx; %2; %3; %2;	%4;	% No. of points to use in z (one side)
%iterm = 1;	% 1, 2, 3, 4 or 6
%i3d = 0;	%0-contour plot	3-3d plot 1-samples in space
nx = 256;   %190;   %256;		%500;
nz = 256;   %125;   %256;		%500;
nxp = nspacopx;
nzp = nspacopz;
%   atitle = [wncvar,' correction factors ',num2str(nxp*2-1),'x',.....
%           num2str(nzp*2-1),', Svel=',num2str(svel),.....
% 	' Pvel=',num2str(pvel),' Dz=',num2str(Dxz),' Dt=',num2str(Dt)];
%disp(size(num2str(Dxz)))
  atitle = [wncvar,' correction factor, Svel=',num2str(svel(1)),.....
	' Pvel=',num2str(pvel(1)),' Dz=',num2str(Dxz),' Dt=',num2str(Dt)];
nxph = nxp*2-1;
nzph = nzp*2-1;
% ixnyqm = 0;		%>0 use Nyquist wavenumber in X
% iznyqm = 0;		%>0 use Nyquist wavenumber in Z
np2x = nextpow2(nx);
np2z = nextpow2(nz);
lfftx = 2^np2x;
lfftz = 2^np2z;
%disp(lfftx)

xvect = (0:lfftx-1)'*Dxz;
zvect = (0:lfftz-1)*Dxz;
r1111 = ones(nspacopz,1);
c1111 = ones(1,nspacopx);
x1111 = ones(size(xvect));
z1111 = ones(size(zvect));
%disp(size(xvect)); disp(size(zvect));

[wa,wnvectx] = fftrl(x1111,xvect,0,lfftx);
[wa,wnvectz] = fftrl(z1111,zvect,0,lfftz);
%disp(size(wnvectx))
clear x1111 z1111 wa 
wx1111 = ones(size(wnvectx));
wz1111 = ones(size(wnvectz));
%These parms used to define the wavenumber correction surfaces for each term
    %(to Nyquist)
nwnx = length(wnvectx);
nwnz = length(wnvectz);

lswavex = round(nwnx*fractx);
lswavez = round(nwnz*fractz);
wnvmx = wnvectx(1:lswavex);
% if ixnyqm>0
%   wnvmx(lswavex) = wnvectx(nwnx);
% end
wnvmz = wnvectz(1:lswavez);
% if iznyqm>0
%   wnvmz(lswavez) = wnvectz(nwnz);
% end
xv = xvect(1:nspacopx)*2*pi;
zv = zvect(1:nspacopz)*2*pi;
nsq = nspacopx*nspacopz;
lsq = lswavex*lswavez;

kmat = sqrt(((wnvectx.^2)*wz1111)+wx1111*(wnvectz.^2));
Tens = zeros(nspacopx,nspacopz,lswavex,lswavez);
itInc = 0;
%if p2vel > 0; itInc = 6; end
init = 1; last = 6;
wncmat = zeros(nxph,nzph,last+itInc);
itInc = 0;
disp(iPlTerm)
if iPlTerm > 0
    init = iPlTerm; last = iPlTerm;
    %lowEff = 1;
end
for iL = 1:nCorr
    for iterm=init:last
      %Define the wavenumber correction surfaces for each term (to Nyquist)
      if iterm==1
        wncorr = (sinc(pvel(iL)*Dt*kmat)./(wx1111*sinc(Dxz*wnvectz))).^2;
        cPSdd(:,1) = 'zPzz';
      end
      if iterm==2
        wncorr = (sinc(pvel(iL)*Dt*kmat)).^2./(sinc(Dxz*wnvectx)*.....
             sinc(Dxz*wnvectz));
        cPSdd(:,2) = 'Pxz ';
      end
      if iterm==3
        wncorr = (sinc(svel(iL)*Dt*kmat)).^2./(sinc(Dxz*wnvectx)*sinc(Dxz*wnvectz));
        cPSdd(:,3) = 'Sxz ';
      end
      if iterm==4
        wncorr = (sinc(svel(iL)*Dt*kmat)./sinc(Dxz*wnvectx*wz1111)).^2;
        cPSdd(:,4) = 'zSxx';
      end
      if iterm==5
        wncorr = (sinc(pvel(iL)*Dt*kmat)./sinc(Dxz*wnvectx*wz1111)).^2;
        cPSdd(:,5) = 'xPxx';
      end
      if iterm==6
        wncorr = (sinc(svel(iL)*Dt*kmat)./sinc(Dxz*wx1111*wnvectz)).^2;
        cPSdd(:,6) = 'xSzz';
      end
      %Build up the array of elementary matrices
      for ic=1:lswavex
        for ir=1:lswavez
        elem = (cos(wnvmx(ic)*xv)*c1111).*(r1111*(cos(wnvmz(ir)*zv)));
        elem(2:nspacopz,:) = elem(2:nspacopz,:)*2;
        elem(:,2:nspacopx) = elem(:,2:nspacopx)*2;
        Tens(:,:,ic,ir) = elem;
        end
      end
      Tensr = reshape(Tens,[nsq lsq]);
      Tmin1 = inv(Tensr*Tensr');
      desir = wncorr(1:lswavex,1:lswavez);
    %   if ixnyqm>0
    % 	desir(:,lswavex) = wncorr(1:lswavez,nwnx)*0.9;
    %   end
    %   if iznyqm>0
    % 	desir(lswavez,:) = wncorr(nwnz,1:lswavex)*0.9;
    %   end
      desirr = reshape(desir,[1 lsq]);
      spc = Tmin1*Tensr*desirr';        %Least squares optimum
      %disp(spc)
      %spcm = reshape(spc,[nspacopz nspacopx]);
      spcm = reshape(spc,[nspacopx nspacopz]);

      zphmat = zeros(nxph,nzph);
      zphmat(nxp:nxph,nzp:nzph) = spcm;
      zphmat(nxp-1:-1:1,nzp:nzph) = spcm(2:nxp,1:nzp);
      zphmat(nzp:nzph,nxp-1:-1:1) = spcm(1:nzp,2:nxp);
      zphmat(nxp-1:-1:1,nzp-1:-1:1) = spcm(2:nxp,2:nzp);
      %size(wmcmat)
      wncmat(:,:,iterm+itInc) = zphmat;
      %disp(zphmat)
    end
    itInc = itInc + 6;
%     if p2vel <= 0; break
%     else pvel = p2vel;
%         svel = s2vel;
%     end
end
if iPlTerm == 0
    save(wncvar, 'wncmat', 'iZlvl')
    disp(atitle)
    disp(wncmat)
else
    %Build complete matrix and try a normal transform
    zxmat = zeros(lfftx,lfftz);
    %disp(size(zxmat))
    zxmat(1:nxph,1:nzph) = zphmat;
    ixx = [nxp:lfftx,1:nxp-1];
    izz = [nzp:lfftz,1:nzp-1];
    zxmat = zxmat(ixx,izz);
    %disp(size(zxmat))
    %[wa,kz,kx]=fktran(zxmat,zvect,xvect);
    wa = fftn(zxmat);
    wahold = wa(1:lfftx/2+1,1:lfftz/2+1);
    %disp(size(wahold))
    %[kx] = fKvector(xvect);

    cvect = (0:.02:2);
    iPass = 1;
    wnc = wncorr;
    while iPass<3
        figure
%         if lowEff==1
%             if iPass == 1
%                 wnc = squeeze(wncorr(1,:,:));
%             else
%                 wnc = squeeze(wahold(1,:,:));
%             end
%             wn1 = wnvy; wn2 = wnvz;
%             xlab = 'Wavenumber in Y'; ylab = 'Wavenumber in Z';
%         else if lowEff==2
%             if iPass == 1
%                 wnc = squeeze(wncorr(:,1,:));
%             else
%                 wnc = squeeze(wahold(1,:,:));
%             end
%                 %wnc = squeeze(wncorr(:,1,:));
%                 %wnc = squeeze(wahold(:,1,:));
%                 wn1 = wnvx; wn2 = wnvz;
%                 xlab = 'Wavenumber in X'; ylab = 'Wavenumber in Z';
%             %else wnc = squeeze(wncorr(:,:,1));
%             else
%                 if iPass == 1
%                     wnc = squeeze(wncorr(:,:,1));
%                 else
%                     wnc = squeeze(wahold(:,:,1));
%                 end
%                 %wnc = squeeze(wahold(:,:,1));
%                 wn1 = wnvx; wn2 = wnvy;
%                 xlab = 'Wavenumber in X'; ylab = 'Wavenumber in Y';
%             end
%         end
%         [C,h,cf]=contourf(wn1,wn2 ,wnc,cvect);
        %disp(size(wnvectx)); disp(size(wnvectz)); disp(size(wnc)); 
        [C,h,cf]=contourf(wnvectx,wnvectz ,wnc',cvect);
        clabv = (0:.1:2);
        clabel(C,h,clabv);
        %clabel(C,h,'LabelSpacing',5000)
        axis equal
        %Indicate design region
        ang = (0:.01:1)*pi*0.5;
        %rad = kx(nkx)/2;    %.04;    %.025;
        rad = wnvectx(nwnx)*fractx;    %.04;    %.025;
        line(sin(ang)*rad,cos(ang)*rad)
        caxis([0 2]);
        colormap jet      %winter
        colorbar
        xlab = 'Wavenumber in X'; ylab = 'Wavenumber in Z';
        xlabel(xlab)
        ylabel(ylab)
    %     nusex = nxph;
    %     nusey = nyph;
    %     nusez = nzph;
        whitefig
%   atitle = [wncvar,' correction factor, Svel=',num2str(svel),.....
% 	' Pvel=',num2str(pvel),' Dz=',num2str(Dxz),' Dt=',num2str(Dt)];
        title2 = ['Svel=',num2str(svel(1)),' Pvel=',num2str(pvel(1)),.....
            ' Dx=',num2str(Dxz),' Dt=',num2str(Dt)];
        str1 = 'Correction operator ';
        str3 = [', for ',cPSdd(:,iterm)'];
        if iPass == 1
            %title(['Ideal ',cPSdd(:,iterm)',' operator '])
            wnc = wahold;
            title1 = [str1, '(ideal)',str3];
        else
%             operator = [cPSdd(:,iterm)',' operator size ',num2str(nxph),....
%             ' by ',num2str(nzph)];
%             title({atitle,operator})
            title1 = [str1,wncvar,', size ',num2str(nxph),' by ',num2str(nzph),str3];
        end
        title({title1,title2})
        iPass = iPass+1;
    end
end