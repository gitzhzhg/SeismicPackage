function plotptwist2(Ux,Uz,frClip,xvect,zvect,Dxz,ibar,iblack)
%Plot pressure and twist functions of the displacement field
%function plotptwist(Ux,Uz,frClip,xvect,zvect,Dxz,ibar,iblack)
%
%This code does an interpretation of the displacements
%by calculating the divergence (resulting from compressional energy)
%and the curl (representing torsional, or shear, energy).
%Pure P energy gives red/green shades, S energy blue/yellow shades.
%
%Ux  ...... %Matrix of displacements in the x direction
%Uz  ...... %Matrix of displacements in the z direction
%frClip ... %Reduces amplitudes higher than frClip to frClip level,
%where the maximum amplitude is translated to 1.
%xvect  ... %X-coordinate values of the Ux and Uz arrays
%zvect  ... %Z-coordinate values of the Ux and Uz arrays
%Dxz  ..... %The sample rate in space
%ibar  .... %Control whether to plot colour key, and at top or bottom
%iblack  .. %Control for colour of the plot background
%
% P.M. Manning, Oct 2008
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

[nx,nz] = size(Ux);
nxm = nx-1;
nzm = nz-1;
ixm = 1:nxm;
ixm2 = 1:nxm-1;
izm = 1:nzm;
Pr(ixm2+1,izm) = (Ux(ixm2,izm)-Ux(ixm2+1,izm))+(Uz(ixm2+1,izm)-Uz(ixm2+1,izm+1));
Pr(1,izm) = -2*Ux(1,izm)+(Uz(1,izm)-Uz(1,izm+1));
Tw(ixm,izm) = (Ux(ixm,izm)-Ux(ixm,izm+1))-(Uz(ixm,izm)-Uz(ixm+1,izm));
%plotcolourk(Pr(ixm,izm),Tw,clip,xvect(1:nxm),zvect(1:nzm),Dxz,1)
%plotcolourk(Pr,Tw,clip,xvect(1:nxm),zvect(1:nzm),Dxz,1)
%function plotcolourk(Uxt,Uzt,frmax,xvect,zvect,ibar,iblack)

if nargin<7
  ibar = 0;     %0 = no colourbar  >0 = bottom,  <0 = top
end
if nargin<8
  iblack = 0;   %<=0 = white background,  >0 = black background
end

if frClip<=0
  frClip = 1;
end
if frClip>1
  frClip = 1;
end
%sz = size(Uxt);
%nx = sz(1);
%nz = sz(2);
Arr = zeros(nz-1,nx-1,3);
amp = sqrt(Pr.^2+Tw.^2);
maxa = max(max(abs(amp)));
%Allow for clipping at fraction frClip of the maximum
maxnew = maxa*frClip;
if maxnew<=0
  maxnew = 1;
end
ampnew = amp;
Prnew = Pr;
Twnew = Tw;
if frClip<1
  large = find(abs(amp)>maxnew);
  ampnew(large) = maxnew;
  Prnew(large) = Pr(large)*maxnew./amp(large);  %Reduce clipped amplitudes in
  Twnew(large) = Tw(large)*maxnew./amp(large);  % proportion for Pr and Tw
end
r3o2 = sqrt(3)/2;

%include colour ring for a key
if ibar~=0
  nuse = min([nx,nz]);
  nbx = 2*round(nuse/20)+1;
  ibx = nx-nbx;
  nbz = nbx;
  if ibar<1
    ibz = 1;
    lbz = nbz;
  else
    %ibz = nz+1-nbz;
    ibz = nz-nbz;
    lbz = nzm;
  end
  
  %ixmid = round(nbx/2);
  %izmid = round(nbz/2);
  ixmid = (nbx+1)/2;
  izmid = ixmid;
  maxr2 = izmid^2;
  Uxmat = zeros(nbx,nbz);
  Uzmat = zeros(nbx,nbz);
  for iz=1:nbz
    for ix=1:nbx
        Uzh = iz-izmid;
        Uxh = ix-ixmid;
        rad2 = Uzh^2+Uxh^2;
        if rad2<maxr2
            %rad = sqrt(rad2);xmat,Uzmat,1,1:10,1:10
            Uxmat(ix,iz) = Uxh/ixmid;
            Uzmat(ix,iz) = Uzh/izmid;
        end
    end
  end
  %plotcolour(Uxmat,Uzmat,1,1:nbx,1:nbz)
  ampmat = sqrt(Uxmat.^2+Uzmat.^2);
  maxmat = max(max(abs(ampmat)));
  Prnew(ibx:nxm,ibz:lbz) = Uxmat*maxnew/maxmat;
  Twnew(ibx:nxm,ibz:lbz) = Uzmat*maxnew/maxmat;
  ampnew = sqrt(Prnew.^2+Twnew.^2);
end

if iblack<=0
  Arr(:,:,3)=1-(ampnew'-Twnew')/(maxnew*2);
  Arr(:,:,1)=1-(ampnew'+(.5*Twnew'-r3o2*Prnew'))/(maxnew*2);
  Arr(:,:,2)=1-(ampnew'+(.5*Twnew'+r3o2*Prnew'))/(maxnew*2);
else
  Arr(ixm,izm,1)=(ampnew'-Twnew')/(maxnew*2);
  Arr(ixm,izm,2)=(ampnew'+(.5*Twnew'-r3o2*Prnew'))/(maxnew*2);
  Arr(ixm,izm,3)=(ampnew'+(.5*Twnew'+r3o2*Prnew'))/(maxnew*2);
end

less0 = find(Arr<0);
Arr(less0) = 0;
%Arr(:,:,1)=uint8(-(Uzt'+amp)/(maxa*2));
%Arr(:,:,2)=uint8(-(amp-(.5*Uzt'+r3o2*Uxt'))/(maxa*2));
%Arr(:,:,3)=uint8(-(amp-(.5*Uzt'-r3o2*Uxt'))/(maxa*2));
image(xvect(ixm),zvect(izm),Arr)

if ibar~=0
%if ibar>999
    % Draw arrows
    hold on
    %Dxz = 2;
    aleng = Dxz*ixmid/2;
    ixmid = ixmid+ibx-1;
    izmid = izmid+ibz-1;
    xv = zeros(1,4);
    zv = zeros(1,4);
    uv = zeros(1,4);
    vv = zeros(1,4);
    anginc = pi/2;
    ang = 0;
    xmid = (ixmid-1)*Dxz+xvect(1);
    zmid = (izmid-1)*Dxz+zvect(1);
    for iang=1:4
        xv(iang) = cos(ang)*aleng/2+xmid;
        zv(iang) = sin(ang)*aleng/2+zmid;
        uv(iang) = cos(ang)*aleng;
        vv(iang) = sin(ang)*aleng;
        ang = ang+anginc;
    end
    quiver(xv,zv,uv,vv,0,'k')
    text(xmid,zmid-aleng*3,'T')
    text(xmid-aleng*3,zmid,'P')
    hold off
end