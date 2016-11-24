function [spnew,ssnew,rhobulk]=...
           flusub(logmat,z1,z2,rhog,zrok,rhoflbr,flkbr,sbr,rhoadd,addk)
%FLUSUB    well log fluid substitution (Smith et al., 2003 Geophysics)
%
%  [spnew,ssnew,rhobulk]=...
%           flusub(logmat,z1,z2,rhog,zrok,rhoflbr,flkbr,sbr,rhoadd,addk)
%
%logmat...  output of function READLAS (matrix containing slowness and
%              density logs. 
%           Must be in the order [depth, density, Sonic, Shear Sonic];
%           a porosity log can also be added to the end otherwise it will
%           be calculated from density.
%z1...      top of fluid substitution depth range.
%z2...      bottom of fluid substitution depth range.
%rhog...    grain density of the rock matrix.
%zrok...    bulk modulus of the mineral matrix.
%rhoflbr... fluid density of the existing pore filling fluid
%flkbr...   bulk modulus of the existing pore filling fluid
%sbr...     fractional saturation of the existing pore filling fluid
%              when replacing fluid is added
%              (zero describes complete replacement).
%rhoadd...  fluid density of the replacing fluid.
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

%addk...    bulk modulus of the replacing fluid.
   
%spnew...   P-wave slowness log following fluid substitution
%              in the depth range between z1 and z2.
%ssnew...   S-wave slowness log following fluid substitution
%              in the depth range between z1 and z2.
%rhobulk... bulk density log following fluid substitution
%              in the depth range between z1 and z2.

%
z=logmat(:,1);
rhosca=logmat(:,2);
if log10(mean(~isnan(rhosca)))>3
    rhosca=rhosca/1000;
end
sp=logmat(:,3);
vp=10^6./sp;
ss=logmat(:,4);
vs=10^6./ss;
sz=size(logmat);
if sz(2)>4
    phi=logmat(:,5);
    if log10(mean(~isnan(phi)))>1
        phi=phi/100;
    end
else
    phi=(rhosca-rhog)./(rhoflbr-rhog);
end
%
ind=between(z1,z2,z);
zint=z(ind);
rhoint=rhosca(ind);
vpint=vp(ind);
vsint=vs(ind);
phint=phi(ind);
%
satkint=rhoint.*(vpint.*vpint-4.0.*vsint.*vsint./3.0);
starkint=(satkint.*(phint.*zrok./flkbr+1.0-phint)-zrok)./...
                   (phint.*zrok./flkbr+satkint./zrok-1.0-phint);
rhoflnew=sbr*rhoflbr+(1.0-sbr)*rhoadd;
flknew=1.0/(sbr/flkbr+(1.0-sbr)/addk);
satknew=starkint+(1.0-starkint./zrok).^2./...
                   (phint./flknew+(1.0-phint)./zrok-starkint./zrok.^2);
rhonew=rhog.*(1.0-phint)+rhoflnew.*phint;
vsnewsq=vsint.*vsint.*rhoint./rhonew;
vpnewsq=satknew./rhonew+4.0.*vsnewsq./3.0;
%
spnew=sp;
spnew(ind)=10^6./sqrt(vpnewsq);
ssnew=ss;
ssnew(ind)=10^6./sqrt(vsnewsq);
rhobulk=rhosca;
rhobulk(ind)=rhonew;