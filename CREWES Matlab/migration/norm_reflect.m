%  NORM_REFLECT.m
%
%  Calculates the normal incidence reflectivity matix given a velocity
%  model matrix.  May be modified to take in a density matrix as well.
%
%  refl=norm_reflect(vel,dens,clipvalue)
%
%  refl...........matrix of normal incidence reflectivities
%  vel............the velocity matrix
%  dens...........matrix of densities same size as vel(default = ones(size(vel))
%  clipvalue......the number of columns and rows to "clip" or remove from
%                 the edge of the matrix (for absorbing boundary
%                 conditions) (default=0)
%
%  Zoron Rodriguez, G.F. Margrave 2006
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

function refl=norm_reflect(vel,dens,clipn)

if(nargin<3)
    clipn=0;
end
if (nargin<2);
    clipn=0;
    dens=ones(size(vel));
end

[nz,nx]=size(vel);

r=zeros(size(vel));

%loop over traces
for k=1:nx
    for s=2:nz
        I2=vel(s,k)*dens(s,k);
        I1=vel(s-1,k)*dens(s-1,k);
        r(s,k)= (I2-I1)/(I2+I1);
    end
end
   refl=zeros(size(r));
   refl(clipn+1:nz-clipn,clipn+1:nx-clipn)=r(clipn+1:nz-clipn,clipn+1:nx-clipn);