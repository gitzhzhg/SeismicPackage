function [unow,uthen]=spongeABC(unow,uthen,nx,nz,nxsponge,nzsponge,coeff)
%
% spongeABC(unow,uthen,nx,nz,nxsponge,nzsponge,coeff)
%
% spongeABC: Sponge Absorbing boundary condtion
%
% 	unow:     the current wavefields
%
%	uthen:    the previous wavefields
%
%	nx:       model size of lateral direction
%
%	nz:       model size of vertical direction
%
%   nxsponge: sponge size of lateral direction
%
%   nzsponge: sponge size of vertical direction
%
%   coeff:    damping coefficient
%   Xiang Du, May   2007, $1.0
%             Sept. 2008, $1.1
%             Nov.  2008, $1.2
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

coeff2=coeff*coeff;

%left side
for t=1:nxsponge
  unow(:,t)=unow(:,t)*exp(-coeff2*(nxsponge-t)*(nxsponge-t));
  uthen(:,t)=uthen(:,t)*exp(-coeff2*(nxsponge-t)*(nxsponge-t));
end

%right side
for t=nx-nxsponge+1:nx
  unow(:,t)=unow(:,t)*exp(-coeff2*(t-nx+nxsponge-1)*(t-nx+nxsponge-1));
  uthen(:,t)=uthen(:,t)*exp(-coeff2*(t-nx+nxsponge-1)*(t-nx+nxsponge-1));
end

%bottom side
for t=nz-nzsponge:nz
  unow(t,:)=unow(t,:)*exp(-coeff2*(t-nz+nzsponge-1)*(t-nz+nzsponge-1));
  uthen(t,:)=uthen(t,:)*exp(-coeff2*(t-nz+nzsponge-1)*(t-nz+nzsponge-1));
end

%top side
for t=1:nzsponge
  unow(t,:)=unow(t,:)*exp(-coeff2*(nzsponge-t)*(nzsponge-t));
  uthen(t,:)=uthen(t,:)*exp(-coeff2*(nzsponge-t)*(nzsponge-t));
end