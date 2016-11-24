function [vel,x,z]=marmousimodel(dx)
% MARMOUSIMODEL ... return the 2D p-wave Marmousi model
%
% [vel,x,z]=marmousimodel(dx)
%
% dx ... must be either 5, 10 or 25. This specifies the grid size in both x
%       and z for the output model. 
% vel ... matrix of p-wave velocities
% x ... x (column) coordinate for vel
% z ... z (row) coordinate for vel
% 
% You can plot the result with
% figure;
% imagesc(x,z,vel);colorbar
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

%
if(nargin==0)
    [vel,x,z]=marmousimodel(5);
    
    figure
    imagesc(x,z,vel);colorbar
    title('Marmousi model, colors indicate velocity')
    xlabel('distance (m)')
    zlabel('depth (m)')
    
    clear
    return;
end



if(dx~=5 && dx~= 10 && dx~=25)
    error('only dx of 5, 10, and 25 are available')
end
vel=[];
x=[];
z=[];
if(dx==25)
    s=which('raymarmousi_demo');
    if(isempty(s))
        error('25 m Marmousi model not found, you need to load and install the CREWES toolbox')
    end
    ind = strfind(s,'raymarmousi_demo');
    sm=[s(1:ind-1) 'marmousi_mod'];
    disp(['Marmousi model loaded from ' sm])
    load(sm)
    ind=find(x>9200);
    if(~isempty(ind))
        x(ind)=[];
        vel(:,ind)=[];
    end
elseif(dx==10)
    s=which('afd_snap');
    if(isempty(s))
        error('10m Marmousi model not found, you need to load and install the CREWES toolbox')
    end
    ind = strfind(s,'afd_snap');
    sm=[s(1:ind-1) 'marmousi_dz10'];
    disp(['Marmousi model loaded from ' sm])
    load(sm)
elseif(dx==5)
    s=which('afd_snap');
    if(isempty(s))
        error('5m Marmousi model not found, you need to load and install the CREWES toolbox')
    end
    ind = strfind(s,'afd_snap');
    sm=[s(1:ind-1) 'marmousi_dz5'];
    disp(['Marmousi model loaded from ' sm])
    load(sm)
end
%make sure first x is zero
x=x-x(1);