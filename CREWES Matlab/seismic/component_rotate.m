function smat = component_rotate( smat, theta, ax )
%
% function rtrc = component_rotate( smat, theta, ax )
%
% This function performs a basic two-component rotation around a specified
% Cartesian axis. Traces are assumed to be in smat columns
%
% smat   = matrix containing three trace multiples representing three components
%         eg. smat(:,1) = v   or  smat(:,1) = v   or  smat(:,1) = z
%             smat(:,2) = h1      smat(:,2) = r       smat(:,2) = x
%             smat(:,3) = h2      smat(:,3) = t       smat(:,3) = y
%             smat(:,4) = v   or  smat(:,4) = v   or  smat(:,4) = z
%             smat(:,5) = h1      smat(:,5) = r       smat(:,5) = x
%             smat(:,6) = h2      smat(:,6) = t       smat(:,6) = y
%                :                  :                  :
%                   ax = 'v'           ax = 'r'           ax = 'y'
% theta  = scalar or vecotr contaning angle to rotate components clockwise 
%          in degrees. number of angles must equal the number of traces in
%          smat/3;
% ax     = Optional. 'v', 'h1', 'h2', 'r', 't', 'x', 'y', 'z', 
%          where 'z' would cause the x and y traces to be 
%          rotated around the 'z' axis. Axis 'z' is the default.
%
% by K.W. Hall, Nov. 2015
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

%Check inputs
if nargin < 2 || nargin > 3
    error('CREWES:component_rotate','Incorrect number of input arguments')
elseif isequal(nargin,2)
    ax = 'z';
end

ntr = size(smat,2);
ntheta = length(theta);

if mod(ntr,3)
    error('CREWES:component_rotate',...
        'Incorrect number of input traces, must be a multiple of three')
end

if ~isequal(ntr/3,ntheta)
    error('CREWES:component_rotate',...
        ['Incorrect number of angles, ' ...
         'length(theta) must be equal to the ' ...
         'number of traces in smat'])
end



%Transpose smat so trace data is in the rows.
% ie. z1,x1,y1     z1,z2,z3
%     z2,x2,y2 =>  x1,x2,x3
%     z3,x3,y3     y1,y2,y3
smat = smat';

%Convert theta to radians
theta = theta*pi()/180.0;

%Select rotation matrix
switch(ax)
    case 'z'
        R = @Rz; 
    case 'v'
        R = @Rz;
    case 'x'
        R = @Rx;
    case 'r'
        R = @Rx;
    case 'h1'
        R = @Rx;        
    case 'y'
        R = @Ry;
    case 't'
        R = @Ry;
    case 'h2'
        R = @Ry;      
    otherwise
        error('CREWES:component_rotate','Unknown axis')        
end

%Do the dirty deed
for ii=1:3:ntr
  smat(ii:ii+2,:) = R(theta(ii))*smat(ii:ii+2,:);
end

%Transpose smat so that trace data is back in the columns
smat = smat';

end %end function component_rotate

%Define rotation matrices
% [z,x,y] trace order
function r = Rz(theta)
r = [1.0 0.0 0.0;
     0.0 cos(theta) -sin(theta);
     0.0 sin(theta) cos(theta)];  
end

function r = Rx(theta)
r = [cos(theta) 0.0 sin(theta);
     0.0 1.0 0.0;
     -sin(theta) 0.0 cos(theta)]; 
end

function r = Ry(theta)
r = [cos(theta) -sin(theta) 0.0;
     sin(theta) cos(theta) 0.0;
     0.0 0.0 1.0];
end
