function [smat,theta] = component_auto_rotate( smat, ax )
%function [smat, theta] = component_auto_rotate( smat, ax )
%
% This function searches for the optimal angle to rotate around axis ax,
% and returns a vector (theta) containing those angles for each
% trace-triple as well a an smat containing the rotated trace.
% Traces are assumed to be in smat columns.
%
% smat = matrix containing three trace multiples representing three components
%         eg. smat(:,1) = v   or  smat(:,1) = v   or  smat(:,1) = z
%             smat(:,2) = h1      smat(:,2) = r       smat(:,2) = x
%             smat(:,3) = h2      smat(:,3) = t       smat(:,3) = y
%             smat(:,4) = v   or  smat(:,4) = v   or  smat(:,4) = z
%             smat(:,5) = h1      smat(:,5) = r       smat(:,5) = x
%             smat(:,6) = h2      smat(:,6) = t       smat(:,6) = y
%                :                  :                  :
%                   ax = 'v'           ax = 'r'           ax = 'y'
%
% ax     = Optional. 'v', 'h1', 'h2', 'r', 't', 'x', 'y', 'z', 
%          where 'v' would cause the x and y traces to be 
%          rotated around the 'v' axis. Axis 'v' is the default.
% 
% theta  = scalar or vector contaning angle to rotate components clockwise 
%          in degrees. The number of angles returned in theta will equal
%          the number of traces in smat divided by 3;
%
% Algorithm searches for the maximum sum of squares of trace amplitudes after 
% rotating from -90 to +80 in 10 degree increments, then scans -10 to +10
% of that answer in 0.1 degree increments to refine theta to one decimal
% place. Note that each theta (+180) returned represents two possible answers - 
% normal or reverse polarity.
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
if nargin < 1 || nargin > 2
    error('CREWES:component_find_rotation_angle','Incorrect number of input arguments')
elseif isequal(nargin,1)
    ax = 'z';
end

ntr = size(smat,2);

if mod(ntr,3)
    error('CREWES:component_find_rotation_angle',...
        'Incorrect number of input traces, must be a multiple of three')
end

%which trace are we maximising energy on?
switch ax
    case 'z'
        trc = 2;
    case 'v'
        trc = 2;
    case 'x'
        trc = 1;
    case 'h1'
        trc = 1;
    case 'r'
        trc = 1;        
    case 'y'
        trc = 1;
    case 'h2'
        trc = 1;        
    case 't'
        trc = 1;
    otherwise
        error('CREWES:component_auto_rotate','Unknown input axis')
end

% Cartesian coordinates, x is inline, y is crossline =>
% for rotation around v(z), we want max energy on r (x,h1)
% for rotation around t(y), we want max energy on v

%generate initial vector of search angles
coarse_search_angles = (-90.0:10.0:80.0);
fine_search_angles = (-9.9:0.1:9.9);

%pre-allocate theta
theta = zeros(1,ntr/3);

%do the dirty deed
jj=1;
for ii=1:3:ntr
  % replicate enough trace-triples to test all angles
  coarse_rotations = repmat(smat(:,ii,ii+2),1,length(coarse_search_angles));
  fine_rotations = repmat(smat(:,ii,ii+2),1,length(fine_search_angles));
  
  %preform the coarse rotations
  coarse_rotations = ...
      component_rotate(coarse_rotations, coarse_search_angles, ax);
  theta(jj) = find_theta(coarse_rotations(:,trc:3:end));
  
  %perform the fine rotations
  fine_rotations = ...
      component_rotate(fine_rotations, fine_search_angles+theta(jj), ax);
  [theta(jj),idx] = find_theta(fine_rotations(:,trc:3:end));  

  %update smat
  smat(:,ii,ii+2) = fine_rotations(:,idx*3-2:idx*3);
  
end %end for ii=1:3:ntr

end %end function component_auto_rotate


function [theta,idx] = find_theta(r,angles)

%sum of the squares of the amplitudes
a2 = sum(r.*r,1);

if isequal(sum(a2_r),0.0)
    ii = angles == 0.0;
    theta = 0.0;
else
    ii = a2 == max(a2);
    theta = angles(idx);
end
  
tnum = 1:length(angles)
idx = tnum(ii);

end %end function find_theta



% theta = rotate_that_puppy( trc, search_angles);
% 
% %Do it again
% search_angles = (-10.0:0.1:10.0)+theta;
% theta = rotate_that_puppy( trc, search_angles);
% 
% end
% 
% function theta = rotate_that_puppy( trc, angles)
% 
% %how many traces, and how many samples per trace
% [m,n] = size(trc);
% 
% %how many angles
% p = length(angles);
% 
% %pre-allocate full rotations matrix
% rotations = zeros(m,n*p);
% 
% %Perform a suite of component rotations
% for aa = 1:length(angles)
%     idx1 = aa*3-2;
%     idx2 = idx1+2;
%     
%     rotations(:,idx1:idx2) = component_rotate(trc,angles(aa));    
% end
% 
% %We only care about the x->r rotation, so separate that out
% r = rotations(:,2:3:end);
% 
% %sum of the squares of the radial component amplitudes
% a2_r = sum(r.*r,1);
% 
% if isequal(sum(a2_r),0.0)
%     theta = 0.0;
% else    
%     %angle that maximizes amplitudes on radial component
%     theta = angles(a2_r == max(a2_r));
% end
% 
% end
% 
% 




