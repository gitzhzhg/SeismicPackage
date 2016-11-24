function editdepth(edit,win)
% Edit the layer1 depth by draging layer1-layer2 interface 
% (values at each receivers) 
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

% Optional median filter applicable to depth values
depth=refdata('get','depth');
recelev=refdata('get','recelev');
set(gcf,'units','pixels','position',[0 0 864 576]);
% Median filter
if (edit==0)
  depth(2,:)=medfilt1(depth(2,:),win);
end
depthelev=recelev(2,:)-depth(2,:);
hold on;
d1handle = plot(depth(1,:),recelev(2,:),'color','y','linestyle','-')
d2handle = plot(depth(1,:),depthelev,'color','g','linestyle','-')
ylabel('Elevation (m)')
title('Depth model (surface=yellow; layer 1-2 interface=green)')
xlabel('Coordinate (m)')
% Call the edit function
editveldepth('init',[d2handle], 'depth');