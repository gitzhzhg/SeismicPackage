function dispplust
% Display of the average Plus Time values with their
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

% corresponding standard deviation and fold for each receiver
f=gcf;
fbcoord=refdata('get','fbcoord');
plust=refdata('get','plust');
% Figure split in three part, first the Plus Time average values,
% second the corresponding standard deviation, and third the fold
figure('menubar','none');
coordaxes = axes('position',[.1 .7 .8 .25]);
stdaxes = axes('position', [.1 .4 .8 .25]);
foldaxes = axes('position',[.1 .1 .8 .25]);
% Plus Time average values
axes(coordaxes);
hold on;
plot(plust(1,:),plust(2,:),'color','b','linestyle','-.')
ylabel('Plus Time (ms)')
title('Plus time')
% Standard deviation
axes(stdaxes);
hold on;
plot(plust(1,:),plust(4,:),'color','b','linestyle','*')
ylabel('Standard deviation (ms)')
% Fold
axes(foldaxes);
hold on;
plot(plust(1,:),plust(3,:),'color','b','linestyle','+')
ylabel('Fold')
xlabel('Coordinate (m)')
set(gcf,'units','pixels','position',[0 0 864 720]);
figure(f); set(gcf,'menubar','none');