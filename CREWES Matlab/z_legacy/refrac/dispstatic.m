function dispstatic
% Display receiver and shot static corrections for the weathering layer
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

% (first layer, the elevation and the total
f=gcf;
recstat=refdata('get','recstat');
shotstat=refdata('get','shotstat');
recelev=refdata('get','recelev');
shotcoord=refdata('get','shotcoord');
nshots=refdata('get','nshots');
nrecs=refdata('get','nrecs');
% Receiver static corrections
figure('menubar','none');
hold on;
plot(recstat(1,:),'color','c','linestyle','+')
plot(recstat(2,:),'color','r','linestyle','*')
plot(recstat(3,:),'color','g','linestyle','o')
xlabel('Receiver number')
ylabel('Static corrections (ms)')
title('Receiver static corrections (weathering=plus; elevation=star; total=circle)')
% Shot static corrections
figure('menubar','none');
hold on;
plot(shotstat(1,:),'color','c','linestyle','+')
plot(shotstat(2,:),'color','r','linestyle','*')
plot(shotstat(3,:),'color','g','linestyle','o')
xlabel('Shot number')
ylabel('Static corrections (ms)')
title('Shot static corrections (weathering=plus; elevation=star; total=circle)')
% Surface consistent statics (X-coordinate)
figure('menubar','none');
hold on;
plot(recelev(1,:),recstat(1,:),'color','c','linestyle','+')
plot(recelev(1,:),recstat(2,:),'color','r','linestyle','*')
plot(recelev(1,:),recstat(3,:),'color','g','linestyle','o')
xlabel('Coordinate (m)')
ylabel('Static corrections (ms)')
title('Surface consistent static corrections (weathering=plus; elevation=star; total=circle)')
xy=axis;
t=xy(4)-xy(3);
d=t/20;
for n=10:10:nshots         % Label every 10th shot
    str=sprintf('%d',n); 
    text(shotcoord(n),xy(3)+d,str)
end
text(xy(1)+100,xy(3)+2*d,'shot number')
for n=20:20:nrecs         % Label every 20th receiver
    str=sprintf('%d',n); 
    text(recelev(1,n),xy(3)+3*d,str)
end
text(xy(1)+100,xy(3)+4*d,'receiver number')
set(gcf,'units','pixels','position',[0 0 864 720],'menubar','none');
figure(f);