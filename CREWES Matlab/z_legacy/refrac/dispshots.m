% Display of the arrival time curves sorted by shot
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

function dispshots(shrange,sh1,sh2,inc)
f=gcf;
fbtime=refdata('get','fbtime');
fbcoord=refdata('get','fbcoord');
nshots = refdata('get','nshots')
shotcoord = refdata('get','shotcoord');
% Arrival time curves for all incremental shot 
if (shrange==0)
   figcent(.6,.5)
   hold on;
   for n=1:inc:nshots
     plot(fbcoord(n,:),fbtime(n,:))
   end
   xy=axis;
   t=xy(4)-xy(3);
   d=t/40;
   for n=10:10:nshots         % Label every 10th shot
      str=sprintf('%d',n); 
      text(shotcoord(n),xy(3)+d,str)
   end
   text(xy(1)+100,xy(3)+2.5*d,'shot number')
   xlabel('Coordinate (m)');
   ylabel('Traveltime (ms)');
   title('Refracted arrivals');
   set(gcf,'units','pixels','menubar','none');
else
   % Arrival time curves for incremental shot in between two specified shot
   figcent(.6,.5);
   hold on;
   for n=sh1:inc:sh2
     plot(fbcoord(n,:),fbtime(n,:))
   end
   xy=axis;
   t=xy(4)-xy(3);
   d=t/10;
   for n=sh1:inc:sh2
     str=sprintf('%d',n); 
     text(shotcoord(n),xy(3)+d,str)
   end
   text(xy(1)+100,xy(3)+2*d,'shot number')
   xlabel('Coordinate (m)');
   ylabel('Traveltime (ms)');
   title('Refracted arrivals');
   set(gcf,'units','pixels','menubar','none');
end
figure(f);