function setit(h,xstuff,ystuff);
% backround outside plot [1,1,1]=white --> Then use black fonts
%                        [0,0,0]=black --> Then use white fonts
%                        [0.5,0.5,0.5] is some type of grey
% and so on.. 
%
% Author: Mostafa Naghizadeh; Copyright (C) 2010
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

set(gcf,'color',[1. ,1. ,1. ],...
	'inverthardcopy','off');

% Background inside the plot
%                        [0.8,0.8,0.8] is less grey 

set(gca,'color',[1.0 1.0 1.0],'xcolor','k','ycolor','k',...
	'fontsize', 16,...
	'fontname', 'Helvetica','FontWeight','bold',...
	'xaxislocation','bottom',...
	'yaxislocation','left',...
        'xdir','normal',...
        'ydir','normal',...
        'box','on');

% Line stuff

%set(h,'LineWidth',2,'color','b');
set(h,'LineWidth',2);


% You can also take at Uli's instructions in the web
% to see what this one do .. 

%set(gca,'position',[0.2,0.2,0.4,0.4]);

xlabel(xstuff);
ylabel(ystuff);