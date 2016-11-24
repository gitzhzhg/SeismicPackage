function htops=plottops(tops,topnames,klr,labelflag,fs,lw)
% PLOTTOPS: plot tops on a log plot
%
% htops=plottops(tops,topnames,klr,labelflag,fs,lw)
%
% PLOTTOPS will plot horizontal lines across the current figure
% for each of a set of formation tops. The tops will be labeled
% and will extend for the length of the x axis. It is assumed
% that the log is plotted with the y coordinate being depth
% (or time).
% 
% tops ... vector of vertical coordinates (depth or time as 
%	appropriate for the figure) for the tops
% topnames ... string matrix containing the top names. One name
%	per row.
% klr ... color to plot with
% ************* default = 'k' (black) ************
% labelflag = -2 ... do not plot top labels
%           = -1 ... top labels will appear on the left
%           =  0 ... top labels will be on both left and right
%           =  1 ... top labels will be on the right
% ************* default = 1 ********* 
% fs = fontsize for tops labels
% ************* default =9 **************
% lw = linewidth for tops line
% ************* default =1 **************
% htops ... vector of handles of the tops and their text labels. htops will
% always be of length 3*ntops where ntops is the number of tops. Unused
% entries in htops will be nan. 
% The tops and their labels can be deleted by the command: delete(htops(~isnan(htops))).
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
if(nargin<3)
    klr='k';
end
if(nargin<4)
    labelflag=1;
end
if(nargin<5)
    fs=9;
end
if(nargin<6)
    lw=0.5;
end
if(~ischar(tops)) 
	action='plot';
else
	action=tops;
end
if(strcmp(action,'plot'))
	%determine the x limits
	xlim=get(gca,'xlim');
    ntops=length(tops);
	htops=nan*zeros(3*ntops,1);
	for k=1:ntops
        if( tops(k) > 10 )
            lbl=sprintf('%5.2f',tops(k));
        else
            lbl=sprintf('%2.5f',tops(k));
        end
        if(labelflag==1 || labelflag==0)
            htops(ntops+k)=text(min(xlim(2)),tops(k),deblank(topnames(k,:)),...
                'verticalalignment','baseline',...
                'horizontalalignment','center',...
                'interpret','no',...
                'color',klr,'fontsize',fs);
        end
        if(labelflag==-1 || labelflag==0)
            htops(2*ntops+k)=text(xlim(1),tops(k),topnames(k,:),...
                'verticalalignment','baseline',...
                'horizontalalignment','center',...
                'interpret','no',...
                'color',klr,'fontsize',fs);
        end
		htops(k)=line(xlim,[tops(k) tops(k)],...
			'color',klr,'linestyle','-.',...
			'userdata',['Top: ' topnames(k,:) ' value: ' lbl],...
			'buttondownfcn','plottops(''sayhey'')','linewidth',lw);
	end
	hmsg=uicontrol('style','text','units','normalized','position',[0 0 1 .05],...
		'string','Click MB3 on any top to see its name and value');
	set(gcf,'userdata',hmsg);
	htops(2*ntops+1)=hmsg;
	return;
end
if(strcmp(action,'sayhey'))
	hmsg=get(gcf,'userdata');
	flag= get(gcf,'selectiontype');
	if(~strcmp(flag,'alt'))
		set(hmsg,'string',...
			'Click MB3 on any top to see its name and value');
		return;
	end
	msg=get(gco,'userdata');
	set(hmsg,'string',msg);
	return;
end