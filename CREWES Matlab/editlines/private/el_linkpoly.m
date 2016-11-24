function el_linkpoly

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

	% turn off the window button functions
	set(gcf,'windowbuttonupfcn','');
	set(gcf,'windowbuttonmotionfcn','');
	
	% Get the storage buckets
	h=get(gcf,'children');
	found=0;
	for k=1:length(h)
		if( strcmp(get(h(k),'type'),'uicontrol') )
			if( strcmp(get(h(k),'style'),'text') )
				if( strcmp(get(h(k),'string'),'yrag') )
					hstor=h(k);
					found=found+1;
% 				elseif( strcmp(get(h(k),'string'),'yrag_params') )
% 					hparams=h(k);
% 					found=found+1;
% 				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
% 					hanchors=h(k);
% 					found=found+1;
% 				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
%                 	hundo=h(k);
%                 	found=found+1;
%                 elseif( strcmp(get(h(k),'string'),'yrag_hors') )
%                 	hhors=h(k);
%                 	found=found+1;
				end
				if( found== 1)
					break;
				end
			end
		end
	end

	dat=get(hstor,'userdata');
	% the user data of hstor is:
    % PREVIOUS
	% dat(1) = handle of the selected line
	% dat(2) = original linestyle
	% dat(3) = original linewidth
	% dat(4:6) = original color
	% dat(7) = handle of duplicate line
	% dat(8) = handle of the anchor line
	% dat(9:length(dat)) = (x,y)'s of the anchors of the selected line
    % NEW
	% dat{1} = handle of the selected line
	% dat{2} = original linestyle
	% dat{3} = original linewidth
	% dat{4} = original color (formerly dat(4:6))
	% dat{5} = handle of duplicate line (formerly dat(7))
	% dat{6} = handle of the anchor line (formerly dat(8))
	% dat{7} = (x,y)'s of the anchors of the selected line (formerly dat(9:length(dat))
	
	% get the axes userdata which has the information on the last point
	% moved
	newstuff=get(gca,'userdata');
    stuff=newstuff{1};
    %stuff=newstuff{1}
    %htext=newstuff{2}
    %hstor=newstuff{3}
    %hline1=newstuff{4}
    %hline2=newstuff{5}
    %hline3=newstuff{6}
	
	% find the last moved point
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
% 	hline3=newstuff{6};%anchor line
% 	if( isgraphics(hline3) )
% 		line_anchors=dat{7};
% % 		nanchors=size(line_anchors,1);
% % 	else
% % 		nanchors=0;
% % 		line_anchors=[];
% 	end

	%get the data from the current curve
	hline1=dat{1};
	x1=get(hline1,'xdata');
	y1=get(hline1,'ydata');
	npts1=length(x1);
	ispoly=0;
	if(x1(1)==x1(npts1) && y1(1)==y1(npts1) )
		ispoly=1;
	end
	
	% get the last moved point
	% test it for validity
	if( it>length(x1) )
		error(' please move something on primary curve, then link');
	end
% 	xit=x1(it);
% 	yit=y1(it);

	if(~ispoly) % handle the polygon link
% 		npts1=npts1+1;
		x1=[x1 x1(1)];
		y1=[y1 y1(1)];
		set(dat{1},'xdata',x1,'ydata',y1);

	else % handle polygon breaking
		npts1=npts1-1;
		x1=x1(1:npts1);
		x1=[x1(it:npts1) x1(1:it-1)]; % break at 'it'
		y1=y1(1:npts1);
		y1=[y1(it:npts1) y1(1:it-1)]; % break at 'it'
		set(dat{1},'xdata',x1,'ydata',y1);
	end