function el_autoseg
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
	
	% get the axes userdata which has the information on the clicked point
	newstuff=get(gca,'userdata');
    stuff=newstuff{1};
    %stuff=newstuff{1}
    %htext=newstuff{2}
    %hstor=newstuff{3}
    %hline1=newstuff{4}
    %hline2=newstuff{5}
    %hline3=newstuff{6}
	
	% find the clicked point
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hline3=newstuff{6};
	if( hline3 )
		line_anchors=dat{7};
		nanchors=size(line_anchors,1);
	else
		nanchors=0;
		line_anchors=[];
	end

	%get the data from the current curve
	hline=dat{1};
	x=get(hline,'xdata');
	y=get(hline,'ydata');
	npts=length(x);
	ispoly=0;
	if(x(1)==x(npts) && y(1)==y(npts) )
		ispoly=1;
	end
	
	% get the clicked point
	xit=x(it);
	yit=y(it);
	
	% return if the point is the first or last
	if( it==1 || it==npts )
		return;
	end
	
	% make sure it is not already segmented
	if( isnan(x(it+1)) || isnan(x(it-1)) )
		return;
	end
	
	% duplicate the point and put a nan between
	
	x=[x(1:it-1) xit nan xit x(it+1:npts)];
	y=[y(1:it-1) yit nan yit y(it+1:npts)];
	
	% see if the point is already an anchor
		
	done=0;
	if(nanchors)
		ind=find(xit==line_anchors(:,1));
		if(~isempty(ind))
			if(yit==line_anchors(ind,2))
					done=1;
			end
		end
	end
	
	if(~done)
			nanchors=nanchors+1;
			line_anchors=[line_anchors; xit yit];
			% line_anchors get updated
			dat{7}=line_anchors;
			set(hstor,'userdata',dat);
	end

	% redisplay the line

	set(hline,'xdata',x,'ydata',y);% the current line

	if( ispoly )
			set(dat(7),'xdata',x(2:npts),'ydata',y(2:npts));
	else
			set(dat(7),'xdata',x,'ydata',y);
	end
	
	
	%fart with the anchors display
	if( nanchors )
		if( isgraphics(dat{6}))
            set(dat{6},'xdata',line_anchors(:,1),'ydata',line_anchors(:,2));
		else
			dat{6} = line(line_anchors(:,1),line_anchors(:,2),'color',...
					[1 0 0],'marker','o','markersize',12,'linestyle','none');
			set(hstor,'userdata',dat);
		end
	end