function el_undo

% the previous action is undone by restroing things to the state saved at the last 
% buttondown
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
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                                        hundo=h(k);
                                        found=found+1;
				end
				if( found== 2)
					break;
				end
			end
		end
	end

	% get the undo information
	undostuff=get(hundo,'userdata');
    if(isempty(undostuff))
        return;
    end
	if(length(undostuff)<5)
		otherstuff=[];
	else
		otherstuff=undostuff(5:8);
	end

	% get the line data
	npts=undostuff{1};
	x=undostuff{2};
	y=undostuff{3};
	ispoly=0;
	if( x(1)==x(npts) && y(1)==y(npts) );ispoly=1;end

	% get dat
% 	dat=undostuff(2*npts+2:length(undostuff));
    dat=undostuff{4};
	% get baddat
	baddat=get(hstor,'userdata');

	% dat is the user data of hstor and is:
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
% 	if(dat(8))
    line_anchors=dat{7};
    nanchors=size(line_anchors,1);
% 	end

	set(dat{1},'xdata',x,'ydata',y);

	if( ispoly )
		set(dat{5},'xdata',x(2:npts),'ydata',y(2:npts));
	else
		set(dat{5},'xdata',x,'ydata',y);
	end
	
	if( isgraphics(dat{6}) )
		%make sure dat(8) still exists
% 		hkids=get(gca,'children');
% 		ind=find(dat(8)==hkids);
% 		if(isempty(ind))
% 			hline3 = line(line_anchors(1:2:2*nanchors),line_anchors...
% 				(2:2:2*nanchors),'color',[1 0 0],'marker','o','markersize',12,...
% 			'linestyle','none','erasemode','xor');
% 			dat(8)=hline3;
% 		end
		set(dat{6},'xdata',line_anchors(1:2:2*nanchors),'ydata',...
			line_anchors(2:2:2*nanchors));
	elseif(isgraphics(baddat{6} ))
		delete(baddat{6});
	end

	set(hstor,'userdata',dat);

	% deal withthe otherstuff
	if(~isempty(otherstuff))
		hline=otherstuff{1};
% 		n2=otherstuff{2};
		x2=otherstuff{3};
		y2=otherstuff{4};
		set(hline,'xdata',x2,'ydata',y2);
	end