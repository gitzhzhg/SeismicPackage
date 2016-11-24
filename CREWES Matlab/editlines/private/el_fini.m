function el_fini

% clean up graphics if needed
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
				elseif( strcmp(get(h(k),'string'),'yrag_params') )
					hparams=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_anchors') )
					hanchors=h(k);
					found=found+1;
				elseif( strcmp(get(h(k),'string'),'yrag_undo') )
                                        hundo=h(k);
                                        found=found+1;
                elseif( strcmp(get(h(k),'string'),'yrag_hors') )
                						hhors=h(k);
                						found=found+1;
				end
				if( found== 5)
					break;
				end
			end
		end
	end

	% delete any locate information that may be on
		newstuff=get(gca,'userdata');
%         stuff=newstuff{1};
        if(~isempty(newstuff))
            htext=newstuff{2};

            if(isgraphics(htext))
                delete(htext);
                newstuff{2}=[];
                set(gca,'userdata',newstuff);
            end
        end

	dat=get(hstor,'userdata');

	% get the anchor info 
	anchors=get(hanchors,'userdata');

	% reset and exit
	if( ~isempty(dat) )
		if(isgraphics(dat{5})); delete(dat{5}); end
        if(isgraphics(dat{6})); delete(dat{6}); end
		if(dat{2}==1)
			ls='-';
		elseif(dat{2}==2)
			ls='--';
		elseif(dat{2}==3)
			ls=':';
		elseif(dat{2}==4)
			ls='-.';
		elseif(dat{2}==5)
			ls='o';
		elseif(dat{2}==6)
			ls='+';
		elseif(dat{2}==7)
			ls='.';
		elseif(dat{2}==8)
			ls='*';
		elseif(dat{2}==9)
			ls='x';
		end
		
		if (dat{2} < 5)
		set(dat{1},'linestyle',ls,'linewidth',dat{3},...
			'color',dat{4});
		elseif (dat{2} >= 5)
		set(dat{1},'marker',ls,'linewidth',dat{3},...
			'color',dat{4});
		end
		
		set(hstor,'userdata',[]);
		% save the anchor information
		ind=el_cellfind( anchors,dat{1} );
		line_anchors=dat{7};
		nanchors=size(line_anchors,1);
		anchors{ind,2}=nanchors;
        anchors{ind,3}=line_anchors;
		%if( ~iscell(anchors) ); anchors=[]; end %this might be rediculous
	end


	% put the anchor info in gca
	set(gca,'userdata',anchors);
	delete(hstor);
	delete(hanchors);
	delete(hparams);
	delete(hundo);
	delete(hhors);
	
	% make sure pointer is an arrow
	set(gcf,'pointer','arrow');