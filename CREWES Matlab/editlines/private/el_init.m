function el_init
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


	anchors=get(gca,'userdata');
    %NOTE: Revision Jan 2015 was caused by the Matlab graphics engine
    %update which elevated graphics handles from doubles to objects.
    %Formerly the anchors vector had the format [h1 na1 xa11 ya11 xa12 ya12
    %... h2 na2 ...] where the h1, h2 etc are the handles of lines
    %available to be edited, the na1, na2 etc are the numbers of anchors on
    %each line, and the xa11,ya11,xa12,ya12 are the x and y coordinates for
    %each anchor for each line. Additionally the graphics handles could be
    %signed with a negative indicating the line was available for immediate
    %editing. This worked when grapics handles were real numbers and could
    %be stored in an ordinary vector. With the graphics update the anchors
    %vector format was modified to be a 2D cell array with the structure {h1
    %na1 pts1; h2 na2 pts2; ...} where the h1, h2 etc are now handle objects,
    %the na1, na2 are integers giving the number of anchors, and the pts1,
    %pts2 are are na1-by-2 and na2-by-2 matrices giving the x and y
    %coordinates of each anchor for each line (x in column1 y in column 2).
    %The negative sign that was formerly attached to the graphics handle to
    %indicate editing availability is now attached to the number of
    %anchors. This presents a special problem when the number of anchors is
    %0 so the convention is adopted that the actual anchor number is
    %abs(round(na)). With this convention, when the number of anchors is
    %zero and the line is ready for editing, then na=-eps is the
    %proper setting for the anchor number (eps is the built in Matlab
    %constant for the machine precision, usually about 10^(-16). In this
    %new format, the anchors array for two lines with no anchors would be
    %anchors={hline1, 0, [];hline2, 0, []}.  Then the number of lines is
    %size(anchors,1), and
    %anchors(:,1) ... cell array of line handles
    %abs(round(anchors(:,2))) ... cell array of anchor numbers for each line
    %anchors(:,3) ... cell array of anchor points for each line
    if(isempty(anchors)) % search the figure for the handles of all lines
        hkids=get(gca,'children');
        ilines=zeros(size(hkids));
        for k=1:length(hkids)
            if( strcmp(get(hkids(k),'type'),'line') )
                %                 anchors = {anchors hkids(k) 0};
                ilines(k)=1;
            end
        end
        nlines=sum(ilines);
        anchors=cell(nlines,3);
        ind=find(ilines==1);
        for k=1:nlines
            anchors{k,1}=hkids(ind(k));
            anchors{k,2}=0;
            anchors{k,3}=[];
        end
    end
    %search the anchors array for 0 as the number of anchors and
    %change these to eps, also check for valid structure
    nlines=size(anchors,1);
    for k=1:nlines
        if(~isempty(anchors{k,1}))%we allow an empty entry in the anchors array
            if(~isgraphics(anchors{k,1}))
                error(['Invalid graphics handle for entry ' int2str(k) ' in anchors array'])
            end
            if(anchors{k,2}==0)
                anchors{k,2}=eps;
            end
            if(size(anchors{k,3},1)~=abs(round(anchors{k,2})))
                error(['Invalid anchor pts array for entry ' int2str(k) ' inanchors array'])
            end
        end
    end
    
	set(gca,'userdata',[]);
	% make an invisible storage bucket 
	uicontrol('style','text','visible','off','string','yrag');
	hparams=uicontrol('style','text','visible','off','string','yrag_params');
	uicontrol('style','text','visible','off','string','yrag_undo');
	hhor=uicontrol('style','text','visible','off','string','yrag_hors');
	% parameters
	% pdat(1) == xonly ... if 1 then y is not changed
	% pdat(2) == yonly ... if 1 then x is not changed
	% pdat(3) == linkmode ... if 1 thenwe are in link mode
	% pdat(4) == locate ... if 1 then we write the cursor location out
	% pdat(5) == dragmode ... if 0 then group drag is elastic, else it is constant
	% pdat(6) == smoothmode ... if 1 then we are in smooth mode
	% pdat(7) == fastopt ... if 1, then dragging a single point is done with a
	% 				less annoying display which, though less accurate, is much faster
	%				graphically
	% pdat(8) == multiple nans ... if 0 then we allow multiple nans, otherwise,
	%				we cull them
	% pdat(9) == nodelete ... if 0 then points can be delete if 1 they cannot
	%				defaults to 0
	% pdat(10) == noadd ... if 0 then points can be added, if 1 they cannot
	%				defaults to 0
	pdat=zeros(1,10);
	set(hparams,'userdata',pdat);
	
	% search for a negative line handle which indicates we should go ahead and select that
	% line for editing. Also in this loop, we create a vector of the handles alone
	% to facilitate quickly searching all lines in link mode
	% We also check to see that any handles found are children of the current
	% axes. If not, they are deleted.
    
    nlines=size(anchors,1);
    hkids=get(gca,'children');
    kstart=[];
    hors=cell(1,nlines);
    for k=1:nlines
        test=find(anchors{k,1}==hkids, 1);
        if(isempty(test))
            %ok, this line is not in the current axis
            anchors{k,1}=[];
            anchors{k,2}=[];
            anchors{k,3}=[];
        else
            %test for editing
            if(anchors{k,2}<0 && isempty(kstart) )
                kstart=k;%this is the first line to edit
            end
        end
        hors{k}=anchors{k,1};
    end

% 	h=anchors{1};%this is the handle of the first editable line
% 	nh=1;
% 	hstart=0;
% 	hors=[];
% 	hkids=get(gca,'children');
% 	while( h ~= 0 )
% 		test=find(h==hkids);
% 		if(isempty(test))
% 			%its bogus, toss it
% 			npts=anchors(nh+1);
% 			anchors(nh:nh+1+2*npts)=[];
% 			if(nh>length(anchors) ); h=0;
% 			else h=anchors(nh);
% 			end
% 		else
% 			hors=[hors abs(h)];
% 			if( h<0 )
% 				if(hstart==0); % only the first negative handle is honored
% 					hstart=abs(h);
% 				end
% 				anchors(nh)=abs(h);
% 			end
% 			npts=anchors(nh+1);
% 			nh=nh+2*npts+2;
% 			if( nh >length(anchors) );h=0;
% 			else h=anchors(nh);
% 			end
% 		end
% 	end
	% make another storage bucket for the anchors	
	uicontrol('style','text','visible','off','string','yrag_anchors',...
		'userdata',anchors);
		
	% put the horizon vector in its bucket
	set(hhor,'userdata',hors);

	%make sure pointer is an arrow
	set(gcf,'pointer','arrow');
		
	% start editing if requested
	if(~isempty(kstart))
		set(gcf,'currentobject',hors{kstart});
		editlines('buttondown');
	end