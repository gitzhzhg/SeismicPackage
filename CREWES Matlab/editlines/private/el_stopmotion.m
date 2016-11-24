function el_stopmotion
% see if a significant motion occured. If it did not, call the appropriate
% button up function. This is done to decrease the sensitivity of the 
% program to accidental small mouse motions
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

	% find the clicked point
    smoothmode=0;   % This probably isn't used anymore.  We'll define it here to make the compiler shut up.
	newstuff=get(gca,'userdata');
    stuff=newstuff{1};
    %stuff=newstuff{1}
    %htext=newstuff{2}
    %hstor=newstuff{3}
    %hline1=newstuff{4}
    %hline2=newstuff{5}
    %hline3=newstuff{6}
    htext=newstuff{2};
	lit=stuff(1,1);
	it=stuff(1,2:lit+1);
	hstor=newstuff{3};
	xit=stuff(1,lit+3);
	yit=stuff(1,lit+4);
	hline=newstuff{4};
	hline2=newstuff{5};
% 	hline3=newstuff{6};
% 	if( hline3 )
% 		line_anchors=stuff(1,lit+8:length(stuff(1,:)));
% 		nanchors=length(line_anchors)/2;
% 	else
% 		nanchors=0;
% 		line_anchors=[];
% 	end
% 	xonly=stuff(2,1);
% 	yonly=stuff(2,2);
	killrd=stuff(2,3);
	ispoly=stuff(2,4);
% 	locate=stuff(2,5);
% 	dragmode=stuff(2,7);
	linkmode=stuff(2,8);
	fastopt=stuff(3,1);
	
	pt=get(gca,'currentpoint');
	xpt=pt(1,1);
	ypt=pt(1,2);		

	%delete any locate text that may be on
	if(isgraphics(htext))
		delete(htext)
        newstuff{2}=[];
		set(gca,'userdata',newstuff);
	end

	% get the button
	flag=get(gcf,'selectiontype');
	if( strcmp(flag,'normal') )
		d=sqrt( (xpt-xit)^2 + (ypt-yit)^2 );
		if( d>.1*killrd )
			set(gcf,'windowbuttonmotionfcn','');
			set(gcf,'windowbuttonupfcn','');

			% test for fastopt
			if(fastopt)
				xy=get(hline2,'userdata');
				set(hline,'xdata',xy(1,:),'ydata',xy(2,:));
				if( ispoly )
                    npts = length(get(hline,'xdata'));
					set(hline2,'xdata',xy(1,2:npts),'ydata',xy(2,2:npts));  
				else
					set(hline2,'xdata',xy(1,:),'ydata',xy(2,:));
				end
			end
			return;
		else
			set(gcf,'windowbuttonmotionfcn','');
			set(gcf,'windowbuttonupfcn','');

			editlines('undo');
			if(~linkmode && ~smoothmode )
				editlines('button1up');
			elseif( linkmode==-1)
				editlines('linkpoly');
			elseif( smoothmode )
				editlines('smooth');
			end
		end
	elseif( strcmp(flag,'alt') )
		d=sqrt( (xpt-xit)^2 + (ypt-yit)^2 );
		if( d>.1*killrd )
			set(gcf,'windowbuttonmotionfcn','');
			set(gcf,'windowbuttonupfcn','');

			% test for fastopt
			if(fastopt)
				xy=get(hline2,'userdata');
				set(hline,'xdata',xy(1,:),'ydata',xy(2,:));
				if( ispoly )
                    npts = length(get(hline,'xdata'));
                    set(hline2,'xdata',xy(1,2:npts),'ydata',xy(2,2:npts));
				else
					set(hline2,'xdata',xy(1,:),'ydata',xy(2,:));
				end
			end
			return;
		else

			editlines('undo');
			if(~linkmode && ~smoothmode )
				editlines('button3up');
			elseif( linkmode)
				editlines('autoseg');
			elseif( smoothmode )
				editlines('smooth3');
			end
		end
	end

	set(gcf,'windowbuttonmotionfcn','');
    