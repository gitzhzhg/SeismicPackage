function drawline(action)
% DRAWLINE: draw a line on a figure window
%
% drawline(action,button)
%
% DRAWLINE is somewhat like SELBOX except that it draws a line rather
% than a rectangle. DRAWLINE uses the userdata of the current axes
% for storage and assumes that it initially contains [pt1x pt1y] the
% x and y coordinates of the starting point of the line. 
% So that DRAWLINE can be convieniently called several times in a row
% without outside intervention, DRAWLINE begins by deleting the graphics
% object whose handle is the fifth entry in the axes user data.
% An abort will occur if this user data entry is non-null with an
% invalid graphics handle.) After returning, the axes user data will
% contain the x,y coordinate of the 2 points defining the 
% rectangle and the graphics handle of the selection rectangle: 
% [p1x p1y p2x p2y handle]
%
% Several convienience functions make it easier to use DRAWLINE and
% you actually will rarely need to call DRAWLINE directly. DRAWLINEINIT
% should be called to begin the selection box process and DRAWLINEFINI
% to end it. You need do nothing else. DRAWLINEFINI, will return five
% values: [pt1x pt1y pt2x pt2y hline]
% Where pt1 and pt2 are the points defining the line and hline
% is the graphics handle to its on-screen image. (Note that 
% DRAWLINEFINI needs no arguments while DRAWLINEINIT has one optional
% argument described below)
% Example:
% [x,y,z]=peaks; % get something to plot
% pcolor(x,y,z); % plot it
% 
% drawlineinit % begin the line drawing
% % the line is drawn on screen
% vals=drawlinefini; % obtain the line coordinates
%
% Often you will want to do some action of your own upon a mouse button up
% event. This is easily done by passing a string argument to DRAWLINEINIT
% which contains any Matlab command (or series of commands).
% drawlineinit('do_my_thing')
%
% DRAWLINEINIT now takes a second argument which is the button number
% to be used in drawing the line. Drawing with any other button has
% no effect. Default is button 1.
%
% G.F. Margrave, CREWES, June 2000
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

if(strcmp(action,'init') )
    p1=get(gca,'currentpoint');
    dat=get(gca,'userdata');
    try
        delete(dat{2})
    catch
        %no line to delete
    end
    set(gca,'userdata',{[p1(1,1:2) p1(1,1:2)]});
    set(gcf,'windowbuttonmotionfcn','drawline(''motion'')');
    return;
end
if(strcmp(action,'fini'))
    set(gcf,'windowbuttonmotionfcn','');
    % % unnecessary??
    %test for a double  click
%     if(length(dat)==2)
%         pt=get(gca,'currentpoint');
%         test=dat(1)-pt(1,1)+dat(2)-pt(1,2);
%         if(~test)
%             set(gca,'userdata',[dat pt(1,1:2) -1]);
%         end
%     end
    return;
end
if( strcmp(action,'motion') )
    checkclick=get(gcf,'selectiontype');
    % will only draw lines on a MB1 click
    if(strcmp(checkclick,'normal'))
        % get the starting point from axes userdata
        dat=get(gca,'userdata');
        p1=dat{1}(1:2);
        % delete any pre-existing line
        try
            delete(dat{2})
        catch
            %no line to delete
        end

        % get the current point from the axes
        p2=get(gca,'currentpoint');
        p2=p2(1,1:2);
        % draw the line
        h=line([p1(1),p2(1)],[p1(2),p2(2)],[1,1]);
        % update the info in userdata

        set(gca,'userdata',{[p1 p2] h});
    end
    return;
end