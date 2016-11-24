function PI_picklinemenu()
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

%global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKS PICKCOLOR XAXISTOP ZOOM_VALUE ZOOM_LOCKS
global PICKS

checkclick=get(gcf,'selectiontype');
hobj=gco;
if(strcmp(checkclick,'alt'))
    linenum=[];
    for ii=1:size(PICKS,1)
        CheckFigure=PICKS{ii};
        if(CheckFigure==gcf)
            CheckHandles=PICKS{ii,3};
            % fail safe
            if(isempty(CheckHandles))
                delete(hobj);
                return
            end
            for jj=1:size(CheckHandles)
                if(hobj==CheckHandles(jj))
                    linenum=jj;
                    break
                end
            end
        end
    end
    cmenu=uicontextmenu;
    set(gco,'uicontextmen',cmenu);
    % fail safe
    if(isempty(linenum))
        delete(hobj)
        return
    end
%     PickMarker=findobj(gcf,'type','line','tag','PICKMARKER');
%     m1=uimenu(cmenu,'label',['Move: ' num2str(linenum)],'callback','plotimage(''MovePickLine'')');
%     m1=uimenu(cmenu,'label','Delete','callback','plotimage(''DeletePickLine'')');
elseif(strcmp(checkclick,'normal'))
    checkaction=get(gcf,'windowbuttondownfcn');
    if(strcmp(checkaction,'plotimage(''PickMoveClose'')'))
        % If the gcf propertie is above, pick lines are in move mode
    end
end