function PI_pick()
global PICKS PICKCOLOR 
%global XAXISTOP ZOOM_VALUE ZOOM_LOCKS
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

% hax=findobj(gcf,'type','axes','tag','MAINAXES');
clickcheck=get(gcf,'selectiontype');
h=get(gcf,'userdata');
hmsg=h{2};
hi=h{5};
xdat=get(hi,'xdata');
ydat=get(hi,'ydata');
hzoompick=h{9};
value=get(hzoompick,'value');
if(strcmp(clickcheck,'normal'))
%     h=get(gcf,'userdata');
%     hzoompick=h{9};
    if(value~=-1)%value will never be -1, this is a way of turning off Chris's 
                 %advanced picking code
        pick=drawlinefini;
    else
        % user has called auto picker, need to get picks info from handle
        % where picks have been stored
        picksout=picksle('EXPORT');
        % need to set data so this thingy can plot properly... thingy is
        % the technical term of course
        % must be [x1 y1 x2 y2] style
        if(size(picksout,1)<1)
            stringinfo='Automatic picking did not find any picks';
            set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
            % not enough points to make a line
            return
        end
        pick=[];
        for ii=1:size(picksout,1)-1
            pick01=[picksout(ii,2) picksout(ii,1) picksout(ii+1,2) picksout(ii+1,1)];
            pick=[pick;pick01];
        end
    end
    
    if length(pick) > 1
        try
            delete(pick{2})
        catch
            %no line to delete or delete failed
        end
    end
    pick = pick{1}; %strip graphics handle from pick
    
    %see if anything bad happened
    test=pick(1)-pick(3)+pick(4)-pick(2); %zero for a single click
    ck1=sort([pick(1)-xdat(1) xdat(end)-pick(1) pick(3)-xdat(1) xdat(end)-pick(3),...
        pick(2)-ydat(1) ydat(end)-pick(2) pick(4)-ydat(1) ydat(end)-pick(4)]);
    if(test==0)
        stringinfo='Please create Pick with greater then zero length';
        col=[1 1 0];
        set(hmsg,'string',stringinfo,'backgroundcolor',col);
        return
    elseif(ck1(1)<=0)
        stringinfo='Please keep Pick within the image area';
        col=[1 1 0];
        set(hmsg,'string',stringinfo,'backgroundcolor',col);
        return
    end
    
    for ii=1:size(pick,1)
        % first two are markes that will appear at the ends
        % of each pick line when moving is instigated
        hpick=line([pick(ii,1) pick(ii,3)],[pick(ii,2) pick(ii,4)],[1 1],'linewidth',2,...
            'color',PICKCOLOR,'buttondownfcn','plotimage(''picklinemenu'')',...
            'userdata','','tag','PICKS');
        if(isempty(PICKS))
            PICKS=cell(1,3);
            PICKS{1}=gcf;
            PICKS{2}=[pick(ii,1) pick(ii,2) pick(ii,3) pick(ii,4)];
            PICKS{3}=hpick;
        else
            checkpicks=[];
            for jj=1:size(PICKS,1)
                CheckMasterFigure=PICKS{jj,1};
                if(CheckMasterFigure==gcf)
                    PicksPositions=PICKS{jj,2};
                    PicksHandles=PICKS{jj,3};
                    PicksPositions=[PicksPositions;pick(ii,1) pick(ii,2) pick(ii,3) pick(ii,4)];
                    PicksHandles=[PicksHandles;hpick];
                    PICKS{jj,2}=PicksPositions;
                    PICKS{jj,3}=PicksHandles;
                    checkpicks='Pick Has Been Set';
                end
            end
            if(isempty(checkpicks))
                % for some reason, need to add picks.
                NewPICK={gcf [pick(ii,1) pick(ii,2) pick(ii,3) pick(ii,4)] hpick};
                PICKS(size(PICKS,1)+1,:)=NewPICK;
            end
        end
    end
    stringinfo='Pick added to PICK buffer, MB1: click and drag for more picks';
    col=[1 1 1];
    set(hmsg,'string',stringinfo,'backgroundcolor',col);
end