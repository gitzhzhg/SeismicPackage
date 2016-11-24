function PI_ImportPicks
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

% Import picks from other figures in familly
global PICKS PICKCOLOR 
h=get(gcf,'userdata');
hmsg=h{2};
% getting name of figure pulling
ttl=get(gcbo,'label');
xx=strfind(ttl,':');
ttl=ttl(xx(1)+2:end);
NewPicksData=get(gcbo,'userdata');
MasterFigure=gcf;
CheckPicks=findobj(gcf,'type','line','tag','PICKS');
if(~isempty(CheckPicks))
    CheckwUser=questdlg('Picks already present.',...
        'Picks Present Alert','Delete Present Picks','Merge Picks','Cancel','Cancel');
    switch CheckwUser
    case 'Delete Present Picks'
        for ii=1:size(PICKS,1)
            if(PICKS{ii,1}==MasterFigure)
                delete(findobj(gcf,'type','line','tag','PICKS'));
                PICKS{ii,2}=[];
                PICKS{ii,3}=[];
                break
            end
        end
    case 'Merge Picks'
    case 'Cancel'
        return
    end
end
xcheck=get(gca,'xlim');
ycheck=get(gca,'ylim');
for ii=1:size(PICKS,1)
    if(PICKS{ii,1}==MasterFigure)
        PicksData=PICKS{ii,2};
        PicksHandles=PICKS{ii,3};
        for jj=1:size(NewPicksData,1)
            xdatsrt=sort([NewPicksData(jj,1) NewPicksData(jj,3)]);
            ydatsrt=sort([NewPicksData(jj,2) NewPicksData(jj,4)]);
            if(xcheck(1)>=xdatsrt(1)||xcheck(2)<=xdatsrt(2)||ycheck(1)>=ydatsrt(1)||ycheck(2)<=ydatsrt(2))
            else
                hpick=line([NewPicksData(jj,1) NewPicksData(jj,3)],...
                    [NewPicksData(jj,2) NewPicksData(jj,4)],[1 1],'linewidth',2,...
                    'color',PICKCOLOR,'buttondownfcn','plotimage(''picklinemenu'')',...
                    'userdata','','tag','PICKS');
                PicksData=[PicksData; NewPicksData(jj,1) NewPicksData(jj,2) NewPicksData(jj,3) NewPicksData(jj,4)];
                PicksHandles=[PicksHandles;hpick];
            end
        end
        PICKS{ii,2}=PicksData;
        PICKS{ii,3}=PicksHandles;
        break
    end
end
xx=length(PICKS{ii,3});
delete(findobj(gcf,'type','line','tag','PICKMARKER'));
delete(findobj(gcf,'type','text','tag','PICKTEXT'));
stringinfo=[num2str(xx) ' Picks have been imported from "' ttl '"'];
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);