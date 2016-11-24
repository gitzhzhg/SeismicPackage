function PI_ClosePlotImage()
% Closing Plot image
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

%global PICKS PICKCOLOR ZOOM_VALUE ZOOM_LOCKS
global PICKS ZOOM_LOCKS

masterfigure=findobj(gcf,'type','uimenu','tag','PLOTIMAGEMASTERFIGURE');
mf=get(masterfigure,'parent');
mf=get(mf,'parent');
if(~isempty(mf))
    button = questdlg('Are you sure you want to quit?',...
        'Closing Plotimage','Yes','No','Yes');
    switch button
    case 'Yes'
    case 'No' 
        return
    end
end
if(size(PICKS,1)>=2)
    NewPICKS=cell(1,3);
    jj=1;
    for ii=1:size(PICKS,1)
        CheckFigure=PICKS{ii,1};
        if(~isempty(PICKS{ii,1}))
            if(CheckFigure==gcf)
            else
                NewPICKS{jj,1}=PICKS{ii,1};
                NewPICKS{jj,2}=PICKS{ii,2};
                NewPICKS{jj,3}=PICKS{ii,3};
                jj=jj+1;
            end
        end
    end
    PICKS=NewPICKS;
else
    PICKS=[];
end
if(~isempty(ZOOM_LOCKS))
    NewZOOM_LOCKS=[];
%     any(ZOOM_LOCKS==gcf)
    for ii=1:size(ZOOM_LOCKS,1)
        if(~isempty(find(ZOOM_LOCKS(ii,:)==gcf)))
        else
            NewZOOM_LOCKS=[NewZOOM_LOCKS,ZOOM_LOCKS(ii,:)];
        end
    end
    ZOOM_LOCKS=NewZOOM_LOCKS;
end

% if(~isempty(masterfigure))
%     % if it is masterfigure, deleteing the other plotimages spawned
%     delete(findobj(0,'type','figure','tag','PLOTIMAGEFIGURE'));
%     clear global;
%     return
% end
delete(gcf);
% if(isempty(findobj(0,'type','figure','tag','PLOTIMAGEFIGURE')));
%     clear global;
% end