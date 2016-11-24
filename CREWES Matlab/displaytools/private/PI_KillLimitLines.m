function PI_KillLimitLines(arg1)
% Killing limit lines
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

if(nargin<=0)
else
    hfig=arg1;
    h=get(hfig,'userdata');
    hi=h{5};
    hlimbox=h{14};
    set(hlimbox,'userdata',[],'value',1);
    set(findobj(hfig,'type','uimenu','tag','LIMITBOXMENU'),'label','Limit Box: ON');
    for ii=1:size(PICKS,1)
        if(PICKS{ii,1}==hfig)
            PICKS{ii,2}=[];
            PICKS{ii,3}=[];
            break
        end
    end
    mfig=findobj(0,'type','figure','tag','MVLINESMEASUREMENTS');

    for ii=1:length(mfig)
        checkfig=get(mfig(ii),'userdata');
        if(checkfig==hfig)
            delete(checkfig);
            break
        end
    end
    delete(findobj(hfig,'tag','LIMITLINE'));
    delete(findobj(hfig,'tag','LIMITPOINT'));
    delete(findobj(hfig,'tag','PICKTEXT'));
    delete(findobj(hfig,'tag','PICKMARKER'));
    delete(get(hi,'uicontextmenu'));
end