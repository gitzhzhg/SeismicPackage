function hbutt=raymigbutton(figseis,figmod,vel,dx,msg,clr)
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


if(isnumeric(figseis))
    action='init';
else
    action=figseis;
end
if(strcmp(action,'init'))
    if(nargin<5)
        msg='';
    end
    if(nargin<6)
        clr='r';
    end
    %see if there is already a button
    hkids=allchild(figseis);
    startpos=.02;
    for k=1:length(hkids)
        tag=get(hkids(k),'tag');
        if(strcmp(tag,'raymigbutton')||strcmp(tag,'clearraysbutton')||...
                strcmp(tag,'clearpicksbutton')||strcmp(tag,'raymodbutton'))
            pos=get(hkids(k),'position');
            rightedge=pos(1)+pos(3);
            if(rightedge>startpos)
                startpos=rightedge+.01;
            end
        end
    end
    if(isempty(msg))
        label=['Migrate fig' int2str(figseis) ' -> fig' int2str(figmod)];
        wpixels=120;%desired width in pixels
        %determine window size
        pos=get(gcf,'position');%should be in pixels
        width=wpixels/pos(3);%width in normaliized units
    else
        label=['Migrate fig' int2str(figseis) ' -> fig' int2str(figmod) ' ' msg];
        wpixels=150;%desired width in pixels
        %determine window size
        pos=get(gcf,'position');%should be in pixels
        width=wpixels/pos(3);%width in normaliized units
    end
    hbutt=uicontrol(figseis,'style','pushbutton','string',label,'units','normalized',...
        'position',[startpos .92 width .05],'callback','raymigbutton(''migrate'')',...
        'userdata',{vel;dx;figseis;figmod;clr},'tag','raymigbutton');
elseif(strcmp(action,'migrate'))
    ud=get(gcbo,'userdata');
    rayvelmod(ud{1},ud{2})
    eventraymig(ud{3},ud{4},nan,ud{5});
end
    