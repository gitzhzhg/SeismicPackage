function PI_DeletePickLine()
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

h=get(gcf,'userdata');
hmsg=h{2};
global PICKS 
delete(findobj(gcf,'type','line','tag','PICKMARKER'));
delete(findobj(gcf,'type','text','tag','PICKTEXT'));
hobj=gco;
for ii=1:size(PICKS,1)
    CheckFigure=PICKS{ii,1};
    if(CheckFigure==gcf)
        PicksPositions=PICKS{ii,2};
        PicksHandles=PICKS{ii,3};
        for jj=1:length(PicksHandles)
            if(PicksHandles(jj)==hobj)
                PicksPositions(jj,:)=[];
                delete(hobj);
                PicksHandles(jj)=[];
                PICKS{ii,2}=PicksPositions;
                PICKS{ii,3}=PicksHandles;
                break
            end
        end
    end
end
stringinfo='Pick Line has been deleted';
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);