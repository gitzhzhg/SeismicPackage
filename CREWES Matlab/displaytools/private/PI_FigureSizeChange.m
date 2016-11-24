function PI_FigureSizeChange()
% changes the look of the plotimage figure for publishable purposes
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

% posax=findobj(gcf,'type','axes','tag','POSITIONAXES');
haxs=findobj(gcf,'type','axes','tag','MAINAXES');
h=get(gcf,'userdata');
hmsg=h{2};
ttl=get(haxs,'title');
dat=get(ttl,'userdata');
if(isempty(dat))
    return
end
checkcolour=get(gcf,'color');
if(sum(checkcolour)~=3);
    dat{4}=get(gcf,'position');
    set(ttl,'userdata',dat);
    whitefig;
    bigfont(gcf,1.5);
    boldlines(gcf,2,2);
    bigfig;
    stringinfo='Publishable Plotimage';
    set(hmsg,'visible','off');
    set(gcbo,'label','Publishable Figure: ON');
else
    siz=dat{4};
    greyfig;
    bigfont(gcf,1/1.5,1);
    boldlines(gcf,.5,.5);
    set(gcf,'position',siz);
    stringinfo='Normal Plotimage';
    set(hmsg,'visible','on');
    set(gcbo,'label','Publishable Figure: OFF');
end
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);