function traceheaderviewer(tracehead)
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


try
if ~isa(tracehead,'TraceHeader')
    me=MExecption('traceheaderviewer:InvalidInputType',...
        'tracehead must be a TraceHeader Object');
    throw(me)
end

indshnm=strcmp(tracehead.definitions.keys,'Name');
inddesc=strcmp(tracehead.definitions.keys,'Description');

dat={};
for k=1:length(tracehead.definitions.values(:,indshnm))
    rowname{k,1}=tracehead.definitions.values{k,indshnm};
    dat{k,1}=tracehead.definitions.values{k,inddesc};
    dat{k,2}=num2str(unique(getfield(tracehead.header,tracehead.definitions.values{k,indshnm})));
end

h.fig=figure('menubar','none','numbertitle','off','name','Trace Header Viewer',...
    'units','normalized','position',[.1,.25,.8,.5]);
h.table=uitable(h.fig,'units','normalize','position',[.05,.15,.9,.8],...
    'columnname',{'Description','Possible Values'},...
    'columnwidth',{400, 500},'RowName',rowname,'data',dat,...
    'CellSelectionCallback',@seecellcontents );
h.ok=uicontrol('style','pushbutton','string','OK','units','normalized',...
    'Position',[.45,.04,.1,.07],'callback',@selectok);
guidata(h.fig,h);



catch me
    error(me.message,me.identifier);
end

    function seecellcontents(hObject,eventdata)
        h=guidata(hObject);
        dat=get(h.table,'data');
        str=dat{eventdata.Indices(1),eventdata.Indices(2)};
        msgbox(str);
    end

    function selectok(hObject, eventdata)
        h=guidata(hObject);
        delete(h.fig);
    end
        



end