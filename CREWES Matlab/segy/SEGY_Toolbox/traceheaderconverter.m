function headerobj=traceheaderconverter(headerobj)
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

tracker()
obj=headerobj;
%try
    %  test to see if headerobj is a traceheader or a binaryheader
    
    if isa(obj,'TraceHeader') || isa(obj,'BinaryHeader')
    else
        me=MException('HEADERCONVERTER:InvalidInputType',...
            'headerobj must be a TraceHeader or BinaryHeader object');
        throw(me)
    end
    key=obj.definitions.keys;
    if ~all(strcmp(key,'Name'))||~all(strcmp(key,'startByte'))||~all(strcmp(key,'endByte'))||~all(strcmp(key,'Type'))
    me=MException('HEADERCONVERTER:ImproperObject',...
            'headerobj must be a TraceHeader or BinaryHeader object');
        throw(me)
    end
    indname=strcmp(obj.definitions.keys,'Name');
inddesc=strcmp(obj.definitions.keys,'Description');
indstart=strcmp(obj.definitions.keys,'startByte');
indend=strcmp(obj.definitions.keys,'endByte');
indtype=strcmp(obj.definitions.keys,'Type');

dat=cell(length(tracehead.definitions.values(:,indname)),6);
for k=1:length(tracehead.definitions.values(:,indname))
    dat{k,1}='false';
    dat{k,2}=tracehead.definitions.values{k,indname};
    dat{k,3}=tracehead.definitions.values{k,indstart};
    dat{k,4}=tracehead.definitions.values{k,indend};
    dat{k,5}=tracehead.definitions.values{k,indtype};
    dat{k,6}=tracehead.definitions.values{k,inddesc};
end

    h.fig=figure('numbertitle','off','menubar','none','units','normalized',...
        'name','Header Format Converter','position',[.05 .1 .9 .8]);
    h.file=uimenu(h.fig,'label','File');
    h.load=uimenu(h.file,'label','Load Definitions','callback',@loaddef);
    h.save=uimenu(h.file,'label','Save Definitions','callback',@savedef);
    h.table=uitable(h.fig,'units','normalized','position',[.05 .05 .5 .9],...
        'CellSelectionCallback',@tablecall);
    h.plotbutton=uicontrol('style','pushbutton','units','normalized',...
        'position',[.8 .85 .15 .05]);
    h.plotaxes=subplot('units','normalized','position',[.65 .25 .3 .5]);
    
    set(h.table,'data',dat,'columnwidth',{500,100,100,100,100,500},'rowname',{},...
        'columnname',{'Plot?','Name','startByte','endByte','Type','Description'},...
        'columnformat',{'logical','text','numerical','numerical','text','text'},...
        'columnEditable',[true true true true true true]);
    guidata(h.fig,h);
% catch me
%     errordlg(me.message, me.identifier);
% end



    function loaddef(hObject,~)
        h=guidata(hObject);
        guidata(hObject,h);
    end

    function savedef(hObject,~)
        h=guidata(hObject);
        guidata(hObject,h);
    end

    function tablecall(hObject,eventdata)
        h=guidata(hObject);
        eventdata
        guidata(hObject,h);
        
    end

end