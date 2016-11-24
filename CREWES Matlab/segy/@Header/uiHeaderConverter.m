function headerobj=uiHeaderConverter(headerobj)
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

obj=headerobj;
try
    %  test to see if headerobj is a traceheader or a binaryheader
    
    if isa(obj,'TraceHeader') || isa(obj,'BinaryHeader')
    else
        me=MException('HEADERCONVERTER:InvalidInputType',...
            'headerobj must be a TraceHeader or BinaryHeader object');
        throw(me)
    end
    
    %gui handles
    h.fig=figure('numbertitle','off','menubar','none','units','normalized',...
        'name','Header Format Converter','position',[.05 .1 .9 .8],...
        'closerequestfcn',@exitconverter);
    h.file=uimenu(h.fig,'label','File');
    h.load=uimenu(h.file,'label','Load Definitions','callback',@loaddef);
    h.save=uimenu(h.file,'label','Save Definitions','callback',@savedef);
    h.exit=uimenu(h.file,'label','Exit','callback',@exitconverter,'separator','on');
    h.table=uitable(h.fig,'units','normalized','position',[.05 .15 .5 .8],...
        'CellEditCallback',@tablecall,'fontsize',10);
    h.addline=uicontrol('style','pushbutton','units','normalized',...
        'position',[.05 .05 .13 .05],'callback',@addline,'string','Add Line',...
        'tooltip','Lines will be added after checked plot fields');
    h.unselline=uicontrol('style','pushbutton','units','normalized',...
        'position',[.23 .05 .14 .05],'callback',@unselline,'string','Unselect All',...
        'tooltip','Use before deleteing lines so unwanted lines are not deleted');
    h.delline=uicontrol('style','pushbutton','units','normalized',...
        'position',[.42 .05 .13 .05],'callback',@delline,'string','Delete Line',...
        'tooltip','Lines that have the plot field checked will be deleted');
    h.recalcbutton=uicontrol('style','pushbutton','units','normalized',...
        'position',[.6 .85 .15 .05],'callback',@standardizehdr,...
        'string','Standardize Header');
    h.plotbutton=uicontrol('style','pushbutton','units','normalized',...
        'position',[.8 .85 .15 .05],'callback',@plotselected,...
        'string','Plot Selected');
    h.plotaxes=subplot('position',[.60 .15 .35 .6]);
    
    
    % variables
    h.tempfilename=which('headerconverter.m');
    h.tempfilename=[ h.tempfilename(1:end-17),'temp.csv'];
    h.obj=obj;
    
    if isa(obj,'TraceHeader')
        rev1def=HeaderDefinitions('segyrev1_trc.csv');
        namepos=strcmpi(rev1def.keys(),'Name');
        h.stdwords=rev1def.values(:,namepos)';
    end
    if isa(obj,'BinaryHeader')
        rev1def=HeaderDefinitions('segyrev1_file.csv');
        namepos=strcmpi(rev1def.keys(),'Name');
        h.stdwords=rev1def.values(:,namepos)';
    end
    guidata(h.fig,h);
    dat=createtabledata(obj);
    set(h.table,'data',dat,'columnwidth',{50,75,100,75,75,75,75,500},'rowname',{},...
        'columnname',{'Plot?','Color','Name','startByte','endByte','Type','SEGY Rev.1 Equivalent','Description'},...
        'columnformat',{'logical',...
        {'Black','Blue','Cyan','Green','Red','Magenta','Random'},...
        'char','numeric','numeric',...
        {'uint8','int8','uint16','int16','uint32','int32','char','uchar',...
        'float-ieee.32','float-ibm.32'},h.stdwords,'char'},...
        'columnEditable',[true true true true true true true true]);
    %set(h.plotaxes,'color','k');
    guidata(h.fig,h);
    uiwait(h.fig);
    h=guidata(h.fig);
    headerobj=h.obj;
    delete(h.fig);
catch me
    errordlg(me.message, me.identifier);
end

    function loaddef(hObject,~)
        h=guidata(hObject);
        [FileName,PathName] = uigetfile('.csv');
        fullname=[PathName,FileName];
        if isnumeric(fullname)
            msgbox('Load Definitions was Aborted');
            return
        end
        h.obj.definitions=HeaderDefinitions(fullname);
        dat=createtabledata(h.obj);
        set(h.table,'data',dat);
        guidata(hObject,h);
    end

    function savedef(hObject,~)
        h=guidata(hObject);
        [FileName,PathName] = uiputfile('.csv');
        fullname=[PathName,FileName];
        if isnumeric(fullname)
            msgbox('Save Definitions was Aborted');
            return
        end
        tabledat=get(h.table,'data');
        writetocsv(tabledat,fullname)
        msgbox([FileName,' has been Saved']);
        guidata(hObject,h);
    end

    function exitconverter(hObject,~)
        h=guidata(hObject);
        uiresume(h.fig)
    end

%     function recalcheader(hObject,~)
%         h=guidata(hObject);
%         tabledat=get(h.table,'data');
%         writetocsv(tabledat,h.tempfilename);
%         h.obj.definitions=HeaderDefinitions(h.tempfilename);
%         h.obj=convertHeader(h.obj);
%         guidata(hObject,h);
%     end

    function delline(hObject, ~)
        h=guidata(hObject);
        dat=get(h.table,'data');
        % find the checked plot variables
        sz=size(dat);
        inddel=false(1,sz(1));
        for p=1:sz(1)
            inddel(1,p)=dat{p,1}==true;
        end
        inddel=find(inddel==true);
        
        if isempty(inddel);
            warndlg('At Least One Line Must Be Selected For Deletion');
        end
        
        newdat=cell((sz(1)-length(inddel)),sz(2));
        inddel=[0,inddel];
        % remove the lines from the table
        for p=1:length(inddel)-1;
            r1=inddel(p)+1:inddel(p+1)-1;
            r2=r1-p+1;
            newdat(r2,:)=dat(r1,:);
        end
        r1=inddel(end)+1:sz(1);
        r2=r1-length(inddel)+1;
        newdat(r2,:)=dat(r1,:);
        % set the table data with the new data
        set(h.table,'data',newdat);
        guidata(hObject,h);
    end

    function addline(hObject, ~)
        h=guidata(hObject);
        dat=get(h.table,'data');
        % find the checked plot variables
        sz=size(dat);
        inddel=false(1,sz(1));
        for p=1:sz(1)
            inddel(1,p)=dat{p,1}==true;
        end
        inddel=find(inddel==true);
        
        % if no lines were selected add line at bottom
        if isempty(inddel);
            inddel=sz(1);
        end
        
        newdat=cell((sz(1)+length(inddel)),sz(2));
        inddel=[0,inddel];
        % create new line
        additonline={false,'Black','','','','int32','',''};
        
        
        % add lines to the table
        for p=1:length(inddel)-1;
            r1=inddel(p)+1:inddel(p+1);
            r2=r1+p-1;
            newdat(r2,:)=dat(r1,:);
            newdat((inddel(p+1)+1),:)=additonline;
        end
        r1=inddel(end)+1:sz(1);
        r2=r1+length(inddel)-1;
        newdat(r2,:)=dat(r1,:);
        % set the table data with the new data
        set(h.table,'data',newdat);
        guidata(hObject,h);
    end

    function unselline(hObject,~)
        h=guidata(hObject);
        dat=get(h.table,'data');
        % find the checked plot variables
        sz=size(dat);
        for p=1:sz(1)
            dat{p,1}=false;
        end
        set(h.table,'data',dat);
        guidata(hObject,h);
    end

    function standardizehdr(hObject, ~)
        h=guidata(hObject);
        tabledat=get(h.table,'data');
        writetocsv(tabledat,h.tempfilename);
        h.obj.definitions=HeaderDefinitions(h.tempfilename);
        h.obj=standardizeheader(h.obj);
        dat=createtabledata(h.obj);
        set(h.table,'data',dat);
        guidata(hObject,h);
    end


    function plotselected(hObject,~)
        h=guidata(hObject);
        tabledat=get(h.table,'data');
        fieldnms=h.obj.definitions.values(:,strcmpi(h.obj.definitions.keys,'Name'));
        ind2plot=cell2mat(tabledat(:,1))==1;
        if all(~ind2plot)
            msgbox('At least one Field must be selected');
            return
        end
        cla;hold on;
        ind2plotfd=find(ind2plot);
        plotcolors=jet;
        for d=1:length(ind2plotfd)
            % get plot data
            n=ind2plotfd(d);
            y=double(getheadervalue(h.obj,fieldnms{n}))';
            x=(1:length(y))';
            % get plot color
            if strcmp(tabledat(n,2),'Random')
                col=plotcolors(randi(length(plotcolors)),:);
                plot(x,y,'color',col);
            else
                col=lower(tabledat{n,2});
                plot(x,y,col);
            end
        end
        %plot(plotdat);
        xlabel('Traces')
        legend(fieldnms(ind2plot),'location','southoutside','orientation','horizontal');
        hold off;
        guidata(hObject,h);
    end

    function tablecall(hObject,eventdata)
        h=guidata(hObject);
        tabledat=get(h.table,'data');
        if(eventdata.Indices(2)==6)
            if findstr(eventdata.NewData,'32')
                ln=3;
            elseif findstr(eventdata.NewData,'16')
                ln=1;
            elseif findstr(eventdata.NewData,'8')
                ln=0;
            elseif findstr(eventdata.NewData,'char')
                ln=0;
            elseif findstr(eventdata.NewData,'float')
                ln=3;
            end
            newend=str2double(tabledat{eventdata.Indices(1),4})+ln;
            tabledat{eventdata.Indices(1),5}=num2str(newend);
            
        elseif(eventdata.Indices(2)==5)
            ln=str2double(eventdata.NewData)-str2double(tabledat{eventdata.Indices(1),4});
            if ln==3
                typ='int32';
            elseif ln==1
                typ='int16';
            elseif ln==0
                typ='int8';
            end
            tabledat(eventdata.Indices(1),6)=typ;
            
        elseif(eventdata.Indices(2)==4)
            typ=tabledat{eventdata.Indices(1),6};
            if findstr(typ,'32')
                ln=3;
            elseif findstr(typ,'16')
                ln=1;
            elseif findstr(typ,'8')
                ln=0;
            elseif findstr(eventdata.NewData,'char')
                ln=0;
            elseif findstr(eventdata.NewData,'float')
                ln=3;
            end
            newend=str2double(eventdata.NewData)+ln;
            tabledat{eventdata.Indices(1),5}=num2str(newend);
        end
        set(h.table,'data',tabledat);
        guidata(hObject,h);
        
    end

    function writetocsv(tabledat,filename)
        sz=size(tabledat);
        fid=fopen(filename,'w');
        fseek(fid,0,'bof');
        fwrite(fid,['Name,startByte,endByte,Type,SEGY_Rev1_EQ,Description',char(13)],'char')
        for k=1:sz(1)
            for m=3:sz(2)
                fwrite(fid,[tabledat{k,m},','],'char');
            end
            fwrite(fid,char(13),'char');
        end
        fclose(fid);
    end

    function dat=createtabledata(obj)
        h=guidata(gcf);
        key=obj.definitions.keys;
        if all(~strcmp(key,'Name'))||all(~strcmp(key,'startByte'))||all(~strcmp(key,'endByte'))||all(~strcmp(key,'Type'))||all(~strcmp(key,'Description'))
            me=MException('HEADERCONVERTER:InsufficentFile',...
                [obj.definitions.filename,' must contain columns with the following headings:',...
                'Name , startByte , endByte , Type , Description ']);
            throw(me)
        end
        indname=strcmpi(obj.definitions.keys,'Name');
        inddesc=strcmpi(obj.definitions.keys,'Description');
        indstart=strcmpi(obj.definitions.keys,'startByte');
        indend=strcmpi(obj.definitions.keys,'endByte');
        indtype=strcmpi(obj.definitions.keys,'Type');
        indeq=strcmpi(obj.definitions.keys,'SEGY_Rev1_EQ');
        
        dat=cell(length(obj.definitions.values(:,indname)),7);
        for k=1:length(obj.definitions.values(:,indname))
            dat{k,1}=false;
            dat{k,2}='Black';
            dat{k,3}=obj.definitions.values{k,indname};
            dat{k,4}=obj.definitions.values{k,indstart};
            dat{k,5}=obj.definitions.values{k,indend};
            dat{k,6}=obj.definitions.values{k,indtype};
            if all(~indeq)
                dat{k,7}=h.stdwords{1,k};
            else
                dat{k,7}=obj.definitions.values{k,indeq};
            end
            dat{k,8}=obj.definitions.values{k,inddesc};
        end
        
    end
end