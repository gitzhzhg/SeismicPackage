function [type bytes endian]=uigettracetype(obj,numoftraces)
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


try
    type=[];
    
    if ~isa(obj,'Trace')
        me=MException('uigettracetype:InvalidInputType',...
            'obj must be a Trace object');
        throw(me)
    end
    
    if nargin<2
        numoftraces=5;
    end
    maxnumoftraces=length(obj.traceheader.traceoffsets)-1;
    if numoftraces>maxnumoftraces
        numoftraces=maxnumoftraces;
    end
    h.numoftraces=numoftraces;
    
    % Create a time vector
    dt=double(getheadervalue(obj.traceheader,'dt'));
    nsamp=double(getheadervalue(obj.traceheader,'ns'));
    h.time=0:dt(1)/1000:nsamp(1);
    h.timex=[];
    for lmnop=1:numoftraces;
    h.timex=[h.timex,h.time];
    end
    
    h.fig=figure('name','Select The Best looking Trace','units','normalized',...
        'position',[.05 .1 .9 .8],'numbertitle','off','menubar','none',...
        'closerequestfcn',@selecttype);
    h.buttonpan=uipanel(h.fig,'units','normalized','position',[0 0 1 1]);
    h.ieeelrad=uicontrol(h.buttonpan,'style','radio','units','normalized',...
        'position',[.05 .85 .03 .03],'callback',@radiocallback);
    h.ieeebrad=uicontrol(h.buttonpan,'style','radio','units','normalized',...
        'position',[.9 .85 .03 .03],'callback',@radiocallback);
    h.ibmlrad=uicontrol(h.buttonpan,'style','radio','units','normalized',...
        'position',[.05 .50 .03 .03],'callback',@radiocallback);
    h.ibmbrad=uicontrol(h.buttonpan,'style','radio','units','normalized',...
        'position',[.9 .50 .03 .03],'callback',@radiocallback);
    h.ieeelplot=subplot('position',[.15 .75 .3 .2]);
    h.ieeebplot=subplot('position',[.55 .75 .3 .2]);
    h.ibmlplot=subplot('position',[.15 .40 .3 .2]);
    h.ibmbplot=subplot('position',[.55 .40 .3 .2]);
    h.traceplot=subplot('position',[.1 .05 .7 .25]);
    h.selectformat=uicontrol(h.fig,'style','pushbutton','units','normalized',...
        'string','Select Format','position',[.85 .10 .10 .10],'callback',@selecttype);
    
    
    guidata(h.fig,h);
    plothistograms(h.fig);
    uiwait(h.fig);
    h=guidata(gcf);
    type=h.type{2};
    bytes=h.type{1};
    endian=h.type{3};
    
    delete(h.fig);
    
catch me
    error(me.message,me.identifier);
end

    function plothistograms(hObject, ~)
        h=guidata(hObject);
        tracenum=1;
        numofbars=100;
        
        set(h.fig,'currentaxes',h.ieeelplot);
        obj.traceheader.tracetype={4,'float32'};obj.traceheader.filefmt='L';obj.traceheader.machineformat='ieee-le';
        h.tracieeel=obj.getTraces(obj.traceheader,'tracenumber',1:h.numoftraces);
        tracein=reformattraces(h.tracieeel);
        hist(tracein,numofbars);histo=findobj(gca,'Type','patch');set(histo,'FaceColor','b');
        title('ieee system format, little endian byte ordering');
        h.numieeel=evaluatetraces(tracein,h.timex);
        if h.numieeel.nan || h.numieeel.inf
            set(h.ieeelrad,'enable','off');
            %delete(h.ieeelplot)
            title('The Standard Deviation was either NaN or Infinite');
        end
        
        set(h.fig,'currentaxes',h.ieeebplot);
        obj.traceheader.tracetype={4,'real*4'};obj.traceheader.filefmt='B';obj.traceheader.machineformat='ieee-be';
        h.tracieeeb=obj.getTraces(obj.traceheader,'tracenumber',1:h.numoftraces);
        tracein=reformattraces(h.tracieeeb);
        hist(tracein,numofbars);histo=findobj(gca,'Type','patch');set(histo,'FaceColor',[0 .75 0])
        title('ieee system format, big endian byte ordering');
        h.numieeeb=evaluatetraces(tracein,h.timex);
        if h.numieeeb.nan || h.numieeeb.inf
              set(h.ieeebrad,'enable','off');
            %delete(h.ieeebplot)
            title('The Standard Deviation was either NaN or Infinite');
        end
        
        set(h.fig,'currentaxes',h.ibmlplot);
        obj.traceheader.tracetype={4,'ibm'};obj.traceheader.filefmt='L';obj.traceheader.machineformat='ieee-le';
        h.tracibml=obj.getTraces(obj.traceheader,'tracenumber',1:h.numoftraces);
        tracein=reformattraces(h.tracibml);
        hist(tracein,numofbars);histo=findobj(gca,'Type','patch');set(histo,'FaceColor','r')
        title('ibm system format, little endian byte ordering');
        h.numibml=evaluatetraces(tracein,h.timex);
        if h.numibml.nan || h.numibml.inf
              set(h.ibmlrad,'enable','off');
            %delete(h.ibmlplot)
            title('The Standard Deviation was either NaN or Infinite');
        end
        
        set(h.fig,'currentaxes',h.ibmbplot);
        obj.traceheader.tracetype={4,'ibm'};obj.traceheader.filefmt='B';obj.traceheader.machineformat='ieee-be';
        h.tracibmb=obj.getTraces(obj.traceheader,'tracenumber',1:h.numoftraces);
        tracein=reformattraces(h.tracibmb);
        hist(tracein,numofbars);histo=findobj(gca,'Type','patch');set(histo,'FaceColor',[.25 .9 .9])
        title('ibm system format, big endian byte ordering');
        h.numibmb=evaluatetraces(tracein,h.timex);
        if h.numibmb.nan || h.numibmb.inf
            set(h.ibmbrad,'enable','off');
            %delete(h.ibmbplot)
            title('The Standard Deviation was either NaN or Infinite');
        end
        guidata(hObject,h);
        guesswhatsright(h.ibmbplot);
        
    end

    function plottrace(hObject, ~)
        h=guidata(hObject);
        tracenum=1;
        set(h.fig,'currentaxes',h.traceplot);
        if strcmp(h.type{2},'ieee') && strcmp(h.type{3},'L')
            plot(h.tracieeel(:,tracenum),'b');
        elseif strcmp(h.type{2},'ieee') && strcmp(h.type{3},'B')
            plot(h.tracieeeb(:,tracenum),'color',[0 .75 0]);
        elseif strcmp(h.type{2},'ibm') && strcmp(h.type{3},'L')
            plot(h.tracibml(:,tracenum),'r');
        elseif strcmp(h.type{2},'ibm') && strcmp(h.type{3},'B')
            plot(h.tracibmb(:,tracenum),'color',[.25 .9 .9]);
        end
        guidata(hObject,h);
    end


    function radiocallback(hObject, ~)
        h=guidata(hObject);
        set(h.ieeelrad,'value',0);
        set(h.ieeebrad,'value',0);
        set(h.ibmlrad,'value',0);
        set(h.ibmbrad,'value',0);
        
        set(hObject,'value',1);
        
        if(hObject==h.ieeelrad)
            h.type={4,'ieee','L'};
        elseif(hObject==h.ieeebrad)
            h.type={4,'ieee','B'};
        elseif(hObject==h.ibmlrad)
            h.type={4,'ibm','L'};
        elseif(hObject==h.ibmbrad)
            h.type={4,'ibm','B'};
        end
        guidata(hObject,h);
        plottrace(hObject);
    end

    function selecttype(hObject, ~)
        h=guidata(hObject);
        type=h.type{2};
        h.type=type;
        uiresume;
    end

    function traceout=reformattraces(tracein)
        tracein=tracein.tracedata.data;
        sz=size(tracein);
        traceout=ones(1,sz(1)*sz(2));
        k1=1;
        k2=sz(1);
        for k=1:sz(2)
            traceout(1,k1:k2)=tracein(:,k)';
            k1=k1+sz(1);
            k2=k2+sz(1);
        end
        traceout=traceout(~isnan(traceout));
    end

    function guesswhatsright(hObject)
         h=guidata(hObject);
         rads=[h.ieeelrad, h.ieeebrad, h.ibmlrad, h.ibmbrad];
         totals=[h.numieeel.total, h.numieeeb.total, h.numibml.total, h.numibmb.total];
         radio=rads(totals==max(totals));radio=radio(end);
         guidata(hObject,h);
         radiocallback(radio,radio)
         
    end
        


end