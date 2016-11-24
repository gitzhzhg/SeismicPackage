function traceheaderdump_g(arg)
% traceheaderdump_g: allows interactive browsing of the SEGY trace headers
%
% traceheaderdump(traces.traceheaders)
%
% Program opens a figure window with 3 axes in it. Associated with each
% axes is a popupmenu that contains all of the non-empty header fields.
% Selecting a header field causes its values to be graphed. This allows
% interactive browsing of the headers. Any 3 header fields can be displayed
% simultansously.
%
% Example: Let path and fname be strings that contain the path and file
% name ot your SEGY data. Then
% [traces1,dt1,texthead1,binaryhead1,extendedhead1]=readsegy([path fname1]); 
% traceheaderdump_g(traces1.traceheader)
% 
%
% by: G.F. Margrave, Devon Canada, 2016
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
if(~ischar(arg))
    action='init';
    traceheaders=arg;
else
    action=arg;
end

if(strcmp(action,'init'))
    figure
    [dump,allwords,inotempty]=traceheaderdump(traceheaders);
    words=allwords(inotempty);
    nwords=length(words);
    
    headervals=cell(size(words));
    for k=1:nwords
        headervals{k}=SEGY_getHeader(traceheaders,words{k});
    end
    
    x0=.1;y0=.1;
    xshift=.05;
    width=.75;
    height=.75/3;%divide into thirds
    ht=.05;wd=.05;
    htfactor=.8;
    ha1=subplot('position',[x0+xshift,y0+2*height,width,htfactor*height]);
    set(ha1,'tag','first');
    uicontrol(gcf,'style','popupmenu','string',words,'tag','firstchoice',...
        'units','normalized','position',[x0-wd,y0+2.5*height-ht,wd,ht],...
        'callback','traceheaderdump_g(''choice1'')','value',1,'userdata',words,'visible','on');
    
    ha2=subplot('position',[x0+xshift,y0+height,width,htfactor*height]);
    set(ha2,'tag','second');
    uicontrol(gcf,'style','popupmenu','string',words,'tag','secondchoice',...
        'units','normalized','position',[x0-wd,y0+1.5*height-ht,wd,ht],...
        'callback','traceheaderdump_g(''choice2'')','value',2,'userdata',headervals,'visible','on');
    
    ha3=subplot('position',[x0+xshift,y0,width,htfactor*height]);
    set(ha3,'tag','third');
    uicontrol(gcf,'style','popupmenu','string',words,'tag','thirdchoice',...
        'units','normalized','position',[x0-wd,y0+0.5*height-ht,wd,ht],...
        'callback','traceheaderdump_g(''choice3'')','value',3,'userdata',traceheaders,'visible','on');
    
    %sliders to control min(x) and max(x)
    uicontrol
    
    prepfiga
    traceheaderdump_g('choice1');
    traceheaderdump_g('choice2');
    traceheaderdump_g('choice3');
    
elseif(strcmp(action,'choice1')||strcmp(action,'choice2')||strcmp(action,'choice3'))
    hm1=findobj(gcf,'tag','firstchoice');
    hm2=findobj(gcf,'tag','secondchoice');
    hm3=findobj(gcf,'tag','thirdchoice');
    if(strcmp(action,'choice1'))
        hax=findobj(gcf,'tag','first');
        hmenu=hm1;
    elseif(strcmp(action,'choice2'))
        hax=findobj(gcf,'tag','second');
        hmenu=hm2;
    elseif(strcmp(action,'choice3'))
        hax=findobj(gcf,'tag','third');
        hmenu=hm3;
    end
    tag=get(hax,'tag');
    axes(hax);
    iword=get(hmenu,'value');
    words=get(hm1,'userdata');
    headervals=get(hm2,'userdata');
    
    word=words{iword};
    headerdata=headervals{iword};
    hdmin=min(headerdata);
    hdmax=max(headerdata);
    hdsep=median(diff(headerdata));
    x=1:length(headerdata);
    
    plot(x,headerdata)
    title([word ' minval=' num2str(hdmin) ', maxval=' num2str(hdmax) ', median sep=' num2str(hdsep)])
    
    set(hm1,'visible','on')
    set(hm2,'visible','on')
    set(hm3,'visible','on')
    set(hax,'tag',tag);  
end
  
  
  