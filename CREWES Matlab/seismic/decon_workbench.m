function decon_workbench(seis,t,x,sref,tref,xref,varargin)
% DECON_WORKBENCH: an iteractive tool for deconvolution and well tying
%
% decon_workbench(seis,t,x,sref,tref,xref)
% 
% seis ... a seismic trace gather to be worked on
% t ... time coordinate for seis.
% NOTE: length(t) must equal size(seis,1)
% x ... space coordinate for seis.
% NOTE: length(x) must equal size(seis,2)
% sref ... reference trace to conpare deconvolved traces to. Usually this
%       is a synthetic seismogram made at a well
% tref ... time coordinate for sref. This mignt not start at t=0 and it is
%       very important because it determines the time alignment of sref and seis.
% NOTE ... length(tref) must equal length(sref)
% xref ... x coordinate of sref. This should be the location at which sref
%       ties seis. This is a scalar.
%
% G.F. Margrave Devon Canada 2016
%
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
global DRAGLINE_MOTION DRAGLINE_YLIMS DRAGLINE_SHOWPOSN DRAGLINE_CALLBACK
global ALIGN_REF_RNEW ALIGN_REF_TRNEW

if(~ischar(seis))
    action='init';
else
    action=seis;
end
% Userdata assignments
% halgs, tag='algorithms', {hpanels, algs, algorithms}
% happly 'tag'='apply', {seis,t,x,sref,tref,xref,is1,sref_array,ir1,tref_array} 
%       is1 is current seismic target trace number for well tie
%       ir1 is current reflectivity
%       sref_array iscell array of possible reflectivities including the original and any from align_ref
%       tref_array is cell array reflectivity time coordinates including the original and any from align_ref
% hresults 'tag'='results', {niter,results} niter is 1x5 and counts the number
%       of times each algorithm has been run. This is used to generate unique
%       titles. results is a cell array of structures, one structure per
%       result.
%       The results structure:
%       thisresult.name ... name of the result
%       thisresult.alg ... algorithm used
%       thisresult.parms ... parameters for the algorithm
%       thisresult.filterparms ... filter parameters
%       thisresult.seis ... unfiltered seismic result
%       thisresult.seisf ... filtered seismic
%       thisresult.t ... time coordinate
%       thisresult.x ... x coordinate
%       thisresult.sref ... reference trace
%       thisresult.tref ... time coordinate for sref
%       thisresult.xref ... x coordinate for sref
%       thisresult.waveletmethod ... wavelet extraction method
%       thisresult.waveletparms ... wavelet extraction parameters
%       thisresult.sreftie ... reference trace after tying
%       thisresult.s1tie ... comparison trace after tying
%       thisresult.is1 ... comparison trace number (comparison trace is thisresult.seis(:,thisresult.is1))
%       thisresult.wletsb ... wavelets before tying
%       thisresult.twsb ... time coordinate for before wavelets
%       thisresult.wletsa ... wavelets after tying
%       thisresult.twsa ... time coordinate for after wavelets
%       thisresult.t0s ... estimation times of the wavelets
%       thisresult.twinw ... window size for wavelet estimation
%       thisresult.tvphase ... timevariant phase
%       thisresult.tvdelay ... time variant delay
%       thisresult.twin ... window size used in time-variant phase and delay
%       thisresult.tinc ... window increment used in time-variant phase and delay
%       
%
% Convenience functions
%   thisresult=getcurrentresult; Get the current result
%   savecurrentresult(thisresult); save the current result
%   parmlist=getparameters(alg,hpanel,t); Get the parameters for algorithm alg (also checks validity)
%   thisparm=findparm(parm,parmlist); find a particular parater by name from a parmlist
%   fparms=getfilterparms(t); Get the filter parms (also checks validity)
%   [t1,t2,t3,t4]=getspectralwindows; Get the spectral window boundaries
%   [td1,td2]=getdesigngate; Get the decon design gate
%   [tcc1,tcc2]=getcorrelationgate; Get the cross correlation gate
%   wmethod=getwaveletmethod; Get the current wavelet estimation method ('simple', 'match', 'roywhite')
%   wparms=getwaveletparms(t); Get the wavelet estimation parms (also checks validity)
%
%
%
if(strcmp(action,'init'))
   
    
   if(nargin<4)
       sref=[];tref=[];xref=[];
   end
   %process extra arguments
   %'prestack' 0 or 1
   %'vmin' min velocity
   %'vmax' max velocity
   prestack=0;
   vmin=[];
   vmax=[];
   name=[];
   nargs=length(varargin);
   if(2*floor(nargs/2)~=nargs)
       error('extra arguments must be name-value pairs');
   end
   for k=1:2:nargs
       name=varargin{k};
       if(~ischar(name))
           error(['expecting extra argument ' int2str(k) ' to be a string']);
       end
       switch name
           case 'prestack'
               prestack=varargin{k+1};
               if(prestack~=0 && prestack ~=1)
                   error('prestack must be 0 or 1');
               end
           case 'vmin'
               vmin=varargin{k+1};
               if(ischar(vmin) || length(vmin)>1)
                   error('vmin must be a numberic scalar');
               end
           case 'vmax'
               vmax=varargin{k+1};
               if(ischar(vmax) || length(vmax)>1)
                   error('vmax must be a numeric scalar');
               end
           case 'title'
               name=varargin{k+1};
               if(~ischar(name))
                   error('title must be a character string');
               end
           otherwise
               error(['unrecognized parameter name ' name])
       end
   end
   if(prestack && (isempty(vmin) || isempty(vmax)))
       error('vmin and vmax must be specified for prestack');
   end
   
   scrsize=get(0,'screensize');
   scrwid=scrsize(3);
   scrht=scrsize(4);
   figwid=.85*scrwid;
   fight=.7*scrht;
   figure('position',[.1*scrwid,.2*scrht,figwid,fight]);
   set(gcf,'name','Decon Workbench');
   if(~isempty(name))
      xnow=.05;ynow=.92;wid=.3;ht=.05;
      fs=16;fontwt='bold';
      uicontrol(gcf,'style','text','string',name,'units','normalized','tag','title',...
          'position',[xnow,ynow,wid,ht],'fontsize',fs,'fontweight',fontwt,...
          'horizontalalignment','left');
   end
   xnot=.05;ynot=.1;
   %input time axes
   axwid=.25;
   ax2wid=.2/3;%width of phase, delay and welltie axes, wavelets axes is 3*axwid
   axht=.3;
   seisfactor=1.4;
   panwid=.13;
   panht=.4;
   xnow=xnot;
   bigsep=.05;
   medsep=.02;
   smsep=.01;
   cht=.05;
   ynow=ynot+axht+2*bigsep;
   hseisin=axes('position',[xnow,ynow,axwid,seisfactor*axht],'tag','seisin');
   %make a context menu for the seismic axes
   hcntx=uicontextmenu;
   uimenu(hcntx,'label','Alignment tool','callback','decon_workbench(''alignment'')');
   hr=uimenu(hcntx,'label','Ref. to use','tag','refs');
   uimenu(hr,'label','1 (original)','callback','decon_workbench(''switchref'')','userdata',1,'checked','on');
   set(hseisin,'uicontextmenu',hcntx);
   if(prestack)
       %upper velocity slider
       v=vmin+.25*(vmax-vmin);
       labelledslider('parent',gcf,'minval',vmin,'maxval',vmax,'val',v,...
           'position',[xnow,ynow+seisfactor*axht+.4*cht,axwid*.3,.4*cht],...
           'label','upper velocity');
       %lower velocity slider
       v=vmax-.25*(vmax-vmin);
       labelledslider('parent',gcf,'minval',vmin,'maxval',vmax,'val',v,...
           'position',[xnow+.67*axwid,ynow+seisfactor*axht+.4*cht,axwid*.3,.4*cht],...
           'label','upper velocity');
   end
   %input frequency axes
   ynow=ynot;
   hfreqin=axes('position',[xnow,ynow,axwid,axht],'tag','freqin');
   %output seismic axes
   xnow=xnot+axwid+medsep+panwid+2*medsep;
   ynow=ynot+axht+2*bigsep;
   axes('position',[xnow,ynow,axwid,seisfactor*axht],'tag','seisout');
   titwid=axwid*.3;
   titht=1.5*cht;
   fs=10;
   fw='bold';
   hresults=uicontrol(gcf,'style','popupmenu','string','Results axes','tag','results',...
       'units','normalized','position',[xnow,ynow+seisfactor*axht-4*smsep,titwid,titht],...
       'callback','decon_workbench(''selectresult'')',...
       'fontsize',fs,'fontweight',fw,'tooltipstring','selects the names of the results');
%    uicontrol(gcf,'style','popupmenu','string',{'filtered','unfiltered'},'tag','filt_yn',...
%        'units','normalized','position',[xnow+titwid+smsep,ynow+seisfactor*axht-4*smsep,titwid,titht],...
%        'callback','decon_workbench(''togglefilter'')');
   uicontrol(gcf,'style','pushbutton','string','delete',...
       'tag','deleteresult','units','normalized',...
       'position',[xnow+2*titwid+2*smsep,ynow+seisfactor*axht+.8*smsep,.5*titwid,.4*titht],...
       'callback','decon_workbench(''deleteresult'')','enable','off',...
       'tooltipstring','delete the displayed result');
   cliplvls=[30 15 10 8 6 5 4 3 2 1 .5 .1 .01];
   clipnames=cell(size(cliplvls));
   for k=1:length(cliplvls)
       clipnames{k}=['clip= ' num2str(cliplvls(k))];
   end
   iclip=near(cliplvls,4);
   uicontrol(gcf,'style','popupmenu','string',clipnames,...
       'tag','clipout','units','normalized','value',iclip,...
       'position',[xnow+2.5*titwid+3*smsep,ynow+seisfactor*axht-4*smsep,.5*titwid,titht],...
       'callback','decon_workbench(''clipout'')','userdata',cliplvls,...
       'tooltipstring','a smaller number means more clipping');
   %output freq axes
   ynow=ynot;
   axes('position',[xnow,ynow,axwid,axht],'tag','freqout');
   
   %tvphase axes
   ynow=ynot+axht+2*bigsep;
   xnow=xnow+axwid+medsep;
   hax=axes('position',[xnow,ynow,ax2wid,seisfactor*axht]);
   ylim([t(1) t(end)]);flipy
   title('tv phase')
   set(hax,'xticklabel',[],'tag','tvphase')
   %tvdelay axes
   hax=axes('position',[xnow+ax2wid,ynow,ax2wid,seisfactor*axht]);   
   title('tv delay')
   set(hax,'yticklabel',[],'xticklabel',[],'tag','tvdelay');
   %welltie axes
   hax=axes('position',[xnow+2*ax2wid,ynow,ax2wid,seisfactor*axht]);
   title('well tie');
   set(hax,'yticklabel',[],'xticklabel',[],'tag','welltie');
   
   %wavelets axes
   ynow=ynot;
   hax=axes('position',[xnow,ynow,1.5*ax2wid,axht]);%wavelets before tying
   set(hax,'yticklabel',[],'tag','wavelets_before');
   title({'wavelets', 'before tying'})
   hax=axes('position',[xnow+1.5*ax2wid,ynow,1.5*ax2wid,axht]);%wavelets after tying
   set(hax,'yticklabel',[],'tag','wavelets_after');
   title({'wavelets','after tying'})
   %legend toggle button
   uicontrol(gcf,'style','pushbutton','string','Legend off','units','normalized',...
       'position',[xnow,ynow-1.5*cht,.15*axwid,.5*cht],'callback','decon_workbench(''waveletlegend'')',...
       'tag','waveletlegend','userdata',1);%1 for on 0 for off
   
   %the algorithms popup
   algs={'deconf','deconw','deconb','tvsw','Gabor decon','nothing'};
   algorithms={'(frequency domain deconvolution)','(Wiener deconvolution)',...
       '(Burg deconvolution)','(time variant spectral whitening)',...
       '(Gabor deconvolution)','(pass input to output)'};
   xnow=xnot+axwid+medsep;
   ynow=ynot+seisfactor*axht+bigsep+axht+.5*bigsep;
   width=panwid;
   height=cht;
   halgs=uicontrol(gcf,'style','popupmenu','string',algs,'tag','algorithms',...
       'units','normalized','callback','decon_workbench(''algorithm'')',...
       'position',[xnow,ynow,width,height],'value',1);
   
   %set results userdata
   niter=zeros(1,length(algs));
   results=[];
   set(hresults,'userdata',{niter,results});
       
   xnow=xnot+axwid+medsep;
   ynow=ynow-medsep;
   %determine initial s1=target trace. This is the trace that will be
   %compared to the reference
   ind=near(x,xref);
   if(length(ind)>1);
       it= x(ind)>xref;
       is1=ind(it);
       if(isempty(is1))
           is1=ind(1);
       end
   else
       is1=ind;
   end
   uicontrol(gcf,'style','pushbutton','string','Apply','tag','apply',...
       'units','normalized','position',[xnow,ynow,width,.5*height],...
       'callback','decon_workbench(''apply'')','userdata',{seis,t,x,sref,tref,xref,is1,{sref},1,{tref}});
   
   %make the control panels
   npanels=length(algs);
   hpanels=cell(size(algs));
   xnow=xnot+axwid+medsep;
   ynow=ynow-.85*panht-.5*medsep;
   for k=1:npanels
       if(k==1)
           vis='on';
       else
           vis='off';
       end
       hpanels{k}=uipanel(gcf,'position',[xnow,ynow,panwid,.85*panht],'visible',vis);
       populatepanel(hpanels{k},algs{k},algorithms{k});

   end
   set(halgs,'userdata',{hpanels,algs,algorithms});
   
   %recompute spectra button
   ynow=ynow-.75*bigsep;
   uicontrol(gcf,'style','pushbutton','string','Recompute spectra','tag','spectra',...
       'units','normalized','position',[xnow,ynow,.5*width,.5*height],...
       'callback','decon_workbench(''recompspectra'')','enable','on');
   %fmax popup
   sep=.02*width;
   uicontrol(gcf,'style','popupmenu','string',{'fmax=Nyq','fmax=0.75*Nyq','fmax=0.5*Nyq','fmax=0.25*Nyq'},...
       'tag','specfmax','units','normalized','position',[xnow+.5*width+sep,ynow,.5*width-sep,.5*height],...
       'enable','on','value',3,'tooltipstring','Maximum frequency for plotting only',...
       'callback','decon_workbench(''spectrafmax'')','userdata',[1 .75 .5 .25]);
   %make the control panels
   
   %make the filter panel
   fpanht=.35*panht;
   ynow=ynow-fpanht;
   hpanfilt=uipanel(gcf,'position',[xnow,ynow,panwid,fpanht],'tag','filter',...
       'title','Filter specifications');
   ht=.07/panht;
   sep=.1*ht;
   xn=0;yn=.8;w=1;
   uicontrol(hpanfilt,'style','text','string','filter (after decon)',...
       'units','normalized','position',[xn,yn,w,ht]);
   yn=yn-.7*ht-sep;
   w=.7;
   uicontrol(hpanfilt,'style','text','string','Lowest frequency',...
       'units','normalized','position',[xn,yn,w,ht],...
       'tooltipstring','flow: lowest frequency to pass (Hz)');
   xn=xn+w+sep;
   w2=.2;
   uicontrol(hpanfilt,'style','edit','string',num2str(5),'tag','flow',...
       'units','normalized','position',[xn,yn+.25*ht,w2,.75*ht],...
            'tooltipstring','enter a value in Hz');
   yn=yn-.7*ht-sep;
   w=.7;
   xn=0;
   uicontrol(hpanfilt,'style','text','string','Highest frequency',...
       'units','normalized','position',[xn,yn,w,ht],...
       'tooltipstring','fhigh: highest frequency to pass (Hz)');
   xn=xn+w+sep;
   w2=.2;
   uicontrol(hpanfilt,'style','edit','string',num2str(100),'tag','fhigh',...
       'units','normalized','position',[xn,yn+.25*ht,w2,.75*ht],...
            'tooltipstring','enter a value in Hz');
   %yn=yn-3*ht-sep;
   yn=0;
   w=.8;
   xn=.1;
   hbg=uibuttongroup(hpanfilt,'position',[xn,yn,w,3*ht],'tag','phase',...
       'bordertype','none');
%    hbg=uibuttongroup(hpanfilt,'position',[xn,yn,w,3*ht],'tag','phase');
   xn2=0;
   yn2=.6;
   w2=1;
   ht=.4;
   uicontrol(hbg,'style','radio','string','Minimum phase','units','normalized',...
       'position',[xn2,yn2,w2,ht],'tag','minphase','value',0);
   yn2=.2;
   uicontrol(hbg,'style','radio','string','Zero phase','units','normalized',...
       'position',[xn2,yn2,w2,ht],'tag','zerophase','value',1);
   w=.4;
   xn=.6;
   hbg=uibuttongroup(hpanfilt,'position',[xn,yn,w,3*ht],'tag','filtapply',...
       'bordertype','none','visible','off');
   xn2=0;
   yn2=.7;
   w2=1;
   uicontrol(hbg,'style','radio','string','Apply','units','normalized',...
       'position',[xn2,yn2,w2,ht],'tag','dofilt','value',1);
   yn2=.3;
   uicontrol(hbg,'style','radio','string','Don''t apply','units','normalized',...
       'position',[xn2,yn2,w2,ht],'tag','nofilt','value',0);
   
   %recompute wavelets button
   ynow=ynow-.75*bigsep;
   uicontrol(gcf,'style','pushbutton','string','Recompute wavelets','tag','redowavelets',...
       'units','normalized','position',[xnow,ynow,.55*width,.5*height],...
       'callback','decon_workbench(''recomputewavelets'')','enable','on');
   %reapply filter button
   uicontrol(gcf,'style','pushbutton','string','Reapply filter','tag','applyfilter',...
       'units','normalized','position',[xnow+.56*width,ynow,.44*width,.5*height],...
       'callback','decon_workbench(''applyfilter'')','enable','on');
   
   %make the wavelet extraction panel
   wpanht=.65*panht;
   ynow=ynow-wpanht-.5*medsep;
   hpanw=uipanel(gcf,'position',[xnow,ynow,panwid,wpanht],'tag','waveex',...
       'title','Wavelet estimation');
   ht=.3;
   sep=.1*ht;
   w=.4;
   xn=.1;
   yn=.65;
   hbg=uibuttongroup(hpanw,'position',[xn,yn,w,ht],'tag','wmethods');
%    hbg=uibuttongroup(hpanw,'position',[xn,yn,w,ht],'tag','wmethods',...
%        'bordertype','none');
   xn2=0;
   yn2=.7;
   w2=1;
   ht2=.3;
   uicontrol(hbg,'style','radio','string','Simple','units','normalized',...
       'position',[xn2,yn2,w2,ht2],'tag','simple','value',1,...
       'tooltipstring','seismic amplitude spectrum with constant phase',...
       'callback','decon_workbench(''waveletmethod'')');
   yn2=yn2-ht2-.1*sep;
   uicontrol(hbg,'style','radio','string','Match filter','units','normalized',...
       'position',[xn2,yn2,w2,ht2],'tag','match','value',0,...
       'tooltipstring','smoothness constrained, least-squares, match filter',...
       'callback','decon_workbench(''waveletmethod'')');
   yn2=yn2-ht2-.1*sep;
   uicontrol(hbg,'style','radio','string','Roy White','units','normalized',...
       'position',[xn2,yn2,w2,ht2],'tag','roywhite','value',0,...
       'tooltipstring','Roy White''s method',...
       'callback','decon_workbench(''waveletmethod'')');

   uicontrol(hpanw,'style','pushbutton','string','Compare methods','units','normalized',...
       'position',[xn+w,yn+.6*ht,1.2*w,.3*ht],'tag','comparemethods',...
       'callback','decon_workbench(''comparewavelets'')',...
       'tooltipstring','compare wavelet estimation methods');
   uicontrol(hpanw,'style','pushbutton','string','Wavelet Explorer','units','normalized',...
       'position',[xn+w,yn+.2*ht,1.2*w,.3*ht],'tag','comparemethods',...
       'callback','decon_workbench(''waveex'')',...
       'tooltipstring','Launch wavelet explorer for the chosen method');
   
   
   ht=.06/panht;
   sep=.1*ht;
   xn=.1;
   yn=yn-ht-sep;
   w=.4;
   uicontrol(hpanw,'style','text','string','Window size',...
       'units','normalized','position',[xn,yn,w,ht],...
       'tooltipstring','winsize: size of wavelet extraction window (sec)');
   xn=xn+w+sep;
   w2=.2;
   uicontrol(hpanw,'style','edit','string',num2str(.5),'tag','winsize',...
       'units','normalized','position',[xn,yn+.5*ht,w2,.5*ht],...
            'tooltipstring','enter a value in seconds');
   xn=.1;
   yn=yn-.8*ht;
   w=.4;
   uicontrol(hpanw,'style','text','string','Wavelet size',...
       'units','normalized','position',[xn,yn,w,ht],...
       'tooltipstring','wavesize: size of wavelet expressed as a fraction of the window size');
   xn=xn+w+sep;
   w2=.2;
   uicontrol(hpanw,'style','edit','string',num2str(.25),'tag','wavesize',...
       'units','normalized','position',[xn,yn+.5*ht,w2,.5*ht],...
            'tooltipstring','enter a value between 0 and 1');
   xn=.1;
   yn=yn-.8*ht;
   w=.4;
   uicontrol(hpanw,'style','text','string','Smoothness','tag','mulabel',...
       'units','normalized','position',[xn,yn,w,ht],...
       'tooltipstring','0 means no smoothness, larger means more smoothness',...
       'visible','off');
   uicontrol(hpanw,'style','text','string','Freq smoother','tag','fsmolabel',...
       'units','normalized','position',[xn,yn,w,ht],...
       'tooltipstring','width of frequency smoother in Hz',...
       'visible','on');
   xn=xn+w+sep;
   w2=.2;
   uicontrol(hpanw,'style','edit','string',num2str(1),'tag','mumatch',...
       'units','normalized','position',[xn,yn+.5*ht,w2,.5*ht],...
       'tooltipstring','enter a nonnegative number','visible','off');
   uicontrol(hpanw,'style','edit','string',num2str(1),'tag','muroy',...
       'units','normalized','position',[xn,yn+.5*ht,w2,.5*ht],...
       'tooltipstring','enter a nonnegative number','visible','off');
   uicontrol(hpanw,'style','edit','string',num2str(5),'tag','fsmo',...
       'units','normalized','position',[xn,yn+.5*ht,w2,.5*ht],...
       'tooltipstring','enter a nonnegative value in Hz','visible','on');
   xn=.1;
   yn=yn-.8*ht;
   w=.4;
   uicontrol(hpanw,'style','text','string','Stability','tag','stablabel',...
       'units','normalized','position',[xn,yn,w,ht],...
       'tooltipstring','a small positive number to stabilize division in frequency domain',...
       'visible','off');
   xn=xn+w+sep;
   w2=.2;
   uicontrol(hpanw,'style','edit','string',num2str(.001),'tag','wstab',...
       'units','normalized','position',[xn,yn+.5*ht,w2,.5*ht],...
            'tooltipstring','enter a value between 0 and 1','visible','off');
      xn=.1;
   yn=yn-.8*ht;
   w=.4;
   uicontrol(hpanw,'style','text','string','Freq smoother','tag','fsmolabelrw',...
       'units','normalized','position',[xn,yn,w,ht],...
       'tooltipstring','width of frequency smoother in Hz',...
       'visible','off');
   xn=xn+w+sep;
   w2=.2;
   uicontrol(hpanw,'style','edit','string',num2str(5),'tag','fsmorw',...
       'units','normalized','position',[xn,yn+.5*ht,w2,.5*ht],...
            'tooltipstring','enter a nonnegqtive value in Hz','visible','off');
   
   
   %initial data display
   plot_tf(seis,t,x,sref,tref,xref,'b',hseisin,hfreqin,cliplvls(iclip),0,0);
   set(hseisin,'tag','seisin');
   set(hfreqin,'tag','freqin');
   axes(hseisin)
   title('input traces')
   
   %set spectral windows
   [t1,t2,t3,t4]=getspectralwindows;
   xl=get(gca,'xlim');
   line(xl,[t1 t1],'linestyle','--','color','k','buttondownfcn',...
       'decon_workbench(''dragline'')','tag','win1');
   line(xl,[t2 t2],'linestyle','--','color','k','buttondownfcn',...
       'decon_workbench(''dragline'')','tag','win2');
   line(xl,[t3 t3],'linestyle','--','color','k','buttondownfcn',...
       'decon_workbench(''dragline'')','tag','win3');
   line(xl,[t4 t4],'linestyle','--','color','k','buttondownfcn',...
       'decon_workbench(''dragline'')','tag','win4');
   
   %set decon design gate
   [td1,td2]=getdesigngate;
   line(xl,[td1 td1],[1 1],'linestyle',':','color','r','buttondownfcn',...
       'decon_workbench(''dragline'')','tag','td1','linewidth',2);
   line(xl,[td2 td2],[1 1],'linestyle',':','color','r','buttondownfcn',...
       'decon_workbench(''dragline'')','tag','td2','linewidth',2);
   
   
elseif(strcmp(action,'algorithm'))
    halg=findobj(gcf,'tag','algorithms');
    ialg=get(halg,'value');
    udat=get(halg,'userdata');
    hpanels=udat{1};
    npanels=length(hpanels);
    for k=1:npanels
        if(k==ialg)
            set(hpanels{k},'visible','on')
        else
            set(hpanels{k},'visible','off');
        end
    end
elseif(strcmp(action,'apply'))
    %determine the current algorithm
    happly=findobj(gcf,'tag','apply');
    udat=get(happly,'userdata');
    seis=udat{1};
    t=udat{2};
    x=udat{3};
    sref=udat{4};
    tref=udat{5};
    xref=udat{6};
    halg=findobj(gcf,'tag','algorithms');
    udat=get(halg,'userdata');
    ialg=get(halg,'value');
    hpanels=udat{1};
    algs=udat{2};
    thisalg=algs{ialg};
    thispanel=hpanels{ialg};
    parmlist=getparameters(thisalg,thispanel,t);
%     if(isempty(parmlist))
%         return;
%     end
    fparms=getfilterparms(t);
%     if(isempty(fparms))
%         return;
%     end
    %parse filterparms
    flow=findparm('flow',fparms);
    fhigh=findparm('fhigh',fparms);
    norder=findparm('norder',fparms);
    filtphase=findparm('phase',fparms);
    fapply=findparm('apply',fparms);
    %get the results array
    hresults=findobj(gcf,'tag','results');
    tmp=get(hresults,'string');
    if(~iscell(tmp))
        results_names{1}=tmp;
    else
        results_names=tmp;
    end
    udat=get(hresults,'userdata');
    niter=udat{1};
    results=udat{2};
    nresults=length(results)+1;
    thisresult.parms=parmlist;
    thisresult.alg=thisalg;
    thisresult.filterparms=fparms;
    thisresult.t=t;
    thisresult.x=x;
    thisresult.sref=sref;
    thisresult.tref=tref;
    thisresult.xref=xref;
    %execute
    kol='r';
    switch thisalg
        case 'deconf'
            %parse the parameters
            tstart=findparm('tstart',parmlist);
            tend=findparm('tend',parmlist);
            fsmo=findparm('fsmo',parmlist);
            stab=findparm('stab',parmlist);
            phase=findparm('phase',parmlist);
            seisd=deconf_stack(seis,t,0,tstart,tend,fsmo,stab,phase);
            thisresult.seis=seisd;
            thisresult.seisf=[];
            if(fapply==1)
                seisd=butterband(seisd,t,flow,fhigh,norder,filtphase);
                thisresult.seisf=seisd;
            end
            plotresult(seisd,t,x,sref,tref,xref,kol)
            niter(1)=niter(1)+1;
            thisresult.name=[thisalg ' #' int2str(niter(1))];
            results_names{nresults}=thisresult.name;
        case 'deconw'
            %parse the parameters
            tstart=findparm('tstart',parmlist);
            tend=findparm('tend',parmlist);
            top=findparm('top',parmlist);
            stab=findparm('stab',parmlist);
            seisd=deconw_stack(seis,t,0,tstart,tend,top,stab);
            thisresult.seis=seisd;
            thisresult.seisf=[];
            if(fapply==1)
                seisd=butterband(seisd,t,flow,fhigh,norder,filtphase);
                thisresult.seisf=seisd;
            end
            plotresult(seisd,t,x,sref,tref,xref,kol)
            niter(2)=niter(2)+1;
            thisresult.name=[thisalg ' #' int2str(niter(2))];
            results_names{nresults}=thisresult.name;
        case 'deconb'
            %parse the parameters
            tstart=findparm('tstart',parmlist);
            tend=findparm('tend',parmlist);
            top=findparm('top',parmlist);
            seisd=deconb_stack(seis,t,0,tstart,tend,top);
            thisresult.seis=seisd;
            thisresult.seisf=[];
            if(fapply==1)
                seisd=butterband(seisd,t,flow,fhigh,norder,filtphase);
                thisresult.seisf=seisd;
            end
            plotresult(seisd,t,x,sref,tref,xref,kol)
            niter(3)=niter(3)+1;
            thisresult.name=[thisalg ' #' int2str(niter(3))];
            results_names{nresults}=thisresult.name;
        case 'tvsw'
            %parse the parameters
            tstart=findparm('tstart',parmlist);
            aeclen=findparm('aeclen',parmlist);
            flow=findparm('flow',parmlist);
            fhigh=findparm('fhigh',parmlist);
            nfilt=findparm('nfilt',parmlist);
            seisd=tvsw_stack(seis,t,tstart,aeclen,flow,fhigh,nfilt);
            thisresult.seis=seisd;
            thisresult.seisf=[];
            if(fapply==1)
                seisd=butterband(seisd,t,flow,fhigh,norder,filtphase);
                thisresult.seisf=seisd;
            end
            plotresult(seisd,t,x,sref,tref,xref,kol)
            niter(4)=niter(4)+1;
            thisresult.name=[thisalg ' #' int2str(niter(4))];
            results_names{nresults}=thisresult.name;
        case 'Gabor decon'
            %parse the parameters
            twin=findparm('twin',parmlist);
            tinc=findparm('tinc',parmlist);
            tsmo=findparm('tsmo',parmlist);
            fsmo=findparm('fsmo',parmlist);
            stab=findparm('stab',parmlist);
            ihyp=findparm('ihyp',parmlist);
            phase=findparm('phase',parmlist);
            seisd=gabordecon_stack(seis,t,twin,tinc,tsmo,fsmo,ihyp,stab,phase);
            thisresult.seis=seisd;
            thisresult.seisf=[];
            if(fapply==1)
                seisd=butterband(seisd,t,flow,fhigh,norder,filtphase);
                thisresult.seisf=seisd;
            end
            plotresult(seisd,t,x,sref,tref,xref,kol)
            niter(5)=niter(5)+1;
            thisresult.name=[thisalg ' #' int2str(niter(5))];
            results_names{nresults}=thisresult.name;
        case 'nothing'
            thisresult.seis=seis;
            thisresult.seisf=seis;
            plotresult(seis,t,x,sref,tref,xref,kol)
            niter(6)=niter(6)+1;
            thisresult.name=[thisalg ' #' int2str(niter(6))];
            results_names{nresults}=thisresult.name;
    end
    results{nresults}=thisresult;
    set(hresults,'string',results_names,'value',nresults,...
        'userdata',{niter,results})
    hdeleteresult=findobj(gcf,'tag','deleteresult');
    set(hdeleteresult,'enable','on');
    decon_workbench('welltie');
elseif(strcmp(action,'selectresult'))
    thisresult=getcurrentresult;
    if(isempty(thisresult))
        return;
    end
    %determine algorithm and panel
    halg=findobj(gcf,'tag','algorithms');
    udat=get(halg,'userdata');
    hpanels=udat{1};
    algs=udat{2};
    thisalg=thisresult.alg;
    for k=1:length(algs)
        if(strcmp(thisalg,algs{k}))
            ialg=k;
        end
    end
    thispanel=hpanels{ialg};
    %fill in the algorithm panel and make it visible
    set(halg,'value',ialg);
    for k=1:length(hpanels)
        if(k==ialg)
            set(hpanels{k},'visible','on');
        else
            set(hpanels{k},'visible','off');
        end
    end
    fillpanel(thisalg,thispanel,thisresult.parms);
    %fill in the filter panel
    fillfilterpanel(thisresult.filterparms);
    apfilt=findparm('apply',thisresult.filterparms);
    kol='r';
    if(apfilt==1)
        plotresult(thisresult.seisf,thisresult.t,thisresult.x,...
            thisresult.sref,thisresult.tref,thisresult.xref,kol);
    else
        plotresult(thisresult.seis,thisresult.t,thisresult.x,...
            thisresult.sref,thisresult.tref,thisresult.xref,kol);
    end
    %fill in the wavelet panel
    fillwaveletpanel(thisresult.waveletmethod,thisresult.waveletparms);
    %plot the well tie
    plotwelltie(thisresult.t,thisresult.tvphase,thisresult.tvdelay,thisresult.sreftie,...
        thisresult.tref,thisresult.s1tie,thisresult.twin,kol);
    %plot the wavelets
    plotwavelets(thisresult.wletsb,thisresult.twsb,thisresult.wletsa,...
        thisresult.twsa,thisresult.t,thisresult.tref);

elseif(strcmp(action,'deleteresult'))
    hresults=findobj(gcf,'tag','results');
    udat=get(hresults,'userdata');
    results=udat{2};
    if(isempty(results))
        return
    end
    iresult=get(hresults,'value');%this will be the result to delete
    result_names=get(hresults,'string');
    result_names(iresult)=[];
    results(iresult)=[];
    %determine which result to display next
    nresults=length(results);
    if(iresult>nresults)%happens if the last one is deleted
        iresult=nresults;
    end
    udat{2}=results;
    if(nresults>0)
        set(hresults,'string',result_names,'value',iresult,'userdata',udat);
        decon_workbench('selectresult');
    else
        set(hresults,'string','Results axes','value',1,'userdata',udat);
        haxe=findobj(gcf,'tag','seisout');
        set(gcf,'currentaxes',haxe);
        cla;
        haxe=findobj(gcf,'tag','freqout');
        set(gcf,'currentaxes',haxe);
        cla;
        haxe=findobj(gcf,'tag','tvphase');
        set(gcf,'currentaxes',haxe);
        cla;
        haxe=findobj(gcf,'tag','tvdelay');
        set(gcf,'currentaxes',haxe);
        cla;
        haxe=findobj(gcf,'tag','welltie');
        set(gcf,'currentaxes',haxe);
        title('welltie')
        cla;
        hdeleteresult=findobj(gcf,'tag','deleteresult');
        set(hdeleteresult,'enable','on');
    end
elseif(strcmp(action,'dragline'))
    hline=gco;
    happly=findobj(gcf,'tag','apply');
    udat=get(happly,'userdata');
    t=udat{2};
    tref=udat{5};
    lineid=get(hline,'tag');
    [t1,t2,t3,t4]=getspectralwindows;
    [td1,td2]=getdesigngate;
    [tcc1,tcc2]=getcorrelationgate;
    factor=.1;
    if(strcmp(lineid,'win1'))
        del=factor*(t2(1)-t(1));
        ylims=[t(1)+del t2(1)-del];
    elseif(strcmp(lineid,'win2'))
        del=factor*(t3(1)-t1(1));
        ylims=[t1(1)+del t3(1)-del];
    elseif(strcmp(lineid,'win3'))
        del=factor*(t4(1)-t2(1));
        ylims=[t2(1)+del t4(1)-del];
    elseif(strcmp(lineid,'win4'));
        del=factor*(t(end)-t3(1));
        ylims=[t3(1)+del t(end)-del];
    elseif(strcmp(lineid,'td1'))
        del=(td2-td1)*factor;
        ylims=[t(1)+del td2-del];
    elseif(strcmp(lineid,'td2'))
        del=(td2-td1)*factor;
        ylims=[td1+del t(end)-del];
    elseif(strcmp(lineid,'tcc1'))
        del=(tcc2-tcc1)*factor;
        ylims=[tref(1) tcc2-del];
    elseif(strcmp(lineid,'tcc2'))
        del=(tcc2-tcc1)*factor;
        ylims=[tcc1+del tref(end)];
    else
        return;
    end
    DRAGLINE_MOTION='yonly';
    DRAGLINE_YLIMS=ylims;
    DRAGLINE_SHOWPOSN='on';
    if(strcmp(lineid,'td1') || strcmp(lineid,'td2'))
        DRAGLINE_CALLBACK='decon_workbench(''designgatechangeline'')';
    elseif(strcmp(lineid,'tcc1') || strcmp(lineid,'tcc2'))
        DRAGLINE_CALLBACK='decon_workbench(''correlationgatechange'')';
    else
        DRAGLINE_CALLBACK='';
    end
    dragline('click');
%     hrecomp=findobj(gcf,'tag','spectra');
%     set(hrecomp,'enable','on','backgroundcolor',[0 .5 .5])
elseif(strcmp(action,'recompspectra'))
   %first the input data
   happly=findobj(gcf,'tag','apply');
   udat=get(happly,'userdata');
   seis=udat{1};
   t=udat{2};
   hfreq=findobj(gcf,'tag','freqin');
   flag_f=0;
   plot_f(seis,t,hfreq,flag_f);
   %now the current result
   decon_workbench('selectresult');
elseif(strcmp(action,'designgatechangeline'))
    setdesigngate('line')
elseif(strcmp(action,'designgatechangetext'))
    setdesigngate('text');
elseif(strcmp(action,'correlationgatechange'))
    hwelltie=findobj(gcf,'tag','welltie');
    hwell=findobj(hwelltie,'tag','well');
    hseis=findobj(hwelltie,'tag','seis');
    tref=get(hwell,'ydata');
    sref=get(hwell,'xdata');
    t=get(hseis,'ydata');
    s1=get(hseis,'xdata');
    [tcc1,tcc2]=getcorrelationgate;
    ind1=near(tref,tcc1,tcc2);
    ind2=near(t,tcc1,tcc2);
    cc=maxcorr(sref(ind1),s1(ind2)-1);%a 1 was added to s1 when plotted
    dt=t(2)-t(1);
    title({'well tie',['cc=' num2str(sigfig(cc(1),2)) ', shift=' time2str(sigfig(cc(2),2)*dt)]});
    set(hwelltie,'tag','welltie')
elseif(strcmp(action,'clipout'))
    decon_workbench('selectresult')
elseif(strcmp(action,'welltie'))
    %get the target trace (s1)
    happly=findobj(gcf,'tag','apply');
    udat=get(happly,'userdata');
    t=udat{2};
    sref=udat{4};
    tref=udat{5};
    is1=udat{7};
    thisresult=getcurrentresult;
    if(isempty(thisresult))
        return;
    end
    s1=thisresult.seisf(:,is1);
    kol='r';
    %now tie the well
    twin=.3;%gaussian window for time-variant analysis
    tinc=.1;%increment between gaussians
    flag=1;
    %s1 is the single traces closest to the reference trace
    s1=s1/max(s1);%normalize
    sref=sref/max(sref);%normalize
    %disp('doing well tie')
    [s1tie,sreftie,tvdelay,tvphase]=welltie(s1,t,sref,tref,twin,tinc,flag);
    plotwelltie(t,tvphase,tvdelay,sreftie,tref,s1tie,twin,kol);
    
    %estimate wavelets
    %before
    [wletsb,twsb,t0s,twinw]=estimate_wavelets(s1,t,sref,tref);
    %after
    [wletsa,twsa]=estimate_wavelets(s1tie,t,sreftie,tref);
    %now plot
    plotwavelets(wletsb,twsb,wletsa,twsa,t,tref);
    %update results
    thisresult.waveletmethod=getwaveletmethod;
    thisresult.waveletparms=getwaveletparms(t);
    thisresult.sreftie=sreftie;
    thisresult.s1tie=s1tie;
    thisresult.is1=is1;
    thisresult.wletsb=wletsb;
    thisresult.twsb=twsb;
    thisresult.wletsa=wletsa;
    thisresult.twsa=twsa;
    thisresult.t0s=t0s;
    thisresult.twinw=twinw;
    thisresult.tvphase=tvphase;
    thisresult.tvdelay=tvdelay;
    thisresult.twin=twin;
    thisresult.tinc=tinc;
    
    savecurrentresult(thisresult);
    
elseif(strcmp(action,'waveletmethod'))
    wmethod=getwaveletmethod;
    hpan=findobj(gcf,'tag','waveex');
    hmulabel=findobj(hpan,'tag','mulabel');
    hmum=findobj(hpan,'tag','mumatch');
    hmuroy=findobj(hpan,'tag','muroy');
    hstablabel=findobj(hpan,'tag','stablabel');
    hstab=findobj(hpan,'tag','wstab');
    hfsmolabel=findobj(hpan,'tag','fsmolabel');
    hfsmo=findobj(hpan,'tag','fsmo');
    hfsmolabelrw=findobj(hpan,'tag','fsmolabelrw');
    hfsmorw=findobj(hpan,'tag','fsmorw');
    switch wmethod
        case 'simple'
            set([hstab hstablabel hmum hmuroy hmulabel hfsmolabelrw hfsmorw],'visible','off');
            set([hfsmolabel hfsmo],'visible','on');
        case 'match'
            set([hstab hstablabel hmuroy hfsmolabel hfsmo hfsmolabelrw hfsmorw],'visible','off');
            set([hmum hmulabel],'visible','on');
        case 'roywhite'
            set([hmulabel hmuroy hstab hstablabel],'visible','on');
            set([hmum hfsmolabel hfsmo hfsmolabelrw hfsmorw],'visible','off');
    end
elseif(strcmp(action,'recomputewavelets'))
    thisresult=getcurrentresult;
    if(isempty(thisresult))
        return
    end
    s1=thisresult.seisf(:,thisresult.is1);
    %s1 is the trace closest to the reference trace
    s1tie=thisresult.s1tie;
    sref=thisresult.sref;
    sreftie=thisresult.sreftie;
    %s1tie is the single traces closest to the reference trace after tying.
    %It will be similar to s1 but with time variant phase rotations
    %sref is the reference trace. Usually rcs from a well
    %sreftie is the reference trace with the seismic bandwidth imposed and
    %any time shifts derived from well tying.
    s1=s1/max(s1);%normalize
    sref=sref/max(sref);%normalize
    t=thisresult.t;
    tref=thisresult.tref;
    
    %estimate wavelets. Function estimate_wavelets is internal and calls
    %internal functions to get the current wavelet method and its
    %parameters.
    %before
    [wletsb,twsb,t0s,twinw]=estimate_wavelets(s1,t,sref,tref);
    %after
    [wletsa,twsa]=estimate_wavelets(s1tie,t,sreftie,tref);
    %now plot
    plotwavelets(wletsb,twsb,wletsa,twsa,t,tref);
    %update results
    thisresult.waveletmethod=getwaveletmethod;
    thisresult.waveletparms=getwaveletparms(t);
    thisresult.wletsb=wletsb;
    thisresult.twsb=twsb;
    thisresult.wletsa=wletsa;
    thisresult.twsa=twsa;
    thisresult.t0s=t0s;
    thisresult.twinw=twinw;
    
    savecurrentresult(thisresult);
elseif(strcmp(action,'applyfilter'))
    thisresult=getcurrentresult;
    if(isempty(thisresult))
        return
    end
    
    %get the filter parms
    fparms=getfilterparms(thisresult.t);
    %parse filterparms
    flow=findparm('flow',fparms);
    fhigh=findparm('fhigh',fparms);
    norder=findparm('norder',fparms);
    filtphase=findparm('phase',fparms);
    
    thisresult.seisf=butterband(thisresult.seis,thisresult.t,flow,fhigh,norder,filtphase);
    
    thisresult.filterparms=fparms;
    
    savecurrentresult(thisresult);
    
    %recompute spectra
    decon_workbench('recompspectra');
    
    %re-do well tie
    decon_workbench('welltie');
elseif(strcmp(action,'spectrafmax'))
    hfmax=findobj(gcf,'tag','specfmax');
    val=get(hfmax,'value');
    factors=get(hfmax,'userdata');
    hfreqin=findobj(gcf,'tag','freqin');
    hfreqout=findobj(gcf,'tag','freqout');
    hlines=findobj(hfreqin,'type','line');
    if(~isempty(hlines))
        f=get(hlines(1),'xdata');
        fnyq=f(end);
        set(hfreqin,'xlim',[f(1) factors(val)*fnyq]);
    end
    hlines=findobj(hfreqout,'type','line');
    if(~isempty(hlines))
        f=get(hlines(1),'xdata');
        fnyq=f(end);
        set(hfreqout,'xlim',[f(1) factors(val)*fnyq]);
    end
elseif(strcmp(action,'waveletlegend'))
    haxe=findobj(gcf,'tag','wavelets_before');
    hbutton=gco;
    val=get(hbutton,'userdata');
    %set(gcf,'currentaxes',haxe);
    if(val==0)
        %turn legend on
        legend(haxe,'show');
        set(hbutton,'string','Legend off','userdata',1);
    else
        %turn legend off
        legend(haxe,'hide')
        set(hbutton,'string','Legend on','userdata',0);
    end
elseif(strcmp(action,'comparewavelets'))
    thisresult=getcurrentresult;
    if(isempty(thisresult))
        return
    end
    htit=findobj(gcf,'tag','title');
    titname=get(htit,'string');
    s1=thisresult.seisf(:,thisresult.is1);
    %s1 is the trace closest to the reference trace
    s1tie=thisresult.s1tie;
    sref=thisresult.sref;
    sreftie=thisresult.sreftie;
    %s1tie is the single traces closest to the reference trace after tying.
    %It will be similar to s1 but with time variant phase rotations
    %sref is the reference trace. Usually rcs from a well
    %sreftie is the reference trace with the seismic bandwidth imposed and
    %any time shifts derived from well tying.
    s1=s1/max(s1);%normalize
    sref=sref/max(sref);%normalize
    t=thisresult.t;
    tref=thisresult.tref;
    
    %compare wavelets. Function compare_wavelets is internal and calls
    %internal functions to get the current wavelet method and its
    %parameters.
    %before
    [wletsb,twsb]=compare_wavelets(s1,t,sref,tref);
    %after
    [wletsa,twsa]=compare_wavelets(s1tie,t,sreftie,tref);
    %generate the stats
    statsb=cell(size(wletsb));
    for k=1:length(wletsb)
        wstats=wavelet_stats(wletsb{k},twsb{k});
        statsb{k}=wstats{1};
    end
    statsa=cell(size(wletsa));
    for k=1:length(wletsa)
        wstats=wavelet_stats(wletsa{k},twsa{k});
        statsa{k}=wstats{1};
    end
    %make the wavelets structure
    wavelets.name=['Wavelets: ' titname '\' thisresult.name];
    wavelets.wletsb=wletsb;
    wavelets.twsb=twsb;
    wavelets.statsb=statsb;
    wavelets.wletsa=wletsa;
    wavelets.twsa=twsa;
    wavelets.statsa=statsa;
    %make the figure and the six axes
    pos=get(gcf,'position');
    figure('position',[pos(1)+.5*pos(3), pos(2), .4*pos(3), .8*pos(4)]);
    set(gcf,'name',wavelets.name)
    haxes=zeros(1,6);
    %rw before
    vsep=.05;
    hsep=.05;
    xnow=.1;ynow=.1;wid=.4;ht=.25;
    haxes(1)=axes('position',[xnow,ynow,wid,ht]);
    %match before
    ynow=ynow+ht+vsep;
    haxes(2)=axes('position',[xnow,ynow,wid,ht]);
    %simple before
    ynow=ynow+ht+vsep;
    haxes(3)=axes('position',[xnow,ynow,wid,ht]);
    %rw after
    xnow=.1+wid+hsep;ynow=.1;
    haxes(4)=axes('position',[xnow,ynow,wid,ht]);
    %match after
    ynow=ynow+ht+vsep;
    haxes(5)=axes('position',[xnow,ynow,wid,ht]);
    %simple after
    ynow=ynow+ht+vsep;
    haxes(6)=axes('position',[xnow,ynow,wid,ht]);
    wavelets.haxes=haxes;
    wavelets.mode=1;%1 for time 2 for frequency
    %make the change button
    xnow=.45;ynow=.02;width=.15;ht=.035;
    uicontrol(gcf,'style','pushbutton','string','Show spectra','units','normalized',...
        'position',[xnow,ynow,width,ht],'tag','change',...
        'callback','decon_workbench(''comparewaveletschange'')','userdata',wavelets);
    comparewaveletstime(wavelets);
elseif(strcmp(action,'comparewaveletschange'))
    wavelets=get(gco,'userdata');
    if(wavelets.mode==1)
        wavelets.mode=2;
        set(gco,'string','Show wavelets','userdata',wavelets);
        comparewaveletsfreq(wavelets);
    else
        wavelets.mode=1;
        set(gco,'string','Show spectra','userdata',wavelets);
        comparewaveletstime(wavelets);
    end
elseif(strcmp(action,'waveexmatch'))
    %determine which axes we are calling from
    tag=get(gca,'tag');
    htrace=gco;%handle trace that was clicked on
%     s=get(htrace,'xdata');%trace samples
%     t=get(htrace,'ydata');%gtrace time
    itrace=get(htrace,'userdata');%the trace number
    hobj=findobj(gcf,'tag','title');
    titname=get(hobj,'string');
    if(strcmp(tag,'seisin'))
        %ok it is the input data
        happly=findobj(gcf,'tag','apply');
        udat=get(happly,'userdata');
        s=udat{1}(:,itrace);
        t=udat{2};
        sref=udat{4};
        tref=udat{5};
        name=[titname ', input data trace number ' int2str(itrace)];
    else
        %it is the current result
        thisresult=getcurrentresult;
        s=thisresult.seisf(:,itrace);
        t=thisresult.t;
        sref=thisresult.sref;
        tref=thisresult.tref;
        name=[titname ', ' thisresult.name ', trace number ' int2str(itrace)];
    end
    waveex_match(s(:),t(:),sref,tref,name);
elseif(strcmp(action,'waveexrw'))
    %determine which axes we are calling from
    tag=get(gca,'tag');
    htrace=gco;%handle trace that was clicked on
    %     s=get(htrace,'xdata');%trace samples
    %     t=get(htrace,'ydata');%gtrace time
    itrace=get(htrace,'userdata');%the trace number
    hobj=findobj(gcf,'tag','title');
    titname=get(hobj,'string');
    if(strcmp(tag,'seisin'))
        %ok it is the input data
        happly=findobj(gcf,'tag','apply');
        udat=get(happly,'userdata');
        s=udat{1}(:,itrace);
        t=udat{2};
        sref=udat{4};
        tref=udat{5};
        name=[titname ', input data trace number ' int2str(itrace)];
    else
        %it is the current result
        thisresult=getcurrentresult;
        s=thisresult.seisf(:,itrace);
        t=thisresult.t;
        sref=thisresult.sref;
        tref=thisresult.tref;
        name=[titname ', ' thisresult.name ', trace number ' int2str(itrace)];
    end
    waveex_rw(s(:),t(:),sref,tref,name);
elseif(strcmp(action,'waveexsimple'))
    %determine which axes we are calling from
    tag=get(gca,'tag');
    htrace=gco;%handle trace that was clicked on
    %     s=get(htrace,'xdata');%trace samples
    %     t=get(htrace,'ydata');%gtrace time
    itrace=get(htrace,'userdata');%the trace number
    hobj=findobj(gcf,'tag','title');
    titname=get(hobj,'string');
    if(strcmp(tag,'seisin'))
        %ok it is the input data
        happly=findobj(gcf,'tag','apply');
        udat=get(happly,'userdata');
        s=udat{1}(:,itrace);
        t=udat{2};
        sref=udat{4};
        tref=udat{5};
        name=[titname ', input data trace number ' int2str(itrace)];
    else
        %it is the current result
        thisresult=getcurrentresult;
        s=thisresult.seisf(:,itrace);
        t=thisresult.t;
        sref=thisresult.sref;
        tref=thisresult.tref;
        name=[titname ', ' thisresult.name ', trace number ' int2str(itrace)];
    end
    waveex_simple(s(:),t(:),sref,tref,name);
elseif(strcmp(action,'waveex'))
    %use the latest result unless its empty
    thisresult=getcurrentresult;
    if(isempty(thisresult))
        happly=findobj(gcf,'tag','apply');
        htitle=findobj(gcf,'tag','title');
        name=get(htitle,'string');
        udat=get(happly,'userdata');
        r=udat{4};
        tr=udat{5};
        seis=udat{1};
        is1=udat{7};
        s=seis(:,is1);
        t=udat{2};
    else
        name=thisresult.name;
        r=thisresult.sref;
        tr=thisresult.tref;
        s=thisresult.seisf(:,thisresult.is1);
        t=thisresult.t;
    end
    wmethod=getwaveletmethod;
    if(strcmp(wmethod,'simple'))
        waveex_simple(s,t,r,tr,name);
    elseif(strcmp(wmethod,'match'))
        waveex_match(s,t,r,tr,name);
    else
        waveex_rw(s,t,r,tr,name);
    end
elseif(strcmp(action,'alignment'))
    happly=findobj(gcf,'tag','apply');
    %htitle=findobj(gcf,'tag','title');
    %name=get(htitle,'string');
    udat=get(happly,'userdata');
    r=udat{4};
    tr=udat{5};
    seis=udat{1};
    is1=udat{7};
    s=seis(:,is1);
    t=udat{2};
    cb='decon_workbench(''newref'')';
    wfact=.5;htfact=.7;
    align_ref(r,tr,s,t,cb,wfact,htfact);
    name=get(gcf,'name');
    set(gcf,'name',[name ' (decon_workbench)'])
elseif(strcmp(action,'newref'))
    %get the globals from align_ref
    rnew=ALIGN_REF_RNEW;
    trnew=ALIGN_REF_TRNEW;
    %get happly userdata and update
    happly=findobj(gcf,'tag','apply');
    udat=get(happly,'userdata');
    sref_array=udat{8};
    tref_array=udat{10};
    nrefs=length(sref_array)+1;
    sref_array{nrefs}=rnew;
    tref_array{nrefs}=trnew;
    rold=udat{4};
    udat{4}=rnew;
    udat{5}=trnew;
    udat{8}=sref_array;
    udat{9}=nrefs;
    udat{10}=tref_array;
    set(happly,'userdata',udat);
    %update the reflectivity plot
    hseisin=findobj(gcf,'tag','seisin');
    href=findobj(hseisin,'tag','ref');
    rold2=get(href,'xdata');
    a=max(rold2)/max(rold);%scale factor;
    set(href,'xdata',rnew*a,'ydata',trnew);
    hrefparent=findobj(gcf,'tag','refs');%parent to list of reflectivities
    hrefs=get(hrefparent,'children');
    %make a new menu
    nrefs=length(hrefs);
    nrefs=nrefs+1;
    uimenu(hrefparent,'label',int2str(nrefs),'userdata',nrefs,...
        'callback','decon_workbench(''switchref'')','userdata',nrefs);
    hrefs=get(hrefparent,'children');%these are in reverse order, first is most recent
    for k=2:nrefs
        set(hrefs(k),'checked','off');
    end
    set(hrefs(1),'checked','on');
elseif(strcmp(action,'switchref'))
    hrefnew=gcbo;
    irnew=get(hrefnew,'userdata');%number of the new reflectivity
    %get happly userdata and update
    happly=findobj(gcf,'tag','apply');
    udat=get(happly,'userdata');
    sref_array=udat{8};
    irold=udat{9};
    tref_array=udat{10};
    rnew=sref_array{irnew};
    trnew=tref_array{irnew};
    rold=sref_array{irold};
    udat{4}=rnew;
    udat{5}=trnew;
    set(happly,'userdata',udat);
    %update the reflectivity plot
    hseisin=findobj(gcf,'tag','seisin');
    href=findobj(hseisin,'tag','ref');
    rold2=get(href,'xdata');
    a=max(rold2)/max(rold);%scale factor;
    set(href,'xdata',rnew*a,'ydata',trnew);
    %update the menus (checked property)
    hrefparent=findobj(gcf,'tag','refs');%parent to list of reflectivities
    hrefs=get(hrefparent,'children');
    nrefs=length(hrefs);
    for k=1:nrefs
       set(hrefs(k),'checked','off');
    end
    set(hrefs(nrefs-irnew+1),'checked','on');
    
end
end

% END OF MAIN FUNCTION DECON_WORKBENCH


% local sub functions follow:
function populatepanel(hpanel,alg,algorithm)
pos=get(hpanel,'position');
panht=pos(4);
ht=.04/panht;
sep=.05*ht;
xnow=0;
ynow=.85;
uicontrol(hpanel,'style','text','string',[alg ' ' algorithm],...
    'units','normalized','position',[xnow,ynow,1,ht]);
[td1,td2]=getdesigngate;
switch alg
    case 'deconf'
        ynow=ynow-ht-sep;
        width=.6;
        uicontrol(hpanel,'style','text','string','design start time',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','tstart: start time for decon operator design (s)');
        xnow=xnow+width+sep;
        width2=.2;
        uicontrol(hpanel,'style','edit','string',num2str(td1),'tag','tstart',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds',...
            'callback','decon_workbench(''designgatechangetext'')');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','design end time',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','tend: end time for decon operator design (s)');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(td2),'tag','tend',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds',...
            'callback','decon_workbench(''designgatechangetext'')');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','frequency smoother size',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','fsmo=width of frequency smoother (Hz)');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(10),'tag','fsmo',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in Hz');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','stability constant',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','also called white noise factor');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(.001),'tag','stab',...
            'units','normalized','position',[xnow,ynow+.5*ht,1.5*width2,.5*ht],...
            'tooltipstring','enter a value between 0 and 1');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','phase',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','1=min phase, 0=zero phase');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(1),'tag','phase',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter either 0 or 1');
    case 'deconw'
        ynow=ynow-ht-sep;
        width=.6;
        uicontrol(hpanel,'style','text','string','design start time',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','tstart: start time for decon operator design (s)');
        xnow=xnow+width+sep;
        width2=.2;
        uicontrol(hpanel,'style','edit','string',num2str(td1),'tag','tstart',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds',...
            'callback','decon_workbench(''designgatechangetext'')');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','design end time',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','tend: end time for decon operator design (s)');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(td2),'tag','tend',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds',...
            'callback','decon_workbench(''designgatechangetext'')');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','Operator length (sec)',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','top: length of decon operator (seconds)');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(0.1),'tag','top',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','stability constant',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','stab: also called white noise factor');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(.001),'tag','stab',...
            'units','normalized','position',[xnow,ynow+.5*ht,1.5*width2,.5*ht],...
            'tooltipstring','enter a value between 0 and 1');
    case 'deconb'
        ynow=ynow-ht-sep;
        width=.6;
        uicontrol(hpanel,'style','text','string','design start time',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','tstart: start time for decon operator design (s)');
        xnow=xnow+width+sep;
        width2=.2;
        uicontrol(hpanel,'style','edit','string',num2str(td1),'tag','tstart',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds',...
            'callback','decon_workbench(''designgatechangetext'')');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','design end time',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','tend: end time for decon operator design (s)');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(td2),'tag','tend',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds',...
            'callback','decon_workbench(''designgatechangetext'')');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','Operator length (sec)',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','top: length of decon operator (seconds)');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(0.1),'tag','top',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds');
    case 'tvsw'
        ynow=ynow-ht-sep;
        width=.6;
        uicontrol(hpanel,'style','text','string','tstart (sec)',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','tstart: earliest time for output');
        xnow=xnow+width+sep;
        width2=.2;
        uicontrol(hpanel,'style','edit','string',num2str(0),'tag','tstart',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a non-negative value in seconds');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','agc operator length (sec)',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','agclen: length of AGC operator in seconds');
        xnow=xnow+width+sep;
        width2=.2;
        uicontrol(hpanel,'style','edit','string',num2str(0.3),'tag','aec',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','lowest frequency of interest (Hz)',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','flow: lowest frequency to be whitened (Hz)');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(5),'tag','flow',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in Hz');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','highest frequency of interest (Hz)',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','fhigh: highest frequency to be whitened (Hz)');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(100),'tag','fhigh',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in Hz');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','number of filter slices',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','nfilt: the number of narrow band filters between flow and fhigh');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(10),'tag','nfilt',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter an integer number');
    case 'Gabor decon'
        ynow=ynow-ht-sep;
        width=.6;
        uicontrol(hpanel,'style','text','string','half width of Gaussian window',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','twin: the temporal Gaussian window (seconds)');
        xnow=xnow+width+sep;
        width2=.2;
        uicontrol(hpanel,'style','edit','string',num2str(0.2),'tag','twin',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value in seconds');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','window increment (sec)',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','tinc: the spacing between adjacent windows');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(0.05),'tag','tinc',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a positive value smaller than twin in seconds');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','size of temporal smoother (sec)',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','tsmo: size of convolutional smoother in seconds');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(0.5),'tag','tsmo',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a value greater than tinc but less than tmax in seconds');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','size of frequency smoother (Hz)',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','fsmo: size of the convolutional smoother in Hz');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(5),'tag','fsmo',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter a positive value in Hz');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','stability constant',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','also called white noise factor');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(.0000001),'tag','stab',...
            'units','normalized','position',[xnow,ynow+.5*ht,1.5*width2,.5*ht],...
            'tooltipstring','stab: enter a value between 0 and 1');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','flag for hyperbolic smoothing',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','ihyp: either 0 or 1');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(1),'tag','ihyp',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter either 0 or 1');
        xnow=0;
        ynow=ynow-ht-sep;
        uicontrol(hpanel,'style','text','string','phase',...
            'units','normalized','position',[xnow,ynow,width,ht],...
            'tooltipstring','1=min phase, 0=zero phase');
        xnow=xnow+width+sep;
        uicontrol(hpanel,'style','edit','string',num2str(1),'tag','phase',...
            'units','normalized','position',[xnow,ynow+.5*ht,width2,.5*ht],...
            'tooltipstring','enter either 0 or 1');
    case 'nothing'
       %this is designed to simply pass the input to wavelet estimation  
end
end


function parmlist=getparameters(alg,hpanel,t)
% find the panels and get the parameters for algorithm alg.
% the parameters will be returned in a cell array of name-value pairs.
% alg ... name of the algorithm
% hpanel ... handle of the panel
% t ... time coordinate vector (needed for error checking)
parmlist=[];
switch alg
    case 'deconf'
        errmsgs=cell(1,6);
        errmsgs{6}='correct your errors and try again';
        nerr=0;
        parmlist=cell(1,10);
        %tstart
        hobj=findobj(hpanel,'tag','tstart');
        val=get(hobj,'string');
        tstart=str2double(val);
        if(tstart<t(1) || tstart>t(end) || isnan(tstart) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for tstart';
        end
        parmlist{1}='tstart';parmlist{2}=tstart;
        %tend
        hobj=findobj(hpanel,'tag','tend');
        val=get(hobj,'string');
        tend=str2double(val);
        if(tend<tstart || tend>t(end) || isnan(tend) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for tend';
        end
        parmlist{3}='tend';parmlist{4}=tend;
        %fsmo
        hobj=findobj(hpanel,'tag','fsmo');
        val=get(hobj,'string');
        fsmo=str2double(val);
        if(fsmo<0 || fsmo>.5/(t(2)-t(1)) || isnan(fsmo) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for fsmo';
        end
        parmlist{5}='fsmo';parmlist{6}=fsmo;
        %stab
        hobj=findobj(hpanel,'tag','stab');
        val=get(hobj,'string');
        stab=str2double(val);
        if(stab<0 || stab>1 || isnan(stab) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for stab';
        end
        parmlist{7}='stab';parmlist{8}=stab;
        %phase
        hobj=findobj(hpanel,'tag','phase');
        val=get(hobj,'string');
        phase=str2double(val);
        if(phase~=0 && phase~=1 || isnan(phase) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for phase';
        end
        parmlist{9}='phase';parmlist{10}=phase;
        if(nerr>0)
            parmlist=[];
            msgbox(errmsgs,'Oh oh... Errors in your parameters');
            return;
        end
    case 'deconw'
        errmsgs=cell(1,5);
        errmsgs{5}='correct your errors and try again';
        nerr=0;
        parmlist=cell(1,8);
        %tstart
        hobj=findobj(hpanel,'tag','tstart');
        val=get(hobj,'string');
        tstart=str2double(val);
        if(tstart<t(1) || tstart>t(end) || isnan(tstart) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for tstart';
        end
        parmlist{1}='tstart';parmlist{2}=tstart;
        %tend
        hobj=findobj(hpanel,'tag','tend');
        val=get(hobj,'string');
        tend=str2double(val);
        if(tend<tstart || tend>t(end) || isnan(tend) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for tend';
        end
        parmlist{3}='tend';parmlist{4}=tend;
        %top
        hobj=findobj(hpanel,'tag','top');
        val=get(hobj,'string');
        top=str2double(val);
        if(top<0 || top>t(end) || isnan(top) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for top';
        end
        parmlist{5}='top';parmlist{6}=top;
        %stab
        hobj=findobj(hpanel,'tag','stab');
        val=get(hobj,'string');
        stab=str2double(val);
        if(stab<0 || stab>1 || isnan(stab) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for stab';
        end
        parmlist{7}='stab';parmlist{8}=stab;
        if(nerr>0)
            parmlist=[];
            msgbox(errmsgs,'Oh oh... Errors in your parameters');
            return;
        end
    case 'deconb'
        errmsgs=cell(1,4);
        errmsgs{4}='correct your errors and try again';
        nerr=0;
        parmlist=cell(1,6);
        %tstart
        hobj=findobj(hpanel,'tag','tstart');
        val=get(hobj,'string');
        tstart=str2double(val);
        if(tstart<t(1) || tstart>t(end) || isnan(tstart) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for tstart';
        end
        parmlist{1}='tstart';parmlist{2}=tstart;
        %tend
        hobj=findobj(hpanel,'tag','tend');
        val=get(hobj,'string');
        tend=str2double(val);
        if(tend<tstart || tend>t(end) || isnan(tend) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for tend';
        end
        parmlist{3}='tend';parmlist{4}=tend;
        %top
        hobj=findobj(hpanel,'tag','top');
        val=get(hobj,'string');
        top=str2double(val);
        if(top<0 || top>t(end) || isnan(top) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for top';
        end
        parmlist{5}='top';parmlist{6}=top;
        if(nerr>0)
            parmlist=[];
            msgbox(errmsgs,'Oh oh... Errors in your parameters');
            return;
        end
        
    case 'tvsw'
        errmsgs=cell(1,6);
        errmsgs{6}='correct your errors and try again';
        nerr=0;
        parmlist=cell(1,10);
        %aeclen
        hobj=findobj(hpanel,'tag','aec');
        val=get(hobj,'string');
        aeclen=str2double(val);
        if(aeclen<t(1) || aeclen>t(end) || isnan(aeclen) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for aeclen';
        end
        parmlist{1}='aeclen';parmlist{2}=aeclen;
        %flow
        hobj=findobj(hpanel,'tag','flow');
        val=get(hobj,'string');
        flow=str2double(val);
        if(flow<0 || flow>.5/(t(2)-t(1)) || isnan(flow) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for flow';
        end
        parmlist{3}='flow';parmlist{4}=flow;
        %fhigh
        hobj=findobj(hpanel,'tag','fhigh');
        val=get(hobj,'string');
        fhigh=str2double(val);
        if(fhigh<flow || fhigh>.5/(t(2)-t(1)) || isnan(fhigh) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for fhigh';
        end
        parmlist{5}='fhigh';parmlist{6}=fhigh;
        %nfilt
        hobj=findobj(hpanel,'tag','nfilt');
        val=get(hobj,'string');
        nfilt=round(str2double(val));
        if(nfilt<1 || nfilt>100 || isnan(nfilt) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for nfilt';
        end
        parmlist{7}='nfilt';parmlist{8}=nfilt;
        %tstart
        hobj=findobj(hpanel,'tag','tstart');
        val=get(hobj,'string');
        tstart=round(str2double(val));
        if(tstart<0 || tstart>t(end) || isnan(tstart) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for tstart';
        end
        parmlist{9}='tstart';parmlist{10}=tstart;
        if(nerr>0)
            parmlist=[];
            msgbox(errmsgs,'Oh oh... Errors in your parameters');
            return;
        end
    case 'Gabor decon'
        errmsgs=cell(1,8);
        errmsgs{8}='correct your errors and try again';
        nerr=0;
        parmlist=cell(1,14);
        %twin
        hobj=findobj(hpanel,'tag','twin');
        val=get(hobj,'string');
        twin=str2double(val);
        if(twin<t(1) || twin>t(end) || isnan(twin) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for twin';
        end
        parmlist{1}='twin';parmlist{2}=twin;
        %tinc
        hobj=findobj(hpanel,'tag','tinc');
        val=get(hobj,'string');
        tinc=str2double(val);
        if(tinc<0 || tinc>twin || isnan(tinc) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for tinc';
        end
        parmlist{3}='tinc';parmlist{4}=tinc;
        %tsmo
        hobj=findobj(hpanel,'tag','tsmo');
        val=get(hobj,'string');
        tsmo=str2double(val);
        if(tsmo<tinc || tsmo>t(end) || isnan(tsmo) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for tsmo';
        end
        parmlist{5}='tsmo';parmlist{6}=tsmo;
        %fsmo
        hobj=findobj(hpanel,'tag','fsmo');
        val=get(hobj,'string');
        fsmo=round(str2double(val));
        if(fsmo<0 || fsmo>.5/(t(2)-t(1)) || isnan(fsmo) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for fsmo';
        end
        parmlist{7}='fsmo';parmlist{8}=fsmo;
        %stab
        hobj=findobj(hpanel,'tag','stab');
        val=get(hobj,'string');
        stab=str2double(val);
        if(stab<0 || stab>1 || isnan(stab) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for stab';
        end
        parmlist{9}='stab';parmlist{10}=stab;
        %ihyp
        hobj=findobj(hpanel,'tag','ihyp');
        val=get(hobj,'string');
        ihyp=str2double(val);
        if(ihyp~=0 && ihyp~=1 || isnan(ihyp) )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for ihyp';
        end
        parmlist{11}='ihyp';parmlist{12}=ihyp;
        %phase
        hobj=findobj(hpanel,'tag','phase');
        val=get(hobj,'string');
        phase=str2double(val);
        if(phase~=0 && phase~=1 || isnan(phase)  )
            nerr=nerr+1;
            errmsgs{nerr}='invalid value for phase';
        end
        parmlist{13}='phase';parmlist{14}=phase;
        
        if(nerr>0)
            parmlist=[];
            msgbox(errmsgs,'Oh oh... Errors in your parameters');
            return;
        end
        
end
end


function thisparm=findparm(parm,parmlist)
nargs=length(parmlist);
thisparm=[];
for k=1:2:nargs
    if(strcmp(parmlist{k},parm))
        thisparm=parmlist{k+1};
    end
end

end


function plotresult(seisd,t,x,sref,tref,xref,kol)
%the only reason for the existence of this function is to reset the axes
%tags. For some annoying reason, Matlab resets the tags to null when you
%plot in the axes. Otherwise, plotting of both input and output is the same
%and uses plot_tf
hseisout=findobj(gcf,'tag','seisout');
hfreqout=findobj(gcf,'tag','freqout');
hclip=findobj(gcf,'tag','clipout');
iclip=get(hclip,'value');
cliplvls=get(hclip,'userdata');
clip=cliplvls(iclip);
plot_tf(seisd,t,x,sref,tref,xref,kol,hseisout,hfreqout,clip,0,0);
set(hseisout,'tag','seisout');
set(hfreqout,'tag','freqout');
end


function plot_tf(seis,t,x,sref,tref,xref,kol,hseis,hfreq,clip,flag_t,flag_f)
%plotting either input or output
% flag_t ... 0 for trplot, 1 for imagesc
% flag_f ... 0 for ave spectra, 1 for individual spectra, 2 for fx
% kol ... color for time domain seismic

if(flag_t==1)
   error('flag_t not implemented'); 
end

happly=findobj(gcf,'tag','apply');
udat=get(happly,'userdata');
is1=abs(udat{7});

%determine if output
output=0;
if(strcmp(kol,'r'))
    output=1;
end
axes(hseis)
cla;
ntraces=length(x);
xmin=min([x(:); xref]);
xmax=max([x(:); xref]);
xspan=xmax-xmin;
xwid=xspan/ntraces;
%normalize and scale the traces
% for k=1:ntraces
%     s=seis(:,k);
%     s=s/max(s);
% end
seis=seis/max(seis(:));
sref=sref/max(sref);
sigma=std(seis(:));
a=(xwid/2)/(clip*sigma);
smax=xwid;
hcntxt=uicontextmenu;
uimenu(hcntxt,'label','Waveex simple','callback','decon_workbench(''waveexsimple'')');
uimenu(hcntxt,'label','Waveex match','callback','decon_workbench(''waveexmatch'')');
uimenu(hcntxt,'label','Waveex Roy White','callback','decon_workbench(''waveexrw'')');
for k=1:ntraces
    s=seis(:,k)*a;
    ind=find(s>smax);
    if(~isempty(ind))
        s(ind)=smax;
    end
    ind=find(s<-smax);
    if(~isempty(ind))
        s(ind)=-smax;
    end
    h=line(s+x(k),t,'color',kol);
    set(h,'uicontextmenu',hcntxt,'userdata',k,'tag','s');
    if(output && k==is1)
        set(h,'linewidth',3*get(h,'linewidth'));
%     elseif(~output && is1==-k)
%         set(h,'linewidth',3*get(h,'linewidth'));
    end
end
sref=sref*a;
ind=find(sref>smax);
if(~isempty(ind))
    sref(ind)=smax;
end
ind=find(sref<-smax);
if(~isempty(ind))
    sref(ind)=-smax;
end
line(sref+xref,tref,'color','k','tag','ref');
grid on
set(gca,'ydir','reverse');
xmin=min([x xref])-xwid;
xmax=max([x xref])+xwid;
xlim([xmin xmax])

%trplot(t,seis,'direction','v','yaxis','on','normalize',1)

plot_f(seis,t,hfreq,flag_f);

% if(output==1)
%     decon_workbench('welltie');
% end

end


function plot_f(seis,t,hfreq,flag_f)
hfmax=findobj(gcf,'tag','specfmax');
val=get(hfmax,'value');
factors=get(hfmax,'userdata');
axes(hfreq)
tag=get(hfreq,'tag');
[t1,t2,t3,t4]=getspectralwindows;
if(flag_f==0)
    %window 1
    iw1=near(t,t1,t2);
    %window 2
    iw2=near(t,t2,t3);
    %window 3
    iw3=near(t,t3,t4);
    %compute spectra
    [nt,ntraces]=size(seis);
    for k=1:ntraces
        [S,f]=fftrl(seis(:,k),t,10,2^nextpow2(nt));
        [S1,f1]=fftrl(seis(iw1,k),t(iw1),10,2^nextpow2(nt));
        [S2,f2]=fftrl(seis(iw2,k),t(iw2),10,2^nextpow2(nt));
        [S3,f3]=fftrl(seis(iw3,k),t(iw3),10,2^nextpow2(nt));
        if(k==1)
            A=abs(S);
            A1=abs(S1);
            A2=abs(S2);
            A3=abs(S3);
        else
            A=A+abs(S);
            A1=A1+abs(S1);
            A2=A2+abs(S2);
            A3=A3+abs(S3);
        end
    end
    A=A/ntraces;
    A1=A1/ntraces;
    A2=A2/ntraces;
    A3=A3/ntraces;
    Amax=max(A);
    plot(f,todb(A,Amax),f1,todb(A1,Amax),f2,todb(A2,Amax),f3,todb(A3,Amax));
    ylim([-100 0])
    xlim([f(1) f(end)*factors(val)])
    legend('Total spectrum',[time2str(t1) ' to ' time2str(t2) 's'],...
        [time2str(t2) ' to ' time2str(t3) 's'],...
        [time2str(t3) ' to ' time2str(t4) 's'],...
        'location','southwest')
    grid on
    xlabel('frequency (Hz)');
    ylabel('decibels')
    title('Average spectra')
    set(hfreq,'tag',tag)
end
end


function fparms=getfilterparms(t)
hfilterpanel=findobj(gcf,'tag','filter');
errmsgs=cell(1,3);
errmsgs{3}='correct filter parameters and try again';
nerr=0;
fparms=cell(1,10);
%flow
hobj=findobj(hfilterpanel,'tag','flow');
val=get(hobj,'string');
flow=str2double(val);
fnyq=.5/(t(2)-t(1));
if(flow<0 || flow>fnyq || isnan(flow))
    nerr=nerr+1;
    errmsgs{1}='flow is invalid';
end
fparms{1}='flow';fparms{2}=flow;
%fhigh
hobj=findobj(hfilterpanel,'tag','fhigh');
val=get(hobj,'string');
fhigh=str2double(val);
if(fhigh<flow || fhigh>fnyq || isnan(fhigh))
    nerr=nerr+1;
    errmsgs{2}='fhigh is invalid';
end
fparms{3}='fhigh';fparms{4}=fhigh;
%phase
hobj=findobj(hfilterpanel,'tag','minphase');
phase=get(hobj,'value');
fparms{5}='phase';fparms{6}=phase;
%order
if(phase==1)
    norder=8;
else
    norder=4;
end
fparms{7}='norder';fparms{8}=norder;
%apply
hobj=findobj(hfilterpanel,'tag','dofilt');
apply=get(hobj,'value');
fparms{9}='apply';fparms{10}=apply;

if(nerr>0)
    fparms=[];
    msgbox(errmsgs,'Oh oh');
    return;
end
end


function fillpanel(thisalg,hpanel,parms)
switch thisalg
    case 'deconf'
       %tstart
       hobj=findobj(hpanel,'tag','tstart');
       tstart=findparm('tstart',parms);
       set(hobj,'string',num2str(tstart));
       %tend
       hobj=findobj(hpanel,'tag','tend');
       tend=findparm('tend',parms);
       set(hobj,'string',num2str(tend));
       %fsmo
       hobj=findobj(hpanel,'tag','fsmo');
       fsmo=findparm('fsmo',parms);
       set(hobj,'string',num2str(fsmo));
       %stab
       hobj=findobj(hpanel,'tag','stab');
       stab=findparm('stab',parms);
       set(hobj,'string',num2str(stab));
       %phase
       hobj=findobj(hpanel,'tag','phase');
       phase=findparm('phase',parms);
       set(hobj,'string',int2str(phase));
    case 'deconw'
       %tstart
       hobj=findobj(hpanel,'tag','tstart');
       tstart=findparm('tstart',parms);
       set(hobj,'string',num2str(tstart));
       %tend
       hobj=findobj(hpanel,'tag','tend');
       tend=findparm('tend',parms);
       set(hobj,'string',num2str(tend));
       %top
       hobj=findobj(hpanel,'tag','top');
       top=findparm('top',parms);
       set(hobj,'string',num2str(top));
       %stab
       hobj=findobj(hpanel,'tag','stab');
       stab=findparm('stab',parms);
       set(hobj,'string',num2str(stab));
        
    case 'deconb'
       %tstart
       hobj=findobj(hpanel,'tag','tstart');
       tstart=findparm('tstart',parms);
       set(hobj,'string',num2str(tstart));
       %tend
       hobj=findobj(hpanel,'tag','tend');
       tend=findparm('tend',parms);
       set(hobj,'string',num2str(tend));
       %top
       hobj=findobj(hpanel,'tag','top');
       top=findparm('top',parms);
       set(hobj,'string',num2str(top));
        
    case 'tvsw'
       %aeclen
       hobj=findobj(hpanel,'tag','aec');
       aeclen=findparm('aeclen',parms);
       set(hobj,'string',num2str(aeclen));
       %flow
       hobj=findobj(hpanel,'tag','flow');
       flow=findparm('flow',parms);
       set(hobj,'string',num2str(flow));
       %fhigh
       hobj=findobj(hpanel,'tag','fhigh');
       fhigh=findparm('fhigh',parms);
       set(hobj,'string',num2str(fhigh));
       %nfilt
       hobj=findobj(hpanel,'tag','nfilt');
       nfilt=findparm('nfilt',parms);
       set(hobj,'string',num2str(nfilt));
       %tstart
       hobj=findobj(hpanel,'tag','tstart');
       tstart=findparm('tstart',parms);
       set(hobj,'string',num2str(tstart));
        
    case 'Gabor decon'
       %twin
       hobj=findobj(hpanel,'tag','twin');
       twin=findparm('twin',parms);
       set(hobj,'string',num2str(twin));
       %tinc
       hobj=findobj(hpanel,'tag','tinc');
       tinc=findparm('tinc',parms);
       set(hobj,'string',num2str(tinc));
       %tsmo
       hobj=findobj(hpanel,'tag','tsmo');
       tsmo=findparm('tsmo',parms);
       set(hobj,'string',num2str(tsmo));
       %fsmo
       hobj=findobj(hpanel,'tag','fsmo');
       fsmo=findparm('fsmo',parms);
       set(hobj,'string',num2str(fsmo));
       %stab
       findobj(hpanel,'tag','stab');
       stab=findparm('stab',parms);
       set(hobj,'string',num2str(stab));
       %ihyp
       findobj(hpanel,'tag','ihyp');
       ihyp=findparm('ihyp',parms);
       set(hobj,'string',num2str(ihyp));
       %phase
       findobj(hpanel,'tag','phase');
       phase=findparm('phase',parms);
       set(hobj,'string',num2str(phase));
end
end


function fillfilterpanel(parms)
    %find the filter panel
    hpanel=findobj(gcf,'tag','filter');
    %flow
    hobj=findobj(hpanel,'tag','flow');
    flow=findparm('flow',parms);
    set(hobj,'string',num2str(flow));
    %fhigh
    hobj=findobj(hpanel,'tag','fhigh');
    fhigh=findparm('fhigh',parms);
    set(hobj,'string',num2str(fhigh));
    %phase
    hobj=findobj(hpanel,'tag','minphase');
    phase=findparm('phase',parms);
    set(hobj,'value',phase);
    hobj=findobj(hpanel,'tag','zerophase');
    set(hobj,'value',~phase);
    %apply
    hobj=findobj(hpanel,'tag','dofilt');
    fapply=findparm('apply',parms);
    set(hobj,'value',fapply);
    hobj=findobj(hpanel,'tag','nofilt');
    set(hobj,'value',~fapply);

end


function [t1,t2,t3,t4]=getspectralwindows
    line1=findobj(gcf,'tag','win1');
    if(isempty(line1))
        %windows do not exist yet, define default windows
        happly=findobj(gcf,'tag','apply');
        udat=get(happly,'userdata');
        t=udat{2};
        twin=t(end)/4;
        t1=t(end)/8;
        t2=t1+twin;
        t3=t2+twin;
        t4=t3+twin;
        return;
    end
    line2=findobj(gcf,'tag','win2');
    line3=findobj(gcf,'tag','win3');
    line4=findobj(gcf,'tag','win4');
    tt=get(line1,'ydata');
    t1=tt(1);
    tt=get(line2,'ydata');
    t2=tt(1);
    tt=get(line3,'ydata');
    t3=tt(1);
    tt=get(line4,'ydata');
    t4=tt(1);
end


function [td1,td2]=getdesigngate
    line1=findobj(gcf,'tag','td1');
    if(isempty(line1))
        %gate not defined yet, define default gate
        happly=findobj(gcf,'tag','apply');
        udat=get(happly,'userdata');
        t=udat{2};
        td1=t(end)/4;
        td2=td1+.5*t(end);
        return;
    end
    line2=findobj(gcf,'tag','td2');
    tt=get(line1,'ydata');
    td1=tt(1);
    tt=get(line2,'ydata');
    td2=tt(1);
end


function [tcc1,tcc2]=getcorrelationgate
    line1=findobj(gcf,'tag','tcc1');
    if(isempty(line1))
        %gate not defined yet, define default gate
        happly=findobj(gcf,'tag','apply');
        udat=get(happly,'userdata');
        tref=udat{5};
        tcc1=tref(1);
        tcc2=tref(end);
        return;
    end
    line2=findobj(gcf,'tag','tcc2');
    tt=get(line1,'ydata');
    tcc1=tt(1);
    tt=get(line2,'ydata');
    tcc2=tt(1);
end


function setdesigngate(from,td1,td2)
% from: one of 'line', 'text', 'result'
% td1,td2: only used if from is 'result'
happly=findobj(gcf,'tag','apply');
udat=get(happly,'userdata');
t=udat{2};
halg=findobj(gcf,'tag','algorithms');
udat=get(halg,'userdata');
hpanels=udat{1};
ialg=get(halg,'value');
hseisin=findobj(gcf,'tag','seisin');
switch from
    case 'line'
        hline1=findobj(hseisin,'tag','td1');
        hline2=findobj(hseisin,'tag','td2');
        tt=get(hline1,'ydata');
        td1=tt(1);
        tt=get(hline2,'ydata');
        td2=tt(1);
        
    case 'text'
        %determine which panel the value came from
        hthispanel=hpanels{ialg};
        tstart=findobj(hthispanel,'tag','tstart');
        val=get(tstart,'string');
        td1=str2double(val);
        tend=findobj(hthispanel,'tag','tend');
        val=get(tend,'string');
        td2=str2double(val);
        errmsg=[];
        if(isempty(td1))
            errmsg='bad design gate start time';
        elseif(isempty(td2))
            errmsg='bad design gate end time';
        elseif(td1>td2)
            errmsg='design gate start time cannont be greater than end time';
        elseif(td1<t(1))
            errmsg=['design gate start time cannot be less than ' num2str(t(1))];
        elseif(td2>t(end))
            errmsg=['design gate end time cannot be greater than ' num2str(t(end))];
        end
        if(~isempty(errmsg))
            msgbox(errmsg,'Oh oh');
            return;
        end
        
end
%ajust td1 and td2 to nearest samples
it=near(t,td1);
td1=t(it);
it=near(t,td2);
td2=t(it);
%ok now we set the gate in panels 1, 2, and 3 and in both lines
%panel1
hthispanel=hpanels{1};
hobj=findobj(hthispanel,'tag','tstart');
set(hobj,'string',num2str(td1));
hobj=findobj(hthispanel,'tag','tend');
set(hobj,'string',num2str(td2));
%panel2
hthispanel=hpanels{2};
hobj=findobj(hthispanel,'tag','tstart');
set(hobj,'string',num2str(td1));
hobj=findobj(hthispanel,'tag','tend');
set(hobj,'string',num2str(td2));
%panel3
hthispanel=hpanels{3};
hobj=findobj(hthispanel,'tag','tstart');
set(hobj,'string',num2str(td1));
hobj=findobj(hthispanel,'tag','tend');
set(hobj,'string',num2str(td2));
%line1
hline1=findobj(hseisin,'tag','td1');
set(hline1,'ydata',[td1 td1]);
%line2
hline2=findobj(hseisin,'tag','td2');
set(hline2,'ydata',[td2 td2]);
end


function wmethod=getwaveletmethod
hsimple=findobj(gcf,'tag','simple');
hmatch=findobj(gcf,'tag','match');
hroy=findobj(gcf,'tag','roywhite');
wmethod=[];
if(get(hsimple,'value')==1)
    wmethod='simple';
elseif(get(hmatch,'value')==1)
    wmethod='match';
elseif(get(hroy,'value')==1);
    wmethod='roywhite';
end

end


function wparms=getwaveletparms(t)
hwavepanel=findobj(gcf,'tag','waveex');
errmsgs=cell(1,3);
errmsgs{3}='correct wavelet parameters and try again';
nerr=0;
wparms=cell(1,12);
%winsize
hobj=findobj(hwavepanel,'tag','winsize');
val=get(hobj,'string');
winsize=str2double(val);
if(winsize<0 || winsize>(t(end)-t(1)) || isnan(winsize))
    nerr=nerr+1;
    errmsgs{1}='winsize is invalid';
end
wparms{1}='winsize';wparms{2}=winsize;
%wavesize
hobj=findobj(hwavepanel,'tag','wavesize');
val=get(hobj,'string');
wavesize=str2double(val);
if(wavesize<0 || wavesize>1 || isnan(wavesize))
    nerr=nerr+1;
    errmsgs{2}='wavesize is invalid';
end
wparms{3}='wavesize';wparms{4}=wavesize;
%match filter smoothness
hobj=findobj(hwavepanel,'tag','mumatch');
val=get(hobj,'string');
mu=str2double(val);
if(mu<0 || isnan(mu))
    nerr=nerr+1;
    errmsgs{2}='smoothness (match filter) is invalid';
end
wparms{5}='smoothnessmatch';wparms{6}=mu;
%Roy White smoothness
hobj=findobj(hwavepanel,'tag','muroy');
val=get(hobj,'string');
mu=str2double(val);
if(mu<0 || isnan(mu))
    nerr=nerr+1;
    errmsgs{2}='smoothness (Roy White) is invalid';
end
wparms{7}='smoothnessroy';wparms{8}=mu;
%stability
hobj=findobj(hwavepanel,'tag','wstab');
val=get(hobj,'string');
stab=str2double(val);
if(stab<0 || stab>1 || isnan(stab))
    nerr=nerr+1;
    errmsgs{2}='wavelet stability is invalid';
end
wparms{9}='stability';wparms{10}=stab;
%fsmo
hobj=findobj(hwavepanel,'tag','fsmo');
val=get(hobj,'string');
fsmo=str2double(val);
fnyq=.5/(t(2)-t(1));
if(fsmo<0 || fsmo>fnyq || isnan(fsmo))
    nerr=nerr+1;
    errmsgs{2}='frequency smoother is invalid';
end
wparms{11}='fsmo';wparms{12}=fsmo;

if(nerr>0)
    wparms=[];
    msgbox(errmsgs,'Oh oh');
    return;
end
end


function [wlets,tws,t0s,twin]=estimate_wavelets(s,t,sref,tref)
wmethod=getwaveletmethod;
%first get the spectral windows. We will do one wavelet per window
[t1,t2,t3,t4]=getspectralwindows;
%adjust the windows in case we do not have a full reference trace in each
if(t1<tref(1))
    t1=tref(1);
end
if(t2<tref(1))
    t2=tref(1);
end
if(t3<tref(1))
    t3=tref(1);
end
if(t2>tref(end))
    t2=tref(end);
end
if(t3>tref(end))
    t3=tref(end);
end
if(t4>tref(end))
    t4=tref(end);
end

%get the wavelet params
wparms=getwaveletparms(t);
twin=findparm('winsize',wparms);
wsize=findparm('wavesize',wparms);
mum=findparm('smoothnessmatch',wparms);
mur=findparm('smoothnessroy',wparms);
stabw=findparm('stability',wparms);
fsmo=findparm('fsmo',wparms);
rwmethod='three';
%next estimate wavelets before well tying
ind=near(t,tref(1),tref(end));
t0s=[.5*(t1+t2) .5*(t2+t3) .5*(t3+t4)];
if(strcmp(wmethod,'match'))
    [wlets,tws]=extract_wavelets_match(s(ind),t(ind),sref,t0s,twin,wsize,mum);
elseif(strcmp(wmethod,'roywhite'))
    [wlets,tws]=extract_wavelets_roywhite(s(ind),t(ind),sref,t0s,twin,wsize,mur,stabw,fsmo,rwmethod);
else
    [wlets,tws]=extract_wavelets_simple(s(ind),t(ind),sref,t0s,twin,fsmo,wsize);
end
end

function [wlets,tws,t0s,twin]=compare_wavelets(s,t,sref,tref)
wlets=cell(1,9);
tws=wlets;
%first get the spectral windows. We will do one wavelet per window
[t1,t2,t3,t4]=getspectralwindows;
%adjust the windows in case we do not have a full reference trace in each
if(t1<tref(1))
    t1=tref(1);
end
if(t2<tref(1))
    t2=tref(1);
end
if(t3<tref(1))
    t3=tref(1);
end
if(t2>tref(end))
    t2=tref(end);
end
if(t3>tref(end))
    t3=tref(end);
end
if(t4>tref(end))
    t4=tref(end);
end

%get the wavelet params
wparms=getwaveletparms(t);
twin=findparm('winsize',wparms);
wsize=findparm('wavesize',wparms);
mum=findparm('smoothnessmatch',wparms);
mur=findparm('smoothnessroy',wparms);
stabw=findparm('stability',wparms);
fsmo=findparm('fsmo',wparms);
rwmethod='three';
%next estimate wavelets before well tying
ind=near(t,tref(1),tref(end));
t0s=[.5*(t1+t2) .5*(t2+t3) .5*(t3+t4)];

[wlets(4:6),tws(4:6)]=extract_wavelets_match(s(ind),t(ind),sref,t0s,twin,wsize,mum);

[wlets(7:9),tws(7:9)]=extract_wavelets_roywhite(s(ind),t(ind),sref,t0s,twin,wsize,mur,stabw,fsmo,rwmethod);

[wlets(1:3),tws(1:3)]=extract_wavelets_simple(s(ind),t(ind),sref,t0s,twin,fsmo,wsize);

end


function thisresult=getcurrentresult
hresults=findobj(gcf,'tag','results');
udat=get(hresults,'userdata');
results=udat{2};
iresult=get(hresults,'value');
nresults=length(results);
if(nresults==0)
    thisresult=[];
else
    thisresult=results{iresult};
end
end


function savecurrentresult(thisresult)
hresults=findobj(gcf,'tag','results');
udat=get(hresults,'userdata');
results=udat{2};
iresult=get(hresults,'value');
results{iresult}=thisresult;
udat{2}=results;
set(hresults,'userdata',udat);
% if(nresults==0)
%     thisresult=[];
% else
%     thisresult=results{iresult};
% end
end


function fillwaveletpanel(method,parms)
%get the panel
hwpan=findobj(gcf,'tag','waveex');
switch method
    case 'simple'
        hobj=findobj(hwpan,'tag','simple');
        set(hobj,'value',1);
    case 'match'
        hobj=findobj(hwpan,'tag','match');
        set(hobj,'value',1);
    case 'roywhite'
        hobj=findobj(hwpan,'tag','roywhite');
        set(hobj,'value',1);
end
decon_workbench('waveletmethod');
%get the parameters
winsize=findparm('winsize',parms);
wavesize=findparm('wavesize',parms);
mumatch=findparm('smoothnessmatch',parms);
muroy=findparm('smoothnessroy',parms);
wstab=findparm('stability',parms);
fsmo=findparm('fsmo',parms);
%winsize
hobj=findobj(hwpan,'tag','winsize');
set(hobj,'string',num2str(winsize));
%wavesize
hobj=findobj(hwpan,'tag','avesize');
set(hobj,'string',num2str(wavesize));
%mumatch
hobj=findobj(hwpan,'tag','mumatch');
set(hobj,'string',num2str(mumatch));
%muroy
hobj=findobj(hwpan,'tag','muroy');
set(hobj,'string',num2str(muroy));
%stability
hobj=findobj(hwpan,'tag','wstab');
set(hobj,'string',num2str(wstab));
%fsmo
hobj=findobj(hwpan,'tag','fsmo');
set(hobj,'string',num2str(fsmo));
end


function plotwelltie(t,tvphase,tvdelay,sreftie,tref,s1tie,twin,kol)
htvphs=findobj(gcf,'tag','tvphase');
htvdelay=findobj(gcf,'tag','tvdelay');
hwelltie=findobj(gcf,'tag','welltie');
axes(htvphs);
%disp('plotting phase')
plot(tvphase,t);
xlabel('deg')
title('tv phase')
set(htvphs,'ydir','reverse','tag','tvphase','ygrid','on','xgrid','on');
xlim([-180 180])
xtick([-100 0 100])
axes(htvdelay);
%disp('plotting delay')
plot(tvdelay,t);
title('tv delay')
xlabel('sec')
%tsmin=min(abs(tstretch));
tsmax=max(abs(tvdelay));
xlnom=.1;
xlmax=ceil(tsmax/xlnom)*xlnom;
xlim([-xlmax xlmax])

%     xl=get(htvdelay,'xlim');
%     xl=max(abs(xl));
%     if(xl<.03)
%         xlim([-.03 .03])
%     end
set(htvdelay,'ydir','reverse','tag','tvdelay');
set(htvdelay,'yticklabel',[],'ygrid','on','xgrid','on');
axes(hwelltie)
[tcc1, tcc2]=getcorrelationgate;
%disp('plotting seismograms')
hh=plot(sreftie,tref,'k',s1tie+1,t,kol);
set(hh(1),'tag','well');
set(hh(2),'tag','seis');
ind1=near(tref,tcc1,tcc2);
ind2=near(t,tcc1,tcc2);
cc=maxcorr(sreftie(ind1),s1tie(ind2));
dt=t(2)-t(1);
xl=[-1 2];
xlim(xl)
%draw cc gate
kol=[0 .7 0];
line(xl,[tcc1 tcc1],[1 1],'linestyle',':','color',kol,'buttondownfcn',...
    'decon_workbench(''dragline'')','tag','tcc1','linewidth',2);
line(xl,[tcc2 tcc2],[1 1],'linestyle',':','color',kol,'buttondownfcn',...
    'decon_workbench(''dragline'')','tag','tcc2','linewidth',2);
title({'well tie',['cc=' num2str(sigfig(cc(1),2)) ', shift=' time2str(sigfig(cc(2),2)*dt)]})
set(hwelltie,'ydir','reverse','yticklabel',[],'xticklabel',[],...
    'tag','welltie','ygrid','on');

end


function plotwavelets(wletsb,twsb,wletsa,twsa,t,tref)
%now plot
%first get the spectral windows.
[t1,t2,t3,t4]=getspectralwindows;
wparms=getwaveletparms(t);
twin=findparm('winsize',wparms);
t1a=max([.5*(t1+t2-twin),tref(1)]);
t1b=min([.5*(t1+t2+twin),tref(end)]);
t2a=max([.5*(t2+t3-twin),tref(1)]);
t2b=min([.5*(t2+t3+twin),tref(end)]);
t3a=max([.5*(t3+t4-twin),tref(1)]);
t3b=min([.5*(t3+t4+twin),tref(end)]);
names={[num2str(t1a) ' to ' num2str(t1b) 's'],[num2str(t2a) ' to ' num2str(t2b) 's'],...
    [num2str(t3a) ' to ' num2str(t3b) 's']};
haxb=findobj(gcf,'tag','wavelets_before');
axes(haxb)
hh=trplot(twsb,wletsb,'order','d','zerolines','y');
legend(cell2mat(hh),names,'location','southwest');
title({'wavelets', 'before tying'})
set(haxb,'tag','wavelets_before');
xl=get(gca,'xlim');
xtick([-xl(2)*.5 0 xl(2)*.5])
haxa=findobj(gcf,'tag','wavelets_after');
axes(haxa)
trplot(twsa,wletsa,'order','d','zerolines','y');
title({'wavelets','after tying'})
set(haxa,'tag','wavelets_after');
xl=get(gca,'xlim');
xtick([-xl(2)*.5 0 xl(2)*.5])
%determine legend status
hlegendbutton=findobj(gcf,'tag','waveletlegend');
val=get(hlegendbutton,'userdata');
if(val==0)
    legend(haxb,'hide');
end

%drawnow;
end
function comparewaveletstime(wavelets)
    %plot the wavelet comparison in time
    %rw before
    wletsb=wavelets.wletsb;
    twsb=wavelets.twsb;
    names=wavelets.statsb;
    axes(wavelets.haxes(1));
    trplot(twsb(7:9),wletsb(7:9),'zerolines','y','tracespacing',1.5,'order','d','names',names(7:9),...
        'nameslocation','middle','namesalign','center','namesshift',.25);
    title('Roy White before')
    yl=get(gca,'ylim');
    %match before
    axes(wavelets.haxes(2));
    trplot(twsb(4:6),wletsb(4:6),'zerolines','y','tracespacing',1.5,'order','d','names',names(4:6),...
        'nameslocation','middle','namesalign','center','namesshift',.25);
    set(gca,'xticklabel',[]);
    title('Match before')
    xlabel('')
    yl2=get(gca,'ylim');
    if(yl2(1)<yl(1))
        yl(1)=yl2(1);
    end
    if(yl2(2)>yl(2))
        yl(2)=yl2(2);
    end
    %simple before
    axes(wavelets.haxes(3));
    trplot(twsb(1:3),wletsb(1:3),'zerolines','y','tracespacing',1.5,'order','d','names',names(1:3),...
        'nameslocation','middle','namesalign','center','namesshift',.25);
    set(gca,'xticklabel',[]);
    title('Simple before');
    xlabel('')
    yl2=get(gca,'ylim');
    if(yl2(1)<yl(1))
        yl(1)=yl2(1);
    end
    if(yl2(2)>yl(2))
        yl(2)=yl2(2);
    end
    set(wavelets.haxes(1:3),'ylim',yl);
    %rw after
    wletsa=wavelets.wletsa;
    twsa=wavelets.twsa;
    names=wavelets.statsa;
    axes(wavelets.haxes(4));
    trplot(twsa(7:9),wletsa(7:9),'zerolines','y','tracespacing',1.5,'order','d','names',names(7:9),...
        'nameslocation','middle','namesalign','center','namesshift',.25);
    title('Roy White after')
    yl=get(gca,'ylim');
    %match after
    axes(wavelets.haxes(5));
    trplot(twsa(4:6),wletsa(4:6),'zerolines','y','tracespacing',1.5,'order','d','names',names(4:6),...
        'nameslocation','middle','namesalign','center','namesshift',.25);
    set(gca,'xticklabel',[]);
    title('Match after')
    xlabel('')
    yl2=get(gca,'ylim');
    if(yl2(1)<yl(1))
        yl(1)=yl2(1);
    end
    if(yl2(2)>yl(2))
        yl(2)=yl2(2);
    end
    %simple after
    axes(wavelets.haxes(6));
    trplot(twsa(1:3),wletsa(1:3),'zerolines','y','tracespacing',1.5,'order','d','names',names(1:3),...
        'nameslocation','middle','namesalign','center','namesshift',.25);
    set(gca,'xticklabel',[]);
    title('Simple after');
    xlabel('')
    yl2=get(gca,'ylim');
    if(yl2(1)<yl(1))
        yl(1)=yl2(1);
    end
    if(yl2(2)>yl(2))
        yl(2)=yl2(2);
    end
    set(wavelets.haxes(4:6),'ylim',yl)
end
function comparewaveletsfreq(wavelets)
    %plot the wavelet comparison in frequency
    dbmax=80;
    %rw before
    wletsb=wavelets.wletsb;
    twsb=wavelets.twsb;
    axes(wavelets.haxes(1));
    dbspec(twsb(7:9),wletsb(7:9));
    title('Roy White before')
    ylim([-dbmax 0])
    %match before
    axes(wavelets.haxes(2));
    dbspec(twsb(4:6),wletsb(4:6));
    set(gca,'xticklabel',[]);
    title('Match before')
    xlabel('')
    ylim([-dbmax 0])
    %simple before
    axes(wavelets.haxes(3));
    dbspec(twsb(1:3),wletsb(1:3));
    set(gca,'xticklabel',[]);
    title('Simple before');
    xlabel('')
    ylim([-dbmax 0])
    %rw after
    wletsa=wavelets.wletsa;
    twsa=wavelets.twsa;
    axes(wavelets.haxes(4));
    dbspec(twsa(7:9),wletsa(7:9));
    title('Roy White after')
    ylim([-dbmax 0])
    set(gca,'yticklabel',[]);ylabel('');
    %match after
    axes(wavelets.haxes(5));
    dbspec(twsa(4:6),wletsa(4:6));
    set(gca,'xticklabel',[]);
    title('Match after')
    xlabel('')
    set(gca,'yticklabel',[]);ylabel('');
    ylim([-dbmax 0])
    %simple after
    axes(wavelets.haxes(6));
    dbspec(twsa(1:3),wletsa(1:3));
    set(gca,'xticklabel',[]);
    title('Simple after');
    xlabel('')
    set(gca,'yticklabel',[]);ylabel('');
    ylim([-dbmax 0])
end