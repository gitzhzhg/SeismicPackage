function plotseismic(seis,t,x,kol)
% PLOTSEISMIC: A wtva seismic viewer with GUI controls
%
% plotseismic(seis,t,x,kol)
%
% PLOTSEISMIC is similar to PLOTSEIS except that it creates a new figure
% window and includes UI controls for things such as scaling, phase
% rotation, comparison between windows, etc. Like PLOTSEIS, It plots the
% columns as wiggle traces centered at their x coordinates. Alternatives
% are PLOTSEIS which does not create a new figure, and PLOTIMAGE. If you
% have more than 100 traces, PLOTIMAGE is usually preferred.  If you have
% many similar gathers to compare, try PLOTGATHERS.
%
% seis ... input seismic matrix, one trace per column
% t ... column vector of times, one entry per row of seis
%      ********** default = 1:size(seis,1) ************
% x ... row vector of distances, one entry per column of seis
%      ********** default = 1:size(seis,2) ************
% kol ... color to draw traces with
%      ********** default = 'b' (blue) **********
% 
% G.F. Margrave, CREWES, 2001-2014
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

global XLSEIS YLSEIS
        
if(nargin<1)
    error('plotseismic requires at least one input');
elseif(nargin==1)
  if(ischar(seis))
     action=seis;
  else
     action='init';
     t=1:size(seis,1);
     x=1:size(seis,2);
     kol='b';
   end
elseif(nargin<3)
   action='init';
   x=1:size(seis,2);
   kol='b';
elseif(nargin<4)
    action='init';
    kol='b';
elseif(nargin==4)
    action='init';
else
   error('plotseismic can have no more than four input arguments');
end


if(strcmp(action,'init'))
	pscale=2;
	cliplvl=1;
       
	hfig=figure;
    
	%set(hfig,'menubar','none')
            
	%add a close button, a hardcopy button, a msg panel, and install simple zooming
	
		sep=1;
		xnow=sep;
		ynow=sep;
		width=40;
		height=20;
		hclose=uicontrol('style','pushbutton','string','Close','position',...
			[xnow,ynow,width,height],'callback','close(gcf)','visible','off');
		
		%xnow=xnow+width+sep;
		
        width=110;
        hamp=uicontrol('style','popupmenu','string','Amp: independent|Amp: master|Amp: slave',...
            'value',1,'position',[xnow,ynow,width,height],'callback','plotseismic(''replot'')',...
            'tooltipstring','Specify amplitude scaling control');
         
        % make an ampllitude control
        xnow=xnow+width+sep;
        width=200;
        ampstring=['Amp lvl ' num2str(pscale)];
        fsize=get(0,'factoryuicontrolfontsize');
        hamplabel=uicontrol('style','text','fontsize',fsize,'string',ampstring,'position',...
            [xnow,ynow+height/2,width,2*height/3],'tooltipstring','Current value of amplitude level');
        hampslider=uicontrol('style','slider','string','Amplevel','position',...
            [xnow,ynow,width,height/2],'callback','plotseismic(''champ'')',...
            'tooltipstring','Change the seismic amplitude level','max',100,'min',.2,...
            'tag','amplevel','value',pscale,'userdata',hamplabel,'sliderstep',[0.01/9.8 .05/9.8]);
		
       
        xnow=xnow+width+sep;
        width=80;
        fsize=get(0,'factoryuicontrolfontsize');
        hlabel=uicontrol('style','text','fontsize',fsize,'string','Phase 0','position',...
            [xnow,ynow+height/2,width,2*height/3],'tooltipstring','Current value of seismic phase');
        hslider=uicontrol('style','slider','string','Phase','position',...
            [xnow,ynow,width,height/2],'callback','plotseismic(''phase'')',...
            'tooltipstring','Rotate the seismic phase','max',180,'min',-180,...
            'tag','phase','value',0,'userdata',hlabel,'sliderstep',[1/360 10/360]);
            
         xnow=xnow+width+sep;
        width=40;
        polarity=1;
        hpolbut=uicontrol('style','pushbutton','string','Polarity','position',...
            [xnow,ynow,width,height],'callback','plotseismic(''polarity'')',...
            'tooltipstring','Flip the seismic polarity','userdata',polarity);
        black=[0 0 0];white=[1 1 1];
        grey=[.7529 .7529 .7529];
         if( polarity==-1 )
            set(hpolbut,'backgroundcolor',black,'foregroundcolor',white)
        end
        % the value of polarity is stored in the menu hpolarity
        
		xnow=xnow+width+sep;
		width=300;
        button=1;
		
	    msg= 'MB1: drag to zoom,  click to unzoom';
		
        %this is now retired. Its invisible instead of deleted because its userdata is needed
		hmsg=uicontrol('style','text','string',msg,'visible','off',...
			'position',[xnow,ynow,width,height]);
        %hmessage=uimenu(hfig,'label',msg,'selectionhighlight','off','hittest','off');
        set(gcf,'name',['PLOTSEISMIC ... ' msg])
       %  set(gcf,'name',['PLOTSEISMIC ... ' msg],'numbertitle','off')
        
        width=60;
        hbigwhite=uicontrol('style','pushbutton','string','Bigfont etc.',...
            'position',[xnow,ynow,width,height],'callback','plotseismic(''bigfontetc'')',...
            'tooltipstring','Double fontsize and linewidth, white background','tag','to');
        %hbigwhite's userdata is used in the callback
        xnow=xnow+width+sep;
        %clipboard button
        width=50;
        if(ispc)
            hcopyclip=uicontrol('style','pushbutton','string','Clipboard','position',...
                [xnow,ynow,width,height],'callback','plotseismic(''toclip'')',...
                'tooltipstring','Copy figure to the Windows clipboard');
        else
           hcopyclip=uicontrol('style','pushbutton','string','Tiff','position',...
                [xnow,ynow,width,height],'callback','plotseismic(''toclip'')',...
                'tooltipstring','Create tiff file'); 
        end
        
        
        xnow=xnow+width+sep;
        width=35;
        htitbut=uicontrol('style','pushbutton','string','Retitle','position',...
            [xnow,ynow,width,height],'callback','plotseismic(''retitle'')',...
            'tooltipstring','Change the figure title','visible','off');
        
        %set(htitbut,'userdata',hist);
        
        xnow=xnow+width+sep;
        width=35;	
        yscale=10;
        %button below is retire. I made it invisible in case its userdata
        %is used somewhere
		hhardcopy=uicontrol('style','pushbutton','string','Print','position',...
			[xnow,ynow,width,height],'callback','plotseismic(''print'')',...
			'userdata',yscale,'tooltipstring','Controlled-scale hardcopy',...
            'visible','off');
        
			
		set(gcf,'userdata',{hclose,hhardcopy,hmsg,0,0,0,0,...
                0,0,0,0,0,0,0,hamp,0,htitbut,hcopyclip,hbigwhite,hslider,...
                0,hpolbut,hlabel hampslider hamplabel});
                
        %add a context menu to both axes
         hcntx=uicontextmenu;
         
         hpub=uimenu(hcntx,'label','Publish Zoom Limits','callback','plotseismic(''pubzoom'')','separator','on',...
                'enable','on');
         hmatch=uimenu(hcntx,'label','Match Zoom','callback','plotseismic(''matchzoom'')','enable','on');
		set(gca,'UIcontextmenu',hcntx);
		simplezoom(button,'plotseismic(''finzoom'')');
        
        
        %store seismic data
        %chek seismic for nans
        ind=find(isnan(seis));
        if(~isempty(ind))
            seis(ind)=0;
        end
        set(hclose,'userdata',{seis t x })
        
        set(hamp,'userdata',[pscale cliplvl]);
        
        %the userdata of hlabel is reserved for the Hilbert transform
        set(hmsg,'userdata',kol);

        plotseismic('replot');
        yl=get(gca,'ylim');
        xl=get(gca,'xlim');
        set(htitbut,'userdata',[xl yl]);
        return;
    elseif(strcmp(action,'finzoom'))
        h=get(gcf,'userdata');
        htitbut=h{17};
        xl=get(gca,'xlim');
        yl=get(gca,'ylim');
        dat=get(htitbut,'userdata');
        if(xl(2)>dat(2)); 
            xl=dat(1:2);
            set(gca,'xlim',xl);
        end
        if(xl(1)<dat(1)); 
            xl=dat(1:2);
            set(gca,'xlim',xl);
        end
        if(yl(2)>dat(4)); 
            yl=dat(3:4);
            set(gca,'ylim',yl);
        end
        h{5}=xl(1);h{6}=xl(2);
        h{7}=yl(1);h{8}=yl(2);
        set(gcf,'userdata',h)
        
        %set(gca,'xlim',dat(1:2),'ylim',dat(3:4));
        %disp('done zoom')
        %xl=h(5:6);
        %set(gca,'xlim',xl);
        %no action required
         return;
    elseif(strcmp(action,'print'))
        h=get(gcf,'userdata');
        hclose=h{1};
        hhard=h{2};
        hmsg=h{3};
        hseis=gca;

        hamp=h{15};

        htitbut=h{17};
        hcopyclip=h{18};
        hbigfont=h{19};
        hpolbut=h{22};
        hlabel=h{23};
        hslider=h{20};
        hamplabel=h{25};
        hampslider=h{24};

        yscale = get(hhard,'userdata');

        %ask for scale info
        q={'Timing scale (inches/second)'};
        a={num2str(yscale)};
        answer = askthingsle('questions',q,'answers',a,'Enter plotting scale');
        if isempty(answer)
          return;
        end
        yscale=sscanf(answer{1},'%G');
        printhist=0;

        set(hhard,'userdata',yscale)%save the new scale

       % y dimensions
       ylim=get(hseis,'ylim');%yaxis limits
       ysize=yscale*abs(diff(ylim));%required size of y axis
       set(hseis,'units','inches');%set y units to inches
       ps=get(hseis,'position');
       ysizeold=ps(4);
       ps(4)=ysize;

       set(hseis,'position',ps);
   
       set(hmsg,'visible','off')
       
       %see if the figure must be resized
       resize=0;
       if(ysize>ysizeold | printhist)
           resize=1;
           funits=get(gcf,'units');
           pfig=get(gcf,'position');
           if(ysize>ysizeold)
                set(gcf,'units','normalized');
                pfig2=get(gcf,'position');
                pfig3=[pfig2(1) 0 pfig2(3) 1];
                set(gcf,'position',pfig3);
            end
            
           set(gcf,'units',funits)
       end
       
       set(hclose,'visible','off');
       set(hhard,'visible','off');
       set(hamp,'visible','off');
       set(htitbut,'visible','off');
       set(hcopyclip,'visible','off');
       set(hbigfont,'visible','off');
       set(hslider,'visible','off');
       set(hlabel,'visible','off');
       set(hpolbut,'visible','off');
       set(hamplabel,'visible','off');
       set(hampslider,'visible','off');

       
       %determine papersize
       punits=get(gcf,'paperunits');
       set(gcf,'paperunits','inches');
       ppos=get(gcf,'paperposition');
       psize=get(gcf,'papersize');
       ysizepaper=1.2*ysize;
       if(ysizepaper>ppos(4))
           ymargin=min([abs(psize(2)-ysizepaper), .5]);
           newppos=ppos;
           newppos(2)=ymargin/2;
           newppos(4)=ysizepaper;
           set(gcf,'paperposition',newppos);
%            if(newppos(4)+ymargin>psize(2))
%                npages=ceil((newppos(4)+ymargin)/psize(2));
%                newpsize=psize;
%                newpsize(2)=psize(2)*npages;
%                set(gcf,'papersize',newpsize)
%            end
       end
       
       printdlg
       
       
       set(hclose,'visible','on');
       set(hhard,'visible','on');
       set(hamp,'visible','on');
       set(htitbut,'visible','on');
       set(hcopyclip,'visible','on');
       set(hbigfont,'visible','on');
       set(hslider,'visible','on');
       set(hlabel,'visible','on');
       set(hpolbut,'visible','on');
       set(hamplabel,'visible','on');
       set(hampslider,'visible','on');
       
       ps(4)=ysizeold;
       
        if(resize)
            set(gcf,'position',pfig);
            set(gcf,'units',funits)
        end
        
       
       set(hseis,'position',ps);
       set(hmsg,'visible','off');
 
        
       set(hseis,'units','normalized');
 
       
       set(gcf,'paperposition',ppos)
       set(gcf,'papersize',psize);
       set(gcf,'paperunits',punits);
       return
   elseif(strcmp(action,'replot'))
        h=get(gcf,'userdata');
        hclose=h{1};
        hhard=h{2};
        hmsg=h{3};
        hseis=gca;
        hamp=h{15};
        hslider=h{20};
        hpolbut=h{22};
        hlabel=h{23};
        hampslider=h{24};
        xlim=[h{5} h{6}];
        ylim=[h{7} h{8}];
        %determine current polarity
        polarity=get(hpolbut,'userdata');
        
        phase=get(hslider,'value');
        
        hax=get(gcf,'currentaxes');
        set(gcf,'currentaxes',hseis)
        
        ampfactors=get(hamp,'userdata');
        
        %determine amplitude option
        flag=get(hamp,'value');
        if(flag==1) 
            fact=ampfactors(1);
        elseif(flag==2)
            fact=[ampfactors(1) -1];
        elseif(flag==3)
            fact=[ampfactors(1) -2];
        end
        
        cliplvl=ampfactors(2);
        
        pcolor=get(hmsg,'userdata');
        
        %get the data
        stuff=get(hclose,'userdata');
        seis=stuff{1};
        t=stuff{2};
        x=stuff{3};
        
        cla
        
        if(phase~=0)
            seis2=get(hlabel,'userdata');
            if(isempty(seis2))
                seis2=hilbert(seis);
                set(hlabel,'userdata',seis2);
            end
            seis=cos(pi*phase/180)*real(seis2)+sin(pi*phase/180)*imag(seis2);
        end
        
        plotseis( polarity*seis, t, x, 1, fact, cliplvl, 1, pcolor, hseis );
        set(gca,'xaxislocation','bottom')
        if(xlim(1))
            set(gca,'xlim',xlim,'ylim',ylim);
        end
        xlim=get(gca,'xlim');
        ylim=get(gca,'ylim');
        h{5}=xlim(1);h{6}=xlim(2);
        h{7}=ylim(1);h{8}=ylim(2);
        set(gcf,'userdata',h);

    elseif(strcmp(action,'toclip'))
        if(ispc)
%             print -dbitmap
            fh=gcf;
            fh.Renderer='opengl';
            hgexport(fh,'-clipboard');
        else
            print -dtiff
        end
    elseif(strcmp(action,'bigfontetc'))
        hbigfont=gcbo;
        flag=get(hbigfont,'tag');
        if(strcmp(flag,'to'))
            set(hbigfont,'string','Normfont','tooltipstring','Restore font, linewidth, and color',...
                'tag','from');
            whitefig;
            bigfont(gcf,1.5);
            boldlines(gcf,2);
            siz=get(gcf,'position');
            %screen=get(0,'screensize');
            %set(gcf,'position',[screen(1:2) .95*screen(3:4)])
            %set(gcf,'position',[screen])
            bigfig;
            set(hbigfont,'userdata',siz);
        else
           set(hbigfont,'string','Bigfont etc','tooltipstring',...
               'Double fontsize and linewidth, white background','tag','to');
            greyfig;
            bigfont(gcf,1/1.5,1);
            boldlines(gcf,.5);
            siz=get(hbigfont,'userdata');
            set(gcf,'position',siz);
        end
    elseif(strcmp(action,'retitle'))
        htit=get(gca,'title');
        titstr=get(htit,'string');
        newtitstr=inputdlg('Specify new title','PLOTSEISMIC wants to know',1,{titstr});
        
        title(newtitstr{1});
    elseif(strcmp(action,'polarity'))
        h=get(gcf,'userdata');
        hclose=h{1};
        hpolbut=gcbo;
        %determine current polarity
        polarity=get(hpolbut,'userdata');
        polarity=-polarity;%flipit
        set(hpolbut,'userdata',polarity);
        txt1='Polarity                         ';
        black=[0 0 0];white=[1 1 1];
        %grey=[.7529 .7529 .7529];
        %grey=[0.8314    0.8157    0.7843];
        grey=get(hclose,'backgroundcolor');
         if( polarity==1 )
         	txt2='Normal';
            set(hpolbut,'backgroundcolor',grey,'foregroundcolor',black)
         elseif( polarity==-1 )
            txt2='Reverse';
            set(hpolbut,'backgroundcolor',black,'foregroundcolor',white)
         else
         	txt2='Crazy';
         end
         %txt=[txt1 txt2];
         %set(hpol,'label',txt);
         plotseismic('replot')
     elseif(strcmp(action,'phase'))
         h=get(gcf,'userdata');
         hmenu=h{21};
         hslider=gcbo;
         phase=round(get(hslider,'value'));
         hlabel=get(hslider,'userdata');
         set(hlabel,'string',['Phase ' int2str(phase)])
         %hphase=findobj(hmenu,'tag','phase');
         %txt1='Phase                            ';
         %txt2=int2str(phase);
         %set(hphase,'label',[txt1 txt2]);
         plotseismic('replot');
     elseif(strcmp(action,'champ'))
         h=get(gcf,'userdata');
         
         hamp=h{15};
         hslider=gcbo;
         pscale=get(hslider,'value');
         hlabel=get(hslider,'userdata');
         if(pscale<10&pscale>=1)
            set(hlabel,'string',['Amp lvl ' num2str(sigfig(pscale,2))])
         else
            set(hlabel,'string',['Amp ' num2str(sigfig(pscale,2))])
         end
         %hpscale=findobj(hmenu,'tag','pscale');
%          txt1='Plot scale                   ';
%          txt2=num2str(pscale);
%          set(hpscale,'label',[txt1 txt2]);
         ampfactors=get(hamp,'userdata');
         ampfactors(1)=pscale;
         set(hamp,'userdata',ampfactors)
         plotseismic('replot');
     elseif(strcmp(action,'hist'))
         h=get(gcf,'userdata');
         htitbut=h{17};
         hmenu=h{21};
         hkids=get(hmenu,'children');
         for k=1:length(hkids)
             hist{k}=get(hkids(k),'label');
         end
         set(htitbut,'userdata',hist);
     elseif(strcmp(action,'pubzoom'))       
        XLSEIS=get(gca,'xlim');
        YLSEIS=get(gca,'ylim');
        
        return;
elseif(strcmp(action,'matchzoom'))

        h=get(gcf,'userdata');
        
        if(isempty(XLSEIS))
           msgbox('You must first ''publish'' one window''s Zoom limits before you can Match Zoom');
           return
        end
        
        set(gca,'xlim',XLSEIS);
        set(gca,'ylim',YLSEIS);
%         h(5:8)=[XLSEIS YLSEIS];
        h{5}=XLSEIS(1);h{6}=XLSEIS(2);
        h{7}=YLSEIS(1);h{8}=YLSEIS(2);
        set(gcf,'userdata',h);

        return;
    end
            
        