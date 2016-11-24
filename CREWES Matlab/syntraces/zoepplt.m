function zoepplt(rho1,a1,b1,rho2,a2,b2,incwav,anginc,flag)
%
% zoepplt(rho1,a1,b1,rho2,a2,b2,incwav,anginc,flag)
% zoepplt(rho1,a1,b1,rho2,a2,b2,incwav,anginc)
% zoepplt(rho1,a1,b1,rho2,a2,b2,incwav)
%
% ZOEPPLT computes displacement reflection and transmission coefficients
% using the ZOEPPRITZ function. Zoepplt produces a plot of these 
% coefficients vs. angle of incidence, and will show all critical
% angles as dashed lines if so desired. 
%
% rho1    = density of the incidence medium
% a1      = p-wave velocity of incidence medium
% b1      = s-wave velocity of incidence medium
% rho2    = density of the transmission medium
% a2      = p-wave velocity of transmission medium
% b2      = s-wave velocity of transmission medium
% incwav  = 1 for incident p-wave 
%	  = 2 for incident s-wave
% anginc  = incidence angles (in degrees)
% ************ Default = 0 to 90 in steps of 1 degree *******************
%
% flag   = 1 shows the critical angle(s) as a dashed line on plot
%        = 2 shows critical angles plus numeric value
%	 = 3 does not show critical angle(s)
%************* Default = 1 (shows critical angles) **********************
%  
% NOTE: if b1 and b2 = 0, a liquid-liquid interface is treated
% NOTE: if b1 = 0 and b2 is non-zero, a liquid-solid interface is treated 
% NOTE: if b1 is non-zero and b2 = 0, a solid-liquid interface is treated
% NOTE: if a1 and a2 = 0, the incident SH wave case is treated
% 
% by Jeff Larsen, CREWES Project, May 1997
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

RCP='P wave reflection coefficient vs. angle of incidence';
RCS='S wave reflection coefficient vs. angle of incidence';
TCP='P wave transmission coefficient vs. angle of incidence';
TCS='S wave transmission coefficient vs. angle of incidence';
TC='TC';
RC='RC';
ANGINC='Angle of Incidence (degrees)';
inc=1;

if nargin<8
  anginc=0:90;
end

if nargin<9
  flag=2;
end

hfig = figcent(.5,.9);  
set(hfig,'menubar','none','units','normalized');
hmenu = uimenu(gcf,'label','Actions');
  hzoom = uimenu(hmenu,'label','Zoom','callback','simplezoom');

%================= Find the critical angles ==============================
if flag==1 | flag==2
	dpr = 180/pi;
	ca1=0; ca2=0; cb2=0; critat=0; critbt=0; critar=0;

	if incwav==1
		velincmed = a1;
	elseif incwav==2
		velincmed = b1;
	end
	if a1~=0
		ca1 = (asin(velincmed/a1))*dpr;
	end
	if a2~=0
		ca2 = (asin(velincmed/a2))*dpr;
	end
	if b2~=0
		cb2 = (asin(velincmed/b2))*dpr; 
	end
	if (real(ca1)==90)
		ca1 = 0;
	end
	if (real(ca2)==90)
		ca2 = 0;
	end
	if (real(cb2)==90)
		cb2 = 0;
	end
else 
	ca1=0; ca2=0; cb2=0; 
end

%============= Liquid-Liquid Interface ================================
if (a1>0)&(a2>0)&(b1==0)&(b2==0)
  if incwav==1  
  	newtitle='Liquid-Liquid Interface for an incident P wave';
       	for irfwav=1:2:3
  	coef=zoeppritz(rho1,a1,b1,rho2,a2,b2,incwav,irfwav,1,anginc);
  	coef=real(coef);
  	subplot(2,1,inc), plot(anginc,coef);
  	grid;
 	xlabel(ANGINC);
  	axis([min(anginc) max(anginc) min(coef) max(coef)]);
    	if irfwav==1
      		title(RCP,'VerticalAlignment','cap','fontweight','bold');
     		 ylabel(RC);
     		 elseif irfwav==3	
      		title(TCP,'VerticalAlignment','cap','fontweight','bold');
      		ylabel(TC);
      		if( ca2 )
      		   line([ca2 ca2],[min(coef) max(coef)],'color','r','LineStyle','--');
      		   if( flag==2 )
                     text(ca2,min(coef),num2str(ca2),'color',[0.6 0 .6], ...
                     'HorizontalAlignment','center','fontweight','bold');
                   end
      		end
        end
  	inc=inc+1;
  	end
  elseif incwav==2
  	close;
        error('You must have an incident P wave'); 
  end
%============ SH Wave case for a Solid-Solid Interface ==============
elseif (a1==0)&(a2==0)&(b1>0)&(b2>0)
  if incwav==2
 	newtitle='SH wave case for a Solid-Solid Interface';
  
 	for irfwav=2:2:4
  	coef=zoeppritz(rho1,a1,b1,rho2,a2,b2,incwav,irfwav,1,anginc);
  	coef=real(coef);
  	subplot(2,1,inc), plot(anginc,coef);
  	grid;
  	xlabel(ANGINC);
  	axis([min(anginc) max(anginc) min(coef) max(coef)]);
    		if irfwav==2
     		 title(RCS,'VerticalAlignment','cap','fontweight','bold');
     		 ylabel(RC);
      		elseif irfwav==4
      		title(TCS,'VerticalAlignment','cap','fontweight','bold');
      		ylabel(TC);
      		if( cb2 )
      		   line([cb2 cb2],[min(coef) max(coef)],'color','r','LineStyle','--');
      		   if( flag==2 )
             	     text(cb2,min(coef),num2str(cb2),'color',[0.6 0 .6], ...
                     'HorizontalAlignment','center','fontweight','bold');
                   end
      		end
    		end
  	inc=inc+1;
  	end
  elseif incwav==1
  	close;
  	error('You must have an incident S wave'); 
  end
%=================== Liquid-Solid Interface ========================= 
elseif (a1>0)&(a2>0)&(b1==0)&(b2>0)
  
  newtitle='Liquid-Solid Interface for an incident P wave';
  
  if incwav==1
         for irfwav=[1 3 4]
     	 coef=zoeppritz(rho1,a1,b1,rho2,a2,b2,incwav,irfwav,1,anginc);
     	 coef=real(coef);
     	 subplot(3,1,inc), plot(anginc,coef,'-');
     	 grid;
     	 xlabel(ANGINC);
     	 axis([min(anginc) max(anginc) min(coef) max(coef)]);	 
         if irfwav==1
        	  title(RCP,'VerticalAlignment','cap','fontweight','bold');
        	  ylabel(RC);
         	 elseif irfwav==3
          title(TCP,'VerticalAlignment','cap','fontweight','bold');
          ylabel(TC);
          if( ca2 )
             line([ca2 ca2],[min(coef) max(coef)],'color','r','LineStyle','--');
             if( flag==2 )
               text(ca2,min(coef),num2str(ca2),'color',[0.6 0 .6], ...
               'HorizontalAlignment','center','fontweight','bold');
             end
          end
          elseif irfwav==4
          title(TCS,'VerticalAlignment','cap','fontweight','bold');
          ylabel(TC);
          if( cb2 )
             line([cb2 cb2],[min(coef) max(coef)],'color','r','LineStyle','--');
             if( flag==2 )
               text(cb2,min(coef),num2str(cb2),'color',[0.6 0 .6], ...
               'HorizontalAlignment','center','fontweight','bold');
             end
          end
        end
      inc=inc+1;
    end
  elseif incwav==2
    close;
    error('You must have an incident P wave'); 
  end
%=================== Solid-Liquid Interface =========================
elseif (a1>0)&(a2>0)&(b1>0)&(b2==0)
   
   if incwav==1
   	newtitle='Solid-Liquid Interface for an incident P wave';
   elseif incwav==2
   	newtitle='Solid-Liquid Interface for an incident S wave';
   end
   
   for irfwav=1:3
     coef=zoeppritz(rho1,a1,b1,rho2,a2,b2,incwav,irfwav,1,anginc);
     coef=real(coef);
     subplot(3,1,irfwav), plot(anginc,coef);
     grid;
     xlabel(ANGINC);
     axis([min(anginc) max(anginc) min(coef) max(coef)]);		
       if irfwav==1
 	 title(RCP,'VerticalAlignment','cap','fontweight','bold');
 	 ylabel(RC);
 	 if( ca1 )
 	    line([ca1 ca1],[min(coef) max(coef)],'color','r','LineStyle','--');
 	    if( flag==2 )
              text(ca1,min(coef),num2str(ca1),'color',[0.6 0 .6], ...
              'HorizontalAlignment','center','fontweight','bold');
            end
 	 end
 	 elseif irfwav==2
 	 title(RCS,'VerticalAlignment','cap','fontweight','bold');
 	 ylabel(RC);
 	 elseif irfwav==3
 	 title(TCP,'VerticalAlignment','cap','fontweight','bold');
 	 ylabel(TC);
 	 if( ca2 )
 	    line([ca2 ca2],[min(coef) max(coef)],'color','r','LineStyle','--');
 	    if( flag==2 )
              text(ca2,min(coef),num2str(ca2),'color',[0.6 0 .6], ...
              'HorizontalAlignment','center','fontweight','bold');
            end
 	 end
       end
   end
%=================== Solid-Solid Interface ==========================   
elseif (b1>0)&(b2>0)&(a1>0)&(a2>0)
  
  if incwav==1
  	newtitle='Solid-Solid Interface for an incident P wave';
  elseif incwav==2
  	newtitle='Solid-Solid Interface for an incident S wave';
  end
    
  for irfwav=1:4
    coef=zoeppritz(rho1,a1,b1,rho2,a2,b2,incwav,irfwav,1,anginc);
    coef=real(coef);
    
    subplot(4,1,irfwav), plot(anginc,coef);
      if irfwav==1
      	   if( ca1 )
     	      line([ca1 ca1],[min(coef) max(coef)],'color','r','LineStyle','--');
     	      if( flag==2 )
       	        text(ca1,min(coef),num2str(ca1),'color',[0.6 0 .6], ...
                'HorizontalAlignment','center','fontweight','bold');
              end
     	   end
     	  title(RCP,'VerticalAlignment','cap','fontweight','bold');
      	  ylabel(RC);
      elseif irfwav==2
      	  title(RCS,'VerticalAlignment','cap','fontweight','bold'); 
      	  ylabel(RC);
      elseif irfwav==3
         if( ca2 )
       	    line([ca2 ca2],[min(coef) max(coef)],'color','r','LineStyle','--');
       	    if( flag==2 )
       	      text(ca2,min(coef),num2str(ca2),'color',[0.6 0 .6], ...
              'HorizontalAlignment','center','fontweight','bold');
            end
       	 end
         title(TCP,'VerticalAlignment','cap','fontweight','bold');
         ylabel(TC);
      elseif irfwav==4
         if( cb2 )
            line([cb2 cb2],[min(coef) max(coef)],'color','r','LineStyle','--');
            if( flag==2 )
              text(cb2,min(coef),num2str(cb2),'color',[0.6 0 .6], ...
              'HorizontalAlignment','center','fontweight','bold');
            end
         end
         title(TCS,'VerticalAlignment','cap','fontweight','bold');
         ylabel(TC);
      end
    grid;
    xlabel(ANGINC);
    axis([min(anginc) max(anginc) min(coef) max(coef)]);
    
    end
%====================================================================
orient tall; % gives better printing result
else
	close;
	error('Please check your input parameters again');	
end
set(hfig,'name',newtitle,'units','pixels');
	
 	