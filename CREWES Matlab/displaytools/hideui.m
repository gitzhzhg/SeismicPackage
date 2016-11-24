function hh=hideui(handin)
% HIDEUI ... hide (or restore) user interface controls
%
% hh=hideui(figno) ... hides the uicontrols on figure figno
% hideui(hh) ... restores the uicontrols whos handles are in hh
%
% G.F. Margave, CREWES, 2000
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

%

if(nargin<1); handin=gcf; end

flag=get(handin(1),'type');

if(strcmp(flag,'figure'))
   hhi=get(handin(1),'children');
   hh=nan*ones(size(hhi));
   for kk=1:length(hhi)
	   tp=get(hhi(kk),'type');
       if(strcmp(tp,'uicontrol'))
           flg=get(hhi(kk),'visible');
           if(strcmp(flg,'on'))
               hh(kk)=hhi(kk);
               set(hhi(kk),'visible','off');
           end
       end
       if(strcmp(tp,'uibuttongroup'))
           flg=get(hhi(kk),'visible');
           if(strcmp(flg,'on'))
               hh(kk)=hhi(kk);
               set(hhi(kk),'visible','off');
               hk=get(hhi(kk),'children');
               for k=1:length(hk)
                   set(hk(k),'visible','off');
               end
           end
       end
       if(strcmp(tp,'uipanel'))
           flg=get(hhi(kk),'visible');
           if(strcmp(flg,'on'))
               hh(kk)=hhi(kk);
               set(hhi(kk),'visible','off');
               hk=get(hhi(kk),'children');
               for k=1:length(hk)
                   set(hk(k),'visible','off');
               end
           end

       end
           
       if(strcmp(tp,'axes'))
          axflag=get(hhi(kk),'tag');
          if(strcmp(axflag,'POSITIONAXES'))  
               hhc=get(hhi(kk),'children');
               set(hhi(kk),'visible','off');
               hh(kk)=hhi(kk);
               for kkk=1:length(hhc)
                   if(crisgraphics(hhc( kkk ),'image'))
                       flg=get(hhc(kkk),'visible');
                       if(strcmp(flg,'on'))
                         set(hhc(kkk),'visible','off');
                       end
                   end
               end
          end
          if(strcmp(axflag,'MAINAXES'))
              psn=get(hhi(kk),'position');
              if(psn(1)==.2910)
                  psn(1)=0.100;
                  psn(3)=.8440;
              end
              set(hhi(kk),'position',psn);
              hh(kk)=hhi(kk);
          end
       end
   end

   ind=find(isnan(hh));
   hh(ind)=[];
else
   for k=1:length(handin)
      tg=get(handin(k),'tag');
      if(strcmp(tg,'MAINAXES'))
          psn=get(handin(k),'position');
              psn(1)=0.291;
              psn(3)=.6860;
          set(handin(k),'position',psn);
      elseif(strcmp(tg,'POSITIONAXES'))
          hhc=get(handin(k),'children');
           set(handin(k),'visible','on');
           for kkk=1:length(hhc)
               if(crisgraphics(hhc( kkk ),'image'))
                   flg=get(hhc(kkk),'visible');
                   if(strcmp(flg,'off'))
                     set(hhc(kkk),'visible','on');
                   end
               end
           end
      else
          set(handin(k),'visible','on');
      end
   end
   hh=[];
end
	