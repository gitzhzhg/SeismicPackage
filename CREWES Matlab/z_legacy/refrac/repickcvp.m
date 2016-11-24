function repickcvp(action)
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

% Determination of the Repicking of cross over point parameter
if( nargin < 1 )
   action = 'init';
end
if( strcmp(action,'init'))
   q=str2mat( 'Enter the polynomial degree for the CVP averages polyfit ?','Repick CVP according to how much deviation from the polyfit curve?');
 a=str2mat('5','50');
   askthingsinit('repickcvp(''answer'')',q,a,[1 1],'Parameter for the Repick CVP function');
elseif( strcmp(action,'answer'))
   a=askthingsfini;
   [strings tmp] = size(a);
 
   cvpi=refdata('get','cvpi');
   cvpj=refdata('get','cvpj');
   nshots=refdata('get','nshots');
   shotcoord=refdata('get','shotcoord');
   fbcoord=refdata('get','fbcoord');
   fbtime=refdata('get','fbtime');
   nd=refdata('get','nd');
   windmn = refdata('get', 'windmn');
   window=refdata('get','window');
   % Call the CVP averaging function
   [cvpavg cvpstd cvpfold]=avgcvp(cvpi,cvpj,nshots);
   f=gcf;
      % Plot the CVP averages with the Polyfit curves
      poly=str2num(a(1,:));
      goodleft=find(~isnan(cvpavg(:,1)));
      goodright=find(~isnan(cvpavg(:,2)));
      offsetleft=shotcoord(goodleft)-cvpavg(goodleft,1);
      offsetright=cvpavg(goodright,2)-shotcoord(goodright);
      l=polyfit(goodleft,offsetleft,poly);
      r=polyfit(goodright,offsetright,poly);
      left=polyval(l,goodleft);
      right=polyval(r,goodright);
      figure('menubar','none')
      hold on
      plot(goodleft,offsetleft,'co')
      plot(goodleft,left,'c')
      plot(goodright,offsetright,'r*')     
      plot(goodright,right,'r')
      title('Cross over point offset average from each shot for the left side (blue circle) and for the right side (red star)')
      ylabel('Offset(m)')
      xlabel('Shot number')
      figure(f); set(gcf,'menubar','none');
      % Find the CVP that are outside of the polyfit deviation limit 
      dev=str2num(a(2,:));
      cvpis = cvpi;
      cvpjs = cvpj;
      nn=1;
      % Find left CVP
      for n=goodleft'
        validcvpi = find(~isnan(cvpis(n,:)));
        [a b]= size(validcvpi);
        if( b ~=0 )
          jj=find( abs((shotcoord(n)-cvpis(n,validcvpi))-left(nn)) >dev );
          j=validcvpi(jj);
          offset(1)=left(nn)-dev;
          offset(2)=left(nn)+dev;
          gs1=n;
          for gs2=j
            % Repick the bad left CVP and limit the offset according to the
            % polyfit deviation limit
            oldcvp = cvpis(gs1,gs2);
	    [cvpi,cvpj]=autopick(gs1,gs2,shotcoord,fbcoord,fbtime,...
            window, nshots, nd, offset, windmn);
            fprintf(1,'Changed CVP(%d,%d) from %f to %f\n',gs1,gs2,...
            oldcvp,cvpi(n,gs2));
            cvpis(n,gs2) = cvpi(n,gs2);
          end
        end
        nn=nn+1;
      end
      refdata('set','cvpi',cvpis);
      mm=1;
      % Find the right CVP
      for n=goodright'
        validcvpj = find(~isnan(cvpjs(:,n)));
        [a b]= size(validcvpj);
        if( a ~=0 )
          ii=find( abs((cvpjs(validcvpj,n)-shotcoord(n))-right(mm)) >dev );
          i=validcvpj(ii)';
          offset(1)=right(mm)-dev;
          offset(2)=right(mm)+dev;
          gs2=n;
          for gs1=i
            % Repick the bad right CVP and limit the offset according to the
            % polyfit deviation limit
            oldcvp = cvpjs(gs1,gs2);
            [cvpi,cvpj]=autopick(gs1,gs2,shotcoord,fbcoord,fbtime,...
            window, nshots, nd, offset, windmn);
            fprintf(1,'Changed CVP(%d,%d) from %f to %f\n',gs1,gs2,...
            oldcvp,cvpj(gs1,n));
            cvpjs(gs1,n) = cvpj(gs1,n);
          end
        end
        mm=mm+1;
      end
      refdata('set','cvpj',cvpjs);
   cvpi=refdata('get','cvpi');
   cvpj=refdata('get','cvpj');
   % Recalculate the CVP average with the new CVP left and CVP right
   [cvpavg cvpstd cvpfold]=avgcvp(cvpi,cvpj,nshots);
   refdata('set','cvpavg',cvpavg);
   % Update menus
   PMTsetmenus;
   f=gcf;
   % Plot the new CVP average with new Polyfit curves of same polynomial degre
   goodleft=find(~isnan(cvpavg(:,1)));
   goodright=find(~isnan(cvpavg(:,2)));
   offsetleft=shotcoord(goodleft)-cvpavg(goodleft,1);
   offsetright=cvpavg(goodright,2)-shotcoord(goodright);
   l=polyfit(goodleft,offsetleft,poly);
   r=polyfit(goodright,offsetright,poly);
   left=polyval(l,goodleft);
   right=polyval(r,goodright);
   figure('menubar','none')
   hold on
   plot(goodleft,offsetleft,'co')
   plot(goodleft,left,'c')
   plot(goodright,offsetright,'r*')     
   plot(goodright,right,'r')
   title('Cross over point offset average after Repicking for the left side (blue circle) and for the right side (red star)')
   ylabel('Offset(m)')
   xlabel('Shot number')
   figure(f); set(gcf,'menubar','none');
end