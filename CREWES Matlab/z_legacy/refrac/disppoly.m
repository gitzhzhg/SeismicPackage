function disppoly(action)
% Determination of the parameter for the polyfit of the
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

% cross over point averages
if( nargin < 1 )
   action = 'init';
end
if( strcmp(action,'init'))
   q=str2mat('Enter the polynomial degree:');
 a=str2mat('5');
   askthingsinit('disppoly(''answer'')',q,a,[1],'Parameter for the CVP average Polyfit function');
elseif( strcmp(action,'answer'))
   a=askthingsfini;
   [strings tmp] = size(a);
   cvpi=refdata('get','cvpi');
   cvpj=refdata('get','cvpj');
   nshots=refdata('get','nshots');
   shotcoord=refdata('get','shotcoord');
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
      plot(goodleft,left,'c-.')
      plot(goodright,offsetright,'r*')     
      plot(goodright,right,'r')
      title('Cross over point offset average from each shot for the left side (blue circle) and for the right side (red star)')
      ylabel('Offset(m)')
      xlabel('Shot number')
      set(gcf,'units','pixels','position',[0 0 864 576]);
      figure(f)
end