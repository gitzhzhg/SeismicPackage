function wlog=l_reflection_coefficients(wlog,varargin)
% Function computes reflection acoustic reflection coefficients in depth and
% adds them as a new curve
% By default, the function assumes that acoustic impedance, density, interval 
% transit time and/or compressional velocity have the standard mnemonics. 
% This can be changed by temporarily redefining the standard mnemonics via 
% function arguments; e.g. {'DTp','DTCO'} specifies that the mnemonic of the 
% sonic log is 'DTCO' rather 'DTp'. Standard mnemonics are defined in 
% function "presets0" (global variable "CURVES")
% If a velocity curve does not exist it is created from the sonic (standard mnemonics) 
%
% Written by: E. Rietsch, February 16, 2001
% Last updated: February 17, 2001: Default mnemonic CURVES.arefl for new output curve
%
%         wlog=l_reflection_coefficients(wlog,varargin)
% INPUT
% wlog    log structure with density curve and sonic and/or velocity curve
% varargin one or more cell arrays; the first element of each cell array is 
%          a keyword string; the following arguments contains a parameter(s). 
%          Accepted keywords are:
%      'action' describes how to handle new curve. Possible values are:
%             'add'     gives an error message if a curve with this mnemonic already exists
%             'replace' gives an error message if a curve with this mnemonic does not exist
%             'add_ne'  (add --- no error) adds a new curve or replaces an existing one
%             Default: {'action','add_ne'}
%      'description'  string with curve description.
%             Default: {'description','Acoustic reflectivity'}
% OUTPUT
% wlog   log structure with reflectivity curve added (default mnemonic: 
%        string stored in "CURVES.arefl"); of course, this can be changed by 
%        temporarily redefining the standard mnemonics, 

%       Set defaults for input parameters
def.action='add_ne';
def.description='Acoustic reflectivity';

%       Decode and assign input arguments
[def,cm]=l_assign_input(def,varargin);

%       Check if necessary log curves exist
[ivp,ier]=curve_index1(wlog,cm.vp);
if ier
   [idtp,ier]=curve_index1(wlog,cm.dtp);
   if ier
      disp([' Neither sonic log "',cm.dtp,'" nor velocity log "',cm.vp,'" are present'])
      disp(' Available curves:')
      disp(cell2str(wlog.curve_info(:,1),', '))
      error(' Abnormal termination')
   end
   vp=1.0e6./wlog.curves(:,idtp);
   if strcmp(wlog.curve_info{idtp,2},'us/ft')
      vunits='ft/s';
   elseif strcmp(wlog.curve_info{idtp,2},'us/m')
      vunits='m/s';
   else
      error([' Unknown units of measurement for sonic log :',wlog.curve_info{idtp,2}])
   end
   wlog=l_curve(wlog,'add',cm.vp,vp,vunits,'Compressional velocity');
else
   vp=wlog.curves(:,ivp);
end

[iaimp,ier]=curve_index1(wlog,cm.aimp);
if ier
   [irho,ier]=curve_index1(wlog,cm.rho);
   if ier
      disp([' Neither density log "',cm.rho,'" nor acoustic impedance log "',cm.aimp,'" are present'])
      disp(' Available curves:')
      disp(cell2str(wlog.curve_info(:,1),', '))
      error(' Abnormal termination')
   end
  
   aimp=vp.*wlog.curves(:,irho);

else
   aimp=wlog.curves(:,iaimp);
end

%       Compute reflection coefficients
temp=diff(aimp)./(aimp(1:end-1)+aimp(2:end));
if strcmp(wlog.curve_info{1,2},'m') || strcmp(wlog.curve_info{1,2},'ft')
   refl=temp.*(vp(1:end-1)+vp(2:end))./diff(wlog.curves(:,1));
   if (strcmp(wlog.curve_info{1,2},'m') && strcmp(l_gu(wlog,cm.vp),'m/s')) || ...
      (strcmp(wlog.curve_info{1,2},'ft') && strcmp(l_gu(wlog,cm.vp),'ft/s'))
      refl=refl/4000;

   elseif strcmp(wlog.curve_info{1,2},'m') && strcmp(l_gu(wlog,cm.vp),'ft/s')
      refl=refl*0.0000762;

   elseif strcmp(wlog.curve_info{1,2},'ft') && strcmp(l_gu(wlog,cm.vp),'m/s')
      refl=refl/304.8; 

   else
      error([' Unknown depth (',wlog.curve_info{1,2},') or velocity (',l_gu(wlog,cm.vp),') units'])
   end
 
elseif strcmp(wlog.curve_info{1,2},'s') || strcmp(wlog.curve_info{1,2},'ms')
   if wlog.step > 0
      refl=temp/wlog.step;
   else
      refl=temp./diff(wlog.curves(:,1));
   end
   if strcmp(wlog.curve_info{1,2},'s')
      refl=refl/1000;
   end
 
else
   error(' Depth units must be either "m", "ft", "s", or "ms"')
end

%       Add reflectivity to (replace reflectivity in) log structure 
wlog=l_curve(wlog,def.action,cm.arefl,[refl;0],'n/a',def.description);





