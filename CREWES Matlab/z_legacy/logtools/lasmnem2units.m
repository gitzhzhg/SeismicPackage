function units=lasmnem2units(mnem,metric)
% units=lasmnem2units(mnem,metric)
%
% Given a LAS log mnemonic return a string giving the standard
% units for that log to be in. Set metric to 1 for metric units,
% 0 for imperial units, and -1 for time. Algorithm currently
% returns 'UNKN' for all time log units
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
if(length(mnem)<4) mnem=[mnem blanks(4-length(mnem))]; end
mnem=upper(mnem);
%strip off any number from the end of the mnemonic
id=str2num(mnem(4));
if(~isempty(id))
		mnem=mnem(1:3);
end
lm=length(mnem);
units='UNKN';
if(metric==1)
	test='DEPT';
	if(strcmp(mnem,test(1:lm))) units='M   '; return; end
	test='AU  ';
	if(strcmp(mnem,test(1:lm))) units='US/M'; return; end
	test='DT  ';
	if(strcmp(mnem,test(1:lm))) units='US/M'; return; end
	test='SON ';
	if(strcmp(mnem,test(1:lm))) units='US/M'; return; end
	test='RHOB';
	if(strcmp(mnem,test(1:lm))) units='K/M3'; return; end
	test='RHGF';
	if(strcmp(mnem,test(1:lm))) units='K/M3'; return; end
	test='RHGA';
	if(strcmp(mnem,test(1:lm))) units='K/M3'; return; end
	test='GRC ';
	if(strcmp(mnem,test(1:lm))) units='GAPI'; return; end
	test='GR  ';
	if(strcmp(mnem,test(1:lm))) units='GAPI'; return; end
	test='SP  ';
	if(strcmp(mnem,test(1:lm))) units='MV  '; return; end
	test='CALI';
	if(strcmp(mnem,test(1:lm))) units='MM  '; return; end
	%don't know how to do shear wave sonic
	test='DTSW';
	if(strcmp(mnem,test(1:lm))) units='US/M'; return; end
	test='NPHI';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHIN';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHIA';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHID';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHID';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHIT';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='DPHI';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='SFLU';
	if(strcmp(mnem,test(1:lm))) units='OHMM'; return; end
	test='SFL ';
	if(strcmp(mnem,test(1:lm))) units='OHMM'; return; end
	test='ILM ';
	if(strcmp(mnem,test(1:lm))) units='OHMM'; return; end
	test='ILD ';
	if(strcmp(mnem,test(1:lm))) units='OHMM'; return; end
	test='SFLR';
	if(strcmp(mnem,test(1:lm))) units='OHMM'; return; end
	test='UNVI';
	if(strcmp(mnem,test(1:lm))) units='MM  '; return; end
	test='MNOR';
	if(strcmp(mnem,test(1:lm))) units='OHMM'; return; end
	test='MINV';
	if(strcmp(mnem,test(1:lm))) units='OHMM'; return; end
	test='DPSS';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
elseif(metric==0)
	test='DEPT';
	if(strcmp(mnem,test(1:lm))) units='FT  '; return; end
	test='AU  ';
	if(strcmp(mnem,test(1:lm))) units='US/F'; return; end
	test='DT  ';
	if(strcmp(mnem,test(1:lm))) units='US/F'; return; end
	test='SON ';
	if(strcmp(mnem,test(1:lm))) units='US/F'; return; end
	test='RHOB';
	if(strcmp(mnem,test(1:lm))) units='LB/F'; return; end
	test='RHGF';
	if(strcmp(mnem,test(1:lm))) units='LB/F'; return; end
	test='RHGA';
	if(strcmp(mnem,test(1:lm))) units='LB/F'; return; end
	test='GRC ';
	if(strcmp(mnem,test(1:lm))) units='GAPI'; return; end
	test='GR  ';
	if(strcmp(mnem,test(1:lm))) units='GAPI'; return; end
	test='SP  ';
	if(strcmp(mnem,test(1:lm))) units='MV  '; return; end
	test='CALI';
	if(strcmp(mnem,test(1:lm))) units='IN  '; return; end
	%don't know how to do shear wave sonic
	test='DTSW';
	if(strcmp(mnem,test(1:lm))) units='US/F'; return; end
	test='NPHI';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHIN';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHIA';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHID';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHID';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='PHIT';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='DPHI';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
	test='SFLU';
	if(strcmp(mnem,test(1:lm))) units='OHMF'; return; end
	test='SFL ';
	if(strcmp(mnem,test(1:lm))) units='OHMF'; return; end
	test='ILM ';
	if(strcmp(mnem,test(1:lm))) units='OHMF'; return; end
	test='ILD ';
	if(strcmp(mnem,test(1:lm))) units='OHMF'; return; end
	test='SFLR';
	if(strcmp(mnem,test(1:lm))) units='OHMF'; return; end
	test='UNVI';
	if(strcmp(mnem,test(1:lm))) units='IN  '; return; end
	test='MNOR';
	if(strcmp(mnem,test(1:lm))) units='OHMF'; return; end
	test='MINV';
	if(strcmp(mnem,test(1:lm))) units='OHMF'; return; end
	test='DPSS';
	if(strcmp(mnem,test(1:lm))) units='PU  '; return; end
end