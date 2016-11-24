function [lognums,coefs,exps,type,des]=eqndeffini

% [lognums,coefs,exps,type,des]=eqndeffini
%
% Call this to complete the Log algebra equation definition dialog. 
% Return values mean:
%  lognums = vector of length four giving the numbers of the four logs
%  coefs = vector of length four giving the scalar coeficients of the four logs
%  exps = vector of length five giving the scalar exponents of the four 
%         logs and the overall exponent
% type = type flag for the new log
% des = string descriptor for the new log
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

	params=get(gca,'userdata');
	if( params==-1 ) %test for a cancel
		lognums=-1;coefs=[];exps=[];type=[];des=[];
		return;
	end

	lognums=params(1:4);
	coefs=params(5:8);
	exps=params(9:13);
	type=params(14);
	
	des= setstr(params(15:length(params)));