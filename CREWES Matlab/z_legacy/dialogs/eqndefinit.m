function eqndefinit(transfer,hmasterfig,lognames,logdefaults,coefs,...
		exps,newlogdes)
% eqndefinit(transfer,hmasterfig,lognames,logdefaults,coefs,...
%		exps,newlogdes)
%
% EQNDEFINIT is called by LOGEDIT to initiate a dialog  to define the
% computation of a new log from existing ones via an algebraic equation.
% The equation is of the form ((a*l1^k + b*l2^m)/(c*l3^n+d*l4^o))^p 
% Here l1,l2,l3,l4 are logs and the other terms are scalar constants. The
% dialog allows the definition of the logs, the scalars, and the destination of
% of the computed log.
%
% transfer ... transfer command to be called when the user terminates
%		the dialog
% hmasterfig ... handle of the masterfigure (usually a LOGEDIT window) in
%		control of the dialog
% lognames ... list of names of logs with individual names separated by '|' 
%		such as: 'fred|sam|billy|wilma'
% logdefaults ... vector of length four containing four integers which are the 
% 		defaults for the four logs. These are taken to be row indicies into
%		lognames
% coefs ... vector of length four containing the defaults for a,b,c, and d
% exps ... vector of length five containing the defaults for k,m,n,o, and p
% newlogdes ... default descriptor for the new log. (Its "name" will
%	be automatically implied by the mnemonic form its type.)
%
%
% G.F. Margrave, Jan 1996
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
% pack the information into the current axes userdata
hax=get(hmasterfig,'currentaxes');
figure(hmasterfig);
set(hax,'userdata',{transfer, lognames, hmasterfig, logdefaults,...
	coefs, exps, newlogdes});
eqndef('init')