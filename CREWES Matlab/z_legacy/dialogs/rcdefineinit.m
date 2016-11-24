function rcdefineinit(transfer,hmasterfig,sections,densopts,holealg,domain,...
			a,m,msg,sonicnum,densitynum,name)
% rcdefineinit(transfer,hmasterfig,sections,densopts,holealg,domain,a,m,msg...
%              sonicnum,densitynum,name)
%
% RCDEFINEINIT is called by LOGSEC to initiate a dialog  to define the
% computation of reflection coefficient sections from sonic log sections
% and, optionally, density log sections.
%
% transfer ... transfer command to be called when the user terminates
%		the dialog
% hmasterfig ... handle of the masterfigure (usually a LOGSEC window) in
%		control of the dialog
% sections ... list of names of possible sonic log sections and density
%		sections to be used to compute the rc's. Should be a string row vector
%		with individual names separated by '|' as is returned by 
%		objget(anyobject,'fieldnames')
% densopts ... flag giving the default for the density option:
%              0 = use constant density
%              1 = use Gardners relation exclusivly; 
%              2 = use density section where defined, fill in holes 
%                  with Garnders relation after hole filling on
%		   sonic section 
%              3 = use density section exclusively. Fill holes on it
%                  independently of the sonic section.
% holealg ... flag signifying the preferred default for the holefilling 
%             algorithm: 1=constant; 2=linear; 3=mean; 4=layermean; 
%                        5=layertrend;
%             See FILLHOLES for more information
% domain ... flag giving the default for the domain of rc computation
%		1=time, 2=depth
% Gardners relation assumes density= a*(vins).
% a ... default scalar multiplier in Gardner's relation
% m ... default scalar exponent in Gardner's relation
% msg ... a message to be displayed at the top of the dialog
% **************** default '' ****************
% sonicnum ... integer denoting the initial selection in sections for the sonic
%		section
% *************** default 1 ***************
% densitynum ... integer denoting the initial selection in sections for the density
%		section
% *************** default 2 ***************
% name ... string giving the initial name choice
% *************** default is automatically generated name *********
%
% G.F. Margrave, April 1994
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
if(nargin<12)
	name=0;
end
if(nargin<11)
	densitynum=2;
end
if(nargin<10)
	sonicnum=1;
end
if(nargin<9)
	msg='';
end
% pack the information into the current axes userdata
hax=get(hmasterfig,'currentaxes');
set(hax,'userdata',[abs(transfer) nan abs(sections) nan abs(msg) nan...
	hmasterfig densopts holealg domain a m sonicnum densitynum abs(name)]);
rcdefine('init')