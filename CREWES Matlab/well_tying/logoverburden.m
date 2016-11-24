function [logout, zout]=logoverburden(login,z,type,parm)
% [logout, zout]=logoverburden(login,z,type,parm)
%
%LOGOVERBURDEN applies a overburden to a log using several methods
%
%Input:
%login = log wich to apply an overburden.  All nulls and NaNs should be
%        removed after the top of the log.  All nulls should be replaced by
%        NaNs
%z     = the depth curve assigned to login should increment by the same
%        value.  The overburden will be assigned from 0 to the first
%        non-Nan on the log.
%type  = type of overburden to be used.
%       0-constant value defined by parm
%       1-linear trend defined starting by intial value (parm) and
%           linearly increasing to the average 10 samples at the top of log
%       2-average log value based on non-Nan entries, parm=[];
%       3-marine setting,where a two layer overburden will be prepared.
%           The first layer is water were as the second layer will be a
%           constant value to the top of the log
%           parm=[land value, water value,water depth];
%       4-marine setting,where a two layer overburden will be prepared.
%           The first layer is water were as the second layer will be a
%           linear gradient from the seafloor to the top of the log
%           parm=[land value, water value,water depth];
%parm  = Is defined separately for each overburden type.
%
%Output:
%logout= the log with the overburden attached
%zout= the depth curve starting at 0.
%
%H.J.E. Lloyd April 2013
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
dz=z(2)-z(1);
zout=(0:dz:z(end))';
indlog=find(~isnan(login),1,'first');
ind=1:near(zout,z(1))+indlog-1;
logout=zeros(size(zout));
if type==0;
    if isempty(parm)||length(parm)>1
        errormsg('parm must contain one value to use for the overburden');
    end
    logout(ind)=parm;
    logout(ind(end)+1:end)=login(indlog(end)+1:end);
end

if type==1;
    if isempty(parm)||length(parm)>1
        errormsg('parm must contain one value to use as the starting value');
    end
    val=mean(login(ind(end)+1:ind(end)+11));
    logout(ind)=linspace(parm,val,length(ind));
    logout(ind(end)+1:end)=login(indlog(end)+1:end);
end

if type==2;
    val=mean(login(~isnan(login)));
    logout(ind)=val;
    logout(ind(end)+1:end)=login(indlog(end)+1:end);
end

if type==3;
    if isempty(parm)||length(parm)<3
        errormsg({'parm must contain three values to use for the overburden',...
            'parm=[land value, water value,water depth]'});
    end
    watbot=near(zout,parm(3));
    logout(1:watbot)=parm(2);
    logout(watbot+1:ind(end))=parm(1);
    logout(ind(end)+1:end)=login(indlog(end)+1:end);
end

if type==4;
    if isempty(parm)||length(parm)<3
        errormsg({'parm must contain three values to use for the overburden',...
            'parm=[land value, water value,water depth]'});
    end
    watbot=near(zout,parm(3));
    logout(1:watbot)=parm(2);
    val=mean(login(ind(end)+1:ind(end)+11));
    ind2=watbot+1:ind(end);
    logout(ind2)=linspace(parm(1),val,length(ind2));
    logout(ind(end)+1:end)=login(indlog(end)+1:end);
end