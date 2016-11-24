function sg=gainmute(s,t,x,xshot,xmute,tmute,gainpow)
% GAINMUTE ... gain and mute a seismic gather
%
% sg=gainmute(s,t,x,xshot,xmute,tmute,gainpow)
%
% s ... seismic gather, one trace per column. This can also be a cell array
%       of gathers, in which case the output will be asimilar cell array.
% t ... time coordinate for s, length(t) must equal size(s,1) 
% x ... receiver coordinate for s, length(x) must equal size(s,2). If s is a
%       cell array, then this should be too.
% xshot ... shot coordinate for s. FOr a single shot, this is a scalar, for
%       s a cell array, this is a vector of the same length as s.
% xmute ... vector of at least two offsets at which mute times are specified.
%       Mutes are applied using absolute value of actual source-receiver
%       offset so this should be non-negative numbers. The simplest
%       meaningfull values would be [0 xoffmax] where xoffmax is the
%       maximum source-receiver offset. (It is not strictly necessary to
%       use the maximum offset as the mute is linearly extrapolated to
%       larger offsets.)
% tmute ... vector of times corresponding to xmute. At each offset, samples at times
%       earlier than the mute time will be zero'd. Traces with offsets not
%       specified in xmute will have mute times by linear interpolation or,
%       if the trace offset is larger than max(xmute), the times are
%       computed by constant-slope extrapolation. NOTE: size(tmute) must
%       equal size(xmute).
% NOTE... to turn off all muting, enter xmute=0 and tmute=0.
% gainpow ... gain applied to each sesimic trace is t.^gainpow . For no
%       gain, use gainpow=0.
% ************** default: gainpow=1 ******************
%
% sg = gather with gain and mute applied.
%
%
% G.F. Margrave, CREWES Project, August 2013
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

if(nargin<7)
    gainpow=1;
end
if(~iscell(s))
    if(length(t)~=size(s,1))
        error('invalid t coordinate vector')
    end
    if(length(x)~=size(s,2))
        error('invalid x coordinate vector')
    end

    if(abs(gainpow)>10 || length(gainpow)>1)
        error('Bad value for gainpow')
    end

    if(size(xmute)~=size(tmute))
        error('xmute and tmute must be the same size')
    end
    
    if(sum(abs(xmute))==0)
        error('xmute cannot be all zero')
    end
    
    if(any(diff(xmute)<0))
        error('xmute must be increasing');
    end
    
    %compute offsets
    xoff=abs(x-xshot);
    
    %interpolate the mute
    if(length(xmute)>1)
        tmutex=interpextrap(xmute,tmute,abs(xoff));
    end

    sg=zeros(size(s));
    g=t.^gainpow;
    dt=t(2)-t(1);
    
    for k=1:length(xoff)
        %apply gain
        if(gainpow~=0)
            tmp=s(:,k).*g;%simple gain
        else
            tmp=s(:,k);
        end
        %apply mute
        if(length(xmute)>1)
            imute=min([round(tmutex(k)/dt)+1,length(t)]);
            tmp(1:imute)=0;
        end

        sg(:,k)=tmp;
    end
else
    nshots=length(s);

    if(~iscell(x))
        xx=x;
        x=cell(1,nshots);
        for k=1:nshots
            x{k}=xx;
        end
    end
    if(length(t)~=size(s{1},1))
        error('invalid t coordinate vector')
    end
    if(length(x{1})~=size(s{1},2))
        error('invalid x coordinate vector')
    end

    if(abs(gainpow)>10 || length(gainpow)>1)
        error('Bad value for gainpow')
    end

    if(size(xmute)~=size(tmute))
        error('xmute and tmute must be the same size')
    end
    
    if(length(xshot)~=nshots)
        error('invalid shot coordinate array')
    end

    sg=cell(size(s));
    g=t(:).^gainpow;
    dt=t(2)-t(1);
    
    for kshot=1:nshots
        ss=s{kshot};
        xx=x{kshot};
        ssg=zeros(size(ss));
        if(length(xx)~=size(ss,2))
            error(['x coordinate for shot ' int2str(kshot) ' is incorrect']);
        end
        
        %offsets for this shot
        xoff=abs(xx-xshot(kshot));
        
        %interpolate the mute
        if(length(xmute)>1)
            tmutex=interpextrap(xmute,tmute,xoff);
        end
        
        for k=1:length(xoff)
            %apply gain
            if(gainpow~=0)
                tmp=ss(:,k).*g;%simple gain
            else
                tmp=ss(:,k);
            end
            %apply mute
            if(length(xmute)>1)
                imute=min([round(tmutex(k)/dt)+1,length(t)]);
                tmp(1:imute)=0;
            end

            ssg(:,k)=tmp;
        end
        sg{kshot}=ssg;
    end
end