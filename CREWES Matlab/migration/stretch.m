function [seisout,yout]=stretch(seisin,yin,vel,zv,dyout,direction,youtmax)
% STRETCH: convert a seismic gather from two-way time to depth or the reverse
%
% [seisout,yout]=stretch(seisin,yin,vel,zv,dyout,direction
%
% Convert a seismic gather from time to depth (or the reverse) by a 1D
% stretch. No protection is offered against aliasing. If fmax is the
% maximum signal frequency, vmin is the slowest velocity, and dz is the
% desired depth sample rate, then the user must ensure that
% dz<vmin/(4*fmax) to avoid aliasing.
% 
% seisin ... the input trace gather of dimension nrows by ncols.  Traces
%       should be in the columns.
% yin ... row coordinate for seisin. If direction=1, then this should be a
%       two-way time coordinate.
% vel ... velocity model (instantaneous or interval). Can either be a
%       matrix with the same number of columns as seisin or a single column
%       vector.  In the first case, each trace gets a unique velocity
%       function, while in the second case, all traces are stretched the
%       same way.
% zv ... depth (row) coordinate for vel. Note that length(zv) must equal
%       size(vel,1).
% dyout ... scalar sample size for the output traces. If direction=1, then this
%       is dz the depth sample rate, if direction=-1, then this is dt, the time sample
%       rate.
% direction ... 1 means we are converting time to depth, -1 is depth to
%       time, anything else is an error.
%
% 
% seisout ... output seismic gather
% yout ... output row coordinate. If Direction =1, then this is depth,
%       otherwise it is time.
% 
% G.F. Margrave, CREWES, Nov, 2000
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

if((abs(direction)-1)~=0)
    error('Direction must be 1 or -1');
end

if(size(vel,2)~=1)
    if(size(vel,2)~=size(seisin,2))
        error('velocity model has an invalid number of columns')
    end
end

if(size(vel,1)~=length(zv))
    error('size(vel,1) must equal length(zv)');
end

if(size(seisin,1)~=length(yin))
    error('size(seisin,1) must equal length(yin)');
end

%process the velocity model for time/depth curves
nvels=size(vel,2);
times=zeros(length(zv),nvels);
depths=times;

for k=1:nvels
    tzcurve=vins2tz(vel(:,k),zv);
    times(:,k)=tzcurve(:,1);
    depths(:,k)=tzcurve(:,2);
end

ntraces=size(seisin,2);
nwrite=min([50 round(ntraces/20)]);%write out this many messages
kwrite=round(ntraces/nwrite);%trace number for the first message
%branch based on direction
if(direction == 1)
    %ok, we are stretching from time to depth
    %determine zmax
    tmax=max(yin);
    tminmax=min(times(end,:));
    %we need tminmax to exceed tmax so that all input samples are mapped to
    %depth
    if(tminmax<tmax)
        %add an extra layer to all time-depth curves
        vend=2*(depths(end,:)-depths(end-1,:))./(times(end,:)-times(end-1,:));
        delt=tmax+.01-times(end,:);%need this much added time on each t-x function
        ind=find(delt<=0);
        if(~isempty(ind))
            delt(ind)=.01;
        end
        %add the layer
        times_nplus1=times(end,:)+delt;
        depths_nplus1=depths(end)+vend.*delt/2;
        times=[times;times_nplus1];
        depths=[depths;depths_nplus1];
    end
    zmax=max(depths(:));
    yout=0:dyout:zmax;
    seisout=zeros(length(yout),ntraces);
    for k=1:ntraces
        if(nvels==1)
            t=times(:,1);
            z=depths(:,1);
        else
            t=times(:,k);
            z=depths(:,k);
        end
        tmax=t(end);
        %see if time depth curve is long enough
%         if(yin(end)>tmax)
%             %need to pad with constant velocity
%             v=(z(end)-z(end-1))/(.5*(t(end)-t(end-1)));
%             delt=yin(end)-tmax;
%             zmax=z(end)+.5*v*delt;
%             t=[t;yin(end)];
%             z=[z;zmax];
%         end
            
        tint=pwlint(z,t,yout);%%%%%%%$$$$$$$$$$$$$
        %sinc interpolation
        tmp=sinci(seisin(:,k),yin,tint);
        seisout(:,k)=tmp(:);
        if(k==kwrite)
            disp(['Finished trace ' int2str(k) ' of ' int2str(ntraces)])
            kwrite=kwrite+round(ntraces/nwrite);
        end
    end
else
    %ok, we are stretching from depth to time
    %determine tmax
    zmax=max(yin);
    zminmax=min(depths(end,:));
    %we need zminmax to exceed zmax so that all input samples are mapped to
    %time
    if(zminmax<zmax)
        %add an extra layer to all time-depth curves
        vend=2*(depths(end,:)-depths(end-1,:))./(times(end,:)-times(end-1,:));
        delz=zmax+10-depths(end,:);%need this much added depth on each t-x function
        ind=find(delz<=0);
        if(~isempty(ind))
            delz(ind)=10;
        end
        %add the layer
        depths_nplus1=depths(end,:)+delz;
        times_nplus1=times(end)+2*delz./vend;
        times=[times;times_nplus1];
        depths=[depths;depths_nplus1];
    end
    zmax=max(depths(:));
    tmax=max(times(:));
    yout=0:dyout:tmax;
    seisout=zeros(length(yout),ntraces);
    for k=1:ntraces
        if(nvels==1)
            t=times(:,1);
            z=depths(:,1);
        else
            t=times(:,k);
            z=depths(:,k);
        end
        zmax=z(end);
        %see if time depth curve is long enough
%         if(yin(end)>zmax)
%             %need to pad with constant velocity
%             v=(z(end)-z(end-1))/(.5*(t(end)-t(end-1)));
%             delz=yin(end)-zmax;
%             tmax=t(end)+2*delz/v;
%             t=[t;tmax];
%             z=[z;yin(end)];
%         end
            
        zint=pwlint(t,z,yout);
        %sinc interpolation
        tmp=sinci(seisin(:,k),yin,zint);
        if(length(tmp)>size(seisout,1))
            seisout=zeros(length(tmp),ntraces);
        end
        if(k==400)
            disp('yo')
        end
        seisout(:,k)=tmp(:);
        if(k==kwrite)
            disp(['Finished trace ' int2str(k) ' of ' int2str(ntraces)])
            kwrite=kwrite+round(ntraces/nwrite);
        end
    end
end