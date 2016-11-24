function [stack,xcmp,stackfold,gathers,xoffs]=cmpstack(shots,t,xrec,xshots,cmp,velrms,tv,xv,normopt,smax,xgath,offlims)
% CMPSTACK ... CMP stack of a cell array of shots (includes NMOR)
%
% [stack,xcmp,stackfold,gathers,xoffs]=cmpstack(shots,t,xrec,xshots,cmp,...
%                       velrms,tv,xv,normopt,smax,xgath,offlims)
%
% shots ... cell array of shot records
% t ... time coordinate vector. Length(t) must equal size(shots{k},1) for
%           all shots 
% xrec ... cell array of receiver coordinates. For the kth shot,
%           length(xrec{k} must equal size(shots{k},2). May also be input
%           as a simple vector provided that all shots have the same
%           receivers.
% xshots ... vector of shot coordinates, one per shot. length(xshots) must
%           equal length(shots)
% cmp ... vector of length 3 giving (1) the cmp interval (usually 1/2 the
%           receiver interval), (2) the first desired output cmp
%           coordinate, and (3) the last desired output cmp coordinate.
% velrms ... rms velocity model. The model dimensions in x and t will
%           automatically be expanded as needed. To avoid this implicit
%           interpolation, you must specify velrms on a grid as large as
%           that spanned by all shots. A given trace will receive moveout
%           correction using the rms velocity form a location in the model
%           corresponding to the midpoint coordinate of the trace.
% tv ... time coordinate vector for velrms. Need not be regularly sampled.
%           Length(tv) must equal size(velrms,1).
% xv ... cmp coordinate vector for velrms. Need not be regularly sampled.
%           Length(xv) must equal size(velrms,2).
% normopt ... 1 means normalize by nominal fold
%             2 means normalize by true time-variant fold (best)
% *************** default =2 **************
% smax ... maimum allowed nmo stretch (percent)
% *************** default 10 *******************
% xgath ... vector of cmp locations at which to output moveout corrected
%           gathers.
% *************** default: [] meaning no gathers will be output **************
% offlims ... length 2 vector where offlims(1) is the minimum absolute-value
%           offset allowed in the stack and offlims(2) is the maximum.
% *************** default offlims=[0 inf] meaning all offsets *************
%
% stack ... cmp stack of the input data
% xcmp ... spatial coordinate vector of the stack
% NOTE: The time coordinate vector of the stack is the same as that for
%   the input shots. Plot the stack with plotimage(stack,t,xcmp)
% stackfold ... fold vector of the stack. Plot like:
%   figure;plot(xcmp,stackfold)
% gathers ... cell array of cmp gathers. One for each entry in xgaths.
% offsets ... cell array of offset coordinate vectors for the gathers. 
%   Plot the kth gather like plotseismic(gathers{k},t,offsets{k})
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

if(nargin<12)
    offlims=[0 inf];
end
if(nargin<11)
    xgath=[];
end
if(isempty(xgath))
    gathers=[];
    xoffs=[];
end
if(nargin<10)
    smax=10;
end
if(nargin<9)
    normopt=2;
end

nshots=length(xshots);

if(~iscell(xrec))
    xx=xrec;
    xrec=cell(1,nshots);
    for k=1:nshots
        xrec{k}=xx;
    end
end

if(length(t)~=size(shots{1},1))
    error('Time coordinate vector not correct')
end
if(length(xrec{1})~=size(shots{1},2))
    %test first shot. others later
    error('Receiver coordinate vector not correct')
end
if(length(xshots)~=length(shots))
    error('shot coordinate vector incorrect')
end
if(length(tv)~=size(velrms,1))
    error('velocity model time coordinate incorrect')
end
if(length(xv)~=size(velrms,2))
    error('velocity model space coordinate incorrect')
end
if(length(cmp)~=3)
    error('cmp must consist of 3 numbers: [increment, first cmp, last cmp]')
end
if(normopt~=1 && normopt~=2)
    error('invalid normalization option');
end


%fold=cell(1,nshots);
ngath=length(xgath);
if(ngath)
    gathers=cell(1,length(xgath));
    igath=round((xgath-cmp(2))/cmp(1))+1;
end
    
tbegin=clock;
for k=1:nshots
    if(length(xrec{k})~=size(shots{k},2))
        error(['Receiver coordinate incorrect for shot ' int2str(k)])
    end
    [shot_nmo,xcmp,xoff,fold]=nmor_cmp(shots{k},t,xrec{k},xshots(k),...
        velrms,xv,tv,cmp(1),cmp(2),cmp(3),1,smax,offlims);
    if(k==1)
        %allocate space for stack
        stack=zeros(size(shot_nmo));
        if(normopt==2); foldstack=stack; end
        stackfold=zeros(size(fold));
    end
    stack=stack+shot_nmo;%update the stack with the current shot
    if(normopt==2)
        shot_fold=ones(size(shot_nmo));
        ind=find(shot_nmo==0);
        shot_fold(ind)=0;
        foldstack=foldstack+shot_fold;%this is the time variant fold
    end
    stackfold=stackfold+fold;%update the fold
    %check for output gather
    if(ngath)
        for kk=1:ngath
            tmp=shot_nmo(:,igath(kk));
            if(sum(abs(tmp))~=0)
                tmp(1)=xoff(igath(kk));%hide offset in first sample, recover later
                gathers{kk}=[gathers{kk} tmp];
            end
        end
    end
    timeused=etime(clock,tbegin);
    timepershot=timeused/k;
    timeleft=timepershot*(nshots-k);
    disp(['CMPSTACK: Processed shot ' int2str(k) ' of ' int2str(nshots)])
    disp(['time used: ' num2str(timeused/60) ' min, time remaining: ' ...
        num2str(timeleft/60) ' min']);
end


%normalize the stack
if(normopt==1)
    for k=1:length(stackfold)
        if(stackfold(k)>0)
            stack(:,k)=stack(:,k)/stackfold(k);
        end
    end
elseif(normopt==2)
   ind=find(foldstack~=0);
   stack(ind)=stack(ind)./foldstack(ind);
end
 
%process gathers to recover offset
if(ngath)
    xoffs=cell(1,ngath);
    for k=1:ngath
        g=gathers{k};
        xoffs{k}=g(1,:);
        g(1,:)=zeros(1,size(g,2));
        gathers{k}=g;
    end
end
    