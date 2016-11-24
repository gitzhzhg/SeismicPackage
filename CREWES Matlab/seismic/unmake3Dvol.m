function [seis,xlineall,ilineall,xcdpall,ycdpall]=unmake3Dvol(seis3D,xline,iline,xdcp,ycdp,varargin)
% UNMAKE3DVOL ... convert a 3D seismic volume to a 3D array
%
% [seis,xlineall,ilineall,xcdpall,ycdpall]=unmake3Dvol(seis3D,xline,iline,xcdp,ycdp,varargin)
%
% Converts a 3D seismic matrix to a 2D array as might be needed for SEGY
% output. Options exist to heep or discard zero traces and to output in
% either single or double precision.
%
% seis3D ... 3D seismic matrix with time in first dimension, xline in
%       second dimension and iline in third dimension
% xline ... xline numbers. Usually an array of consequtive positive
%       integers whos length must equal size(seis3D,2)
% iline ... inline numbers. Usually an array of consequtive positive
%       integers whos length must equal size(seis3D,3)
% xcdp ... cdp coordinates for xline. Must be same size as xline unless
%       entered as nan. If provided as nan, then it is generated
%       automatically as 1:nx
% ycdp ... cdp coordinates for iline. Must be same size as iline unless
%       entered as nan. If provided as nan, then it is generated
%       automatically as 1:ny
%
% extra arguments name-value pairs
% 'precision' ... either 'single' or 'double'
% ************ default 'double' **************
% 'zerotraces' ... 'keep' or 'discard'
% *********** default 'discard' ***********
% 'kxlineall' ... this is the 2D array returned by make3Dvol. Supplying
%           this will automatically set 'zerotraces' to 'keep'. This
%           variable determines to ordering of the output dataset. If
%           obtained from make3Dvol, then this will cause the output to be
%           the same spatial geometry as the input to make3Dvol.
%
% seis ... output seismic dataset as a 2D matrix.
% xlineall ... xline value for each trace in seis
% ilineall ... iline value for each trace in seis
% xcdpall ... xcdp value for each trace in seis
% ycdpall ... ycdp velue for each trace in seis
%
%
% G.F. Margrave, Devon Energy and CREWES (U of Calgary), 2016
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
%    notice. New versions will be made available at https://urldefense.proofpoint.com/v2/url?u=http-3A__www.crewes.org&d=DQIGAg&c=c26JvTNxlBzprEG-YwpBncKWSHipAnfAdXnqmG16xrs&r=9zhNFpMpIzGFea4ykNLmSvoWAoQAaVq-fjm3dAhNeuM&m=iE5AH9j8EbrvpsoIlJgl9FOygwpR1ziqhlrsyAXCQeA&s=x28FUbpew3-4VHX69YAZ7OzWWgWOqRQcqQwhmozUf5A&e=  .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE




sz=size(seis3D);
if(length(sz)~=3)
    error('seis3D is not a 3D matrix')
end

nt=sz(1);
nx=sz(2);
ny=sz(3);

if(length(xline)~=nx)
    error('second dimension of seis3D does not match xline');
end
if(length(iline)~=ny)
    error('third dimension of seis3D does not match iline');
end

if(isnan(xdcp))
    xdcp=1:nx;
end
if(isnan(ycdp))
    ycdp=1:ny;
end

if(length(xdcp)~=nx)
    error('second dimension of seis3D does not match cdpx');
end
if(length(ycdp)~=ny)
    error('third dimension of seis3D does not match cdpy');
end

%parse varargin
nargs=length(varargin);
if(~iseven(nargs))
    error('extra arguments must be name-value pairs')
end
precision='double';
zerotraces='discard';
kxlineall=[];
for k=1:2:nargs
    name=varargin{k};
    switch name
        case 'precision'
            val=varargin{k+1};
            if(~ischar(val) || (~strcmp(val,'single')&&~strcmp(val,'double')))
                error('''precision'' must be either ''single'' or ''double'' ')
            end
            precision=val;
        case 'zerotraces'
            val=varargin{k+1};
            if(~ischar(val) || (~strcmp(val,'keep')&&~strcmp(val,'discard')))
                error('''zerotraces'' must be either ''keep'' or ''discard'' ')
            end
            zerotraces=val;
        case 'kxlineall'
            val=varargin{k+1};
            if((size(val,1)~=nx) || (size(val,2)~=ny))
                error('kxlineall has incorrect dimensions')
            end
            kxlineall=val;
               
        otherwise
            error(['extra argument ' int2str(k) ' is not a recognised name'])
    end
end

%make a default kxline all that gives inline ordering
if(isempty(kxlineall))
    kxlineall=zeros(nx,ny);
    for k=1:ny
        kxlineall(:,k)=(k-1)*nx+1:k*nx;
    end
else
    zerotraces='keep';
end

%determine number of output traces
if(strcmp(zerotraces,'keep'))
    ntraces=nx*ny;
else
%    map=sum(abs(seis3D));
%    ind=find(map~=0);
%    ntraces=length(ind);
    ntraces=max(kxlineall(:));
end

if(strcmp(precision,'single'))
    seis=single(zeros(nt,ntraces));
    precision=1;
else
    seis=zeros(nt,ntraces);
    precision=2;
end
xlineall=zeros(1,ntraces);
ilineall=xlineall;
xcdpall=xlineall;
ycdpall=xlineall;

n=0;
% for kx=1:nx
%     for ky=1:ny
%         tmp=seis3D(:,kx,ky);
%         if(sum(abs(tmp))~=0)
%             n=n+1;
%             if(precision==1)
%                 seis(:,n)=single(tmp);
%             else
%                 seis(:,n)=double(tmp);
%             end
%             xlineall(n)=xline(kx);
%             ilineall(n)=iline(ky);
%             xcdpall=cdpx(kx);
%             ycdpall=cdpy(ky);
%         end
%     end
% end

zerokeep=strcmp(zerotraces,'keep');

for ky=1:ny
    for kx=1:nx
        tmp=seis3D(:,kx,ky);
        test=sum(abs(tmp));
        if(test~=0 || zerokeep==1)
            n=n+1;
            kloc=kxlineall(kx,ky);
            if(precision==1)
                seis(:,kloc)=single(tmp);
            else
                seis(:,kloc)=double(tmp);
            end
            xlineall(kloc)=xline(kx);
            ilineall(kloc)=iline(ky);
            xcdpall(kloc)=xdcp(kx);
            ycdpall(kloc)=ycdp(ky);
        end
    end
end

