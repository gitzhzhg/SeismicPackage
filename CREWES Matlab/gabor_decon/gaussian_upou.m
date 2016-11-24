function [gwin,norm_factor,xnotvec,nwin,i1]=gaussian_upou(x,xnot,xwid,xinc,norm_factor,xnotvec,gdb,pow2option)
% GAUSSIAN_UPOU: design a uniform partition of unity based on Gaussians.
%
% [gwin,norm_factor,xnotvec,nwin,i1]=gaussian_upou(x,xnot,xwid,xinc,norm_factor,xnotvec,gdb,pow2option)
%
% This function designs a uniform partition of unity (POU) for a finite portion
% of the real line. The partition is based on the Gaussian window but the
% window is modified to ensure that the partition is exact. The
% modification amounts to creating an approximate POU using the exact
% Gaussians and then summing these Gaussians. The result of this summation
% is then used as a "normalization factor" in that each Gaussian is divided
% (pointwise) by the sum to create the modified Gaussian. The modified
% Gaussians then form an exact POU. This function is designed to return a
% single modified Gaussian each time it is called. On the first invocation,
% the normalization factor and the vector of window center positions
% (xnotvec) are computed and returned as well. These should then be
% provided as input on all subsequent calls to ensure that all windows come
% from the same POU.
%
% Input:
% x ... x coordinate vector describing the segment of the real line for
%   which the POU is to be designed. Should be regularly sampled.
% xnot ... x coordinate of the center of the desired modified Gaussian. If
%   xnot does not fall directly on a sample, it will be adjusted to do so.
% xwid ... the Gaussian half-width. The Gaussian will have amplitude 1/e at
%   xnot+xwid and xnot-xwid.
% xinc ... increment between window centers. The POU will have a window
%   centered at min(x) and another at max(x) and intermediate windows
%   spaced at xinc. This means (max(x)-min(x))/xinc must be an integer. It
%   will be adjusted from the input value to the nearest value giving an
%   integer.
% norm_factor ... a vector the same length as x giving the normalization
%   factor required to make the POU exact. If omitted or specified as a
%   different size vector than x, it is computed and returned.
% xnotvec ... vector of window center times for the current POU. This is
%   computed and returned at the same time as norm_factor. On the first
%   call to gaussian_upou, it will be unknown, but is computed and
%   returned. To indicate it is unknown, just give it the value 0.
%   This is then computed and returned, along with norm_factor.  It should 
%   be provided on all subsequent calls.
% gdb ... number of decibels below 1 at which to truncate the Gaussian
%   windows. This should be a positive number. Making this larger
%   increases the size of the Gabor transform but may give marginally better
%   performance.
% ********** default = 60 ************
% pow2option ... expand the gaussians so that their length is a power of 2
% ********** default = 1 (yes) ********* (0 means no)
%
% Output:
% gwin ... modified Gaussian window given by
%   gaus=exp(-((x-xnot)/xwid).^2)./norm_factor truncated as determined by gdb
% norm_factor ... Normalization factor
% xnotvec ... vector of window center times used for the current POU and to
%   compute norm_factor on the first call. All window centers will fall
%   exactly on a sample and will be spaced approximately xinc apart.
% nwin ... number of windows required for the desired POU
% i1   ... position within x of the first sample of gwin
% If h is a vector of length(x) giving the values of a function on x. 
% Then the windowed function is gh= h(i1:length(gwin)).*gwin
%
% The length(gwin) is determined by the gdb parameter. Even though a
% Gaussian is infinitely long, it rapidly decays to hundreds of decibels
% (db) below 1 within a few standard deviations. Once the Gaussian designed
% here has decayed to gdb decibels below 1, it is truncated. This truncated 
% length is then expanded such that the final length is a power of 2. 
% The windows designed by this function will all be returned as the same 
% nominal length determined by gdb expanded to a power of 2. However, a 
% window at either end of the design interval
% (defined by the vector x) will be only half length and windows near these
% extremes will also be systematically shorter. These shorter windows are
% padded with zeros to the nominal length.
%
% To see an example of the use of this function, examine fgabor and igabor.
%
% by G.F. Margrave, Feb 2013
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

global XNOM GAUS DX XMIN XMAX XWID

if(nargin<5)
    norm_factor=0;
end
if(nargin<6)
    xnotvec=0;
end
if(nargin<7)
    gdb=60;
end

if(nargin<8)
    pow2option=1;
end

%make sure we have row vectors
[rx,cx]=size(x);
if(cx==1)
    x=x'; 
end

dx=x(2)-x(1);
xmin=min(x);
xmax=max(x);
nx=length(x);
if(pow2option)
    nx=2^nextpow2(nx);
end
if(nx>length(x))
    x=(0:nx-1)*dx+xmin;
    xmax=max(x);
end

makeit=0;
if(isempty(GAUS))
    makeit=1;
elseif( dx~=DX || xmin~=XMIN || xmax ~= XMAX || xwid ~= XWID || nx~=length(GAUS))
    makeit=1;
end
if(makeit)
    %make the nominal Gaussian, this is the elementary window. Pieces of it
    %are used everywhere
    DX=dx;
    XMIN=xmin;XMAX=xmax;XWID=xwid;
    XNOM=xmin-xmax:dx:xmax-xmin;
    GAUS=exp(-(XNOM/xwid).^2);
end

%modify xinc if needed, needed to ensure an integer number of windows
nwin=round((xmax-xmin)/xinc)+1;
xinc=(xmax-xmin)/nwin;


%Now determine window truncation. Superficially we truncate the window
%after it is gdb down from 1. However, this only works nicely for the
%center window. For an end window, strict adherence to this would mean that
%the end window is half as long as the middle window, but this would be
%awkward as the window length determines the frequency sampling. We want a
%t-f decomposition with the same frequency sampling for each window. So, we
%determine the gdb down points and then expand the window as needed to
%reach the standard size.
Xdb=xwid*sqrt(gdb/(20*log10(exp(1))));%offset from XNOM=0 at which we are gdb decibels down
if(isinf(Xdb)); Xdb=(xmax-xmin)/2; end %make sure gdb=inf means no window truncation
nXdb=round(Xdb/dx);%no +1 needed
nwinstandard= 2*nXdb;%this is the standard window size
%windows will always be an even number of samples long.
if(pow2option)
    nwinstandard=2^nextpow2(nwinstandard);
    %redefine nXdb to point to this width
    nXdb=nwinstandard/2;
    Xdb=dx*nXdb;
end
if(nwinstandard>nx)
    nwinstandard=nx;
    %redefine nXdb to point to this width
    nXdb=nwinstandard/2;
    Xdb=dx*nXdb;
end

if(nwinstandard==nx-1)
    nwinstandard=nx;
end

%see if we need to create the normalization factor
test_for_unity=sum(abs(norm_factor-ones(size(norm_factor))));
%if test_for_unity is 1, then the norm_factor has been provided as unity
%which essentially means no normalization, however, we go ahead with the if
%statement below because is calculates the vector of window center times
%and because we do not really want unity for a norm_factor. Rather, we want
%some constant which is the mean value of the true norm_factor.
do_it_anyway=0;
if(test_for_unity==0 && length(norm_factor)==nx)
    do_it_anyway=1;
end
if(length(norm_factor)~=nx || sum(abs(norm_factor(:)))==0 || do_it_anyway)
    norm_factor=zeros(size(x));
    x0=xmin;
    xnotvec=zeros(1,nwin);
    for k=1:nwin
        xnotvec(k)=x0;
%         %grab samples from nominal Gaussian
        gwinnom=get_gaussian(x,x0);
%         %now limit offsets by gdb
        igoff=abs((x-x0)/dx);
        iuse2=find(igoff<=nXdb);%these are the samples to use
        %need to expand iuse if less than the standard size
        iuse=make_standard_size(iuse2,nwinstandard,nx);
        gwin=gwinnom(iuse);
        %iuse=max([nnot-nX/2,1]):min([nnot+nX/2-1,nx]);%this is at most nX samples centered on the window center
        norm_factor(iuse)=norm_factor(iuse)+gwin;
        x0=x0+xinc;
        %adjust x0 to the nearest sample of x
        x0=dx*round((x0-xmin)/dx)+xmin;
        if(k+1==nwin && x0>xmax)
            x0=xmax;
        end
    end
    if(do_it_anyway)
        norm_factor=mean(norm_factor)*ones(size(norm_factor));
    end
end

% if(~between(xmax,xmin,xnot,2))
%     error('Gaussian origin not contained in x vector')
% end
ind=near(xnotvec,xnot);
xnot=xnotvec(ind(1));

%get the current Gaussian from the nominal
gwinnom=get_gaussian(x,xnot);
igoff=abs((x-xnot)/dx);
iuse2=find(igoff<=nXdb);%these are the samples to use
%need to expand iuse if less than the standard size
iuse=make_standard_size(iuse2,nwinstandard,nx);
gwin=gwinnom(iuse);
gwin=gwin./norm_factor(iuse);
i1=iuse(1);

if(cx==1)
    gwin=gwin';
end


function iuse=make_standard_size(iuse2,nwinstandard,nx)
        if(length(iuse2)<nwinstandard)
            if(iuse2(1)==1)
                %ok in this case we extend the samples past iuse(end)
                nu=length(iuse2);
                iuse=[iuse2 (iuse2(end)+1):(iuse2(end)+nwinstandard-nu)];
            elseif(iuse2(end)==nx)
                %in this case we extend samples before iuse(1)
                nu=length(iuse2);
                iuse=[iuse2(1)-nwinstandard+nu:iuse2(1)-1 iuse2];
            else
                error('Total freakin logic failure in gaussian_upou')
            end
        elseif(length(iuse2)==nwinstandard+1)
            %this occurs for a symmetirc window with no truncation
            %we choose to delete the last point
            iuse=iuse2(1:end-1);
        else
            iuse=iuse2;
        end
        if(length(iuse)~=nwinstandard)
            error('blew it')
        end
function gwinnom=get_gaussian(x,x0)
global GAUS XMAX XMIN DX
%grab samples from nominal Gaussian
        xnom1=x(1)-x0;%nominal coordinate of the first sample
        inom1=round((xnom1+XMAX-XMIN)/DX)+1;
        inom=inom1:inom1+length(x)-1;
        gwinnom=GAUS(inom);


    