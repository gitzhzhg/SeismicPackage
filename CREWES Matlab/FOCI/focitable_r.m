function [table,k,nop]=focitable_r(dx,dz,kmin,kmax,dk,nfor,ninv,nwin,ipct,dipnom,pow)
%
% [table,k,nop]=focitable_r(dx,dz,kmin,kmax,dk,nfor,ninv,nwin,ipct,dipnom,pow)
%
% Function to make an operator table for use in the FOCI method
% of depth migration. This differes from focitable in that it allows the
% post design windowing of the FOCI operator.
%
% dx ... spatial samples size
% dz ... depth step size
% kmin ... minimum value of k needed in table
% kmax ... maximum value of k needed
% dk ... increment between rows in table
% nfor ... vector of 2 entries giving the desired number of points in 
%          the forward operator at kmin and kmax.
% ninv ... similar to nfor but for the inverse operator.
% nwin ... similar to nfor but giving the windowed length of final op
%           [0 0] gives no windowing 
% ipct ... percent of imaginary velocity
% dipnom ... dip (degrees) at which the imaginary velocity takes effect
% pow ... exponent (see exop_design)
%
% table ... operator table, one operator of length nfor+ninv-1 per row
%           ceil((kmax-kmin)/dk)+1 rows
% k ... vector giving the k values of the rows of table
% nop ... vector of lengths of the operators in the table
%
%
% G.F. Margrave, CREWES/POTSI 2004
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

ipct=ipct/100;

nk=ceil((kmax-kmin)/dk)+1;
k=kmin+(0:nk-1)*dk;

%fix velocity at 2000 and vary f
v=2000;
f=k*v;
nopmax=max(nwin);
if(nopmax==0) nopmax=max(nfor)+max(ninv)-1; end
table=zeros(nk,nopmax)+i*zeros(nk,nopmax);
nop=zeros(size(k));

for kk=1:nk
    if(nk~=1)
        nf=(kmax-k(kk))*nfor(1)/(kmax-kmin)+ (k(kk)-kmin)*nfor(2)/(kmax-kmin);
        ni=(kmax-k(kk))*ninv(1)/(kmax-kmin)+ (k(kk)-kmin)*ninv(2)/(kmax-kmin);
        nf=2*floor(nf/2)+1; %nearest odd number
        ni=2*floor(ni/2)+1;
    else
        nf=nfor(1);
        ni=ninv(1);
    end
    if(sum(nwin))
        if(nk~=1)
            nw=(kmax-k(kk))*nwin(1)/(kmax-kmin)+ (k(kk)-kmin)*nwin(2)/(kmax-kmin);
            nw=2*floor(nw/2)+1;
        else
            nw=nwin(1);
        end
    else
        nw=nf+ni-1;
    end
    tmp=exop_design(dx,dz,nf,ni,v,f(kk),ipct,dipnom,pow,nw,0,0);
    nop(kk)=length(tmp);
    table(kk,1:length(tmp))=tmp;
end