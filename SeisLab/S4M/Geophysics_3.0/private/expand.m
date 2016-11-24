function y=expand(a,b)
%EXPAND Expand Singleton Dimensions.
% EXPAND(A,B) expands A by replication to match the size of B so that
% element by element mathematical and logical operations between A and B
% are defined.
%
% All nonsingleton dimensions of A must match those of B.
% All singleton dimensions of A are replicated to match those of B.
%
% Examples:
% EXPAND( [1 2], EYE(2) ) produces [1 2;1 2]
% EXPAND( [1;2], EYE(2) ) produces [1 1;2 2]
% EXPAND( [1 2], CAT(3,EYE(2),ONES(2)) ) expands [1 2] in two dimensions
% ans(:,:,1) =
%     1     2
%     1     2
% ans(:,:,2) =
%     1     2
%     1     2
%
% See also REPMAT, CAT

% D.C. Hanselman, University of Maine, Orono, ME 04469
% MasteringMatlab@yahoo.com
% Mastering MATLAB 7
% 2006-02-16

asiz=[size(a) ones(1,ndims(b)-ndims(a))]; % try to make A look as big as B
bsiz=size(b);

if length(asiz)>length(bsiz)
   error('A Must Have The Same Number of Dimensions as B.')
end

ns=(asiz>1);              % nonsingleton dims of A
sd=(~ns);                 % singleton dims of A
if all(asiz==bsiz)        % no work to do, A is already as big as B
   y=a;
   
elseif all(ns==0)         % A is a scalar (protect next test from emptys)
   y=repmat(a,bsiz);      % expand scalar A to the size of B
   
elseif any(asiz(ns)~=bsiz(ns))
   error('All NonSingleton Dimensions of A Must Match Those of B.')
   
else                      % finally, do it
   rep=ones(size(bsiz));  % start with single replication of all dimensions
   rep(sd)=bsiz(sd);      % poke in replications of A required to match B
   y=repmat(a,rep);       % let repmat do the nitty-gritty work
end
