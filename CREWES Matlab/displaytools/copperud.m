function cm=copperud(n)
%copper colormap upside down
% cmap=copperud(n)

if(nargin<1)
    n=64;
end
cm=flipud(copper(n));