function handles=mysubplot(vertical,horizontal)
% Create handles for the axes of subplots.
% The subplots are contiguous. Axes along the horizontal share the same y-axis;
% axes along the vertical share the same x-axis.
%
% Written by: E. Rietsch: March 14, 2009
% Last updated:
%
%
% INPUT
% vertical  numeric vector; the numbers represent the relative hight of the 
%           subplots; e.g. [1 2] means that there are two rows of plots; the 
%           plots at the bottom are twice as high (2/3) as those one on top
%           (1/3).
% horizontal  numeric vector; the numbers represent the relative width of the 
%           subplots; e.g. [3 2] means that there are two columns of plots; the 
%           plots on the left are 50% wider (3/5) than those on the right (2/5).
% OUTPUT
%           matrix with handles of the axes of the subplot. the matrix has size
%           length(vertical) x length(horizontal)
%
% EXAMPLE
%          figure
%          handles=mysubplot([1,2],[3,2]);

%     Set defaults for optional input arguments
param.location=[0.1,0.1,0.8,0.8];


nv=length(vertical);
nh=length(horizontal);
handles=zeros(nv,nh);

heights=param.location(4)*vertical(end:-1:1)/sum(vertical);
widths=param.location(3)*horizontal/sum(horizontal);
xloc0=param.location(1);
yloc=param.location(2);
for ii=1:nv
   xloc=xloc0;
   for jj=1:nh
      handles(ii,jj)=subplot('position',[xloc,yloc,widths(jj),heights(ii)]);
      xloc=xloc+widths(jj);
      box on
      make_axes_bold(handles(ii,jj))
   end
   yloc=yloc+heights(ii);
end
handles=flipud(handles);
