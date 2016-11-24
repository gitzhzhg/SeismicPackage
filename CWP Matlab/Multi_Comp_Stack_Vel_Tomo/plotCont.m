%==============================================================================
clear all;   close all;

load tt.Table
%==============================================================================
% Plot stuff
xsou = [-1: 0.2: 1];   xrec = [-1: 0.2: 1];

figure;
hold on;   grid on;   axis('image');   
axis([min(xrec)  max(xrec)  min(xsou)  max(xsou)]);
[xr, xs] = meshgrid(xrec,xsou);

vv = [1.91: 0.04: 2.5];
set(gca, 'FontSize', 16);
[c, h] = contour(xr, xs, tt, vv, 'k-');
clabel(c,h);
set(h, 'LineWidth', 2);

xlabel('Receivers (km)', 'FontSize', 18);
ylabel('Sources (km)', 'FontSize', 18);
title('Traveltime contours of converted wave', 'FontSize', 20);

%==============================================================================
figure;
hold on;   grid on;   axis('image');   
axis([min(xrec)  max(xrec)  min(xsou)  max(xsou)]);
[px,py] = gradient(tt, 0.2, 0.2);
quiver(xr, xs, px, py);
plot(xrec, xsou, 'r-', 'LineWidth', 2);

set(gca, 'FontSize', 16);
xlabel('Receivers (km)', 'FontSize', 18);
ylabel('Sources (km)', 'FontSize', 18);
title('Traveltime gradient', 'FontSize', 20);

%==============================================================================
figure;
hold on;   grid on;
for i=1:11
  ttt(i) = tt(12-i,i);
end; 
plot(2*xsou, ttt, 'ro', 'LineWidth', 2, 'MarkerSize', 6, ...
     'MarkerEdgeColor', 'r', 'MarkerFaceColor', 'r');

set(gca, 'FontSize', 16);
xlabel('Offset (km)', 'FontSize', 18);
ylabel('Traveltime (s)', 'FontSize', 18);
title('Traveltime at X_{ CMP} = 0', 'FontSize', 20);


%==============================================================================

