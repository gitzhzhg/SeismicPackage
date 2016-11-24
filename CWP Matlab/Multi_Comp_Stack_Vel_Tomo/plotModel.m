%==============================================================================
% Plot model interfaces, positions of sources and receivers
%==============================================================================

function figInterfaceall = plotModel(intermedInt);

global Ninterface NpolX NpolY IntPol
global Source Receiv Npair
%global figInterfaceall

%==============================================================================
% Determine scale of the plots
x1max = max([max(max(Source(1,:))), max(max(Receiv(1,:)))]);
x2max = max([max(max(Source(2,:))), max(max(Receiv(2,:)))]);
x1max = max(x1max, x2max) + 0.1*max(abs(x1max), abs(x2max));   x2max = x1max;
x1max = 0.5;   
x2max = 0.5;  

x1min = min([min(min(Source(1,:))), min(min(Receiv(1,:)))]);
x2min = min([min(min(Source(2,:))), min(min(Receiv(2,:)))]);
x1min = min(x1min, x2min) - 0.1*min(abs(x1min), abs(x2min));   x2min = x1min;
x1min = -0.5;   
x2min = -0.5;  

x1 = x1min: (x1max-x1min)/20: x1max;
x2 = x2min: (x2max-x2min)/20: x2max;

%==============================================================================
% Plot stuff
figInterfaceall = figure;   axis('image');   
x3min = +1.e4;   x3max = -1.e4;

for Iinterface=1:Ninterface
  for ix1=1:length(x1)
    for ix2=1:length(x2)
      x3(ix2,ix1) = funInt(NpolX, NpolY, IntPol, ...
                           Iinterface, x1(ix1), x2(ix2));
    end;
  end;

  if intermedInt == 'Y';
%   Plot individual interfaces
    Interface(Iinterface) = figure;   axis('image');
    surfc(x1,x2,x3);   hold on;   
    axis([x1min-0.001, x1max+0.001, x2min-0.001, x2max+0.001, ...
          min(min(x3))-0.001,  max(max(x3))+0.001]);
    view(-37.5, 45.);
    set(gca,'Zdir','reverse'); 
    set(gca,'Fontsize', 14);
    xlabel('x_1');   ylabel('x_2');   zlabel('Depth');
    title(['Interface ', num2str(Iinterface)], ...
           'Fontsize', 16, 'FontWeight', 'bold');
  end;

% Plot all interfaces
  if Iinterface ~= 1;
    figure(figInterfaceall);   
%%  surf(x1,x2,x3);   hold on;  
    mesh(x1,x2,x3);   hold on;  
    x3min = min(x3min, min(min(x3)));   x3max = max(x3max, max(max(x3)));
  end;
end;

% Annotate the all-interface plot
figure(figInterfaceall);
set(gca,'Zdir','reverse');
set(gca,'Fontsize', 14);
xlabel('x_1');   ylabel('x_2');   zlabel('Depth');
title('Zero-offset rays', 'Fontsize', 12, 'FontWeight', 'bold');

% Plot positions of the sources and receivers
x3max = max([x3max, max(max(Source(3,:))), max(max(Receiv(3,:)))]);
x3max = x3max + 0.1*abs(x3max-x3min);
x3min = min([x3min, min(min(Source(3,:))), min(min(Receiv(3,:)))]);
x3min = x3min - 0.1*abs(x3max-x3min);
axis([x1min, x1max, x2min, x2max, x3min,  x3max]);
view(65, 12);
view(-15, 10);

% Plot sources and receivers
plot3(Source(1,:), Source(2,:), Source(3,:), '^', ...
      'LineWidth', 2, 'MarkerSize', 7, ...
      'MarkerEdgeColor', 'r', 'MarkerFaceColor', 'r');
plot3(Receiv(1,:), Receiv(2,:), Receiv(3,:), '^', ...
      'LineWidth', 2, 'MarkerSize', 7, ...
      'MarkerEdgeColor', 'r', 'MarkerFaceColor', 'r');

%==============================================================================

