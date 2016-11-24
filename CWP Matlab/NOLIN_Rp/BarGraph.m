%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BarGraph.m is an auxiliar program to plot bar-graphs - distribution of the
% inverted models - in the form [local minima (x-axis), number of models
% (y-axis) associated. This routine is NOT fully automated, so the number
% and values of detected minima and the corresponding number of models must
% be entered manually. Also, the axes description is left for the user. The
% user should have some knowledge of basic Matlab graphic routines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close all;
clear all;

% values of local minima detected; x-axis ticks
X=[0.21 0.22 0.23 0.25 0.26 0.29 0.33 0.35 0.43 0.52 0.55 0.63 ...
   0.69];
% number of models corresponding to the vector X; y-axis ticks
Y=[30 12 1 4 4 2 2 3 6 1 2 3 3];
% y-axis labels - not always used, see the set up below
YY=[1 2 3 4 6 12 30]

figure('Position',[300    200    750    400]);
bar(X,Y,'stacked');
set(gca,'Position',[0.06    0.15    0.90    0.80]);

xlabel('F_{obj} local minima','FontSize',20,'Position',[0.45 -3 0]);
ylabel('number of models','FontSize',20);
%set(gca,'YTick',YY,'FontSize',13);
%set(gca,'XTick',X,'XTickLabel',['0.21';' ';'0.23';' ';'0.26';'0.29';'0.33';'0.35';'0.43';'0.52';'0.55';'0.63';'0.69']);
set(gca,'YTick',YY,'YTickLabel',[' ']);
set(gca,'XTick',X,'XTickLabel',...
     [' ']);
text(0.20, -2.0, '0.21', 'Fontsize', 13);
text(0.25, -2.0, '0.22', 'Fontsize', 13);
text(0.30, -2.0, '0.23', 'Fontsize', 13);
text(0.35, -2.0, '0.25', 'Fontsize', 13);
text(0.20, -1.0, '0.26', 'Fontsize', 13);
text(0.25, -1.0, '0.29', 'Fontsize', 13);
text(0.30, -1.0, '0.33', 'Fontsize', 13);
text(0.35, -1.0, '0.35', 'Fontsize', 13);
text(0.4, -1.0, '0.43', 'Fontsize', 13);
text(0.45, -1.0, '0.52', 'Fontsize', 13);
text(0.50, -1.0, '0.55', 'Fontsize', 13);
text(0.55, -1.0, '0.63', 'Fontsize', 13);
text(0.60, -1.0, '0.69', 'Fontsize', 13);

text(0.18, 1.0, '1', 'Fontsize', 13);
text(0.18, 2.0, '2', 'Fontsize', 13);
text(0.18, 3.0, '3', 'Fontsize', 13);
text(0.18, 4.0, '4', 'Fontsize', 13);
text(0.18, 6.0, '6', 'Fontsize', 13);
text(0.18, 12.0, '12', 'Fontsize', 13);
text(0.18, 30.0, '30', 'Fontsize', 13);

grid on;
