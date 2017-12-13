a =[1,100];
c= [239,256];
d = [76,112];
figure1 = figure;hold on;
h= plot(a,c,a,d);
set(gca,'xtick',[])
set(gca,'xticklabel',[])
legend(h,'Low B','High B')
xlabel('C - low to high as you go from left to right')