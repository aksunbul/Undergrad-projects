clc;
clear all;

dy=importdata('DistanceTable.xlsx');
dy=dy.DistanceTable;
dy=dy(2:end,2:94);
bgg=10^6;
for i=1:93
dy(i,i)=bgg;
end
data=dy;

%%%%q1-2%%%
limitkm=30000;
i=1;
km=limitkm;
path=[i];
flag=0;
while ~flag
    while (km>0)*(~flag)
        [mt,j]=min(data(i,:));
        km=km-mt;
        data(:,i)=bgg;
        i=j;
        path=[path i];
        flag=(length(unique(path))==93);
    end
path=[path 1];
i=1;
km=limitkm;
% flag=(unique(path)==93);
end

nmotors=sum(path==1)-1;

%%%%q3%%%%
snc=importdata('sonuc.txt');
dc=[];

for i=2:length(snc)
    dc=[dc ;str2num(snc{i}(end)) str2num(snc{i}(end-4:end-3))];
end
idc=find(dc(:,1));
locdc=unique(dc(idc,2));

tmp=[];
for i=1:length(locdc)
    tmp=[tmp; locdc(i) locdc(i)];
end

%options=[combnk(locdc,2); tmp];
options=[ tmp];


dX=[];
for i=1:length(options)
data=dy;
mloc=options(i,:);
data(mloc,:)=bgg;
km=0;
path=[];
path{1}=mloc(1);
path{2}=mloc(2);
flag=0;
    while ~flag
        [mt,j]=min(data(:,mloc));
        [mtt whic]=min(mt);
        km=km+mtt;
        mloc(whic)=j(whic);
        data(j(whic),:)=bgg;
        path{whic}=[path{whic} j(whic)];
        flag=(length(unique([path{1} path{2}]))==93);
    end
dX=[dX km];
allpath{i}=path;
% flag=(unique(path)==93);
end

[minkm whicoption]=min(dX);
minmotors=options(whicoption,:)
mnpath1=allpath{whicoption}{1}
mnpath2=allpath{whicoption}{2}

pth=mnpath1;
points=[pth(2:end); pth(1:end-1)];
plot(points(1,:),points(2,:),'ro:');hold on;

pth=mnpath2;
points=[pth(2:end); pth(1:end-1)];
plot(points(1,:),points(2,:),'bo:');

ax=[1 97 1 97];
grid on
grid minor


%%%%q4%%%%
snc=importdata('sonuc.txt');
dc=[];

for i=2:length(snc)
    dc=[dc ;str2num(snc{i}(end)) str2num(snc{i}(end-4:end-3))];
end
idc=find(dc(:,1));
locdc=unique(dc(idc,2));

tmp=[];
for i=1:length(locdc)
    for j=1:length(locdc)
        for k=1:length(locdc)
            tmp=[tmp ; locdc(i) locdc(j) locdc(k)];
        end
    end 
end

options=tmp;
dX=[];
for i=1:length(options)
data=dy;
mloc=options(i,:);
data(mloc,:)=bgg;
km=zeros(1,size(options,2));
path=[];
%notavailable=zeros(1,size(options,2));
for i=1:size(options,2)
path{i}=mloc(i);
end
flag=0;
bittila=0;
    while ~flag %| sum(notavailable)~=length(notavailable)
        [mt,j]=min(data(:,mloc));
        if bittila>0 mt(bittila)=bgg; bittila=0; end
        [mtt whic]=min(mt);
        if (km(whic)+mtt)<30000;
            km(whic)=km(whic)+mtt;
            mloc(whic)=j(whic);
            data(j(whic),:)=bgg;
            path{whic}=[path{whic} j(whic)];

            CNTPATH=[];
            for i=1:size(options,2)
                CNTPATH=[path{i} CNTPATH];
            end

            flag=(length(unique(CNTPATH))==93);
        else
            bittila=whic;
        end
    end
dX=[dX ;km];
allpath{i}=path;
% flag=(unique(path)==93);
end

[minkm whicoption]=min(dX);
minmotors=options(whicoption,:)
mnpath1=allpath{whicoption}{1}
mnpath2=allpath{whicoption}{2}

pth=mnpath1;
points=[pth(2:end); pth(1:end-1)];
plot(points(1,:),points(2,:),'ro:');hold on;

pth=mnpath2;
points=[pth(2:end); pth(1:end-1)];
plot(points(1,:),points(2,:),'bo:');

ax=[1 97 1 97];
grid on
grid minor
