libname b "C:\Users\jared_000\Desktop\SGH\Thesis";

%include "C:\Users\jared_000\Desktop\SGH\Thesis\user2.sas";
run;

ods html;

data b.user2;
set Rdata;
run;

data work;
set b.user2;
if userid=15 then life=0;
run;

proc sort data=work;
by userid;
run;

/*Set this value to determine inactivity threshold*/
data work;
thresh = 30;
set work;
rc = 0;
if inactive >= thresh then rc = 1;
if rc = 1 then life = life+thresh;
run;

proc lifetest data=work method=km alpha=0.05 plots=( survival(cl cb=ep atrisk) hazard ls lls p);
	time life * rc(0);
	id userid;
run;

proc lifetest data=work method=life alpha=0.05 plots=( survival(cl cb=ep atrisk) hazard ls lls p) 
				INTERVALS=30 90 180 365 730 1095 1460 1825 2190 2555 2920 3285;
	time life * rc(0);
	id userid;
run;

PROC LIFEREG DATA=work;
MODEL life*rc(0)=styleid maxposts referrerid
/ DISTRIBUTION=WEIBULL;
RUN;

PROC PHREG DATA=work;
MODEL life*rc(0)=styleid maxposts referrerid;
RUN;
