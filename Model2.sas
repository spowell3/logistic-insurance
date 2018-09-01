libname log 'C:\Users\Steven\Documents\MSA\Analytics Foundations\data\Logistic Data';


data insurance_t;
	set log.insurance_t;
	keep DDA, DDABAL, 
          PHONE, TELLER, SAV, SAVBAL, ATMAMT, 
          CD, INV, IRA, CDBAL, 
          MM, MTG, CC, INS;
run;
proc logistic data=insurance_t plots(only)=(effect(clband showobs) oddsratio);
	class sav(ref='0') atm(ref='0')  / param=ref;
	model ins(event='1') = DDA DDABAL 
          PHONE TELLER SAV SAVBAL ATMAMT 
          CD INV IRA CDBAL 
          MM MTG CC / plcl plrl;
	title 'logreg attempt';
run;
