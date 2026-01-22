# UML desing

```{r}

#install.packages("remotes")
#remotes::install_github("rstudio/nomnoml")
 
library(nomnoml) 

nomnoml::nomnoml("
#stroke: #003366
#direction: right

[AnnualObservation|
PlotId;HarvestYear|
StudyId;Origin;Jbnr;PrecropDa;CropDa;Code;PrecropEn;CropEn;
PercTotal;NLeachYear;Pp;Evap;EvapPot;PercYear;
DeltaSoilW;NMineralSpring;NMineralAutumn;NOrgSpring;
NOrgAutumn;Jb;NTopsoil;Clay;
NLevelTotN;OrgC1;OrgC2;OrgC3;TotN1;TotN2;TotN3;
VolVgt1;VolVgt2;VolVgt3;
Clay1;Clay2;Clay3;Humus1;Humus2;Humus3;*
;Id;NOrgYear;ClayDa;Humus;NFix;NLeaching;WinterCover;MainPrevCrop;
WinterPrevCrop;MainCrop;WinterCrop;NFromGrassingAnimals;NoDailyObs;MeanConc;SdConc;
NUdb;NMinTotal;NOrgTotal;
VForf;MAfgr;Vf;Mf;Ma;Va;Mff;Maf;Vaf;NLevelF;VAfgr;MForf;OptNMafg;OptNVafgr;MinHSpringU;MinHSpringK;
]<-[Experiment]

[AnnualObservation]1..1<->1..*[Hydrology]
[AnnualObservation]1..1<->1..*[Concentration]
[Site]1..1->1..*[AnnualObservation]

[Experiment|
ExpID|
StartYear;EndYear;Manager;end_date;Sites;ExpName;Notes;OtherMeas]

[Publication|
 PubType;Authors; PubYear; Title; ExpNo; Journal; doi]

[Site|
SiteEn|
SiteDen;POINTGEOM;CellDmi10New;DmiGridNumOld;XCentreDmiOld;
 YCentreDmiOld;XLonPoint;YLatPoint;exp_dessing_shp]

[Hydrology|
PlotId;date|
year;month;day;pp;temp;temp_max;temp_min;glob_rad;epa;irrigation;perc
]

[Concentration|
PlotId;date|
year;month;day;meankonc
]


[Experiment]->1..*[Site]
[Experiment]->1..*[AnnualObservation]
[Experiment]->1..*[Publication]

[Site]1..1->1..*[Concentration]
[Concentration]1..0<-1..1[Hydrology]"
, 
svg = TRUE)

```

$$\begin{bmatrix}
y_1 \\ y_2 \\ \vdots \\ y_n
\end{bmatrix} =
\begin{bmatrix}
x_{11} & x_{12} & \dots & x_{1p} \\
x_{21} & x_{22} & \dots & x_{2p} \\
\vdots & \vdots & \ddots & \vdots \\
x_{n1} & x_{n2} & \dots & x_{np}
\end{bmatrix}
\begin{bmatrix}
\beta_1 \\ \beta_2 \\ \vdots \\ \beta_p
\end{bmatrix} +
\begin{bmatrix}
\epsilon_1 \\ \epsilon_2 \\ \vdots \\ \epsilon_n
\end{bmatrix}$$

```{r}
lm(Y~X)
```

