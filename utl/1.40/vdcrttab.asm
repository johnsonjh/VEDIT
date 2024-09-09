;
; 		Last change:	04/18/83 RICK FORTSON
;				11/29/84 JEFF KNEALE
;
NUMCRT	=	57		;Number of CRTs in table.
;
	ORG	. & 0FFC0H + 40H
;
CRTMSG:	DC	" (1)  ACT-IV                           (16)  DATAMEDIA" [CR] [LF]
	DC	" (2)  ADDS REGENT 20, 25               (17)  DEC VT-52" [CR] [LF]
	DC	" (3)  ADDS REGENT 30, 35, 40, 60       (18)  DEC VT-100, VT-180" [CR] [LF]
	DC	" (4)  ADDS REGENT 100                  (19)  DEC RAINBOW" [CR] [LF]
	DC	" (5)  ADDS VIEWPOINT                   (20)  DYNABYTE 57" [CR] [LF]
	DC	" (6)  ADDS VIEWPOINT/3A PLUS           (21)  EMULOG 200" [CR] [LF]
	DC	" (7)  ADM-3A                           (22)  FULCRUM VIO-X2" [CR] [LF]
	DC	" (8)  ADM-31                           (23)  HAZELTINE / VOLKER-CRAIG" [CR] [LF]
	DC	" (9)  AMPEX DIALOGUE 80                (24)  HEATH/ZENITH H19, H89" [CR] [LF]
	DC	"(10)  ANN ARBOR 400D                   (26)  HDS CONCEPT" [CR] [LF]
	DC	"(11)  ANSI STANDARD                    (26)  HP-150" [CR] [LF]
	DC	"(12)  ANSI SUBSET (MSDOS 2.0)          (27)  HP 2621, 2645" [CR] [LF]
	DC	"(13)  BEEHIVE                          (28)  IBM 3101"[CR] [LF]
	DC	"(14)  CONTROL DATA CD-110              (29)  IBM DISPLAYWRITER" [CR] [LF]
	DC	"(15)  CONTROL DATA CD-722              (30)  INFOTON 100" [CR] [LF]
	DB	CR,LF
	DCS	"ENTER ANY KEY TO CONTINUE MENU "
;
CRTMS1:	DB	CR,LF
	DC	"(31)  INTERTUBE II (UP TO VER. 1.7)    (46)  TELERAY" [CR] [LF]
	DC	"(32)  INTERTUBE II (LATER), III        (47)  TELEVIDEO 920, 912" [CR] [LF]
	DC	"(33)  ISC 8063 AND OTHERS              (48)  TELEVIDEO 950, 925, 910" [CR] [LF]
	DC	"(34)  KIMTRON ABM 85                   (49)  TI PC (CP/M-86)" [CR] [LF]
	DC	"(35)  LIBERTY FREEDOM 100              (50)  TI PC (MSDOS)" [CR] [LF]
	DC	"(36)  LINWOOD BETA BANDIT              (52)  VICTOR 9000" [CR] [LF]
	DC	"(37)  NEC APC                          (52)  VOLDER-CRAIG 404" [CR] [LF]
	DC	"(38)  NORTH STAR ADVANTAGE             (53)  WYSE WY-50" [CR] [LF]
	DC	"(39)  PERKIN ELMER 1251, 1245          (54)  WYSE WY-100" [CR] [LF]
	DC	"(40)  PERKIN ELMER BANTAM 550          (55)  XEROX 820" [CR] [LF]
	DC	"(41)  SD VDB 8024                      (56)  CUSTOMER 2" [CR] [LF]
	DC	"(42)  SOROC 120, 140                   (57)  CUSTOMER 1" [CR] [LF]
	DC	"(43)  SUPERBRAIN" [CR] [LF]
	DC	"(44)  SWTPC CT-82" [CR] [LF]
	DC	"(45)  TEC 500" [CR] [LF]

	DB	CR,LF
	DCS	"ENTER NUMBER OF CRT TO BE USED "

