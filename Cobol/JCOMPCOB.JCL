//JCOMPCOB JOB IBMUSERB,MSGCLASS=A,CLASS=C,NOTIFY=USERNAME              00010099
//IGYWCLG PROC SYSLBLK=3200,                                            00020099
//             LIBPRFX='CEE'                                            00030099
//*                                                                     00040099
//* SUBSTTITUIR USERNAME E PROGNAME CONFORME OBJETIVO                   00041099
//*                                                                     00042099
//COBOL  EXEC  PGM=IGYCRCTL,REGION=2048K,                               00050099
//             PARM=(LIST,MAP,APOST,DYNAM)                              00060099
//SYSPRINT DD  SYSOUT=*                                                 00070099
//STEPLIB  DD  DISP=SHR,DSN=IGY.V2R1M0.SIGYCOMP                         00080099
//SYSLIN   DD  DSNAME=&&LOADSET,UNIT=SYSDA,                             00090099
//             DISP=(MOD,PASS),SPACE=(TRK,(3,3)),                       00100099
//             DCB=(BLKSIZE=&SYSLBLK)                                   00110099
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))                             00120099
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))                             00130099
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))                             00140099
//SYSUT4   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))                             00150099
//SYSUT5   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))                             00160099
//SYSUT6   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))                             00170099
//SYSUT7   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))                             00180099
//LKED   EXEC  PGM=HEWL,COND=(4,LT,COBOL),REGION=1024K                  00190099
//SYSLIB   DD  DSNAME=&LIBPRFX..SCEELKED,DISP=SHR                       00200099
//         DD  DSN=CSF.SCSFMOD0,DISP=SHR                                00210099
//         DD  DSN=IBMUSER.PGMS.LOADLIB,DISP=SHR                        00220099
//SYSPRINT DD  SYSOUT=*                                                 00230099
//SYSLIN   DD  DSNAME=&&LOADSET,DISP=(OLD,DELETE)                       00240099
//         DD  DDNAME=SYSIN                                             00250099
//SYSLMOD  DD  DSN=FUTURE.LMODUL.COBOL(PROGNAME),DISP=SHR               00260099
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(10,10))                           00270099
//STEPLIB  DD  DSNAME=&LIBPRFX..SCEERUN,DISP=SHR                        00280099
//SYSPRINT DD  SYSOUT=A                                                 00290099
//CEEDUMP  DD  SYSOUT=*                                                 00300099
//SYSUDUMP DD  SYSOUT=*                                                 00310099
//SYSOUT   DD  SYSOUT=A                                                 00320099
//         PEND                                                         00330099
//COBGO    EXEC  IGYWCLG                                                00340099
//COBOL.SYSIN DD DSN=ABS.AULAS.COBOL(PROGNAME),DISP=SHR                 00350099
