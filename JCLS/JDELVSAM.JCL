//JDELVSAM JOB IBMUSERC,MSGCLASS=A,CLASS=C,NOTIFY=ALU0407               00010002
//JOBLIB   DD  DSN=FUTURE.LMODUL.COBOL,DISP=SHR                         00020002
//*                                                                     00040002
//* DELETA ARQUIVO VSAM                                                 00050002
//*                                                                     00060002
//DDLST1   EXEC PGM=IDCAMS,REGION=0M                                    00070002
//SYSPRINT DD SYSOUT=*                                                  00080002
//SYSOUT   DD SYSOUT=*                                                  00090002
//SYSIN    DD *                                                         00100002
 /* DELETANDO O ARQUIVO VSAM */                                         00110002
                                                                        00120002
   DELETE ABS.VSAM.CADTST.CLUSTER                                       00130003
          PURGE                                                         00140003
          SET MAXCC = 0                                                 00150002
//*                                                                     00170002
