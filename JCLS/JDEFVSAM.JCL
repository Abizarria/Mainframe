//JDEFVSAM JOB IBMUSERC,MSGCLASS=A,CLASS=C,NOTIFY=ALU0407               00010004
//*                                                                     00020000
//*SYSOUT   DD SYSOUT=*                                                 00030000
//*                                                                     00040000
//*----------------------------------------------------------*          00050008
//* CRIA ARQUIVO VSAM - O NOME NAO PODE SER UM DSN ALOCADO   *          00060008
//* SE O AQUIVO EXISTIR PRECISA ESTAR FECHADO NO CICS (CEMT) *          00061008
//*----------------------------------------------------------*          00070008
//DEDEF1   EXEC PGM=IDCAMS,REGION=1M                                    00080000
//DDFILE   DD   UNIT=3390,VOL=SER=PR39D7,DISP=SHR                       00090000
//SYSPRINT DD SYSOUT=*                                                  00100000
//SYSIN    DD *                                                         00110000
   DELETE      (ABS.VSAM.CADTST.CLUSTER)                                00120010
   DEFINE CLUSTER -                                                     00130000
          (NAME(ABS.VSAM.CADTST.CLUSTER) -                              00140009
           RECORDSIZE(135 135) -                                        00150001
           VOLUME(PR39D7)    -                                          00160000
           KEY(5 0)          -                                          00170000
           INDEXED           -                                          00180000
           CYL(01 01)        -                                          00190000
           FREESPACE(10 10)  -                                          00200000
           SHAREOPTIONS(2 3) -                                          00210000
          CONTROLINTERVALSIZE(4096) -                                   00220000
           UNIQUE -                                                     00230000
           IMBED -                                                      00240000
           REPLICATE) -                                                 00250000
         DATA -                                                         00260000
         (NAME(ABS.VSAM.CADTST.DATA)) -                                 00270009
         INDEX -                                                        00280000
         (NAME(ABS.VSAM.CADTST.INDEX))                                  00290009
//*                                                                     00300000
