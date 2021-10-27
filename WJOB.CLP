000100040511/*----------------------------------------------------------------*/
000200040517/* PROGRAM     : WJOB                                             */
000300040511/* TYPE        : CLP                                              */
000400040511/* PROGRAMMER  : KEVIN TROTMAN                                    */
000500040511/*----------------------------------------------------------------*/
000600040511/* CREATED ON  : 2003/05/14                                       */
000700040511/* CALLED BY   :                                                  */
000800040517/* DESCRIPTION : WORK JOB            COMMANDS                     */
000900040511/* PARAMETERS  :                                                  */
001000040511/*                                                                */
001100040511/*----------------------------------------------------------------*/
001200050106/* CHANGED BY * YYYY/MM/DD   * DESCRIPTION OF CHANGE               */
001300050106/*-----------------------------------------------------------------*/
001400050106/* R.MOROZ    * 2005/01/06   * RFS 25038 Recompile only            */
001500050106/*-----------------------------------------------------------------*/
001600040526 WJOB:       PGM        PARM(&MOD &NBR &USR &JOB &LIB &QUE)
001700040511/*--- Input Variables        */
001800040511
001900040517             DCL        VAR(&MOD) TYPE(*CHAR) LEN(03)
002000040526             DCL        VAR(&NBR) TYPE(*CHAR) LEN(10)
002100040517             DCL        VAR(&USR) TYPE(*CHAR) LEN(10)
002200040517             DCL        VAR(&JOB) TYPE(*CHAR) LEN(10)
002300040526             DCL        VAR(&QUE) TYPE(*CHAR) LEN(10)
002400040526             DCL        VAR(&LIB) TYPE(*CHAR) LEN(10)
002500040511/*--- Working Storage        */
002600040426
002700040511/*--- File Declaration       */
002800040517
002900040511/*--- Procedure Division     */
003000040511             MONMSG     MSGID(CPF0000)
003100040511
003200040526
003300040517             IF         COND(&MOD *EQ 'CHG') THEN(DO)
003400040526             ?          CHGJOB JOB(&NBR/&USR/&JOB) JOBQ(&LIB/&QUE)
003500040517             RETURN
003600040517             ENDDO
003700040517
003800040517             IF         COND(&MOD *EQ 'HLD') THEN(DO)
003900040526             ?          HLDJOB JOB(&NBR/&USR/&JOB)
004000040517             RETURN
004100040517             ENDDO
004200040517
004300040517             IF         COND(&MOD *EQ 'DLT') THEN(DO)
004400040526             ?          ENDJOB JOB(&NBR/&USR/&JOB)
004500040517             RETURN
004600040517             ENDDO
004700040517
004800040517             IF         COND(&MOD *EQ 'DSP') THEN(DO)
004900040526             DSPJOB     JOB(&NBR/&USR/&JOB) OPTION(*JOBLOG)
005000040517             RETURN
005100040517             ENDDO
005200040517
005300040517             IF         COND(&MOD *EQ 'RLS') THEN(DO)
005400040526             RLSJOB     JOB(&NBR/&USR/&JOB)
005500040517             RETURN
005600040517             ENDDO
005700040517
005800040517
005900040426             RETURN
006000040426             ENDPGM
