000100070323 /* Ahmed Z.  - 2007/03/21 - RFS40906 - The message sent should   */
000200070323 /*           -            - be logged in the job log.            */
000300140410 /* Prabudha J- 2013/11/11 - RFS130051 - Recompile only           */
000400190328 /* B. Vergara - 2019/03/21 - RFS183202 - Updated Error Message   */
000500070323 /*---------------------------------------------------------------*/
000600070323
000700970527             PGM        PARM(&RUNNAME &REPLY)
000800970527             DCL        VAR(&RUNNAME) TYPE(*CHAR) LEN(10)
000900970527             DCL        VAR(&RTVRUN)  TYPE(*CHAR) LEN(10)
001000970527             DCL        VAR(&REPLY)   TYPE(*CHAR) LEN(1)
001100190328/*rfs183202- begin */
001200190118/*           DCL        VAR(&MESSAGE) TYPE(*CHAR) LEN(120)   */
001300190328             DCL        VAR(&MESSAGE) TYPE(*CHAR) LEN(600)
001400190328/*rfs183202 - end */
001500190118
001600070323             DCL        VAR(&LOGCLPGM) TYPE(*CHAR) LEN(10) /* +
001700070323                          RFS40906 */
001800970528             RTVDTAARA  DTAARA(*LDA (201 10)) RTNVAR(&RTVRUN)
001900970527             CHGVAR     VAR(&REPLY)   VALUE(' ')
002000970527
002100970527             IF COND(&RUNNAME *EQ &RTVRUN) THEN(GOTO CMDLBL(END))
002200970527
002300190328/* rfs183202 - begin */
002400190328/*             CHGVAR     VAR(&MESSAGE) VALUE('An error has occurred +   */
002500190328/*                          in running' *BCAT &RUNNAME *TCAT '.  +       */
002600190328/*                          Please call support to investigate. Reply +  */
002700190328/*                          I, R, C')                                    */
002800190118
002900190328               CHGVAR VAR(&MESSAGE) VALUE('An error has occurred in running ' +
003000190328                     *BCAT &RUNNAME                                           +
003100190328                     *TCAT ' .Please call  support to investigate.'            +
003200190328                     *TCAT ' 1. Check Job log '                                +
003300190328                     *TCAT ' 2. STRSRVJOB / STRDBG and save critical info'     +
003400190328                     *TCAT ' 3. Save necessary data / files / DB for rerun'    +
003500190328                     *TCAT  '. Reply I or C to continue')
003600190328
003700070323             RTVJOBA    LOGCLPGM(&LOGCLPGM) /* RFS40906 */
003800070323             IF         COND(&LOGCLPGM *EQ '*NO') THEN(CHGJOB +
003900070323                          LOGCLPGM(*YES)) /* RFS40906 */
004000190328/* rfs183202 - begin */
004100190118/*           SNDUSRMSG  MSG(&MESSAGE) VALUES('C' 'c' 'R' 'r' 'I' 'i') +
004200190118             TOMSGQ(*SYSOPR) MSGRPY(&REPLY) */
004300190118             SNDUSRMSG  MSG(&MESSAGE) VALUES('C' 'c' 'I' 'i') +
004400190118                          TOMSGQ(*SYSOPR) MSGRPY(&REPLY)
004500190328/* rfs183202 - end */
004600190118
004700970527
004800070323             SNDPGMMSG  MSG('Reply . . :   ' *CAT &REPLY)
004900070323             IF         COND(&LOGCLPGM *EQ '*NO') THEN(CHGJOB +
005000070323                          LOGCLPGM(*NO)) /* RFS40906 */
005100070323
005200970527 END:        CHGDTAARA  DTAARA(*LDA (201 10)) VALUE('          ')
005300970527             RETURN
005400970428             ENDPGM
