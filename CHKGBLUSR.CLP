000100070228/*------------------------------------------------------------------*/
000200070228/* TITLE        : CHECK IF USER IS IN THE GLOBAL ENVIRONMENT.       */
000300070228/* SYSTEM       :                                                   */
000400070228/* PROGRAM TYPE : CL                                                */
000500070228/* PROGRAMMER   : KIRISHA WIJAYARATNAM                              */
000600070228/* PROGRAM      : CHKGBLUSR                                         */
000700070228/* DESCRIPTION  : This process checks GBL environments and          */
000800070228/*              : returns a user status indicating whether the user */
000900070228/*              : exists in the global environment.                 */
001000070228/*              : If USRSTS = 'N' means user does not exist in gbl  */
001100070228/*              :             environment.                          */
001200070228/*              : If USRSTS = 'Y' means user does exist in gbl.     */
001300070228/* DATE CREATED : Feb 28, 2007                                      */
001400070228/*------------------ MODIFICATION HISTORY --------------------------*/
001500070228/* PROGRAMMER      | DATE      |           COMMENTS                 */
001600070228/*-----------------|-----------|------------------------------------*/
001601070302/* R MIGUEL        * 2007/03/02* RFS 36813 - NEW PROGRAM.           */
001700070228/*-----------------|-----------|------------------------------------*/
001800070228             PGM        PARM(&USRSTS)
001900070228
002000070228             DCL        VAR(&USRSTS)     TYPE(*CHAR) LEN(01)
002100070228             DCL        VAR(&CLTGBLDTA)  TYPE(*CHAR) LEN(10)
002200070228             DCL        VAR(&CLTTBL)     TYPE(*CHAR) LEN(10)
002300070228             DCL        VAR(&GBLDTA)  TYPE(*CHAR) LEN(10)
002400070228
002500070228             /*Set Default Value to 'N'*/
002600070228             CHGVAR VAR(&USRSTS) VALUE('N')
002700070228
002800070228             /*If the object does not exsit, exit the program*/
002900070228             CHKOBJ     OBJ(MFAGBLDTA) OBJTYPE(*DTAARA)
003000070228             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(ENDPGM))
003100070228
003200070228             /*Retrieves MFAGBLDTA in the clients OJ*/
003300070228             RTVDTAARA  DTAARA(MFAGBLDTA) RTNVAR(&CLTGBLDTA)
003400070228
003500070228             /*Retrieves clients tables library*/
003600070228             RTVDTAARA  DTAARA(MFATABLES) RTNVAR(&CLTTBL)
003700070228
003800070228             IF COND(&CLTGBLDTA *EQ &CLTTBL)           +
003900070228                THEN(DO)
004000070228                    CHGVAR VAR(&USRSTS) VALUE('Y')
004100070228                ENDDO
004200070228
004300070228 ENDPGM:     ENDPGM
