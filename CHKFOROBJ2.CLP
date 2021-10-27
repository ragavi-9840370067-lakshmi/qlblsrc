000100050113/*------------------------------------------------------------------*/
000200050113/* TITLE        : Check for existence of an object                  */
000300050113/* SYSTEM       : UNI                                               */
000400050113/* PROGRAM TYPE : CL                                                */
000500050113/* PROGRAMMER   :                                                   */
000600050113/* PROGRAM      : CHKFOROBJ                                         */
000700050113/* DESCRIPTION  :                                                   */
000800050113/* DATE CREATED : 2005/01/13  RFS 26144                             */
000900050113/*------------------ MODIFICATION HISTORY --------------------------*/
001000050113/* PROGRAMMER      | DATE      |           COMMENTS                 */
001100050113/*-----------------|-----------|------------------------------------*/
001200050113/*                  YYYY/MM/DD                                      */
001300050113/*------------------------------------------------------------------*/
001400050113             PGM        PARM(&OBJECT &TYPE &RESULT)
001500050113
001600990528             DCL        VAR(&OBJECT)  TYPE(*CHAR) LEN(10)
001700990528             DCL        VAR(&TYPE)    TYPE(*CHAR) LEN(10)
001800050113             DCL        VAR(&RESULT)  TYPE(*CHAR) LEN(8)
001900990528
002000050113             CHGVAR     VAR(&RESULT) VALUE(' ')
002100050113
002200990528             CHKOBJ     OBJ(&OBJECT) OBJTYPE(&TYPE)
002300050113             MONMSG     MSGID(CPF9801) EXEC(DO)
002400050113                          CHGVAR     VAR(&RESULT) VALUE('NOTFOUND')
002500050113                          ENDDO
002600990528
002700970428             ENDPGM
