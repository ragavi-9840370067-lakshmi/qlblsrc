000100041208/*----------------------------------------------------------------*/
000200130823/* PROGRAM     : START PROGRAM                                    */
000300041208/* TYPE        : CLP                                              */
000400130828/* PROGRAMMER  : SANGEETHA V                                      */
000500041208/*----------------------------------------------------------------*/
000600130828/* CREATED ON  : 2013/08/28                                       */
000700041208/* CALLED BY   :                                                  */
000800130830/* DESCRITPION : CALL PROGRAM CVAWDPDTE.                          */
000900061018/*----------------------------------------------------------------*/
001000130828/* SANGEETHA V* 2013/08/28   * RFS124934- Change Request for        */
001100130823/*            *              * Conversion of Distribution Options   */
001200110509/* ******************************************************************/
001300130823             PGM
001400041208
001500041208/*--- Input Variables        */
001600130828             DCL        VAR(&REC) TYPE(*DEC) LEN(10 0) VALUE(0)
001700130823
001800130828/*--- Retrieve the record count from CVACCRDP  */
001900130828             RTVMBRD    FILE(CVACCRDP) NBRCURRCD(&REC)
002000130828
002100130828/*--- Check the count */
002200130830             IF         COND(&REC *GT 0) THEN(CALL PGM(CVAWDPDTE))
002300041208
002400130823/*--- End Of Program         */
002500130823             ENDPGM
