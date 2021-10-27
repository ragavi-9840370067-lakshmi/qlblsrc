000001170313     /* %ATTR OPTION(*LSTDBG) */
000101041208/*----------------------------------------------------------------*/
000201130829/* PROGRAM     : JBAWDRDTE                                        */
000301041208/* TYPE        : CLP                                              */
000401130829/* PROGRAMMER  : DAPHNE D                                         */
000501041208/*----------------------------------------------------------------*/
000601130829/* CREATED ON  : 2013/08/23                                       */
000701041208/* CALLED BY   :                                                  */
000801130828/* DESCRITPION : CALL PROGRAM CVAWDRDTE.                          */
000901061018/*----------------------------------------------------------------*/
001001130829/* DAPHNE D   * 2013/08/23   * RFS124934- Change Request for        */
001101130823/*            *              * Conversion of Distribution Options   */
001201110509/* ******************************************************************/
001301130823             PGM
001401041208
001501041208/*--- Input Variables        */
001601130828             DCL        VAR(&REC) TYPE(*DEC) LEN(10 0) VALUE(0)
001701130823
001801130828/*--- Retrieve the record count from CVACCRDP  */
001901130828             RTVMBRD    FILE(CVACCRDP) NBRCURRCD(&REC)
002001130828
002101130828/*--- Check the count */
002201130823             IF         COND(&REC *GT 0) THEN(CALL PGM(CVAWDRDTE))
002301041208
002401130823/*--- End Of Program         */
002501130823             ENDPGM
