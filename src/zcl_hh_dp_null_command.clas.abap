CLASS zcl_hh_dp_null_command DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    interfaces: zif_hh_dp_command.

    aliases:
      execute for zif_hh_dp_command~execute,
      undo for zif_hh_dp_command~undo.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_hh_dp_null_command IMPLEMENTATION.
  METHOD zif_hh_dp_command~execute.

  ENDMETHOD.

  METHOD zif_hh_dp_command~undo.

  ENDMETHOD.

ENDCLASS.
