INTERFACE zif_hh_dp_report_screen
  PUBLIC .
    constants:
      refresh type syucomm value 'REFRESH',
      report_status_name type sypfkey value 'REPORT_SCREEN',
      resume type syucomm value 'RESUME',
      stop type syucomm value 'STOP',
      turn_left type syucomm value 'TURNLEFT',
      turn_right type syucomm value 'TURNRIGHT'.
ENDINTERFACE.
