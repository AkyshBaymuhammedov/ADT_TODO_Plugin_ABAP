CLASS zcl_ab_todo_discovery_res_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_app_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.
  PROTECTED SECTION.
    METHODS: fill_router REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ab_todo_discovery_res_app IMPLEMENTATION.

  METHOD fill_router.

    router->attach( iv_template = '/discovery' iv_handler_class = cl_adt_res_discovery=>co_class_name ).

  ENDMETHOD.

  METHOD if_adt_rest_rfc_application~get_static_uri_path.

    result = '/msg/akysh'.

  ENDMETHOD.

ENDCLASS.
