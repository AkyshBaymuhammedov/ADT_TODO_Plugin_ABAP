CLASS zcl_ab_todo_res_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.

  PROTECTED SECTION.

    METHODS: get_application_title REDEFINITION,
      register_resources REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ab_todo_res_app IMPLEMENTATION.

  METHOD get_application_title.

    result = 'TODOs'.
  ENDMETHOD.

  METHOD register_resources.

    registry->register_discoverable_resource(
        url             = '/resttodos/ab/todos'
        handler_class   = 'CL_SFLIGHT_ADT_RES_CARRIERS'
        description     = 'FIXMEs'
        category_scheme = 'http://www.msg.com/adt/categories/ab/todos'
        category_term   = 'fixmes'
    ).

    DATA(collection) = registry->register_discoverable_resource(
                                   url             = '/resttodos/ab/todos'
                                   handler_class   = ''
                                   description     = 'TODOs'
                                   category_scheme = 'http://www.msg.com/adt/categories/ab/todos'
                                   category_term   = 'todos'
                               ).

    collection->register_disc_res_w_template(
                   relation = 'http://www.msg.com/adt/relations/ab/todos'
                   template = '/resttodos/ab/todos/{OBJ_NAME}/{SCAN_TODO}/{SCAN_FIXME}/{SCAN_XXX}/{DEEP_SCAN}/{CREATED_BY_ME}/{CUSTOM_TEXT}'
                   description = 'TODO plugin'
                   type = 'application/xml'
                   handler_class = zcl_ab_todo_res_controller=>co_class_name
                ).
  ENDMETHOD.

  METHOD if_adt_rest_rfc_application~get_static_uri_path.

    result = '/msg/todo'.
  ENDMETHOD.

ENDCLASS.
